unit FilterParsers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUtf8, expressions, dxctrls, strconsts, dxtypes, mytypes;

type
  { ESQLSelectStatementError }

  ESQLSelectStatementError = class(Exception)
  private
    FPos: Integer;
    FProp: String;
  public
    constructor Create(const Msg, AProp: String; P: Integer);
    property Position: Integer read FPos;
    property Prop: String read FProp;
  end;

  { EFilterParserError }

  EFilterParserError = class(Exception)
  private
    FPos: Integer;
  public
    constructor Create(const msg: string; P: Integer);
    property Position: Integer read FPos;
  end;


  { TSQLFilterParser }

  TSQLFilterParser = class
  private
    FDisableCalcExpr: Boolean;
    FExprBuilder: TExpressionBuilder;
    FPos: Integer;
    FSS: TSession;
    FFlt: String;
  protected
    FOldPos: Integer;
    function FieldNameParse(const FieldName: String): String; virtual;
    function CheckValue(var Value: String): Boolean; virtual;
    function CheckOp(const Op: String): Boolean; virtual;
    function GetAnotherStr(const AValue: String): String; virtual;
    function GetFieldType: String; virtual;
  public
    function Parse(const Flt: String): String;
    property ExprBuilder: TExpressionBuilder read FExprBuilder write FExprBuilder;
    property Position: Integer read FPos;
    property Session: TSession read FSS write FSS;
  end;

  { TSQLLookupFilterParser }

  {TSQLLookupFilterParser = class(TSQLFilterParser)
  private
    FCmp: TdxField;
    FSrcForm: TdxForm;
  protected
    function FieldNameParse(const FieldName: String): String; override;
    function CheckValue(var Value: String): Boolean; override;
  public
    property SrcForm: TdxForm read FSrcForm write FSrcForm;
  end;    }

  { TSQLSourceFilterParser }

  TSQLSourceFilterParser = class(TSQLFilterParser)
  private
    FAliasSL: TStrings;
    FForm: TdxForm;
    FJoinStr, FFieldName, FOp, FValue: String;
    FParForm: TdxForm;
    FCmp: TdxField;
  protected
    function FieldNameParse(const aFieldName: String): String; override;
    function CheckValue(var Value: String): Boolean; override;
    function CheckOp(const Op: String): Boolean; override;
    function GetAnotherStr(const AValue: String): String; override;
    function GetFieldType: String; override;
  public
    property Form: TdxForm read FForm write FForm;
    property ParentForm: TdxForm read FParForm write FParForm;
    property JoinStr: String read FJoinStr write FJoinStr;
    property AliasSL: TStrings read FAliasSL write FAliasSL;
  end;

  { TQryAppendRecParser }

  TQryAppendRecParser = class(TSQLFilterParser)
  private
    FCmp: TdxField;
    FCurRS: TSsRecordSet;
    FOp: String;
    FSrcRS: TSsRecordSet;
  protected
    function FieldNameParse(const FieldName: String): String; override;
    function CheckValue(var Value: String): Boolean; override;
    function CheckOp(const Op: String): Boolean; override;
    function GetFieldType: String; override;
  public
    function Parse(const Flt: String): String;
    property SrcRS: TSsRecordSet read FSrcRS write FSrcRS;
    property CurRS: TSsRecordSet read FCurRS write FCurRS;
  end;

implementation

uses
  apputils, sqlgen, Variants;

procedure DoFilterParserError(const Msg: String; P: Integer);
begin
  raise EFilterParserError.Create(Msg, P);
end;

function CheckOp(const S: String): Boolean;
begin
  Result := Pos(S, '= <> <= >= == #') > 0;
end;

{ ESQLSelectStatementError }

constructor ESQLSelectStatementError.Create(const Msg, AProp: String; P: Integer
  );
begin
  inherited Create(Msg);
  FPos := P;
  FProp := AProp;
end;

{ EFilterParserError }

constructor EFilterParserError.Create(const msg: string; P: Integer);
begin
  inherited Create(msg);
  FPos := P;
end;

{ TSQLLookupFilterParser }

(*function TSQLLookupFilterParser.FieldNameParse(const FieldName: String): String;
begin
  Result := '';
  FCmp := FSrcForm.FindFieldByName(FieldName);
  if (FCmp = nil) or (FCmp is TdxFile) or (FCmp is TdxDBImage) then Exit;
  Result := TableStr(FSrcForm.Id) + '.' + FieldStr(FCmp.Id);
end;

function TSQLLookupFilterParser.CheckValue(var Value: String): Boolean;
begin
  Result := CheckType(FCmp, Value);
  // экранируем апострофы
  if NeedQuote(FCmp) then
    Value := '''' + StringReplace(Value, #39, #39#39, [rfReplaceAll]) + '''';
end; *)

{ TSQLFilterParser }

function TSQLFilterParser.FieldNameParse(const FieldName: String): String;
begin
  Result := '';
end;

function TSQLFilterParser.CheckValue(var Value: String): Boolean;
begin
  Result := True;
end;

function TSQLFilterParser.CheckOp(const Op: String): Boolean;
begin
  Result := Pos(Op, '= <> <= >= == # in notin') > 0;
end;

// Эта функция введена для изменения строки условия, если полем является
// иерархическая группа
function TSQLFilterParser.GetAnotherStr(const AValue: String): String;
begin
  Result := '';
end;

function TSQLFilterParser.GetFieldType: String;
begin
  Result := '';
end;

// Если необязательные поля идут после, то возможна ситуация, когда в
// конце оказывается and или or. В этом случае добавляем фиктивное условие.
function TrailLastOp(const S: String): String;
var
  Tmp: String;
  L: Integer;
begin
  Result := S;
  L := Length(S);
  Tmp := Trim(Copy(S, L - 4, 4));
  if Tmp = 'and'  then Result := S + ' 1=1 '
  else if Tmp = 'or' then Result := S + ' 1=0 ';
end;

function ValueTypeToStr(const Value: String): String;
var
  N: Longint;
  E: Double;
  DT: TDateTime;
begin
  if TryStrToInt(Value, N) then Result := rsNumber
  else if TryStrToFloat(Value, E) then Result := rsNumber
  else if TryStrToTime(Value, DT) then Result := rsTime
  else if TryStrToDate(Value, DT) then Result := rsDate
  else Result := rsText;
end;

function TSQLFilterParser.Parse(const Flt: String): String;
type
  TState = (stField, stOp, stExpr, stBoolOp, stBrace);
var
  S, Wh, WhIn, Expr, Op, BlOp, FlNm, Tmp: String;
  l, BrCnt, BrN, ExprPos, i: Integer;
  Tk: Char;
  St: TState;
  EB: TExpressionBuilder;
  E: TExpression;
  V: Variant;
  Optional, NeedBrace, EmptyInValueExists, PredicateInUsed: Boolean;
  SL: TStringList;
begin
  FFlt := Flt;
  Result := '';
  Wh := '';
  FPos := 1;
  St := stField;
  EB := FExprBuilder;
  E := nil;
  BrN := 0;

  try

  while True do
  begin
    FOldPos := FPos;
    S := ReadToken(Flt, FPos, Tk);
    if (S = '{') and (St = stField) then
    begin
      Inc(BrN);
      Wh := Wh + '(';
      Continue;
    end
    else if (Tk in [' ', '/']) and (St <> stExpr) then Continue;
    case St of
      stField:
        begin
          if Tk = '[' then
          begin
            BlOp := '';
            Optional := False;
            Delete(S, 1, 1);          // Удаляем
            Delete(S, Length(S), 1);  // скобки
            if (Length(S) > 0) and (S[1] = '?') then
            begin
              Optional := True;
              Delete(S, 1, 1);
            end;
            FlNm := FieldNameParse(S);
            if FlNm = '' then DoFilterParserError(Format(rsFPSrcFldNotFound, [S]), FOldPos);
            St := stOp;
          end
          else if Tk = #0 then Break
          else DoFilterParserError(rsFPSrcFldExcept, FOldPos);
        end;
      stOp:
        begin
          if (Tk = '=') or ((Tk = 'a') and ((LowerCase(S) = 'in') or (LowerCase(S) = 'notin'))) then
          begin
            S := LowerCase(S);
            if not CheckOp(S) then DoFilterParserError(rsInvalidCmpOp, FOldPos);
            St := stExpr;
            BrCnt := 0;
            Expr := '';
            if S = '==' then S := ' containing '
            else if S = '#' then S := ' not containing ';
            Op := S;
            ExprPos := FPos;
          end
          else DoFilterParserError(rsFPCmpOpExpect, FOldPos);
        end;
      stExpr:
        begin
          NeedBrace := S = '}';
          if (Tk = #0) or (((S = '&') or (S = '|')) and (BrCnt = 0))
            or (NeedBrace) then
          begin
            try
              FreeAndNil(E);
              E := EB.Build(Expr);
              if E = nil then DoFilterParserError(rsFPExprNotParsed, ExprPos);
              if not FDisableCalcExpr then V := E.Calc;
            except
              on Err: ECalcError do
                DoFilterParserError(Err.Message, ExprPos + Err.Position - 1)
            end;
            if (V = Null) and Optional then
            else if V = Null then
            begin
              if (Op = '<>') or (Op = ' not containing ') or (Op = 'notin') then
                S := ' is not null '
              else
                S := ' is null ';
              Wh := Wh + FlNm + S;
            end
            else
            begin
              S := VarToStr(V);
              if (Op = 'in') or (Op = 'notin') then
              begin
                SL := TStringListUtf8.Create;
                SplitStr(Utf8LowerCase(S), ';', SL);
                WhIn := '';
                EmptyInValueExists := SL.Count = 0;
                for i := 0 to SL.Count - 1 do
                begin
                  S := SL[i];
                  if S = '' then
                  begin
                    EmptyInValueExists := True;
                    Continue;
                  end;
                  if not CheckValue(S) then
                    DoFilterParserError(Format(rsIncompatibleTypesSrcFieldAndExpr,
                      [ValueTypeToStr(S), GetFieldType]), ExprPos);
                  Tmp := GetAnotherStr(S);
                  if Tmp = '' then
                    WhIn := WhIn + S + ','
                  else
                  begin
                    WhIn := WhIn + Tmp;
                    if Op = 'in' then WhIn := WhIn + ' or '
                    else WhIn := WhIn + ' and ';
                  end;
                end;
                if WhIn <> '' then
                begin
                  if WhIn[Length(WhIn)] = ',' then
                  begin
                    SetLength(WhIn, Length(WhIn) - 1);
                    WhIn := 'in (' + WhIn + ')';
                    if Op = 'notin' then WhIn := 'not ' + WhIn;
                    if GetFieldType = rsText then
                      WhIn := 'lower(' + FlNm + ') ' + WhIn
                    else
                      WhIn := FlNm + ' ' + WhIn;
                  end
                  else
                    SetLength(WhIn, Length(WhIn) - 4);
                end;
                if EmptyInValueExists then
                begin
                  if WhIn <> '' then
                  begin
                    if Op = 'in' then WhIn := WhIn + ' or ' + FlNm + ' is null'
                    else WhIn := WhIn + ' and ' + FlNm + ' is not null';
                  end
                  else
                  begin
                    if Op = 'in' then WhIn := FlNm + ' is null'
                    else WhIn := FlNm + ' is not null';
                  end;
                end;
                if WhIn <> '' then
                  Wh := Wh + '(' + WhIn + ')';
                SL.Free;
              end
              else
              begin
                if not CheckValue(S) then
                  DoFilterParserError(Format(rsIncompatibleTypesSrcFieldAndExpr,
                    [VarTypeToStr(V), GetFieldType]), ExprPos);
                Tmp := GetAnotherStr(S);
                if Tmp = '' then
                  Wh := Wh + FlNm + Op + S
                else
                  Wh := Wh + Tmp;
              end;
            end;
            if NeedBrace then
            begin
              Dec(FPos);
              St := stBrace;
            end
            else if Tk <> #0 then
            begin
              St := stBoolOp;
              Dec(FPos);
            end
            else Break;
          end
          else
          begin
            if S = '(' then
            begin
              Inc(BrCnt);
              Expr := Expr + S;
            end
            else if S = ')' then
            begin
              Dec(BrCnt);
              Expr := Expr + S;
            end
            else Expr := Expr + S;
          end;
        end;
      stBoolOp:
        begin
          if Optional and (V = Null) then
          else
          begin
            if S = '&' then Wh := Wh + ' and '
            else if S = '|' then Wh := Wh + '  or '
            else DoFilterParserError(rsFPBoolOpExpect, FOldPos);
            BlOp := S;
          end;
          St := stField;
        end;
      stBrace:
        begin
          if S = '}' then
          begin
            Wh := TrailLastOp(Wh);
            l := Length(Wh);
            if Copy(Wh, l, 1) = '(' then
              Delete(Wh, l, 1)
            else
            begin
              Wh := Wh + ')';
              Optional := False;		// Сбрасываем флаг, иначе идущая после
              											// логическая операция проигнорируется (баг от 28.03.2017).
            end;
            Dec(BrN);
          end
          else if (S = '|') or (S = '&') then
          begin
	          BlOp := S;
	 	        Dec(FPos);
  	  	    St := stBoolOp;
          end
          else if Tk = #0 then Break
          else DoFilterParserError(rsFPBoolOpExpect, FOldPos);
        end;
    end;
  end;
  if (BlOp = '|') or (BlOp = '&') then DoFilterParserError(rsFPSrcFldExcept, FPos)
  else if BrN < 0 then DoFilterParserError(rsCharLBrExpext, 1)
  else if BrN > 0 then DoFilterParserError(rsCharRBrExpect, FPos);
  // Если необязательные поля идут после, то возможна ситуация, когда в
  // конце оказывается and или or
  //S := Trim(Copy(Wh, Length(Wh) - 4, 4));
  //if (S = 'and') or (S = 'or') then Wh := Copy(Wh, 1, Length(Wh) - 4);
  Result := TrailLastOp(Wh);

  finally
    FreeAndNil(E);
  end;
end;

(*function TSQLFilterParser.Parse(const Flt: String): String;
type
  TState = (stField, stOp, stExpr, stBoolOp, stBrace);
var
  S, Wh, Expr, Op, BlOp, FlNm, Tmp: String;
  l, BrCnt, BrN, ExprPos: Integer;
  Tk: Char;
  St: TState;
  EB: TExpressionBuilder;
  E: TExpression;
  V: Variant;
  Optional, NeedBrace: Boolean;
begin
  Result := '';
  Wh := '';
  FPos := 1;
  St := stField;
  EB := FExprBuilder;
  E := nil;
  BrN := 0;

  try

  while True do
  begin
    FOldPos := FPos;
    S := ReadToken(Flt, FPos, Tk);
    if (S = '{') and (St = stField) then
    begin
      Inc(BrN);
      Wh := Wh + '(';
      Continue;
    end
    else if (Tk in [' ', '/']) and (St <> stExpr) then Continue;
    case St of
      stField:
        begin
          if Tk = '[' then
          begin
            BlOp := '';
            Optional := False;
            Delete(S, 1, 1);          // Удаляем
            Delete(S, Length(S), 1);  // скобки
            if (Length(S) > 0) and (S[1] = '?') then
            begin
              Optional := True;
              Delete(S, 1, 1);
            end;
            FlNm := FieldNameParse(S);
            if FlNm = '' then DoFilterParserError(Format(rsFPSrcFldNotFound, [S]), FOldPos);
            St := stOp;
          end
          else if Tk = #0 then Break
          else DoFilterParserError(rsFPSrcFldExcept, FOldPos);
        end;
      stOp:
        begin
          if Tk = '=' then
          begin
            if not CheckOp(S) then DoFilterParserError(rsInvalidCmpOp, FOldPos);
            St := stExpr;
            BrCnt := 0;
            Expr := '';
            if S = '==' then S := ' containing '
            else if S = '#' then S := ' not containing ';
            Op := S;
            ExprPos := FPos;
          end
          else DoFilterParserError(rsFPCmpOpExpect, FOldPos);
        end;
      stExpr:
        begin
          NeedBrace := S = '}';
          if (Tk = #0) or (((S = '&') or (S = '|')) and (BrCnt = 0))
            or (NeedBrace) then
          begin
            try
              FreeAndNil(E);
              E := EB.Build(Expr);
              if E = nil then DoFilterParserError(rsFPExprNotParsed, ExprPos);
              if not FDisableCalcExpr then V := E.Calc;
            except
              on Err: ECalcError do
                DoFilterParserError(Err.Message{Format(rsFPErrInExpr, [Expr, Err.Message])}, ExprPos + Err.Position - 1)
            end;
            if (V = Null) and Optional then
            else if V = Null then
            begin
              if Op = '<>' then
                S := ' is not null '
              else
                S := ' is null ';
              Wh := Wh + FlNm + S;
            end
            else
            begin
              S := VarToStr(V);
              if not CheckValue(S) then
                DoFilterParserError(Format(rsIncompatibleTypesSrcFieldAndExpr,
                  [VarTypeToStr(V), GetFieldType]), ExprPos);
              //if not CheckValue(S) then DoFilterParserError(rsIncompatibleTypesSrcFieldAndExpr, ExprPos);
              Tmp := GetAnotherStr;
              if Tmp = '' then
                Wh := Wh + FlNm + Op + S
              else
                Wh := Wh + Tmp;
            end;
            if NeedBrace then
            begin
              Dec(FPos);
              St := stBrace;
            end
            else if Tk <> #0 then
            begin
              St := stBoolOp;
              Dec(FPos);
            end
            else Break;
          end
          else
          begin
            if S = '(' then
            begin
              Inc(BrCnt);
              Expr := Expr + S;
            end
            else if S = ')' then
            begin
              Dec(BrCnt);
              Expr := Expr + S;
            end
            {else if Tk = '[' then Expr := Expr + '[' + S + ']'
            else if Tk = '''' then
            begin
              if Pos('"', S) > 0 then
                Expr := Expr + '''' + S + ''''
              else
                Expr := Expr + '"' + S + '"';
            end }
            else Expr := Expr + S;
          end;
        end;
      stBoolOp:
        begin
          if Optional and (V = Null) then
          else
          begin
            if S = '&' then Wh := Wh + ' and '
            else if S = '|' then Wh := Wh + '  or '
            else DoFilterParserError(rsFPBoolOpExpect, FOldPos);
            BlOp := S;
          end;
          St := stField;
        end;
      stBrace:
        begin
          if S = '}' then
          begin
            Wh := TrailLastOp(Wh);
            l := Length(Wh);
            if Copy(Wh, l, 1) = '(' then
              Delete(Wh, l, 1)
            else
            begin
              Wh := Wh + ')';
              Optional := False;		// Сбрасываем флаг, иначе идущая после
              											// логическая операция проигнорируется (баг от 28.03.2017).
            end;
            Dec(BrN);
          end
          else if (S = '|') or (S = '&') then
          begin
	          BlOp := S;
	 	        Dec(FPos);
  	  	    St := stBoolOp;
          end
          else if Tk = #0 then Break
          else DoFilterParserError(rsFPBoolOpExpect, FOldPos);
        end;
    end;
  end;
  if (BlOp = '|') or (BlOp = '&') then DoFilterParserError(rsFPSrcFldExcept, FPos)
  else if BrN < 0 then DoFilterParserError(rsCharLBrExpext, 1)
  else if BrN > 0 then DoFilterParserError(rsCharRBrExpect, FPos);
  // Если необязательные поля идут после, то возможна ситуация, когда в
  // конце оказывается and или or
  //S := Trim(Copy(Wh, Length(Wh) - 4, 4));
  //if (S = 'and') or (S = 'or') then Wh := Copy(Wh, 1, Length(Wh) - 4);
  Result := TrailLastOp(Wh);

  finally
    FreeAndNil(E);
  end;
end;   *)

{ TSQLSourceFilterParser }

function TSQLSourceFilterParser.FieldNameParse(const aFieldName: String
  ): String;
var
  Fm: TdxForm;
  SL: TStringList;
  i: Integer;
  FieldName, S, ParentAliasNm, ParentField, AliasName, Tmp, FlNm, AliasNm: String;
  C, ParentC, RootC: TdxField;
begin
  Result := '';
  FieldName := aFieldName;
  if Length(FieldName) = 0 then Exit;
  Fm := FForm;
  if FieldName[1] = '!' then
  begin
    Fm := FParForm;
    Delete(FieldName, 1, 1);
  end;
  if (Length(FieldName) = 0) or (Fm = nil) then Exit;
  AliasName := '';
  C := nil;
  SL := TStringList.Create;

  try

  SplitStr(FieldName, '|', SL);
  for i := 0 to SL.Count - 1 do
  begin
    if i > 0 then AliasName := AliasName + '_';
    AliasName := AliasName + TableStr(Fm.Id);
    S := SL[i];
    C := Fm.FindFieldByName(S);
    if C = nil then Exit;
    if i > 0 then
    begin
      if AliasSL.IndexOf(AliasName) < 0 then
      begin
        AliasSL.Add(AliasName);
        AliasNm := AliasStr(AliasSL, AliasName);
        Tmp := SqlSelectGroups(FSS, Fm.Id, True);
        if Tmp <> '' then Tmp := '(' + Tmp + ')'
        else Tmp := TableStr(Fm.Id);
        JoinStr := JoinStr + GetJoinType(FSS, RootC, ParentC) + Tmp + ' ' + AliasNm +
          ' on ' + ParentAliasNm + '.' + ParentField + '=' + AliasNm + '.id';
      end
      else
        AliasNm := AliasStr(AliasSL, AliasName);
    end
    else
      AliasNm := AliasName;
    ParentAliasNm := AliasNm;
    ParentField := FieldStr(C.Id);
    ParentC := C;
    if C is TdxLookupComboBox then
    begin
      if i = 0 then RootC := C;
      if i < SL.Count - 1 then
      begin
        AliasName := AliasName + '_' + ParentField;
        Fm := FSS.FormMan.FindForm(GetSourceTId(C));
        if Fm = nil then Exit;
      end;
    end
    else if i < SL.Count - 1 then Exit;
  end;
  FlNm := FieldStr(C.Id);
  if C is TdxFile then FlNm := FlNm + 'd'
  else if C is TdxDBImage then FlNm := FlNm + 'src'
  else if C is TdxRecordId then FlNm := 'id';
  FCmp := C;
  Result := AliasNm + '.' + FlNm;

  finally
    SL.Free;
  end;
  FFieldName := Result;
end;

function TSQLSourceFilterParser.CheckValue(var Value: String): Boolean;
var
  Tmp, AbsValue: String;
begin
  Result := CheckType(FCmp, Value);
  // экранируем апострофы
  if (FCmp is TdxEdit) or (FCmp is TdxMemo) or (FCmp is TdxComboBox) or
    (FCmp is TdxDateEdit) or (FCmp is TdxFile) or (FCmp is TdxDBImage) then
  begin
    Value := '''' + EscapeSQuotes(Value) + ''''
  end
  else if FCmp is TdxCalcEdit then
  begin
    AbsValue := StringReplace(Value, '-', '', []);
    if FOp = '>=' then
    	Value := Value + '-' + AbsValue + '*2e-12'
    else if FOp = '<=' then
    	Value := Value + '+' + AbsValue + '*2e-12';
  end
  else if FCmp is TdxTimeEdit then
  begin
    if FOp = '>=' then
    begin
    	CheckTime(TdxTimeEdit(FCmp).TimeFormat, Value, Tmp);
    end
    else if FOp = '<=' then
    begin
    	CheckTime(TdxTimeEdit(FCmp).TimeFormat, Tmp, Value);
    end;
    // = обрабатывается в GetAnotherStr
    {else}
    if (FOp <> '=') and (FOp <> '<>') and (FOp <> 'in') and (FOp <> 'notin') then
    	Value := '''' + Value + '''';
  end;
  //FValue := Value;
end;

function TSQLSourceFilterParser.CheckOp(const Op: String): Boolean;
begin
  FOp := Op;
  Result:=inherited CheckOp(Op);
  if (FCmp is TdxLookupComboBox) and (Op <> '=') and (Op <> '<>') and
    (Op <> 'in') and (Op <> 'notin') then Result := False;
end;

function TSQLSourceFilterParser.GetAnotherStr(const AValue: String): String;
var
  Fm: TdxForm;
  TId: Integer;
  Bg, Ed, AbsValue, S, Op: String;
begin
  Result := '';
  if FOp = 'in' then Op := '='
  else if FOp = 'notin' then Op := '<>'
  else Op := FOp;

  if FCmp is TdxLookupComboBox then
  begin
    TId := GetSourceTId(FCmp);
    Fm := FSS.FormMan.FindForm(TId);
    if (Fm <> nil) and (Fm.ParentField > 0) then
    begin
      S := SqlSelectIDs(FSS, TId, AValue);
      Result := '''' + S + ''' containing ''\'' || ' + FFieldName + ' || ''\''';
      if Op = '<>' then Result := 'not (' + Result + ')';
    end;
  end
  else if FCmp is TdxCalcEdit then
  begin
    if Op = '=' then
    begin
      AbsValue := StringReplace(AValue, '-', '', []);
      Result := FFieldName + '>=' + AValue + '-' + AbsValue + '*2e-12 and ' +
      	FFieldName + '<=' + AValue + '+' + AbsValue + '*2e-12';
    end
    else if Op = '<>' then
    begin
      AbsValue := StringReplace(AValue, '-', '', []);
      Result := '(' + FFieldName + '<' + AValue + '-' + AbsValue + '*2e-12 or ' +
      	FFieldName + '>' + AValue + '+' + AbsValue + '*2e-12)';
    end;
  end
  else if FCmp is TdxTimeEdit then
  begin
    if (Op = '=') or (Op = '<>') then
    begin
      Bg := AValue;
      Ed := AValue;
      CheckTime(TdxTimeEdit(FCmp).TimeFormat, Bg, Ed);
      if Op = '=' then
        Result := FFieldName + '>=''' + Bg + ''' and ' +
        	FFieldName + '<=''' + Ed + ''''
      else
        Result := '(' + FFieldName + '<''' + Bg + ''' or ' +
          FFieldName + '>''' + Ed + ''')';
    end;
  end;
end;

(*function TSQLSourceFilterParser.CheckValue(var Value: String): Boolean;
var
  Tmp, AbsValue: String;
begin
  Result := CheckType(FCmp, Value);
  if FCmp is TdxLookupComboBox then
  // экранируем апострофы
  else if (FCmp is TdxEdit) or (FCmp is TdxMemo) or (FCmp is TdxComboBox) or
    (FCmp is TdxDateEdit) or (FCmp is TdxFile) or (FCmp is TdxDBImage) then
    Value := '''' + EscapeSQuotes(Value) + ''''
  else if FCmp is TdxCalcEdit then
  begin
    AbsValue := StringReplace(Value, '-', '', []);
    if FOp = '>=' then
    	Value := Value + '-' + AbsValue + '*2e-12'
    else if FOp = '<=' then
    	Value := Value + '+' + AbsValue + '*2e-12';
  end
  else if FCmp is TdxTimeEdit then
  begin
    if FOp = '>=' then
    begin
    	CheckTime(TdxTimeEdit(FCmp).TimeFormat, Value, Tmp);
      //Value := '''' + Value + '''';
    end
    else if FOp = '<=' then
    begin
    	CheckTime(TdxTimeEdit(FCmp).TimeFormat, Tmp, Value);
      //Value := '''' + Value + '''';
    end;
    // = обрабатывается в GetAnotherStr
    {else}
    if FOp <> '=' then
    	Value := '''' + Value + '''';
  end;
  FValue := Value;
end;

function TSQLSourceFilterParser.CheckOp(const Op: String): Boolean;
begin
  FOp := Op;
  Result:=inherited CheckOp(Op);
  if (FCmp is TdxLookupComboBox) and (Op <> '=') and (Op <> '<>') then Result := False;
end;

function TSQLSourceFilterParser.GetAnotherStr: String;
var
  Fm: TdxForm;
  TId: Integer;
  Ed, AbsValue: String;
begin
  Result := '';
  if FCmp is TdxLookupComboBox then
  begin
    TId := GetSourceTId(FCmp);
    Fm := FSS.FormMan.FindForm(TId);
    if (Fm <> nil) and (Fm.ParentField > 0) then
    begin
      FValue := SqlSelectIDs(FSS, TId, FValue);
      Result := '''' + FValue + ''' containing ''\'' || ' + FFieldName + ' || ''\''';
      if FOp = '<>' then Result := 'not (' + Result + ')';
    end;
  end
  else if FCmp is TdxCalcEdit then
  begin
    if FOp = '=' then
    begin
      AbsValue := StringReplace(FValue, '-', '', []);
      Result := FFieldName + '>=' + FValue + '-' + AbsValue + '*2e-12 and ' +
      	FFieldName + '<=' + FValue + '+' + AbsValue + '*2e-12';
    end;
  end
  else if FCmp is TdxTimeEdit then
  begin
    if FOp = '=' then
    begin
      Ed := FValue;
      CheckTime(TdxTimeEdit(FCmp).TimeFormat, FValue, Ed);
      Result := FFieldName + '>=''' + FValue + ''' and ' +
      	FFieldName + '<=''' + Ed + '''';
    end;
  end;
end;      *)

function TSQLSourceFilterParser.GetFieldType: String;
begin
  Result:=GetComponentDataTypeStr(FCmp);
end;


{ TQryAppendRecParser }

function TQryAppendRecParser.FieldNameParse(const FieldName: String): String;
var
  S: String;
begin
  S := FieldName;
  if Copy(S, 1, 1) = '!' then Delete(S, 1, 1);
  FCmp := FSrcRS.Form.FindFieldByName(S);
  Result := FieldName;
end;

function TQryAppendRecParser.CheckValue(var Value: String): Boolean;
begin
  Result:=True;
  if (FOp <> '=') or (FCmp = nil) or (FCmp is TdxCounter) or (FCmp is TdxRecordId) or (not CheckType(FCmp, Value)) then Exit;
  if FCmp is TdxLookupComboBox then
    with TdxLookupComboBox(FCmp) do
    begin
      if (SourceTId > 0) and (SourceFId > 0) and (Value <> '0') then
        FSrcRS.SetDSField(FCmp, StrToInt(Value));
    end
  else if FCmp is TdxDateEdit then
    FSrcRS.SetDSField(FCmp, StrToDate(Value))
  else
    FSrcRS.SetDSField(FCmp, Value)
end;

function TQryAppendRecParser.CheckOp(const Op: String): Boolean;
begin
  FOp := Op;
  Result:=True;
end;

function TQryAppendRecParser.GetFieldType: String;
begin
  Result:=GetComponentDataTypeStr(FCmp);
end;

function TQryAppendRecParser.Parse(const Flt: String): String;
begin
  Result := '';
  ExprBuilder := TExpressionBuilder.Create;
  ExprBuilder.RecordSet := FCurRS;
  ExprBuilder.SkipLabels:=True;
  try
    inherited Parse(Flt);
  finally
    ExprBuilder.Free;
    ExprBuilder := nil;
  end;
end;

end.

