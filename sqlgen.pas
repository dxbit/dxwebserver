{-------------------------------------------------------------------------------

    Copyright 2016-2024 Pavel Duborkin ( mydataexpress@mail.ru )

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

-------------------------------------------------------------------------------}

unit SqlGen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dxctrls, Db, SQLDb, dxtypes, dxreports, mytypes, strconsts;

function SqlSelectStatement(ARecordSet: TSsRecordSet; Fields: TList; UseSelCond,
  UseFormFilter: Boolean; CallerObj: TComponent; const UserWhere: String;
  RecId: Integer; Distinct: Boolean; aStart, aCount: Integer; OnlyCount: Boolean): String;
//function SqlSelect(Fm: TdxForm; aStart, aCount: Integer): String;
//function SqlLookupSelect(ARecordSet: TSsRecordSet; C: TdxField;
//  UseFilter: Boolean; RecId: Integer): String; //function SqlLookupSelect(C: TdxComboBox; Wh: String): String;
//function SqlLookupFilter(SS: TSession; Cbx: TdxComboBox; DS: TDataSet; const RecId: String): String;
function SqlReportSelect(RD: TReportData; ARecordSet: TSsRecordSet): String;
function GetFuncSql(Fl: TRpField): String;
function SqlSelCondFilter(SrcRS, ARecordSet: TSsRecordSet; const Cond: String;
  var JStr: String; AliasSL: TStrings): String;
function SqlSelectGroups(SS: TSession; SourceTId: Integer; AllFields: Boolean): String;
function SqlSelectIDs(SS: TSession; SourceTId: Integer; SId: String): String;
function GetJoinType(SS: TSession; ParentObj, Obj: TdxField): String;
procedure CheckTime(fmt: TdxTimeFormat; var Bg, Ed: String);
function SqlUpdateStatementOnlyChanged(RS: TSsRecordSet): String;
function SqlLCbxSelect(ARecordSet: TSsRecordSet; LCbx: TdxLookupComboBox;
  const Fragments: String; First, Skip: Integer): String;
function SqlComboBoxSelect(ARecordSet: TSsRecordSet; Cbx: TdxComboBox;
  const Fragments: String; First, Skip: Integer): String;
function SqlFilterLCbxSelect(SS: TSession; LCbx: TdxCustomComboBox;
  const Fragments: String; First, Skip: Integer): String;
function SqlLookupSelect(SS: TSession; C: TdxField; RecId: Integer): String;
function SqlSimpleSelectStatement(Fm: TdxForm; RecId: Integer): String;
function AliasStr(AliasSL: TStrings; const AliasName: String): String;

implementation

uses
  apputils, expressions, filterparsers, strutils, dateutils, dbengine,
  dxsqlquery;

function CheckNumber(const S: String): String;
begin
  Result := StringReplace(S, DefaultFormatSettings.DecimalSeparator, '.', []);
end;

procedure CheckTime(fmt: TdxTimeFormat; var Bg, Ed: String);
begin
  case fmt of
    ttHH:
      begin
        if Bg <> '' then
	        Bg := FormatDateTime('hh:00:00:0000', StrToTime(Bg), DefaultSQLFormatSettings);
        if Ed <> '' then
	        Ed := FormatDateTime('hh:59:59:9999', StrToTime(Ed), DefaultSQLFormatSettings);
      end;
    ttHHMM:
      begin
        if Bg <> '' then
	        Bg := FormatDateTime('hh:nn:00:0000', StrToTime(Bg), DefaultSQLFormatSettings);
        if Ed <> '' then
	        Ed := FormatDateTime('hh:nn:59:9999', StrToTime(Ed), DefaultSQLFormatSettings);
      end;
    ttHHMMSS:
      begin
        if Bg <> '' then
	        Bg := FormatDateTime('hh:nn:ss:0000', StrToTime(Bg), DefaultSQLFormatSettings);
        if Ed <> '' then
	        Ed := FormatDateTime('hh:nn:ss:9999', StrToTime(Ed), DefaultSQLFormatSettings);
      end;
  end;
end;

function SqlSelectGroups(SS: TSession; SourceTId: Integer; AllFields: Boolean): String;
var
  Fm: TdxForm;
  TNm, FNm, PFNm, S, LevelChars: String;
  i, FId: Integer;
  C: TdxField;
  L: TList;
begin
  Result := '';
  if SourceTId = 0 then Exit;
  Fm := SS.FormMan.FindForm(SourceTId);
  FId := Fm.GetFormParentFieldFieldId;
  if FId = 0 then Exit;
  TNm := TableStr(SourceTId);
  FNm := FieldStr(FId);
  PFNm := FieldStr(Fm.ParentField);
	LevelChars := IntToStr(Fm.LevelCount * GetFieldSize(Fm.FindField(FId)) + Fm.LevelCount - 1);
  Result := 'with recursive pathes as ' +
	  '(select id, CAST(LEFT(' + FNm + ',' +
    LevelChars + ') as VARCHAR(' + LevelChars + ')) as ' + FNm + ' from ' + TNm +
    ' where ' + PFNm + ' is null ' +
    'union all ' +
    'select t.id,LEFT(p.' + FNm + ' || ''\'' || t.' + FNm + ',' + LevelChars + ') from ' + TNm +
    ' t join pathes p on t.' + PFNm + '=p.id) ';

  if AllFields then
  begin
    L := TList.Create;
    Fm.GetFields(L);
    S := '';
    for i := 0 to L.Count - 1 do
    begin
      C := TdxField(L[i]);
      if (C is TdxObjectField) or (C is TdxRecordId) then Continue;
      if C.Id <> FId then
      	S := S + ',' + TableStr(Fm.Id) + '.' + FieldStr(C.Id);
    end;
    Result := Result + 'select p.id,p.' + FNm +
    	S + ' from pathes p inner join ' +
    	TableStr(Fm.Id) + ' on p.id=' + TableStr(Fm.Id) + '.id';
    L.Free;
  end
  else
    Result := Result + 'select id,' + FNm + ' from pathes';
end;

function SqlSelectIDs(SS: TSession; SourceTId: Integer; SId: String): String;
var
  Fm: TdxForm;
  S, PFNm, TNm, Tmp: String;
  p: Integer;
begin
  Result := '';
  Fm := SS.FormMan.FindForm(SourceTId);
  if Fm.ParentField = 0 then Exit;
  TNm := TableStr(SourceTId);
  PFNm := FieldStr(Fm.ParentField);
  S := 'with recursive pathes as ' +
    '(select id, cast(id as varchar(100)) as pid from ' + TNm +
    ' where ' + PFNm + ' is null ' +
    'union all ' +
    'select t.id, t.id || ''\'' || p.pid from ' + TNm +
    ' t join pathes p on t.' + PFNm + '=p.id) ' +
    'select pid from pathes';
  with SS.DBase.OpenDataSet(S) do
  try
    S := '\';
    SId := '\' + SId + '\';
    while not Eof do
    begin
      Tmp := Fields[0].AsString;
      if Pos(SId, '\' + Tmp + '\') > 0 then
      begin
        p := Pos('\', Tmp);
        if p > 0 then S := S + Copy(Tmp, 1, p - 1) + '\'
        else S := S + Tmp + '\';
      end;
      Next;
    end;
    Result := S;
  finally
    Free;
  end;
end;

(*function SqlLookupSelect(C: TdxComboBox; Wh: String): String;
begin
  if Wh <> '' then Wh := ' where ' + Wh;
  Result := 'select id, ' + FieldStr(C.SourceFId) + ' from ' +
    TableStr(C.SourceTId) + Wh + ' order by ' + FieldStr(C.SourceFId);
end; *)

function SqlLookupFilter(ARecordSet: TSsRecordSet; Cbx: TdxField;
  AliasSL: TStrings; var JStr: String): String;
var
  EB: TExpressionBuilder;
  P: TSQLSourceFilterParser;
  SrcFm: TdxForm;
  Flt, S: String;
  SS: TSession;
begin
  Result := '';
  SrcFm := ARecordSet.Session.FormMan.FindForm(GetSourceTId(Cbx));
  if SrcFm = nil then Exit;
  SS := ARecordSet.Session;
  Flt := GetComboFilter(Cbx);

  if Flt <> '' then
  begin
    EB := TExpressionBuilder.Create;
    EB.RecordSet := ARecordSet;
    EB.SkipLabels:=True;
    P := TSQLSourceFilterParser.Create;
    P.ExprBuilder := EB;
    P.Form := SrcFm;
    P.ParentForm := SrcFm;
    P.AliasSL := AliasSL;
    P.JoinStr := JStr;
    P.Session := SS;
    try
      Result := P.Parse(Flt);
      JStr := P.JoinStr;
    finally
      P.Free;
      EB.Free;
    end;
  end;

  // !!! Доступ
  if SS.UserMan.GetApplySelCondToObj(SS.RoleId, SrcFm.Id) then
  begin
    S := SS.UserMan.GetSelCond(SS.RoleId, SrcFm.Id);
    if S <> '' then
    begin
      S := SqlSelCondFilter(ARecordSet, nil, S, JStr, AliasSL);
      if Result <> '' then Result := Result + ' and ';
      Result := Result + '(' + S + ')';
    end;
  end;
  //
end;

function GetAliasNameCbx(Cbx: TdxField): String;
begin
  with TdxLookupComboBox(Cbx) do
	  Result := TableStr(Form.Id) + '_' + FieldStr(Id) + '_' +
    	TableStr(SourceTId);
end;

function ProcessJoinObject(SS: TSession; Obj: TdxLookupComboBox; var Jn: String;
  AliasSL: TStringList): Boolean;
var
  STa, ST, T, F, AliasNm: String;
begin
  Result := True;
  T := TableStr(Obj.Form.Id);
  F := FieldStr(Obj.Id);
  with Obj do
    if (SourceTId > 0) and (SourceFId > 0) then
    begin
      STa := GetAliasNameCbx(Obj);
      if AliasSL.IndexOf(STa) < 0 then
      begin
        AliasSL.Add(STa);
        AliasNm := AliasStr(AliasSL, STa);

        ST := SqlSelectGroups(SS, SourceTId, True);
        if ST = '' then ST := TableStr(SourceTId)
        else ST := '(' + ST + ')';
        Jn := Jn + GetJoinType(SS, Obj, Obj) + ST + ' ' + AliasNm + ' on ' + T + '.' + F + '=' +
          AliasNm + '.id';
      end;
    end
    else
      Result := False;
end;

function ProcessObjectField(SS: TSession; Obj: TdxLookupComboBox; ObjF: TdxObjectField;
  var Fl, Jn: String; AliasSL: TStringList): Boolean;
var
  SrcFm: TdxForm;
  C: TdxField;
  F, Ta, ST, STa, CbxAliasNm, AliasNm, FlNm: String;
begin
  Result := False;
  if not ProcessJoinObject(SS, Obj, Jn, AliasSL) then Exit;

  SrcFm := SS.FormMan.FindForm(Obj.SourceTId);
  if SrcFm = nil then Exit;
  C := SrcFm.FindField(ObjF.FieldId);
  if C = nil then Exit;
  Ta := GetAliasNameCbx(Obj);
  CbxAliasNm := AliasStr(AliasSL, Ta);

  F := FieldStr(C.Id);
  if C is TdxLookupComboBox then
    with TdxLookupComboBox(C) do
    begin
      if (SourceTId > 0) and (SourceFId > 0) then
      begin
        STa := Ta + '_' + FieldStr(C.Id) + '_' + TableStr(SourceTId);
        if AliasSL.IndexOf(STa) < 0 then
        begin
          AliasSL.Add(STa);
          AliasNm := AliasStr(AliasSL, STa);

          ST := SqlSelectGroups(SS, SourceTId, True);
          if ST = '' then ST := TableStr(SourceTId)
          else ST := '(' + ST + ')';
          Jn := Jn + GetJoinType(SS, Obj, C) + ST + ' ' + AliasNm + ' on ' + CbxAliasNm + '.' + F + '=' +
            AliasNm + '.id';
        end
        else
          AliasNm := AliasStr(AliasSL, STa);

        if LookupObjectField(SS, ObjF, True) is TdxRecordId then FlNm := 'id'
        else FlNm := FieldStr(SourceFId);

        Fl := Fl + AliasNm + '.' + FlNm + ' as ' + FieldStr(ObjF.Id) + ',';
        Fl := Fl + AliasNm + '.id as ' + FieldStr(ObjF.Id) + 'id,';
      end
      else Exit;
    end
    else if C is TdxRecordId then
      Fl := Fl + CbxAliasNm + '.id as ' + FieldStr(ObjF.Id) + ','
    else
    begin
      Fl := Fl + CbxAliasNm + '.' + F + ' as ' + FieldStr(ObjF.Id) + ',';
    end;
  Result := True;
end;

{procedure GetObjectFieldComponent(SS: TSession; var C: TdxField; var FlNm: String);
var
  ObjF: TdxObjectField;
  Obj: TdxField;
  Fm, SrcFm: TdxForm;
begin
  ObjF := TdxObjectField(C);
  C := nil;
  Fm := ObjF.Form;
  Obj := Fm.FindField(ObjF.ObjId);
  if Obj <> nil then
  begin
    SrcFm := SS.FormMan.FindForm(GetSourceTId(Obj));
    if SrcFm <> nil then
    begin
      C := SrcFm.FindField(ObjF.FieldId);
      FlNm := TableStr(Fm.Id) + '_' + FieldStr(Obj.Id) + '_' +
      	TableStr(SrcFm.Id) + '.';// + FieldStr(ObjF.FieldId);
      if C is TdxRecordId then
        FlNm := FlNm + 'id'
      else
        FlNm := FlNm + FieldStr(ObjF.FieldId);
    end;
  end;
end;    }

procedure GetObjectFieldComponent(SS: TSession; var C: TdxField; out AliasTblNm, FlNm: String);
var
  ObjF: TdxObjectField;
  Obj: TdxField;
  Fm, SrcFm: TdxForm;
begin
  ObjF := TdxObjectField(C);
  C := nil; AliasTblNm := '';
  Fm := ObjF.Form;
  Obj := Fm.FindField(ObjF.ObjId);
  if Obj <> nil then
  begin
    SrcFm := SS.FormMan.FindForm(GetSourceTId(Obj));
    if SrcFm <> nil then
    begin
      C := SrcFm.FindField(ObjF.FieldId);
      AliasTblNm := TableStr(Fm.Id) + '_' + FieldStr(Obj.Id) + '_' +
      	TableStr(SrcFm.Id);
      if C is TdxRecordId then
        FlNm := 'id'
      else
        FlNm := FieldStr(ObjF.FieldId);
    end;
  end;
end;

function CheckDate(const Value: String): String;
var
  DT: TDateTime;
begin
  if Value = '' then Exit('');
  DT := StrToDate(Value);
  Result := Date2Str(DT);
end;

function DateCodeToPeriod(const S: String; out Bg, Ed: String): Boolean;
begin
  Result := False;
  if Copy(S, 1, 1) = '$' then
  begin
    Ed := DateToStr(Date);
    case TPeriodType(StrToInt(Copy(S, 2, 10))) of
      ptToday: Bg := DateToStr(Date);
      ptThisWeek: Bg := DateToStr( IncDay(Date, -DayOfTheWeek(Date)+1) );
      ptThisMonth: Bg := DateToStr( IncDay(Date, -DayOf(Date)+1) );
      ptThisYear: Bg := DateToStr( EncodeDate(YearOf(Date), 1, 1) );
    end;
    Result := True;
  end;
end;

function SqlFormFilter(SS: TSession; Fm: TdxForm; var Jn: String; AliasSL: TStringList): String;
var
  S, W, V, FlNm, Bg, BgAbs, Ed, EdAbs, Tmp, AliasName: String;
  i, j: Integer;
  C: TdxField;
  F: TFilterField;
  Flt: TFilterObject;
  ObjF: TdxObjectField;
begin
  Result := '';
  S := '';
  if Fm.RecordSet = nil then
    Flt := SS.FormList.GetForm(Fm).Filter
  else
    Flt := Fm.Filter;
  for i := 0 to Flt.Count - 1 do
  begin
    F := Flt.Fields[i];
    C := Fm.FindField(F.FId);
    if C = nil then Continue;
    FlNm := TableStr(Fm.Id) + '.' + FieldStr(C.Id);
    // Поле объекта
    if C is TdxObjectField then
    begin
      ObjF := TdxObjectField(C);
      GetObjectFieldComponent(SS, C, AliasName, FlNm);
      if C = nil then Continue;
      // ObjId > 0 и FieldId > 0, потому что C <> nil
      FlNm := AliasStr(AliasSL, AliasName) + '.' + FlNm;
      ProcessObjectField(SS, TdxLookupcomboBox(Fm.FindField(ObjF.ObjId)), ObjF, Tmp, Jn, AliasSL);
    end
    else if C is TdxFile then
      FlNm := FlNm + 'd'
    else if C is TdxDBImage then
      FlNm := FlNm + 'src'
    else if C is TdxRecordId then
      FlNm := TableStr(Fm.Id) + '.id';
    //
    V := '';
    if F.IsNull then
      V := V + FlNm + ' is null or ';
    for j := 0 to F.Values.Count - 1 do
    begin
      W := F.Values[j];
      if W = '' then Continue;

      if C is TdxLookupComboBox then
      begin
        Tmp := SqlSelectIds(SS, GetSourceTId(C), W);
        if Tmp <> '' then
          V := V + '''' + Tmp + ''' containing ''\'' || ' + FlNm + ' || ''\'' or '
        else
          V := V + FlNm + '=' + W + ' or ';
      end
      else if (C is TdxEdit) or (C is TdxComboBox) or (C is TdxMemo) or (C is TdxFile)
        or (C is TdxDBImage) then
        V := V + FlNm + ' containing ''' + EscapeSQuotes(W) + ''' or '
      else if C is TdxCalcEdit then
      begin
        V := V + '(';
        Bg := F.Value[j];
        if Bg <> '' then
        begin
          Bg := CheckNumber(F.Value[j]);
          BgAbs := StringReplace(Bg, '-', '', []);
          Bg := Bg + '-' + BgAbs + '*2e-12';
          V := V + FlNm + '>=' + Bg + ' and ';
        end;
        Ed := F.EndValue[j];
        if Ed <> '' then
        begin
          Ed := CheckNumber(Ed);
          EdAbs := StringReplace(Ed, '-', '', []);
          Ed := Ed + '+' + EdAbs + '*2e-12';
          V := V + FlNm + '<=' + Ed + ' and ';
        end;
        V := Copy(V, 1, Length(V) - 5) + ') or ';
      end
      else if (C is TdxCounter) or (C is TdxRecordId) then
      begin
        Bg := CheckNumber(F.Value[j]);
        Ed := CheckNumber(F.EndValue[j]);
        V := V + '(';
        if Bg <> '' then
          V := V + FlNm + '>=' + Bg + ' and ';
        if Ed <> '' then
          V := V + FlNm + '<=' + Ed + ' and ';
        V := Copy(V, 1, Length(V) - 5) + ') or ';
      end
      else if C is TdxDateEdit then
      begin
        if not DateCodeToPeriod(W, Bg, Ed) then
        begin
          Bg := CheckDate(F.Value[j]);
          Ed := CheckDate(F.EndValue[j]);
        end;
        V := V + '(';
        if Bg <> '' then
          V := V + FlNm + '>=''' + Bg + ''' and ';
        if Ed <> '' then
          V := V + FlNm + '<=''' + Ed + ''' and ';
        V := Copy(V, 1, Length(V) - 5) + ') or ';
      end
      else if C is TdxTimeEdit then
      begin
        Bg := F.Value[j];
        Ed := F.EndValue[j];
        CheckTime(TdxTimeEdit(C).TimeFormat, Bg, Ed);
        V := V + '(';
        if Bg <> '' then
          V := V + FlNm + '>=''' + Bg  + ''' and ';
        if Ed <> '' then
          V := V + FlNm + '<=''' + Ed + ''' and ';
        V := Copy(V, 1, Length(V) - 5) + ') or ';
      end
      else if C is TdxCheckBox then
        V := V + FlNm + '=' + W + ' or '
    end;
    V := Copy(V, 1, Length(V) - 4);
    if V <> '' then
    begin
      if F.IsNot then V := 'not (' + V + ')';
      S := S + '(' + V + ') and ';
    end;
  end;
  S := Copy(S, 1, Length(S) - 5);
  Result := S;
end;

function SqlSelectStatement(ARecordSet: TSsRecordSet; Fields: TList;
  UseSelCond, UseFormFilter: Boolean; CallerObj: TComponent;
  const UserWhere: String; RecId: Integer; Distinct: Boolean; aStart,
  aCount: Integer; OnlyCount: Boolean): String;
var
  i, Len: Integer;
  C, FmFl: TdxField;
  Fl, Jn, S,
  T,    // главная таблица
  F,    // поле главной таблицы
  //ST,   // таблица-источник (для лукап полей)
  SF,   // поле таблицы-источника
  STa   // алиас таблицы-источника
  , Wh, Order, W, AliasNm: String;
  AliasSL: TStringList;
  L: TList;
  Gr: TdxGrid;
  SC: TdxSortCol;
  Col: TdxColumn;
  SL: TStringListUtf8;
  SFm, Fm: TdxForm;
  SS: TSession;
begin
  Result := '';
  Fm := ARecordSet.Form;
  SS := ARecordSet.Session;
  AliasSL := TStringList.Create;
  T := TableStr(Fm.Id);
  Fl := T + '.id,'; Jn := T;
  if Fm.PId > 0 then
    Fl := Fl + T + '.pid,' + T + '.id as oldid,';

  L := nil;

  if Fields <> nil then
  	Len := Fields.Count
  else if OnlyCount then
    Len := 0
  else
  begin
    L := TList.Create;
    Fm.GetFields(L);
    Len := L.Count;
  end;

  for i := 0 to Len - 1 do
  begin
    if Fields <> nil then
    	C := TdxField(Fields[i])
    else
	    C := TdxField(L[i]);

    F := FieldStr(C.Id);
    if not (C is TdxObjectField) and not (C is TdxRecordId) then
      Fl := Fl + T + '.' + F + ',';
    if C is TdxLookupComboBox then
    begin
      if ProcessJoinObject(SS, TdxLookupComboBox(C), Jn, AliasSL) then
      begin
        AliasNm := AliasStr(AliasSL, GetAliasNameCbx(C));
        if GetListSourceField(ARecordSet.Session, TdxLookupComboBox(C)) is TdxRecordId then
          SF := 'id'
        else
          SF := FieldStr(GetSourceFId(C));
        Fl := Fl + AliasNm + '.' + SF + ' as ' + F + 'l,';
      end
      else
        Fl := Fl + 'null as ' + F + 'l,'; // заглушка
    end
    else if C is TdxObjectField then
    begin
      with TdxObjectField(C) do
        if (ObjId = 0) or (FieldId = 0) or not
          ProcessObjectField(SS, TdxLookupComboBox(Fm.FindField(ObjId)), TdxObjectField(C), Fl, Jn, AliasSL) then
          Fl := Fl + 'null as ' + F + ',';
    end
    else if C is TdxDBImage then
    begin
      Fl := Fl + T + '.' + F + 'src,' + T + '.' + F + 'dest,' + T + '.' +
        F + 'thumb,' + '0 as ' + F + 'c,';
    end
    else if C is TdxFile then
    begin
      Fl := Fl + T + '.' + F + 'src,' + T + '.' + F + 'dest,' + T + '.' +
        F + 'd,' + '0 as ' + F + 'c,';
    end
    else if C is TdxRecordId then
    begin
      Fl := Fl + T + '.id as ' + F + ',';
    end;
  end;
  SetLength(Fl, Length(Fl) - 1);

  if OnlyCount then Fl := 'count(' + T + '.id)';

  Wh := '';
  if RecId >= 0 then
  begin
    Wh := TableStr(Fm.Id) + '.id=' + IntToStr(RecId);
    // В иерархических справочниках, когда родитель является обязательным полем,
    // корневые элементы (без родителя) не попадают в выборку. Зато корневые
    // элементы можно увидеть в родительском поле. Однако при попытке редактировать
    // их (пункт "Изменить" в контекстном меню объекта) программа собщает, что
    // запись удалена другим пользователем. Просто из-за inner join элементы
    // с пустым родителем не выбираются. Поэтому при выборке одной записи
    // делаем left join, чтобы запись все-таки попала в выборку и ее можно
    // было редактировать.
    Jn := StringReplace(Jn, 'inner join', 'left join', [rfReplaceAll]);
  end
  else //if UserFilter = '' then
  begin
    // !!! Доступ
    if UseSelCond then
    begin
      S := SS.UserMan.GetSelCond(SS.RoleId, Fm.Id);
      try
        S := SqlSelCondFilter(ARecordSet, nil, S, Jn, AliasSL);
        if S <> '' then
	        Wh := Wh + '(' + S + ')';
      except
        on E: EFilterParserError do
          raise ESQLSelectStatementError.Create(E.Message, rsSelCond, E.Position);
      end;
    end;
    //
    if UseFormFilter then
    begin
      S := SqlFormFilter(SS, Fm, Jn, AliasSL);
      if S <> '' then S := S + ' and ';
      SL := TStringListUtf8.Create;
      SS.FormMan.GetSubForms(Fm.Id, SL);
      for i := 0 to SL.Count - 1 do
      begin
        SFm := TdxForm(SL.Objects[i]);
        W := SqlFormFilter(SS, SFm, Jn, AliasSL);
        if W <> '' then
          S := S + 'exists (select ' + TableStr(SFm.Id) + '.id from ' +
            TableStr(SFm.Id) + ' where ' +
            TableStr(SFm.Id) + '.pid=' + TableStr(Fm.Id) + '.id and ' + W + ') and ';
      end;
      SL.Free;
      SetLength(S, Length(S) - 5);
      if S <> '' then
      begin
        if Wh <> '' then Wh := Wh + ' and ';
        Wh := Wh + '(' + S + ')';
      end;
    end;
    {if CallerObj <> nil then
      try
  	    S := SqlListFilter(Fm, CallerObj, Jn, AliasSL);
        if S <> '' then
        begin
	        if Wh <> '' then Wh := Wh + ' and ';
  	      Wh := Wh + '(' + S + ')';
        end;
      except
        on E: EFilterParserError do
          raise ESQLSelectStatementError.Create(E.Message, rsListFilter, E.Position);
      end; }
    if Fm.CustomFilter <> '' then
    begin
      S := SqlSelCondFilter(ARecordSet, TSsRecordSet(Fm.CustomFilterRS), Fm.CustomFilter, Jn, AliasSL);
      if S <> '' then
      begin
        if Wh <> '' then Wh := Wh + ' and ';
        Wh := Wh + '(' + S + ')';
      end;
    end;
	end;

  if UserWhere <> '' then
  begin
    if Wh <> '' then Wh := Wh + ' and ';
    Wh := Wh + '(' + UserWhere + ')';
  end;

  if Distinct then
  	Result := 'select distinct '
  else if (aStart <> 0) or (aCount <> 0) then
    Result := 'select first ' + IntToStr(aCount) + ' skip ' + IntToStr(aStart) + ' '
  else
    Result := 'select ';

  Result := Result + Fl + ' from ' + Jn;
  if Wh <> '' then Result := Result + ' where ' + Wh;

  // Сортировка
  if not OnlyCount and (RecId < 0) then
  begin

    Gr := Fm.Grid;
    if Gr.SortCols.Count > 0 then
    begin
      Order := ' order by ';
      for i := 0 to Gr.SortCols.Count - 1 do
      begin
        SC := Gr.SortCols[i];
        Col := Gr.Columns[SC.Index];
        Order := Order + FieldStr(Col.Id);
        FmFl := Fm.FindField(Col.Id);
        if FmFl is TdxLookupComboBox then Order := Order + 'l'
        else if FmFl is TdxFile then Order := Order + 'd';
        if SC.Desc then
          Order := Order + ' desc';
        if i < Gr.SortCols.Count - 1 then
          Order := Order + ',';
      end;
      Result := Result + Order;
    end
    else if Fm.PId > 0 then
      Result := Result + ' order by id';

  end;

  AliasSL.Free;
  FreeAndNil(L);
end;

(*function SqlSelect(Fm: TdxForm; aStart, aCount: Integer): String;
var
  i, j: Integer;
  Fl, Jn,
  T,    // главная таблица
  F,    // поле главной таблицы
  ST,   // таблица-источник (для лукап полей)
  SF,   // поле таблицы-источника
  STa   // алиас таблицы-источника
  : String;
  L: TList;
  C, CC: TdxField;
begin
  Result := '';
  T := TableStr(Fm.Id);
  Fl := T + '.id,'; Jn := T;
  if Fm.PId > 0 then
    Fl := Fl + T + '.pid,';
  L := TList.Create;
  Fm.GetFields(L);
  for i := 0 to L.Count - 1 do
  begin
    C := TdxField(L[i]);
    // Вставляем заглушки для неопределенных полей объектов
    if C is TdxObjectField then
    begin
      with TdxObjectField(C) do
        if (ObjId = 0) or (FieldId = 0) then
          Fl := Fl + 'null as ' + FieldStr(C.Id) + ',';
      Continue;
    end;
    F := FieldStr(C.Id);
    Fl := Fl + T + '.' + F + ',';
    if C is TdxLookupComboBox then
    begin
      with TdxLookupComboBox(C) do
        if (SourceTId > 0) and (SourceFId > 0) then
        begin
          ST := TableStr(SourceTId);
          STa := ST + '_' + IntToStr(i);
          SF := FieldStr(SourceFId);
          Fl := Fl + STa + '.' + SF + ' as ' + F + 'l,';
          Jn := Jn + ' left join ' + ST + ' ' + STa + ' on ' + T + '.' + F + '=' +
            STa + '.id';

          // Поля объекта
          for j := 0 to L.Count - 1 do
          begin
            CC := TdxField(L[j]);
            if CC is TdxObjectField then
              with TdxObjectField(CC) do
                if (ObjId = C.Id) and (FieldId > 0) then
                  Fl := Fl + STa + '.' + FieldStr(FieldId) + ' as ' +
                    FieldStr(Id) + ',';
          end;
        end
        else
          Fl := Fl + 'null as ' + F + 'l,'; // заглушка
    end
    else if C is TdxDBImage then
      Fl := Fl + T + '.' + F + 'src,' + T + '.' + F + 'dest,' + T + '.' +
        F + 'thumb,'
    else if C is TdxFile then
      Fl := Fl + T + '.' + F + 'src,' + T + '.' + F + 'dest,' + T + '.' +
        F + 'd,';
  end;
  Fl := Copy(Fl, 1, Length(Fl) - 1);
  if (aStart <> 0) or (aCount <> 0) then
    Result := 'select first ' + IntToStr(aCount) + ' skip ' + IntToStr(aStart) + ' '
  else
    Result := 'select ';
  Result := Result + Fl + ' from ' + Jn;
  L.Free;
end;     *)

(*function SqlLookupFilter(SS: TSession; Cbx: TdxComboBox; DS: TDataSet;
  const RecId: String): String;
var
  EB: TExpressionBuilder;
  P: TSQLLookupFilterParser;
  SrcFm: TdxForm;
  Flt: String;
begin
  Result := '';
  SrcFm := SS.FormMan.FindForm(Cbx.SourceTId);
  if SrcFm = nil then Exit;
  Flt := Cbx.Filter;

  if Flt <> '' then
  begin
    EB := TExpressionBuilder.Create;
    EB.Session := SS;
    EB.DataSet := DS;
    EB.Form := Cbx.Form;
    EB.RecId:=RecId;
    EB.SkipLabels:=True;
    P := TSQLLookupFilterParser.Create;
    P.ExprBuilder := EB;
    P.SrcForm := SrcFm;
    try
      Result := P.Parse(Flt);
    finally
      P.Free;
      EB.Free;
    end;
  end;

 { // !!! Доступ
  if UserMan.GetApplySelCondToObj(SrcFm.Id) then
  begin
    S := UserMan.GetSelCond(SrcFm.Id);
    if S <> '' then
    begin
      S := SqlSelCondFilter(SrcFm, S);
      if Result <> '' then Result := Result + ' and ';
      Result := Result + '(' + S + ')';
    end;
  end;
  //    }
end;          *)

function GetFuncSql(Fl: TRpField): String;
var
  Nm: String;
begin
  Result := '';
  Nm := 'f' + IntToStr(Fl.Id);
  case Fl.Func of
    tfSum: Result:= 'coalesce(sum(' + Nm + '),0)';
    tfAvg: Result:= 'coalesce(avg(' + Nm + '),0)';
    tfMax: Result:= 'max(' + Nm + ')';
    tfMin: Result:= 'min(' + Nm + ')';
    tfCount: Result:= 'count(' + Nm + ')';
    tfProfit: Result:= 'coalesce(sum(income' + IntToStr(Fl.Id) + ')-sum(outcome' + IntToStr(Fl.Id) + '),0)';
    tfDistCount: Result := 'count(distinct ' + Nm + ')';
    tfMergeAll: Result := 'list(' + Nm + ',''; '')';
    tfMerge: Result := 'list(distinct ' + Nm + ',''; '')';
  end;
end;

function GetRpFieldComponent(SS: TSession; F: TRpField; aLow: Boolean): TdxComponent;
var
  Fm: TdxForm;
begin
  Result := nil;
  if aLow then F := GetLowField(@F)^;
  if F.TId = 0 then Exit;
  Fm := SS.FormMan.FindForm(F.TId);
  Result := Fm.FindField(F.FId);
end;

function SqlSourceFilter(ARecordSet: TSsRecordSet; Src: TRpSource;
  AliasSL: TStrings; var JoinStr: String): String;
var
  EB: TExpressionBuilder;
  P: TSQLSourceFilterParser;
  SS: TSession;
begin
  SS := ARecordSet.Session;
  EB := TExpressionBuilder.Create;
  // Запрос
  if ARecordSet.Parent <> nil then
    EB.RecordSet := ARecordSet.Parent
  // Отчет
  else
    EB.RecordSet := ARecordSet;
  EB.SkipLabels:=True;
  P := TSQLSourceFilterParser.Create;
  P.Session := SS;
  P.ExprBuilder := EB;
  P.ParentForm := SS.FormMan.FindForm(Src.Id);
  if Src.TId > 0 then
    P.Form := SS.FormMan.FindForm(Src.TId);
  P.AliasSL := AliasSL;
  P.JoinStr:=JoinStr;
  try
    Result := P.Parse(Src.Filter);
    JoinStr := P.JoinStr;
  finally
    P.Free;
    EB.Free;
  end;
end;

function GetJoinType(SS: TSession; ParentObj, Obj: TdxField): String;
begin
  if GetRequired(ParentObj) and GetRequired(Obj) and IsHierarchyObj(SS, Obj) then Result := ' inner join '
  else Result := ' left join ';
end;

function GetJoinTypeByRpField(SS: TSession; Fl: TRpField): String;
var
  Fm: TdxForm;
  PC, C: TdxField;
begin
  Fm := SS.FormMan.FindForm(Fl.TId);
  C := Fm.FindField(Fl.FId);
  PC := C;
  if Fl.Parent <> nil then
  begin
    Fl := Fl.Parent^;
    Fm :=SS.FormMan.FindForm(Fl.TId);
    PC := Fm.FindField(Fl.FId);
  end;
  Result := GetJoinType(SS, PC, C);
end;

function SqlSourceSelect2(ARecordSet: TSsRecordSet; Src: TRpSource): String;
var
  FStr, JStr, Flt: String;
  AliasSL: TStringList;
  i: Integer;
  Fl: TRpField;
  SS: TSession;

  function GetAliasName(const Fl: TRpField): String;
  begin
    if Fl.Parent = nil then
      Result := TableStr(Fl.TId)
    else
      Result := GetAliasName(Fl.Parent^) + '_' + FieldStr(Fl.Parent^.FId) +
      	'_' + TableStr(Fl.TId);
  end;

  function GetAliasOrTblNm(const Fl: TRpField): String;
  begin
    Result := GetAliasName(Fl);
    if Fl.Parent <> nil then
      Result := AliasStr(AliasSL, Result);
  end;

  procedure ProcessJoin(Fl: TRpField);
  var
    S, Tmp, AliasNm, ParentAliasNm: String;
  begin
    S := GetAliasName(Fl);
    if AliasSL.IndexOf(S) < 0 then
    begin
      AliasSL.Add(S);
      AliasNm := AliasStr(AliasSL, S);
      ParentAliasNm := GetAliasOrTblNm(Fl.Parent^);

      Tmp := SqlSelectGroups(SS, Fl.TId, True);
      if Tmp <> '' then Tmp := '(' + Tmp + ')'
      else Tmp := TableStr(Fl.TId);
      JStr := JStr + GetJoinTypeByRpField(SS, Fl.Parent^) + Tmp + ' ' + AliasNm +
        ' on ' + ParentAliasNm + '.' + FieldStr(Fl.Parent^.FId) + '=' + AliasNm + '.id';
    end;
  end;

  procedure ProcessField(const TopFl, Fl: TRpField);
  var
    TblNm, FlNm: String;
  begin
    if Fl.Parent <> nil then ProcessJoin(Fl);

    if (Fl.Tp = flObject) and (Fl.Src <> nil) then
      ProcessField(TopFl, Fl.Src^)
    else
    begin
      TblNm := GetAliasOrTblNm(Fl);
      if TopFl.Func = tfProfit then
      begin
        if Src.Kind = skIncome then
          FStr := FStr + TblNm + '.' + FieldStr(Fl.FId) + ' as income' + IntToStr(TopFl.Id) +
            ',0 as outcome' + IntToStr(TopFl.Id) + ','
        else
          FStr := FStr + '0 as income' + IntToStr(TopFl.Id) + ',' + TblNm + '.' + FieldStr(Fl.FId) +
            ' as outcome' + IntToStr(TopFl.Id) + ',';
      end
      else
      begin
        FlNm := FieldStr(Fl.FId);
        if Fl.Tp = flFile then FlNm := FlNm + 'd'
        else if Fl.Tp = flRecId then FlNm := 'id';
        FStr := FStr + TblNm + '.' + FlNm;
        FStr := FStr + ' as ' + FieldStr(TopFl.Id) + ',';
      end;
    end;
  end;

  procedure ProcessZero(Fl: TRpField);
  begin
    if Fl.Func = tfProfit then
      FStr := FStr + '0 as income' + IntToStr(Fl.Id) + ', 0 as outcome' + IntToStr(Fl.Id) + ','
    else
    begin
      case Fl.Func of
        tfSum: FStr := FStr + '0 as f' + IntToStr(Fl.Id) + ',';
        tfCount:
          if Fl.AllZeros then
            FStr := FStr + '0 as f' + IntToStr(Fl.Id) + ','
          else
	          FStr := FStr + 'null as f' + IntToStr(Fl.Id) + ',';
        else
          FStr := FStr + 'null as f' + IntToStr(Fl.Id) + ',';
      end;
    end;
  end;

begin
  SS := ARecordSet.Session;
  AliasSL := TStringList.Create;

  try

  FStr := ''; JStr := '';
  for i := 0 to Src.Fields.Count - 1 do
  begin
    Fl := Src.Fields[i]^;
    if Fl.Tp = flNone then Continue;
    if Fl.Zero then
      ProcessZero(Fl)
    //else if Fl.Func = tfProfit then
    //  ProcessSumField(Fl)
    else
      ProcessField(Fl, Fl);
  end;
  FStr := Copy(FStr, 1, Length(FStr) - 1);
  Result := 'select ' + FStr + ' from ' + TableStr(Src.Id);
  if Src.TId > 0 then
    Result := Result + ' left join ' + TableStr(Src.TId) + ' on ' + TableStr(Src.Id) + '.id=' +
      TableStr(Src.TId) + '.pid';
  Flt := '';
  if Trim(Src.Filter) <> '' then
    Flt := SqlSourceFilter(ARecordSet, Src, AliasSL, JStr);
  Result := Result + JStr;
  if Flt <> '' then
    Result := Result + ' where ' + Flt;

  finally
    AliasSL.Free;
  end;
end;

function GetHavingClause(SS: TSession; Fl: TRpField): String;
var
  i, p: Integer;
  S, V1, V2, Fn, rS, Tmp, AbsValue1, AbsValue2: String;
  SL: TStringList;
  C: TdxComponent;
  Tp: TRpFieldType;
begin
  rS := '';
  Result := '';
  Fn := GetFuncSql(Fl);
  if Fl.Func in [tfCount, tfDistCount] then Tp := flNumber
  else Tp := GetLowField(@Fl)^.Tp;

  SL := TStringList.Create;
  SplitStr(Fl.Value, ';', SL);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    if S = '' then Continue;
    Tmp := '';
    p := Pos(' .. ', S);
    if p > 0 then
    begin
      V1 := Copy(S, 1, p - 1);
      V2 := Copy(S, p + 4, 255);
    end
    else V1 := S;
    if Tp = flNumber then
    begin
      V1 := CheckNumber(Copy(S, 1, p - 1));
      AbsValue1 := StringReplace(V1, '-', '', []);
      V2 := CheckNumber(Copy(S, p + 4, 255));
      AbsValue2 := StringReplace(V2, '-', '', []);
      if V1 <> '' then
        Tmp := Tmp + Fn + '>=' + V1 + '-' + AbsValue1 + '*2e-12 and ';
      if V2 <> '' then
        Tmp := Tmp + Fn + '<=' + V2 + '+' + AbsValue2 + '*2e-12 and ';
      Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
      rS := rS + '(' + Tmp + ') or ';
    end
    else if Tp = flDate then
    begin
      if V1 <> '' then
      	Tmp := Tmp + Fn + '>=''' + V1 + ''' and ';
    	if V2 <> '' then
        Tmp := Tmp + Fn + '<=''' + V2 + ''' and ';
      Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
      rS := rS + '(' + Tmp + ') or ';
    end
    else if Tp = flTime then
    begin
      C := GetRpFieldComponent(SS, Fl, True);
      CheckTime(TdxTimeEdit(C).TimeFormat, V1, V2);
      if V1 <> '' then
      	Tmp := Tmp + Fn + '>=''' + V1 + ''' and ';
    	if V2 <> '' then
        Tmp := Tmp + Fn + '<=''' + V2 + ''' and ';
      Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
      rS := rS + '(' + Tmp + ') or ';
    end
    else if Tp in [flCounter, flRecId] then
    begin
      if V1 <> '' then
      	Tmp := Tmp + Fn + '>=' + V1 + ' and ';
    	if V2 <> '' then
        Tmp := Tmp + Fn + '<=' + V2 + ' and ';
      Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
      rS := rS + '(' + Tmp + ') or ';
    end
    else if Tp = flText then
    begin
      rS := rS + Fn + ' containing ''' + EscapeSQuotes(V1) + ''' or ';
    end
    else if Tp = flObject then
    begin
      C := GetRpFieldComponent(SS, Fl, True);
      Tmp := SqlSelectIDs(SS, GetSourceTId(C), V1);
      if Tmp <> '' then rS := rS + '''' + Tmp + ''' containing ''\'' || ' + Fn + ' || ''\'' or '
      else rS := rS + Fn + '=' + V1 + ' or ';
    end;
  end;
  if Fl.Nul then
    rS := rS + Fn + ' is null or ';
  rS := Copy(rS, 1, Length(rS) - 4);
  if rS <> '' then
  begin
    if Fl.No then
      rS := 'not (' + rS + ')';
    Result := '(' + rS + ')';
  end;
  SL.Free;
end;

function GetWhereClause(SS: TSession; aFl: TRpField): String;
var
  S, Bg, Ed, FlNm, W, Tmp, AbsValue: String;
  p: Integer;
  i: Integer;
  SL: TStringList;
  C: TdxComponent;
  Tp: TRpFieldType;
begin
  Result := '';
  SL := TStringList.Create;
  SplitStr(aFl.Value, ';', SL);
  FlNm := 'f' + IntToStr(aFl.Id);
  Tp := GetLowField(@aFl)^.Tp;

  W := '';
  if aFl.Nul then W := W + FlNm + ' is null or ';
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    if S = '' then Continue;
    case Tp of
      flText, flFile:
        begin
          S := UnEscapeSemicolon(S);
          W := W + FlNm + ' containing ''' + EscapeSQuotes(S) + ''' or ';
        end;
      flDate:
        begin
          p := Pos(' .. ', S);
          Bg := Copy(S, 1, p - 1);
          Ed := Copy(S, p + 4, 1024);
          Tmp := '';
          if Bg <> '' then
            Tmp := Tmp + FlNm + '>=''' + Bg + ''' and ';
          if Ed <> '' then
            Tmp := Tmp + FlNm + '<=''' + Ed + ''' and ';
          Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
          if Tmp <> '' then
            W := W + '(' + Tmp + ') or ';
        end;
			flTime:
        begin
          p := Pos(' .. ', S);
          Bg := Copy(S, 1, p - 1);
          Ed := Copy(S, p + 4, 1024);
          C := GetRpFieldComponent(SS, aFl, False);
          CheckTime(TdxTimeEdit(C).TimeFormat, Bg, Ed);
          Tmp := '';
          if Bg <> '' then
            Tmp := Tmp + FlNm + '>=''' + Bg + ''' and ';
          if Ed <> '' then
            Tmp := Tmp + FlNm + '<=''' + Ed + ''' and ';
          Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
          if Tmp <> '' then
            W := W + '(' + Tmp + ') or ';
        end;
      flNumber:
        begin
          p := Pos(' .. ', S);
          Bg := Copy(S, 1, p - 1);
          Ed := Copy(S, p + 4, 1024);
          Tmp := '';
          if Bg <> '' then
          begin
            Bg := CheckNumber(Bg);
            AbsValue := StringReplace(Bg, '-', '', []);
            Bg := Bg + '-' + AbsValue + '*2e-12';
            Tmp := Tmp + FlNm + '>=' + Bg + ' and ';
          end;
          if Ed <> '' then
          begin
            Ed := CheckNumber(Ed);
            AbsValue := StringReplace(Ed, '-', '', []);
            Ed := Ed + '+' + AbsValue + '*2e-12';
            Tmp := Tmp + FlNm + '<=' + Ed + ' and ';
          end;
          Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
          if Tmp <> '' then
            W := W + '(' + Tmp + ') or ';
        end;
      flCounter, flRecId:
        begin
          p := Pos(' .. ', S);
          Bg := Copy(S, 1, p - 1);
          Ed := Copy(S, p + 4, 1024);
          Tmp := '';
          if Bg <> '' then
            Tmp := Tmp + FlNm + '>=' + Bg + ' and ';
          if Ed <> '' then
            Tmp := Tmp + FlNm + '<=' + Ed + ' and ';
          Tmp := Copy(Tmp, 1, Length(Tmp) - 5);
          if Tmp <> '' then
            W := W + '(' + Tmp + ') or ';
        end;
      flBool: W := W + FlNm + '=' + S + ' or ';
      flObject:
        begin
          C := GetRpFieldComponent(SS, aFl, True);
          Tmp := SqlSelectIDs(SS, GetSourceTId(C), S);
          if Tmp <> '' then W := W + '''' + Tmp + ''' containing ''\'' || ' + FlNm + ' || ''\'' or '
          else W := W + FlNm + '=' + S + ' or ';
        end;
    end;
  end;
  SL.Free;
  W := Copy(W, 1, Length(W) - 4);
  if W <> '' then
  begin
    if aFl.No then W := 'not (' + W + ')';
    Result := '(' + W + ')';
  end;
end;

// Устанавливает в полях флаг "Не выбрано ни одного поля для функции Количество".
// В этом случае, если для функции "Количество" не указаны поля, то считается
// количество записей. Если указано хотя бы одно поле, то считаются записи для
// источников, в которых поле указано.
procedure SetAllZeros(RD: TReportData);
var
  i: Integer;

  function IsAllZeros(idx: Integer): Boolean;
  var
    j: Integer;
  begin
    Result := True;
    for j := 0 to RD.Sources.Count - 1 do
      if RD.Sources[j]^.Fields[idx]^.Zero = False then Exit(False);
  end;

  procedure _SetAllZeros(idx: Integer);
  var
    j: Integer;
  begin
    for j := 0 to RD.Sources.Count - 1 do
      RD.Sources[j]^.Fields[idx]^.AllZeros := True;
  end;

begin
  for i := 0 to RD.Sources[0]^.Fields.Count - 1 do
    if IsAllZeros(i) then _SetAllZeros(i);
end;

function CalcFieldExistsInSort(RD: TReportData): Boolean;
var
  i: Integer;
  S: String;
begin
  Result := False;
  for i := 0 to RD.Grid.SortCols.Count - 1 do
  begin
    S := UpperCase(Copy(RD.Grid.SortCols[i].Col.FieldNameDS, 1, 2));
    if S = 'CF' then Exit(True);
  end;
end;

function InnerSqlReportSelect(RD: TReportData; ARecordSet: TSsRecordSet): String;
var
  Sr: TRpSource;
  FStr, GStr, FromStr, Nm, Srt, S, Hav, Wh: String;
  i: Integer;
  Fl: TRpField;
  Sim, NeedGroup: Boolean;
  Col: TRpGridSortData;
  //CF: TRpCalcField;
begin
  Result := '';
  if RD.Sources.Count = 0 then Exit;
  SetAllZeros(RD);

  Sr := RD.Sources[0]^;

  FStr := '';
  GStr := '';
  Hav := '';
  Wh := '';
  NeedGroup := False;
  for i := 0 to Sr.Fields.Count - 1 do
  begin
    Fl := RD.FindField(Sr.Fields[i]^.Id)^;
    if Fl.Tp = flNone then Continue;
    Nm := 'f' + IntToStr(Fl.Id);

    if Fl.Param then
    begin
      if Fl.Func <> tfNone then
      begin
        S := GetHavingClause(ARecordSet.Session, Fl);
        if S <> '' then
          Hav := Hav + S + ' and '
      end
      else
      begin
        S := GetWhereClause(ARecordSet.Session, Fl);
        if S <> '' then
          Wh := Wh + S + ' and '
      end;
    end;

    if not Fl.Visible then Continue;

    if i = RD.DateField then
    begin
      case RD.DateDetail of
        ddDay: FStr := FStr + Nm + ',';
        ddWeek: FStr := FStr + Format('(extract (year from %0:s) || ''.'' || trim(iif(extract (week from %0:s) < 10, ''0'', '''') || (extract (week from %0:s)))) as %0:s,', [Nm]);
        ddMonth: FStr := FStr + Format('(extract (year from %0:s) || ''.'' || trim(iif(extract (month from %0:s) < 10, ''0'', '''') || (extract (month from %0:s)))) as %0:s,', [Nm]);
        ddQuart: FStr := FStr + Format('(extract (year from %0:s)  || ''.'' || ROUND((CAST(EXTRACT(MONTH FROM %0:s) AS FLOAT)/3 + 0.3))) as %0:s,', [Nm]);
        ddHalfYear: FStr := FStr + Format('(extract (year from %0:s)  || ''.'' ||iif(extract(month from %0:s) <= 6, 1, 2)) as %0:s,', [Nm]);
        ddYear: FStr := FStr + Format('(extract (year from %0:s)) as %0:s,', [Nm]);
      end;
      GStr := GStr + Nm + ',';
      NeedGroup := True;
    end
    else if Fl.Func = tfNone then
    begin
      FStr := FStr + Nm + ',';
      GStr := GStr + Nm + ',';
    end
    else
    begin
      FStr := FStr + GetFuncSql(Fl) + ' as ' + Nm + ',';
      NeedGroup := True;
    end;
  end;
  Wh := Copy(Wh, 1, Length(Wh) - 5);

  Hav := Copy(Hav, 1, Length(Hav) - 5);

  FromStr := '';
  for i := 0 to RD.Sources.Count - 1 do
  begin
    try
      FromStr := FromStr + SqlSourceSelect2(ARecordSet, RD.Sources[i]^);
    except
      on E: EFilterParserError do
        raise ESourceFilterError.Create(E.Message, RD.Sources[i]^.Filter, i+1, E.Position);
    end;
    if i < RD.Sources.Count - 1 then FromStr := FromStr + ' union all ';
  end;

  Sim := RD.IsSimple;
  if Sim then
  begin
    S := ' ' + TableStr(RD.Sources[0]^.Id) + '.id as id';
    if RD.Sources[0]^.TId > 0 then S := S + ',' + TableStr(RD.Sources[0]^.TId) + '.id as tid';
    if FStr <> '' then S := S + ',';
    Insert(S, FromStr, 7);
    if RD.Sources[0]^.TId > 0 then FStr := 'tid,' + FStr;
    FStr := 'id,' + FStr;
  end;
  // Добавляем поля-пустышки для вычисляемых полей
  (*for i := 0 to RD.CalcFields.Count - 1 do
  begin
    CF := RD.CalcFields[i]^;
    case CF.Tp of
      flNumber: S := '0.0';
      flDate: S := 'CURRENT_DATE';
      flTime: S := 'CURRENT_TIME';
      else S := Format('(CAST('' '' AS VARCHAR(%d)))', [CF.Size])
      //else S := '(''' + DupeString(' ', CF.Size) + ''')';
    end;
    //FStr := FStr + '(''' + DupeString(' ', 200) + ''') as cf' +
    FStr := FStr + S + ' as cf' + IntToStr(CF.Id) + ',';
  end; *)
  //
  FStr := Copy(FStr, 1, Length(FStr) - 1);
  GStr := Copy(GStr, 1, Length(GStr) - 1);
  Result := 'select ';
  if RD.FirstRecordCount > 0 then
    Result := Result + 'first ' + IntToStr(RD.FirstRecordCount) + ' ';
  Result := Result + FStr + ' from (' + FromStr + ') ';
  if Wh <> '' then
    Result := Result + ' where ' + Wh;
  if (not Sim) and NeedGroup then
  begin
    if GStr <> '' then
      Result := Result + ' group by ' + GStr;
    if Hav <> '' then
      Result := Result + ' having ' + Hav;
  end;

  // Сортировка. Пропускаем вычисляемые поля.
  Srt := '';
  for i := 0 to RD.Grid.SortCols.Count - 1 do
  begin
    Col := RD.Grid.SortCols[i];
    if UpperCase(Copy(Col.Col.FieldNameDS, 1, 2)) = 'CF' then Continue;
    Srt := Srt + Col.Col.FieldNameDS;
    if Col.Desc then Srt := Srt + ' desc';
    Srt := Srt + ',';
  end;
  Srt := Copy(Srt, 1, Length(Srt) - 1);
  if Srt <> '' then
    Result := Result + ' order by ' + Srt;
  //DebugStr(Result);
end;

function InnerSqlReportSelectSQL(RD: TReportData; ARecordSet: TObject): String;
var
  i, FieldIndex: Integer;
  Col: TRpGridSortData;
  SortOrder: String;
begin
  Result := ParseSQL(RD.SQL, TSsRecordSet(ARecordSet).Session, ARecordSet);
  SortOrder := '';
  for i := 0 to RD.Grid.SortCols.Count - 1 do
  begin
    Col := RD.Grid.SortCols[i];
    FieldIndex := RD.IndexOfNameDS(Col.Col.FieldNameDS);
    // Если есть сортировка по вычисляемому полю, то будем сортировать локально
    if RD.IsCalcField(FieldIndex) then
    begin
      SortOrder := '';
      Break;
    end;
    SortOrder := SortOrder + IntToStr(FieldIndex + 1) +
      IIF(Col.Desc, ' desc', '') + ',';
  end;
  SetLength(SortOrder, Length(SortOrder) - 1);
  if SortOrder <> '' then
    Result := Result + ' order by ' + SortOrder;
end;

function SqlReportSelect(RD: TReportData; ARecordSet: TSsRecordSet): String;
begin
  if not RD.SqlMode then
    Result := InnerSqlReportSelect(RD, ARecordSet)
  else
    Result := InnerSqlReportSelectSQL(RD, ARecordSet);
end;

{function SqlSelCondFilter(SrcFm, Fm: TdxForm; const Cond: String; var JStr: String; AliasSL: TStrings): String;
var
  EB: TExpressionBuilder;
  P: TSQLSourceFilterParser;
begin
  EB := TExpressionBuilder.Create;
  EB.SkipLabels:=True;
  if Fm <> nil then
  begin
    EB.Form := Fm;
    if Fm.ParentForm <> nil then
	    EB.ParentForm := Fm.ParentForm
    else
      EB.ParentForm := Fm;
    EB.DataSet := Fm.DataSet;
  end;
  P := TSQLSourceFilterParser.Create;
  P.ExprBuilder := EB;
  P.Form := SrcFm;
  P.ParentForm := SrcFm;
  P.AliasSL := AliasSL;
  P.JoinStr := JStr;
  try
    Result := P.Parse(Cond);
  finally
    JStr := P.JoinStr;
    P.Free;
    EB.Free;
  end;
end;  }

function SqlSelCondFilter(SrcRS, ARecordSet: TSsRecordSet; const Cond: String;
  var JStr: String; AliasSL: TStrings): String;
var
  EB: TExpressionBuilder;
  P: TSQLSourceFilterParser;
  DummyRS: TSsRecordSet;
begin
  DummyRS := nil;
  EB := TExpressionBuilder.Create;
  if ARecordSet = nil then
  begin
    DummyRS := TSsRecordSet.Create(SrcRS.Session, nil);
    EB.RecordSet := DummyRS;
  end
  else
    EB.RecordSet := ARecordSet;
  EB.SkipLabels:=True;
  P := TSQLSourceFilterParser.Create;
  P.ExprBuilder := EB;
  P.Form := SrcRS.Form;
  P.ParentForm := SrcRS.Form;
  P.Session := SrcRS.Session;
  P.AliasSL := AliasSL;
  P.JoinStr := JStr;
  try
    Result := P.Parse(Cond);
  finally
    JStr := P.JoinStr;
    P.Free;
    EB.Free;
    FreeAndNil(DummyRS);
  end;
end;

function SqlUpdateStatementOnlyChanged(RS: TSsRecordSet): String;
var
  i: Integer;
  C: TComponent;
  FNm: String;
  F: TField;
  AllFields: Boolean;
begin
  // В таблицах строки могут быть поменяны местами. В этом случае обновлять
  // надо все поля.
  {AllFields := (Fm.PId > 0) and (Fm.DataSet['id'] <> Fm.DataSet['oldid']);
  Result := 'update ' + TableStr(Fm.Id) + ' set id=:id,';
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    FNm := FieldStr(C);

    if not IsField(C) then Continue
    else if C is TdxDBImage then
    begin
      if AllFields or TdxDBImage(C).WasChanged then
        Result := Result + FNm + '=:' + FNm + ',' +
          FNm + 'src=:' + FNm + 'src,' + FNm + 'dest=:' +
          FNm + 'dest,' + FNm + 'thumb=:' + FNm + 'thumb,';
    end
    else if C is TdxFile then
    begin
      if AllFields or TdxFile(C).WasChanged then
        Result := Result + FNm + '=:' + FNm + ',' +
          FNm + 'src=:' + FNm + 'src,' + FNm + 'dest=:' +
          FNm + 'dest,' + FNm + 'd=:' + FNm + 'd,';
    end
    else
    begin
      F := Fm.DataSet.FieldByName(FNm);
      if AllFields or (F.Value <> F.OldValue) then
        Result := Result + FNm + '=:' + FNm + ',';
    end;
  end;
  Result := Copy(Result, 1, Length(Result) - 1) + ' where id=:id'; }
end;

function DateFormatToSql(const FlNm: String): String;
const
  _lpad = 'lpad(extract(%s from %s), %d, ''0'') || ''%s'' || ';
var
  Fmt, PartDate: String;
  i, n: Integer;
  DateSep: Char;
begin
  Result := '';
  Fmt := AnsiLowerCase(DefaultFormatSettings.ShortDateFormat);
  DateSep := DefaultFormatSettings.DateSeparator;
  PartDate := '';
  for i := 1 to Length(Fmt) do
  begin
    case Fmt[i] of
      'd': begin PartDate := 'day'; n := 2; end;
      'm': begin PartDate := 'month'; n := 2; end;
      'y': begin PartDate := 'year'; n := 4; end;
      else
      begin
        if PartDate <> '' then
        	Result := Result + Format(_lpad, [PartDate, FlNm, n, DateSep]);
        PartDate := '';
        Continue;
      end;
    end;
  end;
  if PartDate <> '' then
  	Result := Result + Format(_lpad, [PartDate, FlNm, n, DateSep]);

  Result := Copy(Result, 1, Length(Result) - 11);
end;

function IsValidCharsSql(const S: String; Sep: Char): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(S) do
  begin
    if not (S[i] in ['0'..'9', Sep]) then
    	Exit(False);
  end;
end;

function GetCastByFieldId(Fm: TdxForm; FId: Integer; const S: String; LCbxFields: TStrings): String;
var
  C: TdxField;
  FlNm: String;
  i: Integer;
begin
  Result := '';
  FlNm := TableStr(Fm.Id) + '.' + FieldStr(FId);
  C := Fm.FindField(FId);
  if C is TdxCalcEdit then
  begin
    if IsValidCharsSql(S, DefaultFormatSettings.DecimalSeparator) then
    	Result := 'substring(' + FlNm + ' from 1 for position(''.'',' + FlNm + ')+' +
      	IntToStr(TdxCalcEdit(C).Precission) + ') containing ''' +
        StringReplace(S, DefaultFormatSettings.DecimalSeparator, '.', []) + ''''
  end
  else if C is TdxDateEdit then
  begin
    if IsValidCharsSql(S, DefaultFormatSettings.DateSeparator) then
	    Result := DateFormatToSql(FlNm) + ' containing ''' + S + ''''
  end
  else if C is TdxTimeEdit then
  begin
    if IsValidCharsSql(S, DefaultFormatSettings.TimeSeparator) then
    	Result := FlNm + ' containing ''' + S + ''''
  end
  else if (C is TdxCheckBox) or (C is TdxCounter) then
  begin
    if IsValidCharsSql(S, '0') then
    	Result := FlNm + ' containing ''' + S + ''''
  end
  else if C is TdxLookupComboBox then
  begin
    i := LCbxFields.IndexOfObject(C);
    if i >= 0 then
    	Result := LCbxFields[i] + ' containing ''' + S + ''''
  end
  else if C is TdxRecordId then
  begin
    if IsValidCharsSql(S, '0') then
    	Result := TableStr(Fm.Id) + '.id containing ''' + S + ''''
  end
  else
	  Result := FlNm + ' containing ''' + S + '''';

  if Result = '' then Result := '1=0';
end;

function SqlLCbxSelect(ARecordSet: TSsRecordSet; LCbx: TdxLookupComboBox;
  const Fragments: String; First, Skip: Integer): String;
var
  TId, FId, i, j: Integer;
  Grps, Wh, JStr, ST, STa, SF, F, T, S, Flt, FlNm, AliasNm: String;
  SrcFm: TdxForm;
  LF: TLCbxListField;
  C: TdxField;
  AliasSL, SL, LCbxFields: TStringList;
begin
  Result := '';
  TId := LCbx.SourceTId;
  FId := LCbx.SourceFId;
  if (TId = 0) or (FId = 0) then Exit;
  T := TableStr(TId);

  SrcFm := ARecordSet.Session.FormMan.FindForm(TId);
  AliasSL := TStringList.Create;
  LCbxFields := TStringList.Create;

  JStr := '';
  Result := 'select first ' + IntToStr(First);
  if Skip > 0 then Result := Result + ' skip ' + IntToStr(Skip);
  if GetListSourceField(ARecordSet.Session, LCbx) is TdxRecordId then FlNm := 'id'
  else FlNm := FieldStr(FId);
  Result := Result + ' ' + T + '.id,' + T + '.' + FlNm;

  for i := 0 to LCbx.ListFields.Count - 1 do
  begin
    LF := LCbx.ListFields[i];
    F := FieldStr(LF.FieldId);
    C := SrcFm.FindField(LF.FieldId);
    if C is TdxLookupComboBox then
    	with TdxLookupComboBox(C) do
	    begin
	      if (SourceTId > 0) and (SourceFId > 0) then
        begin
          STa := GetAliasNameCbx(C);
          AliasSL.Add(STa);
          AliasNm := AliasStr(AliasSL, STa);

          ST := SqlSelectGroups(ARecordSet.Session, SourceTId, True);
          if ST = '' then ST := TableStr(SourceTId)
          else ST := '(' + ST + ')';
          SF := FieldStr(SourceFId);
          Result := Result + ',' + AliasNm + '.' + SF + ' as ' + F;// + 'l,';
          JStr := JStr + GetJoinType(ARecordSet.Session, LCbx, C) + ST + ' ' + AliasNm + ' on ' + T + '.' + F + '=' +
            AliasNm + '.id';
          LCbxFields.AddObject(AliasNm + '.' + SF, C);
        end
        else
          Result := Result + ',null as ' + F; // заглушка
      end
    else
    begin
      if C is TdxRecordId then F := 'id';
	  	Result := Result + ',' + T + '.' + F;
    end;
  end;

	Wh := SqlLookupFilter(ARecordSet, LCbx, AliasSL, JStr);

  if Trim(Fragments) <> '' then
  begin

    Flt := '';
    SL := TStringList.Create;
    SplitStr(Fragments, ' ', SL);
    for i := 0 to SL.Count - 1 do
    begin
      S := EscapeSQuotes(SL[i]);
      if S = '' then Continue;

      Flt := Flt + '(' + GetCastByFieldId(SrcFm, FId, S, nil) + ' or ';

      for j := 0 to LCbx.ListFields.Count - 1 do
      begin
        LF := LCbx.ListFields[j];
        if not LF.Searchable then Continue;

        Flt := Flt + GetCastByFieldId(SrcFm, LF.FieldId, S, LCbxFields) + ' or ';
      end;

      Flt := Copy(Flt, 1, Length(Flt) - 4) + ') and ';
    end;
    Flt := Copy(Flt, 1, Length(Flt) - 5);
    SL.Free;

    if Wh <> '' then Wh := '(' + Wh + ') and ';
    Wh := Wh + '(' + Flt + ')';
  end;

  Grps := SqlSelectGroups(ARecordSet.Session, TId, True);
  if Grps <> '' then
  	Result := Result + ' from (' + Grps + ') ' + T
  else
    Result := Result + ' from ' + T;

  Result := Result + JStr;

  if Wh <> '' then Result := Result + ' where ' + Wh;

  Result := Result + ' order by 2';

  AliasSL.Free;
  LCbxFields.Free;
  //DebugStr(Result);
end;

function SqlComboBoxSelect(ARecordSet: TSsRecordSet; Cbx: TdxComboBox;
  const Fragments: String; First, Skip: Integer): String;
var
  TId, FId, i: Integer;
  Grps, Wh, JStr, T, S, Flt, FlNm: String;
  SrcFm: TdxForm;
  AliasSL, SL: TStringList;
begin
  Result := '';
  TId := Cbx.SourceTId;
  FId := Cbx.SourceFId;
  if (TId = 0) or (FId = 0) then Exit;
  T := TableStr(TId);

  SrcFm := ARecordSet.Session.FormMan.FindForm(TId);
  AliasSL := TStringList.Create;

  JStr := '';
  Result := 'select first ' + IntToStr(First);
  if Skip > 0 then Result := Result + ' skip ' + IntToStr(Skip);
  if GetListSourceField(ARecordSet.Session, Cbx) is TdxRecordId then
    FlNm := 'id'
  else
    FlNm := FieldStr(FId);
  Result := Result + ' ' + T + '.id,' + T + '.' + FlNm;

	Wh := SqlLookupFilter(ARecordSet, Cbx, AliasSL, JStr);

  if Trim(Fragments) <> '' then
  begin

    Flt := '';
    SL := TStringList.Create;
    SplitStr(Fragments, ' ', SL);
    for i := 0 to SL.Count - 1 do
    begin
      S := EscapeSQuotes(SL[i]);
      if S = '' then Continue;

      Flt := Flt + GetCastByFieldId(SrcFm, FId, S, nil) + ' or ';
    end;
    Flt := Copy(Flt, 1, Length(Flt) - 4);
    SL.Free;

    if Wh <> '' then Wh := '(' + Wh + ') and ';
    Wh := Wh + '(' + Flt + ')';
  end;

  Grps := SqlSelectGroups(ARecordSet.Session, TId, True);
  if Grps <> '' then
  	Result := Result + ' from (' + Grps + ') ' + T
  else
    Result := Result + ' from ' + T;

  Result := Result + JStr;

  if Wh <> '' then Result := Result + ' where ' + Wh;

  Result := Result + ' order by 2';

  AliasSL.Free;
end;

function SqlFilterLCbxSelect(SS: TSession; LCbx: TdxCustomComboBox;
  const Fragments: String; First, Skip: Integer): String;
var
  TId, FId, i: Integer;
  Grps, Wh, T, S, Flt, FlNm: String;
  SrcFm: TdxForm;
  SL: TStringList;
begin
  Result := '';
  TId := LCbx.SourceTId;
  FId := LCbx.SourceFId;
  if (TId = 0) or (FId = 0) then Exit;
  T := TableStr(TId);

  SrcFm := SS.FormMan.FindForm(TId);

  Result := 'select first ' + IntToStr(First);
  if Skip > 0 then Result := Result + ' skip ' + IntToStr(Skip);

  if GetListSourceField(SS, LCbx) is TdxRecordId then
    FlNm := 'id'
  else
    FlNm := FieldStr(FId);
  Result := Result + ' ' + T + '.id,' + T + '.' + FlNm;

	Wh := '';

  if Trim(Fragments) <> '' then
  begin

    Flt := '';
    SL := TStringList.Create;
    SplitStr(Fragments, ' ', SL);
    for i := 0 to SL.Count - 1 do
    begin
      S := EscapeSQuotes(SL[i]);
      if S = '' then Continue;

      Flt := Flt + GetCastByFieldId(SrcFm, FId, S, nil) + ' and ';
    end;
    Flt := Copy(Flt, 1, Length(Flt) - 5);
    SL.Free;

    Wh := Flt;
  end;

  Grps := SqlSelectGroups(SS, TId, True);
  if Grps <> '' then
  	Result := Result + ' from (' + Grps + ') ' + T
  else
    Result := Result + ' from ' + T;

  if Wh <> '' then Result := Result + ' where ' + Wh;

  Result := Result + ' order by 2';
end;

function SqlLookupSelect(SS: TSession; C: TdxField; RecId: Integer): String;
var
  TId, FId: Integer;
  Grps, FlNm: String;
begin
  Result := '';
  TId := GetSourceTId(C);
  FId := GetSourceFId(C);
  if (TId = 0) or (FId = 0) then Exit;

  if GetListSourceField(SS, TdxCustomComboBox(C)) is TdxRecordId then
    FlNm := 'id'
  else
    FlNm := FieldStr(FId);

  Result := 'select ' + TableStr(TId) + '.id,' +
  	TableStr(TId) + '.' + FlNm + ' from ';

  Grps := SqlSelectGroups(SS, TId, True);
  if Grps <> '' then
  	Result := Result + '(' + Grps + ') ' + TableStr(TId)
  else
    Result := Result + TableStr(TId);

	Result := Result + ' where id=' + IntToStr(RecId);
end;

function SqlSimpleSelectStatement(Fm: TdxForm; RecId: Integer): String;
var
  i: Integer;
  C: TdxComponent;
  S, FNm: String;
begin
  S := 'select ';
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not (C is TdxField) or (C is TdxObjectField) or (C is TdxRecordId) then Continue;
    FNm := FieldStr(TdxField(C).Id);
    if C is TdxFile then
      S := S + FNm + 'src,' + FNm + 'dest,' + FNm + 'd,'
    else if C is TdxDBImage then
      S := S + FNm + 'src,' + FNm + 'dest,'
    else
      S := S + FNm + ',';
  end;
  S := Copy(S, 1, Length(S) - 1);
  Result := S + ' from ' + TableStr(Fm.Id) + ' where id=' + IntToStr(RecId);
end;

function AliasStr(AliasSL: TStrings; const AliasName: String): String;
var
  i: Integer;
begin
  i := AliasSL.IndexOf(AliasName);
  if i < 0 then raise Exception.Create('AliasStr: ' + AliasName + ' is -1');
  Result := 'a' + IntToStr(i);
end;

end.

