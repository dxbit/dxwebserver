{-------------------------------------------------------------------------------

    Copyright 2016-2025 Pavel Duborkin ( mydataexpress@mail.ru )

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

unit ExprFuncs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dxctrls, Db, SqlDb, strconsts, dxreports, dxtypes, mytypes;

//type
//  EGetQueryDataError = class(Exception);

function GetRecordSet(AMainRecordSet: TSsRecordSet; const FormName,
  FieldName: String; out ARecordSet: TSsRecordSet; out aF: TField): Boolean;
function CalcAggFunc(ARecordSet: TSsRecordSet; const FormName, FieldName,
  Cond: String; Func: TRpTotalFunc): Variant;
function FirstLetterUpper(const S: String): String;
function MyFrac(E: Double; Digits: Byte): Int64;
function GetWeekName(D: TDateTime; Brief: Boolean): String;
function GetMonthName(D: TDateTime; Brief: Boolean): String;
function MyPower(V1, V2: Double): Double;
function GetRecNo(ARecordSet: TSsRecordSet; const aName: String): Variant;
function MyRoundToStr(V: Double; Digits: Integer): String;
function MyHoursBetween(D1, T1, D2, T2: TDateTime): Int64;
function MyMinutesBetween(D1, T1, D2, T2: TDateTime): Int64;
function MySecondsBetween(D1, T1, D2, T2: TDateTime): Int64;
function AddHour(ARecordSet: TSsRecordSet; D, T: TDateTime; N: Integer;
  const DateField, Func: String): TDateTime;
function MyIndexOf(i: Integer; const S: String): String;
function FmtDate(DT: TDateTime): String;
function MergeRows(ARecordSet: TSsRecordSet; const FormName, FieldName,
  Delim: String): Variant;
function MergeRowsEx(ARecordSet: TSsRecordSet; const FormName, Expr,
  Delim: String): String;
function GetRecId(ARecordSet: TSsRecordSet; const aName: String): Variant;
function GetObjId(SS: TSession; const FormName, FieldName, FieldValue: String): Variant;
//function GetUser(SS: TSession): String;
//function GetRole(SS: TSession): String;
function DBGet(ARecordSet: TSsRecordSet; const FormName,
  FieldName, Filter: String; Func: TRpTotalFunc): Variant;
function DBGetById(SS: TSession; const FormName, FieldName: String; aId: Integer): Variant;
function GetMaxV(Vals: array of Variant): Variant;
function GetMinV(Vals: array of Variant): Variant;
function SetVar(SS: TSession; const aName: String; aValue: Variant): Variant;
function GetVar(SS: TSession; const aName: String): Variant;
function CalcPeriod(D1, D2: TDateTime; Detail: Integer; Age: Boolean): String;
//function CalcPeriodRu(D1, D2: TDateTime; Detail: Integer; Age: Boolean): String;
//function SumPeriod(aForm: TdxForm; const FormName, Date1, Date2: String; Detail: Integer): String;
function Block(Vals: array of Variant): Variant;
function VarExists(SS: TSession; const aName: String): Integer;
function PathLength(const Path: String): Integer;
function ExtractPath(const Path: String; aStart, aLen: Integer): String;
function IsNewRec(DS: TDataSet): Integer;
function GetFieldValue(ARecordSet: TSsRecordSet; const FormName, FieldName: String): Variant;
function GetOldValue(ARecordSet: TSsRecordSet; const FieldName: String): Variant;
function BeginYear(D: TDateTime): TDateTime;
function BeginMonth(D: TDateTime): TDateTime;
function BeginWeek(D: TDateTime): TDateTime;
function EndYear(D: TDateTime): TDateTime;
function EndMonth(D: TDateTime): TDateTime;
function EndWeek(D: TDateTime): TDateTime;
function BeginQuarter(D: TDateTime): TDateTime;
function EndQuarter(D: TDateTime): TDateTime;
function QuarterOf(D: TDateTime): Word;
function DBUnique(ARecordSet: TSsRecordSet; const Fields: String): Integer;
function IsUniqueRecords(ARecordSet: TSsRecordSet; const FormName, Fields: String): Integer;
function MsgBox(const Caption, Msg: String): Variant;
function YesNoBox(const Caption, Msg: String): Integer;
function YesNoCancelBox(const Caption, Msg: String): Integer;
function SetField(ARecordSet: TSsRecordSet; const aFieldName: String; Value: Variant): Variant;
function SetLabel(ARecordSet: TSsRecordSet; const aFieldName: String; Value: Variant): Variant;
function IsEditRec(ARecordSet: TSsRecordSet): Integer;
function IsModifiedRec(ARecordSet: TSsRecordSet): Integer;
function Concat(Vals: array of Variant): String;
//function Fmt(Args: array of Variant): String;
function FNumber(V: Double; Digits: Integer): String;
function TextFormat(const S: String; ARecordSet: TSsRecordSet): String;
function GetTimeStamp(D, T: Variant): Variant;
function CaseOf(const AValue, AItems: String): String;
function GetTypedText(SS: TSession; AIndex: Integer): Variant;
procedure SetTypedText(SS: TSession; S: String);
function IsWebServer: Boolean;

//var
//  VarList: TVarList;

implementation

uses
  formmanager, LazUtf8, sqlgen, dbengine,
  apputils, dateutils, Math, expressions, Variants, StrUtils,
  dxusers;

function GetRecordSet(AMainRecordSet: TSsRecordSet; const FormName,
  FieldName: String; out ARecordSet: TSsRecordSet; out aF: TField): Boolean;
var
  RS: TSsRecordSet;
  Fl: TdxField;
  Col: TRpGridColumn;
begin
  RS := AMainRecordSet.FindRecordSetByName(FormName);
  if (RS = nil) and (AMainRecordSet.QGrid <> nil) then
    RS := AMainRecordSet.Parent.FindRecordSetByName(FormName);
  if RS = nil then raise Exception.Create(Format(rsFormQryNotFound, [FormName]))
  else if (RS.RD <> nil) and RS.QGrid.ManualRefresh and
    (rstRecalculate in RS.GetTopParent.RecordSetState) {not RS.DataSet.Active} then Exit(False);

  Fl := nil; Col := nil;
  if RS.Form <> nil then
  begin
    if FieldName <> '' then
    begin
      Fl := RS.Form.FindFieldByName(FieldName);
      if Fl = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
    end;
    if (RS.Form.PId > 0) and not RS.Open then raise Exception.CreateFmt(rsCouldNotOpenFormDataSet, [
      RS.Form.FormCaption]);
    if Fl <> nil then
      aF := RS.DataSet.FieldByName(FieldStr(Fl.Id));
  end
  else
  begin
    if FieldName <> '' then
    begin
      Col := RS.RD.Grid.FindColumnByFieldName(FieldName);
      if Col = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
    end;
    if not RS.Open then raise Exception.CreateFmt(rsCouldNotOpenQueryDataSet,
      [RS.RD.Name]);
    if Col <> nil then
      aF := RS.DataSet.FieldByName(Col.FieldNameDS);
  end;
  ARecordSet := RS;
  Result := True;
end;

function CalcAggFunc(ARecordSet: TSsRecordSet; const FormName, FieldName,
  Cond: String; Func: TRpTotalFunc): Variant;
var
  DS: TDataSet;
  F: TField;
  E: TExpression;
  N: Integer;
  Sum: Double;
  Mn, Mx, V: Variant;
  FirstValue: Boolean;
  RS: TSsRecordSet;
  B: TBookMark;
begin
  if (FieldName = '') and (Func <> tfCount) then raise Exception.Create(rsFieldNameEmpty);
  Result := Null;
  E := nil;

  if not GetRecordSet(ARecordSet, FormName, FieldName, RS, F) then
  begin
    if Func in [tfCount, tfSum, tfAvg] then Result := 0;
    Exit;
  end;

  if Cond <> '' then
    with TExpressionBuilder.Create do
    try
      RecordSet := RS;
      SkipLabels:=True;
      E := Build(Cond);
    finally
      Free;
    end;

  DS := RS.DataSet;
  RS.DisableScrollEvents;
  B := DS.GetBookmark;
  DS.First;
  FirstValue := True;
  N := 0; Sum := 0;
  Mx := Null; Mn := Null;
  try
    while not DS.EOF do
    begin
      if E <> nil then
      begin
        V := E.Calc;
        if VarIsBool(V) and (V = False) then
        begin
          DS.Next;
          Continue;
        end;
      end;
      if F <> nil then
      begin
        if Func in [tfMax, tfMin] then
        begin
          if FirstValue then
          begin
            if not F.IsNull then
            begin
	            Mx := F.Value;
  	          Mn := Mx;
    	        FirstValue := False;
            end;
          end
          else if not F.IsNull then
          begin
            if F.Value > Mx then Mx := F.Value;
            if F.Value < Mn then Mn := F.Value;
          end;
        end
        else if Func in [tfSum, tfAvg] then
        begin
          if not F.IsNull then
          begin
	          Sum := Sum + F.AsFloat;
            Inc(N);
          end;
        end
        // Функция TAKE
        else if Func = tfNone then
        begin
          Result := F.Value;
          Break;
        end;
      end;
      if Func = tfCount then Inc(N);
      DS.Next;
    end;
  finally
    FreeAndNil(E);
    DS.GotoBookmark(B);
    DS.FreeBookmark(B);
    RS.EnableScrollEvents;
  end;

  case Func of
    tfSum: Result := Sum;
    tfAvg:
    	if N > 0 then
    		Result := Sum / N
      else
      	Result := 0;
    tfMax: Result := Mx;
    tfMin: Result := Mn;
    tfCount: Result := N;
  end;
end;

function FirstLetterUpper(const S: String): String;
var
  L: String;
begin
  Result := Utf8LowerCase(S);
  if Result = '' then Exit;
  L := Utf8UpperCase(Utf8Copy(S, 1, 1));
  Utf8Delete(Result, 1, 1);
  Result := L + Result;
end;

function MyFrac(E: Double; Digits: Byte): Int64;
var
  F: Double;
  i: Integer;
begin
  F := Frac(E);
  for i := 1 to Digits do
    F := F * 10;
  Result := Round(F);
end;

function GetWeekName(D: TDateTime; Brief: Boolean): String;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    if Brief then
      SplitStr(rsWeekNamesBrief, ' ', SL)
    else
      SplitStr(rsWeekNames, ' ', SL);
    Result := SL[DayOfTheWeek(D) - 1];
  finally
    SL.Free;
  end;
end;

function GetMonthName(D: TDateTime; Brief: Boolean): String;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    if Brief then
      SplitStr(rsMonthNamesBrief, ' ', SL)
    else
      SplitStr(rsMonthNames, ' ', SL);
    Result := SL[MonthOf(D) - 1];
  finally
    SL.Free;
  end;
end;

function MyPower(V1, V2: Double): Double;
begin
  Result := Power(V1, V2);
end;

{function GetRecNo(SS: TSession; aForm: TdxForm; const FormName: String
  ): Variant;
var
  Fm: TdxForm;
  DS: TDataSet;
  F: TField;
  RD: TReportData;
begin
  GetFormData(SS, aForm, SS.RecId, FormName, '', Fm, DS, F);
  if Fm = nil then
    GetQueryData(SS, aForm, nil, SS.RecId, FormName, '', RD, DS, F);
  if DS = nil then raise Exception.CreateFmt(rsFormQryNotFound, [FormName]);
  Result := DS.RecNo;
end; }
function GetRecNo(ARecordSet: TSsRecordSet; const aName: String): Variant;
var
  RS: TSsRecordSet;
  F: TField;
begin
  if GetRecordSet(ARecordSet, AName, '', RS, F) then
    Result := RS.DataSet.RecNo
  else
    Result := 0;
end;

function MyRoundToStr(V: Double; Digits: Integer): String;
var
  S: String;
  d: Double;
begin
  d := MathRound(V, Digits);
  S := '0';
  if Digits > 0 then
    S := '0.' + DupeString('0', Digits);
  Result := FormatFloat(S, d);
end;

function MergeDateTime(D1, T1: TDateTime): TDateTime;
begin
  {Result := EncodeDateTime(YearOf(D1), MonthOf(D1), DayOf(D1), HourOf(T1), MinuteOf(T1),
    SecondOf(T1), 0);}
  Result := DateOf(D1) + TimeOf(T1);
end;

procedure SplitDateTime(DT: TDateTime; var D1, T1: TDateTime);
var
  Y, M, D, H, Mn, S, Ms: Word;
begin
  DecodeDateTime(DT, Y, M, D, H, Mn, S, Ms);
  D1 := EncodeDate(Y, M, D);
  T1 := EncodeTime(H, Mn, S, Ms);
end;

// Оказывается HoursBetween некорректно вычисляет результат.
// Разница между 01.03.2017 12:00:00 и 01.03.2017 13:00:00 будет 0,
// хотя должно быть 1. Все дело в миллисекундах. Чтобы избежать
// ошибки, надо к большему значению добавить миллисекунду.
procedure ShiftDT(D1, T1, D2, T2: TDateTime; out DT1, DT2: TDateTime);
begin
  DT1 := MergeDateTime(D1, T1);
  DT2 := MergeDateTime(D2, T2);
  if DT1 > DT2 then
  begin
    DT1 := RecodeMilliSecond(DT1, 1);
    DT2 := RecodeMilliSecond(DT2, 0);
  end
  else
  begin
    DT1 := RecodeMilliSecond(DT1, 0);
    DT2 := RecodeMilliSecond(DT2, 1);
  end;
  {if MilliSecondOf(T1) = MilliSecondOf(T2) then
  begin
	  if DT1 > DT2 then DT1 := IncMilliSecond(DT1)
  	else if DT2 > DT1 then DT2 := IncMilliSecond(DT2);
  end; }
end;

function MyHoursBetween(D1, T1, D2, T2: TDateTime): Int64;
var
  DT1, DT2: TDateTime;
begin
  ShiftDT(D1, T1, D2, T2, DT1, DT2);
  Result := HoursBetween(DT1, DT2);
end;

function MyMinutesBetween(D1, T1, D2, T2: TDateTime): Int64;
var
  DT1, DT2: TDateTime;
begin
  ShiftDT(D1, T1, D2, T2, DT1, DT2);
  Result := MinutesBetween(DT1, DT2);
end;

function MySecondsBetween(D1, T1, D2, T2: TDateTime): Int64;
var
  DT1, DT2: TDateTime;
begin
  ShiftDT(D1, T1, D2, T2, DT1, DT2);
  Result := SecondsBetween(DT1, DT2);
end;

function AddHour(ARecordSet: TSsRecordSet; D, T: TDateTime; N: Integer;
  const DateField, Func: String): TDateTime;
var
  DE, DT: TDateTime;
  F: TField;
  C: TdxField;
  DS: TDataSet;
begin
  if ARecordSet.Form = nil then raise Exception.Create(rsFormNotAvail);
  DS := ARecordSet.DataSet;
  F := nil;
  if DateField <> '' then
  begin
    C := ARecordSet.Form.FindFieldByName(DateField);
    if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [DateField]);
    if not (C is TdxDateEdit) then raise Exception.CreateFmt(rsFieldNotDate, [DateField]);
    F := DS.FieldByName(FieldStr(C.Id));
  end;
  if Func = 'h' then
    DT := IncHour(MergeDateTime(D, T), N)
  else if Func = 'm' then
    DT := IncMinute(MergeDateTime(D, T), N)
  else if Func = 's' then
    DT := IncSecond(MergeDateTime(D, T), N);
  SplitDateTime(DT, DE, Result);
  if (F <> nil) and (DS.State in [dsInsert, dsEdit]) then
    F.AsDateTime:=DE;
end;

function MyIndexOf(i: Integer; const S: String): String;
var
  SL: TStringList;
begin
  Result := '';
  SL := TStringList.Create;
  SplitStr(S, ';', SL);
  if (i >= 0) and (i < SL.Count) then
    Result := SL[i];
  SL.Free;
end;

{function FmtDateRu(DT: TDateTime): String;
const
  MStr: array[1..12] of String = ('января', 'февраля', 'марта', 'апреля', 'мая',
    'июня', 'июля', 'августа', 'сентября', 'октября', 'ноября', 'декабря');
var
  Y, M, D: word;
begin
  DecodeDate(DT, Y, M, D);
  Result := IntToStr(D) + ' ' + MStr[M] + ' ' + IntToStr(Y);
end;     }

function FmtDate(DT: TDateTime): String;
var
  Y, M, D: word;
  SL: TStringList;
begin
  SL := TStringList.Create;
  SplitStr(rsFmtDateMonth, ' ', SL);
  DecodeDate(DT, Y, M, D);
  Result := IntToStr(D) + ' ' + SL[M-1] + ' ' + IntToStr(Y);
  SL.Free;
end;

function MergeRows(ARecordSet: TSsRecordSet; const FormName, FieldName,
  Delim: String): Variant;
var
  DS: TDataSet;
  F: TField;
  S: String;
  C: TdxField;
  RS: TSsRecordSet;
  B: TBookMark;
begin
  if FieldName = '' then raise Exception.Create(rsFieldNameEmpty);
  Result := '';
  if not GetRecordSet(ARecordSet, FormName, FieldName, RS, F) then Exit;
  DS := RS.DataSet;

  if RS.Form <> nil then
  begin
    C := RS.Form.FindFieldByName(FieldName);
    if C is TdxLookupComboBox then
      F := DS.FieldByName(FieldStr(C.Id) + 'l');
  end;

  S := '';
  B := DS.GetBookmark;
  RS.DisableScrollEvents;

  try
    DS.First;
    while not DS.Eof do
    begin
      S := S + F.AsString + Delim;
      DS.Next;
    end;
  finally
    DS.GotoBookmark(B);
    DS.FreeBookmark(B);
    RS.EnableScrollEvents;
  end;
  SetLength(S, Length(S) - Length(Delim));
  Result := S;
end;

function MergeRowsEx(ARecordSet: TSsRecordSet; const FormName, Expr,
  Delim: String): String;
var
  //Fm: TdxForm;
  DS: TDataSet;
  F: TField;
  S: String;
  E: TExpression;
  V: Variant;
  RS: TSsRecordSet;
  B: TBookMark;
begin
  Result := '';
  if not GetRecordSet(ARecordSet, FormName, '', RS, F) then Exit;

  with TExpressionBuilder.Create do
  try
    RecordSet := RS;
    SkipLabels := True;
    E := Build(Expr);
  finally
    Free;
  end;

  if E = nil then Exit;

  DS := RS.DataSet;
  B := DS.GetBookmark;
  RS.DisableScrollEvents;

  try
    S := '';
    DS.First;
    while not DS.Eof do
    begin
      V := E.Calc;
      if V <> Null then S := S + VarToStr(V) + Delim;
      DS.Next;
    end;
    SetLength(S, Length(S) - Length(Delim));
    Result := S;
  finally
    DS.GotoBookmark(B);
    DS.FreeBookmark(B);
    RS.EnableScrollEvents;
  end;
end;

function GetRecId(ARecordSet: TSsRecordSet; const aName: String): Variant;
var
  RS: TSsRecordSet;
  F: TField;
begin
  if GetRecordSet(ARecordSet, AName, '', RS, F) then
  begin
    Result := RS.DataSet.FieldByName('id').AsInteger;
  end
  else
    Result := 0;
end;

function GetObjId(SS: TSession; const FormName, FieldName, FieldValue: String
  ): Variant;
var
  Fm: TdxForm;
  SQL: String;
  C, CC: TdxField;
begin
  Fm := SS.FormMan.FindFormByName(FormName);
  if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [FormName]);
  C := Fm.FindFieldByName(FieldName);
  if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);

  SQL := 'select id from ' + TableStr(Fm.Id) + ' where ' + FieldStr(C.Id) +
    '=''' + FieldValue + '''';
  if (Fm.ParentField > 0) and (Fm.GetFormParentFieldFieldId = C.Id) then
  begin
    CC := Fm.FindField(Fm.ParentField);
    if GetSourceFId(CC) = C.Id then
    begin
      SQL := SqlSelectGroups(SS, Fm.Id, False, False);
      SQL := SQL + ' where ' + FieldStr(C.Id) + '=''' + FieldValue + '''';
    end;
  end;

  with SS.DBase.OpenDataSet(SQL) do
  begin
    Result := Fields[0].AsInteger;
    Free;
  end;
end;

{function GetUser(SS: TSession): String;
var
  U: TdxUser;
begin
  Result := '';
  U := SS.UserMan.Users.FindUser(SS.UserId);// CurrentUser;
  if U <> nil then Result := U.Name;
end;

function GetRole(SS: TSession): String;
var
  R: TdxRole;
begin
  Result := '';
  R := SS.UserMan.Roles.FindRole(SS.RoleId);
  if R = nil then Exit;
  Result := R.Name;
end; }

function ExtractFieldName(const S: String): String;
var
  p: SizeInt;
begin
  p := Pos('|', S);
  if p > 0 then
    Result := Copy(S, 1, p - 1)
  else
    Result := S;
end;

function DBGet(ARecordSet: TSsRecordSet; const FormName, FieldName,
  Filter: String; Func: TRpTotalFunc): Variant;
var
  Fm, PFm, AFm: TdxForm;
  RD: TReportData;
  pSr: PRpSource;
  pF: PRpField;
  FlNm: String;
  C: TdxField;
  SS: TSession;
  QRS: TSsRecordSet;
begin
  Result := Null;
  SS := ARecordSet.Session;
  Fm := SS.FormMan.FindFormByName(FormName);
  if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [FormName]);

  if Fm.PId > 0 then
    PFm := SS.FormMan.FindForm(Fm.PId)
  else
	  PFm := nil;

  if FieldName <> '' then
  begin
    FlNm := FieldName;
    if FlNm[1] = '!' then
    begin
      Delete(FlNm, 1, 1);
      if PFm <> nil then AFm := PFm
      else AFm := Fm;
    end
    else
    	AFm := Fm;
    if LookupComponent(SS, AFm, FlNm) = nil then
      raise Exception.CreateFmt(rsFieldNotFound, [FlNm]);
    C := Fm.FindFieldByName(ExtractFieldName(FlNm));
  end;
  RD := TReportData.Create;
  RD.Sources.AddSource(pSr);
  if PFm <> nil then
  begin
    pSr^.Id:=PFm.Id;
    pSr^.TId := Fm.Id;
  end
  else
    pSr^.Id := Fm.Id;
  pSr^.Filter:=Filter;
  if FieldName <> '' then
  begin
    pSr^.Fields.AddField(pF);
    SetupRpField(SS, C, FieldName, pF);
    pF^.Func:=Func;
    pF^.Visible := True;
  end
  else if Func = tfCount then
  begin
    pSr^.Fields.AddField(pF);
    pF^.Zero:=True;
    pF^.Tp := flNumber;
    pF^.Func:=Func;
    pF^.Visible := True;
  end;

  QRS := TSsRecordSet.Create(SS, ARecordSet);
  QRS.RD := RD;
  try
    if (Func in [tfSum, tfAvg]) and not (GetLowField(pF)^.Tp in [flNumber{, flCounter, flRecId}]) then
      raise Exception.CreateFmt(rsDBSumFieldNotNumeric, [FieldName]);
    QRS.DataSet.SQL.Text := SqlReportSelect(RD, QRS);
    QRS.DataSet.Open;
    with QRS.DataSet do
    begin
      if RecordCount > 0 then
        Pfm := Pfm;
      if (FieldName <> '') or (Func = tfCount) then
        Result := FieldByName('f0').Value
      else if Fields.Count > 0 then
      begin
        if PFm <> nil then
          Result := Fields[1].Value
        else
          Result := Fields[0].Value;
      end;
      if (Func in [tfSum, tfAvg, tfCount, tfDistCount]) and (Result = Null) then Result := 0;
    end;
  finally
    RD.Free;
    QRS.Free;
  end;
end;

function DBGetById(SS: TSession; const FormName, FieldName: String; aId: Integer
  ): Variant;
var
  Fm: TdxForm;
  SQL: String;
  C: TdxField;
begin
  Result := Null;
  Fm := SS.FormMan.FindFormByName(FormName);
  if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [FormName]);
  C := Fm.FindFieldByName(FieldName);
  if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);

  SQL := 'select ' + FieldStr(C.Id) + ' from ' + TableStr(Fm.Id) + ' where id=' +
    IntToStr(aId);
  with SS.DBase.OpenDataSet(SQL) do
  begin
    Result := Fields[0].Value;
    Free;
  end;
end;

function GetMaxV(Vals: array of Variant): Variant;
var
  i: Integer;
begin
  Result := Null;
  if Length(Vals) = 0 then Exit;
  Result := Vals[0];
  for i := 1 to Length(Vals) - 1 do
    if Vals[i] > Result then Result := Vals[i];
end;

function GetMinV(Vals: array of Variant): Variant;
var
  i: Integer;
begin
  Result := Null;
  if Length(Vals) = 0 then Exit;
  Result := Vals[0];
  for i := 1 to Length(Vals) - 1 do
    if Vals[i] < Result then Result := Vals[i];
end;

function SetVar(SS: TSession; const aName: String; aValue: Variant): Variant;
var
  pV: PVrData;
begin
  pV := SS.VarList.FindVar(aName);
  if pV = nil then
    pV := SS.VarList.AddVar(aName, aValue)
  else
    pV^.Value := aValue;
  Result := aValue;
end;

function GetVar(SS: TSession; const aName: String): Variant;
var
  pV: PVrData;
begin
  Result := Null;
  pV := SS.VarList.FindVar(aName);
  if pV <> nil then
    Result := pV^.Value;
  //else raise Exception.Create(Format(rsVarNotFound, [aName]));
end;

procedure Period(Date1, Date2: TDateTime; var D, M, Y: Word);
begin
  PeriodBetween(Date1, Date2, Y, M, D);
end;

function FormatPeriodRu(Y, M, D, Detail: Integer): String;
const
  YStr: array [0..9] of String = ('лет', 'год', 'года', 'года', 'года', 'лет',
    'лет', 'лет', 'лет', 'лет');
  MStr: array [0..9] of String = ('месяцев', 'месяц', 'месяца', 'месяца', 'месяца',
    'месяцев', 'месяцев', 'месяцев', 'месяцев', 'месяцев');
  DStr: array [0..9] of String = ('дней', 'день', 'дня', 'дня', 'дня', 'дней',
    'дней', 'дней', 'дней', 'дней');

  function Idx(n: Integer): Integer;
  begin
    if n in [5..20] then Result := 0
    else Result := n mod 10;
  end;

begin
  Result := '';
  if Y > 0 then
    Result := IntToStr(Y) + ' ' + YStr[Idx(Y)] + ' ';
  if (M > 0) and (Detail > 1) then
    Result := Result + IntToStr(M) + ' ' + MStr[Idx(M)] + ' ';
  if (D > 0) and (Detail > 2) then
    Result := Result + IntToStr(D) + ' ' + DStr[Idx(D)];
  Result := Trim(Result);
end;

function CalcPeriod(D1, D2: TDateTime; Detail: Integer; Age: Boolean): String;
var
  Y, M, D: Word;
begin
  if not Age then
  begin
    if D1 > D2 then D1 := IncDay(D1, 1)
    else D2 := IncDay(D2, 1);
  end;
  Period(D1, D2, D, M, Y);
  Result := FormatPeriodRu(Y, M, D, Detail);
end;

function Block(Vals: array of Variant): Variant;
begin
  Result := Vals[High(Vals)];
end;

function VarExists(SS: TSession; const aName: String): Integer;
begin
  if SS.VarList.FindVar(aName) <> nil then Result := 1
  else Result := 0;
end;

function PathLength(const Path: String): Integer;
var
  i: Integer;
begin
  if Path = '' then Exit(0);
  Result := 1;
  for i := 0 to Length(Path) - 1 do
    if Path[i] = '\' then Inc(Result);
end;

function ExtractPath(const Path: String; aStart, aLen: Integer): String;
var
  SL: TStringList;
  i, e: Integer;
begin
  Result := '';
  SL := TStringList.Create;
  SplitStr(Path, '\', SL);
  e := aStart + aLen - 1;
  if (aStart >= 1) and (aStart <= SL.Count) and (e >= 1) and (e <= SL.Count) then
    for i := aStart to e do
    begin
      Result := Result + SL[i-1];
      if i < e then Result := Result + '\'
    end;
  SL.Free;
end;

function IsNewRec(DS: TDataSet): Integer;
begin
  Result := 0;
  if DS.State = dsInsert then Result := 1;
end;

function GetFieldValue(ARecordSet: TSsRecordSet; const FormName,
  FieldName: String): Variant;
var
  F: TField;
  RS: TSsRecordSet;
begin
  if FieldName = '' then raise Exception.Create(rsFieldNameEmpty);
  if not GetRecordSet(ARecordSet, FormName, '', RS, F) then Exit(Null);
  if RS.RD <> nil then Result := RS.QGrid.Fields[FieldName]
  else if RS.Form <> nil then Result := RS.Form.Fields[FieldName]
  //else raise Exception.Create(Format(rsQueryNotFound, [FormName]))
end;

function GetOldValue(ARecordSet: TSsRecordSet; const FieldName: String
  ): Variant;
var
  C: TdxField;
begin
  if ARecordSet.Form = nil then raise Exception.Create(rsFormNotAvail);
  Result := Null;
  C := ARecordSet.Form.FindFieldByName(FieldName);
  if C <> nil then
    Result := ARecordSet.DataSet.FieldByName(FieldStr(C.Id)).OldValue
  else
    raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
end;

function BeginYear(D: TDateTime): TDateTime;
begin
  Result := EncodeDate(YearOf(D), 1, 1);
end;

function BeginMonth(D: TDateTime): TDateTime;
begin
  Result := EncodeDate(YearOf(D), MonthOf(D), 1);
end;

function BeginWeek(D: TDateTime): TDateTime;
begin
  Result := IncDay(D, -DayOfTheWeek(D)+1);
end;

function EndYear(D: TDateTime): TDateTime;
begin
  Result := EncodeDate(YearOf(D), 12, 31);
end;

function EndMonth(D: TDateTime): TDateTime;
begin
  Result := IncDay(IncMonth(EncodeDate(YearOf(D), MonthOf(D), 1)), -1);
end;

function EndWeek(D: TDateTime): TDateTime;
begin
  Result := IncDay(D, 7-DayOfTheWeek(D));
end;

function BeginQuarter(D: TDateTime): TDateTime;
var
  y, q: Word;
begin
  y := YearOf(D);
  q := QuarterOf(D);
  case q of
    1: Result := EncodeDate(y, 1, 1);
    2: Result := EncodeDate(y, 4, 1);
    3: Result := EncodeDate(y, 7, 1);
    4: Result := EncodeDate(y, 10, 1);
  end;
end;

function EndQuarter(D: TDateTime): TDateTime;
var
  y, q: Word;
begin
  y := YearOf(D);
  q := QuarterOf(D);
  case q of
    1: Result := EncodeDate(y, 3, 1);
    2: Result := EncodeDate(y, 6, 1);
    3: Result := EncodeDate(y, 9, 1);
    4: Result := EncodeDate(y, 12, 1);
  end;
  Result := IncDay(IncMonth(Result), -1);
end;

function QuarterOf(D: TDateTime): Word;
begin
  case MonthOf(D) of
    1..3: Result := 1;
    4..6: Result := 2;
    7..9: Result := 3;
    10..12: Result := 4;
  end;
end;

function DBUnique(ARecordSet: TSsRecordSet; const Fields: String): Integer;
var
  SL: TStrings;
  S, SQL, Value, AbsValue: String;
  i: Integer;
  F: TField;
  AnyFieldModified: Boolean;
  C: TdxField;
  Form: TdxForm;
  DS: TDataSet;
begin
  Result := 1;
  if (ARecordSet.Form = nil) or (ARecordSet.Form.Id = DummyForm) then raise Exception.Create(rsFormNotAvail);
  if Trim(Fields) = '' then Exit;
  Form := ARecordSet.Form;
  DS := ARecordSet.DataSet;
  SL := TStringList.Create;

  try

  SplitStr(Fields, ';', SL);
  SQL := 'select id from ' + TableStr(Form.Id) + ' where ';
  AnyFieldModified := False;
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    C := Form.FindFieldByName(S);
    if C = nil then raise Exception.Create(Format(rsFieldNotFound, [S]));
    if C is TdxObjectField then
      raise Exception.CreateFmt(rsObjFieldNotUseDBUnique, [C.FieldName]);
    F := DS.FieldByName(FieldStr(C.Id));
    if F.IsNull then
      SQL := SQL + F.FieldName + ' is null and '
    else if F is TStringField then
      SQL := SQL + 'LOWER(' + F.FieldName + ')=''' +
        EscapeSQuotes(Utf8LowerCase(F.AsString)) + ''' and '
    else if F is TDateTimeField then
      SQL := SQL + F.FieldName + '=''' + F.AsString + ''' and '
    else if F is TFloatField then
    begin
      Value := StringReplace(F.AsString, DefaultFormatSettings.DecimalSeparator, '.', []);
      AbsValue := StringReplace(Value, '-', '', []);
      SQL := SQL + F.FieldName + '>=' + Value + '-' + AbsValue + '*2e-12 and ' +
      	F.FieldName + '<=' + Value + '+' + AbsValue + '*2e-12 and ';
    end
    else if C is TdxRecordId then
      SQL := SQL + 'id=' + F.AsString + ' and '
    else
      SQL := SQL + F.FieldName + '=' + F.AsString + ' and ';
    if (DS.Modified) and (MyUtf8CompareText(F.AsString, VarToStr(F.OldValue)) <> 0) then
      AnyFieldModified:=True;
  end;
  SQL := Copy(SQL, 1, Length(SQL) - 5);
  with ARecordSet.Session.DBase.OpenDataSet(SQL) do
  begin
    if DS.State = dsInsert then
    begin
      if RecordCount > 0 then Result := 0;
    end
    else
    begin
      if (RecordCount > 0) and AnyFieldModified then Result := 0
      else if RecordCount > 1 then Result := 0;
    end;
    Free;
  end;

  finally
    SL.Free;
  end;
end;

function IsUniqueRecords(ARecordSet: TSsRecordSet; const FormName,
  Fields: String): Integer;
var
  Fm: TdxForm;
  DS: TDataSet;
  Recs, FL: TList;
  SL, Vals: TStringList;
  S: String;
  F: TField;
  i: Integer;
  //RD: TReportData;
  rC: TRpGridColumn;
  C: TdxField;
  RS: TSsRecordSet;
  B: TBookMark;

  // True, если нет совпадений
  function CheckUnique: Boolean;
  var
    j, n, a: Integer;
    VL: TStringList;
    Fld: TField;
  begin
    Result := True;
    for j := 0 to Recs.Count - 1 do
    begin
      VL := TStringList(Recs[j]);
      a := 0;
      for n := 0 to VL.Count - 1 do
      begin
        Fld := TField(FL[n]);
        if MyUtf8CompareText(Fld.AsString, VL[n]) = 0 then Inc(a);
      end;
      if a = VL.Count then Exit(False);
    end;
  end;

begin
  Result := 1;
  if Trim(Fields) = '' then Exit;
  if not GetRecordSet(ARecordSet, FormName, '', RS, F) then Exit;
  DS := RS.DataSet;
  Recs := TList.Create;
  FL := TList.Create;
  SL := TStringList.Create;

  try

  SplitStr(Fields, ';', SL);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    if RS.Form <> nil then
    begin
      C := RS.Form.FindFieldByName(S);
      if C = nil then raise Exception.Create(Format(rsFieldNotFound, [S]));
      F := DS.FieldByName(FieldStr(C.Id));
    end
    else
    begin
      rC := RS.RD.Grid.FindColumnByFieldName(S);
      if rC = nil then raise Exception.Create(Format(rsFieldNotFound, [S]));
      F := DS.FieldByName(rC.FieldNameDS);
    end;
    FL.Add(F);
  end;

  RS.DisableScrollEvents;
  B := DS.GetBookmark;
  try
    DS.First;
    while not DS.Eof do
    begin
      if CheckUnique then
      begin
        Vals := TStringList.Create;
        for i := 0 to FL.Count - 1 do
        begin
          Vals.Add(TField(FL[i]).AsString);
        end;
        Recs.Add(Vals);
      end
      else
      begin
        Result := 0;
        Break;
      end;
      DS.Next;
    end;
  finally
    DS.GotoBookmark(B);
    DS.FreeBookmark(B);
    RS.EnableScrollEvents;
  end;

  finally
    ClearList(Recs);
    Recs.Free;
    FL.Free;
    SL.Free;
  end;
end;

function MsgBox(const Caption, Msg: String): Variant;
begin
  Result := 0;
  //MessageDlg(Caption, Msg, mtInformation, [mbOk], 0);
end;

function YesNoBox(const Caption, Msg: String): Integer;
begin
  Result := 0;{MessageDlg(Caption, Msg, mtConfirmation, [mbYes, mbNo], 0);
  case Result of
    mrYes: Result := 1;
    mrNo: Result := 2;
  end;         }
end;

function YesNoCancelBox(const Caption, Msg: String): Integer;
begin
  Result := 0;{MessageDlg(Caption, Msg, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
  case Result of
    mrYes: Result := 1;
    mrNo: Result := 2;
    mrCancel: Result := 3;
  end;         }
end;

function SetField(ARecordSet: TSsRecordSet; const aFieldName: String;
  Value: Variant): Variant;
var
  F: TdxField;
begin
  if ARecordSet.Form = nil then raise Exception.Create(rsFormNotAvail);
  F := ARecordSet.Form.FindFieldByName(aFieldName);
  if F = nil then raise Exception.Create(Format(rsFieldNotFound, [aFieldName]));
  with ARecordSet do
    if DataSet.Active and (DataSet.State in [dsInsert, dsEdit]) then
      ARecordSet.SetDSField(F, Value);
  (*DS := ARecordSet.DataSet;
  if DS.Active and (DS.State in [dsInsert, dsEdit]) then
  begin
    if C is TdxLookupComboBox then
    begin
      if Value <> Null then
        DS.FieldByName(FieldStr(C.Id) + 'l').Value := GetObjFieldValue(ARecordSet.Session, C, Value, True)
      else
        DS.FieldByName(FieldStr(C.Id) + 'l').SetData(nil);
      DS.FieldByName(FieldStr(C.Id)).Value := Value;
    end
    {else if C is TdxDBImage then
      with TdxDBImage(C) do
      begin
        if Value = Null then Clear
        else
        begin
          S := VarToStr(Value);
          if FileExists(S) then LoadFromFile(S)
          else raise Exception.CreateFmt(rsFileNotExists, [S]);
        end;
      end
    else if C is TdxFile then
      with TdxFile(C) do
      begin
        if Value = Null then Clear
        else
        begin
          S := VarToStr(Value);
          if FileExists(S) then LoadFromFile(S)
          else raise Exception.CreateFmt(rsFileNotExists, [S]);
        end;
      end}
    else
      DS.FieldByName(FieldStr(C.Id)).Value:=Value;
  end;       *)
  Result := Value;
end;

function SetLabel(ARecordSet: TSsRecordSet; const aFieldName: String;
  Value: Variant): Variant;
var
  L: TdxLabel;
begin
  if ARecordSet.Form = nil then raise Exception.Create(rsFormNotAvail);
  L := ARecordSet.Form.FindLabelByName(aFieldName, True);
  if L = nil then raise Exception.Create(Format(rsFieldNotFound, [aFieldName]));
  L.Caption:=VarToStr(Value);
  L.Value:=Value;
  Result := Value;
  aRecordSet.LabelChanged(L);
end;

function IsEditRec(ARecordSet: TSsRecordSet): Integer;
begin
  if (ARecordSet.Form = nil) or (ARecordSet.Form.Id = DummyForm) then raise Exception.Create(rsFormNotAvail);
  if ARecordSet.DataSet.State = dsEdit then Result := 1
  else Result := 0;
end;

function IsModifiedRec(ARecordSet: TSsRecordSet): Integer;
begin
  if (ARecordSet.Form = nil) or (ARecordSet.Form.Id = DummyForm) then raise Exception.Create(rsFormNotAvail);
  Result := 0;
  if ARecordSet.DataSet.Modified then Result := 1;
end;

function Concat(Vals: array of Variant): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(Vals) - 1 do
  	Result := Result + VarToStr(Vals[i]);
  Result := Trim(Result);
end;

function FNumber(V: Double; Digits: Integer): String;
var
  S: String;
  d: Double;
begin
  d := MathRound(V, Digits);
  S := ',0';
  if Digits > 0 then
    S := ',0.' + DupeString('0', Digits);
  Result := FormatFloat(S, d);
end;

function TextFormat(const S: String; ARecordSet: TSsRecordSet): String;
var
  i, Len: Integer;
  Ch: Char;
  FlNm, Expr: String;
  State: (stText, stField, stExpr);
  RS: TSsRecordSet;
  F: TdxField;
  V: Variant;

  function NextCh: Char;
  begin
    if i < Len then Result := S[i + 1]
    else Result := #0;
  end;

begin
  if ARecordSet.Form = nil then
  begin
    if ARecordSet.Parent = nil then raise Exception.Create(rsFormNotAvail)
    else RS := ARecordSet.Parent;
  end
  else RS := ARecordSet;

  Result := '';
  State := stText;
  i := 1; Len := Length(S);
  while i <= Len do
  begin
    Ch := S[i];
    case State of
      stText:
        case Ch of
          '[':
            begin
              if NextCh = '[' then
              begin
                Result := Result + NextCh;
                Inc(i);
              end
              else
              begin
                State := stField;
                FlNm := '';
              end;
            end;
          '{':
            begin
              if NextCh = '{' then
              begin
                Result := Result + NextCh;
                Inc(i);
              end
              else
              begin
                State := stExpr;
                Expr := '';
              end;
            end;
          #10, #13:
            begin
              if NextCh in [#10, #13] then
                Inc(i);
              Result := Result + ' ';
            end;
          else
            Result := Result + Ch;
        end;
      stField:
        case Ch of
          ']':
            begin
              if (Copy(FlNm, 1, 1) = '!') and (RS.Parent <> nil) then
              begin
                Delete(FlNm, 1, 1);
                V := RS.Parent.GetFieldValue(FlNm);
              end
              else
                V := RS.GetFieldValue(FlNm);
              Result := Result + VarToStr(V);
              State := stText;
            end;
          else
            FlNm := FlNm + Ch;
        end;
      stExpr:
        case Ch of
          '}':
            begin
              if NextCh = '}' then
              begin
                Expr := Expr + NextCh;
                Inc(i);
              end
              else
              begin
                Result := Result + VarToStr(EvalExpr(Expr, RS));
                State := stText;
              end;
            end;
          else
            Expr := Expr + Ch;
        end;
    end;
    Inc(i);
  end;
end;

function GetTimeStamp(D, T: Variant): Variant;
var
  DT1, DT2: TDateTime;
begin
  if (D = Null) and (T = Null) then Result := Null
  else
  begin
    if D = Null then D := 0
    else if T = Null then T := 0;
    ShiftDT(D, T, 0, 0, DT1, DT2);
    Result := SecondsBetween(DT1, DT2);
    if DT1 < 0 then Result := -Result;
  end;
end;

{function CaseOf(const AValue, AItems: String): String;
var
  SL: TStringListUtf8;
begin
  SL := TStringListUtf8.Create;
  SL.Delimiter:=';';
  SL.StrictDelimiter:=True;
  SL.DelimitedText := AItems;
  Result := SL.Values[AValue];
  SL.Free;
end;  }

function CaseOf(const AValue, AItems: String): String;
var
  SL: TStringListUtf8;
begin
  SL := TStringListUtf8.Create;
  SplitStr(AItems, ';', SL);
  Result := SL.Values[AValue];
  SL.Free;
end;

function GetTypedText(SS: TSession; AIndex: Integer): Variant;
var
  S, W: String;
  i: Integer;
begin
  Result := GetVar(SS, '__TypedText__');
  if (AIndex <= 0) or (Result = Null) then Exit;

  S := VarToStr(Result);
  for i := 1 to AIndex do
  begin
    S := Trim(S);
    W := CutStr(S, ' ');
  end;
  if W = '' then Result := Null
  else Result := W;
end;

procedure SetTypedText(SS: TSession; S: String);
begin
  S := Trim(S);
  SetVar(SS, '__TypedText__', IIF(S <> '', S, Null));
end;

function IsWebServer: Boolean;
begin
  Result := True;
end;

end.

