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

unit AppUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strconsts, dxctrls, Db, SQLDb, formmanager, dxreports,
  reportmanager, dxtypes, typeswrapper, fpjson, BGRAGraphics, dxsqlquery, process;

function AppPath: String;
//function FilesPath: String;
//function TempPath(const SId: String): String;
//function TemplatesPath(SS: TSession): String;
function GetHtmlPath: String;
function GetMetaDataPath(MD: TMetaData; Html: Boolean = False): String;
function GetCachePath(SS: TSession; Html: Boolean = False): String;
function GetImagesPath(MD: TMetaData; Html: Boolean = False): String;
function GetEmbeddedImagesPath(MD: TMetaData; Html: Boolean = False): String;
function GetAbsolutePath(const Path: String): String;
//function SplitStr(const S: String; D: Char): TStrings;
procedure SplitStr(const S, D: String; SL: TStrings);
procedure DebugStr(const S: String; SS: TSession = nil);
function GetUniqueFileName(RecId, FieldId: Integer; FileName: String): String;
function Bool2Str(B: Boolean): String;
function Str2Bool(const S: String): Boolean;
procedure ClearList(L: TList);
function LoadString(const FileName: String): String;
//procedure LogError(E: Exception);
procedure LogString(const S: String);
function ReadToken(const S: String; var P: Integer; var Tk: Char): String;
//procedure OpenUrl(const S: String);
//procedure OpenFile(const FileName: String);
//procedure ErrMsg(const Msg: String);
function FormatField(F: TField; Nbsp: Boolean = True): String;
procedure SetDisplayFormat(FormMan: TFormManager; aFm: TdxForm; aDS: TDataSet);
procedure SetDisplayFormatQ(RD: TReportData; DS: TDataSet);
function GetRpFieldType(SS: TSession; F: TdxField): TRpFieldType;
function CheckType(C: TdxField; var Value: String): Boolean;
function NeedQuote(C: TdxField): Boolean;
function XmlToStr(S: String): String;
function StrToXml(S: String): String;
procedure CalcQuery(ARecordSet: TSsRecordSet);
procedure FilterQuery(ARecordSet: TSsRecordSet);
procedure BuildSortIndexes(RD: TReportData; DataSet: TDataSet);
function MyStrToFloat(const S: String; out E: Double): Boolean;
function MyStrToDate(const S: String; out D: TDateTime): Boolean;
function MyStrToTime(const S: String; out T: TDateTime): Boolean;
function EscapeSQuotes(const S: String): String;
function UnEscapeSemicolon(const S: String): String;
function MathRound(X: Double; N: Integer): Double;
function MyUtf8CompareText(const S1, S2: String): PtrInt;
function XmlToHtml(S: String): String;
function HtmlToXml(S: String): String;
function VarTypeToStr(V: Variant): String;
function GetComponentDataTypeStr(C: TdxField): String;
function GetObjFieldValue(SS: TSession; Obj: TdxField; aKey: Integer; FullPath: Boolean): String;
function ExceptionInvalidValueForField(E: Exception): Boolean;
function LookupObjectField(SS: TSession; ObjF: TdxObjectField; ForceObject: Boolean): TdxField;
function GetComponentFieldValue(DS: TDataSet; C: TdxField): Variant;
function GetComponentDataSetFieldName(C: TdxField): String;
function SetZeros(E: Double; N: Integer): String;
function GetObjectFieldField(SS: TSession; ObjField: TdxObjectField): TdxField;
function LookupComponent(SS: TSession; Fm: TdxForm; FlNm: String): TdxField;
procedure SetupRpField(SS: TSession; C: TdxField; FlNm: String; pFld: PRpField);
function IIF(Condition, Value1, Value2: Variant): Variant;
function TableStr(Id: Integer): String;
function FieldStr(Id: Integer): String;
function IsHierarchyObj(SS: TSession; Obj: TdxField): Boolean;
function RpFieldToFormField(SS: TSession; RD: TReportData; FId: Integer): TdxField;
function CreateReportForm(SS: TSession; RD: TReportData; out SQL: String): TdxForm;
function MakeNumberFormat(Prec: Integer; Group, PadZeros: Boolean): String;
function DecodeURLElement(Const S: AnsiString): AnsiString;
function GetGridColumnAlignment(SS: TSession; Fm: TdxForm; Col: TdxColumn): TAlignment;
function GetGridColumnLayout(Col: TdxColumn): TTextLayout;
function GetRpGridColumnAlignment(SS: TSession; RD: TReportData; Col: TRpGridColumn): TAlignment;
function GetRpGridColumnLayout(Col: TRpGridColumn): TTextLayout;
function Date2Str(D: TDateTime): String;
function Str2Date(const S: String): TDateTime;
function Time2Str(T: TDateTime): String;
function MakeJsonObjectString(Items: array of const): String;
function MakeJsonErrString(Code: Integer; const Msg: String): String;
function GetTopControl(Cont: TdxWinControl): TdxComponent;
function TruncTime(Fmt: TdxTimeFormat; DT: TDateTime): TDateTime;
function GenerateId: String;
//function ColorToHtmlColor(C: TColor): String;
function StrToHtml(const S: String; ReplaceNewLines: Boolean = False; Nbsp: Boolean = False): String;
//function CheckActionEnabled(B: TdxButton): Boolean;
procedure ScaleForm(Fm: TdxForm; DesignTimePPI: Integer);
//procedure ScaleForms(FMan: TFormManager; DesignTimePPI: Integer);
procedure ScaleReport(RD: TReportData; FromPPI, ToPPI: Integer);
//procedure ScaleReports(RMan: TReportManager; FromPPI, ToPPI: Integer);
procedure SplitComponentName(const AName: String; out ANameStr: String;
  out ANameNum: Integer);
procedure SkipBOM(St: TStream);
function ShellExec(const Operation, FileName, Params, WorkDir: String;
  ShowCmd: LongInt): Boolean;
function GetBuildDate: TDateTime;
procedure DebugFile(const FileName: String; Value: Variant);
procedure TryStrToColor(const ColorStr: String; out Color: TColor);
function DecodeCellText(const S: String): String;
function le2br(const S: String): String;
function RunAction(const ActionData: String; ARecordSet: TSsRecordSet): Variant;
function ExceptionToHtml(E: Exception): String;
//function FormLookupFieldValue(RS: TSsRecordSet; const aFieldName: String): Variant;
function CanEnabledControl(C: TdxControl): Boolean;
function SQLSelect(SS: TSession; const SQL: String): TdxSQLQuery;
procedure SQLExecute(SS: TSession; const SQL: String);
function NowTime: Integer;
function GetFormHRef(Fm: TdxForm): String;
procedure CheckVisiblePageControls(SS: TSession; Fm: TdxForm);
function GetListSourceField(SS: TSession; Obj: TdxCustomComboBox): TdxField;
function FieldExists(const FieldName, E: String): Boolean;
function FormExistsInExpr(const FormName, Expr: String): Boolean;
function FieldExistsForQuery(const FieldName, E: String): Boolean;
function IsNumericComponent(SS: TSession; C: TdxField): Boolean;
function CutStr(var S: String; D: Char): String;
function GetComponentDisplayFormat(SS: TSession; Fm: TdxForm; C: TdxField): String;
procedure SetDSFieldDisplayFormat(F: TField; Fmt: String);
function GetFieldDisplayText(F: TField): String;
function GetRpFieldComponent(SS: TSession; F: TRpField; aLow: Boolean): TdxComponent;
//procedure HideUnvisibleControls(Fm: TdxForm);

implementation

uses
  LazUtf8, LazFileUtils, appsettings, sqlgen, DateUtils, {$IFDEF windows}ShellApi,{$ENDIF}
  Variants, expressions, StrUtils, Math, dbengine, dxactions, pivotgrid,
  mainserver, scriptmanager, uPSRuntime, mylogger, TypInfo;

function AppPath: String;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function FilesPath: String;
begin
  Result := AppPath + 'files' + DirectorySeparator;
end;

function TempPath(SS: TSession): String;
begin
  Result := GetCachePath(SS) + 'temp' + DirectorySeparator;
end;

{function TemplatesPath(SS: TSession): String;
var
  S: String;
begin
  S := SS.DBItem.TemplatesPath;
  if S <> '' then
  begin
    if not FilenameIsAbsolute(S) then
      Result := AppPath + S
    else
      Result := S;
    Result := IncludeTrailingPathDelimiter(Result);
  end
  else
    Result := AppPath + 'templates' + DirectorySeparator;
end;   }

function GetHtmlPath: String;
begin
  Result := AppPath + 'html' + DirectorySeparator;
end;

function GetMetaDataPath(MD: TMetaData; Html: Boolean): String;
begin
  if Html then
    Result := '/cache/' + Utf8LowerCase(MD.ConnectName) + MD.Id + '/'
  else
    Result := AppPath + 'cache' + DirectorySeparator +
      Utf8LowerCase(MD.ConnectName) + MD.Id + DirectorySeparator;
end;

function GetCachePath(SS: TSession; Html: Boolean): String;
begin
  Result := GetMetaDataPath(SS.MetaData, Html) + SS.BrowserId +
    IIF(Html, '/', DirectorySeparator);
end;

function GetImagesPath(MD: TMetaData; Html: Boolean): String;
begin
  Result := GetMetaDataPath(MD, Html) + 'img' +
    IIF(Html, '/', DirectorySeparator);
end;

function GetEmbeddedImagesPath(MD: TMetaData; Html: Boolean): String;
begin
  Result := GetMetaDataPath(MD, Html) + 'eimg' +
    IIF(Html, '/', DirectorySeparator);
end;

function GetAbsolutePath(const Path: String): String;
begin
  Result := Path;
  if (Pos(':', Result) <> 2) and (Pos('/', Result) <> 1) then
    Result := AppPath + Result;
end;

{function SplitStr(const S: String; D: Char): TStrings;
var
  W: String;
  i: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  W := '';
  for i := 1 to Length(S) do
  begin
    if S[i] = D then
    begin
      SL.Add(W);
      W := '';
    end
    else
      W := W + S[i];
  end;
  if W <> '' then
    SL.Add(W);
  Result := SL;
end; }


procedure SplitStr(const S, D: String; SL: TStrings);
var
  W: String;
  i, Len, DLen: Integer;
begin
  SL.Clear;
  if S = '' then Exit;
  W := '';
  Len := Length(S);
  DLen := Length(D);
  i := 1;
  while i <= Len do
  begin
    if Copy(S, i, DLen) = D then
    begin
      SL.Add(W);
      W := '';
      i := i + DLen;
    end
    else
    begin
      W := W + S[i];
      Inc(i);
    end;
  end;
  SL.Add(W);
end;

procedure DebugStr(const S: String; SS: TSession);
begin
  if AppSet.DebugMode then
  begin
    if SS <> nil then
      LogString('[' + Copy(SS.BrowserId, 1, 8) + '] ' + S)
    else
      LogString(S);
  end;
end;

function GetUniqueFileName(RecId, FieldId: Integer; FileName: String): String;
var
  Ext: String;
begin
  Ext := ExtractFileExt(FileName);
  FileName := ChangeFileExt(FileName, '');
  Result := IntToStr(RecId) + '_' + IntToStr(FieldId) + '_' + FileName;
  Result := Utf8Copy(Result, 1, 255 - Utf8Length(Ext)) + Ext;
end;

function Bool2Str(B: Boolean): String;
begin
  if B then Result := '1'
  else Result := '0';
end;

function Str2Bool(const S: String): Boolean;
begin
  if S = '1' then Result := True
  else Result := False;
end;

procedure ClearList(L: TList);
var
  i: Integer;
begin
  for i := 0 to L.Count - 1 do
    if L[i] <> nil then
      TObject(L[i]).Free;
  L.Clear;
end;

function LoadString(const FileName: String): String;
begin
  with TStringList.Create do
  try
    LoadFromFile(Utf8ToSys(FileName));
    Result := Text;
  finally
    Free;
  end;
end;

function DumpExceptionCallStack: String;
var
  I: Integer;
  Frames: PPointer;
  Report: string;
  EObj: PExceptObject;
  E: Exception;
begin
  Report := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  EObj := RaiseList;
  while EObj <> nil do
  begin
    E := Exception(EObj^.FObject);
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
      'Message: ' + E.Message + LineEnding;
    Report := Report + BackTraceStrFunc(EObj^.Addr);
    Frames := EObj^.Frames;
    for I := 0 to EObj^.Framecount - 1 do
      Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
    EObj := EObj^.Next;
  end;
  Result := Report;
end;

{procedure LogError(E: Exception);
var
  Mode: Integer;
  S: String;
begin
  if not FileExists(Utf8ToSys(AppPath + 'dxwebsrv.log')) then
    Mode := fmCreate
  else
    Mode := fmOpenWrite;
  with TFileStream.Create(Utf8ToSys(AppPath + 'dxwebsrv.log'), Mode) do
  try
    S := DateTimeToStr(Now) + ' (' + E.ClassName + '): ' + E.Message + LineEnding;
    //S := DateTimeToStr(Now) + DumpExceptionCallStack + LineEnding;
    Position := Size;
    Write(Pointer(S)^, Length(S));
  finally
    Free;
  end;
end;   }

(*function ReadToken(const S: String; var P: Integer; var Tk: Char): String;
var
  Len, c: Integer;
  W, Qt: String;

  procedure SkipWhites;
  begin
    while (P <= Len) and (S[P] in [#9, #10, #13, #32]) do
      Inc(P);
    // Если это комментарии
    if (S[P] = '/') and (P < Len) then
    begin
      if S[P + 1] = '/' then
      begin
        while (P <= Len) and (not (S[P] in [#10, #13])) do
          Inc(P);
        SkipWhites;
      end
      else if S[P + 1] = '*' then
      begin
        while (P < Len) and (Copy(S, P, 2) <> '*/') do
          Inc(P);
        P := P + 2;
        SkipWhites;
      end;
    end;
  end;

begin
  Len := Length(S);
  SkipWhites;
  if P > Len then
  begin
    Tk := #0;
    Exit('');
  end;
  case S[P] of
    '[':
      begin
        Tk := '[';
        Inc(P); c := 0; W := '';
        while (P <= Len) do
        begin
          if S[P] = ']' then
          begin
            if c = 0 then Break
            else Dec(c);
          end
          else if S[P] = '[' then Inc(c);
          W := W + S[P];
          Inc(P);
        end;
        Result := W; Inc(P);
      end;
    '=', '<', '>', '+', '-', '*', '/', '|', '&', '(', ')', '#':
      begin
        Tk := '=';
        W := S[P];
        if (P < Len) and (S[P + 1] in ['>', '=']) then
        begin
          Inc(P);
          W := W + S[P];
        end;
        Inc(P);
        Result := W;
      end;
    '''', '"':
      begin
        Tk := '''';
        Qt := S[P];
        W := ''; Inc(P);
        while (P <= Len) and (S[P] <> Qt) do
        begin
          W := W + S[P];
          Inc(P);
        end;
        Inc(P); Result := W;
      end;
    '0'..'9':
      begin
        Tk := '0';
        W := '';
        while S[P] in ['0'..'9', '.'] do
        begin
          W := W + S[P];
          Inc(P);
        end;
        Result := W;
      end;
    'a'..'z', 'A'..'Z', '_':
      begin
        Tk := 'a'; W := '';
        while (P <= Len) and (S[P] in ['a'..'z', 'A'..'Z', '_', '0'..'9']) do
        begin
          W := W + S[P];
          Inc(P);
        end;
        Result := W;
      end
    else
    begin
      Tk := S[P];
      Result := Tk;
      Inc(P);
    end;
  end;
end; *)

{procedure LogString(const S: String);
var
  Mode: Integer;
  Buf: String;
begin
  if not FileExists(AppPath + 'dxwebsrv.log') then
    Mode := fmCreate
  else
    Mode := fmOpenWrite + fmShareDenyNone;
  with TFileStream.Create(AppPath + 'dxwebsrv.log', Mode) do
  try try
    Buf := DateTimeToStr(Now) + ' ' + S + LineEnding;
    Position := Size;
    Write(Pointer(Buf)^, Length(Buf));
  except
    ; // Глушим ошибку, когда файл уже занят другим потоком.
  end;
  finally
    Free;
  end;
end; }

procedure LogString(const S: String);
begin
  MyLog.WriteToFile(S);
end;

function ReadToken(const S: String; var P: Integer; var Tk: Char): String;
var
  Len, Start: Integer;
  Qt: String;
begin
  Len := Length(S);
  Start := P;
  if P > Len then
  begin
    Tk := #0;
    Exit('');
  end;
  case S[P] of
    #1..#32:
      begin
        while (P <= Len) and (S[P] in [#1..#32]) do
        begin
          Inc(P);
        end;
        Tk := ' ';
        Result := Copy(S, Start, P - Start);
      end;
    '[':
      begin
        Tk := '[';
        while (P <= Len) and (S[P] <> ']') do
        begin
          Inc(P);
        end;
        Inc(P);
        Result := Copy(S, Start, P - Start);
      end;
    '=', '<', '>', '+', '-', '*', '|', '&', '(', ')', '#':
      begin
        Tk := '=';
        if (P < Len) and (S[P + 1] in ['>', '=']) and not (S[P] in ['(', ')']) then
        begin
          Inc(P);
        end;
        Inc(P);
        Result := Copy(S, Start, P - Start);
      end;
    '''', '"':
      begin
        Tk := '''';
        Qt := S[P];
        //W := '';
        Inc(P);
        while (P <= Len) and (S[P] <> Qt) do
        begin
          Inc(P);
        end;
        Inc(P);
        Result := Copy(S, Start, P - Start);
      end;
    '0'..'9':
      begin
        Tk := '0';
        while S[P] in ['0'..'9', '.'] do
        begin
          Inc(P);
        end;
        Result := Copy(S, Start, P - Start);
      end;
    'a'..'z', 'A'..'Z', '_':
      begin
        Tk := 'a';
        while (P <= Len) and (S[P] in ['a'..'z', 'A'..'Z', '_', '0'..'9']) do
        begin
          Inc(P);
        end;
        Result := Copy(S, Start, P - Start);
      end;
    '/':
      begin
        if (P < Len) and (S[P + 1] = '/') then
        begin
          while (P <= Len) and not (S[P] in [#13, #10]) do
          begin
            Inc(P);
          end;
          Tk := '/';
          Result := Copy(S, Start, P - Start);
        end
        else if (P < Len) and (S[P + 1] = '*') then
        begin
          Inc(P, 2);
          while P < Len do
          begin
            if (S[P] = '*') and (S[P + 1] = '/') then
            begin
              Inc(P, 2);
              Break;
            end;
            Inc(P);
          end;
          Tk := '/';
          Result := Copy(S, Start, P - Start);
        end
        else
        begin
          Tk := '=';
          Result := S[P];
          Inc(P);
        end;
      end
    else
    begin
      Tk := S[P];
      Result := Tk;
      Inc(P);
    end;
  end;
end;

{procedure OpenUrl(const S: String);
begin
  ShellExecute(0, 'open', PChar(S), nil, nil, 1);
end;

procedure OpenFile(const FileName: String);
var
  S: String;
begin
  S := Utf8ToSys(FileName);
  if not FileExists(S) then
    ErrMsg(Format(rsFileNotExists, [FileName]))
  else
    ShellExecute(0,nil,pchar(S),nil,nil,1);
end;  }

{procedure ErrMsg(const Msg: String);
begin
  MessageDlg(rsError, Msg, mtError, [mbOk], 0);
end;  }

function FormatField(F: TField; Nbsp: Boolean): String;
begin
  Result := F.AsString;
  if Result = '' then Exit;
  if F.DataType = ftFloat then
    with TNumericField(F) do
    begin
      if DisplayFormat <> '' then
      begin
        Result := FormatFloat(DisplayFormat, AsFloat);
        if Nbsp then StringReplace(Result, ' ', '&nbsp;', [rfReplaceAll]);
      end;
    end
  else if F.DataType = ftTime then
    with TTimeField(F) do
    begin
      if DisplayFormat <> '' then
        Result := FormatDateTime(DisplayFormat, AsDateTime);
    end;
end;

procedure SetDisplayFormat(FormMan: TFormManager; aFm: TdxForm; aDS: TDataSet);
var
  L: TList;
  i: Integer;
  F, FF: TdxField;
  Fl: TField;
  Fm: TdxForm;
begin
  L := TList.Create;
  aFm.GetFields(L);
  for i := 0 to L.Count - 1 do
  begin
    F := TdxField(L[i]);
    if not ((F is TdxCalcEdit) or (F is TdxTimeEdit) or (F is TdxObjectField)) then Continue;
    Fl := aDS.FieldByName(FieldStr(F.Id));
    if F is TdxObjectField then
      with TdxObjectField(F) do
        if (ObjId > 0) and (FieldId > 0) then
        begin
          FF := F.Form.FindField(ObjId);
          Fm := FormMan.FindForm(TdxLookupComboBox(FF).SourceTId);
          F := Fm.FindField(FieldId);
        end;
    if F is TdxCalcEdit then
      TNumericField(Fl).DisplayFormat:=GetPrecStr(F)
    else if F is TdxTimeEdit then
      TTimeField(Fl).DisplayFormat:=TdxTimeEdit(F).TimeFormatStr;
  end;
  L.Free;
end;

{function FindNotZeroField(RD: TReportData; idx: Integer): PRpField;
var
  i: Integer;
  pF: PRpField;
begin
  Result := nil;
  for i := 0 to RD.Sources.Count - 1 do
  begin
    pF := RD.Sources[i]^.Fields[idx];
    if (pF^.Tp <> flNone) and (pF^.Zero = False) then Exit(pF);
  end;
end;  }

function FindSrcField(RD: TReportData; idx: Integer): PRpField;
var
  i: Integer;
  Sr: TRpSource;
  pFl: PRpField;
begin
  Result := nil;
  for i := 0 to RD.Sources.Count - 1 do
  begin
    Sr := RD.Sources[i]^;
    if idx < Sr.Fields.Count then
    begin
      pFl := Sr.Fields[idx];
      if not pFl^.Zero then
        Exit(pFl);
    end;
  end;
end;

procedure SetDisplayFormatQ(RD: TReportData; DS: TDataSet);
var
  i: Integer;
  Fl: TField;
begin
  for i := 0 to RD.GetFieldCount - 1 do
  begin
    if not RD.GetFieldVisible(i) then Continue;
    Fl := DS.FieldByName( RD.GetFieldNameDS(i) );
    if Fl is TNumericField then
      TNumericField(Fl).DisplayFormat := RD.GetDisplayFormat(i)
    else if Fl is TDateTimeField then
      TDateTimeField(Fl).DisplayFormat := RD.GetDisplayFormat(i);
  end;
end;

(*procedure SetDisplayFormatQ(FormMan: TFormManager; RD: TReportData; DS: TDataSet);
var
  L: TRpFieldList;
  i: Integer;
  F, LowF: TRpField;
  Fm: TdxForm;
  C: TdxField;
  Fl: TField;
  pF: PRpField;
  CF: TRpCalcField;
  S: String;
begin
  L := RD.Sources[0]^.Fields;
  for i := 0 to L.Count - 1 do
  begin
    F := PRpField(L[i])^;
    if (F.Visible = False) or (F.Tp = flNone) then Continue;
    if F.Zero then
    begin
      pF := FindSrcField(RD, i);  // Почти то же самое, что и RD.FindField
      // Так может быть, если ни одного поля не выбрано, а итоговая ф-ция Count
      if pF = nil then Continue;
      F := pF^;
    end;
    LowF := GetLowField(@F)^;
    Fm := FormMan.FindForm(LowF.TId);
    C := Fm.FindField(LowF.FId);
    Fl := DS.FieldByName('f' + IntToStr(F.Id));
    if (C is TdxCalcEdit) and (not (F.Func in [tfMerge, tfMergeAll, tfCount, tfDistCount])) then
      TNumericField(Fl).DisplayFormat := GetPrecStr(C)
    else if (C is TdxTimeEdit) and (F.Func = tfNone) then
      TTimeField(Fl).DisplayFormat:=TdxTimeEdit(C).TimeFormatStr;
  end;
  for i := 0 to RD.CalcFields.Count - 1 do
  begin
    CF := RD.CalcFields[i]^;
    if CF.Tp = flNumber then
    begin
      S := ',0';
      if CF.Size > 0 then S := ',0.' + DupeString('0', CF.Size);
      TNumericField(DS.FieldByName('cf' + IntToStr(CF.Id))).DisplayFormat:=S;
    end
    else if CF.Tp = flTime then
    	TTimeField(DS.FieldByName('cf' + IntToStr(CF.Id))).DisplayFormat:='hh:mm:ss';
  end;
end;    *)

{procedure SetDisplayFormatQ(FormMan: TFormManager; RD: TReportData;
  aDS: TDataSet);
var
  L: TRpFieldList;
  i: Integer;
  rF: TRpField;
  pF: PRpField;
  Fm: TdxForm;
  Fl: TField;
  F: TdxField;
begin
  L := RD.Sources[0]^.Fields;
  for i := 0 to L.Count - 1 do
  begin
    rF := PRpField(L[i])^;
    if (rF.Visible) and (rF.Tp <> flNone) then
    begin
      if rF.Tp = flObject then rF := GetLowField(@rF)^;
      if rF.Zero then
      begin
        pF := FindNotZeroField(RD, i);
        if pF <> nil then
          rF := pF^
        else Continue;
      end;
      if rF.Tp in [flNumber, flTime] then
      begin
        Fm := FormMan.FindForm(rF.TId);
        F := Fm.FindField(rF.FId);
        Fl := aDS.FieldByName(FieldStr(rF.Id));
        if F is TdxCalcEdit then
          TNumericField(Fl).DisplayFormat:=GetPrecStr(F)
        else if F is TdxTimeEdit then
          TTimeField(Fl).DisplayFormat:=TdxTimeEdit(F).TimeFormatStr;
      end;
    end;
  end;
end;  }

function GetRpFieldType(SS: TSession; F: TdxField): TRpFieldType;
begin
  if F is TdxObjectField then F := GetObjectFieldField(SS, TdxObjectField(F));

  if (F is TdxEdit) or (F is TdxComboBox) or (F is TdxMemo) then Result := flText
  else if F is TdxCalcEdit then Result := flNumber
  else if F is TdxDateEdit then Result := flDate
  else if F is TdxTimeEdit then Result := flTime
  else if F is TdxLookupComboBox then Result := flObject
  else if F is TdxCounter then Result := flCounter
  else if F is TdxCheckBox then Result := flBool
  else if F is TdxRecordId then Result := flRecId
  else Result := flNone;
end;

function CheckType(C: TdxField; var Value: String): Boolean;
var
  N: integer;
  E: Double;
  D: TDateTime;
  FS: TFormatSettings;
begin
  Result := True;
  if ((C is TdxLookupComboBox) or (C is TdxCounter) or (C is TdxRecordId)) and (not TryStrToInt(Value, N)) then
    Result := False
  else if (C is TDxCheckBox) and ((Value <> '0') and (Value <> '1')) then
    Result := False
  else if C is TdxCalcEdit then
  begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator:='.';
    if TryStrToFloat(Value, E) then Value := FloatToStr(E, FS)
    else Result := False;
  end
  else if C is TdxDateEdit then
  begin
    if TryStrToDate(Value, D) then Value := DateToStr(D)
    else Result := False;
  end
  else if C is TdxTimeEdit then
  begin
    if TryStrToTime(Value, D) then Value := TimeToStr(D)
    else Result := False;
  end
end;

function NeedQuote(C: TdxField): Boolean;
begin
  Result := (C is TdxEdit) or (C is TdxMemo) or (C is TdxComboBox) or
    (C is TdxDateEdit) or (C is TdxTimeEdit);
end;

function XmlToStr(S: String): String;
begin
  S := StringReplace(S, '&amp;', '&', [rfReplaceall]);
  S := StringReplace(S, '&quot;', '"', [rfReplaceAll]);
  S := StringReplace(S, '&lt;', '<', [rfReplaceAll]);
  S := StringReplace(S, '&gr;', '>', [rfReplaceAll]);
  S := StringReplace(S, '&gt;', '>', [rfReplaceAll]);
  Result := S;
end;

function StrToXml(S: String): String;
begin
  S := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
  S := StringReplace(S, '"', '&quot;', [rfReplaceAll]);
  S := StringReplace(S, '<', '&lt;', [rfReplaceAll]);
  S := StringReplace(S, '>', '&gt;', [rfReplaceAll]);
  Result := S;
end;

procedure CalcQuery(ARecordSet: TSsRecordSet);
var
  EB: TExpressionBuilder;
  EL, FL: TList;
  i: Integer;
  S: String;
  E: TExpression;
  F: TField;
  AftScr, BefScr: TDataSetNotifyEvent;
  CF: TRpCalcField;
  V: Variant;
  RD: TReportData;
  DS: TDataSet;
begin
  RD := ARecordSet.RD;
  DS := ARecordSet.DataSet;
  if RD.CalcFields.Count = 0 then Exit;
  EB := TExpressionBuilder.Create;
  EB.SkipLabels:=True;
  EB.RecordSet := ARecordSet;
  EB.SkipLabels := True;
  EL := TList.Create;
  FL := TList.Create;
  for i := 0 to RD.CalcFields.Count - 1 do
  begin
    CF := RD.CalcFields[i]^;
    S := CF.Expr;
    if Trim(S) <> '' then
    begin
      //try
        E := EB.Build(S);
        EL.Add(E);         // Здесь nil уместен (см. else)
      {except
        on Ex: Exception do
        begin
          EL.Add(nil);
        end;
      end;   }
    end
    else
    begin
      EL.Add(nil);
    end;
    FL.Add(DS.FieldByName('cf' + IntToStr(CF.Id)));
  end;
  EB.Free;

  // Снимаем флаг обязательный
  //for i := 0 to DS.Fields.Count - 1 do
  //  DS.Fields[i].Required:=False;
  //

  AftScr := DS.AfterScroll;
  BefScr := DS.BeforeScroll;
  DS.AfterScroll := nil;
  DS.BeforeScroll:=nil;

  try

  DS.First;
  while not DS.Eof do
  begin
    DS.Edit;
    for i := 0 to EL.Count - 1 do
    begin
      CF := RD.CalcFields[i]^;
      E := TExpression(EL[i]);
      F := TField(FL[i]);
      if E = nil then
      begin
        F.Value := Null;
      end
      else
        //try
          V := E.Calc;
          if (CF.Tp = flNumber) and (V <> Null) then V := MathRound(V, CF.Size);
          if DS.State <> dsEdit then DS.Edit;
          F.Value := V;
        {except
          on Ex: Exception do
          begin
            F.Value := Null;
          end;
        end; }
    end;
    DS.Post;
    DS.Next;
  end;

  finally
    DS.First;
    DS.AfterScroll:=AftScr;
    DS.BeforeScroll:=BefScr;

    ClearList(EL);
    EL.Free;
    FL.Free;
  end;
end;

(*var
  EB: TExpressionBuilder;
  EL, FL: TList;
  Errs: TStringList;
  i: Integer;
  S: String;
  E: TExpression;
  F: TField;
  CF: TRpCalcField;
begin
  if aRD.CalcFields.Count = 0 then Exit;
  EB := TExpressionBuilder.Create;
  //EB.SkipLabels:=True;
  EB.RD := aRD;
  EB.RDSet := DS;
  EB.DataSet := FormDS;
  EB.Form := aForm;
  EB.Session := SS;
  EL := TList.Create;
  FL := TList.Create;
  Errs := TStringList.Create;
  for i := 0 to aRD.CalcFields.Count - 1 do
  begin
    CF := aRD.CalcFields[i]^;
    S := CF.Expr;
    if Trim(S) <> '' then
    begin
      try
        E := EB.Build(S);
        EL.Add(E);
        Errs.Add('');
      except
        on Ex: Exception do
        begin
          EL.Add(nil);
          Errs.Add(Ex.Message);
        end;
      end;
    end
    else
    begin
      EL.Add(nil);
      Errs.Add('');
    end;
    FL.Add(DS.FieldByName('cf' + IntToStr(CF.Id)));
  end;

  // Снимаем флаг обязательный
  for i := 0 to DS.Fields.Count - 1 do
    DS.Fields[i].Required:=False;
  //

  DS.First;
  while not DS.Eof do
  begin
    DS.Edit;
    for i := 0 to EL.Count - 1 do
    begin
      E := TExpression(EL[i]);
      F := TField(FL[i]);
      if (E = nil) and (Errs[i] <> '') then
        F.Value := Errs[i]
      else if E <> nil then
        try
          F.Value:=E.Calc;
        except
          on Ex: Exception do
            F.Value := Ex.Message;
        end;
    end;
    DS.Post;
    DS.Next;
  end;
  DS.First;

  EB.Free;
  FL.Free;
  ClearList(EL);
  EL.Free;
  Errs.Free;
end;  *)

procedure FilterQuery(ARecordSet: TSsRecordSet);
var
  EB: TExpressionBuilder;
  E: TExpression;
  V: Variant;
  AftScr, BefScr: TDataSetNotifyEvent;
  RD: TReportData;
  DS: TDataSet;
begin
  RD := ARecordSet.RD;
  if Trim(RD.Filter) = '' then Exit;
  DS := ARecordSet.DataSet;
  EB := TExpressionBuilder.Create;
  EB.RecordSet := ARecordSet;
  EB.SkipLabels:=True;
  try
    E := EB.Build(RD.Filter);
  finally
    EB.Free;
  end;

  if E = nil then Exit;

  try
    AftScr := DS.AfterScroll;
    BefScr := DS.BeforeScroll;
    DS.AfterScroll := nil;
    DS.BeforeScroll := nil;
    DS.DisableControls;
    DS.First;
    while not DS.Eof do
    begin
      V := E.Calc;
      if VarIsBool(V) and (V = False) then
        DS.Delete
      else
        DS.Next;
    end;
  finally
    DS.First;
    DS.EnableControls;
    DS.AfterScroll:=AftScr;
    DS.BeforeScroll := BefScr;
    if BefScr <> nil then DS.BeforeScroll(DS);
    if AftScr <> nil then DS.AfterScroll(DS);
    E.Free;
  end;
end;

(*var
  EB: TExpressionBuilder;
  E: TExpression;
  V: Variant;
begin
  if Trim(aRD.Filter) = '' then Exit;
  EB := TExpressionBuilder.Create;
  EB.RD := aRD;
  EB.RDSet := DS;
  EB.DataSet := FormDS;
  EB.Form := aForm;
  EB.Session := SS;
  E := nil;
  try
    E := EB.Build(aRD.Filter);
    DS.First;
    while not DS.Eof do
    begin
      V := E.Calc;
      if VarIsBool(V) and (V = False) then
        DS.Delete
      else
        DS.Next;
    end;
  finally
    DS.First;
    FreeAndNil(E);
    EB.Free;
  end;
end;       *)

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

procedure BuildSortIndexes(RD: TReportData; DataSet: TDataSet);
var
  i: Integer;
  Col: TRpGridColumn;
  SCol: TRpGridSortData;
  DescFieldNames, FieldNames: String;
  DS: TSQLQuery;
begin
  if not CalcFieldExistsInSort(RD) then Exit;
  DS := TSQLQuery(DataSet);
  if DS.RecordCount = 0 then Exit;
  if not DS.IndexDefs.Updated then
  	DS.IndexDefs.Update;
  FieldNames := '';
  DescFieldNames := '';
  for i := 0 to RD.Grid.SortCols.Count - 1 do
  begin
    SCol := RD.Grid.SortCols[i];
    Col := SCol.Col;
    FieldNames := FieldNames + Col.FieldNameDS + ';';
    if SCol.Desc then
      DescFieldNames := DescFieldNames + Col.FieldNameDS + ';';
  end;
  FieldNames := Copy(FieldNames, 1, Length(FieldNames) - 1);
  DescFieldNames := Copy(DescFieldNames, 1, Length(DescFieldNames) - 1);
  if FieldNames <> '' then
  begin
	  DS.AddIndex('MY_INDEX', FieldNames, [], DescFieldNames);
  	DS.IndexName:='MY_INDEX';
    DS.First;
  end;
end;

function MyStrToFloat(const S: String; out E: Double): Boolean;
begin
  Result := TryStrToFloat(StringReplace(S, ' ', DefaultFormatSettings.DecimalSeparator, []), E);
end;

function MyStrToDate(const S: String; out D: TDateTime): Boolean;
begin
  Result := TryStrToDate(StringReplace(S, ' ', DefaultFormatSettings.DateSeparator, [rfReplaceAll]), D);
end;

function MyStrToTime(const S: String; out T: TDateTime): Boolean;
begin
  Result := TryStrToTime(StringReplace(S, ' ', DefaultFormatSettings.TimeSeparator, [rfReplaceAll]), T);
end;

function EscapeSQuotes(const S: String): String;
begin
  Result := StringReplace(S, #39, #39#39, [rfReplaceAll]);
end;

function UnEscapeSemicolon(const S: String): String;
begin
  Result := StringReplace(S, '#59', ';', [rfReplaceAll]);
end;

// Алгоритм взят с этого форума:
// http://www.planetaexcel.ru/forum/?PAGE_NAME=read&FID=8&TID=3483
function MathRound(X: Double; N: Integer): Double;
var
  b: Double;
begin
  If N < 0 Then
  begin
    b := Power(10, -N) * 1.0;
    Result := Round(X / b + X * 2E-16) * b;
  end
  Else
    Result := RoundTo(X + X * 2E-16,-N);

  If Abs(Result) = 0 Then Result := 0;
End;

// В линукс-версии 1.8.2-1.8.4 MyUtf8CompareText работает некорректно.
function MyUtf8CompareText(const S1, S2: String): PtrInt;
begin
  {$ifdef windows}
  Result := Utf8CompareText(S1, S2);
  {$else}
  Result := Utf8CompareStr(Utf8LowerCase(S1), Utf8LowerCase(S2));
  {$endif}
end;

function XmlToHtml(S: String): String;
begin
  S := XmlToStr(S);
  S := StringReplace(S, '#amp;', '&amp;', [rfReplaceAll]);
  S := StringReplace(S, '#quot;', '&quot;', [rfReplaceAll]);
  S := StringReplace(S, '#lt;', '&lt;', [rfReplaceAll]);
  S := StringReplace(S, '#gt;', '&gt;', [rfReplaceAll]);
  Result := S;
end;

function HtmlToXml(S: String): String;
begin
  S := StringReplace(S, '&amp;', '#amp;', [rfReplaceAll]);
  S := StringReplace(S, '&quot;', '#quot;', [rfReplaceAll]);
  S := StringReplace(S, '&lt;', '#lt;', [rfReplaceAll]);
  S := StringReplace(S, '&gt;', '#gt;', [rfReplaceAll]);
  Result := StrToXml(S);
end;

function VarTypeToStr(V: Variant): String;
begin
  Result := '';
  if VarIsStr(V) then Result := rsText
  else if VarIsBool(V) then Result := rsBoolean
  else if VarType(V) = varDate then Result := rsDate + '/' + rsTime
  else if VarIsNumeric(V) then Result := rsNumber;
end;

function GetComponentDataTypeStr(C: TdxField): String;
begin
  if (C is TdxEdit) or (C is TdxMemo) or (C is TdxFile) or (C is TdxComboBox) or
    (C is TdxFile) or (C is TdxDBImage) then Result := rsText
  else if (C is TdxCalcEdit) or (C is TdxCheckBox) or (C is TdxCounter) or
    (C is TdxLookupComboBox) or (C is TdxRecordId) then Result := rsNumber
  else if C is TdxDateEdit then Result := rsDate
  else if C is TdxTimeEdit then Result := rsTime
  else Result := rsUnknown;
end;

function GetObjFieldValue(SS: TSession; Obj: TdxField; aKey: Integer;
  FullPath: Boolean): String;
var
  SQL: String;
  n, p: Integer;
begin
  Result := '';
  if (aKey = 0) or (GetSourceTId(Obj) = 0) or (GetSourceFId(Obj) = 0) then Exit;

  SQL := SqlLookupSelect(SS, Obj, aKey);
  with SS.DBase.OpenDataSet(SQL) do
  begin
    if RecordCount > 0 then
    	Result := Fields[1].AsString;
    Free;
  end;
  if not FullPath and IsHierarchyObj(SS, Obj) then
  begin
    p := Pos('\', Result);
    n := 0;
    while p > 0 do
		begin
    	Delete(Result, 1, p);
      Inc(n);
      p := Pos('\', Result);
    end;
  	Result := DupeString('  ', n) + Result;
  end;
end;

function ExceptionInvalidValueForField(E: Exception): Boolean;
begin
  Result := (E is EDatabaseError) and (Copy(E.Message, 1, 24) = 'Invalid value for field ');
end;

function LookupObjectField(SS: TSession; ObjF: TdxObjectField;
  ForceObject: Boolean): TdxField;
var
  Fm: TdxForm;
  Obj: TdxField;
begin
  Result := nil;
  Fm := ObjF.Form;
  Obj := Fm.FindField(ObjF.ObjId);
  if Obj <> nil then
  begin
    Fm := SS.FormMan.FindForm(GetSourceTId(Obj));
    if Fm <> nil then
    begin
      Result := Fm.FindField(ObjF.FieldId);
      // Если поле объекта само является объектом, то находим его поле.
      if (Result <> nil) and (Result is TdxLookupComboBox) and ForceObject then
      begin
        Fm := SS.FormMan.FindForm(GetSourceTId(Result));
        if Fm <> nil then
        begin
          Result := Fm.FindField(GetSourceFId(Result));
        end;
      end;
    end;
  end;
end;

function GetComponentField(DS: TDataSet; C: TdxField): TField;
begin
  if C is TdxFile then
    Result := DS.FieldByName(FieldStr(C.Id) + 'd')
  else if C is TdxDBImage then
    Result := DS.FieldByName(FieldStr(C.Id) + 'src')
  else
    Result := DS.FieldByName(FieldStr(C.Id))
end;

function GetComponentFieldValue(DS: TDataSet; C: TdxField): Variant;
begin
  Result := GetComponentField(DS, C).Value;
end;

function GetComponentDataSetFieldName(C: TdxField): String;
begin
  if C is TdxFile then
    Result := FieldStr(C.Id) + 'd'
  else if C is TdxDBImage then
    Result := FieldStr(C.Id) + 'src'
  else
    Result := FieldStr(C.Id)
end;

function SetZeros(E: Double; N: Integer): String;
var
  i: Integer;
  S: String;
begin
  S := IntToStr(Trunc(E));
  Result := S;
  for i := Length(S) to N - 1 do
    Result := '0' + Result;
end;

function GetObjectFieldField(SS: TSession; ObjField: TdxObjectField): TdxField;
var
  Fm: TdxForm;
  C: TdxField;
begin
  Result := nil;
  Fm := ObjField.Form;
  C := Fm.FindField(ObjField.ObjId);
  if C <> nil then
  begin
    Fm := SS.FormMan.FindForm(TdxLookupComboBox(C).SourceTId);
    if Fm <> nil then
	    Result := Fm.FindField(ObjField.FieldId);
  end;
end;


function LookupComponent(SS: TSession; Fm: TdxForm; FlNm: String): TdxField;
var
  L: TStringList;
  i, TId: Integer;
  C: TdxField;
begin
  C := nil;
  Result := nil;

  L := TStringList.Create;
  try

  SplitStr(FlNm, '|', L);
  for i := 0 to L.Count - 1 do
  begin
    C := Fm.FindFieldByName(L[i]);
    if C = nil then Exit;
    if C is TdxLookupComboBox then
    begin
      TId := GetSourceTId(C);
      Fm := SS.FormMan.FindForm(TId);
      if Fm = nil then Exit;
    end;
  end;
  Result := C;

  finally
    L.Free;
  end;
end;

procedure SetupRpField(SS: TSession; C: TdxField; FlNm: String; pFld: PRpField);
var
  L: TStringList;
  i: Integer;
  Fm: TdxForm;
begin
  L := TStringList.Create;
  SplitStr(FlNm, '|', L);   // Все поля - объекты, кроме последнего
  for i := 0 to L.Count - 1 do
  begin
    Fm := C.Form;
    pFld^.TId:=Fm.Id;
    pFld^.FId:=C.Id;
    pFld^.Tp := GetTypeByComponent(C);
    if (pFld^.Tp = flObject) and (i < L.Count - 1) then
    begin
      pFld^.Src:=NewRpField;
      pFld^.Src^.Parent := pFld;
      pFld := pFld^.Src;
      Fm := SS.FormMan.FindForm(GetSourceTId(C));
      if Fm <> nil then
        C := Fm.FindFieldByName(L[i + 1]);
      if (Fm = nil) or (C = nil) then
      begin
        Dispose(pFld^.Src);
        pFld^.Src := nil;
        Break;
      end;
    end;
  end;
  L.Free;
end;

function IIF(Condition, Value1, Value2: Variant): Variant;
begin
  if Condition = Null then
    raise Exception.Create(rsIIFNullDetect)
  else if not VarIsBool(Condition) then
    raise Exception.Create(rsIIFParamNotLogic);
  if Condition then
    Result := Value1
  else
    Result := Value2;
end;

function TableStr(Id: Integer): String;
begin
  Result := 't' + IntToStr(Id);
end;

function FieldStr(Id: Integer): String;
begin
  Result := 'f' + IntToStr(Id);
end;

function IsHierarchyObj(SS: TSession; Obj: TdxField): Boolean;
var
  Fm: TdxForm;
begin
  Result := False;
  Fm := SS.FormMan.FindForm(GetSourceTId(Obj));
  if Fm <> nil then
  	Result := Fm.ParentField > 0;
end;

function RpFieldToFormField(SS: TSession; RD: TReportData; FId: Integer): TdxField;
var
  i, j: Integer;
  Fm: TdxForm;
  rF: TRpField;
begin
  Result := nil;
  if RD.Sources.Count = 0 then Exit;
  for i := 0 to RD.Sources[0]^.Fields.Count - 1 do
    if RD.Sources[0]^.Fields[i]^.Id = FId then
      for j := 0 to RD.Sources.Count - 1 do
      begin
        rF := RD.Sources[j]^.Fields[i]^;
        if not rF.Zero then
        begin
          rF := GetLowField(@rF)^;
          Fm := SS.FormMan.FindForm(rF.TId);
          if Fm <> nil then
            Exit(Fm.FindField(rF.FId));
        end;
      end;
end;

function CreateReportForm(SS: TSession; RD: TReportData; out SQL: String
  ): TdxForm;

  function CreateNumberField(Fm: TdxForm; LowF: TRpField;
    const FieldName, Value: String; var n: Integer): String;
  var
    C, Cmp: TdxCalcEdit;
    S: String;
    Frm: TdxForm;
  begin
    C := TdxCalcEdit.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    C.Parent := Fm;
    //Fm.Controls.Add(C);

    if LowF.TId > 0 then
    begin
      Frm := SS.FormMan.FindForm(LowF.TId);
      Cmp := TdxCalcEdit(Frm.findField(LowF.FId));
      C.Precission := Cmp.Precission;
      C.GroupDigits := Cmp.GroupDigits;
    end
    else
      C.GroupDigits := True;

    if Value > '' then
    begin
      S := StringReplace(Value, DefaultFormatSettings.ThousandSeparator, '', [rfReplaceAll]);
      S := StringReplace(S, DefaultFormatSettings.DecimalSeparator, '.', []);
      Result := Format('%s as f%d,', [S, n]);
    end
    else Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateTotalNumberField(Fm: TdxForm; const FieldName, Value: String;
    var n: Integer): String;
  var
    S: String;
    p: SizeInt;
    C: TdxCalcEdit;
  begin
    S := StringReplace(Value, DefaultFormatSettings.ThousandSeparator, '', [rfReplaceAll]);
    S := StringReplace(S, DefaultFormatSettings.DecimalSeparator, '.', []);
    p := Pos('.', S);
    C := TdxCalcEdit.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    C.GroupDigits := True;
    C.Parent := Fm;
    //Fm.Controls.Add(C);
    if p > 0 then C.Precission := Length(S) - p;
    if S > '' then Result := Format('%s as f%d,', [S, n])
    else Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateDateField(Fm: TdxForm; const FieldName, Value: String;
    var n: Integer): String;
  var
    C: TdxDateEdit;
    DT: TDateTime;
  begin
    C := TdxDateEdit.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    C.Parent := Fm;
    //Fm.Controls.Add(C);
    if (Value > '') and TryStrToDate(Value, DT) then
      Result := Format('(CAST(''%s'' AS DATE)) as f%d,', [Date2Str(DT), n])
    else
      Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateTimeField(Fm: TdxForm; LowF: TRpField;
    const FieldName, Value: String; var n: Integer): String;
  var
    C: TdxTimeEdit;
    Frm: TdxForm;
    Cmp: TdxTimeEdit;
    DT: TDateTime;
  begin
    Frm := SS.FormMan.FindForm(LowF.TId);
    Cmp := TdxTimeEdit(Frm.FindField(LowF.FId));
    C := TdxTimeEdit.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    C.TimeFormat := Cmp.TimeFormat;
    C.Parent := Fm;
    //Fm.Controls.Add(C);
    if (Value > '') and TryStrToTime(Value, DT) then
      Result := Format('(CAST(''%s'' AS TIME)) as f%d,', [Time2Str(DT), n])
    else
      Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateTotalTimeField(Fm: TdxForm; const FieldName, Value: String;
    var n: Integer): String;
  var
    C: TdxTimeEdit;
    DT: TDateTime;
  begin
    C := TdxTimeEdit.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    C.TimeFormat:=ttHHMMSS;
    C.Parent := Fm;
    //Fm.Controls.Add(C);
    if (Value > '') and TryStrToTime(Value, DT) then
      Result := Format('(CAST(''%s'' AS TIME)) as f%d,', [Time2Str(DT), n])
    else
      Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateCounterField(Fm: TdxForm; const FieldName, Value: String;
    var n: Integer): String;
  var
    C: TdxCounter;
  begin
    C := TdxCounter.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    C.Parent := Fm;
    //Fm.Controls.Add(C);
    if Value > '' then Result := Format('%s as f%d,', [Value, n])
    else Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateRecIdField(Fm: TdxForm; const FieldName, Value: String;
    var n: Integer): String;
  var
    C: TdxRecordId;
  begin
    C := TdxRecordId.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    if Value > '' then Result := Format('%s as f%d,', [Value, n])
    else Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateObjectField(Fm: TdxForm; LowF: TRpField;
    const FieldName, Value: String; var n: Integer): String;
  var
    C, Cmp: TdxLookupComboBox;
    Frm: TdxForm;
    S: String;
  begin
    Frm := SS.FormMan.FindForm(LowF.TId);
    Cmp := TdxLookupComboBox(Frm.FindField(LowF.FId));
    C := TdxLookupComboBox.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    C.SourceTId := Cmp.SourceTId;
    C.SourceFId := Cmp.SourceFId;
    C.Parent := Fm;
    //Fm.Controls.Add(C);
    if Value > '' then
    begin
      S := GetObjFieldValue(SS, C, StrToInt(Value), True);
      Result := Format('%s as f%d,', [Value, n]) +
        Format('(CAST(''%s'' AS VARCHAR(%d))) as f%dl,', [S, Utf8Length(S), n])
    end
    else Result := Format('null as f%0:d,null as f%0:dl,', [n]);
    Inc(n);
  end;

  function CreateCheckBoxField(Fm: TdxForm; const FieldName, Value: String;
    var n: Integer): String;
  var
    C: TdxCheckBox;
  begin
    C := TdxCheckBox.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    C.Parent := Fm;
    //Fm.Controls.Add(C);
    if Value > '' then Result := Format('%s as f%d,', [Value, n])
    else Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateTextField(Fm: TdxForm; const FieldName, Value: String;
    var n: Integer): String;
  var
    C: TdxEdit;
  begin
    C := TdxEdit.Create(Fm);
    C.Id := n;
    C.FieldName := FieldName;
    C.Parent := Fm;
    //Fm.Controls.Add(C);
    if Value > '' then
      Result := Format('(CAST(''%s'' as VARCHAR(%d))) as f%d,', [Value, Utf8Length(Value), n])
    else
      Result := Format('null as f%d,', [n]);
    Inc(n);
  end;

  function CreateTotalField(Fm: TdxForm; TF: TRpTotalData;
    var n: Integer): String;
  var
    S: String;
    pF: PRpField;
    Tp: TRpFieldType;
    //pCF: PRpCalcField;
    i: Integer;
  begin
    S := TF.FieldNameDS;
    pF := nil; //pCF := nil;
    if TF.Func = tfCount {in [tfCount, tfSum, tfAvg]} then
      Tp := flNumber
    else
    begin
      i := RD.IndexOfNameDS(S);
      Tp := RD.GetFieldType(i);
      pF := RD.TryGetRpField(i);
      {if Copy(S, 1, 1) = 'f' then
      begin
        System.Delete(S, 1, 1);
        pF := GetLowField(RD.FindField(StrToInt(S)));
        Tp := pF^.Tp;
      end
      else
      begin
        if S > '' then
        begin
          System.Delete(S, 1, 2);
          pCF := RD.CalcFields.FindField(StrToInt(S));
          Tp := pCF^.Tp;
        end;
      end;}
    end;

    case Tp of
      flNumber:
        begin
          if pF <> nil then Result := CreateNumberField(Fm, pF^, TF.Caption, TF.Value, n)
          else Result := CreateTotalNumberField(Fm, TF.Caption, TF.Value, n);
        end;
      flDate: Result := CreateDateField(Fm, TF.Caption, TF.Value, n);
      flTime:
        begin
          if pF <> nil then Result := CreateTimeField(Fm, pF^, TF.Caption, TF.Value, n)
          else Result := CreateTotalTimeField(Fm, TF.Caption, TF.Value, n);
        end;
      flCounter: Result := CreateCounterField(Fm, TF.Caption, TF.Value, n);
      flRecId: Result := CreateRecIdField(Fm, TF.Caption, TF.Value, n);
      else Result := CreateTextField(Fm, TF.Caption, TF.Value, n);
    end;
  end;

  function EmptyStrIfNotParams(const S: String): String;
  var
    i: Integer;
  begin
    Result := '';
    for i := 1 to Length(S) do
      if S[i] <> ';' then Exit(S);
  end;

  function RDParamsToFields(Fm: TdxForm): String;
  var
    i, n: Integer;
    F, LowF: TRpField;
    SL: TStringList;
    Value, S, S1, S2: String;
    p: SizeInt;
    T: TRpTotalData;
  begin
    n := 1;
    Result := '';
    SL := TStringList.Create;
    if RD.Sources.Count > 0 then
    begin
      for i := 0 to RD.Sources[0]^.Fields.Count - 1 do
      begin
        F := RD.Sources[0]^.Fields[i]^;
        LowF := GetLowField(@F)^;
        if F.Param then
        begin
          // Поле_text содержит текстовое представление всех значений параметра

          Result := Result +
            CreateTextField(Fm, F.Name + '_text', EmptyStrIfNotParams(F.ValueStr), n) +
            CreateCheckBoxField(Fm, F.Name + '_not', Bool2Str(F.No), n) +
            CreateCheckBoxField(Fm, F.Name + '_null', Bool2Str(F.Nul), n);
          SplitStr(F.Value, ';', SL);
          // Поля создаются только для одного значения параметра, остальные игнорируются.
          if SL.Count > 0 then Value := SL[0]
          else Value := '';
          if LowF.Tp in [flNumber, flDate, flTime, flCounter, flRecId] then
          begin
            {if (LowF.Tp = flDate) and DateCodeToPeriod(Value, S1, S2) then
              Value := S1 + ' .. ' + S2
            else
            begin }
              S := Value;
              if S = '' then S := ' .. ';
              p := Pos(' .. ', S);
              S1 := Copy(S, 1, p - 1);
              S2 := Copy(S, p + 4, 200);
            //end;
            Result := Result + CreateTextField(Fm, F.Name, Value, n);
            case LowF.Tp of
              flNumber: Result := Result + CreateNumberField(Fm, LowF, F.Name + '_begin', S1, n) +
                CreateNumberField(Fm, LowF, F.Name + '_end', S2, n);
              flDate: Result := Result + CreateDateField(Fm, F.Name + '_begin', S1, n) +
                CreateDateField(Fm, F.Name + '_end', S2, n);
              flTime: Result := Result + CreateTimeField(Fm, LowF, F.Name + '_begin', S1, n) +
                CreateTimeField(Fm, LowF, F.Name + '_end', S2, n);
              flCounter: Result := Result + CreateCounterField(Fm, F.Name + '_begin', S1, n) +
                CreateCounterField(Fm, F.Name + '_end', S2, n);
              flRecId: Result := Result + CreateRecIdField(Fm, F.Name + '_begin', S1, n) +
                CreateRecIdField(Fm, F.Name + '_end', S2, n);
            end;
          end
          else if LowF.Tp = flObject then
            Result := Result + CreateObjectField(Fm, LowF, F.Name, Value, n)
          else if LowF.Tp = flBool then
            Result := Result + CreateCheckBoxField(Fm, F.Name, Value, n)
          else
          begin
            Result := Result + CreateTextField(Fm, F.Name, UnEscapeSemicolon(Value), n);
          end;
          //
        end;
      end;
    end;

    for i := 0 to RD.Totals.Count - 1 do
    begin
      T := RD.Totals[i];
      if RD.IndexOfNameDS(T.FieldNameDS) < 0 then Continue;
      Result := Result + CreateTotalField(Fm, T, n);
    end;
    Result := Copy(Result, 1, Length(Result) - 1);
    SL.Free;
  end;

var
  Q: TdxQueryGrid;
begin
  Result := TdxForm.Create(nil);
  Result.FormCaption := rsReportWindow;
  Result.ViewType := vtGridOnly;
  Result.CalcFields.Assign(RD.PrintFields);
  SQL := RDParamsToFields(Result);
  Q := TdxQueryGrid.Create(Result);
  with Q do
  begin
    Name := 'QGrid';
    //ManualRefresh := True;
    Id := RD.Id;
    Parent := Result;
  end;
  //Result.Controls.Add(Q);
end;

function MakeNumberFormat(Prec: Integer; Group, PadZeros: Boolean): String;
begin
  Result := '0';
  if Prec > 0 then
  begin
    if PadZeros then
      Result := '0.' + DupeString('0', Prec)
    else
      Result := '0.' + DupeString('#', Prec)
  end;
  if Group then Result := ',' + Result;
end;

// Взято из fphttpclient
function DecodeURLElement(const S: AnsiString): AnsiString;

var
  i,l,o : Integer;
  c: AnsiChar;
  p : pchar;
  h : string;

begin
  l := Length(S);
  if l=0 then exit;
  SetLength(Result, l);
  P:=PChar(Result);
  i:=1;
  While (I<=L) do
    begin
    c := S[i];
    if (c<>'%') then
      begin
      P^:=c;
      Inc(P);
      end
    else if (I<L-1) then
      begin
      H:='$'+Copy(S,I+1,2);
      o:=StrToIntDef(H,-1);
      If (O>=0) and (O<=255) then
        begin
        P^:=char(O);
        Inc(P);
        Inc(I,2);
        end;
      end;
    Inc(i);
  end;
  SetLength(Result, P-Pchar(Result));
end;

function GetGridColumnAlignment(SS: TSession; Fm: TdxForm; Col: TdxColumn
  ): TAlignment;
var
  F: TdxField;
begin
  if Col.AutoAlignment then
  begin
    F := Fm.FindField(Col.Id);
    if F is TdxObjectField then F := GetObjectFieldField(SS, TdxObjectField(F));
    if (F is TdxCalcEdit) or (F is TdxDateEdit) or (F is TdxTimeEdit) or
      (F is TdxCounter) or (F is TdxRecordId) then Result := taRightJustify
    else if (F is TdxCheckBox) or (F is TdxDBImage) then Result := taCenter
    else Result := taLeftJustify;
  end
  else
    Result := Col.Alignment;
end;

function GetGridColumnLayout(Col: TdxColumn): TTextLayout;
begin
  if Col.AutoLayout then
    Result := tlCenter
  else
    Result := Col.Layout;
end;

function GetRpGridColumnAlignment(SS: TSession; RD: TReportData;
  Col: TRpGridColumn): TAlignment;
var
  i: Integer;
begin
  if Col.AutoAlignment then
  begin
    i := RD.IndexOfNameDS(Col.FieldNameDS);
    case RD.GetFieldType(i) of
      flNumber, flCounter, flDate, flTime, flObject: Result := taRightJustify;
      flBool, flImage: Result := taCenter;
      else Result := taLeftJustify;
    end;
    {RD.FindFieldByColumn(Col, pF, pCF);
    if pF <> nil then
      case GetRealRpFieldType(RD, pF) of
        flNumber, flCounter, flDate, flTime, flObject: Result := taRightJustify;
        flBool: Result := taCenter;
        else Result := taLeftJustify;
      end
    else
      case pCF^.Tp of
        flNumber, flDate, flTime: Result := taRightJustify;
        else Result := taLeftJustify;
      end;  }
  end
  else
    Result := Col.Alignment;
end;

function GetRpGridColumnLayout(Col: TRpGridColumn): TTextLayout;
begin
  if Col.AutoLayout then
    Result := tlCenter
  else
    Result := Col.Layout;
end;

function Date2Str(D: TDateTime): String;
var
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DateSeparator := '-';
  FS.ShortDateFormat := 'YYYY/MM/DD';
  Result := DateToStr(D, FS);
end;

function Str2Date(const S: String): TDateTime;
begin
  Result := StrToDate(S, 'YYYY/MM/DD', '-');
end;

function Time2Str(T: TDateTime): String;
begin
  Result := TimeToStr(T, DefaultSQLFormatSettings);
end;

function MakeJsonObjectString(Items: array of const): String;
var
  Obj: TJSONObject;
begin
  Obj := TJsonObject.Create(Items);
  Result := Obj.AsJSON;
  Obj.Free;
end;

function MakeJsonErrString(Code: Integer; const Msg: String): String;
begin
  Result := MakeJsonObjectString(['code', Code, 'error', Msg]);
end;

function GetTopControl(Cont: TdxWinControl): TdxComponent;
var
  i, TabOrd, j: Integer;
  C: TdxControl;
begin
  Result := nil;
  TabOrd := 0;

  for i := 0 to Cont.ControlCount - 1 do
  	for j := 0 to Cont.ControlCount - 1 do
    begin
      C := Cont.Controls[j];
      if C.TabOrder = TabOrd then
      begin
        if C is TdxWinControl then Result := GetTopControl(TdxWinControl(C));
        if Result = nil then
        begin
          if C.TabStop then Result := C
          else
          begin
            Inc(TabOrd);
            Break;
          end;
        end;
        Exit;
      end;
    end;
end;

function TruncTime(Fmt: TdxTimeFormat; DT: TDateTime): TDateTime;
var
  n: Word;
begin
  n := RecodeLeaveFieldAsIs;
  DT := DT - Trunc(DT);     // Оставляем только время
  case Fmt of
    ttHH: Result := RecodeTime(DT, n, 0, 0, 0);
    ttHHMM: Result := RecodeTime(DT, n, n, 0, 0);
    ttHHMMSS: Result := RecodeTime(DT, n, n, n, 0);
  end;
end;

function GenerateId: String;
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := GUIDToString(GUID);
  Delete(Result, 1, 1);
  SetLength(Result, Length(Result) - 1);
end;

{function ColorToHtmlColor(C: TColor): String;
var
  Col: LongInt;
begin
  Col := ColorToRGB(C);
  Result := 'rgb(' + IntToStr(Red(Col)) + ',' +
    IntToStr(Green(Col)) + ',' + IntToStr(Blue(Col)) + ')';
end;      }

function StrToHtml(const S: String; ReplaceNewLines: Boolean; Nbsp: Boolean
  ): String;
var
  i, L: Integer;
begin
  Result := '';
  L := Length(S);
  i := 1;
  while i <= L do
  begin
    if S[i] = '&' then
      Result := Result + '&amp;'
    else if S[i] = '"' then
      Result := Result + '&quot;'
    else if S[i] = '<' then
      Result := Result + '&lt;'
    else if S[i] = '>' then
      Result := Result + '&gt;'
    //else if S[i] = '-' then
    //  Result := Result + '&#8209;'
    else if (S[i] = #13) and ReplaceNewLines then
    begin
      while (i <= L) and (S[i] = #13) do
      begin
        Result := Result + '<br>';
        Inc(i);
      end;
      if S[i] <> #10 then Continue;
    end
    else if (S[i] = #10) and ReplaceNewLines then
      Result := Result + '<br>'
    else if Nbsp and (S[i] = ' ') then
      Result := Result + '&nbsp;'
    else
      Result := Result + S[i];
    Inc(i);
  end;
end;

{function CheckActionEnabled(B: TdxButton): Boolean;

  function _Check(L: TActionLines): Boolean;
  var
    i: Integer;
    AL: TActionLine;
  begin
    Result := True;
    for i := 0 to L.Count - 1 do
    begin
      AL := L[i];
      if AL.Kind in [alkIf, alkElseIf, alkElse] then
      begin
        Result := _Check(AL.Lines);
        if not Result then Exit;
      end
      else if AL.Kind = alkAction then
      begin
        if AL.Action is TActionCustom then Exit(False);
      end;
    end;
  end;

var
  AR: TActionRunner;
begin
  AR := TActionRunner.Create;
  AR.Load(B.ActionOnClick);
  Result := _Check(AR.Lines);
  AR.Free;
end;   }

{function ReplaceBr(const S, Br: String): String;
var
  i, L: Integer;
  Ch: Char;
begin
  if Br = '' then Exit(S);

  Result := '';
  L := Length(S);
  Ch := #0;
  for i := 1 to L do
  begin
    if S[i] in [#13, #10] then
    begin
      if not (Ch in [#13, #10]) then
        Result := Result + Br
    end
    else
      Result := Result + S[i];
    Ch := S[i];
  end;
end;   }

procedure ChangeScaleControl(C: TdxControl; ToPPI, FromPPI: Integer);

  function ScaleV(X: Integer): Integer;
  begin
    Result := MulDiv(X, ToPPI, FromPPI);
  end;

  procedure ChangeScaleGrid(G: TdxCustomGrid);
  var
    i: Integer;
    Col: TdxColumn;
  begin
    //G.DefaultRowHeight := ScaleV(G.DefaultRowHeight);
    {if not G.TitleFont.IsEqual(G.Font) then
      G.TitleFont.Height := ScaleV(G.TitleFont.Height);}
    //if not G.Buttons.IsParentFont then
      G.ButtonFont.Size := ScaleV(G.ButtonFont.Size);
    G.ButtonSize := ScaleV(G.ButtonSize);
    for i := 0 to G.Columns.Count - 1 do
    begin
      Col := G.Columns[i];
      Col.Width := ScaleV(Col.Width);
      {if not G.Font.IsEqual(Col.Font) then
        Col.Font.Height := ScaleV(Col.Font.Height);
      if not G.TitleFont.IsEqual(Col.Title.Font) then
        Col.Title.Font.Height := ScaleV(Col.Title.Font.Height);  }
    end;
  end;

  procedure ChangeScalePivotFields(Col: TFieldCollection);
  var
    i: Integer;
    FI: TFieldItem;
  begin
    for i := 0 to Col.Count - 1 do
    begin
      FI := Col[i];
      FI.Font.Size := ScaleV(FI.Font.Size);
      FI.FixedFont.Size := ScaleV(FI.FixedFont.Size);
      FI.TotalFont.Size := ScaleV(FI.TotalFont.Size);
      FI.TotalFixedFont.Size := ScaleV(FI.TotalFixedFont.Size);
      FI.Width := ScaleV(FI.Width);
      FI.Height := ScaleV(FI.Height);
      FI.TotalWidth := ScaleV(FI.TotalWidth);
    end;
  end;

  procedure ChangeScalePivotGrid(G: TdxPivotGrid);
  begin
    G.FixedFont.Size := ScaleV(G.FixedFont.Size);
    G.SelectedFont.Size := ScaleV(G.SelectedFont.Size);
    G.GrandTotalFixedFont.Size := ScaleV(G.GrandTotalFixedFont.Size);
    G.GrandTotalFont.Size := ScaleV(G.GrandTotalFont.Size);
    G.Indent := ScaleV(G.Indent);
    G.GrandTotalWidth := ScaleV(G.GrandTotalWidth);
    ChangeScalePivotFields(G.ColFields);
    ChangeScalePivotFields(G.RowFields);
    ChangeScalePivotFields(G.DataFields);
  end;

begin
  C.Left:=ScaleV(C.Left);
  C.Top:=ScaleV(C.Top);
  C.Width:=ScaleV(C.Width);
  C.Height:=ScaleV(C.Height);
  if not C.ParentFont or (C is TdxForm) or (C is TdxGrid) then
  begin
    C.Font.Size:=ScaleV(C.Font.Size);
  end;
  if C is TdxCustomGrid then ChangeScaleGrid(TdxCustomGrid(C))
  else if C is TdxPivotGrid then ChangeScalePivotGrid(TdxPivotGrid(C));
end;

procedure ScaleReport(RD: TReportData; FromPPI, ToPPI: Integer);

  function ScaleV(X: Integer): Integer;
  begin
    Result := MulDiv(X, ToPPI, FromPPI);
  end;

var
  G: TRpGrid;
  i: Integer;
  Col: TRpGridColumn;
begin
  if FromPPI = ToPPI then Exit;

  G := RD.Grid;
  G.DefaultRowHeight:=ScaleV(G.DefaultRowHeight);
  G.Font.Size:=ScaleV(G.Font.Size);
  G.TitleFont.Size:=ScaleV(G.TitleFont.Size);
  for i := 0 to G.ColumnCount - 1 do
  begin
    Col := G.Columns[i];
    Col.Width := ScaleV(Col.Width);
    Col.Font.Size := ScaleV(Col.Font.Size);
    Col.TitleFont.Size := ScaleV(Col.TitleFont.Size);
  end;
end;

procedure ScaleForm(Fm: TdxForm; DesignTimePPI: Integer);

  procedure _DoScale(WC: TdxWinControl);
  var
    i: Integer;
    C: TdxControl;
  begin
    for i := 0 to WC.ControlCount - 1 do
    begin
      C := WC.Controls[i];
      //if (C is TGridButtons) or (C is TSpeedButton) then Continue;
      ChangeScaleControl(C, 96, DesignTimePPI);
      if C is TdxWinControl then
        _DoScale(TdxWinControl(C));
    end;
  end;

begin
  if DesignTimePPI = 96 then Exit;
  ChangeScaleControl(Fm, 96, DesignTimePPI);
  _DoScale(Fm);
end;

{procedure ScaleForms(FMan: TFormManager; DesignTimePPI: Integer);
var
  i: Integer;
begin
  if 96 = DesignTimePPI then Exit;
  for i := 0 to FMan.FormCount - 1 do
    ScaleForm(FMan.Forms[i], DesignTimePPI);
end;

procedure ScaleReports(RMan: TReportManager; FromPPI, ToPPI: Integer);
var
  i: Integer;
begin
  if FromPPI = ToPPI then Exit;
  for i := 0 to RMan.ReportCount - 1 do
    ScaleReport(RMan.Reports[i], FromPPI, ToPPI);
end;  }

procedure SplitComponentName(const AName: String; out ANameStr: String;
  out ANameNum: Integer);
var
  L, p: Integer;
begin
  L := Length(AName);
  p := L;
  while (p > 1) do
  begin
    if not (AName[p] in ['0'..'9']) then Break;
    Dec(p);
  end;
  ANameStr := Copy(AName, 1, p);
  if (p < L) and TryStrToInt(Copy(AName, p + 1, L), ANameNum) then
  else ANameNum := 0;
end;

procedure SkipBOM(St: TStream);
var
  MarkHolder: Cardinal;
begin
  if (St.Size > 3) and (St.Position = 0) then
  begin
	  MarkHolder := St.ReadDWord;
  	if (MarkHolder and $00FFFFFF) = $00BFBBEF then
    	St.Position := 3
	  else
  	  St.Position := 0;
  end;
end;

procedure ParamsToArray(Params: String; var Arr: TStringArray);
var
  p, Size: Integer;
begin
  Size := 0;
  while Length(Params) > 0 do
  begin
    Trim(Params);
    if Params[1] = '"' then
    begin
      Delete(Params, 1, 1);
      p := Pos('"', Params);
      if p = 0 then Break;
    end
    else
    begin
      p := Pos(' ', Params);
      if p = 0 then p := Length(Params) + 1;
    end;
    SetLength(Arr, Size + 1);
    Arr[Size] := Copy(Params, 1, p - 1);
    Delete(Params, 1, p);
  end;
end;

function RemoveQuotes(const S: String): String;
var
  Len: Integer;
begin
  Len := Length(S);
  if (Copy(S, 1, 1) = '"') and (Copy(S, Len, 1) = '"') then
  	Result := Copy(S, 2, Len - 2)
  else
  	Result := S;
end;

function ShellExec(const Operation, FileName, Params, WorkDir: String;
  ShowCmd: LongInt): Boolean;
var
  OutS, S: String;
  Arr: TStringArray;
begin
  {$ifdef windows}
  Result := ShellExecute(0, PChar(Operation), PChar(Utf8ToWinCP(FileName)),
    PChar(Utf8ToWinCP(Params)), PChar(Utf8ToWinCP(WorkDir)), ShowCmd) > 32;
  {$else}
  S := RemoveQuotes(FileName);
  ParamsToArray(Params, Arr);
  Result := RunCommandInDir(WorkDir, S, Arr, OutS, [poNoConsole]);
  SetLength(Arr, 0);
  {$endif}
end;

function GetBuildDate: TDateTime;
var
  FS: TFormatSettings;
begin
  {FS := DefaultFormatSettings;
  FS.DateSeparator:='.';
  FS.ShortDateFormat:='yy.m.d';
  Result := StrToDate(APP_VERSION, FS);}
  FS.DateSeparator := '/';
  FS.ShortDateFormat := 'y/m/d';
  Result := StrToDate({$INCLUDE %DATE}, FS);
end;

procedure DebugFile(const FileName: String; Value: Variant);
var
  mode: Integer;
  S: String;
begin
  S := VarToStr(Value) + LineEnding;
  if not FileExists(FileName) then mode := fmCreate
  else mode := fmOpenWrite + fmShareDenyNone;
  with TFileStream.Create(FileName, mode) do
    try
      Position := Size;
      WriteBuffer(Pointer(S)^, Length(S));
    finally
      Free;
    end;
end;

procedure TryStrToColor(const ColorStr: String; out Color: TColor);
begin
  if ColorStr = '' then
    Color := clNone
  else
    Color := StringToColor(ColorStr);
end;

function DecodeCellText(const S: String): String;
begin
  Result := StringReplace(S, '&00A6', '|', [rfReplaceAll]);
  Result := StringReplace(Result, '&003B', ';', [rfReplaceAll]);
end;

function le2br(const S: String): String;
begin
  Result := StringReplace(S, LineEnding, '<br>', [rfReplaceAll]);
end;

function RunAction(const ActionData: String; ARecordSet: TSsRecordSet): Variant;
begin
  Result := Null;
  if ActionData = '' then Exit;

  with TActionRunner.Create do
  try
    RS := ARecordSet;
    Load(ActionData);
    Result := Run;
  finally
    Free;
  end;
end;

function ExceptionToHtml(E: Exception): String;
begin
  if E is EPSException then
    Result := EPSExceptionToString(EPSException(E))
  else
    Result := Format(rsExceptionMsg, [E.Message + LineEnding + LineEnding, E.ClassName]);
  Result := le2br(Result);
end;

(*function FormLookupFieldValue(RS: TSsRecordSet; const aFieldName: String): Variant;
var
  SL: TStringList;
  Fm: TdxForm;
  i: Integer;
  C: TdxField;
  DS: TDataSet;
  V: Variant;
  Tmp: String;
  Cbx: TdxLookupComboBox;
begin
  Result := Null;
  if aFieldName = '' then raise Exception.Create(rsFieldNameEmpty);

  SL := TStringList.Create;
  SplitStr(aFieldName, '|', SL);
  Fm := RS.Form;
  DS := RS.DataSet;

  try

  C := Fm.FindFieldByName(SL[0]);
  if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [aFieldName]);

  for i := 0 to SL.Count - 1 do
  begin
    if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [aFieldName]);
    if (C is TdxLookupComboBox) and (i < SL.Count - 1) then
    begin
      V := DS.FieldByName(FieldStr(C.Id)).Value;
      Result := V;
      if V = Null then Break;
      Fm := RS.Session.FormMan.FindForm(GetSourceTId(C));
      if Fm = nil then Exit;
      if DS <> RS.DataSet then FreeAndNil(DS);
      Cbx := TdxLookupComboBox(C);

      C := Fm.FindFieldByName(SL[i+1]);
      if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [aFieldName]);
      // Если поле объекта отображается в компоненте, то значение берется из
      // компонента без запроса к базе.
      if (C.Id = Cbx.SourceFId) and (SL.Count = 2) then
      begin
        Result := RS.DataSet.FieldByName(FieldStr(Cbx.Id) + 'l').Value;
        Break;
      end
      else if C is TdxRecordId then Break;

      Tmp := SqlSelectGroups(RS.Session, Fm.Id, True);
      if Tmp <> '' then Tmp := '(' + Tmp + ')'
      else Tmp := TableStr(Fm.Id);

      DS := RS.Session.DBase.OpenDataSet('select ' + GetComponentDataSetFieldName(C)
        + ' from ' + Tmp + ' where id=' + VarToStr(V));
    end
    else
    begin
      Result := GetComponentFieldValue(DS, C);
      Break;
    end;
  end;

  finally
    if DS <> RS.DataSet then FreeAndNil(DS);
    SL.Free;
  end;
end;          *)

function CanEnabledControl(C: TdxControl): Boolean;
begin
  Result := C.Enabled;
  if Result and (C.Parent <> nil) then Result := CanEnabledControl(C.Parent);
end;

function SQLSelect(SS: TSession; const SQL: String): TdxSQLQuery;
begin
  Result := TdxSQLQuery.Create(SS, nil, SQL);
  try
    Result.Open
  except
  	Result.Free;
    raise;
  end;
end;

procedure SQLExecute(SS: TSession; const SQL: String);
begin
  SS.DBase.Execute(SQL);
end;

function NowTime: Integer;
const
  TIME_START = 32874; // 01.01.1990
begin
  Result := SecondsBetween(Now, TIME_START);
end;

function GetFormHRef(Fm: TdxForm): String;
begin
  Result := '?fm=' + IntToStr(Fm.Id) + IIF(Fm.ViewType = vtSimpleForm, '&rec=1', '');
end;

procedure CheckVisiblePageControls(SS: TSession; Fm: TdxForm);
var
  i, j: Integer;
  C: TdxComponent;
  Pages: TdxPageControl;
  Tab: TdxTabSheet;
begin
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if not (C is TdxPageControl) then Continue;

    Pages := TdxPageControl(C);
    for j := 0 to Pages.ControlCount - 1 do
    begin
      Tab := Pages.Pages[j];
      Tab.ControlVisible := SS.UserMan.CheckControlVisible(SS.RoleId, Fm.Id, Tab.Name);
    end;
    if Pages.ActivePageIndex >= 0 then
    begin
      Tab := Pages.Pages[Pages.ActivePageIndex];
      if not Tab.ControlVisible or not Tab.TabVisible then
        Pages.SetVisiblePage;
    end;
  end;
end;

function GetListSourceField(SS: TSession; Obj: TdxCustomComboBox): TdxField;
var
  Fm: TdxForm;
begin
  Result := nil;
  if (Obj.SourceTId = 0) or (Obj.SourceFId = 0) then Exit;
  Fm := SS.FormMan.FindForm(Obj.SourceTId);
  Result := Fm.FindField(Obj.SourceFId);
end;

function FieldExists(const FieldName, E: String): Boolean;
var
  S, SS: String;
  L, i, p: Integer;
  IsField: Boolean;
begin
  Result := False;
  S := Utf8LowerCase(FieldName);
  L := Length(E);
  IsField := False;
  for i := 1 to L do
  begin
    if E[i] = '[' then
    begin
      IsField := True;
      p := i + 1;
    end
    else if IsField and (p = i) and (E[i] in ['!', ':']) then
      p := i + 1
    else if IsField and ((E[i] = ']') or (i = L)) then
    begin
      SS := Utf8LowerCase(Copy(E, p, i - p));
      if S = SS then Exit(True);
      IsField := False;
    end;
  end;
end;

function FormExistsInExpr(const FormName, Expr: String): Boolean;
var
  E, Nm: String;
begin
  E := Trim(Utf8LowerCase(Expr));
  if E = '' then Exit(False);
  Nm := Utf8LowerCase(FormName);
  Result := (Pos('''' + Nm + '''', E) > 0) or (Pos('"' + Nm + '"', E) > 0);
end;

// Эта версия функции для проверки присутствия поля формы в выходном фильтре или
// в вычисляемом поле запроса
function FieldExistsForQuery(const FieldName, E: String): Boolean;
var
  S, SS: String;
  L, i, p: Integer;
  IsField: Boolean;
begin
  Result := False;
  S := Utf8LowerCase(FieldName);
  L := Length(E);
  IsField := False;
  for i := 1 to L do
  begin
    // Принимаем только поля с префиксами
    if (E[i] = '[') and ( (Copy(E, i+1, 1) = '!') or (Copy(E, i+1, 1) = ':') ) then
    begin
      IsField := True;
      p := i + 2;
    end
    else if IsField and ((E[i] in [']', '|']) or (i = L)) then
    begin
      SS := Utf8LowerCase(Copy(E, p, i - p));
      if S = SS then Exit(True);
      IsField := False;
    end;
  end;
end;

function IsNumericComponent(SS: TSession; C: TdxField): Boolean;
begin
  Result := (C is TdxCalcEdit) or (C is TdxCounter) or (C is TdxRecordId) or
    (C is TdxLookupComboBox) or (C is TdxCheckBox) or ((C is TdxObjectField) and
      IsNumericComponent( SS, GetObjectFieldField(SS, TdxObjectField(C)) ));
end;

function CutStr(var S: String; D: Char): String;
var
  p: SizeInt;
begin
  p := Pos(D, S);
  if p = 0 then
  begin
    Result := S;
    S := '';
  end
  else
  begin
    Result := Copy(S, 1, p - 1);
    Delete(S, 1, p);
  end;
end;

function GetComponentDisplayFormat(SS: TSession; Fm: TdxForm; C: TdxField): String;
var
  ObjC: TdxField;
  SrcFm: TdxForm;
begin
  Result := '';
  if C is TdxObjectField then
  begin
    with TdxObjectField(C) do
      if (ObjId > 0) and (FieldId > 0) then
      begin
        ObjC := Fm.FindField(ObjId);
        SrcFm := SS.FormMan.FindForm(GetSourceTId(ObjC));
        C := SrcFm.FindField(FieldId);
      end;
  end
  else if C is TdxLookupComboBox then
    with TdxLookupComboBox(C) do
      if (SourceTId > 0) and (SourceFId > 0) then
      begin
        SrcFm := SS.FormMan.FindForm(SourceTId);
        C := SrcFm.FindField(SourceFId);
      end;

  if C is TdxCalcEdit then
    Result := TdxCalcEdit(C).PrecStr
  else if C is TdxTimeEdit then
    Result := TdxTimeEdit(C).TimeFormatStr;
end;

procedure SetDSFieldDisplayFormat(F: TField; Fmt: String);
var
  PInfo: PPropInfo;
begin
  PInfo := GetPropInfo(F, 'DisplayFormat');
  if PInfo <> nil then
    SetPropValue(F, PInfo, Fmt)
end;

function GetFieldDisplayText(F: TField): String;
begin
  if F.DataType <> ftMemo then
    Result := F.DisplayText
  else
    Result := F.AsString;
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

(*procedure HideAllControls(WC: TdxWinControl);
var
  i: Integer;
  C: TdxControl;
begin
  for i := 0 to WC.ControlCount - 1 do
  begin
    C := WC.Controls[i];
    C.Hidden := True;
    if C is TdxWinControl then
      HideAllControls(TdxWinControl(C));
  end;
end;

procedure HideOverlapControls(Fm: TdxForm);
var
  i, j: Integer;
  C, CC: TdxControl;
begin
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    if not (Fm.Components[i] is TdxControl) then Continue;
    C := TdxControl(Fm.Components[i]);
    if C.Hidden then Continue;

    for j := 0 to Fm.ComponentCount - 1 do
    begin
      if not (Fm.Components[j] is TdxControl) then Continue;
      CC := TdxControl(Fm.Components[j]);
      if (CC.Hidden) or (C = CC) then Continue;

      if (C.Parent = CC.Parent) and (C.Left >= CC.Left) and (C.Top >= CC.Top)
        and (C.Left + C.Width <= CC.Left + CC.Width)
        and (C.Top + C.Height <= CC.Top + CC.Height) and (i < j) then
      begin
        C.Hidden := True;
        Break;
      end;
    end;
  end;
end;

{ Скрываем все компоненты, которые находятся вне зоны видимости формы, а
  также те компоненты, которые перекрыты другими компонентами (например,
  запросы перекрытые сводными таблицами). Скрытые компоненты не возвращаются
  сервером, в том числе в GetEvalChangesAsJson. }
procedure HideUnvisibleControls(Fm: TdxForm);
var
  i: Integer;
  C: TdxControl;
begin
  for i := 0 to Fm.ControlCount - 1 do
  begin
    C := Fm.Controls[i];
    if (C.Left + C.Width < 0) or (C.Left > Fm.Width) or
      (C.Top + C.Height < 0) or (C.Top > Fm.Height) then
    begin
      C.Hidden := True;
      if C is TdxWinControl then
        HideAllControls(TdxWinControl(C));
    end;
  end;

  HideOverlapControls(Fm);
end;*)

end.

