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

unit HtmlShow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DXCtrls, FPimage, BGRAGraphics, Db, sqldb,
  DxReports, strconsts, dxtypes, fpjson, typeswrapper, mytypes,
  pivotgrid, dbengine;

const
  rcOk = 200;
  rcAjaxOk = 222;
  rcAjaxError = 223;
  rcValidateError = 224;
  rcBadRequest = 400;
  rcPageNotFound = 404;
  rcServerError = 500;
  rcRecordSetNotFound = 1;
  rcRecordUsed = 2;
  rcDeleteRecLimited = 3;
  rcRecordNotActive = 4;
  rcTemplateNotFound = 5;
  rcPrintError = 6;
  rcLoginFailed = 7;
  rcRecordReadOnly = 8;
  rcInvalidFreshValue = 9;
  rcConnectError = 10;
  rcAnyError = 99;

type

  TDuplicateParam = (dpNone, dpOne, dpAll);
  TEvalFlag = (efDelRow);
  TEvalFlags = set of TEvalFlag;

  { THtmlShow }

  THtmlShow = class
  private
    FCSS: TStringList;
    FResultCode: Integer;
    FRS: TSsRecordSet;
    FSs: TSession;
    FTabOrderList: TList;
    FTopCtrl: TdxComponent;
    FRecordCopies: array of TdxMemDataSet;
    function GetJsCode: String;
    function BuildHRef(Level: Integer): String;
    function GetHRef(FmId, RecId: Integer; TblId: Integer = 0; Row: Integer = 0): String;
    function GetColorCSS(C: TdxControl): string;
    //function GetFontCSS(F: TdxFont): String;
    function GetFontCSS(F, ParentF: TdxFont; DefaultColor: Boolean = False): String;
    function GetStyleClass(C: TdxControl): String;
    function GetGridButtonsStyle(Gr: TdxCustomGrid): String;
    function GetRpGridStyle(RD: TReportData): String;
    function GetBoundsCSS(C: TdxControl): String;
    function GetBoundsGridButtonsCSS(C: TdxCustomGrid): String;
    function GetVisible(C: TdxControl): String;
    function GetEnabled(C: TdxControl): String;
    function GetReadOnly(C: TdxControl): String;
    function GetMaxLength(C: TdxControl): String;
    function GetAutoFocus(C: TdxComponent): String;
    function CreateCSS: String;
    function GetFieldValue(F: TdxField): String;
    //function GetFilterLookupComboBoxItems(C: TdxLookupComboBox; const Value: String): String;
    //function GetFilterComboBoxItems(C: TdxComboBox; const Value: String): String;
    function ShowFilterRecordId(F: TdxRecordId; const Value: String): String;
    function ShowFilterObjectField(F: TdxObjectField; const Value: String): String;
    function ShowFilterCounter(F: TdxCounter; const Value: String): String;
    function ShowFilterTimeEdit(F: TdxTimeEdit; const Value: String): String;
    function ShowFilterEdit(F: TdxEdit; const Value: String): String;
    function ShowFilterCalcEdit(F: TdxCalcEdit; const Value: String): String;
    function ShowFilterDateEdit(F: TdxDateEdit; const Value: String): String;
    function ShowFilterCheckBox(F: TdxCheckBox; const Value: String): String;
    function ShowFilterComboBox(C: TdxComboBox; const Value: String): String;
    function ShowFilterLookupComboBox(C: TdxLookupComboBox; const Value: String): String;
    function ShowFilterField(F: TdxField; const Value: String): String;
    function ShowSortFields(Fm: TdxForm): String;
    function ShowPages(Fm: TdxForm; RecCount, Page, Cnt: Integer): String;
    //function CheckReadOnly(C: TdxComponent; AReadOnly: Boolean = True; AOnChange: Boolean = True): String;
    function ControlReadOnly(C: TdxControl): Boolean;
    function ShowRecordId(C: TdxRecordId): String;
    function ShowChart(C: TdxChart): String;
    function ShowPivotGrid(C: TdxPivotGrid): String;
    function ShowButton(C: TdxButton): String;
    function ShowCounter(C: TdxCounter): String;
    function ShowTimeEdit(C: TdxTimeEdit): String;
    function ShowObjectField(C: TdxObjectField): String;
    function ShowFile(C: TdxFile): String;
    function ShowDBImage(C: TdxDBImage; FileNameOnly: Boolean = False): String;
    function ShowDBImageThumbnail(DS: TDataSet; C: TdxDBImage; FileNameOnly: Boolean = False): String;
    function ShowQueryThumbnail(RD: TReportData; ColIndex: Integer; DS: TDataSet): String;
    function ShowImage(C: TdxImage): String;
    function ShowTabSheet(C: TdxTabSheet; Visible: Boolean): String;
    function ShowPageControl(C: TdxPageControl): String;
    function ShowGroup(C: TdxGroupBox): String;
    function ShowQueryGridRecords(ARS: TSsRecordSet; Skip: Integer): String;
    function ShowQueryGrid(C: TdxQueryGrid; GridOnly: Boolean): String;
    function ShowGridRecords(ARS: TSsRecordSet; Skip: Integer): String;
    function ShowGrid(C: TdxGrid; GridOnly: Boolean): String;
    function ShowShape(C: TdxShape): String;
    function ShowLookupComboBox(C: TdxLookupComboBox): String;
    function ShowComboBox(C: TdxComboBox): String;
    function ShowCheckBox(C: TdxCheckBox): String;
    function ShowMemo(C: TdxMemo): String;
    function ShowDateEdit(C: TdxDateEdit): String;
    function ShowCalcEdit(C: TdxCalcEdit): String;
    function ShowEdit(C: TdxEdit): String;
    function ShowLabel(C: TdxLabel): String;
    function ShowDummyInput: String;
    function ShowDummyComponent(C: TdxControl): String;
    function ShowControl(C: TdxControl): String;
    function ShowContainer(Cont: TdxWinControl): String;
    procedure Clear;
    function GetTabOrder(C: TdxComponent): Integer;
    function ShowTabs(UserMonShowed: Boolean = False): String;
    function ShowLastEditRecords(Fm: TdxForm): String;
    function ShowDateDetailField(Pr: TRpDateDetail): String;
    function ShowSideBar: String;
    function ShowUser: String;
    function ShowDebug: String;
    //function CheckAccessChildren: Boolean;
    //function CheckCond(Fm: TdxForm; const Cond: String): Boolean;
    //function CheckFormCond(FmId: Integer; Edit: Boolean): Boolean;
    function ShowAccessDenied: String;
    function GetEvalChangesAsJson(Flags: TEvalFlags = []): String;
    function GetLookupComboBoxListForm(C: TdxLookupComboBox; Skip: Integer;
      const Frags: String; FragList: TStrings): String;
    function GetLookupComboBoxListSource(C: TdxLookupComboBox; Skip: Integer;
      const Frags: String; FragList: TStrings): String;
    function GetLookupComboBoxList(C: TdxLookupComboBox; Skip: Integer;
      const Frags: String; FragList: TStrings): String;
    function GetComboBoxList(C: TdxComboBox; const Frags: String; Skip: Integer;
      FragList: TStrings): String;
    function GetFilterLookupComboBoxList(C: TdxCustomComboBox; Skip: Integer;
      const Frags: String; FragList: TStrings): String;
    function GetFilterComboBoxList(C: TdxCustomComboBox; Skip: Integer;
      const Frags: String; FragList: TStrings): String;
    procedure CopyRecordForDuplicate(DupParam: TDuplicateParam);
    procedure PasteRecordForDuplicate(DupParam: TDuplicateParam);
    function ShowRedirect(const Url: String): String;
    function ShowReportRecords(ARS: TSsRecordSet; Skip: Integer): String;
  public
    constructor Create;
    destructor Destroy; override;
    //function ShowLogin(Fail: Boolean): String;
    function ShowFormNotFound: String;
    function ShowLoginUser: String;
    function ShowConnectionProgress(const UserName: String): String;
    function ShowNullForm: String;
    function ShowForm: String;
    function ShowEditForm: String;
    function PostEditForm(AParams: TStrings; DupParam: TDuplicateParam = dpNone; KeepRecordSet: Boolean = False): String;
    function FormDeleteRow: String;
    function FormAppend(AFields: TStrings): String;
    function TableAppend(AFields: TStrings): String;
    function TableEdit(AFields: TStrings): String;
    function TableDeleteRow(AFields: TStrings): String;
    function TableScroll(AFields: TStrings): String;
    function TableFetch(AFields: TStrings): String;
    //function ShowImageUploadForm: String;
    function UploadImage(AParams: TStrings; const FileName: String; St: TStream): String;
    //function ShowFileUploadForm: String;
    function UploadFile(AParams: TStrings; const FileName: String; St: TStream): String;
    function DownloadFile(AParams: TStrings): String;
    function ViewImage: String;
    function ClearImage(AParams: TStrings): String;
    function ClearFile(AParams: TStrings): String;
    //function AppendRecord: String;
    function DeleteRecord(AParams: TStrings = nil): String;
    function ShowFilter: String;
    function ApplyFilter(JsonStr: TStrings): String;
    function ApplyFilterPreset: String;
    function ClearAllFilters: String;
    function PrintForm(AParams: TStrings): String;
    function PrintReport: String;
    function ApplyReportFilter(JsonStr: TStrings): String;
    function ShowReport: String;
    function ReportFetch(AParams: TStrings): String;
    function GetFirstForm: String;
    function ShowFirstForm: String;
    function ApplySort(Fields: TStrings): String;
    function ApplyQrySort(Fields: TStrings): String;
    function ApplyRpSort(Fields: TStrings): String;
    //function ShowSort: String;
    function ShowErrorPage(const Caption, Msg: String): String;
    function ShowError2Page(const Caption, Msg: String): String;
    function ShowIndexPage: String;
    function FieldChange(AFields: TStrings): String;
    function QueryScroll(AParams: TStrings): String;
    function GetList(AParams: TStrings): String;
    function GetFilterList(AParams: TStrings): String;
    function GetRpFilterList(AParams: TStrings): String;
    function CancelChanges: String;
    function SetActivePage(AParams: TStrings): String;
    function AppendQueryRecord(AParams: TStrings): String;
    function QueryEdit(AParams: TStrings): String;
    function QueryDelRow(AParams: TStrings): String;
    function QueryFetch(AParams: TStrings): String;
    function ObjectAppend(AParams: TStrings): String;
    function ObjectEdit(AParams: TStrings): String;
    function DuplicateRecord(AParams: TStrings; DupParam: TDuplicateParam): String;
    function BnClick(AParams: TStrings): String;
    function MsgBnClick(AParams: TStrings): String;
    function ShowDebugAjax: String;
    function CloseDebug: String;
    function ClearDebug: String;
    function ShowUserMonitor: String;
    function ShowCompileError: String;
    property Session: TSession read FSs write FSs;
    property ResultCode: Integer read FResultCode;
  end;

implementation

uses
  sqlgen, LazUtf8, apputils, Math, LazFileUtils, dxusers,
  expressions, Variants, xmlreport, dateutils, StrUtils, QSort, appsettings,
  filterparsers, dxactions, mainserver, scriptmanager, uPSRuntime, exprfuncs;

type
  TMarkData = record
    Start, Len: Integer;
  end;

  { TFormColorData }

  TFormColorData = class
  public
    Color: TColor;
    E: TExpression;
    FieldName: String;
    Exists: Boolean;
  end;

  { TFormColors }

  TFormColors = class(TList)
  private
    function GetColors(Index: Integer): TFormColorData;
  public
    function AddColor(Color: TColor; E: TExpression; const FieldName: String): TFormColorData;
    procedure Clear; override;
    property Colors[Index: Integer]: TFormColorData read GetColors; default;
  end;

  TUserMonData = class
  public
    UserName, IP: String;
    ConnectTime: TDateTime;
    IsWeb, IsSingle: Boolean;
  end;

procedure ColoringToCSS(RS: TSsRecordSet; var CSS: String);
var
  i: Integer;
  CD: TColoringData;
  CL: TColoringList;
  Pfx: String;
begin
  if RS.RD <> nil then
  begin
    CL := RS.RD.Coloring;
    Pfx := 'q' + IntToStr(RS.RD.Id);
  end
  else
  begin
    Pfx := 'fm' + IntToStr(RS.Form.Id);
    CL := RS.Form.Coloring;
  end;


  for i := 0 to CL.Count - 1 do
  begin
    CD := CL[i];
    if CD.E = nil then Continue;
    if CD.FieldName = '' then
      CSS := CSS + '.' + Pfx + 'tr' + IntToStr(i) + ' td {background-color:' +
        ColorToHtml(CD.Color) + ';}' + LineEnding
    else
      CSS := CSS + 'tr td.' + Pfx + 'td' + IntToStr(i) + ' {background-color:' +
        ColorToHtml(CD.Color) + ';}' + LineEnding;
  end;
end;

procedure ColoringToCSS(RS: TSsRecordSet; CSS: TStrings); overload;
var
  i: Integer;
  CD: TColoringData;
  CL: TColoringList;
  Pfx: String;
begin
  if RS.RD <> nil then
  begin
    CL := RS.RD.Coloring;
    Pfx := 'q' + IntToStr(RS.RD.Id);
  end
  else
  begin
    Pfx := 'fm' + IntToStr(RS.Form.Id);
    CL := RS.Form.Coloring;
  end;

  for i := 0 to CL.Count - 1 do
  begin
    CD := CL[i];
    if CD.E = nil then Continue;
    if CD.FieldName = '' then
      CSS.AddObject('.' + Pfx + 'tr' + IntToStr(i) + ' td {background-color:' +
        ColorToHtml(CD.Color) + ';}', TObject(1))
    else
      CSS.AddObject('tr td.' + Pfx + 'td' + IntToStr(i) + ' {background-color:' +
        ColorToHtml(CD.Color) + ';}', TObject(1))
  end;
end;

function CalcColoring(RS: TSsRecordSet; const FieldName: String): String;
var
  i: Integer;
  CD: TColoringData;
  V: Variant;
  CL: TColoringList;
begin
  Result := '';
  if RS.RD <> nil then CL := RS.RD.Coloring
  else CL := RS.Form.Coloring;
  try
    for i := 0 to CL.Count - 1 do
    begin
      CD := CL[i];
      if (CD.E <> nil) and (CD.FieldName = FieldName) then
      begin
        V := TExpression(CD.E).Calc;
        if VarIsBool(V) and (V = True) then
        begin
          if RS.RD <> nil then
          begin
            Result := 'q' + IntToStr(RS.RD.Id);
            if FieldName = '' then Result := Result + 'tr'
            else Result := Result + 'td';
          end
          else
            Result := 'fm' + IntToStr(RS.Form.Id) + 'tr';
          Result := Result + IntToStr(i);
          Break;
        end;
      end;
    end;
  except
    ;
  end;
end;

function DeleteThousandSeparators(const Value: String): String;
begin
  Result := StringReplace(Value, DefaultFormatSettings.ThousandSeparator, '',
    [rfReplaceAll]);
end;

function CheckField(F: TdxField; const Value: String; out V: Variant): Boolean;
var
  E: Double;
  D: TDateTime;
  N: integer;
begin
  Result := True;
  if Value = '' then V := Null
  else if F is TdxCalcEdit then
  begin
    Result := MyStrToFloat( DeleteThousandSeparators(Value), E );
    if Result then V := E
    else V := Null;
  end
  else if F is TdxDateEdit then
  begin
    Result := MyStrToDate(Value, D);
    if Result then V := D
    else V := Null;
  end
  else if F is TdxTimeEdit then
  begin
    Result := MyStrToTime(Value, D);
    if Result then V := D
    else V := Null;
  end
  else if F is TdxCounter then
  begin
    Result := TryStrToInt(Value, N);
    if Result then V := N
    else V := Null;
  end
  else
    V := Value;
end;

{function GetObjectValue(SS: TSession; F: TdxLookupComboBox; aKey: String): String;
begin
  if aKey = '' then Exit('');
  with SS.DBase.OpenDataSet('select ' + FieldStr(F.SourceFId) + ' from ' +
    TableStr(F.SourceTId) + ' where id=' + aKey) do
  try
    Result := Fields[0].AsString;
  finally
    Free;
  end;
end;}

function PeriodToStr(P: TPeriodType): String;
var
  DB, DE: TDateTime;
begin
  DE := Date;
  case P of
    ptToday: DB := Date;
    ptThisWeek: DB := IncDay(Date, -DayOfTheWeek(Date)+1);
    ptThisMonth: DB := IncDay(Date, -DayOf(Date)+1);
    ptThisYear: DB := EncodeDate(YearOf(Date), 1, 1);
  end;
  Result := DateToStr(DB) + ' .. ' + DateToStr(DE);
end;

function AlignmentToCSS(A: TAlignment): String;
begin
  case A of
    taLeftJustify: Result := 'left';
    taCenter: Result := 'center';
    taRightJustify: Result := 'right';
  end;
end;

function TextLayoutToCSS(TL: TTextLayout): String;
begin
  case TL of
    tlTop: Result := 'top';
    tlCenter: Result := 'middle';
    tlBottom: Result := 'bottom';
  end;
end;

{ TFormColors }

function TFormColors.GetColors(Index: Integer): TFormColorData;
begin
  Result := TFormColorData(Items[Index]);
end;

function TFormColors.AddColor(Color: TColor; E: TExpression;
  const FieldName: String): TFormColorData;
begin
  Result := TFormColorData.Create;
  Result.Color := Color;
  Result.E := E;
  Result.FieldName := FieldName;
  Add(Result);
end;

procedure TFormColors.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Colors[i].Free;
  inherited Clear;
end;

{ THtmlShow }

function THtmlShow.GetJsCode: String;

  function ReplaceSeparator(const Fmt: String; NewSep: Char): String;
  var
    i: Integer;
  begin
    Result := Fmt;
    for i := 1 to Length(Result) do
      if Result[i] in ['a'..'z'] then
      else Result[i] := NewSep;
  end;

var
  FS: TFormatSettings;
begin
  FS := DefaultformatSettings;
  Result := 'const formatSettings = {' +
    'shortDateFormat: "' + ReplaceSeparator(LowerCase(FS.ShortDateFormat), FS.DateSeparator) + '", ' +
    'dateSeparator: "' + FS.DateSeparator + '", ' +
    'longTimeFormat: "' + ReplaceSeparator(LowerCase(FS.LongTimeFormat), FS.TimeSeparator) + '", ' +
    'timeSeparator: "' + FS.TimeSeparator + '" }';
end;

function THtmlShow.BuildHRef(Level: Integer): String;
begin
  Result := '';
  if Level > 0 then
    Result := '?fm=' + IntToStr(FSs.FormId);
  if Level > 1 then
    Result := Result + '&rec=' + IntToStr(FSs.RecId);
  if Level > 2 then
    Result := Result + '&tbl=' + IntToStr(FSs.TableId);
  if Level > 3 then
    Result := Result + '&row=' + IntToStr(FSs.TableRecId);
end;

function THtmlShow.GetHRef(FmId, RecId: Integer; TblId: Integer; Row: Integer
  ): String;
begin
  Result := '?fm=' + IntToStr(FmId) + '&rec=' + IntToStr(RecId);
  if TblId > 0 then
  begin
    Result := Result + '&tbl=' + IntToStr(TblId);
    if Row > 0 then Result := Result + '&row=' + IntToStr(Row);
  end;
end;

function THtmlShow.GetColorCSS(C: TdxControl): string;
begin
  Result := '';
  if C.Color <> clDefault then
    Result := Result + 'background-color:' + ColorToHtml(C.Color) + ';';
end;

(*function THtmlShow.AppendRecord: String;
var
  Fm: TdxForm;
begin
  Fm := FSs.FormMan.FindForm(FSs.GetFmId);
  if Fm = nil then
  begin
    FResultCode := rcPageNotFound;
    Exit(ShowFormNotFound);
  end;
  // !!! Доступ
  if not FSS.UserMan.CheckFmAdding(FSS.RoleId, Fm.Id) {or ((Fm.PId > 0) and
    ((not FSS.UserMan.CheckFmEditing(FSS.RoleId, Fm.PId)) or (not CheckFormCond(Fm.PId, True))))}
    then Exit(ShowAccessDenied);
  //
  try

  if FSS.TableId > 0 then
  begin
    FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
    if FRS = nil then Exit(ShowErrorPage(rsError, rsNoLinkFormMsg));
    if FRS.CanAppend <> asOk then Exit(ShowAccessDenied);

    // Перенаправляем на редактируемую запись таблицы
    if FRS.DataSet.State in [dsInsert, dsEdit] then
    begin
      FSS.TableRecId := FRS.DataSet['id'];
      FSS.Msg := rsEditRecNotFinished;
      Exit('<html><head><meta http-equiv=refresh content="1;url=' +
        BuildHRef(4) + '"></head><body></body></html>');
    end
  end
  else
  begin
    if (Fm.ViewType = vtSimpleForm) and (FSS.FindRecordSet(Fm.Id, 1, 0) <> nil) then
    begin
      FSS.RecId := 1;
      Exit('<html><head><meta http-equiv=refresh content="1;url=' +
        BuildHRef(2) + '"></head><body></body></html>');
    end;
    FRS := FSS.AddRecordSet(Fm);
    // Здесь CanAppend не нужен, т. к. CheckFmAdding есть выше.
    FRS.OpenRecord(0);
  end;

  FRS.Append;
  FRS.OpenDetails;

  if FSs.TableId = 0 then
  begin
    FSs.RecId := FRS.RecId;
    Result := '<html><head><meta http-equiv=refresh content="1;url=' +
      BuildHRef(2) + '"></head><body></body></html>';
  end
  else
  begin
    FSs.TableRecId := FRS.RecId;
    Result := '<html><head><meta http-equiv=refresh content="1;url=' +
      BuildHRef(4) + '"></head><body></body></html>';
  end;

  except
    on E: EPSException do
    begin
      FResultCode := rcAnyError;
      Result := ShowErrorPage(rsError, EPSExceptionToString(E));
    end;
    on E: Exception do
    begin
      FResultCode := rcAnyError;
      Result := ShowErrorPage(rsError, E.Message);
    end;
  end;
end;   *)

function THtmlShow.DeleteRecord(AParams: TStrings): String;
var
  FmId: Integer;
  FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;

  FmId := FSS.GetFmId;
  if not FSS.UserMan.CheckFmDeleting(FSS.RoleId, FmId) then
    Exit(MakeJsonErrString(rcAnyError, rsAccessDenied));
  if (AParams <> nil) and not TryStrToInt(AParams.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));

  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);

  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if (AParams <> nil) and (FRS.FreshValue <> FreshValue) then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));

  if (FRS.Editing <> asOk) or (FRS.Deleting <> asOk) then
    Exit(MakeJsonErrString(rcAnyError, rsAccessDenied));

  if FRS.Form.PId = 0 then
  begin
    // !!! Доступ
    if not FRS.CheckAccessDetails then
      Exit(MakeJsonErrString(rcDeleteRecLimited, rsDeleteRecLimited))
    //
  end
  else
  begin
    if FRS.RecId <> FSS.TableRecId then
      Exit(MakeJsonErrString(rcRecordNotActive, rsRecordNotActive));
  end;

  try
    if FRS.Delete = asHasRef then
      Exit(MakeJsonErrString(rcRecordUsed, rsRecordUsed));
    if FRS.Form.PId = 0 then
    begin
      FSS.RecordSets.DeleteRecordSet(FRS);
      FRS := nil;
    end;
    FResultCode := rcAjaxOk;
  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
    end;
  end;
end;

function THtmlShow.ShowFilter: String;
var
  Fm: TdxForm;
  i, j: Integer;
  F: TdxField;
  Content, Btns, Ctrls, Flds, Chk, Chk2: String;
  Flt: TFilterObject;
  FF: TFilterField;
  SL: TStringListUtf8;
begin
  Result := '';
  Fm := FSs.FormMan.FindForm(FSs.FormId);
  if Fm = nil then
  begin
    FResultCode := rcPageNotFound;
    Exit(ShowErrorPage(rsFilterError, rsCantShowFilterMsg));
  end;
  if Fm.Id <> FSs.FilterId then
  begin
    Fm := FSs.FormMan.FindForm(FSs.FilterId);
    if Fm = nil then
    begin
      FResultCode := rcPageNotFound;
      Exit(ShowErrorPage(rsFilterError, rsCantShowFilterMsg));
    end
    else if Fm.PId <> FSs.FormId then
    begin
      FResultCode := rcPageNotFound;
      Exit(ShowErrorPage(rsFilterError, rsCantShowFilterMsg));
    end;
  end;
  Btns := '<button type=submit><img src="/img/ok.svg"></button><button type=button' +
    ' onclick="goBack()"><img src="/img/back.svg"></button>';
  Btns := Btns + '<span>' + '<img src="/img/filterfm.svg">' + Fm.GetRecordsCaption + '</span>';

  Flds := ''; Ctrls := '';
  SL := TStringListUtf8.Create;
  Fm.GetFields(SL);
  SL.Sort;
  for i := 0 to SL.Count - 1 do
  begin
    F := TdxField(SL.Objects[i]);
    if (Fm.PId > 0) and (F is TdxObjectField) then Continue;
    Flds := Flds + '<option value=' + IntToStr(F.Id) + '>' + F.FieldName + '</option>';
    Ctrls := Ctrls + '<div>' + ShowFilterField(F, '') + '</div>';
  end;
  SL.Free;

  Flt := FSS.FormList.GetForm(Fm).Filter;
  Content := '';
  for i := 0 to Flt.Count - 1 do
  begin
    FF := Flt.Fields[i];
    F := Fm.FindField(FF.FId);
    if FF.IsNot then Chk := ' checked'
    else Chk := '';
    if FF.IsNull then Chk2 := ' checked'
    else Chk2 := '';
    Content := Content + '<div class=filter-item data-id=' + IntToStr(F.Id) +
      '><div class=field><button class=fltbn type=button onclick="delField(this)"><img src="/img/delete.svg"></button><span>' +
      F.FieldName + '</span><input type=checkbox' + Chk + '><span>' + rsNot +
      '</span><input type=checkbox' + Chk2 + '><span>' + rsEmpty + '</span></div>';
    if FF.Values.Count = 0 then FF.Values.Add('');
    for j := 0 to FF.Values.Count - 1 do
      Content := Content + '<div class=value><div>' +
        ShowFilterField(F, FF.Values[j]) + '</div>' +
        '<button class=fltbn type=button onclick="delValue(this)"><img src="/img/delete.svg"></button>' +
        '<button class=fltbn type=button onclick="addValue(this)"><img src="/img/add.svg"></button></div>';
    Content := Content + '</div>';
  end;

  //Res := 'const rsNot="' + rsNot + '", rsEmpty="' + rsEmpty + '";';

  Result := LoadString(GetHtmlPath + 'filterform.html');
  Result := StringReplace(Result, '[lng]', AppSet.Language, []);
  Result := StringReplace(Result, '[title]', Fm.GetRecordsCaption, []);
  Result := StringReplace(Result, '[javascript]', GetJsCode, []);
  //Result := StringReplace(Result, '[resourcestrings]', Res, []);
  Result := StringReplace(Result, '[user]', ShowUser, []);
  Result := StringReplace(Result, '[sidebar]', ShowSideBar, []);
  Result := StringReplace(Result, '[tabs]', ShowTabs, []);
  Result := StringReplace(Result, '[buttons]', Btns, []);
  Result := StringReplace(Result, '[controls]', Ctrls, []);
  Result := StringReplace(Result, '[fields]', Flds, []);
  Result := StringReplace(Result, '[content]', Content, []);
end;

function CheckFltFieldValue(Tp: TRpFieldType; var V1, V2: String): Boolean;
var
  E: Double;
  D: TDateTime;
  N: integer;
begin
  Result := True;
  if Tp = flNumber then
  begin
    if (V1 <> '') and (not MyStrToFloat(V1, E)) then Exit(False);
    if (V2 <> '') and (not MyStrToFloat(V2, E)) then Exit(False);
  end
  else if Tp = flDate then
  begin
    if V1 <> '' then
    begin
      if MyStrToDate(V1, D) then
        V1 := DateToStr(D)
      else
        Exit(False);
    end;
    if V2 <> '' then
    begin
      if MyStrToDate(V2, D) then
        V2 := DateToStr(D)
      else
        Exit(False);
    end;
  end
  else if Tp = flTime then
  begin
    if V1 <> '' then
    begin
      if MyStrToTime(V1, D) then
        V1 := TimeToStr(D)
      else
        Exit(False);
    end;
    if V2 <> '' then
    begin
      if MyStrToTime(V2, D) then
        V2 := TimeToStr(D)
      else
        Exit(False);
    end;
  end
  else if Tp in [flCounter, flRecId] then
  begin
    if (V1 <> '') and (not TryStrToInt(V1, N)) then Exit(False);
    if (V2 <> '') and (not TryStrToInt(V2, N)) then Exit(False);
  end
end;

function THtmlShow.ApplyFilter(JsonStr: TStrings): String;
var
  Fm: TdxForm;
  i, j: Integer;
  F: TdxField;
  V, V1, V2: String;
  Flt: TFilterObject;
  FF: TFilterField;
  Json, JsonVals: TJSONData;
  JsonItem, JsonVal: TJSONObject;
  FlTp: TRpFieldType;
begin
  Result := '';
  FResultCode := rcAjaxError;
  Fm := FSs.FormMan.FindForm(FSs.FilterId);
  if Fm = nil then Exit(MakeJsonErrString(rcAnyError, 'Fm=nil'));
  Flt := FSS.FormList.GetForm(Fm).Filter;
  Flt.Clear;
  Json := GetJSON(JsonStr.Text);
  for i := 0 to Json.Count - 1 do
  begin
    JsonItem := TJsonObject(Json.Items[i]);
    FF := Flt.AddField;
    FF.FId := JsonItem['field'].AsInteger;
    FF.IsNot := JsonItem['not'].AsBoolean;
    FF.IsNull := JsonItem['nul'].AsBoolean;
    JsonVals := JsonItem['vals'];
    F := Fm.FindField(FF.FId);
    FlTp := GetRpFieldType(FSS, F);
    for j := 0 to JsonVals.Count - 1 do
    begin
      JsonVal := TJsonObject(JsonVals.Items[j]);
      V1 := JsonVal['value'].AsString;
      if JsonVal.IndexOfName('valueEnd') >= 0 then
        V2 := JsonVal['valueEnd'].AsString
      else
        V2 := '';
      if not CheckFltFieldValue(FlTp, V1, V2) then Continue;
      V := V1;
      if (FlTp in [flNumber, flCounter, flDate, flTime, flRecId]) and
        ((V1 <> '') or (V2 <> '')) then V := V1 + ' .. ' + V2;
      FF.Values.Add(V);
    end;
  end;
  Json.Free;
  FResultCode := rcAjaxOk;
end;

function THtmlShow.ApplyFilterPreset: String;
var
  Fm: TdxForm;
begin
  Result := '';
  Fm := FSs.FormMan.FindForm(FSs.FormId);
  if (Fm = nil) or (Fm.Filters.Count <= FSs.FilterPrId) then
  begin
    FResultCode := rcPageNotFound;
    Exit(ShowErrorPage(rsFilterError, rsCantApplyFilterMsg));
  end;
  FSs.Page:=1;
  FSS.FormList.GetForm(Fm).Filter.Load(Fm.Filters.ValueFromIndex[FSs.FilterPrId]);

  Result := '<html><head><meta http-equiv=refresh content="1;url=' +
    BuildHRef(1) + '"></head><body></body></html>';
end;

function THtmlShow.ClearAllFilters: String;
var
  Fm, F: TdxForm;
  i: Integer;
begin
  Result := '';
  Fm := FSs.FormMan.FindForm(FSs.FormId);
  if Fm = nil then
  begin
    FResultCode := rcPageNotFound;
    Exit(ShowFormNotFound);
  end;
  FSs.Page:=1;
  FSS.FormList.GetForm(Fm).Filter.Clear;
  for i := 0 to FSS.FormMan.FormCount - 1 do
  begin
    F := FSS.FormMan.Forms[i];
    if F.PId = Fm.Id then
      FSS.FormList.GetForm(F).Filter.Clear;
  end;

  Result := '<html><head><meta http-equiv=refresh content="1;url=' +
    BuildHRef(1) + '"></head><body></body></html>';
end;

function THtmlShow.PrintForm(AParams: TStrings): String;
var
  Tpl, TplFullName, OutName, Errs: String;
  RS: TSsRecordSet;
  JsonObj: TJSONObject;
  FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AParams.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));
  RS := FSS.FindRecordSet(FSs.FormId, FSs.RecId, 0);
  if RS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if RS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));
  if FSs.TemplateId >= RS.Form.Templates.Count then
    Exit(MakeJsonErrString(rcAnyError, 'TemplateId > Count'));

  Tpl := RS.Form.Templates[FSs.TemplateId];
  TplFullName := FSS.GetTemplatesPath + StringReplace(Tpl, '\', DirectorySeparator, [rfReplaceAll]);
  OutName := ExtractFileName(TplFullName);
  if not FileExists(TplFullName) then
    Exit(MakeJsonErrString(rcTemplateNotFound, Format(rsTemplateNotFound, [Tpl])));

  try
    ReportToXXX(RS, TplFullName, GetCachePath(FSS) + OutName, Errs);
    JsonObj := TJsonObject.Create(['file', GetCachePath(FSS, True) + OutName]);
    if Errs <> '' then JsonObj.Add('errors', LineEnding + Errs);
    Result := JsonObj.AsJSON;
    FResultCode := rcAjaxOk;
    JsonObj.Free;
  except
    on E: Exception do
      Result := MakeJsonErrString(rcPrintError, Format(rsPrintErrMsg, [E.Message]));
  end;
end;

function THtmlShow.PrintReport: String;
var
  Tpl, TplFullName, OutName, Errs, SQL: String;
  RS, RpRS: TSsRecordSet;
  Fm: TdxForm;
  JsonObj: TJsonObject;
begin
  Result := '';
  FResultCode := rcAjaxError;
  RpRS := FSS.FindRpRecordSet(FSS.RpId);
  if (RpRS = nil) or (FSs.TemplateId >= RpRS.RD.Templates.Count) then
    Exit(MakeJsonErrString(rcAnyError, 'RpRS=nil'));

  Tpl := RpRS.RD.Templates[FSs.TemplateId];
  TplFullName := FSS.GetTemplatesPath + StringReplace(Tpl, '\',
    DirectorySeparator, [rfReplaceAll]);
  OutName := ExtractFileName(TplFullName);
  if not FileExists(TplFullName) then
    Exit(MakeJsonErrString(rcTemplateNotFound, Format(rsTemplateNotFound, [Tpl])));

  Fm := nil; RS := nil;
  try try
    Fm := CreateReportForm(FSS, RpRS.RD, SQL);
    if SQL <> '' then SQL := ',' + SQL;
    RS := TSsRecordSet.Create(FSS, nil);

    RS.AssignReportForm(Fm, RpRS.RD);
    RS.DataSet.SQL.Text := Format('select 0 as id %s from rdb$database', [SQL]);
    RS.DataSet.Open;

    ReportToXXX(RS, TplFullName, GetCachePath(FSS) + OutName, Errs);
    JsonObj := TJsonObject.Create(['file', GetCachePath(FSS, True) + OutName]);
    if Errs <> '' then JsonObj.Add('errors', LineEnding + Errs);
    Result := JsonObj.AsJSON;
    FResultCode := rcAjaxOk;
    JsonObj.Free;
  except
    on E: Exception do
      Result := MakeJsonErrString(rcPrintError, Format(rsPrintErrMsg, [E.Message]));
  end;
  finally
    FreeAndNil(RS);
    FreeAndNil(Fm);
  end;
end;

function THtmlShow.ApplyReportFilter(JsonStr: TStrings): String;
var
  Rp: TReportData;
  i, j, idx: Integer;
  pF: PRpField;
  V, V1, V2, S: String;
  JsonItem, JsonVal: TJSONObject;
  JsonVals, Json: TJSONData;
  RS: TSsRecordSet;
  Tp: TRpFieldType;
begin
  Result := '';
  FResultCode := rcAjaxError;
  RS := FSS.FindRpRecordSet(FSS.RpId);
  if RS = nil then Exit(MakeJsonErrString(rcAnyError, 'RS=nil'));
  Rp := RS.RD;
  Json := GetJson(JsonStr.Text);
  // Сбрасываем параметры
  for i := 0 to Rp.Sources.Count - 1 do
    for j := 0 to Rp.Sources[i]^.Fields.Count - 1 do
      with Rp.Sources[i]^.Fields[j]^ do
      begin
        Value := '';
        if (Tp = flBool) and (Param) then Value := '0';
      end;
  //
  JsonItem := TJsonObject(Json.Items[0]);
  Rp.DateDetail := TRpDateDetail(JsonItem.Elements['period'].AsInteger);
  for i := 1 to Json.Count - 1 do
  begin
    JsonItem := TJsonObject(Json.Items[i]);
    pF := Rp.FindField(JsonItem['field'].AsInteger);
    if (pF <> nil) and (pF^.Param) then
    begin
      pF^.No := JsonItem['not'].AsBoolean;
      pF^.Nul := JsonItem['nul'].AsBoolean;
      S := '';
      JsonVals := JsonItem['vals'];
      for j := 0 to JsonVals.Count - 1 do
      begin
        JsonVal := TJsonObject(JsonVals.Items[j]);
        V1 := JsonVal['value'].AsString;
        if JsonVal.IndexOfName('valueEnd') >= 0 then
          V2 := JsonVal['valueEnd'].AsString
        else
          V2 := '';
        Tp := GetLowField(pF)^.Tp;
        if not CheckFltFieldValue(Tp, V1, V2) then Continue;
        V := V1;
        if (Tp in [flNumber, flDate, flTime, flCounter, flRecId]) and ((V <> '') or (V2 <> '')) then
          V := V1 + ' .. ' + V2;
        S := S + V + ';';
      end;
      SetLength(S, Length(S) - 1);

      idx := Rp.FindFieldIndex(pF^.Id);
      for j := 0 to Rp.Sources.Count - 1 do
        with Rp.Sources[j]^.Fields[idx]^ do
        begin
          Tp := pF^.Tp;
          No := pF^.No;
          Nul := pF^.Nul;
          Value := S;
        end;
    end;
  end;
  Json.Free;
  FResultCode := rcAjaxOk;
end;

{function ParamExists(Sr: TRpSource): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Sr.Fields.Count - 1 do
    if Sr.Fields[i]^.Param then Exit(True);
end;  }

function GetValueCount(L: TRpFieldList): Integer;
var
  rF: TRpField;
  i: Integer;
  SL: TStrings;
begin
  Result := 0;
  SL := TStringList.Create;
  for i := 0 to L.Count - 1 do
  begin
    rF := L[i]^;
    if (not rF.Param) or (rF.Tid = 0) or (rF.FId = 0) then Continue;
    SplitStr(rF.Value, ';', SL);
    if SL.Count > Result then Result := SL.Count;
  end;
  SL.Free;
end;

function CalcTotal(RS: TSsRecordSet; aFieldName: String; aFunc: TRpTotalFunc): Variant;
var
  F: TField;
  Mn, Mx, Sum, V: Variant;
  Cnt: Integer;
  FirstRec: Boolean;
  DS: TSQLQuery;
begin
  case aFunc of
    tfSum, tfAvg, tfCount: Result := 0;
    else Result := '';
  end;

  DS := RS.DataSet;
  FirstRec := True;
  Cnt := 0; Sum := 0;
  if aFieldName <> '' then F := DS.FieldByName(aFieldName)
  else F := nil;
  RS.DisableScrollEvents;
  try
    DS.First;
    while not DS.EOF do
    begin
      if F <> nil then
      begin
        V := F.Value;
        if V = Null then
        begin
          DS.Next;
          Continue;
        end;
      end;

      if FirstRec then
      begin
        Mn := V;
        Mx := V;
        FirstRec := False;
      end;
      Inc(Cnt);
      case aFunc of
        tfSum, tfAvg: Sum := Sum + V;
        tfMin: if V < Mn then Mn := V;
        tfMax: if V > Mx then Mx := V;
      end;
      DS.Next;
    end;
    case aFunc of
      tfSum: Result := Sum;
      tfAvg: Result := Sum / Cnt;
      tfCount: Result := Cnt;
      tfMax: Result := Mx;
      tfMin: Result := Mn;
    end;
  finally
    DS.First;
    RS.EnableScrollEvents;
  end;
end;

(*procedure GetTotalRpFieldType(SS: TSession; RD: TReportData; const FieldName: String;
  out FTp: TRpFieldType; out Fmt: String);
var
  FId: LongInt;
  C: TdxField;
  CF: TRpCalcField;
begin
  Fmt := '';
  if Copy(FieldName, 1, 1) = 'f' then
  begin
    FId := StrToInt(Copy(FieldName, 2, 10));
    FTp := RD.FindField(FId)^.Tp;
    C := RpFieldToFormField(SS, RD, FId);
    if C is TdxCalcEdit then
      Fmt := TdxCalcEdit(C).PrecStr
    else if C is TdxTimeEdit then
      Fmt := TdxTimeEdit(C).TimeFormatStr;
  end
  else
  begin
    FId := StrToInt(Copy(FieldName, 3, 10));
    CF := RD.CalcFields.FindField(FId)^;
    FTp := CF.Tp;
    if FTp = flNumber then
      Fmt := MakeNumberFormat(CF.Size, True, True)
    else if FTp = flTime then
      Fmt := 'hh:nn:ss';
  end;
end;  *)

procedure GetTotalRpFieldType(RD: TReportData; const FieldName: String;
  out FTp: TRpFieldType; out Fmt: String);
var
  i: Integer;
begin
  i := RD.IndexOfNameDS(FieldName);
  Assert(i < 0, 'GetRpFieldType: i < 0');

  FTp := RD.GetFieldType(i);
  Fmt := RD.GetDisplayFormat(i);
end;

function CalcGrandTotal(RS: TSsRecordSet): String;
var
  Fmt: String;
  i: Integer;
  T: TRpTotalData;
  V: Variant;
  FTp: TRpFieldType;
begin
  Result := '<table>';
  for i := 0 to RS.RD.Totals.Count - 1 do
  begin
    T := RS.RD.Totals[i];
    if RS.RD.IndexOfNameDS(T.FieldNameDS) < 0 then Continue;
    Result := Result + '<tr><td>' + T.Caption + ':</td>';
    try
      if T.Func = tfCount then
      begin
        FTp := flNumber;
        Fmt := ',0';
      end
      else
        GetTotalRpFieldType(RS.RD, T.FieldNameDS, FTp, Fmt);

      V := CalcTotal(RS, T.FieldNameDS, T.Func);

      if FTp = flNumber then
        T.Value := FormatFloat(Fmt, V)
      else if FTp = flTime then
        T.Value := FormatDateTime(Fmt, V)
      else
        T.Value := VarToStr(V);
    except
      on Ex: Exception do
        T.Value := Ex.Message;
    end;
    Result := Result + '<td>' + T.Value + '</td></tr>';
  end;
  Result := Result + '</table>';
end;

function THtmlShow.ShowReport: String;
var
  Btns, Flt, Data, Totals, S, CSS: String;
  SL: TStringList;
  i, j: Integer;
  Rp: TReportData;
  Sr: TRpSource;
  rF: TRpField;
  F: TdxField;
  SortCol: TRpGridSortData;
  si, EditFmId: Integer;
  RS: TSsRecordSet;
  CanGotoRec: Boolean;
  Col: TRpGridColumn;
begin
  Result := '';
  RS := FSS.FindRpRecordSet(FSS.RpId);
  if RS = nil then
  begin
    Rp := FSS.ReportMan.FindReport(FSS.RpId);
    if Rp = nil then
    begin
      FResultCode := rcPageNotFound;
      Exit(ShowErrorPage(rsAnyRpNotFound, rsAnyRpNotFoundMsg));
    end
    else if Rp.IsEmpty then
    begin
      FResultCode := rcAnyError;
      Exit(ShowErrorPage(rsError, rsRpUnderDevelopment));
    end;
    // !!! Доступ
    if not FSS.UserMan.CheckRpVisible(FSS.RoleId, Rp.Id) then Exit(ShowAccessDenied);
    RS := FSS.AddRpRepordSet(Rp);
  end;
  Rp := RS.RD;

  if Rp.IsSimple then
  begin
    EditFmId := Rp.GetEditFormId;
    CanGotoRec := FSs.UserMan.CheckFmVisible(FSS.RoleId, EditFmId);
    //dx := 2;
  end
  else
  begin
    CanGotoRec := False;
    //dx := 1;
  end;

  Btns := '';
  if CanGotoRec and FSS.UserMan.CheckFmAdding(FSS.RoleId, EditFmId) then
    Btns := Btns + '<button type=button id=addbn onclick="formAdd(' +
      IntToStr(EditFmId) + ')"><img src="/img/add.svg"></button>';
  Btns := Btns + '<button type=submit id=refreshbn><img src="/img/refresh.svg"></button>';
  if Rp.Templates.Count > 0 then
    Btns := Btns + '<button type=button id=printbn onclick="printClick()"><img src="/img/print.svg"></button>';
  Btns := Btns + '<button type=button id=menubn onclick="menuClick(this)"><img src="/img/menu.svg"></button>';
  if Rp.DateField >= 0 then
    Btns := Btns + ShowDateDetailField(Rp.DateDetail);
  Btns := Btns + '<span>' + Rp.Name + '</span>';
  if Rp.Templates.Count > 0 then
  begin
    Btns := Btns + '<div id=templates style="display: none;">';
    for i := 0 to Rp.Templates.Count - 1 do
    begin
      S := ExtractFileNameWithoutExt(Rp.Templates[i]);
      Btns := Btns + '<a data-index=' + IntToStr(i) + ' onclick="templateClick(this)">' + S + '</a>';
    end;
    Btns := Btns + '</div>';
  end;

  Flt := '';
  if Rp.ParamExists then
  begin
    Sr := Rp.Sources[0]^;
    SL := TStringList.Create;
    for i := 0 to Sr.Fields.Count - 1 do
    begin
      rF := Sr.Fields[i]^;
      if not rF.Param then Continue;
      F := RpFieldToFormField(FSS, Rp, rF.Id);
      Flt := Flt + '<div class=filter-item data-id=' + IntToStr(rF.Id) +
        '><div class=field><span>' + rF.Name + '</span><input type=checkbox' +
        IIF(rF.No, ' checked', '') + '><span>' + rsNot + '</span><input type=checkbox' +
        IIF(rF.Nul, ' checked', '') +
        '><span>' + rsEmpty + '</span></div>';
      SplitStr(rF.Value, ';', SL);
      if SL.Count = 0 then SL.Add('');
      for j := 0 to SL.Count - 1 do
      begin
        S := SL[j];
        Flt := Flt + '<div class=value><div>' + ShowFilterField(F, S) + '</div>' +
          '<button class=fltbn type=button onclick="delValue(this)"><img src="/img/delete.svg"></button>' +
          '<button class=fltbn type=button onclick="addValue(this)"><img src="/img/add.svg"></button></div>';
      end;

      Flt := Flt + '</div>';
    end;
    SL.Free;
  end;

  CSS := '';
  RS.FreshValue := NowTime;
  S := '<table data-freshvalue=' + IntToStr(RS.FreshValue) + IIF(CanGotoRec,
    '', ' class=gridro') + ' onclick="tableClick()"><thead><tr>';
  //if CanGotoRec then
  begin
    S := S + '<th></th>';
    (*if not CanGotoRec then
      CSS := CSS + '.data table th:first-child, .data table td:first-child { display: none; }' + LineEnding;
      //CSS := CSS + '.data table td:first-child { text-align: center; white-space: nowrap; }' + LineEnding;*)
  end;
  for i := 0 to Rp.Grid.ColumnCount - 1 do
  begin
    Col := Rp.Grid.Columns[i]; //TRpGridColumn(L[i]);
    if not Col.Visible or (Col.Width = 0) then Continue;
    S := S + '<th><span data-fid=' + Col.FieldNameDS + IIF(Rp.Grid.AllowChangeSort,
      ' onclick="headerClick()"', '');
    SortCol := Rp.Grid.SortCols.FindCol(Col);
    if SortCol <> nil then
    begin
      si := Rp.Grid.SortCols.IndexOf(SortCol);
      S := S + ' class=sort data-order=' + IIF(SortCol.Desc, '1', '0') + '>';
      // Порядковый номер (на первом не ставится)
      if si > 0 then
        S := S + IntToStr(si + 1);
      //
      S := S + '<img src="/img/';
      if SortCol.Desc then
        S := S + 'down-sort.svg'
      else
        S := S + 'up-sort.svg';
      S := S + '">';
    end
    else
      S := S + '>';
    S := S + Col.Caption + '</span></th>';
    CSS := CSS + '.grid table td:nth-child(' + IntToStr(i + 2) + ') { text-align: ' +
      AlignmentToCSS(GetRpGridColumnAlignment(FSS, Rp, Col)) + '; vertical-align: ' +
      TextLayoutToCSS(GetRpGridColumnLayout(Col)) + '; }' + LineEnding;
  end;
  S := S + '</tr></thead><tbody>';

  ColoringToCSS(RS, CSS);
  try
    RS.OpenReport;
    Totals := CalcGrandTotal(RS);
    Data := S + ShowReportRecords(RS, 0) + '</tbody></table>';
  except
    on E: Exception do
      Data := E.Message;
  end;

  Result := LoadString(GetHtmlPath + 'report.html');
  Result := StringReplace(Result, '[lng]', AppSet.Language, []);
  Result := StringReplace(Result, '[css]', CSS, []);
  Result := StringReplace(Result, '[tabs]', ShowTabs, []);
  Result := StringReplace(Result, '[title]', Rp.Name, []);
  Result := StringReplace(Result, '[javascript]', GetJsCode, []);
  Result := StringReplace(Result, '[user]', ShowUser, []);
  Result := StringReplace(Result, '[sidebar]', ShowSideBar, []);
  Result := StringReplace(Result, '[buttons]', Btns, []);
  Result := StringReplace(Result, '[filter]', Flt, []);
  Result := StringReplace(Result, '[totals]', Totals, []);
  Result := StringReplace(Result, '[content]', Data, []);
end;

function THtmlShow.ReportFetch(AParams: TStrings): String;
var
  SkipRows, FreshValue: Longint;
  RS: TSsRecordSet;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AParams.Values['skip'], SkipRows) then
    Exit(MakeJsonErrString(rcAnyError, 'Invalid skip value'));
  if not TryStrToInt(AParams.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));

  RS := FSS.FindRpRecordSet(FSS.RpId);
  if RS = nil then
    Exit(MakeJsonErrString(rcAnyError, 'Report not found'))
  else if RS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));

  try
    Result := ShowReportRecords(RS, SkipRows);
    FResultCode := rcAjaxOk;
  except
    on E: Exception do
      Result := MakeJsonErrString(rcAnyError, E.Message);
  end;
end;

function CustomSortByFormIndex(List: TStringList; Index1, Index2: Integer
  ): Integer;
var
  Fm1, Fm2: TdxForm;
begin
  Fm1 := TdxForm(List.Objects[Index1]);
  Fm2 := TdxForm(List.Objects[Index2]);
  Result := Fm1.Index - Fm2.Index;
end;

function THtmlShow.GetFirstForm: String;
var
  SL: TStringList;
  i, FmId: Integer;
  Fm: TdxForm;
  Intf: TdxIntf;
begin
  Result := '';
  FmId := 0;
  Intf := FSS.UserMan.GetIntf(FSS.RoleId);
  if Intf <> nil then
    for i := 0 to Intf.Tabs.Count - 1 do
    begin
      FmId := Intf.Tabs[i];
      if FSS.UserMan.CheckFmVisible(FSS.RoleId, FmId) then Break;
    end
  else
  begin
    SL := TStringList.Create;
    FSs.FormMan.GetFormList(SL);
    SL.CustomSort(@CustomSortByFormIndex);
    for i := 0 to SL.Count - 1 do
    begin
      Fm := TdxForm(SL.Objects[i]);
      if Fm.AutoOpen and FSS.UserMan.CheckFmVisible(FSS.RoleId, Fm.Id) then
      begin
        FmId := Fm.Id;
        Break;
      end;
    end;
    SL.Free;
  end;

  Result := '?fm=' + IntToStr(FmId);
  //Result := '<html><head><meta http-equiv=refresh content="1;url=?fm=' +
  //  IntToStr(FmId) + '"></head><body></body></html>';
end;

function THtmlShow.ShowFirstForm: String;
begin
  Result := '<html><head><meta http-equiv=refresh content="1;url=' +
    GetFirstForm + '"></head><body></body></html>';
end;

function THtmlShow.ApplySort(Fields: TStrings): String;
var
  SortOrder: String;
  Fm: TdxForm;
  SC: TdxSortCol;
  Col: TdxColumn;
  FId: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  Fm := FSs.FormMan.FindForm(FSs.FormId);
  if Fm = nil then Exit(MakeJsonErrString(rcAnyError, 'Fm=nil'));
  if not TryStrToInt(Fields.Values['id'], FId) then Exit(MakeJsonErrString(rcAnyError, 'Invalid id'));
  SortOrder := Fields.Values['order'];
  with Fm.Grid do
  begin
    Col := Columns.FindCol(FId);
    if Col = nil then Exit(MakeJsonErrString(rcAnyError, 'Col=nil'));

    if Fields.Values['add'] <> '1' then
      SortCols.Clear;
    SC := SortCols.FindCol(Columns.IndexOf(Col));
    if (SC = nil) and ((SortOrder = '') or (SortOrder = '0')) then
    begin
      SC := SortCols.AddCol;
      SC.Index := Columns.IndexOf(Col);
    end;
    if SC <> nil then
    begin
      if SortOrder = '1' then SortCols.DeleteCol(SC)
      else SC.Desc := SortOrder = '0';
    end;
  end;
  FResultCode := rcAjaxOk;
end;

function THtmlShow.ApplyQrySort(Fields: TStrings): String;
var
  QId, FreshValue: Longint;
  DSFieldName, SortOrder: String;
  QRS: TSsRecordSet;
  Col: TRpGridColumn;
  SC: TRpGridSortData;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(Fields.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));
  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));
  if not TryStrToInt(Fields.Values['qid'], QId) then Exit(MakeJsonErrString(rcAnyError, 'Invalid qid'));
  DSFieldName := Fields.Values['fid'];
  SortOrder := Fields.Values['order'];
  QRS := FRS.Queries.FindRpById(QId);
  if QRS = nil then Exit(MakeJsonErrString(rcAnyError, 'QRS=nil'));
  with QRS.RD.Grid do
  begin
    Col := FindColumnByFieldName(DSFieldName);
    if Col = nil then Exit;
    if Fields.Values['add'] <> '1' then SortCols.Clear;
    SC := SortCols.FindCol(Col);
    if (SC = nil) and ((SortOrder = '') or (SortOrder = '0')) then
      SC := SortCols.AddCol(Col, False);
    if SC <> nil then
    begin
      if SortOrder = '1' then SortCols.RemoveCol(SC)
      else SC.Desc := SortOrder = '0';
    end;
  end;
  FRS.QueryApplyChangeSort(QId);
  Result := GetEvalChangesAsJson;
  FResultCode := rcAjaxOk;
end;

function THtmlShow.ApplyRpSort(Fields: TStrings): String;
var
  RS: TSsRecordSet;
  DSFieldName, SortOrder: String;
  Col: TRpGridColumn;
  SC: TRpGridSortData;
begin
  Result := '';
  FResultCode := rcAjaxError;
  RS := FSS.FindRpRecordSet(FSS.RpId);
  if RS = nil then Exit(MakeJsonErrString(rcAnyError, 'RS=nil'));
  DSFieldName := Fields.Values['fid'];
  SortOrder := Fields.Values['order'];
  with RS.RD.Grid do
  begin
    Col := FindColumnByFieldName(DSFieldName);
    if Col = nil then Exit;
    if Fields.Values['add'] <> '1' then SortCols.Clear;
    SC := SortCols.FindCol(Col);
    if (SC = nil) and ((SortOrder = '') or (SortOrder = '0')) then
      SC := SortCols.AddCol(Col, False);
    if SC <> nil then
    begin
      if SortOrder = '1' then SortCols.RemoveCol(SC)
      else SC.Desc := SortOrder = '0';
    end;
  end;
  FResultCode := rcAjaxOk;
end;

procedure THtmlShow.Clear;
begin
  FCSS.Clear;
  //FPgCtrlId := 0;
  //FImgId := 0;
end;

function THtmlShow.GetTabOrder(C: TdxComponent): Integer;
begin
  Result := FTabOrderList.IndexOf(C);
  if Result >= 0 then Inc(Result);
end;

function THtmlShow.ShowTabs(UserMonShowed: Boolean): String;
var
  S, Cls: String;
  SL: TStringList;
  i, FmId: Integer;
  Fm: TdxForm;
  Intf: TdxIntf;
  TabExists: Boolean;
  RD: TReportData;
begin
  S := '';
  TabExists := False;
  Intf := FSS.UserMan.GetIntf(FSS.RoleId);
  if Intf <> nil then
    for i := 0 to Intf.Tabs.Count - 1 do
    begin
      FmId := Intf.Tabs[i];
      if FSS.UserMan.CheckFmVisible(FSS.RoleId, FmId) then
      begin
        if FmId = FSS.FormId then
        begin
          Cls := ' class=sel';
          TabExists := True;
        end
        else
          Cls := '';
        Fm := FSS.FormMan.FindForm(FmId);
        S := S + '<span' + Cls + '><a href="' + GetFormHRef(Fm) + '">' +
          Fm.GetRecordsCaption + '</a></span>';
      end;
    end
  else
  begin
    SL := TStringList.Create;
    FSs.FormMan.GetFormList(SL);
    SL.CustomSort(@CustomSortByFormIndex);
    for i := 0 to SL.Count - 1 do
    begin
      Fm := TdxForm(SL.Objects[i]);
      if Fm.AutoOpen and FSS.UserMan.CheckFmVisible(FSS.RoleId, Fm.Id) then
      begin
        if Fm.Id = FSS.FormId then
        begin
          Cls := ' class=sel';
          TabExists := True;
        end
        else
          Cls := '';
        S := S + '<span' + Cls + '><a href="' + GetFormHRef(Fm) + '">' +
          Fm.GetRecordsCaption + '</a></span>';
      end;
    end;
    SL.Free;
  end;

  if not TabExists then
  begin
    Fm := FSS.FormMan.FindForm(FSS.FormId);
    if Fm <> nil then
      S := S + '<span class=sel><a href="' + GetFormHRef(Fm) + '">' +
        Fm.GetRecordsCaption + '</a></span>'
    else if FSS.RpId > 0 then
    begin
      RD := FSS.ReportMan.FindReport(FSS.RpId);
      if RD <> nil then
        S := S + '<span class=sel><a href="?rp=' + IntToStr(RD.Id) + '">' + RD.Name +
          '</a></span>'
    end;
  end;

  if UserMonShowed then
    S := S + '<span class=sel><a href="?usermon">' + rsUserMonitor + '</a></span>';

  if S = '' then S := '<span>&nbsp;</span>';
  Result := S;
end;

function THtmlShow.ShowLastEditRecords(Fm: TdxForm): String;
var
  SL: TStringList;
  S: String;
  i: Integer;
begin
  S := '';
  SL := FSs.FormList.GetForm(Fm).LastEdits;
  for i := SL.Count - 1 downto 0 do
  begin
    S := S + '<a href="' + BuildHRef(1) + '&rec=' +
      IntToStr(PtrInt(SL.Objects[i])) + '">' + StrToHtml(SL[i], False, False) + '</a>';
    {if i > 0 then
      S := S + '&nbsp;|&nbsp;';}
  end;
  Result := S;
end;

function THtmlShow.ShowDateDetailField(Pr: TRpDateDetail): String;
var
  SL: TStrings;
  S: String;
  i: Integer;
begin
  S := '<select id=prd>';
  SL := TStringList.Create;
  SplitStr(rsDateDetail, ' ', SL);
  for i := 0 to SL.Count - 1 do
  begin
    S := S + '<option value=' + IntToStr(i);
    if i = Integer(Pr) then
      S := S + ' selected';
    S := S + '>' + SL[i] + '</option>';
  end;
  S := S + '</select>';
  SL.Free;
  Result := S;
end;

function THtmlShow.ShowSideBar: String;

  function ShowFm(Id: Integer): String;
  var
    Fm: TdxForm;
  begin
    Fm := FSS.FormMan.FindForm(Id);
    Result := '<a' + IIF(Id = FSS.FormId, ' class=sel', '') +
      ' href="' + GetFormHRef(Fm) + '">' + FSS.FormMan.FindForm(Id).GetRecordsCaption + '</a>';
  end;

  function ShowRp(Id: Integer): String;
  begin
    Result := '<a';
    if Id = FSS.RpId then Result := Result + ' class=sel';
    Result := Result + ' href="?rp=' + IntToStr(Id) + '">' + FSS.ReportMan.FindReport(Id).Name + '</a>';
  end;

  function ShowMenuItem(MI: TdxMenuItem): String;
  begin
    case MI.Kind of
      miMenu: Result := '<b>' + MI.Caption + '</b>';
      miDiv: Result := '<hr>';
      miForm: Result := ShowFm(MI.Id);
      miReport: Result := ShowRp(MI.Id);
      else Result := '';
    end;
  end;

  function ShowMenu(Menu: TdxMenuItemList): String;
  var
    i: Integer;
    MI: TdxMenuItem;
    Cls: String;
  begin
    if Menu.Count = 0 then Exit('');
    Result := '<ul>';
    for i := 0 to Menu.Count - 1 do
    begin
      MI := Menu[i];
      if ((MI.Kind = miForm) and (MI.Id = FSS.FormId)) or
        ((MI.Kind = miReport) and (MI.Id = FSS.RpId)) then
        Cls := ' class=sel'
      else
        Cls := '';
      if MI.Visible then
        Result := Result + '<li' + Cls + '>' + ShowMenuItem(MI) +
          ShowMenu(MI.Items) + '</li>';
    end;
    Result := Result + '</ul>';
  end;

  function CheckVisibleMenu(Menu: TdxMenuItemList): Boolean;
  var
    i: Integer;
    MI, DivMI: TdxMenuItem;
    ImDiv: Boolean;
  begin
    Result := False;
    for i := 0 to Menu.Count - 1 do
    begin
      MI := Menu[i];
      if MI.Kind = miForm then
        MI.Visible := FSS.UserMan.CheckFmVisible(FSS.RoleId, MI.Id)
      else if MI.Kind = miReport then
        MI.Visible := FSS.UserMan.CheckRpVisible(FSS.RoleId, MI.Id)
      else if MI.Kind = miMenu then
        MI.Visible := CheckVisibleMenu(MI.Items)
      else if MI.Kind = miDiv then
        MI.Visible := True;
      if (MI.Kind <> miDiv) and MI.Visible then Result := True;
    end;

    // Убираем лишние разделители
    ImDiv := True;
    DivMI := nil;
    for i := 0 to Menu.Count - 1 do
    begin
      MI := Menu[i];
      if MI.Kind = miDiv then
      begin
        MI.Visible := not ImDiv;
        ImDiv := True;
        DivMI := MI;
      end
      else if MI.Visible then
      begin
        ImDiv := False;
        if (Mi.Kind = miMenu) and (DivMI <> nil) then DivMI.Visible := False;
        DivMI := nil;
      end;
    end;
    if DivMI <> nil then DivMI.Visible := False;
  end;

var
  Intf: TdxIntf;
  SL: TStringListUtf8;
  i, Id: Integer;
  Cls: String;
begin
  Intf := FSS.UserMan.GetIntf(FSS.RoleId);
  if Intf <> nil then
  begin
    CheckVisibleMenu(Intf.Menu);
    Result := ShowMenu(Intf.Menu);
  end
  else
  begin
    SL := TStringListUtf8.Create;
    FSS.FormMan.GetFormList(SL);
    for i := SL.Count - 1 downto 0 do
      if not FSS.UserMan.CheckFmVisible(FSS.RoleId, TdxForm(SL.Objects[i]).Id) then
        SL.Delete(i);
    SL.Sort;
    Result := '<ul>';
    if SL.Count > 0 then
    begin
      Result := Result + '<li><b>' + rsData + '</b><ul>';
      for i := 0 to SL.Count - 1 do
      begin
        Id := TdxForm(SL.Objects[i]).Id;
        if (Id = FSS.FormId) then Cls := ' class=sel'
        else Cls := '';
        Result := Result + '<li' + Cls + '>' + ShowFm(Id) + '</li>';
      end;
      Result := Result + '</ul></li>';
    end;
    SL.Clear;
    FSS.ReportMan.GetReportList(SL);
    for i := SL.Count - 1 downto 0 do
      if not FSS.UserMan.CheckRpVisible(FSS.RoleId, TReportData(SL.Objects[i]).Id) then
        SL.Delete(i);
    SL.Sort;
    if SL.Count > 0 then
    begin
      Result := Result + '<li><b>' + rsReports + '</b><ul>';
      for i := 0 to SL.Count - 1 do
      begin
        Id := TReportData(SL.Objects[i]).Id;
        if (Id = FSS.RpId) then Cls := ' class=sel'
        else Cls := '';
        Result := Result + '<li' + Cls + '>' + ShowRp(TReportData(SL.Objects[i]).Id) + '</li>';
      end;
      Result := Result + '</ul></li>';
    end;
    Result := Result + '</ul>';
    SL.Free;
  end;
end;

function THtmlShow.ShowUser: String;
var
  U: TdxUser;
begin
  U := FSS.UserMan.Users.FindUser(FSS.UserId);
  Result := '<div><img src="/img/logout.svg"></div><div><a href=?logout>';
  if U <> nil then
    Result := Result + U.Name + ' - ';
  Result := Result + rsExit + '</a></div><div><button type=button ' +
    'onclick="closeSidebarClick(this)"><img src=/img/close.svg></button></div>';
end;

function THtmlShow.ShowDebug: String;
begin
  if not FSS.DebugShow then Exit('');
  Result := '<div id=debug-block class=debug><div class=debug-title><span>' + rsOutput +
    '</span>' + '<button type=button onclick="clearDebug()"><img src=/img/clear.svg></button>' +
    '<button type=button onclick="closeDebug()"><img src=/img/close.svg></button></div>' +
    '<div id=debug-body class=debug-body>' + StrToHtml(FSS.DebugText, True) +
    '</div></div>';
  FSS.DebugMsg := '';
end;

function THtmlShow.ShowDebugAjax: String;
begin
  FSS.DebugShow := True;
  Result := ShowDebug;
  FResultCode := rcAjaxOk;
  FSS.DebugMsg := '';
end;

function THtmlShow.CloseDebug: String;
begin
  FSS.DebugShow := False;
  FResultCode := rcAjaxOk;
  Result := '';
end;

function THtmlShow.ClearDebug: String;
begin
  FSS.DebugText := '';
  FResultCode := rcAjaxOk;
  Result := '';
end;

constructor THtmlShow.Create;
begin
  FCSS := TStringList.Create;
  FResultCode := rcOk;
end;

destructor THtmlShow.Destroy;
begin
  FCSS.Free;
  inherited Destroy;
end;

function THtmlShow.ShowFormNotFound: String;
begin
  Result := ShowErrorPage(rsAnyFormNotFound, rsAnyFormNotFoundMsg);
end;

function THtmlShow.ShowLoginUser: String;
var
  S: String;
begin
  S := '<div id=msg>' + rsWelcome + '</div><div>' +
     rsUser + '</div><div><input type=text name=user autofocus></div><div>' + rsPassword +
    '</div><div><input type=password name=pwd></div><div class=bn>' +
    '<div id=loader class=hide><div></div></div><button id=submit type=submit>' +
    '<img src="/img/login.svg"><span>' + rsLogin + '</span></button></div>';
  Result := LoadString(GetHtmlPath + 'loginuser.html');
  Result := StringReplace(Result, '[lng]', AppSet.Language, []);
  Result := StringReplace(Result, '[title]', rsLoginTitle, []);
  Result := StringReplace(Result, '[content]', S, []);
end;

function THtmlShow.ShowConnectionProgress(const UserName: String): String;
var
  S: String;
begin
  S := '<div id=msg data-connecting>' + rsConnecting + '</div><div>' +
     rsUser + '</div><div><input disabled type=text value="' + StrToHtml(UserName) + '"></div><div>' + rsPassword +
    '</div><div><input disabled type=password></div><div class=bn>' +
    '<div id=loader class=loader><div></div></div></div>';
  Result := LoadString(GetHtmlPath + 'loginuser.html');
  Result := StringReplace(Result, '[lng]', AppSet.Language, []);
  Result := StringReplace(Result, '[title]', rsLoginTitle, []);
  Result := StringReplace(Result, '[content]', S, []);
end;

function THtmlShow.ShowNullForm: String;
var
  Btns, Cnt: String;
begin
  Btns := '<button id=menubn type=button onclick="menuClick(this)"><img src="/img/menu.svg"></button>';
  if FSS.FormMan.FormCount > 0 then
    Cnt := rsSelectAnyForm
  else
    Cnt := rsNoFormInDB;
  Result := LoadString(GetHtmlPath + 'form.html');
  Result := StringReplace(Result, '[lng]', AppSet.Language, []);
  Result := StringReplace(Result, '[title]', rsFormNotSel, []);
  Result := StringReplace(Result, '[user]', ShowUser, []);
  Result := StringReplace(Result, '[sidebar]', ShowSideBar, []);
  Result := StringReplace(Result, '[tabs]', {'<span>&nbsp;</span>'}ShowTabs, []);
  Result := StringReplace(Result, '[buttons]', Btns, []);
  Result := StringReplace(Result, '[lasteditrecords]', '', []);
  Result := StringReplace(Result, '[pages]', '', []);
  Result := StringReplace(Result, '[content]', Cnt, []);
  Result := StringReplace(Result, '[debug]', ShowDebug, []);
end;

// Проверяем записи таблиц на возможность редактирования/удаления
{function THtmlShow.CheckAccessChildren: Boolean;
var
  i, FmId: Integer;
  DelCondStr, EditCondStr: String;
  DS: TSQLQuery;
  EB: TExpressionBuilder;
  DelCond, EditCond: TExpression;
  V: Variant;
  RS: TSsRecordSet;
  B: TBookMark;
begin
  Result := False;
  for i := 0 to FRS.Forms.Count - 1 do
  begin
    FmId := FRS.Forms[i].Form.Id;
    if not FSS.UserMan.CheckFmAdding(FSS.RoleId, FmId) then Exit;
  end;
  EditCond := nil; DelCond := nil;
  EB := TExpressionBuilder.Create;
  EB.SkipLabels := True;

  try

  for i := 0 to FRS.Forms.Count - 1 do
  begin
    RS := FRS.Forms[i];
    EB.RecordSet := RS;
    EditCondStr := Trim(FSS.UserMan.GetEditCond(FSS.RoleId, RS.Form.Id));
    DelCondStr := Trim(FSS.UserMan.GetDelCond(FSS.RoleId, RS.Form.Id));
    if (EditCondStr = '') and (DelCondStr = '') then Continue;
    if EditCondStr <> '' then
      EditCond := EB.Build(EditCondStr);
    if DelCondStr <> '' then
      DelCond := EB.Build(DelCondStr);
    DS := RS.DataSet;
    B := DS.GetBookmark;
    RS.DisableScrollEvents;
    DS.First;
    try
      while not DS.Eof do
      begin
        if EditCond <> nil then
        begin
          V := EditCond.Calc;
          if VarIsBool(V) and (V = False) then Exit;
        end;
        if DelCond <> nil then
        begin
          V := DelCond.Calc;
          if VarIsBool(V) and (V = False) then Exit;
        end;
        DS.Next;
      end;
    finally
      DS.GotoBookmark(B);
      DS.FreeBookmark(B);
      RS.EnableScrollEvents;
      FreeAndNil(DelCond);
      FreeAndNil(EditCond);
    end;
  end;
  Result := True;

  finally
    FreeAndNil(EB);
  end;
end;      }

{function THtmlShow.CheckCond(Fm: TdxForm; const Cond: String): Boolean;
var
  EB: TExpressionBuilder;
  E: TExpression;
  V: Variant;
begin
  if Trim(Cond) = '' then Exit(True);
  E := nil;
  EB := TExpressionBuilder.Create;
  EB.Session := FSS;
  EB.Form := Fm;
  EB.DataSet := FDS;

  try
    E := EB.Build(Cond);
    V := E.Calc;
    if not VarIsBool(V) then Exit(True);
    Result := V;
  finally
    FreeAndNil(E);
    EB.Free;
  end
end;  }

{function THtmlShow.CheckFormCond(FmId: Integer; Edit: Boolean): Boolean;
var
  V: Variant;
  Cond: String;
  RS: TSsRecordSet;
  Fm: TdxForm;
begin
  Result := False;
  if (FmId <> FSS.FormId) and (FmId <> FSS.TableId) then Exit;
  Fm := FSS.FormMan.FindForm(FmId);
  if Edit then
    Cond := FSS.UserMan.GetEditCond(FSS.RoleId, FmId)
  else
    Cond := FSS.UserMan.GetDelCond(FSS.RoleId, FmId);
  if Trim(Cond) = '' then Exit(True);

  if Fm.PId = 0 then
    RS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, 0)
  else
    RS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if RS = nil then Exit;

  if RS.DataSet.State in [dsInsert, dsEdit] then Exit(True)
  else begin
    V := EvalField(RS, Cond, True);
    if VarIsBool(V) then Result := V;
  end;
end;   }

function THtmlShow.ShowAccessDenied: String;
begin
  Result := ShowErrorPage(rsAccessDenied, rsAccessDenied);
end;

function THtmlShow.GetEvalChangesAsJson(Flags: TEvalFlags): String;
var
  i, j: Integer;
  JsonRoot, JsonObj: TJSONObject;
  JsonFields, JsonImages, JsonLabels, JsonQs, JsonErrs, JsonPivots,
    JsonProps, JsonTs: TJsonArray;
  F: TdxField;
  Lbl: TdxLabel;
  QRS, TRS: TSsRecordSet;
  L: TList;
  Prp: TUniPropData;
  TblGrid: TdxGrid;
begin
  JsonRoot := TJsonObject.Create;

  if FRS.GotoUrl = '' then
  begin
    if not (efDelRow in Flags) then
    begin
      if FRS.DataSet.IsEmpty then FRS.GotoUrl := 'back'
      else if (FRS.OldState <> FRS.DataSet.State) or (FRS.OldRecId <> FRS.RecId) then
      begin
        if (FSS.FormId = FRS.Form.Id) or (FSS.TableId = FRS.Form.Id) then
          FRS.Form.GotoForm(FRS.Form.FormCaption, FRS.RecId, gtoReplaceUrl);
      end;
    end;
  end;

  if FRS.GotoUrl = '' then
  begin

    JsonFields := TJsonArray.Create;
    JsonImages := TJsonArray.Create;
    JsonLabels := TJsonArray.Create;
    JsonProps := TJsonArray.Create;
    JsonTs := TJsonArray.Create;
    JsonQs := TJsonArray.Create;
    JsonPivots := TJsonArray.Create;
    JsonErrs := TJsonArray.Create;

    for i := 0 to FRS.ChangedFields.Count - 1 do
    begin
      F := TdxField(FRS.ChangedFields[i]);
      if F.Hidden then Continue;

      JsonObj := TJsonObject.Create(['field', FieldStr(F.Id)]);
      if F is TdxLookupComboBox then
      begin
        JsonObj.Add('key', FRS.DataSet.FieldByName(FieldStr(F.Id)).AsInteger);
        JsonObj.Add('value', FRS.GetObjValue(F));
        JsonFields.Add(JsonObj);
      end
      else if (F is TdxCalcEdit) or (F is TdxTimeEdit) then
      begin
        if not FRS.GetDSField(F).IsNull then
          JsonObj.Add('value', FormatField(FRS.GetDSField(F), False))
        else
          JsonObj.Add('value', '');
        JsonFields.Add(JsonObj);
      end
      else if F is TdxDBImage then
      begin
        JsonObj.Add('src', ShowDBImage(TdxDBImage(F), True));
        JsonImages.Add(JsonObj);
      end
      else if F is TdxFile then
      begin
        //JsonObj.Nulls['key'] := True;
        JsonObj.Add('value', FRS.DataSet.FieldByName(FieldStr(F.Id) + 'd').AsString);
        JsonFields.Add(JsonObj);
      end
      else
      begin
        //JsonObj.Nulls['key'] := True;
        JsonObj.Add('value', FRS.DataSet.FieldByName(FieldStr(F.Id)).AsString);
        JsonFields.Add(JsonObj);
      end
    end;

    for i := 0 to FRS.ChangedLabels.Count - 1 do
    begin
      Lbl := TdxLabel(FRS.ChangedLabels[i]);
      if Lbl.Hidden then Continue;
      JsonLabels.Add(TJSONObject.Create(['fieldName', LbL.FieldName,
        'value', StrToHtml(Lbl.Caption, True)]));
    end;

    for i := 0 to FRS.ChangedProps.Count - 1 do
    begin
      Prp := FRS.ChangedProps[i];
      if TdxControl(Prp.Control).Hidden then Continue;

      if Prp.PropName = 'recno' then
      begin
        if ((Prp.Control is TdxQueryGrid) and
          (FRS.ChangedQueries.IndexOf(TdxQueryGrid(Prp.Control).RecordSet) >= 0)) or
          ((Prp.Control is TdxForm) and
          (FRS.ChangedTables.IndexOf(TdxForm(Prp.Control).RecordSet) >= 0)) then
          Continue;
      end;
      JsonProps.Add(TJsonObject.Create(['name', Prp.GetControlName,
        'type', LowerCase(Prp.Control.ClassName), 'prop', Prp.PropName,
        'value', Prp.GetPropValue]));
    end;

    for i := 0 to FRS.ChangedTables.Count - 1 do
    begin
      TRS := TSsRecordSet(FRS.ChangedTables[i]);
      TblGrid := FRS.Form.FindTable(TRS.Form.Id);
      if TblGrid.Hidden then Continue;

      JsonTs.Add(TJSONObject.Create(['table', 't' + IntToStr(TRS.Form.Id),
        'html', ShowGrid(TblGrid, True)]));
    end;

    L := TList.Create;
    if FRS.ChangedQueries.Count > 0 then
      FRS.Form.GetPivots(L);
    for i := 0 to FRS.ChangedQueries.Count - 1 do
    begin
      QRS := TSsRecordSet(FRS.ChangedQueries[i]);

      if not QRS.QGrid.Hidden then
        JsonQs.Add(TJSONObject.Create(['query', 'q' + IntToStr(QRS.RD.Id),
          'html', ShowQueryGrid(FRS.Form.FindQuery(QRS.RD.Id), True)]));

      for j := 0 to L.Count - 1 do
        with TdxPivotGrid(L[j]) do
          if Id = QRS.RD.Id then
          begin
            Build;
            JsonPivots.Add(TJsonObject.Create(['pivot', Id, 'html',
              PivotGridToHtml(TdxPivotGrid(L[j]))]))
          end;
    end;
    L.Free;

    for i := 0 to FRS.Form.Errs.Count - 1 do
    begin
      JsonObj := TJsonObject.Create(['msg', FRS.Form.Errs[i], 'id', PtrInt(FRS.Form.Errs.Objects[i])]);
      JsonErrs.Add(JsonObj);
    end;
    FRS.Form.Errs.Clear;

    //if FRS.Actions <> nil then
    if FRS.MsgInfo.Visible then
    begin
      JsonRoot.Add('msgInfo', FRS.MsgInfoToJson);
    end;

    JsonRoot.Add('fields', JsonFields);
    JsonRoot.Add('images', JsonImages);
    JsonRoot.Add('labels', JsonLabels);
    JsonRoot.Add('props', JsonProps);
    JsonRoot.Add('tables', JsonTs);
    JsonRoot.Add('queries', JsonQs);
    JsonRoot.Add('pivots', JsonPivots);
    JsonRoot.Add('errors', JsonErrs);
    if FSS.DebugShow then
    begin
      JsonRoot.Add('debug', StrToHtml(FSS.DebugMsg, True));
      FSS.DebugMsg := '';
    end;
  end
  else
  begin
    JsonRoot.Add('gotoUrl', FRS.GotoUrl);
    JsonRoot.Add('gotoOption', Ord(FRS.GotoOption));
  end;

  if FRS.DocUrl <> '' then
    JsonRoot.Add('docUrl', FRS.DocUrl);

  if FRS.PrintErrors <> '' then
    JsonRoot.Add('printErrors', FRS.PrintErrors);

  if (FRS.GotoUrl = 'back') and (FRS.Form.PId = 0) then
  begin
    FSS.RecordSets.DeleteRecordSet(FRS);
    FRS := nil;
  end;

  Result := JsonRoot.AsJSON;
  JsonRoot.Free;
end;

function THtmlShow.ShowErrorPage(const Caption, Msg: String): String;
var
  Btns: String;
begin
  DebugStr('Error page. ' + Msg, FSS);
  Result := LoadString(GetHtmlPath + 'error.html');
  Result := StringReplace(Result, '[title]', Caption, []);
  Result := StringReplace(Result, '[user]', ShowUser, []);
  Result := StringReplace(Result, '[sidebar]', ShowSideBar, []);
  Result := StringReplace(Result, '[tabs]', {'<span>&nbsp;</span>'}ShowTabs, []);
  Btns := '<button id=menubn type=button onclick="menuClick(this)"><img src="/img/menu.svg"></button>';
  Result := StringReplace(Result, '[buttons]', Btns, []);
  Result := StringReplace(Result, '[err-title]', Caption, []);
  Result := StringReplace(Result, '[err-text]', Msg, []);
end;

function THtmlShow.ShowError2Page(const Caption, Msg: String): String;
begin
  DebugStr('Error page 2. ' + Msg);
  Result := LoadString(GetHtmlPath + 'error2.html');
  Result := StringReplace(Result, '[title]', Caption, []);
  Result := StringReplace(Result, '[err-title]', Caption, []);
  Result := StringReplace(Result, '[err-text]', Msg, []);
end;

function THtmlShow.ShowIndexPage: String;
var
  AppVer: String;
begin
  {FS.DateSeparator := '/';
  FS.ShortDateFormat := 'y/m/d';
  AppVer := FormatDateTime('yy.m.d', StrToDate({$INCLUDE %DATE}, FS));}
  AppVer := FormatDateTime('yy.m.d', GetBuildDate);

  Result := LoadString(GetHtmlPath + 'index.html');
  Result := StringReplace(Result, '[title]', rsServerItself, []);
  Result := StringReplace(Result, '[content]', Format(rsAboutText, [AppVer]), []);
end;

function THtmlShow.FieldChange(AFields: TStrings): String;
var
  Fm: TdxForm;
  F: TdxField;
  V: Variant;
  S: String;
  FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AFields.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));

  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if not (FRS.DataSet.State in [dsInsert, dsEdit]) then
    Exit(MakeJsonErrString(rcRecordReadOnly, rsCanNotChangeRec))
  else if FRS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));
  Fm := FRS.Form;

  try
    F := Fm.FindField(StrToInt(Copy(AFields.Names[0], 2, 255)));
    if F <> nil then
    begin
      S := AFields.ValueFromIndex[0];
      if CheckField(F, S, V) then
        FRS.ChangeField(F, V)
      else
        FRS.RevertFieldValue(F);
      Result := GetEvalChangesAsJson;
    end;
    FResultCode := rcAjaxOk;
  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
    end;
  end;
end;

function THtmlShow.QueryScroll(AParams: TStrings): String;
var
  QId, Row, FreshValue: Longint;
  QRS: TSsRecordSet;
  i: Integer;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AParams.Values['id'], QId) then
    Exit(MakeJsonErrString(rcAnyError, 'Invalid query id'));
  if not TryStrToInt(AParams.Values['row'], Row) then
    Exit(MakeJsonErrString(rcAnyError, 'Invalid query row'));
  if not TryStrToInt(AParams.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));

  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));

  try
    //FRS.QueryScroll(QId, Row - 1);
    FRS.ClearChanges;

    QRS := FRS.Queries.FindRpById(QId);
    QRS.DataSet.MoveBy(Row - QRS.DataSet.RecNo);
    if Row = 1 then
      if QRS.ScrollEventsEnabled then QRS.DataSet.AfterScroll(QRS.DataSet);
    for i := 0 to FRS.Queries.Count - 1 do
      if not FRS.Queries[i].QGrid.ManualRefresh then FRS.Queries[i].Open;

    Result := GetEvalChangesAsJson;
    FResultCode := rcAjaxOk;
  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
    end;
  end;
end;

function CompareInt(const elem1, elem2): Integer;
var
  d1: TMarkData absolute elem1;
  d2: TMarkData absolute elem2;
begin
  Result := d1.Start - d2.Start;
end;

function MarkFragments(const Value: String; Frags: TStrings): String;
var
  S, Frg: String;
  Marks: array [0..1000] of TMarkData;
  n, p, i, Len: Integer;
  M: TMarkData;

  function CheckMarkExists: Boolean;
  var
    j: Integer;
    MD: TMarkData;
  begin
    Result := False;
    for j := 0 to n - 1 do
    begin
      MD := Marks[j];
      if (p >= MD.Start) and (p <= MD.Start + MD.Len - 1) then
      begin
        Result := True;
        if p + Len > MD.Start + MD.Len then
          Marks[j].Len := p + Len - MD.Start;
      end
      else if (p + Len - 1 >= MD.Start) and (p + Len <= MD.Start + MD.Len) then
      begin
        Result := True;
        if p < MD.Start then
        begin
          Marks[j].Len := MD.Start + MD.Len - p;
          Marks[j].Start := p;
        end;
      end;
    end;
  end;

begin
  if Frags.Count = 0 then Exit(Value);
  Result := '';
  S := Utf8LowerCase(Value);
  n := 0;
  for i := 0 to Frags.Count - 1 do
  begin
    Frg := StrToHtml(Frags[i]);
    p := Utf8Pos(Frg, S, 1);
    Len := Utf8Length(Frg);
    while p > 0 do
    begin
      if not CheckMarkExists then
      begin
        Marks[n].Start := p;
        Marks[n].Len := Len;
        Inc(n);
      end;
      p := Utf8Pos(Frg, S, p + Len);
    end;
  end;
  AnySort(Marks, n, SizeOf(TMarkData), @CompareInt);

  p := 1;
  for i := 0 to n - 1 do
  begin
    M := Marks[i];
    Result := Result + Utf8Copy(Value, p, M.Start - p) + '<b>' +
      Utf8Copy(Value, M.Start, M.Len) + '</b>';
    p := M.Start + M.Len;
  end;
  Result := Result + Utf8Copy(Value, p, 8096);
end;

function SortFrags(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := -(Utf8Length(List[Index1]) - Utf8Length(List[Index2]));
end;

{
var
  SrcFm: TdxForm;
  SrcQRS: TSsRecordSet;

  function GetKeyField(DS: TDataSet): TField;
  begin
    if C.ListSource = 0 then
      Result := DS.Fields[0]
    else
      Result := DS.FieldByName(C.ListKeyField);
  end;

  function GetListField(DS: TDataSet; i: Integer): TField;
  begin
    if C.ListSource = 0 then
      Result := DS.Fields[i + 2]
    else
      Result := DS.FieldByName(C.ListFields[i].FieldName);
  end;

  function GetListColumnTitle(i: Integer): String;
  var
    SrcF: TdxField;
    idx: Integer;
  begin
    if C.ListSource = 0 then
    begin
      if SrcFm = nil then SrcFm := FSS.FormMan.FindForm(C.SourceTId);
      if i < 0 then
        SrcF := SrcFm.FindField(C.SourceFId)
      else
        SrcF := SrcFm.FindField(C.ListFields[i].FieldId);
      Result := SrcF.FieldName;
    end
    else
    begin
      SrcQRS := FRS.Queries.FindRpById(C.ListSource);
      idx := SrcQRS.RD.IndexOfNameDS(C.ListFields[i].FieldName);
      Result := SrcQRS.RD.GetFieldName(idx);
    end;
  end;
}

function CalcListColumnAutoWidth(C: TdxLookupComboBox): Integer;
var
  ZeroCount, W, i: Integer;
begin
  ZeroCount := 0;
  W := C.Width + C.ListWidthExtra;
  for i := 0 to C.ListFields.Count - 1 do
  begin
    if C.ListFields[i].Width = 0 then Inc(ZeroCount);
    W := W - C.ListFields[i].Width;
  end;
  if ZeroCount > 0 then
    Result := W div ZeroCount
  else
    Result := 0;
end;

procedure LCbxSetDisplayFormat(SS: TSession; LCbx: TdxLookupComboBox;
  DS: TDataSet);
var
  SrcFm: TdxForm;
  i: Integer;
  C: TdxField;
begin
  SrcFm := SS.FormMan.FindForm(LCbx.SourceTId);
  C := SrcFm.FindField(LCbx.SourceFId);
  SetDSFieldDisplayFormat(DS.Fields[1], GetComponentDisplayFormat(SS, SrcFm, C));
  for i := 0 to LCbx.ListFields.Count - 1  do
  begin
    C := SrcFm.FindField(LCbx.ListFields[i].FieldId);
    SetDSFieldDisplayFormat(DS.Fields[i+2], GetComponentDisplayFormat(SS, SrcFm, C));
  end;
end;

function THtmlShow.GetLookupComboBoxListForm(C: TdxLookupComboBox;
  Skip: Integer; const Frags: String; FragList: TStrings): String;
var
  W, i: Integer;
  SrcFm: TdxForm;
  SrcF: TdxField;
  DS: TSQLQuery;
  S, Attr: String;
begin
  Result := '';
  if (C.SourceTId = 0) or (C.SourceFId = 0) then Exit;
  if Skip = 0 then
  begin
    // Ширина первой колонки
    W := C.Width + C.ListWidthExtra;
    for i := 0 to C.ListFields.Count - 1 do
      W := W - C.ListFields[i].Width;
    Result := Result + '<colgroup><col width=' + IntToStr(W) + '>';
    //
    for i := 0 to C.ListFields.Count - 1 do
      Result := Result + '<col width=' + IntToStr(C.ListFields[i].Width) + '>';
    Result := Result + '</colgroup>';
    if C.ListFields.Count = 0 then
      Result := Result + '<tr style="display: none;"><th></th></tr>'
    else
    begin
      Result := Result + '<tr>';
      SrcFm := FSS.FormMan.FindForm(C.SourceTId);
      SrcF := SrcFm.FindField(C.SourceFId);
      Result := Result + '<th>' + SrcF.FieldName + '</th>';
      for i := 0 to C.ListFields.Count - 1 do
      begin
        SrcF := SrcFm.FindField(C.ListFields[i].FieldId);
        Result := Result + '<th>' + SrcF.FieldName + '</th>';
      end;
      Result := Result + '</tr>'
    end;
  end;

  // Пустая строка для очистки поля
  if Skip = 0 then
  begin
    Result := Result + '<tr key="">';
    for i := 0 to C.ListFields.Count do       // + основное поле
      Result := Result + '<td empty>&nbsp;</td>';
    Result := Result + '</tr>';
  end;

  DS := FSS.DBase.OpenDataSet(SqlLCbxSelect(FRS, C, Frags, 100, Skip));
  LCbxSetDisplayFormat(FSS, C, DS);

  while not DS.Eof do
  begin
    S := StrToHtml(GetFieldDisplayText(DS.Fields[1]));
    if S <> '' then
    begin
      S := MarkFragments(S, FragList);
      Attr := '';
    end
    else
    begin
      S := '&nbsp;';
      Attr := ' empty';
    end;
    Result := Result + '<tr key=' + DS.Fields[0].AsString + '><td' + Attr + '>' +
      S + '</td>';
    for i := 0 to C.ListFields.Count - 1 do
    begin
      Result := Result + '<td>';
      S := StrToHtml(GetFieldDisplayText(DS.Fields[i + 2]));
      if S <> '' then
      begin
        if C.ListFields[i].Searchable then
          S := MarkFragments(S, FragList);
      end
      else
        S := '&nbsp;';
      Result := Result + S + '</td>';
    end;
    Result := Result + '</tr>';
    DS.Next;
  end;
  if DS.RecordCount = 100 then
  begin
    Result := Result + '<tr><td colspan=' + IntToStr(C.ListFields.Count + 1) +
      '><input type=button value="' + rsMore + '"></td></tr>';
  end;
  DS.Free;
end;

function THtmlShow.GetLookupComboBoxListSource(C: TdxLookupComboBox;
  Skip: Integer; const Frags: String; FragList: TStrings): String;

  procedure SetupTextSearch(RD: TReportData);
  var
    i, idx: Integer;
    LF: TLCbxListField;
    pF: PRpField;
  begin
    if RD.SqlMode then Exit;

    RD.SearchText := Frags;
    for i := 0 to C.ListFields.Count - 1 do
    begin
      LF := C.ListFields[i];
      if not LF.Searchable then Continue;
      idx := RD.IndexOfNameDS(LF.FieldName);
      if idx < 0 then Continue;

      pF := RD.TryGetRpField(idx);
      if pF <> nil then pF^.TextSearch := True;
    end;
  end;

  procedure ClearTextSearch(RD: TReportData);
  var
    i: Integer;
  begin
    if RD.Sources.Count = 0 then Exit;

    for i := 0 to RD.Sources[0]^.Fields.Count - 1 do
      RD.Sources[0]^.Fields[i]^.TextSearch := False;
    RD.SearchText := '';
  end;

var
  W0, i, W, idx, n: Integer;
  QRS: TSsRecordSet;
  DSFields: TList;
  S, Attr: String;
begin
  Result := '';
  if (C.ListKeyField = '') or (C.ListFields.Count = 0) then Exit;

  SetTypedText(FSS, Frags);

  QRS := FRS.Queries.FindRpById(C.ListSource);

  if Skip = 0 then
  begin
    W0 := CalcListColumnAutoWidth(C);
    Result := Result + '<colgroup>';
    //
    for i := 0 to C.ListFields.Count - 1 do
    begin
      W := C.ListFields[i].Width;
      if W = 0 then W := W0;
      Result := Result + '<col width=' + IntToStr(W) + '>';
    end;
    Result := Result + '</colgroup>';
    if C.ListFields.Count = 1 then
      Result := Result + '<tr style="display: none;"><th></th></tr>'
    else
    begin
      Result := Result + '<tr>';
      for i := 0 to C.ListFields.Count - 1 do
      begin
        idx := QRS.RD.IndexOfNameDS(C.ListFields[i].FieldName);
        Result := Result + '<th>' + QRS.RD.GetFieldName(idx) + '</th>';
      end;
      Result := Result + '</tr>'
    end;
  end;

  // Пустая строка для очистки поля
  if Skip = 0 then
  begin
    Result := Result + '<tr key="">';
    for i := 0 to C.ListFields.Count - 1 do
      Result := Result + '<td empty>&nbsp;</td>';
    Result := Result + '</tr>';
  end;

  if Skip = 0 then
  begin
    SetupTextSearch(QRS.RD);
    QRS.Close;
    QRS.Open;
    ClearTextSearch(QRS.RD);
    QRS.DataSet.First;
  end
  else
  begin
    if not QRS.DataSet.Active then QRS.Open;
    QRS.QGrid.MoveTo(Skip);
  end;

  DSFields := TList.Create;
  DSFields.Add(QRS.DataSet.FieldByName(C.ListKeyField));
  for i := 0 to C.ListFields.Count - 1 do
    DSFields.Add(QRS.DataSet.FieldByName(C.ListFields[i].FieldName));

  for n := 1 to 100 do
  begin
    if QRS.DataSet.Eof then Break;

    {S := StrToHtml(TField(DSFields[1]).AsString);
    if S <> '' then
    begin
      S := MarkFragments(S, FragList);
      Attr := '';
    end
    else
    begin
      S := '&nbsp;';
      Attr := ' empty';
    end;              }
    Result := Result + '<tr key=' + TField(DSFields[0]).AsString + '>';{<td' + Attr + '>' +
      S + '</td>'; }
    for i := 0 to C.ListFields.Count - 1 do
    begin
      S := StrToHtml(GetFieldDisplayText(TField(DSFields[i + 1])));
      if S <> '' then
      begin
        if C.ListFields[i].Searchable then
          S := MarkFragments(S, FragList);
        Attr := '';
      end
      else
      begin
        S := '&nbsp;';
        Attr := ' empty';
      end;
      Result := Result + '<td' + IIF(i = 0, Attr, '') + '>';
      Result := Result + S + '</td>';
    end;
    Result := Result + '</tr>';
    QRS.DataSet.Next;
  end;

  if not QRS.DataSet.Eof then
  begin
    Result := Result + '<tr><td colspan=' + IntToStr(C.ListFields.Count + 1) +
      '><input type=button value="' + rsMore + '"></td></tr>';
  end;

  DSFields.Free;
end;

function THtmlShow.GetLookupComboBoxList(C: TdxLookupComboBox; Skip: Integer;
  const Frags: String; FragList: TStrings): String;
begin
  if C.ListSource = 0 then
    Result := GetLookupComboBoxListForm(C, Skip, Frags, FragList)
  else
    Result := GetLookupComboBoxListSource(C, Skip, Frags, FragList);
end;

function THtmlShow.GetComboBoxList(C: TdxComboBox; const Frags: String;
  Skip: Integer; FragList: TStrings): String;
var
  DS: TSQLQuery;
  S, Attr: String;
begin
  Result := '';
  if (C.SourceTId = 0) or (C.SourceFId = 0) then Exit;
  if Skip = 0 then
  begin
    Result := Result + '<colgroup><col width=100%></colgroup>';
    Result := Result + '<tr style="display: none;"><th></th></tr>';
  end;

  DS := FSS.DBase.OpenDataSet(SqlComboBoxSelect(FRS, C, Frags, 100, Skip));
  while not DS.Eof do
  begin
    S := StrToHtml(DS.Fields[1].AsString);
    if S <> '' then
    begin
      S := MarkFragments(S, FragList);
      Attr := '';
    end
    else
    begin
      Attr := ' empty';
      S := '&nbsp;';
    end;
    Result := Result + '<tr><td' + Attr + '>' + S + '</td></tr>';
    DS.Next;
  end;
  if DS.RecordCount = 100 then
    Result := Result + '<tr><td><input type=button value="' + rsMore + '"></td></tr>';
  DS.Free;
end;

procedure Abrakadabra(var S: String; out Depth: Integer);
var
  i, n, p, Len: Integer;
begin
  n := 0; p := 1; Len := Length(S);
  for i := 1 to Len do
  begin
    if S[i] = '\' then
    begin
      p := i + 1;
      Inc(n);
    end;
  end;
  //Result := DupeString('&nbsp;&nbsp;', n) + Copy(S, p, Len - p + 1);
  S := Copy(S, p, Len - p + 1);
  Depth := n;
end;

function THtmlShow.GetFilterLookupComboBoxList(C: TdxCustomComboBox;
  Skip: Integer; const Frags: String; FragList: TStrings): String;
var
  DS: TSQLQuery;
  S, Attr, Style: String;
  IsHierarhy: Boolean;
  Depth: Integer;
begin
  Result := '';
  if (C.SourceTId = 0) or (C.SourceFId = 0) then Exit;
  if Skip = 0 then
    Result := Result + '<colgroup><col width=100%></colgroup>' +
      '<tr style="display: none;"><th></th></tr>';

  IsHierarhy := IsHierarchyObj(FSS, C);
  Depth := 0;
  Style := '';

  // Пустая строка для очистки поля
  if Skip = 0 then
    Result := Result + '<tr key=""><td empty>&nbsp;</td></tr>';

  DS := FSS.DBase.OpenDataSet(SqlFilterLCbxSelect(FSS, C, Frags, 100, Skip));
  while not DS.Eof do
  begin
    S := StrToHtml(DS.Fields[1].AsString);
    if IsHierarhy then
    begin
      Abrakadabra(S, Depth);
      Style := ' style="' + 'padding-left: ' + IntToStr(Depth * 10) + 'px;"';
    end;
    if S <> '' then
    begin
      S := MarkFragments(S, FragList);
      Attr := '';
    end
    else
    begin
      S := '&nbsp;';
      Attr := ' empty';
    end;
    Result := Result + '<tr key=' + DS.Fields[0].AsString + '><td' + Style +
      Attr + '>' + S + '</td></tr>';
    DS.Next;
  end;
  if DS.RecordCount = 100 then
    Result := Result + '<tr><td><input type=button value="' + rsMore + '"></td></tr>';
  DS.Free;
end;

function THtmlShow.GetFilterComboBoxList(C: TdxCustomComboBox; Skip: Integer;
  const Frags: String; FragList: TStrings): String;
var
  DS: TSQLQuery;
  S, Attr: String;
begin
  Result := '';
  if (C.SourceTId = 0) or (C.SourceFId = 0) then Exit;
  if Skip = 0 then
    Result := Result + '<colgroup><col width=100%></colgroup>' +
      '<tr style="display: none;"><th></th></tr>';

  DS := FSS.DBase.OpenDataSet(SqlFilterLCbxSelect(FSS, C, Frags, 100, Skip));
  while not DS.Eof do
  begin
    S := StrToHtml(DS.Fields[1].AsString);
    if S <> '' then
    begin
      S := MarkFragments(S, FragList);
      Attr := '';
    end
    else
    begin
      S := '&nbsp;';
      Attr := ' empty';
    end;
    Result := Result + '<tr key=' + DS.Fields[0].AsString + '><td' +
      Attr + '>' + S + '</td></tr>';
    DS.Next;
  end;
  if DS.RecordCount = 100 then
    Result := Result + '<tr><td><input type=button value="' + rsMore + '"></td></tr>';
  DS.Free;
end;

procedure THtmlShow.CopyRecordForDuplicate(DupParam: TDuplicateParam);

  procedure CopyRecord(RS: TSsRecordSet; DS: TdxMemDataSet);
  var
    i: Integer;
    F: TdxField;
    Fm: TdxForm;
    L: TList;
    RDS: TSQLQuery;
  begin
    Fm := RS.Form;
    RDS := RS.DataSet;
    L := TList.Create;
    Fm.GetFields(L);
    DS.Append;
    for i := 0 to L.Count - 1 do
    begin
      F := TdxField(L[i]);
      if (F is TdxCounter) or (F is TdxObjectField) or (F is TdxRecordId) then Continue;
      if F is TdxDBImage then
      begin
        DS.FieldByName(FieldStr(F.Id)).Value := RDS.FieldByName(FieldStr(F.Id)).Value;
        DS.FieldByName(FieldStr(F.Id) + 'thumb').Value := RDS.FieldByName(FieldStr(F.Id) + 'thumb').Value;
        DS.FieldByName(FieldStr(F.Id) + 'dest').Value := RDS.FieldByName(FieldStr(F.Id) + 'dest').Value;
        DS.FieldByName(FieldStr(F.Id) + 'src').Value := RDS.FieldByName(FieldStr(F.Id) + 'src').Value;
      end
      else if F is TdxFile then
      begin
        DS.FieldByName(FieldStr(F.Id)).Value := RDS.FieldByName(FieldStr(F.Id)).Value;
        DS.FieldByName(FieldStr(F.Id) + 'dest').Value := RDS.FieldByName(FieldStr(F.Id) + 'dest').Value;
        DS.FieldByName(FieldStr(F.Id) + 'src').Value := RDS.FieldByName(FieldStr(F.Id) + 'src').Value;
        DS.FieldByName(FieldStr(F.Id) + 'd').Value := RDS.FieldByName(FieldStr(F.Id) + 'd').Value;
      end
      else
      begin
        DS.FieldByName(FieldStr(F.Id)).Value := RS.GetDSField(F).Value;
        {N := DS.FieldByName(FieldStr(F.Id)).Size;
        N2 := RS.GetDSField(F).Size;
        V := RS.GetDSField(F).Value;
        S := DS.FieldByName(FieldStr(F.Id)).AsString;
        V := Null; }
      end;
    end;
    DS.Post;
    L.Free;
  end;

  procedure CopyGrid(RS: TSsRecordSet; DS: TdxMemDataSet);
  var
    RDS: TSQLQuery;
    B: TBookMark;
  begin
    RDS := RS.DataSet;
    RS.DisableScrollEvents;
    B := RDS.GetBookmark;
    try
      RDS.First;
      while not RDS.Eof do
      begin
        CopyRecord(RS, DS);
        RDS.Next;
      end;
    finally
      RDS.GotoBookmark(B);
      RDS.FreeBookmark(B);
      RS.EnableScrollEvents;
    end;
  end;

var
  DS: TdxMemDataset;
  i, n: Integer;
begin
  if DupParam = dpAll then n := FRS.Forms.Count + 1
  else n := 1;
  SetLength(FRecordCopies, n);
  DS := TdxMemDataSet.Create(nil);
  DS.CopyFromDataset(FRS.DataSet, False);
  DS.Open;
  FRecordCopies[0] := DS;
  CopyRecord(FRS, DS);
  if DupParam = dpAll then
    for i := 0 to FRS.Forms.Count - 1 do
    begin
      DS := TdxMemDataSet.Create(nil);
      DS.CopyFromDataset(FRS.Forms[i].DataSet, False);
      DS.Open;
      FRecordCopies[i+1] := DS;
      CopyGrid(FRS.Forms[i], DS);
    end;
end;

procedure THtmlShow.PasteRecordForDuplicate(DupParam: TDuplicateParam);

  procedure PasteRecord(RS: TSsRecordSet; DS: TdxMemDataSet);
  var
    i: Integer;
    F: TdxField;
    Fm: TdxForm;
    L: TList;
    RDS: TSQLQuery;
  begin
    Fm := RS.Form;
    RDS := RS.DataSet;
    L := TList.Create;
    Fm.GetFields(L);
    for i := 0 to L.Count - 1 do
    begin
      F := TdxField(L[i]);
      if (F is TdxCounter) or (F is TdxObjectField) then Continue;
      if F is TdxDBImage then
      begin
        RDS.FieldByName(FieldStr(F.Id)).Value := DS.FieldByName(FieldStr(F.Id)).Value;
        RDS.FieldByName(FieldStr(F.Id) + 'thumb').Value := DS.FieldByName(FieldStr(F.Id) + 'thumb').Value;
        RDS.FieldByName(FieldStr(F.Id) + 'dest').Value := DS.FieldByName(FieldStr(F.Id) + 'dest').Value;
        RDS.FieldByName(FieldStr(F.Id) + 'src').Value := DS.FieldByName(FieldStr(F.Id) + 'src').Value;
      end
      else if F is TdxFile then
      begin
        RDS.FieldByName(FieldStr(F.Id)).Value := DS.FieldByName(FieldStr(F.Id)).Value;
        RDS.FieldByName(FieldStr(F.Id) + 'dest').Value := DS.FieldByName(FieldStr(F.Id) + 'dest').Value;
        RDS.FieldByName(FieldStr(F.Id) + 'src').Value := DS.FieldByName(FieldStr(F.Id) + 'src').Value;
        RDS.FieldByName(FieldStr(F.Id) + 'd').Value := DS.FieldByName(FieldStr(F.Id) + 'd').Value;
      end
      else if F is TdxRecordId then
        RS.SetDSField(F, RS.RecId)
      else
        RS.SetDSField(F, DS.FieldByName(FieldStr(F.Id)).Value);
    end;
    L.Free;
  end;

  procedure PasteGrid(RS: TSsRecordSet; DS: TdxMemDataSet);
  begin
    DS.First;
    while not DS.Eof do
    begin
      RS.Append;
      PasteRecord(RS, DS);
      RS.Post;
      DS.Next;
    end;
    RS.DataSet.First;
  end;

var
  i: Integer;
begin
  try
    PasteRecord(FRS, FRecordCopies[0]);
    if DupParam = dpAll then
      for i := 0 to FRS.Forms.Count - 1 do
        PasteGrid(FRS.Forms[i], FRecordCopies[i+1]);
  finally
    for i := 0 to High(FRecordCopies) do
      FRecordCopies[i].Free;
    SetLength(FRecordCopies, 0);
  end;
  if (DupParam = dpOne) and (FRS.Forms.Count > 0) then
    FRS.EvalAggFields('');
end;

function THtmlShow.ShowRedirect(const Url: String): String;
begin
  Result := '<html><head><meta http-equiv=refresh content="1;url=' +
    Url + '"></head><body></body></html>';
end;

function THtmlShow.ShowReportRecords(ARS: TSsRecordSet; Skip: Integer): String;
var
  DS: TSQLQuery;
  TrCls, S, TdCls, ImgName: String;
  i, EditFmId, RealColCount, RecCounter: Integer;
  Rp: TReportData;
  Col: TRpGridColumn;
  CanGotoRec, Deleting: Boolean;
begin
  DS := ARS.DataSet;
  if not DS.Active then Exit('');
  Rp := ARS.RD;

  if Rp.IsSimple then
  begin
    EditFmId := Rp.GetEditFormId;
    CanGotoRec := FSs.UserMan.CheckFmVisible(FSS.RoleId, EditFmId);
  end
  else
  begin
    CanGotoRec := False;
  end;

  if CanGotoRec then
  begin
    if FSS.UserMan.CheckFmEditing(FSS.RoleId, EditFmId) then
      ImgName := 'edit.svg'
    else
      ImgName := 'view.svg';
    Deleting := FSS.UserMan.CheckFmDeleting(FSS.RoleId, EditFmId);
  end;

  S := '';
  RecCounter := 1;
  DS.First;
  DS.MoveBy(Skip);
  while not DS.Eof do
  begin
    TrCls := CalcColoring(ARS, '');
    S := S + '<tr' + IIF(TrCls <> '', ' class=' + TrCls, '') + '>';
    if CanGotoRec then
    begin
      S := S + '<td>';
      if Rp.Grid.ShowRowDeleteButton and Deleting then S := S + '<img src="/img/delrow.svg" class=del>';
      S := S + '<a href="?fm=' + IntToStr(EditFmId) +
        '&rec=' + DS.FieldByName('id').AsString + '"><img src="/img/' + ImgName + '"></a></td>';
    end
    else
      S := S + '<td></td>';
    for i := 0 to Rp.Grid.ColumnCount - 1 do
    begin
      Col := Rp.Grid.Columns[i]; //TRpGridColumn(L[i]);
      if not Col.Visible or (Col.Width = 0) then Continue;
      TdCls := CalcColoring(ARS, Col.FieldNameDS);

      S := S + '<td' + IIF(TdCls <> '', ' class=' + TdCls, '') + '>';

      if not Col.IsImage then
        S := S + StrToHtml( FormatField(DS.FieldByName(Col.FieldNameDS)), True )
      else
        S := S + ShowQueryThumbnail(Rp, i, ARS.DataSet);

      S := S + '</td>';

      {S := S + '<td' + IIF(TdCls <> '', ' class=' + TdCls, '') + '>' +
        StrToHtml( FormatField(DS.FieldByName(Col.FieldNameDS)), True ) + '</td>';}
    end;
    S := S + '</tr>';
    if RecCounter = 100 then Break;
    DS.Next;
    Inc(RecCounter);
  end;

  if not DS.Eof then
  begin
    RealColCount := Rp.Grid.GetVisibleColumnCount;
    if CanGotoRec then Inc(RealColCount);
    S := S + '<tr class=gridbns><td colspan=' + IntToStr(RealColCount) +
      '><button type=button onclick="reportFetch()">' + rsMore +
      '</button></td></tr>'
  end
  else
    DS.Close;
  Result := S;
end;

function THtmlShow.GetList(AParams: TStrings): String;
var
  Skip: Integer;
  C: TdxField;
  sSkip, Frags: String;
  FragList: TStringList;
  FId: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm));

  sSkip := AParams.Values['skip'];
  if sSkip = '' then Skip := 0
  else if not TryStrToInt(sSkip, Skip) then Exit(MakeJsonErrString(rcAnyError, 'Invalid skip value'));
  if not TryStrToInt(AParams.Values['id'], FId) then Exit(MakeJsonErrString(rcAnyError, 'Invalid id'));
  Frags := AParams.Values['frags'];
  FragList := TStringList.Create;
  SplitStr(Utf8LowerCase(Frags), ' ', FragList);
  FragList.CustomSort(@SortFrags);

  C := FRS.Form.FindField(FId);
  try
    if C is TdxLookupComboBox then
      Result := GetLookupComboBoxList(TdxLookupComboBox(C), Skip, Frags, FragList)
    else if C is TdxComboBox then
      Result := GetComboBoxList(TdxComboBox(C), Frags, Skip, FragList);
    FResultCode := rcAjaxOk;
  except
    on E: Exception do
      Result := MakeJsonErrString(rcAnyError, E.Message);
  end;

  FragList.Free;
end;

function THtmlShow.GetFilterList(AParams: TStrings): String;
var
  Skip: Integer;
  C: TdxField;
  sSkip, Frags: String;
  FragList: TStringList;
  FId: Longint;
  Fm: TdxForm;
begin
  Result := '';
  FResultCode := rcAjaxError;
  Fm := FSS.FormMan.FindForm(FSS.FilterId);
  if Fm = nil then Exit( MakeJsonErrString(rcAnyError, 'Form not found.') );

  sSkip := AParams.Values['skip'];
  if sSkip = '' then Skip := 0
  else if not TryStrToInt(sSkip, Skip) then Exit;
  if not TryStrToInt(AParams.Values['id'], FId) then Exit;
  Frags := AParams.Values['frags'];
  FragList := TStringList.Create;
  SplitStr(Utf8LowerCase(Frags), ' ', FragList);
  FragList.CustomSort(@SortFrags);

  C := Fm.FindField(FId);
  try
    if C is TdxObjectField then C := GetObjectFieldField(FSS, TdxObjectField(C));
    if C is TdxLookupComboBox then
      Result := GetFilterLookupComboBoxList(TdxCustomComboBox(C), Skip, Frags, FragList)
    else if C is TdxComboBox then
      Result := GetFilterComboBoxList(TdxCustomComboBox(C), Skip, Frags, FragList);
    FResultCode := rcAjaxOk;
  except
    on E: Exception do
      Result := MakeJsonErrString(rcAnyError, E.Message);
  end;

  FragList.Free;
end;

function THtmlShow.GetRpFilterList(AParams: TStrings): String;
var
  Skip: Integer;
  C: TdxField;
  sSkip, Frags: String;
  FragList: TStringList;
  FId: Longint;
  RD: TReportData;
begin
  Result := '';
  FResultCode := rcAjaxError;
  RD := FSS.ReportMan.FindReport(FSS.RpId);
  if RD = nil then Exit( MakeJsonErrString(rcAnyError, 'Report not found.') );

  sSkip := AParams.Values['skip'];
  if sSkip = '' then Skip := 0
  else if not TryStrToInt(sSkip, Skip) then Exit;
  if not TryStrToInt(AParams.Values['id'], FId) then Exit;
  Frags := AParams.Values['frags'];
  FragList := TStringList.Create;
  SplitStr(Utf8LowerCase(Frags), ' ', FragList);
  FragList.CustomSort(@SortFrags);

  C := RpFieldToFormField(FSS, RD, FId);
  try
    if C is TdxLookupComboBox then
      Result := GetFilterLookupComboBoxList(TdxCustomComboBox(C), Skip, Frags, FragList)
    else if C is TdxComboBox then
      Result := GetFilterComboBoxList(TdxCustomComboBox(C), Skip, Frags, FragList);
    FResultCode := rcAjaxOk;
  except
    on E: Exception do
      Result := MakeJsonErrString(rcAnyError, E.Message);
  end;

  FragList.Free;
end;

function THtmlShow.CancelChanges: String;
begin
  Result := '';
  FResultCode := rcAjaxError;
  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS <> nil then
  begin
    if (FRS.Form.PId > 0) and (FRS.RecId <> FSS.TableRecId) then
      Exit(MakeJsonErrString(rcRecordNotActive, rsRecordNotActive));
    try
      if FRS.DataSet.State in [dsInsert, dsEdit] then
        FRS.Cancel;
      if FRS.Form.PId = 0 then
        FSS.RecordSets.DeleteRecordSet(FRS);
      FResultCode := rcAjaxOk;
    except
      on E: EPSException do
      begin
        FResultCode := rcAjaxError;
        Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
      end;
      on E: Exception do
      begin
        FResultCode := rcAjaxError;
        Result := MakeJsonErrString(rcAnyError, E.Message);
      end;
    end;
  end
  else
    Result := MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm);
end;

function THtmlShow.SetActivePage(AParams: TStrings): String;
var
  Index, FreshValue: Longint;
  C: TdxPageControl;
  PgId: String;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AParams.Values['index'], Index) then
    Exit(MakeJsonErrString(rcAnyError, 'Invalid index value.'));
  if not TryStrToInt(AParams.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));
  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));
  PgId := AParams.Values['pgid'];
  C := TdxPageControl(FRS.Form.FindComponent(PgId));
  if C = nil then
    Exit(MakeJsonErrString(rcAnyError, 'Component ' + PgId + ' not found.'));
  C.ActivePageIndex := Index;
  FResultCode := rcAjaxOk;
end;

function THtmlShow.AppendQueryRecord(AParams: TStrings): String;
var
  Fm: TdxForm;
  CurRS, QRS: TSsRecordSet;
  Parser: TQryAppendRecParser;
  QId, FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AParams.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));
  CurRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if CurRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if CurRS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));
  FRS := CurRS;
  if (FRS.DataSet.State = dsInsert) and (FRS.Form.ViewType <> vtSimpleForm) then
  begin
    Result := PostEditForm(AParams, dpNone, True);
    if FResultCode <> rcAjaxOk then Exit;
  end;

  if not TryStrToInt(AParams.Values['id'], QId) then Exit(MakeJsonErrString(rcAnyError, 'Invalid query id'));
  QRS := CurRS.Queries.FindRpById(QId);
  if (QRS = nil) or (QRS.RD.Sources.Count = 0) then Exit(MakeJsonErrString(rcAnyError, 'QRS=nil or empty query'));
  Fm := FSS.FormMan.FindForm(QRS.RD.GetEditFormId);
  if Fm = nil then Exit(MakeJsonErrString(rcAnyError, 'Fm=nil'));

  try

  FRS := nil;
  FRS := FSS.AddRecordSet(Fm);
  if QRS.QGrid.OnCreateForm <> nil then QRS.QGrid.OnCreateForm(QRS.QGrid, FRS.Form);
  FRS.CallerRS := QRS;

  FRS.OpenRecord(0);
  FRS.Append;
  if QRS.RD.GetSourceFilter <> '' then
  begin
    Parser := TQryAppendRecParser.Create;
    try try
      Parser.SrcRS := FRS;
      Parser.CurRS := CurRS;
      Parser.Parse(QRS.RD.GetSourceFilter);
    except
      on E: Exception do
        FRS.Form.Errs.Add(QRS.RD.Name + ': ' + E.Message);
    end;
    finally
      Parser.Free;
    end;
  end;
  FRS.OpenDetails;
  //FRS.GotoURL := GetHRef(FRS.Form.Id, FRS.RecId);
  //Result := GetEvalChangesAsJson(FRS);
  Result := GetHRef(FRS.Form.Id, FRS.RecId);
  FResultCode := rcAjaxOk;

  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
      if FRS <> nil then FSS.RecordSets.DeleteRecordSet(FRS);
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
      if FRS <> nil then FSS.RecordSets.DeleteRecordSet(FRS);
    end;
  end;
end;

function THtmlShow.QueryEdit(AParams: TStrings): String;
var
  Fm: TdxForm;
  QRS, NewRS: TSsRecordSet;
  QId, Row, FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AParams.Values['id'], QId) then
    Exit(MakeJsonErrString(rcAnyError, 'Invalid query id'));
  if not TryStrToInt(AParams.Values['row'], Row) then
    Exit(MakeJsonErrString(rcAnyError, 'Invalid query row'));
  if not TryStrToInt(AParams.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));

  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));

  QRS := FRS.Queries.FindRpById(QId);
  if (QRS = nil) or (QRS.RD.Sources.Count = 0) then Exit(MakeJsonErrString(rcAnyError, 'QRS=nil or empty query'));
  Fm := FSS.FormMan.FindForm(QRS.RD.GetEditFormId);
  if Fm = nil then Exit(MakeJsonErrString(rcAnyError, 'Fm=nil'));

  NewRS := nil;
  try
    QRS.DataSet.MoveBy(Row - QRS.DataSet.RecNo);
    NewRS := FSS.FindRecordSet(Fm.Id, QRS.RecId, 0);
    if NewRS = nil then
    begin
      NewRS := FSS.AddRecordSet(Fm);
      if QRS.QGrid.OnCreateForm <> nil then QRS.QGrid.OnCreateForm(QRS.QGrid, NewRS.Form);
      NewRS.OpenRecord(QRS.RecId);
    end;
    Result := GetHRef(Fm.Id, QRS.RecId);
    FResultCode := rcAjaxOk;

  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
      if NewRS <> nil then FSS.RecordSets.DeleteRecordSet(NewRS);
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
      if NewRS <> nil then FSS.RecordSets.DeleteRecordSet(NewRS);
    end;
  end;
end;

function THtmlShow.QueryDelRow(AParams: TStrings): String;
var
  Fm: TdxForm;
  QRS, NewRS, OldRS: TSsRecordSet;
  QId, Row, FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AParams.Values['id'], QId) then
    Exit(MakeJsonErrString(rcAnyError, 'Invalid query id'));
  if not TryStrToInt(AParams.Values['row'], Row) then
    Exit(MakeJsonErrString(rcAnyError, 'Invalid query row'));
  if not TryStrToInt(AParams.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));

  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));

  QRS := FRS.Queries.FindRpById(QId);
  if (QRS = nil) or (QRS.RD.Sources.Count = 0) then Exit(MakeJsonErrString(rcAnyError, 'QRS=nil or empty query'));
  Fm := FSS.FormMan.FindForm(QRS.RD.GetEditFormId);
  if Fm = nil then Exit(MakeJsonErrString(rcAnyError, 'Fm=nil'));

  NewRS := nil;
  try
    QRS.DataSet.MoveBy(Row - QRS.DataSet.RecNo);
    NewRS := FSS.FindRecordSet(Fm.Id, QRS.RecId, 0);
    if NewRS = nil then
    begin
      NewRS := FSS.AddRecordSet(Fm);
      //if QRS.QGrid.OnCreateForm <> nil then QRS.QGrid.OnCreateForm(QRS.QGrid, NewRS.Form);
      NewRS.OpenRecord(QRS.RecId);
      if NewRS.DataSet.RecordCount = 0 then
      begin
        FSS.RecordSets.DeleteRecordSet(NewRS);
        Exit(MakeJsonErrString(rcAnyError, rsRecordNotFound));
      end;
    end;
    FSS.Clear;
    FSS.FormId := Fm.Id;
    FSS.RecId := QRS.RecId;
    OldRS := FRS;
    Result := DeleteRecord;
    QRS.QGrid.Refresh;
    if FResultCode = rcAjaxOk then
    begin
      FRS := OldRS;
      Result := GetEvalChangesAsJson([efDelRow]);
    end;
  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
      if NewRS <> nil then FSS.RecordSets.DeleteRecordSet(NewRS);
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
      if NewRS <> nil then FSS.RecordSets.DeleteRecordSet(NewRS);
    end;
  end;
end;

function THtmlShow.QueryFetch(AParams: TStrings): String;
var
  QRS: TSsRecordSet;
  QId, FreshValue, SkipRecs: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AParams.Values['id'], QId) then
    Exit(MakeJsonErrString(rcAnyError, 'Invalid query id'));
  if not TryStrToInt(AParams.Values['skip'], SkipRecs) then
    Exit(MakeJsonErrString(rcAnyError, 'Invalid skip value'));
  if not TryStrToInt(AParams.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));

  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));

  QRS := FRS.Queries.FindRpById(QId);
  if (QRS = nil) or (QRS.RD.Sources.Count = 0) then Exit(MakeJsonErrString(rcAnyError, 'QRS=nil or empty query'));
  try
    Result := ShowQueryGridRecords(QRS, SkipRecs);
    FResultCode := rcAjaxOk;
  except
    on E: Exception do
      Exit(MakeJsonErrString(rcAnyError, E.Message));
  end;
end;

function THtmlShow.ObjectAppend(AParams: TStrings): String;
var
  ObjId: Longint;
  ObjF: TdxLookupComboBox;
  Fm: TdxForm;
  ObjRS: TSsRecordSet;
begin
  Result := '';
  FResultCode := rcAjaxError;
  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm));

  if not TryStrToInt(AParams.Values['id'], ObjId) then Exit(MakeJsonErrString(rcAnyError, 'Invalid object id'));
  ObjF := TdxLookupComboBox(FRS.Form.FindField(ObjId));
  Fm := FSS.FormMan.FindForm(ObjF.SourceTId);
  if Fm = nil then Exit(MakeJsonErrString(rcAnyError, 'Fm=nil'));

  ObjRS := nil;
  try
    ObjRS := FSS.AddRecordSet(Fm);
    if ObjF.OnCreateForm <> nil then ObjF.OnCreateForm(ObjF, ObjRS.Form);
    ObjRS.CallerRS := FRS;
    ObjRS.CallerObj := ObjF;
    ObjRS.OpenRecord(0);
    ObjRS.Append;
    Result := GetHRef(Fm.Id, ObjRS.RecId);
    FResultCode := rcAjaxOk;
  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
      if ObjRS <> nil then FSS.RecordSets.DeleteRecordSet(ObjRS);
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
      if ObjRS <> nil then FSS.RecordSets.DeleteRecordSet(ObjRS);
    end;
  end;
end;

function THtmlShow.ObjectEdit(AParams: TStrings): String;
var
  ObjId, RId: Longint;
  ObjF: TdxLookupComboBox;
  Fm: TdxForm;
  ObjRS: TSsRecordSet;
begin
  Result := '';
  FResultCode := rcAjaxError;
  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm));

  if not TryStrToInt(AParams.Values['id'], ObjId) then Exit(MakeJsonErrString(rcAnyError, 'Invalid object id'));
  if not TryStrToInt(AParams.Values['rec'], RId) then Exit(MakeJsonErrString(rcAnyError, 'Invalid record id'));
  ObjF := TdxLookupComboBox(FRS.Form.FindField(ObjId));
  Fm := FSS.FormMan.FindForm(ObjF.SourceTId);
  if Fm = nil then Exit(MakeJsonErrString(rcAnyError, 'Fm=nil'));

  ObjRS := nil;
  try
    ObjRS := FSS.FindRecordSet(Fm.Id, RId, 0);
    if ObjRS = nil then
    begin
      ObjRS := FSS.AddRecordSet(Fm);
      if ObjF.OnCreateForm <> nil then ObjF.OnCreateForm(ObjF, ObjRS.Form);
      ObjRS.OpenRecord(RId);
    end;
    ObjRS.CallerRS := FRS;
    ObjRS.CallerObj := ObjF;
    Result := GetHRef(Fm.Id, ObjRS.RecId);
    FResultCode := rcAjaxOk;
  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
      if ObjRS <> nil then FSS.RecordSets.DeleteRecordSet(ObjRS);
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
      if ObjRS <> nil then FSS.RecordSets.DeleteRecordSet(ObjRS);
    end;
  end;
end;

function THtmlShow.DuplicateRecord(AParams: TStrings; DupParam: TDuplicateParam
  ): String;
begin
  Result := PostEditForm(AParams, DupParam, True);
  if FResultCode <> rcAjaxOk then Exit;
  Result := '';
  FResultCode := rcAjaxError;
  {Fm := FSs.FormMan.FindForm(FSs.GetFmId);
  if Fm = nil then Exit(MakeJsonErrString(rcAnyError, 'Fm=nil'));  }

  try

  {if FSS.TableId > 0 then
  begin
    FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
    if FRS = nil then
      Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm));
  end
  else
  begin
    FRS := FSS.AddRecordSet(Fm);
    FRS.OpenRecord(0);
  end;  }

  FRS.BeginDuplicate;

  FRS.Append;
  FRS.OpenDetails;
  PasteRecordForDuplicate(DupParam);

  if FSs.TableId = 0 then
  begin
    FSs.RecId := FRS.RecId;
    Result := BuildHRef(2);
  end
  else
  begin
    FSs.TableRecId := FRS.RecId;
    Result := BuildHRef(4);
  end;
  FResultCode := rcAjaxOk;
  FSS.Msg := rsDuplicateRecordWasCreated;
  FRS.EndDuplicate;

  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
      if FRS.Form.PId = 0 then FSS.RecordSets.DeleteRecordSet(FRS);
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
      if FRS.Form.PId = 0 then FSS.RecordSets.DeleteRecordSet(FRS);
    end;
  end;
end;

function THtmlShow.BnClick(AParams: TStrings): String;
var
  Bn: TdxButton;
  FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AParams.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));
  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));
  Bn := TdxButton(FRS.Form.FindComponent(AParams.Values['bn']));
  if (Bn = nil) {or not Bn.ActionEnabled} then
    Exit(MakeJsonErrString(rcAnyError, rsButtonNotFound));
  FResultCode := rcAjaxOk;

  try
    FRS.ClearChanges;
    Bn.Click;
    Result := GetEvalChangesAsJson;
  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
    end;
  end;
end;

function THtmlShow.MsgBnClick(AParams: TStrings): String;

  function GetBnCode(const BnType: String): Integer;
  begin
    if BnType = 'ok' then Result := 1
    else if BnType = 'cancel' then Result := 2
    else if BnType = 'abort' then Result := 3
    else if BnType = 'retry' then Result := 4
    else if BnType = 'ignore' then Result := 5
    else if BnType = 'yes' then Result := 6
    else if BnType = 'no' then Result := 7
    else if BnType = 'all' then Result := 8
    else if BnType = 'noToAll' then Result := 9
    else if BnType = 'yesToAll' then Result := 10
    else if BnType = 'close' then Result := 11
    else Result := 0;
  end;

  function GetBnType(const BnType: String): TMsgDlgBtn;
  begin
    if BnType = 'ok' then Result := mbOk
    else if BnType = 'cancel' then Result := mbCancel
    else if BnType = 'abort' then Result := mbAbort
    else if BnType = 'retry' then Result := mbRetry
    else if BnType = 'ignore' then Result := mbIgnore
    else if BnType = 'yes' then Result := mbYes
    else if BnType = 'no' then Result := mbNo
    else if BnType = 'all' then Result := mbAll
    else if BnType = 'noToAll' then Result := mbNoToAll
    else if BnType = 'yesToAll' then Result := mbYesToAll
    else if BnType = 'close' then Result := mbClose
    else if BnType = 'help' then Result := mbHelp
    else raise Exception.Create('Unknown BnType');
  end;

var
  AR: TActionRunner;
  BnType: String;
  FreshValue: Longint;
  i: Integer;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AParams.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));

  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));
  FResultCode := rcAjaxOk;

  try //try
    FRS.MsgInfo.Visible := False;
    //AR := TActionRunner(FRS.Actions);
    BnType := AParams.Values['bn'];
    SetLength(BnType, Length(BnType) - 2);
    if FRS.MsgInfo.IsAction then
      FRS.Form.ActionResult := GetBnCode(BnType)
    else if FRS.Form.OnMsgButtonClick <> nil then
      FRS.Form.OnMsgButtonClick(FRS.Form, GetBnType(BnType));

    //if AR <> nil then AR.Run;
    for i := FRS.ActionList.Count - 1 downto 0 do
    begin
      AR := TActionRunner(FRS.ActionList[i]);
      try
        AR.Run;
      finally
        if not AR.NeedContinue then
        begin
          AR.Free;
          FRS.ActionList.Delete(i);
        end;
      end;
    end;
    Result := GetEvalChangesAsJson;
  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
    end;
  end;
  {finally
    if (FRS <> nil) and (AR <> nil) and not AR.NeedContinue then
    begin
      AR.Free;
      FRS.Actions := nil;
    end;
  end;  }
end;

function THtmlShow.ShowUserMonitor: String;
var
  Html, ElTime, S, Btns: String;
  i: Integer;
  Sess: TSession;
  UL: TStringListUtf8;
  U: TdxUser;
  UMD: TUserMonData;
  Elapsed: Integer;
  Mon: TDBEngine;
  DS: TSQLQuery;
begin
  UL := TStringListUtf8.Create;

  // Web

  DebugStr('User monitor. Sessions.Lock');
  MainSrv.Sessions.Lock;
  try
    for i := 0 to MainSrv.Sessions.Count - 1 do
    begin
      Sess := MainSrv.Sessions[i];
      if Sess.DBItem <> FSS.DBItem then Continue;
      UMD := TUserMonData.Create;
      U := Sess.UserMan.Users.FindUser(Sess.UserId);
      if U <> nil then
        UMD.UserName := U.Name
      else
        UMD.UserName := Format(rsNoNameUser, [UL.Count + 1]);
      UMD.IP := Sess.IP;
      UMD.ConnectTime := Sess.ConnectTime;
      UMD.IsWeb := True;
      UL.AddObject(UMD.UserName, UMD);
    end;

  finally
    DebugStr('User monitor. Sessions.UnLock');
    MainSrv.Sessions.UnLock;
  end;

  // Desktop

  DS := nil;

  Mon := TDBEngine.Create;
  Mon.Database := FSS.DBase.Database;
  Mon.DBPwd:=FSS.Dbase.DBPwd;

  try
    Mon.Connect;
    S := 'select c.id, c.uid, m.mon$remote_address, c.mode, c.dtime, CURRENT_TIMESTAMP as ctime ' +
      'from dx_conn c left join mon$attachments m on c.id=m.mon$attachment_id order by c.id';
    DS := Mon.OpenDataSet(S);
    while DS.Eof = False do
    begin
      U := FSS.UserMan.Users.FindUser(DS.Fields[1].AsInteger);
      if U = nil then Continue;
      UMD := TUserMonData.Create;
      UMD.UserName := U.Name;
      UMD.IP := DS.Fields[2].AsString;
      UMD.IsSingle := DS.Fields[3].AsInteger = 1;
      UMD.ConnectTime := DS.Fields[4].AsDateTime;
      UL.AddObject(UMD.UserName, UMD);
      DS.Next;
    end;
  finally
    FreeAndNil(DS);
    Mon.Disconnect;
    Mon.Free;
  end;

  UL.Sort;

  Html := '<thead><tr><th>#</th><th>' + rsUserName + '</th><th>' + rsIP +
    '</th><th>' + rsConnectTime + '</th><th>' + rsElapsedTime + '</th><th>' +
    rsType + '</th></tr></thead><tbody>';
  for i := 0 to UL.Count - 1 do
  begin
    UMD := TUserMonData(UL.Objects[i]);
    Elapsed := MinutesBetween(Now, UMD.ConnectTime);
    ElTime := Format(rsElapsedTimeStr, [Elapsed div 60, Elapsed - (Elapsed div 60) * 60]);
    Html := Html + '<tr><td>' + IntToStr(i+1) + '</td><td>' + UMD.UserName +
      '</td><td>' + UMD.IP + '</td><td>' + DateTimeToStr(UMD.ConnectTime) +
      '</td><td>' + ElTime + '</td><td>' + IIF(UMD.IsWeb, rsWeb, rsDesktop) + '</td></tr>';
  end;
  Html := Html + '</tbody>';

  Result := LoadString(GetHtmlPath + 'usermon.html');
  Result := StringReplace(Result, '[title]', rsUserMonitor, []);
  Result := StringReplace(Result, '[user]', ShowUser, []);
  Result := StringReplace(Result, '[sidebar]', ShowSideBar, []);
  Result := StringReplace(Result, '[tabs]', ShowTabs(True), []);
  Btns := '<button id=menubn type=button onclick="menuClick(this)"><img src="/img/menu.svg"></button><span>' +
    rsUserMonitor + '</span>';
  Result := StringReplace(Result, '[buttons]', Btns, []);
  Result := StringReplace(Result, '[content]', Html, []);

  for i := 0 to UL.Count - 1 do
    UL.Objects[i].Free;
  UL.Free;
end;

function THtmlShow.ShowCompileError: String;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  FSS.MetaData.ScriptMan.MessagesToList(SL, True);
  Result := ShowErrorPage(rsCompileError, le2br(SL.Text));
  SL.Free;
end;

function THtmlShow.GetStyleClass(C: TdxControl): String;
var
  S: String;
  i: Integer;
begin
  S := '';
  if (C is TdxForm) or (C is TdxGrid) or (C is TdxQueryGrid) then
  begin
    S := GetFontCSS(C.Font, nil, True);
  end
  else if (C is TdxPageControl) or (C is TdxTabSheet) or (C is TdxGroupBox) then
    S := S + GetFontCSS(C.Font, C.Parent.Font)
  else if C is TdxImage then
    with TdxImage(C) do
    begin
      if Center then S := S + 'object-position:center;'
      else S := S + 'object-position: left top;';
      if Stretch then
      begin
        if KeepSize then
          S := S + 'object-fit:scale-down;'
        else if Proportional then
          S := S + 'object-fit:contain;'
      end
      else
        S := S + 'object-fit:none;'
    end
  else if C is TdxDBImage then
    Result := 'dbimg'
    //S := S + 'object-position:center;object-fit:scale-down;'
  else
    S := GetFontCSS(C.Font, C.Parent.Font) + GetColorCSS(C);
  if C is TdxLabel then S := S + 'white-space:nowrap;';

  if S <> '' then
  begin
    i := FCSS.IndexOf(S);
    if i < 0 then
      i := FCSS.Add(S);
    Result := 'c' + IntToStr(i);
  end;

  if (C is TdxField) and (C.Form.Errs.IndexOfObject(TObject(PtrInt(TdxField(C).Id))) >= 0) then
    Result := Result + ' err';
end;

function THtmlShow.GetGridButtonsStyle(Gr: TdxCustomGrid): String;
var
  S: String;
  i: Integer;
begin
  S := GetFontCSS(Gr.ButtonFont, nil);
  i := FCSS.IndexOf(S);
  if i < 0 then
    i := FCSS.Add(S);
  Result := 'c' + IntToStr(i);
end;

function THtmlShow.GetRpGridStyle(RD: TReportData): String;
var
  S: String;
  i: Integer;
begin
  S := GetFontCSS(RD.Grid.Font, nil, True);
  i := FCSS.IndexOf(S);
  if i < 0 then
    i := FCSS.Add(S);
  Result := 'c' + IntToStr(i);
end;

function THtmlShow.CreateCSS: String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FCSS.Count - 1 do
    if FCSS.Objects[i] = nil then
      Result := Result + '.c' + IntToStr(i) + ' {' + FCSS[i] + '}' + LineEnding
    else
      Result := Result + FCSS[i] + LineEnding;
end;

function THtmlShow.GetFieldValue(F: TdxField): String;
begin
  if F is TdxFile then
    Result := FRS.DataSet.FieldByName(FieldStr(F.Id) + 'd').AsString
  else if (F is TdxCalcEdit) or (F is TdxTimeEdit) or (F is TdxObjectField) then
    Result := FormatField(FRS.DataSet.FieldByName(FieldStr(F.Id)))
  else
    Result := FRS.DataSet.FieldByName(FieldStr(F.Id)).AsString;
  Result := StrToHtml(Result);
end;

function THtmlShow.ShowFilterRecordId(F: TdxRecordId; const Value: String
  ): String;
var
  V, V2: String;
  p: SizeInt;
begin
  p := Pos(' .. ', Value);
  V := Copy(Value, 1, p - 1);
  V2 := Copy(Value, p + 4, 255);
  Result := '<input class=one type=text value="' + V +
    '"><input class=two type=text value="' + V2 + '">';
end;

function AbrakadabraOld(S: String): String;
var
  i, n, p, Len: Integer;
begin
  n := 0; p := 1; Len := Length(S);
  for i := 1 to Len do
  begin
    if S[i] = #9 then
    begin
      p := i + 1;
      Inc(n);
    end;
  end;
  Result := DupeString('&nbsp;&nbsp;', n) + Copy(S, p, Len - p + 1);
end;

function THtmlShow.ShowFilterObjectField(F: TdxObjectField; const Value: String
  ): String;
var
  ObjF: TdxField;
begin
  ObjF := GetObjectFieldField(FSS, F);
  if ObjF <> nil then
    Result := ShowFilterField(ObjF, Value)
  else
    Result := '';
end;

function THtmlShow.ShowFilterCounter(F: TdxCounter; const Value: String
  ): String;
var
  V, V2: String;
  p: SizeInt;
begin
  p := Pos(' .. ', Value);
  V := Copy(Value, 1, p - 1);
  V2 := Copy(Value, p + 4, 255);
  Result := '<input class=one type=text value="' + V +
    '"><input class=two type=text value="' + V2 + '">';
end;

function THtmlShow.ShowFilterTimeEdit(F: TdxTimeEdit; const Value: String
  ): String;
var
  V, V2: String;
  p: SizeInt;
begin
  p := Pos(' .. ', Value);
  V := Copy(Value, 1, p - 1);
  V2 := Copy(Value, p + 4, 255);
  Result := '<input class=onebn type=text value="' + V +
    '"><button type=button class=inpbn onclick="showTime(this)"><img src="/img/time.svg"></button>' +
    '<input class=twobn type=text value="' + V2 +
    '"><button type=button class=inpbn onclick="showTime(this)"><img src="/img/time.svg"></button>';
end;

function THtmlShow.ShowFilterEdit(F: TdxEdit; const Value: String): String;
begin
  Result := '<input class=single type=text value="' + Value + '">';
end;

function THtmlShow.ShowFilterCalcEdit(F: TdxCalcEdit; const Value: String
  ): String;
var
  V, V2: String;
  p: SizeInt;
begin
  p := Pos(' .. ', Value);
  V := Copy(Value, 1, p - 1);
  V2 := Copy(Value, p + 4, 255);
  Result := '<input class=one type=text value="' + V +
    '"><input class=two type=text value="' + V2 + '">';
end;

function THtmlShow.ShowFilterDateEdit(F: TdxDateEdit; const Value: String
  ): String;
var
  V, V2, S: String;
  p: SizeInt;
  pt: TPeriodType;
begin
  S := Value;
  if Copy(S, 1, 1) = '$' then
  begin
    pt := TPeriodType(StrToInt(Copy(S, 2, 10)));
    S := PeriodToStr(pt);
  end;
  p := Pos(' .. ', S);
  V := Copy(S, 1, p - 1); V2 := Copy(S, p + 4, 255);
  Result := '<input class=onebn type=text value="' + V +
    '"><button type=button class=inpbn onclick="showCalendar(this)"><img src="/img/calendar.svg"></button>' +
    '<input class=twobn type=text value="' + V2 +
    '"><button type=button class=inpbn onclick="showCalendar(this)"><img src="/img/calendar.svg"></button>';
end;

function THtmlShow.ShowFilterCheckBox(F: TdxCheckBox; const Value: String
  ): String;
var
  V1, V2: String;
begin
  Result := '<select class=single>' +
    '<option value=""></option><option value=1 %s>' + rsYes + '</options>' +
    '<option value=0 %s>' + rsNo + '</option></select>';
  V1 := ''; V2 := '';
  if Value = '1' then V1 := 'selected'
  else if Value = '0' then V2 := 'selected';
  Result := Format(Result, [V1, V2]);
end;

function THtmlShow.ShowFilterComboBox(C: TdxComboBox; const Value: String
  ): String;
var
  S, Attr: String;
  i: Integer;
  FixedList: Boolean;
begin
  FixedList := (C.SourceTId = 0) or (C.SourceFId = 0);
  Result := '<input class=cbx type=text value="' + Value +
    '" onkeydown="' + IIF(FixedList, 'fixedCbxKeyDown(', 'cbxKeyDown(') + ')"' +
    '><button class=cbxbn type=button tabindex=-1 onclick="' +
    IIF(FixedList, 'showFixedList(', 'showList(') + ')"' + '><i></i></button>';
  if FixedList then
  begin
    Result := Result + '<div class=listcbx style="overflow-x: hidden; overflow-y: auto; border: 1px solid black; ' +
      'background: white; visibility: hidden; position: absolute;">' +
      '<table class=list><colgroup><col width=100%></colgroup>';
    for i := 0 to C.Items.Count - 1 do
    begin
      S := C.Items[i];
      if S <> '' then Attr := ''
      else
      begin
        S := '&nbsp;';
        Attr := ' empty';
      end;
      Result := Result + '<tr><td' + Attr + '>' + S + '</td></tr>';
    end;
    Result := Result + '</table></div>';
  end;
end;

{var
  i: Integer;
  Sel, V: String;
  OkSelect: Boolean;
begin
  Result := '<select class=two name=f' + IntToStr(C.Id) + idx + '>';
  OkSelect := False;
  if C.SourceTId = 0 then
  begin
    Result := Result + '<option value=""></option>';
    for i := 0 to C.Items.Count - 1 do
    begin
      Sel := '';
      if Utf8CompareText(C.Items[i], Value) = 0 then
      begin
        Sel := ' selected ';
        OkSelect := True;
      end;
      Result := Result + '<option value="' + C.Items[i] + '"' + Sel + '>' + C.Items[i] + '</option>';
    end;
  end
  else
  begin
    Result := Result + GetFilterComboBoxItems(C, Value);
    OkSelect := Pos('" selected >', Result) > 0;
  end;
  Result := Result + '</select>';
  if (not OkSelect) and (Value <> '') then V := ' value="' + Value + '" ';
  Result := Result + '&nbsp;<input class=two type=text ' + V + ' name=f' + IntToStr(C.Id) + 'e>';
end;   }

function THtmlShow.ShowFilterLookupComboBox(C: TdxLookupComboBox;
  const Value: String): String;
var
  ObjValue: String;
begin
  if Value <> '' then ObjValue := StrToHtml(GetObjFieldValue(FSS, C, StrToInt(Value), False))
  else ObjValue := '';
  Result := '<input type=hidden value="' + Value + '">' +
    '<input class=cbx type=text readonly value="' +  ObjValue +
    '" onclick="showList()" onkeydown="lcbxKeyDown()"' +
    '><button class=cbxbn type=button tabindex=-1' +
    ' onclick="showList()"><i></i></button>';
end;

function THtmlShow.ShowFilterField(F: TdxField; const Value: String): String;
var
  S: String;
begin
  S := '';
  if (F is TdxEdit) or (F is TdxMemo) or (F is TdxDBImage) or (F is TdxFile) then S := ShowFilterEdit(TdxEdit(F), Value)
  else if F is TdxCalcEdit then S := ShowFilterCalcEdit(TdxCalcEdit(F), Value)
  else if F is TdxDateEdit then S := ShowFilterDateEdit(TdxDateEdit(F), Value)
  else if F is TdxCheckBox then S := ShowFilterCheckBox(TdxCheckBox(F), Value)
  else if F is TdxLookupComboBox then S := ShowFilterLookupComboBox(TdxLookupComboBox(F), Value)
  else if F is TdxComboBox then S := ShowFilterComboBox(TdxComboBox(F), Value)
  else if F is TdxTimeEdit then S := ShowFilterTimeEdit(TdxTimeEdit(F), Value)
  else if F is TdxCounter then S := ShowFilterCounter(TdxCounter(F), Value)
  else if F is TdxObjectField then S := ShowFilterObjectField(TdxObjectField(F), Value)
  else if F is TdxRecordId then S := ShowFilterRecordId(TdxRecordId(F), Value);
  Result := S;
end;

function THtmlShow.ShowSortFields(Fm: TdxForm): String;
var
  S: String;
  SL: TStringListUtf8;
  i: Integer;
  F: TdxField;
begin
  S := '<select onchange="fieldAdd(this)"><option value=""></option>';
  SL := TStringListUtf8.Create;
  Fm.GetFields(SL);
  SL.Sort;
  for i := 0 to SL.Count - 1 do
  begin
    F := TdxField(SL.Objects[i]);
    if not (F is TdxDBImage) then
      S := S + '<option value=' + IntToStr(F.Id) + '>' + F.FieldName + '</option>';
  end;
  S := S + '</select>';
  SL.Free;
  Result := S;
end;

{function THtmlShow.GetRecordCount(Fm: TdxForm): Integer;
var
  SQL: String;
begin
  SQL := SQLSelectStatement(FSS, Fm, nil, True,
  SQL := 'select count(id) from ' + TableStr(Fm.Id);
  if Flt <> '' then SQL := SQL + ' where ' + Flt;
  with DBase.OpenDataSet(SQL) do
  try
    Result := Fields[0].AsInteger;
  finally
    Free;
  end;
end;      }

function THtmlShow.ShowPages(Fm: TdxForm; RecCount, Page, Cnt: Integer): String;
var
  PgN, StartPage, EndPage, i: Integer;
  Dsb: String;
begin
  PgN := RecCount div Cnt;
  if RecCount mod Cnt > 0 then
    Inc(PgN);
  if PgN <= 1 then Exit('');
  // Расчитываем начальную и конечную страницы
  if PgN <= 5 then
  begin
    StartPage := 1;
    EndPage := PgN;
  end
  else
  begin
    if Page > 3 then
    begin
      if Page + 2 <= PgN then
      begin
        StartPage := Page - 2;
        EndPage := Page + 2;
      end else
      begin
        StartPage := PgN - 5;
        EndPage := PgN;
      end;
    end
    else
    begin
      StartPage := 1;
      EndPage := 5;
    end;
  end;
  Result := '';
  Dsb := '';
  if Page = 1 then Dsb := ' disabled ';
  Result := Result + '<button type=button onclick="location.href=''' +
    BuildHRef(1) + '&pg=1''"' + Dsb + '>|&lt;</button>&nbsp;<button type=button onclick="location.href=''' +
    BuildHRef(1) + '&pg=' + IntToStr(Page - 1) + '''"' + Dsb + '>&lt;</button>&nbsp;';
  for i := StartPage to EndPage do
  begin
    if i <> Page then
      Result := Result + '<button type=button onclick="location.href=''' +
        BuildHRef(1) + '&pg=' + IntToStr(i) + '''">' + IntToStr(i) + '</button>&nbsp;'
      else
        Result := Result + '<span>' + IntToStr(i) + '</span>&nbsp;';
  end;
  Dsb := '';
  if Page = PgN then Dsb := ' disabled ';
  Result := Result + '<button type=button onclick="location.href=''' +
    BuildHRef(1) + '&pg=' + IntToStr(Page + 1) + '''"' + Dsb + '>&gt;</button>&nbsp;' +
    '<button type=button onclick="location.href=''' +
    BuildHRef(1) + '&pg=' + IntToStr(PgN) + '''"' + Dsb + '>&gt;|</button>';
end;

function THtmlShow.ControlReadOnly(C: TdxControl): Boolean;
begin
  Result := ((Trim(GetExpression(C)) <> '') and not GetEditable(C)) or (FRS.Editing <> asOk)
    or not FSS.UserMan.CheckControlEditing(FSS.RoleId, C.Form.Id, C.Name) or
    ((C is TdxCounter) and TdxCounter(C).ReadOnly);
end;

function THtmlShow.ShowRecordId(C: TdxRecordId): String;
begin
  Result := '<input class="' + GetStyleClass(C) + '" name=f' + IntToStr(C.Id) +
    ' id=f' + IntToStr(C.Id) +
    ' type=text value="' + GetFieldValue(C) + '" style="' + GetBoundsCSS(C) +
    GetVisible(C) + '" tabindex=' + IntToStr(GetTabOrder(C)) + GetEnabled(C) +
    ' readonly>';
end;

function THtmlShow.ShowChart(C: TdxChart): String;
begin
  Result := '<div id=' + C.Name + ' class=chart style="' + GetBoundsCSS(C) + GetVisible(C) + '"></div>';
end;

function THtmlShow.ShowPivotGrid(C: TdxPivotGrid): String;
var
  QRS: TSsRecordSet;
begin
  Result := '<div class=pgrid id=pgrid' + IntToStr(C.Id) +
    ' style="' + GetBoundsCSS(C) + GetVisible(C) + '"' + GetEnabled(C) + '>';
  QRS := FRS.Queries.FindRpById(C.Id);
  if QRS <> nil then
  begin
    C.DataSet := QRS.DataSet;
    if QRS.DataSet.Active then
    begin
      C.Build;
      Result := Result + PivotGridToHtml(C);
    end;
  end;
  Result := Result + '</div>';
end;

function THtmlShow.ShowButton(C: TdxButton): String;
var
  ImgD: TImageData;
  FlNm: String;
begin
  FlNm := '';
  if C.ImageName <> '' then
  begin
    ImgD := FSS.ImageMan.FindImage(C.ImageName);
    if ImgD <> nil then
      FlNm := GetImagesPath(FSS.MetaData, True) + ImgD.Name + '.' + ImgD.Ext;
  end
  else if C.HasGlyph then
    FlNm := GetEmbeddedImagesPath(FSS.MetaData, True) + IntToStr(C.Form.Id) +
      C.Name + '.png';

  {if C.ActionEnabled = Unassigned then
    C.ActionEnabled := CheckActionEnabled(C); }

  Result := '<button id=' + C.Name + ' type=button class="' +
    GetStyleClass(C) + ' bn" style="' + GetBoundsCSS(C) + GetVisible(C) + '" tabindex=' +
    IntToStr(GetTabOrder(C)) + GetEnabled(C) + ' onclick="bnClick(''' + C.Name + ''')">' +
    //IIF(C.ActionEnabled = True, ' onclick="bnClick(''' + C.Name + ''')"', ' disabled') +
    IIF(FlNm <> '', '<img src="' + FlNm + '">', '') +
    IIF(C.Caption <> '', '<span>' + StrToHtml(C.Caption) + '</span>', '') + '</button>';
end;

function THtmlShow.ShowCounter(C: TdxCounter): String;
begin
  Result := '<input class="' + GetStyleClass(C) + '" name=f' + IntToStr(C.Id) +
    ' id=f' + IntToStr(C.Id) +
    ' type=text value="' + GetFieldValue(C) + '" style="' + GetBoundsCSS(C) +
    GetVisible(C) + '" tabindex=' + IntToStr(GetTabOrder(C)) + GetEnabled(C) +
    GetReadOnly(C) + '>';
end;

function THtmlShow.ShowTimeEdit(C: TdxTimeEdit): String;
begin
  Result := '<input class="' + GetStyleClass(C) + '" name=f' + IntToStr(C.Id) +
    ' id=f' + IntToStr(C.Id) +
    ' type=text value="' + GetFieldValue(C) + '" style="' + GetBoundsCSS(C) +
    GetVisible(C) + '" tabindex=' + IntToStr(GetTabOrder(C)) + GetEnabled(C) +
    GetReadOnly(C) + '>';
  if not C.HideButton then
    Result := Result + '<button class=editbn type=button style="position: absolute; left: ' +
      IntToStr(C.Left + C.Width) + 'px; top: ' + IntToStr(C.Top) + 'px; width: ' +
      IntToStr(C.Height) + 'px; height: ' +
      IntToStr(C.Height) + 'px; padding: 0px;' + GetVisible(C) + '" ' +
      IIF(not ControlReadOnly(C) and C.Enabled, 'onclick="showTime(this)">', 'disabled>') +
      '<img src="/img/time.svg"></button>';
end;

function THtmlShow.ShowObjectField(C: TdxObjectField): String;
begin
  Result := '<input class="' + GetStyleClass(C) + '" name=f' + IntToStr(C.Id) +
    ' id=f' + IntToStr(C.Id) +
    ' type=text value="' + GetFieldValue(C) + '" style="' + GetBoundsCSS(C) +
    GetVisible(C) + '" tabindex=' + IntToStr(GetTabOrder(C)) + GetEnabled(C) +
    ' readonly>';
end;

function THtmlShow.ShowFile(C: TdxFile): String;
begin
  Result := '<input class="' + GetStyleClass(C) + '" name=f' + IntToStr(C.Id) +
    ' id=f' + IntToStr(C.Id) +
    ' type=text value="' + GetFieldValue(C) + '" style="' + GetBoundsCSS(C) +
    GetVisible(C) + '" tabindex=' + IntToStr(GetTabOrder(C)) + GetEnabled(C) +
    ' readonly' + IIF(ControlReadOnly(C), ' file-readonly', '') +
    '><button class=editbn type=button style="left: ' + IntToStr(C.Left + C.Width + 2) + 'px; ' +
    'top: ' + IntToStr(C.Top) + 'px; width: ' + IntToStr(C.Height) +
    'px; height: ' + IntToStr(C.Height) + 'px; position: absolute; padding: 0px;' +
    GetVisible(C) + '"' + GetEnabled(C) + 'onclick="showFileMenu(' + IntToStr(C.Id) +
    ')"><img src="/img/file.svg"></button>';
end;

function THtmlShow.ShowDBImage(C: TdxDBImage; FileNameOnly: Boolean): String;
var
  ImgName: String;
begin
  Result := '';
  if not C.ShowThumbnail then
  begin
    ImgName := IntToStr(C.Form.Id) + '-' + IntToStr(FSs.GetRecId) + '-' +
      IntToStr(C.Id) + '.png';
    try
      if SaveImageToFile(GetCachePath(FSS) + ImgName, Max(C.Width, C.Height), C, FRS.DataSet) then
        ImgName := GetCachePath(FSS, True) + ImgName
      else
        ImgName := '/img/noimg.svg';
    except
      ;
    end;
  end
  else
  begin
    ImgName := ShowDBImageThumbnail(FRS.DataSet, C, True);
    if C.SourceFileName = '' then ImgName := '/img/noimg.svg'
    else if ImgName = '' then ImgName := '/img/nothumb.svg';
  end;

  if FileNameOnly then
    Result := ImgName
  else
    Result := '<img id=f' + IntToStr(C.Id) + ' class="' + GetStyleClass(C) + '"' +
      IIF(ControlReadOnly(C), ' data-readonly', '') +
      IIF(C.StorageType = StorageTypeLink, ' data-noupload', '') +
      ' style="cursor: pointer; ' + GetBoundsCSS(C) + GetVisible(C) +
      '" src="' + ImgName + '"' + GetEnabled(C) + 'onclick="showImageMenu(' +
      IntToStr(C.Id) + ')">';
end;

function THtmlShow.ShowDBImageThumbnail(DS: TDataSet; C: TdxDBImage;
  FileNameOnly: Boolean): String;
var
  ImgName: String;
begin
  Result := '';
  if (C.ThumbSize = 0) or (DS.FieldByName(FieldStr(C.Id) + 'thumb').IsNull) then Exit;
  ImgName := IntToStr(C.Form.Id) + '-' + DS.Fields[0].AsString + '-' +
    IntToStr(C.Id) + 't.png';
  try
    if SaveThumbnailToFile(GetCachePath(FSS) + ImgName, C.Id, DS) then
      ImgName := GetCachePath(FSS, True) + ImgName
    else
      ImgName := '';
  except
    ;
  end;
  if FileNameOnly then
    Result := ImgName
  else
    Result := '<img class="thumb" src="' + ImgName + '" width=' + IntToStr(C.ThumbSize) +
      'px height=' + IntToStr(C.ThumbSize) + '>';
end;

function THtmlShow.ShowQueryThumbnail(RD: TReportData; ColIndex: Integer;
  DS: TDataSet): String;
var
  Col: TRpGridColumn;
  ImgName: String;
  pF: PRpField;
  idx: Integer;
begin
  Result := '';
  Col := RD.Grid.Columns[ColIndex];
  if Col.ThumbSize = 0 then Exit;

  ImgName := IntToStr(RD.Id) + '-' + IntToStr(ColIndex) + '-' + IntToStr(DS.RecNo) +
    't.png';

  idx := RD.IndexOfNameDS(Col.FieldNameDS);
  pF := RD.TryGetRpField(idx);

  if pF <> nil then
  begin
    try
      if SaveThumbnailToFile(GetCachePath(FSS) + ImgName, pF^.Id, DS) then
        ImgName := GetCachePath(FSS, True) + ImgName
      else
        ImgName := '';
    except
      ;
    end;

    if ImgName <> '' then
      Result := '<img class="thumb" src="' + ImgName + '" width=' + IntToStr(Col.ThumbSize) +
        'px height=' + IntToStr(Col.ThumbSize) + '>';

  end;
end;

function THtmlShow.ShowImage(C: TdxImage): String;
//var
  //ImgName: String;
  //ImgD: TImageData;
begin
  //ImgName := C.GetImagePath(True);
  {if C.ImageName <> '' then
  begin
    ImgD := FSS.ImageMan.FindImage(C.ImageName);
    if ImgD <> nil then
      ImgName := ImagesPath(FSS.MetaData, True) + ImgD.Name + '.' + ImgD.Ext;
  end
  else if C.Ext <> '' then
    ImgName := EmbeddedImagesPath(FSS.Metadata, True) + IntToStr(C.Form.Id) +
      C.Name + '.' + C.Ext; }
  Result := '<img id=' + C.Name + ' class="' + GetStyleClass(C) + '" style="' + GetBoundsCSS(C) +
    GetVisible(C) + '"' + GetEnabled(C) + 'src="' +
    C.GetImagePath(True) + '">';
end;

function GetFontSize(C: TdxControl): Integer;
begin
  Result := C.Font.Size;
  if (Result = 0) and (C.Parent <> nil) then
    Result := GetFontSize(C.Parent);
  if Result = 0 then Result := 10;
end;

(*function THtmlShow.ShowTabSheet(C: TdxTabSheet; Visible: Boolean): String;
var
  CC: TdxControl;
  H: Integer;
begin
  CC := C.Parent;
  H := GetFontSize(CC);
  H := H + H div 2;
  Result := '<div id=' + C.Name + ' style="position: absolute; left: 0px; top: ' +
    IntToStr(H) + 'px; width: ' +
    IntToStr(CC.Width) + 'px; ' + 'height: ' + IntToStr(CC.Height - H) +
    'px; ';
  if not Visible then
    Result := Result + 'display: none; ';
  Result := Result + '" class=tabsheet><div class=tabarea>' + ShowContainer(C) + '</div></div>';
end;*)

function THtmlShow.ShowTabSheet(C: TdxTabSheet; Visible: Boolean): String;
begin
  Result := '<div id=' + C.Name;
  if not Visible then
    Result := Result + ' style="display: none;"';
  Result := Result + ' class="tabsheet ' + GetStyleClass(C) + '">' +
    ShowContainer(C) + '</div>';
end;

(*function THtmlShow.ShowPageControl(C: TdxPageControl): String;
var
  i, H, idx: Integer;
  Tab: TdxTabSheet;
begin
  H := GetFontSize(C);
  H := H + H div 2;

  idx := C.ActivePageIndex;
  if idx < 0 then idx := C.ActivePageIndex;
  Result := '<div class="pages ' + GetStyleClass(C) + '" id=' + C.Name + ' data-tabindex=' + IntToStr(idx) +
    GetEnabled(C) + ' style="position: absolute; left: ' + IntToStr(C.Left) + 'px; top: ' +
    IntToStr(C.Top) + 'px; width: ' + IntToStr(C.Width) + 'px; ' +
    'height: ' + IntToStr(C.Height) + 'px;' + GetVisible(C) + '">';
  Result := Result + '<div class=tabsheets style="position: absolute; left: 0px; top: 0px; width: ' +
    IntToStr(C.Width) + 'px; ' + 'height: ' + IntToStr(H) + 'px;">';
  for i := 0 to C.PageCount - 1 do
  begin
    Tab := C.Pages[i];
    // !!! Доступ
    if FSS.UserMan.CheckControlVisible(FSS.RoleId, C.Form.Id, Tab.Name) then
    begin
      Result := Result + '<span class="' + GetStyleClass(C);
      if i = idx then
        Result := Result + ' sel';
      Result := Result + '"';
      if not Tab.TabVisible then Result := Result + ' style="display:none;"';
      Result := Result + ' onclick="pagecontrol_tabclick(''' + C.Name + ''',' + IntToStr(i) + ')">' +
        StringReplace(StrToHtml(Tab.Caption), ' ', '&nbsp;', [rfReplaceall]) + '</span>';
    end
    else
      Result := Result + '<span style="display:none;"></span>';
  end;
  Result := Result + '</div>';
  for i := 0 to C.PageCount - 1 do
  begin
    Tab := C.Pages[i];
    // !!! Доступ
    if FSS.UserMan.CheckControlVisible(FSS.RoleId, C.Form.Id, Tab.Name) then
      Result := Result + ShowTabSheet(Tab, i = idx)
    else
      Result := Result + '<div style="display:none;"></div>';
  end;
  Result := Result + '</div>';
end;      *)

function THtmlShow.ShowPageControl(C: TdxPageControl): String;
var
  i, idx: Integer;
  Tab: TdxTabSheet;
begin
  idx := C.ActivePageIndex;
  if idx < 0 then idx := C.ActivePageIndex;
  Result := '<div class="pages ' + GetStyleClass(C) + '" id=' + C.Name + ' data-tabindex=' + IntToStr(idx) +
    GetEnabled(C) + ' style="position: absolute; left: ' + IntToStr(C.Left) + 'px; top: ' +
    IntToStr(C.Top) + 'px; width: ' + IntToStr(C.Width) + 'px; ' +
    'height: ' + IntToStr(C.Height) + 'px;' + GetVisible(C) + '">';
  Result := Result + '<div class=tabsheets>';
  for i := 0 to C.PageCount - 1 do
  begin
    Tab := C.Pages[i];
    // !!! Доступ
    //if FSS.UserMan.CheckControlVisible(FSS.RoleId, C.Form.Id, Tab.Name) then
    if Tab.ControlVisible then
    begin
      Result := Result + '<span';
      if i = idx then
        Result := Result + ' class=sel';
      if not Tab.TabVisible then Result := Result + ' style="display:none;"';
      Result := Result + ' onclick="pagecontrol_tabclick(''' + C.Name + ''',' + IntToStr(i) + ')">' +
        StrToHtml(Tab.Caption) + '</span>';
    end
    else
      Result := Result + '<span style="display:none;"></span>';
  end;
  Result := Result + '</div><div class=tabsarea>';
  for i := 0 to C.PageCount - 1 do
  begin
    Tab := C.Pages[i];
    //if FSS.UserMan.CheckControlVisible(FSS.RoleId, C.Form.Id, Tab.Name) then
    if Tab.ControlVisible then
      Result := Result + ShowTabSheet(Tab, i = idx)
    else
      Result := Result + '<div style="display:none;"></div>';
  end;
  Result := Result + '</div></div>';
end;

(*function THtmlShow.ShowGroup(C: TdxGroupBox): String;
var
  H: Integer;
begin
  //H := GetFontSize(C);
  H := C.Font.Size;
  Result := '<div id=' + C.Name + ' style="position:absolute;left: ' + IntToStr(C.Left) + 'px;top:' +
    IntToStr(C.Top + (H div 2) + 2) + 'px;width:' + IntToStr(C.Width) + 'px;' +
    'height:' + IntToStr(C.Height - (H div 2) - 2) + 'px;' + GetVisible(C) +
    '" class="groupbox ' + GetStyleClass(C) + '">';
  Result := Result + '<div style="position:absolute;left:1px;top:' + IntToStr(H div 2 + 4) + 'px;' +
    'width:' + IntToStr(C.Width - 4) + 'px;' +
    'height:' + IntToStr(C.Height - H - 8) + 'px;">';
  Result := Result + ShowContainer(C);
  Result := Result + '</div></div>';
  Result := Result + '<span style="position:absolute;left:' + IntToStr(C.Left + 8) +
    'px;top:' + IntToStr(C.Top) + 'px;' + GetVisible(C) + '" class="groupcap ' +
    GetStyleClass(C) + '">' + StrToHtml(C.Caption) + '</span>';
end;    *)

function THtmlShow.ShowGroup(C: TdxGroupBox): String;
begin
  Result := '<div id=' + C.Name + ' style="position:absolute;left: ' + IntToStr(C.Left) + 'px;top:' +
    IntToStr(C.Top) + 'px;width:' + IntToStr(C.Width) + 'px;' +
    'height:' + IntToStr(C.Height) + 'px;' + GetVisible(C) +
    '" class="groupbox ' + GetStyleClass(C) + '">' +
    '<div class=groupframe><div class=groupcap>' + StrToHtml(C.Caption) +
    '</div><div class=grouparea>' + ShowContainer(C) +
    '</div></div></div>';
end;

function THtmlShow.ShowQueryGridRecords(ARS: TSsRecordSet; Skip: Integer
  ): String;
var
  B: TBookMark;
  TrCls, S, ImgName, TdCls: String;
  i, FmId, Row, RecCounter, RealColCount: Integer;
  IsEditable, Deleting, DataSetEof, CanAppendBn: Boolean;
  RD: TReportData;
  Col: TRpGridColumn;
  QG: TdxQueryGrid;
begin
  S := '';
  RD := ARS.RD;
  QG := ARS.QGrid;
  // !!! Доступ
  FmId := RD.GetEditFormId;
  IsEditable := RD.IsSimple and FSS.UserMan.CheckFmVisible(FSS.RoleId, FmId);
  if IsEditable then
  begin
    if FSS.UserMan.CheckFmEditing(FSS.RoleId, FmId) then ImgName := 'edit.svg'
    else ImgName := 'view.svg';
    Deleting := FSS.UserMan.CheckFmDeleting(FSS.RoleId, FmId);
  end;
  //

  RecCounter := 1;
  with ARS.DataSet do
    if Active then
    begin
      Row := RecNo;
      ARS.DisableScrollEvents;
      B := GetBookmark;
      First;
      MoveBy(Skip);
      while not Eof do
      begin
        TrCls := CalcColoring(ARS, '');
        if Row = RecNo then
          S := S + '<tr class=sel' + IIF(TrCls <> '', ' data-old-class=' + TrCls, '') + '>'
        else
          S := S + '<tr' + IIF(TrCls <> '', ' class=' + TrCls, '') + '>';
        if IsEditable then
        begin
          S := S + '<td>';
          if RD.Grid.ShowRowDeleteButton and Deleting then S := S + '<img class=del src="/img/delrow.svg">';
          S := S + '<img class=edit src="/img/' + ImgName + '"></td>';
        end
        else
          S := S + '<td></td>';
        for i := 0 to RD.Grid.ColumnCount - 1 do
        begin
          Col := RD.Grid.Columns[i]; //TRpGridColumn(L[i]);
          if Col.Visible and (Col.Width > 0) then
          begin
            TdCls := CalcColoring(ARS, Col.FieldNameDS);

            S := S + '<td' + IIF(TdCls <> '', ' class=' + TdCls, '') + '>';

            if not Col.IsImage then
              S := S + StrToHtml( FormatField(FieldByName(Col.FieldNameDS)), True )
            else
              S := S + ShowQueryThumbnail(RD, i, ARS.DataSet);

            S := S + '</td>';
          end;
        end;
        S := S + '</tr>';
        // Подаем запрос порциями по 100 записей. Предотвращаем Next, чтобы не загрузилась очередная порция.
        if RecCounter = 100 then Break;
        Next;
        Inc(RecCounter);
      end;
      DataSetEof := Eof;
      GotoBookmark(B);
      FreeBookmark(B);
      ARS.EnableScrollEvents;
    end
  else
    DataSetEof := True;

  CanAppendBn := not (QG.ShowButtons and (gbnAppend in QG.VisibleButtons)) and
    IsEditable and FSS.UserMan.CheckFmAdding(FSS.RoleId, FmId) and (FRS.Editing = asOk);
  if not DataSetEof or CanAppendBn then
  begin
    RealColCount := RD.Grid.GetVisibleColumnCount;
    if IsEditable then Inc(RealColCount);
    S := S + '<tr class=gridbns><td colspan=' + IntToStr(RealColCount) + '>';
    if CanAppendBn then
      S := S + '<button type=button' +
        ' onclick="queryAdd(' + IntToStr(QG.Id) + ')"' + GetEnabled(QG) + '>' +
        rsAppend + '</button>';
    if not DataSetEof then
    begin
      S := S + '<button type=button' +
        ' onclick="tableFetch(' + IntToStr(QG.Id) + ',true)"' + GetEnabled(QG) + '>' +
        rsMore + '</button>';
    end;
  end;

  Result := S;
end;

function THtmlShow.ShowQueryGrid(C: TdxQueryGrid; GridOnly: Boolean): String;
var
  RD: TReportData;
  i: Integer;
  S: String;
  IsEditable: Boolean;
  SortCol: TRpGridSortData;
  si, FmId: Integer;
  RS: TSsRecordSet;
  Col: TRpGridColumn;
begin
  S := '';
  RS := FRS.Queries.FindRpById(C.Id);
  RD := RS.RD;
  if RD.IsEmpty then Exit(ShowDummyComponent(C));

  // !!! Доступ
  FmId := RD.GetEditFormId;
  IsEditable := RD.IsSimple and FSS.UserMan.CheckFmVisible(FSS.RoleId, FmId);
  //

  if not C.ManualRefresh then RS.Open;

  if not GridOnly then
  begin
    if C.ShowButtons and (gbnAppend in C.VisibleButtons) and IsEditable and
      (FRS.Editing = asOk) and FSS.UserMan.CheckFmAdding(FSS.RoleId, FmId) then
    begin
      S := S + '<div class="gridcmd ' + GetGridButtonsStyle(C) +
        '" style="' + GetBoundsGridButtonsCSS(C) + GetVisible(C) + '"><div style="float: ' +
        IIF(C.AlignmentButtons = taLeftJustify, 'left', 'right') +
        '"><button type=button onclick="queryAdd(' + IntToStr(C.Id) + ')" style="height: ' +
        IntToStr(C.ButtonSize - 2) + 'px"' + GetEnabled(C) + '>' + rsAppend + '</button></div></div>';
    end;
  end;

  try

  if not GridOnly then
  begin
    S := S + '<div id=q' + IntToStr(RD.Id) + GetEnabled(C) + ' class="grid ' +
      GetStyleClass(C) + IIF(IsEditable, '', ' gridro') +
      '" style="' + GetBoundsCSS(C) + GetVisible(C) + '"' + GetEnabled(C) + '>';
    if RD.Grid.ColumnCount = 0 then Exit(S + '</div>');
  end;
  S := S + '<table class=qgrid' + IntToStr(RD.Id) +
    ' onclick="tableClick(event, true)">' +
    '<thead><tr>';
  //if IsEditable then
  begin
    S := S + '<th></th>';
    (*if not IsEditable then
      FCSS.AddObject('.grid table.qgrid' + IntToStr(RD.Id) + ' th:first-child, .grid table.qgrid' +
        IntToStr(RD.Id) + ' td:first-child { display: none; }', TObject(1));*)
    //dx := 2;
  end;
  {else
    dx := 1;}
  for i := 0 to RD.Grid.ColumnCount - 1 do
  begin
    Col := RD.Grid.Columns[i]; //TRpGridColumn(L[i]);
    if not Col.Visible or (Col.Width = 0) then Continue;
    S := S + '<th><span data-fid=' + Col.FieldNameDS + IIF(RD.Grid.AllowChangeSort,
      ' onclick="headerClick()"', '');
    SortCol := RD.Grid.SortCols.FindCol(Col);
    if SortCol <> nil then
    begin
      si := RD.Grid.SortCols.IndexOf(SortCol);
      S := S + ' class=sort data-order=' + IIF(SortCol.Desc, '1', '0') + '>';
      // Порядковый номер (на первом не ставится)
      if si > 0 then
        S := S + IntToStr(si + 1);
      //
      S := S + '<img src="/img/';
      if SortCol.Desc then
        S := S + 'down-sort.svg'
      else
        S := S + 'up-sort.svg';
      S := S + '">';
    end
    else
      S := S + '>';
    S := S + Col.Caption + '</span></th>';
    FCSS.AddObject('.grid table.qgrid' + IntToStr(RD.Id) + ' td:nth-child(' + IntToStr(i + 2) + ') { text-align: ' +
      AlignmentToCSS(GetRpGridColumnAlignment(FSS, RD, Col)) + '; vertical-align: ' +
      TextLayoutToCSS(GetRpGridColumnLayout(Col)) + '; }', TObject(1));
  end;
  S := S + '</tr></thead><tbody>';

  ColoringToCSS(RS, FCSS);

  S := S + ShowQueryGridRecords(RS, 0);

  S := S + '</tbody></table></div>';

   Result := S;

  except
    on E: Exception do
      if not GridOnly then
        Result := '<div style="border: 1px solid; overflow: auto; ' + GetBoundsCSS(C) + '">' +
          E.Message + '</div>'
      else
        Result := '';
  end;
end;

function THtmlShow.ShowGridRecords(ARS: TSsRecordSet; Skip: Integer): String;
var
  S, TrCls, ImgName: String;
  Gr, GridCtrl: TdxGrid;
  i, RecCounter, RealColCount: Integer;
  Col: TdxColumn;
  Fm: TdxForm;
  F, FF: TdxField;
  DataSetEof, Editing, Deleting, CanAppendBn: Boolean;
begin
  S := '';
  Fm := ARS.Form;
  Gr := Fm.Grid;

  // !!! Доступ
  Editing := (ARS.Parent.Editing = asOk) and FSS.UserMan.CheckFmEditing(FSS.RoleId, Fm.Id);
  Deleting := Editing and FSS.UserMan.CheckFmDeleting(FSS.RoleId, Fm.Id);
  if Editing then ImgName := 'edit.svg'
  else ImgName := 'view.svg';
  //

  with ARS.MemDS do
  begin
    RecCounter := 1;
    First;
    MoveBy(Skip);
    while not Eof do
    begin
      TrCls := CalcColoring(ARS, '');
      S := S + '<tr';
      if ARS.DataSet.RecNo = RecNo then
      begin
        S := S + ' class=sel';
        if TrCls <> '' then S := S + ' data-old-class=' + TrCls;
      end
      else if TrCls <> '' then
        S := S + ' class=' + TrCls;
      S := S + '><td>';
      if Gr.ShowRowDeleteButton and Deleting then S := S + '<img class=del src="/img/delrow.svg">';
      S := S + '<img class=edit src="/img/' + ImgName + '">';
      for i := 0 to Gr.Columns.Count - 1 do
      begin
        Col := Gr.Columns[i];
        if not Col.Visible or (Col.Width = 0) then Continue;
        F := Fm.FindField(Col.Id);
        if F is TdxObjectField then
          FF := GetObjectFieldField(FSS, TdxObjectField(F))
        else
          FF := F;
        if F is TdxCalcEdit then
          S := S + '<td>' + FormatFloat(TdxCalcEdit(F).PrecStr,
            FieldByName(FieldStr(F.Id)).AsFloat) + '</td>'
        else if F is TdxDBImage then
          S := S + '<td>' + ShowDBImageThumbnail(ARS.MemDS, TdxDBImage(F)) +
            '</td>'
        else if F is TdxFile then
          S := S + '<td>' + StrToHtml(FieldByName(FieldStr(F.Id) + 'd').AsString) + '</td>'
        else if F is TdxLookupComboBox then
          S := S + '<td>' + StrToHtml(FieldByName(FieldStr(F.Id) + 'l').AsString) + '</td>'
        else if (F is TdxCalcEdit) or (F is TdxTimeEdit) or (FF is TdxCalcEdit) or (FF is TdxTimeEdit) then
          S := S + '<td>' + FormatField(FieldByName(FieldStr(F.Id))) +
            '</td>'
        else if F is TdxCheckBox then
        begin
          if FieldByName(FieldStr(F.Id)).AsInteger = 1 then
            S := S + '<td>' + TdxCheckBox(F).CheckedText + '</td>'
          else
           S := S + '<td>' + TdxCheckBox(F).UnCheckedText + '</td>'
        end
        else if F is TdxMemo then
          S := S + '<td>' + StrToHtml(FieldByName(FieldStr(F.Id)).AsString, True) + '</td>'
        else
          S := S + '<td>' + StrToHtml(FieldByName(FieldStr(F.Id)).AsString) +
            '</td>';
      end;
      S := S + '</tr>';

      if RecCounter = 100 then Break;
      Next;
      Inc(RecCounter);
    end;
    DataSetEof := Eof;
  end;

  GridCtrl := ARS.Parent.Form.FindTable(Fm.Id);
  CanAppendBn := not (GridCtrl.ShowButtons and (gbnAppend in GridCtrl.VisibleButtons)) and
    Editing and FSS.UserMan.CheckFmAdding(FSS.RoleId, Fm.Id);

  if not DataSetEof or CanAppendBn then
  begin
    RealColCount := Gr.GetVisibleColumnCount + 1;
    S := S + '<tr class=gridbns><td colspan=' + IntToStr(RealColCount) + '>';
    if CanAppendBn then
      S := S + '<button type=button' +
        ' onclick="tableAdd(' + IntToStr(Fm.Id) + ')"' + GetEnabled(Gr) + '>' +
        rsAppend + '</button>';
    if not DataSetEof then
    begin
      S := S + '<button type=button' +
        ' onclick="tableFetch(' + IntToStr(Fm.Id) + ',false)"' + GetEnabled(Gr) + '>' +
        rsMore + '</button>';
    end;
  end;

  Result := S;
end;

function THtmlShow.ShowGrid(C: TdxGrid; GridOnly: Boolean): String;
var
  Fm: TdxForm;
  i, si: Integer;
  F, FF: TdxField;
  FlNm: String;
  Gr: TdxGrid;
  SC: TdxSortCol;
  RS: TSsRecordSet;
  Editing: Boolean;
  Col: TdxColumn;
begin
  Result := '';
  RS := FRS.Forms.FindFormById(C.Id);
  if RS = nil then Exit;
  RS.Open;
  Fm := RS.Form;
  Gr := Fm.Grid;

  if GridOnly and not FSS.UserMan.CheckFmVisible(FSS.RoleId, Fm.Id) then Exit;

  // !!! Доступ
  Editing := (RS.Parent.Editing = asOk) and FSS.UserMan.CheckFmEditing(FSS.RoleId, Fm.Id);
  {Deleting := Editing and FSS.UserMan.CheckFmDeleting(FSS.RoleId, Fm.Id);
  if Editing then ImgName := 'edit.svg'
  else ImgName := 'view.svg'; }
  //

  if not GridOnly then
  begin
    if C.ShowButtons and (gbnAppend in C.VisibleButtons) and Editing and
      FSS.UserMan.CheckFmAdding(FSS.RoleId, Fm.Id) then
    begin
      Result := Result + '<div class="gridcmd ' + GetGridButtonsStyle(C) +
        '" style="' + GetBoundsGridButtonsCSS(C) + GetVisible(C) + '"><div style="float: ' +
        IIF(C.AlignmentButtons = taLeftJustify, 'left', 'right') +
        '"><button type=button onclick="tableAdd(' + IntToStr(C.Id) + ')" style="height: ' +
        IntToStr(C.ButtonSize - 2) + 'px"' + GetEnabled(C) + '>' +
        rsAppend + '</button></div></div>';
    end;
  end;

  if not GridOnly then
  begin
    Result := Result + '<div id=t' + IntToStr(C.Id) + ' class="grid ' + GetStyleClass(C) +
      '" style="' + GetBoundsCSS(C) + GetVisible(C) + '"' + GetEnabled(C) + '>';
    // !!! Доступ
    if not FSS.UserMan.CheckFmVisible(FSS.RoleId, Fm.Id) then Exit(Result + '</div>');
    //
  end;

  Result := Result + '<table class=grid' + IntToStr(Fm.Id) + ' onclick="tableClick(event, false)">';
  Result := Result + '<thead><tr><th></th>';

  for i := 0 to Gr.Columns.Count - 1 do
  begin
    Col := Gr.Columns[i];
    if not Col.Visible or (Col.Width = 0) then Continue;
    F := Fm.FindField(Col.Id);
    if Col.Caption <> ' ' then FlNm := StrToHtml(Col.Caption)
    else FlNm := F.FieldName;
    Result := Result + '<th><span';
    SC := Gr.SortCols.FindCol(i);
    if SC <> nil then
    begin
      si := Gr.SortCols.IndexOf(SC);
      Result := Result + ' class=sort>';
      // Порядковый номер (на первом не ставится)
      if si > 0 then
        Result := Result + IntToStr(si + 1);
      //
      Result := Result + '<img src="/img/';
      if SC.Desc then
        Result := Result + 'down-sort.svg'
      else
        Result := Result + 'up-sort.svg';
      Result := Result + '">';
    end
    else
      Result := Result + '>';
    Result := Result + FlNm + '</span></th>';
    FCSS.AddObject('div.grid table.grid' + IntToStr(Fm.Id) + ' td:nth-child(' + IntToStr(i + 2) + ') { text-align: ' +
      AlignmentToCSS(GetGridColumnAlignment(FSS, Fm, Col)) + '; vertical-align: ' +
      TextLayoutToCSS(GetGridColumnLayout(Col)) + '; }', TObject(1));
  end;

  Result := Result + '</tr></thead><tbody>';

  ColoringToCSS(RS, FCSS);

  Result := Result + ShowGridRecords(RS, 0) + '</tbody></table></div>';
end;

function THtmlShow.ShowShape(C: TdxShape): String;
begin
  Result := '<img id=' + C.Name + ' style="' + GetBoundsCSS(C) + GetVisible(C) + '" src="' + C.GetImagePath(True) + '">';
end;

function THtmlShow.ShowLookupComboBox(C: TdxLookupComboBox): String;
var
  RdOnly: Boolean;
  Access: Integer;
begin
  RdOnly := ControlReadOnly(C) or (C.HideList and C.HideButton);

  if not RdOnly then
  begin
    Access := 0;
    if FSS.UserMan.CheckFmVisible(FSS.RoleId, C.SourceTId) then Inc(Access);
    if FSS.UserMan.CheckFmEditing(FSS.RoleId, C.SourceTId) then Inc(Access);
    if FSS.UserMan.CheckFmAdding(FSS.RoleId, C.SourceTId) then Inc(Access);
  end;
  Result := '<input type=hidden name=f' + IntToStr(C.Id) + ' value="' + GetFieldValue(C) +
    '" onchange="fieldChange(this)"><input type=text class="lcbx ' + GetStyleClass(C) +
    '" id=f' + IntToStr(C.Id) + GetEnabled(C) +
    IIF(not RdOnly and (Access > 0), ' data-fm=' + IntToStr(C.SourceTId) +
    ' data-access=' + IntToStr(Access), '') +
    ' style="position: absolute; left: ' + IntToStr(C.Left) +
    'px; top: ' + IntToStr(C.Top) + 'px; width: ' + IntToStr(C.Width) +
    'px; height: ' + IntToStr(C.Height) + 'px;' + GetVisible(C) +
    '" tabindex=' + IntToStr(GetTabOrder(C)) +
    ' readonly value="' + StrToHtml(FRS.GetObjValue(C)) +
    IIF(not RdOnly, '" extra-width=' + IntToStr(C.ListWidthExtra) +
    ' onclick="showList()" onkeydown="lcbxKeyDown()"', '"') + '>';
  if not C.HideList or not C.HideButton then
    Result := Result + '<button class=cbxbn type=button ' + GetEnabled(C) +
      ' style="position: absolute; left: ' +
      IntToStr(C.Left + C.Width) + 'px; top: ' + IntToStr(C.Top) + 'px; width: 18px; ' +
      'height: ' + IntToStr(C.Height) + 'px;' + GetVisible(C) +
      '" tabindex=-1' + IIF(not RdOnly, ' onclick="showList()"', '') + '><i></i></button>'
  // Заглушка
  else
    Result := Result + '<span></span>';
end;

function THtmlShow.ShowComboBox(C: TdxComboBox): String;
var
  sId, S, Attr: String;
  i: Integer;
  FixedList, RdOnly: Boolean;
begin
  FixedList := (C.SourceTId = 0) or (C.SourceFId = 0);
  sId := IntToStr(C.Id);
  RdOnly := ControlReadOnly(C);
  Result := '<input type=text class="' +
    IIF(FixedList and (C.Style = csDropDownList), 'fcbx ', 'cbx ') +
    GetStyleClass(C) + '" name=f' + sId +
    ' id=f' + sId + GetEnabled(C) + ' style="position: absolute; left: ' + IntToStr(C.Left) +
    'px; top: ' + IntToStr(C.Top) + 'px; width: ' + IntToStr(C.Width - 18) +
    'px; height: ' + IntToStr(C.Height) + 'px;' + GetVisible(C) + '" tabindex=' + IntToStr(GetTabOrder(C)) +
    ' value="' + GetFieldValue(C) + '"' + GetMaxLength(C) + GetReadOnly(C) +
    IIF((C.Style = csDropDownList) and FixedList and not RdOnly,
    ' readonly onclick="showFixedList(' + ')"', '') +
    IIF(not RdOnly, ' onkeydown="' +
    IIF(FixedList, 'fixedCbxKeyDown(', 'cbxKeyDown(') + ')"', '') +
    '><button class=cbxbn type=button ' + GetReadOnly(C) + ' style="position: absolute; left: ' +
    IntToStr(C.Left + C.Width - 18) + 'px; top: ' + IntToStr(C.Top) + 'px; width: 18px; ' +
    'height: ' + IntToStr(C.Height) + 'px;' + GetVisible(C) + '" tabindex=-1' +
    GetEnabled(C) + IIF(not RdOnly, ' onclick="' + IIF(FixedList,
    'showFixedList(', 'showList(') + ')"', '') + '><i></i></button>';
  if FixedList and not RdOnly then
  begin
    Result := Result + '<div id=f' + IntToStr(C.Id) + 'l class=listcbx style="position: absolute; left: ' + IntToStr(C.Left) +
      'px; top: ' + IntToStr(C.Top + C.Height) + 'px; width: ' + IntToStr(C.Width) +
      'px; overflow-x: hidden; overflow-y: scroll; border: 1px solid black; ' +
      'background: white; visibility: hidden;"><table class=list><colgroup><col width=100%></colgroup>';
    for i := 0 to C.Items.Count - 1 do
    begin
      S := StrToHtml(C.Items[i]);
      if S <> '' then Attr := ''
      else
      begin
        S := '&nbsp;';
        Attr := ' empty';
      end;
      Result := Result + '<tr><td' + Attr + '>' + S + '</td></tr>';
    end;
    Result := Result + '</table></div>';
  end;
end;

function THtmlShow.ShowCheckBox(C: TdxCheckBox): String;
var
  V: String;
begin
  V := GetFieldValue(C);
  Result := '<span class="checkbox ' + GetStyleClass(C) + '" style="' + GetBoundsCSS(C) +
    GetVisible(C) + '">';
  Result := Result + '<input type=checkbox name=f' + IntToStr(C.Id) +
    ' id=f' + IntToStr(C.Id) + IIF(ControlReadOnly(C) or not C.Enabled,
    ' disabled ', ' onchange="fieldChange(this)"') + ' value="' + V + '"';
  if V = '1' then
    Result := Result + ' checked';
  Result := Result + ' tabindex=' + IntToStr(GetTabOrder(C)) + '>' +
    StrToHtml(C.Caption) + '</span>';
end;

function THtmlShow.ShowMemo(C: TdxMemo): String;
begin
  Result := '<textarea class="' + GetStyleClass(C) + '" name=f' + IntToStr(C.Id) +
    ' id=f' + IntToStr(C.Id) + GetEnabled(C) + ' style="' + GetBoundsCSS(C) +
    GetVisible(C) + '" tabindex=' + IntToStr(GetTabOrder(C)) +
    GetMaxLength(C) + GetReadOnly(C) + '>' +
    GetFieldValue(C) + '</textarea>';
end;

function THtmlShow.ShowDateEdit(C: TdxDateEdit): String;
begin
  Result := '<input class="' + GetStyleClass(C) + '" name=f' + IntToStr(C.Id) +
    ' id=f' + IntToStr(C.Id) + GetEnabled(C) +
    ' type=text value="' + GetFieldValue(C) + '" style="' + GetBoundsCSS(C) +
    GetVisible(C) + '" tabindex=' + IntToStr(GetTabOrder(C)) + GetReadOnly(C) + '>';
  if not C.HideButton then
    Result := Result + '<button class=editbn type=button style="position: absolute; left: ' +
      IntToStr(C.Left + C.Width) + 'px; top: ' + IntToStr(C.Top) + 'px; width: ' +
      IntToStr(C.Height) + 'px; height: ' +
      IntToStr(C.Height) + 'px; padding: 0px;' + GetVisible(C) + '"' +
      GetEnabled(C) + // может быть 2 disabled (!!!)
      IIF(not ControlReadOnly(C), 'onclick="showCalendar(this)">', 'disabled>') +
      '<img src="/img/calendar.svg"></button>';
end;

function THtmlShow.ShowCalcEdit(C: TdxCalcEdit): String;
begin
  Result := '<input class="' + GetStyleClass(C) + '" name=f' + IntToStr(C.Id) +
    ' id=f' + IntToStr(C.Id) + GetEnabled(C) +
    ' type=text value="' + GetFieldValue(C) + '" style="' + GetBoundsCSS(C) +
    GetVisible(C) + '" tabindex=' + IntToStr(GetTabOrder(C)) + GetReadOnly(C) + '>';
end;

function THtmlShow.ShowEdit(C: TdxEdit): String;
begin
  Result := '<input class="' + GetStyleClass(C) + '" name=f' + IntToStr(C.Id) +
    ' id=f' + IntToStr(C.Id) + GetEnabled(C) +
    ' type=text value="' + GetFieldValue(C) + '" style="' + GetBoundsCSS(C) +
    GetVisible(C) + '" tabindex=' + IntToStr(GetTabOrder(C)) + GetReadOnly(C) + '>';
end;

function THtmlShow.ShowForm: String;
const
  Cnt = 20;
var
  Fm: TdxForm;
  i, si, RecCount: Integer;
  F, FF: TdxField;
  Btns, Buf, Pgs, FlNm, ImgName, CSS, TrCls: String;
  SL: TStringListUtf8;
  Gr: TdxGrid;
  SC: TdxSortCol;
  Col: TdxColumn;
  RS: TSsRecordSet;
  Deleting: Boolean;
begin
  Result := '';
  if FSS.FormId = 0 then Exit(ShowNullForm);

  Fm := FSs.FormMan.FindForm(FSs.FormId);
  if Fm = nil then
  begin
    FResultCode := rcPageNotFound;
    Exit(ShowFormNotFound);
  end;

  if Fm.ViewType = vtSimpleForm then
    Exit('<html><head><meta http-equiv=refresh content="1;url=' +
      GetFormHRef(Fm) + '"></head><body></body></html>');

  Gr := Fm.Grid;

  // !!! Доступ
  if not FSS.UserMan.CheckFmVisible(FSS.RoleId, Fm.Id) then
    Exit(ShowAccessDenied);

  if FSS.UserMan.CheckFmAdding(FSS.RoleId, Fm.Id) then
    Btns := '<button id=addbn type=button onclick="formAdd(' + IntToStr(Fm.Id) +
      ')"><img src=/img/add.svg></button>'
  else Btns := '';
  Btns := Btns + '<button id=fltbn type=button onclick="filterClick(this)"><img src="/img/filter.svg"></button>' +
    '<button id=menubn type=button onclick="menuClick(this)"><img src="/img/menu.svg"></button>';
  Btns := Btns + '<span>' + Fm.GetRecordsCaption + '</span>';
  //
  // Фильтр
  Btns := Btns + '<div id=filters class=filters>';
  SL := TStringListUtf8.Create;
  FSs.FormMan.GetSubForms(Fm.Id, SL);
  SL.Sort;
  SL.InsertObject(0, Fm.GetRecordsCaption, Fm);
  for i := 0 to SL.Count - 1 do
    Btns := Btns + '<a href="' + BuildHRef(1) + '&flt=' +
      IntToStr(TdxForm(SL.Objects[i]).Id) + '">' +
      TdxForm(SL.Objects[i]).GetRecordsCaption + '</a>';
  SL.Free;
  {Btns := Btns + '<select id=filters>' + W + '</select>&nbsp;' +
    '<button type=button " onclick="location.href=''' + BuildHRef(2) +
    '&flt=''+document.getElementById(''filters'').options[document.getElementById(''filters'').selectedIndex].value">' +
    rsFilter + '</button>';}
  // Пресеты фильтра
  if Fm.Filters.Count > 0 then
  begin
    Btns := Btns + '<hr>';
    //W := '<option value="">' + rsFilterPresets + '</option>';
    for i := 0 to Fm.Filters.Count - 1 do
      Btns := Btns + '<a href="' + BuildHRef(1) + '&fltpr=' +
        IntToStr(i) + '">' +
        Fm.Filters.Names[i] + '</a>';

    {W := W + '<option value=' + IntToStr(i) + '>' + Fm.Filters.Names[i] +
        '</option>';
    Btns := Btns + '&nbsp;<select onchange="location.href=''' + BuildHRef(2) +
      '&fltpr='' + this.options[this.selectedIndex].value">' + W + '</select>';}
  end;
  Btns := Btns + '<hr><a href="' + BuildHRef(1) + '&fltclr">' + rsClearAllFilters +
    '</a>';
  Btns := Btns + '</div>';
  //
  {if Gr.AllowChangeSort then
    Btns := Btns + '&nbsp;<input type=button value="' + rsSort + '" onclick="location.href=''' +
      BuildHRef(2) + '&sort''">';}

  Buf := '<table onclick="tableClick()"><thead><tr><th></th>';
  {Buf := '<colgroup><col width=48px>';

  for i := 0 to Gr.Columns.Count - 1 do
    with Gr.Columns[i] do
      if Visible and (Width > 0) then
        Buf := Buf + Format('<col width=%dpx>', [Width]);
  Buf := Buf + '</colgroup><thead><tr><th></th>';}

  CSS := '';
  for i := 0 to Gr.Columns.Count - 1 do
  begin
    Col := Gr.Columns[i];
    if not Col.Visible or (Col.Width = 0) then Continue;
    F := Fm.FindField(Col.Id);
    if Col.Caption <> ' ' then FlNm := StrToHtml(Col.Caption)
    else FlNm := F.FieldName;
    Buf := Buf + '<th><span data-fid=' + IntToStr(F.Id) + IIF(Gr.AllowChangeSort,
      ' onclick="headerClick()"', '');
    SC := Gr.SortCols.FindCol(i);
    if SC <> nil then
    begin
      si := Gr.SortCols.IndexOf(SC);
      Buf := Buf + ' class=sort data-order=' + IIF(SC.Desc, '1', '0') + '>';
      // Порядковый номер (на первом не ставится)
      if si > 0 then
        Buf := Buf + IntToStr(si + 1);
      //
      Buf := Buf + '<img src="/img/';
      if SC.Desc then
        Buf := Buf + 'down-sort.svg'
      else
        Buf := Buf + 'up-sort.svg';
      Buf := Buf + '">';
    end
    else
      Buf := Buf + '>';
    Buf := Buf + FlNm + '</span></th>';
    CSS := CSS + 'div.data td:nth-child(' + IntToStr(i + 2) + ') { text-align: ' +
      AlignmentToCSS(GetGridColumnAlignment(FSS, Fm, Col)) + '; vertical-align: ' +
      TextLayoutToCSS(GetGridColumnLayout(Col)) + '; }' + LineEnding;
  end;

  Buf := Buf + '</tr></thead><tbody>';

  RS := TSsRecordSet.Create(FSS, nil);
  RS.Form := Fm;
  RecCount := RS.OpenRecordCount;
  RS.OpenPage((FSS.Page - 1) * Cnt, Cnt);
  if FSS.UserMan.CheckFmEditing(FSS.RoleId, Fm.Id) then
  begin
    //LinkText := rsEdit;
    ImgName := 'edit.svg';
  end
  else
  begin
    //LinkText := rsView;
    ImgName := 'view.svg';
  end;
  Deleting := FSS.UserMan.CheckFmDeleting(FSS.RoleId, Fm.Id);

  ColoringToCSS(RS, CSS);

  with RS.DataSet do
  try
    while not Eof do
    begin
      TrCls := CalcColoring(RS, '');
      Buf := Buf + '<tr' + IIF(TrCls <> '', ' class=' + TrCls, '')
        + '><td>';
      if Gr.ShowRowDeleteButton and Deleting then
        Buf := Buf + '<img src="/img/delrow.svg" class=del>';
      Buf := Buf + '<a href="' + BuildHRef(1) + '&rec=' +
        Fields[0].AsString + '"><img src="/img/' + ImgName + '"></a></td>';
      for i := 0 to Gr.Columns.Count - 1 do
      begin
        Col := Gr.Columns[i];
        if not Col.Visible or (Col.Width = 0) then Continue;
        F := Fm.FindField(Gr.Columns[i].Id);
        if F is TdxObjectField then
          FF := GetObjectFieldField(FSS, TdxObjectField(F))
        else
          FF := F;
        if F is TdxDBImage then
          Buf := Buf + '<td>' + ShowDBImageThumbnail(RS.DataSet, TdxDBImage(F)) + '</td>'
        else if F is TdxFile then
          Buf := Buf + '<td>' + StrToHtml(FieldByName(FieldStr(F.Id) + 'd').AsString) + '</td>'
        else if F is TdxLookupComboBox then
          Buf := Buf + '<td>' + StrToHtml(FieldByName(FieldStr(F.Id) + 'l').AsString) + '</td>'
        else if F is TdxCheckBox then
        begin
          if FieldByName(FieldStr(F.Id)).AsInteger = 1 then
            Buf := Buf + '<td>' + TdxCheckBox(F).CheckedText + '</td>'
          else
           Buf := Buf + '<td>' + TdxCheckBox(F).UnCheckedText + '</td>'
        end
        else if (F is TdxCalcEdit) or (F is TdxTimeEdit) or (FF is TdxCalcEdit) or (FF is TdxTimeEdit) then
          Buf := Buf + '<td>' + FormatField(FieldByName(FieldStr(F.Id))) + '</td>'
        else if F is TdxMemo then
          Buf := Buf + '<td>' + StrToHtml(FieldByName(FieldStr(F.Id)).AsString, True) + '</td>'
        else
        begin
          Buf := Buf + '<td>' + StrToHtml(FieldByName(FieldStr(F.Id)).AsString) +
            '</td>';
        end;
      end;
      Buf := Buf + '</tr>';
      Next;
    end;
  finally
    Buf := Buf + '</tbody></table>';
    Pgs := ShowPages(Fm, RecCount, FSs.Page, Cnt);
    Result := LoadString(GetHtmlPath + 'form.html');
    Result := StringReplace(Result, '[lng]', AppSet.Language, []);
    Result := StringReplace(Result, '[css]', CSS, []);
    Result := StringReplace(Result, '[title]', Fm.GetRecordsCaption, []);
    Result := StringReplace(Result, '[user]', ShowUser, []);
    Result := StringReplace(Result, '[sidebar]', ShowSideBar, []);
    Result := StringReplace(Result, '[tabs]', ShowTabs, []);
    Result := StringReplace(Result, '[buttons]', Btns, []);
    Result := StringReplace(Result, '[lasteditrecords]', ShowLastEditRecords(Fm), []);
    Result := StringReplace(Result, '[pages]', Pgs, []);
    Result := StringReplace(Result, '[content]', Buf, []);
    Result := StringReplace(Result, '[debug]', ShowDebug, []);
    RS.Free;
  end;
end;

{function IsDefaultFont(F: TdxFont): Boolean;
begin
  Result := (F.Name = '') and (F.Color = clDefault) and (F.Size = 0) and (F.Style = []);
end;

function THtmlShow.GetFontCSS(F: TdxFont): String;
begin
  if IsDefaultFont(F) then
  begin
    Result := 'font:inherit;color:inherit;text-decoration:inherit;';
    Exit;
  end;
  Result := 'font-family:';
  if F.Name <> '' then
    Result := Result + '''' + F.Name + '''; '
  else
    Result := Result + 'inherit;';
  Result := Result + 'font-size:';
  if F.Size > 0 then
    Result := Result + IntToStr(F.Size) + 'px;'
  else
    Result := Result + 'inherit;';
  Result := Result + 'color:';
  if F.Color <> clDefault then
    Result := Result + ColorToHtmlColor(F.Color) + ';'
  else
    Result := Result + 'inherit;';
  if F.Style = [] then
    Result := Result + 'font-style:normal;font-weight:normal;text-decoration:none;'
  else
  begin
    if fsItalic in F.Style then
      Result := Result + 'font-style:italic;';
    if fsBold in F.Style then
      Result := Result + 'font-weight:bold;';
    if (fsUnderline in F.Style) or (fsStrikeOut in F.Style) then
    begin
      Result := Result + 'text-decoration:';
      if fsUnderline in F.Style then
        Result := Result + 'underline';
      if fsStrikeOut in F.Style then
        Result := Result + 'line-through';
      Result := Result + ';';
    end;
  end;
end;     }

function THtmlShow.GetFontCSS(F, ParentF: TdxFont; DefaultColor: Boolean
  ): String;
begin
  Result := '';
  {if IsParentFont then
  begin
    Result := 'font:inherit;color:inherit;text-decoration:' +
      IIF(fsUnderline in F.Style, 'underline',
      IIF(fsStrikeOut in F.Style, 'line-through', 'none')) + ';';
    Exit;
  end;  }
  if F.Name <> '' then
    Result := Result + 'font-family:''' + F.Name + ''';';
  if F.Size > 0 then
    Result := Result + 'font-size:' + IntToStr(F.Size) + 'px;';
  if DefaultColor and (F.Color = clDefault) then
  else if (F.Color = clDefault) or ((ParentF <> nil) and (ParentF.Color = F.Color)) then
    Result := Result + 'color:inherit;'
  else
    Result := Result + 'color:' + ColorToHtml(F.Color) + ';';
  {if not DefaultColor and (F.Color <> clDefault) then
  begin
    if (ParentF = nil) or (ParentF.Color <> F.Color) then
      Result := Result + 'color:' + ColorToHtml(F.Color) + ';'
    else
      Result := Result + 'color:inherit;';
  end;  }
  Result := Result +
    'font-style:' + IIF(fsItalic in F.Style, 'italic', 'normal') +
    ';font-weight:' + IIF(fsBold in F.Style, 'bold', 'normal') +
    ';text-decoration:' + IIF(F.Style * [fsUnderline, fsStrikeOut] = [], 'none',
      IIF(fsUnderline in F.Style, 'underline', '') + IIF(fsStrikeOut in F.Style, ' line-through', '')) + ';';
end;

function THtmlShow.GetBoundsCSS(C: TdxControl): String;
begin
  Result := 'position:absolute;left:' + IntToStr(C.Left) + 'px;top:' +
    IntToStr(C.Top) + 'px;';
  // В хроме текст почему-то переносится
  //if C is TdxLabel then
    //Result := Result + 'width: ' + IntToStr(C.Width + 10) + 'px; '
  //else
  if not (C is TdxLabel) then
  begin
    Result := Result + 'width:' + IntToStr(C.Width) + 'px;';
    Result := Result + 'height:' + IntToStr(C.Height) + 'px;';
  end;
  //if C is TdxShape then Result := Result + 'z-index:-1;';
end;

function THtmlShow.GetBoundsGridButtonsCSS(C: TdxCustomGrid): String;
begin
  Result := 'position:absolute;left:' + IntToStr(C.Left) + 'px;top:' +
    IntToStr(C.Top - C.ButtonSize) + 'px;width:' + IntToStr(C.Width) +
    'px; height:' + IntToStr(C.ButtonSize) + 'px;';
end;

function THtmlShow.GetVisible(C: TdxControl): String;
begin
  if C.Visible then Result := ''
  else Result := 'display:none;';
end;

function THtmlShow.GetEnabled(C: TdxControl): String;
begin
  if CanEnabledControl(C) then Result := ''
  else Result := ' disabled ';
end;

function THtmlShow.GetReadOnly(C: TdxControl): String;
begin
  if ControlReadOnly(C) then Result := ' readonly '
  else Result := ' onchange="fieldChange(this)" ';
end;

function THtmlShow.GetMaxLength(C: TdxControl): String;
var
  n: Integer;
begin
  n := GetFieldSize(C);
  if (n > 0) and not ControlReadOnly(C) then
    Result := ' maxlength=' + IntToStr(n)
  else
    Result := '';
end;

function THtmlShow.GetAutoFocus(C: TdxComponent): String;
begin
  {if C = FTopCtrl then Result := ' autofocus'
  else} Result := '';
end;

function THtmlShow.ShowLabel(C: TdxLabel): String;
begin
  Result := '<span id=' + C.Name + ' class="' + GetStyleClass(C) + '" style="' + GetBoundsCSS(C);
  if C.Alignment = taCenter then
    Result := Result + 'text-align:center;'
  else if C.Alignment = taRightJustify then
    Result := Result + 'text-align:right;';
  Result := Result + GetVisible(C);
  if Trim(C.Expression) <> '' then Result := Result + '" fieldname="' + C.FieldName;
  Result := Result + '">' + StrToHtml(C.Caption, True) + '</span>';
end;

{ Если на форме только одно поле ввода, то всегда происходит отправка данных по
ENTER, даже когда нет кнопок submit и даже если ты этого очень не хочешь )).
Такое поведение заложено в стандарте HTML: http://www.w3.org/MarkUp/html-spec/html-spec_8.html#SEC8.2
Странное, конечно, поведение... Короче, чтобы этого избежать, я добавляю фиктивное
невидимое поле ввода. Заодно, буду хранить в нем состояние набора данных.
}
function THtmlShow.ShowDummyInput: String;
var
  S, PS: String;
begin
  if FRS.Form.ViewType <> vtSimpleForm then
  begin
    case FRS.DataSet.State of
      dsInsert: S := 'insert';
      dsEdit: S := 'edit';
      dsBrowse: S := 'view';
      else S := 'unknown';
    end;
    if FRS.Parent <> nil then
    begin
      case FRS.Parent.DataSet.State of
        dsInsert: PS := 'insert';
        dsEdit: PS := 'edit';
        dsBrowse: PS := 'view';
        else PS := 'unknown';
      end;
    end
    else PS := 'none';
  end
  else
  begin
    S := 'simple';
    PS := 'none';
  end;
  FRS.FreshValue := NowTime;
  Result := '<input id=datastate type=text style="position:absolute;width:0px;height:0px;display:none;"' +
    ' data-fresh=' + IntToStr(FRS.FreshValue) +
    ' data-state=' + S + ' data-parent-state=' + PS + ' data-modified=' + Bool2Str(FRS.DataSet.Modified) +
    ' data-confirm-save=' + Bool2Str(FRS.Form.ConfirmSaveRecord) +
    ' data-confirm-cancel=' + Bool2Str(FRS.Form.ConfirmCancelEditing) + '>';
end;

function THtmlShow.ShowDummyComponent(C: TdxControl): String;
begin
  Result := '<div class=dummy style="border:1px solid;' + GetBoundsCSS(C) + '"></div>';
end;

function THtmlShow.ShowControl(C: TdxControl): String;
begin
  if C.Hidden then Exit('');
  // !!! Доступ
  if not FSS.UserMan.CheckControlVisible(FSS.RoleId, C.Form.Id, C.Name) then
  begin
    Result := ShowDummyComponent(C);
    Exit;
  end;
  //
  if C is TdxLabel then Result := ShowLabel(TdxLabel(C))
  else if C is TdxEdit then Result := ShowEdit(TdxEdit(C))
  else if C is TdxCalcEdit then Result := ShowCalcEdit(TdxCalcEdit(C))
  else if C is TdxDateEdit then Result := ShowDateEdit(TdxDateEdit(C))
  else if C is TdxMemo then Result := ShowMemo(TdxMemo(C))
  else if C is TdxCheckBox then Result := ShowCheckBox(TdxCheckBox(C))
  else if C is TdxLookupComboBox then Result := ShowLookupComboBox(TdxLookupComboBox(C))
  else if C is TdxComboBox then Result := ShowComboBox(TdxComboBox(C))
  else if C is TdxShape then Result := ShowShape(TdxShape(C))
  else if C is TdxGrid then Result := ShowGrid(TdxGrid(C), False)
  else if C is TdxQueryGrid then Result := ShowQueryGrid(TdxQueryGrid(C), False)
  else if C is TdxGroupBox then Result := ShowGroup(TdxGroupBox(C))
  else if C is TdxPageControl then Result := ShowPageControl(TdxPageControl(C))
  else if C is TdxImage then Result := ShowImage(TdxImage(C))
  else if C is TdxDBImage then Result := ShowDBImage(TdxDBImage(C))
  else if C is TdxFile then Result := ShowFile(TdxFile(C))
  else if C is TdxObjectField then Result := ShowObjectField(TdxObjectField(C))
  else if C is TdxTimeEdit then Result := ShowTimeEdit(TdxTimeEdit(C))
  else if C is TdxCounter then Result := ShowCounter(TdxCounter(C))
  else if C is TdxButton then Result := ShowButton(TdxButton(C))
  else if C is TdxPivotGrid then Result := ShowPivotGrid(TdxPivotGrid(C))
  else if C is TdxChart then Result := ShowChart(TdxChart(C))
  else if C is TdxRecordId then Result := ShowRecordId(TdxRecordId(C))
end;

function THtmlShow.ShowContainer(Cont: TdxWinControl): String;
var
  i: Integer;
  C: TdxControl;
begin
  Result := '';
  // В десктопе фигуры, картинки и надписи всегда перекрываются компонентами
  for i := 0 to Cont.ControlCount - 1 do
  begin
    C := Cont.Controls[i];
    if (C is TdxShape) or (C is TdxLabel) or (C is TdxImage) or (C is TdxDBImage) then
      Result := Result + ShowControl(C);
  end;

  for i := 0 to Cont.ControlCount - 1 do
  begin
    C := Cont.Controls[i];
    if not (C is TdxShape) and not (C is TdxLabel) and not (C is TdxImage) and
      not (C is TdxDBImage) then
      Result := Result + ShowControl(C);
  end;
end;

function THtmlShow.ShowEditForm: String;
var
  Fm: TdxForm;
  S, Btns, Errs: String;
  i: Integer;
  JsonObj: TJSONObject;
  AR: TActionRunner;
begin
  Result := '';
  Clear;
  Fm := FSs.FormMan.FindForm(FSS.GetFmId);
  if (Fm = nil) or ((Fm.Id = FSS.FormId) and (Fm.PId > 0)) then
  begin
    FResultCode := rcPageNotFound;
    Exit(ShowFormNotFound);
  end;

  // !!! Доступ
  if not FSS.UserMan.CheckFmVisible(FSS.RoleId, Fm.Id) then Exit(ShowAccessDenied);

  // Если это простая форма, то rec должен быть всегда равен 1.
  if (Fm.ViewType = vtSimpleForm) and (FSS.RecId <> 1) then
    Exit(ShowRedirect(GetFormHRef(Fm)));

  try try

  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then
  begin
    if Fm.PId = 0 then
    begin
      //if Fm.ViewType = vtSimpleForm then Exit(AppendRecord);

      FRS := FSS.AddRecordSet(Fm);
      if Fm.ViewType <> vtSimpleForm then
      begin
        FRS.OpenRecord(FSS.RecId);
        if FRS.DataSet.RecordCount = 0 then
        begin
          FSS.RecordSets.DeleteRecordSet(FRS);
          FResultCode := rcPageNotFound;
          Exit(ShowErrorPage(rsRecordNotFound, rsRecordNotFoundMsg));
        end;
      end
      else
      begin
        FRS.OpenRecord(0);
        FRS.Append;
      end;
      FRS.OpenDetails;
      if FRS.Editing = asOk then FRS.Edit;
    end
    else
    begin
      FSS.TableId := 0;
      FSS.TableRecId := 0;
      Result := '<html><head><meta http-equiv=refresh content="1;url=' +
        BuildHRef(2) + '"></head><body></body></html>';
      Exit;
    end;
  end
  else if (FRS.Form.PId > 0) and (FSS.TableRecId > 0) then
  begin
    // Перенаправляем на редактируемую запись таблицы
    if FRS.DataSet.State in [dsInsert, dsEdit] then
    begin
      if FRS.DataSet['id'] <> FSS.TableRecId then
      begin
        FSS.TableRecId := FRS.DataSet['id'];
        FSS.Msg := rsEditRecNotFinished;
        Exit(ShowRedirect(BuildHRef(4)));
      end;
    end
    else if not FRS.DataSet.Locate('id', FSS.TableRecId, []) then
    begin
      FSS.TableId := 0;
      FSS.TableRecId := 0;
      FSS.Msg := rsSubFormRecNotFound;
      Exit(ShowRedirect(BuildHRef(2)));
      Exit;
    end;
    if FRS.Editing = asOk then FRS.Edit;
    FRS.RequeryAllQueries;
  end
  else
  begin
    if FRS.Editing = asOk then FRS.Edit;
    FRS.RequeryAllQueries;
  end;
  Fm := FRS.Form;
  FRS.CalcAllLabels;

  // продолжаем выполнение действий
  if (FRS.ActionList.Count > 0) and not FRS.MsgInfo.Visible then
    for i := FRS.ActionList.Count - 1 downto 0 do
    begin

      AR := TActionRunner(FRS.ActionList[i]);
      //AR := TActionRunner(FRS.Actions);
      //if (AR <> nil) and not FRS.MsgInfo.Visible then
      begin
        FRS.GotoUrl := '';
        try try
          AR.Run;
          if FRS.GotoUrl <> '' then
            Exit(ShowRedirect(FRS.GotoUrl));
        except
          on E: EPSException do
          begin
            FResultCode := rcAnyError;
            Exit(ShowErrorPage(rsError, EPSExceptionToString(E)));
          end;
          on E: Exception do
          begin
            FResultCode := rcAnyError;
            Exit(ShowErrorPage(rsError, E.Message));
          end;
        end;
        finally
          if not AR.NeedContinue then
          begin
            AR.Free;
            FRS.ActionList.Delete(i);
            //FRS.Actions := nil;
          end;
        end;
      end;

    end;

  if FRS.Form.OnShowForm <> nil then
  begin
    FRS.GotoUrl := '';
    FRS.Form.OnShowForm(FRS.Form);
    if FRS.GotoUrl <> '' then
      Exit(ShowRedirect(FRS.GotoUrl));
  end;

  // Состояние набора данных может быть изменено в OnShowForm
  if (FRS.Editing = asOk) and not (FRS.DataSet.State in [dsInsert, dsEdit]) then
  begin
    FRS.Editing := asCantEdit;
    FRS.Deleting := asCantDelete;
  end;

  FRS.OldState := FRS.DataSet.State;
  FRS.OldRecId := FRS.RecId;

  FTabOrderList := TList.Create;
  Fm.GetTabOrderList(FTabOrderList);

  Btns := '';
  if Fm.ViewType <> vtSimpleForm then
  begin
    if FRS.Editing = asOk  then
      Btns := '<button type=button id=okbn onclick="okClick()"><img src="/img/ok.svg"></button>';
    Btns := Btns + '<button id=cancelbn type=button onclick="cancelClick()"><img src="/img/' +
      IIF(FRS.Editing = asOk, 'cancel.svg', 'back.svg') + '"></button>';
    if FRS.Deleting = asOk then
    begin
      Btns := Btns + '<button type=button id=delbn onclick="deleteClick()"><img src="/img/delete.svg"></button>';
    end;
  end;
  if Fm.Templates.Count > 0 then
    Btns := Btns + '<button type=button id=printbn onclick="printClick()"><img src="/img/print.svg"></button>';
  if Fm.ViewType = vtSimpleForm then
    Btns := Btns + '<button type=button id=menubn onclick="menuClick(this)"><img src="/img/menu.svg"></button>';
  if (FRS.Editing = asOk) and FSS.UserMan.CheckFmAdding(FSS.RoleId, Fm.Id) and (Fm.ViewType <> vtSimpleForm) then
    Btns := Btns + '<button type=button id=dupbn ' + IIF(FRS.Forms.Count > 0, 'data-haschilds ', '') +
      'onclick="duplicateClick()"><img src="/img/copy.svg"></button>';
  Btns := Btns + '<span>';
  if Fm.ViewType <> vtSimpleForm then
    Btns := Btns + '<img src="/img/' + IIF(FRS.DataSet.State = dsInsert,
      'newrec', IIF(FRS.DataSet.State = dsEdit, 'editrec', 'viewrec')) +
      '.svg">';
  Btns := Btns + Fm.GetRecordCaption + '</span>';
  if Fm.Templates.Count > 0 then
  begin
    Btns := Btns + '<div id=templates style="display: none;">';
    for i := 0 to Fm.Templates.Count - 1 do
      Btns := Btns + '<a data-index=' + IntToStr(i) + ' onclick="templateClick(this)">' +
        ChangeFileExt(Fm.Templates[i], '') + '</a>';
    Btns := Btns + '</div>';
  end;
  Errs := '';
  for i := 0 to Fm.Errs.Count - 1 do
    Errs := Errs + le2br(Fm.Errs[i]) + '<br>';
  Errs := Errs + FSs.Msg;
  Errs := '<div id=errs class=errs' + IIF(Errs = '', ' style="display:none;"', '') + '>' +
    Errs + '</div>';
  {if Fm.ViewType <> vtSimpleForm then
  begin
    if FSs.TableId = 0 then
      Act:= BuildHRef(3)
    else
      Act := BuildHRef(5);
    Act := Act + '&post'
  end
  else Act := ''; }
  S := '<div style="position: relative;" class="' + GetStyleClass(Fm) + '" style="width: ' +
    IntToStr(Fm.Width) + 'px; height: ' + IntToStr(Fm.Height) + 'px; ">';
  {if FSs.TableId = 0 then
    FlChange := BuildHRef(3)
  else
    FlChange := BuildHRef(5);}

  FTopCtrl := GetTopControl(Fm);
  S := S + ShowDummyInput + ShowContainer(Fm);
  S := S + '</div>';
  if FRS.MsgInfo.Visible then
  begin
    JsonObj := FRS.MsgInfoToJson;
    S := S + '<script id="$msginfo" type="application/json">' + JsonObj.AsJson +
      '</script>';
    JsonObj.Free;
  end;
  Result := LoadString(GetHtmlPath + 'editform.html');
  Result := StringReplace(Result, '[lng]', AppSet.Language, []);
  Result := StringReplace(Result, '[css]', CreateCSS, []);
  Result := StringReplace(Result, '[javascript]', GetJsCode, []);
  {Result := StringReplace(Result, '[fieldchange]', FlChange, []);
  Result := StringReplace(Result, '[queryscroll]', FlChange, []);
  Result := StringReplace(Result, '[getlist]', FlChange, []);
  Result := StringReplace(Result, '[getlist]', FlChange, []);}
  Result := StringReplace(Result, '[title]', FRS.Form.GetRecordCaption, []);
  Result := StringReplace(Result, '[user]', ShowUser, []);
  Result := StringReplace(Result, '[sidebar]', ShowSideBar, []);
  Result := StringReplace(Result, '[tabs]', ShowTabs, []);
  //Result := StringReplace(Result, '[action]', Act, []);
  Result := StringReplace(Result, '[buttons]', Btns, []);
  Result := StringReplace(Result, '[errors]', Errs, []);
  Result := StringReplace(Result, '[content]', S, []);
  Result := StringReplace(Result, '[debug]', ShowDebug, []);
  FSs.Msg := '';

  except
    on E: EPSException do
    begin
      FResultCode := rcAnyError;
      Result := ShowErrorPage(rsError, EPSExceptionToString(E));
    end;
    on E: Exception do
    begin
      FResultCode := rcAnyError;
      Result := ShowErrorPage(rsError, E.Message);
    end;
  end;
  finally
    //FreeAndNil(FDS);
    FreeAndNil(FTabOrderList);
    Fm.Errs.Clear;
    //FSs.Errs.Clear;
  end;
end;

function THtmlShow.TableEdit(AFields: TStrings): String;
var
  TId, Row, FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AFields.Values['id'], TId) then
    Exit(MakeJsonErrString(rcAnyError, 'Invalid table id'));
  if not TryStrToInt(AFields.Values['row'], Row) then
    Exit(MakeJsonErrString(rcAnyError, 'Invalid table row'));
  if not TryStrToInt(AFields.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));

  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, TId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.Parent.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));
  try
    if FRS.DataSet.State in [dsInsert, dsEdit] then
    begin
      if FRS.DataSet.RecNo <> Row then
        FSS.Msg := rsEditRecNotFinished;
    end
    else
    begin
      if Row <> FRS.DataSet.RecNo then
        FRS.DataSet.MoveBy(Row - FRS.DataSet.RecNo);
    end;
    Result := GetHRef(FSS.FormId, FSS.RecId, TId, FRS.RecId);
    FResultCode := rcAjaxOk;
  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
    end;
  end;
end;

function THtmlShow.TableDeleteRow(AFields: TStrings): String;
var
  TId, Row, FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AFields.Values['id'], TId) then
    Exit(MakeJsonErrString(rcAnyError, 'Invalid table id'));
  if not TryStrToInt(AFields.Values['row'], Row) then
    Exit(MakeJsonErrString(rcAnyError, 'Invalid table row'));
  if not TryStrToInt(AFields.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));

  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, TId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.Parent.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));

  if FRS.DataSet.State in [dsInsert, dsEdit] then
  begin
    if FRS.DataSet.RecNo <> Row then
      Exit(MakeJsonErrString(rcAnyError, rsAnotherRecordEditing));
  end
  else
    FRS.DataSet.MoveBy(Row - FRS.DataSet.RecNo);

  FSS.TableId := TId;
  FSS.TableRecId := FRS.RecId;

  FRS.Parent.ClearChanges;
  Result := DeleteRecord;

  if FResultCode = rcAjaxOk then
  begin
    FRS := FRS.Parent;
    Result := GetEvalChangesAsJson([efDelRow]);
  end;
end;

function THtmlShow.TableScroll(AFields: TStrings): String;
var
  TId, Row, FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AFields.Values['id'], TId) then
    Exit(MakeJsonErrString(rcAnyError, 'Invalid table id'));
  if not TryStrToInt(AFields.Values['row'], Row) then
    Exit(MakeJsonErrString(rcAnyError, 'Invalid table row'));
  if not TryStrToInt(AFields.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));

  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, TId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.Parent.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));

  try
    FRS.Parent.ClearChanges;
    if not (FRS.DataSet.State in [dsInsert, dsEdit]) then
      FRS.DataSet.MoveBy(Row - FRS.DataSet.RecNo);
    FRS := FRS.Parent;
    Result := GetEvalChangesAsJson;
    FResultCode := rcAjaxOk;
  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
    end;
  end;
end;

function THtmlShow.TableFetch(AFields: TStrings): String;
var
  SkipRows, TId, FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AFields.Values['id'], TId) then
   Exit(MakeJsonErrString(rcAnyError, 'Invalid table id'));
  if not TryStrToInt(AFields.Values['skip'], SkipRows) then
   Exit(MakeJsonErrString(rcAnyError, 'Invalid skip value'));
  if not TryStrToInt(AFields.Values['fresh'], FreshValue) then
   Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));

  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, TId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.Parent.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));

  try
    Result := ShowGridRecords(FRS, SkipRows);
    FResultCode := rcAjaxOk;
  except
    on E: Exception do
      Result := MakeJsonErrString(rcAnyError, E.Message);
  end;
end;

function THtmlShow.PostEditForm(AParams: TStrings; DupParam: TDuplicateParam;
  KeepRecordSet: Boolean): String;
var
  IsModified: Boolean;
  FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AParams.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));
  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg))
  else if FRS.Form.PId > 0 then
  begin
    if FRS.RecId <> FSS.TableRecId then
      Exit(MakeJsonErrString(rcRecordNotActive, rsRecordNotActive));
  end;

  //FSS.Errs.Clear;
  FRS.ClearChanges;

  try

  if FRS.Validate then
  begin
    IsModified := FRS.DataSet.Modified;
    FRS.Post;

    if FRS.CallerRS <> nil then
      try
        if FRS.CallerRS.Form <> nil then
          FRS.CallerRS.SetDSField(FRS.CallerObj, FRS.RecId)
        else
          FRS.CallerRS.QryRecId := FRS.RecId;
      except
        ;
      end;

    if DupParam <> dpNone then
    begin
      CopyRecordForDuplicate(DupParam);
    end;
    if IsModified then FRS.AddToHistory;
    if not KeepRecordSet and (FRS.Form.PId = 0) then
      FSS.RecordSets.DeleteRecordSet(FRS);

    FResultCode := rcAjaxOk;
  end
  else
  begin
    Result := GetEvalChangesAsJson;
    //FSS.Errs.Clear;
    FResultCode := rcValidateError;
  end;

  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
    end;
  end;
end;

function THtmlShow.FormDeleteRow: String;
var
  Fm: TdxForm;
begin
  Result := '';
  FResultCode := rcAjaxError;

  if not FSS.UserMan.CheckFmDeleting(FSS.RoleId, FSS.FormId) then
    Exit(MakeJsonErrString(rcAnyError, rsAccessDenied));

  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, 0);

  if FRS = nil then
  begin
    Fm := FSS.FormMan.FindForm(FSS.FormId);
    FRS := FSS.AddRecordSet(Fm);
    FRS.OpenRecord(FSS.RecId);
    if FRS.DataSet.RecordCount = 0 then
    begin
      FSS.RecordSets.DeleteRecordSet(FRS);
      FRS := nil;
      Exit(MakeJsonErrString(rcAnyError, rsRecordNotFound));
    end;
  end;

  Result := DeleteRecord;
end;

function THtmlShow.FormAppend(AFields: TStrings): String;
var
  Fm: TdxForm;
  FmId: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AFields.Values['id'], FmId) then Exit(MakeJsonErrString(rcAnyError, 'Invalid form id.'));

  Fm := FSs.FormMan.FindForm(FmId);
  if Fm = nil then Exit(MakeJsonErrString(rcAnyError, 'Form not found.'));

  // !!! Доступ
  if not FSS.UserMan.CheckFmAdding(FSS.RoleId, Fm.Id) then Exit(MakeJsonErrString(rcAnyError, 'Access denied.'));
  //

  try

  FRS := FSS.AddRecordSet(Fm);
  FRS.OpenRecord(0);
  FRS.Append;
  //FRS.OpenDetails;
  Result := GetHRef(FmId, FRS.RecId);
  FResultCode := rcAjaxOk;

  except
    on E: EPSException do
    begin
      FResultCode := rcAnyError;
      Result := ShowErrorPage(rsError, EPSExceptionToString(E));
    end;
    on E: Exception do
    begin
      FResultCode := rcAnyError;
      Result := ShowErrorPage(rsError, E.Message);
    end;
  end;
end;

function THtmlShow.TableAppend(AFields: TStrings): String;
var
  TId, FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AFields.Values['id'], TId) then
    Exit(MakeJsonErrString(rcAnyError, 'Invalid table id'));
  if not TryStrToInt(AFields.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));

  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, TId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.Parent.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg));

  try
    if FRS.DataSet.State in [dsInsert, dsEdit] then
    begin
      FSS.Msg := rsEditRecNotFinished;
    end
    else
    begin
      FRS.Append;
    end;
    {FSS.TableId := TId;
    FSS.TableRecId := FRS.RecId;
    FRS.GotoUrl := BuildHRef(5);
    Result := GetEvalChangesAsJson(FRS);}
    Result := GetHRef(FSS.FormId, FSS.RecId, TId, FRS.RecId);
    FResultCode := rcAjaxOk;
  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
    end;
  end;
end;

{function THtmlShow.ShowImageUploadForm: String;
var
  S, Act: String;
begin
  if FSs.TableId = 0 then
    Act := BuildHRef(3) + '&imgup=' + IntToStr(FSs.CId)
  else
    Act := BuildHRef(5) + '&imgup=' + IntToStr(FSs.CId);
  S := '<input type=file name=image accept="image/*">' +
    '<input type=submit value="' + rsUpload + '">';
  Result := LoadString(HtmlPath + 'imgupform.html');
  Result := StringReplace(Result, '[title]', rsUploadImage, []);
  Result := StringReplace(Result, '[action]', Act, []);
  Result := StringReplace(Result, '[content]', S, []);
end;   }

function THtmlShow.UploadImage(AParams: TStrings; const FileName: String;
  St: TStream): String;
var
  TmpFlName: String;
  Fm: TdxForm;
  Img: TdxField;
  FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AParams.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));
  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg))
  else if (FRS.Form.PId > 0) and (FRS.RecId <> FSS.TableRecId) then
    Exit(MakeJsonErrString(rcRecordNotActive, rsRecordNotActive))
  else if not (FRS.DataSet.State in [dsInsert, dsEdit]) then
    Exit(MakeJsonErrString(rcRecordReadOnly, rsCanNotChangeRec));
  Fm := FRS.Form;
  Img := Fm.FindField(FSs.CId);
  if Img = nil then Exit(MakeJsonErrString(rcAnyError, 'Img=nil'));
  if TdxDBImage(Img).StorageType = StorageTypeLink then
    Exit(MakeJsonErrString(rcAnyError, 'StorageTypeLink not support'));

  try
    TmpFlName := GetCachePath(FSS) + FileName;
    with TFileStream.Create(Utf8ToSys(TmpFlName), fmCreate) do
    try
      CopyFrom(St, St.Size);
    finally
      Free;
    end;
    FRS.ChangeField(Img, TmpFlName);
    DeleteFile(TmpFlName);
    Result := GetEvalChangesAsJson;
    FResultCode := rcAjaxOk;
  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
    end;
  end;
end;

{function THtmlShow.ShowFileUploadForm: String;
var
  Act, S: String;
begin
  if FSs.TableId = 0 then
    Act := BuildHRef(3) + '&flup=' + IntToStr(FSs.CId)
  else
    Act := BuildHRef(5) + '&flup=' + IntToStr(FSs.CId);
  S := '<input type=file name=file>' +
    '<input type=submit value="' + rsUpload + '">';
  Result := LoadString(HtmlPath + 'flupform.html');
  Result := StringReplace(Result, '[title]', rsUploadFile, [rfReplaceAll]);
  Result := StringReplace(Result, '[action]', Act, []);
  Result := StringReplace(Result, '[content]', S, []);
end;           }

function THtmlShow.UploadFile(AParams: TStrings; const FileName: String;
  St: TStream): String;
var
  Fm: TdxForm;
  Fl: TdxField;
  TmpFlName: String;
  FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AParams.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));
  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS. TableId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg))
  else if (FRS.Form.PId > 0) and (FRS.RecId <> FSS.TableRecId) then
    Exit(MakeJsonErrString(rcRecordNotActive, rsRecordNotActive))
  else if not (FRS.DataSet.State in [dsInsert, dsEdit]) then
    Exit(MakeJsonErrString(rcRecordReadOnly, rsCanNotChangeRec));
  Fm := FRS.Form;
  Fl := Fm.FindField(FSs.CId);
  if Fl = nil then Exit(MakeJsonErrString(rcAnyError, 'Fl=nil'));
  if TdxFile(Fl).StorageType = StorageTypeLink then
    Exit(MakeJsonErrString(rcAnyError, 'StorageTypeLink not support'));
  TmpFlName := GetCachePath(FSS) + FileName;
  try
    with TFileStream.Create(Utf8ToSys(TmpFlName), fmCreate) do
    try
      CopyFrom(St, St.Size);
    finally
      Free;
    end;
    FRS.ChangeField(Fl, TmpFlName);
    DeleteFile(TmpFlName);
    Result := GetEvalChangesAsJson;
    FResultCode := rcAjaxOk;
  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
    end;
  end;
end;

function THtmlShow.DownloadFile(AParams: TStrings): String;
var
  Fm: TdxForm;
  Fl: TdxField;
  FileName, ErrStr: String;
  JsonObj: TJSONObject;
  FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AParams.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));
  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS. TableId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg))
  else if (FRS.Form.PId > 0) and (FRS.RecId <> FSS.TableRecId) then
    Exit(MakeJsonErrString(rcRecordNotActive, rsRecordNotActive));
  Fm := FRS.Form;
  Fl := Fm.FindField(FSs.CId);
  if Fl = nil then Exit(MakeJsonErrString(rcAnyError, 'Fl=nil'));
  FileName := ExtractFileName(GetFileFileName(TdxFile(Fl), FRS.DataSet));
  ErrStr := SaveFileToFile(GetCachePath(FSS) + FileName, TdxFile(Fl), FRS.DataSet);
  JsonObj := TJsonObject.Create;
  if ErrStr <> '' then
    JsonObj.Add('error', ErrStr)
  else if FileExists(GetCachePath(FSS) + FileName) then
    JsonObj.Add('file', GetCachePath(FSS, True) + FileName);
  Result := JsonObj.AsJSON;
  JsonObj.Free;
  FResultCode := rcAjaxOk;
end;

function THtmlShow.ViewImage: String;
var
  Fm: TdxForm;
  Img: TdxField;
  ImgName, Ext: String;
  F: TField;
  NeedConvert: Boolean;
begin
  Result := '';
  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then Exit(ShowErrorPage(rsError, rsNoLinkForm))
  else if (FRS.Form.PId > 0) and (FRS.RecId <> FSS.TableRecId) then
    Exit(ShowErrorPage(rsError, rsRecordNotActive));

  Fm := FRS.Form;
  Img := TdxDBImage(Fm.FindField(FSs.CId));
  if (Img = nil) or not (Img is TdxDBImage) then
    Exit(ShowErrorPage(rsError, Format(rsFieldNotFound, [FieldStr(FSS.CId)])));
  F := FRS.DataSet.FieldByName(FieldStr(Img.Id) + 'src');
  Ext := LowerCase(ExtractFileExt(F.AsString));
  NeedConvert := (Ext = '.tif') or (Ext = '.tiff');
  if NeedConvert then Ext := '.png';

  try
    ImgName := IntToStr(Fm.Id) + '-' + IntToStr(FRS.RecId) + '-' +
      IntToStr(FSS.CId) + 'f' + Ext;
    if NeedConvert then
    begin
      SaveImageToFileConvert(GetCachePath(FSS) + ImgName, TdxDBImage(Img), FRS.DataSet);
      ImgName := GetCachePath(FSS, True) + ImgName;
    end
    else if SaveImageToFile(GetCachePath(FSS) + ImgName, TdxDBImage(Img), FRS.DataSet) then
      ImgName := GetCachePath(FSS, True) + ImgName
    else
      ImgName := '';
  except
    on E: Exception do
      Exit(ShowErrorPage(rsError, E.Message));
  end;
  Result := LoadString(GetHtmlPath + 'imgview.html');
  Result := StringReplace(Result, '[content]',
    '<img src="' + ImgName + '">', []);
end;

function THtmlShow.ClearImage(AParams: TStrings): String;
var
  Fm: TdxForm;
  Img: TdxField;
  FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AParams.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));
  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg))
  else if (FRS.Form.PId > 0) and (FRS.RecId <> FSS.TableRecId) then
    Exit(MakeJsonErrString(rcRecordNotActive, rsRecordNotActive))
  else if not (FRS.DataSet.State in [dsInsert, dsEdit]) then
    Exit(MakeJsonErrString(rcRecordReadOnly, rsCanNotChangeRec));
  Fm := FRS.Form;
  Img := Fm.FindField(FSs.CId);
  if Img = nil then Exit(MakeJsonErrString(rcAnyError, 'Img=nil'));
  try
    FRS.ChangeField(Img, Null);
    Result := GetEvalChangesAsJson;
    FResultCode := rcAjaxOk;
  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
    end;
  end;
end;

function THtmlShow.ClearFile(AParams: TStrings): String;
var
  Fm: TdxForm;
  Fl: TdxField;
  FreshValue: Longint;
begin
  Result := '';
  FResultCode := rcAjaxError;
  if not TryStrToInt(AParams.Values['fresh'], FreshValue) then
    Exit(MakeJsonErrString(rcAnyError, rsInvalidFreshValue));
  FRS := FSS.FindRecordSet(FSS.FormId, FSS.RecId, FSS.TableId);
  if FRS = nil then
    Exit(MakeJsonErrString(rcRecordSetNotFound, rsNoLinkForm))
  else if FRS.FreshValue <> FreshValue then
    Exit(MakeJsonErrString(rcInvalidFreshValue, rsInvalidFreshValueMsg))
  else if (FRS.Form.PId > 0) and (FRS.RecId <> FSS.TableRecId) then
    Exit(MakeJsonErrString(rcRecordNotActive, rsRecordNotActive))
  else if not (FRS.DataSet.State in [dsInsert, dsEdit]) then
    Exit(MakeJsonErrString(rcRecordReadOnly, rsCanNotChangeRec));
  Fm := FRS.Form;
  Fl := Fm.FindField(FSs.CId);
  if Fl = nil then Exit(MakeJsonErrString(rcAnyError, 'Fm=nil'));
  try
    FRS.ChangeField(Fl, Null);
    Result := GetEvalChangesAsJson;
    FResultCode := rcAjaxOk;
  except
    on E: EPSException do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, EPSExceptionToString(E));
    end;
    on E: Exception do
    begin
      FResultCode := rcAjaxError;
      Result := MakeJsonErrString(rcAnyError, E.Message);
    end;
  end;
end;

end.

