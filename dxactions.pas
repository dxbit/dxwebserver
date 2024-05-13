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

unit DxActions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strconsts, sqldb, dxtypes, mytypes;

type
  EActionBreak = class(Exception);

  TdxActionType = (actNone, actGotoForm, actPrint, actMassCalc, actOpenReport,
    actSaveChanges, actUserMonitor, actCallFunc, actClearFields, actCustom, actShowMessage);

  //T2DVariant = array of array of Variant;

  { TActionProp }

  TActionProp = class
  public
    Name: String;
    Value: String;
    //Values: T2DVariant;
  end;

  { TActionProps }

  TActionProps = class(TList)
  private
    function GetProps(Index: Integer): TActionProp;
  public
    function AddProp: TActionProp;
    function Find(const AName: String): TActionProp;
    procedure Clear; override;
    property Props[Index: Integer]: TActionProp read GetProps; default;
  end;

  TBaseActionClass = class of TBaseAction;

  { TBaseAction }

  TBaseAction = class
  private
    //FActionResult: String;
    FActionType: TdxActionType;
    FDisabled: Boolean;
    FDSProc: TObject;
    FDSRi: Integer;
    FRS: TSsRecordSet;
    //FSS: TSession;
  protected
    function GetActionName: String; virtual;
    function InnerExecute: Variant; virtual;
  public
    constructor Create; virtual;
    function Execute: Variant;
    property ActionName: String read GetActionName;
    property ActionType: TdxActionType read FActionType;
    property DSProc: TObject read FDSProc write FDSProc;
    property DSRi: Integer read FDSRi write FDSRi;
    property Disabled: Boolean read FDisabled write FDisabled;
    property RS: TSsRecordSet read FRS write FRS;
    //property Session: TSession read FSS write FSS;
    //property ActionResult: String read FActionResult;
  end;

  { TGotoFormAction }

  TGotoFormAction = class(TBaseAction)
  private
    FFormName: String;
  protected
    function InnerExecute: Variant; override;
  public
    property FormName: String read FFormName write FFormName;
  end;

  { TPrintAction }

  TPrintFileAction = (pfaNone, pfaOpen, pfaPrint);

  TPrintAction = class(TBaseAction)
  private
    FExpression: String;
    FFileAction: TPrintFileAction;
    FOutFile: String;
    FSaveRecord: Boolean;
    FTemplateFile: String;
  protected
    function InnerExecute: Variant; override;
  public
    constructor Create; override;
    property TemplateFile: String read FTemplateFile write FTemplateFile;
    property Expression: String read FExpression write FExpression;
    property OutFile: String read FOutFile write FOutFile;
    property SaveRecord: Boolean read FSaveRecord write FSaveRecord;
    property FileAction: TPrintFileAction read FFileAction write FFileAction;
  end;

  { TMassCalcAction }

  TMassCalcAction = class(TBaseAction)
  private
    FExpression: String;
    FFieldName: String;
    FFilter: String;
    FFormName: String;
    FTableName: String;
  protected
    function InnerExecute: Variant; override;
  public
    property FormName: String read FFormName write FFormName;
    property Filter: String read FFilter write FFilter;
    property TableName: String read FTableName write FTableName;
    property FieldName: String read FFieldName write FFieldName;
    property Expression: String read FExpression write FExpression;
  end;

  { TOpenReportAction }

  TOpenReportAction = class(TBaseAction)
  private
    FRpName: String;
  protected
    function InnerExecute: Variant; override;
  public
    property RpName: String read FRpName write FRpName;
  end;

  { TSaveChangesAction }

  TSaveChangesAction = class(TBaseAction)
  protected
  	function InnerExecute: Variant; override;
  end;

  { TUserMonitorAction }

  TUserMonitorAction = class(TBaseAction)
  protected
    function InnerExecute: Variant; override;
  end;

  { TCallFuncAction }

  TCallFuncAction = class(TBaseAction)
  private
    FExpr: String;
  protected
    function InnerExecute: Variant; override;
  public
    property Expression: String read FExpr write FExpr;
  end;

  { TClearFieldsAction }

  TClearFieldsAction = class(TBaseAction)
  private
    FFields: TStringList;
  protected
    function InnerExecute: Variant; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  	property Fields: TStringList read FFields;
  end;

  { TShowMessageAction }

  TShowMessageAction = class(TBaseAction)
  private
    FButtons: TMsgDlgButtons;
    FExprMsg: String;
    FMessage: String;
    FTitle: String;
    FMsgType: TMsgDlgType;
  protected
    function InnerExecute: Variant; override;
  public
    constructor Create; override;
  	property Title: String read FTitle write FTitle;
    property Message: String read FMessage write FMessage;
    property ExprMsg: String read FExprMsg write FExprMsg;
    property MsgType: TMsgDlgType read FMsgType write FMsgType;
    property Buttons: TMsgDlgButtons read FButtons write FButtons;
  end;

  { TActionCustom }

  TActionCustom = class(TBaseAction)
  private
    FActionId: String;
  	FProps: TActionProps;
  protected
    function GetActionName: String; override;
    function InnerExecute: Variant; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Props: TActionProps read FProps;
    property ActionId: String read FActionId write FActionId;
  end;

  TActionLineKind = (alkNone, alkAction, alkIf, alkElseIf, alkElse, alkComment);

  TActionLines = class;

  { TActionLine }

  TActionLine = class
  public
    Kind: TActionLineKind;
    Action: TBaseAction;
    Cond, Text: String;
    Lines: TActionLines;
    StopHere: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  { TActionLines }

  TActionLines = class(TList)
  private
    function GetLines(Index: Integer): TActionLine;
  public
  	function AddLine(NextLine: Pointer = nil): TActionLine;
    procedure DeleteLine(L: TActionLine);
    procedure Clear; override;
    property Lines[Index: Integer]: TActionLine read GetLines; default;
  end;

  { TActionRunner }

  TActionRunner = class
  private
    FDSProc: TObject;
    FDSRi: Integer;
  	FLines: TActionLines;
    FRS: TSsRecordSet;
    FNeedContinue: Boolean;
    function SaveAction(A: TBaseAction): String;
    function SaveLine(ALine: TActionLine): String;
    function SaveLines(Lines: TActionLines): String;
    procedure RunLines(ALines: TActionLines);
  public
    constructor Create;
    destructor Destroy; override;
  	procedure Load(const Xml: String);
    procedure Save(var Xml: String);
    function Run: Variant;
    property Lines: TActionLines read FLines;
    property DSProc: TObject read FDSProc write FDSProc;
    property DSRi: Integer read FDSRi write FDSRi;
    property RS: TSsRecordSet read FRS write FRS;
    property NeedContinue: Boolean read FNeedContinue;
  end;

function CreateAction(act: TdxActionType): TBaseAction;
//function ActionTypeToStr(act: TdxActionType): String;

implementation

uses
  SAX, saxbasereader, dxctrls, apputils, expressions, variants, formmanager,
  reportmanager, scriptmanager, Db, dxreports, LazUtf8, LazFileUtils, xmlreport,
  uPSUtils, uPSRuntime, uPSDebugger, BGRAGraphics, Math;

type

  { TActionReader }

  {TActionReader = class(TSAXBaseReader)
  private
    FAction: TBaseAction;
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
  public
    property Action: TBaseAction read FAction write FAction;
    procedure ParseXml(const Xml: String);
  end;    }

  { TActionLinesReader }

  TActionLinesReader = class(TSAXBaseReader)
  private
    FActionRunner: TActionRunner;
    FLines: TActionLines;
    FStack: TList;
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
    procedure DoEndElement(const NamespaceURI, LocalName, QName: SAXString); override;
  public
    property ActionRunner: TActionRunner read FActionRunner write FActionRunner;
    procedure ParseXml(const Xml: String);
  end;

function CreateAction(act: TdxActionType): TBaseAction;
begin
  Result := nil;
  case act of
    actCustom: Result := TActionCustom.Create;
    actGotoForm: Result := TGotoFormAction.Create;
    actPrint: Result := TPrintAction.Create;
    actMassCalc: Result := TMassCalcAction.Create;
    actOpenReport: Result := TOpenReportAction.Create;
    actSaveChanges: Result := TSaveChangesAction.Create;
    actUserMonitor: Result := TUserMonitorAction.Create;
    actCallFunc: Result := TCallFuncAction.Create;
    actClearFields: Result := TClearFieldsAction.Create;
    actShowMessage: Result := TShowMessageAction.Create;
  end;
  Result.FActionType := act;
end;

{function CalcExpression(const Expr: String; DSP: TDataSetProcessor; DSRi: Integer; var V: Variant): Boolean;
var
  DSR: TDataSetRec;
  Ex: TExpression;
begin
  Result := False;
  DSR := DSP.DataSets[DSRi]^;
  Ex := nil;
  with TExpressionBuilder.Create do
  try
    SkipLabels:=True;
    Form := DSR.Form;
    ParentForm := DSP.Form;
    DataSet := DSR.DataSet;
    Ex := Build(Expr);
    if Ex <> nil then
      V := Ex.Calc;
    Result := True;
  finally
    Free;
    FreeAndNil(Ex);
  end;
end; }

function ActionTypeToStr(act: TdxActionType): String;
var
  S: String;
begin
  S := '';
  case act of
    actGotoForm: S := rsGotoForm;
    actPrint: S := rsPrint;
    actMassCalc: S := rsMassCalc;
    actOpenReport: S := rsOpenReport;
    actSaveChanges: S := rsSaveChanges;
    //actUserMonitor: S := rsUserMonitor;
    actCallFunc: S := rsCallFunction;
    actClearFields: S := rsClearFields;
    actShowMessage: S := rsShowMessage;
  end;
  Result := S;
end;

{ TShowMessageAction }

function TShowMessageAction.InnerExecute: Variant;
var
  V: Variant;
  MI: TMsgInfo;
begin
  Result := Null;
  if Trim(FExprMsg) = '' then MI.Msg := FMessage
  else
  begin
    V := EvalExpr(FExprMsg, FRS);
    MI.Msg := VarToStr(V);
  end;
  MI.Title := FTitle;
  MI.MsgType := FMsgType;
  MI.Buttons := FButtons;
  MI.Visible := True;
  MI.IsAction := True;
  FRS.MsgInfo := MI;
end;

constructor TShowMessageAction.Create;
begin
  inherited Create;
  FMsgType := mtWarning;
  FButtons := [mbOk];
end;

{ TActionLinesReader }

procedure TActionLinesReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  SL: TStringList;
  i: Integer;
  Pm: TActionProp;
  TagName, AttName: String;
  act: TdxActionType;
  A: TBaseAction;
  Line: TActionLine;
  B: TMsgDlgButtons;
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  TagName := AnsiLowerCase(Utf16ToUtf8(LocalName));
  if TagName = 'action' then
  begin
    act := TdxActionType(GetInt(Atts, 'type'));
    if act = actNone then Exit;
    A := CreateAction(act);
    A.Disabled := GetBool(Atts, 'disabled');
    A.DSProc := FActionRunner.DSProc;
    A.DSRi := FActionRunner.DSRi;
    Line := FLines.AddLine;
    Line.Kind:=alkAction;
    Line.Action := A;
  end
  else if TagName = 'if' then
  begin
    Line := FLines.AddLine;
    Line.Kind := alkIf;
    Line.Cond := XmlToStr(GetStr(Atts, 'cond'));
    FStack.Add(FLines);
    FLines := Line.Lines;
  end
  else if TagName = 'elseif' then
  begin
    Line := FLines.AddLine;
    Line.Kind := alkElseIf;
    Line.Cond := XmlToStr(GetStr(Atts, 'cond'));
    FStack.Add(FLines);
    FLines := Line.Lines;
  end
  else if TagName = 'else' then
  begin
    Line := FLines.AddLine;
    Line.Kind := alkElse;
    FStack.Add(FLines);
    FLines := Line.Lines;
  end
  else if TagName = 'comment' then
  begin
    Line := FLines.AddLine;
    Line.Kind := alkComment;
    Line.Text := XmlToStr(GetStr(Atts, 'text'));
  end;

  if TagName <> 'action' then Exit;

  if A is TGotoFormAction then
    with TGotoFormAction(A) do
    begin
      FormName := XmlToStr(GetStr(Atts, 'form'));
    end
  else if A is TPrintAction then
    with TPrintAction(A) do
    begin
      TemplateFile := XmlToStr(GetStr(Atts, 'template'));
      Expression := XmlToStr(GetStr(Atts, 'expression'));
      OutFile := XmlToStr(GetStr(Atts, 'outfile'));
      if AttrExists(Atts, 'fileaction') then
        FileAction := TPrintFileAction(GetInt(Atts, 'fileaction'));
      SaveRecord := GetBool(Atts, 'saverecord');
    end
  else if A is TMassCalcAction then
    with TMassCalcAction(A) do
    begin
      FormName := XmlToStr(GetStr(Atts, 'form'));
      Filter := XmlToStr(GetStr(Atts, 'filter'));
      TableName := XmlToStr(GetStr(Atts, 'table'));
      FieldName := XmlToStr(GetStr(Atts, 'field'));
      Expression := XmlToStr(GetStr(Atts, 'expression'));
    end
  else if A is TOpenReportAction then
    with TOpenReportAction(A) do
    begin
      RpName := XmlToStr(GetStr(Atts, 'rp'));
    end
  else if A is TCallFuncAction then
    with TCallFuncAction(A) do
    begin
      Expression := XmlToStr(GetStr(Atts, 'expression'))
    end
	else if A is TClearFieldsAction then
    with TClearFieldsAction(A) do
    begin
      SplitStr(XmlToStr(GetStr(Atts, 'fields')), ';', Fields);
    end
  else if A is TShowMessageAction then
  	with TShowMessageAction(A) do
    begin
      Title := XmlToStr(GetStr(Atts, 'title'));
      Message := XmlToStr(GetStr(Atts, 'message'));
      ExprMsg := XmlToStr(GetStr(Atts, 'exprmsg'));
      MsgType := TMsgDlgType(GetInt(Atts, 'msgtype'));
      SL := TStringList.Create;
      SplitStr(GetStr(Atts, 'buttons'), ';', SL);
      B := [];
      if SL[0] = '1' then Include(B, mbOk);
      if SL[1] = '1' then Include(B, mbCancel);
      if SL[2] = '1' then Include(B, mbAbort);
      if SL[3] = '1' then Include(B, mbRetry);
      if SL[4] = '1' then Include(B, mbIgnore);
      if SL[5] = '1' then Include(B, mbYes);
      if SL[6] = '1' then Include(B, mbNo);
      if SL[7] = '1' then Include(B, mbAll);
      if SL[8] = '1' then Include(B, mbNoToAll);
      if SL[9] = '1' then Include(B, mbYesToAll);
      if SL[10] = '1' then Include(B, mbClose);
      SL.Free;
      Buttons := B;
    end
  else if A is TActionCustom then
  begin
  	TActionCustom(A).ActionId := GetStr(Atts, 'id');
    for i := 0 to Atts.Length - 1 do
    begin
      AttName := Utf16ToUtf8(Atts.GetLocalName(i));
      if (AttName = 'type') or (AttName = 'id') then Continue;
      Pm := TActionCustom(A).Props.AddProp;
      Pm.Name := AttName;
      Pm.Value := XmlToStr(Utf16ToUtf8(Atts.GetValue(i)));
    end;
  end;
end;

procedure TActionLinesReader.DoEndElement(const NamespaceURI, LocalName,
  QName: SAXString);
var
  TagName: String;
  i: Integer;
begin
  inherited DoEndElement(NamespaceURI, LocalName, QName);
  TagName := AnsiLowerCase(Utf16ToUtf8(LocalName));
  if (TagName = 'if') or (TagName = 'elseif') or (TagName = 'else') then
    if FStack.Count > 0 then
    begin
      i := FStack.Count - 1;
	    FLines := TActionLines(FStack[i]);
      FStack.Delete(i);
    end;
end;

procedure TActionLinesReader.ParseXml(const Xml: String);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(Xml);
  FLines := FActionRunner.Lines;
  FStack := TList.Create;
  try
	  ParseStream(SS);
  finally
  	FStack.Free;
    SS.Free;
  end;
end;

{ TActionLine }

constructor TActionLine.Create;
begin
  Lines := TActionLines.Create;
end;

destructor TActionLine.Destroy;
begin
  if Action <> nil then Action.Free;
  Lines.Free;
  inherited Destroy;
end;

{ TActionLines }

function TActionLines.GetLines(Index: Integer): TActionLine;
begin
  Result := TActionLine(Items[Index]);
end;

function TActionLines.AddLine(NextLine: Pointer): TActionLine;
var
  i: Integer;
begin
	Result := TActionLine.Create;
  if NextLine = nil then
  	Add(Result)
  else
  begin
	  i := IndexOf(NextLine);
  	Insert(i, Result);
  end;
end;

procedure TActionLines.DeleteLine(L: TActionLine);
begin
  Remove(L);
  L.Free;
end;

procedure TActionLines.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  	Lines[i].Free;
  inherited Clear;
end;

{ TActionRunner }

function SaveGotoFormAction(A: TGotoFormAction): String;
begin
  Result := 'form="' +  StrToXml(A.FormName) + '"';
end;

function SavePrintAction(A: TPrintAction): String;
begin
  Result:='template="' + StrToXml(A.TemplateFile) +
    '" expression="' + StrToXml(A.Expression) +
    '" outfile="' + StrToXml(A.OutFile) +
    '" fileaction="' + IntToStr(Ord(A.FileAction)) +
    '" saverecord="' + Bool2Str(A.SaveRecord) + '"';
end;

function SaveMassCalcAction(A: TMassCalcAction): String;
begin
  Result := 'form="' + StrToXml(A.FormName) + '" filter="' +
    StrToXml(A.Filter) + '" table="' + StrToXml(A.TableName) + '" field="' +
    StrToXml(A.FieldName) + '" expression="' + StrToXml(A.Expression) + '"';
end;

function SaveOpenReportAction(A: TOpenReportAction): String;
begin
  Result := 'rp="' + StrToXml(A.RpName) + '"';
end;

function SaveCallFuncAction(A: TCallFuncAction): String;
begin
  Result:='expression="' + StrToXml(A.Expression) + '"';
end;

function SaveClearFieldsAction(A: TClearFieldsAction): String;
var
  i: Integer;
  S: String;
begin
  S := '';
  for i := 0 to A.Fields.Count - 1 do
  begin
    S := S + A.Fields[i];
    if i < A.Fields.Count - 1 then
    	S := S + ';'
  end;
  Result := 'fields="' + StrToXml(S) + '"';
end;

function SaveShowMessageAction(A: TShowMessageAction): String;
var
  S, B: String;
begin
  S := 'title="' + StrToXml(A.Title) + '" message="' + StrToXml(A.Message) +
  	'" msgtype="' + IntToStr(Ord(A.MsgType)) + '" exprmsg="' +
    StrToXml(A.ExprMsg) + '" buttons="';
  B := '';
  if mbOk in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbCancel in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbAbort in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbRetry in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbIgnore in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbYes in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbNo in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbAll in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbNoToAll in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbYesToAll in A.Buttons then B := B + '1;' else B := B + '0;';
  if mbClose in A.Buttons then B := B + '1' else B := B + '0';
  Result := S + B + '"';
end;

function SaveCustomAction(A: TACtionCustom): String;
var
  i: Integer;
  P: TActionProp;
  S: String;
begin
  S := 'id="' + A.ActionId + '" ';
  for i := 0 to A.Props.Count - 1 do
  begin
    P := A.Props[i];
    S := S + AnsiLowerCase(P.Name) + '="' + StrToXml(P.Value) + '" ';
  end;
  Result := S;
end;

function TActionRunner.SaveAction(A: TBaseAction): String;
var
  S: String;
begin
  S := '';
  case A.ActionType of
		actGotoForm: S := SaveGotoFormAction(TGotoFormAction(A));
    actPrint: S := SavePrintAction(TPrintAction(A));
    actMassCalc: S := SaveMassCalcAction(TMassCalcAction(A));
    actOpenReport: S := SaveOpenReportAction(TOpenReportAction(A));
    actCallFunc: S := SaveCallFuncAction(TCallFuncAction(A));
    actClearFields: S := SaveClearFieldsAction(TClearFieldsAction(A));
    actShowMessage: S := SaveShowMessageAction(TShowMessageAction(A));
    actCustom: S := SaveCustomAction(TActionCustom(A));
  end;
  Result := '<action ';
  if A.Disabled then Result := Result + 'disabled="1" ';
  Result := Result + 'type="' + IntToStr(Ord(A.ActionType)) + '" ' + S + '/>';
end;

function TActionRunner.SaveLine(ALine: TActionLine): String;
begin
  case ALine.Kind of
    alkAction: Result := SaveAction(ALine.Action);
    alkIf: Result := '<if cond="' + StrToXml(ALine.Cond) + '">' + SaveLines(ALine.Lines) +
  		'</if>';
    alkElseIf: Result := '<elseif cond="' + StrToXml(ALine.Cond) + '">' + SaveLines(ALine.Lines) +
  		'</elseif>';
    alkElse: Result := '<else>' + SaveLines(ALine.Lines) + '</else>';
    alkComment: Result := '<comment text="' + StrToXml(ALine.Text) + '"/>';
  end;
end;

function TActionRunner.SaveLines(Lines: TActionLines): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Lines.Count - 1 do
  	Result := Result + SaveLine(Lines[i]);
end;

procedure TActionRunner.RunLines(ALines: TActionLines);
var
  i: Integer;
  L: TActionLine;
  Cond: Variant;
  SkipCond: Boolean;
  Fm: TdxForm;
begin
  Fm := FRS.Form;
  for i := 0 to ALines.Count - 1 do
  begin
    L := ALines[i];

    if FNeedContinue then
    begin
      if (L.Kind = alkAction) and L.StopHere then
      begin
        L.StopHere := False;
        FNeedContinue := False;
        //Continue;
      end
      else if L.Kind in [alkIf, alkElseIf, alkElse] then
        RunLines(L.Lines);
      //else Continue;
      Continue;
    end;

    if not (L.Kind in [alkElseIf, alkElse]) then SkipCond := False;

    if L.Kind = alkAction then
    begin
      if not L.Action.Disabled then
      begin
        L.Action.RS := FRS;
        Fm.ActionResult := L.Action.Execute;
        {if FRS.GotoUrl <> '' then
        begin
          raise EActionBreak.Create('');
        end
        else }if FRS.MsgInfo.Visible or (FRS.GotoUrl <> '')  then
        begin
          L.StopHere := True;
          FNeedContinue := True;
          raise EActionBreak.Create('');
        end;
      end;
    end
    else if (L.Kind in [alkIf, alkElseIf]) and (not SkipCond) then
    begin
      Cond := EvalExpr(L.Cond, FRS);
      if VarIsBool(Cond) and (Cond = True) then
      begin
        SkipCond := True;
        RunLines(L.Lines);
      end;
    end
    else if (L.Kind = alkElse) and (not SkipCond) then
    	RunLines(L.Lines);
  end;
end;

constructor TActionRunner.Create;
begin
  FLines := TActionLines.Create;
end;

destructor TActionRunner.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

procedure TActionRunner.Load(const Xml: String);
begin
	Lines.Clear;
  with TActionLinesReader.Create do
  try
     ActionRunner := Self;
     ParseXml(Xml);
  finally
    Free;
  end;
end;

procedure TActionRunner.Save(var Xml: String);
var
  S: String;
begin
  Xml := '';
  S := SaveLines(FLines);
  if S <> '' then
	  Xml := '<actions>' + S + '</actions>';
end;

function TActionRunner.Run: Variant;
var
  Fm: TdxForm;
  //OldResult: Variant;
begin
  Fm := FRS.Form;
  //OldResult := Fm.ActionResult;
  if not FNeedContinue then
    Fm.ActionResult := Null;

  try
    RunLines(FLines);
    Result := Fm.ActionResult;
    FNeedContinue := False;
  except
    on E: EActionBreak do
      Result := Fm.ActionResult;
  end;

  //Fm.ActionResult := OldResult;
end;

{ TSaveChangesAction }

function TSaveChangesAction.InnerExecute: Variant;
var
  IsModified: Boolean;
begin
  Result := False;
  if FRS.Form.ViewType = vtSimpleForm then Exit;

  if FRS.DataSet.State in [dsInsert, dsEdit] then
  begin
    if not FRS.Validate then Exit;
    IsModified := FRS.DataSet.Modified;
    FRS.Post;
    if IsModified then FRS.AddToHistory;
    FRS.Edit;
    Result := True;
  end;
end;

{ TActionCustom }

function TActionCustom.GetActionName: String;
var
  EA: TExprAction;
begin
  Result := '';
  EA := FRS.Session.ScriptMan.Actions.FindAction(FActionId);
  if EA <> nil then Result := EA.Name;
end;

function TActionCustom.InnerExecute: Variant;
var
  EAction: TExprAction;
  Params: TPSList;
  i, j, z, MaxCol, idx, n: Integer;
  EAC: TEAControl;
  P: PPSVariant;
  S: string;
  Prop: TActionProp;
  Rows, Cols, Titles: TStringList;
  Ex: TPSDebugExec;
  Tmp: PIFTypeRec;
  pV: PIFVariant;
  OldSelf: TObject;
  ProcNo: Cardinal;
  Proc: TPSInternalProcRec;
  Res: TbtString;
  Num: Longint;
  SD: TScriptData;
  Fm: TdxForm;
begin
  Result := inherited InnerExecute;
  EAction := FRS.Session.ScriptMan.Actions.FindAction(FActionId);
  if (EAction = nil) or not EAction.WebExists then Exit;

  Params := TIfList.Create;

  try

  SD := FRS.Session.ScriptMan.Scripts[EAction.SDi];
  if SD.Kind <> skWebExpr then Exit(Null);
  Ex := FRS.Session.ExtRunMan.GetExec(SD);

  // Сохраняем текущее значение Self
  pV := Ex.GetVar2('Self');
  if pV = nil then raise ECalcError.CreateFmt(rsExecActionFailedMsg, [EAction.Name, SD.Name]);
	OldSelf := PSGetObject(@PPSVariantData(pV)^.Data, pV^.FType);
  Fm := FRS.Form;
  if Fm.Id > 0 then
    SetVariantToClass(pV, Fm)
  // Dummy form для Main
  else
    SetVariantToClass(pV, nil);
  //

  n := EAction.Controls.GetParamCount;
  for i := EAction.Controls.Count - 1 downto 0 do
  begin
    EAC := EAction.Controls[i];
    if EAC.ControlType = eacDivider then Continue;
    Prop := FProps.Find(EAC.Name);
    if Prop <> nil then S := Prop.Value
    else S := '';

    case EAC.ControlType of
      eacNumber, eacCheckBox:
        begin
	      	P := CreateHeapVariant(Ex.FindType2(btS32));
          if TryStrToInt(S, Num) then
          	VSetInt(P, Num);
        end;
      eacColor:
        begin
      	  P := CreateHeapVariant(Ex.FindType2(btS32));
          TryStrToColor(S, Num);
      	  VSetInt(P, Num);
        end;
      eacGrid:
        begin
          Tmp := Ex.GetTypeNo(Ex.GetType('TVARIANTARRAY2D'));
          if Tmp <> nil then
  	      	P := CreateHeapVariant(Tmp)
          else
            P := CreateHeapVariant(Ex.FindType2(btS32));

          if (S <> '') and (Tmp <> nil) then
          begin

            Rows := TStringList.Create;
            Cols := TStringList.Create;
            Titles := TStringList.Create;

            SplitStr(S, '|', Rows);
            PSDynArraySetLength(PPSVariantDynamicArray(P)^.Data,
          	  PPSVariantDynamicArray(P)^.VI.FType, Rows.Count - 1);

            SplitStr(Rows[0], ';', Titles);
            Rows.Delete(0);	// Удаляем заголовок

            // При заполнении массива учитывает случаи, когда количество
            // и порядок столбцов меняется.
            MaxCol := Min(EAC.Controls.Count, Titles.Count);

            for j := 0 to Rows.Count - 1 do
            begin
              SplitStr(Rows[j], ';', Cols);
              // В случае с одной колонкой и пустой строкой надо добавлять один
              // элемент, чтобы не было ошибки index of bounds.
              if Cols.Count = 0 then Cols.Add('');

              SetLength(TVariantArray2d(PPSVariantDynamicArray(P)^.Data)[j], EAC.Controls.Count);

              // Если не будут проинициализированы ВСЕ элементы, то
              // в скрипте массив будет пустым.
              for z := 0 to EAC.Controls.Count - 1 do
              	TVariantArray2d(PPSVariantDynamicArray(P)^.Data)[j][z] := Null;

						  for z := 0 to MaxCol - 1 do
              begin
                idx := EAC.Controls.FindIndexByName(Titles[z]);
                if idx >= 0 then
                begin
                  S := DecodeCellText(Cols[z]);
                  if EAC.Controls[idx].ControlType in [eacNumber, eacCheckBox] then
                  begin
                    if TryStrToInt(S, Num) then
                      TVariantArray2d(PPSVariantDynamicArray(P)^.Data)[j][idx] := Num;
                  end
                  else if EAC.Controls[idx].ControlType = eacColor then
                  begin
                    TryStrToColor(S, Num);
                    TVariantArray2d(PPSVariantDynamicArray(P)^.Data)[j][idx] := Num;
                  end
                  else
	                  TVariantArray2d(PPSVariantDynamicArray(P)^.Data)[j][idx] := S;
                end;
              end;
            end;

            Cols.Free;
            Titles.Free;
            Rows.Free;

          end;
        end;
      else
      begin
        P := CreateHeapVariant(Ex.FindType2(btString));
        VSetString(P, S);
      end;
    end;

    Params.Add(P);
    Dec(n);
  end;

  ProcNo := Ex.GetProc(EAction.OrigName);
  Proc := Ex.GetProcNo(ProcNo) as TPSInternalProcRec;
  if Proc = nil then Exit;

  // Добавляем Result
  S := Proc.ExportDecl;
  Res := grfw(S);
  if Res <> '-1' then
  begin
    P := CreateHeapVariant(Ex.GetTypeNo(StrToInt(Res)));
    Params.Add(P);
  end
  else P := nil;
  //

	Ex.RunProc(Params, ProcNo);
  Ex.RaiseCurrentException;

  Result := Null;
  if P <> nil then PIFVariantToVariant(P, Result);

  finally
  	FreePIFVariantList(Params);
    SetVariantToClass(pV, OldSelf);
  end;
end;

constructor TActionCustom.Create;
begin
  inherited Create;
  FProps := TActionProps.Create;
end;

destructor TActionCustom.Destroy;
begin
  FProps.Free;
  inherited Destroy;
end;

{ TActionProps }

function TActionProps.GetProps(Index: Integer): TActionProp;
begin
  Result := TActionProp(Items[Index]);
end;

function TActionProps.AddProp: TActionProp;
begin
	Result := TActionProp.Create;
  Add(Result);
end;

function TActionProps.Find(const AName: String): TActionProp;
var
  i: Integer;
  P: TActionProp;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    P := Props[i];
    if CompareText(AName, P.Name) = 0 then Exit(P);
  end;
end;

procedure TActionProps.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  	Props[i].Free;
  inherited Clear;
end;

{ TClearFieldsAction }

function TClearFieldsAction.InnerExecute: Variant;
var
  i: Integer;
  F: TdxField;
begin
  inherited InnerExecute;
  Result := False;

  if not (FRS.DataSet.State in [dsInsert, dsEdit]) then Exit;

  for i := 0 to FFields.Count - 1 do
  begin
    F := FRS.Form.FindFieldByName(FFields[i]);
    if F = nil then Continue;
    if F is TdxDBImage then FRS.ClearImage(F)
    else if F is TdxFile then FRS.ClearFile(F)
    else if F is TdxLookupComboBox then FRS.ClearObject(F)
    else if F is TdxCheckBox then FRS.SetDSField(F, 0)
    else FRS.SetDSField(F, Null);
  end;
  Result := True;
end;

constructor TClearFieldsAction.Create;
begin
  inherited Create;
  FFields := TStringListUtf8.Create;
end;

destructor TClearFieldsAction.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

{ TCallFuncAction }

function TCallFuncAction.InnerExecute: Variant;
begin
  inherited InnerExecute;
  if Trim(FExpr) <> '' then
    Result := EvalExpr(FExpr, FRS);
end;

{ TUserMonitorAction }

function TUserMonitorAction.InnerExecute: Variant;
begin
  inherited InnerExecute;
  FRS.GotoUrl := '?usermon';
  FRS.GotoOption := gtoDefault;
  Result := True;
end;

{ TOpenReportAction }

function TOpenReportAction.InnerExecute: Variant;
var
  RD: TReportData;
begin
  Result := True;
  RD := FRS.Session.ReportMan.FindReportByName(FRpName);
  FRS.GotoUrl := '?rp=' + IntToStr(RD.Id);
  FRS.GotoOption := gtoDefault;
end;

{ TMassCalcAction }

{procedure Recalculate(RS: TSsRecordSet; ATableName, AFieldName, aExpr: String);
var
  E: TExpression;
  EB: TExpressionBuilder;
  C: TdxField;
  TblRS: TSsRecordSet;
begin
  if ATableName <> '' then
  begin
    TblRS := RS.Forms.FindFormByName(ATableName);
    C := TblRS.Form.FindFieldByName(AFieldName);
  end
  else
    C := RS.Form.FindFieldByName(AFieldName);

  E := nil;
  aExpr := Trim(aExpr);
  if aExpr <> '' then
  begin
    EB := TExpressionBuilder.Create;
    EB.SkipLabels := True;
    if ATableName = '' then
      EB.RecordSet := RS
    else
      EB.RecordSet := TblRS;
    try
      E := EB.Build(aExpr);
    finally
      EB.Free;
    end;
  end;

  if (E = nil) and (aExpr <> '') then Exit;

  try
    RS.DataSet.First;
    while not RS.DataSet.EOF do
    begin
      RS.DataSet.Edit;
      if ATableName <> '' then
      begin
        TblRS.Open;
        while not TblRS.DataSet.Eof do
        begin
          TblRS.DataSet.Edit;
          TblRS.SetDSField(C, E.Calc);
          TblRS.Post;
          TblRS.DataSet.Next;
        end;
      end
      else
        RS.SetDSField(C, E.Calc);
      RS.Post;
      RS.DataSet.Next;

    end;

  finally
    FreeAndNil(E);
  end;
end;      }

function TMassCalcAction.InnerExecute: Variant;
var
  Fm: TdxForm;
  RSet: TSsRecordSet;
begin
  Result := True;
  Fm := FRS.Session.FormMan.FindFormByName(FFormName);
  RSet := TSsRecordSet.Create(FRS.Session, nil);
  RSet.AssignForm(Fm);
  RSet.Form.CustomFilter := FFilter;
  RSet.Form.CustomFilterRS := FRS;
  RSet.Form.UseSelectCondition := False;
  RSet.OpenRecords;
  RSet.Recalculate(FTableName, FFieldName, FExpression);
  RSet.Free;
end;

{ TPrintAction }

{function TrySaveRecord(DSP: TDataSetProcessor): Boolean;
begin
  Result := True;
  if DSP.MasterSet.State in [dsInsert, dsEdit] then
  begin
    if DSP.Validate(0, True) then
    begin
    	DSP.Post;
      DSP.InnerEdit(0, True, True, False);
    end
    else Result := False;
  end;
end;    }

function TPrintAction.InnerExecute: Variant;
var
  Tpl, TplFullName, OutName, Errs: String;
  IsModified: Boolean;
begin
  Result := False;
  //if FSaveRecord and not FRS.Validate then Exit;

  if FSaveRecord and (FRS.DataSet.State in [dsInsert, dsEdit]) and
    (FRS.Form.ViewType <> vtSimpleForm) then
  begin
    if not FRS.Validate then Exit;
    IsModified := FRS.DataSet.Modified;
    FRS.Post;
    if IsModified then FRS.AddToHistory;
    FRS.Edit;
  end;


  Tpl := Trim(FTemplateFile);
  if (Tpl = '') and (Trim(FExpression) <> '') then
    Tpl := VarToStr(EvalExpr(FExpression, FRS));
  if Tpl = '' then Exit;
  TplFullName := FRS.Session.GetTemplatesPath + StringReplace(Tpl, '\',
    DirectorySeparator, [rfReplaceAll]);
  OutName := ExtractFileName(TplFullName);

  if not FileExists(TplFullName) then
    raise Exception.Create(Format(rsTemplateNotFound, [Tpl]));

  ReportToXXX(RS, TplFullName, GetCachePath(FRS.Session) + OutName, Errs);
  FRS.DocUrl := GetCachePath(FRS.Session, True) + OutName;
  FRS.PrintErrors := Errs;
  Result := True;
end;

constructor TPrintAction.Create;
begin
  inherited Create;
  FFileAction := pfaOpen;
end;

{ TGotoFormAction }

function TGotoFormAction.InnerExecute: Variant;
var
  Fm: TdxForm;
begin
  Result := True;
  Fm := FRS.Session.FormMan.FindFormByName(FFormName);
  FRS.GotoUrl := GetFormHRef(Fm);// '?fm=' + IntToStr(Fm.Id);
  FRS.GotoOption := gtoDefault;
end;

{ TActionReader }

(*procedure TActionReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  SL: TStringList;
  i: Integer;
  AttName: SAXString;
  Pm: TActionProp;
  Fm: TdxForm;
  C: TComponent;
  RD: TReportData;
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  {with FAction do
  begin
    ExecCond := XmlToStr(Atts.GetValue('', 'execcond'));
    ConfirmMsg := XmlToStr(Atts.GetValue('', 'confirm'));
    Msg := XmlToStr(Atts.GetValue('', 'msg'));
  end; }
  if FAction is TGotoFormAction then
    with TGotoFormAction(FAction) do
    begin
      Fm := FormMan.FindForm(GetInt(Atts, 'formid'));
      if Fm <> nil then FormName := Fm.FormCaption;
    end
  else if FAction is TPrintAction then
    with TPrintAction(FAction) do
    begin
      TemplateFile := Atts.GetValue('', 'template');
      Expression := XmlToStr(Atts.GetValue('', 'expression'));
      OutFile := XmlToStr(Atts.GetValue('', 'outfile'));
      SaveRecord := GetBool(Atts, 'saverecord');
      //ToPrinter := GetBool(Atts, 'toprinter');
    end
  else if FAction is TMassCalcAction then
    with TMassCalcAction(FAction) do
    begin
      Fm := FormMan.FindForm(GetInt(Atts, 'formid'));
      if Fm <> nil then FormName := Fm.FormCaption;
      Filter := XmlToStr(Atts.GetValue('', 'filter'));
      if Fm <> nil then
      begin
	      C := FindById(Fm, GetInt(Atts, 'fieldid'));
        if C <> nil then FieldName := GetFieldName(C);
      end;
      Expression := XmlToStr(Atts.GetValue('', 'expression'));
    end
  else if FAction is TOpenReportAction then
    with TOpenReportAction(FAction) do
    begin
      RD := ReportMan.FindReport(GetInt(Atts, 'rpid'));
      if RD <> nil then RpName := RD.Name;
    end
  else if FAction is TCallFuncAction then
    with TCallFuncAction(FAction) do
    begin
      Expression := XmlToStr(Atts.GetValue('', 'expression'))
    end
	else if FAction is TClearFieldsAction then
    with TClearFieldsAction(FAction) do
    begin
      SL := TStringList.Create;
      SplitStr(Atts.GetValue('', 'fields'), ';', SL);
      Fm := TdxForm(FTmpForm);
      for i := 0 to SL.Count - 1 do
      begin
        C := FindById(Fm, StrToInt(SL[i]));
        if C <> nil then
        	Fields.Add(GetFieldName(C));
      end;
      SL.Free;
    end
	else if FAction is TActionCustom then
  begin
    for i := 0 to Atts.Length - 1 do
    begin
      AttName := Atts.GetLocalName(i);
      Pm := TActionCustom(FAction).Props.AddProp;
      Pm.Name := AttName;
      Pm.Value := XmlToStr(Atts.GetValue(i));
    end;
  end;
end;

procedure TActionReader.ParseXml(const Xml: String);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(Xml);
  ParseStream(SS);
  SS.Free;
end;     *)

{ TBaseAction }

function TBaseAction.GetActionName: String;
begin
  Result := ActionTypeToStr(FActionType);
end;

function TBaseAction.InnerExecute: Variant;
begin
  Result := Null;
end;

constructor TBaseAction.Create;
begin

end;

{procedure TBaseAction.Load(const Xml: String);
begin
  with TActionReader.Create do
  begin
    Action := Self;
    ParseXml(Xml);
    Free;
  end;
end;   }

function TBaseAction.Execute: Variant;
begin
  try
    Result := InnerExecute;
  except
    on E: Exception do
    begin
      raise Exception.CreateFmt(rsActionExecError, [ActionName, LineEnding + LineEnding + E.Message]);
    end;
  end;
end;

end.

