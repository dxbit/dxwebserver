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

unit ScriptManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPSCompiler, uPSRuntime, uPSUtils, sqldb, dxctrls,
  uPSPreProcessor, uPSDebugger, strconsts, SAX, SAXBaseReader, mytypes,
  FormManager, LazFileUtils;

type
  TScriptData = class;

  TCompilerMsg = class
  public
    Col, Row, Pos: Integer;
    Msg, ErrorType, ModuleName: String;
    SD: TScriptData;
  end;

  TScriptKind = (skNone, skMain, skForm, skUser, skExpr, skWebMain, skWebForm, skWebExpr);

  { TSourceMarkData }

  TSourceMarkData = class
  private
    FBookmarkNumber: Integer;
    FColumn: Integer;
    FRow: Integer;
  public
    constructor Create;
    function IsBookmark: Boolean;
    property BookmarkNumber: Integer read FBookmarkNumber write FBookmarkNumber;
    property Row: Integer read FRow write FRow;
    property Column: Integer read FColumn write FColumn;
  end;

  { TSourceMarks }

  TSourceMarks = class(TList)
  private
    function GetMarks(Index: Integer): TSourceMarkData;
  public
    function AddMark: TSourceMarkData;
    procedure Clear; override;
    procedure ClearBreakpoints;
    function HasBreakpoints: Boolean;
    function FindBreakpoint(aRow: Integer): TSourceMarkData;
    procedure DeleteBreakpoint(Mark: TSourceMarkData);
    property Marks[Index: Integer]: TSourceMarkData read GetMarks; default;
  end;

  { TScriptSourceData }

  TScriptSourceData = class
  private
    FCaretX: Integer;
    FCaretY: Integer;
    FFoldState: String;
    FLeftChar: Integer;
    FMarks: TSourceMarks;
    FTopLine: Integer;
    FXml: TSAXBaseReader;
    procedure XmlStartElement(Sender: TObject; const NamespaceURI, LocalName,
      QName: SAXString; Atts: TSAXAttributes);
  public
    constructor Create;
    destructor Destroy; override;
    function SaveToString: String;
    procedure LoadFromString(const Buf: String);
    procedure SaveToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String);
    property CaretX: Integer read FCaretX write FCaretX;
    property CaretY: Integer read FCaretY write FCaretY;
    property TopLine: Integer read FTopLine write FTopLine;
    property LeftChar: Integer read FLeftChar write FLeftChar;
    property FoldState: String read FFoldState write FFoldState;
    property Marks: TSourceMarks read FMarks;
  end;

  TLineData = class
  public
    SD: TScriptData;
    Line,							// Позиция в модуле
    AbsLine: Integer;		// Позиция в сборном исходнике (абсолютная позиция)
    Breakpoint: Boolean;
  end;

  { TScriptData }
  TScriptData = class
  private
    FAuthor: String;
    FDescription: String;
    FHomePage: String;
    FMsgs, FLines: TList;
    FSourceData: TScriptSourceData;
    FVersion: String;
    function GetLines(Index: Integer): TLineData;
    function GetMsgs(Index: Integer): TCompilerMsg;
  public
    Name, Source, Bin, DebugData: String;
    FmId: Integer;
    Kind: TScriptKind;
    Form: TdxForm;
    constructor Create;
    destructor Destroy; override;
    function GetModuleName: String;
    function AddMsg: TCompilerMsg;
    function MsgCount: Integer;
    procedure Clear;
    function AddLine(Line: Integer): TLineData;
    function LineCount: Integer;
    function HasBreakpoints: Boolean;
    property Lines[Index: Integer]: TLineData read GetLines;
    property Msgs[Index: Integer]: TCompilerMsg read GetMsgs;
    property SourceData: TScriptSourceData read FSourceData;
    property Author: String read FAuthor;
    property Version: String read FVersion;
    property HomePage: String read FHomePage;
    property Description: String read FDescription;
  end;

  //TScriptManager = class;

  { TScriptCompiler }

  TScriptCompiler = class(TPSPascalCompiler)
  private
    //FScriptMan: TSCriptManager;
    FSD: TScriptData;
    FPre: TPSPreProcessor;
    FIncludes, FCallers: TStringListUtf8;
    FMetaData: TObject;
    procedure AddLines(const Source: String);
    function RegProcExists(const ProcName: String): Boolean;
    procedure FunctionStartHandler(name: tbtString; Pos, Row, Col: Integer);
  public
    constructor Create(AMetaData: TObject);
    destructor Destroy; override;
    function Compile(const Source: String): Boolean;
    property SD: TScriptData read FSD write FSD;
    property Pre: TPSPreProcessor read FPre;
    property Includes: TStringListUtf8 read FIncludes;
    property Callers: TStringListUtf8 read FCallers;
   // property ScriptMan: TScriptManager read FScriptMan;
  end;

  TExprFunc = class
  public
    OrigName: String;    		// Имя функции в модуле выражений
    SDi: Integer;						// Индекс модуля
    Name: String;         	// Имя функции в выражении
    Args: String;
    ResultType: Char;
    Description: String;
    Group: String;
    WebExists: Boolean;     // Есть ли веб-версия
  end;

  { TExprFuncs }

  TExprFuncs = class(TList)
  private
    function GetFuncs(Index: Integer): TExprFunc;
  public
    function AddFunc: TExprFunc;
    function FindFunc(const Name: String): TExprFunc;
    function FindFuncIndex(const Name: String): Integer;
    procedure Clear; override;
    property Funcs[Index: Integer]: TExprFunc read GetFuncs; default;
  end;

  TEAControls = class;

  TEAControlType = (eacNone, eacGrid, eacText, eacNumber, eacCheckBox, eacFile,
  	eacFolder, eacExpr, eacForm, eacChildForm, eacQuery, eacObject, eacField,
    eacComponent, eacReport, eacTemplate, eacList, eacFilter, eacDivider,
    eacColor, eacImage);
  TEAControlTypes = set of TEAControlType;

  { TEAControl }

  TEAControl = class
  private
    FControls: TEAControls;
  public
    ControlType: TEAControlType;
    Name, Caption, Filter, Source, Form, Items, DefaultValue, TextHint: String;
    Width, Height: Integer;
    Required, NoForm, Editing: Boolean;
    constructor Create;
    destructor Destroy; override;
    property Controls: TEAControls read FControls;
  end;

  { TEAControls }

  TEAControls = class(TList)
  private
    function GetControls(Index: Integer): TEAControl;
  public
    function AddControl: TEAControl;
    function FindByName(const AName: String): TEAControl;
    function FindIndexByName(const AName: String): Integer;
    function GetParamCount: Integer;
    procedure Clear; override;
    property Controls[Index: Integer]: TEAControl read GetControls; default;
  end;

  TEACheckList = class;

  { TEACheck }

  TEACheck = class
  private
    FChecks: TEACheckList;
  public
    Expr, Msg, FocusControl, GridControl: String;
    constructor Create;
    destructor Destroy; override;
    property Checks: TEACheckList read FChecks;
  end;

  { TEACheckList }

  TEACheckList = class(TList)
  private
    function GetChecks(Index: Integer): TEACheck;
  public
    function AddCheck: TEACheck;
    procedure Clear; override;
    property Checks[Index: Integer]: TEACheck read GetChecks; default;
  end;

  { TExprAction }

  TActionTarget = (atAll, atForm, atButton, atMain);
  TActionTargets = set of TActionTarget;

  // Хотя использую приставку Expr, "действия" к выражениям никак не относятся.
  TExprAction = class
  private
    FChecks: TEACheckList;
    FControls: TEAControls;
  public
    Id: String;							// Уникальный ID
    Target: TActionTarget;  // Где применяется действие: форма, кнопка или везде.
    OrigName: String;   		// Имя процедуре в модуле расширений
    SDi: Integer;						// Индекс модуля
    Name, Group: String;
    //Glyph: String;
    Description: String;
    WebExists: Boolean;     // Есть ли веб-версия?
    constructor Create;
    destructor Destroy; override;
    property Controls: TEAControls read FControls;
    property Checks: TEACheckList read FChecks;
  end;

  { TExprActions }

  TExprActions = class(TList)
  private
    function GetActions(Index: Integer): TExprAction;
  public
    function AddAction: TExprAction;
    function FindAction(const Id: String): TExprAction;
    function ActionExists(Group, Name: String): Boolean;
    procedure Clear; override;
    property Actions[Index: Integer]: TExprAction read GetActions; default;
  end;

  { TScriptManager }

  TScriptManager = class
  private
    FActions: TExprActions;
    //FExprModule: TScriptData;
    //FNeedUpdateFuncs: Boolean;
    FScripts: TList;
    FCompiler: TScriptCompiler;
    FFuncs: TExprFuncs;
    FUseDebugInfo: Boolean;
    FMetaData: TObject;
    function GetScripts(Index: Integer): TScriptData;
    procedure ParseExprModules;
  public
    constructor Create(AMetaData: TObject);
    destructor Destroy; override;
    procedure ModulesToList(SL: TStrings; Kind: TScriptKind);
    function AddScript(FmId: Integer; const aName, Source: String): TScriptData;
    function FindScript(FmId: Integer): TScriptData;
    function FindScriptByName(const aName: String): TScriptData;
    function GetScriptIndex(SD: TScriptData): Integer;
    function ScriptCount: Integer;
    procedure CompileModule(SD: TScriptData);
    procedure CompileAll;
    //procedure CompileMain;
    procedure CompileExpr;
    procedure ModuleMessagesToList(SD: TScriptData; L: TStrings; OnlyErrors: Boolean = False);
    procedure MessagesToList(L: TStrings; OnlyErrors: Boolean = False);
    function HasErrorsInModule(SD: TScriptData): Boolean;
    function HasErrors: Boolean;
    function MakeUniqueScriptName(const AnyName: String): String;
    procedure ParseExprModule(SD: TScriptData);
    property Scripts[Index: Integer]: TScriptData read GetScripts;
    //property ExprModule: TScriptData read FExprModule;
    property Funcs: TExprFuncs read FFuncs;
    property Actions: TExprActions read FActions;
    //property NeedUpdateFuncs: Boolean read FNeedUpdateFuncs write
		//	FNeedUpdateFuncs; // Говорит окну выражений обновить список ключевых слов
  end;

  { TRunScript }

  TRunScript = class
  private
    FErrorMsg: String;
    FExec: TPSDebugExec;
    FImporter: TPSRuntimeClassImporter;
    FSD: TScriptData;
    FUseDebugInfo: Boolean;
    FRS: TObject;
    FSS: TObject;
  public
    constructor Create(SS: TObject);
    destructor Destroy; override;
    function LoadBin: Boolean;
    function TryRunProc(const ProcName: String; Args: array of Variant): Boolean;
    function TryRunFunc(const FuncName: String; Args: array of Variant; var aResult: Variant): Boolean;
    procedure BindForm(ARecordSet: TObject);
    procedure BindVars;
    property SD: TScriptData read FSD write FSD;
    property Exec: TPSDebugExec read FExec;
    property UseDebugInfo: Boolean read FUseDebugInfo write FUseDebugInfo;
    property ErrorMsg: String read FErrorMsg;
  end;

  { TExtRunManager }

  TExtRunManager = class
  private
    FScripts: TList;
    FSS: TObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    function TryRunFunc(SD: TScriptData; const FuncName: String; Args: array of Variant; var aResult: Variant): Boolean;
		function GetExec(SD: TScriptData): TPSDebugExec;
    property Session: TObject read FSS write FSS;
  end;

  { EModuleParserError }

  EModuleParserError = class(Exception)
  private
    FPos: Integer;
  public
    constructor Create(const Msg: String; aPos: Integer);
    property Position: Integer read FPos;
  end;

  { TModuleParser }

  TModuleParser = class
  private
    FActions: TExprActions;
    FAuthor: String;
    FDescription: String;
    FFuncs: TExprFuncs;
    FPos, FLen: Integer;
    FBuf: String;
    FHomePage: String;
    FVersion: String;
    FSDi: Integer;
    FWebExtMode: Boolean;
    procedure SkipWhites;
    function Exists(const S: String): Boolean;
    procedure Consume(const S: String);
    function GetComment: String;
    function GetStr: String;
    function GetMultiStr(const EndChars: String): String;
    procedure DoError(const Msg: String; Params: array of const);
    procedure ParseAction;
    procedure ParseFunction;
    procedure ParseHead;
  public
    procedure Parse(const Buf: String; OnlyHead: Boolean; SDi: Integer);
    property Funcs: TExprFuncs read FFuncs write FFuncs;
    property Actions: TExprActions read FActions write FActions;
    property Author: String read FAuthor;
    property Version: String read FVersion;
    property HomePage: String read FHomePage;
    property Description: String read FDescription;
    property WebExtMode: Boolean read FWebExtMode write FWebExtMode;
  end;

  TScriptLastError = record
    ModuleName, ProcName: String;
    ExObj: TObject;
    Kind: TScriptKind;
    SD: TScriptData;
  end;

var
  //MainModule: TRunScript;
  //ExtRunMan: TExtRunManager;
  ScriptLastError: TScriptLastError;

function ScriptLastErrorToString: String;
function EPSExceptionToString(E: EPSException): String;

implementation

uses
  apputils, LazUtf8, FileUtil, StrUtils, dxtypes, compilerdecls, rundecls, uPSR_dll;

type

  { TUIReader }

  TUIReader = class(TSAXBaseReader)
  private
    FAction: TExprAction;
    FEAControl: TEAControl;
    FGridChk: TEACheck;
    FNames: TStringList;
    procedure CheckActionComponentName(const TagName, S: String);
    procedure CheckActionComponentSource(const TagName, S: String);
    procedure CheckActionComponentForm(const TagName, S: String);
    procedure CheckActionFocusComponent(const S: String);
    procedure CheckActionGridComponent(const S: String);
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
    procedure DoEndElement(const NamespaceURI, LocalName, QName: SAXString); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseXml(const Xml: String);
    property Action: TExprAction read FAction write FAction;
  end;

function ScriptKindToString(K: TScriptKind): String;
begin
  case K of
    skMain: Result := 'Main';
    skWebMain: Result := 'WebMain';
    skForm: Result := rsForm;
    skWebForm: Result := rsWebForm;
    skUser: Result := rsUser;
    skExpr: Result := rsExtension;
    skWebExpr: Result := rsWebExpression;
    else Result := '';
  end;
end;

function ScriptLastErrorToString: String;
begin
  if ScriptLastError.ExObj <> nil then
    Result := Format(rsScriptErrorMsg, [
      Exception(ScriptLastError.ExObj).Message + LineEnding + LineEnding,
      ScriptLastError.ExObj.ClassName + LineEnding,
      ScriptLastError.ModuleName + LineEnding, ScriptKindToString(ScriptLastError.Kind) + LineEnding,
      ScriptLastError.ProcName])
  else
    Result := '';
end;

function EPSExceptionToString(E: EPSException): String;
var
  SD: TScriptData;
  Proc: PIFProcRec;
  S: String;
begin
  SD := TRunScript(E.Exec.Id).SD;
  Proc := E.Exec.GetProcNo(E.ProcNo);
  if Proc is TPSInternalProcRec then S := TPSInternalProcRec(Proc).ExportName
  else if Proc is TPSExternalProcRec then S := TPSExternalProcRec(Proc).Name
  else S := '';
  Result := Format(rsScriptErrorMsg, [E.Message + LineEnding + LineEnding,
    E.ClassName + LineEnding, SD.GetModuleName + LineEnding,
    ScriptKindToString(SD.Kind) + LineEnding,
    S]);
end;

procedure ExecException(Sender: TPSExec; ExError: TPSError;
  const ExParam: tbtstring; ExObject: TObject; ProcNo, Position: Cardinal);
var
  SD: TScriptData;
  Proc: PIFProcRec;
  S: String;
begin
  if ExObject = nil then Exit;
  SD := TRunScript(Sender.Id).SD;
  ScriptLastError.ModuleName:=SD.GetModuleName;
  ScriptLastError.Kind := SD.Kind;
  ScriptLastError.ExObj := ExObject;
  Proc := Sender.GetProcNo(ProcNo);
  if Proc is TPSInternalProcRec then S := TPSInternalProcRec(Proc).ExportName
  else if Proc is TPSExternalProcRec then S := TPSExternalProcRec(Proc).Name
  else S := '';
  ScriptLastError.ProcName:=S;
end;

{ TEACheck }

constructor TEACheck.Create;
begin
  FChecks := TEACheckList.Create;
end;

destructor TEACheck.Destroy;
begin
  FChecks.Free;
  inherited Destroy;
end;

{ TEACheckList }

function TEACheckList.GetChecks(Index: Integer): TEACheck;
begin
  Result := TEACheck(Items[Index]);
end;

function TEACheckList.AddCheck: TEACheck;
begin
  Result := TEACheck.Create;
  Add(Result);
end;

procedure TEACheckList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Checks[i].Free;
  inherited Clear;
end;

{ TExtRunManager }

procedure ExecExceptionLoadData(Sender: TPSExec; ExError: TPSError;
  const ExParam: tbtstring; ExObject: TObject; ProcNo, Position: Cardinal);
var
  SD: TScriptData;
begin
  SD := TRunScript(Sender.Id).SD;
  TRunScript(Sender.Id).FErrorMsg := Format(rsLoadDataErrorMsg, [LineEnding,
    PSErrorToString(ExError, ExParam) + LineEnding,
    SD.GetModuleName + LineEnding, ScriptKindToString(SD.Kind)]);
end;

constructor TExtRunManager.Create;
begin
  FScripts := TList.Create;
end;

destructor TExtRunManager.Destroy;
begin
  ClearList(FScripts);
  FScripts.Free;
  inherited Destroy;
end;

procedure TExtRunManager.Init;
var
  i: Integer;
  SD: TScriptData;
  RS: TRunScript;
  ScriptMan: TScriptManager;
begin
  ClearList(FScripts);
  ScriptMan := TSession(FSS).ScriptMan;
  for i := 0 to ScriptMan.ScriptCount - 1 do
  begin
    SD := ScriptMan.Scripts[i];
    if SD.Kind <> skWebExpr then Continue;
    RS := TRunScript.Create(FSS);
    RS.SD := SD;
    RS.LoadBin;
    RS.BindVars;
    FScripts.Add(RS);
  end;
end;

function TExtRunManager.TryRunFunc(SD: TScriptData; const FuncName: String;
  Args: array of Variant; var aResult: Variant): Boolean;
var
  i: Integer;
  RS: TRunScript;
begin
  Result := False;
  for i := 0 to FScripts.Count - 1 do
  begin
  	RS := TRunScript(FScripts[i]);
    if RS.SD = SD then
    	Exit( RS.TryRunFunc(FuncName, Args, aResult) );
  end;
end;

function TExtRunManager.GetExec(SD: TScriptData): TPSDebugExec;
var
  i: Integer;
  RS: TRunScript;
begin
  Result := nil;
  for i := 0 to FScripts.Count - 1 do
  begin
  	RS := TRunScript(FScripts[i]);
    if RS.SD = SD then
    	Exit(RS.Exec);
  end;
end;

{ TExprAction }

constructor TExprAction.Create;
begin
  FControls := TEAControls.Create;
  FChecks := TEACheckList.Create;
end;

destructor TExprAction.Destroy;
begin
  FChecks.Free;
  FControls.Free;
  inherited Destroy;
end;

{ TEAControls }

function TEAControls.GetControls(Index: Integer): TEAControl;
begin
  Result := TEAControl(Items[Index]);
end;

function TEAControls.AddControl: TEAControl;
begin
	Result := TEAControl.Create;
  Add(Result);
end;

function TEAControls.FindByName(const AName: String): TEAControl;
var
  i: Integer;
  EAC: TEAControl;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    EAC := Controls[i];
    if CompareText(AName, EAC.Name) = 0 then Exit(EAC);
  end;
end;

function TEAControls.FindIndexByName(const AName: String): Integer;
var
  i: Integer;
  EAC: TEAControl;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    EAC := Controls[i];
    if CompareText(AName, EAC.Name) = 0 then Exit(i);
  end;
end;

function TEAControls.GetParamCount: Integer;
var
  n, i: Integer;
begin
  n := 0;
  for i := 0 to Count - 1 do
    if Controls[i].ControlType <> eacDivider then Inc(n);
  Result := n;
end;

procedure TEAControls.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  	Controls[i].Free;
  inherited Clear;
end;

{ TEAControl }

constructor TEAControl.Create;
begin
  FControls := TEAControls.Create;
end;

destructor TEAControl.Destroy;
begin
  FControls.Free;
  inherited Destroy;
end;

function CheckValidName(const S: String): Boolean;
var
  i: Integer;
begin
  for i := 1 to Length(S) do
  begin
    if S[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_'] then
    else Exit(False);
  end;
  if S[1] in ['A'..'Z', 'a'..'z', '_'] then Result := True
  else Exit(False);
end;

function CheckReservedName(const S: String): Boolean;
begin
  Result := (CompareText(S, 'id') <> 0) and (CompareText(S, 'type') <> 0);
end;

procedure TUIReader.CheckActionComponentName(const TagName, S: String);
begin
  if S = '' then
  begin
    if AnsiLowerCase(TagName) <> 'divider' then
	    raise Exception.CreateFmt('Name in tag [%s] not defined.', [TagName])
  end
  else if not CheckValidName(S) then
    raise Exception.CreateFmt('Invalid name in tag [%s]: %s.', [TagName, S])
  else if not CheckReservedName(S) then
    raise Exception.CreateFmt('Name [%s] is reserved in tag [%s].', [S, TagName])
  else if FNames.IndexOf(S) >= 0 then
    raise Exception.CreateFmt('Duplicate name in tag [%s]: %s', [TagName, S]);
end;

procedure TUIReader.CheckActionComponentSource(const TagName, S: String);
begin
  if FNames.IndexOf(S) < 0 then
    raise Exception.CreateFmt('Source in tag [%s] not found: %s', [TagName, S]);
end;

procedure TUIReader.CheckActionComponentForm(const TagName, S: String);
begin
  if FNames.IndexOf(S) < 0 then
    raise Exception.CreateFmt('Form in tag [%s] not found: %s', [TagName, S]);
end;

procedure TUIReader.CheckActionFocusComponent(const S: String);
begin
  if S = '' then Exit;
  if FNames.IndexOf(S) < 0 then
    raise Exception.Create('Component in tag [if] not found: ' + S);
end;

procedure TUIReader.CheckActionGridComponent(const S: String);
begin
  if FNames.IndexOf(S) < 0 then
    raise Exception.Create('Grid in tag [ifgrid] not found: ' + S);
end;

procedure TUIReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  Nm: String;
  eac: TEAControlType;
  C: TEAControl;
  i: Integer;
  S: SAXString;
  Chk: TEACheck;
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  eac := eacNone;
  Nm := AnsiLowerCase(LocalName);
  case Nm of
    'grid': eac := eacGrid;
		'text': eac := eacText;
    'number': eac := eacNumber;
    'checkbox': eac := eacCheckBox;
    'file': eac := eacFile;
    'folder': eac := eacFolder;
    'expr': eac := eacExpr;
    'form': eac := eacForm;
    'childform': eac := eacChildForm;
    'query': eac := eacQuery;
    'object': eac := eacObject;
    'field': eac := eacField;
    'component': eac := eacComponent;
    'report': eac := eacReport;
    'template': eac := eacTemplate;
    'list': eac := eacList;
    'filter': eac := eacFilter;
    'divider': eac := eacDivider;
    'color': eac := eacColor;
    'image': eac := eacImage;
    'ui', 'if', 'ifgrid':;
    else raise Exception.CreateFmt('Unknown UI Tag: %s', [LocalName]);
  end;

  if eac = eacNone then
  begin
    if Nm = 'if' then
    begin
      if Atts = nil then raise Exception.Create('Attribute [expr] in tag [if] not defined.');
      for i := 0 to Atts.Length - 1 do
      begin
        S := Atts.GetLocalName(i);
        if Pos(';' + LowerCase(S) + ';', ';expr;msg;focus;') = 0 then
          raise Exception.CreateFmt('Unknown attribute [%s] in tag [if]', [S]);
      end;
      if FGridChk = nil then
        Chk := FAction.Checks.AddCheck
      else
        Chk := FGridChk.Checks.AddCheck;
      Chk.Expr := XmlToStr(GetStr(Atts, 'expr'));
      if Chk.Expr = '' then raise Exception.Create('Attribute [expr] in tag [if] not defined.');
      Chk.Msg := XmlToStr(GetStr(Atts, 'msg'));
      if Chk.Msg = '' then raise Exception.Create('Attribute [msg] in tag [if] not defined.');
      Chk.FocusControl := GetStr(Atts, 'focus');
      CheckActionFocusComponent(Chk.FocusControl);
    end
    else if Nm = 'ifgrid' then
    begin
      if Atts = nil then raise Exception.Create('Attribute [grid] in tag [ifgrid] not defined.');
      for i := 0 to Atts.Length - 1 do
      begin
        S := Atts.GetLocalName(i);
        if Pos(';' + LowerCase(S) + ';', ';grid;') = 0 then
          raise Exception.CreateFmt('Unknown attribute [%s] in tag [ifgrid]', [S]);
      end;
      Chk := FAction.Checks.AddCheck;
      Chk.GridControl := GetStr(Atts, 'grid');
      if Chk.GridControl = '' then raise Exception.Create('Attribute [grid] in tag [ifgrid] not defined.');
      CheckActionGridComponent(Chk.GridControl);
      FGridChk := Chk;
    end;
    Exit;
  end;

  if FEAControl = nil then
	  C := FAction.Controls.AddControl
  else
    C := FEAControl.Controls.AddControl;

  C.ControlType:=eac;
  if eac = eacGrid then FEAControl := C;

	if Atts = nil then
  begin
    CheckActionComponentName(LocalName, '');
    Exit;
  end;

  for i := 0 to Atts.Length - 1 do
  begin
    S := Atts.GetLocalName(i);
    if Pos(';' + LowerCase(S) + ';',
      ';name;caption;filter;source;items;width;height;required;defaultvalue;noform;form;editing;texthint;') = 0 then
      raise Exception.CreateFmt('Unknown attribute [%s] in tag [%s]', [S, LocalName]);
  end;

  C.Name := GetStr(Atts, 'name');
  CheckActionComponentName(LocalName, C.Name);
  C.Caption := XmlToStr(GetStr(Atts, 'caption'));
  C.Filter := XmlToStr(GetStr(Atts, 'filter'));
  C.Source := GetStr(Atts, 'source');
  if C.Source <> '' then CheckActionComponentSource(LocalName, C.Source);
  C.Form := GetStr(Atts, 'form');
  if C.Form <> '' then CheckActionComponentForm(LocalName, C.Form);
  C.Items := XmlToStr(GetStr(Atts, 'items'));
  C.Width := GetInt(Atts, 'width');
  C.Height := GetInt(Atts, 'height');
  C.Required := GetBool(Atts, 'required');
  C.DefaultValue := XmlToStr(GetStr(Atts, 'defaultvalue'));
  C.NoForm := GetBool(Atts, 'noform');
  C.Editing := GetBool(Atts, 'editing');
  C.TextHint:=XmlToStr(GetStr(Atts, 'texthint'));
  FNames.Add(C.Name);
end;

procedure TUIReader.DoEndElement(const NamespaceURI, LocalName, QName: SAXString
  );
var
  Nm: String;
begin
  inherited DoEndElement(NamespaceURI, LocalName, QName);
  Nm := AnsiLowerCase(LocalName);
  if Nm = 'grid' then FEAControl := nil
  else if Nm = 'ifgrid' then FGridChk := nil
end;

constructor TUIReader.Create;
begin
  FNames := TStringList.Create;
end;

destructor TUIReader.Destroy;
begin
  FNames.Free;
  inherited Destroy;
end;

procedure TUIReader.ParseXml(const Xml: String);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(Xml);
  try
	  ParseStream(SS);
  finally
    SS.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure CompilerBindVars(Cl: TScriptCompiler);
var
  Fm, Tbl: TdxForm;
  i: Integer;
  C: TdxComponent;
  SD: TScriptData;
  FormMan: TFormManager;
begin
  SD := Cl.SD;
  Cl.AddUsedVariableN('Session', 'TSession');
  if SD.Kind = skWebMain then Exit;
  Cl.AddUsedVariableN('Self', 'TdxForm');
  if SD.Kind = skWebForm then
  begin
    FormMan := TMetaData(Cl.FMetaData).FormMan;
    Fm := FormMan.FindForm(SD.FmId);
    Cl.AddUsedVariableN(Fm.Name, Fm.ClassName);
    for i := 0 to Fm.ComponentCount - 1 do
    begin
      C := Fm.Components[i];
      if (C.Name = '') or not (C is TdxControl) then Continue;
      Cl.AddUsedVariableN(C.Name, C.ClassName);
      if C is TdxGrid then
      begin
        Tbl := FormMan.FindForm(TdxGrid(C).Id);
        Cl.AddUsedVariableN(Tbl.Name, Tbl.ClassName);
      end;
    end;
  end;
end;

function CompilerUses(Sender: TPSPascalCompiler; const aName: tbtString
  ): Boolean;
begin
  Result := True;
  if aName = 'SYSTEM' then
  begin
    SIRegister_All(Sender);
    CompilerBindVars(TScriptCompiler(Sender));
  end;
end;

{ TScriptCompiler }

function NeedFile(Sender: TPSPreProcessor; const callingfilename: tbtstring;
  var FileName, Output: tbtstring): Boolean;
var
  SD: TScriptData;
  Compiler: TScriptCompiler;
  i: Integer;
  ScriptMan: TScriptManager;
begin
  Compiler := TScriptCompiler(Sender.Id);
  ScriptMan := TMetaData(Compiler.FMetaData).ScriptMan;
  i := Compiler.Includes.IndexOf(FileName);
  if i < 0 then
  begin
    Compiler.Includes.Add(FileName);
    Compiler.Callers.Add(callingfilename);
    SD := ScriptMan.FindScriptByName(FileName);
    Result := SD <> nil;
    if Result then
  	  Output := SD.Source;
  end
  else
  begin
    raise EPSPreProcessor.CreateFmt('Module "%s" already included in "%s"',
      [FileName, Compiler.Callers[i]]);
  end;
end;

procedure UnknownDirective(Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser; const Active: Boolean;
  const DirectiveName, DirectiveParam: tbtString; var Continue: Boolean);
begin
  Continue := True;
end;

{ TExprActions }

function TExprActions.GetActions(Index: Integer): TExprAction;
begin
  Result := TExprAction(Items[Index]);
end;

function TExprActions.AddAction: TExprAction;
begin
	Result := TExprAction.Create;
  Add(Result);
end;

function TExprActions.FindAction(const Id: String): TExprAction;
var
  i: Integer;
  A: TExprAction;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    A := Actions[i];
    if A.Id = Id then Exit(A);
  end;
end;

function TExprActions.ActionExists(Group, Name: String): Boolean;
var
  i: Integer;
  A: TExprAction;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    A := Actions[i];
    if (MyUtf8CompareText(A.Group, Group) = 0) and
    	(MyUtf8CompareText(A.Name, Name) = 0) then Exit(True);
  end;
end;

procedure TExprActions.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  	Actions[i].Free;
  inherited Clear;
end;

{ TSourceMarkData }

constructor TSourceMarkData.Create;
begin
  FBookmarkNumber:=-1;
end;

function TSourceMarkData.IsBookmark: Boolean;
begin
  Result := FBookmarkNumber >= 0;
end;

{ TSourceMarks }

function TSourceMarks.GetMarks(Index: Integer): TSourceMarkData;
begin
  Result := TSourceMarkData(Items[Index]);
end;

function TSourceMarks.AddMark: TSourceMarkData;
begin
  Result := TSourceMarkData.Create;
  Add(Result);
end;

procedure TSourceMarks.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  	Marks[i].Free;
  inherited Clear;
end;

procedure TSourceMarks.ClearBreakpoints;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if not Marks[i].IsBookmark then
    begin
      Marks[i].Free;
      Delete(i);
    end;
end;

function TSourceMarks.HasBreakpoints: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  	if not Marks[i].IsBookmark then Exit(True);
end;

function TSourceMarks.FindBreakpoint(aRow: Integer): TSourceMarkData;
var
  i: Integer;
  M: TSourceMarkData;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    M := Marks[i];
    if (M.Row = aRow) and (M.IsBookmark = False) then Exit(M);
  end;
end;

procedure TSourceMarks.DeleteBreakpoint(Mark: TSourceMarkData);
begin
  Remove(Mark);
  Mark.Free;
end;

{ TScriptSourceData }

procedure TScriptSourceData.XmlStartElement(Sender: TObject;
  const NamespaceURI, LocalName, QName: SAXString; Atts: TSAXAttributes);
var
  M: TSourceMarkData;
begin
  if Atts = nil then Exit;
	if LocalName = 'sourcedata' then
  begin
		FCaretX := FXml.GetInt(Atts, 'caretx');
    FCaretY := FXml.GetInt(Atts, 'carety');
    FTopLine := FXml.GetInt(Atts, 'topline');
    FLeftChar := FXml.GetInt(Atts, 'leftchar');
  	FFoldState := FXml.GetStr(Atts, 'foldstate');
  end
  else if LocalName = 'mark' then
  begin
    M := FMarks.AddMark;
    M.BookmarkNumber := FXml.GetInt(Atts, 'bm');
    M.Column := FXml.GetInt(Atts, 'column');
    M.Row := FXml.GetInt(Atts, 'line');
  end;
end;

constructor TScriptSourceData.Create;
begin
  FMarks := TSourceMarks.Create;
end;

destructor TScriptSourceData.Destroy;
begin
  FMarks.Free;
  inherited Destroy;
end;

function TScriptSourceData.SaveToString: String;
var
  i: Integer;
  M: TSourceMarkData;
begin
  Result := '<sourcedata caretx="' + IntToStr(FCaretX) +
  	'" carety="' + IntToStr(FCaretY) +
    '" topline="' + IntToStr(FTopLine) +
    '" leftchar=" ' + IntToStr(FLeftChar) +
    '" foldstate="' + FFoldState + '"><marks>';
  for i := 0 to FMarks.Count - 1 do
  begin
    M := FMarks[i];
    Result := Result + '<mark bm="' + IntToStr(M.BookmarkNumber) +
    	'" column="' + IntToStr(M.Column) +
      '" line="' + IntToStr(M.Row) + '" />';
  end;
  Result := Result + '</marks></sourcedata>'
end;

procedure TScriptSourceData.LoadFromString(const Buf: String);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create(Buf);
  FXml := TSAXBaseReader.Create;
  FXml.OnStartElement:=@XmlStartElement;
  try
  	FXml.ParseStream(SS);
  finally
	  FXml.Free;
    SS.Free;
  end;
end;

procedure TScriptSourceData.SaveToFile(const FileName: String);
var
  Buf: String;
begin
  Buf := SaveToString;
  with TFileStream.Create(FileName, fmCreate) do
  try
    Write(Pointer(Buf)^, Length(Buf));
  finally
    Free;
  end;
end;

procedure TScriptSourceData.LoadFromFile(const FileName: String);
var
  Buf: String;
begin
	with TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone) do
  try
  	SetLength(Buf, Size);
    Read(Pointer(Buf)^, Size);
  finally
    Free;
  end;
  LoadFromString(Buf);
end;

{ EModuleParserError }

constructor EModuleParserError.Create(const Msg: String; aPos: Integer);
begin
  inherited Create(Msg);
  FPos := aPos;
end;

{ TModuleParser }

procedure TModuleParser.SkipWhites;
begin
  while (FPos <= FLen) and (FBuf[FPos] <= #32) do
  	Inc(FPos);
end;

function TModuleParser.Exists(const S: String): Boolean;
var
  Len: Integer;
begin
  SkipWhites;
  Len := Length(S);
  Result := LowerCase(Copy(FBuf, FPos, Len)) = S;
end;

procedure TModuleParser.Consume(const S: String);
var
  Len: Integer;
begin
	SkipWhites;
  Len := Length(S);
  if LowerCase(Copy(FBuf, FPos, Len)) <> S then
  	DoError('%s expected', [S]);
  FPos := FPos + Len;
end;

(*procedure TModuleParser.FindComment(const Str: String);
var
  P: Integer;
  S: String;
begin
  while FPos <= FLen do
  begin
    P := PosEx('{@', FBuf, FPos);
    if P = 0 then
    begin
    	FPos := FLen + 1;
      Break;
    end
    else FPos := P + 2;
    P := FPos;
    while (FPos <= FLen) and (FBuf[FPos] in ['A'..'Z', 'a'..'z']) do
      Inc(FPos);
    S := Copy(FBuf, P, FPos - P);
    if LowerCase(S) = Str then
      Break;
  end;
end;     *)

function TModuleParser.GetComment: String;
var
  P: Integer;
begin
  Result := '';
  P := PosEx('{@', FBuf, FPos);
  if P = 0 then
  begin
    FPos := FLen + 1;
    Exit;
  end
  else FPos := P + 2;
  P := FPos;
  while (FPos <= FLen) and (FBuf[FPos] in ['A'..'Z', 'a'..'z']) do
    Inc(FPos);
  Result := Copy(FBuf, P, FPos - P);
end;

function TModuleParser.GetStr: String;
var
  P: Integer;
begin
  P := FPos;
  while (FPos <= FLen) and (not (FBuf[FPos] in [#10, #13])) do
    Inc(FPos);
  Result := Trim(Copy(FBuf, P, FPos - P));
end;

function TModuleParser.GetMultiStr(const EndChars: String): String;
var
  P, L: Integer;
begin
  P := FPos;
  L := Length(EndChars);
  while (FPos <= FLen) and (AnsiLowerCase(Copy(FBuf, FPos, L)) <> EndChars) do
    Inc(FPos);
  Result := Copy(FBuf, P, FPos - P);
  //Consume(EndChars);
end;

procedure TModuleParser.DoError(const Msg: String; Params: array of const);
begin
  raise EModuleParserError.Create('[Error] ' + Format(Msg, Params), FPos);
end;

function ReplaceGtChars(const Str: String): String;
var
  p, oldp, L: Integer;
  InQuote: Boolean;
begin
  Result := '';
  p := 1;
  oldp := 1;
  L := Length(Str);
  InQuote := False;
  while p <= L do
  begin
    case Str[p] of
      '"': InQuote := not InQuote;
      '>':
        if InQuote then
        begin
          Result := Result + Copy(Str, oldp, p - oldp) + '&gt;';
          oldp := p + 1;
        end;
    end;
    Inc(p);
  end;
  Result := Result + Copy(Str, oldp, p - oldp);
end;

procedure TModuleParser.ParseAction;
var
  Nm, OrigName, UI, Grp, Id, Tr: String;
  A: TExprAction;
  UIR: TUIReader;
  at: TActionTarget;
begin
  Consume('id');
  Consume('=');
  Id := GetStr;

  if Trim(Id) = '' then
    DoError('Undefined ID.', [])
  else if FWebExtMode then
  begin
    A := FActions.FindAction(Id);
    if A = nil then DoError('Action with ID %s not found. Please check extension module exists.', [Id])
    else
    begin
      if A.WebExists then DoError('Duplicate action ID: %s', [Id])
      else
      begin
        A.WebExists := True;
        A.SDi := FSDi;          // Подмениваем на веб-модуль
      end;
    end;
    Consume('@}');
    Exit;
  end
  else if FActions.FindAction(Id) <> nil then
  	DoError('Duplicate action ID: %s', [Id]);

  at := atAll;
  if Exists('target') then
  begin
    Consume('target');
    Consume('=');
    Tr := LowerCase(GetStr);
    if Tr = 'form' then at := atForm
    else if Tr = 'button' then at := atButton
    else if Tr = 'main' then at := atMain
    else if Tr = 'all' then at := atAll
    else if Trim(Tr) = '' then DoError('Undefined target.', [])
    else DoError('Unknown target. Valid values are: form, button, main, all.', []);
  end;

  Consume('origname');
  Consume('=');
  OrigName := UpperCase(GetStr);
  if Trim(OrigName) = '' then DoError('Undefined origname.', []);
  Consume('name');
  Consume('=');
  Nm := GetStr;
  if Trim(Nm) = '' then DoError('Undefined name.', []);
  Consume('group');
  Consume('=');
  Grp := GetStr;
  if Trim(Grp) = '' then DoError('Undefined group.', []);

  if FActions.ActionExists(Grp, Nm) then
  	DoError('The action with the name [%s] already exists in the group [%s]', [Nm, Grp]);

  A := FActions.AddAction;
  A.Id := Id;
  A.Target := at;
  A.OrigName := OrigName;
  A.SDi := FSDi;
  A.Name := Nm;
  A.Group := Grp;

  //Consume('glyph');
  //Consume('=');
  //A.Glyph := GetStr;
  Consume('ui');
  Consume('=');
  UI := GetMultiStr('description=');
  UI := ReplaceGtChars(UI);

  UIR := TUIReader.Create;
  UIR.Action := A;
  try try
	  UIR.ParseXml(UI);
  except
    on E: Exception do
    	DoError(E.Message, []);
  end;
  finally
    UIR.Free;
  end;

  Consume('description');
  Consume('=');
  A.Description:=GetMultiStr('@}');
  Consume('@}');
end;

function CheckFuncArgs(const S: String): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(S) do
    if not (S[i] in ['s', 'n', 'b', 'd', 't', 'v']) then Exit(False);
end;

procedure TModuleParser.ParseFunction;
var
  F: TExprFunc;
  S, OrigName, Nm: String;
begin
  if not FWebExtMode then
  begin
    Consume('origname');
    Consume('=');
    OrigName := UpperCase(GetStr);
    if Trim(OrigName) = '' then DoError('Undefined origname.', []);
  end;
  Consume('name');
  Consume('=');
  Nm := UpperCase(GetStr);
  if Trim(Nm) = '' then DoError('Undefined name.', []);

  if FWebExtMode then
  begin
    F := FFuncs.FindFunc(Nm);
    if F = nil then DoError('Function %s not found. Please check extensions module exists.', [Nm])
    else
    begin
      if F.WebExists then DoError('Duplicate function name: %s', [Nm])
      else
      begin
        F.WebExists := True;
        F.SDi := FSDi;            // Подмениваем на веб-модуль
      end;
    end;
    Consume('@}');
    Exit;
  end
  else if FFuncs.FindFunc(Nm) <> nil then
  	DoError('Duplicate function name: %s', [Nm]);

  F := FFuncs.AddFunc;
  F.OrigName := OrigName;
  F.SDi := FSDi;
  F.Name := Nm;

  Consume('args');
  Consume('=');
  F.Args:=LowerCase(GetStr);
  if not CheckFuncArgs(F.Args) then DoError('Function arguments may be folowing types: s, n, b, d, t, v.', []);
  Consume('result');
  Consume('=');
  S := LowerCase(GetStr);
  {if Trim(S) = '' then DoError('Undefined function result.', [])
  else }if Length(Trim(S)) > 1 then DoError('Function result must be one char.', [])
  else if not CheckFuncArgs(S) then DoError('Function result may be folowing types: s, n, b, d, t, v.', [])
  else if Trim(S) <> '' then F.ResultType:=S[1]
  else F.ResultType := #0;
  {if S <> '' then
	  F.ResultType:=S[1]
  else
    F.ResultType := #0;}
  Consume('group');
  Consume('=');
  F.Group:=GetStr;
  if Trim(F.Group) = '' then DoError('Undefined group.', []);
  Consume('description');
  Consume('=');
  F.Description:=GetMultiStr('@}');
  Consume('@}');
end;

procedure TModuleParser.ParseHead;
begin
  Consume('author');
  Consume('=');
  FAuthor := GetStr;
  Consume('version');
  Consume('=');
  FVersion := GetStr;
  if Exists('homepage') then
  begin
    Consume('homepage');
    Consume('=');
    FHomePage := GetStr;
  end
  else
    FHomePage := '';
  Consume('description');
  Consume('=');
  FDescription := GetMultiStr('@}');
  Consume('@}');
end;

procedure TModuleParser.Parse(const Buf: String; OnlyHead: Boolean; SDi: Integer
  );
var
  S: String;
begin
  FSDi := SDi;
  FAuthor := ''; FVersion := ''; FDescription := '';
  FBuf := Buf; FPos := 1; FLen := Length(FBuf);
  while FPos <= FLen do
  begin
    S := LowerCase(GetComment);
    if (not OnlyHead) and (S = 'function') then ParseFunction
    else if (not OnlyHead) and (S = 'action') then ParseAction
    else if S = 'module' then
    begin
      ParseHead;
      if OnlyHead then Break;
    end;
  end;
end;

(*procedure TModuleParser.Parse(const Buf: String; Funcs: TExprFuncs);
var
  F: TExprFunc;
  S: String;
begin
  FBuf := Buf; FPos := 1; FLen := Length(FBuf);
  while FPos <= FLen do
  begin
    FindComment('function');
    if FPos > FLen then Break;
    F := Funcs.AddFunc;
    Consume('origname');
    Consume('=');
    F.OrigName := UpperCase(GetStr);
    Consume('name');
    Consume('=');
    F.Name:=UpperCase(GetStr);
    Consume('args');
    Consume('=');
    F.Args:=LowerCase(GetStr);
    Consume('result');
    Consume('=');
    S := LowerCase(GetStr);
    if S <> '' then
	    F.ResultType:=S[1]
    else
    	F.ResultType := #0;
    Consume('group');
    Consume('=');
    F.Group:=GetStr;
    Consume('description');
    Consume('=');
    F.Description:=GetMultiStr;
  end;
end;

procedure TModuleParser.ParseHeadModule(const Buf: String; var Author, Version,
  Description: String);
begin
  FPos := 1;
  FLen := Length(Buf);
  FBuf := Buf;
  Author := '';
  Version := '';
  Description:='';
  FindComment('module');
  if FPos < FLen then
  begin
    try
  	  Consume('author');
      Consume('=');
      Author := GetStr;
      Consume('version');
      Consume('=');
      Version := GetStr;
      Consume('description');
      Consume('=');
      Description := GetMultiStr;
    except
      on E: EModuleParserError do ;
    end;
  end;
end;   *)

{ TExprFuncs }

function TExprFuncs.GetFuncs(Index: Integer): TExprFunc;
begin
  Result := TExprFunc(Items[Index]);
end;

function TExprFuncs.AddFunc: TExprFunc;
begin
	Result := TExprFunc.Create;
  Add(Result);
end;

function TExprFuncs.FindFunc(const Name: String): TExprFunc;
var
  i: Integer;
  F: TExprFunc;
begin
  Result := nil;
	for i := 0 to Count - 1 do
  begin
    F := Funcs[i];
    if F.Name = Name then Exit(F);
  end;
end;

function TExprFuncs.FindFuncIndex(const Name: String): Integer;
var
  i: Integer;
  F: TExprFunc;
begin
  Result := -1;
	for i := 0 to Count - 1 do
  begin
    F := Funcs[i];
    if F.Name = Name then Exit(i);
  end;
end;

procedure TExprFuncs.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
		Funcs[i].Free;
  inherited Clear;
end;

// Собираем данные о позиции каждой строки исходника.
procedure GetSourceLineCount(const Source: String; L: TCardinalList);
var
  p, Len: Integer;
begin
  p := 1; Len := Length(Source);
  L.AddValue(0);
  while p <= Len do
  begin
    if Source[p] = #13 then
    begin
      if (p < Len) and (Source[p+1] = #10) then
      	Inc(p);
      L.AddValue(p);
    end
    else if Source[p] = #10 then
    	L.AddValue(p);
    Inc(p);
  end;
end;

// По позициям строк находим текущую строку
function GetCurrentLine(L: TCardinalList; LineOffset: Cardinal): Integer;
var
  i: Integer;
  V: Cardinal;
begin
  Result := -1;
  for i := 0 to L.Count - 1 do
  begin
    V := L[i];
    if LineOffset < V then Exit(i-1)
    else if LineOffset = V then Exit(i);
  end;
end;

procedure TScriptCompiler.AddLines(const Source: String);
var
  i, j, Line: Integer;
  LI: TPSLineInfo;
  SD1: TScriptData;
  LD: TLineData;
  L: TCardinalList;
  ScriptMan: TScriptManager;
begin
  ScriptMan := TMetaData(FMetaData).ScriptMan;
  L := TCardinalList.Create;
  GetSourceLineCount(Source, L);
  for i := FPre.CurrentLineInfo.Count - 1 downto 0 do
  begin
    LI := FPre.CurrentLineInfo[i];
    if LI.FileName = '' then SD1 := FSD
    else SD1 := ScriptMan.FindScriptByName(LI.FileName);
    for j := 0 to LI.LineOffsetCount - 1 do
    begin
      Line := GetCurrentLine(L, LI.StartPos + LI.LineOffset[j]);
      LD := SD.AddLine(Line);
      if LD <> nil then
      begin
        LD.SD := SD1;
        LD.Breakpoint := SD1.SourceData.Marks.FindBreakpoint(j) <> nil;
        LD.Line := j;
      end;
    end;
  end;
  L.Free;
end;

function TScriptCompiler.RegProcExists(const ProcName: String): Boolean;
var
  i: Integer;
  RP: TPSRegProc;
begin
  Result := False;
  for i := 0 to GetRegProcCount - 1 do
	begin
    RP := GetRegProc(i);
    if RP.Name = ProcName then Exit(True);
  end;
end;

procedure TScriptCompiler.FunctionStartHandler(name: tbtString; Pos, Row,
  Col: Integer);
begin
  name := UpperCase(name);
  if RegProcExists(name) then
  	MakeError('', ecCustomError, Format('A registered function named ''%s'' already exists.', [name]));
end;

constructor TScriptCompiler.Create(AMetaData: TObject);
begin
  inherited Create;
  FMetaData := AMetaData;
  BooleanShortCircuit:=True;
  AllowNoBegin:=True;
  AllowNoEnd:=True;
  AllowDuplicateRegister:=False;
  OnUses:=@CompilerUses;
  OnFunctionStart:=@FunctionStartHandler;
  FPre := TPSPreprocessor.Create;
  FPre.Id := Self;
  FPre.OnProcessUnknowDirective:=@UnknownDirective;
  FPre.OnNeedFile:=@NeedFile;
  FIncludes := TStringListUtf8.Create;
  FCallers := TStringListUtf8.Create;
end;

destructor TScriptCompiler.Destroy;
begin
  FCallers.Free;
  FIncludes.Free;
  FPre.Free;
  inherited Destroy;
end;

function TScriptCompiler.Compile(const Source: String): Boolean;
var
  S: String;
begin
  FIncludes.Clear;
  FCallers.Clear;
  FPre.Clear;
  {$ifdef windows}
  FPre.Defines.Add('WINDOWS');
  {$else}
  FPre.Defines.Add('LINUX');
  {$endif}
  FPre.MainFile:=Source;
  S := '';
  FPre.PreProcess('', S);
  Result := inherited Compile(S);
  FPre.AdjustMessages(Self);
  AddLines(S);
end;

{ TRunScript }

constructor TRunScript.Create(SS: TObject);
begin
  FSS := SS;
  FExec := TPSDebugExec.Create;
  FImporter := TPSRuntimeClassImporter.Create;
  RegisterClassLibraryRuntime(FExec, FImporter);
  RIRegister_All(FImporter);
  RegisterDateTimeLibrary_R(FExec);
  RIRegister_Functions(FExec);
  RegisterDllRuntime(FExec);
end;

destructor TRunScript.Destroy;
begin
  FImporter.Free;
  FExec.Free;
  inherited Destroy;
end;

function TRunScript.LoadBin: Boolean;
begin
  if SD.Bin = '' then Exit(False);

  FExec.Id:=Self;
  FExec.OnException := @ExecExceptionLoadData;
	Result := FExec.LoadData(SD.Bin);
  if not Result then
  begin
    LogString('Load bin failed.');
    if SD.Form <> nil then LogString(SD.Form.FormCaption)
    else LogString(SD.Name);
    Exit;
  end;
  FExec.OnException:=@ExecException;
  if SD.HasBreakpoints then
  begin
		FExec.DebugEnabled:=True;
		FExec.LoadDebugData(SD.DebugData);
  end;
end;

function TRunScript.TryRunProc(const ProcName: String; Args: array of Variant
  ): Boolean;
var
  ProcNo: Cardinal;
begin
  Result := True;
  ProcNo := FExec.GetProc(ProcName);
  if ProcNo <> InvalidVal then
  begin
    FExec.RunProcP(Args, ProcNo);
    FExec.RaiseCurrentException;
  end
  else Result := False;
end;

function TRunScript.TryRunFunc(const FuncName: String; Args: array of Variant;
  var aResult: Variant): Boolean;
var
  ProcNo: Cardinal;
begin
  Result := True;
  ProcNo := FExec.GetProc(FuncName);
  if ProcNo <> InvalidVal then
  begin
    aResult := FExec.RunProcP(Args, ProcNo);
    FExec.RaiseCurrentException;
  end
  else Result := False;
end;

procedure TRunScript.BindForm(ARecordSet: TObject);
var
  i: Integer;
  C: TdxComponent;
  Fm: TdxForm;
  FmList: TSsRecordSets;
begin
  FRS := ARecordSet;
  Fm := TSsRecordSet(FRS).Form;
  SetVariantToClass(FExec.GetVar2('Self'), Fm);
  SetVariantToClass(FExec.GetVar2(Fm.Name), Fm);
  for i := 0 to Fm.ComponentCount - 1 do
  begin
    C := Fm.Components[i];
    if (C.Name = '') or not (C is TdxControl) then Continue;
    SetVariantToClass(FExec.GetVar2(C.Name), C);
  end;
  if Fm.PId = 0 then
  begin
    FmList := TSsRecordSet(FRS).Forms;
    for i := 0 to FmList.Count - 1 do
      SetVariantToClass(FExec.GetVar2(FmList[i].Form.Name), FmList[i].Form);
  end;
end;

procedure TRunScript.BindVars;
begin
  {SetVariantToClass(FExec.GetVar2('Application'), Application);}
  SetVariantToClass(FExec.GetVar2('Session'), FSS);
  if FSD.Kind = skWebExpr then
    SetVariantToClass(FExec.GetVar2('Self'), nil);
end;

{ TScriptData }

function TScriptData.GetMsgs(Index: Integer): TCompilerMsg;
begin
  Result := TCompilerMsg(FMsgs[Index]);
end;

function TScriptData.GetLines(Index: Integer): TLineData;
begin
  Result := TLineData(FLines[Index]);
end;

constructor TScriptData.Create;
begin
  FMsgs := TList.Create;
  FLines := TList.Create;
  FSourceData := TScriptSourceData.Create;
end;

destructor TScriptData.Destroy;
begin
  FSourceData.Free;
  ClearList(FLines);
  FLines.Free;
  ClearList(FMsgs);
  FMsgs.Free;
  inherited Destroy;
end;

function TScriptData.GetModuleName: String;
begin
  if Kind = skMain then Result := 'Main'
  else if Kind = skWebMain then Result := 'WebMain'
  else if Kind = skWebForm then Result := Form.FormCaption
  else Result := Name;
end;

function TScriptData.AddMsg: TCompilerMsg;
begin
  Result := TCompilerMsg.Create;
  FMsgs.Add(Result);
end;

function TScriptData.MsgCount: Integer;
begin
  Result := FMsgs.Count;
end;

procedure TScriptData.Clear;
begin
  Bin := '';
  DebugData := '';
  ClearList(FMsgs);
  ClearList(FLines);
end;

function TScriptData.AddLine(Line: Integer): TLineData;
var
  i: Integer;
  LD: TLineData;
begin
  Result := TLineData.Create;

  Result.AbsLine := Line;
  for i := 0 to LineCount - 1 do
  begin
    LD := Lines[i];
    if LD.AbsLine > Line then
    begin
      FLines.Insert(i, Result);
      Exit;
    end
    else if LD.AbsLine = Line then
    begin
      FreeAndNil(Result);
      Exit;
    end;
  end;

  FLines.Add(Result);
end;

function TScriptData.LineCount: Integer;
begin
  Result := FLines.Count;
end;

function TScriptData.HasBreakpoints: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to LineCount - 1 do
  	if Lines[i].Breakpoint then Exit(True);
end;

{ TScriptManager }

function TScriptManager.GetScripts(Index: Integer): TScriptData;
begin
  Result := TScriptData(FScripts[Index]);
end;

function TScriptManager.AddScript(FmId: Integer; const aName, Source: String
  ): TScriptData;
begin
  Result := TScriptData.Create;
  Result.FmId := FmId;
  Result.Name := aName;
  Result.Source := Source;
  FScripts.Add(Result);
end;

constructor TScriptManager.Create(AMetaData: TObject);
begin
  FMetaData := AMetaData;
  FScripts := TList.Create;
  //FExprModule := TScriptData.Create;
  //FExprModule.Kind := skExpr;
  FCompiler := TScriptCompiler.Create(AMetaData);
  FFuncs := TExprFuncs.Create;
  FActions := TExprActions.Create;
  //FNeedUpdateFuncs:=True;
  FUseDebugInfo := True;
end;

destructor TScriptManager.Destroy;
begin
  FActions.Free;
  FFuncs.Free;
  FCompiler.Free;
  //FExprModule.Free;
  ClearList(FScripts);
  FScripts.Free;
  inherited Destroy;
end;

procedure TScriptManager.ModulesToList(SL: TStrings; Kind: TScriptKind);
var
  i: Integer;
  SD: TScriptData;
begin
  SL.Clear;
  for i := 0 to ScriptCount - 1 do
  begin
    SD := Scripts[i];
    if SD.Kind = Kind then SL.AddObject(SD.Name, SD);
  end;
end;

procedure TScriptManager.ParseExprModules;
var
  i: Integer;
begin
  FFuncs.Clear;
  FActions.Clear;
  for i := 0 to ScriptCount - 1 do
    if Scripts[i].Kind = skExpr then
      ParseExprModule(Scripts[i]);
  for i := 0 to ScriptCount - 1 do
    if Scripts[i].Kind = skWebExpr then
      ParseExprModule(Scripts[i]);
end;

function TScriptManager.FindScript(FmId: Integer): TScriptData;
var
  i: Integer;
  SD: TScriptData;
begin
  Result := nil;
  for i := 0 to FScripts.Count - 1 do
  begin
    SD := GetScripts(i);
    if SD.FmId = FmId then Exit(SD);
  end;
end;

function TScriptManager.FindScriptByName(const aName: String): TScriptData;
var
  i: Integer;
  SD: TScriptData;
begin
  Result := nil;
  if aName = '' then Exit;
  for i := 0 to FScripts.Count - 1 do
  begin
    SD := GetScripts(i);
    if MyUtf8CompareText(aName, SD.Name) = 0 then Exit(SD);
  end;
end;

function TScriptManager.GetScriptIndex(SD: TScriptData): Integer;
begin
  Result := FScripts.IndexOf(SD);
end;

function TScriptManager.ScriptCount: Integer;
begin
  Result := FScripts.Count;
end;

procedure ExtractColRow(const S: String; var R, C: Integer);
var
  p1, p2: SizeInt;
begin
  R := 1; C := 1;
  p1 := Pos(' at ', S);
  p2 := Pos(':', S);
  if (p1 <> 0) and (p2 > p1) then
  begin
    p1 := p1 + 4;
    TryStrToInt(Copy(S, p1, p2 - p1), R);
    TryStrToInt(Copy(S, p2 + 1, 255), C);
  end;
end;

procedure TScriptManager.CompileModule(SD: TScriptData);
var
  CMsg: TCompilerMsg;
  j: Integer;
  Msg: TPSPascalCompilerMessage;
begin
  SD.Clear;
  FCompiler.Clear;
  FCompiler.SD := SD;

  try

  if FCompiler.Compile(SD.Source) then
  begin
    if FCompiler.GetOutput(SD.Bin) = False then
    begin
      CMsg := SD.AddMsg;
      CMsg.Msg := 'Unsuccessfull compile';
      CMsg.SD := SD;
      CMsg.ErrorType := 'Error';
    end
    else
    	FCompiler.GetDebugOutput(SD.DebugData);
    //if SD.Source <> '' then ShowMessage(IntToStr(Length(SD.Bin)));
  end;

  for j := 0 to FCompiler.MsgCount - 1 do
  begin
    Msg := FCompiler.Msg[j];
    CMsg := SD.AddMsg;
    CMsg.Col:=Msg.Col;
    CMsg.Row := Msg.Row;
    CMsg.Msg:= Msg.MessageToString;
    CMsg.ErrorType:=Msg.ErrorType;
    CMsg.ModuleName:=Msg.ModuleName;
    CMsg.SD:=SD;
  end;

  except
    //on E: EPSPreProcessor do
    on E: Exception do
    begin
      CMsg := SD.AddMsg;
      CMsg.Msg := E.Message;
      CMsg.SD := SD;
      CMsg.ErrorType := 'Error';
      ExtractColRow(E.Message, CMsg.Row, CMsg.Col);
    end;
  end;
end;

procedure TScriptManager.CompileAll;
var
  i: Integer;
  SD: TScriptData;
begin
  for i := 0 to FScripts.Count - 1 do
  begin
    SD := GetScripts(i);
    if SD.Kind in [skWebMain, skWebForm] then CompileModule(SD);
  end;
  CompileExpr;
end;

{procedure TScriptManager.CompileMain;
var
  SD: TScriptData;
begin
  SD := FindScriptByName('Main');
  CompileModule(SD);
end;    }

procedure TScriptManager.CompileExpr;
var
  i: Integer;
  SD: TScriptData;
begin
  for i := 0 to FScripts.Count - 1 do
  begin
    SD := GetScripts(i);
    if SD.Kind = skWebExpr then CompileModule(SD);
  end;
  ParseExprModules;
  //FNeedUpdateFuncs:=True;
end;

procedure TScriptManager.ModuleMessagesToList(SD: TScriptData; L: TStrings;
  OnlyErrors: Boolean);
var
  S: String;
  j: Integer;
  Msg: TCompilerMsg;
begin
  S := '';
  if SD.Name <> '' then
    S := SD.Name + ': ';
  for j := 0 to SD.MsgCount - 1 do
  begin
    Msg := SD.Msgs[j];
    if OnlyErrors and (Msg.ErrorType <> 'Error') then Continue;
    L.AddObject(S + Msg.Msg, Msg);
  end;
end;

procedure TScriptManager.MessagesToList(L: TStrings; OnlyErrors: Boolean);
var
  i: Integer;
begin
  L.Clear;
  for i := 0 to FScripts.Count - 1 do
    ModuleMessagesToList(GetScripts(i), L, OnlyErrors);
end;

function TScriptManager.HasErrorsInModule(SD: TScriptData): Boolean;
var
  j: Integer;
  Msg: TCompilerMsg;
begin
  Result := False;
  for j := 0 to SD.MsgCount - 1 do
  begin
    Msg := SD.Msgs[j];
    if Msg.ErrorType = 'Error' then Exit(True);
  end;
end;

function TScriptManager.HasErrors: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to ScriptCount - 1 do
    if HasErrorsInModule(Scripts[i]) then Exit(True);
end;

function TScriptManager.MakeUniqueScriptName(const AnyName: String): String;
var
  S: String;
  n: Integer;
begin
  SplitComponentName(AnyName, S, n);
  Inc(n);
  while FindScriptByName(S + IntToStr(n)) <> nil do
    Inc(n);
  Result := S + IntToStr(n);
end;

procedure TScriptManager.ParseExprModule(SD: TScriptData);
var
  P: TModuleParser;
  Msg: TCompilerMsg;
  SDi: Integer;
begin
  P := TModuleParser.Create;
  P.Funcs := FFuncs;
  P.Actions := FActions;
  P.WebExtMode := SD.Kind = skWebExpr;
  SDi := GetScriptIndex(SD);
  try try
  	P.Parse(SD.Source, False, SDi);
    SD.FAuthor := P.Author;
    SD.FVersion := P.Version;
    SD.FHomePage := P.HomePage;
    SD.FDescription := P.Description;
  except
    on E: EModuleParserError do
    begin
      Msg := SD.AddMsg;
      Msg.ErrorType:='Error';
      Msg.ModuleName:=SD.Name;
      Msg.SD := SD;
      Msg.Msg:=E.Message;
      Msg.Pos := E.Position;
    end;
  end;
  finally
    P.Free;
  end;
end;

end.

