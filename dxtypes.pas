{-------------------------------------------------------------------------------

    Copyright 2016-2026 Pavel Duborkin ( mydataexpress@mail.ru )

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

unit DxTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Db, SqlDb, formmanager, reportmanager, dxctrls, dxusers,
  DateUtils, Math, dxreports, MemDs, Variants, strconsts, dbengine, dxmains,
  scriptmanager, appsettings, mytypes, uPSRuntime, dxsqlquery, fpjson,
  fphttpserver, LazFileUtils;

type

  TPeriodType = (ptNone, ptToday, ptThisWeek, ptThisMonth, ptThisYear);

  TSession = class;
  TSsRecordSets = class;

  TSsRecordSetState = (rstInsert, rstPost, rstPrint, rstKeepPos, rstRecalculate,
    rstDuplicate);
  TSsRecordSetStates = set of TSsRecordSetState;

  { TSsRecordSet }

  TSsRecordSet = class
  private
    //FActions: TObject;
    FCallerObj: TdxLookupComboBox;
    FCallerRS: TSsRecordSet;
    FChangedFields: TUniList;
    //FChangedLabels: TUniList;
    FChangedQueries: TUniList;
    FChangedProps: TUniPropList;
    FChangedTables: TUniList;
    FChangedTimers: TUniList;
    FFreshValue: Integer;
    FDeleting: TAccessStatus;
    FDocUrl: String;
    FEditing: TAccessStatus;
    FFields, FExprs, FCalcLabels: TList;
    FGotoOption: TGotoOption;
    FGotoUrl: String;
    FMemDS: TdxMemDataSet;
    FDataSet: TSQLQuery;
    FForm: TdxForm;
    FForms: TSsRecordSets;
    FNeedRequery: Boolean;
    FOldState: TDataSetState;
    FParent: TSsRecordset;
    FPrintErrors: String;
    FQGrid: TdxQueryGrid;
    FQryRecId: Integer;
    FQueries: TSsRecordSets;
    FRD: TReportData;
    FSS: TSession;
    FMaxRecId: Integer;             // Для простых форм
    FState: TSsRecordSetStates;
    FScrollEventsCounter: Integer;
    FFormAssigned, FReportAssigned: Boolean;
    FCalcCounter: Integer;
    FRunScript: TRunScript;
    FIsNewRecord: Boolean;
    FOldRecId: Integer;
    FActionList: TList;
    FFreeCallerList: TList;
    procedure ChangeObjectFields(Obj: TdxField);
    procedure ControlPropertyChange(Sender: TObject; const PropName: String);
    procedure DataSetAfterCancel(DataSet: TDataSet);
    procedure DataSetAfterDelete(DataSet: TDataSet);
    procedure DataSetAfterEdit(DataSet: TDataSet);
    procedure DataSetAfterInsert(DataSet: TDataSet);
    procedure DataSetAfterPost(DataSet: TDataSet);
    procedure DataSetAfterOpen(DataSet: TDataSet);
    procedure DataSetAfterClose(DataSet: TDataSet);
    procedure DataSetAfterScroll(ADataSet: TDataSet);
    procedure DataSetBeforeCancel(DataSet: TDataSet);
    procedure DataSetBeforeClose(DataSet: TDataSet);
    procedure DataSetBeforeDelete(DataSet: TDataSet);
    procedure DataSetBeforeEdit(DataSet: TDataSet);
    procedure DataSetBeforeInsert(DataSet: TDataSet);
    procedure DataSetBeforeOpen(DataSet: TDataSet);
    procedure DataSetBeforePost(DataSet: TDataSet);
    procedure DataSetBeforeScroll(DataSet: TDataSet);
    procedure FieldChange(Sender: TField);
    procedure InsertObjectValues(F: TdxField);
    function GetRecId: Integer;
    procedure SetCallerRS(AValue: TSsRecordSet);
    procedure SetNeedRequeryLinkedQueries(AChangedQuery: TSsRecordSet);
    procedure RequeryQueries(const AChangedField: String);
    procedure EvalAggLabels(const AFormName: String);
    procedure EvalLabels(const AChangedField: String);
    procedure EvalFields(const AChangedField: String);
    procedure ClearCalcLabels;
    procedure AddToList(L: TList; O: TObject);
    procedure SetFieldChangeEvent;
    procedure ClearFieldRequired;
    function CheckAccess(IsEdit: Boolean): Boolean;
    procedure SetQGrid(AValue: TdxQueryGrid);
    procedure UpdateAccessState;
    procedure InitRpGrid;
    procedure ParentFormSetModified;
  public
    MsgInfo: TMsgInfo;
  public
    constructor Create(SS: TSession; AParent: TSsRecordSet);
    destructor Destroy; override;
    procedure AssignForm(AValue: TdxForm);
    procedure AssignReportForm(AValue: TdxForm; RD: TReportData);
    procedure AssignReport(AValue: TReportData);
    function OpenRecordCount: Integer;
    function Open: Boolean;
    procedure OpenRecords;
    procedure OpenPage(AStart, ACount: Integer);
    procedure OpenRecord(ARecId: Integer; AUseSelCond: Boolean);
    procedure OpenDetails;
    procedure OpenObjectFields(FL: TList; ARecId: Integer);
    procedure OpenReport;
    procedure Close;
    procedure RequeryAllQueries;
    procedure ChangeField(F: TdxField; V: Variant);
    procedure RevertFieldValue(F: TdxField);
    //procedure QueryScroll(AId, ARow: Integer);
    procedure QueryApplyChangeSort(AId: Integer);
    function FindRecordSetByName(const AName: String): TSsRecordSet;
    procedure Append;
    procedure Insert;
    procedure Edit;
    procedure Post;
    procedure Cancel;
    function Delete: TAccessStatus;
    //procedure DeleteAll;
    procedure CalcAllLabels;
    function Validate: Boolean;
    function GetObjValue(F: TdxField): String;
    //function IsStartsCalcs(F: TdxField): Boolean;
    function FieldChanged(F: TdxField): Boolean;
    procedure LabelChanged(L: TdxLabel);
    procedure ClearObject(F: TdxField);
    procedure ClearImage(F: TdxField);
    procedure ClearFile(F: TdxField);
    procedure DisableScrollEvents;
    procedure EnableScrollEvents;
    function ScrollEventsEnabled: Boolean;
    function GetDSField(F: TdxField): TField;
    procedure SetDSField(F: TdxField; V: Variant);
    function GetFieldValue(const aFieldName: String): Variant;
    procedure EvalAggFields(const AChangedForm: String);
    procedure ClearChanges;
    procedure AddToHistory;
    function CheckDeleteRecord: Boolean;
    function CheckRecordModify: Byte;
    function CheckAccessDetails: Boolean;
    function CanAppend: TAccessStatus;
    function CanEdit: TAccessStatus;
    function CanDelete: TAccessStatus;
    function WhoEdit(ARecId: Integer): String;
    function MsgInfoToJson: TJsonObject;
    procedure Recalculate(ATableName, AFieldName, aExpr: String);
    procedure BeginDuplicate;
    procedure EndDuplicate;
    function GetCurrentForm: TdxForm;
    //function GetTopParent: TSsRecordSet;
    function GetFormRecordset: TSsRecordSet;
    procedure InitColoring;
    procedure DoneColoring;
    property Session: TSession read FSS;
    property Form: TdxForm read FForm write FForm;
    property QGrid: TdxQueryGrid read FQGrid write SetQGrid;
    property RD: TReportData read FRD write FRD;
    property DataSet: TSQLQuery read FDataSet write FDataSet;
    property MemDS: TdxMemDataSet read FMemDS;
    property Forms: TSsRecordSets read FForms;
    property Queries: TSsRecordSets read FQueries;
    property Parent: TSsRecordset read FParent;
    property RecId: Integer read GetRecId;
    property NeedRequery: Boolean read FNeedRequery write FNeedRequery;
    property ChangedFields: TUniList read FChangedFields;
    //property ChangedLabels: TUniList read FChangedLabels;
    property ChangedTables: TUniList read FChangedTables;
    property ChangedQueries: TUniList read FChangedQueries;
    property ChangedProps: TUniPropList read FChangedProps;
    property ChangedTimers: TUniList read FChangedTimers;
    property GotoUrl: String read FGotoUrl write FGotoUrl;
    property GotoOption: TGotoOption read FGotoOption write FGotoOption;
    property DocUrl: String read FDocUrl write FDocUrl;
    property PrintErrors: String read FPrintErrors write FPrintErrors;
   // property Actions: TObject read FActions write FActions;
    property RunScript: TRunScript read FRunScript;
    property Editing: TAccessStatus read FEditing write FEditing;
    property Deleting: TAccessStatus read FDeleting write FDeleting;
    property OldState: TDataSetState read FOldState write FOldState;
    property OldRecId: Integer read FOldRecId write FOldRecId;
    property CallerRS: TSsRecordSet read FCallerRS write SetCallerRS;
    property CallerObj: TdxLookupComboBox read FCallerObj write FCallerObj;
    property QryRecId: Integer read FQryRecId write FQryRecId;
    property FreshValue: Integer read FFreshValue write FFreshValue;
    property ActionList: TList read FActionList;
    property RecordSetState: TSsRecordSetStates read FState;
  end;

  { TSsRecordSets }

  TSsRecordSets = class(TList)
  private
    function GetRecordSets(Index: Integer): TSsRecordSet;
  public
    function FindFormById(Id: Integer): TSsRecordSet;
    function FindFormByName(AFormName: String): TSsRecordSet;
    function FindRpById(Id: Integer): TSsRecordSet;
    function FindRpByName(AName: String): TSsRecordSet;
    procedure DeleteRecordSet(RS: TSsRecordSet);
    procedure Clear; override;
    property RecordSets[Index: Integer]: TSsRecordSet read GetRecordSets; default;
  end;

  { TSsFormData }

  TSsFormData = class
  private
    FFilter: TFilterObject;
    FId: Integer;
    FLastEdits: TStringList;
    //FWhere: String;
  public
    constructor Create;
    destructor Destroy; override;
    property Id: Integer read FId write FId;
    property Filter: TFilterObject read FFilter write FFilter;
    //property Where: String read FWhere write FWhere;
    property LastEdits: TStringList read FLastEdits;
  end;

  { TSsFormList }

  TSsFormList = class(TList)
  private
    function GetForms(Index: Integer): TSsFormData;
    function AddForm(FmId: Integer): TSsFormData;
    function FindForm(FmId: Integer): TSsFormData;
  public
    function GetForm(Fm: TdxForm): TSsFormData;
    //procedure DeleteForm(Fm: TdxForm);
    procedure Clear; override;
    property Forms[Index: Integer]: TSsFormData read GetForms; default;
  end;

  TMetaData = class;
  TImageManager = class;
  TWebServerRequestHandler = function (Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
    var AResponse : TFPHTTPConnectionResponse): Boolean of object;

  { TSession }

  TSession = class
  private
    FBrowserId: String;
    FClientHeight: Integer;
    FClientWidth: Integer;
    FConnectName: String;
    FDebugMsg: String;
    FDebugShow: Boolean;
    FDebugText: String;
    FIsService: Boolean;
    FMainErrorMsg: String;
    FMetaData: TMetaData;
    FBusy: Boolean;
    FDBase: TDBEngine;
    FDBItem: TDBItem;
    //FErrs: TStringList;
    FFilterPrId: Integer;
    FFormId: Integer;
    FCId: Integer;
    //FFormMan: TFormManager;
    FFormList: TSsFormList;
    //FId: String;
    FIP: String;
    FConnectTime, FLastTime: TDateTime;
    FMsg: String;
    FOnCreateForm: TCreateFormEvent;
    FOnDatabaseClose: TNotifyEvent;
    FOnDestroyForm: TCreateFormEvent;
    FOnHandleRequest: TWebServerRequestHandler;
    FPage: Integer;
    FRecId: Integer;
    FRecordSets, FRpRecordSets: TSsRecordSets;
    FRequest: TFPHTTPConnectionRequest;
    FRoleId: Integer;
    FRunScript: TRunScript;
    FExtRunMan: TExtRunManager;
    //FReportMan: TReportManager;
    FTableId: Integer;
    FTableRecId: Integer;
    FRpId: Integer;
    FTemplateId: Integer;
    FFilterId: Integer;
    FUserId: Integer;
    //FUserMan: TdxUserManager;
    FVarList: TVarList;
    FLock: TRTLCriticalSection;
    function GetDXMain: TDXMain;
    function GetFormMan: TFormManager;
    function GetImageMan: TImageManager;
    function GetReportMan: TReportManager;
    function GetUserMan: TdxUserManager;
    function GetScriptMan: TScriptManager;
    procedure SetMetaData(AValue: TMetaData);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ConnectDB(const AConnectName: String);
    procedure Clear;
    function GetFmId: Integer;
    function GetRecId: Integer;
    //procedure InitFormData;
    function AddRecordSet(AForm: TdxForm): TSsRecordSet;
    function FindRecordSet(AFormId, ARecId, ATableId: Integer): TSsRecordSet;
    function AddRpRepordSet(RD: TReportData): TSsRecordSet;
    function FindRpRecordSet(RDId: Integer): TSsRecordSet;
    procedure LockSession;
    procedure UnlockSession;
    //function GetUserName: String;
    property IP: String read FIP write FIP;
    property FormId: Integer read FFormId write FFormId;
    property Page: Integer read FPage write FPage;
    property RecId: Integer read FRecId write FRecId;
    property TableId: Integer read FTableId write FTableId;
    property TableRecId: Integer read FTableRecId write FTableRecId;
    property CId: Integer read FCId write FCId;
    property FormList: TSsFormList read FFormList;
    property RpId: Integer read FRpId write FRpId;
    property TemplateId: Integer read FTemplateId write FTemplateId;
    //property Id: String read FId write FId;
    //property Errs: TStringList read FErrs;
    property Msg: String read FMsg write FMsg;
    property ConnectTime: TDateTime read FConnectTime write FConnectTime;
    property LastTime: TDateTime read FLastTime write FLastTime;
    property FilterId: Integer read FFilterId write FFilterId;
    property FilterPrId: Integer read FFilterPrId write FFilterPrId;
    property Main: TDXMain read GetDXMain;
    property FormMan: TFormManager read GetFormMan;
    property ReportMan: TReportManager read GetReportMan;
    property VarList: TVarList read FVarList;
    property UserMan: TdxUserManager read GetUserMan;
    property ImageMan: TImageManager read GetImageMan;
    property ScriptMan: TScriptManager read GetScriptMan;
    property RecordSets: TSsRecordSets read FRecordSets;
    property DBase: TDBEngine read FDBase;
    property DBItem: TDBItem read FDBItem write FDBItem;
    property MetaData: TMetaData read FMetaData write SetMetaData;
    property UserId: Integer read FUserId write FUserId;
    property RoleId: Integer read FRoleId write FRoleId;
    property BrowserId: String read FBrowserId write FBrowserId;
    property ConnectName: String read FConnectName write FConnectName;
    property RunScript: TRunScript read FRunScript;
    property ExtRunMan: TExtRunManager read FExtRunMan;
    property MainErrorMsg: String read FMainErrorMsg write FMainErrorMsg;
    property DebugMsg: String read FDebugMsg write FDebugMsg;
    property DebugText: String read FDebugText write FDebugText;
    property DebugShow: Boolean read FDebugShow write FDebugShow;
    property Busy: Boolean read FBusy;
  public
    function CreateForm(const FormName: String): TdxForm;
    function SQLSelect(const SQL: String): TdxSQLQuery;
    procedure SQLExecute(const SQL: string);
    function EvalExpr(const Expr: String; Fm: TdxForm): Variant;
    function FindForm(const FormName: String; ARecId: Integer): TdxForm;
    function GetCacheDir: String;
    procedure Debug(Value: Variant);
    function GetCurrentUser: String;
    function GetCurrentRole: String;
    function GetCurrentDatabase: String;
    function GetTemplatesPath: String;
    function SetExprVar(const AName: String; AValue: Variant): Variant;
    function GetExprVar(const AName: String): Variant;
    property Request: TFPHTTPConnectionRequest read FRequest write FRequest;
    property IsService: Boolean read FIsService write FIsService;
    property ClientWidth: Integer read FClientWidth write FClientWidth;
    property ClientHeight: Integer read FClientHeight write FClientHeight;
    property OnCreateForm: TCreateFormEvent read FOnCreateForm write FOnCreateForm;
    property OnDestroyForm: TCreateFormEvent read FOnDestroyForm write FOnDestroyForm;
    property OnDatabaseClose: TNotifyEvent read FOnDatabaseClose write FOnDatabaseClose;
    property OnHandleRequest: TWebServerRequestHandler read FOnHandleRequest write FOnHandleRequest;
  end;

  { TSessionList }

  TSessionList = class(TList)
  private
    FLock: TRTLCriticalSection;
    function GetSessions(Index: Integer): TSession;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    //function FindSession(const Id: String): TSession;
    function FindSessionByBrowserId(const BrowserId, ConnectName: String): TSession;
    procedure DeleteSession(var SS: TSession);
    procedure Lock;
    procedure Unlock;
    property Sessions[Index: Integer]: TSession read GetSessions; default;
  end;

  TImageData = class
  public
    Name, Ext: String;
  end;

  { TImageManager }

  TImageManager = class(TList)
  private
    function GetImages(Index: Integer): TImageData;
  public
    function AddImage(const Name, Ext: String): TImageData;
    function FindImage(const Name: String): TImageData;
    procedure Clear; override;
    property Images[Index: Integer]: TImageData read GetImages; default;
  end;

  { TMetaData }

  TMetaData = class
  private
    FConnectName: String;
    FId: String;
    FImageMan: TImageManager;
    FKeepMetaData: Boolean;
    FLoadComplete: Boolean;
    FLock: TRTLCriticalSection;
    FDatabase: String;
    FFormMan: TFormManager;
    FLastModified: TDateTime;
    FMain: TdxMain;
    FRef: Integer;
    FReportMan: TReportManager;
    FScriptMan: TScriptManager;
    FUserMan: TdxUserManager;
    FUsersLoaded: Boolean;
    FFormDataSet: TdxMemDataSet;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadMain(DBase: TDBEngine);
    procedure LoadForms(DBase: TDBEngine);
    function LoadForm(FormId: Integer): TdxForm;
    procedure LoadReports(DBase: TDBEngine);
    procedure LoadUsers(DBase: TDBEngine);
    procedure LoadImages(DBase: TDBEngine);
    procedure LoadScripts(DBase: TDBEngine);
    procedure IncRef;
    procedure DecRef;
    procedure Lock;
    procedure Unlock;
    property Id: String read FId;
    property Database: String read FDatabase write FDatabase;
    property LastModified: TDateTime read FLastModified write FLastModified;
    property FormMan: TFormManager read FFormMan;
    property ReportMan: TReportManager read FReportMan;
    property UserMan: TdxUserManager read FUserMan;
    property ImageMan: TImageManager read FImageMan;
    property ScriptMan: TScriptManager read FScriptMan;
    property Main: TdxMain read FMain;
    property Ref: Integer read FRef;
    property ConnectName: String read FConnectName write FConnectName;
    property LoadComplete: Boolean read FLoadComplete write FLoadComplete;
    property UsersLoaded: Boolean read FUsersLoaded write FUsersLoaded;
    property KeepMetaData: Boolean read FKeepMetaData write FKeepMetaData;
  end;

  { TMetaManager }

  TMetaManager = class(TList)
  private
    FLock: TRTLCriticalSection;
    function GetMetaData(Index: Integer): TMetaData;
  public
    constructor Create;
    destructor Destroy; override;
    function FindMetaData(const ADatabase: String; ALastModified: TDateTime): TMetaData;
    function NewMetaDataExists(OldMD: TMetaData): Boolean;
    procedure DeleteMetaData(MD: TMetaData);
    procedure DeleteOldMetaData(const ADatabase: String);
    procedure Clear; override;
    procedure Lock;
    procedure Unlock;
    property MetaData[Index: Integer]: TMetaData read GetMetaData; default;
  end;

function MsgDlgTypeToStr(T: TMsgDlgType): String;
function MsgDlgBtnToStr(T: TMsgDlgBtn): String;

implementation

uses
  LazUtf8, FileUtil, sqlgen, apputils, expressions, lfmparser, dxactions,
  BGRABitmapTypes, exprfuncs;

function MsgDlgTypeToStr(T: TMsgDlgType): String;
begin
  case T of
    mtWarning: Result := 'warning';
    mtError: Result := 'error';
    mtInformation: Result := 'info';
    mtConfirmation: Result := 'confirm';
    mtCustom: Result := 'custom';
    else Result := '';
  end;
end;

function MsgDlgBtnToStr(T: TMsgDlgBtn): String;
begin
  case T of
    mbYes: Result := 'yes';
    mbNo: Result := 'no';
    mbOK: Result := 'ok';
    mbCancel: Result := 'cancel';
    mbAbort: Result := 'abort';
    mbRetry: Result := 'retry';
    mbIgnore: Result := 'ignore';
    mbAll: Result := 'all';
    mbNoToAll: Result := 'noToAll';
    mbYesToAll: Result := 'yesToAll';
    mbHelp: Result := 'help';
    mbClose: Result := 'close';
    else Result := '';
  end;
end;

{ TImageManager }

function TImageManager.GetImages(Index: Integer): TImageData;
begin
  Result := TImageData(Items[Index]);
end;

function TImageManager.AddImage(const Name, Ext: String): TImageData;
begin
  Result := TImageData.Create;
  Result.Name := Name;
  Result.Ext := Ext;
  Add(Result);
end;

function TImageManager.FindImage(const Name: String): TImageData;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Images[i].Name = Name then Exit(Images[i]);
end;

procedure TImageManager.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Images[i].Free;
  inherited Clear;
end;

{ TMetaData }

constructor TMetaData.Create;
begin
  FId := SetZeros(Random(100000), 6);
  FMain := TdxMain.Create;
  FFormMan := TFormManager.Create;
  FReportMan := TReportManager.Create;
  FUserMan := TdxUserManager.Create;
  FImageMan := TImageManager.Create;
  FScriptMan := TScriptManager.Create(Self);
  FFormDataSet := TdxMemDataSet.Create(nil);
  FRef := 1;
  InitCriticalSection(FLock);
end;

destructor TMetaData.Destroy;
begin
  FScriptMan.Free;
  FImageMan.Free;
  FUserMan.Free;
  FReportMan.Free;
  FFormMan.Free;
  FMain.Free;
  FFormDataSet.Free;
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure TMetaData.LoadMain(DBase: TDBEngine);
var
  St: TStream;
  DS: TSQLQuery;
begin
  DS := DBase.OpenDataSet('select actions, settings, lastmodified from dx_main where id=1');
  St := nil;
  try
    St := DS.CreateBlobStream(DS.Fields[0], bmRead);
    FMain.LoadActions(St);
    FreeAndNil(St);
    St := DS.CreateBlobStream(DS.Fields[1], bmRead);
    FMain.LoadSettings(St);
    FMain.LastModified := DS.Fields[2].AsDateTime;
  finally
    FreeAndNil(St);
    DS.Free;
  end;
end;

procedure TMetaData.LoadForms(DBase: TDBEngine);
var
  SS: TStringStream;
  Lfm: TLfmParser;
  Fm: TdxForm;
  DS: TSQLQuery;
begin
  ForceDirectories(GetEmbeddedImagesPath(Self));
  DS := DBase.OpenDataSet('select id, form from dx_forms');
  FFormDataSet.CopyFromDataset(DS, True);
  DS.Free;
  with FFormDataSet do
  try
    First;
    SS := TStringStream.Create('');
    Lfm := TLfmParser.Create;
    Lfm.EmbeddedImagesDir := GetEmbeddedImagesPath(Self);
    while not Eof do
    begin
      SS.Size := 0;
      TBlobField(Fields[1]).SaveToStream(SS);
      Fm := Lfm.Parse(SS.DataString);
      Fm.ExtractFormGrid;
      //if Fm.Filters.Count > 0 then Fm.Filter.Load(Fm.Filters.ValueFromIndex[0]);
      FFormMan.AddForm(Fm);
      ScaleForm(Fm, Main.DesignTimePPI);
      Next;
    end;
  finally
    Lfm.Free;
    SS.Free;
    //Free;
  end;
end;

function TMetaData.LoadForm(FormId: Integer): TdxForm;
var
  SS: TStringStream;
  Lfm: TLfmParser;
begin
  with FFormDataSet do
  try
    SS := TStringStream.Create('');
    Lfm := TLfmParser.Create;
    Lfm.EmbeddedImagesDir := GetEmbeddedImagesPath(Self);
    Locate('id', FormId, []);
    TBlobField(Fields[1]).SaveToStream(SS);
    Result := Lfm.Parse(SS.DataString);
    Result.ExtractFormGrid;
    if Result.Filters.Count > 0 then Result.Filter.Load(Result.Filters.ValueFromIndex[0]);
    ScaleForm(Result, Main.DesignTimePPI);
    //HideUnvisibleControls(Result);
  finally
    Lfm.Free;
    SS.Free;
  end;
end;

procedure TMetaData.LoadReports(DBase: TDBEngine);
var
  DS: TSQLQuery;
  MS: TMemoryStream;
  RD: TReportData;
begin
  DS := DBase.OpenDataSet('select id, data from dx_reports');
  MS := TMemoryStream.Create;
  try
    while DS.EOF = False do
    begin
      MS.Size:=0;
      TBlobField(DS.Fields[1]).SaveToStream(MS);
      MS.Position := 0;
      RD := TReportData.Create;
      RD.LoadFromStream(MS);
      RD.Grid.SortColumns;
      FReportMan.AddReport(RD);
      ScaleReport(RD, Main.DesignTimePPI, 96);
      DS.Next;
    end;
  finally
    MS.Free;
    DS.Free;
  end;
end;

procedure TMetaData.LoadUsers(DBase: TDBEngine);
var
  DS: TSQLQuery;
  St: TStream;
begin
  DS := DBase.OpenDataSet('select id, users, roles, intfs from dx_users');
  St := nil;
  try
    if DS.RecordCount = 0 then Exit;
    St := DS.CreateBlobStream(DS.Fields[1], bmRead);
    FUserMan.Users.LoadFromStream(St);
    FreeAndNil(St);
    St := DS.CreateBlobStream(DS.Fields[2], bmRead);
    FUserMan.Roles.LoadFromStream(St);
    FreeAndNil(St);
    St := DS.CreateBlobStream(DS.Fields[3], bmRead);
    FUserMan.Intfs.LoadFromStream(St);
  finally
    FreeAndNil(St);
    DS.Free;
  end;
end;

procedure TMetaData.LoadImages(DBase: TDBEngine);
var
  FlNm, Ext: String;
  St: TStream;
  FS: TFileStream;
begin
  with DBase.OpenDataSet('select name, img_100 from dx_images') do
  try
    if RecordCount > 0 then
      ForceDirectories(GetImagesPath(Self));
    while not Eof do
    begin
      St := CreateBlobStream(Fields[1], bmRead);
      Ext := SuggestImageExtension(DetectFileFormat(St));
      if Ext <> '?' then
      begin
        FlNm := GetImagesPath(Self) + Fields[0].AsString + '.' + Ext;
        FS := TFileStream.Create(FlNm, fmCreate);
        FS.CopyFrom(St, St.Size);
        FS.Free;
        FImageMan.AddImage(Fields[0].AsString, Ext);
      end;
      St.Free;
      Next;
    end;
  finally
    Free;
  end;
end;

procedure TMetaData.LoadScripts(DBase: TDBEngine);
begin
  with DBase.OpenDataSet(Format('select id, fmid, name, script, kind from dx_scripts ' +
    'where kind<>%0:d and kind<>%1:d', [Ord(skMain), Ord(skForm)])) do
  begin
    while not Eof do
    begin
      with FScriptMan.AddScript(Fields[1].AsInteger,
      	Fields[2].AsString, Fields[3].AsString) do
        begin
          Kind := TScriptKind(Fields[4].AsInteger);
          if Kind = skWebForm then Form := FFormMan.FindForm(FmId);
          //
          //SourceData.LoadFromString(Fields[5].AsString);
        end;
      Next;
    end;
    Free;
  end;
  // Главный модуль
  if FScriptMan.FindScriptByName('WebMain') = nil then
    FScriptMan.AddScript(0, 'WebMain', '').Kind:=skWebMain;
end;

procedure TMetaData.IncRef;
begin
  Inc(FRef);
end;

procedure TMetaData.DecRef;
begin
  Dec(FRef);
end;

procedure TMetaData.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TMetaData.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

{ TMetaManager }

function TMetaManager.GetMetaData(Index: Integer): TMetaData;
begin
  Result := TMetaData(Items[Index]);
end;

constructor TMetaManager.Create;
begin
  inherited Create;
  InitCriticalSection(FLock);
end;

destructor TMetaManager.Destroy;
begin
  DoneCriticalsection(FLock);
  inherited Destroy;
end;

procedure TMetaManager.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    MetaData[i].Free;
  inherited Clear;
end;

function TMetaManager.FindMetaData(const ADatabase: String;
  ALastModified: TDateTime): TMetaData;
var
  i: Integer;
  MD: TMetaData;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    MD := MetaData[i];
    if (MyUtf8CompareText(ADatabase, MD.Database) = 0) and
      (ALastModified = MD.LastModified) then Exit(MD);
  end;
end;

function TMetaManager.NewMetaDataExists(OldMD: TMetaData): Boolean;
var
  i: Integer;
  MD: TMetaData;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    MD := MetaData[i];
    if (MyUtf8CompareText(MD.Database, OldMD.Database) = 0) and
      (MD.LastModified > OldMD.LastModified) then Exit(True);
  end;
end;

procedure TMetaManager.DeleteMetaData(MD: TMetaData);
begin
  DebugStr('DeleteMetaData Lock');
  Lock;

  try
    DebugStr('MD.DecRef');
    MD.DecRef;
    if (MD.Ref = 0) and ( not MD.KeepMetaData or NewMetaDataExists(MD) ) then
    begin
      DebugStr('Delete meta dir: ' + GetMetaDataPath(MD));
      if not DeleteDirectory(GetMetaDataPath(MD), False) then
      begin
        if DirectoryExists(GetMetaDataPath(MD)) then
          DebugStr('Can not delete metadata directory ' + GetMetaDataPath(MD));
      end;
      DebugStr('Remove(MD)');
      Remove(MD);
      FreeAndNil(MD);
    end;
  finally
    DebugStr('DeleteMetaData Unlock');
    UnLock;
  end;

  if MD = nil then
    DebugStr('Delete metadata. Count: ' + IntToStr(Count));
end;

procedure TMetaManager.DeleteOldMetaData(const ADatabase: String);
var
  i: Integer;
  MD: TMetaData;
begin
  DebugStr('ClearOldMetaData Lock');
  Lock;

  try

    for i := Count - 1 downto 0 do
    begin
      MD := MetaData[i];
      if (MyUtf8CompareText(ADatabase, MD.Database) = 0) and (MD.Ref = 0) then
      begin
        DebugStr('Delete meta dir: ' + GetMetaDataPath(MD));
        if not DeleteDirectory(GetMetaDataPath(MD), False) then
        begin
          if DirectoryExists(GetMetaDataPath(MD)) then
            DebugStr('Can not delete metadata directory ' + GetMetaDataPath(MD));
        end;
        DebugStr('Remove(MD)');
        Remove(MD);
        FreeAndNil(MD);
      end;
    end;

  finally
    DebugStr('ClearOldMetaData Unlock');
    UnLock;
  end;
end;

procedure TMetaManager.Lock;
begin
  EnterCriticalsection(FLock);
end;

procedure TMetaManager.Unlock;
begin
  LeaveCriticalsection(FLock);
end;

{ TSsRecordSets }

function TSsRecordSets.GetRecordSets(Index: Integer): TSsRecordSet;
begin
  Result := TSsRecordSet(Items[Index]);
end;

function TSsRecordSets.FindRpById(Id: Integer): TSsRecordSet;
var
  i: Integer;
  RS: TSsRecordSet;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    RS := RecordSets[i];
    if (RS.RD <> nil) and (RS.RD.Id = Id) then Exit(RS);
  end;
end;

function TSsRecordSets.FindRpByName(AName: String): TSsRecordSet;
var
  i: Integer;
  RS: TSsRecordSet;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    RS := RecordSets[i];
    if (RS.RD <> nil) and (MyUtf8CompareText(RS.RD.Name, AName) = 0) then Exit(RS);
  end;
end;

function TSsRecordSets.FindFormById(Id: Integer): TSsRecordSet;
var
  i: Integer;
  RS: TSsRecordSet;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    RS := RecordSets[i];
    if (RS.Form <> nil) and (RS.Form.Id = Id) then Exit(RS);
  end;
end;

function TSsRecordSets.FindFormByName(AFormName: String): TSsRecordSet;
var
  i: Integer;
  RS: TSsRecordSet;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    RS := RecordSets[i];
    if (RS.Form <> nil) and (MyUtf8CompareText(RS.Form.FormCaption, AFormName) = 0) then Exit(RS);
  end;
end;

procedure TSsRecordSets.DeleteRecordSet(RS: TSsRecordSet);
begin
  if RS.Form <> nil then
  begin
    if RS.Form.OnDestroy <> nil then RS.Form.OnDestroy(RS.Form);
    if RS.Session.OnDestroyForm <> nil then RS.Session.OnDestroyForm(RS.Session, RS.Form);
  end;
  Remove(RS);
  RS.Free;
end;

procedure TSsRecordSets.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    RecordSets[i].Free;
  inherited Clear;
end;

{ TSsRecordSet }

{function FieldExistsInExpr(const Expr, FieldName: String): Boolean;
var
  E, Fl: String;
begin
  E := Trim(Utf8LowerCase(Expr));
  if E = '' then Exit(False);
  Fl := Utf8LowerCase(FieldName);
  Result := (Pos('[' + Fl + ']', E) > 0) or (Pos('[' + Fl + '|', E) > 0);
end;  }

{function FieldExistsInQueryFilters(RD: TReportData; const FieldName: String
  ): Boolean;
var
  i: Integer;
begin
  for i := 0 to RD.Sources.Count - 1 do
    if FieldExists(FieldName, RD.Sources[i]^.Filter) then Exit(True);
  Result := FieldExistsForQuery(FieldName, RD.Filter);
  if not Result then
    for i := 0 to RD.CalcFields.Count - 1 do
      if FieldExistsForQuery(FieldName, RD.CalcFields[i]^.Expr) then Exit(True);
end;   }

{function QueryExistsInQFilters(RD: TReportData; const AQueryName: String): Boolean;
var
  i: Integer;
begin
  for i := 0 to RD.Sources.Count - 1 do
    if FormExistsInExpr(AQueryName, RD.Sources[i]^.Filter) then Exit(True);
  Result := FormExistsInExpr(AQueryName, RD.Filter);
  if not Result then
    for i := 0 to RD.CalcFields.Count - 1 do
      if FormExistsInExpr(AQueryName, RD.CalcFields[i]^.Expr) then Exit(True);
end;      }

procedure TSsRecordSet.ChangeObjectFields(Obj: TdxField);
var
  SrcFm: TdxForm;
  RId: Variant;
  FL, SrcFL: TList;
  i: Integer;
  FNm, SrcFNm, S: String;
  SrcDS, DS: TDataSet;
  HierarchyFId: Integer;
  C, SrcC: TdxField;
  ObjRS: TSsRecordSet;
begin
  if not (Obj is TdxLookupComboBox) then Exit;
  SrcFm := FSS.FormMan.FindForm(GetSourceTId(Obj));
  if SrcFm = nil then Exit;
  ObjRS := TSsRecordset.Create(FSS, nil);
  ObjRS.Form := SrcFm;

  RId := GetDSField(Obj).Value;
  DS := FDataSet;

  FL := TList.Create;
  SrcFL := TList.Create;
  for i := 0 to FFields.Count - 1 do
  begin
    C := TdxField(FFields[i]);
    if (C is TdxObjectField) and (TdxObjectField(C).ObjId = Obj.Id) then
    begin
      SrcC := SrcFm.FindField(TdxObjectField(C).FieldId);
      if SrcC <> nil then
      begin
        FL.Add(C);
        SrcFL.Add(SrcC);
      end;
    end;
  end;

  HierarchyFId := 0;
  if SrcFm.ParentField > 0 then
  begin
    SrcC := SrcFm.FindField(SrcFm.ParentField);
    // Добавляем родительский объект, если форма иерархическая и в полях объекта
    // нет родительского объекта, а поле объекта "Название" присутствует.
    // Таким образом SrcFL.Count > FL.Count на 1.
    if SrcFL.IndexOf(SrcC) < 0 then
    begin
      C := SrcFm.FindField(GetSourceFId(SrcC));
      if SrcFL.IndexOf(C) >= 0 then
      begin
        SrcFL.Add(SrcC);
        HierarchyFId := C.Id;
      end;
    end
    // Если есть и родитель, и "название", то запоминает id "названия".
    else
    begin
      C := SrcFm.FindField(GetSourceFId(SrcC));
      if SrcFL.IndexOf(C) >= 0 then
        HierarchyFId := C.Id;
    end;
  end;

  if FL.Count > 0 then
  begin
  	if RId <> Null then
  	begin
      ObjRS.OpenObjectFields(SrcFL, RId);
    	SrcDS := ObjRS.DataSet;
      if SrcDS.RecordCount > 0 then
		  	for i := 0 to FL.Count - 1 do
        begin
          SrcC := TdxField(SrcFL[i]);
          SrcFNm := FieldStr(SrcC.Id);
          C := TdxField(FL[i]);
          FNm := FieldStr(C.Id);
          if SrcC is TdxLookupComboBox then
          begin
            if (GetSourceTId(SrcC) > 0) and (GetSourceFId(SrcC) > 0) then
            begin
              DS.FieldByName(FNm + 'id').Value := SrcDS.FieldByName(SrcFNm).Value;
              DS.FieldByName(FNm).Value:=SrcDS.FieldByName(SrcFNm + 'l').Value;
            end;
          end
          else
          begin
            if SrcC.Id = HierarchyFId then
            begin
              S := SrcDS.FieldByName(FieldStr(SrcFm.ParentField) + 'l').AsString;
              if S <> '' then S := S + '\';
              DS.FieldByName(FNm).Value := S + SrcDS.FieldByName(SrcFNm).AsString;
            end
            else
              DS.FieldByName(FNm).Value := SrcDS.FieldByName(SrcFNm).Value;
          end;
        end;
    end
    else
    begin
      for i := 0 to FL.Count - 1 do
      begin
        SrcC := TdxField(SrcFL[i]);
        C := TdxField(FL[i]);
        FNm := FieldStr(C.Id);
        if SrcC is TdxLookupComboBox then
        	DS.FieldByName(FNm + 'id').SetData(nil);
        DS.FieldByName(FNm).SetData(nil);
      end;
    end;
  end;
  FL.Free;
  SrcFL.Free;
  ObjRS.Free;
end;

procedure TSsRecordSet.ControlPropertyChange(Sender: TObject;
  const PropName: String);

  procedure _DoChildren(WC: TdxWinControl);
  var
    i: Integer;
    C: TdxControl;
  begin
    for i := 0 to WC.ControlCount - 1 do
    begin
      C := WC.Controls[i];
      FChangedProps.AddItem(C, PropName);
      if C is TdxWinControl then
        _DoChildren(TdxWinControl(C));
    end;
  end;

begin
  if FState = [] then
  begin
    FChangedProps.AddItem(Sender, PropName);
    if (PropName = 'enabled') and (Sender is TdxWinControl) then
      _DoChildren(TdxWinControl(Sender));
  end;
end;

procedure TSsRecordSet.DataSetAfterCancel(DataSet: TDataSet);
var
  i: Integer;
begin
  if FForm = nil then Exit;
  if FForm.PId = 0 then
  begin
    for i := 0 to FForms.Count - 1 do
      FForms[i].NeedRequery:=True;
  end;
  if (FForm <> nil) and (FForm.OnAfterCancel <> nil) then FForm.OnAfterCancel(FForm);
end;

procedure TSsRecordSet.DataSetAfterDelete(DataSet: TDataSet);
begin

end;

procedure TSsRecordSet.DataSetAfterEdit(DataSet: TDataSet);
begin
  if FForm = nil then Exit;

  if rstRecalculate in FState then Exit;

  UpdateAccessState;
  if FForm.OnAfterEdit <> nil then FForm.OnAfterEdit(FForm);
  if FForm.OnStateChange <> nil then FForm.OnStateChange(FForm);
end;

procedure TSsRecordSet.DataSetAfterInsert(DataSet: TDataSet);
var
  L: TList;
  i: Integer;
  F: TdxField;
  S: String;
  V: Variant;
begin
  if FForm = nil then Exit;

  if FForm.ViewType = vtSimpleForm then
    FDataSet['id'] := 1
  else if (FForm.PId > 0) and (FParent.Form.ViewType = vtSimpleForm) then
  begin
    Inc(FMaxRecId);
    FDataSet['id'] := FMaxRecId;
  end
  else
    FDataSet['id'] := FSS.DBase.GenId('gen_' + TableStr(FForm.Id));

  if FForm.PId > 0 then
    FDataSet['pid'] := FParent.RecId;

  if rstDuplicate in FState then
  begin
    Exclude(FState, rstInsert);
    Exit;
  end;

  L := TList.Create;
  FForm.GetFields(L);

  for i := 0 to L.Count - 1 do
  begin
    F := TdxField(L[i]);
    S := Trim(GetDefaultValue(F));
    if (F is TdxDateEdit) and (TdxDateEdit(F).DateNow) then
      SetDSField(F, Date)
    else if (F is TdxTimeEdit) and (TdxTimeEdit(F).CurTime) then
      SetDSField(F, Time)
    else if F is TdxRecordId then
      SetDSField(F, DataSet['id'])
    else if (S <> '') and GetDSField(F).IsNull then
      try
        V := EvalField(Self, S, True);
        if V <> Null then SetDSField(F, V);
      except
        on E: Exception do
          FForm.Errs.AddObject(F.FieldName + ': ' + E.Message, TObject(PtrInt(F.Id)));
      end;
  end;

  L.Free;
  Exclude(FState, rstInsert);

  UpdateAccessState;
  if FForm.OnAfterInsert <> nil then FForm.OnAfterInsert(FForm);
  if FForm.OnStateChange <> nil then FForm.OnStateChange(FForm);
end;

procedure TSsRecordSet.DataSetAfterPost(DataSet: TDataSet);
var
  i: Integer;
begin
  if FForm = nil then Exit;

  if FForm.PId = 0 then
  begin
    if FForm.ViewType <> vtSimpleForm then
    begin
      FSS.DBase.ApplyDataSet(TSQLQuery(FDataSet));
      for i := 0 to FForms.Count - 1 do
        FSS.DBase.ApplyDataSet(TSQLQuery(FForms[i].DataSet));
      FSS.DBase.Commit;
    end;
  end
  else
  begin
    ParentFormSetModified;
    FParent.EvalAggFields(FForm.FormCaption);
    FParent.EvalAggLabels(FForm.FormCaption);
    if (FParent.FState = []) and (FState = []) then
      FParent.FChangedTables.AddItem(Self);
  end;

  if rstRecalculate in FState then Exit;

  if FMemDS <> nil then
  begin
    if FIsNewRecord then
    begin
      //if FDataSet.RecNo = FDataSet.RecordCount then
        FMemDS.Append;
      {else
      begin
        FMemDS.First;
        FMemDS.MoveBy(FDataSet.RecNo);
        FMemDS.Insert;
      end;}
      FMemDS['id'] := FDataSet['id'];
    end
    else if FMemDS.Locate('id', FDataSet['id'], []) then
      FMemDS.Edit
    else
      raise Exception.Create('TSsRecordSet.Post - can not locate record.');
    for i := 0 to FMemDS.Fields.Count - 1 do
      FMemDS.Fields[i].Value := FDataSet.Fields[i].Value;
    FMemDS.Post;
  end;

  if rstDuplicate in FState then Exit;

  UpdateAccessState;
  if FForm.OnAfterPost <> nil then FForm.OnAfterPost(FForm);
  if FForm.OnStateChange <> nil then FForm.OnStateChange(FForm);
end;

procedure TSsRecordSet.DataSetAfterOpen(DataSet: TDataSet);
begin
  if [rstRecalculate, rstDuplicate] * FState <> [] then Exit;
  if (FForm <> nil) and (FForm.OnAfterOpen <> nil) then FForm.OnAfterOpen(FForm);
end;

procedure TSsRecordSet.DataSetAfterClose(DataSet: TDataSet);
begin
  if [rstRecalculate, rstDuplicate] * FState <> [] then Exit;
  if (FForm <> nil) and (FForm.OnAfterClose <> nil) then FForm.OnAfterClose(FForm)
  else if FQGrid <> nil then
  begin
    if FQGrid.OnAfterClose <> nil then FQGrid.OnAfterClose(FQGrid);
  end;
end;

procedure TSsRecordSet.DataSetAfterScroll(ADataSet: TDataSet);
var
  QRS: TSsRecordSet;
  i: Integer;
begin
  if FForm <> nil then
  begin
    for i := 0 to FForms.Count - 1 do
      FForms[i].NeedRequery := True;
    for i := 0 to FQueries.Count - 1 do
      {if not FQueries[i].QGrid.ManualRefresh then} FQueries[i].NeedRequery := True;
    if [rstRecalculate, rstDuplicate] * FState = [] then
    begin
      UpdateAccessState;
      if FForm.OnAfterScroll <> nil then FForm.OnAfterScroll(FForm);
    end;
  end
  else if FQGrid <> nil then
  begin
    FParent.SetNeedRequeryLinkedQueries(Self);

    if [rstRecalculate, rstDuplicate] * FState = [] then
    begin
      if (FQGrid <> nil) and (FQGrid.OnAfterScroll <> nil) then FQGrid.OnAfterScroll(FQGrid);
    end;

    if FState = [] then
    begin
      for i := 0 to FParent.Queries.Count - 1 do
      begin
        QRS := FParent.Queries[i];
        if not QRS.QGrid.ManualRefresh and QRS.NeedRequery then FParent.ChangedQueries.AddItem(QRS);
      end;
    end;
  end;
end;

procedure TSsRecordSet.DataSetBeforeCancel(DataSet: TDataSet);
begin
  if (FForm <> nil) and (FForm.OnBeforeCancel <> nil) then FForm.OnBeforeCancel(FForm);
end;

procedure TSsRecordSet.DataSetBeforeClose(DataSet: TDataSet);
begin
  if [rstRecalculate, rstDuplicate] * FState <> [] then Exit;
  if (FForm <> nil) and (FForm.OnBeforeClose <> nil) then FForm.OnBeforeClose(FForm)
  else if (FQGrid <> nil) and (FQGrid.OnBeforeClose <> nil) then FQGrid.OnBeforeClose(FQGrid);
end;

procedure TSsRecordSet.DataSetBeforeDelete(DataSet: TDataSet);
begin
  if (FForm <> nil) and (FForm.OnBeforeDelete <> nil) then FForm.OnBeforeDelete(FForm);
end;

procedure TSsRecordSet.DataSetBeforeEdit(DataSet: TDataSet);
begin
  if rstRecalculate in FState then Exit;
  if (FForm <> nil) and (FForm.OnBeforeEdit <> nil) then FForm.OnBeforeEdit(FForm);
end;

procedure TSsRecordSet.DataSetBeforeInsert(DataSet: TDataSet);
begin
  if FForm = nil then Exit;

  if rstDuplicate in fState then Exit;
  Include(FState, rstInsert);
  if FForm.OnBeforeInsert <> nil then FForm.OnBeforeInsert(FForm);
end;

procedure TSsRecordSet.DataSetBeforeOpen(DataSet: TDataSet);
begin
  if [rstRecalculate, rstDuplicate] * FState <> [] then Exit;
  if (FForm <> nil) and (FForm.OnBeforeOpen <> nil) then FForm.OnBeforeOpen(FForm)
  else if (FQGrid <> nil) and (FQGrid.OnBeforeOpen <> nil) then FQGrid.OnBeforeOpen(FQGrid);
end;

procedure TSsRecordSet.DataSetBeforePost(DataSet: TDataSet);
var
  i: Integer;
  F: TdxField;
begin
  if FForm = nil then Exit;

  FIsNewRecord := FDataSet.State = dsInsert;

  if FIsNewRecord then
    for i := 0 to FFields.Count - 1 do
    begin
      F := TdxField(FFields[i]);
      if F is TdxCounter then
      begin
        if GetDSField(F).IsNull then
          SetDSField(F, FSS.DBase.GenId('gen_' + FieldStr(F.Id)));
      end;
    end;

  if [rstRecalculate, rstDuplicate] * FState <> [] then Exit;
  if (FForm <> nil) and (FForm.OnBeforePost <> nil) then FForm.OnBeforePost(FForm);
end;

procedure TSsRecordSet.DataSetBeforeScroll(DataSet: TDataSet);
begin
  if [rstRecalculate, rstDuplicate] * FState <> [] then Exit;
  if FState = [] then
  begin
    if FForm <> nil then
    begin
      if FForm.PId > 0 then FParent.ChangedProps.AddItem(FForm , 'recno');
    end
    else if FQGrid <> nil then
      FParent.ChangedProps.AddItem(FQGrid, 'recno');
  end;
  if (FForm <> nil) and (FForm.OnBeforeScroll <> nil) then FForm.OnBeforeScroll(FForm)
  else if (FQGrid <> nil) and (FQGrid.OnBeforeScroll <> nil) then FQGrid.OnBeforeScroll(FQGrid);
end;

procedure TSsRecordSet.FieldChange(Sender: TField);
var
  F: TdxField;
  V: Variant;
  Prec: Integer;
begin
  F := TdxField(FFields[Sender.Tag]);
  V := Sender.Value;
  if V <> Null then
  begin
    // Автоматическое округление числа до указанной точности.
    if F is TdxCalcEdit then
    begin
      Prec := TdxCalcEdit(F).Precission;
      if ((V >= 0) and (V > Power(10, 15 - Prec))) or
  	    ((V < 0) and (V < -Power(10, 15 - Prec))) then V := Null
      else
        V := AppUtils.MathRound(V, Prec);
    end
    // Отсекаем время
    else if F is TdxDateEdit then
    begin
      V := Trunc(Double(V))
    end
    // Автоматическое усечение времени до указанного формата
    else if F is TdxTimeEdit then
    begin
      V := TruncTime(TdxTimeEdit(F).TimeFormat, V)
    end
    else if (F is TdxEdit) or (F is TdxMemo) or (F is TdxComboBox) then
    begin
      if GetFieldSize(F) > 0 then
        V := Utf8Copy(V, 1, GetFieldSize(F));
    end
    else if F is TdxCounter then
    begin
      if (V > Integer.MaxValue) or (V < Integer.MinValue) then V := Null;
    end;
    Sender.OnChange := nil;
    Sender.Value := V;
    Sender.OnChange := @FieldChange;
  end;

  if rstDuplicate in FState then
  begin
    EvalFields(F.FieldName);
    Exit;
  end;

  if FState = [] then FChangedFields.AddItem(F);
  ChangeObjectFields(F);
  InsertObjectValues(F);
  EvalFields(F.FieldName);

  if rstRecalculate in FState then Exit;

  if not (rstInsert in FState) then
  begin
    EvalLabels(F.FieldName);
    RequeryQueries(F.FieldName);
  end;

  if FForm.OnFieldChange <> nil then FForm.OnFieldChange(FForm, F, F.FieldName);
end;

procedure TSsRecordSet.InsertObjectValues(F: TdxField);
var
  Fm, SrcFm: TdxForm;
  RId: Variant;
  FL, SrcFL: TList;
  i: Integer;
  C, SrcC: TdxField;
  FNm, SrcFNm: String;
  DS, SrcDS: TDataSet;
  Vl: TInsertValueData;
  ObjRS: TSsRecordSet;
  Obj: TdxLookupComboBox;
begin
  if not (F is TdxLookupComboBox) then Exit;
  Obj := TdxLookupComboBox(F);
  Fm := Obj.Form;
  RId := GetDSField(Obj).Value;
  DS := FDataSet;

  SrcFm := FSS.FormMan.FindForm(Obj.SourceTId);
  ObjRS := TSsRecordSet.Create(FSS, nil);
  ObjRS.Form := SrcFm;

  FL := TList.Create;
  SrcFL := TList.Create;
  for i := 0 to Obj.InsertedValues.Count - 1 do
  begin
    Vl := Obj.InsertedValues[i];
    SrcC := SrcFm.FindField(Vl.SrcField);
    C := Fm.findField(Vl.DestField);
    if (SrcC = nil) or (C = nil) then Continue;
    Fl.Add(C);
    SrcFL.Add(SrcC);
  end;

  if FL.Count > 0 then
  begin
  	if RId <> Null then
  	begin
      ObjRS.OpenObjectFields(SrcFL, RId);
    	SrcDS := ObjRS.DataSet;
      if SrcDS.RecordCount > 0 then
		  	for i := 0 to FL.Count - 1 do
        begin
          SrcC := TdxField(SrcFL[i]);
          SrcFNm := FieldStr(SrcC.Id);
          C := TdxField(FL[i]);
          FNm := FieldStr(C.Id);
          if SrcC is TdxLookupComboBox then
          begin
            DS.FieldByName(FNm + 'l').Value := SrcDS.FieldByName(SrcFNm + 'l').Value;
            DS.FieldByName(FNm).Value:=SrcDS.FieldByName(SrcFNm).Value;
          end
          else if C is TdxLookupComboBox then
          begin
            DS.FieldByName(FNm + 'l').Value := GetObjFieldValue(FSS, C, SrcDS.FieldByName(SrcFNm).Value, True);
            DS.FieldByName(FNm).Value := SrcDS.FieldByName(SrcFNm).Value;
          end
          else if C is TdxFile then
          begin
            DS.FieldByName(FNm).Value := SrcDS.FieldByName(SrcFNm).Value;
            DS.FieldByName(FNm + 'd').Value := SrcDS.FieldByName(SrcFNm + 'd').Value;
            DS.FieldByName(FNm + 'dest').Value := SrcDS.FieldByName(SrcFNm + 'dest').Value;
            DS.FieldByName(FNm + 'src').Value := SrcDS.FieldByName(SrcFNm + 'src').Value;
            // Просто меняем значение поля для определения, что blob был изменен.
            DS.FieldByName(FNm + 'c').AsInteger:=DS.FieldByName(FNm + 'c').AsInteger+1;
          end
          else if C is TdxDBImage then
          begin
            DS.FieldByName(FNm).Value := SrcDS.FieldByName(SrcFNm).Value;
            DS.FieldByName(FNm + 'thumb').Value := SrcDS.FieldByName(SrcFNm + 'thumb').Value;
            DS.FieldByName(FNm + 'dest').Value := SrcDS.FieldByName(SrcFNm + 'dest').Value;
            DS.FieldByName(FNm + 'src').Value := SrcDS.FieldByName(SrcFNm + 'src').Value;
            // Просто меняем значение поля для определения, что blob был изменен.
            DS.FieldByName(FNm + 'c').AsInteger:=DS.FieldByName(FNm + 'c').AsInteger+1;
          end
          else
            DS.FieldByName(FNm).Value:=SrcDS.FieldByName(SrcFNm).Value;
        end;
    end
    else
    begin
      for i := 0 to FL.Count - 1 do
      begin
        C := TdxField(FL[i]);
        SetDSField(C, Null);
        {if C is TdxLookupComboBox then
          ClearObject(C)
        else if C is TdxFile then
          ClearFile(C)
        else if C is TdxDBImage then
          ClearImage(C)
        else
	        GetDSField(C).SetData(nil);  }
      end;
    end;
  end;
  FL.Free;
  SrcFL.Free;
  ObjRS.Free;
end;

procedure TSsRecordSet.AssignForm(AValue: TdxForm);

  procedure DoScript(RS: TSsRecordSet);
  var
    SD: TScriptData;
  begin
    SD := FSS.MetaData.ScriptMan.FindScript(RS.Form.Id);
    try
      if SD <> nil then
      begin
        RS.RunScript.SD := SD;
        RS.RunScript.LoadBin;
        RS.RunScript.BindVars;
        RS.RunScript.BindForm(RS);
        RS.RunScript.TryRunProc('FORM_CREATE', []);
      end;
      if FSS.OnCreateForm <> nil then FSS.OnCreateForm(FSS, RS.Form);
      RunAction(RS.Form.ActionOnCreate, RS);

    except
      on E: EPSException do
        RS.Form.Errs.Add(le2br(EPSExceptionToString(E)));
      on E: Exception do
        RS.Form.Errs.Add(le2br(E.Message));
    end;
  end;

var
  L: TList;
  i: Integer;
  RS: TSsRecordSet;
  EB: TExpressionBuilder;
  ED: TExprData;
  C: TdxComponent;
begin
  FFormAssigned := True;
  FForm := FSS.MetaData.LoadForm(AValue.Id);
  CheckVisibleControls(FSS, FForm);
  FForm.RecordSet := Self;

  EB := TExpressionBuilder.Create;
  EB.RecordSet := Self;
  EB.SkipLabels := True;

  L := TList.Create;

  FForm.GetOrderedFields(FFields);

  FForm.GetCalcFields(L);
  for i := 0 to L.Count - 1 do
    try
      ED := TExprData.Create;
      ED.F := TdxField(L[i]);
      ED.Expr := GetExpression(ED.F);
      ED.E := EB.Build(ED.Expr);
      if ED.E <> nil then
        FExprs.Add(ED)
      else
        ED.Free;
    except
      on E: Exception do
      begin
        FForm.Errs.AddObject(E.Message, TObject(PtrInt(TdxField(L[i]).Id)));
        ED.Free;
      end;
    end;

  EB.SkipLabels := False;
  FForm.GetCalcLabels(L);
  for i := 0 to L.Count - 1 do
    try
      ED := TExprData.Create;
      ED.L := TdxLabel(L[i]);
      ED.Expr := ED.L.Expression;
      ED.E := EB.Build(ED.Expr);
      if ED.E <> nil then
        FCalcLabels.Add(ED)
      else
        ED.Free;
    except
      on E: Exception do
      begin
        FForm.Errs.Add(E.Message);
        ED.Free;
      end;
    end;

  EB.Free;

  if FForm.PId = 0 then
  begin
    FForm.GetTables(L);
    for i := 0 to L.Count - 1 do
    begin
      RS := TSsRecordset.Create(FSS, Self);
      RS.AssignForm(FSS.FormMan.FindForm(TdxGrid(L[i]).Id));
      FForms.Add(RS);
    end;
  end;

  FForm.GetQueries(L);
  for i := 0 to L.Count - 1 do
  begin
    RS := TSsRecordset.Create(FSS, Self);
    RS.QGrid := TdxQueryGrid(L[i]);
    RS.QGrid.RecordSet := RS;
    RS.AssignReport( FSS.ReportMan.FindReport(RS.QGrid.Id) );
    {RS.RD := FSS.ReportMan.FindReport(RS.QGrid.Id);
    RS.InitColoring;  }
    FQueries.Add(RS);
  end;

  L.Free;

  InitColoring;

  if FForm.PId = 0 then
  begin
    DoScript(Self);
    for i := 0 to FForms.Count - 1 do
      DoScript(FForms[i]);
  end;

  for i := 0 to FForm.ComponentCount - 1 do
  begin
    C := FForm.Components[i];
    if C is TdxControl then
      with TdxControl(C) do
      begin
        OnPropertyChange:=@ControlPropertyChange;
        // Установка свойств в соответствии с Hidden
        {if Hidden then
        begin
          Visible := False;
          if C is TdxTabSheet then
            TdxTabSheet(C).TabVisible := False;
        end;   }
      end;
    if C is TdxGrid then
      TdxControl(C).Font := FForms.FindFormById(TdxGrid(C).Id).Form.Grid.Font
    else if C is TdxQueryGrid then
      TdxControl(C).Font := FQueries.FindRpById(TdxQueryGrid(C).Id).RD.Grid.Font;
  end;
end;

procedure TSsRecordSet.AssignReportForm(AValue: TdxForm; RD: TReportData);
var
  L: TList;
  //i: Integer;
  RS: TSsRecordSet;
  EB: TExpressionBuilder;
begin
  FForm := AValue;

  EB := TExpressionBuilder.Create;
  EB.RecordSet := Self;
  EB.SkipLabels := True;

  L := TList.Create;

  FForm.GetOrderedFields(FFields);

  RS := TSsRecordset.Create(FSS, Self);
  RS.RD := RD;
  RS.QGrid := TdxQueryGrid(FForm.FindComponent('QGrid'));
  RS.QGrid.RecordSet := RS;
  FQueries.Add(RS);

  EB.Free;
  L.Free;
end;

procedure TSsRecordSet.AssignReport(AValue: TReportData);
var
  St: TMemoryStream;
begin
  FReportAssigned := True;
  FRD := TReportData.Create;
  FRD.Session := FSS;
  St := TMemoryStream.Create;
  AValue.SaveToStream(St);
  St.Position := 0;
  FRD.LoadFromStream(St);
  St.Free;
  InitColoring;
  InitRpGrid;
end;

function TSsRecordSet.GetRecId: Integer;
begin
  Result := FDataSet.FieldByName('id').AsInteger;
end;

procedure TSsRecordSet.SetCallerRS(AValue: TSsRecordSet);
begin
  if FCallerRS=AValue then Exit;
  if AValue <> nil then
    AValue.FFreeCallerList.Add(Self)
  else
    FCallerRS.FFreeCallerList.Remove(Self);
  FCallerRS:=AValue;
end;

procedure TSsRecordSet.SetNeedRequeryLinkedQueries(AChangedQuery: TSsRecordSet);
var
  i: Integer;
  QRS: TSsRecordSet;
begin
  for i := 0 to FQueries.Count - 1 do
  begin
    QRS := FQueries[i];
    if {not QRS.QGrid.ManualRefresh and} (QRS <> AChangedQuery) and QRS.RD.QueryExistsInExpr(AChangedQuery.RD.Name) then
    begin
      QRS.NeedRequery := True;
      SetNeedRequeryLinkedQueries(QRS);
    end;
  end;
end;

procedure TSsRecordSet.RequeryQueries(const AChangedField: String);
var
  i: Integer;
  QRS: TSsRecordSet;
begin
  for i := 0 to FQueries.Count - 1 do
  begin
    QRS := FQueries[i];
    if {not QRS.QGrid.ManualRefresh and} QRS.RD.FieldExistsInExpr(AChangedField) then
    begin
      QRS.NeedRequery := True;
      SetNeedRequeryLinkedQueries(QRS);
    end;
  end;

  if FState = [] then
  begin
    for i := 0 to FQueries.Count - 1 do
    begin
      QRS := FQueries[i];
      if not QRS.QGrid.ManualRefresh and QRS.NeedRequery then FChangedQueries.AddItem(QRS);
    end;
    for i := 0 to FQueries.Count - 1 do
    begin
      QRS := FQueries[i];
      if not QRS.QGrid.ManualRefresh and QRS.NeedRequery then QRS.Open;
    end;
  end;
end;

procedure TSsRecordSet.EvalAggLabels(const AFormName: String);
var
  Lbl: TdxLabel;
  i: Integer;
  ED: TExprData;
begin
  for i := 0 to FCalcLabels.Count - 1 do
  begin
    ED := TExprData(FCalcLabels[i]);
    Lbl := ED.L;
    if FormExistsInExpr(AFormName, ED.Expr) then
    begin
      if Lbl.Value = Unassigned then
        try
          Lbl.Value := ED.E.Calc;
        except
          on Ex: Exception do
          begin
            Lbl.Value := Lbl.FieldName;
            FForm.Errs.Add(Lbl.FieldName + ': ' + Ex.Message);
          end;
        end;
      Lbl.Caption := VarToStr(Lbl.Value);
      EvalLabels(Lbl.FieldName);
    end;
  end;

  {for i := 0 to FCalcLabels.Count - 1 do
  begin
    ED := TExprData(FCalcLabels[i]);
    if ED.L.Value <> Unassigned then
      FChangedLabels.AddItem(ED.L);
  end;}
end;

procedure TSsRecordSet.EvalAggFields(const AChangedForm: String);
var
  i: Integer;
  F: TdxField;
  //V: Variant;
  ED: TExprData;
begin
  for i := 0 to FExprs.Count - 1 do
  begin
    ED := TExprData(FExprs[i]);
    F := ED.F;
    try
      if (AChangedForm = '') or FormExistsInExpr(AChangedForm, ED.Expr) then
        SetDSField(F, ED.E.Calc);
    except
      on E: Exception do
        FForm.Errs.AddObject(F.FieldName + ': ' + E.Message, TObject(PtrInt(F.Id)));
    end;
  end;
end;

procedure TSsRecordSet.ClearChanges;
begin
  FChangedTimers.Clear;
  FChangedFields.Clear;
  FChangedTables.Clear;
  FChangedQueries.Clear;
  FChangedProps.Clear;
  if FForm <> nil then FForm.Errs.Clear;
  ClearCalcLabels;
  FGotoUrl := '';
  FDocUrl := '';
  FPrintErrors := '';
  //FreeAndNil(FActions);
  ClearList(FActionList);
  MsgInfo.Msg := '';
  MsgInfo.MsgType := mtWarning;
  MsgInfo.Buttons := [];
  MsgInfo.Visible := False;
end;

procedure TSsRecordSet.AddToHistory;
var
  S: String;
  n, i: Integer;
  //L: TList;
  FD: TSsFormData;
  F: TdxField;
begin
  if (FForm.PId <> 0) or (FForm.ViewType = vtSimpleForm) then Exit;

  //L := TList.Create;
  //FForm.GetFields(L);

  S := '';
  n := 2;
  if FFields.Count - 1 < n then n := FFields.Count - 1;
  for i := 0 to FFields.Count - 1 do
  begin
    F := TdxField(FFields[i]);
    if F is TdxLookupComboBox then
      S := S + GetObjValue(F) + ' '
    else if F is TdxFile then
      S := S + VarToStr(GetFieldValue(F.FieldName)) + ' '
    else if not (F is TdxDBImage) then
      S := S + GetDSField(F).AsString + ' '
    else
      Continue;
    Dec(n);
    if n < 0 then Break;
  end;

  if Trim(S) = '' then S := IntToStr(RecId);
  FD := FSs.FormList.GetForm(FForm);
  i := FD.LastEdits.IndexOfObject(TObject(PtrInt(RecId)));
  if i >= 0 then
    FD.LastEdits.Delete(i)
  else if FD.LastEdits.Count = 10 then
    FD.LastEdits.Delete(0);
  if Utf8Length(S) > 25 then S := Utf8Copy(S, 1, 22) + '...';
  FD.LastEdits.AddObject(S, TObject(PtrInt(RecId)));
  //L.Free;
end;

function TSsRecordSet.CheckRecordModify: Byte;
var
  S: String;
  i: Integer;
  F: TField;
begin
  Result := 0;                                            // Изменений нет
  S := SqlSimpleSelectStatement(FForm, RecId);
  with FSS.DBase.OpenDataSet(S) do
  try
    if RecordCount = 0 then Result := 1                   // Запись удалена другим пользователем
    else
      for i := 0 to Fields.Count - 1 do
      begin
        F := FDataSet.FieldByName(Fields[i].FieldName);
        if F.Value <> Fields[i].Value then
        begin
          Result := 2;                                    // Запись изменена другим пользователем
          Break;
        end;
      end;
  finally
    Free;
  end;
end;

function TSsRecordSet.CanAppend: TAccessStatus;
begin
  Result := asOk;
  with FSS do
    if (not UserMan.CheckFmAdding(RoleId, FForm.Id)) or ((FForm.PId > 0) and
      ((not UserMan.CheckFmEditing(RoleId, FForm.PId)) or
      (not Parent.CheckAccess(True))) and not (FParent.DataSet.State in [dsInsert, dsEdit])) then
      Result := asCantAppend;
end;

function TSsRecordSet.CanEdit: TAccessStatus;
var
  Ok: Boolean;
begin
  Result := asOk;
  //if FDataSet.RecordCount = 0 then Exit(asCantEdit);
  with FSS do
  begin
    Ok := UserMan.CheckFmEditing(RoleId, FForm.Id) and CheckAccess(True);
    if FForm.PId > 0 then
      Ok := Ok and UserMan.CheckFmEditing(RoleId, FForm.PId) and
        Parent.CheckAccess(True) and (Parent.DataSet.State in [dsEdit, dsInsert]);
    if Ok then
    begin
      {if FForm.PId = 0 then
        case CheckRecordModify of
          1: Result := asDeleted;
          2: Result := asModified;
        end; }
    end
    else
      Result := asCantEdit;
  end;
end;

function TSsRecordSet.CanDelete: TAccessStatus;
begin
  Result := CanEdit;
  if Result = asCantEdit then
    Result := asCantDelete
  else if Result = asOk then
    with FSS do
      if UserMan.CheckFmDeleting(RoleId, FForm.Id) and CheckAccess(False) then
      begin
        {if FForm.PId = 0 then
        begin
          if not CheckAccessDetails then Result := asCantDelete
          else if not CheckDeleteRecord then Result := asHasRef;
        end; }
      end
      else
        Result := asCantDelete;
end;

function TSsRecordSet.WhoEdit(ARecId: Integer): String;
var
  S: String;
  U: TdxUser;
begin
  Result := '';
  S := 'select uid from dx_lock where fmid=' + IntToStr(FForm.Id) + ' and recid=' +
    IntToStr(ARecId);
  with FSS.DBase.OpenDataSet(S) do
  begin
    if RecordCount > 0 then
    begin
      U := FSS.UserMan.Users.FindUser(Fields[0].AsInteger);
      if U <> nil then Result := U.Name;
    end;
    Free;
  end;
end;

function TSsRecordSet.MsgInfoToJson: TJsonObject;
var
  JsonBns: TJSONArray;
  n: TMsgDlgBtn;
begin
  Result := TJsonObject.Create(['type', Ord(MsgInfo.MsgType),
    'msg', StrToHtml(MsgInfo.Msg, True), 'title', StrToHtml(MsgInfo.Title)]);
  JsonBns := TJsonArray.Create;
  for n := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    if n in MsgInfo.Buttons then
      JsonBns.Add(MsgDlgBtnToStr(n));
  Result.Add('buttons', JsonBns);
end;

procedure TSsRecordSet.Recalculate(ATableName, AFieldName, aExpr: String);
var
  E: TExpression;
  EB: TExpressionBuilder;
  C: TdxField;
  TblRS: TSsRecordSet;
begin
  if ATableName <> '' then
  begin
    TblRS := FForms.FindFormByName(ATableName);
    C := TblRS.Form.FindFieldByName(AFieldName);
  end
  else
    C := FForm.FindFieldByName(AFieldName);

  aExpr := Trim(aExpr);
  if aExpr = '' then
    aExpr := Trim(GetExpression(C));
  if aExpr = '' then Exit;

  EB := TExpressionBuilder.Create;
  EB.SkipLabels := True;
  if ATableName = '' then
    EB.RecordSet := Self
  else
    EB.RecordSet := TblRS;
  try
    E := EB.Build(aExpr);
  finally
    EB.Free;
  end;

  if E = nil then Exit;

  try
    Include(FState, rstRecalculate);
    FDataSet.First;
    while not FDataSet.EOF do
    begin
      FDataSet.Edit;
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
        SetDSField(C, E.Calc);
      Post;
      FDataSet.Next;

    end;

  finally
    FreeAndNil(E);
    Exclude(FState, rstRecalculate);
  end;
end;

procedure TSsRecordSet.BeginDuplicate;
begin
  Include(FState, rstDuplicate);
  if FForm.OnBeforeDuplicate <> nil then FForm.OnBeforeDuplicate(FForm);
end;

procedure TSsRecordSet.EndDuplicate;
begin
  Exclude(FState, rstDuplicate);
  if FForm.OnAfterDuplicate <> nil then FForm.OnAfterDuplicate(FForm);
  UpdateAccessState;
end;

function TSsRecordSet.GetCurrentForm: TdxForm;
var
  RS: TSsRecordSet;
begin
  RS := GetFormRecordSet;
  if RS <> nil then Result := RS.Form
  else Result := nil;
  //Result := FForm;
  //if (Result = nil) and (Parent <> nil) then Result := Parent.Form;
end;

{function TSsRecordSet.GetTopParent: TSsRecordSet;
begin
  Result := Parent;
  if (Result <> nil) and (Result.Parent <> nil) then Result := Result.Parent;
end;}

function TSsRecordSet.GetFormRecordset: TSsRecordSet;
begin
  if Form <> nil then Exit(Self)
  else if Parent <> nil then Result := Parent.GetFormRecordSet
  else Result := nil;
end;

procedure TSsRecordSet.EvalLabels(const AChangedField: String);
var
  CalcCounter: Integer;

  procedure InnerEvalLabels(const ChangedField: String);
  var
    Lbl: TdxLabel;
    i: Integer;
    ED: TExprData;
  begin
    // Обнаружения зацикливаний
    if CalcCounter > 100 then
      raise Exception.CreateFmt(rsLoopDetectedCalc, [ChangedField]);
    Inc(CalcCounter);
    //

    for i := 0 to FCalcLabels.Count - 1 do
    begin
      ED := TExprData(FCalcLabels[i]);
      Lbl := ED.L;
      if (ChangedField = '') or (FieldExists(ChangedField, ED.Expr) and
        (MyUtf8CompareText(Lbl.FieldName, AChangedField) <> 0)) then
      begin
        if Lbl.Value = Unassigned  then
          try
            Lbl.Value := ED.E.Calc;
          except
            on Ex: Exception do
            begin
              Lbl.Value := Lbl.FieldName;
              FForm.Errs.Add(Lbl.FieldName + ': ' + Ex.Message);
            end;
          end;
        Lbl.Caption := VarToStr(Lbl.Value);
        if Lbl.FieldName <> ChangedField then
          InnerEvalLabels(Lbl.FieldName);
      end;
    end;

    {for i := 0 to FCalcLabels.Count - 1 do
    begin
      ED := TExprData(FCalcLabels[i]);
      if ED.L.Value <> Unassigned then
        FChangedLabels.AddItem(ED.L);
    end;  }

    Dec(CalcCounter);
  end;

begin
  CalcCounter := 0;
  try
    InnerEvalLabels(AChangedField);
  except
    on Ex: Exception do
      FForm.Errs.Add(Ex.Message);
  end;
end;

procedure TSsRecordSet.EvalFields(const AChangedField: String);
var
  i: Integer;
  F: TdxField;
  ED: TExprData;
begin
  // Обнаружения зацикливаний
  if FCalcCounter > 100 then
  begin
    FCalcCounter := 0;
    raise Exception.CreateFmt(rsLoopDetectedCalc, [AChangedField]);
  end;
  Inc(FCalcCounter);
  //

  for i := 0 to FExprs.Count - 1 do
  begin
    ED := TExprData(FExprs[i]);
    F := ED.F;
    try
      if FieldExists(AChangedField, ED.Expr) and (MyUtf8CompareText(ED.F.FieldName, AChangedField) <> 0) then
      begin
        SetDSField(F, ED.E.Calc);
        //GetDSField(F).Value := ED.E.Calc;
      end;
    except
      on E: Exception do
        FForm.Errs.AddObject(F.FieldName + ': ' + E.Message, TObject(PtrInt(F.Id)));
    end;
  end;
  Dec(FCalcCounter);
end;

procedure TSsRecordSet.ClearCalcLabels;
var
  i: Integer;
begin
  for i := 0 to FCalcLabels.Count - 1 do
    TExprData(FCalcLabels[i]).L.Value := Unassigned;
end;

procedure TSsRecordSet.AddToList(L: TList; O: TObject);
begin
  if L.IndexOf(O) < 0 then L.Add(O);
end;

function TSsRecordSet.GetDSField(F: TdxField): TField;
begin
  Result := FDataSet.FieldByName(FieldStr(F.Id));
end;

procedure TSsRecordSet.SetDSField(F: TdxField; V: Variant);
begin
  if F is TdxLookupComboBox then
  begin
    if V <> Null then
      FDataSet.FieldByName(FieldStr(F.Id) + 'l').Value := GetObjFieldValue(FSS, F, V, True)
    else
      FDataSet.FieldByName(FieldStr(F.Id) + 'l').Value := Null;
    GetDSField(F).Value := V;
  end
  else if F is TdxDBImage then
  begin
    if V <> Null then
      LoadImageFromFile(V, TdxDBImage(F), FDataSet)
    else
      ClearImage(F);
  end
  else if F is TdxFile then
  begin
    if V <> Null then
      LoadFileFromFile(V, tdxFile(F), FDataSet)
    else
      ClearFile(F);
  end
  else
    GetDSField(F).Value := V;
end;

function TSsRecordSet.GetFieldValue(const aFieldName: String): Variant;
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
  Fm := FForm;
  DS := FDataSet;

  try

  C := Fm.FindFieldByName(SL[0]);
  //if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [aFieldName]);

  for i := 0 to SL.Count - 1 do
  begin
    if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [aFieldName]);
    if (C is TdxLookupComboBox) and (i < SL.Count - 1) then
    begin
      V := DS.FieldByName(FieldStr(C.Id)).Value;
      Result := V;
      if V = Null then Break;
      Fm := FSS.FormMan.FindForm(GetSourceTId(C));
      if Fm = nil then Exit;
      if DS <> FDataSet then FreeAndNil(DS);
      Cbx := TdxLookupComboBox(C);

      C := Fm.FindFieldByName(SL[i+1]);
      if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [aFieldName]);
      // Если поле объекта отображается в компоненте, то значение берется из
      // компонента без запроса к базе.
      if (C.Id = Cbx.SourceFId) and (SL.Count = 2) then
      begin
        Result := FDataSet.FieldByName(FieldStr(Cbx.Id) + 'l').Value;
        Break;
      end
      else if C is TdxRecordId then Break;

      Tmp := SqlSelectGroups(FSS, Fm.Id, True, False);
      if Tmp <> '' then Tmp := '(' + Tmp + ')'
      else Tmp := TableStr(Fm.Id);

      DS := FSS.DBase.OpenDataSet('select ' + GetComponentDataSetFieldName(C)
        {FieldStr(C)} + ' from ' + Tmp + ' where id=' + VarToStr(V));
    end
    else
    begin
      Result := GetComponentFieldValue(DS, C);
      //Result := DS.FieldByName(FieldStr(C)).Value;
      Break;
    end;
  end;

  finally
    if DS <> FDataSet then FreeAndNil(DS);
    SL.Free;
  end;
end;

procedure TSsRecordSet.SetFieldChangeEvent;
var
  i: Integer;
  F: TdxField;
  FNm: String;
begin
  for i := 0 to FFields.Count - 1 do
  begin
    F := TdxField(FFields[i]);
    if (F is TdxDBImage) or (F is TdxFile) then
      FNm := FieldStr(F.Id) + 'src'
    else
      FNm := FieldStr(F.Id);
    with FDataSet.FieldByName(FNm) do
    begin
      OnChange := @FieldChange;
      Tag := i;
    end;
  end;
end;

procedure TSsRecordSet.ClearFieldRequired;
var
  i: Integer;
begin
  for i := 0 to FDataSet.Fields.Count - 1 do
    FDataSet.Fields[i].Required := False;
end;

function TSsRecordSet.CheckAccess(IsEdit: Boolean): Boolean;
var
  V: Variant;
  Cond: String;
begin
  Result := False;
  if IsEdit then
    Cond := FSS.UserMan.GetEditCond(FSS.RoleId, FForm.Id)
  else
    Cond := FSS.UserMan.GetDelCond(FSS.RoleId, FForm.Id);
  if Trim(Cond) = '' then Exit(True);

  V := EvalField(Self, Cond, True);
  if VarIsBool(V) then Result := V;
end;

procedure TSsRecordSet.SetQGrid(AValue: TdxQueryGrid);
begin
  if FQGrid=AValue then Exit;
  FQGrid:=AValue;
  //if FQGrid.ManualRefresh then FNeedRequery := False;
end;

function TSsRecordSet.CheckAccessDetails: Boolean;
var
  i: Integer;
  RS: TSsRecordSet;
  B: TBookMark;
begin
  Result := True;
  if (FForm.PId > 0) or (FDataSet.State = dsInsert) then Exit;
  for i := 0 to FForms.Count - 1 do
  begin
    RS := FForms[i];
    if not FSS.UserMan.CheckFmAdding(FSS.RoleId, RS.Form.Id) or
      not FSS.UserMan.CheckFmEditing(FSS.RoleId, RS.Form.Id) or
      not FSS.UserMan.CheckFmDeleting(FSS.RoleId, RS.Form.Id) then Exit(False);

    if RS.NeedRequery then RS.Open;
    B := RS.DataSet.GetBookmark;
    RS.DisableScrollEvents;
    RS.DataSet.First;
    try
      while not RS.DataSet.Eof do
      begin
        if not RS.CheckAccess(True) or not  RS.CheckAccess(False) then Exit(False);
        RS.DataSet.Next;
      end;
    finally
      RS.DataSet.GotoBookmark(B);
      RS.DataSet.FreeBookmark(B);
      RS.EnableScrollEvents;
    end;
  end;
end;

function TSsRecordSet.FieldChanged(F: TdxField): Boolean;
begin
  if (F is TdxDBImage) or (F is TdxFile) then
    with FDataSet.FieldByName(FieldStr(F.Id) + 'c') do
      Result := Value <> OldValue
  else
    with GetDSField(F) do
      Result := Value <> OldValue;
end;

procedure TSsRecordSet.LabelChanged(L: TdxLabel);
begin
  //ChangedLabels.AddItem(L);
  EvalLabels(L.FieldName);
end;

procedure TSsRecordSet.ClearObject(F: TdxField);
begin
	FDataSet.FieldByName(FieldStr(F.Id) + 'l').SetData(nil);
  FDataSet.FieldByName(FieldStr(F.Id)).SetData(nil);
end;

procedure TSsRecordSet.ClearImage(F: TdxField);
var
  FlNm, ImgName: String;
begin
  FlNm := FieldStr(F.Id);
  FDataSet.FieldByName(FlNm).SetData(nil);
  FDataSet.FieldByName(FlNm + 'thumb').SetData(nil);
  FDataSet.FieldByName(FlNm + 'src').SetData(nil);
  FDataSet.FieldByName(FlNm + 'dest').SetData(nil);
  FDataSet.FieldByName(FlNm + 'c').AsInteger := FDataSet.FieldByName(FlNm + 'c').AsInteger+1;

  ImgName := IntToStr(FForm.Id) + '-' + IntToStr(RecId) + '-' +
    IntToStr(F.Id) + '.png';
  DeleteFile(GetCachePath(FSS) + ImgName);
  ImgName := IntToStr(FForm.Id) + '-' + IntToStr(RecId) + '-' +
    IntToStr(F.Id) + 't.png';
  DeleteFile(GetCachePath(FSS) + ImgName);
end;

procedure TSsRecordSet.ClearFile(F: TdxField);
var
  FlNm: String;
begin
  FlNm := FieldStr(F.Id);
  FDataSet.FieldByName(FlNm).SetData(nil);
  FDataSet.FieldByName(FlNm + 'd').SetData(nil);
  FDataSet.FieldByName(FlNm + 'src').SetData(nil);
  FDataSet.FieldByName(FlNm + 'dest').SetData(nil);
  FDataSet.FieldByName(FlNm + 'c').AsInteger := FDataSet.FieldByName(FlNm + 'c').AsInteger+1;
end;

procedure TSsRecordSet.DisableScrollEvents;
begin
  FDataSet.AfterScroll := nil;
  FDataSet.BeforeScroll := nil;
  Inc(FScrollEventsCounter);
end;

procedure TSsRecordSet.EnableScrollEvents;
begin
  if FScrollEventsCounter > 0 then
    Dec(FScrollEventsCounter);
  if FScrollEventsCounter = 0 then
  begin
    FDataSet.AfterScroll := @DataSetAfterScroll;
    FDataSet.BeforeScroll := @DataSetBeforeScroll;
  end;
end;

function TSsRecordSet.ScrollEventsEnabled: Boolean;
begin
  Result := FDataSet.AfterScroll <> nil;
end;

constructor TSsRecordSet.Create(SS: TSession; AParent: TSsRecordSet);
begin
  FSS := SS;
  FParent := AParent;
  FForms := TSsRecordSets.Create;
  FQueries := TSsRecordSets.Create;
  FFields := TList.Create;
  FExprs := TList.Create;
  FCalcLabels := TList.Create;
  FFreeCallerList := TList.Create;
  FChangedFields := TUniList.Create;
  //FChangedLabels := TUniList.Create;
  FChangedTables := TUniList.Create;
  FChangedQueries := TUniList.Create;
  FChangedProps := TUniPropList.Create;
  FChangedTimers := TUniList.Create;
  FNeedRequery := True;
  FDataSet := TdxDataSet.Create(nil);
  FDataSet.PacketRecords := 100;
  TdxDataSet(FDataSet).RS := Self;
  FSS.DBase.AttachDataSet(FDataSet);

  FDataSet.AfterInsert:=@DataSetAfterInsert;
  FDataSet.AfterEdit:=@DataSetAfterEdit;
  FDataSet.AfterDelete:=@DataSetAfterDelete;
  FDataSet.AfterCancel:=@DataSetAfterCancel;
  FDataSet.AfterPost:=@DataSetAfterPost;
  FDataSet.AfterClose:=@DataSetAfterClose;
  FDataSet.AfterOpen:=@DataSetAfterOpen;
  FDataSet.AfterScroll:=@DataSetAfterScroll;
  FDataSet.BeforeOpen:=@DataSetBeforeOpen;
  FDataSet.BeforeInsert:=@DataSetBeforeInsert;
  FDataSet.BeforeEdit:=@DataSetBeforeEdit;
  FDataSet.BeforeDelete:=@DataSetBeforeDelete;
  FDataSet.BeforeScroll:=@DataSetBeforeScroll;
  FDataSet.BeforeCancel:=@DataSetBeforeCancel;
  FDataSet.BeforeClose:=@DataSetBeforeClose;
  FDataSet.BeforePost:=@DataSetBeforePost;

  FRunScript := TRunScript.Create(SS);
  FActionList := TList.Create;
end;

destructor TSsRecordSet.Destroy;

  procedure DoScript(RS: TSsRecordSet);
  var
    j: Integer;
  begin
    try
      for j := 0 to FQueries.Count - 1 do
        FQueries[j].DataSet.Close;
      RS.DataSet.Close;
      RS.RunScript.TryRunProc('FORM_DESTROY', []);
    except
      ;
    end;
  end;

var
  i: Integer;
begin
  ClearList(FActionList);
  FActionList.Free;

  if FFormAssigned and (FForm.PId = 0) then
  begin
    for i := FForms.Count - 1 downto 0 do
      DoScript(FForms[i]);
    DoScript(Self);
  end;
  //FreeAndNil(FActions);
  FRunScript.Free;
  FChangedTimers.Free;
  FChangedQueries.Free;
  FChangedTables.Free;
  //FChangedLabels.Free;
  FChangedFields.Free;
  FChangedProps.Free;
  ClearList(FCalcLabels);
  FCalcLabels.Free;
  for i := 0 to FFreeCallerList.Count - 1 do
    TSsRecordSet(FFreeCallerList[i]).FCallerRS := nil;
  FFreeCallerList.Free;
  ClearList(FExprs);
  FExprs.Free;
  FFields.Free;
  FreeAndNil(FMemDS);
  FreeAndNil(FDataSet);
  FQueries.Free;
  FForms.Free;
  if FFormAssigned then FForm.Free;
  if FReportAssigned then FRD.Free;
  if FCallerRS <> nil then CallerRS := nil;
  inherited Destroy;
end;

function TSsRecordSet.OpenRecordCount: Integer;
var
  SQL: String;
begin
  if FForm.PId = 0 then
  begin
    SQL := SQLSelectStatement(Self, nil, True, True, nil, '', -1, False, 0, 0, True);
    with FSS.DBase.OpenDataSet(SQL) do
    begin
      Result := Fields[0].AsInteger;
      Free;
    end;
  end
  else Result := 0;
end;

function TSsRecordSet.Open: Boolean;
var
  Wh, SQL: String;
  n: LongInt;
begin
  Result := True;
  if not FNeedRequery then Exit;
  FNeedRequery := False;

  try

  if FForm <> nil then
  begin
    DisableScrollEvents;
    FDataSet.Close;
    Wh := '';
    if FForm.PId > 0 then
      Wh := 'pid=' + IntToStr(FParent.RecId);
    FDataSet.SQL.Text := SqlSelectStatement(Self, nil, True, True, nil, Wh, -1,
      False, 0, 0, False);
    FDataSet.DeleteSQL.Text := 'delete from ' + TableStr(FForm.Id) + ' where id=:id';
    FDataSet.Open;
    SetDisplayFormat(FSS.FormMan, FForm, FDataSet);
    ClearFieldRequired;
    SetFieldChangeEvent;
    if FForm.PId > 0 then
    begin
      if FMemDS = nil then
        FMemDS := TdxMemDataSet.Create(nil);
      FMemDS.CopyFromDataset(FDataSet, True);
    end;
    EnableScrollEvents;
    if ScrollEventsEnabled then FDataSet.AfterScroll(FDataSet);
  end
  else if FRD <> nil then
  begin
    if rstKeepPos in FState then
    begin
      if FDataSet.Active then
      begin
        if FRD.IsSimple then n := RecId
        else n := FDataSet.RecNo;
        if FQryRecId > 0 then
        begin
          n := FQryRecId;
          FQryRecId := 0;
        end;
      end
      else n := 0;
    end;

    FDataSet.Close;
    FDataSet.ClearIndexes;
    FDataSet.IndexName := '';
    FDataSet.MaxIndexesCount := 100;

    SQL := SqlReportSelect(FRD, Self);
    if FDataSet.Active then Exit    // В процессе вычислений уже может быть открыт.
    else if SQL = '' then Exit(False);

    FDataSet.SQL.Text := SQL;
    FDataSet.DeleteSQL.Text := 'delete from rdb$database';
    DisableScrollEvents;
    FDataSet.Open;
    SetDisplayFormatQ(FRD, FDataSet);
    CalcQuery(Self);
    FilterQuery(Self);
    BuildSortIndexes(FRD, FDataSet);

    if (rstKeepPos in FState) and (n > 0) then
    begin
      if FRD.IsSimple then
        FDataSet.Locate('id', n, [])
      else
      begin
        FDataSet.First;
        FDataSet.MoveBy(n - 1);
      end;
    end;

    EnableScrollEvents;

    if [rstRecalculate, rstDuplicate] * FState = [] then
    begin
      if FQGrid.OnAfterOpen <> nil then FQGrid.OnAfterOpen(FQGrid);
    end;
    if ScrollEventsEnabled then FDataSet.AfterScroll(FDataSet);

    if FParent.DataSet.State in [dsInsert, dsEdit] then FParent.EvalAggFields(FRD.Name);
    FParent.EvalAggLabels(FRD.Name);
  end;

  except
    on E: Exception do
    begin
      if FRD <> nil then
        FParent.Form.Errs.Add(FRD.Name + ': ' + E.Message)
      else
        FForm.Errs.Add(FForm.FormCaption + ': ' + E.Message);
      Result := False;
    end;
  end;
end;

procedure TSsRecordSet.OpenRecords;
begin
  DisableScrollEvents;
  FDataSet.Close;
  FDataSet.SQL.Text := SqlSelectStatement(Self, nil, FForm.UseSelectCondition,
    False, nil, '', -1, False, 0, 0, False);
  FDataSet.DeleteSQL.Text := 'delete from ' + TableStr(FForm.Id) + ' where id=:id';
  FDataSet.Open;
  SetDisplayFormat(FSS.FormMan, FForm, FDataSet);
  ClearFieldRequired;
  SetFieldChangeEvent;
  EnableScrollEvents;
  if ScrollEventsEnabled then FDataSet.AfterScroll(FDataSet);
end;

procedure TSsRecordSet.OpenPage(AStart, ACount: Integer);
begin
  DisableScrollEvents;
  FDataSet.Close;
  FDataSet.SQL.Text := SqlSelectStatement(Self, nil, True,
    True, nil, '', -1, False, AStart, ACount, False);
  FDataSet.Open;
  SetDisplayFormat(FSS.FormMan, FForm, FDataSet);
  FNeedRequery := False;
end;

procedure TSsRecordSet.OpenRecord(ARecId: Integer; AUseSelCond: Boolean);
begin
  DisableScrollEvents;
  FDataSet.Close;
  FDataSet.SQL.Text := SqlSelectStatement(Self, nil, AUseSelCond,
    False, nil, '', ARecId, False, 0, 0, False);
  FDataSet.DeleteSQL.Text := 'delete from ' + TableStr(FForm.Id) + ' where id=:id';
  FDataSet.Open;
  SetDisplayFormat(FSS.FormMan, FForm, FDataSet);
  ClearFieldRequired;
  SetFieldChangeEvent;
  EnableScrollEvents;
  if ScrollEventsEnabled then FDataSet.AfterScroll(FDataSet);
  FNeedRequery := False;
end;

procedure TSsRecordSet.OpenDetails;
var
  i: Integer;
begin
  for i := 0 to FForms.Count - 1 do
    FForms[i].Open;
  for i := 0 to FQueries.Count - 1 do
    if not FQueries[i].QGrid.ManualRefresh then FQueries[i].Open;
end;

procedure TSsRecordSet.OpenObjectFields(FL: TList; ARecId: Integer);
begin
  DisableScrollEvents;
  FDataSet.Close;
  FDataSet.SQL.Text := SqlSelectStatement(Self, FL, False,
    False, nil, '', ARecId, False, 0, 0, False);
  FDataSet.Open;
  //SetDisplayFormat(FSS.FormMan, FForm, FDataSet);
  FNeedRequery := False;
end;

procedure TSsRecordSet.OpenReport;
begin
  FDataSet.AfterScroll := nil;
  FDataSet.Close;
  FDataSet.ClearIndexes;
  FDataSet.IndexName := '';
  FDataSet.MaxIndexesCount := 100;
  FDataSet.SQL.Text := SqlReportSelect(FRD, Self);
  FDataSet.DeleteSQL.Text := 'delete from rdb$database';
  FDataSet.Open;
  SetDisplayFormatQ(FRD, FDataSet);
  CalcQuery(Self);
  FilterQuery(Self);
  BuildSortIndexes(FRD, FDataSet);
end;

procedure TSsRecordSet.Close;
var
  i: Integer;
begin
  FDataSet.Close;
  FNeedRequery := True;
  for i := 0 to Forms.Count - 1 do
    Forms[i].Close;
  for i := 0 to FQueries.Count - 1 do
    FQueries[i].Close;
end;

procedure TSsRecordSet.RequeryAllQueries;
var
  Q: TSsRecordSet;
  i: Integer;
begin
  for i := 0 to FQueries.Count - 1 do
  begin
    Q := FQueries[i];
    if not Q.QGrid.ManualRefresh then
    begin
      Q.NeedRequery := True;
      Include(Q.FState, rstKeepPos);
    end;
  end;

  for i := 0 to FQueries.Count - 1 do
  begin
    Q := FQueries[i];
    if not Q.QGrid.ManualRefresh then Q.Open;
    Exclude(Q.FState, rstKeepPos);
  end;
end;

procedure TSsRecordSet.ChangeField(F: TdxField; V: Variant);
begin
  ClearChanges;

  //if FDataSet.State in [dsInsert, dsEdit] then
  SetDSField(F, V)
  //else
  //  FSS.Errs.Add(rsCanNotChangeRec);
end;

procedure TSsRecordSet.RevertFieldValue(F: TdxField);
begin
  ClearChanges;
  FChangedFields.AddItem(F);
end;

{procedure TSsRecordSet.QueryScroll(AId, ARow: Integer);
var
  QRS: TSsRecordSet;
  i: Integer;
begin
  ClearChanges;

  QRS := FQueries.FindRpById(AId);
  QRS.DataSet.MoveBy(ARow + 1 - QRS.DataSet.RecNo);
  if ARow = 0 then
    if QRS.ScrollEventsEnabled then QRS.DataSet.AfterScroll(FDataSet);
  for i := 0 to FQueries.Count - 1 do
    FQueries[i].Open;
end;   }

procedure TSsRecordSet.QueryApplyChangeSort(AId: Integer);
var
  QRS: TSsRecordSet;
begin
  {FChangedFields.Clear;
  FChangedLabels.Clear;
  FChangedQueries.Clear;
  FSS.Errs.Clear;
  ClearCalcLabels;}
  ClearChanges;

  QRS := FQueries.FindRpById(AId);
  FChangedQueries.Add(QRS);
  QRS.NeedRequery:=True;
  QRS.Open;
end;

function TSsRecordSet.FindRecordSetByName(const AName: String): TSsRecordSet;
var
  i: Integer;
begin
  Result := nil;
  if (FForm <> nil) and (MyUtf8CompareText(FForm.FormCaption, AName) = 0) then Exit(Self)
  else if (FRD <> nil) and (MyUtf8CompareText(RD.Name, AName) = 0) then Exit(Self);

  for i := 0 to FForms.Count - 1 do
    if MyUtf8CompareText(FForms[i].Form.FormCaption, AName) = 0 then Exit(FForms[i]);
  for i := 0 to FQueries.Count - 1 do
    if MyUtf8CompareText(FQueries[i].RD.Name, AName) = 0 then Exit(FQueries[i]);
end;

procedure TSsRecordSet.Append;
begin
  FDataSet.Append;
end;

procedure TSsRecordSet.Insert;
begin
  FDataSet.Insert;
end;

procedure TSsRecordSet.Edit;
begin
  FDataSet.Edit;
end;

procedure TSsRecordSet.Post;
begin
  if FDataSet.State in [dsInsert, dsEdit] then FDataSet.Post;
end;

procedure TSsRecordSet.Cancel;
begin
  FDataSet.Cancel;
end;

function TSsRecordSet.CheckDeleteRecord: Boolean;
var
  i, j: Integer;
  Fm: TdxForm;
  L: TList;
  F: TdxField;
  SQL: String;
  LCbx: TdxLookupComboBox;
begin
  Result := True;
  L := TList.Create;
  try

  for i := 0 to FSS.FormMan.FormCount - 1 do
  begin
    Fm := FSS.FormMan.Forms[i];
    L.Clear;
    Fm.GetFields(L);
    for j := 0 to L.Count - 1 do
    begin
      F := TdxField(L[j]);
      if F is TdxLookupComboBox then
      begin
        LCbx := TdxLookupComboBox(F);
        if (LCbx.SourceTId = FForm.Id) and (LCbx.SourceFId <> 0) then
        begin
          SQL := 'select id from ' + TableStr(Fm.Id) + ' where ' + FieldStr(LCbx.Id) +
            '=' + IntToStr(RecId);
          with FSS.DBase.OpenDataSet(SQL) do
          try
            if RecordCount > 0 then Exit(False);
          finally
            Free;
          end;
        end;
      end;
    end;
  end;

  finally
    L.Free;
  end;
end;

procedure TSsRecordSet.UpdateAccessState;
var
  i: Integer;
begin
  if (FDataSet.State = dsInsert) or ((FParent <> nil) and (FParent.DataSet.State = dsInsert)) then
  begin
    FEditing := asOk;
    FDeleting := asOk;
  end
  else if FDataSet.State = dsEdit then
  begin
    FEditing := asOk;
    FDeleting := CanDelete;
  end
  else
  begin
    FEditing := CanEdit;
    FDeleting := CanDelete;
  end;
  if FForm.PId = 0 then
    for i := 0 to FForms.Count - 1 do
      if FForms[i].Form.Opened then FForms[i].UpdateAccessState;
end;

procedure TSsRecordSet.InitColoring;
var
  EB: TExpressionBuilder;
  i: Integer;
  CD: TColoringData;
  CL: TColoringList;
begin
  if FRD <> nil then
    CL := FRD.Coloring
  else
    CL := FForm.Coloring;

  EB := TExpressionBuilder.Create;
  EB.RecordSet := Self;
  EB.SkipLabels := True;
  EB.UseMemDS := (FForm <> nil) and (FForm.PId > 0);

  for i := 0 to CL.Count - 1 do
  begin
    CD := CL[i];
    try
      CD.E := EB.Build(CD.Expr);
    except
      on E: Exception do
        Break;
    end;
  end;

  EB.Free;
end;

procedure TSsRecordSet.DoneColoring;
var
  i: Integer;
  CD: TColoringData;
begin
  if FForm = nil then Exit;
  for i := 0 to FForm.Coloring.Count - 1 do
  begin
    CD := FForm.Coloring[i];
    CD.E.Free;
    CD.E := nil;
  end;
end;

procedure TSsRecordSet.InitRpGrid;
var
  i: Integer;
  pF: PRpField;
  C: TdxDBImage;
  Col: TRpGridColumn;
begin
  for i := 0 to FRD.GetRpSQLFieldCount - 1 do
  begin
    if FRD.GetFieldType(i) <> flImage then Continue;

    Col := FRD.Grid.FindColumnByFieldNameDS(FRD.GetFieldNameDS(i));

    pF := FRD.TryGetRpField(i);
    if pF <> nil then
    begin
      C := TdxDBImage(GetRpFieldComponent(FSS, pF^, True));
      Col.IsImage := True;
      Col.ThumbSize := C.ThumbSize;
    end;
  end;
end;

procedure TSsRecordSet.ParentFormSetModified;
begin
  Parent.DataSet['id'] := Parent.DataSet['id'];
end;

function TSsRecordSet.Delete: TAccessStatus;

  procedure DeleteChildRecords(RecId: Integer);
  var
    SQL: String;
    i: Integer;
  begin
    SQL := '';
    for i := 1 to FForms.Count - 1 do
    begin
      SQL := SQL + 'delete from ' + TableStr(FForms[i].Form.Id) + ' where pid=' +
        IntToStr(RecId) + ';';
    end;
    FSS.DBase.Execute(SQL);
  end;

var
  n, RId: Integer;
begin
  if FDataSet.State = dsInsert then
  begin
    FDataSet.Delete;
    Exit(asOk);
  end;

  if (FForm.PId = 0) and not CheckDeleteRecord then Exit(asHasRef);

  RId := RecId;
  FDataSet.Delete;
  if FForm.PId = 0 then
  begin
    DeleteChildRecords(RId);
    FSS.DBase.ApplyDataSet(TSQLQuery(FDataSet));
    FSS.DBase.Commit;
  end
  else
  begin
    FParent.EvalAggFields(FForm.FormCaption);
    FParent.EvalAggLabels(FForm.FormCaption);
    if (FParent.FState = []) and (FState = []) then FParent.FChangedTables.AddItem(Self);
  end;
  if FMemDS <> nil then
  begin
    if FMemDS.Locate('id', RId, []) then
      FMemDS.Delete
    else
      raise Exception.Create('TSsRecordSet.Delete - can not locate record.');
  end;

  if FForm.PId = 0 then
  begin
    with FSS.FormList.GetForm(FForm).LastEdits do
    begin
      n := IndexOfObject(TObject(PtrInt(RId)));
      if n >= 0 then Delete(n)
    end;
  end
  else
    ParentFormSetModified;

  if FForm.OnAfterDelete <> nil then FForm.OnAfterDelete(FForm);
  Result := asOk;
end;

{procedure TSsRecordSet.DeleteAll;
begin
  FDataSet.First;
  while not FDataSet.EOF do
    FDataSet.Delete;
end;}

procedure TSsRecordSet.CalcAllLabels;
begin
  ClearCalcLabels;
  EvalLabels('');
end;

function TSsRecordSet.Validate: Boolean;
var
  i: Integer;
  F: TdxField;
  Expr, S: String;
begin
  Result := True;
  if not (FDataSet.State in [dsInsert, dsEdit]) then Exit;

  FForm.Errs.Clear;

  if FForm.PId = 0 then
  begin
    for i := 0 to FForms.Count - 1 do
      if FForms[i].Validate then FForms[i].Post
      else Exit(False);
  end;

  for i := 0 to FFields.Count - 1 do
  begin
    F := TdxField(FFields[i]);
    if GetRequired(F) and (GetComponentFieldValue(FDataSet, F) = Null) and
      not (F is TdxCounter) and not IsHierarchyObj(FSS, F) then
    begin
      Result := False;
      FForm.Errs.AddObject(Format(rsRequiredMsg, [F.FieldName]), TObject(PtrInt(F.Id)));
    end;
  end;
  if not Result then Exit;
  for i := 0 to FFields.Count - 1 do
  begin
    F := TdxField(FFields[i]);
    Expr := Trim(GetCheckExpression(F));
    if Expr <> '' then
    begin
      try
        S := VarToStr(EvalField(Self, Expr, True));
        if S <> '' then
        begin
          FForm.Errs.AddObject(F.FieldName + ': ' + S, TObject(PtrInt(F.Id)));
          Exit(False);
        end;
      except
        on E: Exception do
        begin
          FForm.Errs.AddObject(F.FieldName + ': ' + E.Message, TObject(PtrInt(F.Id)));
          Exit(False);
        end;
      end;
    end;
  end;
  if FForm.OnValidate <> nil then FForm.OnValidate(FForm, Result);
end;

function TSsRecordSet.GetObjValue(F: TdxField): String;
begin
  Result := FDataSet.FieldByName(FieldStr(F.Id) + 'l').AsString;
end;

{ TSsFormData }

constructor TSsFormData.Create;
begin
  FLastEdits := TStringList.Create;
end;

destructor TSsFormData.Destroy;
begin
  FreeAndNil(FFilter);
  FLastEdits.Free;
  inherited Destroy;
end;

{ TSsFormList }

function TSsFormList.GetForms(Index: Integer): TSsFormData;
begin
  Result := TSsFormData(Items[Index]);
end;

procedure TSsFormList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Forms[i].Free;
  inherited Clear;
end;

function TSsFormList.AddForm(FmId: Integer): TSsFormData;
begin
  Result := TSsFormData.Create;
  Result.Id := FmId;
  Add(Result);
end;

function TSsFormList.FindForm(FmId: Integer): TSsFormData;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Forms[i].Id = FmId then Exit(Forms[i]);
end;

function TSsFormList.GetForm(Fm: TdxForm): TSsFormData;
begin
  Result := FindForm(Fm.Id);
  if Result = nil then
  begin
    Result := AddForm(Fm.Id);
    Result.Filter := TFilterObject.Create(Fm);
    if Fm.Filters.Count > 0 then
      Result.Filter.Load(Fm.Filters.ValueFromIndex[0]);
  end;
end;

{procedure TSsFormList.DeleteForm(Fm: TdxForm);
var
  FD: TSsFormData;
begin
  FD := FindForm(Fm);
  if FD <> nil then Remove(FD);
end;   }

{ TSession }

function TSession.GetFormMan: TFormManager;
begin
  Result := FMetaData.FormMan;
end;

function TSession.GetDXMain: TDXMain;
begin
  Result := MetaData.Main;
end;

function TSession.GetImageMan: TImageManager;
begin
  Result := FMetaData.ImageMan;
end;

function TSession.GetReportMan: TReportManager;
begin
  Result := FMetaData.ReportMan;
end;

function TSession.GetUserMan: TdxUserManager;
begin
  Result := FMetaData.UserMan;
end;

function TSession.GetScriptMan: TScriptManager;
begin
  Result := FMetaData.ScriptMan;
end;

procedure TSession.SetMetaData(AValue: TMetaData);
begin
  if FMetaData=AValue then Exit;
  FMetaData:=AValue;
end;

constructor TSession.Create;
begin
  FFormList := TSsFormList.Create;
  FVarList := TVarList.Create;
  FRecordSets := TSsRecordSets.Create;
  FRpRecordSets := TSsRecordSets.Create;
  FDBase := TDBEngine.Create;
  FRunScript := TRunScript.Create(Self);
  FExtRunMan := TExtRunManager.Create;
  FExtRunMan.Session := Self;
  FPage := 1;
  InitCriticalSection(FLock);
end;

destructor TSession.Destroy;
begin
  DebugStr('DoneCriticalSection', Self);
  DoneCriticalsection(FLock);
  DebugStr('FRecordSets.Free', Self);
  FRpRecordSets.Free;
  FRecordSets.Free;
  DebugStr('FExtRunMan.Free', Self);
  FExtRunMan.Free;
  DebugStr('FRunScript.Free', Self);
  FRunScript.Free;
  DebugStr('FVarList.Free', Self);
  FVarList.Free;
  DebugStr('FFormList.Free', Self);
  FFormList.Free;
  // Имеет смысл удалять папку, если есть метаданные
  if (FBrowserId <> '') and (MetaData <> nil) then
  begin
    DebugStr('Delete session dir: ' + GetCachePath(Self), Self);
    if not DeleteDirectory(GetCachePath(Self), False) then
    begin
      if DirectoryExists(GetCachePath(Self)) then
        DebugStr('Can not delete session directory ' + GetCachePath(Self), Self);
    end;
  end;
  DebugStr('FDBase.Disconnect', Self);
  if FDBase.Connected then FDBase.Disconnect;
  DebugStr('FDBase.Free', Self);
  FDBase.Free;
  inherited Destroy;
end;

procedure TSession.ConnectDB(const AConnectName: String);
begin
  FDBItem := AppSet.DBList.FindItem(AConnectName);
  if FDBItem = nil then raise Exception.CreateFmt(rsConnectionNotFound,
    [AConnectName]);
  FDBase.Database := FDBItem.DatabasePath;
  FDBase.DBPwd := FDBItem.DBPwd;
  FDBase.Connect;
  FConnectTime := Now;
  //FUserMan.LoadFromDb;
end;

procedure TSession.Clear;
begin
  FFormId := 0;
  FPage := 1;
  FRecId := 0;
  FTableId := 0;
  FTableRecId := 0;
  FCId := 0;
  FRpId := 0;
  FFilterId := 0;
end;

function TSession.GetFmId: Integer;
begin
  if FTableId = 0 then
    Result := FFormId
  else
    Result := FTableId;
end;

function TSession.GetRecId: Integer;
begin
  if FTableId = 0 then
    Result := FRecId
  else
    Result := FTableRecId;
end;

{procedure TSession.InitFormData;
var
  i: Integer;
  Fm: TdxForm;
  FD: TSsFormData;
begin
  for i := 0 to FormMan.FormCount - 1 do
  begin
    Fm := FormMan.Forms[i];
    FD := Forms.AddForm(Fm.Id);
    if Fm.Filters.Count > 0 then FD.Filter := Fm.Filters.ValueFromIndex[0];
  end;
end;    }

function TSession.CreateForm(const FormName: String): TdxForm;
var
  Fm: TdxForm;
  RS: TSsRecordSet;
begin
  Fm := FormMan.FindFormByName(FormName);
  if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [FormName]);
  if Fm.PId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormName]);
  RS := AddRecordSet(Fm);
  //RS := TSsRecordSet.Create(Self, nil);
  //RS.AssignForm(Fm);
  Result := RS.Form;
end;

function TSession.SQLSelect(const SQL: String): TdxSQLQuery;
begin
  Result := AppUtils.SQLSelect(Self, SQL);
end;

procedure TSession.SQLExecute(const SQL: string);
begin
  AppUtils.SQLExecute(Self, SQL);
end;

function TSession.EvalExpr(const Expr: String; Fm: TdxForm): Variant;
var
  RS: TSsRecordSet;
begin
  if Fm = nil then
    RS := TSsRecordSet.Create(Self, nil)
  else
    RS := TSsRecordSet(Fm.RecordSet);
  try
    Result := Expressions.EvalExpr(Expr, RS);
  finally
    if Fm = nil then RS.Free;
  end;
end;

function TSession.FindForm(const FormName: String; ARecId: Integer): TdxForm;
var
  i: Integer;
  RS: TSsRecordSet;
begin
  Result := nil;
  for i := 0 to FRecordSets.Count - 1 do
  begin
    RS := FRecordSets[i];
    if (RS.Form <> nil) and RS.Form.Opened and
      (MyUtf8CompareText(FormName, RS.Form.FormCaption) = 0) and (RS.RecId = ARecId)
      then Exit(RS.Form);
  end;
end;

function TSession.GetCacheDir: String;
begin
  Result := AppUtils.GetCachePath(Self);
end;

procedure TSession.Debug(Value: Variant);
begin
  if FDebugShow then
    FDebugMsg := FDebugMsg + VarToStr(Value) + LineEnding ;
  FDebugText := FDebugText + VarToStr(Value) + LineEnding;
  FDebugShow := True;
end;

function TSession.GetCurrentUser: String;
var
  U: TdxUser;
begin
  Result := '';
  U := UserMan.Users.FindUser(FUserId);// CurrentUser;
  if U <> nil then Result := U.Name;
end;

function TSession.GetCurrentRole: String;
var
  R: TdxRole;
begin
  Result := '';
  R := UserMan.Roles.FindRole(FRoleId);
  if R = nil then Exit;
  Result := R.Name;
end;

function TSession.GetCurrentDatabase: String;
begin
  Result := DBase.Database;
end;

function TSession.GetTemplatesPath: String;
var
  S: String;
begin
  S := DBItem.TemplatesPath;
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
end;

function TSession.SetExprVar(const AName: String; AValue: Variant): Variant;
begin
  Result := SetVar(Self, AName, AValue);
end;

function TSession.GetExprVar(const AName: String): Variant;
begin
  Result := GetVar(Self, AName);
end;

function TSession.AddRecordSet(AForm: TdxForm): TSsRecordSet;
begin
  Result := TSsRecordSet.Create(Self, nil);
  Result.AssignForm(AForm);
  FRecordSets.Add(Result);
end;

function TSession.FindRecordSet(AFormId, ARecId, ATableId: Integer
  ): TSsRecordSet;
var
  i: Integer;
  RS: TSsRecordSet;
begin
  Result := nil;
  for i := 0 to FRecordSets.Count - 1 do
  begin
    RS := FRecordSets[i];
    if RS.DataSet.Active and (RS.Form <> nil) and (RS.Form.Id = AFormId) and (RS.RecId = ARecId) then
    begin
      if ATableId > 0 then
        Exit(RS.Forms.FindFormById(ATableId))
      else
        Exit(RS);
    end;
  end;
end;

function TSession.AddRpRepordSet(RD: TReportData): TSsRecordSet;
begin
  Result := TSsRecordSet.Create(Self, nil);
  Result.AssignReport(RD);
  FRpRecordSets.Add(Result);
end;

function TSession.FindRpRecordSet(RDId: Integer): TSsRecordSet;
begin
  Result := FRpRecordSets.FindRpById(RDId);
end;

procedure TSession.LockSession;
begin
  EnterCriticalsection(FLock);
  FBusy := True;
end;

procedure TSession.UnlockSession;
begin
  FBusy := False;
  LeaveCriticalsection(FLock);
end;

{ дубль
function TSession.GetUserName: String;
var
  U: TdxUser;
begin
  U := UserMan.Users.FindUser(FUserId);
  if U <> nil then Result := U.Name
  else Result := '';
end;   }


{ TSessionList }

function TSessionList.GetSessions(Index: Integer): TSession;
begin
  Result := TSession(Items[Index]);
end;

constructor TSessionList.Create;
begin
  inherited Create;
  InitCriticalSection(FLock);
end;

destructor TSessionList.Destroy;
begin
  DoneCriticalsection(FLock);
  inherited Destroy;
end;

procedure TSessionList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Sessions[i].Free;
  inherited Clear;
end;

{function TSessionList.FindSession(const Id: String): TSession;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Sessions[i].Id = Id then
      Exit(Sessions[i]);
end;   }

function TSessionList.FindSessionByBrowserId(const BrowserId,
  ConnectName: String): TSession;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if (Sessions[i].BrowserId = BrowserId) and
      (MyUtf8CompareText(Sessions[i].ConnectName, ConnectName) = 0) then
      Exit(Sessions[i]);
end;

procedure TSessionList.DeleteSession(var SS: TSession);
begin
  Remove(SS);
  FreeAndNil(SS);
  DebugStr('DeleteSession pass.');
end;

procedure TSessionList.Lock;
begin
  EnterCriticalsection(FLock);
end;

procedure TSessionList.Unlock;
begin
  LeaveCriticalsection(FLock);
end;

end.

