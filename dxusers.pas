unit DXUsers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mytypes;

type

  { TdxControlRight }

  TdxControlRight = class
  private
    FEditing: Boolean;
    FName: String;
    FVisible: Boolean;
  public
    property Name: String read FName write FName;
    property Visible: Boolean read FVisible write FVisible;
    property Editing: Boolean read FEditing write FEditing;
  end;

  { TdxControlRightList }

  TdxControlRightList = class(TList)
  private
    function GetRights(Index: Integer): TdxControlRight;
  public
    function AddRight: TdxControlRight;
    function FindRight(const aName: String): TdxControlRight;
    procedure Clear; override;
    procedure DeleteRight(aCR: TdxControlRight);
    property Rights[Index: Integer]: TdxControlRight read GetRights; default;
  end;

  { TdxFormRight }

  TdxFormRight = class
  private
    FAdding: Boolean;
    FApplySelCondToObj: Boolean;
    FControls: TdxControlRightList;
    FDelCond: String;
    FEditCond: String;
    FSelCond: String;
    FVisible: Boolean;
    FDeleting: Boolean;
    FEditing: Boolean;
    //FExporting: Boolean;
    FFormId: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property FormId: Integer read FFormId write FFormId;
    property Visible: Boolean read FVisible write FVisible;
    property Adding: Boolean read FAdding write FAdding;
    property Editing: Boolean read FEditing write FEditing;
    property Deleting: Boolean read FDeleting write FDeleting;
    //property Exporting: Boolean read FExporting write FExporting;
    property Controls: TdxControlRightList read FControls;
    property SelCond: String read FSelCond write FSelCond;
    property EditCond: String read FEditCond write FEditCond;
    property DelCond: String read FDelCond write FDelCond;
    property ApplySelCondToObj: Boolean read FApplySelCondToObj write FApplySelCondToObj;
  end;

  { TdxFormRightList }

  TdxFormRightList = class(TList)
  private
    function GetRights(Index: Integer): TdxFormRight;
  public
    function AddRight: TdxFormRight;
    function FindRight(FmId: Integer): TdxFormRight;
    procedure DeleteRight(aFR: TdxFormRight);
    procedure Clear; override;
    property Rights[Index: Integer]: TdxFormRight read GetRights; default;
  end;

  { TdxReportRight }

  TdxReportRight = class
  private
    FReportId: Integer;
    FVisible: Boolean;
  public
    property ReportId: Integer read FReportId write FReportId;
    property Visible: Boolean read FVisible write FVisible;
  end;

  { TdxReportRightList }

  TdxReportRightList = class(TList)
  private
    function GetRights(Index: Integer): TdxReportRight;
  public
    function AddRight: TdxReportRight;
    function FindRight(RpId: Integer): TdxReportRight;
    procedure DeleteRight(aRR: TdxReportRight);
    procedure Clear; override;
    property Rights[Index: Integer]: TdxReportRight read GetRights; default;
  end;

  { TdxRole }

  TdxRole = class
  private
    FFormRights: TdxFormRightList;
    FId: Integer;
    FIntfId: Integer;
    FName: String;
    FReportRights: TdxReportRightList;
  public
    constructor Create;
    destructor Destroy; override;
    property Id: Integer read FId write FId;
    property Name: String read FName write FName;
    property IntfId: Integer read FIntfId write FIntfId;
    property FormRights: TdxFormRightList read FFormRights;
    property ReportRights: TdxReportRightList read FReportRights;
  end;

  { TdxRoleList }

  TdxRoleList = class(TList)
  private
    function GetRoles(Index: Integer): TdxRole;
    function GetUniqueId: Integer;
  public
    function AddRole: TdxRole;
    function FindRole(aId: Integer): TdxRole;
    procedure DeleteRole(aR: TdxRole);
    procedure Clear; override;
    procedure LoadFromStream(St: TStream);
    procedure FillStrings(L: TStrings);
    property Roles[Index: Integer]: TdxRole read GetRoles; default;
  end;

  { TdxUser }

  TdxUser = class
  private
    FId: Integer;
    FName: String;
    FPassword: String;
    FRoleId: Integer;
  public
    //function GetRole: TdxRole;
    property Id: Integer read FId write FId;
    property Name: String read FName write FName;
    property Password: String read FPassword write FPassword;
    property RoleId: Integer read FRoleId write FRoleId;
  end;

  { TdxUserList }

  TdxUserList = class(TList)
  private
    function GetUsers(Index: Integer): TdxUser;
    function GetUniqueId: Integer;
  public
    function AddUser: TdxUser;
    function FindUser(aId: Integer): TdxUser;
    function FindUserByName(const AName: String): TdxUser;
    procedure Clear; override;
    procedure DeleteUser(aU: TdxUser);
    procedure LoadFromStream(St: TStream);
    procedure FillStrings(L: TStrings);
    property Users[Index: Integer]: TdxUser read GetUsers; default;
  end;

  TdxMenuItemKind = (miMenu, miDiv, miForm, miReport);

  TdxMenuItemList = class;

  { TdxMenuItem }

  TdxMenuItem = class
  private
    FCaption: String;
    FId: Integer;
    FItems: TdxMenuItemList;
    FKind: TdxMenuItemKind;
    FVisible: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property Caption: String read FCaption write FCaption;
    property Kind: TdxMenuItemKind read FKind write FKind;
    property Id: Integer read FId write FId;
    property Items: TdxMenuItemList read FItems;
    property Visible: Boolean read FVisible write FVisible;     // Доступ !!!
  end;

  { TdxMenuItemList }

  TdxMenuItemList = class(TList)
  private
    function GetMenuItems(Index: Integer): TdxMenuItem;
  public
    function AddItem: TdxMenuItem;
    procedure DeleteItem(Item: TdxMenuItem);
    procedure Clear; override;
    property MenuItems[Index: Integer]: TdxMenuItem read GetMenuItems; default;
  end;

  { TdxTabList }

  TdxTabList = class(TList)
  private
    function GetTabs(Index: Integer): Integer;
  public
    function AddTab(FmId: Integer): Integer;
    property Tabs[Index: Integer]: Integer read GetTabs; default;
  end;

  { TdxIntf }

  TdxIntf = class
  private
    FId: Integer;
    FIsDefault: Boolean;
    FMenu: TdxMenuItemList;
    FName: String;
    FTabs: TdxTabList;
  public
    constructor Create;
    destructor Destroy; override;
    property Id: Integer read FId write FId;
    property Name: String read FName write FName;
    property Tabs: TdxTabList read FTabs;
    property Menu: TdxMenuItemList read FMenu;
    property IsDefault: Boolean read FIsDefault write FIsDefault;
  end;

  { TdxIntfList }

  TdxIntfList = class(TList)
  private
    function GetIntfs(Index: Integer): TdxIntf;
    function GetUniqueId: Integer;
  public
    function AddIntf: TdxIntf;
    function FindIntf(aId: Integer): TdxIntf;
    function FindIntfByName(const aName: String): TdxIntf;
    function FindDefaultIntf: TdxIntf;
    procedure DeleteIntf(Intf: TdxIntf);
    procedure Clear; override;
    procedure LoadFromStream(St: TStream);
    procedure FillStrings(SL: TStrings);
    property Intfs[Index: Integer]: TdxIntf read GetIntfs; default;
  end;

  { TdxUserManager }

  TdxUserManager = class
  private
    FCurUserId: Integer;
    FRoles: TdxRoleList;
    FUsers: TdxUserList;
    FIntfs: TdxIntfList;
    //FSS: TObject;
    function GetFormRight(RoleId, FmId: Integer): TdxFormRight;
    function GetReportRight(RoleId, RpId: Integer): TdxReportRight;
  public
    constructor Create;
    destructor Destroy; override;
    //function CurrentUser: TdxUser;
    function CheckFmVisible(RoleId, FmId: Integer): Boolean;
    function CheckFmAdding(RoleId, FmId: Integer): Boolean;
    function CheckFmEditing(RoleId, FmId: Integer): Boolean;
    function CheckFmDeleting(RoleId, FmId: Integer): Boolean;
    //function CheckFmExporting(FmId: Integer): Boolean;
    function CheckRpVisible(RoleId, RpId: Integer): Boolean;
    function CheckControlVisible(RoleId, FmId: Integer; const aName: String): Boolean;
    function CheckControlEditing(RoleId, FmId: Integer; const aName: String): Boolean;
    function GetSelCond(RoleId, FmId: Integer): String;
    function GetEditCond(RoleId, FmId: Integer): String;
    function GetDelCond(RoleId, FmId: Integer): String;
    function GetApplySelCondToObj(RoleId, FmId: Integer): Boolean;
    function GetIntf(RoleId: Integer): TdxIntf;
    property Users: TdxUserList read FUsers;
    property Roles: TdxRoleList read FRoles;
    property Intfs: TdxIntfList read FIntfs;
    //property CurrentUserId: Integer read FCurUserId write FCurUserId;
  end;

implementation

uses
  LazUtf8, sqldb, apputils, SAX, SAX_XML, dxtypes, saxbasereader;

type

  { TdxBaseReader }

  {TdxBaseReader = class(TSaxXmlReader)
  protected
    function GetBool(Atts: TSAXAttributes; const aName: String): Boolean;
    function GetInt(Atts: TSAXAttributes; const aName: String): Integer;
  end;  }

  { TdxRolesReader }

  TdxRolesReader = class(TSAXBaseReader)
  private
    FRoles: TdxRoleList;
    FRole: TdxRole;
    FFR: TdxFormRight;
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
  public
    property Roles: TdxRoleList read FRoles write FRoles;
  end;

  { TdxIntfsReader }

  TdxIntfsReader = class(TSAXBaseReader)
  private
    FIntfs: TdxIntfList;
    FIntf: TdxIntf;
    FParentMenu: TdxMenuItemList;
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
    procedure DoEndElement(const NamespaceURI, LocalName, QName: SAXString); override;
  public
    property Intfs: TdxIntfList read FIntfs write FIntfs;
  end;

  { TdxUsersReader }

  TdxUsersReader = class(TSAXBaseReader)
  private
    FUsers: TdxUserList;
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
  public
    property Users: TdxUserList read FUsers write FUsers;
  end;


{ TdxIntfsReader }

procedure TdxIntfsReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  MI: TdxMenuItem;
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  if LocalName = 'interface' then
  begin
    FIntf := FIntfs.AddIntf;
    FIntf.Id := GetInt(Atts, 'id');
    FIntf.Name := XmlToStr(GetStr(Atts, 'name'));
    FIntf.IsDefault:=GetBool(Atts, 'default');
    FParentMenu := FIntf.Menu;
  end
  else if LocalName = 'tab' then
  begin
    FIntf.Tabs.AddTab(GetInt(Atts, 'fmid'));
  end
  else if LocalName = 'menuitem' then
  begin
    MI := FParentMenu.AddItem;
    MI.Kind := TdxMenuItemKind(GetInt(Atts, 'kind'));
    MI.Caption := XmlToStr(GetStr(Atts, 'caption'));
    MI.Id:=GetInt(Atts, 'id');
    FParentMenu := MI.Items;
  end;
end;

function FindParentMenu(Menu, Cur: TdxMenuItemList): TdxMenuItemList;
var
  i: Integer;
  MI: TdxMenuItem;
begin
  Result := nil;
  if Menu = Cur then Exit(Cur);
  for i := 0 to Menu.Count - 1 do;
  begin
    MI := Menu[i];
    if MI.Items = Cur then Exit(Menu)
    else Result := FindParentMenu(MI.Items, Cur)
  end;
end;

procedure TdxIntfsReader.DoEndElement(const NamespaceURI, LocalName,
  QName: SAXString);
begin
  inherited DoEndElement(NamespaceURI, LocalName, QName);
  if LocalName = 'menuitem' then
    FParentMenu := FindParentMenu(FIntf.Menu, FParentMenu);
end;

{ TdxTabList }

function TdxTabList.GetTabs(Index: Integer): Integer;
begin
  Result := PtrInt(Items[Index]);
end;

function TdxTabList.AddTab(FmId: Integer): Integer;
begin
  Result := Add(Pointer(FmId));
end;

{ TdxIntfList }

function TdxIntfList.GetIntfs(Index: Integer): TdxIntf;
begin
  Result := TdxIntf(Items[Index]);
end;

function TdxIntfList.GetUniqueId: Integer;
var
  i, n: Integer;
  Intf: TdxIntf;
begin
  n := -1;
  for i := 0 to Count - 1 do
  begin
    Intf := Intfs[i];
    if Intf.Id > n then n := Intf.Id;
  end;
  Result := n + 1;
end;

function TdxIntfList.AddIntf: TdxIntf;
begin
  Result := TdxIntf.Create;
  Result.Id := GetUniqueId;
  Add(Result);
end;

function TdxIntfList.FindIntf(aId: Integer): TdxIntf;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Intfs[i].Id = aId then Exit(Intfs[i]);
end;

function TdxIntfList.FindIntfByName(const aName: String): TdxIntf;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if MyUtf8CompareText(Intfs[i].Name, aName) = 0 then Exit(Intfs[i]);
end;

function TdxIntfList.FindDefaultIntf: TdxIntf;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Intfs[i].IsDefault then Exit(Intfs[i]);
end;

procedure TdxIntfList.DeleteIntf(Intf: TdxIntf);
begin
  Remove(Intf);
  FreeAndNil(Intf);
end;

procedure TdxIntfList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Intfs[i].Free;
  inherited Clear;
end;

procedure TdxIntfList.LoadFromStream(St: TStream);
var
  R: TdxIntfsReader;
begin
  Clear;
  if St = nil then Exit;
  St.Position:=0;
  R := TdxIntfsReader.Create;
  try
    R.Intfs := Self;
    R.ParseStream(St);
  finally
    R.Free;
  end;
end;

procedure TdxIntfList.FillStrings(SL: TStrings);
var
  i: Integer;
begin
  SL.Clear;
  for i := 0 to Count - 1 do
    SL.AddObject(Intfs[i].Name, Intfs[i]);
end;

{ TdxIntf }

constructor TdxIntf.Create;
begin
  FTabs := TdxTabList.Create;
  FMenu := TdxMenuItemList.Create;
end;

destructor TdxIntf.Destroy;
begin
  FMenu.Free;
  FTabs.Free;
  inherited Destroy;
end;

{ TdxMenuItem }

constructor TdxMenuItem.Create;
begin
  FItems := TdxMenuItemList.Create;
end;

destructor TdxMenuItem.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

{ TdxMenuItemList }

function TdxMenuItemList.GetMenuItems(Index: Integer): TdxMenuItem;
begin
  Result := TdxMenuItem(Items[Index]);
end;

function TdxMenuItemList.AddItem: TdxMenuItem;
begin
  Result := TdxMenuItem.Create;
  Add(Result);
end;

procedure TdxMenuItemList.DeleteItem(Item: TdxMenuItem);
begin
  Remove(Item);
  Item.Free;
end;

procedure TdxMenuItemList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    MenuItems[i].Free;
  inherited Clear;
end;

{ TdxFormRight }

constructor TdxFormRight.Create;
begin
  FControls := TdxControlRightList.Create;
  FApplySelCondToObj := True;
end;

destructor TdxFormRight.Destroy;
begin
  FControls.Free;
  inherited Destroy;
end;

{ TdxControlRightList }

function TdxControlRightList.GetRights(Index: Integer): TdxControlRight;
begin
  Result := TdxControlRight(Items[Index]);
end;

function TdxControlRightList.AddRight: TdxControlRight;
begin
  Result := TdxControlRight.Create;
  Add(Result);
end;

function TdxControlRightList.FindRight(const aName: String): TdxControlRight;
var
  i: Integer;
  R: TdxControlRight;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    R := Rights[i];
    if R.Name = aName then Exit(R);
  end;
end;

procedure TdxControlRightList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Rights[i].Free;
  inherited Clear;
end;

procedure TdxControlRightList.DeleteRight(aCR: TdxControlRight);
begin
  Remove(aCR);
  aCR.Free;
end;

{function TdxUser.GetRole: TdxRole;
begin
  Result := UserMan.Roles.FindRole(FRoleId);
end; }

{ TdxUsersReader }

procedure TdxUsersReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  U: TdxUser;
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  if LocalName = 'user' then
  begin
    U := FUsers.AddUser;
    U.Id := GetInt(Atts, 'id');
    U.Name := XmlToStr(GetStr(Atts, 'name'));
    U.Password:=GetStr(Atts, 'password');
    U.RoleId:=GetInt(Atts, 'roleid');
  end;
end;

{ TdxBaseReader }

{function TdxBaseReader.GetBool(Atts: TSAXAttributes; const aName: String
  ): Boolean;
var
  S: SAXString;
begin
  Result := False;
  S := GetStr(Atts, aName);
  if S = '1' then Result := True;
end;

function TdxBaseReader.GetInt(Atts: TSAXAttributes; const aName: String
  ): Integer;
var
  S: SAXString;
begin
  Result := 0;
  S := GetStr(Atts, aName);
  if S <> '' then Result := StrToInt(S);
end;    }

{ TdxRolesReader }

procedure TdxRolesReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  CR: TdxControlRight;
  RR: TdxReportRight;
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  if LocalName = 'role' then
  begin
    FRole := FRoles.AddRole;
    FRole.Id := GetInt(Atts, 'id');
    FRole.Name := GetStr(Atts, 'name');
    FRole.IntfId := GetInt(Atts, 'intfid');
  end
  else if LocalName = 'fright' then
  begin
    FFR := FRole.FormRights.AddRight;
    FFR.FormId:=GetInt(Atts, 'form');
    FFR.Visible:=GetBool(Atts, 'visible');
    FFR.Adding:=GetBool(Atts, 'adding');
    FFR.Editing:=GetBool(Atts, 'editing');
    FFR.Deleting:=GetBool(Atts, 'deleting');
    //FFR.Exporting:=GetBool(Atts, 'exporting');
    FFR.SelCond:=XmlToStr(GetStr(Atts, 'selcond'));
    FFR.EditCond:=XmlToStr(GetStr(Atts, 'editcond'));
    FFR.DelCond := XmlToStr(GetStr(Atts, 'delcond'));
    FFR.ApplySelCondToObj:=GetBool(Atts, 'applyselcondtoobj');
  end
  else if LocalName = 'cright' then
  begin
    CR := FFR.Controls.AddRight;
    CR.Name:=GetStr(Atts, 'name');
    CR.Visible:=GetBool(Atts, 'visible');
    CR.Editing:=GetBool(Atts, 'editing');
  end
  else if LocalName = 'rright' then
  begin
    RR := FRole.ReportRights.AddRight;
    RR.ReportId:=GetInt(Atts, 'report');
    RR.Visible:=GetBool(Atts, 'visible');
  end;
end;

{ TdxUserManager }

constructor TdxUserManager.Create;
begin
  //FSS := SS;
  FUsers := TdxUserList.Create;
  FRoles := TdxRoleList.Create;
  FIntfs := TdxIntfList.Create;
end;

destructor TdxUserManager.Destroy;
begin
  FIntfs.Free;
  FRoles.Free;
  FUsers.Free;
  inherited Destroy;
end;

function TdxUserManager.GetFormRight(RoleId, FmId: Integer): TdxFormRight;
var
  R: TdxRole;
begin
  Result := nil;
  if RoleId >= 0 then
  begin
    R := Roles.FindRole(RoleId);
    if R = nil then Exit;
    Result := R.FormRights.FindRight(FmId);
  end;
end;

function TdxUserManager.GetReportRight(RoleId, RpId: Integer): TdxReportRight;
var
  R: TdxRole;
begin
  Result := nil;
  if RoleId >= 0 then
  begin
    R := Roles.FindRole(RoleId);
    if R = nil then Exit;
    Result := R.ReportRights.FindRight(RpId);
  end;
end;

{function TdxUserManager.CurrentUser: TdxUser;
begin
  Result := FUsers.FindUser(FCurUserId);
end; }

function TdxUserManager.CheckFmVisible(RoleId, FmId: Integer): Boolean;
var
  FR: TdxFormRight;
begin
  Result := True;
  FR := GetFormRight(RoleId, FmId);
  if FR <> nil then
    Result := FR.Visible;
end;

function TdxUserManager.CheckFmAdding(RoleId, FmId: Integer): Boolean;
var
  FR: TdxFormRight;
begin
  Result := True;
  FR := GetFormRight(RoleId, FmId);
  if FR <> nil then
    Result := FR.Adding;
end;

function TdxUserManager.CheckFmEditing(RoleId, FmId: Integer): Boolean;
var
  FR: TdxFormRight;
begin
  Result := True;
  FR := GetFormRight(RoleId, FmId);
  if FR <> nil then
    Result := FR.Editing;
end;

function TdxUserManager.CheckFmDeleting(RoleId, FmId: Integer): Boolean;
var
  FR: TdxFormRight;
begin
  Result := True;
  FR := GetFormRight(RoleId, FmId);
  if FR <> nil then
    Result := FR.Deleting;
end;

{function TdxUserManager.CheckFmExporting(FmId: Integer): Boolean;
var
  FR: TdxFormRight;
begin
  Result := True;
  FR := GetFormRight(FmId);
  if FR <> nil then
    Result := FR.Exporting;
end;}

function TdxUserManager.CheckRpVisible(RoleId, RpId: Integer): Boolean;
var
  RR: TdxReportRight;
begin
  Result := True;
  RR := GetReportRight(RoleId, RpId);
  if RR <> nil then
    Result := RR.Visible;
end;

function TdxUserManager.CheckControlVisible(RoleId, FmId: Integer;
  const aName: String): Boolean;
var
  FR: TdxFormRight;
  CR: TdxControlRight;
begin
  Result := True;
  if aName = '' then Exit;
  FR := GetFormRight(RoleId, FmId);
  if FR = nil then Exit;
  CR := FR.Controls.FindRight(aName);
  if CR = nil then Exit;
  Result := CR.Visible;
end;

function TdxUserManager.CheckControlEditing(RoleId, FmId: Integer;
  const aName: String): Boolean;
var
  FR: TdxFormRight;
  CR: TdxControlRight;
begin
  Result := True;
  FR := GetFormRight(RoleId, FmId);
  if FR = nil then Exit;
  CR := FR.Controls.FindRight(aName);
  if CR = nil then Exit;
  Result := CR.Editing;
end;

function TdxUserManager.GetSelCond(RoleId, FmId: Integer): String;
var
  FR: TdxFormRight;
begin
  Result := '';
  FR := GetFormRight(RoleId, FmId);
  if FR = nil then Exit;
  Result := FR.SelCond;
end;

function TdxUserManager.GetEditCond(RoleId, FmId: Integer): String;
var
  FR: TdxFormRight;
begin
  Result := '';
  FR := GetFormRight(RoleId, FmId);
  if FR = nil then Exit;
  Result := FR.EditCond;
end;

function TdxUserManager.GetDelCond(RoleId, FmId: Integer): String;
var
  FR: TdxFormRight;
begin
  Result := '';
  FR := GetFormRight(RoleId, FmId);
  if FR = nil then Exit;
  Result := FR.DelCond;
end;

function TdxUserManager.GetApplySelCondToObj(RoleId, FmId: Integer): Boolean;
var
  FR: TdxFormRight;
begin
  Result := False;
  FR := GetFormRight(RoleId, FmId);
  if FR = nil then Exit;
  Result := FR.ApplySelCondToObj;
end;

function TdxUserManager.GetIntf(RoleId: Integer): TdxIntf;
var
  U: TdxUser;
  R: TdxRole;
begin
  Result := nil;
  R := FRoles.FindRole(RoleId);
  if R <> nil then
    Result := FIntfs.FindIntf(R.IntfId);
  if Result = nil then
    Result := FIntfs.FindDefaultIntf;
end;

{ TdxUserList }

function TdxUserList.GetUsers(Index: Integer): TdxUser;
begin
  Result := TdxUser(Items[Index]);
end;

function TdxUserList.GetUniqueId: Integer;
var
  i, n: Integer;
  U: TdxUser;
begin
  n := -1;
  for i := 0 to Count - 1 do
  begin
    U := Users[i];
    if U.Id > n then n := U.Id;
  end;
  Result := n + 1;
end;

function TdxUserList.AddUser: TdxUser;
begin
  Result := TdxUser.Create;
  Result.Id := GetUniqueId;
  Add(Result);
end;

function TdxUserList.FindUser(aId: Integer): TdxUser;
var
  U: TdxUser;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    U := Users[i];
    if U.Id = aId then Exit(U);
  end;
end;

function TdxUserList.FindUserByName(const AName: String): TdxUser;
var
  U: TdxUser;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    U := Users[i];
    if MyUtf8CompareText(U.Name, AName) = 0 then Exit(U);
  end;
end;

procedure TdxUserList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Users[i].Free;
  inherited Clear;
end;

procedure TdxUserList.DeleteUser(aU: TdxUser);
begin
  Remove(aU);
  aU.Free;
end;

procedure TdxUserList.LoadFromStream(St: TStream);
var
  R: TdxUsersReader;
begin
  Clear;
  St.Position:=0;
  R := TdxUsersReader.Create;
  try
    R.Users := Self;
    R.ParseStream(St);
  finally
    R.Free;
  end;
end;

procedure TdxUserList.FillStrings(L: TStrings);
var
  SL: TStringListUtf8;
  i: Integer;
  U: TdxUser;
begin
  SL := TStringListUtf8.Create;
  for i := 0 to Count - 1 do
  begin
    U := Users[i];
    SL.AddObject(U.Name, U);
  end;
  SL.Sort;
  L.Assign(SL);
  SL.Free;
end;

{ TdxRoleList }

function TdxRoleList.GetRoles(Index: Integer): TdxRole;
begin
  Result := TdxRole(Items[Index]);
end;

function TdxRoleList.GetUniqueId: Integer;
var
  i, n: Integer;
  R: TdxRole;
begin
  n := -1;
  for i := 0 to Count - 1 do
  begin
    R := Roles[i];
    if R.Id > n then n := R.Id;
  end;
  Result := n + 1;
end;

function TdxRoleList.AddRole: TdxRole;
begin
  Result := TdxRole.Create;
  Result.Id := GetUniqueId;
  Add(Result);
end;

function TdxRoleList.FindRole(aId: Integer): TdxRole;
var
  R: TdxRole;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    R := Roles[i];
    if R.Id = aId then Exit(R);
  end;
end;

procedure TdxRoleList.DeleteRole(aR: TdxRole);
begin
  Remove(aR);
  aR.Free;
end;

procedure TdxRoleList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Roles[i].Free;
  inherited Clear;
end;

procedure TdxRoleList.LoadFromStream(St: TStream);
var
  R: TdxRolesReader;
begin
  Clear;
  St.Position:=0;
  R := TdxRolesReader.Create;
  try
    R.Roles := Self;
    R.ParseStream(St);
  finally
    R.Free;
  end;
end;

procedure TdxRoleList.FillStrings(L: TStrings);
var
  SL: TStringListUtf8;
  i: Integer;
  R: TdxRole;
begin
  SL := TStringListUtf8.Create;
  for i := 0 to Count - 1 do
  begin
    R := Roles[i];
    SL.AddObject(R.Name, R);
  end;
  SL.Sort;
  L.Assign(SL);
  SL.Free;
end;

{ TdxReportRightList }

function TdxReportRightList.GetRights(Index: Integer): TdxReportRight;
begin
  Result := TdxReportRight(Items[Index]);
end;

function TdxReportRightList.AddRight: TdxReportRight;
begin
  Result := TdxreportRight.Create;
  Add(Result);
end;

function TdxReportRightList.FindRight(RpId: Integer): TdxReportRight;
var
  R: TdxReportRight;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    R := Rights[i];
    if R.ReportId = RpId then Exit(R);
  end;
end;

procedure TdxReportRightList.DeleteRight(aRR: TdxReportRight);
begin
  Remove(aRR);
  aRR.Free;
end;

procedure TdxReportRightList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Rights[i].Free;
  inherited Clear;
end;

{ TdxFormRightList }

function TdxFormRightList.GetRights(Index: Integer): TdxFormRight;
begin
  Result := TdxFormRight(Items[Index]);
end;

function TdxFormRightList.AddRight: TdxFormRight;
begin
  Result := TdxFormRight.Create;
  Add(Result);
end;

function TdxFormRightList.FindRight(FmId: Integer): TdxFormRight;
var
  i: Integer;
  R: TdxFormRight;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    R := Rights[i];
    if R.FormId = FmId then Exit(R);
  end;
end;

procedure TdxFormRightList.DeleteRight(aFR: TdxFormRight);
begin
  Remove(aFR);
  aFR.Free;
end;

procedure TdxFormRightList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Rights[i].Free;
  inherited Clear;
end;

{ TdxRole }

constructor TdxRole.Create;
begin
  FFormRights := TdxFormRightList.Create;
  FReportRights := TdxReportRightList.Create;
end;

destructor TdxRole.Destroy;
begin
  FReportRights.Free;
  FFormRights.Free;
  inherited Destroy;
end;

end.

