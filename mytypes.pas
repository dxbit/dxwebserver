unit MyTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUtf8, TypesWrapper, BGRAGraphics, fpjson;

type

  TMsgDlgType    = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);
  TMsgDlgBtn     = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
                    mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose);
  TMsgDlgButtons = set of TMsgDlgBtn;

  TMsgInfo = record
    Msg, Title: String;
    MsgType: TMsgDlgType;
    Buttons: TMsgDlgButtons;
    Visible, IsAction: Boolean;
  end;

  TVariantArray2d = array of array of Variant;

  TGotoOption = (gtoDefault, gtoReplaceUrl, gtoNewTab);

  { TUniList }

  TUniList = class(TList)
  public
    function AddItem(AItem: Pointer): Integer;
  end;

  { TUniPropData }

  TUniPropData = class
  public
    Control: TObject;
    PropName: String;
    function GetControlName: String;
    function GetPropValue: String;
  end;

  { TUniPropList }

  TUniPropList = class(TList)
  private
    function GetItem(Index: Integer): TUniPropData;
  public
    function AddItem(AControl: TObject; const PropName: String): TUniPropData;
    function FindItem(AControl: TObject; const PropName: String): TUniPropData;
    procedure Clear; override;
    property Item[Index: Integer]: TUniPropData read GetItem; default;
  end;

  { TStringListUtf8 }

  TStringListCompareEvent = function(Sender: TStringList; const s1, s2: String): Integer of object;

  TStringListUtf8 = class(TStringList)
  private
    FOnCompare: TStringListCompareEvent;
  protected
    function DoCompareText(const s1, s2: string): PtrInt; override;
  public
    property OnCompare: TStringListCompareEvent read FOnCompare write FOnCompare;
  end;

  { TCardinalList }

  TCardinalList = class(TList)
  private
   function GetValues(Index: Integer): Cardinal;
  public
   function AddValue(Value: Cardinal): Integer;
   procedure Clear; override;
   property Values[Index: Integer]: Cardinal read GetValues; default;
  end;

  PVrData = ^TVrData;
  TVrData = record
    Name: String;
    Value: Variant;
  end;

  { TVarList }

  TVarList = class(TList)
  private
   function GetVars(Index: Integer): PVrData;
  public
   function AddVar(const aName: String; aValue: Variant): PVrData;
   function FindVar(const aName: String): PVrData;
   procedure Clear; override;
   property Vars[Index: Integer]: PVrData read GetVars; default;
  end;

  TParamNotifyEvent = procedure (Sender: TObject; const ParamName: String) of object;

  { TParamData }

  TParamData = class
  private
   FName: String;
   FObj: TObject;
   FValue: Variant;
  public
   property Name: String read FName write FName;
   property Value: Variant read FValue write FValue;
   property Obj: TObject read FObj write FObj;
  end;

  { TParamList }

  TParamList = class(TList)
  private
   FOnGetParam: TParamNotifyEvent;
   FOnSetParam: TParamNotifyEvent;
   function GetNames(Index: Integer): String;
   function GetObjectFromIndex(Index: Integer): TObject;
   function GetObjects(Name: String): TObject;
   function GetValueFromIndex(Index: Integer): Variant;
   function GetValues(Name: String): Variant;
   procedure SetObjectFromIndex(Index: Integer; AValue: TObject);
   procedure SetObjects(Name: String; AValue: TObject);
   procedure SetValueFromIndex(Index: Integer; AValue: Variant);
   procedure SetValues(Name: String; AValue: Variant);
   function GetParams(Index: Integer): TParamData;
   function Find(const aName: String): TParamData;
   procedure DoSetParam(const Name: String);
   procedure DoGetParam(const Name: String);
  public
   function AddParam(const aName: String; aValue: Variant; aObj: TObject): TParamData;
   procedure Clear; override;
   function ParamExists(const aName: String): Boolean;
   property Values[Name: String]: Variant read GetValues write SetValues;
   property Objects[Name: String]: TObject read GetObjects write SetObjects;
   property Names[Index: Integer]: String read GetNames;
   property ValueFromIndex[Index: Integer]: Variant read GetValueFromIndex write
     SetValueFromIndex;
   property ObjectFromIndex[Index: Integer]: TObject read GetObjectFromIndex write
     SetObjectFromIndex;
   property OnSetParam: TParamNotifyEvent read FOnSetParam write FOnSetParam;
   property OnGetParam: TParamNotifyEvent read FOnGetParam write FOnGetParam;
  end;

  { TJSONFloatNumberEx }

  TJSONFloatNumberEx = class(TJSONFloatNumber)
  protected
    function GetAsString: TJSONStringType; override;
  public
    JSonFormatFloatEnabled: Boolean;
  end;

implementation

uses
  dxctrls, pivotgrid, apputils;

{ TUniPropData }

function TUniPropData.GetControlName: String;
begin
  if Control is TdxField then
    Result := FieldStr(TdxField(Control).Id)
  else if Control is TdxQueryGrid then
    Result := 'q' + IntToStr(TdxQueryGrid(Control).Id)
  else if Control is TdxGrid then
    Result := TableStr(TdxGrid(Control).Id)
  else if Control is TdxForm then
    Result := TableStr(TdxForm(Control).Id)
  else if Control is TdxPivotGrid then
    Result := 'pgrid' + IntToStr(TdxPivotGrid(Control).Id)
  else
    Result := TdxComponent(Control).Name;
end;

function FontStyleToHtml(Style: TFontStyles): String;
begin
  Result := '';
  if fsBold in Style then Result := Result + 'b';
  if fsItalic in Style then Result := Result + 'i';
  if fsUnderline in Style then Result := Result + 'u';
  if fsStrikeOut in Style then Result := Result + 's';
end;

function TUniPropData.GetPropValue: String;
var
  C: TdxControl;
begin
  C := TdxControl(Control);
  if PropName = 'visible' then
    Result := Bool2Str(C.Visible)
  else if PropName = 'enabled' then
    Result := Bool2Str(CanEnabledControl(C))
  else if PropName = 'color' then
    Result := ColorToHtml(C.Color)
  else if PropName = 'font' then
    Result := C.Font.Name + ';' + IntToStr(C.Font.Size) + ';' +
      FontStyleToHtml(C.Font.Style) + ';' + ColorToHtml(C.Font.Color)
  else if PropName = 'bounds' then
    Result := IntToStr(C.Left) + ';' + IntToStr(C.Top) + ';' +
      IntToStr(C.Width) + ';' + IntToStr(C.Height)
  else if PropName = 'caption' then
    Result := StrToHtml(C.Caption, True)
  else if PropName = 'tabindex' then
    Result := IntToStr(TdxPageControl(C).ActivePageIndex)
  else if PropName = 'bitmap' then
  begin
    if C is TdxImage then
      Result := TdxImage(C).GetImagePath(True)
    else if C is TdxShape then
    begin
      ShapeToFile(TdxShape(C), TdxShape(C).GetImagePath(False));
      Result := TdxShape(C).GetImagePath(True);
    end;
  end
  else if PropName = 'recno' then
  begin
    if C is TdxForm then Result := IntToStr(TdxForm(C).RecNo)
    else Result := IntToStr(TdxQueryGrid(C).RecNo);
  end
  else
    Result := '';
end;

{ TUniPropList }

function TUniPropList.GetItem(Index: Integer): TUniPropData;
begin
  Result := TUniPropData(Items[Index]);
end;

function TUniPropList.AddItem(AControl: TObject; const PropName: String
  ): TUniPropData;
begin
  Result := FindItem(AControl, PropName);
  if Result = nil then
  begin
    Result := TUniPropData.Create;
    Result.Control := AControl;
    Result.PropName := PropName;
    Add(Result);
  end;
end;

function TUniPropList.FindItem(AControl: TObject; const PropName: String
  ): TUniPropData;
var
  i: Integer;
  D: TUniPropData;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    D := Item[i];
    if (D.Control = AControl) and (D.PropName = PropName) then Exit(D);
  end;
end;

procedure TUniPropList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Item[i].Free;
  inherited Clear;
end;

{ TUniList }

function TUniList.AddItem(AItem: Pointer): Integer;
begin
  Result := IndexOf(AItem);
  if Result < 0 then
    Result := Add(AItem);
end;

{ TCardinalList }

function TCardinalList.GetValues(Index: Integer): Cardinal;
begin
  Result := PCardinal(Items[Index])^;
end;

function TCardinalList.AddValue(Value: Cardinal): Integer;
var
  pV: PCardinal;
begin
	New(pV);
  pV^ := Value;
  Result := Add(pV);
end;

procedure TCardinalList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  	Dispose(PCardinal(Items[i]));
  inherited Clear;
end;

{ TStringListUtf8 }

function TStringListUtf8.DoCompareText(const s1, s2: string): PtrInt;
begin
  if FOnCompare <> nil then Result := FOnCompare(Self, s1, s2)
  else
  begin
    if not CaseSensitive then
      Result := MyUtf8CompareText(s1, s2)
    else
      Result := Utf8CompareStr(s1, s2);
  end;
end;

{ TVarList }

function TVarList.GetVars(Index: Integer): PVrData;
begin
  Result := PVrData(Items[Index]);
end;

function TVarList.AddVar(const aName: String; aValue: Variant): PVrData;
begin
  New(Result);
  Result^.Name := aName;
  Result^.Value := aValue;
  Add(Result);
end;

function TVarList.FindVar(const aName: String): PVrData;
var
  i: Integer;
  pV: PVrData;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    pV := Vars[i];
    if MyUtf8CompareText(pV^.Name, aName) = 0 then Exit(pV);
  end;
end;

procedure TVarList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Dispose(Vars[i]);
  inherited Clear;
end;

{ TParamList }

function TParamList.GetNames(Index: Integer): String;
begin
  Result := GetParams(Index).Name;
end;

function TParamList.GetObjectFromIndex(Index: Integer): TObject;
begin
  DoGetParam(Names[Index]);
  Result := GetParams(Index).Obj;
end;

function TParamList.GetObjects(Name: String): TObject;
var
  PD: TParamData;
begin
  DoGetParam(Name);
  Result := nil;
  PD := Find(Name);
  if PD <> nil then Result := PD.Obj;
end;

function TParamList.GetValueFromIndex(Index: Integer): Variant;
begin
  DoGetParam(Names[Index]);
  Result := GetParams(Index).Value;
end;

function TParamList.GetValues(Name: String): Variant;
var
  PD: TParamData;
begin
  DoGetParam(Name);
  Result := Null;
  PD := Find(Name);
  if PD <> nil then Result := PD.Value;
end;

procedure TParamList.SetObjectFromIndex(Index: Integer; AValue: TObject);
begin
  GetParams(Index).Obj := AValue;
  DoSetParam(Names[Index]);
end;

procedure TParamList.SetObjects(Name: String; AValue: TObject);
var
  PD: TParamData;
begin
  PD := Find(Name);
  if PD = nil then
    PD := AddParam(Name, Null, AValue)
  else
    PD.Obj := AValue;
  DoSetParam(Name);
end;

procedure TParamList.SetValueFromIndex(Index: Integer; AValue: Variant);
begin
  GetParams(Index).Value := AValue;
  DoSetParam(Names[Index]);
end;

procedure TParamList.SetValues(Name: String; AValue: Variant);
var
  PD: TParamData;
begin
  PD := Find(Name);
  if PD = nil then
    PD := AddParam(Name, AValue, nil)
  else
    PD.Value := AValue;
  DoSetParam(Name);
end;

function TParamList.GetParams(Index: Integer): TParamData;
begin
  Result := TParamData(Items[Index]);
end;

function TParamList.Find(const aName: String): TParamData;
var
  i: Integer;
  PD: TParamData;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    PD := GetParams(i);
    if MyUtf8CompareText(PD.Name, aName) = 0 then Exit(PD);
  end;
end;

procedure TParamList.DoSetParam(const Name: String);
begin
  if OnSetParam <> nil then FOnSetParam(Self, Name);
end;

procedure TParamList.DoGetParam(const Name: String);
begin
  if OnGetParam <> nil then FOnGetParam(Self, Name);
end;

function TParamList.AddParam(const aName: String; aValue: Variant; aObj: TObject
  ): TParamData;
begin
  Result := TParamData.Create;
  Result.Name := aName;
  Result.Value := aValue;
  Result.Obj := aObj;
  Add(Result);
end;

procedure TParamList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    GetParams(i).Free;
  inherited Clear;
end;

function TParamList.ParamExists(const aName: String): Boolean;
begin
  Result := Find(aName) <> nil;
end;

{ TJSONFloatNumberEx }

function TJSONFloatNumberEx.GetAsString: TJSONStringType;
var
  FS: TFormatSettings;
begin
  if JSONFormatFloatEnabled then
  begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator := '.';
    Result := FloatToStr(AsFloat, FS);
    if Frac(AsFloat) = 0 then Result := Result + '.0';
  end
  else
    Result := inherited GetAsString;
end;

initialization
  SetJSONInstanceType(jitNumberFloat, TJSONFloatNumberEx);

end.

