unit FormLayouts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, dxctrls;

type

  PControlLayoutItem = ^TControlLayoutItem;
  TControlLayoutItem = record
    Name: String;
    Left, Top, Width, Height: Integer;
    Anchors: TAnchors;
  end;

  { TControlLayoutList }

  TControlLayoutList = class(TList)
  private
    function GetControlItems(Index: Integer): PControlLayoutItem;
  public
    function AddControl: PControlLayoutItem;
    function FindControl(AControlName: String): PControlLayoutItem;
    procedure Clear; override;
    property ControlItems[Index: Integer]: PControlLayoutItem read GetControlItems; default;
  end;

  PFormLayout = ^TFormLayout;
  TFormLayout = record
    Name: String;
    MinWidth: Integer;
    Width, Height: Integer;
    Web, Desktop: Boolean;
    FixedHeight: Boolean;
    Disabled: Boolean;
    Controls: TControlLayoutList;
  end;

  { TFormLayoutList }

  TFormLayoutList = class(TList)
  private
    function GetLayouts(Index: Integer): PFormLayout;
  public
    function AddLayout(const AName: String): PFormLayout;
    procedure ApplyLayout(const AName: String; AForm: TdxForm);
    function FindLayout(const AName: String): PFormLayout;
    function FindLayoutWidth(const AWidth: Integer): PFormLayout;
    procedure Clear; override;
    property Layouts[Index: Integer]: PFormLayout read GetLayouts; default;
  end;

  PFormLayoutForm = ^TFormLayoutForm;
  TFormLayoutForm = record
    Id: Integer;
    Layouts: TFormLayoutList;
  end;

  { TFormLayoutFormList }

  TFormLayoutFormList = class(TList)
  private
    function GetForms(Index: Integer): PFormLayoutForm;
  public
    function AddForm(FmId: Integer): PFormLayoutForm;
    function FindForm(FmId: Integer): PFormLayoutForm;
    procedure Clear; override;
    procedure LoadFromStream(AFm: PFormLayoutForm; St: TStream);
    procedure SaveToStream(pFm: PFormLayoutForm; St: TStream);
    property Forms[Index: Integer]: PFormLayoutForm read GetForms; default;
  end;

implementation

uses
  SAX, saxbasereader, apputils;

type

  { TFormLayoutsReader }

  TFormLayoutsReader = class(TSAXBaseReader)
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
  public
    pLay: PFormLayout;
    pFm: PFormLayoutForm;
  end;

{ TFormLayoutsReader }

procedure TFormLayoutsReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  pCLI: PControlLayoutItem;
  S: String;
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  if LocalName = 'layout' then
  begin
    pLay := pFm^.Layouts.AddLayout(GetStr(Atts, 'name'));
    with pLay^ do
    begin
      Web := GetBool(Atts, 'web');
      Desktop := GetBool(Atts, 'desktop');
      S := GetStr(Atts, 'size');
      Width := StrToInt(CutStr(S, ';'));
      Height := StrToInt(CutStr(S, ';'));
      MinWidth := GetInt(Atts, 'minwidth');
      FixedHeight := GetBool(Atts, 'fixedheight');
      Disabled := GetBool(Atts, 'disabled');
    end;
  end
  else if LocalName = 'control' then
  begin
    pCLI := pLay^.Controls.AddControl;
    with pCLI^ do
    begin
      Name := GetStr(Atts, 'name');
      S := GetStr(Atts, 'bounds');
      Left := StrToInt(CutStr(S, ';'));
      Top := StrToInt(CutStr(S, ';'));
      Width := StrToInt(CutStr(S, ';'));
      Height := StrToInt(CutStr(S, ';'));
      Anchors := TAnchors(GetInt(Atts, 'anchors'));
    end;
  end;
end;

{ TControlLayoutList }

function TControlLayoutList.GetControlItems(Index: Integer): PControlLayoutItem;
begin
  Result := PControlLayoutItem(Items[Index]);
end;

function TControlLayoutList.AddControl: PControlLayoutItem;
begin
  New(Result);
  FillChar(Result^, SizeOf(TControlLayoutItem), 0);
  Add(Result);
end;

function TControlLayoutList.FindControl(AControlName: String): PControlLayoutItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if ControlItems[i]^.Name = AControlName then
      Exit(ControlItems[i]);
end;

procedure TControlLayoutList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Dispose(ControlItems[i]);
  inherited Clear;
end;

{ TFormLayoutList }

function TFormLayoutList.GetLayouts(Index: Integer): PFormLayout;
begin
  Result := PFormLayout(Items[Index]);
end;

function TFormLayoutList.AddLayout(const AName: String): PFormLayout;
begin
  New(Result);
  FillChar(Result^, SizeOf(TFormLayout), 0);
  Result^.Name := AName;
  Result^.Controls := TControlLayoutList.Create;
  Add(Result);
end;

procedure TFormLayoutList.ApplyLayout(const AName: String; AForm: TdxForm);
var
  OnResizeArr, OnChangeBoundsArr: array of TNotifyEvent;
  Ctrls: TControlLayoutList;
  C: TdxControl;

  procedure DisableResizing;
  var
    j: Integer;
  begin
    SetLength(OnResizeArr, Ctrls.Count + 1);
    SetLength(OnChangeBoundsArr, Ctrls.Count + 1);
    OnResizeArr[0] := AForm.OnResize;
    OnChangeBoundsArr[0] := AForm.OnChangeBounds;
    AForm.OnResize := nil;
    AForm.OnChangeBounds := nil;
    for j := 1 to Ctrls.Count - 1 do
    begin
      //Debug(Ctrls[j]^.Name);
      C := TdxControl(AForm.FindComponent(Ctrls[j]^.Name));
      OnResizeArr[j] := C.OnResize;
      OnChangeBoundsArr[j] := C.OnChangeBounds;
      C.OnResize := nil;
      C.OnChangeBounds := nil;
      C.Anchors := [akLeft, akTop];
    end;
  end;

  procedure EnableResizing;
  var
    j: Integer;
  begin
    AForm.OnResize := OnResizeArr[0];
    AForm.OnChangeBounds := OnChangeBoundsArr[0];
    for j := 1 to Ctrls.Count - 1 do
    begin
      C := TdxControl(AForm.FindComponent(Ctrls[j]^.Name));
      C.OnResize := OnResizeArr[j];
      C.OnChangeBounds := OnChangeBoundsArr[j];
    end;
    SetLength(OnResizeArr, 0);
    SetLength(OnChangeBoundsArr, 0);
  end;

var
  i: Integer;
  pLay: PFormLayout;
  CLI: TControlLayoutItem;
begin
  pLay := FindLayout(AName);
  Ctrls := pLay^.Controls;

  DisableResizing;

  with AForm do
    SetBounds(Left, Top, pLay^.Width, pLay^.Height);

  for i := 0 to Ctrls.Count - 1 do
  begin
    CLI := Ctrls[i]^;
    C := TdxControl(AForm.FindComponent(CLI.Name));
    C.SetBounds(CLI.Left, CLI.Top, CLI.Width, CLI.Height);
    C.Anchors := CLI.Anchors;
  end;

  EnableResizing;
end;

function TFormLayoutList.FindLayout(const AName: String): PFormLayout;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Layouts[i]^.Name = AName then Exit(Layouts[i]);
end;

// Ищем наиболее подходящий по ширине макет. Ищем макет с наибольшей шириной,
// но меньше текущей ширины формы. Если такого нет, то выбираем минимальную
// ширину.
function TFormLayoutList.FindLayoutWidth(const AWidth: Integer): PFormLayout;
var
  i: Integer;
  pLay, pLayMax, pLayMin: PFormLayout;
begin
  pLayMax := nil;
  pLayMin := nil;
  for i := 0 to Count - 1 do
  begin
    pLay := Layouts[i];
    if not pLay^.Web or pLay^.Disabled then Continue;

    if pLayMin = nil then pLayMin := pLay
    else if pLay^.MinWidth < pLayMin^.MinWidth then pLayMin := pLay;

    if pLayMax = nil then
    begin
      if pLay^.MinWidth <= AWidth then pLayMax := pLay;
    end
    else if (pLay^.MinWidth > pLayMax^.MinWidth) and (pLay^.MinWidth <= AWidth) then pLayMax := pLay;
  end;

  if pLayMax <> nil then Result := pLayMax
  else Result := pLayMin;
end;

procedure TFormLayoutList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Layouts[i]^.Controls.Free;
    Dispose(Layouts[i]);
  end;
  inherited Clear;
end;

{ TFormLayoutFormList }

function TFormLayoutFormList.GetForms(Index: Integer): PFormLayoutForm;
begin
  Result := PFormLayoutForm(Items[Index]);
end;

function TFormLayoutFormList.AddForm(FmId: Integer): PFormLayoutForm;
begin
  New(Result);
  Result^.Id := FmId;
  Result^.Layouts := TFormLayoutList.Create;
  Add(Result);
end;

function TFormLayoutFormList.FindForm(FmId: Integer): PFormLayoutForm;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Forms[i]^.Id = FmId then Exit(Forms[i]);
end;

procedure TFormLayoutFormList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Forms[i]^.Layouts.Free;
    Dispose(Forms[i]);
  end;
  inherited Clear;
end;

procedure TFormLayoutFormList.LoadFromStream(AFm: PFormLayoutForm; St: TStream);
begin
  AFm^.Layouts.Clear;
  with TFormLayoutsReader.Create do
  try
    pFm := AFm;
    ParseStream(St);
  finally
    Free;
  end;
end;

procedure TFormLayoutFormList.SaveToStream(pFm: PFormLayoutForm; St: TStream);

  procedure WrStr(const S: String);
  begin
    St.Write(Pointer(S)^, Length(S));
  end;

  procedure WrControl(C: TControlLayoutItem);
  begin
    WrStr('<control name="' + C.Name +
      '" bounds="' + Format('%d;%d;%d;%d', [C.Left, C.Top, C.Width, C.Height]) +
      '" anchors="' + IntToStr(Cardinal(C.Anchors)) + '"/>');
  end;

  procedure WrLayout(Lay: TFormLayout);
  var
    i: Integer;
  begin
    WrStr('<layout name="' + StrToXml(Lay.Name) + '" web="' + Bool2Str(Lay.Web) +
      '" desktop="' + Bool2Str(Lay.Desktop) + '" minwidth="' + IntToStr(Lay.MinWidth) +
      '" fixedheight="' + Bool2Str(Lay.FixedHeight) +
      '" size="' + Format('%d;%d', [Lay.Width, Lay.Height]) +
      '" disabled="' + Bool2Str(Lay.Disabled) + '"><controls>');
    for i := 0 to Lay.Controls.Count - 1 do
      WrControl(Lay.Controls[i]^);
    WrStr('</controls></layout>');
  end;

var
  i: Integer;
begin
  WrStr('<layouts>');
  for i := 0 to pFm^.Layouts.Count - 1 do
    WrLayout(pFm^.Layouts[i]^);
  WrStr('</layouts>');
end;

end.

