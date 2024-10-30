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

unit PivotGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, dxctrls, dxreports, BGRAGraphics;

type
  TKHAlign = (halLeft, halCenter, halRight, halJustify);

  TKVAlign = (valTop, valCenter, valBottom);

  TKGridDrawStateMembers = (gdEdited, gdFixed, gdFocused, gdMouseDown, gdMouseOver,
    gdSelected, gdSorted, gdColsSortedUp, gdColsSortedDown, gdRowsSortedUp,
    gdRowsSortedDown);

  TKGridDrawState = set of TKGridDrawStateMembers;

  TKCellSpan = record
    ColSpan: Integer;
    RowSpan: Integer;
  end;

  TFieldCollection = class;

  { TFieldItem }

  TFieldItem = class
  private
    FCaption: String;
    FColor: TColor;
    FDataType: TRpFieldType;
    FFieldName: String;
    FFixedColor: TColor;
    FFixedFont: TdxFont;
    FFont: TdxFont;
    FFunc: TRpTotalFunc;
    FHAlign: TKHAlign;
    FHeight: Integer;
    FShowTotal: Boolean;
    FTotalColor: TColor;
    FTotalCaption: String;
    FTotalFixedColor: TColor;
    FTotalFixedFont: TdxFont;
    FTotalFont: TdxFont;
    FTotalWidth: Integer;
    FVAlign: TKVAlign;
    FWidth: Integer;
    FList: TFieldCollection;
    function GetIndex: Integer;
    procedure SetFixedFont(AValue: TdxFont);
    procedure SeTdxFont(AValue: TdxFont);
    procedure SetTotalFixedFont(AValue: TdxFont);
    procedure SetTotalFont(AValue: TdxFont);
  public
    constructor Create(AList: TFieldCollection);
    destructor Destroy; override;
    procedure Assign(Source: TFieldItem);
  published
    property FieldName: String read FFieldName write FFieldName;
    property Caption: String read FCaption write FCaption;
    property TotalCaption: String read FTotalCaption write FTotalCaption;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property TotalWidth: Integer read FTotalWidth write FTotalWidth;
    property Func: TRpTotalFunc read FFunc write FFunc;
    property ShowTotal: Boolean read FShowTotal write FShowTotal;
    property FixedColor: TColor read FFixedColor write FFixedColor;
    property TotalFixedColor: TColor read FTotalFixedColor write FTotalFixedColor;
    property Color: TColor read FColor write FColor;
    property TotalColor: TColor read FTotalColor write FTotalColor;
    property FixedFont: TdxFont read FFixedFont write SetFixedFont;
    property TotalFixedFont: TdxFont read FTotalFixedFont write SetTotalFixedFont;
    property Font: TdxFont read FFont write SeTdxFont;
    property TotalFont: TdxFont read FTotalFont write SetTotalFont;
    property VAlign: TKVAlign read FVAlign write FVAlign;
    property HAlign: TKHAlign read FHAlign write FHAlign;
    property DataType: TRpFieldType read FDataType write FDataType;
    property Index: Integer read GetIndex;
  end;

  { TFieldCollection }

  TFieldCollection = class(TList)
  private
    function GetFields(Index: Integer): TFieldItem;
  public
    procedure AssignList(Source: TFieldCollection);
    function AddField: TFieldItem;
    function FindFieldByFieldName(const S: String): TFieldItem;
    procedure Clear; override;
    property Fields[Index: Integer]: TFieldItem read GetFields; default;
  end;

  PPivotValue = ^TPivotValue;
  TPivotValue = record
    V: Variant;
    dt: TRpFieldType;
  end;

  { TPivotValues }

  TPivotValues = class(TList)
  private
    function GetValues(Index: Integer): PPivotValue;
  public
    procedure Clear; override;
    function FindValue(aV: Variant): Integer;
    procedure SortValues;
    procedure SortValuesPreview;
    property Values[Index: Integer]: PPivotValue read GetValues; default;
  end;

  TKGridCell = record
    Text: String;
    Obj: TObject;
    ColSpan, RowSpan: Integer;
  end;

  TKGridCells = array of array of TKGridCell;

  TColRowArray = array of Integer;

  { TKGridColors }

  TKGridColors = class
  public
    FixedCellBkGnd, CellBkgnd, FixedCellLines, CellLines: TColor;
    procedure Assign(Source: TKGridColors);
  end;

  { TdxPivotGrid }

  TdxPivotGrid = class(TdxControl)
  private
    FCells: TKGridCells;
    FColFields: TFieldCollection;
    FDataFields: TFieldCollection;
    FRowFields: TFieldCollection;
    FColWidths, FRowHeights: TColRowArray;
    procedure SetColFields(AValue: TFieldCollection);
    procedure SetDataFields(AValue: TFieldCollection);
    procedure SetRowFields(AValue: TFieldCollection);
    procedure ClearCell(var ACell: TKGridCell);
  private
    FColCount: Integer;
    FColors: TKGridColors;
    FCornerColor: TColor;
    FDataDelimiter: String;
    FDS: TDataSet;
    FFixedCols: Integer;
    FFixedFont: TdxFont;
    FFixedRows: Integer;
    FGrandTotalCaption: String;
    FGrandTotalColor: TColor;
    FGrandTotalFixedColor: TColor;
    FGrandTotalFixedFont: TdxFont;
    FGrandTotalFont: TdxFont;
    FGrandTotalWidth: Integer;
    FId: Integer;
    FIndent: Integer;
    FNeedBuild: Boolean;
    FOnBuild: TNotifyEvent;
    FPreview: Boolean;
    FRowCount: Integer;
    FSelectedFont: TdxFont;
    FShowGrandTotalX: Boolean;
    FShowGrandTotalY: Boolean;
    FWordWrap: Boolean;
    FBuilded: Boolean;
    function GetCells(Col, Row: Integer): String;
    function GetColSpan(Col, Row: Integer): Integer;
    function GetObjects(Col, Row: Integer): TObject;
    function GetRowSpan(Col, Row: Integer): Integer;
    procedure GetValues(const aField: String; dt: TRpFieldType; VL: TPivotValues;
      Filter: array of Variant);
    procedure BuildRows;
    procedure BuildCols;
    procedure FillData;
    procedure CalcRowTotal(r: Integer);
    procedure CalcColTotal(c: Integer);
    procedure HideTotals;
    procedure SetCells(Col, Row: Integer; AValue: String);
    procedure SetColCount(AValue: Integer);
    procedure SetColSpan(Col, Row: Integer; AValue: Integer);
    procedure SetFixedFont(AValue: TdxFont);
    procedure SetGrandTotalFixedFont(AValue: TdxFont);
    procedure SetGrandTotalFont(AValue: TdxFont);
    procedure SetObjects(Col, Row: Integer; AValue: TObject);
    procedure SetRowCount(AValue: Integer);
    procedure SetRowSpan(Col, Row: Integer; AValue: Integer);
    procedure SetSelectedFont(AValue: TdxFont);
    procedure CheckFields(FL: TFieldCollection);
    procedure ClearGrid;
  protected
    //function DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TKGridDrawState
    //  ): Boolean; override;
    //procedure Paint; override;
    //procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TdxComponent); override;
    destructor Destroy; override;
    procedure Build;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    //procedure GetCellStyle(ACol, ARow: Integer; out CSSClass: String; out AFont: TdxFont; out AHAlign: TKHAlign; out AVAlign: TKVAlign; IsPrint: Boolean);
    property DataSet: TDataSet read FDS write FDS;
    property Preview: Boolean read FPreview write FPreview;
    property NeedBuild: Boolean read FNeedBuild write FNeedBuild;
    property RowFields: TFieldCollection read FRowFields write SetRowFields;
    property ColFields: TFieldCollection read FColFields write SetColFields;
    property DataFields: TFieldCollection read FDataFields write SetDataFields;
    property GrandTotalFixedColor: TColor read FGrandTotalFixedColor write FGrandTotalFixedColor;
    property GrandTotalColor: TColor read FGrandTotalColor write FGrandTotalColor;
    property GrandTotalFixedFont: TdxFont read FGrandTotalFixedFont write SetGrandTotalFixedFont;
    property GrandTotalFont: TdxFont read FGrandTotalFont write SetGrandTotalFont;
    property GrandTotalWidth: Integer read FGrandTotalWidth write
      FGrandTotalWidth;
    property CornerColor: TColor read FCornerColor write FCornerColor;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property GrandTotalCaption: String read FGrandTotalCaption write FGrandTotalCaption;
    property ShowGrandTotalX: Boolean read FShowGrandTotalX write FShowGrandTotalX;
    property ShowGrandTotalY: Boolean read FShowGrandTotalY write FShowGrandTotalY;
    property Id: Integer read FId write FId;
    property FixedFont: TdxFont read FFixedFont write SetFixedFont;
    property SelectedFont: TdxFont read FSelectedFont write SetSelectedFont;
    property DataDelimiter: String read FDataDelimiter write FDataDelimiter;
    property Indent: Integer read FIndent write FIndent;
    property OnBuild: TNotifyEvent read FOnBuild write FOnBuild;

    property RowCount: Integer read FRowCount write SetRowCount;
    property ColCount: Integer read FColCount write SetColCount;
    property Cells[Col, Row: Integer]: String read GetCells write SetCells;
    property Objects[Col, Row: Integer]: TObject read GetObjects write SetObjects;
    property FixedRows: Integer read FFixedRows write FFixedRows;
    property FixedCols: Integer read FFixedCols write FFixedCols;
    property ColSpan[Col, Row: Integer]: Integer read GetColSpan write SetColSpan;
    property RowSpan[Col, Row: Integer]: Integer read GetRowSpan write SetRowSpan;
    property Colors: TKGridColors read FColors;
  end;

function PivotGridToHtml(Grid: TdxPivotGrid): String;

implementation

uses
  LazUtf8, Variants, apputils;

type
  PCalcTotal = ^TCalcTotal;
  TCalcTotal = record
    First: Boolean;
    sum: Double;
    V: Variant;
    n: Integer;
    RowN: Integer;
  end;

  PSpanInfo = ^TSpanInfo;
  TSpanInfo = record
    R, C, RSpan, CSpan: Integer;
  end;

function ColorToHtml(Color: TColor): String;
var
  S: String;
  RGB: LongInt;
begin
  //if ColorToIdent(Color, S) then Exit('""');
  RGB := ColorToRGB(Color);
  S := HexStr(RGB, 8);
  Result := '#' + Copy(S, 7, 2) + Copy(S, 5, 2) + Copy(S, 3, 2);
end;

function FontToHtml(Font: TdxFont): String;
begin
  Result := '';
  if fsItalic in Font.Style then Result := Result + 'italic ';
  if fsBold in Font.Style then Result := Result + 'bold ';
  if Font.Size > 0 then
  	Result := Result + IntToStr(Font.Size) + 'px '
  else
    Result := Result + '100% ';
  if (Font.Name > '') and (Font.Name <> 'default') then
  	Result := Result + Font.Name
  else
  	Result := Result + 'verdana';
  if Result <> '' then
	  Result := 'font:' + Result + ';';
  if Font.Color <> clDefault then
  	Result := Result + 'color:' + ColorToHtml(Font.Color) + ';';
end;

function VAlignToHtml(V: TKVAlign): String;
begin
  case V of
    valTop: Result := 'top';
    valCenter: Result := 'middle';
    valBottom: Result := 'bottom';
  end;
end;

function HAlignToHtml(V: TKHAlign): String;
begin
  case V of
    halLeft: Result := 'left';
    halCenter: Result := 'center';
    halRight: Result := 'right';
  end;
end;

procedure GetCellStyle(AGrid: TdxPivotGrid; ACol, ARow: Integer; out CSSClass: String;
  out ABorderColor, AColor: TColor; out AFont: TdxFont; out AHAlign: TKHAlign;
  out AVAlign: TKVAlign);
var
  i, ColFN, RowFN, DatFN: Integer;
  Clr, BrdClr: TColor;
  Fnt: TdxFont;

  function GetRowIdx(c: Integer): Integer;
  var
    j: Integer;
  begin
    Result := -1;
    for j := 0 to AGrid.ColFields.Count - 2 do
      if AGrid.RowSpan[c, j] > 1 then Exit(j);
  end;

  function GetColIdx(r: Integer): Integer;
  var
    j: Integer;
  begin
    Result := -1;
    for j := 0 to AGrid.RowFields.Count - 1 do
      if AGrid.Objects[j, r] <> nil then Exit(j);
  end;

begin
  Clr := clNone;
  BrdClr := clNone;
  CSSClass := '';
  Fnt := nil;
  ColFN := AGrid.ColFields.Count;
  RowFN := AGrid.RowFields.Count;
  DatFN := AGrid.DataFields.Count;

  // Левый верхний угол
  if (ARow = 0) and (ACol = 0) then
  begin
    CSSClass := 'pgrid-corner';
    Clr := AGrid.CornerColor;
    BrdClr := AGrid.Colors.FixedCellLines;
  end
  // Общий итог по оси Х фикс.
  else if (ACol <= RowFN) and (ARow >= AGrid.RowCount - DatFN) and AGrid.ShowGrandTotalX then
  begin
    CSSClass := 'pgrid-gtf-x';
    BrdClr := AGrid.Colors.FixedCellLines;
    Clr := TFieldItem(AGrid.Objects[RowFN, ARow]).FixedColor;
    Fnt := TFieldItem(AGrid.Objects[RowFN, ARow]).FixedFont;
    if Clr = clNone then
      Clr := AGrid.GrandTotalFixedColor;
    if Fnt.IsDefault then
      Fnt := AGrid.GrandTotalFixedFont;
  end
  // Обший итог по оси Х данные
  else if (ACol > RowFN) and (ARow >= AGrid.RowCount - DatFN) and AGrid.ShowGrandTotalX then
  begin
    CSSClass := 'pgrid-gt-x';
    //BrdClr := Colors.CellLines;
    Clr := TFieldItem(AGrid.Objects[RowFN, ARow]).Color;
    Fnt := TFieldItem(AGrid.Objects[RowFN, ARow]).Font;

    if Clr = clNone then
      Clr := AGrid.GrandTotalColor;
    if Fnt.IsDefault then
      Fnt := AGrid.GrandTotalFont;

    if AGrid.ShowGrandTotalY and (ACol = AGrid.ColCount - 1) then
    else
    begin
      i := GetRowIdx(ACol);
      if i >= 0 then
      begin
        if Clr = clNone then
          Clr := AGrid.ColFields[i].TotalColor;
        if Fnt.IsDefault then
          Fnt := AGrid.ColFields[i].TotalFont;
      end;
    end;
  end
  // Общий итог по оси У фикс.
  else if (ARow = 0) and (ACol = AGrid.ColCount - 1) and AGrid.ShowGrandTotalY then
  begin
    CSSClass := 'pgrid-gtf-y';
    BrdClr := AGrid.Colors.FixedCellLines;
    Clr := AGrid.GrandTotalFixedColor;
    Fnt := AGrid.GrandTotalFixedFont;
  end
  // Общий итог по оси У данные
  else if (ARow >= ColFN) and (ACol = AGrid.ColCount - 1) and AGrid.ShowGrandTotalY then
  begin
    CSSClass := 'pgrid-gt-y';
    //BrdClr := Colors.CellLines;
    Clr := TFieldItem(AGrid.Objects[RowFN, ARow]).Color;
    Fnt := TFieldItem(AGrid.Objects[RowFN, ARow]).Font;
    if Clr = clNone then
      Clr := AGrid.GrandTotalColor;
    if Fnt.IsDefault then
      Fnt := AGrid.GrandTotalFont;
  end
  // Поля строк
  else if ACol < RowFN then
  begin
    CSSClass := 'pgrid-cf';
    BrdClr := AGrid.Colors.FixedCellLines;
    Clr := AGrid.RowFields[ACol].FixedColor;
    Fnt := AGrid.RowFields[ACol].FixedFont;
  end
  // Поля данных фикс.
  else if (ACol = RowFN) and (ARow >= ColFN) then
  begin
    CSSClass := 'pgrid-cf';
    BrdClr := AGrid.Colors.FixedCellLines;
    Clr := TFieldItem(AGrid.Objects[RowFN, ARow]).FixedColor;
    Fnt := TFieldItem(AGrid.Objects[RowFN, ARow]).FixedFont;
    if ((Clr = clNone) or (Fnt.IsDefault)) and (AGrid.Objects[ACol, ARow] <> nil) then
    begin
      i := TFieldItem(AGrid.Objects[ACol, ARow]).Index;
      i := GetColIdx(ARow - i);
      if Clr = clNone then
        Clr := AGrid.RowFields[i].FixedColor;
      if Fnt.IsDefault then
        Fnt := AGrid.RowFields[i].FixedFont;
    end;
  end
  // Поля данных данные
  else if (ACol > RowFN) and (ARow >= ColFN) then
  begin
    CSSClass := 'pgrid-c';
    //BrdClr := Colors.CellLines;
    // Поля данных
    Clr := TFieldItem(AGrid.Objects[RowFN, ARow]).Color;
    Fnt := TFieldItem(AGrid.Objects[RowFN, ARow]).Font;
    i := TFieldItem(AGrid.Objects[RowFN, ARow]).Index;
    i := GetColIdx(ARow - i);
    if (Clr = clNone) or (Fnt.IsDefault) or (AGrid.RowFields[i].ShowTotal = False) then
    begin
      // Поля строк данные
      if (Clr = clNone) or ((AGrid.RowFields[i].ShowTotal = False) and
        (i < RowFN - 1)) then Clr := AGrid.RowFields[i].Color;
      if (Fnt.IsDefault) or ((AGrid.RowFields[i].ShowTotal = False) and
        (i < RowFN - 1)) then Fnt := AGrid.RowFields[i].Font;

      if (Clr = clNone) or (Fnt.IsDefault) then
        i := GetRowIdx(ACol);
      // Поля столбцов данные
      if Clr = clNone then
      begin
        if i < 0 then
          Clr := AGrid.ColFields[ColFN - 1].Color
        // Промежуточный итог по оси У
        else
        begin
          Clr := AGrid.ColFields[i].TotalColor;
          if Clr <> clNone then
            Clr := Clr;
        end;
      end;
      if Fnt.IsDefault then
      begin
        if i < 0 then
          Fnt := AGrid.ColFields[ColFN - 1].Font
        // Промежуточный итог по оси У
        else
          Fnt := AGrid.ColFields[i].TotalFont;
      end;
    end;
  end
  // Поля столбцов
  else if ARow < AGrid.ColFields.Count then
  begin
    CSSClass := 'pgrid-cf';
    BrdClr := AGrid.Colors.FixedCellLines;
    Clr := AGrid.ColFields[ARow].FixedColor;
    Fnt := AGrid.ColFields[ARow].FixedFont;
    if ARow < AGrid.ColFields.Count - 1 then
    begin
      if AGrid.RowSpan[ACol, ARow] > 1 then
      begin
        Clr := AGrid.ColFields[ARow].TotalFixedColor;
        Fnt := AGrid.ColFields[ARow].TotalFixedFont;
      end;
    end;
  end;

  // Стандартные
  //if IsPrint {and (Clr = clNone) or (Fnt = nil) or (Fnt.IsDefault)} then
  begin
    if (ARow < AGrid.FixedRows) or (ACol < AGrid.FixedCols) then
    begin
      if Clr = clNone then
	      Clr := AGrid.Colors.FixedCellBkGnd;
      if (Fnt = nil) or (Fnt.IsDefault) then
	      Fnt := AGrid.FixedFont;
    end;
  end;

  AVAlign := valCenter;
  AHAlign := halLeft;
  if (ACol > RowFN) and (ARow >= ColFn) and (AGrid.Objects[RowFN, ARow] <> nil) then
  begin
    with TFieldItem(AGrid.Objects[RowFN, ARow]) do
    begin
      AVAlign:=VAlign;
      AHAlign:=HAlign;
    end;
  end;

  ABorderColor := BrdClr; AColor := Clr; AFont := Fnt;
  //if IsPrint then CSSClass := '';
end;

function PivotGridToHtml(Grid: TdxPivotGrid): String;
var
  S, Tmp, Cls: String;
  r, c, i: Integer;
  Spans: TList;
  pSI: PSpanInfo;
  Clr, BrdClr: TColor;
  Fnt: TdxFont;
  HA: TKHAlign;
  VA: TKVAlign;

  function IsDefFont(Fnt: TdxFont): Boolean;
  begin
    Result := (Fnt.Name = Grid.Font.Name) and (Fnt.Size = Grid.Font.Size) and
      (Fnt.Style = Grid.Font.Style) and (Fnt.Color = Grid.Font.Color);
  end;

  function CellSpaned(Col, Row: Integer): Boolean;
  var
    j: Integer;
  begin
    Result := False;
    for j := Spans.Count - 1 downto 0 do
    begin
      with PSpanInfo(Spans[j])^ do
      	if (Col >= C) and (Row >= R) and (Col <= C + CSpan - 1) and (Row <= R + RSpan - 1) then
        	Exit(True);
    end;
  end;

  function GetTableWidth: Integer;
  var
    j: Integer;
  begin
    Result := 0;
    for j := 0 to Grid.ColCount - 1 do
    	Result := Result + Grid.FColWidths[j];
  end;

begin
  if Grid.NeedBuild then Grid.Build;

  Spans := TList.Create;

  S := '<table width=' + IntToStr(GetTableWidth) + ' border=1';
  if Grid.Colors.CellBkgnd <> clNone then
    S := S + ' bgcolor=' + ColorToHtml(Grid.Colors.CellBkgnd) + ' bordercolor=' +
      ColorToHtml(Grid.Colors.CellLines);
  S := S + ' style=" border-collapse: collapse;' + FontToHtml(Grid.Font) + '">';
  for r := 0 to Grid.RowCount - 1 do
  begin
    S := S + Format('<tr height=%dpx>', [Grid.FRowHeights[r]]);
  	for c := 0 to Grid.ColCount - 1 do
    begin
      if CellSpaned(c, r) then Continue;

      S := S + '<td';

      GetCellStyle(Grid, c, r, Cls, BrdClr, Clr, Fnt, HA, VA);
      if Clr <> clNone then
        S := S + ' bgcolor=' + ColorToHtml(Clr);
      {else if Cls <> '' then
        S := S + ' class=' + Cls;}
      S := S + Format(' width=%dpx', [Grid.FColWidths[c]]);

      if (Grid.ColSpan[c, r] > 1) or (Grid.RowSpan[c, r] > 1) then
      begin
        New(pSI);
        pSI^.C := c; pSI^.R := r;
        pSI^.CSpan := Grid.ColSpan[c, r]; pSI^.RSpan := Grid.RowSpan[c, r];
        Spans.Add(pSI);

        if Grid.ColSpan[c, r] > 1 then
          S := S + Format(' colspan=%d', [Grid.ColSpan[c, r]]);
        if Grid.RowSpan[c, r] > 1 then
          S := S + Format(' rowspan=%d', [Grid.RowSpan[c, r]]);
      end;

      if HA <> halLeft then
      	S := S + ' align=' + HAlignToHtml(HA);
      if VA <> valCenter then
      	S := S + ' valign=' + VAlignToHtml(VA);

      Tmp := '';
      if (Fnt <> nil) and not (IsDefFont(Fnt) or Fnt.IsDefault) then
        Tmp := Tmp + FontToHtml(Fnt);

      if (BrdClr <> clNone) and (BrdClr <> Grid.Colors.CellLines) then
        Tmp := Tmp + 'border-color:' + ColorToHtml(BrdClr);

      if Tmp <> '' then
      	S := S + ' style="' + Tmp + '"';

      S := S + '>';
      // Если текст не умещается в ячейку, то она будет автоматически расширена.
      // Это нежелательно, когда надо скрыть ячейки, установив ширину в 0.
      // Поэтому для нулевых текст пропускаем.
      if Grid.FColWidths[c] > 0 then
      	S := S + StrToHtml(Grid.Cells[c, r]);
      S := S + '</td>';
    end;
    S := S + '</tr>';
  end;
  Result := S + '</table>';

  for i := 0 to Spans.Count - 1 do
  	Dispose(PSpanInfo(Spans[i]));
  Spans.Free;
end;




{ TPivotValues }

function SortFunc(Item1, Item2: Pointer): Integer;
var
  V1, V2: PPivotValue;
begin
  V1 := PPivotValue(Item1);
  V2 := PPivotValue(Item2);
  if V1^.dt = flText then
    Result := MyUtf8CompareText(VarToStr(V1^.V), VarToStr(V2^.V))
  else
  begin
    if V1^.V > V2^.V then Result := 1
    else if V1^.V < V2^.V then Result := -1
    else Result := 0;
  end;
end;

function SortFuncPreview(Item1, Item2: Pointer): Integer;
var
  V1, V2: PPivotValue;
begin
  V1 := PPivotValue(Item1);
  V2 := PPivotValue(Item2);
  Result := MyUtf8CompareText(VarToStr(V1^.V), VarToStr(V2^.V));
end;

{ TKGridColors }

procedure TKGridColors.Assign(Source: TKGridColors);
begin
  FixedCellBkGnd := Source.FixedCellBkGnd;
  CellBkgnd := Source.CellBkgnd;
  FixedCellLines := Source.FixedCellLines;
  CellLines := Source.CellLines;
end;

function TPivotValues.GetValues(Index: Integer): PPivotValue;
begin
  Result := PPivotValue(Items[Index]);
end;

procedure TPivotValues.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Dispose(Values[i]);
  inherited Clear;
end;

function TPivotValues.FindValue(aV: Variant): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Values[i]^.V = aV then Exit(i);
end;

procedure TPivotValues.SortValues;
begin
  Sort(@SortFunc);
end;

procedure TPivotValues.SortValuesPreview;
begin
  Sort(@SortFuncPreview);
end;

{ TFieldItem }

procedure TFieldItem.SetFixedFont(AValue: TdxFont);
begin
  FFixedFont.Assign(AValue);
end;

function TFieldItem.GetIndex: Integer;
begin
  Result := FList.IndexOf(Self);
end;

procedure TFieldItem.SeTdxFont(AValue: TdxFont);
begin
  FFont.Assign(AValue);
end;

procedure TFieldItem.SetTotalFixedFont(AValue: TdxFont);
begin
  FTotalFixedFont.Assign(AValue);
end;

procedure TFieldItem.SetTotalFont(AValue: TdxFont);
begin
  FTotalFont.Assign(AValue);
end;

constructor TFieldItem.Create(AList: TFieldCollection);
begin
  FList := AList;
  FFont := TdxFont.Create;
  FTotalFont := TdxFont.Create;
  FFixedFont := TdxFont.Create;
  FTotalFixedFont := TdxFont.Create;
  FHAlign := halLeft;
  FVAlign := valCenter;
  Width := 80; Height := 20;
  TotalWidth := 80;
  Color:=clNone; TotalColor := clNone;
  FixedColor := clNone; TotalFixedColor := clNone;
end;

destructor TFieldItem.Destroy;
begin
  FFont.Free;
  FTotalFont.Free;
  FFixedFont.Free;
  FTotalFixedFont.Free;
  inherited Destroy;
end;

procedure TFieldItem.Assign(Source: TFieldItem);
begin
  FieldName := Source.FieldName;
  Caption := Source.Caption;
  Width := Source.Width;
  Height := Source.Height;
  TotalWidth := Source.TotalWidth;
  Func := Source.Func;
  ShowTotal := Source.ShowTotal;
  TotalCaption := Source.TotalCaption;
  FixedColor := Source.FixedColor;
  Color := Source.Color;
  FTotalColor := Source.TotalColor;
  TotalFixedColor := Source.TotalFixedColor;
  FixedFont := Source.FixedFont;
  Font := Source.Font;
  TotalFont := Source.TotalFont;
  TotalFixedFont := Source.TotalFixedFont;
  VAlign := Source.VAlign;
  HAlign := Source.HAlign;
  DataType := Source.DataType;
end;

{ TFieldCollection }

function TFieldCollection.GetFields(Index: Integer): TFieldItem;
begin
  Result := TFieldItem(Items[Index]);
end;

procedure TFieldCollection.AssignList(Source: TFieldCollection);
var
  i: Integer;
  PF: TFieldItem;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
  begin
    PF := AddField;
    PF.Assign(Source[i]);
  end;
end;

function TFieldCollection.AddField: TFieldItem;
begin
  Result := TFieldItem.Create(Self);
  Add(Result);
end;

function TFieldCollection.FindFieldByFieldName(const S: String): TFieldItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(Fields[i].FieldName, S) = 0 then Exit(Fields[i]);
end;

procedure TFieldCollection.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Fields[i].Free;
  inherited Clear;
end;

{ TdxPivotGrid }

procedure TdxPivotGrid.SetColFields(AValue: TFieldCollection);
begin
  FColFields.AssignList(AValue);
end;

procedure TdxPivotGrid.SetDataFields(AValue: TFieldCollection);
begin
  FDataFields.AssignList(AValue);
end;

procedure TdxPivotGrid.SetRowFields(AValue: TFieldCollection);
begin
  FRowFields.AssignList(AValue);
end;

procedure TdxPivotGrid.ClearCell(var ACell: TKGridCell);
begin
  ACell.ColSpan := 1;
  ACell.RowSpan := 1;
  ACell.Obj := nil;
  ACell.Text := '';
end;

procedure TdxPivotGrid.GetValues(const aField: String; dt: TRpFieldType;
  VL: TPivotValues; Filter: array of Variant);
var
  F: TField;
  V: Variant;
  pV: PPivotValue;

  function GetFieldValue(F: TField): Variant;
  begin
  	case dt of
      flText: Result := F.Text;
      flNumber: Result := F.AsFloat;
      flDate, flTime: Result := F.AsDateTime;
      else Result := Null;
    end
  end;

  function IsEqual: Boolean;
  var
    i: Integer;
    FF: TField;
  begin
    Result := True;
    if Length(Filter) = 0 then Exit;

    i := 0;
    while i < Length(Filter) do
    begin
      FF := FDS.FieldByName(Filter[i]);
      if VarToStr(FF.Value) <> VarToStr(Filter[i + 1]) then Exit(False);
      //if GetFieldValue(FF) <> Filter[i + 1] then Exit(False);
      i := i + 2;
    end;
  end;

begin
  VL.Clear;
  FDS.First;
  F := FDS.FieldByName(aField);
  while not FDS.Eof do
  begin
    if IsEqual then
    begin
      V := '';
      if not FPreview then
        {case dt of
          flText: V := F.Text;
          flNumber: V := F.AsFloat;
          flDate, flTime: V := F.AsDateTime;
        end }
      	V := GetFieldValue(F)
      else
        V := F.Text;

      if VL.FindValue(V) < 0 then
      begin
        New(pV);
        pV^.V := V;
        pV^.dt := dt;
        VL.Add(pV);
      end;
    end;
    FDS.Next;
  end;
  if not FPreview then
    VL.SortValues
  else
    VL.SortValuesPreview;
end;

function TdxPivotGrid.GetCells(Col, Row: Integer): String;
begin
  Result := FCells[Row, Col].Text;
end;

function TdxPivotGrid.GetColSpan(Col, Row: Integer): Integer;
begin
  Result := FCells[Row, Col].ColSpan;
  if Result = 0 then Result := 1;
end;

function TdxPivotGrid.GetObjects(Col, Row: Integer): TObject;
begin
  Result := FCells[Row, Col].Obj;
end;

function TdxPivotGrid.GetRowSpan(Col, Row: Integer): Integer;
begin
  Result := FCells[Row, Col].RowSpan;
  if Result = 0 then Result := 1;
end;

procedure TdxPivotGrid.BuildRows;
var
  RowN: Integer;
  NN, N, j: Integer;

  function _Build(Idx: Integer; Filter: array of Variant): Integer;
  var
    Flt: array of Variant;
    i, j, OldRowN, RowSpn, RN: Integer;
    VL: TPivotValues;
  begin
    OldRowN := RowN;
    SetLength(Flt, Length(Filter) + 2);
    for i := 0 to Length(Filter) - 1 do
      Flt[i] := Filter[i];

    VL := TPivotValues.Create;
    GetValues(FRowFields[Idx].FieldName, FRowFields[Idx].DataType, VL, Filter);
    for i := 0 to VL.Count - 1 do
    begin
      RowCount := RowN + NN;
      Cells[Idx, RowN] := VarToStr(VL[i]^.V);
      Objects[Idx, RowN] := TObject(Idx + 2);
      ColSpan[Idx, RowN] := N - Idx;
      RowSpan[Idx, RowN] := NN;
      for j := 0 to NN - 1 do
      begin
        Cells[N, RowN + j] := FDataFields[j].Caption;
        Objects[N, RowN + j] := FDataFields[j];
      end;

      RN := RowN;
      RowN := RowN + NN;
      Flt[Length(Flt) - 2] := FRowFields[Idx].FieldName;
      Flt[Length(Flt) - 1] := VL[i]^.V;
      if Idx < FRowFields.Count - 1 then
      begin
        RowSpn := _Build(Idx + 1, Flt);
        RowSpan[Idx, RN + NN] := RowSpn;
      end;
    end;

    VL.Free;
    SetLength(Flt, 0);
    Result := RowN - OldRowN;
  end;

begin
  FixedCols := FRowFields.Count + 1;
  for j := 0 to FRowFields.Count - 2 do
    FColWidths[j] := FIndent;

  RowN := FColFields.Count;
  NN := FDataFields.Count;
  N := FRowFields.Count;
  _Build(0, []);
  RowCount := RowN + NN;
  Cells[0, RowN] := '';
  ColSpan[0, RowN] := FRowFields.Count;
  RowSpan[0, RowN] := NN;
  for j := 0 to NN - 1 do
  begin
    Cells[N, RowN + j] := FDataFields[j].Caption;
    Objects[N, RowN + j] := FDataFields[j];
  end;
  Objects[0, RowN] := TObject(1);
  Cells[0, RowN] := FGrandTotalCaption;

  N := FRowFields.Count;
  FColWidths[N - 1] := FRowFields[N - 1].Width;
  FColWidths[N] := FDataFields[0].Width;
  for j := FColFields.Count to RowCount - 1 do
  begin
    FRowHeights[j] := TFieldItem(Objects[FRowFields.Count, j]).Height;
  end;
end;

// В промежуточныъ итогах хранится индекс поля. Он увеличен на 2, потому что
// индекс поля начинается с 0 и необходимо хранить "маячок" общего итога (значение 1).
// Число столбцов увеличивается на 100. Увеличение столбцов медленная операция,
// т. к. нужно выделить память для каждой строки. Чтобы уменьшить число операций
// выделения, число столбцов увеличивается не на 1, а на 100.
procedure TdxPivotGrid.BuildCols;
var
  ColN, i, j: Integer;

  function _Build(Idx: Integer; Filter: array of Variant): Integer;
  var
    VL: TPivotValues;
    i, OldColN, CN, ColSpn: Integer;
    Flt: array of Variant;
  begin
    OldColN := ColN;
    SetLength(Flt, Length(Filter) + 2);
    for i := 0 to Length(Filter) - 1 do
      Flt[i] := Filter[i];

    VL := TPivotValues.Create;
    GetValues(FColFields[Idx].FieldName, FColFields[Idx].DataType, VL, Filter);
    for i := 0 to VL.Count - 1 do
    begin
      if ColN > ColCount - 1 then
        ColCount := ColCount + 100;
      Cells[ColN, Idx] := VarToStr(VL[i]^.V);
      CN := ColN;
      Flt[Length(Flt) - 2] := FColFields[Idx].FieldName;
      Flt[Length(Flt) - 1] := VL[i]^.V;
      if Idx < FColFields.Count - 1 then
      begin
        ColSpn := _Build(Idx + 1, Flt);
        ColSpan[CN, Idx] := ColSpn;
      end;
      if FColFields[Idx].ShowTotal then
      begin
        if ColN > ColCount - 1 then ColCount := ColCount + 100;
        Inc(ColN);
        RowSpan[ColN, Idx] := FColFields.Count - Idx;
        Objects[ColN, Idx] := TObject(Idx+2);
        Cells[ColN, Idx] := FColFields[Idx].TotalCaption;
      end;
      Inc(ColN);
    end;
    VL.Free;
    SetLength(Flt, 0);
    Result := ColN - OldColN;
    Dec(ColN);
  end;

begin
  FixedRows := FColFields.Count;
  ColCount := 100;
  ColN := FRowFields.Count + 1;
  _Build(0, []);
  ColCount := ColN + 2;
  Objects[ColCount - 1, 0] := TObject(1);
  Cells[ColCount - 1, 0] := FGrandTotalCaption;

  for i := FRowFields.Count + 1 to ColCount - 1 do
  begin
    FColWidths[i] := FColFields[FColFields.Count - 1].Width;
    for j := FColFields.Count - 2 downto 0 do
      if Objects[i, j] <> nil then
      begin
        FColWidths[i] := FColFields[j].TotalWidth;
      end;
  end;
  if FShowGrandTotalY then FColWidths[ColCount - 1] := FGrandTotalWidth;
  for i := 0 to FColFields.Count - 1 do
    FRowHeights[i] := FColFields[i].Height;
end;

procedure TdxPivotGrid.FillData;
var
  r, c, i: Integer;
  DFL: TList;
  SL: TStringList;
  S: String;

  function FindRow(Vals: TStringList): Integer;
  var
    i, RowN, NN: Integer;
  begin
    RowN := FColFields.Count;
    NN := FDataFields.Count;
    for i := 0 to FRowFields.Count - 1 do
    begin
      while RowN < RowCount - NN do
      begin
        if (PtrInt(Objects[i, RowN]) - 2 = i) and (Cells[i, RowN] = Vals[i]) then Break;
        RowN := RowN + NN;
      end;
    end;
    Result := RowN;
  end;

  function FindCol(Vals: TStringList): Integer;
  var
    i, ColN: Integer;
  begin
    Result := 0;
    ColN := FRowFields.Count;
    for i := 0 to FColFields.Count - 1 do
    begin
      while ColN < ColCount - 1 do
      begin
        if Cells[ColN, i] = Vals[i] then
          Break;
        Inc(ColN);
      end;
    end;
    Result := ColN;
  end;

begin
  DFL := TList.Create;
  SL := TStringList.Create;
  FDS.First;
  for i := 0 to FDataFields.Count - 1 do
    DFL.Add(FDS.FieldByName(FDataFields[i].FieldName));

  while FDS.EOF = False do
  begin
    SL.Clear;
    for i := 0 to FRowFields.Count - 1 do
      SL.Add(FDS.FieldByName(FRowFields[i].FieldName).AsString);
    r := FindRow(SL);
    SL.Clear;
    for i := 0 to FColFields.Count - 1 do
      SL.Add(FDS.FieldByName(FColFields[i].FieldName).AsString);
    c := FindCol(SL);
    if (r >= FixedRows) and (c >= FixedCols) then
    begin
      for i := 0 to DFL.Count - 1 do
        if DFL[i] <> nil then
        begin
          S := Cells[c, r + i];
          if TField(DFL[i]).IsNull then Continue;
          if S <> '' then S := S + FDataDelimiter;
          Cells[c, r + i] := S + TField(DFL[i]).Text;
        end;
    end;
    FDS.Next;
  end;
  DFL.Free;
  SL.Free;
end;

function ConvertValue(const S: String; dt: TRpFieldType; out
  V: Variant): Boolean;
var
  E: Double;
  N: integer;
  D: TDateTime;
  SS: String;
begin
  Result := True;
  case dt of
    flNumber:
      begin
        SS := StringReplace(S, DefaultFormatSettings.ThousandSeparator, '', [rfReplaceAll]);
        Result := TryStrToFloat(SS, E);
        if Result then V := E;
      end;
    flObject, flBool, flCounter, flRecId:
      begin
        Result := TryStrToInt(S, N);
        if Result then V := N;
      end;
    flDate, flTime:
      begin
        Result := TryStrToDateTime(S, D);
        if Result then V := D;
      end
    else
      V := S;
  end;
end;

procedure _DoCalc(pCT: PCalcTotal; fn: TRpTotalFunc; V: Variant);
begin
  if pCT^.First then
  begin
    pCT^.V := V;
    pCT^.n := 1;
    pCT^.First := False;
  end
  else
  begin
    case fn of
      tfSum, tfAvg: pCT^.V := pCT^.V + V;
      tfMax: if V > pCT^.V then pCT^.V := V;
      tfMin: if V < pCT^.V then pCT^.V := V;
    end;
	  Inc(pCT^.n)
  end;
end;

procedure DoCalc(pCT: PCalcTotal; fn: TRpTotalFunc; dt: TRpFieldType; const S, Delim: String);
var
  V: Variant;
  SL: TStringList;
  i: Integer;
begin
  if (fn = tfNone) or (S = '') then Exit;

  if ConvertValue(S, dt, V) then
  	_DoCalc(pCT, fn, V)
  // Считаем множественное значение в ячейке
  else
  begin
    SL := TStringList.Create;
    SplitStr(S, Delim, SL);
    for i := 0 to SL.Count - 1 do
    begin
      if SL[i] = '' then Continue;
      if ConvertValue(SL[i], dt, V) then
  			_DoCalc(pCT, fn, V);
    end;
    SL.Free;
  end;
end;

function GetResult(pCT: PCalcTotal; fn: TRpTotalFunc; dt: TRpFieldType; const Fmt: String): String;
var
  V: Variant;
begin
  Result := '';
  V := Null;
  case fn of
    tfSum, tfMax, tfMin: V := pCT^.V;
    tfAvg: V := pCT^.V / pCT^.n;
    tfCount: V := pCT^.n;
  end;
  if V <> Null then
    case dt of
      flDate: Result := DateToStr(V);
      flTime: Result := TimeToStr(V);
      else
      begin
        if (dt = flNumber) and (fn <> tfCount) and (Fmt <> '') then
          Result := FormatFloat(Fmt, V)
        else
	        Result := VarToStr(V);
      end;
    end;
end;

function GetValueDisplayFormat(FDS: TDataSet; FI: TFieldItem): String;
var
  F: TField;
begin
  Result := '';
  if FI.Func = tfCount then Exit;
  F := FDS.FieldByName(FI.FieldName);
  if F is TNumericField then
  	Result := TNumericField(F).DisplayFormat;
end;

procedure TdxPivotGrid.CalcRowTotal(r: Integer);
var
  i, j, idx: Integer;
  fn: TRpTotalFunc;
  Tots: TList;
  pCT: PCalcTotal;
  dt: TRpFieldType;
  FI: TFieldItem;
  fmt: String;

  function FindIdx(c: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := 0 to FColFields.Count - 1 do
      if Objects[c, i] <> nil then Exit(PtrInt(Objects[c, i]) - 1);
  end;

  // Строки с итогами имеют объединенные ячейки
  function IsRowTotal(r: Integer): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to FRowFields.Count - 1 do
      if ColSpan[i, r] > 1 then Exit(True);
  end;

begin
  FI := TFieldItem(Objects[FRowFields.Count, r]);
  fn := FI.Func;
  if fn = tfNone then Exit;
  dt := FI.DataType;
  fmt := GetValueDisplayFormat(FDS, FI);

  // Функция "Количество" неправильно считает итог в итоговой строке, меняем ее на "Сумма".
  if (fn = tfCount) and IsRowTotal(r) then
  begin
    fn := tfSum;
    dt := flNumber;
  end;

  Tots := TList.Create;
  for i := 0 to FColFields.Count do        { + Общий итог}
  begin
    New(pCT);
    pCT^.First := True;
    Tots.Add(pCT);
  end;

  for i := RowFields.Count + 1 to ColCount - 1 do
  begin
    idx := FindIdx(i);

    if idx < 0 then
    begin
      for j := 0 to Tots.Count - 1 do
      begin
        if j > 0 then
          if not FColFields[j-1].ShowTotal then Continue;
        pCT := PCalcTotal(Tots[j]);
	      DoCalc(pCT, fn, dt, Cells[i, r], FDataDelimiter);
      end;
    end
    else
    begin
      pCT := PCalcTotal(Tots[idx]);
      if not pCT^.First then
      begin
        Cells[i, r] := GetResult(pCT, fn, dt, fmt);
        pCT^.First := True;
      end;
    end;
  end;

  for i := 0 to Tots.Count - 1 do
    Dispose(PCalcTotal(Tots[i]));
  Tots.Free;
end;

procedure TdxPivotGrid.CalcColTotal(c: Integer);
var
  Tots: TList;
  pCT: PCalcTotal;
  i, j, idx, oldIdx, NN, RowN: Integer;
  fn: TRpTotalFunc;
  dt: TRpFieldType;
  FI: TFieldItem;
  fmt: String;

  function FindIdx(r: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := FRowFields.Count - 1 downto 0 do
      if Objects[i, r] <> nil then Exit(PtrInt(Objects[i, r]) - 1);
  end;

begin
  Tots := TList.Create;
  for i := 0 to FRowFields.Count do        { + Общий итог}
  begin
    New(pCT);
    pCT^.First := True;
    pCT^.RowN := 0;
    Tots.Add(pCT);
  end;

  oldIdx := -1;
  NN := FDataFields.Count;
  for i := 0 to NN - 1 do
  begin
    RowN := FColFields.Count + i;
    FI := TFieldItem(Objects[FRowFields.Count, RowN]);
    fn := FI.Func;
    dt := FI.DataType;
    fmt := GetValueDisplayFormat(FDS, FI);

    PCalcTotal(Tots[0])^.RowN:=RowCount - NN + i;
    while RowN < RowCount do
    begin
      idx := FindIdx(RowN - i);
      if idx < oldIdx then
      begin
        for j := idx to oldIdx-1 do
        begin
          pCT := PCalcTotal(Tots[j]);
          if not pCT^.First then
          begin
            Cells[c, pCT^.RowN] := GetResult(pCT, fn, dt, fmt);
            pCT^.First := True;
          end;
        end;
      end
      else if idx = Tots.Count - 1 then
      begin
        for j := 0 to Tots.Count - 1 do
        begin
          pCT := PCalcTotal(Tots[j]);
          DoCalc(pCT, fn, dt, Cells[c, RowN], FDataDelimiter);
        end;
      end;
      oldIdx := idx;
      PCalcTotal(Tots[idx])^.RowN := RowN;
      RowN := RowN + NN;
    end;
  end;

  for i := 0 to Tots.Count - 1 do
    Dispose(PCalcTotal(Tots[i]));
  Tots.Free;
end;

procedure TdxPivotGrid.HideTotals;
var
  i, j, z, RowN, NN: Integer;
begin
  NN := FDataFields.Count;
  for i := FRowFields.Count - 2 downto 0 do
  begin
    if not FRowFields[i].ShowTotal then
    begin
      RowN := FColFields.Count;
      while RowN < RowCount - NN do
      begin
        if Objects[i, RowN] <> nil then
        begin
          for j := FRowFields.Count to ColCount - 1 do
          begin
            for z := 0 to NN - 1 do
            begin
              if j = FRowFields.Count then
              begin
                //RowHeights[RowN + z] := 0;
              end;
              Cells[j, RowN + z] := '';
            end;
            RowSpan[j, RowN] := NN;
          end;
          ColSpan[i, RowN] := FRowFields.Count - i + 1;
          RowSpan[i, RowN] := NN;
          //RowHeights[RowN] := FRowFields[i].Height;
        end;
        RowN := RowN + NN;
      end;
    end;
  end;
  if not FShowGrandTotalX then
    RowCount := RowCount - NN;
  if not ShowGrandTotalY then
    ColCount := ColCount - 1;
end;

procedure TdxPivotGrid.SetCells(Col, Row: Integer; AValue: String);
begin
  FCells[Row, Col].Text := AValue;
end;

procedure TdxPivotGrid.SetColCount(AValue: Integer);
var
  i, j: Integer;
begin
  if FColCount = AValue then Exit;

  for i := 0 to RowCount - 1 do
  begin
    SetLength(FCells[i], AValue);
    for j := FColCount to AValue - 1 do
      ClearCell(FCells[i, j]);
  end;
  SetLength(FColWidths, AValue);
  for i := FColCount to AValue - 1 do
    FColWidths[i] := 40;
  FColCount := AValue;
end;

procedure TdxPivotGrid.SetColSpan(Col, Row: Integer; AValue: Integer);
begin
  if AValue > 0 then
    FCells[Row, Col].ColSpan := AValue;
end;

procedure TdxPivotGrid.SetFixedFont(AValue: TdxFont);
begin
  FFixedFont.Assign(AValue);
end;

{procedure TdxPivotGrid.HideTotals;
var
  i, j, NN: Integer;
  S: String;

  procedure ChangeParentRowSpan;
  begin

  end;

begin
  NN := FDataFields.Count;
  CellSpan[0, RowCount - NN] := MakeCellSpan(1, 1);
  for i := FRowFields.Count - 2 downto 0 do
  begin
    if not FRowFields[i].ShowTotal then
    begin
      for j := RowCount - NN - NN downto FColFields.Count do
        if Objects[i, j] <> nil then
        begin
          CellSpan[i, j] := MakeCellSpan(1, 1);
          DeleteRows(j+1, NN-1);
          Cells[NN, j] := '';
          CellSpan[i, j] := MakeCellSpan(NN - i + 1, 1);
          Objects[NN, j] := FRowFields[i];                 // Подменяем FDataFields
          RowHeights[j] := FRowFields[i].Height;
        end;
    end;
  end;
  //CellSpan[0, RowCount - NN] := MakeCellSpan(FRowFields.Count - 1, NN);
end;   }

procedure TdxPivotGrid.SetGrandTotalFixedFont(AValue: TdxFont);
begin
  FGrandTotalFixedFont.Assign(AValue);
end;

procedure TdxPivotGrid.SetGrandTotalFont(AValue: TdxFont);
begin
  FGrandTotalFont.Assign(AValue);
end;

procedure TdxPivotGrid.SetObjects(Col, Row: Integer; AValue: TObject);
begin
  FCells[Row, Col].Obj := AValue;
end;

procedure TdxPivotGrid.SetRowCount(AValue: Integer);
var
  i, j: Integer;
begin
  if FRowCount = AValue then Exit;

  SetLength(FCells, AValue);
  SetLength(FRowHeights, AValue);
  if FRowCount < AValue then
  begin
    for i := FRowCount to AValue - 1 do
    begin
      SetLength(FCells[i], FColCount);
      for j := 0 to FColCount - 1 do
        ClearCell(FCells[i, j]);
      FRowHeights[i] := 20;
    end;
  end;
  FRowCount := AValue;
end;

procedure TdxPivotGrid.SetRowSpan(Col, Row: Integer; AValue: Integer);
begin
  if AValue > 0 then
    FCells[Row, Col].RowSpan := AValue;
end;

procedure TdxPivotGrid.SetSelectedFont(AValue: TdxFont);
begin
  FSelectedFont.Assign(AValue);
end;

procedure TdxPivotGrid.CheckFields(FL: TFieldCollection);
var
  i: Integer;
  F: TField;
begin
  for i := FL.Count - 1 downto 0 do
  begin
    F := FDS.FindField(FL[i].FieldName);
    if F = nil then FL.Delete(i);
  end;
end;

procedure TdxPivotGrid.ClearGrid;
begin
  ColCount := 0;
  RowCount := 0;
  FixedCols := 0;
  FixedRows := 0;
end;

{function TdxPivotGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TKGridDrawState): Boolean;
var
  ColFN, RowFN: Integer;
  Clr: TColor;
  Fnt: TdxFont;
  VA: TKVAlign;
  HA: TKHAlign;
begin
  if not FBuilded then
  begin
    Result := inherited DrawCell(ACol, ARow, ARect, AState);
    Exit;
  end;

  if FWordWrap then
  begin
    CellPainter.Attributes:=[taWordBreak, taClip, taTrimWhiteSpaces];
  end
  else
    CellPainter.Attributes:=[taClip];

  ColFN := FColFields.Count;
  RowFN := FRowFields.Count;

  GetCellStyle(ACol, ARow, Clr, Fnt, HA, VA, False);
  if gdFocused in AState then
  begin
    Fnt := FSelectedFont;
    Clr := Colors.FocusedCellBkGnd;
  end;

  if (ACol > RowFN) and (ARow >= ColFN) and (Objects[RowFN, ARow] <> nil) then
  begin
    with TFieldItem(Objects[RowFN, ARow]) do
    begin
      Self.CellPainter.VAlign:=VA;
      Self.CellPainter.HAlign:=HA;
    end;
  end;

  if (Fnt <> nil) and Fnt.IsDefault and ((ACol < FixedCols) or (ARow < FixedRows)) then
    Fnt := FFixedFont;

  if (Fnt <> nil) and (not Fnt.IsDefault) then
    Canvas.Font.Assign(Fnt);
  if Clr <> clNone then
    Canvas.Brush.Color := Clr;

  Result:=inherited DrawCell(ACol, ARow, ARect, AState);
end;

procedure TdxPivotGrid.Paint;
var
  TS: TTextStyle;
begin
  inherited Paint;
  if csDesigning in ComponentState then
  begin
    TS := Canvas.TextStyle;
    TS.Alignment := taCenter;
    TS.Layout := tlCenter;
    TS.SingleLine := False;
    TS.WordBreak := True;
    Canvas.TextRect(ClientRect, 0, 0, Name, TS);
  end;
end;     }

{procedure TdxPivotGrid.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  Invalidate;
end;}

procedure SetDefaultdxFont(F: TdxFont);
begin
  F.Name := 'Verdana';
  //F.Height := -13;
  F.Size := 13;
end;

constructor TdxPivotGrid.Create(AOwner: TdxComponent);
begin
  inherited Create(AOwner);
  FColors := TKGridColors.Create;
  FColors.CellBkgnd := clWhite;
  FColors.FixedCellBkGnd := $DDDDDD;
  FColors.CellLines := clSilver;
  Width := 300;
  Height := 150;
  //DefaultRowHeight := 20;
  //DefaultColWidth := 50;
  FRowFields := TFieldCollection.Create;
  FColFields := TFieldCollection.Create;
  FDataFields := TFieldCollection.Create;
  SetDefaultdxFont(Font);
  FFixedFont := TdxFont.Create;
  SetDefaultdxFont(FFixedFont);
  FSelectedFont := TdxFont.Create;
  SetDefaultdxFont(FSelectedFont);
  FGrandTotalFont := TdxFont.Create;
  SetDefaultdxFont(FGrandTotalFont);
  FGrandTotalFixedFont := TdxFont.Create;
  SetDefaultdxFont(FGrandTotalFixedFont);
  FGrandTotalColor := Colors.FixedCellBkGnd;// clNone;
  FGrandTotalFixedColor := Colors.FixedCellBkGnd;// clNone;
  FCornerColor := Colors.FixedCellBkGnd;// clNone;
  FGrandTotalWidth := 50;
  FDataDelimiter := '; ';
  Indent := 10;
  {Options := Options - [goHeader, goThemedCells, goIndicateHiddenCells, goRangeSelect] +
    [goIndicateSelection, goRowSizing, goColSizing]; }
  //MinColWidth:=1;
  //MinRowHeight:=1;
  ColCount := 2;
  RowCount := 2;
end;

destructor TdxPivotGrid.Destroy;
begin
  FColors.Free;
  FSelectedFont.Free;
  FFixedFont.Free;
  FGrandTotalFixedFont.Free;
  FGrandTotalFont.Free;
  FDataFields.Free;
  FColFields.Free;
  FRowFields.Free;
  inherited Destroy;
end;

procedure TdxPivotGrid.Build;
var
  i: Integer;
  AftScr: TDataSetNotifyEvent;
  B: TBookmark;
begin
  //if not FPreview then FindQueryGrid(TdxForm(Owner), FId).RequeryIfNeed;

  FNeedBuild := False;

  CheckFields(FRowFields);
  CheckFields(FColFields);
  CheckFields(FDataFields);

  FBuilded := False;
  AftScr := FDS.AfterScroll;
  FDS.AfterScroll := nil;
  B := FDS.GetBookmark;
  try try
    ClearGrid;

    if (FColFields.Count = 0) or (FRowFields.Count = 0) or (FDataFields.Count = 0)
    	or (FDS.RecordCount = 0) then Exit;

    ColCount := 100; RowCount := 100;
    BuildRows;
    BuildCols;
    RowSpan[0, 0] := FColFields.Count;
    ColSpan[0, 0] := FRowFields.Count + 1;
    RowSpan[ColCount - 1, 0] := FColFields.Count;
    FillData;
    for i := RowFields.Count + 1 to ColCount - 1 do
      CalcColTotal(i);
    for i := FColFields.Count to RowCount - 1 do
      CalcRowTotal(i);
    HideTotals;
    FBuilded := True;
  except
    on E: Exception do
    begin
      ClearGrid;
      //ErrMsg(ExceptionToString(E, False, False));
    end;
  end;
  finally
    FDS.GotoBookmark(B);
    FDS.FreeBookmark(B);
    FDS.AfterScroll := AftScr;
  end;

  //if FOnBuild <> nil then FOnBuild(Self);
end;

procedure TdxPivotGrid.Assign(Source: TPersistent);
var
  Src: TdxPivotGrid;
begin
  inherited Assign(Source);
  Src := TdxPivotGrid(Source);
  Id := Src.Id;
  Colors.Assign(Src.Colors);
  CornerColor:=Src.CornerColor;
  GrandTotalColor:=Src.GrandTotalColor;
  GrandTotalFixedColor:=Src.GrandTotalFixedColor;
  GrandTotalFont := Src.GrandTotalFont;
  GrandTotalFixedFont := Src.GrandTotalFixedFont;
  GrandTotalWidth := Src.GrandTotalWidth;
  GrandTotalCaption:=Src.GrandTotalCaption;
  ShowGrandTotalX:=Src.ShowGrandTotalX;
  ShowGrandTotalY:=Src.ShowGrandTotalY;
  Font := Src.Font;
  FixedFont := Src.FixedFont;
  SelectedFont := Src.SelectedFont;
  RowFields := Src.RowFields;
  ColFields := Src.ColFields;
  DataFields := Src.DataFields;
  //Flat := Src.Flat;
  WordWrap := Src.WordWrap;
  //Options := Src.Options;
  //OptionsEx := Src.OptionsEx;
  DataDelimiter := Src.DataDelimiter;
  Indent := Src.Indent;
  RowCount := Src.RowCount;
  ColCount := Src.ColCount;
  FixedRows := Src.FixedRows;
  FixedCols := Src.FixedCols;
end;

procedure TdxPivotGrid.Clear;
begin
  FRowFields.Clear;
  FColFields.Clear;
  FDataFields.Clear;
  FId := 0;
end;

end.

