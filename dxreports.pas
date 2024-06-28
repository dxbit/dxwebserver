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

unit DXReports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dxctrls, Db, strconsts, SqlDb, typeswrapper, BGRAGraphics;

type

  { ESourceFilterError }

  ESourceFilterError = class(Exception)
  private
    FExpr: String;
    FPosition: Integer;
    FSourceNum: Integer;
  public
    constructor Create(const Msg, Expr: String; ASourceNum, APos: Integer);
    property Expr: String read FExpr;
    property SourceNum: Integer read FSourceNum;
    property Position: Integer read FPosition;
  end;

  TRpSourceKind = (skNone, skIncome, skOutcome);
  TRpFieldType = (flNone, flText, flNumber, flDate, flBool, flObject, flTime,
    flCounter, flFile, flRecId, flImage);
  TRpFieldTypes = set of TRpFieldType;
  TRpTotalFunc = (tfNone, tfSum, tfAvg, tfMax, tfMin, tfCount, tfProfit, tfDistCount,
    tfMergeAll, tfMerge);

  TRpFieldList = class;

  PRpField = ^TRpField;
  TRpField = record
    TId, FId: Integer;
    Name: String;
    Param, Visible, No, Nul, Zero: Boolean;
    AllZeros: Boolean; // Устанавливается для полей с функцией "Количество", если не указано ни одного поля
    Tp: TRpFieldType;
    Value: String;
    ValueStr: String;     // Текстовое представление параметра
    Parent, Src: PRpField;
    Func: TRpTotalFunc;
    Id: Integer;
    TextSearch: Boolean;
  end;

  PRpSource = ^TRpSource;
  TRpSource = record
    Id, TId: Integer;
    Kind: TRpSourceKind;
    Fields: TRpFieldList;
    Filter: String;
  end;

  { TRpFieldList }

  TRpFieldList = class(TList)
  private
    function GetFields(Index: Integer): PRpField;
  public
    function AddField(var F: PRpField): Integer;
    procedure Clear; override;
    function FindField(Id: Integer): PRpField;
    function FindFieldByName(const S: String): PRpField;
    property Fields[Index: Integer]: PRpField read GetFields; default;
  end;

  { TRpSourceList }

  TRpSourceList = class(TList)
  private
    function GetSources(Index: Integer): PRpSource;
  public
    function AddSource(var S: PRpSource): Integer;
    procedure Clear; override;
    property Sources[Index: Integer]: PRpSource read GetSources; default;
  end;

  PRpCalcField = ^TRpCalcField;
  TRpCalcField = record
    Id: Integer;
    Name: String;
    Expr: String;
    Tp: TRpFieldType;
    Size: Integer;
  end;

  { TRpCalcFieldList }

  TRpCalcFieldList = class(TList)
  private
    function GetFields(Index: Integer): PRpCalcField;
  public
    function AddField(var pF: PRpCalcField): Integer;
    procedure Clear; override;
    function FindField(Id: Integer): PRpCalcField;
    function FindFieldByName(const S: String): PRpCalcField;
    property Fields[Index: Integer]: PRpCalcField read GetFields; default;
  end;

  { TSQLField }

  TSQLField = class
  private
    FDisplayFormat: String;
    FFieldNameDS: String;
    FName: String;
    FTp: TRpFieldType;
  public
    procedure CopyFrom(SrcF: TSQLField);
  published
    property FieldNameDS: String read FFieldNameDS write FFieldNameDS;
    property Name: String read FName write FName;
    property Tp: TRpFieldType read FTp write FTp;
    property DisplayFormat: String read FDisplayFormat write FDisplayFormat;
  end;

  { TSQLFieldList }

  TSQLFieldList = class(TList)
  private
    function GetFields(Index: Integer): TSQLField;
  public
    function AddField: TSQLField;
    //function InsertFromDataSetField(AIndex: Integer; F: TField): TSQLField;
    function FindFieldDS(const FieldNameDS: String): TSQLField;
    function FindByName(const FieldName: String): TSQLField;
    procedure DeleteField(AIndex: Integer);
    procedure Clear; override;
    procedure CopyFrom(SourceList: TSQLFieldList);
    property Fields[Index: Integer]: TSQLField read GetFields; default;
  end;

  { TRpGridColumn }

  TRpGridColumn = class
  private
    FAlignment: TAlignment;
    FAutoAlignment: Boolean;
    FAutoLayout: Boolean;
    FCaption: String;
    FColor: TColor;
    FFieldNameDS: String;
    FFixedColor: TColor;
    FFont: TdxFont;
    FIndex: Integer;
    FIsImage: Boolean;
    FLayout: TTextLayout;
    FThumbSize: Integer;
    FTitleAlignment: TAlignment;
    FTitleFont: TdxFont;
    FTitleLayout: TTextLayout;
    FVisible: Boolean;
    FWidth: Integer;
    function GetFieldId: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function IsCalcField: Boolean;
    property Font: TdxFont read FFont write FFont;
    property Color: TColor read FColor write FColor;
    property TitleFont: TdxFont read FTitleFont write FTitleFont;
    property FixedColor: TColor read FFixedColor write FFixedColor;
    property Caption: String read FCaption write FCaption;
    property FieldNameDS: String read FFieldNameDS write FFieldNameDS;
    property FieldId: Integer read GetFieldId;
    property Width: Integer read FWidth write FWidth;
    property Index: Integer read FIndex write FIndex;
    property Visible: Boolean read FVisible write FVisible;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Layout: TTextLayout read FLayout write FLayout;
    property AutoAlignment: Boolean read FAutoAlignment write FAutoAlignment;
    property AutoLayout: Boolean read FAutoLayout write FAutoLayout;
    property TitleAlignment: TAlignment read FTitleAlignment write FTitleAlignment;
    property TitleLayout: TTextLayout read FTitleLayout write FTitleLayout;
    // Эти свойства используются только в рантайме
    property IsImage: Boolean read FIsImage write FIsImage;
    property ThumbSize: Integer read FThumbSize write FThumbSize;
  end;

  { TRpGridSortData }

  TRpGridSortData = class
  public
    Col: TRpGridColumn;
    Desc: Boolean;
  end;

  { TRpGridSortList }

  TRpGridSortList = class(TList)
  private
    function GetCols(Index: Integer): TRpGridSortData;
  public
    function AddCol(Col: TRpGridColumn; aDesc: Boolean): TRpGridSortData;
    function FindCol(Col: TRpGridColumn): TRpGridSortData;
    procedure RemoveCol(CD: TRpGridSortData);
    procedure Clear; override;
    property Cols[Index: Integer]: TRpGridSortData read GetCols; default;
  end;

  { TRpGrid }

  TRpGrid = class
  private
    FAllowChangeSort: Boolean;
    FAlternateColor: TColor;
    FCellEllipsis: Boolean;
    FColMove: Boolean;
    FColor: TColor;
    FDefaultRowHeight: Integer;
    FFixedColor: TColor;
    //FFixedHotColor: TColor;
    FFlat: Boolean;
    FFont: TdxFont;
    FGridLineColor: TColor;
    FGridLineStyle: TPenStyle;
    FHorzLines: Boolean;
    FInactiveSelectedColor: TColor;
    FInactiveSelectedTextColor: TColor;
    FIndicator: Boolean;
    FRowHighlight: Boolean;
    FRowSelect: Boolean;
    FSelectedColor: TColor;
    FSelectedTextColor: TColor;
    FShowHints: Boolean;
    FShowRowDeleteButton: Boolean;
    FSortCols: TRpGridSortList;
    FThumbTracking: Boolean;
    FTitleFont: TdxFont;
    FVertLines: Boolean;
    FColumns: TList;
    FWordWrap: Boolean;
    function GetColumns(Index: Integer): TRpGridColumn;
  public
    constructor Create;
    destructor Destroy; override;
    function ColumnCount: Integer;
    function AddColumn: TRpGridColumn;
    function FindColumnByFieldName(const FieldName: String): TRpGridColumn;
    function FindColumnByTitle(const S: String): TRpGridColumn;
    function FindColumnIndex(Col: TRpGridColumn): Integer;
    //procedure DeleteColumn(Col: TRpGridColumn);
    procedure SortColumns;//(L: TList);
    procedure ClearColumns;
    function GetVisibleColumnCount: Integer;
    property Color: TColor read FColor write FColor;
    property AlternateColor: TColor read FAlternateColor write FAlternateColor;
    property SelectedColor: TColor read FSelectedColor write FSelectedColor;
    property SelectedTextColor: TColor read FSelectedTextColor write
      FSelectedTextColor;
    property InactiveSelectedColor: TColor read FInactiveSelectedColor write
    	FInactiveSelectedColor;
    property InactiveSelectedTextColor: TColor read FInactiveSelectedTextColor write
      FInactiveSelectedTextColor;
    property FixedColor: TColor read FFixedColor write FFixedColor;
    //property FixedHotColor: TColor read FFixedHotColor write FFixedHotColor;
    property GridLineColor: TColor read FGridLineColor write FGridLineColor;
    property GridLineStyle: TPenStyle read FGridLineStyle write FGridLineStyle;
    property Font: TdxFont read FFont write FFont;
    property TitleFont: TdxFont read FTitleFont write FTitleFont;
    property DefaultRowHeight: Integer read FDefaultRowHeight write
      FDefaultRowHeight;
    property VertLines: Boolean read FVertLines write FVertLines;
    property HorzLines: Boolean read FHorzLines write FHorzLines;
    property Flat: Boolean read FFlat write FFlat;
    property Columns[Index: Integer]: TRpGridColumn read GetColumns;
    property SortCols: TRpGridSortList read FSortCols;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property RowSelect: Boolean read FRowSelect write FRowSelect;
    property CellEllipsis: Boolean read FCellEllipsis write FCellEllipsis;
    property ShowHints: Boolean read FShowHints write FShowHints;
    property ThumbTracking: Boolean read FThumbTracking write FThumbTracking;
    property AllowChangeSort: Boolean read FAllowChangeSort write FAllowChangeSort;
    property ColMove: Boolean read FColMove write FColMove;
    property RowHighlight: Boolean read FRowHighlight write FRowHighlight;
    property Indicator: Boolean read FIndicator write FIndicator;
    property ShowRowDeleteButton: Boolean read FShowRowDeleteButton write FShowRowDeleteButton;
  end;


  TRpDateDetail = (ddDay, ddWeek, ddMonth, ddQuart, ddHalfYear, ddYear);
  TReportKind = (rkReport, rkQuery);

  { TRpTotalData }

  TRpTotalData = class
  public
    Caption: String;
    FieldNameDS: String;
    Func: TRpTotalFunc;
    Value: String;      // для печати в шаблонах
  end;

  { TRpTotalList }

  TRpTotalList = class(TList)
  private
    function GetTotals(Index: Integer): TRpTotalData;
  public
    function AddTotal: TRpTotalData;
    function FindTotal(const FieldName: String): TRpTotalData;
    procedure RemoveTotal(T: TRpTotalData);
    procedure Clear; override;
    property Totals[Index: Integer]: TRpTotalData read GetTotals; default;
  end;

  { TReportData }

  TReportData = class
  private
    FCalcFields: TRpCalcFieldList;
    FColoring: TColoringList;
    FDateDetail: TRpDateDetail;
    FDateField: Integer;
    FFilter: String;
    FFirstRecordCount: Integer;
    FGrid: TRpGrid;
    FHelpText: String;
    FId: Integer;
    FName: String;
    FKind: TReportKind;
    FPrintFields: TStringList;
    FSearchText: String;
    FSession: TObject;
    //FPivotGrid: TObject;
    FSortOrder: String;
    FSources: TRpSourceList;
    FSQL: String;
    FSqlFields: TSQLFieldList;
    FSqlMode: Boolean;
    FTemplates: TStringList;
    FTotals: TRpTotalList;
    FVersion: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(St: TStream);
    procedure SaveToStream(St: TStream);
    procedure Clear;
    function FindField(aId: Integer): PRpField;
    function FindFieldIndex(aId: Integer): Integer;
    function FindFieldByName(const aName: String): PRpField;
    procedure FindFieldByColumn(Col: TRpGridColumn; out pF: PRpField; out pCF: PRpCalcField);

    function TryGetRpField(AIndex: Integer): PRpField;
    function TryGetCalcField(AIndex: Integer): PRpCalcField;
    function TryGetSQLField(AIndex: Integer): TSQLField;
    function GetFieldCount: Integer;
    function GetRpSQLFieldCount: Integer;
    function IsEmpty: Boolean;
    procedure CheckFieldIndex(AIndex: Integer);
    function GetFieldNameDS(AIndex: Integer): String;
    function GetFieldName(AIndex: Integer): String;
    function GetFieldType(AIndex: Integer): TRpFieldType;
    function GetFieldVisible(AIndex: Integer): Boolean;
    function GetFieldParam(AIndex: Integer): Boolean;
    function GetDisplayFormat(AIndex: Integer): String;
    function IsCalcField(AIndex: Integer): Boolean;
    function IndexOfName(AFieldName: String): Integer;
    function IndexOfNameDS(AFieldNameDS: String): Integer;
    function QueryExistsInExpr(AQueryName: String): Boolean;
    function FieldExistsInExpr(AFieldName: String): Boolean;
    function GetEditFormId: Integer;
    function GetSourceFilter: String;
    function IsSimple: Boolean;
    function HasParentIdField: Boolean;
    function ParamExists: Boolean;

    property Id: Integer read FId write FId;
    property Name: String read FName write FName;
    property Sources: TRpSourceList read FSources;
    property DateField: Integer read FDateField write FDateField;
    property DateDetail: TRpDateDetail read FDateDetail write FDateDetail;
    property SortOrder: String read FSortOrder write FSortOrder;
    property Grid: TRpGrid read FGrid;
    property Kind: TReportKind read FKind write FKind;
    property Filter: String read FFilter write FFilter;
    property CalcFields: TRpCalcFieldList read FCalcFields;
    property Totals: TRpTotalList read FTotals;
    property Coloring: TColoringList read FColoring;
    property HelpText: String read FHelpText write FHelpText;
    property Version: Integer read FVersion write FVersion;
    property Templates: TStringList read FTemplates;
    property PrintFields: TStringList read FPrintFields;
    property FirstRecordCount: Integer read FFirstRecordCount write FFirstRecordCount;
    property SqlMode: Boolean read FSqlMode write FSqlMode;
    property SqlFields: TSQLFieldList read FSqlFields;
    property SQL: String read FSQL write FSQL;

    property SearchText: String read FSearchText write FSearchText;

    property Session: TObject read FSession write FSession;
  end;

function NewRpField: PRpField;
//function GetTopField(Fl: PRpField): PRpField;
function GetLowField(Fl: PRpField): PRpField;
function GetTypeByComponent(C: TdxField): TRpFieldType;
//function RpFieldTypeToStr(Tp: TRpFieldType): String;
//procedure FillFields(aFm, aTbl: TdxForm; L: TStrings; Types: TRpFieldTypes);
//function GetFullFieldName(F: TRpField): String;
//procedure SetQueryDisplayFormat(RD: TReportData; DS: TDataSet);
//function SourceKindToStr(sk: TRpSourceKind): String;
//function TotalFuncToStr(tf: TRpTotalFunc): String;
//function IsSimpleReport(RD: TReportData): Boolean;
//function SortFieldsToStr(RD: TReportData): String;
//procedure AddCalcFields(RD: TReportData; DS: TDataSet);
//function GetRealRpFieldType(RD: TReportData; Fl: PRpField): TRpFieldType;

implementation

uses
  apputils, SAX, LazUtf8, formmanager, sqlgen, expressions, strutils,
  Variants, DateUtils, reportmanager, saxbasereader, dxtypes;

type

  { TRpReader }

  TRpReader = class(TSaxBaseReader)
  private
    FSrc: PRpSource;
    FParent: PRpField;
    FColumn: TRpGridColumn;
    function GetColor(Atts: TSAXAttributes; const aName: String; DefaultColor: TColor): TColor;
    function GetPenStyle(Atts: TSAXAttributes; const aName: String): TPenStyle;
    procedure ReadSortCols;
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
    procedure DoEndElement(const NamespaceURI, LocalName, QName: SAXString); override;
  public
    RD: TReportData;
  end;

  { TSQLReportTotalsFilter }

  {TSQLReportTotalsFilter = class(TSQLFilterParser)
  private
    FRD: TReportData;
  protected
    function FieldNameParse(const aFieldName: String): String; override;
    function CheckValue(var Value: String): Boolean; override;
  public
    property RD: TReportData read FRD write FRD;
  end; }


function NewRpField: PRpField;
begin
  New(Result);
  FillChar(Result^, SizeOf(TRpField), 0);
end;

{function GetTopField(Fl: PRpField): PRpField;
begin
  if Fl^.Parent <> nil then Result := GetTopField(Fl^.Parent)
  else Result := Fl;
end; }

function GetLowField(Fl: PRpField): PRpField;
begin
  if Fl^.Src <> nil then Result := GetLowField(Fl^.Src)
  else Result := Fl;
end;

function GetTypeByComponent(C: TdxField): TRpFieldType;
begin
  Result := flNone;
  if (C is TdxEdit) or (C is TdxComboBox) or (C is TdxMemo) then
    Result := flText
  else if C is TdxCalcEdit then
    Result := flNumber
  else if C is TdxDateEdit then
    Result := flDate
  else if C is TdxCheckBox then
    Result := flBool
  else if C is TdxLookupComboBox then
    Result := flObject
  else if C is TdxTimeEdit then
    Result := flTime
  else if C is TdxCounter then
    Result := flCounter
  else if C is TdxFile then
    Result := flFile
  else if C is TdxDBImage then
    Result := flImage
  else if C is TdxRecordId then
    Result := flRecId
end;

{function RpFieldTypeToStr(Tp: TRpFieldType): String;
const
  TpS: array [TRpFieldType] of String = ('', rsText, rsNumber, rsDate,
  	rsCheckBox, rsObject, rsTime, rsCounter, rsFile);
begin
  Result := TpS[Tp];
end;     }

{function SourceKindToStr(sk: TRpSourceKind): String;
begin
  Result := '';
  case sk of
    skIncome: Result := rsIncoming;
    skOutcome: Result := rsOutcoming;
  end;
end;      }

{function TotalFuncToStr(tf: TRpTotalFunc): String;
begin
  Result := '';
  case tf of
    tfSum: Result := rsSum;
    tfAvg: Result := rsAverage;
    tfMax: Result := rsMaximum;
    tfMin: Result := rsMinimum;
    tfCount: Result := rsCount;
    tfProfit: Result := rsBalance;
    tfDistCount: Result := rsDistinctCount;
    tfMergeAll: Result := rsMergeAll;
    tfMerge: Result := rsMerge;
  end;
end; }

{function IsSimpleReport(RD: TReportData): Boolean;
var
  Sr: TRpSource;
  i: Integer;
  Fl: TRpField;
begin
  Result := False;
  if RD.Sources.Count = 1 then
  begin
    Sr := RD.Sources[0]^;
    for i := 0 to Sr.Fields.Count - 1 do
    begin
      Fl := Sr.Fields[i]^;
      if Fl.Func <> tfNone then Exit;
    end;
    Result := RD.DateField < 0;
  end;
end;  }

{function GetRealRpFieldType(RD: TReportData; Fl: PRpField): TRpFieldType;
begin
  Result := GetLowField(Fl)^.Tp;
  if RD.Sources.Count = 0 then Exit;
  if (RD.DateField >= 0) and (RD.Sources[0]^.Fields[RD.DateField]^.Id = Fl^.Id) then
  begin
    if RD.DateDetail = ddYear then Result := flNumber
    else if RD.DateDetail <> ddDay then Result := flText;
  end
  else if Fl^.Func <> tfNone then
  begin
    if Fl^.Func in [tfCount, tfDistCount] then Result := flNumber
    else if Fl^.Func in [tfMerge, tfMergeAll] then Result := flText;
  end
end;  }

{ ESourceFilterError }

constructor ESourceFilterError.Create(const Msg, Expr: String; ASourceNum,
  APos: Integer);
begin
  inherited Create(Msg);
  FExpr := Expr;
  FPosition := APos;
  FSourceNum := ASourceNum;
end;

{ TRpCalcFieldList }

function TRpCalcFieldList.GetFields(Index: Integer): PRpCalcField;
begin
  Result := PRpCalcField(Items[Index]);
end;

function TRpCalcFieldList.AddField(var pF: PRpCalcField): Integer;
begin
  New(pF);
  pF^.Id:=0;
  pF^.Name := '';
  pF^.Expr := '';
  pF^.Tp := flText;
  pF^.Size := 200;
  Result := Add(pF);
end;

procedure TRpCalcFieldList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Dispose(PRpCalcField(Items[i]));
  inherited Clear;
end;

function TRpCalcFieldList.FindField(Id: Integer): PRpCalcField;
var
  i: Integer;
  pF: PRpCalcField;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    pF := Fields[i];
    if pF^.Id = Id then Exit(pF);
  end;
end;

function TRpCalcFieldList.FindFieldByName(const S: String): PRpCalcField;
var
  i: Integer;
  pF: PRpCalcField;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    pF := Fields[i];
    if MyUtf8CompareText(pF^.Name, S) = 0 then Exit(pF);
  end;
end;

{ TSQLField }

procedure TSQLField.CopyFrom(SrcF: TSQLField);
begin
  FName := SrcF.Name;
  FFieldNameDS := SrcF.FieldNameDS;
  FTp := SrcF.Tp;
  FDisplayFormat := SrcF.DisplayFormat;
end;

{ TSQLFieldList }

function TSQLFieldList.GetFields(Index: Integer): TSQLField;
begin
  Result := TSQLField(Items[Index]);
end;

function TSQLFieldList.AddField: TSQLField;
begin
  Result := TSQLField.Create;
  Add(Result);
end;

function TSQLFieldList.FindFieldDS(const FieldNameDS: String): TSQLField;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(Fields[i].FieldNameDS, FieldNameDS) = 0 then
      Exit(Fields[i]);
end;

function TSQLFieldList.FindByName(const FieldName: String): TSQLField;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if MyUtf8CompareText(Fields[i].Name, FieldName) = 0 then
      Exit(Fields[i]);
end;

procedure TSQLFieldList.DeleteField(AIndex: Integer);
begin
  Fields[AIndex].Free;
  Delete(AIndex);
end;

procedure TSQLFieldList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Fields[i].Free;
  inherited Clear;
end;

procedure TSQLFieldList.CopyFrom(SourceList: TSQLFieldList);
var
  i: Integer;
  F: TSQLField;
begin
  Clear;
  for i := 0 to SourceList.Count - 1 do
  begin
    F := AddField;
    F.CopyFrom(SourceList[i]);
  end;
end;

{ TRpTotalList }

function TRpTotalList.GetTotals(Index: Integer): TRpTotalData;
begin
  Result := TRpTotalData(Items[Index]);
end;

function TRpTotalList.AddTotal: TRpTotalData;
begin
  Result := TRpTotalData.Create;
  Add(Result);
end;

function TRpTotalList.FindTotal(const FieldName: String): TRpTotalData;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(FieldName, Totals[i].FieldNameDS) = 0 then Exit(Totals[i]);
end;

procedure TRpTotalList.RemoveTotal(T: TRpTotalData);
begin
  Remove(T);
  T.Free;
end;

procedure TRpTotalList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Totals[i].Free;
  inherited Clear;
end;

{ TRpGridSortList }

function TRpGridSortList.GetCols(Index: Integer): TRpGridSortData;
begin
  Result := TRpGridSortData(Items[Index]);
end;

function TRpGridSortList.AddCol(Col: TRpGridColumn; aDesc: Boolean
  ): TRpGridSortData;
begin
  Result := TRpGridSortData.Create;
  Result.Col := Col;
  Result.Desc := aDesc;
  Add(Result);
end;

function TRpGridSortList.FindCol(Col: TRpGridColumn): TRpGridSortData;
var
  i: Integer;
  CD: TRpGridSortData;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    CD := Cols[i];
    if CD.Col = Col then Exit(CD);
  end;
end;

procedure TRpGridSortList.RemoveCol(CD: TRpGridSortData);
begin
  Remove(CD);
  CD.Free;
end;

procedure TRpGridSortList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Cols[i].Free;
  inherited Clear;
end;

{ TSQLReportTotalsFilter }

{function TSQLReportTotalsFilter.FieldNameParse(const aFieldName: String
  ): String;
var
  pFl: PRpField;
  idx: Integer;
begin
  Result := '';
  pFl := RD.Sources[0]^.Fields.FindFieldByName(aFieldName);
  if pFl = nil then Exit;
  idx := RD.Sources[0]^.Fields.IndexOf(pFl);
  if pFl^.Func <> tfNone then
    Result := GetFuncSql(pFl^, idx);
end;

function TSQLReportTotalsFilter.CheckValue(var Value: String): Boolean;
var
  FS: TFormatSettings;
  E: Extended;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator:='.';
  Result:=TryStrToFloat(Value, E, FS);
  if Result then Value := FloatToStr(E, FS);
end; }

{ TRpGridColumn }

function TRpGridColumn.GetFieldId: Integer;
begin
  if not IsCalcField then
  	Result := StrToInt(Copy(FFieldNameDS, 2, 100))
  else
		Result := StrToInt(Copy(FFieldNameDS, 3, 100))
end;

constructor TRpGridColumn.Create;
begin
  FFont := TdxFont.Create;
  FTitleFont := TdxFont.Create;
  FVisible := True;
  FAutoAlignment := True;
  FAutoLayout := True;
end;

destructor TRpGridColumn.Destroy;
begin
  FTitleFont.Free;
  FFont.Free;
  inherited Destroy;
end;

function TRpGridColumn.IsCalcField: Boolean;
begin
  Result := LowerCase(Copy(FFieldNameDS, 1, 2)) = 'cf';
end;

{ TRpGrid }

function TRpGrid.GetColumns(Index: Integer): TRpGridColumn;
begin
  Result := TRpGridColumn(FColumns[Index]);
end;

constructor TRpGrid.Create;
begin
  FFont := TdxFont.Create;
  FFont.Name:='Verdana';
  FFont.Size := 10;
  FTitleFont := TdxFont.Create;
  FTitleFont.Name:='Verdana';
  FTitleFont.Size := 10;
  FColumns := TList.Create;
  FSortCols := TRpGridSortList.Create;
  FGridLineColor:=clSilver;
  FGridLineStyle:=psSolid;
  FFixedColor := clSilver;
  //FFixedHotColor:=cl3DLight;
  FSelectedColor := clSkyBlue;
  FSelectedTextColor:=clBlack;
  FInactiveSelectedColor:=clSilver;
  FInactiveSelectedTextColor:=clBlack;
  FColor := clWhite;
  FAlternateColor:=FColor;
  FShowHints:=True;
  FThumbTracking:=True;
  FAllowChangeSort:=True;
end;

destructor TRpGrid.Destroy;
begin
  FSortCols.Free;
  ClearList(FColumns);
  FColumns.Free;
  FFont.Free;
  FTitleFont.Free;
  inherited Destroy;
end;

function TRpGrid.ColumnCount: Integer;
begin
  Result := FColumns.Count;
end;

function TRpGrid.AddColumn: TRpGridColumn;
begin
  Result := TRpGridColumn.Create;
  Result.Color := clWhite;
  Result.FixedColor := clSilver;
  FColumns.Add(Result);
end;

function TRpGrid.FindColumnByFieldName(const FieldName: String): TRpGridColumn;
var
  i: Integer;
  C: TRpGridColumn;
begin
  Result := nil;
  for i := 0 to ColumnCount - 1 do
  begin
    C := Columns[i];
    if CompareText(C.FieldNameDS, FieldName) = 0 then Exit(C);
  end;
end;

function TRpGrid.FindColumnByTitle(const S: String): TRpGridColumn;
var
  i: Integer;
  C: TRpGridColumn;
begin
  Result := nil;
  for i := 0 to ColumnCount - 1 do
  begin
    C := Columns[i];
    if MyUtf8CompareText(C.Caption, S) = 0 then Exit(C);
  end;
end;

function TRpGrid.FindColumnIndex(Col: TRpGridColumn): Integer;
begin
  Result := FColumns.IndexOf(Col);
end;

{procedure TRpGrid.DeleteColumn(Col: TRpGridColumn);
var
  SC: TRpGridSortData;
  C: TRpGridColumn;
  i: Integer;
begin
  SC := FSortCols.FindCol(Col);
  for i := 0 to ColumnCount - 1 do
  begin
    C := Columns[i];
    if C.Index > Col.Index then C.Index := C.Index - 1;
  end;
  if SC <> nil then
    FSortCols.RemoveCol(SC);
  FColumns.Remove(Col);
  Col.Free;
end;   }

function RpGridSortCompareFunc(Item1, Item2: Pointer): Integer;
begin
  Result := TRpGridColumn(Item1).Index - TRpGridColumn(Item2).Index;
end;

procedure TRpGrid.SortColumns;
begin
  FColumns.Sort(@RpGridSortCompareFunc);
end;

{procedure TRpGrid.SortColumns(L: TList);
var
  i: Integer;

  procedure AddToList(C: TRpGridColumn);
  var
    j: Integer;
  begin
    for j := 0 to L.Count - 1 do
      if C.Index < TRpGridColumn(L[j]).Index then
      begin
        L.Insert(j, C);
        Exit;
      end;
    L.Add(C);
  end;

begin

  for i := 0 to ColumnCount - 1 do
    AddToList(Columns[i]);
end;  }

procedure TRpGrid.ClearColumns;
begin
  ClearList(FColumns);
end;

function TRpGrid.GetVisibleColumnCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to ColumnCount - 1 do
    if Columns[i].Visible and (Columns[i].Width > 0) then Inc(Result);
end;

{ TRpReader }

function TRpReader.GetColor(Atts: TSAXAttributes; const aName: String;
  DefaultColor: TColor): TColor;
var
  S: String;
begin
  //Result := clDefault;
  S := GetStr(Atts, aName);
  if S <> '' then Result := StringToColor(S)
  else Result := DefaultColor;
end;

function TRpReader.GetPenStyle(Atts: TSAXAttributes; const aName: String
  ): TPenStyle;
var
  S: String;
begin
  Result := psSolid;
  S := GetStr(Atts, aName);
  if S = 'psDash' then Result := psDash
  else if S = 'psDot' then Result := psDot;
end;

procedure TRpReader.ReadSortCols;
var
  SL: TStringList;
  i{, n}: Integer;
  S: String;
  Desc: Boolean;
  Col: TRpGridColumn;
begin
  SL := TStringList.Create;
  SplitStr(RD.SortOrder, ';', SL);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    if S = '' then Continue;
    Desc := False;
    if S[1] = '*' then
    begin
      Desc := True;
      Delete(S, 1, 1);
    end;
    if S[1] in ['0'..'9'] then S := 'f' + S;
    Col := RD.Grid.FindColumnByFieldName(S);
    //n := StrToInt(S);
    //Col := RD.Grid.FindColumnByFieldName('f' + IntToStr(n));
    if Col <> nil then
      RD.Grid.SortCols.AddCol(Col, Desc);
  end;
  SL.Free;
end;

procedure TRpReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  pS: PRpSource;
  pF: PRpField;
  G: TRpGrid;
  C: TRpGridColumn;
  Fnt: TdxFont;
  T: TRpTotalData;
  CD: TColoringData;
  pCalcF: PRpCalcField;
  SqlF: TSQLField;
begin
  inherited DoStartElement(NamespaceURI, LocalName, QName, Atts);
  if LocalName = 'source' then
  begin
    RD.Sources.AddSource(pS);
    pS^.Kind:=TRpSourceKind(GetInt(Atts, 'kind'));
    pS^.Id:=GetInt(Atts, 'id');
    pS^.TId := GetInt(Atts, 'tid');
    pS^.Filter := XmlToStr(GetStr(Atts, 'filter'));
    FSrc := pS;
  end
  else if LocalName = 'field' then
  begin
    pS := FSrc;
    if FParent = nil then
    begin
      pS^.Fields.AddField(pF);
      FParent := pF;
    end
    else
    begin
      pF := NewRpField;
      FParent^.Src:=pF;
      pF^.Parent := FParent;
      FParent := pF;
    end;
    pF^.Name := GetStr(Atts, 'name');
    pF^.Tp:= TRpFieldType(GetInt(Atts, 'type'));
    pF^.TId := GetInt(Atts, 'tid');
    pF^.FId := GetInt(Atts, 'fid');
    pF^.Param:=GetBool(Atts, 'param');
    pF^.Visible := GetBool(Atts, 'visible');
    pF^.No:=GetBool(Atts, 'not');
    pF^.Nul:=GetBool(Atts, 'null');
    pF^.Zero:=GetBool(Atts, 'zero');
    pF^.Value := GetStr(Atts, 'value');
    pF^.Func := TRpTotalFunc(GetInt(Atts, 'func'));
    pF^.Id := GetInt(Atts, 'id');
  end
  else if LocalName = 'sqlfield' then
  begin
    SqlF := RD.SqlFields.AddField;
    SqlF.Name := XmlToStr(GetStr(Atts, 'name'));
    SqlF.FieldNameDS := XmlToStr(GetStr(Atts, 'field'));
    SqlF.Tp := TRpFieldType(GetInt(Atts, 'tp'));
    SqlF.DisplayFormat := XmlToStr(GetStr(Atts, 'format'));
  end
  else if LocalName = 'reportdata' then
  begin
    RD.Id:= GetInt(Atts, 'id');
    RD.Name:=GetStr(Atts, 'name');
    RD.SortOrder:=GetStr(Atts, 'sortorder');
    RD.Kind:=TReportKind(GetInt(Atts, 'kind'));
    RD.DateField:=GetInt(Atts, 'datefield');
    RD.DateDetail:=TRpDateDetail(GetInt(Atts, 'datedetail'));
    RD.FirstRecordCount:=GetInt(Atts, 'first');
    RD.Filter := XmlToStr(GetStr(Atts, 'filter'));
    RD.HelpText:=XmlToHtml(GetStr(Atts, 'helptext'));
    RD.SQL := XmlToStr(GetStr(Atts, 'sql'));
    RD.SqlMode := GetBool(Atts, 'sqlmode');
    RD.Version := GetInt(Atts, 'version');
  end
  else if LocalName = 'grid' then
  begin
    G := RD.Grid;
    G.Color:=GetColor(Atts, 'color', clWhite);
    G.AlternateColor:=GetColor(Atts, 'alternatecolor', clWhite);
    G.SelectedColor:=GetColor(Atts, 'selectedcolor', clSkyBlue);
    G.SelectedTextColor:=GetColor(Atts, 'selectedtextcolor', clBlack);
    G.InactiveSelectedColor:=GetColor(Atts, 'inactiveselectedcolor', clSilver);
    G.InactiveSelectedTextColor:=GetColor(Atts, 'inactiveselectedtextcolor', clBlack);
    G.FixedColor:=GetColor(Atts, 'fixedcolor', clSilver);
    //G.FixedHotColor:=GetColor(Atts, 'fixedhotcolor', cl3DLight);
    G.GridLineColor:=GetColor(Atts, 'gridlinecolor', clSilver);
    G.GridLineStyle:=GetPenStyle(Atts, 'gridlinestyle');
    G.DefaultRowHeight:=GetInt(Atts, 'defaultrowheight');
    G.VertLines:=GetBool(Atts, 'vertlines');
    G.HorzLines:=GetBool(Atts, 'horzlines');
    G.Flat:=GetBool(Atts, 'flat');
    G.WordWrap:=GetBool(Atts, 'wordwrap');
    G.RowSelect:=GetBool(Atts, 'rowselect');
    G.RowHighlight:=GetBool(Atts, 'rowhighlight');
    G.CellEllipsis:=GetBool(Atts, 'cellellipsis');
    G.ShowHints:=GetBool(Atts, 'showhints');
    G.ThumbTracking:=GetBool(Atts, 'thumbtracking');
    G.ColMove:=GetBool(Atts, 'colmove');
    G.AllowChangeSort:=GetBool(Atts, 'allowchangesort');
    G.Indicator:=GetBool(Atts, 'indicator');
    G.ShowRowDeleteButton:=GetBool(Atts, 'rowdelbn');
  end
  else if LocalName = 'column' then
  begin
    C := RD.Grid.AddColumn;
    C.Color := GetColor(Atts, 'color', clWhite);
    C.FixedColor:=GetColor(Atts, 'fixedcolor', clSilver);
    C.Width:=GetInt(Atts, 'width');
    C.FieldNameDS:=GetStr(Atts, 'fieldname');
    C.Caption := GetStr(Atts, 'caption');
    C.Index := GetInt(Atts, 'index');
    if GetStr(Atts, 'visible') <> '' then
      C.Visible:=GetBool(Atts, 'visible');
    if GetStr(Atts, 'autoalignment') <> '' then
    begin
      C.AutoAlignment:=GetBool(Atts, 'autoalignment');
      C.Alignment:=TAlignment(GetInt(Atts, 'alignment'));
    end;
    if GetStr(Atts, 'autolayout') <> '' then
    begin
      C.AutoLayout:=GetBool(Atts, 'autolayout');
      C.Layout:=TTextLayout(GetInt(Atts, 'layout'));
    end;
    C.TitleAlignment := TAlignment(GetInt(Atts, 'titlealignment'));
    C.TitleLayout := TTextLayout(GetInt(Atts, 'textlayout'));
    FColumn := C;
  end
  else if (LocalName = 'font') or (LocalName = 'titlefont') then
  begin
    if FColumn <> nil then
    begin
      if LocalName = 'font' then Fnt := FColumn.Font
      else if LocalName = 'titlefont' then Fnt := FColumn.TitleFont;
    end
    else
    begin
      if LocalName = 'font' then Fnt := RD.Grid.Font
      else if LocalName = 'titlefont' then Fnt := RD.Grid.TitleFont;
    end;
    Fnt.Name := GetStr(Atts, 'name');
    Fnt.Size:=Abs(GetInt(Atts, 'height'));
    // Со временем убрать
    //if Fnt.Height = 0 then
    //  Fnt.Size:=GetInt(Atts, 'size');
    Fnt.Color := GetColor(Atts, 'color', clBlack);
    if GetBool(Atts, 'bold') then Fnt.Style := Fnt.Style + [fsBold];
    if GetBool(Atts, 'italic') then Fnt.Style := Fnt.Style + [fsItalic];
    if GetBool(Atts, 'underline') then Fnt.Style := Fnt.Style + [fsUnderline];
    if GetBool(Atts, 'strikeout') then Fnt.Style := Fnt.Style + [fsStrikeOut];
  end
  else if LocalName = 'calcfield' then
  begin
    RD.CalcFields.AddField(pCalcF);
    pCalcF^.Name := GetStr(Atts, 'name');
    pCalcF^.Expr := XmlToStr(GetStr(Atts, 'expression'));
    pCalcF^.Id := GetInt(Atts, 'id');
    pCalcF^.Size := GetInt(Atts, 'size');
    pCalcF^.Tp := TRpFieldType(GetInt(Atts, 'tp'));
    if pCalcF^.Tp = flNone then
    begin
      pCalcF^.Tp := flText;
      pCalcF^.Size := 200;
    end;
  end
  else if LocalName = 'total' then
  begin
    T := RD.Totals.AddTotal;
    T.Caption := GetStr(Atts, 'caption');
    T.FieldNameDS := GetStr(Atts, 'field');
    T.Func:=TRpTotalFunc(GetInt(Atts, 'func'));
  end
  else if LocalName = 'coloringdata' then
  begin
    CD := RD.Coloring.AddColoring;
    CD.Color:=StringToColor(GetStr(Atts, 'color'));
    CD.FieldName:=XmlToStr(GetStr(Atts, 'fieldname'));
    CD.Expr:=XmlToStr(GetStr(Atts, 'expression'));
  end
  else if LocalName = 'template' then
    RD.Templates.Add(XmlToStr(GetStr(Atts, 'filename')))
  else if LocalName = 'printfield' then
    RD.PrintFields.Add(XmlToStr(GetStr(Atts, 'name')) + '=' +
      XmlToStr(GetStr(Atts, 'value')));
end;

procedure TRpReader.DoEndElement(const NamespaceURI, LocalName, QName: SAXString
  );
begin
  inherited DoEndElement(NamespaceURI, LocalName, QName);
  if LocalName = 'field' then
  begin
    if FParent <> nil then
      FParent := FParent^.Parent;
  end
  else if LocalName = 'column' then
    FColumn := nil
  else if LocalName = 'grid' then
    ReadSortCols;
end;

{ TRpSourceList }

function TRpSourceList.GetSources(Index: Integer): PRpSource;
begin
  Result := PRpSource(Items[Index]);
end;

function TRpSourceList.AddSource(var S: PRpSource): Integer;
begin
  New(S);
  FillChar(S^, SizeOf(TRpSource), 0);
  S^.Fields := TRpFieldList.Create;
  S^.Filter := '';
  Result := Add(S);
end;

procedure TRpSourceList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Sources[i]^.Fields.Free;
    Dispose(Sources[i]);
  end;
  inherited Clear;
end;

{ TRpFieldList }

function TRpFieldList.GetFields(Index: Integer): PRpField;
begin
  Result := PRpField(Items[Index]);
end;

function TRpFieldList.AddField(var F: PRpField): Integer;
begin
  F := NewRpField;
  Result := Add(F);
end;

procedure DisposeField(F: PRpField);
begin
  if F^.Src <> nil then
  begin
    DisposeField(F^.Src);
    Dispose(F^.Src);
  end;
end;

procedure TRpFieldList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    DisposeField(Fields[i]);
    Dispose(Fields[i]);
  end;
  inherited Clear;
end;

function TRpFieldList.FindField(Id: Integer): PRpField;
var
  i: Integer;
  pF: PRpField;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    pF := Fields[i];
    if pF^.Id = Id then Exit(pF);
  end;
end;

function TRpFieldList.FindFieldByName(const S: String): PRpField;
var
  i: Integer;
  pF: PRpField;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    pF := Fields[i];
    if MyUtf8CompareText(S, pF^.Name) = 0 then Exit(pF);
  end;
end;

{ TReportData }

constructor TReportData.Create;
begin
  FDateField := -1;
  FSources := TRpSourceList.Create;
  FGrid := TRpGrid.Create;
  FGrid.Color := clWhite;
  FGrid.AlternateColor := clWhite;
  FGrid.SelectedColor := clSkyBlue;
  FGrid.FixedColor := clBlack;
  FGrid.GridLineColor := clGray;
  FGrid.DefaultRowHeight:={GetTextHeight('Fj') + 5;//} 20;
  FGrid.Flat:=False;
  FGrid.GridLineStyle:=psSolid;
  FGrid.VertLines:=True;
  FGrid.HorzLines:=True;
  FCalcFields := TRpCalcFieldList.Create;
  FTotals := TRpTotalList.Create;
  FColoring := TColoringList.Create;
  FTemplates := TStringList.Create;
  FPrintFields := TStringList.Create;
  FSQLFields := TSQLFieldList.Create;
end;

destructor TReportData.Destroy;
begin
  FSQLFields.Free;
  FPrintFields.Free;
  FTemplates.Free;
  FColoring.Free;
  FTotals.Free;
  FCalcFields.Free;
  FGrid.Free;
  FSources.Free;
  inherited Destroy;
end;

procedure TReportData.LoadFromStream(St: TStream);
begin
  with TRpReader.Create do
  try
    RD := Self;
    ParseStream(St);
  finally
    Free;
  end;
end;

procedure TReportData.SaveToStream(St: TStream);

  procedure WrStr(const S: String);
  begin
    St.Write(Pointer(S)^, Length(S));
  end;

  procedure WrFld(F: TRpField);
  begin
    WrStr('<field name="' + F.Name + '" type="' + IntToStr(Ord(F.Tp)) +
      '" tid="' + IntToStr(F.TId) + '" fid="' + IntToStr(F.FId) + '" param="' + Bool2Str(F.Param) +
      '" visible="' + Bool2Str(F.Visible) + '" not="' + Bool2Str(F.No) +
      '" null="' + Bool2Str(F.Nul) + '" zero="' + Bool2Str(F.Zero) +
      '" value="' + F.Value + '" func="' + IntToStr(Ord(F.Func)) +
      '" id="' + IntToStr(F.Id) + '">');
    if F.Src <> nil then
      WrFld(F.Src^);
    WrStr('</field>');
  end;

  procedure WrSrc(S: TRpSource);
  var
    i: Integer;
  begin
    WrStr('<source kind="' + IntToStr(Ord(S.Kind)) + '" id="' + IntToStr(S.Id) +
      '" tid="' + IntToStr(S.TId) + '" filter="' + StrToXml(S.Filter) + '">');
    WrStr('<fields>');
    for i := 0 to S.Fields.Count - 1 do
      WrFld(S.Fields[i]^);
    WrStr('</fields>');
    WrStr('</source>');
  end;

  function PenStyle2Str(PS: TPenStyle): String;
  const
    PSStr: array [TPenStyle] of String = ('psSolid', 'psDash', 'psDot',
      'psDashDot', 'psDashDotDot', 'psinsideFrame', 'psPattern', 'psClear');
  begin
    Result := PSStr[PS];
  end;

  procedure WrFont(F: TdxFont; const TagName: String);
  begin
    WrStr('<' + TagName + ' name="' + F.Name +
      '" height="' + IntToStr(F.Size{Height}) +
      '" color="' + ColorToString(F.Color) + '" bold="' + Bool2Str(fsBold in F.Style) +
      '" italic="' + Bool2Str(fsItalic in F.Style) + '" underline="' +
      Bool2Str(fsUnderline in F.Style) + '" strikeout="' +
      Bool2Str(fsStrikeOut in F.Style) + '"/>');
  end;

  procedure WrColumn(C: TRpGridColumn);
  begin
    WrStr('<column color="' + ColorToString(C.Color) + '" fixedcolor="' +
      ColorToString(C.FixedColor) + '" caption="' + C.Caption +
      '" width="' + IntToStr(C.Width) + '" fieldname="' + C.FieldNameDS +
      '" index="' + IntToStr(C.Index) + '" visible="' + Bool2Str(C.Visible));
    if not C.AutoAlignment then
      WrStr('" autoalignment="0" alignment="' + IntToStr(Ord(C.Alignment)));
    if not C.AutoLayout then
      WrStr('" autolayout="0" layout="' + IntToStr(Ord(C.Layout)));
    if C.TitleAlignment <> taLeftJustify then
      WrStr('" titlealignment="' + IntToStr(Ord(C.TitleAlignment)));
    if C.TitleLayout <> tlCenter then
      WrStr('" titlelayout="' + IntToStr(Ord(C.TitleLayout)));
    WrStr('">');
    WrFont(C.Font, 'font');
    WrFont(C.TitleFont, 'titlefont');
    WrStr('</column>');
  end;

  procedure WrGrid(G: TRpGrid);
  var
    i: Integer;
  begin
    WrStr('<grid color="' + ColorToString(G.Color) + '" alternatecolor="' +
      ColorToString(G.AlternateColor) + '" selectedcolor="' + ColorToString(G.SelectedColor) +
      '" selectedtextcolor="' + ColorToString(G.SelectedTextColor) +
      '" inactiveselectedcolor="' + ColorToString(G.InactiveSelectedColor) +
      '" inactiveselectedtextcolor="' + ColorToString(G.InactiveSelectedTextColor) +
      '" fixedcolor="' + ColorToString(G.FixedColor) +
      {'" fixedhotcolor="' + ColorToString(G.FixedHotColor) + }
      '" gridlinecolor="' + ColorToString(G.GridLineColor) +
      '" gridlinestyle="' + PenStyle2Str(G.GridLineStyle) +
      '" defaultrowheight="' + IntToStr(G.DefaultRowHeight) +
      '" vertlines="' + Bool2Str(G.VertLines) +
      '" horzlines="' + Bool2Str(G.HorzLines) +
      '" flat="' + Bool2Str(G.Flat) +
      '" wordwrap="' + Bool2Str(G.WordWrap) +
      '" rowselect="' + Bool2Str(G.RowSelect) +
      '" rowhighlight="' + Bool2Str(G.RowHighlight) +
      '" cellellipsis="' + Bool2Str(G.CellEllipsis) +
      '" showhints="' + Bool2Str(G.ShowHints) +
      '" thumbtracking="' + Bool2Str(G.ThumbTracking) +
      '" colmove="' + Bool2Str(G.ColMove) +
      '" allowchangesort="' + Bool2Str(G.AllowChangeSort) +
      '" indicator="' + Bool2Str(G.Indicator) +
      '" rowdelbn="' + Bool2Str(G.ShowRowDeleteButton) +
      '">');
    WrFont(G.Font, 'font');
    WrFont(G.TitleFont, 'titlefont');
    WrStr('<columns>');
    for i := 0 to G.ColumnCount - 1 do
      WrColumn(G.Columns[i]);
    WrStr('</columns></grid>');
  end;

  procedure WrCalcFields;
  var
    i: Integer;
    CF: TRpCalcField;
  begin
    WrStr('<calcfields>');
    for i := 0 to FCalcFields.Count - 1 do
    begin
      CF := FCalcFields[i]^;
      WrStr('<calcfield name="' + StrToXml(CF.Name) + '" expression="' +
        StrToXml(CF.Expr) + '" id="' + IntToStr(CF.Id) +
        '" tp="' + IntToStr(Ord(CF.Tp)) + '" size="' + IntToStr(CF.Size) + '"/>');
    end;
    WrStr('</calcfields>');
  end;

  procedure WrSortCols;
  var
    S, Tmp: String;
    i: Integer;
    Col: TRpGridSortData;
  begin
    S := '';
    for i := 0 to Grid.SortCols.Count - 1 do
    begin
      Col := Grid.SortCols[i];
      Tmp := Col.Col.FieldNameDS;
      if Col.Desc then Tmp := '*' + Tmp;
      S := S + Tmp;
      if i < Grid.SortCols.Count - 1 then S := S + ';';
    end;
    SortOrder := S;
  end;

  procedure WrTotals;
  var
    i: Integer;
    T: TRpTotalData;
  begin
    WrStr('<totals>');
    for i := 0 to FTotals.Count - 1 do
    begin
      T := FTotals[i];
      WrStr('<total caption="' + T.Caption + '" field="' + T.FieldNameDS +
        '" func="' + IntToStr(Ord(T.Func)) + '"/>');
    end;
    WrStr('</totals>');
  end;

  procedure WrColoring;
  var
    i: Integer;
    C: TColoringData;
  begin
    WrStr('<coloring>');
    for i := 0 to FColoring.Count - 1 do
    begin
      C := FColoring[i];
      WrStr('<coloringdata color="' + ColorToString(C.Color) + '" ' +
        'fieldname="' + C.FieldName + '" expression="' + StrToXml(C.Expr) + '"/>');
    end;
    WrStr('</coloring>');
  end;

  procedure WrTemplates;
  var
    i: Integer;
  begin
    WrStr('<templates>');
    for i := 0 to FTemplates.Count - 1 do
      WrStr('<template filename="' + StrToXml(FTemplates[i]) + '"/>');
    WrStr('</templates>');
  end;

  procedure WrPrintFields;
  var
    i: Integer;
  begin
    WrStr('<printfields>');
    for i := 0 to FPrintFields.Count - 1 do
      WrStr('<printfield name="' + StrToXml(FPrintFields.Names[i]) +
        '" value="' + StrToXml(FPrintFields.ValueFromIndex[i]) + '"/>');
    WrStr('</printfields>');
  end;

  procedure WrSqlFields;
  var
    i: Integer;
    F: TSQLField;
  begin
    WrStr('<sqlfields>');
    for i := 0 to FSqlFields.Count - 1 do
    begin
      F := FSqlFields[i];
      WrStr('<sqlfield name="' + StrToXml(F.Name) + '" field="' + StrToXml(F.FieldNameDS) +
        '" tp="' + IntToStr(Ord(F.Tp)) + '" format="' + StrToXml(F.DisplayFormat) + '">');
    end;
    WrStr('</sqlfields>');
  end;

var
  i: Integer;

begin
  WrSortCols;
  WrStr('<reportdata id="' + IntToStr(FId) + '" name="' + FName +
    '" sortorder="' + FSortOrder + '" kind="' + IntToStr(Ord(FKind)) +
    '" datefield="' + IntToStr(FDateField) +
    '" datedetail="' + IntToStr(Ord(FDateDetail)) +
    '" first="' + IntToStr(FFirstRecordCount) +
    '" filter="' + StrToXml(FFilter) + '" helptext="' + HtmlToXml(FHelpText) +
    '" sql="' + StrToXml(FSQL) + '" sqlmode="' + Bool2Str(FSqlMode) +
    '" version="' + IntToStr(FVersion) + '">');
  WrStr('<sources>');
  for i := 0 to Sources.Count - 1 do
    WrSrc(Sources[i]^);
  WrStr('</sources>');
  WrCalcFields;
  WrTotals;
  WrColoring;
  WrGrid(Grid);
  WrTemplates;
  WrPrintFields;
  WrSqlFields;
  WrStr('</reportdata>');
end;

procedure TReportData.Clear;
begin
  Sources.Clear;
  DateField := 0;
  DateDetail:=ddDay;
  Filter:='';
  Grid.ClearColumns;
  Grid.SortCols.Clear;
end;

function TReportData.FindField(aId: Integer): PRpField;
var
  i: Integer;
  pF: PRpField;
begin
  Result := nil;
  for i := 0 to Sources.Count - 1 do
  begin
    pF := Sources[i]^.Fields.FindField(aId);
    if pF <> nil then Exit(pF);
  end;
end;

function TReportData.FindFieldIndex(aId: Integer): Integer;
var
  i: Integer;
  pF: PRpField;
begin
  Result := -1;
  for i := 0 to Sources.Count - 1 do
  begin
    pF := Sources[i]^.Fields.FindField(aId);
    if (pF <> nil) and (not pF^.Zero) then Exit(Sources[i]^.Fields.IndexOf(pF));
  end;
end;

function TReportData.FindFieldByName(const aName: String): PRpField;
var
  i: Integer;
  pF: PRpField;
begin
  pF := nil;
  for i := 0 to Sources.Count - 1 do
  begin
    pF := Sources[i]^.Fields.FindFieldByName(aName);
    if (pF <> nil) and (not pF^.Zero) then Exit(pF);
  end;
  Result := pF;
end;

procedure TReportData.FindFieldByColumn(Col: TRpGridColumn; out pF: PRpField;
  out pCF: PRpCalcField);
begin
  pF := nil; pCF := nil;
  if Col.IsCalcField then
    pCF := FCalcFields.FindField(Col.GetFieldId)
  else
    pF := FindField(Col.GetFieldId);
end;

function TReportData.TryGetRpField(AIndex: Integer): PRpField;
var
  i: Integer;
  pF: PRpField;
begin
  Result := nil;
  if FSources.Count = 0 then Exit;

  if FSources[0]^.Fields.Count > AIndex then
  begin
    for i := 0 to FSources.Count - 1 do
    begin
      pF := FSources[i]^.Fields[AIndex];
      if not pF^.Zero then Exit(pF);
    end;

    // Нет выбранных полей - это поле с функцией "Количество".
    Result := FSources[0]^.Fields[AIndex];
  end;
end;

function TReportData.TryGetCalcField(AIndex: Integer): PRpCalcField;
var
  n: Integer;
begin
  Result := nil;
  if FSources.Count > 0 then
    n := FSources[0]^.Fields.Count
  else
    n := FSQLFields.Count;

  if n + FCalcFields.Count > AIndex then
    Result := FCalcFields[AIndex - n];
end;

function TReportData.TryGetSQLField(AIndex: Integer): TSQLField;
begin
  Result := nil;
  if FSQLFields.Count > AIndex then
    Result := FSQLFields[AIndex];
end;

function TReportData.GetFieldCount: Integer;
var
  n: Integer;
begin
  if FSources.Count > 0 then
    n := FSources[0]^.Fields.Count
  else
    n := FSQLFields.Count;
  Result := n + FCalcFields.Count;
end;

function TReportData.GetRpSQLFieldCount: Integer;
begin
  if FSources.Count > 0 then
    Result := FSources[0]^.Fields.Count
  else
    Result := FSQLFields.Count;
end;

function TReportData.IsEmpty: Boolean;
begin
  Result := (FSources.Count = 0) and (FSQLFields.Count = 0);
end;

procedure TReportData.CheckFieldIndex(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= GetFieldCount) then
    raise Exception.Create('Field index out of range.');
end;

function TReportData.GetFieldNameDS(AIndex: Integer): String;
var
  pF: PRpField;
  pCF: PRpCalcField;
  SqlF: TSQLField;
begin
  CheckFieldIndex(AIndex);

  pF := TryGetRpField(AIndex);
  if pF <> nil then Exit('f' + IntToStr(pF^.Id));

  SqlF := TryGetSQLField(AIndex);
  if SqlF <> nil then Exit(SqlF.FieldNameDS);

  pCF := TryGetCalcField(AIndex);
  if pCF <> nil then Exit('cf' + IntToStr(pCF^.Id));
end;

function TReportData.GetFieldName(AIndex: Integer): String;
var
  pF: PRpField;
  pCF: PRpCalcField;
  SqlF: TSQLField;
begin
  CheckFieldIndex(AIndex);

  pF := TryGetRpField(AIndex);
  if pF <> nil then Exit(pF^.Name);

  SqlF := TryGetSQLField(AIndex);
  if SqlF <> nil then Exit(SqlF.Name);

  pCF := TryGetCalcField(AIndex);
  if pCF <> nil then Exit(pCF^.Name);
end;

function TReportData.GetFieldType(AIndex: Integer): TRpFieldType;
var
  pF: PRpField;
  pCF: PRpCalcField;
  SqlF: TSQLField;
begin
  CheckFieldIndex(AIndex);

  pF := TryGetRpField(AIndex);
  if pF <> nil then
  begin
    if pF^.Func in [tfMerge, tfMergeAll] then Exit(flText)
    else if pF^.Func in [tfCount, tfDistCount] then Exit(flNumber)
    else if (pF^.Tp = flDate) and (DateField = AIndex) then
    begin
      if FDateDetail = ddDay then Exit(flDate)
      else if FDateDetail = ddYear then Exit(flNumber)
      else Exit(flText);
    end
    else Exit(GetLowField(pF)^.Tp);
  end;

  SqlF := TryGetSQLField(AIndex);
  if SqlF <> nil then Exit(SqlF.Tp);

  pCF := TryGetCalcField(AIndex);
  if pCF <> nil then Exit(pCF^.Tp);
end;

function TReportData.GetFieldVisible(AIndex: Integer): Boolean;
var
  pF: PRpField;
begin
  CheckFieldIndex(AIndex);
  pF := TryGetRpField(AIndex);
  if pF <> nil then Exit(pF^.Visible);

  // В ином случае...
  Result := True;
end;

function TReportData.GetFieldParam(AIndex: Integer): Boolean;
var
  pF: PRpField;
begin
  CheckFieldIndex(AIndex);
  pF := TryGetRpField(AIndex);
  if pF <> nil then Exit(pF^.Param);

  // В ином случае...
  Result := False;
end;

function TReportData.GetDisplayFormat(AIndex: Integer): String;
var
  pF, pLowF: PRpField;
  pCF: PRpCalcField;
  SqlF: TSQLField;
  Fm: TdxForm;
  SS: TSession;
  C: TdxField;
begin
  CheckFieldIndex(AIndex);
  Result := '';
  SS := TSession(FSession);

  pF := TryGetRpField(AIndex);
  if pF <> nil then
  begin
    if pF^.Zero then Exit;

    pLowF := GetLowField(pF);
    Fm := SS.FormMan.FindForm(pLowF^.TId);
    C := Fm.FindField(pLowF^.FId);
    if (C is TdxCalcEdit) and not (pF^.Func in [tfMerge, tfMergeAll, tfCount, tfDistCount]) then
      Result := TdxCalcEdit(C).PrecStr
    else if (C is TdxTimeEdit) and (pF^.Func = tfNone) then
      Result := TdxTimeEdit(C).TimeFormatStr;
    Exit;
  end;

  SqlF := TryGetSQLField(AIndex);
  if SqlF <> nil then
    Exit(SqlF.DisplayFormat);

  pCF := TryGetCalcField(AIndex);
  if pCF <> nil then
  begin
    if pCF^.Tp = flNumber then
    begin
      Result := ',0';
      if pCF^.Size > 0 then Result := ',0.' + DupeString('0', pCF^.Size);
    end
    else if pCF^.Tp = flTime then
    	Result := 'hh:mm:ss';
  end;
end;

function TReportData.IsCalcField(AIndex: Integer): Boolean;
begin
  CheckFieldIndex(AIndex);
  if (TryGetRpField(AIndex) = nil) and (TryGetSQLField(AIndex) = nil) and
    (TryGetCalcField(AIndex) <> nil) then Result := True
  else
    Result := False;
end;

function TReportData.IndexOfName(AFieldName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to GetFieldCount - 1 do
    if MyUtf8CompareText(GetFieldName(i), AFieldName) = 0 then Exit(i);
end;

function TReportData.IndexOfNameDS(AFieldNameDS: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to GetFieldCount - 1 do
    if CompareText(GetFieldNameDS(i), AFieldNameDS) = 0 then Exit(i);
end;

function TReportData.QueryExistsInExpr(AQueryName: String): Boolean;
var
  j: Integer;
  CF: TRpCalcField;
begin
  Result := True;
  for j := 0 to FSources.Count - 1 do
  begin
    if FormExistsInExpr(AQueryName, FSources[j]^.Filter) then Exit;
  end;
  if FormExistsInExpr(AQueryName, FSQL) then Exit;
  if FormExistsInExpr(AQueryName, FFilter) then Exit;
  for j := 0 to FCalcFields.Count - 1 do
  begin
    CF := FCalcFields[j]^;
    if FormExistsInExpr(AQueryName, CF.Expr) then Exit;
  end;
  Result := False;
end;

function TReportData.FieldExistsInExpr(AFieldName: String): Boolean;
var
  i: Integer;
  CF: TRpCalcField;
begin
  Result := False;
  for i := 0 to FSources.Count - 1 do
    if FieldExists(AFieldName, FSources[i]^.Filter) then Exit(True);
  if FieldExistsForQuery(AFieldName, FFilter) then Exit(True);
  for i := 0 to FCalcFields.Count - 1 do
  begin
    CF := FCalcFields[i]^;
    if FieldExistsForQuery(AFieldName, CF.Expr) then Exit(True);
  end;
  Result := FieldExistsForQuery(AFieldName, FSQL);
end;

function TReportData.GetEditFormId: Integer;
begin
  if FSources.Count = 1 then
    Result := FSources[0]^.Id
  else
    Result := 0;
end;

function TReportData.GetSourceFilter: String;
begin
  if FSources.Count = 1 then
    Result := FSources[0]^.Filter
  else
    Result := '';
end;

function TReportData.IsSimple: Boolean;
var
  Sr: TRpSource;
  i: Integer;
  Fl: TRpField;
begin
  Result := False;
  if FSources.Count = 1 then
  begin
    Sr := FSources[0]^;
    for i := 0 to Sr.Fields.Count - 1 do
    begin
      Fl := Sr.Fields[i]^;
      if Fl.Func <> tfNone then Exit;
    end;
    Result := FDateField < 0;
  end;
end;

function TReportData.HasParentIdField: Boolean;
begin
  Result := (FSources.Count = 1) and (FSources[0]^.TId > 0);
end;

function TReportData.ParamExists: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to GetFieldCount - 1 do
    if GetFieldParam(i) then Exit(True);
end;

end.

