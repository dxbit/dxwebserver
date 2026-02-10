{-------------------------------------------------------------------------------

    Copyright 2016-2025 Pavel Duborkin ( mydataexpress@mail.ru )

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

unit DXCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, BGRAReadTiff, Db, FPCanvas,
  FPImage, BGRAGraphics, typeswrapper, strconsts, mytypes, SqlDb;

const
  StorageTypeDB = 0;
  StorageTypeFolder = 1;
  StorageTypeLink = 2;

  DummyForm = -1;


type
  TdxForm = class;

  TAccessStatus = (asOk, asCantAppend, asCantEdit, asCantDelete, asModified,
    asDeleted, asLocked, asHasRef);
  TLockMode = (lmNoLock, lmPessimistic);
  TCreateFormEvent = procedure (Sender: TObject; Fm: TdxForm) of object;
  TValidateEvent = procedure (Sender: TObject; var Ok: Boolean) of object;
  TFieldChangeEvent = procedure (Sender, Control: TObject; const FieldName: String) of object;
  TPrintActionType = (paBeginPrint, paEndPrint, paPrintField, paBeginData, paNextData,
    paBeforeOpenFile, paAfterOpenFile, paPrintError);
  TPrintEvent = procedure (Sender: TObject; Action: TPrintActionType;
  	const SourceName, FieldName: String; var Value: String; var Accept: Boolean) of object;
  TAnchorKind = (akTop, akLeft, akRight, akBottom);
  TAnchors = set of TAnchorKind;


  TdxComponent = class;
  TdxWinControl = class;
  TdxComponentClass = class of TdxComponent;
  TdxControlClass = class of TdxControl;

  { TdxFont }
  TdxFont = class(TFont)
  private
    FColor: TColor;
    function GetColor: TColor;
    function GetStyle: TFontStyles;
    procedure SetColor(AValue: TColor);
    procedure SetStyle(AValue: TFontStyles);
  protected
    procedure SetName(AValue: String); override;
    procedure SetSize(AValue: Integer); override;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    function IsDefault: Boolean;
    function IsEqual(F: TdxFont): Boolean;
    property Color: TColor read GetColor write SetColor;
    property Style: TFontStyles read GetStyle write SetStyle;
  end;

  {TdxFont = class
  private
    FColor: TColor;
    FHeight: Integer;
    FName: String;
    FSize: Integer;
    FStyle: TFontStyles;
  public
    constructor Create;
    procedure Assign(Source: TdxFont);
    property Name: String read FName write FName;
    property Style: TFontStyles read FStyle write FStyle;
    property Color: TColor read FColor write FColor;
    property Size: Integer read FSize write FSize;
    property Height: Integer read FHeight write FHeight;
  end;   }

  { TdxBrush }

  TdxBrush = class(TBrush)
  protected
    procedure SetStyle(AValue: TFPBrushStyle); override;
    procedure SetFPColor(const AValue: TFPColor); override;
  public
    procedure Assign(Source: TPersistent); override;
  end;

  { TdxPen }

  TdxPen = class(TPen)
  protected
    procedure SetStyle(AValue: TFPPenStyle); override;
    procedure SetFPColor(const AValue: TFPColor); override;
    procedure SetWidth(AValue: Integer); override;
  public
    procedure Assign(Source: TPersistent); override;
  end;

  {private
    FColor: TColor;
    FFontStyles: TFontStyles;
    //FHeight: Integer;
    FName: String;
    FSize: Integer;
  public
    constructor Create;
    procedure AssignFont(Fnt: TdxFont);
    property Name: String read FName write FName;
    property Style: TFontStyles read FFontStyles write FFontStyles;
    property Color: TColor read FColor write FColor;
    property Size: Integer read FSize write FSize;
    //property Height: Integer read FHeight write FHeight;
  end; }

  TComponentList = class;

  { TdxComponent }

  TdxComponent = class(TPersistent)
  private
    FName: String;
    FComponents: TComponentList;
    FOwner: TdxComponent;
    function GetComponentCount: Integer;
    function GetComponents(Index: Integer): TdxComponent;
  public
    constructor Create(AOwner: TdxComponent); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Form: TdxForm;
    function FindComponent(const AName: String): TdxComponent;
    property Name: String read FName write FName;
    property Components[Index: Integer]: TdxComponent read GetComponents;
    property ComponentCount: Integer read GetComponentCount;
    property Owner: TdxComponent read FOwner;
  end;

  { TComponentList }

  TComponentList = class(TList)
  private
    function GetComponents(Index: Integer): TdxComponent;
  public
    procedure Clear; override;
    property Components[Index: Integer]: TdxComponent read GetComponents; default;
  end;

  TPropertyChangeEvent = procedure (Sender: TObject; const PropName: String) of object;

  { TdxControl }

  TdxControl = class(TdxComponent)
  private
    FAnchors: TAnchors;
    FCaption: String;
    FColor: TColor;
    FControlVisible: Boolean;
    FFontStyleParsed: Boolean;
    FHidden: Boolean;
    FEnabled: Boolean;
    FFont: TdxFont;
    FHeight: Integer;
    FHint: String;
    FLeft: Integer;
    FOnChangeBounds: TNotifyEvent;
    FOnPropertyChange: TPropertyChangeEvent;
    FOnResize: TNotifyEvent;
    FParent: TdxWinControl;
    FParentFont: Boolean;
    FTabOrder: Integer;
    FTabStop: Boolean;
    FTop: Integer;
    FVisible: Boolean;
    FWidth: Integer;
    procedure DoResize;
    procedure DoChangeBounds;
    procedure ProcessResize(dw, dh: Integer);
    procedure FontChange(Sender: TObject);
    function GetBoundsRect: TRect;
    procedure SetBoundsRect(AValue: TRect);
    procedure SetCaption(AValue: String);
    procedure SetColor(AValue: TColor);
    procedure SetEnabled(AValue: Boolean);
    procedure SetHeight(AValue: Integer);
    procedure SetLeft(AValue: Integer);
    procedure SetParent(AValue: TdxWinControl);
    procedure DoPropertyChange(const PropName: String);
    procedure SetParentFont(AValue: Boolean);
    procedure SetTop(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
    procedure SetWidth(AValue: Integer);
    procedure SetFont(AValue: TdxFont);
  public
    constructor Create(AOwner: TdxComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Hide;
    procedure Show;
    procedure SetBounds(X, Y, W, H: Integer);
    function GetRealFont: TdxFont;
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Parent: TdxWinControl read FParent write SetParent;
    property Color: TColor read FColor write SetColor;
    property Font: TdxFont read FFont write SetFont;
    property TabOrder: Integer read FTabOrder write FTabOrder;
    property TabStop: Boolean read FTabStop write FTabStop;
    property ParentFont: Boolean read FParentFont write SetParentFont;
    property Visible: Boolean read FVisible write SetVisible;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property Caption: String read FCaption write SetCaption;
    property Hidden: Boolean read FHidden write FHidden;
    property FontStyleParsed: Boolean read FFontStyleParsed write FFontStyleParsed;
    property ControlVisible: Boolean read FControlVisible write FControlVisible;
    property Hint: String read FHint write FHint;
    property Anchors: TAnchors read FAnchors write FAnchors;
    property OnPropertyChange: TPropertyChangeEvent read FOnPropertyChange
      write FOnPropertyChange;
    property OnChangeBounds: TNotifyEvent read FOnChangeBounds write FOnChangeBounds;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
  end;

  { TControlList }

  TControlList = class(TList)
  private
    function GetControls(Index: Integer): TdxControl;
  public
    property Controls[Index: Integer]: TdxControl read GetControls; default;
  end;

  { TdxField }

  TdxField = class(TdxControl)
  private
    FFieldName: String;
    FId: Integer;
    FTextHint: String;
  public
    procedure Assign(Source: TPersistent); override;
    property Id: Integer read FId write FId;
    property FieldName: String read FFieldName write FFieldName;
    property TextHint: String read FTextHint write FTextHint;
  end;

  { TdxWinControl }

  TdxWinControl = class(TdxControl)
  private
    FControls: TControlList;
    function GetControlCount: Integer;
    function GetControls(Index: Integer): TdxControl;
  public
    constructor Create(AOwner: TdxComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Controls[Index: Integer]: TdxControl read GetControls;
    property ControlCount: Integer read GetControlCount;
  end;

  { TdxLabel }

  TdxLabel = class(TdxControl)
  private
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FExpr: String;
    FFieldName: String;
    FLayout: TTextLayout;
    FValue: Variant;
    FWordWrap: Boolean;
  public
    constructor Create(AOwner: TdxComponent); override;
    procedure Assign(Source: TPersistent); override;
    property FieldName: String read FFieldName write FFieldName;
    property Expression: String read FExpr write FExpr;
    property Value: Variant read FValue write FValue;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Layout: TTextLayout read FLayout write FLayout;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property AutoSize: Boolean read FAutoSize write FAutoSize;
  end;

  { TdxEdit }

  TdxEdit = class(TdxField)
  private
    FCheckExpression: String;
    FDefaultValue: String;
    FEditable: Boolean;
    FExpression: String;
    FFieldSize: Integer;
    FRequired: Boolean;
  public
    procedure Assign(Source: TPersistent); override;
    property Expression: String read FExpression write FExpression;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property Required: Boolean read FRequired write FRequired;
    property FieldSize: Integer read FFieldSize write FFieldSize;
    property Editable: Boolean read FEditable write FEditable;
  end;

  { TdxCalcEdit }

  TdxCalcEdit = class(TdxField)
  private
    FCheckExpression: String;
    FDefaultValue: String;
    FEditable: Boolean;
    FExpression: String;
    FGroupDigits: Boolean;
    FMaxValue: Double;
    FMinValue: Double;
    FPadZeros: Boolean;
    FPrecission: Integer;
    FRequired: Boolean;
  public
    function PrecStr: String;
    procedure Assign(Source: TPersistent); override;
    property Expression: String read FExpression write FExpression;
    property Precission: Integer read FPrecission write FPrecission;
    property MinValue: Double read FMinValue write FMinValue;
    property MaxValue: Double read FMaxValue write FMaxValue;
    property Required: Boolean read FRequired write FRequired;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property Editable: Boolean read FEditable write FEditable;
    property GroupDigits: Boolean read FGroupDigits write FGroupDigits;
    property PadZeros: Boolean read FPadZeros write FPadZeros;
  end;

  { TdxDateEdit }

  TdxDateEdit = class(TdxField)
  private
    FCheckExpression: String;
    FDateNow: Boolean;
    FDefaultValue: String;
    FEditable: Boolean;
    FExpression: String;
    FHideButton: Boolean;
    FRequired: Boolean;
  public
    procedure Assign(Source: TPersistent); override;
    property DateNow: Boolean read FDateNow write FDateNow;
    property Expression: String read FExpression write FExpression;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property Required: Boolean read FRequired write FRequired;
    property Editable: Boolean read FEditable write FEditable;
    property HideButton: Boolean read FHideButton write FHideButton;
  end;

  { TdxMemo }

  TdxMemo = class(TdxField)
  private
    FCheckExpression: String;
    FDefaultValue: String;
    FEditable: Boolean;
    FExpression: String;
    FFieldSize: Integer;
    FRequired: Boolean;
  public
    procedure Assign(Source: TPersistent); override;
    property Required: Boolean read FRequired write FRequired;
    property FieldSize: Integer read FFieldSize write FFieldSize;
    property Expression: String read FExpression write FExpression;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property Editable: Boolean read FEditable write FEditable;
  end;

  { TdxCheckBox }

  TdxCheckBox = class(TdxField)
  private
    FCheckedText: String;
    FCheckExpression: String;
    FDefaultValue: String;
    FEditable: Boolean;
    FExpression: String;
    FUnCheckedText: String;
  public
    procedure Assign(Source: TPersistent); override;
    property CheckedText: String read FCheckedText write FCheckedText;
    property UnCheckedText: String read FUnCheckedText write FUnCheckedText;
    property Expression: String read FExpression write FExpression;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property Editable: Boolean read FEditable write FEditable;
  end;

  { TdxCustomComboBox }

  TdxCustomComboBox = class(TdxField)
  private
    FCheckExpression: String;
    FDefaultValue: String;
    FDropDownCount: Integer;
    FEditable: Boolean;
    FExpression: String;
    FFieldSize: Integer;
    FFilter: String;
    FItems: TStrings;
    FRequired: Boolean;
    FSourceFId: Integer;
    FSourceTId: Integer;
    FStyle: TComboBoxStyle;
    function GetSourceFieldName: String;
    function GetSourceFormName: String;
    procedure SetItems(AValue: TStrings);
  public
    constructor Create(AOwner: TdxComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Items: TStrings read FItems write SetItems;
    property SourceTId: Integer read FSourceTId write FSourceTId;
    property SourceFId: Integer read FSourceFId write FSourceFId;
    property Filter: String read FFilter write FFilter;
    property Style: TComboBoxStyle read FStyle write FStyle;
    property Required: Boolean read FRequired write FRequired;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
    property FieldSize: Integer read FFieldSize write FFieldSize;
    property Expression: String read FExpression write FExpression;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property Editable: Boolean read FEditable write FEditable;
    property DropDownCount: Integer read FDropDownCount write FDropDownCount;

    property SourceFormName: String read GetSourceFormName;
    property SourceFieldName: String read GetSourceFieldName;
  end;

  { TdxComboBox }

  TdxComboBox = class(TdxCustomComboBox)
  private
    FItemsOnly: Boolean;
  public
    property ItemsOnly: Boolean read FItemsOnly write FItemsOnly;
  end;

  { TInsertValueData }

  TInsertValueData = class
  public
    SrcField, DestField: Integer;
  end;

  { TInsertedValues }

  TInsertedValues = class(TList)
  private
    function GetValues(Index: Integer): TInsertValueData;
  public
    procedure Assign(S: TInsertedValues);
    function AddValue: TInsertValueData;
    procedure DeleteValue(Index: Integer);
    procedure Clear; override;
    property Values[Index: Integer]: TInsertValueData read GetValues; default;
  end;

    { TLCbxListField }

  TLCbxListField = class
  private
    FFieldId, FWidth: Integer;
    FFieldName: String;
    FSearchable: Boolean;
  public
    property FieldId: Integer read FFieldId write FFieldId;
    property FieldName: String read FFieldName write FFieldName;
	  property Width: Integer read FWidth write FWidth;
    property Searchable: Boolean read FSearchable write FSearchable;
  end;

  { TLCbxListFields }

  TLCbxListFields = class(TList)
  private
    function GetFields(Index: Integer): TLCbxListField;
  public
    procedure Assign(S: TLCbxListFields);
    function AddField: TLCbxListField;
    procedure Clear; override;
    property Fields[Index: Integer]: TLCbxListField read GetFields; default;
  end;

  { TdxLookupComboBox }

  TdxLookupComboBox = class(TdxCustomComboBox)
  private
    FHideButton: Boolean;
    FHideList: Boolean;
    FInsertedValues: TInsertedValues;
    FListFields: TLCbxListFields;
    FListKeyField: String;
    FListSource: Integer;
    FListWidthExtra: Integer;
    FOnCreateForm: TCreateFormEvent;
    FShowAsTreeList: Boolean;
    procedure SetListFields(AValue: TLCbxListFields);
  public
    constructor Create(AOwner: TdxComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property InsertedValues: TInsertedValues read FInsertedValues;
    property ListFields: TLCbxListFields read FListFields write SetListFields;
    property ListWidthExtra: Integer read FListWidthExtra write FListWidthExtra;
    property HideList: Boolean read FHideList write FHideList;
    property HideButton: Boolean read FHideButton write FHideButton;
    property ListKeyField: String read FListKeyField write FListKeyField;
    property ListSource: Integer read FListSource write FListSource;
    property ShowAsTreeList: Boolean read FShowAsTreeList write FShowAsTreeList;
    property OnCreateForm: TCreateFormEvent read FOnCreateForm write FOnCreateForm;
  end;

  { TdxObjectField }

  TdxObjectField = class(TdxField)
  private
    FFieldId: Integer;
    FObjId: Integer;
  public
    procedure Assign(Source: TPersistent); override;
    property ObjId: Integer read FObjId write FObjId;
    property FieldId: Integer read FFieldId write FFieldId;
  end;

  { TdxImage }

  TdxImage = class(TdxControl)
  private
    //FBitmap: TBGRABitmap;
    FCenter: Boolean;
    FExt: String;
    FImageName: String;
    FKeepSize: Boolean;
    FProportional: Boolean;
    FStretch: Boolean;
    FModified: Boolean;
    procedure SetImageName(AValue: String);
  public
    constructor Create(AOwner: TdxComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetImagePath(Html: Boolean): String;
    //property Bitmap: TBGRABitmap read FBitmap;
    procedure Clear;
    procedure SaveToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String);
    procedure LoadFromStream(St: TStream);
    procedure SaveToStream(St: TStream);
    property Ext: String read FExt write FExt;
    property Center: Boolean read FCenter write FCenter;
    property Proportional: Boolean read FProportional write FProportional;
    property Stretch: Boolean read FStretch write FStretch;
    property KeepSize: Boolean read FKeepSize write FKeepSize;
    property ImageName: String read FImageName write SetImageName;
  end;

  { TdxDBImage }

  TdxDBImage = class(TdxField)
  private
    FCheckExpression: String;
    FIsQuery: Boolean;
    FPrintSize: Integer;
    FRequired: Boolean;
    FShowThumbnail: Boolean;
    FStorageFolder: String;
    FStorageType: Integer;
    FThumbSize: Integer;
    function GetSourceFileName: String;
    function GetStoredFileName: String;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure SaveToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String);
    procedure SaveToStream(St: TStream);
    function WasChanged: Boolean;
    property ShowThumbnail: Boolean read FShowThumbnail write FShowThumbnail;
    property StorageType: Integer read FStorageType write FStorageType;
    property StorageFolder: String read FStorageFolder write FStorageFolder;
    property ThumbSize: Integer read FThumbSize write FThumbSize;
    property PrintSize: Integer read FPrintSize write FPrintSize;
    property Required: Boolean read FRequired write FRequired;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property SourceFileName: String read GetSourceFileName;
    property StoredFileName: String read GetStoredFileName;
    property IsQuery: Boolean read FIsQuery write FIsQuery;
  end;

  TShapeType = (stRectangle, stSquare, stRoundRect, stRoundSquare, stEllipse,
    stCircle, stSquaredDiamond, stDiamond, stTriangle, stTriangleLeft,
    stTriangleRight, stTriangleDown, stStar, stStarDown, stPolygon);
  TShapeTypeEx = (steNone, steVertLine, steHorzLine, steBDiagonal, steFDiagonal, steCross, steDiagCross);

  { TdxShape }

  TdxShape = class(TdxControl)
  private
    FBrush: TdxBrush;
    FPen: TdxPen;
    FShape: TShapeType;
    FModified: Boolean;
    FShapeEx: TShapeTypeEx;
    procedure BrushChange(Sender: TObject);
    procedure PenChange(Sender: TObject);
    procedure SetBrush(AValue: TdxBrush);
    procedure SetPen(AValue: TdxPen);
    procedure SetShape(AValue: TShapeType);
    procedure SetShapeEx(AValue: TShapeTypeEx);
    //procedure Paint(Canvas: TBGRACanvas);
  public
    constructor Create(AOwner: TdxComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetImagePath(Html: Boolean): String;
    //procedure SaveToFile(const FileName: String);
    property Brush: TdxBrush read FBrush write SetBrush;
    property Pen: TdxPen read FPen write SetPen;
    property Shape: TShapeType read FShape write SetShape;
    property ShapeEx: TShapeTypeEx read FShapeEx write SetShapeEx;
    property Modified: Boolean read FModified write FModified;
  end;

  { TdxFile }

  TdxFile = class(TdxField)
  private
    FCheckExpression: String;
    FFieldSize: Integer;
    FIsQuery: Boolean;
    FRequired: Boolean;
    FStorageFolder: String;
    FStorageType: Integer;
    function GetDescription: String;
    function GetSourceFileName: String;
    function GetStoredFileName: String;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure SaveToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String);
    procedure SaveToStream(St: TStream);
    function WasChanged: Boolean;
    property StorageType: Integer read FStorageType write FStorageType;
    property StorageFolder: String read FStorageFolder write FStorageFolder;
    property FieldSize: Integer read FFieldSize write FFieldSize;
    property Required: Boolean read FRequired write FRequired;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property SourceFileName: String read GetSourceFileName;
    property StoredFileName: String read GetStoredFileName;
    property Description: String read GetDescription;
    property IsQuery: Boolean read FIsQuery write FIsQuery;
  end;

  { TdxColumn }

  TdxColumn = class
  private
    FAlignment: TAlignment;
    FAutoAlignment: Boolean;
    FAutoLayout: Boolean;
    FCaption: String;
    FId: Integer;
    FLayout: TTextLayout;
    FVisible: Boolean;
    FWidth: Integer;
  public
    constructor Create;
    procedure Assign(S: TdxColumn);
    property Id: Integer read FId write FId;
    property Width: Integer read FWidth write FWidth;
    property Visible: Boolean read FVisible write FVisible;
    property Caption: String read FCaption write FCaption;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Layout: TTextLayout read FLayout write FLayout;
    property AutoAlignment: Boolean read FAutoAlignment write FAutoAlignment;
    property AutoLayout: Boolean read FAutoLayout write FAutoLayout;
  end;

  { TdxColumnList }

  TdxColumnList = class(TList)
  private
    function GetColumns(Index: Integer): TdxColumn;
  public
    procedure Assign(S: TdxColumnList);
    function FindCol(Id: Integer): TdxColumn;
    procedure Clear; override;
    property Columns[Index: Integer]: TdxColumn read GetColumns; default;
  end;

  { TdxSortCol }

  TdxSortCol = class
  public
    Index: Integer;
    Desc: Boolean;
  end;

  { TdxSortColList }

  TdxSortColList = class(TList)
  private
    function GetCols(Index: Integer): TdxSortCol;
  public
    procedure Assign(S: TdxSortColList);
    function AddCol: TdxSortCol;
    function FindCol(Index: Integer): TdxSortCol;
    procedure DeleteCol(SC: TdxSortCol);
    procedure Clear; override;
    property Cols[Index: Integer]: TdxSortCol read GetCols; default;
  end;

  TGridButtonType = (gbnAppend, gbnEdit, gbnDelete, gbnDuplicate, gbnShopping,
    gbnMoveUp, gbnMoveDown, gbnRefresh, gbnGoto);

  TGridButtonSet = set of TGridButtonType;

  { TdxCustomGrid }

  TdxCustomGrid = class(TdxControl)
  private
    FAlignButtons: TAlignment;
    FAllowChangeSort: Boolean;
    FButtonFont: TdxFont;
    FButtonSize: Integer;
    FColumns: TdxColumnList;
    FId: Integer;
    FShowButtons: Boolean;
    FShowRowDeleteButton: Boolean;
    FSortCols: TdxSortColList;
    FVisibleButtons: TGridButtonSet;
  public
    constructor Create(AOwner: TdxComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetVisibleColumnCount: Integer;
    property Columns: TdxColumnList read FColumns;
    property Id: Integer read FId write FId;
    property SortCols: TdxSortColList read FSortCols;
    property AllowChangeSort: Boolean read FAllowChangeSort write FAllowChangeSort;
    property AlignmentButtons: TAlignment read FAlignButtons write FAlignButtons;
    property ShowButtons: Boolean read FShowButtons write FShowButtons;
    property VisibleButtons: TGridButtonSet read FVisibleButtons write FVisibleButtons;
    property ButtonSize: Integer read FButtonSize write FButtonSize;
    property ButtonFont: TdxFont read FButtonFont write FButtonFont;
    property ShowRowDeleteButton: Boolean read FShowRowDeleteButton write FShowRowDeleteButton;
  end;

  { TdxGrid }

  TdxGrid = class(TdxCustomGrid);

  { TdxQueryGrid }

  TdxQueryGrid = class(TdxCustomGrid)
  private
    FManualRefresh: Boolean;
    FOnAfterClose: TNotifyEvent;
    FOnAfterOpen: TNotifyEvent;
    FOnAfterScroll: TNotifyEvent;
    FOnBeforeClose: TNotifyEvent;
    FOnBeforeOpen: TNotifyEvent;
    FOnBeforeScroll: TNotifyEvent;
    FOnCreateForm: TCreateFormEvent;
    FRS: TObject;
    function GetAsDT(AIndex: String): TDateTime;
    function GetAsF(AIndex: String): Double;
    function GetAsI(AIndex: String): Integer;
    function GetAsS(AIndex: String): String;
    function GetEditable: Boolean;
    function GetQueryFields(AIndex: String): Variant;
    function GetQueryName: String;
    procedure RequeryIfNeed;
  public
    procedure Assign(Source: TPersistent); override;
    property RecordSet: TObject read FRS write FRS;
    property ManualRefresh: Boolean read FManualRefresh write FManualRefresh;

    property AllowChangeSort: Boolean read FAllowChangeSort write FAllowChangeSort;
    property Editable: Boolean read GetEditable;
    property QueryName: String read GetQueryName;
  public
    procedure Refresh;
    procedure Close;
    procedure MoveFirst;
    procedure MovePrior;
    procedure MoveNext;
    procedure MoveLast;
    procedure MoveBy(Distance: Integer);
    procedure MoveTo(ARecNo: Integer);
    function BOF: Boolean;
    function EOF: Boolean;
    function RecNo: Integer;
    function RecId: Integer;
    function RecordCount: Integer;
    function Locate(const FieldNames: String; FieldValues: array of Variant; Options: TLocateOptions): Boolean;
    function GotoRecord(aRecId: Integer): Boolean;
    procedure DisableScrollEvents;
    procedure EnableScrollEvents;
    function ScrollEventsDisabled: Boolean;
    function FindColumnByTitle(const ATitle: String): TdxColumn;

    function GetSourceFileName(const aName: String): String;
    function GetStoredFileName(const aName: String): String;
    procedure SaveBlobToStreamOrFile(const aName: String; St: TStream; const AFileName: String);
    procedure SaveBlobToStream(const aName: String; St: TStream);
    procedure SaveBlobToFile(const aName, AFileName: String);
    procedure SaveThumbnailToStream(const aName: String; St: TStream);

    property Fields[AIndex: String]: Variant read GetQueryFields;
    property AsI[AIndex: String]: Integer read GetAsI;
    property AsF[AIndex: String]: Double read GetAsF;
    property AsDT[AIndex: String]: TDateTime read GetAsDT;
    property AsS[AIndex: String]: String read GetAsS;
    //property RS: TObject read FRS;
    property OnAfterClose: TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnAfterOpen: TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnAfterScroll: TNotifyEvent read FOnAfterScroll write FOnAfterScroll;
    property OnBeforeClose: TNotifyEvent read FOnBeforeClose write FOnBeforeClose;
    property OnBeforeOpen: TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnBeforeScroll: TNotifyEvent read FOnBeforeScroll write FOnBeforeScroll;
    property OnCreateForm: TCreateFormEvent read FOnCreateForm write FOnCreateForm;
  end;

  { TdxGroupBox }

  TdxGroupBox = class(TdxWinControl)
  public
    procedure Assign(Source: TPersistent); override;
  end;

  { TdxTabSheet }

  TdxTabSheet = class(TdxWinControl)
  private
    function GetPageIndex: Integer;
    function GetTabVisible: Boolean;
    procedure SetTabVisible(AValue: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    property TabVisible: Boolean read GetTabVisible write SetTabVisible;
    property PageIndex: Integer read GetPageIndex;
  end;

  { TdxPageControl }

  TdxPageControl = class(TdxWinControl)
  private
    FTabIndex: Integer;
    function GetPageCount: Integer;
    function GetPages(Index: Integer): TdxTabSheet;
    procedure SetTabIndex(AValue: Integer);
  public
    procedure Assign(Source: TPersistent); override;
    procedure SetVisiblePage;
    property ActivePageIndex: Integer read FTabIndex write SetTabIndex;
    property Pages[Index: Integer]: TdxTabSheet read GetPages;
    property PageCount: Integer read GetPageCount;
  end;

  { TFilterField }

  TFilterField = class
  private
    FForm: TdxForm;
    FFId: Integer;
    FIsNot: Boolean;
    FIsNull: Boolean;
    FValues: TStringList;
    function GetFieldName: String;
    function GetValue(Index: Integer): String;
    function GetEndValue(Index: Integer): String;
  public
    constructor Create(AForm: TdxForm);
    destructor Destroy; override;
    property FId: Integer read FFId write FFId;
    property IsNull: Boolean read FIsNull write FIsNull;
    property IsNot: Boolean read FIsNot write FIsNot;
    property Values: TStringList read FValues;
    property Value[Index: Integer]: String read GetValue;
    property EndValue[Index: Integer]: String read GetEndValue;
    property FieldName: String read GetFieldName;
  end;

  { TFilterObject }

  TFilterObject = class(TList)
  private
    FForm: TdxForm;
    function GetFields(Index: Integer): TFilterField;
  public
    constructor Create(AForm: TdxForm);
    function AddField: TFilterField;
    function FindField(FId: Integer): TFilterField;
    procedure DeleteField(F: TFilterField);
    procedure Clear; override;
    procedure Load(const Filter: String);
    function Save: String;
    function ValuesExists: Boolean;
    property Fields[Index: Integer]: TFilterField read GetFields; default;
  public
    function AddFieldByName(const FieldName: String): TFilterField;
    function FindFieldByName(const FieldName: String): TFilterField;
  end;

  TViewType = (vtGridTop, vtGridBottom, vtGridLeft, vtGridRight, vtGridOnly,
    vtWithoutGrid, vtSimpleForm, vtDefault);

    { TColoringData }

  TColoringData = class
  public
    Color: TColor;
    FieldName: String;
    Expr: String;
    E: TObject;
    destructor Destroy; override;
  end;

  { TColoringList }

  TColoringList = class(TList)
  private
    function GetColorings(Index: Integer): TColoringData;
  public
    function AddColoring: TColoringData;
    function FindColoring(const FieldName: String): TColoringData;
    procedure DeleteColoring(CD: TColoringData);
    procedure Clear; override;
    procedure AssignList(Src: TColoringList);
    property Colorings[Index: Integer]: TColoringData read GetColorings; default;
  end;

  { TdxForm }

  TMsgButtonClickEvent = procedure (Sender: TObject; Button: TMsgDlgBtn) of object;

  TdxForm = class(TdxWinControl)
  private
    FActionOnCreate: String;
    FActionResult: Variant;
    FAutoOpen: Boolean;
    FCalcFields: TStrings;
    FColoring: TColoringList;
    FConfirmCancelEditing: Boolean;
    FConfirmSaveRecord: Boolean;
    FCustomFilter: String;
    FCustomFilterRS: TObject;
    FErrs: TStringList;
    FFilter: TFilterObject;
    FFilters: TStrings;
    FFormCaption: String;
    FGrid: TdxGrid;
    FTree: TdxComponent;
    FId: Integer;
    FIndex: Integer;
    FLevelCount: Integer;
    FLockMode: TLockMode;
    FOnAfterCancel: TNotifyEvent;
    FOnAfterClose: TNotifyEvent;
    FOnAfterDelete: TNotifyEvent;
    FOnAfterDuplicate: TNotifyEvent;
    FOnAfterEdit: TNotifyEvent;
    FOnAfterInsert: TNotifyEvent;
    FOnAfterOpen: TNotifyEvent;
    FOnAfterPost: TNotifyEvent;
    FOnAfterScroll: TNotifyEvent;
    FOnBeforeCancel: TNotifyEvent;
    FOnBeforeClose: TNotifyEvent;
    FOnBeforeDelete: TNotifyEvent;
    FOnBeforeDuplicate: TNotifyEvent;
    FOnBeforeEdit: TNotifyEvent;
    FOnBeforeInsert: TNotifyEvent;
    FOnBeforeOpen: TNotifyEvent;
    FOnBeforePost: TNotifyEvent;
    FOnBeforeScroll: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnFieldChange: TFieldChangeEvent;
    FOnMsgButtonClick: TMsgButtonClickEvent;
    FOnPrint: TPrintEvent;
    FOnShowForm: TNotifyEvent;
    FOnStateChange: TNotifyEvent;
    FOnValidate: TValidateEvent;
    FParams: TParamList;
    FParentField: Integer;
    FPId: Integer;
    FRecordCaption: String;
    FRecordsCaption: String;
    FRS: TObject;
    FTemplates: TStrings;
    FUseSelectCondition: Boolean;
    FViewType: TViewType;
    //FScrollEventsCounter: Integer;
    //FOldBeforeScroll, FOldAfterScroll: TDataSetNotifyEvent;
    function GetAsDT(AIndex: String): TDateTime;
    function GetAsF(AIndex: String): Double;
    function GetAsI(AIndex: String): Integer;
    function GetAsS(AIndex: String): String;
    function GetCustomFilterForm: TdxForm;
    function GetField(AIndex: String): TField;
    //function GetFields(AIndex: String): Variant;
    function GetFiles(AIndex: String): TdxFile;
    function GetFormByIndex(AIndex: Integer): TdxForm;
    function GetFormCount: Integer;
    function GetFormFields(AIndex: String): Variant;
    function GetForms(AIndex: String): TdxForm;
    function GetImages(AIndex: String): TdxDBImage;
    function GetModified: Boolean;
    function GetOldValues(AIndex: String): Variant;
    function GetParentForm: TdxForm;
    function GetQueries(AIndex: String): TdxQueryGrid;
    function GetQueryByIndex(AIndex: Integer): TdxQueryGrid;
    function GetQueryCount: Integer;
    function GetState: TDataSetState;
    procedure SetCustomFilterForm(AValue: TdxForm);
    procedure SetFields(AIndex: String; AValue: Variant);
    //function GetGrid: TdxGrid;
  public
    constructor Create(AOwner: TdxComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure GetTabOrderList(L: TList);
    //procedure GetFocusControls(L: TList);
    procedure GetFields(L: TList);
    procedure GetGrids(L: TList);
    procedure GetTables(L: TList);
    procedure GetQueries(L: TList);
    procedure GetOrderedFields(L: TList);
    procedure GetFields(SL: TStrings); overload;
    procedure GetCalcFields(L: TList);
    procedure GetCalcLabels(L: TList);
    procedure GetPivots(L: TList);
    procedure ExtractFormGrid;

    function FindField(aId: Integer): TdxField;
    function FindTable(aId: Integer): TdxGrid;
    function FindFieldByName(const S: String): TdxField;
    function FindLabelByName(const S: String; All: Boolean = False): TdxLabel;
    function FindQuery(aId: Integer): TdxQueryGrid;
    function FindPivotByName(const aName: String): TObject;
    function GetFormParentFieldFieldId: Integer;
    function FindComponent(const S: String): TdxComponent;
    function GetRecordsCaption: String;
    function GetRecordCaption: String;
    procedure RequeryIfNeed;
    procedure DoPrintEvent(Action: TPrintActionType; const SourceName,
      FieldName: String; var Value: String; var Accept: Boolean);

    property Id: Integer read FId write FId;
    property PId: Integer read FPId write FPId;
    property FormCaption: String read FFormCaption write FFormCaption;
    property RecordsCaption: String read FRecordsCaption write FRecordsCaption;
    property RecordCaption: String read FRecordCaption write FRecordCaption;
    property CalcFields: TStrings read FCalcFields;
    property Templates: TStrings read FTemplates;
    property Filters: TStrings read FFilters;
    property Coloring: TColoringList read FColoring;
    property ParentField: Integer read FParentField write FParentField;
    property LevelCount: Integer read FLevelCount write FLevelCount;
    property ViewType: TViewType read FViewType write FViewType;
    property Grid: TdxGrid read FGrid;
    property AutoOpen: Boolean read FAutoOpen write FAutoOpen;
    property Index: Integer read FIndex write FIndex;
    property ActionResult: Variant read FActionResult write FActionResult;
    property CustomFilter: String read FCustomFilter write FCustomFilter;
    property CustomFilterRS: TObject read FCustomFilterRS write FCustomFilterRS;
    property ConfirmSaveRecord: Boolean read FConfirmSaveRecord write FConfirmSaveRecord;
    property ConfirmCancelEditing: Boolean read FConfirmCancelEditing write FConfirmCancelEditing;
    property ActionOnCreate: String read FActionOnCreate write FActionOnCreate;

    property RecordSet: TObject read FRS write FRS;
    property Errs: TStringList read FErrs;
  public
    function Append: TAccessStatus;
    function Insert: TAccessStatus;
    function Edit: TAccessStatus;
    function Delete: TAccessStatus;
    procedure Post;
    procedure Cancel;
    procedure Refresh;
    procedure MoveFirst;
    procedure MovePrior;
    procedure MoveNext;
    procedure MoveLast;
    procedure MoveBy(Distance: Integer);
    procedure MoveTo(ARecNo: Integer);
    function BOF: Boolean;
    function EOF: Boolean;
    function RecNo: Integer;
    function RecId: Integer;
    function RecordCount: Integer;
    function Print(const TemplateName, OutFileName: String; out AErrs: String; aOpenFile: Boolean): String;
    function Locate(const FieldNames: String; FieldValues: array of Variant; Options: TLocateOptions): Boolean;
    function GotoRecord(aRecId: Integer): Boolean;
    function CanAppend: TAccessStatus;
    function CanEdit: TAccessStatus;
    function CanDelete: TAccessStatus;
    procedure Open;
    procedure OpenRecord(ARecId: Integer);
    procedure OpenRecords(const AFilter: String; Fm: TdxForm; SelCond: Boolean);
    function Opened: Boolean;
    procedure Close;
    function Validate: Boolean;
    function FindComponentByFieldName(const FieldName: String): TdxComponent;
    procedure DisableScrollEvents;
    procedure EnableScrollEvents;
    function ScrollEventsDisabled: Boolean;
    function WhoEdit(ARecId: Integer): String;
    procedure GotoForm(const AFormName: String; ARecId: Integer; AGotoOption: TGotoOption);
    procedure GotoReport(const AReportName: String; AGotoOption: TGotoOption);
    procedure GotoUrl(const Url: String; AGotoOption: TGotoOption);
    procedure MessageDlg(const Title, Msg: String; MsgType: TMsgDlgType; Buttons: TMsgDlgButtons; ClickHandler: TMsgButtonClickEvent);
    procedure MsgBox(const Title, Msg: String);

    property Fields[AIndex: String]: Variant read GetFormFields write SetFields;
    property Field[AIndex: String]: TField read GetField;
    property AsI[AIndex: String]: Integer read GetAsI;
    property AsF[AIndex: String]: Double read GetAsF;
    property AsDT[AIndex: String]: TDateTime read GetAsDT;
    property AsS[AIndex: String]: String read GetAsS;
    property OldValues[AIndex: String]: Variant read GetOldValues;
    property State: TDataSetState read GetState;
    property Forms[AIndex: String]: TdxForm read GetForms;
    property FormByIndex[AIndex: Integer]: TdxForm read GetFormByIndex;
    property FormCount: Integer read GetFormCount;
    property Params: TParamList read FParams;
    property ParentForm: TdxForm read GetParentForm;
    property Modified: Boolean read GetModified;
    property CustomFilterForm: TdxForm read GetCustomFilterForm write SetCustomFilterForm;
    property UseSelectCondition: Boolean read FUseSelectCondition write FUseSelectCondition;
    property Images[AIndex: String]: TdxDBImage read GetImages;
    property Files[AIndex: String]: TdxFile read GetFiles;
    property LockMode: TLockMode read FLockMode write FLockMode;
    property Filter: TFilterObject read FFilter;
    property Queries[AIndex: String]: TdxQueryGrid read GetQueries;
    property QueryByIndex[AIndex: Integer]: TdxQueryGrid read GetQueryByIndex;
    property QueryCount: Integer read GetQueryCount;

    property OnAfterCancel: TNotifyEvent read FOnAfterCancel write FOnAfterCancel;
    property OnAfterClose: TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnAfterDelete: TNotifyEvent read FOnAfterDelete write FOnAfterDelete;
    property OnAfterEdit: TNotifyEvent read FOnAfterEdit write FOnAfterEdit;
    property OnAfterInsert: TNotifyEvent read FOnAfterInsert write FOnAfterInsert;
    property OnAfterOpen: TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnAfterPost: TNotifyEvent read FOnAfterPost write FOnAfterPost;
    property OnAfterScroll: TNotifyEvent read FOnAfterScroll write FOnAfterScroll;
    property OnAfterDuplicate: TNotifyEvent read FOnAfterDuplicate write FOnAfterDuplicate;

    property OnBeforeCancel: TNotifyEvent read FOnBeforeCancel write FOnBeforeCancel;
    property OnBeforeClose: TNotifyEvent read FOnBeforeClose write FOnBeforeClose;
    property OnBeforeDelete: TNotifyEvent read FOnBeforeDelete write FOnBeforeDelete;
    property OnBeforeEdit: TNotifyEvent read FOnBeforeEdit write FOnBeforeEdit;
    property OnBeforeInsert: TNotifyEvent read FOnBeforeInsert write FOnBeforeInsert;
    property OnBeforeOpen: TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnBeforePost: TNotifyEvent read FOnBeforePost write FOnBeforePost;
    property OnBeforeScroll: TNotifyEvent read FOnBeforeScroll write FOnBeforeScroll;
    property OnBeforeDuplicate: TNotifyEvent read FOnBeforeDuplicate write FOnBeforeDuplicate;

    property OnValidate: TValidateEvent read FOnValidate write FOnValidate;
    property OnFieldChange: TFieldChangeEvent read FOnFieldChange write FOnFieldChange;
		property OnPrint: TPrintEvent read FOnPrint write FOnPrint;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnMsgButtonClick: TMsgButtonClickEvent read FOnMsgButtonClick write FOnMsgButtonClick;
    property OnShowForm: TNotifyEvent read FOnShowForm write FOnShowForm;
  end;

  { TdxTimeEdit }

  TdxTimeFormat = (ttHH, ttHHMM, ttHHMMSS);

  TdxTimeEdit = class(TdxField)
  private
    FCheckExpression: String;
    FCurTime: Boolean;
    FDefaultValue: String;
    FEditable: Boolean;
    FExpression: String;
    FHideButton: Boolean;
    FRequired: Boolean;
    FTimeFormat: TdxTimeFormat;
  public
    procedure Assign(Source: TPersistent); override;
    function TimeFormatStr: String;
    property CurTime: Boolean read FCurTime write FCurTime;
    property TimeFormat: TdxTimeFormat read FTimeFormat write FTimeFormat;
    property Expression: String read FExpression write FExpression;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
    property Required: Boolean read FRequired write FRequired;
    property Editable: Boolean read FEditable write FEditable;
    property HideButton: Boolean read FHideButton write FHideButton;
  end;

  { TdxCounter }

  TdxCounter = class(TdxField)
  private
    FCheckExpression: String;
    FReadOnly: Boolean;
    FRequired: Boolean;
  public
    procedure Assign(Source: TPersistent); override;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Required: Boolean read FRequired write FRequired;
    property CheckExpression: String read FCheckExpression write FCheckExpression;
  end;

  { TdxButton }

  TdxButton = class(TdxControl)
  private
    FActionOnClick: String;
    FHasGlyph: Boolean;
    FImageName: String;
    FOnClick: TNotifyEvent;
  public
    procedure Assign(Source: TPersistent); override;
    property ImageName: String read FImageName write FImageName;
    property HasGlyph: Boolean read FHasGlyph write FHasGlyph;
    property ActionOnClick: String read FActionOnClick write FActionOnClick;
  public
    function Click: Boolean;
    procedure SetClickHandler(ClickHandler: TNotifyEvent);
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TdxChart = class(TdxWinControl)
  end;

  TdxRecordId = class(TdxField)

  end;

  //TdxBarSeries = class(TdxComponent)
  //end;

procedure CreateThumbnail(const FileName: String; Image: TdxDBImage; DS: TDataSet);
function LoadImageFromFile(const FileName: String; Image: TdxDBImage; DS: TDataSet): String;
function SaveImageToStream(Dest: TStream; Image: TdxDBImage; DS: TDataSet): Boolean;
function SaveImageToFile(const FileName: String; Image: TdxDBImage; DS: TDataSet): Boolean;
function SaveImageToFile(const FileName: String; MaxSize: Integer; Image: TdxDBImage; DS: TDataSet): Boolean;
function GetImageStream(Image: TdxDBImage; DS: TDataSet): TStream;
function GetImageFileName(Image: TdxDBImage; DS: TDataSet): String;
function GetImageFileExt(Image: TdxDBImage; DS: TDataSet): String;
function SaveThumbnailToFile(const FileName: String; FieldId: Integer; DS: TDataSet): Boolean;
procedure SaveImageToFileConvert(const FileName: String; Image: TdxDBImage; DS: TDataSet);
procedure GetImageSize(const FileName: String; var Size: TPoint);
procedure CreateBlankImage(const FileName: String);
procedure ShapeToFile(Cmp: TdxShape; const FileName: String);

function LoadFileFromFile(const FileName: String; aFile: TdxFile; DS: TDataSet): String;
function SaveFileToFile(const FileName: String; aFile: TdxFile; DS: TDataSet): String;
function SaveFileToStream(Dest: TStream; aFile: TdxFile; DS: TDataSet): String;
function GetFileStream(aFile: TdxFile; DS: TDataSet): TStream;
function GetFileFileName(aFile: TdxFile; DS: TDataSet): String;
function GetExpression(C: TdxComponent): String;
function GetEditable(C: TdxComponent): Boolean;
//function IsAcceptFilter(F: TdxField): Boolean;
function GetPrecStr(F: TdxField): String;
function GetRequired(F: TdxField): Boolean;
function GetDefaultValue(C: TdxComponent): String;
function GetCheckExpression(C: TdxComponent): String;
function GetSourceTId(C: TdxComponent): Integer;
function GetSourceFId(C: TdxComponent): Integer;
function GetFieldSize(C: TdxComponent): Integer;
function GetComboFilter(C: TdxField): String;

implementation

uses
  FPReadJPEG, BGRAReadJPEG, LazFileUtils, apputils, BGRAThumbnail, Variants,
  LazUtf8, FileUtil, dxtypes, pivotgrid, scriptfuncs, expressions,
  xmlreport, dxreports, dxactions, Math;

function CopyToStorageFolder(const Src, Dir, Dest: String): String;
begin
  Result := '';
  if not ForceDirectories(Dir) then
    Exit(Format(rsCouldNotCreateFolder, [Dir]));
  if not CopyFile(Src, Dir + Dest) then
    Exit(Format(rsCantCopyFileToStorageFolder, [Src, Dir + Dest]));
end;

{procedure LoadImageFromStream(aBitmap: TBGRAWinBitmap; aQuality: Integer; St: TStream);
var
  fmt: TBGRAImageFormat;
  reader: TFPCustomImageReader;
begin
  fmt := DetectFileFormat(St);
  if fmt = ifJpeg then
  begin
    reader := TBGRAReaderJpeg.Create;
    with TBGRAReaderJpeg(reader) do
    begin
      if aQuality in [0..3] then
        Scale:=TJpegScale(aQuality);
      Performance:=jpBestSpeed;
    end;
  end
  else
    reader := CreateBGRAImageReader(fmt);
  try
    aBitmap.LoadFromStream(St,reader);
  finally
    reader.Free;
  end;
end; }

function SaveImageToFile(const FileName: String; MaxSize: Integer;
  Image: TdxDBImage; DS: TDataSet): Boolean;
var
  St: TStream;
  FBmp, FBmp2: TBGRABitmap;
  w, h: Integer;
  e: Double;
begin
  Result := False;
  St := GetImageStream(Image, DS);
  if St = nil then Exit;
  FBmp := TBGRABitmap.Create(0, 0);
  FBmp2 := TBGRABitmap.Create(0, 0);
  try
    FBmp.LoadFromStream(St);
    w := FBmp.Width;
    h := FBmp.Height;
    if (w > MaxSize) or (h > MaxSize) then
    begin
      e := w / h;
      if e >= 1 then
      begin
        w := MaxSize; h := Round(MaxSize / e);
      end else
      begin
        h := MaxSize; w := Round(MaxSize * e);
      end;
    end;
    FBmp2.SetSize(w, h);
    FBmp2.StretchPutImage(Rect(0, 0, w, h), FBmp, dmSet);
    FBmp2.SaveToFileUTF8(FileName);
    Result := True;
  finally
    FBmp2.Free;
    FBmp.Free;
    St.Free;
  end;
end;

function GetImageStream(Image: TdxDBImage; DS: TDataSet): TStream;
var
  FlNm, FileName: String;
  Tp: Integer;
begin
  Result := nil;
  FlNm := fieldStr(Image.Id);
  Tp := Image.StorageType;
  //if (Tp = StorageTypeDB) and (DS.State in [dsInsert, dsEdit]) then Tp := StorageTypeLink;
  case Tp of
    StorageTypeDb:
      begin
        if Image.IsQuery then FlNm := FlNm + 'data';
        Result := DS.CreateBlobStream(DS.FieldByName(FlNm), bmRead);
      end;
    StorageTypeFolder:
      begin
        FileName := GetAbsolutePath(Image.StorageFolder) + DS.FieldByName(FlNm + 'dest').AsString;
        if FileExistsUtf8(FileName) then
          Result := TFileStream.Create(Utf8ToSys(FileName), fmOpenRead + fmShareDenyNone);
      end;
    StorageTypeLink:
      begin
        if not Image.IsQuery then FlNm := FlNm + 'src';
        FileName := DS.FieldByName(FlNm).AsString;
        if FileExistsUtf8(FileName) then
          Result := TFileStream.Create(Utf8ToSys(FileName), fmOpenRead + fmShareDenyNone);
      end;
  end;
  if (Result <> nil) and (Result.Size = 0) then FreeAndNil(Result);
end;

function GetImageFileName(Image: TdxDBImage; DS: TDataSet): String;
begin
  Result := '';
  case Image.StorageType of
    StorageTypeDB, StorageTypeLink:
      Result := DS.FieldByName(FieldStr(Image.Id) + 'src').AsString;
    StorageTypeFolder:
      if Image.StorageFolder <> '' then
        Result := GetAbsolutePath(Image.StorageFolder) +
          DS.FieldByName(FieldStr(Image.Id) + 'dest').AsString;
  end;
end;

function GetImageFileExt(Image: TdxDBImage; DS: TDataSet): String;
begin
  Result := ExtractFileExt(GetImageFileName(Image, DS));
end;

function SaveThumbnailToFile(const FileName: String; FieldId: Integer;
  DS: TDataSet): Boolean;
var
  FlNm: String;
  St: TStream;
begin
  Result := False;
  FlNm := FieldStr(FieldId) + 'thumb';
  if DS.FieldByName(FlNm).IsNull then Exit;

  St := DS.CreateBlobStream(DS.FieldByName(FlNm), bmRead);
  with TFileStream.Create(Utf8ToSys(FileName), fmCreate) do
  try
    CopyFrom(St, St.Size);
    Result := True;
  finally
    Free;
    St.Free;
  end;
end;

procedure SaveImageToFileConvert(const FileName: String; Image: TdxDBImage;
  DS: TDataSet);
var
  St: TStream;
  Bmp: TBGRABitmap;
begin
  St := GetImageStream(Image, DS);
  if St = nil then Exit;
  try
    Bmp := nil;
    Bmp := TBGRABitmap.Create(St);
    Bmp.SaveToFileUTF8(FileName);
  finally
    St.Free;
    FreeAndNil(Bmp);
  end;
end;

procedure GetImageSize(const FileName: String; var Size: TPoint);
var
  FBmp: TBGRABitmap;
begin
  Size.x := 0;
  Size.y := 0;
  FBmp := TBGRABitmap.Create(0, 0);
  try try
    FBmp.LoadFromFile(FileName);
    Size.x := FBmp.Width;
    Size.y := FBmp.Height;
  except
    ;
  end;

  finally
    FBmp.Free;
  end;
end;

procedure CreateBlankImage(const FileName: String);
var
  Bmp: TBGRABitmap;
begin
  Bmp := TBGRABitmap.Create(2, 2);
  try
    Bmp.SaveToFile(Utf8ToSys(FileName));
  finally
    Bmp.Free;
  end;
end;

// Angle of 5-angled star is function(N=0..9, Down) = pi/5 * N + pi/2 * IfThen(Down, -1, 1);
const
  CosStarBig: array[0..4, Boolean] of Single = (
    (Cos(       + pi/2), Cos(       - pi/2)),
    (Cos(2*pi/5 + pi/2), Cos(2*pi/5 - pi/2)),
    (Cos(4*pi/5 + pi/2), Cos(4*pi/5 - pi/2)),
    (Cos(6*pi/5 + pi/2), Cos(6*pi/5 - pi/2)),
    (Cos(8*pi/5 + pi/2), Cos(8*pi/5 - pi/2))
    );
  SinStarBig: array[0..4, Boolean] of Single = (
    (Sin(       + pi/2), Sin(       - pi/2)),
    (Sin(2*pi/5 + pi/2), Sin(2*pi/5 - pi/2)),
    (Sin(4*pi/5 + pi/2), Sin(4*pi/5 - pi/2)),
    (Sin(6*pi/5 + pi/2), Sin(6*pi/5 - pi/2)),
    (Sin(8*pi/5 + pi/2), Sin(8*pi/5 - pi/2))
    );
  CosStarSmall: array[0..4, Boolean] of Single = (
    (Cos(  pi/5 + pi/2), Cos(  pi/5 - pi/2)),
    (Cos(3*pi/5 + pi/2), Cos(3*pi/5 - pi/2)),
    (Cos(5*pi/5 + pi/2), Cos(5*pi/5 - pi/2)),
    (Cos(7*pi/5 + pi/2), Cos(7*pi/5 - pi/2)),
    (Cos(9*pi/5 + pi/2), Cos(9*pi/5 - pi/2))
    );
  SinStarSmall: array[0..4, Boolean] of Single = (
    (Sin(  pi/5 + pi/2), Sin(  pi/5 - pi/2)),
    (Sin(3*pi/5 + pi/2), Sin(3*pi/5 - pi/2)),
    (Sin(5*pi/5 + pi/2), Sin(5*pi/5 - pi/2)),
    (Sin(7*pi/5 + pi/2), Sin(7*pi/5 - pi/2)),
    (Sin(9*pi/5 + pi/2), Sin(9*pi/5 - pi/2))
    );

procedure ShapeToFile(Cmp: TdxShape; const FileName: String);
const
  cStarError = 2; // Detect N pixels error for 5-star horizontal lines
var
  PaintRect: TRect;
  MinSize: Longint;
  P: array of TPoint;
  PCenter: TPoint;
  PenInc, PenDec, i, x, y: Integer;
  Bmp: TBGRABitmap;
  RadiusBig, RadiusBig2, RadiusSm: Int64;
begin
  Bmp := TBGRABitmap.Create(Cmp.Width, Cmp.Height);
  try try

  Bmp.FillTransparent;
  with Bmp.CanvasBGRA do
  begin
    Pen.Assign(Cmp.Pen);
    Brush.Assign(Cmp.Brush);

    PenInc := Pen.Width div 2;
    PenDec := (Pen.Width - 1) div 2;

    PaintRect := Rect(PenInc, PenInc, Cmp.Width - PenDec, Cmp.Height - PenDec);
    if PaintRect.Left = PaintRect.Right then
      PaintRect.Right := PaintRect.Right + 1;
    if PaintRect.Top = PaintRect.Bottom then
      PaintRect.Bottom := PaintRect.Bottom + 1;

    with PaintRect do
    begin
      MinSize := Min(Right - Left, Bottom - Top);
      if Cmp.Shape in [stSquare, stRoundSquare, stCircle, stSquaredDiamond] then
      begin
        Left := Left + ((Right - Left) - MinSize) div 2;
        Top := Top + ((Bottom - Top) - MinSize) div 2;
        Right := Left + MinSize;
        Bottom := Top + MinSize;
      end;
    end;

    if Cmp.ShapeEx = steNone then
      case Cmp.Shape of
        stRectangle, stSquare:
          Rectangle(PaintRect);
        stRoundRect, stRoundSquare:
          RoundRect(PaintRect, MinSize div 4, MinSize div 4);
        stCircle, stEllipse:
          Ellipse(PaintRect);
        stSquaredDiamond, stDiamond:
        begin
          with PaintRect do
          begin
            SetLength(P, 4);
            P[0].x := Left;
            P[0].y := (Top + Bottom) div 2;
            P[1].x := (Left + Right) div 2;
            P[1].y := Top;
            P[2].x := Right - 1;
            P[2].y := P[0].y;
            P[3].x := P[1].x;
            P[3].y := Bottom - 1;
            Polygon(P);
          end;
        end;
        stTriangle:
        begin
          SetLength(P, 3);
          P[0].x := (Width - 1) div 2;
          P[0].y := PenInc;
          P[1].x := Width - PenInc - 1;
          P[1].y := Height - PenInc - 1;
          P[2].x := PenInc;
          P[2].y := Height - PenInc - 1;
          Polygon(P);
        end;
        stTriangleDown:
        begin
          SetLength(P, 3);
          P[0].x := (Width - 1) div 2;
          P[0].y := Height - PenInc - 1;
          P[1].x := Width - PenInc - 1;
          P[1].y := PenInc;
          P[2].x := PenInc;
          P[2].y := PenInc;
          Polygon(P);
        end;
        stTriangleLeft:
        begin
          SetLength(P, 3);
          P[0].x := PenInc;
          P[0].y := Height div 2;
          P[1].x := Width - PenInc - 1;
          P[1].y := PenInc;
          P[2].x := Width - PenInc - 1;
          P[2].y := Height - PenInc - 1;
          Polygon(P);
        end;
        stTriangleRight:
        begin
          SetLength(P, 3);
          P[0].x := Width - PenInc - 1;
          P[0].y := Height div 2;
          P[1].x := PenInc;
          P[1].y := PenInc;
          P[2].x := PenInc;
          P[2].y := Height - PenInc - 1;
          Polygon(P);
        end;
        stStar, stStarDown:
        begin
          //radius if star scaled by height
          RadiusBig := Trunc((Height-Pen.Width) / (1+cos(pi/5)));
          //radius if star scaled by width
          RadiusBig2 := Trunc((Width-Pen.Width) / (2*sin(pi*2/5)));

          if RadiusBig<=RadiusBig2 then
          begin
            if Cmp.Shape=stStar then
              PCenter.Y := RadiusBig+PenDec
            else
              PCenter.Y := Height-RadiusBig-PenDec;
          end
          else begin
            RadiusBig := RadiusBig2;
            PCenter.Y := Height div 2;
          end;

          PCenter.X := Width div 2;
          RadiusSm := RadiusBig * 57 div 150;

          SetLength(P, 10);
          for i := 0 to 4 do
          begin
            P[i*2].x := PCenter.X + Round(RadiusBig*CosStarBig[i, Cmp.Shape=stStarDown]);
            P[i*2].y := PCenter.Y - Round(RadiusBig*SinStarBig[i, Cmp.Shape=stStarDown]);
            P[i*2+1].x := PCenter.X + Round(RadiusSm*CosStarSmall[i, Cmp.Shape=stStarDown]);
            P[i*2+1].y := PCenter.Y - Round(RadiusSm*SinStarSmall[i, Cmp.Shape=stStarDown]);
          end;

          // Fix 1 pixel error of horizontal lines, adjust point on small radius to the point on big one
          for i := 0 to 4 do
            if Abs(P[i*2].y - P[i*2+1].y) <= cStarError then
              P[i*2+1].y := P[i*2].y;
          for i := 1 to 4 do
            if Abs(P[i*2].y - P[i*2-1].y) <= cStarError then
              P[i*2-1].y := P[i*2].y;

          Polygon(P);
        end;
      end
    else
      case Cmp.ShapeEx of
        steVertLine:
          begin
            x := Width div 2;
            Line(x, 0, x, Height);
          end;
        steHorzLine:
          begin
            y := Height div 2;
            Line(0, y, Width, y);
          end;
        steBDiagonal:
          begin
            Line(0, Height, Width, 0);
          end;
        steFDiagonal:
          begin
            Line(0, 0, Width, Height);
          end;
        steCross:
          begin
            x := Width div 2;
            Line(x, 0, x, Height);
            y := Height div 2;
            Line(0, y, Width, y);
          end;
        steDiagCross:
          begin
            Line(0, Height, Width, 0);
            Line(0, 0, Width, Height);
          end;
      end;
  end;
  Bmp.SaveToFile(FileName);

  except
    ;
  end;
  finally
    Bmp.Free;
  end;
end;

(*procedure CreateThumbnail(const FileName: String; Image: TdxDBImage; DS: TDataSet);
var
  Bmp: TBGRABitmap;
  St: TMemoryStream;
begin
  if Image.ThumbSize <= 0 then
  begin
    DS.FieldByName(FieldStr(Image.Id) + 'thumb').SetData(nil);
    Exit;
  end;
  Bmp := GetFileThumbnail(FileName, Image.ThumbSize, Image.ThumbSize, BGRAWhite, False, nil);
  if Bmp = nil then Exit;
  St := TMemoryStream.Create;
  try
    Bmp.SaveToStreamAsPng(St);
    TBlobField(DS.FieldByName(FieldStr(Image.Id) + 'thumb')).LoadFromStream(St);
  finally
    St.Free;
    Bmp.Free;
  end;
end;

procedure LoadImageFromFile(const FileName: String; Image: TdxDBImage; DS: TDataSet);
var
  FS: TFileStream;
  S, FNm: String;
begin
  FNm := FieldStr(Image.Id);
  DS.FieldByName(FNm + 'src').AsString := FileName;
  if Image.StorageType = StorageTypeDB then
  begin
    FS := TFileStream.Create(Utf8ToSys(FileName), fmOpenRead + fmShareDenyWrite);
    try
      TBlobField(DS.FieldByName(FNm)).LoadFromStream(FS);
    finally
      FS.Free;
    end;
  end
  else if (Image.StorageType = StorageTypeFolder) and (Image.StorageFolder <> '') then
  begin
    S := CopyToStorageFolder(FileName, GetAbsolutePath(Image.StorageFolder));
    DS.FieldByName(FNm + 'dest').AsString := S;
  end;
  CreateThumbnail(FileName, Image, DS);
end;

function SaveImageToFile(const FileName: String; Image: TdxDBImage; DS: TDataSet
  ): Boolean;
var
  St: TStream;
  FS: TFileStream;
begin
  Result := False;
  FS := nil; St := nil;
  try
    St := GetImageStream(Image, DS);
    if (St <> nil) and (St.Size > 0) then
    begin
      FS := TFileStream.Create(Utf8ToSys(FileName), fmCreate);
      FS.CopyFrom(St, St.Size);
      Result := True;
    end;
  finally
    FreeAndNil(St);
    FreeAndNil(FS);
  end;
end;   *)

procedure CreateThumbnail(const FileName: String; Image: TdxDBImage; DS: TDataSet);
var
  Bmp: TBGRABitmap;
  St: TMemoryStream;
begin
  if Image.ThumbSize <= 0 then
  begin
    DS.FieldByName(FieldStr(Image.Id) + 'thumb').SetData(nil);
    Exit;
  end;
  Bmp := GetFileThumbnail(FileName, Image.ThumbSize, Image.ThumbSize, BGRAWhite, False, nil);
  if Bmp = nil then Exit;
  St := TMemoryStream.Create;
  try
    Bmp.SaveToStreamAsPng(St);
    TBlobField(DS.FieldByName(FieldStr(Image.Id) + 'thumb')).LoadFromStream(St);
  finally
    St.Free;
    Bmp.Free;
  end;
end;

function LoadImageFromFile(const FileName: String; Image: TdxDBImage;
  DS: TDataSet): String;
var
  FS: TFileStream;
  S, FNm, ErrStr: String;
begin
  Result := '';
  FNm := FieldStr(Image.Id);
  try

  if Image.StorageType = StorageTypeDB then
  begin
    FS := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
    try
      TBlobField(DS.FieldByName(FNm)).LoadFromStream(FS);
      DS.FieldByName(FNm + 'dest').SetData(nil);
    finally
      FS.Free;
    end;
  end
  else if (Image.StorageType = StorageTypeFolder) and (Image.StorageFolder <> '') then
  begin
    S := GetUniqueFileName(DS.Fields[0].AsInteger, Image.Id, ExtractFileName(FileName));
    ErrStr := CopyToStorageFolder(FileName, GetAbsolutePath(Image.StorageFolder), S);
    if ErrStr = '' then
      DS.FieldByName(FNm + 'dest').AsString := Utf8Copy(S, 1, 150)
    else Exit(ErrStr);
  end;
  CreateThumbnail(FileName, Image, DS);
  DS.FieldByName(FNm + 'src').AsString := Utf8Copy(FileName, 1, 255);
  if Image.StorageType <> StorageTypeDB then
    DS.FieldByName(FNm).SetData(nil);
  // Просто меняем значение поля для определения, что blob был изменен.
  DS.FieldByName(FNm + 'c').AsInteger:=DS.FieldByName(FNm + 'c').AsInteger+1;

  except
    on E: Exception do
    	Result := E.Message;
      //ErrMsg(E.Message);
  end;
end;

function SaveImageToStream(Dest: TStream; Image: TdxDBImage; DS: TDataSet
  ): Boolean;
var
  St: TStream;
begin
  St := nil;
  try
    St := GetImageStream(Image, DS);
    Result := St <> nil;
    if Result then
      Dest.CopyFrom(St, St.Size);
  finally
    FreeAndNil(St);
  end;
end;

function SaveImageToFile(const FileName: String; Image: TdxDBImage; DS: TDataSet
  ): Boolean;
var
  St: TStream;
  FS: TFileStream;
begin
  FS := nil; St := nil;
  try
    FS := TFileStream.Create(FileName, fmCreate);
    St := GetImageStream(Image, DS);
    Result := St <> nil;
    if Result then
      FS.CopyFrom(St, St.Size);
  finally
    FreeAndNil(St);
    FreeAndNil(FS);
  end;
end;


////////////////////////////////////////////////////////////////////////////////

function LoadFileFromFile(const FileName: String; aFile: TdxFile; DS: TDataSet
  ): String;
var
  FNm, S, ErrStr: String;
  FS: TFileStream;
begin
  Result := '';
  FNm := FieldStr(aFile.Id);

  try

  if aFile.StorageType = StorageTypeDB then
  begin
    FS := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
    try
      TBlobField(DS.FieldByName(FNm)).LoadFromStream(FS);
      S := ExtractFileName(FileName);
      DS.FieldByName(FNm + 'dest').SetData(nil);
    finally
      FS.Free;
    end;
  end
  else if (aFile.StorageType = StorageTypeFolder) and (aFile.StorageFolder <> '') then
  begin
    S := GetUniqueFileName(DS.Fields[0].AsInteger, aFile.Id, ExtractFileName(FileName));
    ErrStr := CopyToStorageFolder(FileName, GetAbsolutePath(aFile.StorageFolder), S);
    if ErrStr = '' then
      DS.FieldByName(FNm + 'dest').AsString := Utf8Copy(S, 1, 150)
    else
      Exit(ErrStr);
  end
  else
  	S := FileName;
  DS.FieldByName(FNm + 'd').AsString := Utf8Copy(S, 1, aFile.FieldSize);
  DS.FieldByName(FNm + 'src').AsString := Utf8Copy(FileName, 1, 255);
  if aFile.StorageType <> StorageTypeDB then
    DS.FieldByName(FNm).SetData(nil);
  // Просто меняем значение поля для определения, что blob был изменен.
  DS.FieldByName(FNm + 'c').AsInteger:=DS.FieldByName(FNm + 'c').AsInteger+1;

  except
    on E: Exception do
    	Result := E.Message;
  end;
end;

function SaveFileToFile(const FileName: String; aFile: TdxFile; DS: TDataSet
  ): String;
var
  FS: TFileStream;
  St: TStream;
begin
  Result := '';
  FS := nil; St := nil;
  try try
    FS := TFileStream.Create(FileName, fmCreate);
    St := GetFileStream(aFile, DS);
    if St <> nil then
      FS.CopyFrom(St, St.Size);
  except
    on E: Exception do
    	Result := E.Message;
  end;
  finally
    FreeAndNil(St);
    FreeAndNil(FS);
  end;
end;

function SaveFileToStream(Dest: TStream; aFile: TdxFile; DS: TDataSet): String;
var
  St: TStream;
begin
  Result := '';
  St := nil;
  try try
    St := GetFileStream(aFile, DS);
    if St <> nil then
      Dest.CopyFrom(St, St.Size);
  except
    on E: Exception do
    	Result := E.Message;
  end;
  finally
    FreeAndNil(St);
  end;
end;

function GetFileStream(aFile: TdxFile; DS: TDataSet): TStream;
var
  FlNm, FileName: String;
begin
  Result := nil;
  FlNm := fieldStr(aFile.Id);
  case aFile.StorageType of
    StorageTypeDb:
      begin
        if aFile.IsQuery then FlNm := FlNm + 'data';
        Result := DS.CreateBlobStream(DS.FieldByName(FlNm), bmRead);
      end;
    StorageTypeFolder:
      begin
        FileName := GetAbsolutePath(aFile.StorageFolder) + DS.FieldByName(FlNm + 'dest').AsString;
        if FileExistsUtf8(FileName) then
          Result := TFileStream.Create(Utf8ToSys(FileName), fmOpenRead + fmShareDenyWrite);
      end;
    StorageTypeLink:
      begin
        FileName := DS.FieldByName(FlNm + 'src').AsString;
        if FileExistsUtf8(FileName) then
          Result := TFileStream.Create(Utf8ToSys(FileName), fmOpenRead + fmShareDenyNone);
      end;
  end;
  if (Result <> nil) and (Result.Size = 0) then FreeAndNil(Result);
end;

function GetFileFileName(aFile: TdxFile; DS: TDataSet): String;
var
  S: String;
begin
  Result := '';
  case aFile.StorageType of
    StorageTypeDB, StorageTypeLink:
      Result := DS.FieldByName(FieldStr(aFile.Id) + 'src').AsString;
    StorageTypeFolder:
      if aFile.StorageFolder <> '' then
      begin
        S := DS.FieldByName(FieldStr(aFile.Id) + 'dest').AsString;
        if S <> '' then
	        Result := GetAbsolutePath(aFile.StorageFolder) + S;
      end;
  end;
end;

function GetExpression(C: TdxComponent): String;
begin
  Result := '';
  if C is TdxEdit then
    Result := TdxEdit(C).Expression
  else if C is TdxCalcEdit then
    Result := TdxCalcEdit(C).Expression
  else if C is TdxDateEdit then
    Result := TdxDateEdit(C).Expression
  else if C is TdxTimeEdit then
    Result := TdxTimeEdit(C).Expression
  else if C is TdxCheckBox then
    Result := TdxCheckBox(C).Expression
  else if C is TdxMemo then
    Result := TdxMemo(C).Expression
  else if C is TdxCustomComboBox then
    Result := TdxCustomComboBox(C).Expression
  else if C is TdxLabel then
    Result := TdxLabel(C).Expression
end;

function GetEditable(C: TdxComponent): Boolean;
begin
  if C is TdxEdit then
    Result := TdxEdit(C).Editable
  else if C is TdxCalcEdit then
    Result := TdxCalcEdit(C).Editable
  else if C is TdxDateEdit then
    Result := TdxDateEdit(C).Editable
  else if C is TdxMemo then
      Result := TdxMemo(C).Editable
  else if C is TdxCheckBox then
    Result := TdxCheckBox(C).Editable
  else if C is TdxCustomComboBox then
    Result := TdxCheckBox(C).Editable
  else if C is TdxTimeEdit then
      Result := TdxTimeEdit(C).Editable
  else Result := True
end;

{function IsAcceptFilter(F: TdxField): Boolean;
begin
  Result := (F is TdxEdit) or (F is TdxCalcEdit) or (F is TdxDateEdit) or
    (F is TdxCheckBox) or (F is TdxCustomComboBox) or
    (F is TdxTimeEdit) or (F is TdxCounter) or (F is TdxMemo);
end;   }

function GetPrecStr(F: TdxField): String;
begin
  REsult := '';
  if F is TdxCalcEdit then
    Result := TdxCalcEdit(F).PrecStr
end;

function GetRequired(F: TdxField): Boolean;
begin
  Result := False;
  if F is TdxEdit then Result := TdxEdit(F).Required
  else if F is TdxCalcEdit then Result := TdxCalcEdit(F).Required
  else if F is TdxDateEdit then Result := TdxDateEdit(F).Required
  else if F is TdxTimeEdit then Result := TdxTimeEdit(F).Required
  else if F is TdxMemo then Result := TdxMemo(F).Required
  else if F is TdxCustomComboBox then Result := TdxCustomComboBox(F).Required
  else if F is TdxCounter then Result := TdxCounter(F).Required
  else if F is TdxFile then Result := TdxFile(F).Required
  else if F is TdxDBImage then Result := TdxDBImage(F).Required
end;

function GetDefaultValue(C: TdxComponent): String;
begin
  Result := '';
  if C is TdxEdit then
    Result := TdxEdit(C).DefaultValue
  else if C is TdxCalcEdit then
    Result := TdxCalcEdit(C).DefaultValue
  else if C is TdxDateEdit then
    Result := TdxDateEdit(C).DefaultValue
  else if C is TdxTimeEdit then
    Result := TdxTimeEdit(C).DefaultValue
  else if C is TdxCheckBox then
    Result := TdxCheckBox(C).DefaultValue
  else if C is TdxMemo then
    Result := TdxMemo(C).DefaultValue
  else if C is TdxCustomComboBox then
    Result := TdxCustomComboBox(C).DefaultValue
end;

function GetCheckExpression(C: TdxComponent): String;
begin
  Result := '';
  if C is TdxEdit then
    Result := TdxEdit(C).CheckExpression
  else if C is TdxCalcEdit then
    Result := TdxCalcEdit(C).CheckExpression
  else if C is TdxDateEdit then
    Result := TdxDateEdit(C).CheckExpression
  else if C is TdxTimeEdit then
    Result := TdxTimeEdit(C).CheckExpression
  else if C is TdxCheckBox then
    Result := TdxCheckBox(C).CheckExpression
  else if C is TdxMemo then
    Result := TdxMemo(C).CheckExpression
  else if C is TdxCustomComboBox then
    Result := TdxCustomComboBox(C).CheckExpression
  else if C is TdxCounter then
    Result := TdxCounter(C).CheckExpression
  else if C is TdxDBImage then
    Result := TdxDBImage(C).CheckExpression
  else if C is TdxFile then
    Result := TdxFile(C).CheckExpression
end;

function GetSourceTId(C: TdxComponent): Integer;
begin
  Result := 0;
  if C is TdxCustomComboBox then
    Result := TdxCustomComboBox(C).SourceTId
end;

function GetSourceFId(C: TdxComponent): Integer;
begin
  Result := 0;
  if C is TdxCustomComboBox then
    Result := TdxCustomComboBox(C).SourceFId;
end;

function GetFieldSize(C: TdxComponent): Integer;
begin
  Result := 0;
  if C is TdxEdit then Result := TdxEdit(C).FieldSize
  else if C is TdxMemo then Result := TdxMemo(C).FieldSize
  else if C is TdxComboBox then Result := TdxComboBox(C).FieldSize
  else if C is TdxFile then Result := TdxFile(C).FieldSize
end;

function GetComboFilter(C: TdxField): String;
begin
  if C is TdxCustomComboBox then
    Result := TdxCustomComboBox(C).Filter
  else
    Result := '';
end;

{ TColoringData }

destructor TColoringData.Destroy;
begin
  FreeAndNil(E);
  inherited Destroy;
end;

{ TComponentList }

function TComponentList.GetComponents(Index: Integer): TdxComponent;
begin
  Result := TdxComponent(Items[Index]);
end;

procedure TComponentList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Components[i].Free;
  inherited Clear;
end;

{ TdxControl }

procedure TdxControl.SetFont(AValue: TdxFont);
begin
  FFont.Assign(AValue);
end;

procedure TdxControl.SetHeight(AValue: Integer);
var
  dh: Integer;
begin
  if FHeight=AValue then Exit;
  dh := AValue - FHeight;
  FHeight:=AValue;
  ProcessResize(0, dh);
  DoChangeBounds;
  DoResize;
  DoPropertyChange('bounds');
end;

procedure TdxControl.SetLeft(AValue: Integer);
begin
  if FLeft=AValue then Exit;
  FLeft:=AValue;
  DoChangeBounds;
  DoPropertyChange('bounds');
end;

function TdxControl.GetBoundsRect: TRect;
begin
  Result := Rect(Left, Top, Left + Width, Top + Height);
end;

procedure TdxControl.DoResize;
begin
  if FOnResize <> nil then FOnResize(Self);
end;

procedure TdxControl.DoChangeBounds;
begin
  if FOnChangeBounds <> nil then FOnChangeBounds(Self);
end;

procedure TdxControl.ProcessResize(dw, dh: Integer);
var
  i: Integer;
  WC: TdxWinControl;
  C: TdxControl;
  R: TRect;
begin
  if not (Self is TdxWinControl) then Exit;

  WC := TdxWinControl(Self);
  for i := 0 to WC.ControlCount - 1 do
  begin
    C := WC.Controls[i];
    if C.Anchors = [akLeft, akTop] then Continue;
    R := C.BoundsRect;
    if akRight in C.Anchors then
    begin
      if akLeft in C.Anchors then
        R.Right := R.Right + dw
      else
        R.Offset(dw, 0);
    end;
    if akBottom in C.Anchors then
    begin
      if akTop in C.Anchors then
        R.Bottom := R.Bottom + dh
      else
        R.Offset(0, dh);
    end;
    C.BoundsRect := R;
  end;
end;

procedure TdxControl.FontChange(Sender: TObject);

  procedure _BoundsChange(C: TdxControl);
  begin
    if (C is TdxEdit) or (C is TdxCalcEdit) or (C is TdxDateEdit) or
      (C is TdxTimeEdit) or (C is TdxCounter) or (C is TdxComboBox) or
      (C is TdxLookupComboBox) or (C is TdxObjectField) or (C is TdxFile) then
    begin
      Height := Abs(Trunc(C.Font.Height * 1.32));
    end;
  end;

  {procedure _FontChange(WC: TdxWinControl);
  var
    i: Integer;
    C: TdxControl;
  begin
    for i := 0 to WC.ControlCount - 1 do
    begin
      C := WC.Controls[i];
      if C.ParentFont then C.Font := Self.Font;
      if C is TdxWinControl then
        _FontChange(TdxWinControl(C));
    end;
  end; }

var
  i: Integer;
  WC: TdxWinControl;
  C: TdxControl;
begin
  DoPropertyChange('font');
  //_BoundsChange(Self);
  FParentFont := (FParent <> nil) and FFont.IsEqual(FParent.Font);
  {if Self is TdxWinControl then
    _FontChange(TdxWinControl(Self)); }
  if not (Self is TdxWinControl) then Exit;

  WC := TdxWinControl(Self);
  for i := 0 to WC.ControlCount - 1 do
  begin
    C := WC.Controls[i];
    if C.ParentFont then C.Font := Font;
  end;
end;

procedure TdxControl.SetBoundsRect(AValue: TRect);
begin
  SetBounds(AValue.Left, AValue.Top, AValue.Width, AValue.Height);
  //DoPropertyChange('bounds');
end;

procedure TdxControl.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  DoPropertyChange('caption');
end;

procedure TdxControl.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  DoPropertyChange('color');
end;

procedure TdxControl.SetEnabled(AValue: Boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
  DoPropertyChange('enabled');
end;

procedure TdxControl.SetParent(AValue: TdxWinControl);
begin
  if FParent = AValue then Exit;
  if FParent <> nil then FParent.FControls.Remove(Self);
  FParent := AValue;
  if FParent <> nil then FParent.FControls.Add(Self);
  if ParentFont then Font := GetRealFont;
end;

procedure TdxControl.DoPropertyChange(const PropName: String);
begin
  if FOnPropertyChange <> nil then FOnPropertyChange(Self, PropName);
end;

procedure TdxControl.SetParentFont(AValue: Boolean);
begin
  if FParentFont=AValue then Exit;
  FParentFont:=AValue;
  if ParentFont then Font := GetRealFont;
end;

procedure TdxControl.SetTop(AValue: Integer);
begin
  if FTop=AValue then Exit;
  FTop:=AValue;
  DoChangeBounds;
  DoPropertyChange('bounds');
end;

procedure TdxControl.SetVisible(AValue: Boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
  DoPropertyChange('visible');
end;

procedure TdxControl.SetWidth(AValue: Integer);
var
  dw: Integer;
begin
  if FWidth=AValue then Exit;
  dw := FWidth - AValue;
  FWidth:=AValue;
  ProcessResize(dw, 0);
  DoChangeBounds;
  DoResize;
  DoPropertyChange('bounds');
end;

constructor TdxControl.Create(AOwner: TdxComponent);
begin
  inherited Create(AOwner);
  FFont := TdxFont.Create;
  FFont.OnChange:=@FontChange;
  FColor := clDefault;
  FTabStop := True;
  FParentFont := True;
  FVisible := True;
  FEnabled := True;
  FControlVisible := True;
  FAnchors := [akLeft, akTop];
end;

destructor TdxControl.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TdxControl.Assign(Source: TPersistent);
var
  S: TdxControl;
begin
  inherited Assign(Source);
  S := TdxControl(Source);
  Left := S.Left;
  Top := S.Top;
  Width := S.Width;
  Height := S.Height;
  Color := S.Color;
  Font := S.Font;
  TabOrder := S.TabOrder;
  TabStop := S.TabStop;
  ParentFont := S.ParentFont;
  Hidden := S.Hidden;
end;

procedure TdxControl.Hide;
begin
  Visible := False;
end;

procedure TdxControl.Show;
begin
  Visible := True;
end;

procedure TdxControl.SetBounds(X, Y, W, H: Integer);
var
  dw, dh: Integer;
begin
  dw := W - FWidth;
  dh := H - FHeight;
  FLeft := X; FTop := Y;
  FWidth := W; FHeight := H;
  ProcessResize(dw, dh);
  DoChangeBounds;
  if (dw <> 0) or (dh <> 0) then DoResize;
  DoPropertyChange('bounds');
end;

function TdxControl.GetRealFont: TdxFont;

  function _GetParentFont(C: TdxControl): TdxFont;
  begin
    if C.ParentFont and (C.Parent <> nil) then
      Result := _GetParentFont(TdxWinControl(C.Parent))
    else
      Result := C.Font;
  end;

begin
  Result := _GetParentFont(Self);
end;

{ TdxPageControl }

function TdxPageControl.GetPageCount: Integer;
begin
  Result := ControlCount;
end;

function TdxPageControl.GetPages(Index: Integer): TdxTabSheet;
begin
  Result := TdxTabSheet(Controls[Index]);
end;

procedure TdxPageControl.SetTabIndex(AValue: Integer);
begin
  if FTabIndex=AValue then Exit;
  FTabIndex:=AValue;
  DoPropertyChange('tabindex');
end;

procedure TdxPageControl.SetVisiblePage;
var
  i: Integer;
begin
  for i := ActivePageIndex - 1 downto 0 do
    if Pages[i].TabVisible and Pages[i].ControlVisible then
    begin
      ActivePageIndex := i;
      Exit;
    end;
  for i := ActivePageIndex + 1 to PageCount - 1 do
    if Pages[i].TabVisible and Pages[i].ControlVisible then
    begin
      ActivePageIndex := i;
      Exit;
    end;
  ActivePageIndex := -1;
end;

procedure TdxPageControl.Assign(Source: TPersistent);
var
  S: TdxPageControl;
begin
  inherited Assign(Source);
  S := TdxPageControl(Source);
  ActivePageIndex := S.ActivePageIndex;
end;

{ TdxPen }

procedure TdxPen.SetStyle(AValue: TFPPenStyle);
begin
  inherited SetStyle(AValue);
  Changed;
end;

procedure TdxPen.SetFPColor(const AValue: TFPColor);
begin
  inherited SetFPColor(AValue);
  Changed;
end;

procedure TdxPen.SetWidth(AValue: Integer);
begin
  inherited SetWidth(AValue);
  Changed;
end;

procedure TdxPen.Assign(Source: TPersistent);
var
  S: TdxPen;
begin
  S := TdxPen(Source);
  Width := S.Width;
  Style := S.Style;
  Color := S.Color;
end;

{ TdxBrush }

procedure TdxBrush.SetStyle(AValue: TFPBrushStyle);
begin
  inherited SetStyle(AValue);
  Changed;
end;

procedure TdxBrush.SetFPColor(const AValue: TFPColor);
begin
  inherited SetFPColor(AValue);
  Changed;
end;

procedure TdxBrush.Assign(Source: TPersistent);
var
  S: TdxBrush;
begin
  S := TdxBrush(Source);
  Color := S.Color;
  Style := S.Style;
end;

{ TdxFont }

function TdxFont.GetColor: TColor;
begin
  Result := {inherited }FColor;
end;

function TdxFont.GetStyle: TFontStyles;
begin
  Result := inherited Style;
end;

procedure TdxFont.SetColor(AValue: TColor);
begin
  {inherited }FColor := AValue;
  Changed;
end;

procedure TdxFont.SetStyle(AValue: TFontStyles);
begin
  inherited Style := AValue;
  Changed;
end;

procedure TdxFont.SetName(AValue: String);
begin
  if AValue <> Name then
  begin
    inherited SetName(AValue);
    Changed;
  end;
end;

procedure TdxFont.SetSize(AValue: Integer);
begin
  if AValue <> Size then
  begin
    inherited SetSize(AValue);
    Changed;
  end;
end;

constructor TdxFont.Create;
begin
  inherited Create;
  Color := clDefault;
end;

procedure TdxFont.Assign(Source: TPersistent);
var
  S: TdxFont;
begin
  S := TdxFont(Source);
  if IsEqual(S) then Exit;

  inherited Name := S.Name;
  inherited Size := S.Size;
  FColor := S.Color;
  inherited Style := S.Style;
  Changed;
end;

function TdxFont.IsDefault: Boolean;
begin
  Result := (Name = '') and (Color = {0}clDefault) and (Size = 0) and (Style = []);
end;

function TdxFont.IsEqual(F: TdxFont): Boolean;
begin
  Result := (Name = F.Name) and (Color = F.Color) and (Style = F.Style) and
    (Size = F.Size);
end;

{ TdxButton }

procedure TdxButton.Assign(Source: TPersistent);
var
  S: TdxButton;
begin
  inherited Assign(Source);
  S := TdxButton(Source);
  Caption := S.Caption;
  ImageName := S.ImageName;
  HasGlyph := S.HasGlyph;
  ActionOnClick := S.ActionOnClick;
  //ActionEnabled := S.ActionEnabled;
end;

function TdxButton.Click: Boolean;
var
  AR: TActionRunner;
  RS: TSsRecordSet;
begin
  Result := False;
  RS := TSsRecordSet(Form.RecordSet);
  if FActionOnClick <> '' then
  begin
    {if RS.Actions <> nil then
    begin
      RS.Actions.Free;
      RS.Actions := nil;
    end;}
    AR := TActionRunner.Create;
    AR.Load(FActionOnClick);
    AR.RS := RS;
    AR.Run;
    if AR.NeedContinue then RS.ActionList.Add(AR) //RS.Actions := AR
    else AR.Free;
    Result := True;
  end
  else if FOnClick <> nil then
  begin
    FOnClick(Self);
    Result := True;
  end;
end;

procedure TdxButton.SetClickHandler(ClickHandler: TNotifyEvent);
begin
  FOnClick := ClickHandler;
end;

{ TdxCounter }

procedure TdxCounter.Assign(Source: TPersistent);
var
  S: TdxCounter;
begin
  inherited Assign(Source);
  S := TdxCounter(Source);
  ReadOnly := S.ReadOnly;
  Required := S.Required;
  CheckExpression := S.CheckExpression;
end;

{ TdxTabSheet }

function TdxTabSheet.GetPageIndex: Integer;
begin
  Result := FParent.FControls.IndexOf(Self);
end;

function TdxTabSheet.GetTabVisible: Boolean;
begin
  Result := Visible;
end;

procedure TdxTabSheet.SetTabVisible(AValue: Boolean);
begin
  Visible := AValue;
  if (TdxPageControl(Parent).ActivePageIndex = PageIndex) and not Visible then
    TdxPageControl(Parent).SetVisiblePage;
end;

procedure TdxTabSheet.Assign(Source: TPersistent);
var
  S: TdxTabSheet;
begin
  inherited Assign(Source);
  S := TdxTabSheet(Source);
  Caption := S.Caption;
  ControlVisible := S.ControlVisible;
end;

{ TdxGroupBox }

procedure TdxGroupBox.Assign(Source: TPersistent);
var
  S: TdxGroupBox;
begin
  inherited Assign(Source);
  S := TdxGroupBox(Source);
  Caption := S.Caption;
end;

{ TdxQueryGrid }

function TdxQueryGrid.GetAsDT(AIndex: String): TDateTime;
begin
  Result := Nz(Fields[AIndex], 0);
end;

function TdxQueryGrid.GetAsF(AIndex: String): Double;
begin
  Result := Nz(Fields[AIndex], 0);
end;

function TdxQueryGrid.GetAsI(AIndex: String): Integer;
begin
  Result := Nz(Fields[AIndex], 0);
end;

function TdxQueryGrid.GetAsS(AIndex: String): String;
begin
  Result := Nz(Fields[AIndex], '');
end;

function TdxQueryGrid.GetEditable: Boolean;
begin
  Result := TSsRecordSet(FRS).RD.IsSimple;
end;

function TdxQueryGrid.GetQueryFields(AIndex: String): Variant;
var
  C: TRpGridColumn;
begin
  C := TSsRecordSet(FRS).RD.Grid.FindColumnByFieldName(AIndex);
  if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [AIndex]);
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).DataSet.FieldByName(C.FieldNameDS).Value;
end;

function TdxQueryGrid.GetQueryName: String;
begin
  Result := TSsRecordSet(FRS).RD.Name;
end;

procedure TdxQueryGrid.RequeryIfNeed;
begin
  with TSsRecordSet(FRS) do
    if not ManualRefresh and NeedRequery then Open;
end;

procedure TdxQueryGrid.Assign(Source: TPersistent);
var
  S: TdxQueryGrid;
begin
  inherited Assign(Source);
  S := TdxQueryGrid(Source);
  ManualRefresh := S.ManualRefresh;
end;

procedure TdxQueryGrid.Refresh;
begin
  Close;
  TSsRecordSet(FRS).Open;
end;

procedure TdxQueryGrid.Close;
begin
  TSsRecordSet(FRS).Close;
  TSsRecordSet(FRS).Parent.ChangedQueries.AddItem(FRS);
end;

procedure TdxQueryGrid.MoveFirst;
begin
  RequeryIfNeed;
  TSsRecordSet(FRS).DataSet.First;
end;

procedure TdxQueryGrid.MovePrior;
begin
  RequeryIfNeed;
  TSsRecordSet(FRS).DataSet.Prior;
end;

procedure TdxQueryGrid.MoveNext;
begin
  RequeryIfNeed;
  TSsRecordSet(FRS).DataSet.Next;
end;

procedure TdxQueryGrid.MoveLast;
begin
  RequeryIfNeed;
  TSsRecordSet(FRS).DataSet.Last;
end;

procedure TdxQueryGrid.MoveBy(Distance: Integer);
begin
  RequeryIfNeed;
  TSsRecordSet(FRS).DataSet.MoveBy(Distance);
end;

procedure TdxQueryGrid.MoveTo(ARecNo: Integer);
begin
  MoveBy(ARecNo - RecNo);
end;

function TdxQueryGrid.BOF: Boolean;
begin
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).DataSet.BOF;
end;

function TdxQueryGrid.EOF: Boolean;
begin
  Result := TSsRecordSet(FRS).DataSet.EOF;
end;

function TdxQueryGrid.RecNo: Integer;
begin
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).DataSet.RecNo;
end;

function TdxQueryGrid.RecId: Integer;
begin
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).RecId;
end;

function TdxQueryGrid.RecordCount: Integer;
begin
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).DataSet.RecordCount;
end;

function TdxQueryGrid.Locate(const FieldNames: String;
  FieldValues: array of Variant; Options: TLocateOptions): Boolean;
var
  C: TRpGridColumn;
  VArr: Variant;
  i: Integer;
  S: String;
  SL: TStringList;
  RSet: TSsRecordSet;
begin
  RequeryIfNeed;
  RSet := TSsRecordSet(FRS);
  S := '';
  SL := TStringList.Create;
  try
    SplitStr(FieldNames, ';', SL);
    for i := 0 to SL.Count - 1 do
    begin
      C := RSet.RD.Grid.FindColumnByFieldName(SL[i]);
      if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [SL[i]]);
      S := S + C.FieldNameDS;
      if i < SL.Count - 1 then S := S + ';';
    end;
    VArr := VarArrayOf(FieldValues);
    Result := RSet.DataSet.Locate(S, VArr, Options);
  finally
    SL.Free;
    VarClear(VArr);
  end;
end;

function TdxQueryGrid.GotoRecord(aRecId: Integer): Boolean;
begin
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).DataSet.Locate('id', aRecId, []);
end;

procedure TdxQueryGrid.DisableScrollEvents;
begin
  TSsRecordSet(FRS).DisableScrollEvents;
end;

procedure TdxQueryGrid.EnableScrollEvents;
begin
  TSsRecordSet(FRS).EnableScrollEvents;
end;

function TdxQueryGrid.ScrollEventsDisabled: Boolean;
begin
  Result := not TSsRecordSet(FRS).ScrollEventsEnabled;
end;

// Не работает
function TdxQueryGrid.FindColumnByTitle(const ATitle: String): TdxColumn;
var
  i: Integer;
  C: TdxColumn;
begin
  Result := nil;
  for i := 0 to Columns.Count - 1 do
  begin
    C := Columns[i];
    if MyUtf8CompareText(C.Caption, ATitle) = 0 then Exit(C);
  end;
end;

function TdxQueryGrid.GetSourceFileName(const aName: String): String;
var
  RD: TReportData;
  Tp: TRpFieldType;
  FlNm: String;
  idx: Integer;
  QRS: TSsRecordSet;
begin
  QRS := TSsRecordSet(FRS);
  RD := QRS.RD;
  idx := RD.IndexOfName(aName);
  if idx < 0 then
    raise Exception.CreateFmt(rsFieldNotFound, [aName]);
  Tp := RD.GetFieldType(idx);
  if not (Tp in [flFile, flImage]) then
    raise Exception.CreateFmt(rsFieldNotFileImage, [aName]);

  FlNm := RD.GetFieldNameDS(idx);
  if Tp = flFile then FlNm := FlNm + 'src';

  RequeryIfNeed;
  Result := QRS.DataSet.FieldByName(FlNm).AsString;
end;

function TdxQueryGrid.GetStoredFileName(const aName: String): String;
var
  RD: TReportData;
  Tp: TRpFieldType;
  FlNm: String;
  idx: Integer;
  QRS: TSsRecordSet;
begin
  QRS := TSsRecordSet(FRS);
  RD := QRS.RD;
  idx := RD.IndexOfName(aName);
  if idx < 0 then
    raise Exception.CreateFmt(rsFieldNotFound, [aName]);
  Tp := RD.GetFieldType(idx);
  if not (Tp in [flFile, flImage]) then
    raise Exception.CreateFmt(rsFieldNotFileImage, [aName]);

  FlNm := RD.GetFieldNameDS(idx) + 'dest';

  RequeryIfNeed;
  Result := QRS.DataSet.FieldByName(FlNm).AsString;
end;

procedure TdxQueryGrid.SaveBlobToStreamOrFile(const aName: String; St: TStream;
  const AFileName: String);
var
  RD: TReportData;
  Tp: TRpFieldType;
  idx: Integer;
  pF: PRpField;
  SrcImg, TmpImg: TdxDBImage;
  QRS: TSsRecordSet;
  C: TdxComponent;
  SrcFile, TmpFile: TdxFile;
begin
  QRS := TSsRecordSet(FRS);
  RD := QRS.RD;
  idx := RD.IndexOfName(aName);
  if idx < 0 then
    raise Exception.CreateFmt(rsFieldNotFound, [aName]);
  Tp := RD.GetFieldType(idx);
  if not (Tp in [flFile, flImage]) then
    raise Exception.CreateFmt(rsFieldNotFileImage, [aName]);

  pF := RD.TryGetRpField(idx);
  if pF = nil then Exit;

  C := GetRpFieldComponent(QRS.Session, pF^, True);

  RequeryIfNeed;

  if C is TdxDBImage then
  begin

    SrcImg := TdxDBImage(C);

    TmpImg := TdxDBImage.Create(nil);
    with TmpImg do
    begin
      Id := pF^.Id;
      StorageType := SrcImg.StorageType;
      StorageFolder := SrcImg.StorageFolder;
      IsQuery := True;
    end;

    try
      if St <> nil then
        SaveImageToStream(St, TmpImg, QRS.DataSet)
      else if AFileName <> '' then
        SaveImageToFile(AFileName, TmpImg, QRS.DataSet);
    finally
      TmpImg.Free;
    end;

  end
  else
  begin
    SrcFile := TdxFile(C);

    TmpFile := TdxFile.Create(nil);
    with TmpFile do
    begin
      Id := pF^.Id;
      StorageType := SrcFile.StorageType;
      StorageFolder := SrcFile.StorageFolder;
      IsQuery := True;
    end;

    try
      if St <> nil then
        SaveFileToStream(St, TmpFile, QRS.DataSet)
      else if AFileName <> '' then
        SaveFileToFile(AFileName, TmpFile, QRS.DataSet);
    finally
      TmpFile.Free;
    end;
  end;
end;

procedure TdxQueryGrid.SaveBlobToStream(const aName: String; St: TStream);
begin
  SaveBlobToStreamOrFile(aName, St, '');
end;

procedure TdxQueryGrid.SaveBlobToFile(const aName, AFileName: String);
begin
  SaveBlobToStreamOrFile(aName, nil, AFileName);
end;

procedure TdxQueryGrid.SaveThumbnailToStream(const aName: String; St: TStream);
var
  RD: TReportData;
  Tp: TRpFieldType;
  FlNm: String;
  idx: Integer;
  QRS: TSsRecordSet;
begin
  QRS := TSsRecordSet(FRS);
  RD := QRS.RD;
  idx := RD.IndexOfName(aName);
  if idx < 0 then
    raise Exception.CreateFmt(rsFieldNotFound, [aName]);
  Tp := RD.GetFieldType(idx);
  if Tp <> flImage then
    raise Exception.CreateFmt(rsFieldNotImage, [aName]);

  FlNm := RD.GetFieldNameDS(idx) + 'thumb';

  RequeryIfNeed;

  with QRS.DataSet do
    TBlobField(FieldByName(FlNm)).SaveToStream(St);
end;

{ TdxFile }

function TdxFile.GetDescription: String;
begin
  Result := TSsRecordSet(Form.RecordSet).DataSet.FieldByName(FieldStr(Id) + 'd').AsString;
end;

function TdxFile.GetSourceFileName: String;
begin
  Result := TSsRecordSet(Form.RecordSet).DataSet.FieldByName(FieldStr(Id) + 'src').AsString;
end;

function TdxFile.GetStoredFileName: String;
begin
  Result := TSsRecordSet(Form.RecordSet).DataSet.FieldByName(FieldStr(Id) + 'dest').AsString;
end;

procedure TdxFile.Assign(Source: TPersistent);
var
  S: TdxFile;
begin
  inherited Assign(Source);
  S := TdxFile(Source);
  StorageType := S.StorageType;
  StorageFolder := S.StorageFolder;
  FieldSize := S.FieldSize;
  Required := S.Required;
  CheckExpression := S.CheckExpression;
end;

procedure TdxFile.Clear;
begin
  TSsRecordSet(TdxForm(Owner).RecordSet).ClearFile(Self);
end;

procedure TdxFile.SaveToFile(const FileName: String);
begin
  SaveFileToFile(FileName, Self, TSsRecordSet(Form.RecordSet).DataSet);
end;

procedure TdxFile.LoadFromFile(const FileName: String);
begin
  LoadFileFromFile(FileName, Self, TSsRecordSet(Form.RecordSet).DataSet);
end;

procedure TdxFile.SaveToStream(St: TStream);
begin
  SaveFileToStream(St, Self, TSsRecordSet(Form.RecordSet).DataSet);
end;

function TdxFile.WasChanged: Boolean;
begin
  with TSsRecordSet(Form.RecordSet).DataSet.FieldByName(FieldStr(Id) + 'c') do
    Result := Value <> OldValue;
end;

{ TdxDBImage }

function TdxDBImage.GetSourceFileName: String;
begin
  Result := TSsRecordSet(Form.RecordSet).DataSet.FieldByName(FieldStr(Id) + 'src').AsString;
end;

function TdxDBImage.GetStoredFileName: String;
begin
  Result := TSsRecordSet(Form.RecordSet).DataSet.FieldByName(FieldStr(Id) + 'dest').AsString;
end;

procedure TdxDBImage.Assign(Source: TPersistent);
var
  S: TdxDBImage;
begin
  inherited Assign(Source);
  S := TdxDBImage(Source);
  StorageType := S.StorageType;
  StorageFolder := S.StorageFolder;
  ThumbSize := S.ThumbSize;
  PrintSize := S.PrintSize;
  Required := S.Required;
  ShowThumbnail := S.ShowThumbnail;
end;

procedure TdxDBImage.Clear;
begin
  TSsRecordSet(TdxForm(Owner).RecordSet).ClearImage(Self);
end;

procedure TdxDBImage.SaveToFile(const FileName: String);
begin
  SaveImageToFile(FileName, Self, TSsRecordSet(Form.RecordSet).DataSet);
end;

procedure TdxDBImage.LoadFromFile(const FileName: String);
begin
  LoadImageFromFile(FileName, Self, TSsRecordSet(Form.RecordSet).DataSet);
end;

procedure TdxDBImage.SaveToStream(St: TStream);
begin
  SaveImageToStream(St, Self, TSsRecordSet(Form.RecordSet).DataSet);
end;

function TdxDBImage.WasChanged: Boolean;
begin
  with TSsRecordSet(Form.RecordSet).DataSet.FieldByName(FieldStr(Id) + 'c') do
    Result := Value <> OldValue;
end;

{ TdxObjectField }

procedure TdxObjectField.Assign(Source: TPersistent);
var
  S: TdxObjectField;
begin
  inherited Assign(Source);
  S := TdxObjectField(Source);
  ObjId := S.ObjId;
  FieldId := S.FieldId;
end;

{ TdxCheckBox }

procedure TdxCheckBox.Assign(Source: TPersistent);
var
  S: TdxCheckBox;
begin
  inherited Assign(Source);
  S := TdxCheckBox(Source);
  Caption := S.Caption;
  CheckedText := S.CheckedText;
  UnCheckedText := S.UnCheckedText;
  Expression := S.Expression;
  DefaultValue := S.DefaultValue;
  CheckExpression := S.CheckExpression;
  Editable := S.Editable;
end;

{ TdxMemo }

procedure TdxMemo.Assign(Source: TPersistent);
var
  S: TdxMemo;
begin
  inherited Assign(Source);
  S := TdxMemo(Source);
  Required := S.Required;
  FieldSize := S.FieldSize;
  Expression := S.Expression;
  DefaultValue := S.DefaultValue;
  CheckExpression := S.CheckExpression;
  Editable := S.Editable;
end;

{ TdxDateEdit }

procedure TdxDateEdit.Assign(Source: TPersistent);
var
  S: TdxDateEdit;
begin
  inherited Assign(Source);
  S := TdxDateEdit(Source);
  DateNow := S.DateNow;
  Expression := S.Expression;
  DefaultValue := S.DefaultValue;
  CheckExpression := S.CheckExpression;
  Required := S.Required;
  Editable := S.Editable;
  HideButton := S.HideButton;
end;

{ TdxEdit }

procedure TdxEdit.Assign(Source: TPersistent);
var
  S: TdxEdit;
begin
  inherited Assign(Source);
  S := TdxEdit(Source);
  Expression := S.Expression;
  DefaultValue := S.DefaultValue;
  CheckExpression := S.CheckExpression;
  Required := S.Required;
  FieldSize := S.FieldSize;
  Editable := S.Editable;
end;

{ TdxLabel }

constructor TdxLabel.Create(AOwner: TdxComponent);
begin
  inherited Create(AOwner);
  FAutoSize := True;
end;

procedure TdxLabel.Assign(Source: TPersistent);
var
  S: TdxLabel;
begin
  inherited Assign(Source);
  S := TdxLabel(Source);
  Caption := S.Caption;
  FieldName := S.FieldName;
  Expression := S.Expression;
  Value := S.Value;
  Alignment := S.Alignment;
end;

{ TdxField }

procedure TdxField.Assign(Source: TPersistent);
var
  S: TdxField;
begin
  inherited Assign(Source);
  S := TdxField(Source);
  Id := S.Id;
  FieldName := S.FieldName;
end;

{ TLCbxListFields }

function TLCbxListFields.GetFields(Index: Integer): TLCbxListField;
begin
  Result := TLCbxListField(Items[Index]);
end;

procedure TLCbxListFields.Assign(S: TLCbxListFields);
var
  LF: TLCbxListField;
  i: Integer;
begin
  Clear;
  for i := 0 to S.Count - 1 do
  begin
    LF := S[i];
    with AddField do
    begin
      FieldId := LF.FieldId;
      Width := LF.Width;
      Searchable := LF.Searchable;
    end;
  end;
end;

function TLCbxListFields.AddField: TLCbxListField;
begin
	Result := TLCbxListField.Create;
  Add(Result);
end;

procedure TLCbxListFields.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Fields[i].Free;
  inherited Clear;
end;

{ TdxLookupComboBox }

procedure TdxLookupComboBox.SetListFields(AValue: TLCbxListFields);
begin
  FListFields.Assign(AValue);
end;

constructor TdxLookupComboBox.Create(AOwner: TdxComponent);
begin
  inherited Create(AOwner);
  FInsertedValues := TInsertedValues.Create;
  FListFields := TLCbxListFields.Create;
end;

destructor TdxLookupComboBox.Destroy;
begin
  FListFields.Free;
  FInsertedValues.Free;
  inherited Destroy;
end;

procedure TdxLookupComboBox.Assign(Source: TPersistent);
var
  S: TdxLookupComboBox;
begin
  inherited Assign(Source);
  S := TdxLookupComboBox(Source);
  InsertedValues.Assign(S.InsertedValues);
  ListFields.Assign(S.ListFields);
  ListWidthExtra := S.ListWidthExtra;
end;

{ TInsertedValues }

function TInsertedValues.GetValues(Index: Integer): TInsertValueData;
begin
  Result := TInsertValueData(Items[Index]);
end;

procedure TInsertedValues.Assign(S: TInsertedValues);
var
  i: Integer;
  SrcV: TInsertValueData;
begin
  Clear;
  for i := 0 to S.Count - 1 do
  begin
    SrcV := S[i];
    with AddValue do
    begin
      SrcField := SrcV.SrcField;
      DestField := SrcV.DestField;
    end;
  end;
end;

function TInsertedValues.AddValue: TInsertValueData;
begin
  Result := TInsertValueData.Create;
  Add(Result);
end;

procedure TInsertedValues.DeleteValue(Index: Integer);
begin
  Values[Index].Free;
  Delete(Index);
end;

procedure TInsertedValues.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  	Values[i].Free;
  inherited Clear;
end;

////////////////////////////////////////////////////////////////////////////////
{ TdxSortColList }

function TdxSortColList.GetCols(Index: Integer): TdxSortCol;
begin
  Result := TdxSortCol(Items[Index]);
end;

procedure TdxSortColList.Assign(S: TdxSortColList);
var
  i: Integer;
  SC: TdxSortCol;
begin
  Clear;
  for i := 0 to S.Count - 1 do
  begin
    SC := S[i];
    with AddCol do
    begin
      Index := SC.Index;
      Desc := SC.Desc;
    end;
  end;
end;

function TdxSortColList.AddCol: TdxSortCol;
begin
  Result := TdxSortCol.Create;
  Add(Result);
end;

function TdxSortColList.FindCol(Index: Integer): TdxSortCol;
var
  i: Integer;
  C: TdxSortCol;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    C := Cols[i];
    if C.Index = Index then Exit(C);
  end;
end;

procedure TdxSortColList.DeleteCol(SC: TdxSortCol);
begin
  Remove(SC);
  SC.Free;
end;

procedure TdxSortColList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Cols[i].Free;
  inherited Clear;
end;

{ TdxCalcEdit }

function TdxCalcEdit.PrecStr: String;
begin
  Result := MakeNumberFormat(FPrecission, FGroupDigits, FPadZeros);
end;

procedure TdxCalcEdit.Assign(Source: TPersistent);
var
  S: TdxCalcEdit;
begin
  inherited Assign(Source);
  S := TdxCalcEdit(Source);
  Expression := S.Expression;
  Precission := S.Precission;
  MinValue := S.MinValue;
  MaxValue := S.MaxValue;
  Required := S.Required;
  DefaultValue := S.DefaultValue;
  CheckExpression := S.CheckExpression;
  Editable := S.Editable;
  GroupDigits := S.GroupDigits;
  PadZeros := S.PadZeros;
end;

{ TdxTimeEdit }

procedure TdxTimeEdit.Assign(Source: TPersistent);
var
  S: TdxTimeEdit;
begin
  inherited Assign(Source);
  S := TdxTimeEdit(Source);
  CurTime := S.CurTime;
  TimeFormat := S.TimeFormat;
  Expression := S.Expression;
  DefaultValue := S.DefaultValue;
  CheckExpression := S.CheckExpression;
  Required := S.Required;
  Editable := S.Editable;
  HideButton := S.HideButton;
end;

function TdxTimeEdit.TimeFormatStr: String;
const
  TmFmt: array [TdxTimeFormat] of String = ('hh', 'hh:nn', 'hh:nn:ss');
begin
  Result := TmFmt[FTimeFormat];
end;

{ TdxColumn }

constructor TdxColumn.Create;
begin
  FVisible := True;
  FAutoAlignment := True;
  FAutoLayout := True;
end;

procedure TdxColumn.Assign(S: TdxColumn);
begin
  Id := S.Id;
  Width := S.Width;
  Visible := S.Visible;
  Caption := S.Caption;
  Alignment := S.Alignment;
  Layout := S.Layout;
  AutoAlignment := S.AutoAlignment;
  AutoLayout := S.AutoLayout;
end;

{ TdxShape }

{procedure TdxShape.Paint(Canvas: TBGRACanvas);
var
  PaintRect: TRect;
  MinSize: Longint;
  P: array[0..3] of TPoint;
  PenInc, PenDec: Integer;
begin
  with Canvas do
  begin
    Pen.Assign(Self.FPen);
    Brush.Assign(Self.FBrush);

    PenInc := Pen.Width div 2;
    PenDec := (Pen.Width - 1) div 2;

    PaintRect := Rect(PenInc, PenInc, Self.Width - PenDec, Self.Height - PenDec);
    if PaintRect.Left = PaintRect.Right then
      PaintRect.Right := PaintRect.Right + 1;
    if PaintRect.Top = PaintRect.Bottom then
      PaintRect.Bottom := PaintRect.Bottom + 1;

    with PaintRect do
    begin
      MinSize := Min(Right - Left, Bottom - Top);
      if FShape in [stSquare, stRoundSquare, stCircle, stSquaredDiamond] then
      begin
        Left := Left + ((Right - Left) - MinSize) div 2;
        Top := Top + ((Bottom - Top) - MinSize) div 2;
        Right := Left + MinSize;
        Bottom := Top + MinSize;
      end;
    end;

    case FShape of
      stRectangle, stSquare:
        Rectangle(PaintRect);
      stRoundRect, stRoundSquare:
        RoundRect(PaintRect, MinSize div 4, MinSize div 4);
      stCircle, stEllipse:
        Ellipse(PaintRect);
      stSquaredDiamond, stDiamond:
      begin
        with PaintRect do
        begin
          P[0].x := Left;
          P[0].y := (Top + Bottom) div 2;
          P[1].x := (Left + Right) div 2;
          P[1].y := Top;
          P[2].x := Right - 1;
          P[2].y := P[0].y;
          P[3].x := P[1].x;
          P[3].y := Bottom - 1;
          Polygon(P);
        end;
      end;
      stTriangle:
      begin
        with Self do
        begin
          P[0].x := (Width - 1) div 2;
          P[0].y := PenInc;
          P[1].x := Width - PenInc - 1;
          P[1].y := Height - PenInc - 1;
          P[2].x := PenInc;
          P[2].y := Height - PenInc - 1;
          P[3].x := P[0].x;
          P[3].y := P[0].y;
          Polygon(P);
        end;
      end;
    end;
  end;
end;   }

procedure TdxShape.SetBrush(AValue: TdxBrush);
begin
  FBrush.Assign(AValue);
end;

procedure TdxShape.PenChange(Sender: TObject);
begin
  FModified := True;
  DoPropertyChange('bitmap');
end;

procedure TdxShape.BrushChange(Sender: TObject);
begin
  FModified := True;
  DoPropertyChange('bitmap');
end;

procedure TdxShape.SetPen(AValue: TdxPen);
begin
  FPen.Assign(AValue);
end;

procedure TdxShape.SetShape(AValue: TShapeType);
begin
  if FShape=AValue then Exit;
  FShape:=AValue;
  FModified := True;
  DoPropertyChange('bitmap');
end;

procedure TdxShape.SetShapeEx(AValue: TShapeTypeEx);
begin
  if FShapeEx=AValue then Exit;
  FShapeEx:=AValue;
  FModified := True;
  DoPropertyChange('bitmap');
end;

constructor TdxShape.Create(AOwner: TdxComponent);
begin
  inherited Create(AOwner);
  FPen := TdxPen.Create;
  FPen.Color:=RGBToColor(10, 10, 10);  // чистый черный не сохраняется
  FPen.Style:=psSolid;
  FPen.Width:=1;
  FPen.OnChange := @PenChange;
  FBrush := TdxBrush.Create;
  FBrush.Color := clWhite;
  FBrush.Style := bsSolid;
  FBrush.OnChange:=@BrushChange;
end;

destructor TdxShape.Destroy;
begin
  FBrush.Free;
  FPen.Free;
  inherited Destroy;
end;

procedure TdxShape.Assign(Source: TPersistent);
var
  S: TdxShape;
begin
  inherited Assign(Source);
  S := TdxShape(Source);
  Brush.Assign(S.Brush);
  Pen.Assign(S.Pen);
  Shape := S.Shape;
  FModified := False;
end;

function TdxShape.GetImagePath(Html: Boolean): String;
var
  SS: TSession;
begin
  SS := TSsRecordSet(Form.RecordSet).Session;
  if FModified then
    Result := GetCachePath(SS, Html) + IntToStr(Form.Id) + '-' + IntToStr(Form.RecId) + '-' +
      Name + '.png'
  else
    Result := GetEmbeddedImagesPath(SS.Metadata, Html) + IntToStr(Form.Id) +
      Name + '.png';
end;

{procedure TdxShape.SaveToFile(const FileName: String);
var
  Bmp: TBGRABitmap;
begin
  Bmp := TBGRABitmap.Create(Width, Height);
  try
    Bmp.FillTransparent;
    Paint(Bmp.CanvasBGRA);
    Bmp.SaveToFile(FileName);
  finally
    Bmp.Free;
  end;
end;      }

////////////////////////////////////////////////////////////////////////////////

{ TdxImage }

procedure TdxImage.SetImageName(AValue: String);
begin
  if FImageName=AValue then Exit;
  FImageName:=AValue;
  DoPropertyChange('bitmap');
end;

constructor TdxImage.Create(AOwner: TdxComponent);
begin
  inherited Create(AOwner);
  //FBitmap := TBGRABitmap.Create(0, 0);
end;

destructor TdxImage.Destroy;
begin
  //FBitmap.Free;
  inherited Destroy;
end;

procedure TdxImage.Assign(Source: TPersistent);
var
  S: TdxImage;
begin
  inherited Assign(Source);
  S := TdxImage(Source);
  //Bitmap.Assign(S.Bitmap);
  Ext := S.Ext;
  Center := S.Center;
  Proportional := S.Proportional;
  Stretch := S.Stretch;
  KeepSize := S.KeepSize;
  ImageName := S.ImageName;
end;

function TdxImage.GetImagePath(Html: Boolean): String;
var
  SS: TSession;
  ImgD: TImageData;
begin
  SS := TSsRecordSet(Form.RecordSet).Session;
  if FImageName <> '' then
  begin
    ImgD := SS.ImageMan.FindImage(FImageName);
    if ImgD <> nil then
      Result := GetImagesPath(SS.MetaData, Html) + ImgD.Name + '.' + ImgD.Ext;
  end
  else if FExt <> '' then
  begin
    if FModified then
      Result := GetCachePath(SS, Html) + IntToStr(Form.Id) + '-' + IntToStr(Form.RecId) + '-' +
        Name + '.' + FExt
    else
      Result := GetEmbeddedImagesPath(SS.Metadata, Html) + IntToStr(Form.Id) +
        Name + '.' + FExt
  end
  else
    Result := '';
end;

procedure TdxImage.Clear;
begin
  FImageName := '';
  FExt := '';
  FModified := True;
  DoPropertyChange('bitmap');
end;

procedure TdxImage.SaveToFile(const FileName: String);
begin
  CopyFile(GetImagePath(False), FileName);
end;

procedure TdxImage.LoadFromFile(const FileName: String);
begin
  FImageName := '';
  FExt := ExtractFileExt(FileName);
  FModified := True;
  CopyFile(FileName, GetImagePath(False));
  DoPropertyChange('bitmap');
end;

procedure TdxImage.LoadFromStream(St: TStream);
begin
  FImageName := '';
  FExt := 'png';
  FModified := True;
  with TFileStream.Create(GetImagePath(False), fmCreate) do
  try
    St.Position := 0;
    CopyFrom(St, St.Size);
    DoPropertyChange('bitmap');
  finally
    Free;
  end;
end;

procedure TdxImage.SaveToStream(St: TStream);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(GetImagePath(False), fmOpenRead or fmShareDenyNone);
  try
    St.CopyFrom(FS, FS.Size);
  finally
    FS.Free;
  end;
end;

{ TColoringList }

function TColoringList.GetColorings(Index: Integer): TColoringData;
begin
  Result := TColoringData(Items[Index]);
end;

function TColoringList.AddColoring: TColoringData;
begin
  Result := TColoringData.Create;
  Add(Result);
end;

function TColoringList.FindColoring(const FieldName: String): TColoringData;
var
  i: Integer;
  CD: TColoringData;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    CD := Colorings[i];
    if CompareText(CD.FieldName, FieldName) = 0 then Exit(CD);
  end;
end;

procedure TColoringList.DeleteColoring(CD: TColoringData);
begin
  Remove(CD);
  CD.Free;
end;

procedure TColoringList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Colorings[i].Free;
  inherited Clear;
end;

procedure TColoringList.AssignList(Src: TColoringList);
var
  i: Integer;
  CD: TColoringData;
begin
  for i := 0 to Src.Count - 1 do
  begin
    CD := AddColoring;
    CD.Color := Src[i].Color;
    CD.FieldName := Src[i].FieldName;
    CD.Expr := Src[i].Expr;
  end;
end;

{ TdxForm }

{function TdxForm.GetGrid: TdxGrid;
begin
  Result := FindTable(0);
end;}

{function TdxForm.GetFields(AIndex: String): Variant;
begin
  RequeryIfNeed;
  Result := FormLookupFieldValue(TSsRecordSet(FRS), AIndex);
end;}

function TdxForm.GetFiles(AIndex: String): TdxFile;
var
  F: TdxField;
begin
  RequeryIfNeed;
  F := FindFieldByName(AIndex);
  if F is TdxFile then Result := TdxFile(F)
  else raise Exception.CreateFmt(rsFieldNotFound, [AIndex]);
end;

function TdxForm.GetFormByIndex(AIndex: Integer): TdxForm;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  Result := TSsRecordSet(FRS).Forms[AIndex].Form;
end;

function TdxForm.GetFormCount: Integer;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  Result := TSsRecordSet(FRS).Forms.Count;
end;

function TdxForm.GetFormFields(AIndex: String): Variant;
begin
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).GetFieldValue(AIndex);
end;

function TdxForm.GetForms(AIndex: String): TdxForm;
var
  RS: TSsRecordSet;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  RS := TSsRecordSet(FRS).Forms.FindFormByName(AIndex);
  if RS = nil then
    raise Exception.CreateFmt(rsFormNotFound, [AIndex]);
  Result := RS.Form;
end;

function TdxForm.GetImages(AIndex: String): TdxDBImage;
var
  F: TdxField;
begin
  RequeryIfNeed;
  F := FindFieldByName(AIndex);
  if F is TdxDBImage then Result := TdxDBImage(F)
  else raise Exception.CreateFmt(rsFieldNotFound, [AIndex]);
end;

function TdxForm.GetModified: Boolean;
begin
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).DataSet.Modified;
end;

function TdxForm.GetOldValues(AIndex: String): Variant;
begin
  RequeryIfNeed;
  Result := Field[AIndex].OldValue;
end;

function TdxForm.GetParentForm: TdxForm;
begin
  if FPId = 0 then Result := nil
  else Result := TSsRecordSet(FRS).Parent.Form;
end;

function TdxForm.GetQueries(AIndex: String): TdxQueryGrid;
var
  RS: TSsRecordSet;
begin
  RS := TSsRecordSet(FRS).Queries.FindRpByName(AIndex);
  if RS = nil then
    raise Exception.CreateFmt(rsQueryNotFound, [AIndex]);
  Result := RS.Parent.Form.FindQuery(RS.RD.Id);
end;

function TdxForm.GetQueryByIndex(AIndex: Integer): TdxQueryGrid;
var
  RS: TSsRecordSet;
begin
  RS := TSsRecordSet(FRS).Queries[AIndex];
  Result := RS.Parent.Form.FindQuery(RS.RD.Id);
end;

function TdxForm.GetQueryCount: Integer;
begin
  Result :=  TSsRecordSet(FRS).Queries.Count;
end;

function TdxForm.GetState: TDataSetState;
begin
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).DataSet.State;
end;

procedure TdxForm.SetCustomFilterForm(AValue: TdxForm);
begin
  if AValue <> nil then
    FCustomFilterRS := AValue.RecordSet
  else
    FCustomFilterRS := nil;
end;

function TdxForm.GetField(AIndex: String): TField;
var
  F: TdxField;
begin
  F := FindFieldByName(AIndex);
  if F = nil then raise Exception.CreateFmt(rsFieldNotFound, [AIndex]);
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).GetDSField(F);
end;

function TdxForm.GetAsDT(AIndex: String): TDateTime;
begin
  Result := Nz(Fields[AIndex], 0);
end;

function TdxForm.GetAsF(AIndex: String): Double;
begin
  Result := Nz(Fields[AIndex], 0);
end;

function TdxForm.GetAsI(AIndex: String): Integer;
begin
  Result := Nz(Fields[AIndex], 0);
end;

function TdxForm.GetAsS(AIndex: String): String;
begin
  Result := Nz(Fields[AIndex], '');
end;

function TdxForm.GetCustomFilterForm: TdxForm;
begin
  if CustomFilterRS <> nil then
    Result := TSsRecordSet(CustomFilterRS).Form
  else
    Result := nil;
end;

procedure TdxForm.SetFields(AIndex: String; AValue: Variant);
var
  C: TdxField;
begin
  C := FindFieldByName(AIndex);
  if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [AIndex]);
  RequeryIfNeed;
  TSsRecordSet(FRS).SetDSField(C, AValue);
end;

constructor TdxForm.Create(AOwner: TdxComponent);
begin
  inherited Create(AOwner);
  FCalcFields := TStringListUtf8.Create;
  FTemplates := TStringListUtf8.Create;
  FFilters := TStringList.Create;
  FColoring := TColoringList.Create;
  FErrs := TStringList.Create;
  FParams := TParamList.Create;
  FFilter := TFilterObject.Create(Self);
  FActionResult := Null;
end;

destructor TdxForm.Destroy;
begin
  FFilter.Free;
  FParams.Free;
  FErrs.Free;
  FColoring.Free;
  FFilters.Free;
  FTemplates.Free;
  FCalcFields.Free;
  // см. ExtractFormGrid
  FGrid.Free;
  FTree.Free;
  inherited Destroy;
end;

procedure TdxForm.Assign(Source: TPersistent);
var
  S: TdxForm;
begin
  inherited Assign(Source);
  S := TdxForm(Source);
  Id := S.Id;
  PId := S.PId;
  FormCaption := S.FormCaption;
  RecordCaption := S.RecordCaption;
  RecordsCaption := S.RecordsCaption;
  CalcFields.Assign(S.CalcFields);
  Templates.Assign(S.Templates);
  Filters.Assign(S.Filters);
  Coloring.AssignList(S.Coloring);
  ParentField := S.ParentField;
  LevelCount := S.LevelCount;
  ViewType := S.ViewType;
  AutoOpen := S.AutoOpen;
  Index := S.Index;
  ConfirmSaveRecord := S.ConfirmSaveRecord;
  ConfirmCancelEditing := S.ConfirmCancelEditing;
  ActionOnCreate := S.ActionOnCreate;
end;

procedure TdxForm.GetTabOrderList(L: TList);

  // Пример: 008-010-120
  function _GetTabOrder(C: TdxControl): String;
  var
    n: Integer;
  begin
    if C is TdxTabSheet then
      n := C.Parent.FControls.IndexOf(C)
    else
      n := C.TabOrder;
    Result := SetZeros(n, 3);
    if C.Parent <> C.Form then
      Result := _GetTabOrder(C.Parent) + Result;
  end;

var
  i: Integer;
  Ords: TStringList;
  C: TdxComponent;
begin
  L.Clear;
  Ords := TStringList.Create;
  Ords.Sorted:=True;
  for i := 0 to ComponentCount - 1 do
  begin
    C := Components[i];
    if ((C is TdxField) or (C is TdxButton)) and TdxControl(C).TabStop then
      Ords.AddObject(_GetTabOrder(TdxControl(C)), C);
  end;

  for i := 0 to Ords.Count - 1 do
    L.Add(Ords.Objects[i]);

  Ords.Free;
end;

{procedure TdxForm.GetFocusControls(L: TList);

  procedure _GetCmps(Cont: TdxWinControl);
  var
    i: Integer;
    C: TdxComponent;
  begin
    for i := 0 to Cont.ControlCount - 1 do
    begin
      C := Cont.Controls[i];
      if C is TdxWinControl then
        _GetCmps(TdxWinControl(C))
      else if not ((C is TdxLabel) or (C is TdxCustomGrid)) {and C.TabStop} then
        L.Add(C)
    end;
  end;

begin
  L.Clear;
  _GetCmps(Self);
end;  }

procedure TdxForm.GetFields(L: TList);

  procedure _GetFields(Cont: TdxWinControl);
  var
    i: Integer;
    C: TdxComponent;
  begin
    for i := 0 to Cont.ControlCount - 1 do
    begin
      C := Cont.Controls[i];
      if C is TdxField then
        L.Add(C)
      else if C is TdxWinControl then
        _GetFields(TdxWinControl(C));
    end;
  end;

begin
  L.Clear;
  _GetFields(Self);
end;

procedure TdxForm.GetGrids(L: TList);

  procedure _GetGrids(Cont: TdxWinControl);
  var
    i: Integer;
    C: TdxComponent;
  begin
    for i := 0 to Cont.ControlCount - 1 do
    begin
      C := Cont.Controls[i];
      if (C is TdxGrid) or (C is TdxQueryGrid) then
        L.Add(C)
      else if C is TdxWinControl then
        _GetGrids(TdxWinControl(C));
    end;
  end;

begin
  L.Clear;
  _GetGrids(Self);
end;

procedure TdxForm.GetTables(L: TList);

  procedure _GetTables(Cont: TdxWinControl);
  var
    i: Integer;
    C: TdxComponent;
  begin
    for i := 0 to Cont.ControlCount - 1 do
    begin
      C := Cont.Controls[i];
      if (C is TdxGrid) and (TdxGrid(C).Id <> 0) then
        L.Add(C)
      else if C is TdxWinControl then
        _GetTables(TdxWinControl(C));
    end;
  end;

begin
  L.Clear;
  _GetTables(Self);
end;

procedure TdxForm.GetQueries(L: TList);

  procedure _GetQueries(Cont: TdxWinControl);
  var
    i: Integer;
    C: TdxComponent;
  begin
    for i := 0 to Cont.ControlCount - 1 do
    begin
      C := Cont.Controls[i];
      if C is TdxQueryGrid then
        L.Add(C)
      else if C is TdxWinControl then
        _GetQueries(TdxWinControl(C));
    end;
  end;

begin
  L.Clear;
  _GetQueries(Self);
end;

procedure TdxForm.GetOrderedFields(L: TList);

  // Пример: 008-010-120
  function _GetTabOrder(C: TdxControl): String;
  var
    n: Integer;
  begin
    if C is TdxTabSheet then
      n := C.Parent.FControls.IndexOf(C)
    else if C is TdxDBImage then
      n := 999
    else
      n := C.TabOrder;
    Result := SetZeros(n, 3);
    if C.Parent <> C.Form then
      Result := _GetTabOrder(C.Parent) + Result;
  end;

var
  i: Integer;
  Ords: TStringList;
  C: TdxComponent;
begin
  L.Clear;
  Ords := TStringList.Create;
  //Ords.Sorted:=True;
  for i := 0 to ComponentCount - 1 do
  begin
    C := Components[i];
    if C is TdxField then
      Ords.AddObject(_GetTabOrder(TdxControl(C)), C);
  end;

  Ords.Sort;
  for i := 0 to Ords.Count - 1 do
    L.Add(Ords.Objects[i]);

  Ords.Free;
end;

(*var
  i: Integer;
  C: TdxControl;
  Ords: TStringList;
  S: String;
  LL: TList;

  // Пример: 008-010-120
  function GetTabOrder(C: TdxControl): String;
  var
    i: Integer;
  begin
    if C is TdxTabSheet then
      Result := IntToStr(C.Parent.FControls.IndexOf(C))
    else if C is TdxDBImage then
      Result := 'zzz'
    else
      Result := IntToStr(C.TabOrder);
    for i := Length(Result) to 3 - 1 do
      Result := '0' + Result;
    if C.Parent <> C.Form then
      Result := GetTabOrder(C.Parent) + ' ' + Result;
  end;

begin
  L.Clear;
  Ords := TStringList.Create;
  Ords.Sorted:=True;
  LL := TList.Create;
  GetFocusControls(LL);
  for i := 0 to LL.Count - 1 do
  begin
    C := TdxControl(LL[i]);
    if not (C is TdxField) and OnlyFields then Continue;

    {if C is TdxDBImage then
    begin
      L.Add(C);
      Continue;
    end;}

    S := GetTabOrder(C);
    Ords.AddObject(S, C);
  end;
  for i := 0 to Ords.Count - 1 do
    L.Add(Ords.Objects[i]);
  Ords.Free;
  LL.Free;
end;    *)

procedure TdxForm.GetFields(SL: TStrings);
var
  LL: TList;
  i: Integer;
begin
  SL.Clear;
  LL := TList.Create;
  GetFields(LL);
  for i := 0 to LL.Count - 1 do
    SL.AddObject(TdxField(LL[i]).FieldName, TObject(LL[i]));
  LL.Free;
end;

procedure TdxForm.GetCalcFields(L: TList);
var
  i: Integer;
begin
  GetFields(L);
  for i := L.Count - 1 downto 0 do
    if Trim(GetExpression(TdxComponent(L[i]))) = '' then L.Delete(i);
end;

procedure TdxForm.GetCalcLabels(L: TList);

  procedure _Get(Cont: TdxWinControl);
  var
    i: Integer;
    C: TdxComponent;
  begin
    for i := 0 to Cont.ControlCount - 1 do
    begin
      C := Cont.Controls[i];
      if (C is TdxLabel) and (Trim(GetExpression(C)) <> '') then
        L.Add(C)
      else if C is TdxWinControl then
        _Get(TdxWinControl(C));
    end;
  end;

begin
  L.Clear;
  _Get(Self);
end;

procedure TdxForm.GetPivots(L: TList);

  procedure _Get(Cont: TdxWinControl);
  var
    i: Integer;
    C: TdxComponent;
  begin
    for i := 0 to Cont.ControlCount - 1 do
    begin
      C := Cont.Controls[i];
      if C is TdxPivotGrid then
        L.Add(C)
      else if C is TdxWinControl then
        _Get(TdxWinControl(C));
    end;
  end;

begin
  L.Clear;
  _Get(Self);
end;

procedure TdxForm.ExtractFormGrid;
begin
  FGrid := TdxGrid(FindComponent('Grid'));
  FComponents.Remove(FGrid);
  FControls.Remove(FGrid);
  FTree := FindComponent('Tree');
  FComponents.Remove(FTree);
  FControls.Remove(FTree);
end;

function TdxForm.FindField(aId: Integer): TdxField;
var
  L: TList;
  i: Integer;
  F: TdxField;
begin
  Result := nil;
  L := TList.Create;
  try
    GetFields(L);
    for i := 0 to L.Count - 1 do
    begin
      F := TdxField(L[i]);
      if F.Id = aId then Exit(F);
    end;
  finally
    L.Free;
  end;
end;

function TdxForm.FindTable(aId: Integer): TdxGrid;
var
  L: TList;
  i: Integer;
  G: TdxGrid;
begin
  Result := nil;
  L := TList.Create;
  try
    GetGrids(L);
    for i := 0 to L.Count - 1 do
    begin
      if TObject(L[i]) is TdxGrid then
      begin
        G := TdxGrid(L[i]);
        if G.Id = aId then Exit(G);
      end;
    end;
  finally
    L.Free;
  end;
end;

function TdxForm.FindFieldByName(const S: String): TdxField;
var
  L: TList;
  i: Integer;
  F: TdxField;
begin
  Result := nil;
  L := TList.Create;
  try
    GetFields(L);
    for i := 0 to L.Count - 1 do
    begin
      F := TdxField(L[i]);
      if MyUtf8CompareText(F.FieldName, S) = 0 then Exit(F);
    end;
  finally
    L.Free;
  end;
end;

function TdxForm.FindLabelByName(const S: String; All: Boolean): TdxLabel;

  function _Find(Cont: TdxWinControl): TdxLabel;
  var
    i: Integer;
    C: TdxComponent;
  begin
    Result := nil;
    for i := 0 to Cont.ControlCount - 1 do
    begin
      C := Cont.Controls[i];
      if C is TdxLabel then
        with TdxLabel(C) do
        begin
          if (All or (Expression <> '')) and (MyUTF8CompareText(FieldName, S) = 0) then
            Exit(TdxLabel(C))
        end
      else if C is TdxWinControl then
      begin
        Result := _Find(TdxWinControl(C));
        if Result <> nil then Exit;
      end;
    end;
  end;

begin
  Result := _Find(Self);
end;

function TdxForm.FindQuery(aId: Integer): TdxQueryGrid;
var
  L: TList;
  Q: TdxQueryGrid;
  i: Integer;
begin
  Result := nil;
  L := TList.Create;
  GetQueries(L);
  for i := 0 to L.Count - 1 do
  begin
    Q := TdxQueryGrid(L[i]);
    if Q.Id = aId then
    begin
      Result := Q;
      Break;
    end;
  end;
  L.Free;
end;

function TdxForm.FindPivotByName(const aName: String): TObject;
var
  L: TList;
  i: Integer;
begin
  L := TList.Create;
  GetPivots(L);
  for i := 0 to L.Count - 1 do
    if CompareText(TdxPivotGrid(L[i]).Name, aName) = 0 then
    begin
      Result := TObject(L[i]);
      Break;
    end;
  L.Free;
end;

function TdxForm.GetFormParentFieldFieldId: Integer;
var
  C: TdxField;
begin
  Result := 0;
  if FParentField = 0 then Exit;
  C := FindField(FParentField);
  if C <> nil then
  	Result := GetSourceFId(C);
end;

function TdxForm.FindComponent(const S: String): TdxComponent;

  function _Find(Cont: TdxWinControl): TdxComponent;
  var
    i: Integer;
    C: TdxComponent;
  begin
    Result := nil;
    for i := 0 to Cont.ControlCount - 1 do
    begin
      C := Cont.Controls[i];
      if C.Name = S then Exit(C);
      if C is TdxWinControl then
      begin
        Result := _Find(TdxWinControl(C));
        if Result <> nil then Exit;
      end;
    end;
  end;

begin
  Result := _Find(Self);
end;

function TdxForm.GetRecordsCaption: String;
begin
  Result := FRecordsCaption;
  if Result = '' then Result := FFormCaption;
end;

function TdxForm.GetRecordCaption: String;
begin
  Result := FRecordCaption;
  if Result = '' then Result := FRecordsCaption;
  if Result = '' then Result := FFormCaption;
end;

procedure TdxForm.RequeryIfNeed;
begin
  if (FPId > 0) and (TSsRecordSet(FRS).NeedRequery) then
    TSsRecordSet(FRS).Open;
end;

procedure TdxForm.DoPrintEvent(Action: TPrintActionType; const SourceName,
  FieldName: String; var Value: String; var Accept: Boolean);
begin
  if FOnPrint <> nil then FOnPrint(Self, Action, SourceName, FieldName, Value, Accept);
end;

function TdxForm.Append: TAccessStatus;
begin
  Result := CanAppend;
  if (FPId > 0) and not (ParentForm.State in [dsInsert, dsEdit]) then Exit;
  RequeryIfNeed;
  TSsRecordSet(FRS).Append;
  Result := asOk;
end;

function TdxForm.Insert: TAccessStatus;
begin
  Result := CanAppend;
  if (FPId > 0) and not (ParentForm.State in [dsInsert, dsEdit]) then Exit;
  RequeryIfNeed;
  if FPId = 0 then
    TSsRecordSet(FRS).Insert
  else
    TSsRecordSet(FRS).Append;
  Result := asOk;
end;

function TdxForm.Edit: TAccessStatus;
begin
  Result := CanEdit;
  if (FPId > 0) and not (ParentForm.State in [dsInsert, dsEdit]) then Exit;
  TSsRecordSet(FRS).Edit;
  Result := asOk;
end;

function TdxForm.Delete: TAccessStatus;
begin
  Result := CanDelete;
  if (FPId > 0) and not (ParentForm.State in [dsInsert, dsEdit]) then Exit;
  if not (Result in [asDeleted, asHasRef]) then
    Result := TSsRecordSet(FRS).Delete;
end;

procedure TdxForm.Post;
begin
  RequeryIfNeed;
  TSsRecordSet(FRS).Post;
end;

procedure TdxForm.Cancel;
begin
  RequeryIfNeed;
  TSsRecordSet(FRS).Cancel;
end;

procedure TdxForm.Refresh;
begin
  Close;
  Open;
end;

procedure TdxForm.MoveFirst;
begin
  RequeryIfNeed;
  TSsRecordSet(FRS).DataSet.First;
end;

procedure TdxForm.MovePrior;
begin
  RequeryIfNeed;
  TSsRecordSet(FRS).DataSet.Prior;
end;

procedure TdxForm.MoveNext;
begin
  RequeryIfNeed;
  TSsRecordSet(FRS).DataSet.Next;
end;

procedure TdxForm.MoveLast;
begin
  RequeryIfNeed;
  TSsRecordSet(FRS).DataSet.Last;
end;

procedure TdxForm.MoveBy(Distance: Integer);
begin
  RequeryIfNeed;
  TSsRecordSet(FRS).DataSet.MoveBy(Distance);
end;

procedure TdxForm.MoveTo(ARecNo: Integer);
begin
  RequeryIfNeed;
  MoveBy(ARecNo - RecNo);
end;

function TdxForm.BOF: Boolean;
begin
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).DataSet.BOF;
end;

function TdxForm.EOF: Boolean;
begin
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).DataSet.EOF;
end;

function TdxForm.RecNo: Integer;
begin
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).DataSet.RecNo;
end;

function TdxForm.RecId: Integer;
begin
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).RecId;
end;

function TdxForm.RecordCount: Integer;
begin
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).DataSet.RecordCount;
end;

// В OutFileName путь не имеет значения, т. к. метод берет только имя файла.
function TdxForm.Print(const TemplateName, OutFileName: String; out
  AErrs: String; aOpenFile: Boolean): String;
var
  RS: TSsRecordSet;
  TplFullName, OutName: String;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  RequeryIfNeed;
  RS := TSsRecordSet(FRS);

  TplFullName := RS.Session.GetTemplatesPath +
    StringReplace(TemplateName, '\', DirectorySeparator, [rfReplaceAll]);
  if not FileExists(TplFullName) then
    raise Exception.CreateFmt(rsTemplateNotFound, [TemplateName]);

  if OutFileName = '' then
    OutName := ExtractFileName(TplFullName)
  else
    OutName := ExtractFileName(OutFileName);

  // Если выходной файл не указан или указано только имя файла, то файл будет в кэше
  if (OutFileName = '') or (OutName = OutFileName) then
  begin
    if aOpenFile then
      RS.DocUrl := GetCachePath(RS.Session, True) + OutName;
    OutName := GetCachePath(RS.Session) + OutName;
  end;

  ReportToXXX(RS, TplFullName, OutName, AErrs);
  Result := OutName;
end;

function TdxForm.Locate(const FieldNames: String;
  FieldValues: array of Variant; Options: TLocateOptions): Boolean;
var
  VArr: Variant;
  i: Integer;
  S: String;
  C: TdxField;
  SL: TStringList;
begin
  RequeryIfNeed;
  S := '';
  SL := TStringList.Create;
  try
    SplitStr(FieldNames, ';', SL);
    for i := 0 to SL.Count - 1 do
    begin
      C := FindFieldByName(SL[i]);
      S := S + FieldStr(C.Id);
      if i < SL.Count - 1 then S := S + ';';
    end;
    VArr := VarArrayOf(FieldValues);
    Result := TSsRecordSet(FRS).DataSet.Locate(S, VArr, Options);
  finally
    SL.Free;
    VarClear(VArr);
  end;
end;

function TdxForm.GotoRecord(aRecId: Integer): Boolean;
begin
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).DataSet.Locate('id', aRecId, []);
end;

function TdxForm.CanAppend: TAccessStatus;
begin
  Result := TSsRecordSet(FRS).CanAppend;
end;

function TdxForm.CanEdit: TAccessStatus;
begin
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).Editing;
end;

function TdxForm.CanDelete: TAccessStatus;
var
  RS: TSsRecordSet;
begin
  RequeryIfNeed;
  RS := TSsRecordSet(FRS);
  Result := RS.Deleting;
  if Result = asOk then
  begin
    if not RS.CheckAccessDetails then Result := asCantDelete
    else if (PId = 0) and not RS.CheckDeleteRecord then Result := asHasRef;
  end;
end;

procedure TdxForm.Open;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  with TSsRecordSet(FRS) do
  begin
    NeedRequery := True;
    Open;
  end;
end;

procedure TdxForm.OpenRecord(ARecId: Integer);
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  TSsRecordSet(FRS).OpenRecord(ARecId, False);
end;

procedure TdxForm.OpenRecords(const AFilter: String; Fm: TdxForm;
  SelCond: Boolean);
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  FCustomFilter := AFilter;
  if Fm <> nil then
    FCustomFilterRS := TSsRecordSet(Fm.RecordSet)
  else
    FCustomFilterRS := nil;
  FUseSelectCondition := SelCond;
  TSsRecordSet(FRS).OpenRecords;
end;

function TdxForm.Opened: Boolean;
begin
  Result := TSsRecordSet(FRS).DataSet.Active;
end;

procedure TdxForm.Close;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  TSsRecordSet(FRS).Close;
end;

function TdxForm.Validate: Boolean;
begin
  RequeryIfNeed;
  Result := TSsRecordSet(FRS).Validate;
end;

function TdxForm.FindComponentByFieldName(const FieldName: String
  ): TdxComponent;
begin
  Result := FindFieldByName(FieldName);
  if Result = nil then raise Exception.CreateFmt(
    rsComponentWithFieldNameNotFound, [FieldName]);
end;

procedure TdxForm.EnableScrollEvents;
begin
  TSsRecordSet(FRS).EnableScrollEvents;
  {if FScrollEventsCounter = 0 then Exit;
  Dec(FScrollEventsCounter);
  if FScrollEventsCounter = 0 then
    with TSsRecordSet(FRS) do
    begin
      Dataset.AfterScroll := FOldAfterScroll;
      DataSet.BeforeScroll := FOldBeforeScroll;
    end; }
end;

procedure TdxForm.DisableScrollEvents;
begin
  TSsRecordSet(FRS).DisableScrollEvents;
  {if FScrollEventsCounter = 0 then
    with TSsRecordSet(FRS) do
    begin
      FOldAfterScroll := Dataset.AfterScroll;
      FOldBeforeScroll := DataSet.BeforeScroll;
      DataSet.AfterScroll := nil;
      DataSet.BeforeScroll := nil;
    end;
  Inc(FScrollEventsCounter);}
end;

function TdxForm.ScrollEventsDisabled: Boolean;
begin
  Result := not TSsRecordSet(FRS).ScrollEventsEnabled;
  //Result := FScrollEventsCounter > 0;
end;

function TdxForm.WhoEdit(ARecId: Integer): String;
begin
  if FPId > 0 then raise Exception.CreateFmt(rsInvalidMethotCall, [FormCaption]);
  Result := TSsRecordSet(FRS).WhoEdit(ARecId);
end;

procedure TdxForm.GotoForm(const AFormName: String; ARecId: Integer;
  AGotoOption: TGotoOption);
var
  Fm: TdxForm;
  Tbl: TSsRecordSet;
  FormId, RId: Integer;
begin
  with TSsRecordSet(FRS) do
  begin
    if FPId > 0 then
    begin
      Tbl := Parent.Forms.FindFormByName(AFormName);
      FormId := Parent.Form.Id;
      RId := Parent.RecId;
    end
    else
    begin
      Tbl := Forms.FindFormByName(AFormName);
      FormId := FId;
      RId := RecId;
    end;
    if Tbl <> nil then
      GotoUrl := '?fm=' + IntToStr(FormId) + '&rec=' + IntToStr(RId) +
        '&tbl=' + IntToStr(Tbl.Form.Id) + '&row=' + IntToStr(ARecId)
    else
    begin
      Fm := Session.FormMan.FindFormByName(AFormName);
      if Fm = nil then raise Exception.CreateFmt(rsFormNotFound, [AFormName]);
      if Fm.PId > 0 then raise Exception.CreateFmt('Unable go to form [%s].', [AFormName]);
      GotoUrl := '?fm=' + IntToStr(Fm.Id) + '&rec=' + IntToStr(ARecId);
    end;
    GotoOption := AGotoOption;
  end;
end;

procedure TdxForm.GotoReport(const AReportName: String; AGotoOption: TGotoOption
  );
var
  Rp: TReportData;
begin
  Rp := TSsRecordSet(FRS).Session.ReportMan.FindReportByName(AReportName);
  if Rp = nil then raise Exception.CreateFmt(rsReportNotFound, [AReportName]);
  GotoUrl('?rp=' + IntToStr(Rp.Id), AGotoOption);
end;

procedure TdxForm.GotoUrl(const Url: String; AGotoOption: TGotoOption);
begin
  with TSsRecordSet(FRS) do
  begin
    GotoUrl := Url;
    TSsRecordSet(FRS).GotoOption := AGotoOption;
  end;
end;

procedure TdxForm.MessageDlg(const Title, Msg: String; MsgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; ClickHandler: TMsgButtonClickEvent);
var
  MI: TMsgInfo;
begin
  MI.Title := Title;
  MI.Msg := Msg;
  MI.MsgType := MsgType;
  MI.Buttons := Buttons;
  MI.Visible := True;
  MI.IsAction := False;
  TSsRecordSet(FRS).MsgInfo := MI;
  Self.OnMsgButtonClick := ClickHandler;
end;

procedure TdxForm.MsgBox(const Title, Msg: String);
begin
  MessageDlg(Title, Msg, mtWarning, [mbOk], nil);
end;

{ TFilterObject }

function TFilterObject.GetFields(Index: Integer): TFilterField;
begin
  Result := TFilterField(Items[Index]);
end;

constructor TFilterObject.Create(AForm: TdxForm);
begin
  inherited Create;
  FForm := AForm;
end;

function TFilterObject.AddField: TFilterField;
begin
  Result := TFilterField.Create(FForm);
  Add(Result);
end;

function TFilterObject.FindField(FId: Integer): TFilterField;
var
  i: Integer;
  F: TFilterField;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    F := Fields[i];
    if F.FId = FId then Exit(F);
  end;
end;

procedure TFilterObject.DeleteField(F: TFilterField);
begin
  Remove(F);
  F.Free;
end;

procedure TFilterObject.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  	Fields[i].Free;
  inherited Clear;
end;

procedure TFilterObject.Load(const Filter: String);
var
  SL, SL2: TStringList;
  i: Integer;
  S: String;
  F: TFilterField;
begin
  Clear;
  S := Filter;
  if Copy(S, 1, 7) <> 'FILTER:' then Exit;
  System.Delete(S, 1, 7);
  SL := TStringList.Create;
  SL2 := TStringList.Create;
  SplitStr(S, ' ~~ ', SL);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    SplitStr(S, '|', SL2);
    F := AddField;
    F.FId := StrToInt(SL2[0]);
    F.IsNot := Str2Bool(SL2[1]);
    F.IsNull := Str2Bool(SL2[2]);
    SplitStr(SL2[3], ';', F.Values);
  end;
  SL2.Free;
  SL.Free;
end;

// Вместо TStrings.DelimitedText, т. к. последний добавляет в пустую строку
// два QuoteChar. Засранец!
function GetDelimitedText(SL: TStrings; const D: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to SL.Count - 1 do
  begin
    Result := Result + SL[i];
    if i < SL.Count - 1 then Result := Result + D;
  end;
end;

// поле|not|null|значение;значение... ~~ поле|... ~~ ...
function TFilterObject.Save: String;
var
  i: Integer;
  S: String;
  F: TFilterField;
begin
  S := '';
  for i := 0 to Count - 1 do
  begin
    F := Fields[i];
    S := S + IntToStr(F.FId) + '|' + Bool2Str(F.IsNot) + '|' +
    	Bool2Str(F.IsNull) + '|' + GetDelimitedText(F.Values, ';');
    if i < Count - 1 then S := S + ' ~~ ';
  end;
  if S <> '' then S := 'FILTER:' + S;
  Result := S;
end;

function TFilterObject.ValuesExists: Boolean;
var
  i, j: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    if Fields[i].IsNull then Exit(True);
  	for j := 0 to Fields[i].Values.Count - 1 do
  		if Fields[i].Values[j] <> '' then Exit(True);
  end;
end;

function TFilterObject.AddFieldByName(const FieldName: String): TFilterField;
var
  C: TdxField;
begin
  C := FForm.FindFieldByName(FieldName);
  if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
  Result := AddField;
  Result.FId := C.Id;
end;

function TFilterObject.FindFieldByName(const FieldName: String): TFilterField;
var
  C: TdxField;
begin
  C := FForm.FindFieldByName(FieldName);
  if C = nil then raise Exception.CreateFmt(rsFieldNotFound, [FieldName]);
	Result := FindField(C.Id);
end;

{ TFilterField }

function TFilterField.GetFieldName: String;
var
  C: TdxField;
begin
  Result := '';
  C := FForm.FindField(FFId);
  if C <> nil then Result := C.FieldName;
end;

constructor TFilterField.Create(AForm: TdxForm);
begin
  FForm := AForm;
  FValues := TStringList.Create;
  FValues.Delimiter:=';';
  FValues.QuoteChar:=#0;
  FValues.StrictDelimiter:=True;
end;

destructor TFilterField.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

function TFilterField.GetValue(Index: Integer): String;
var
  p: SizeInt;
begin
	Result := FValues[Index];
  p := Pos(' .. ', Result);
  Result := Copy(Result, 1, p - 1);
end;

function TFilterField.GetEndValue(Index: Integer): String;
var
  p: SizeInt;
begin
  Result := FValues[Index];
  p := Pos(' .. ', Result);
  if p = 0 then Exit;
  Result := Copy(Result, p + 4, 255);
end;

{ TdxCustomGrid }

constructor TdxCustomGrid.Create(AOwner: TdxComponent);
begin
  inherited Create(AOwner);
  FColumns := TdxColumnList.Create;
  FSortCols := TdxSortColList.Create;
  FButtonFont := TdxFont.Create;
end;

destructor TdxCustomGrid.Destroy;
begin
  FButtonFont.Free;
  FSortCols.Free;
  FColumns.Free;
  inherited Destroy;
end;

procedure TdxCustomGrid.Assign(Source: TPersistent);
var
  S: TdxCustomGrid;
begin
  inherited Assign(Source);
  S := TdxCustomGrid(Source);
  Columns.Assign(S.Columns);
  Id := S.Id;
  SortCols.Assign(S.SortCols);
  AllowChangeSort := S.AllowChangeSort;
  AlignmentButtons := S.AlignmentButtons;
  ShowButtons := S.ShowButtons;
  VisibleButtons := S.VisibleButtons;
  ButtonSize := S.ButtonSize;
  ButtonFont.Assign(S.ButtonFont);
  ShowRowDeleteButton:=S.ShowRowDeleteButton;
end;

function TdxCustomGrid.GetVisibleColumnCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Columns.Count - 1 do
    if Columns[i].Visible and (Columns[i].Width > 0) then Inc(Result);
end;

{ TdxColumnList }

function TdxColumnList.GetColumns(Index: Integer): TdxColumn;
begin
  Result := TdxColumn(Items[Index]);
end;

procedure TdxColumnList.Assign(S: TdxColumnList);
var
  i: Integer;
  Col, NewCol: TdxColumn;
begin
  Clear;
  for i := 0 to S.Count - 1 do
  begin
    Col := S[i];
    NewCol := TdxColumn.Create;
    NewCol.Assign(Col);
    Add(NewCol);
  end;
end;

function TdxColumnList.FindCol(Id: Integer): TdxColumn;
var
  i: Integer;
  C: TdxColumn;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    C := Columns[i];
    if C.Id = Id then Exit(C);
  end;
end;

procedure TdxColumnList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Columns[i].Free;
  inherited Clear;
end;

{ TdxCustomComboBox }

procedure TdxCustomComboBox.SetItems(AValue: TStrings);
begin
  FItems.Assign(AValue);
end;

function TdxCustomComboBox.GetSourceFieldName: String;
var
  Fm: TdxForm;
begin
  if FSourceFId = 0 then Exit('');
  Fm := TSsRecordSet(Form.RecordSet).Session.FormMan.FindForm(FSourceTId);
  Result := Fm.FindField(FSourceFId).FieldName;
end;

function TdxCustomComboBox.GetSourceFormName: String;
var
  Fm: TdxForm;
begin
  if FSourceTId = 0 then Exit('');
  Fm := TSsRecordSet(Form.RecordSet).Session.FormMan.FindForm(FSourceTId);
  Result := Fm.FormCaption;
end;

constructor TdxCustomComboBox.Create(AOwner: TdxComponent);
begin
  inherited Create(AOwner);
  FItems := TStringList.Create;
  FDropDownCount := 8;
end;

destructor TdxCustomComboBox.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TdxCustomComboBox.Assign(Source: TPersistent);
var
  S: TdxCustomComboBox;
begin
  inherited Assign(Source);
  S := TdxCustomComboBox(Source);
  Items := S.Items;
  SourceTId := S.SourceTId;
  SourceFId := S.SourceFId;
  Filter := S.Filter;
  Style := S.Style;
  Required := S.Required;
  DefaultValue := S.DefaultValue;
  FieldSize := S.FieldSize;
  Expression := S.Expression;
  CheckExpression := S.CheckExpression;
  Editable := S.Editable;
  DropDownCount := S.DropDownCount;
end;

{ TdxWinControl }

function TdxWinControl.GetControlCount: Integer;
begin
  Result := FControls.Count;
end;

function TdxWinControl.GetControls(Index: Integer): TdxControl;
begin
  Result := Fcontrols[Index];
end;

constructor TdxWinControl.Create(AOwner: TdxComponent);
begin
  inherited Create(AOwner);
  FControls := TControlList.Create;
end;

destructor TdxWinControl.Destroy;
begin
  FControls.Free;
  inherited Destroy;
end;

procedure TdxWinControl.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

{ TdxFont }

{constructor TdxFont.Create;
begin
  Color := clDefault;
end;

procedure TdxFont.AssignFont(Fnt: TdxFont);
begin
  Name := Fnt.Name;
  Style := Fnt.Style;
  Color := Fnt.Color;
  Size := Fnt.Size;
end;    }

{ TdxComponent }

function TdxComponent.GetComponentCount: Integer;
begin
  Result := FComponents.Count;
end;

function TdxComponent.GetComponents(Index: Integer): TdxComponent;
begin
  Result := FComponents[Index];
end;

constructor TdxComponent.Create(AOwner: TdxComponent);
begin
  FComponents := TComponentList.Create;
  FOwner := AOwner;
  if AOwner <> nil then
    AOwner.FComponents.Add(Self);
end;

destructor TdxComponent.Destroy;
begin
  FComponents.Free;
  inherited Destroy;
end;

procedure TdxComponent.Assign(Source: TPersistent);
var
  S: TdxComponent;
  i: Integer;
begin
  S := TdxComponent(Source);
  Name := S.Name;
end;

function TdxComponent.Form: TdxForm;
begin
  Result := TdxForm(FOwner);
end;

function TdxComponent.FindComponent(const AName: String): TdxComponent;
var
  i: Integer;
  C: TdxComponent;
begin
  Result := nil;
  for i := 0 to FComponents.Count - 1 do
  begin
    C := FComponents[i];
    if CompareText(C.Name, AName) = 0 then Exit(C);
  end;
end;

{ TControlList }

function TControlList.GetControls(Index: Integer): TdxControl;
begin
  Result := TdxControl(Items[Index]);
end;

end.

