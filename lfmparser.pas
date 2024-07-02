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

unit LfmParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAGraphics, dxctrls, typeswrapper, BGRABitmapTypes, Math,
  pivotgrid;

type

  TLfmError = (lfeUnknownToken, lfeUnknownObject, lfeUnexpectEOF,
    lfePropAssignExpect, lfeUnknownPropValue);

  { ELfmParserError }

  ELfmParserError = class(Exception)
  private
    FErrorCode: TLfmError;
  public
    constructor Create(ErrCode: TLfmError);
    property ErrorCode: TLfmError read FErrorCode;
  end;

  { TLfmParser }

  TLfmParser = class
  private
    FEmbeddedImagesDir: String;
    FPos, FLen: Integer;
    FTk: Char;
    FTkStr: String;
    FObj: TdxControl;
    FBuf: String;
    FForm: TdxComponent;
    procedure DoError(ErrCode: TLfmError);
    procedure NextToken;
    procedure CheckEof;
    //procedure CheckObject(const ObjType: String);
    function GetObjectClass(const ObjType: String): TdxControlClass;
    function CreateObject(const ObjType, ObjName: String): TdxControl;
    function CheckProp(const PropName: String): Boolean;
    procedure SkipStrings;
    procedure SkipCollection;
    procedure ProcessStrings(SL: TStrings);
    procedure ProcessColoring(CL: TColoringList);
    procedure ProcessColumn(Col: TdxColumn);
    procedure ProcessColumns(CL: TdxColumnList);
    procedure ProcessListField(LF: TLCbxListField);
    procedure ProcessListFields(L: TLCbxListFields);
    procedure ProcessPivotField(PF: TFieldItem);
    procedure ProcessPivotFields(L: TFieldCollection);
    procedure ProcessProp(const PropName, PropValue: String);
    procedure ParseObject;
    procedure HexToImageFile(const Buf: String; Cmp: TdxComponent);
    //procedure ShapeToFile(Cmp: TdxShape);
  public
    constructor Create;
    destructor Destroy; override;
    function Parse(const Buf: String): TdxForm;
    property EmbeddedImagesDir: String read FEmbeddedImagesDir write FEmbeddedImagesDir;
  end;

function LfmErrorToString(Err: TLfmError): String;

implementation

uses
  apputils, StrUtils, BGRABitmap, dxreports;

const
  toNumber = #6;
  toSet = #7;
  toData = #8;

function GetToken(const S: String; L: Integer; var P: Integer; var Buf: String): Char;
var
  W: String;
begin
  Result := toEof;
  Buf := '';
  while (P <= L) and (S[P] in [#9, #10, #13, #32]) do
    Inc(P);
  if P > L then Exit;
  case S[P] of
    'A'..'Z', 'a'..'z', '_':
      begin
        while (P <= L) and (S[P] in ['A'..'Z', 'a'..'z', '_', '0'..'9', '.']) do
        begin
          Buf := Buf + S[P];
          Inc(P);
        end;
        Result := toSymbol;
      end;
    '0'..'9', '-':
      begin
        while (P <= L) and (S[P] in ['0'..'9', '.', '-', '+', 'e', 'E']) do
        begin
          Buf := Buf + S[P];
          Inc(P);
        end;
        Result := toNumber;
      end;
    '''', '#':
      begin
        if S[P] = '''' then Inc(P);
        while P <= L do
        begin
          if S[P] = '#' then
          begin
            W := '';
            while (P <= L) and (S[P] <> '''') and (S[P] >= #32) do
            begin
              W := W + S[P];
              Inc(P);
            end;
            W := StringReplace(W, '#13', #13, [rfReplaceAll]);
            W := StringReplace(W, '#10', #10, [rfReplaceAll]);
            Buf := Buf + W;
          end;
          if (P <= L) and (S[P] = '''') then
          begin
            Inc(P);
            if (P <= L) and (S[P] = '#') then Continue;
          end;
          if (P > L) or (S[P] in [#10, #13]) then Break;
          Buf := Buf + S[P];
          Inc(P);
        end;
        Inc(P);
        Result := toString;
      end;
    '[':
      begin
        Inc(P);
        while (P <= L) and (S[P] <> ']') do
        begin
          Buf := Buf + S[P];
          Inc(P);
        end;
        Inc(P);
        Result := toSet;
      end;
    '{':
      begin
        Inc(P);
        while (P <= L) and (S[P] <> '}') do
        begin
          Buf := Buf + S[P];
          Inc(P);
        end;
        Inc(P);
        Result := toData;
      end
    else
      begin
        Buf := S[P];
        Result := S[P];
        Inc(P);
      end;
  end;
end;

function LfmErrorToString(Err: TLfmError): String;
const
  Errs: array [TLfmError] of String = ('Unknown token', 'Unknown object',
    'Unexpected end of file', 'Property assignment expected',
    'Unknown property value');
begin
  Result := Errs[Err];
end;

function StrToBoolean(const S: String): Boolean;
begin
  if S = 'True' then Result := True
  else Result := False;
end;

function StrToShape(const S: String): TShapeType;
const
  St: array [0..8] of String = ('stRectangle', 'stSquare', 'stRoundRect', 'stRoundSquare',
      'stEllipse', 'stCircle', 'stSquaredDiamond', 'stDiamond', 'stTriangle');
var
  i: Integer;
begin
  Result := stRectangle;
  for i := 0 to High(St) do
    if S = St[i] then Exit(TShapeType(i));
end;

function StrToTimeFormat(const S: String): TdxTimeFormat;
begin
  if S = 'ttHH' then Result := ttHH
  else if S = 'ttHHMM' then Result := ttHHMM
  else if S = 'ttHHMMSS' then Result := ttHHMMSS
end;

function StrToComboBoxStyle(const S: String): TComboBoxStyle;
begin
  if S = 'csDropDownList' then Result := csDropDownList
  else Result := csDropDown;
end;

function StrToAlignment(const S: String): TAlignment;
begin
  if S = 'taRightJustify' then Result := taRightJustify
  else if S = 'taCenter' then Result := taCenter
  else Result := taLeftJustify;
end;

function StrToTextLayout(const S: String): TTextLayout;
begin
  if S = 'tlTop' then Result := tlTop
  else if S = 'tlCenter' then Result := tlCenter
  else Result := tlBottom;
end;

procedure ProcessFontStyle(Fnt: TdxFont; const Styles: String);
var
  SL: TStrings;
  S: String;
  i: Integer;
begin
  SL := TStringList.Create;
  SplitStr(Styles, ',', SL);
  Fnt.Style := [];
  for i := 0 to SL.Count - 1 do
  begin
    S := Trim(SL[i]);
    if S = 'fsBold' then Fnt.Style:=Fnt.Style + [fsBold]
    else if S = 'fsItalic' then Fnt.Style:=Fnt.Style + [fsItalic]
    else if S = 'fsStrikeOut' then Fnt.Style:=Fnt.Style + [fsStrikeOut]
    else if S = 'fsUnderline' then Fnt.Style:=Fnt.Style + [fsUnderline];
  end;
  SL.Free;
end;

procedure ProcessPenStyle(Pen: TdxPen; const Style: String);
begin
  if Style = 'psSolid' then Pen.Style := psSolid
  else if Style = 'psDash' then Pen.Style := psDash
  else if Style = 'psDot' then Pen.Style := psDot
  else if Style = 'psDashDot' then Pen.Style := psDashDot
  else if Style = 'psDashDotDot' then Pen.Style := psDashDotDot
  else if Style = 'psClear' then Pen.Style := psClear
  else if Style = 'psInsideFrame' then Pen.Style := psInsideFrame
  else if Style = 'psPattern' then Pen.Style := psPattern
end;

procedure ProcessLockMode(Fm: TdxForm; const Value: String);
begin
  if Value = 'lmNoLock' then Fm.LockMode := lmNoLock
  else if Value = 'lmPessimistic' then Fm.LockMode := lmPessimistic;
end;

procedure StrToSortCols(const Str: String; L: TdxSortColList);
var
  SL: TStringList;
  i, p: Integer;
  S: String;
  SC: TdxSortCol;
begin
  SL := TStringList.Create;
  SplitStr(Str, '|', SL);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    p := Pos(';', S);
    SC := L.AddCol;
    SC.Index := StrToInt(Copy(S, 1, p - 1));
    SC.Desc:=Str2Bool(Copy(S, p + 1, 1));
  end;
  SL.Free;
end;

function StrToViewType(const S: String): TViewType;
begin
  if S = 'vtGridTop' then Result := vtGridTop
  else if S = 'vtGridBottom' then Result := vtGridBottom
  else if S = 'vtGridLeft' then Result := vtGridLeft
  else if S = 'vtGridRight' then Result := vtGridRight
  else if S = 'vtGridOnly' then Result := vtGridOnly
  else if S = 'vtWithoutGrid' then Result := vtWithoutGrid
  else if S = 'vtSimpleForm' then Result := vtSimpleForm
  else raise Exception.Create('Unknown view type: ' + S);
end;

function MyStrToFloat(const S: String): Double;
var
  FS: TFormatSettings;
begin
  FS := FormatSettings;
  FS.DecimalSeparator:='.';
  Result := StrToFloat(S, FS);
end;

procedure ProcessInsertedValues(L: TInsertedValues; const PropValue: String);
var
  S: String;
  i, p, SrcId, DestId: Integer;
  Vl: TInsertValueData;
  SL: TStringList;
begin
  SL := TStringList.Create;
  SplitStr(PropValue, '|', SL);
	for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    p := Pos(';', S);
    TryStrToInt( Copy(S, 1, p - 1), SrcId );
    TryStrToInt( Copy(S, p + 1, 255), DestId );
    Vl := L.AddValue;
    Vl.SrcField := SrcId;
    Vl.DestField := DestId;
  end;
  SL.Free;
end;

function StrToGridButtonSet(const PropValue: String): TGridButtonSet;
var
  SL: TStringList;
  i: Integer;
  S: String;
begin
  Result := [];
  SL := TStringList.Create;
  SplitStr(PropValue, ', ', SL);
  for i := 0 to SL.Count - 1 do
  begin
    S := SL[i];
    if S = 'gbnAppend' then Include(Result, gbnAppend)
    else if S = 'gbnEdit' then Include(Result, gbnEdit)
    else if S = 'gbnDelete' then Include(Result, gbnDelete)
    else if S = 'gbnDuplicate' then Include(Result, gbnDuplicate)
    else if S = 'gbnShopping' then Include(Result, gbnShopping)
    else if S = 'gbnMoveUp' then Include(Result, gbnMoveUp)
    else if S = 'gbnMoveDown' then Include(Result, gbnMoveDown)
    else if S = 'gbnRefresh' then Include(Result, gbnRefresh)
    else if S = 'gbnGoto' then Include(Result, gbnGoto)
  end;
  SL.Free;
end;

function StrToTotalFunc(const S: String): TRpTotalFunc;
const
  StrFuncs: array [TRpTotalFunc] of String = ('tfNone', 'tfSum', 'tfAvg', 'tfMax', 'tfMin',
    'tfCount', 'tfProfit', 'tfDistCount', 'tfMergeAll', 'tfMerge');
var
  i: TRpTotalFunc;
begin
  for i := Low(StrFuncs) to High(StrFuncs) do
    if StrFuncs[i] = S then Exit(i);
end;

function StrToKVAlign(const S: String): TKVAlign;
const
  StrAlign: array [TKVAlign] of String = ('valTop', 'valCenter', 'valBottom');
var
  i: TKVAlign;
begin
  for i := Low(StrAlign) to High(StrAlign) do
    if StrAlign[i] = S then Exit(i);
end;

function StrToKHAlign(const S: String): TKHAlign;
const
  StrAlign: array [TKHAlign] of String = ('halLeft', 'halCenter', 'halRight', 'halJustify');
var
  i: TKHAlign;
begin
  for i := Low(StrAlign) to High(StrAlign) do
    if StrAlign[i] = S then Exit(i);
end;

function StrToRpFieldType(const S: String): TRpFieldType;
const
  StrType: array [TRpFieldType] of String = ('flNone', 'flText', 'flNumber',
    'flDate', 'flBool', 'flObject', 'flTime', 'flCounter', 'flFile', 'flRecId', 'flImage');
var
  i: TRpFieldType;
begin
  for i := Low(StrType) to High(StrType) do
    if StrType[i] = S then Exit(i);
end;

////////////////////////////////////////////////////////////////////////////////

{ ELfmParserError }

constructor ELfmParserError.Create(ErrCode: TLfmError);
begin
  inherited Create(LfmErrorToString(ErrCode));
  FErrorCode := ErrCode;
end;

{ TLfmParser }

procedure TLfmParser.DoError(ErrCode: TLfmError);
begin
  raise ELfmParserError.Create(ErrCode);
end;

procedure TLfmParser.ParseObject;
var
  OldP: Integer;
  Obj: TdxControl;
  PropName, PropValue, OldTkStr, ObjName: String;
begin
  NextToken; CheckEof;
  if FTk <> toSymbol then DoError(lfeUnknownToken);
  // У некоторых компонентов есть имя (например, Grid)
  OldP := FPos; OldTkStr := FTkStr;
  NextToken; CheckEof;
  if FTkStr = ':' then
  begin
    ObjName := OldTkStr;
    NextToken; CheckEof;
    if FTk <> toSymbol then DoError(lfeUnknownToken);
  end
  else
  begin
    ObjName := '';
    FPos := OldP;
    FTkStr := OldTkStr;
  end;
  //
  Obj := CreateObject(FTkStr, ObjName);
  if (FObj <> nil) and (FObj is TdxControl) then
  begin
    Obj.Parent := TdxWinControl(FObj);
    Obj.Font := Obj.GetRealFont;
    //TdxWinControl(FObj).Controls.Add(Obj);
  end;
  FObj := Obj;
  // В любом случае, первый объект - форма.
  if FForm = nil then
    FForm := Obj;
  while True do
  begin
    NextToken; CheckEof;
    if FTkStr = 'end' then
    begin
      if FObj.Parent <> nil then
      begin
        // В метаданных нет Font.Style = [], поэтому сбрасываем стиль вручную
        if not FObj.FontStyleParsed and not FObj.ParentFont then FObj.Font.Style := [];
        FObj := FObj.Parent;
      end;
      Exit;
    end
    else if FTkStr = 'object' then
    begin
      ParseObject;
    end
    else
    begin
      PropName := FTkStr;
      NextToken; CheckEof;
      if FTkStr <> '=' then
        DoError(lfePropAssignExpect);
      NextToken; CheckEof;
      PropValue := FTkStr;
      ProcessProp(PropName, PropValue);
    end;
  end;
end;

procedure TLfmParser.HexToImageFile(const Buf: String; Cmp: TdxComponent);
var
  i, Len: Integer;
  Hx, Ext, FlNm: String;
  pCh: PChar;
  MS: TMemoryStream;
  Bmp: TBGRABitmap;
begin
  if FEmbeddedImagesDir = '' then Exit;
  Ext := '?';
  MS := TMemoryStream.Create;
  New(pCh);
  Len := Length(Buf);
  i := 1;
  try
    while i <= Len do
    begin
      if Buf[i] > #32 then
      begin
        Hx := Copy(Buf, i, 2);
        HexToBin(PChar(Hx), pCh, 1);
        MS.Write(pCh^, 1);
        i := i + 2;
      end
      else Inc(i);
    end;
    if Cmp is TdxImage then
    begin
      MS.Position := 0;
      Ext := SuggestImageExtension(DetectFileFormat(MS));
      if Ext <> '?' then
      begin
        FlNm := FEmbeddedImagesDir + IntToStr(Cmp.Form.Id) + Cmp.Name + '.' + Ext;
        TdxImage(Cmp).Ext := Ext;
        MS.SaveToFile(FlNm);
      end;
    end
    else if Cmp is TdxButton then
    begin
      MS.Position := 4;
      FlNm := FEmbeddedImagesDir + IntToStr(Cmp.Form.Id) + Cmp.Name + '.png';
      Bmp := TBGRABitmap.Create(MS);
      Bmp.SaveToFileUTF8(FlNm);
      Bmp.Free;
      TdxButton(Cmp).HasGlyph := True;
    end;
  finally
    Dispose(pCh);
    MS.Free;
  end;
end;

{procedure TLfmParser.ShapeToFile(Cmp: TdxShape);
var
  PaintRect: TRect;
  MinSize: Longint;
  P: array[0..3] of TPoint;
  PenInc, PenDec: Integer;
  Bmp: TBGRABitmap;
begin
  Bmp := TBGRABitmap.Create(Cmp.Width, Cmp.Height);
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
  Bmp.SaveToFile(FEmbeddedImagesDir + IntToStr(Cmp.Form.Id) + Cmp.Name + '.png');
  Bmp.Free;
end; }

procedure TLfmParser.NextToken;
begin
  FTk := GetToken(FBuf, FLen, FPos, FTkStr);
end;

procedure TLfmParser.CheckEof;
begin
  if FTk = toEof then DoError(lfeUnexpectEOF);
end;

{procedure TLfmParser.CheckObject(const ObjType: String);
const
  ObjTypes = ' TdxForm TdxEdit TdxCalcEdit TdxDateEdit TdxMemo TdxCheckBox' +
    ' TdxComboBox TdxLookupComboBox TdxImage TdxDBImage TdxShape TdxGrid' +
    ' TdxFile TdxGroup TdxPageControl TdxTabSheet TdxQueryGrid TdxObjectField' +
    ' TdxTimeEdit TdxCounter TdxButton ';
begin
  if Pos(' ' + ObjType + ' ', ObjTypes) = 0 then
    DoError(lfeUnknownObject);
end;    }

function TLfmParser.GetObjectClass(const ObjType: String): TdxControlClass;
const
  ObjCls: array [0..24] of TdxControlClass = (TdxLabel, TdxEdit, TdxCalcEdit,
    TdxDateEdit, TdxMemo, TdxCheckBox, TdxComboBox, TdxLookupComboBox,
    TdxImage, TdxDBImage, TdxShape, TdxFile, TdxGrid, TdxGroupBox, TdxPageControl,
    TdxTabSheet, TdxForm, TdxQueryGrid, TdxObjectField, TdxTimeEdit, TdxCounter,
    TdxButton, TdxChart, TdxPivotGrid, TdxRecordId);
  ObjTypes: array [0..24] of String = ('TdxLabel', 'TdxEdit', 'TdxCalcEdit',
    'TdxDateEdit', 'TdxMemo', 'TdxCheckBox', 'TdxComboBox', 'TdxLookupComboBox',
    'TdxImage', 'TdxDBImage', 'TdxShape', 'TdxFile', 'TdxGrid', 'TdxGroupBox',
    'TdxPageControl', 'TdxTabSheet', 'TdxForm', 'TdxQueryGrid', 'TdxObjectField',
    'TdxTimeEdit', 'TdxCounter', 'TdxButton', 'TdxChart', 'TdxPivotGrid',
    'TdxRecordId');
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to High(ObjTypes) do
    if CompareText(ObjType, ObjTypes[i]) = 0 then
      Exit(ObjCls[i]);
end;

function TLfmParser.CreateObject(const ObjType, ObjName: String): TdxControl;
var
  Cls: TdxControlClass;
begin
  Cls := GetObjectClass(ObjType);
  if Cls <> nil then
  begin
    Result := Cls.Create(FForm);
    Result.Name:=ObjName;
  end
  else   //DoError(lfeUnknownObject);
  begin
    Result := TdxControl.Create(FForm);
    Result.Name := ObjName;
  end;
end;

function TLfmParser.CheckProp(const PropName: String): Boolean;
var
  S: String;
begin
  S := ' ' + PropName + ' ';
  if FObj is TdxControl then
    Result := Pos(S, ' Left Top Width Height Color Font.Name ' +
      ' Font.Height Font.Color Font.Style TabOrder StopTab ParentFont Caption Hidden ') > 0;
  if (not Result) and (FObj is TdxField) then
    Result := Pos(S, ' Id FieldName ') > 0;
  if (not Result) and (FObj is TdxLabel) then
    Result := Pos(S, ' Expression Alignment ') > 0;
  if (not Result) and (FObj is TdxEdit) then
    Result := Pos(S, ' Expression DefaultValue CheckExpression Required FieldSize Editable ') > 0;
  if (not Result) and (FObj is TdxCalcEdit) then
    Result := Pos(S, ' Expression DefaultValue CheckExpression Precission MinValue MaxValue Required DefaultValue Editable GroupDigits PadZeros ') > 0;
  if (not Result) and (FObj is TdxDateEdit) then
    Result := Pos(S, ' DateNow Expression DefaultValue CheckExpression Required Editable HideButton ') > 0;
  if (not Result) and (FObj is TdxCheckBox) then
    Result := Pos(S, ' CheckedText UnCheckedText Expression DefaultValue CheckExpression Editable ') > 0;
  if (not Result) and (FObj is TdxMemo) then
    Result := Pos(S, ' Required FieldSize Expression DefaultValue CheckExpression Editable ') > 0;
  if (not Result) and (FObj is TdxCustomComboBox) then
    Result := Pos(S, ' Items.Strings SourceTId SourceFId Filter Style Required DefaultValue FieldSize Expression CheckExpression Editable DropDownCount ') > 0;
  if not Result and (FObj is TdxLookupComboBox) then
    Result := Pos(S, ' InsertedValues ListFields ListWidthExtra HideList HideButton ListSource ListKeyField ') > 0;
  if (not Result) and (FObj is TdxShape) then
    Result := Pos(S, ' Brush.Color Pen.Color Pen.Style Pen.Width Shape ') > 0;
  if (not Result) and (FObj is TdxImage) then
    Result := Pos(S, ' Data Center Proportional Stretch KeepSize ImageName ') > 0;
  if (not Result) and (FObj is TdxDBImage) then
    Result := Pos(S, ' StorageType StorageFolder ThumbSize PrintSize Required ShowThumbnail CheckExpression ') > 0;
  if (not Result) and (FObj is TdxFile) then
    Result := Pos(S, ' StorageType StorageFolder FieldSize Required CheckExpression ') > 0;
  if (not Result) and (FObj is TdxCustomGrid) then
    Result := Pos(S, ' Id Columns SortCols AllowChangeSort AlignmentButtons ShowButtons VisibleButtons ButtonSize ButtonFont.Name ButtonFont.Height ButtonFont.Color ButtonFont.Style ShowRowDeleteButton ') > 0;
  if (not Result) and (FObj is TdxQueryGrid) then
    Result := Pos(S, ' ManualRefresh ') > 0;
  {if (not Result) and (FObj is TdxGroupBox) then
    Result := Pos(S, ' ') > 0;}
  if (not Result) and (FObj is TdxPageControl) then
    Result := Pos(S, ' TabIndex ') > 0;
  {if (not Result) and (FObj is TdxTabSheet) then
    Result := Pos(S, ' ') > 0;  }
  if (not Result) and (FObj is TdxForm) then
    Result := Pos(S, ' Id PId FormCaption CalcFields.Strings Templates.Strings ' +
      'Filters.Strings Coloring.Strings ParentField LevelCount ViewType AutoOpen ' +
      'Index ConfirmSaveRecord ConfirmCancelEditing ActionOnCreate RecordsCaption ' +
      'RecordCaption LockMode ') > 0;
  if (not Result) and (FObj is TdxObjectField) then
    Result := Pos(S, ' ObjId FieldId ') > 0;
  if (not Result) and (FObj is TdxTimeEdit) then
    Result := Pos(S, ' CurTime TimeFormat Expression Required DefaultValue CheckExpression Editable HideButton ') > 0;
  if (not Result) and (FObj is TdxCounter) then
    Result := Pos(S, ' ReadOnly Required CheckExpression ') > 0;
  if (not Result) and (FObj is TdxButton) then
    Result := Pos(S, ' ImageName Glyph.Data ActionOnClick ') > 0;
  if (not Result) and (FObj is TdxPivotGrid) then
    Result := Pos(S, ' RowFields ColFields DataFields GrandTotalFixedColor GrandTotalColor GrandTotalFixedFont.Name GrandTotalFixedFont.Height GrandTotalFixedFont.Color GrandTotalFixedFont.Style GrandTotalFont.Name GrandTotalFont.Height GrandTotalFont.Color GrandTotalFont.Style GrandTotalWidth CornerColor WordWrap GrandTotalCaption ShowGrandTotalX ShowGrandTotalY Id FixedFont.Name FixedFont.Height FixedFont.Color FixedFont.Style SelectedFont.Name SelectedFont.Height SelectedFont.Color SelectedFont.Style DataDelimiter Indent Colors.FixedCellBkGnd Colors.CellBkGnd Colors.FixedCellLines Colors.CellLines ColCount RowCount ') > 0;
end;

procedure TLfmParser.SkipStrings;
begin
  while True do
  begin
    NextToken; CheckEof;
    if FTkStr = ')' then Exit;
  end;
end;

procedure TLfmParser.SkipCollection;
begin
  while True do
  begin
    NextToken; CheckEof;
    if FTkStr = '<' then SkipCollection
    else if FTkStr = '>' then Exit;
  end;
end;

procedure TLfmParser.ProcessStrings(SL: TStrings);
begin
  while True do
  begin
    NextToken; CheckEof;
    if FTkStr = ')' then Exit;
    if FTk <> Classes.toString then DoError(lfeUnknownToken);
    SL.Add(FTkStr);
  end;
end;

procedure TLfmParser.ProcessColoring(CL: TColoringList);
var
  SL: TStringList;
  i, p: Integer;
  S: String;
  Color: TColor;
  CD: TColoringData;
begin
  SL := TStringList.Create;
  ProcessStrings(SL);
  for i := 0 to SL.Count -1 do
  begin
    S := SL[i];
    p := Pos(';', S);
    Color := StringToColor(Copy(S, 1, p - 1));
    Delete(S, 1, p);
    CD := CL.AddColoring;
    CD.Color := Color;
    CD.Expr := S;
  end;
  SL.Free;
end;

procedure TLfmParser.ProcessColumn(Col: TdxColumn);
var
  PropName, PropValue: String;
begin
  while True do
  begin
    NextToken; CheckEof;
    if FTkStr = 'end' then Exit;
    PropName := FTkStr;
    NextToken; CheckEof;
    if FTkStr <> '=' then
      DoError(lfePropAssignExpect);
    NextToken; CheckEof;
    PropValue := FTkStr;

    if PropName = 'Tag' then
      Col.Id := StrToInt(PropValue)
    else if PropName = 'Width' then
      Col.Width := StrToInt(PropValue)
    else if PropName = 'Visible' then
      Col.Visible := StrToBoolean(PropValue)
    else if PropName = 'Title.Caption' then
      Col.Caption := PropValue
    else if PropName = 'Alignment' then
      Col.Alignment := StrToAlignment(PropValue)
    else if PropName = 'Layout' then
      Col.Layout := StrToTextLayout(PropValue)
    else if PropName = 'AutoAlignment' then
      Col.AutoAlignment := StrToBoolean(PropValue)
    else if PropName = 'AutoLayout' then
      Col.AutoLayout := StrToBoolean(PropValue)
  end;
end;

procedure TLfmParser.ProcessColumns(CL: TdxColumnList);
var
  Col: TdxColumn;
begin
  while True do
  begin
    NextToken; CheckEof;
    if FTkStr = '>' then Exit
    else if FTkStr = 'item' then
    begin
      Col := TdxColumn.Create;
      CL.Add(Col);
      ProcessColumn(Col);
    end;
  end;
end;

procedure TLfmParser.ProcessListField(LF: TLCbxListField);
var
  PropName, PropValue: String;
begin
  while True do
  begin
    NextToken; CheckEof;
    if FTkStr = 'end' then Exit;
    PropName := FTkStr;
    NextToken; CheckEof;
    if FTkStr <> '=' then
      DoError(lfePropAssignExpect);
    NextToken; CheckEof;
    PropValue := FTkStr;

    if PropName = 'FieldId' then
      LF.FieldId := StrToInt(PropValue)
    else if PropName = 'FieldName' then
      LF.FieldName := PropValue
    else if PropName = 'Width' then
      LF.Width := StrToInt(PropValue)
    else if PropName = 'Searchable' then
      LF.Searchable := StrToBoolean(PropValue)
  end;
end;

procedure TLfmParser.ProcessListFields(L: TLCbxListFields);
var
  LF: TLCbxListField;
begin
  while True do
  begin
    NextToken; CheckEof;
    if FTkStr = '>' then Exit
    else if FTkStr = 'item' then
    begin
      LF := L.AddField;
      ProcessListField(LF);
    end;
  end;
end;

procedure TLfmParser.ProcessPivotField(PF: TFieldItem);
var
  PropName, PropValue: String;
begin
  while True do
  begin
    NextToken; CheckEof;
    if FTkStr = 'end' then Exit;
    PropName := FTkStr;
    NextToken; CheckEof;
    if FTkStr <> '=' then
      DoError(lfePropAssignExpect);
    NextToken; CheckEof;
    PropValue := FTkStr;

    if PropName = 'FieldName' then
      PF.FieldName := PropValue
    else if PropName = 'Caption' then
      PF.Caption := PropValue
    else if PropName = 'TotalCaption' then
      PF.TotalCaption := PropValue
    else if PropName = 'Width' then
      PF.Width := StrToInt(PropValue)
    else if PropName = 'Height' then
      PF.Height := StrToInt(PropValue)
    else if PropName = 'TotalWidth' then
      PF.TotalWidth := StrToInt(PropValue)
    else if PropName = 'Func' then
      PF.Func := StrToTotalFunc(PropValue)
    else if PropName = 'ShowTotal' then
      PF.ShowTotal := StrToBoolean(PropValue)
    else if PropName = 'FixedColor' then
      PF.FixedColor := StringToColor(PropValue)
    else if PropName = 'TotalFixedColor' then
      PF.TotalFixedColor := StringToColor(PropValue)
    else if PropName = 'Color' then
      PF.Color := StringToColor(PropValue)
    else if PropName = 'TotalColor' then
      PF.TotalColor := StringToColor(PropValue)
    else if PropName = 'FixedFont.Name' then
      PF.FixedFont.Name := PropValue
    else if PropName = 'FixedFont.Height' then
      PF.FixedFont.Size := Abs(StrToInt(PropValue))
    else if PropName = 'FixedFont.Color' then
      PF.FixedFont.Color := StringToColor(PropValue)
    else if PropName = 'FixedFont.Style' then
      ProcessFontStyle(PF.FixedFont, PropValue)
    else if PropName = 'TotalFixedFont.Name' then
      PF.TotalFixedFont.Name := PropValue
    else if PropName = 'TotalFixedFont.Height' then
      PF.TotalFixedFont.Size := Abs(StrToInt(PropValue))
    else if PropName = 'TotalFixedFont.Color' then
      PF.TotalFixedFont.Color := StringToColor(PropValue)
    else if PropName = 'TotalFixedFont.Style' then
      ProcessFontStyle(PF.TotalFixedFont, PropValue)
    else if PropName = 'Font.Name' then
      PF.Font.Name := PropValue
    else if PropName = 'Font.Height' then
      PF.Font.Size := Abs(StrToInt(PropValue))
    else if PropName = 'Font.Color' then
      PF.Font.Color := StringToColor(PropValue)
    else if PropName = 'Font.Style' then
      ProcessFontStyle(PF.Font, PropValue)
    else if PropName = 'TotalFont.Name' then
      PF.TotalFont.Name := PropValue
    else if PropName = 'TotalFont.Height' then
      PF.TotalFont.Size := Abs(StrToInt(PropValue))
    else if PropName = 'TotalFont.Color' then
      PF.TotalFont.Color := StringToColor(PropValue)
    else if PropName = 'TotalFont.Style' then
      ProcessFontStyle(PF.TotalFont, PropValue)
    else if PropName = 'VAlign' then
      PF.VAlign := StrToKVAlign(PropValue)
    else if PropName = 'HAlign' then
      PF.HAlign := StrToKHAlign(PropValue)
    else if PropName = 'DataType' then
      PF.DataType := StrToRpFieldType(PropValue)
  end;
end;

procedure TLfmParser.ProcessPivotFields(L: TFieldCollection);
var
  PF: TFieldItem;
begin
  while True do
  begin
    NextToken; CheckEof;
    if FTkStr = '>' then Exit
    else if FTkStr = 'item' then
    begin
      PF := L.AddField;
      ProcessPivotField(PF);
    end;
  end;
end;

procedure TLfmParser.ProcessProp(const PropName, PropValue: String);
begin
  if not CheckProp(PropName) then
  begin
    if FTk <> toString then
    begin
      if PropValue = '(' then SkipStrings
      else if PropValue = '<' then SkipCollection;
    end;
    Exit;
  end;
  if FObj is TdxControl then
  begin
    if PropName = 'Left' then
      FObj.Left:=StrToInt(PropValue)
    else if PropName = 'Top' then
      FObj.Top := StrToInt(PropValue)
    else if PropName = 'Width' then
      FObj.Width := StrToInt(PropValue)
    else if PropName = 'Height' then
      FObj.Height := StrToInt(PropValue)
    else if PropName = 'Color' then
      FObj.Color := StringToColor(PropValue)
    else if PropName = 'Font.Name' then
      FObj.Font.Name:=PropValue
    else if PropName = 'Font.Height' then
      FObj.Font.Size := Abs(StrToInt(PropValue))
    else if PropName = 'Font.Color' then
      FObj.Font.Color := StringToColor(PropValue)
    else if PropName = 'Font.Style' then
    begin
      ProcessFontStyle(FObj.Font, PropValue);
      FObj.FontStyleParsed := True;
    end
    else if PropName = 'TabOrder' then
      FObj.TabOrder:=StrToInt(PropValue)
    else if PropName = 'StopTab' then
      FObj.TabStop := StrToBoolean(PropValue)
    else if PropName = 'ParentFont' then
      FObj.ParentFont := StrToBoolean(PropValue)
    else if PropName = 'Caption' then
      FObj.Caption := PropValue
    else if PropName = 'Hidden' then
      FObj.Hidden := StrToBoolean(PropValue)
  end;
  if FObj is TdxField then
    with TdxField(FObj) do
    begin
      if PropName = 'Id' then
        Id := StrToInt(PropValue)
      else if PropName = 'FieldName' then
        FieldName := PropValue;
    end;
  if FObj is TdxLabel then
    with TdxLabel(FObj) do
    begin
      if PropName = 'Caption' then
        FieldName := PropValue
      else if PropName = 'Expression' then
        Expression := PropValue
      else if PropName = 'Alignment' then
        Alignment := StrToAlignment(PropValue);
    end
  else if FObj is TdxEdit then
    with TdxEdit(FObj) do
    begin
      if PropName = 'Expression' then
        Expression := PropValue
      else if PropName = 'DefaultValue' then
        DefaultValue := PropValue
      else if PropName = 'CheckExpression' then
        CheckExpression := PropValue
      else if PropName = 'Required' then
        Required := StrToBoolean(PropValue)
      else if PropName = 'FieldSize' then
        FieldSize := StrToInt(PropValue)
      else if PropName = 'Editable' then
        Editable := StrToBoolean(PropValue)
    end
  else if FObj is TdxCalcEdit then
    with TdxCalcEdit(FObj) do
    begin
      if PropName = 'Expression' then
        Expression := PropValue
      else if PropName = 'Precission' then
        Precission := StrToInt(PropValue)
      else if PropName = 'MinValue' then
        MinValue := MyStrToFloat(PropValue)
      else if PropName = 'MaxValue' then
        MaxValue := MyStrToFloat(PropValue)
      else if PropName = 'Required' then
        Required := StrToBoolean(PropValue)
      else if PropName = 'DefaultValue' then
        DefaultValue:=PropValue
      else if PropName = 'CheckExpression' then
        CheckExpression := PropValue
      else if PropName = 'Editable' then
        Editable := StrToBoolean(PropValue)
      else if PropName = 'GroupDigits' then
        GroupDigits := StrToBoolean(PropValue)
      else if PropName = 'PadZeros' then
        PadZeros := StrToBoolean(PropValue)
    end
  else if FObj is TdxDateEdit then
    with TdxDateEdit(FObj) do
    begin
      if PropName = 'DateNow' then
        DateNow := StrToBoolean(PropValue)
      else if PropName = 'Expression' then
        Expression := PropValue
      else if PropName = 'DefaultValue' then
        DefaultValue := PropValue
      else if PropName = 'CheckExpression' then
        CheckExpression := PropValue
      else if PropName = 'Required' then
        Required := StrToBoolean(PropValue)
      else if PropName = 'Editable' then
        Editable := StrToBoolean(PropValue)
      else if PropName = 'HideButton' then
        HideButton := StrToBoolean(PropValue)
    end
  else if FObj is TdxMemo then
    with TdxMemo(FObj) do
    begin
      if PropName = 'Required' then
        Required := StrToBoolean(PropValue)
      else if PropName = 'FieldSize' then
        FieldSize := StrToInt(PropValue)
      else if PropName = 'Expression' then
        Expression := PropValue
      else if PropName = 'DefaultValue' then
        DefaultValue := PropValue
      else if PropName = 'CheckExpression' then
        CheckExpression := PropValue
      else if PropName = 'Editable' then
        Editable := StrToBoolean(PropValue)
    end
  else if FObj is TdxCheckBox then
    with TdxCheckBox(FObj) do
    begin
      if PropName = 'CheckedText' then
        CheckedText:=PropValue
      else if PropName = 'UnCheckedText' then
        UnCheckedText:=PropValue
      else if PropName = 'Expression' then
        Expression := PropValue
      else if PropName = 'DefaultValue' then
        DefaultValue := PropValue
      else if PropName = 'CheckExpression' then
        CheckExpression := PropValue
      else if PropName = 'Editable' then
        Editable := StrToBoolean(PropValue)
    end
  else if FObj is TdxCustomComboBox then
    with TdxCustomComboBox(FObj) do
    begin
      if (PropName = 'Items.Strings') and (PropValue = '(') then
        ProcessStrings(Items)
      else if PropName = 'SourceTId' then
        SourceTId := StrToInt(PropValue)
      else if PropName = 'SourceFId' then
        SourceFId := StrToInt(PropValue)
      else if PropName = 'Filter' then
        Filter := PropValue
      else if PropName = 'Style' then
        Style := StrToComboBoxStyle(PropValue)
      else if PropName = 'Required' then
        Required := StrToBoolean(PropValue)
      else if PropName = 'DefaultValue' then
        DefaultValue:=PropValue
      else if PropName = 'FieldSize' then
        FieldSize := StrToInt(PropValue)
      else if PropName = 'Expression' then
        Expression := PropValue
      else if PropName = 'CheckExpression' then
        CheckExpression := PropValue
      else if PropName = 'Editable' then
        Editable := StrToBoolean(PropValue)
      else if PropName = 'DropDownCount' then
        DropDownCount := StrToInt(PropValue)
      else if FObj is TdxLookupComboBox then
        with TdxLookupComboBox(FObj) do
        begin
          if PropName = 'InsertedValues' then
            ProcessInsertedValues(InsertedValues, PropValue)
          else if PropName = 'ListFields' then
            ProcessListFields(ListFields)
          else if PropName = 'ListWidthExtra' then
            ListWidthExtra := StrToInt(PropValue)
          else if PropName = 'HideList' then
            HideList := StrToBoolean(PropValue)
          else if PropName = 'HideButton' then
            HideButton := StrToBoolean(PropValue)
          else if PropName = 'ListSource' then
            ListSource := StrToInt(PropValue)
          else if PropName = 'ListKeyField' then
            ListKeyField := PropValue
        end
    end
  else if FObj is TdxShape then
    with TdxShape(FObj) do
    begin
      if PropName = 'Brush.Color' then
        Brush.Color:=StringToColor(PropValue)
      else if PropName = 'Pen.Color' then
        Pen.Color := StringToColor(PropValue)
      else if PropName = 'Pen.Style' then
        ProcessPenStyle(Pen, PropValue)
      else if PropName = 'Pen.Width' then
        Pen.Width := StrToInt(PropValue)
      else if PropName = 'Shape' then
        Shape:=StrToShape(PropValue);
      ShapeToFile(TdxShape(FObj), FEmbeddedImagesDir + IntToStr(Form.Id) + FObj.Name + '.png');
      Modified := False;
    end
  else if FObj is TdxImage then
    with TdxImage(FObj) do
    begin
      if PropName = 'Data' then
        HexToImageFile(PropValue, FObj)
      else if PropName = 'Center' then
        Center := StrToBoolean(PropValue)
      else if PropName = 'Proportional' then
        Proportional := StrToBoolean(PropValue)
      else if PropName = 'Stretch' then
        Stretch := StrToBoolean(PropValue)
      else if PropName = 'KeepSize' then
        KeepSize := StrToBoolean(PropValue)
      else if PropName = 'ImageName' then
        ImageName := PropValue
    end
  else if FObj is TdxDBImage then
    with TdxDBImage(FObj) do
    begin
      if PropName = 'StorageType' then
        StorageType:=StrToInt(PropValue)
      else if PropName = 'StorageFolder' then
        {$ifdef windows}
        StorageFolder := StringReplace(PropValue, '/', PathDelim, [rfReplaceAll])
        {$else}
        StorageFolder := StringReplace(PropValue, '\', PathDelim, [rfReplaceAll])
        {$endif}
      else if PropName = 'ThumbSize' then
        ThumbSize:=StrToInt(PropValue)
      else if PropName = 'PrintSize' then
        PrintSize := StrToInt(PropValue)
      else if PropName = 'Required' then
        Required := StrToBoolean(PropValue)
      else if PropName = 'CheckExpression' then
        CheckExpression := PropValue
      else if PropName = 'ShowThumbnail' then
        ShowThumbnail := StrToBoolean(PropValue)
    end
  else if FObj is TdxFile then
    with TdxFile(FObj) do
    begin
      if PropName = 'StorageType' then
        StorageType:=StrToInt(PropValue)
      else if PropName = 'StorageFolder' then
        {$ifdef windows}
        StorageFolder := StringReplace(PropValue, '/', PathDelim, [rfReplaceAll])
        {$else}
        StorageFolder := StringReplace(PropValue, '\', PathDelim, [rfReplaceAll])
        {$endif}
      else if PropName = 'FieldSize' then
        FieldSize := StrToInt(PropValue)
      else if PropName = 'Required' then
        Required := StrToBoolean(PropValue)
      else if PropName = 'CheckExpression' then
        CheckExpression := PropValue
    end
  else if FObj is TdxCustomGrid then
  begin
    with TdxCustomGrid(FObj) do
      if (PropName = 'Columns') and (PropValue = '<') then
        ProcessColumns(Columns)
      else if PropName = 'Id' then
        Id := StrToInt(PropValue)
      else if PropName = 'SortCols' then
        StrToSortCols(PropValue, SortCols)
      else if PropName = 'AllowChangeSort' then
        AllowChangeSort := StrToBoolean(PropValue)
      else if PropName = 'AlignmentButtons' then
        AlignmentButtons := StrToAlignment(PropValue)
      else if PropName = 'ShowButtons' then
        ShowButtons := StrToBoolean(PropValue)
      else if PropName = 'VisibleButtons' then
        VisibleButtons := StrToGridButtonSet(PropValue)
      else if PropName = 'ButtonSize' then
        ButtonSize := StrToInt(PropValue)
      else if PropName = 'ButtonFont.Name' then
        ButtonFont.Name:=PropValue
      else if PropName = 'ButtonFont.Height' then
        ButtonFont.Size := Abs(StrToInt(PropValue))
      else if PropName = 'ButtonFont.Color' then
        ButtonFont.Color := StringToColor(PropValue)
      else if PropName = 'ButtonFont.Style' then
        ProcessFontStyle(ButtonFont, PropValue)
      else if PropName = 'ShowRowDeleteButton' then
        ShowRowDeleteButton := StrToBoolean(PropValue)
      else if FObj is TdxQueryGrid then
      begin
        with TdxQueryGrid(FObj) do
          if PropName = 'ManualRefresh' then
            ManualRefresh := StrToBoolean(PropValue)
      end
  end
  {else if FObj is TdxQueryGrid then
    with TdxQueryGrid(FObj) do
    begin
      if (PropName = 'Columns') and (PropValue = '<') then
        ProcessColumns(Columns)
      else if PropName = 'Id' then
        Id := StrToInt(PropValue)
    end}
  else if FObj is TdxGroupBox then
    with TdxGroupBox(FObj) do
    begin

    end
  else if FObj is TdxPageControl then
    with TdxPageControl(FObj) do
    begin
      if PropName = 'TabIndex' then
        ActivePageIndex := StrToInt(PropValue);
    end
  else if FObj is TdxTabSheet then
    with TdxTabSheet(FObj) do
    begin

    end
  else if FObj is TdxForm then
    with TdxForm(FObj) do
    begin
      if PropName = 'Id' then
        Id := StrToInt(PropValue)
      else if PropName = 'PId' then
        PId := StrToInt(PropValue)
      else if PropName = 'FormCaption' then
        FormCaption := PropValue
      else if (PropName = 'CalcFields.Strings') and (PropValue = '(') then
        ProcessStrings(CalcFields)
      else if (PropName = 'Templates.Strings') and (PropValue = '(') then
        ProcessStrings(Templates)
      else if (PropName = 'Filters.Strings') and (PropValue = '(') then
        ProcessStrings(Filters)
      else if (PropName = 'Coloring.Strings') and (PropValue = '(') then
        ProcessColoring(Coloring)
      else if PropName = 'ParentField' then
        ParentField := StrToInt(PropValue)
      else if PropName = 'LevelCount' then
        LevelCount := StrToInt(PropValue)
      else if PropName = 'ViewType' then
        ViewType := StrToViewType(PropValue)
      else if PropName = 'AutoOpen' then
        AutoOpen := StrToBoolean(PropValue)
      else if PropName = 'Index' then
        Index := StrToInt(PropValue)
      else if PropName = 'ConfirmSaveRecord' then
        ConfirmSaveRecord := StrToBoolean(PropValue)
      else if PropName = 'ConfirmCancelEditing' then
        ConfirmCancelEditing := StrToBoolean(PropValue)
      else if PropName = 'ActionOnCreate' then
        ActionOnCreate := PropValue
      else if PropName = 'RecordsCaption' then
        RecordsCaption := PropValue
      else if PropName = 'RecordCaption' then
        RecordCaption := PropValue
      else if PropName = 'LockMode' then
        ProcessLockMode(TdxForm(FObj), PropValue)
    end
  else if FObj is TdxObjectField then
    with TdxObjectField(FObj) do
    begin
      if PropName = 'ObjId' then
        ObjId := StrToInt(PropValue)
      else if PropName = 'FieldId' then
        FieldId := StrToInt(PropValue)
    end
  else if FObj is TdxTimeEdit then
    with TdxTimeEdit(FObj) do
    begin
      if PropName = 'CurTime' then
        CurTime := StrToBoolean(PropValue)
      else if PropName = 'TimeFormat' then
        TimeFormat := StrToTimeFormat(PropValue)
      else if PropName = 'Expression' then
        Expression:=PropValue
      else if PropName = 'DefaultValue' then
        DefaultValue := PropValue
      else if PropName = 'CheckExpression' then
        CheckExpression := PropValue
      else if PropName = 'Required' then
        Required := StrToBoolean(PropValue)
      else if PropName = 'Editable' then
        Editable := StrToBoolean(PropValue)
      else if PropName = 'HideButton' then
        HideButton := StrToBoolean(PropValue)
    end
  else if FObj is TdxCounter then
    with TdxCounter(FObj) do
    begin
      if PropName = 'ReadOnly' then
        ReadOnly := StrToBoolean(PropValue)
      else if PropName = 'Required' then
        Required := StrToBoolean(PropValue)
      else if PropName = 'CheckExpression' then
        CheckExpression := PropValue
    end
  else if FObj is TdxButton then
    with TdxButton(FObj) do
    begin
      if PropName = 'ImageName' then
        ImageName := PropValue
      else if PropName = 'Glyph.Data' then
        HexToImageFile(PropValue, FObj)
      else if PropName = 'ActionOnClick' then
        ActionOnClick := PropValue
    end
  else if FObj is TdxPivotGrid then
    with TdxPivotGrid(FObj) do
    begin
      if PropName = 'RowFields' then
        ProcessPivotFields(RowFields)
      else if PropName = 'ColFields' then
        ProcessPivotFields(ColFields)
      else if PropName = 'DataFields' then
        ProcessPivotFields(DataFields)
      else if PropName = 'GrandTotalFixedColor' then
        GrandTotalFixedColor := StringToColor(PropValue)
      else if PropName = 'GrandTotalColor' then
        GrandTotalColor := StringToColor(PropValue)
      else if PropName = 'GrandTotalFixedFont.Name' then
        GrandTotalFixedFont.Name := PropValue
      else if PropName = 'GrandTotalFixedFont.Height' then
        GrandTotalFixedFont.Size := Abs(StrToInt(PropValue))
      else if PropName = 'GrandTotalFixedFont.Color' then
        GrandTotalFixedFont.Color := StringToColor(PropValue)
      else if PropName = 'GrandTotalFixedFont.Style' then
        ProcessFontStyle(GrandTotalFixedFont, PropValue)
      else if PropName = 'GrandTotalFont.Name' then
        GrandTotalFont.Name := PropValue
      else if PropName = 'GrandTotalFont.Height' then
        GrandTotalFont.Size := Abs(StrToInt(PropValue))
      else if PropName = 'GrandTotalFont.Color' then
        GrandTotalFont.Color := StringToColor(PropValue)
      else if PropName = 'GrandTotalFont.Style' then
        ProcessFontStyle(GrandTotalFont, PropValue)
      else if PropName = 'GrandTotalWidth' then
        GrandTotalWidth := StrToInt(PropValue)
      else if PropName = 'CornerColor' then
        CornerColor := StringToColor(PropValue)
      else if PropName = 'WordWrap' then
        WordWrap := StrToBoolean(PropValue)
      else if PropName = 'GrandTotalCaption' then
        GrandTotalCaption := PropValue
      else if PropName = 'ShowGrandTotalX' then
        ShowGrandTotalX := StrToBoolean(PropValue)
      else if PropName = 'ShowGrandTotalY' then
        ShowGrandTotalY := StrToBoolean(PropValue)
      else if PropName = 'Id' then
        Id := StrToInt(PropValue)
      else if PropName = 'FixedFont.Name' then
        FixedFont.Name := PropValue
      else if PropName = 'FixedFont.Height' then
        FixedFont.Size := Abs(StrToInt(PropValue))
      else if PropName = 'FixedFont.Color' then
        FixedFont.Color := StringToColor(PropValue)
      else if PropName = 'FixedFont.Style' then
        ProcessFontStyle(FixedFont, PropValue)
      else if PropName = 'SelectedFont.Name' then
        SelectedFont.Name := PropValue
      else if PropName = 'SelectedFont.Height' then
        SelectedFont.Size := Abs(StrToInt(PropValue))
      else if PropName = 'SelectedFont.Color' then
        SelectedFont.Color := StringToColor(PropValue)
      else if PropName = 'SelectedFont.Style' then
        ProcessFontStyle(SelectedFont, PropValue)
      else if PropName = 'DataDelimiter' then
        DataDelimiter := PropValue
      else if PropName = 'Indent' then
        Indent := StrToInt(PropValue)
      else if PropName = 'Colors.FixedCellBkGnd' then
        Colors.FixedCellBkGnd := StringToColor(PropValue)
      else if PropName = 'Colors.CellBkGnd' then
        Colors.CellBkGnd := StringToColor(PropValue)
      else if PropName = 'Colors.CellLines' then
        Colors.CellLines := StringToColor(PropValue)
      else if PropName = 'Colors.FixedCellLines' then
        Colors.FixedCellLines := StringToColor(PropValue)
      else if PropName = 'ColCount' then
        ColCount := StrToInt(PropValue)
      else if PropName = 'RowCount' then
        RowCount := StrToInt(PropValue)
    end
end;

constructor TLfmParser.Create;
begin

end;

destructor TLfmParser.Destroy;
begin
  inherited Destroy;
end;

function TLfmParser.Parse(const Buf: String): TdxForm;
begin
  Result := nil;
  FForm := nil;
  FPos := 1; FLen := Length(Buf);
  FBuf := Buf; FObj := nil;
  NextToken;
  if FTkStr = 'object' then
  begin
    ParseObject;
    Result := TdxForm(FForm);
    {if FObj <> nil then
    begin
      if FObj is TdxForm then
        Exit(TdxForm(FObj))
      else
        FreeAndNil(FObj);
    end;  }
  end
  else
    Exit;
end;

end.

