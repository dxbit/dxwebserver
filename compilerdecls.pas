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

unit CompilerDecls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPSCompiler, uPSUtils;

procedure SIRegister_All(Cl: TPSPascalCompiler);

implementation

uses
  UPSC_DLL;

procedure SIRegisterTObject(CL: TPSPascalCompiler);
begin
  with Cl.AddClassN(nil, 'TObject') do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('procedure Free');
    RegisterProperty('ClassName', 'String', iptR);
  end;
end;

procedure SIRegisterTPersistent(CL: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TObject'), 'TPersistent') do
  begin
    RegisterMethod('procedure Assign(Source: TPersistent)');
  end;
end;

procedure SIRegisterTComponent(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TPersistent'), 'TComponent') do
  begin
    RegisterMethod('function FindComponent(AName: string): TComponent');
    RegisterMethod('constructor Create(AOwner: TComponent); virtual');
    RegisterProperty('Owner', 'TComponent', iptR);
    RegisterProperty('Components', 'TComponent Integer', iptr);
    RegisterProperty('ComponentCount', 'Integer', iptr);
    RegisterProperty('Name', 'string', iptrw);
  end;
end;

procedure SIRegisterTCOLLECTIONITEM(CL: TPSPascalCompiler);
Begin
  cl.AddClassN(cl.FindClass('TPersistent'), 'TCollection');
  with cl.AddClassN(cl.FindClass('TPersistent'),'TCollectionItem') do
  begin
    RegisterProperty('Collection', 'TCollection', iptrw);
    RegisterProperty('Index', 'Integer', iptrw);
  end;
end;

procedure SIRegisterTCOLLECTION(CL: TPSPascalCompiler);
Begin
  With CL.FindClass('TCollection') do
  begin
    RegisterMethod('procedure Clear');
    RegisterProperty('Count', 'Integer', iptr);
    RegisterProperty('Items', 'TCollectionItem Integer', iptrw);
  end;
end;

procedure SIRegister_Std_TypesAndConsts(Cl: TPSPascalCompiler);
begin
  cl.AddTypeS('TPoint', 'record X,Y: LongInt; end;');
  Cl.AddTypeS('TRect', 'record Left, Top, Right, Bottom: Integer; end;');
end;

//!!!
procedure SIRegister_Std(Cl: TPSPascalCompiler);
begin
  SIRegister_Std_TypesAndConsts(Cl);
  SIRegisterTObject(CL);
  SIRegisterTPersistent(Cl);
  SIRegisterTComponent(Cl);
  SIRegisterTCollectionItem(Cl);
  SIRegisterTCollection(Cl);
end;

procedure SIRegisterTStrings(cl: TPSPascalCompiler); // requires TPersistent
begin
  with Cl.AddClassN(cl.FindClass('TPersistent'), 'TStrings') do
  begin
    IsAbstract := True;
    RegisterMethod('constructor Create');
    RegisterMethod('function Add(S: string): Integer;');
    RegisterMethod('procedure AddStrings(Strings: TStrings);');
    RegisterMethod('procedure Clear;');
    RegisterMethod('procedure Delete(Index: Integer);');
    RegisterMethod('function IndexOf(const S: string): Integer; ');
    RegisterMethod('procedure Insert(Index: Integer; S: string); ');
    RegisterMethod('procedure LoadFromFile(FileName: string);');
    RegisterMethod('procedure SaveToFile(FileName: string);');
    RegisterMethod('procedure BeginUpdate;');
    RegisterMethod('procedure EndUpdate;');
    RegisterMethod('function Equals(Strings: TStrings): Boolean;');
    RegisterMethod('procedure Exchange(Index1, Index2: Integer);');
    RegisterMethod('function IndexOfName(Name: string): Integer;');
    RegisterMethod('procedure LoadFromStream(Stream: TStream); ');
    RegisterMethod('procedure Move(CurIndex, NewIndex: Integer); ');
    RegisterMethod('procedure SaveToStream(Stream: TStream); ');
    RegisterMethod('function AddObject(S: string; AObject: TObject): Integer');
    RegisterMethod('function IndexOfObject(AObject: TObject): Integer');
    RegisterMethod('procedure InsertObject(Index: Integer; S: string; AObject: TObject)');

    RegisterProperty('Capacity', 'Integer', iptRW);
    RegisterProperty('Delimiter', 'Char', iptRW);
    RegisterProperty('DelimitedText', 'string', iptrw);
    RegisterProperty('NameValueSeparator', 'Char', iptRW);
    RegisterProperty('QuoteChar', 'Char', iptRW);
    RegisterProperty('Count', 'Integer', iptR);
    RegisterProperty('Text', 'string', iptrw);
    RegisterProperty('CommaText', 'string', iptrw);
    RegisterProperty('Strings', 'string Integer', iptRW);
    SetDefaultPropery('Strings');
    RegisterProperty('StrictDelimiter', 'Boolean', iptRW);
    RegisterProperty('Objects', 'TObject Integer', iptRW);
    RegisterProperty('Names', 'string Integer', iptr);
    RegisterProperty('Values', 'string string', iptRW);
    RegisterProperty('ValueFromIndex', 'string Integer', iptRW);

  end;
end;

procedure SIRegisterTStringList(cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TStrings'), 'TStringList') do
  begin
    RegisterMethod('function Find(S: string; var Index: Integer): Boolean');
    RegisterMethod('procedure Sort');
    RegisterProperty('CaseSensitive', 'Boolean', iptrw);
    RegisterProperty('Duplicates', 'TDuplicates', iptrw);
    RegisterProperty('Sorted', 'Boolean', iptrw);
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
    RegisterProperty('OnChanging', 'TNotifyEvent', iptrw);
  end;

  Cl.AddTypeS('TStringListCompareEvent', 'function(Sender: TStringList; const s1, s2: String): Integer');
  with Cl.AddClassN(cl.FindClass('TStringList'), 'TStringListUtf8') do
  begin
    RegisterProperty('OnCompare', 'TStringListCompareEvent', iptrw);
  end;
end;

procedure SIRegisterTSTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TObject'), 'TStream') do
  begin
    //IsAbstract := True;
    RegisterMethod('function Read(Buffer: string; Count: LongInt): LongInt; virtual;');
    RegisterMethod('function Write(Buffer: string; Count: LongInt): LongInt; virtual;');
    RegisterMethod('function Seek(Offset: LongInt; Origin: Word): LongInt; virtual;');
    RegisterMethod('procedure ReadBuffer(Buffer: string; Count: LongInt)');
    RegisterMethod('procedure WriteBuffer(Buffer: string; Count: LongInt)');
    RegisterMethod('function CopyFrom(Source: TStream; Count: Int64): Int64');
    RegisterProperty('Position', 'Int64', iptrw);
    RegisterProperty('Size', 'Int64', iptrw);
  end;
end;

procedure SIRegisterTHANDLESTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TStream'), 'THandleStream') do
  begin
    RegisterMethod('constructor Create(AHandle: Integer)');
    RegisterProperty('Handle', 'Integer', iptr);
  end;
end;

procedure SIRegisterTMEMORYSTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TStream'), 'TMemoryStream') do
  begin
    RegisterMethod('procedure Clear');
    RegisterMethod('procedure LoadFromStream(Stream: TStream)');
    RegisterMethod('procedure LoadFromFile(FileName: string)');
    RegisterMethod('procedure SaveToStream(Stream: TStream)');
    RegisterMethod('procedure SaveToFile(FileName: string)');
    RegisterMethod('procedure SetSize(NewSize: LongInt)');
  end;
end;

procedure SIRegisterTFILESTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TStream'), 'TFileStream') do
  begin
    RegisterMethod('constructor Create(FileName: string; Mode: Word)');
    RegisterProperty('Handle', 'Integer', iptr);
  end;
end;

procedure SIRegisterTSTRINGSTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TMemoryStream'), 'TStringStream') do
  begin
    RegisterMethod('constructor Create(AString: string)');
    RegisterProperty('DataString', 'String', iptR);
  end;
end;

procedure SIRegister_Classes_TypesAndConsts(Cl: TPSPascalCompiler);
begin
  cl.AddConstantN('soFromBeginning', 'LongInt').Value^.ts32 := 0;
  cl.AddConstantN('soFromCurrent', 'LongInt').Value^.ts32 := 1;
  cl.AddConstantN('soFromEnd', 'LongInt').Value^.ts32 := 2;
  cl.AddConstantN('toEOF', 'Char').SetString(#0);
  cl.AddConstantN('toSymbol', 'Char').SetString(#1);
  cl.AddConstantN('toString', 'Char').SetString(#2);
  cl.AddConstantN('ToInteger', 'Char').SetString(#3);
  cl.AddConstantN('toFloat', 'Char').SetString(#4);
  cl.AddConstantN('fmCreate', 'LongInt').Value^.ts32 := $FF00;//$FFFF;
  cl.AddConstantN('fmOpenRead', 'LongInt').Value^.ts32 := 0;
  cl.AddConstantN('fmOpenWrite', 'LongInt').Value^.ts32 := 1;
  cl.AddConstantN('fmOpenReadWrite', 'LongInt').Value^.ts32 := 2;
  cl.AddConstantN('fmShareCompat', 'LongInt').Value^.ts32 := 0;
  cl.AddConstantN('fmShareExclusive', 'LongInt').Value^.ts32 := $10;
  cl.AddConstantN('fmShareDenyWrite', 'LongInt').Value^.ts32 := $20;
  cl.AddConstantN('fmShareDenyRead', 'LongInt').Value^.ts32 := $30;
  cl.AddConstantN('fmShareDenyNone', 'LongInt').Value^.ts32 := $40;
  cl.AddConstantN('SecsPerDay', 'LongInt').Value^.ts32 := 86400;
  cl.AddConstantN('MSecPerDay', 'LongInt').Value^.ts32 := 86400000;
  cl.AddConstantN('DateDelta', 'LongInt').Value^.ts32 := 693594;
  cl.AddTypeS('TAlignment', '(taLeftJustify, taRightJustify, taCenter)');
  //cl.AddTypeS('THelpEvent', 'function (Command: Word; Data: LongInt; var CallHelp: Boolean): Boolean');
  //cl.AddTypeS('TGetStrProc', 'procedure(const S: string)');
  cl.AddTypeS('TDuplicates', '(dupIgnore, dupAccept, dupError)');
  //cl.AddTypeS('TOperation', '(opInsert, opRemove)');
  cl.AddTypeS('THandle', 'LongInt');

  cl.AddTypeS('TNotifyEvent', 'procedure (Sender: TObject)');
end;

//!!!
procedure SIRegister_Classes(Cl: TPSPascalCompiler);
begin
  SIRegister_Classes_TypesAndConsts(Cl);
  SIRegisterTSTREAM(Cl);
  SIRegisterTStrings(cl);
  SIRegisterTStringList(cl);
  SIRegisterTFILESTREAM(Cl);
  SIRegisterTMEMORYSTREAM(Cl);
  SIRegisterTSTRINGSTREAM(Cl);
end;

procedure SIRegister_GraphicsTypes(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TColor', 'Integer');
  cl.AddConstantN('clBlack', 'Integer').Value^.ts32 := $000000;
  cl.AddConstantN('clMaroon', 'Integer').Value^.ts32 := $000080;
  cl.AddConstantN('clGreen', 'Integer').Value^.ts32 := $008000;
  cl.AddConstantN('clOlive', 'Integer').Value^.ts32 := $008080;
  cl.AddConstantN('clNavy', 'Integer').Value^.ts32 := $800000;
  cl.AddConstantN('clPurple', 'Integer').Value^.ts32 := $800080;
  cl.AddConstantN('clTeal', 'Integer').Value^.ts32 := $808000;
  cl.AddConstantN('clGray', 'Integer').Value^.ts32 := $808080;
  cl.AddConstantN('clSilver', 'Integer').Value^.ts32 := $C0C0C0;
  cl.AddConstantN('clRed', 'Integer').Value^.ts32 := $0000FF;
  cl.AddConstantN('clLime', 'Integer').Value^.ts32 := $00FF00;
  cl.AddConstantN('clYellow', 'Integer').Value^.ts32 := $00FFFF;
  cl.AddConstantN('clBlue', 'Integer').Value^.ts32 := $FF0000;
  cl.AddConstantN('clFuchsia', 'Integer').Value^.ts32 := $FF00FF;
  cl.AddConstantN('clAqua', 'Integer').Value^.ts32 := $FFFF00;
  cl.AddConstantN('clLtGray', 'Integer').Value^.ts32 := $C0C0C0;
  cl.AddConstantN('clDkGray', 'Integer').Value^.ts32 := $808080;
  cl.AddConstantN('clWhite', 'Integer').Value^.ts32 := $FFFFFF;
  cl.AddConstantN('clNone', 'Integer').Value^.ts32 := $1FFFFFFF;
  cl.AddConstantN('clDefault', 'Integer').Value^.ts32 := $20000000;

  cl.AddConstantN('clMoneyGreen', 'Integer').Value^.ts32 := $C0DCC0;
  cl.AddConstantN('clSkyBlue', 'Integer').Value^.ts32 := $F0CAA6;
  cl.AddConstantN('clCream', 'Integer').Value^.ts32 := $F0FBFF;
  cl.AddConstantN('clMedGray', 'Integer').Value^.ts32 := $A4A0A0;

  Cl.addTypeS('TFontStyle', '(fsBold, fsItalic, fsUnderline, fsStrikeOut)');
  Cl.addTypeS('TFontStyles', 'set of TFontStyle');

  cl.AddTypeS('TPenStyle', '(psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear, psInsideFrame)');
  cl.AddTypeS('TBrushStyle', '(bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross)');
end;

procedure SIRegister_Font(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TPersistent'), 'TFont') do
  begin
    RegisterMethod('constructor Create');
    RegisterProperty('Color', 'TColor', iptRW);
    RegisterProperty('Height', 'Integer', iptRW);
    RegisterProperty('Name', 'string', iptRW);
    RegisterProperty('Size', 'Integer', iptRW);
    RegisterProperty('Style', 'TFontStyles', iptrw);
  end;
end;

procedure SIRegister_Brush(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TPersistent'), 'TBrush') do
  begin
    RegisterMethod('constructor Create');
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Style', 'TBrushStyle', iptrw);
  end;
end;

procedure SIRegister_Pen(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TPersistent'), 'TPen') do
  begin
    RegisterMethod('constructor Create');
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Style', 'TPenStyle', iptrw);
    RegisterProperty('Width', 'Integer', iptrw);
  end;
end;

//!!!
procedure SIRegister_Graphics(Cl: TPSPascalCompiler);
begin
  SIRegister_GraphicsTypes(Cl);
  SIRegister_Font(Cl);
  SIRegister_Brush(Cl);
  SIRegister_Pen(Cl);
end;

procedure SIRegisterTControl(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TComponent'), 'TControl') do
  begin
    RegisterMethod('procedure Hide');
    RegisterMethod('procedure Show');
    RegisterMethod('procedure SetBounds(X,Y,w,h: Integer)');
    RegisterProperty('Left', 'Integer', iptRW);
    RegisterProperty('Top', 'Integer', iptRW);
    RegisterProperty('Width', 'Integer', iptRW);
    RegisterProperty('Height', 'Integer', iptRW);
    RegisterProperty('Visible', 'Boolean', iptRW);
    RegisterProperty('Enabled', 'Boolean', iptrw);
    RegisterProperty('BoundsRect', 'TRect', iptRW);

    RegisterProperty('Caption', 'String', iptRW);
    RegisterProperty('Color', 'TColor', iptRW);
    RegisterProperty('Font', 'TFont', iptRW);
    //RegisterProperty('ParentColor', 'Boolean', iptRW);
    RegisterProperty('ParentFont', 'Boolean', iptRW);

    RegisterProperty('TabOrder', 'Integer', iptRW);
    RegisterProperty('TabStop', 'Boolean', iptRW);

    RegisterProperty('OnChangeBounds', 'TNotifyEvent', iptRW);
    RegisterProperty('OnResize', 'TNotifyEvent', iptRW);
  end;
end;

procedure SIRegisterTWinControl(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TControl'), 'TWinControl') do
  begin
    with Cl.FindClass('TControl') do
    begin
      RegisterProperty('Parent', 'TWinControl', iptRW);
    end;
    RegisterProperty('Controls', 'TControl Integer', iptr);
    RegisterProperty('ControlCount', 'Integer', iptr);
  end;
end;

//!!!
procedure SIRegister_Controls(Cl: TPSPascalCompiler);
begin
  SIRegisterTControl(CL);
  SIRegisterTWinControl(CL);
end;

procedure SIRegister_Timer(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TComponent'), 'TTimer') do
  begin
    RegisterProperty('Enabled', 'Boolean', iptRW);
    RegisterProperty('Interval', 'Integer', iptRW);
    RegisterProperty('OnTimer', 'TNotifyEvent', iptRW);
  end;
end;

//!!!
procedure RegisterDatetimeLibrary_C(S: TPSPascalCompiler);
begin
  s.AddType('TDateTime', btDouble).ExportName := True;
  s.AddDelphiFunction('function EncodeDate(Year, Month, Day: Word): TDateTime;');
  s.AddDelphiFunction('function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;');
  s.AddDelphiFunction('function TryEncodeDate(Year, Month, Day: Word; var Date: TDateTime): Boolean;');
  s.AddDelphiFunction('function TryEncodeTime(Hour, Min, Sec, MSec: Word; var Time: TDateTime): Boolean;');
  s.AddDelphiFunction('procedure DecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word);');
  s.AddDelphiFunction('procedure DecodeTime(const DateTime: TDateTime; var Hour, Min, Sec, MSec: Word);');
  s.AddDelphiFunction('function DayOfWeek(const DateTime: TDateTime): Word;');
  s.AddDelphiFunction('function Date: TDateTime;');
  s.AddDelphiFunction('function Time: TDateTime;');
  s.AddDelphiFunction('function Now: TDateTime;');
  //s.AddDelphiFunction('function DateTimeToUnix(D: TDateTime): Int64;');
  //s.AddDelphiFunction('function UnixToDateTime(U: Int64): TDateTime;');

  s.AddDelphiFunction('function DateToStr(D: TDateTime): string;');
  s.AddDelphiFunction('function StrToDate(const S: string): TDateTime;');
  s.AddDelphiFunction('function FormatDateTime(const fmt: string; D: TDateTime): string;');
end;

////////////////////////////////////////////////////////////////////////////////

procedure SIRegister_Json(Cl: TPSPascalCompiler);
var
  JsonDataCls, JSONObjCls: TPSCompileTimeClass;
begin
  Cl.AddTypeS('TJSONtype', '(jtUnknown, jtNumber, jtString, jtBoolean, jtNull, jtArray, jtObject)');
  Cl.AddTypeS('TFormatOption', '(foSingleLineArray, foSingleLineObject, foDoNotQuoteMembers, foUseTabchar, foSkipWhiteSpace, foFormatFloat)');
  Cl.AddTypeS('TFormatOptions', 'set of TFormatOption');
  JsonDataCls := Cl.AddClassN(Cl.FindClass('TObject'), 'TJSONData');
  with JsonDataCls do
  begin
    //RegisterMethod('constructor Create; virtual');
    RegisterMethod('procedure Clear');
    RegisterMethod('function FindPath(Const APath : String) : TJSONdata');
    RegisterMethod('function GetPath(Const APath : String) : TJSONdata');
    RegisterMethod('function Clone : TJSONData');
    RegisterMethod('function JSONType: TJSONType');
    RegisterMethod('function FormatJSON(Options: TFormatOptions; Indentsize: Integer): String');

    RegisterProperty('Count', 'Integer', iptR);
    RegisterProperty('Items', 'TJSONData Integer', iptRW);
    SetDefaultPropery('Items');
    RegisterProperty('Value', 'variant', iptRW);
    RegisterProperty('AsString', 'String', iptRW);
    RegisterProperty('AsUnicodeString', 'UnicodeString', iptRW);
    RegisterProperty('AsFloat', 'Double', iptRW);
    RegisterProperty('AsInteger', 'Integer', iptRW);
    RegisterProperty('AsInt64', 'Int64', iptRW);
    //RegisterProperty('AsQWord', 'Int64', iptRW);        QWORD не поддерживается PS
    RegisterProperty('AsBoolean', 'Boolean', iptRW);
    RegisterProperty('IsNull', 'Boolean', iptR);
    RegisterProperty('AsJSON', 'String', iptR);
  end;
  JSONObjCls := Cl.AddClassN(JsonDataCls, 'TJSONObject');
  with Cl.AddClassN(JsonDataCls, 'TJSONArray') do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('constructor CreateArray(Elements: array of const)');
    RegisterMethod('function Add(Value: Variant): Integer');
    RegisterMethod('function AddArray(Arr: TJSONArray): Integer');
    RegisterMethod('function AddObject(Obj: TJSONObject): Integer');
    RegisterMethod('Procedure Delete(Index : Integer)');
    RegisterMethod('procedure Exchange(Index1, Index2: Integer)');
    RegisterMethod('procedure Insert(Index: Integer; Value: Variant)');
    RegisterMethod('procedure InsertArray(Index: Integer; Arr: TJSONArray)');
    RegisterMethod('procedure InsertObject(Index: Integer; Obj: TJSONObject)');
    RegisterMethod('Procedure Remove(Item : TJSONData)');
    RegisterMethod('function IndexOf(obj: TJSONData): Integer');
  end;
  with JSONObjCls do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('constructor CreateObject(Elements: array of const)');
    RegisterMethod('function Add(const AName: String; Value: Variant): Integer');
    RegisterMethod('function AddArray(const AName: String; Arr: TJSONArray): Integer');
    RegisterMethod('function AddObject(const AName: String; Obj: TJSONObject): Integer');
    RegisterMethod('Procedure Delete(Index : Integer)');
    RegisterMethod('Procedure Remove(Item : TJSONData)');
    RegisterMethod('function IndexOf(obj: TJSONData): Integer');
    RegisterMethod('Function IndexOfName(const AName: String): Integer');
    RegisterProperty('Names', 'String Integer', iptR);
    RegisterProperty('Elements', 'TJSONData String', iptRW);
    SetDefaultPropery('Elements');
  end;
end;

procedure SIRegister_Xml(Cl: TPSPascalCompiler);
var
  NodeListCls, DomNodeCls, DocCls: TPSCompileTimeClass;
begin
  Cl.AddConstantN('ELEMENT_NODE', 'LongInt').SetInt(1);
  Cl.AddConstantN('ATTRIBUTE_NODE', 'LongInt').SetInt(2);
  Cl.AddConstantN('TEXT_NODE', 'LongInt').SetInt(3);
  Cl.AddConstantN('CDATA_SECTION_NODE', 'LongInt').SetInt(4);
  Cl.AddConstantN('ENTITY_REFERENCE_NODE', 'LongInt').SetInt(5);
  Cl.AddConstantN('ENTITY_NODE', 'LongInt').SetInt(6);
  Cl.AddConstantN('PROCESSING_INSTRUCTION_NODE', 'LongInt').SetInt(7);
  Cl.AddConstantN('COMMENT_NODE', 'LongInt').SetInt(8);
  Cl.AddConstantN('DOCUMENT_NODE', 'LongInt').SetInt(9);
  Cl.AddConstantN('DOCUMENT_TYPE_NODE', 'LongInt').SetInt(10);
  Cl.AddConstantN('DOCUMENT_FRAGMENT_NODE', 'LongInt').SetInt(11);
  Cl.AddConstantN('NOTATION_NODE', 'LongInt').SetInt(12);

  Cl.AddTypeS('TXMLReaderFlag', '(xrfAllowLowerThanInAttributeValue, xrfAllowSpecialCharsInAttributeValue, xrfAllowSpecialCharsInComments, xrfPreserveWhiteSpace)');
  Cl.AddTypeS('TXMLReaderFlags', 'set of TXMLReaderFlag');
  Cl.AddTypeS('TXMLWriterFlag', '(xwfSpecialCharsInAttributeValue, xwfPreserveWhiteSpace)');
  Cl.AddTypeS('TXMLWriterFlags', 'set of TXMLWriterFlag');

  NodeListCls := Cl.AddClassN(Cl.FindClass('TObject'), 'TDOMNodeList');
  DomNodeCls := Cl.AddClassN(Cl.FindClass('TObject'), 'TDOMNode');
  DocCls := Cl.AddClassN(DomNodeCls, 'TXmlDocument');

  with DomNodeCls do
  begin
    RegisterProperty('NodeName', 'String', iptR);
    RegisterProperty('NodeValue', 'String', iptRW);
    RegisterProperty('NodeType', 'Integer', iptR);
    RegisterProperty('ParentNode', 'TDOMNode', iptR);
    RegisterProperty('FirstChild', 'TDOMNode', iptR);
    RegisterProperty('LastChild', 'TDOMNode', iptR);
    RegisterProperty('ChildNodes', 'TDOMNodeList', iptR);
    RegisterProperty('PreviousSibling', 'TDOMNode', iptR);
    RegisterProperty('NextSibling', 'TDOMNode', iptR);
    RegisterProperty('OwnerDocument', 'TXmlDocument', iptR);
    RegisterProperty('AttrCount', 'LongWord', iptR);
    RegisterProperty('Attrs', 'String String', iptRW);
    SetDefaultPropery('Attrs');
    RegisterProperty('Attr', 'TDOMNode LongWord', iptR);

    RegisterMethod('function InsertBefore(NewChild, RefChild: TDOMNode): TDOMNode; virtual');
    RegisterMethod('function ReplaceChild(NewChild, OldChild: TDOMNode): TDOMNode; virtual');
    //RegisterMethod('function DetachChild(OldChild: TDOMNode): TDOMNode; virtual');
    RegisterMethod('function RemoveChild(OldChild: TDOMNode): TDOMNode');
    RegisterMethod('function AppendChild(NewChild: TDOMNode): TDOMNode');
    RegisterMethod('function HasChildNodes: Boolean; virtual');
    RegisterMethod('function CloneNode(deep: Boolean): TDOMNode; virtual');
    RegisterMethod('function GetLevel: Longint');
    RegisterMethod('function FindNode(const ANodeName: String): TDOMNode; virtual');
    RegisterMethod('function RemoveAttr(const AName: String): TDOMNode');
    RegisterMethod('function AttrExists(const AName: String): Boolean');
  end;

  with DocCls do
  begin
    RegisterProperty('Root', 'TDOMNode', iptR);
    RegisterMethod('constructor Create');
    RegisterMethod('function CreateNode(const NodeName: String): TDOMNode');
    RegisterMethod('function CreateText(const AText: String): TDOMNode');
    RegisterMethod('function CreateCDATA(const Data: String): TDOMNode');
  end;

  with NodeListCls do
  begin
    RegisterProperty('Item', 'TDomNode LongWord', iptR);
    SetDefaultPropery('Item');
    RegisterProperty('Count', 'LongWord', iptR);
  end;
end;

procedure SIRegister_Template(Cl: TPSPascalCompiler);
begin
  //Cl.AddTypeS('TGetParamEvent', 'Procedure(Sender : TObject; Const ParamName : String; var AValue : String)');
  Cl.AddTypeS('TReplaceTagEvent', 'Procedure(Sender : TObject; Const TagString : String; TagParams:TStringList; var ReplaceText : String)');

  with Cl.AddClassN(Cl.FindClass('TPersistent'), 'TTemplate') do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('Function HasContent: Boolean');
    RegisterMethod('Function GetContent: String');
    RegisterMethod('procedure ClearTags');
    RegisterProperty('StartDelimiter', 'String', iptRW);
    RegisterProperty('EndDelimiter', 'String', iptRW);
    RegisterProperty('ParamStartDelimiter', 'String', iptRW);
    RegisterProperty('ParamEndDelimiter', 'String', iptRW);
    RegisterProperty('ParamValueSeparator', 'String', iptRW);
    RegisterProperty('FileName', 'String', iptRW);
    RegisterProperty('Template', 'String', iptRW);
    //RegisterProperty('OnGetParam', 'TGetParamEvent', iptRW);
    RegisterProperty('OnReplaceTag', 'TReplaceTagEvent', iptRW);
    //RegisterProperty('AllowTagParams', 'Boolean', iptRW);
    RegisterProperty('Tags', 'Variant String', iptRW);
    RegisterProperty('TagByIndex', 'Variant Integer', iptRW);
    RegisterProperty('TagCount', 'Integer', iptR);
  end;
end;

procedure SIRegister_HttpClient(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TRedirectEvent', 'Procedure (Sender : TObject; Const ASrc : String; Var ADest: String)');
  Cl.AddTypeS('TPasswordEvent', 'Procedure (Sender : TObject; Var RepeatRequest : Boolean)');
  Cl.AddTypeS('TDataEvent', 'Procedure (Sender : TObject; Const ContentLength, CurrentPos : Int64)');
  Cl.AddTypeS('THttpClientErrorEvent', 'procedure (Sender: TObject; const ErrorMsg: String)');

  with Cl.AddClassN(Cl.FindClass('TPersistent'), 'TProxyData') do
  begin
    RegisterProperty('Host', 'string', iptRW);
    RegisterProperty('Port', 'Word', iptRW);
    RegisterProperty('UserName', 'String', iptRW);
    RegisterProperty('Password', 'String', iptRW);
  end;

  with Cl.AddClassN(Cl.FindClass('TObject'), 'THttpClient') do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('Procedure Terminate');
    RegisterMethod('Function IndexOfHeader(Const AHeader : String) : Integer)');
    RegisterMethod('Procedure AddHeader(Const AHeader,AValue : String)');
    RegisterMethod('Function  GetHeader(Const AHeader : String) : String');
    //RegisterMethod('Procedure HTTPMethod(Const AMethod,AURL : String; Stream : TStream; Const AllowedResponseCodes : Array of Integer); virtual');
    RegisterMethod('procedure Send(const AMethod, AURL: String)');
    RegisterMethod('Procedure FormPost(const URL : string; FormData:  TStrings)');
    RegisterMethod('procedure StreamFormPost(const AURL: string; FormData: TStrings; const AFieldName, AFileName: string; const AStream: TStream)');
    RegisterProperty('Terminated', 'Boolean', iptR);

    RegisterProperty('IOTimeout', 'Integer', iptRW);
    RegisterProperty('RequestHeaders', 'TStrings', iptRW);
    RegisterProperty('Cookies', 'TStrings', iptRW);
    RegisterProperty('RequestBody', 'TStream', iptRW);
    RegisterProperty('HTTPversion', 'String', iptRW);
    RegisterProperty('ResponseHeaders', 'TStrings', iptR);
    RegisterProperty('ServerHTTPVersion', 'String', iptR);
    RegisterProperty('ResponseStatusCode', 'Integer', iptR);
    RegisterProperty('ResponseStatusText', 'String', iptR);
    RegisterProperty('AllowRedirect', 'Boolean', iptRW);
    RegisterProperty('MaxRedirects', 'Byte', iptRW);
    RegisterProperty('OnRedirect', 'TRedirectEvent', iptRW);
    RegisterProperty('Proxy', 'TProxyData', iptRW);
    RegisterProperty('UserName', 'String', iptRW);
    RegisterProperty('Password', 'String', iptRW);
    RegisterProperty('Connected', 'Boolean', iptR);
    RegisterProperty('KeepConnection', 'Boolean', iptRW);
    RegisterProperty('OnPassword', 'TPasswordEvent', iptRW);
    RegisterProperty('OnDataReceived', 'TDataEvent', iptRW);
    RegisterProperty('OnHeaders', 'TNotifyEvent', iptRW);

    RegisterProperty('ContentStream', 'TStream', iptRW);
    RegisterProperty('Content', 'String', iptR);
    RegisterProperty('OnFinish', 'TNotifyEvent', iptRW);
    RegisterProperty('OnError', 'THttpClientErrorEvent', iptRW);
    RegisterProperty('MultiThreaded', 'Boolean', iptRW);
    RegisterProperty('ConnectionCount', 'Integer', iptR);
  end;
end;

procedure SIRegister_HttpServer(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TObject'), 'THttpHeader') do
  begin
    RegisterProperty('Accept', 'String', iptRW);
    RegisterProperty('AcceptCharset', 'String', iptRW);
    RegisterProperty('AcceptEncoding', 'String', iptRW);
    RegisterProperty('AcceptLanguage', 'String', iptRW);
    RegisterProperty('Authorization', 'String', iptRW);
    RegisterProperty('Connection', 'String', iptRW);
    RegisterProperty('ContentEncoding', 'String', iptRW);
    RegisterProperty('ContentLanguage', 'String', iptRW);
    RegisterProperty('ContentLength', 'Integer', iptRW);
    RegisterProperty('ContentType', 'String', iptRW);
    RegisterProperty('Date', 'String', iptRW);
    RegisterProperty('Expires', 'String', iptRW);
    RegisterProperty('From', 'String', iptRW);
    RegisterProperty('Host', 'String', iptRW);
    RegisterProperty('IfModifiedSince', 'String', iptRW);
    RegisterProperty('LastModified', 'String', iptRW);
    RegisterProperty('Location', 'String', iptRW);
    RegisterProperty('Pragma', 'String', iptRW);
    RegisterProperty('Referer', 'String', iptRW);
    RegisterProperty('RetryAfter', 'String', iptRW);
    RegisterProperty('Server', 'String', iptRW);
    RegisterProperty('UserAgent', 'String', iptRW);
    RegisterProperty('Warning', 'String', iptRW);
    RegisterProperty('WWWAuthenticate', 'String', iptRW);
    RegisterProperty('Via', 'String', iptRW);
    // Headers, not in HTTP spec.
    RegisterProperty('Cookie', 'String', iptRW);
    RegisterProperty('SetCookie', 'String', iptRW);
    RegisterProperty('HTTPXRequestedWith', 'String', iptRW);
    RegisterProperty('HttpVersion', 'String', iptRW);
    RegisterProperty('ProtocolVersion', 'String', iptRW);
    // Specials, mostly from CGI protocol/Apache.
    RegisterProperty('PathInfo', 'String', iptRW);
    RegisterProperty('PathTranslated', 'String', iptRW);
    RegisterProperty('RemoteAddr', 'String', iptRW);
    RegisterProperty('RemoteHost', 'String', iptRW);
    RegisterProperty('ScriptName', 'String', iptRW);
    RegisterProperty('ServerPort', 'Word', iptRW);
    RegisterProperty('Method', 'String', iptRW);
    RegisterProperty('URL', 'String', iptRW);
    RegisterProperty('Query', 'String', iptRW);
    RegisterProperty('Content', 'String', iptRW);
    // Lists
    RegisterProperty('CookieFields', 'TStrings', iptRW);
    RegisterProperty('ContentFields', 'TStrings', iptR);
    RegisterProperty('QueryFields', 'TStrings', iptR);
    RegisterProperty('CustomHeaders', 'TStringList', iptR);
  end;

  with Cl.AddClassN(Cl.FindClass('TCollectionItem'), 'TUploadedFile') do
  begin
    RegisterProperty('FieldName', 'String', iptRW);
    RegisterProperty('FileName', 'String', iptRW);
    RegisterProperty('Stream', 'TStream', iptR);
    RegisterProperty('Size', 'Int64', iptRW);
    RegisterProperty('ContentType', 'String', iptRW);
    RegisterProperty('Disposition', 'String', iptRW);
    RegisterProperty('LocalFileName', 'String', iptRW);
    RegisterProperty('Description', 'String', iptRW);
  end;

  with Cl.AddClassN(Cl.FindClass('TCollection'), 'TUploadedFiles') do
  begin
    RegisterProperty('Files', 'TUploadedFile Integer', iptR);
    SetDefaultPropery('Files');
  end;

  with Cl.AddClassN(Cl.FindClass('THttpHeader'), 'TFPHTTPConnectionRequest') do
  begin
    RegisterProperty('Files', 'TUploadedFiles', iptR);
  end;

  with Cl.AddClassN(Cl.FindClass('TCollectionItem'), 'TCookie') do
  begin
    RegisterMethod('procedure Expire');
    RegisterProperty('Name', 'string', iptRW);
    RegisterProperty('Value', 'string', iptRW);
    RegisterProperty('Domain', 'string', iptRW);
    RegisterProperty('Path', 'string', iptRW);
    RegisterProperty('Expires', 'TDateTime', iptRW);
    RegisterProperty('Secure', 'Boolean', iptRW);
    RegisterProperty('HttpOnly', 'Boolean', iptRW);
    RegisterProperty('AsString', 'String', iptR);
  end;

  with Cl.AddClassN(Cl.FindClass('TCollection'), 'TCookies') do
  begin
    RegisterMethod('function  Add: TCookie');
    RegisterMethod('Function CookieByName(AName : String) : TCookie');
    RegisterMethod('Function FindCookie(AName : String): TCookie');
    RegisterMethod('Function IndexOfCookie(AName : String) : Integer');
    RegisterProperty('Items', 'TCookie Integer', iptR);
    SetDefaultPropery('Items');
  end;

  with Cl.AddClassN(Cl.FindClass('THttpHeader'), 'TFPHTTPConnectionResponse') do
  begin
    RegisterMethod('Procedure SendContent');
    RegisterMethod('Procedure SendHeaders');
    RegisterProperty('Code', 'Integer', iptRW);
    RegisterProperty('CodeText', 'String', iptRW);
    RegisterProperty('Age', 'String', iptRW);
    RegisterProperty('Allow', 'String', iptRW);
    RegisterProperty('CacheControl', 'String', iptRW);
    RegisterProperty('ContentLocation', 'String', iptRW);
    RegisterProperty('ContentMD5', 'String', iptRW);
    RegisterProperty('ContentRange', 'String', iptRW);
    RegisterProperty('ETag', 'String', iptRW);
    RegisterProperty('ProxyAuthenticate', 'String', iptRW);
    RegisterProperty('RetryAfter', 'String', iptRW);
    RegisterProperty('FirstHeaderLine', 'String', iptRW);
    RegisterProperty('ContentStream', 'TStream', iptRW);
    RegisterProperty('Content', 'String', iptRW);
    RegisterProperty('Contents', 'TStrings', iptRW);
    RegisterProperty('HeadersSent', 'Boolean', iptR);
    RegisterProperty('ContentSent', 'Boolean', iptR);
    RegisterProperty('Cookies', 'TCookies', iptR);
    RegisterProperty('FreeContentStream', 'Boolean', iptRW);
  end;

  Cl.AddTypeS('TWebServerRequestHandler',
    'function (Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse : TFPHTTPConnectionResponse): Boolean');
  //Cl.AddTypeS('THttpServerErrorHandler', 'procedure (Sender: TObject; const ErrorMsg: String)');

  {with Cl.AddClassN(Cl.FindClass('TObject'), 'THttpServer') do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('procedure Start');
    RegisterProperty('IOTimeout', 'Integer', iptRW);
    //RegisterProperty('AcceptIdleTimeout', 'Integer', iptRW);
    RegisterProperty('Active', 'Boolean', iptR);
    //RegisterProperty('Address', 'String', iptRW);
    RegisterProperty('Port', 'Word', iptRW);
    RegisterProperty('OnRequest', 'THTTPServerRequestHandler', iptRW);
    RegisterProperty('OnError', 'THTTPServerErrorHandler', iptRW);
  end; }
end;

procedure SIRegister_IniFiles(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TIniFileOption', '(ifoStripComments, ifoStripInvalid, ifoEscapeLineFeeds, ifoCaseSensitive, ifoStripQuotes, ifoFormatSettingsActive)');
	Cl.AddTypeS('TIniFileOptions', 'Set of TIniFileOption');
  with Cl.AddClassN(Cl.FindClass('TObject'), 'TIniFile') do
  begin
    RegisterMethod('constructor Create(const AFileName: string; AOptions : TIniFileOptions)');
    RegisterMethod('function SectionExists(const Section: string): Boolean');
    RegisterMethod('function ReadString(const Section, Ident, Default: string): string');
    RegisterMethod('procedure WriteString(const Section, Ident, Value: String)');
    RegisterMethod('function ReadInteger(const Section, Ident: string; Default: Longint): Longint');
    RegisterMethod('procedure WriteInteger(const Section, Ident: string; Value: Longint)');
    RegisterMethod('function ReadInt64(const Section, Ident: string; Default: Int64): Int64');
    RegisterMethod('procedure WriteInt64(const Section, Ident: string; Value: Int64)');
    RegisterMethod('function ReadBool(const Section, Ident: string; Default: Boolean): Boolean');
    RegisterMethod('procedure WriteBool(const Section, Ident: string; Value: Boolean)');
    RegisterMethod('function ReadDate(const Section, Ident: string; Default: TDateTime): TDateTime');
    RegisterMethod('function ReadDateTime(const Section, Ident: string; Default: TDateTime): TDateTime');
    RegisterMethod('function ReadFloat(const Section, Ident: string; Default: Double): Double');
    RegisterMethod('function ReadTime(const Section, Ident: string; Default: TDateTime): TDateTime');
    RegisterMethod('function ReadBinaryStream(const Section, Name: string; Value: TStream): Integer');
    RegisterMethod('procedure WriteDate(const Section, Ident: string; Value: TDateTime)');
    RegisterMethod('procedure WriteDateTime(const Section, Ident: string; Value: TDateTime)');
    RegisterMethod('procedure WriteFloat(const Section, Ident: string; Value: Double)');
    RegisterMethod('procedure WriteTime(const Section, Ident: string; Value: TDateTime)');
    RegisterMethod('procedure WriteBinaryStream(const Section, Name: string; Value: TStream)');
    RegisterMethod('procedure ReadSection(const Section: string; Strings: TStrings)');
    RegisterMethod('procedure ReadSections(Strings: TStrings)');
    RegisterMethod('procedure ReadSectionValues(const Section: string; Strings: TStrings)');
    RegisterMethod('procedure EraseSection(const Section: string)');
    RegisterMethod('procedure DeleteKey(const Section, Ident: String)');
    RegisterMethod('procedure UpdateFile');
    RegisterMethod('function ValueExists(const Section, Ident: string): Boolean');
    RegisterProperty('FileName', 'string', iptR);
    RegisterProperty('EscapeLineFeeds', 'boolean', iptR);
    RegisterProperty('CaseSensitive', 'Boolean', iptRW);
    RegisterProperty('StripQuotes', 'Boolean', iptRW);

    RegisterMethod('procedure ReadSectionRaw(const Section: string; Strings: TStrings)');
    RegisterProperty('CacheUpdates', 'Boolean', iptRW);
  end;
end;

procedure SIRegister_dxSQLQuery(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TUseGeneratorOption', '(ugNotUse, ugAppend, ugApplyUpdates)');
  with Cl.AddClassN(Cl.FindClass('TObject'), 'TdxSQLQuery') do
  begin
    {RegisterMethod('constructor Create(const SQL: String)');
    RegisterMethod('procedure Open');
    RegisterMethod('procedure Close');
    RegisterMethod('function Opened: Boolean');}
    RegisterMethod('procedure Append');
    RegisterMethod('procedure Edit');
    RegisterMethod('procedure Delete');
    RegisterMethod('procedure Cancel');
    RegisterMethod('procedure Post');
    RegisterMethod('procedure ApplyUpdates');
    RegisterMethod('procedure CancelUpdates');
    RegisterMethod('procedure MoveFirst');
    RegisterMethod('procedure MovePrior');
    RegisterMethod('procedure MoveNext');
    RegisterMethod('procedure MoveLast');
    RegisterMethod('procedure MoveBy(Distance: Integer)');
    RegisterMethod('procedure MoveTo(aRecNo: Integer)');
    RegisterMethod('function BOF: Boolean');
    RegisterMethod('function EOF: Boolean');
    RegisterMethod('function RecNo: Integer');
    RegisterMethod('function RecordCount: Integer');
    RegisterMethod('function FieldCount: Integer');
    RegisterMethod('function Locate(const FieldNames: String; FieldValues: array of Variant; Options: TLocateOptions): Boolean');
    RegisterMethod('procedure LoadFromStream(const FieldName: String; Stream: TStream)');
    RegisterMethod('procedure SaveToStream(const FieldName: String; Stream: TStream)');
    RegisterProperty('Fields', 'Variant String', iptRW);
    SetDefaultPropery('Fields');
    RegisterProperty('Field', 'TField Integer', iptR);
    RegisterProperty('AsI', 'Integer String', iptRW);
    RegisterProperty('AsF', 'Double String', iptRW);
    RegisterProperty('AsDT', 'TDateTime String', iptRW);
    RegisterProperty('AsS', 'String String', iptRW);
    RegisterProperty('State', 'TDataSetState', iptR);
    RegisterProperty('UseGenerator', 'TUseGeneratorOption', iptRW);
    RegisterProperty('UseExecuteBlock', 'Boolean', iptRW);
  end;
end;

procedure SIRegister_dxTypes(Cl: TPSPascalCompiler);
begin
  Cl.AddClassN(Cl.FindClass('TWinControl'), 'TdxForm');
  Cl.AddTypeS('TStringArray', 'array of String');
  Cl.AddTypeS('TViewType', '(vtGridTop, vtGridBottom, vtGridLeft, vtGridRight, vtGridOnly, vtWithoutGrid, vtSimpleForm, vtDefault)');
  Cl.AddTypeS('TCreateFormEvent', 'procedure (Sender: TObject; Fm: TdxForm)');
  Cl.AddTypeS('TValidateEvent', 'procedure (Sender: TObject; var Ok: Boolean)');
  Cl.AddTypeS('TFieldChangeEvent', 'procedure (Sender, Control: TObject; const FieldName: String)');
  Cl.AddTypeS('TAccessStatus', '(asOk, asCantAppend, asCantEdit, asCantDelete, asModified, asDeleted, asLocked, asHasRef)');
  Cl.AddTypeS('TPrintAction', '(paBeginPrint, paEndPrint, paPrintField, paBeginData, paNextData, paBeforeOpenFile, paAfterOpenFile, paPrintError)');
  Cl.AddTypeS('TPrintEvent', 'procedure (Sender: TObject; Action: TPrintAction;	const SourceName, FieldName: String; var Value: String; var Accept: Boolean)');
  Cl.AddTypeS('TVariantArray2d', 'array of array of Variant');
  Cl.AddTypeS('TParamNotifyEvent', 'procedure (Sender: TObject; const ParamName: String)');
  Cl.AddTypeS('TGotoOption', '(gtoDefault, gtoReplaceUrl, gtoNewTab)');

  with Cl.AddClassN(Cl.FindClass('TObject'), 'TParamList') do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('procedure Clear');
    RegisterMethod('function ParamExists(const aName: String): Boolean');
    RegisterProperty('Values', 'Variant String', iptRW);
    SetDefaultPropery('Values');
    RegisterProperty('Objects', 'TObject String', iptRW);
    RegisterProperty('Names', 'String Integer', iptR);
    RegisterProperty('ValueFromIndex', 'Variant Integer', iptRW);
    RegisterProperty('ObjectFromIndex', 'TObject Integer', iptRW);
    RegisterProperty('Count', 'Integer', iptR);
    RegisterProperty('OnGetParam', 'TParamNotifyEvent', iptRW);
    RegisterProperty('OnSetParam', 'TParamNotifyEvent', iptRW);
  end;
end;

procedure SIRegister_TFIELD(CL: TPSPascalCompiler);
Begin
  cl.AddTypeS('TFieldType', '(ftUnknown, ftString, ftSmallint, ftInteger, ftWord, ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,'+
    'ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,'+
    'ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd)');
  //cl.AddTypeS('TFieldChars', 'set of Char');
  cl.addTypeS('TLocateOption','(loCaseInsensitive, loPartialKey)');
  cl.addtypes('TLocateOptions','set of TLocateOption');
  Cl.AddTypeS('TDataSetState', '(dsInactive, dsBrowse, dsEdit, dsInsert, dsSetKey, dsCalcFields, dsFilter, dsNewValue, dsOldValue, dsCurValue, dsBlockRead, dsInternalCalc, dsOpening)');

  With Cl.AddClassN(Cl.FindClass('TComponent'), 'TField') do
  begin
    RegisterMethod('procedure Clear; virtual');
    RegisterMethod('procedure FocusControl');
    //RegisterMethod('function IsValidChar(InputChar: Char): Boolean');
    RegisterProperty('AsBoolean', 'Boolean', iptrw);
    RegisterProperty('AsCurrency', 'Currency', iptrw);
    RegisterProperty('AsDateTime', 'TDateTime', iptrw);
    RegisterProperty('AsFloat', 'Double', iptrw);
    RegisterProperty('AsInteger', 'LongInt', iptrw);
    RegisterProperty('AsString', 'string', iptrw);
    RegisterProperty('AsVariant', 'Variant', iptrw);
    RegisterProperty('CanModify', 'Boolean', iptr);
    RegisterProperty('DataType', 'TFieldType', iptr);
    //RegisterProperty('EditMask', 'String', iptrw);
    RegisterProperty('IsNull', 'Boolean', iptr);
    RegisterProperty('OldValue', 'Variant', iptr);
    //RegisterProperty('Text', 'string', iptrw);
    //RegisterProperty('ValidChars', 'TFieldChars', iptrw);
    RegisterProperty('Value', 'Variant', iptrw);
    RegisterProperty('Alignment', 'TAlignment', iptrw);
    RegisterProperty('ReadOnly', 'Boolean', iptrw);
    //RegisterProperty('DisplayFormat', 'String', iptrw);
    //RegisterProperty('EditFormat', 'String', iptrw);
    //RegisterProperty('InsertState', 'Boolean', iptr);
    //RegisterProperty('EditState', 'Boolean', iptr);
    RegisterProperty('State', 'TDataSetState', iptr);
    RegisterProperty('FieldName', 'String', iptr);
  end;
end;

procedure SIRegister_CSVData(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TObject'), 'TCsvData') do
  begin
    RegisterMethod('constructor Create');
    RegisterMethod('procedure LoadFromFile(const AFileName: String; FromANSI: Boolean)');
    RegisterMethod('procedure LoadFromStream(AStream: TStream; FromANSI: Boolean)');
    RegisterMethod('procedure SaveToFile(const AFileName: String; ToANSI: Boolean)');
    RegisterMethod('procedure SavetoStream(AStream: TStream; ToANSI: Boolean)');
    RegisterProperty('RowCount', 'Integer', iptRW);
    RegisterProperty('ColCount', 'Integer', iptRW);
    RegisterProperty('Cells', 'String Integer Integer', iptRW);
    SetDefaultPropery('Cells');
    RegisterProperty('Delimiter', 'Char', iptRW);
  end;
end;

procedure SIRegister_dxForm(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TObject'), 'TFilterField') do
  begin
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('IsNot', 'Boolean', iptRW);
    RegisterProperty('IsNull', 'Boolean', iptRW);
    RegisterProperty('Values', 'TStringList', iptR);
    RegisterProperty('Value', 'String Integer', iptR);
		RegisterProperty('EndValue', 'String Integer', iptR);
  end;

  with Cl.AddClassN(Cl.FindClass('TObject'), 'TFilterObject') do
  begin
  	RegisterMethod('function AddField(const FieldName: String): TFilterField');
    RegisterMethod('function FindField(const FieldName: String): TFilterField');
    RegisterMethod('procedure DeleteField(F: TFilterField)');
    RegisterMethod('procedure Clear');
    RegisterProperty('Fields', 'TFilterField Integer', iptR);
    SetDefaultPropery('Fields');
    RegisterProperty('Count', 'Integer', iptR);
  end;

  Cl.AddTypeS('TMsgDlgType', '( mtWarning, mtError, mtInformation, mtConfirmation, mtCustom )');
  Cl.AddTypeS('TMsgDlgBtn', '( mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp )');
  Cl.AddTypeS('TMsgDlgButtons', 'set of TMsgDlgBtn');
  Cl.AddTypeS('TMsgButtonClickEvent', 'procedure (Sender: TObject; Button: TMsgDlgBtn)');
  with Cl.FindClass('TdxForm') do
  begin
    //RegisterMethod('constructor Create(FormName: String)');
    RegisterMethod('procedure Free');
    RegisterMethod('function Append: TAccessStatus');
    RegisterMethod('function Insert: TAccessStatus');
    RegisterMethod('function Edit: TAccessStatus');
    RegisterMethod('function Delete: TAccessStatus');
    RegisterMethod('procedure Post');
    RegisterMethod('procedure Cancel');
    RegisterMethod('procedure Refresh');
    RegisterMethod('procedure MoveFirst');
    RegisterMethod('procedure MovePrior');
    RegisterMethod('procedure MoveNext');
    RegisterMethod('procedure MoveLast');
    RegisterMethod('procedure MoveBy(Distance: Integer)');
    RegisterMethod('procedure MoveTo(aRecNo: Integer)');
    RegisterMethod('function Bof: Boolean');
    RegisterMethod('function Eof: Boolean');
    RegisterMethod('function RecNo: Integer');
    RegisterMethod('function RecId: Integer');
    RegisterMethod('function RecordCount: Integer');
    RegisterMethod('function Print(const TemplateName, OutFileName: String; out Errs: String; aOpenFile: Boolean): String');
    RegisterMethod('function Locate(const FieldNames: String; FieldValues: array of Variant; Options: TLocateOptions): Boolean');
    RegisterMethod('function GotoRecord(aRecId: Integer): Boolean');
    RegisterMethod('function CanAppend: TAccessStatus');
    RegisterMethod('function CanEdit: TAccessStatus');
    RegisterMethod('function CanDelete: TAccessStatus');
    RegisterMethod('procedure Open');
    RegisterMethod('procedure OpenRecord(RecId: Integer)');
    RegisterMethod('procedure OpenRecords(const Filter: String; Form: TdxForm; SelCond: Boolean)');
    RegisterMethod('function Opened: Boolean');
    RegisterMethod('procedure Close');
    RegisterMethod('function Validate: Boolean');
    RegisterMethod('function FindComponentByFieldName(const FieldName: String): TComponent');
    RegisterMethod('procedure DisableScrollEvents');
    RegisterMethod('procedure EnableScrollEvents');
    RegisterMethod('function ScrollEventsDisabled: Boolean');
    RegisterMethod('function GetRecordsCaption: String');
    RegisterMethod('function GetRecordCaption: String');
    RegisterMethod('function WhoEdit(ARecId: Integer): String');
    RegisterMethod('procedure GotoForm(const AFormName: String; ARecId: Integer; AGotoOption: TGotoOption)');
    RegisterMethod('procedure GotoReport(const AReportName: String; AGotoOption: TGotoOption)');
    RegisterMethod('procedure GotoUrl(const Url: String; AGotoOption: TGotoOption)');
    RegisterMethod('procedure MessageDlg(const Title, Msg: String; MsgType: TMsgDlgType; Buttons: TMsgDlgButtons; ClickHandler: TMsgButtonClickEvent)');
    RegisterMethod('procedure MsgBox(const Title, Msg: String)');

    RegisterProperty('Fields', 'Variant String', iptRW);
    SetDefaultPropery('Fields');
    RegisterProperty('Field', 'TField String', iptR);
    RegisterProperty('AsI', 'Integer String', iptR);
    RegisterProperty('AsF', 'Double String', iptR);
    RegisterProperty('AsDT', 'TDateTime String', iptR);
    RegisterProperty('AsS', 'String String', iptR);
    RegisterProperty('OldValues', 'Variant String', iptR);
    RegisterProperty('State', 'TDataSetState', iptR);
    RegisterProperty('Forms', 'TdxForm String', iptR);
    RegisterProperty('FormByIndex', 'TdxForm Integer', iptR);
    RegisterProperty('FormCount', 'Integer', iptR);
    RegisterProperty('Params', 'TParamList', iptR);
    RegisterProperty('ParentForm', 'TdxForm', iptR);
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('PId', 'Integer', iptR);
    RegisterProperty('FormCaption', 'String', iptR);
    RegisterProperty('Filter', 'TFilterObject', iptR);
    RegisterProperty('Modified', 'Boolean', iptR);
    RegisterProperty('ViewType', 'TViewType', iptR);
    RegisterProperty('CustomFilter', 'String', iptRW);
    RegisterProperty('CustomFilterForm', 'TdxForm', iptRW);
    RegisterProperty('UseSelectCondition', 'Boolean', iptRW);
    RegisterProperty('RecordsCaption', 'String', iptRW);
    RegisterProperty('RecordCaption', 'String', iptRW);
    RegisterProperty('Images', 'TdxDBImage String', iptR);
    RegisterProperty('Files', 'TdxFile String', iptR);
    Cl.AddTypeS('TLockMode', '(lmNoLock, lmPessimistic)');
    RegisterProperty('LockMode', 'TLockMode', iptR);
    RegisterProperty('ActionResult', 'Variant', iptRW);
    RegisterProperty('Msgs', 'TStringList', iptRW);

    RegisterProperty('OnAfterCancel', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterClose', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterDelete', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterEdit', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterInsert', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterOpen', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterPost', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterScroll', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterDuplicate', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeCancel', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeClose', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeDelete', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeEdit', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeInsert', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeOpen', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforePost', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeScroll', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeDuplicate', 'TNotifyEvent', iptRW);
    RegisterProperty('OnValidate', 'TValidateEvent', iptRW);
    RegisterProperty('OnFieldChange', 'TFieldChangeEvent', iptRW);
    RegisterProperty('OnPrint', 'TPrintEvent', iptRW);
    RegisterProperty('OnStateChange', 'TNotifyEvent', iptRW);
    RegisterProperty('OnDestroy', 'TNotifyEvent', iptRW);
    //RegisterProperty('OnMsgButtonClick', 'TMsgButtonClickEvent', iptRW);
    RegisterProperty('OnShowForm', 'TNotifyEvent', iptRW);
  end;
end;

procedure SIRegister_dxQueryGrid(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxQueryGrid') do
  begin
    RegisterMethod('procedure MoveFirst');
    RegisterMethod('procedure MovePrior');
    RegisterMethod('procedure MoveNext');
    RegisterMethod('procedure MoveLast');
    RegisterMethod('procedure MoveBy(Distance: Integer)');
    RegisterMethod('procedure MoveTo(aRecNo: Integer)');
    RegisterMethod('function EOF: Boolean');
    RegisterMethod('function BOF: Boolean');
    RegisterMethod('function RecNo: Integer');
    RegisterMethod('function RecId: Integer');
    RegisterMethod('function RecordCount: Integer');
    RegisterMethod('function Locate(const FieldNames: String; FieldValues: array of Variant; aOptions: TLocateOptions): Boolean');
    RegisterMethod('function GotoRecord(aRecId: Integer): Boolean');
    RegisterMethod('procedure Refresh');
    RegisterMethod('procedure Close');
    RegisterMethod('procedure DisableScrollEvents');
    RegisterMethod('procedure EnableScrollEvents');
    RegisterMethod('function ScrollEventsDisabled: Boolean');
    //RegisterMethod('function FindColumnByTitle(const ATitle: String): TdxColumn');

    RegisterProperty('QueryName', 'String', iptR);
    RegisterProperty('Fields', 'Variant String', iptR);
    SetDefaultPropery('Fields');
    RegisterProperty('AsI', 'Integer String', iptR);
    RegisterProperty('AsF', 'Double String', iptR);
    RegisterProperty('AsDT', 'TDateTime String', iptR);
    RegisterProperty('AsS', 'String String', iptR);
    RegisterProperty('ManualRefresh', 'Boolean', iptRW);
    RegisterProperty('Editable', 'Boolean', iptR);

    RegisterProperty('OnAfterClose', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterOpen', 'TNotifyEvent', iptRW);
    RegisterProperty('OnAfterScroll', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeClose', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeOpen', 'TNotifyEvent', iptRW);
    RegisterProperty('OnBeforeScroll', 'TNotifyEvent', iptRW);
    RegisterProperty('OnCreateForm', 'TCreateFormEvent', iptRW);

    RegisterMethod('function GetSourceFileName(const FieldName: String): String');
    RegisterMethod('function GetStoredFileName(const FieldName: String): String');
    RegisterMethod('procedure SaveToStream(const FieldName: String; St: TStream)');
    RegisterMethod('procedure SaveToFile(const FieldName, FileName: String)');
    RegisterMethod('procedure SaveThumbnailToStream(const FieldName: String; St: TStream)');
  end;

  with Cl.FindClass('TdxForm') do
  begin
  	RegisterProperty('Queries', 'TdxQueryGrid String', iptR);
    RegisterProperty('QueryByIndex', 'TdxQueryGrid Integer', iptR);
    RegisterProperty('QueryCount', 'Integer', iptR);
  end;
end;

procedure SIRegister_dxControls(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxLabel') do
  begin
    RegisterProperty('Expression', 'String', iptR);
    RegisterProperty('FieldName', 'String', iptR);
  end;
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxEdit') do
  begin
    RegisterProperty('CheckExpression', 'String', iptR);
    RegisterProperty('DefaultValue', 'String', iptR);
    RegisterProperty('Editable', 'Boolean', iptR);
    RegisterProperty('Expression', 'String', iptR);
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('FieldSize', 'Integer', iptR);
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('Required', 'Boolean', iptR);
  end;
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxCalcEdit') do
  begin
    RegisterProperty('CheckExpression', 'String', iptR);
	  RegisterProperty('DefaultValue', 'String', iptR);
	  RegisterProperty('Editable', 'Boolean', iptR);
	  RegisterProperty('Expression', 'String', iptR);
	  RegisterProperty('FieldName', 'String', iptR);
	  RegisterProperty('Id', 'Integer', iptR);
	  RegisterProperty('MaxValue', 'Double', iptR);
	  RegisterProperty('MinValue', 'Double', iptR);
	  RegisterProperty('Precision', 'Integer', iptR);
	  RegisterProperty('Required', 'Boolean', iptR);
  end;
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxDateEdit') do
  begin
    RegisterProperty('CheckExpression', 'String', iptR);
    RegisterProperty('DateNow', 'Boolean', iptR);
	  RegisterProperty('DefaultValue', 'String', iptR);
	  RegisterProperty('Editable', 'Boolean', iptR);
	  RegisterProperty('Expression', 'String', iptR);
	  RegisterProperty('FieldName', 'String', iptR);
	  RegisterProperty('Id', 'Integer', iptR);
	  RegisterProperty('Required', 'Boolean', iptR);
  end;
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxTimeEdit') do
  begin
    RegisterProperty('CheckExpression', 'String', iptR);
    RegisterProperty('CurTime', 'Boolean', iptR);
	  RegisterProperty('DefaultValue', 'String', iptR);
	  RegisterProperty('Editable', 'Boolean', iptR);
	  RegisterProperty('Expression', 'String', iptR);
	  RegisterProperty('FieldName', 'String', iptR);
	  RegisterProperty('Id', 'Integer', iptR);
	  RegisterProperty('Required', 'Boolean', iptR);
  end;
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxCounter') do
  begin
    RegisterProperty('CheckExpression', 'String', iptR);
	  RegisterProperty('FieldName', 'String', iptR);
	  RegisterProperty('Id', 'Integer', iptR);
	  RegisterProperty('Required', 'Boolean', iptR);
  end;
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxMemo') do
  begin
    RegisterProperty('CheckExpression', 'String', iptR);
    RegisterProperty('DefaultValue', 'String', iptR);
    RegisterProperty('Editable', 'Boolean', iptR);
    RegisterProperty('Expression', 'String', iptR);
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('FieldSize', 'Integer', iptR);
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('Required', 'Boolean', iptR);
  end;
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxCheckBox') do
  begin
    RegisterProperty('CheckExpression', 'String', iptR);
    RegisterProperty('CheckedText', 'String', iptRW);
	  RegisterProperty('DefaultValue', 'String', iptR);
	  RegisterProperty('Editable', 'Boolean', iptR);
	  RegisterProperty('Expression', 'String', iptR);
	  RegisterProperty('FieldName', 'String', iptR);
	  RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('UnCheckedText', 'String', iptRW);
  end;
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxComboBox') do
  begin
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('Expression', 'String', iptR);
    RegisterProperty('DefaultValue', 'String', iptR);
    RegisterProperty('CheckExpression', 'String', iptR);
    RegisterProperty('Required', 'Boolean', iptR);
    RegisterProperty('FieldSize', 'Integer', iptR);
    RegisterProperty('Filter', 'String', iptRW);
    RegisterProperty('Editable', 'Boolean', iptR);
    RegisterProperty('SourceFormName', 'String', iptR);
    RegisterProperty('SourceFieldName', 'String', iptR);
    RegisterProperty('Items', 'TStrings', iptRW);
  end;
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxLookupComboBox') do
  begin
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('Expression', 'String', iptR);
    RegisterProperty('DefaultValue', 'String', iptR);
    RegisterProperty('CheckExpression', 'String', iptR);
    RegisterProperty('Required', 'Boolean', iptR);
    RegisterProperty('Filter', 'String', iptRW);
    RegisterProperty('HideList', 'Boolean', iptRW);
    RegisterProperty('Editable', 'Boolean', iptR);
    RegisterProperty('SourceFormName', 'String', iptR);
    RegisterProperty('SourceFieldName', 'String', iptR);
    RegisterProperty('OnCreateForm', 'TCreateFormEvent', iptRW);
  end;
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxObjectField') do
  begin
    RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('ObjId', 'Integer', iptR);
    RegisterProperty('FieldId', 'Integer', iptR);
  end;
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxImage') do
  begin
    RegisterMethod('procedure Clear');
    RegisterMethod('procedure SaveToFile(const FileName: String)');
    RegisterMethod('procedure LoadFromFile(const FileName: String)');
    RegisterMethod('procedure SaveToStream(St: TStream)');
    RegisterMethod('procedure LoadFromStream(St: TStream)');
    RegisterProperty('Center', 'Boolean', iptRW);
    RegisterProperty('ImageName', 'String', iptRW);
    RegisterProperty('KeepSize', 'Boolean', iptRW);
    RegisterProperty('Proportional', 'Boolean', iptRW);
    RegisterProperty('Stretch', 'Boolean', iptRW);
  end;
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxDBImage') do
  begin
    RegisterMethod('procedure Clear');
    RegisterMethod('procedure SaveToFile(const FileName: String)');
    RegisterMethod('procedure LoadFromFile(const FileName: String)');
    RegisterMethod('procedure SaveToStream(St: TStream)');
    RegisterMethod('function WasChanged: Boolean');

    RegisterProperty('CheckExpression', 'String', iptR);
	  RegisterProperty('FieldName', 'String', iptR);
	  RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('PrintSize', 'Integer', iptRW);
	  RegisterProperty('Required', 'Boolean', iptR);
    RegisterProperty('ShowThumbnail', 'Boolean', iptR);
    RegisterProperty('SourceFileName', 'String', iptR);
    RegisterProperty('StorageFolder', 'String', iptR);
    RegisterProperty('StorageType', 'Integer', iptR);
    RegisterProperty('StoredFileName', 'String', iptR);
    RegisterProperty('ThumbSize', 'String', iptR);
  end;
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxFile') do
  begin
    RegisterMethod('procedure Clear');
    RegisterMethod('procedure SaveToFile(const FileName: String)');
    RegisterMethod('procedure LoadFromFile(const FileName: String)');
    RegisterMethod('procedure SaveToStream(St: TStream)');
    RegisterMethod('function WasChanged: Boolean');

    RegisterProperty('CheckExpression', 'String', iptR);
    RegisterProperty('Description', 'String', iptR);
	  RegisterProperty('FieldName', 'String', iptR);
    RegisterProperty('FieldSize', 'Integer', iptR);
	  RegisterProperty('Id', 'Integer', iptR);
	  RegisterProperty('Required', 'Boolean', iptR);
    RegisterProperty('SourceFileName', 'String', iptR);
    RegisterProperty('StorageFolder', 'String', iptR);
    RegisterProperty('StorageType', 'Integer', iptR);
    RegisterProperty('StoredFileName', 'String', iptR);
  end;
  Cl.AddTypeS('TShapeType', '(stRectangle, stSquare, stRoundRect, stRoundSquare, stEllipse, stCircle, stSquaredDiamond, stDiamond, stTriangle, stTriangleLeft, stTriangleRight, stTriangleDown, stStar, stStarDown, stPolygon)');
  Cl.AddTypeS('TShapeTypeEx', '(steNone, steVertLine, steHorzLine, steBDiagonal, steFDiagonal, steCross, steDiagCross)');
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxShape') do
  begin
    RegisterProperty('Brush', 'TBrush', iptRW);
	  RegisterProperty('Pen', 'TPen', iptRW);
	  RegisterProperty('Shape', 'TShapeType', iptRW);
    RegisterProperty('ShapeEx', 'TShapeTypeEx', iptRW);
  end;
  Cl.AddClassN(Cl.FindClass('TControl'), 'TdxGrid');
  SIRegister_dxQueryGrid(Cl);
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxButton') do
  begin
    RegisterMethod('procedure Click');
    RegisterMethod('procedure SetClickHandler(ClickHandler: TNotifyEvent)');
    RegisterProperty('OnClick', 'TNotifyEvent', iptRW);
  end;
  Cl.AddClassN(Cl.FindClass('TControl'), 'TdxPivotGrid');
  Cl.AddClassN(Cl.FindClass('TControl'), 'TdxChart');
  with Cl.AddClassN(Cl.FindClass('TWinControl'), 'TdxTabSheet') do
  begin
    RegisterProperty('TabVisible', 'Boolean', iptRW);
    RegisterProperty('PageIndex', 'Integer', iptR);
  end;
  with Cl.AddClassN(Cl.FindClass('TWinControl'), 'TdxPageControl') do
  begin
    RegisterProperty('Pages', 'TdxTabSheet Integer', iptR);
    RegisterProperty('PageCount', 'Integer', iptR);
    RegisterProperty('ActivePageIndex', 'Integer', iptRW);
  end;
  Cl.AddClassN(Cl.FindClass('TWinControl'), 'TdxGroupBox');
  with Cl.AddClassN(Cl.FindClass('TControl'), 'TdxRecordId') do
  begin
    RegisterProperty('Id', 'Integer', iptR);
    RegisterProperty('FieldName', 'String', iptR);
  end;
end;

procedure SIRegister_Session(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(Cl.FindClass('TObject'), 'TSession') do
  begin
    RegisterMethod('function CreateForm(const FormName: String): TdxForm');
    RegisterMethod('function SQLSelect(const SQL: String): TdxSQLQuery');
    RegisterMethod('procedure SQLExecute(const SQL: String)');
    RegisterMethod('function EvalExpr(const Expr: String; Fm: TdxForm): Variant');
    RegisterMethod('function FindForm(const FormName: String; ARecId: Integer): TdxForm');
    RegisterMethod('function GetCacheDir: String');
    RegisterMethod('procedure Debug(Value: Variant)');
    RegisterMethod('function GetCurrentUser: String');
    RegisterMethod('function GetCurrentRole: String');
    RegisterMethod('function GetCurrentDatabase: String');
    RegisterMethod('function GetTemplatesDir: String');
    RegisterMethod('function GetExprVar(const AName: String): Variant');
    RegisterMethod('function SetExprVar(const AName: String; AValue: Variant): Variant');
    RegisterProperty('FormCount', 'Integer', iptR);
    RegisterProperty('Forms', 'TdxForm Integer', iptR);
    RegisterProperty('Request', 'TFPHTTPConnectionRequest', iptR);
    RegisterProperty('OnCreateForm', 'TCreateFormEvent', iptRW);
    RegisterProperty('OnDestroyForm', 'TCreateFormEvent', iptRW);
    RegisterProperty('OnDatabaseClose', 'TNotifyEvent', iptRW);
    RegisterProperty('OnHandleRequest', 'TWebServerRequestHandler', iptRW);
  end;
end;

procedure SIRegister_Consts(Cl: TPSPascalCompiler);
begin
  Cl.AddConstantN('LineEnding', 'String').SetString(LineEnding);
  Cl.AddConstantN('PathDelim', 'Char').SetChar( {$IFDEF WINDOWS}'\'{$ELSE}'/'{$ENDIF} );

  Cl.AddConstantN('faReadOnly', 'LongInt').SetInt(faReadOnly);
  Cl.AddConstantN('faHidden', 'LongInt').SetInt(faHidden);
  Cl.AddConstantN('faSysFile', 'LongInt').SetInt(faSysFile);
  Cl.AddConstantN('faVolumeId', 'LongInt').SetInt(faVolumeId);
  Cl.AddConstantN('faDirectory', 'LongInt').SetInt(faDirectory);
  Cl.AddConstantN('faArchive', 'LongInt').SetInt(faArchive);
  Cl.AddConstantN('faSymLink', 'LongInt').SetInt(faSymLink);
  Cl.AddConstantN('faAnyFile', 'LongInt').SetInt(faAnyFile);
end;

procedure SIRegister_Functions(Cl: TPSPascalCompiler);
begin
  Cl.AddTypeS('TMonthNameArray', 'array [1..12] of String');
  Cl.AddTypeS('TWeekNameArray', 'array [1..7] of String');
  Cl.AddTypeS('TFormatSettings', 'record CurrencyFormat: Byte; NegCurrFormat: Byte; ' +
    'ThousandSeparator: Char; DecimalSeparator: Char; CurrencyDecimals: Byte; ' +
    'DateSeparator: Char; TimeSeparator: Char; ListSeparator: Char; ' +
    'CurrencyString: string; ShortDateFormat: string; LongDateFormat: string; ' +
    'TimeAMString: string; TimePMString: string; ShortTimeFormat: string; ' +
    'LongTimeFormat: string; ShortMonthNames: TMonthNameArray; ' +
    'LongMonthNames: TMonthNameArray; ShortDayNames: TWeekNameArray; ' +
    'LongDayNames: TWeekNameArray; TwoDigitYearCenturyWindow: Word; end;');
  Cl.AddTypeS('TReplaceFlag', '(rfReplaceAll, rfIgnoreCase)');
  Cl.AddTypeS('TReplaceFlags', 'set of TReplaceFlag');

  Cl.AddDelphiFunction('function UTF8Length(const s: string): LongInt;');
  Cl.AddDelphiFunction('function UTF8Pos(const SearchForText, SearchInText: string; StartPos: LongInt): LongInt;');
  Cl.AddDelphiFunction('function UTF8Copy(const s: string; StartCharIndex, CharCount: LongInt): string');
  Cl.AddDelphiFunction('procedure UTF8Delete(var s: String; StartCharIndex, CharCount: LongInt)');
  Cl.AddDelphiFunction('procedure UTF8Insert(const source: String; var s: string; StartCharIndex: LongInt);');
  Cl.AddDelphiFunction('function UTF8StringReplace(const S, OldPattern, NewPattern: String; Flags: TReplaceFlags): String;');
  Cl.AddDelphiFunction('function UTF8LowerCase(const AInStr: string): string;');
  Cl.AddDelphiFunction('function UTF8UpperCase(const AInStr: string): string;');
  Cl.AddDelphiFunction('function UTF8CompareStr(const S1, S2: string): LongInt;');
  Cl.AddDelphiFunction('function Utf8CompareText(const S1, S2: string): LongInt');
  Cl.AddDelphiFunction('function WinCPToUtf8(const s: String): String');
  Cl.AddDelphiFunction('function Utf8ToWinCP(const s: String) : String');
  Cl.AddDelphiFunction('function UTF8ToUTF16(const S: AnsiString): UnicodeString');
  Cl.AddDelphiFunction('function UTF16ToUTF8(const S: UnicodeString): AnsiString');

  Cl.AddTypeS('TCopyFileFlag', '(cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime)');
  Cl.AddTypeS('TCopyFileFlags', 'set of TCopyFileFlag');
  Cl.AddDelphiFunction('function FileExists(const Filename: string): boolean;');
  Cl.AddDelphiFunction('function FileAge(const FileName: string): int64;');
  Cl.AddDelphiFunction('function DirectoryExists(const Directory: string): Boolean;');
  Cl.AddDelphiFunction('function ExpandFileName(const FileName, BaseDir: string): string;');
  Cl.AddDelphiFunction('function FileSetDate(const FileName: String; Age: int64): Longint;');
  Cl.AddDelphiFunction('function FileGetAttr(const FileName: String): Longint;');
  Cl.AddDelphiFunction('function FileSetAttr(const Filename: String; Attr: longint): Longint;');
  Cl.AddDelphiFunction('function DeleteFile(const FileName: String): Boolean;');
  Cl.AddDelphiFunction('function RenameFile(const OldName, NewName: String): Boolean;');
  Cl.AddDelphiFunction('function GetCurrentDir: String;');
  Cl.AddDelphiFunction('function CreateDir(const NewDir: String): Boolean;');
  Cl.AddDelphiFunction('function RemoveDir(const Dir: String): Boolean;');
  Cl.AddDelphiFunction('function ForceDirectories(const Dir: string): Boolean;');
  Cl.AddDelphiFunction('function CopyFile(const SrcFilename, DestFilename: string; Flags: TCopyFileFlags): boolean;');
  Cl.AddDelphiFunction('procedure FindAllFiles(AList: TStrings; const SearchPath: String; SearchMask: String; SearchSubDirs: Boolean; DirAttr: Word);');
  Cl.AddDelphiFunction('procedure FindAllDirectories(AList: TStrings; const SearchPath: String; SearchSubDirs: Boolean);');
  Cl.AddDelphiFunction('function ExtractFileName(const FileName: string): string;');
  Cl.AddDelphiFunction('function ExtractFileNameOnly(const AFilename: string): string;');
  Cl.AddDelphiFunction('function ExtractFileExt(const FileName: string): string;');
  Cl.AddDelphiFunction('function ExtractFilePath(const FileName: string): string;');
  Cl.AddDelphiFunction('function ExtractFileDrive(const FileName: string): string;');
  Cl.AddDelphiFunction('function ExtractFileDir(Const FileName : string): string;');
  Cl.AddDelphiFunction('function ChangeFileExt(const FileName, Extension: string): string;');
  Cl.AddDelphiFunction('function IncludeTrailingPathDelimiter(Const Path : String) : String;');
  Cl.AddDelphiFunction('function ExcludeLeadingPathDelimiter(Const Path: string): string;');
  Cl.AddDelphiFunction('function GetTempFileName: String');
  Cl.AddDelphiFunction('function GetTempDir: String');
  Cl.AddDelphiFunction('function ShellExecute(const Operation, FileName, Params, WorkDir: String; ShowCmd: LongInt): Boolean');
  Cl.AddDelphiFunction('Function DateTimeToFileDate(DateTime : TDateTime) : int64');
	Cl.AddDelphiFunction('Function FileDateToDateTime (Filedate : int64) :TDateTime');
  Cl.AddDelphiFunction('function FileSize(const Filename: string): int64');
  Cl.AddDelphiFunction('function Random(n: LongInt): LongInt');
  Cl.AddDelphiFunction('function DCount(DataSet: TObject): Integer');
  Cl.AddDelphiFunction('function DSum(DataSet: TObject; const FieldName: String): Double');
  Cl.AddDelphiFunction('function DAvg(DataSet: TObject; const FieldName: String): Double');
  Cl.AddDelphiFunction('function DMax(DataSet: TObject; const FieldName: String): Variant');
  Cl.AddDelphiFunction('function DMin(DataSet: TObject; const FieldName: String): Variant');
  Cl.AddDelphiFunction('function DMerge(DataSet: TObject; const FieldName, Delimiter: String): String');
  Cl.AddDelphiFunction('function ToWords(Money: Currency): String');
  Cl.AddDelphiFunction('function RurToWords(Money: Currency): String');
  Cl.AddDelphiFunction('function Nz(V1, V2: Variant): Variant');
  Cl.AddDelphiFunction('function RoundTo(AValue: Double; Digits: Integer): Double');
  Cl.AddDelphiFunction('function Frac(E: Double): Double');
  Cl.AddDelphiFunction('function Power(base, exponent: Double): Double');
  Cl.AddDelphiFunction('Function YearsBetween(const ANow, AThen: TDateTime): Integer');
  Cl.AddDelphiFunction('Function MonthsBetween(const ANow, AThen: TDateTime): Integer');
  Cl.AddDelphiFunction('Function WeeksBetween(const ANow, AThen: TDateTime): Integer');
  Cl.AddDelphiFunction('Function DaysBetween(const ANow, AThen: TDateTime): Integer');
  Cl.AddDelphiFunction('Function HoursBetween(const ANow, AThen: TDateTime): Int64');
  Cl.AddDelphiFunction('Function MinutesBetween(const ANow, AThen: TDateTime): Int64');
  Cl.AddDelphiFunction('Function SecondsBetween(const ANow, AThen: TDateTime): Int64');
  Cl.AddDelphiFunction('Function MilliSecondsBetween(const ANow, AThen: TDateTime): Int64');
  Cl.AddDelphiFunction('Function AddYear(const AValue: TDateTime; const ANumberOfYears: Integer ): TDateTime');
  Cl.AddDelphiFunction('function AddMonth(const DateTime: TDateTime; NumberOfMonths: integer ): TDateTime');
  Cl.AddDelphiFunction('Function AddWeek(const AValue: TDateTime; const ANumberOfWeeks: Integer): TDateTime');
  Cl.AddDelphiFunction('Function AddDay(const AValue: TDateTime; const ANumberOfDays: Integer): TDateTime');
  Cl.AddDelphiFunction('Function AddHour(const AValue: TDateTime; const ANumberOfHours: Int64): TDateTime');
  Cl.AddDelphiFunction('Function AddMinute(const AValue: TDateTime; const ANumberOfMinutes: Int64): TDateTime');
  Cl.AddDelphiFunction('Function AddSecond(const AValue: TDateTime; const ANumberOfSeconds: Int64): TDateTime');
  Cl.AddDelphiFunction('Function YearOf(const AValue: TDateTime): Word');
  Cl.AddDelphiFunction('Function MonthOf(const AValue: TDateTime): Word');
  Cl.AddDelphiFunction('Function WeekOf(const AValue: TDateTime): Word');
  Cl.AddDelphiFunction('Function DayOf(const AValue: TDateTime): Word');
  Cl.AddDelphiFunction('Function HourOf(const AValue: TDateTime): Word');
  Cl.AddDelphiFunction('Function MinuteOf(const AValue: TDateTime): Word');
  Cl.AddDelphiFunction('Function SecondOf(const AValue: TDateTime): Word');
  Cl.AddDelphiFunction('function StrToTime(const S: String): TDateTime');
  Cl.AddDelphiFunction('function TimeToStr(Time: TDateTime): string');
  Cl.AddDelphiFunction('function TryStrToDate(const S: string; out Value: TDateTime): Boolean');
  Cl.AddDelphiFunction('function TryStrToTime(const S: string; out Value: TDateTime): Boolean');
  Cl.AddDelphiFunction('function TryStrToDateTime(const S: string; out Value: TDateTime): Boolean');
  Cl.AddDelphiFunction('function TryStrToInt(const s: string; Out i : LongInt) : boolean');
  Cl.AddDelphiFunction('function TryStrToInt64(const s: string; Out i : Int64) : boolean');
  Cl.AddDelphiFunction('Function TryStrToFloat(Const S : String; Out Value: Double): Boolean');
  Cl.AddDelphiFunction('function StrToDateTime(const S: String): TDateTime');
  Cl.AddDelphiFunction('function IntToHex(Value: Int64; Digits: Integer): String');

  Cl.AddDelphiFunction('function GetWeekName(D: TDateTime; Brief: Boolean): String');
  Cl.AddDelphiFunction('function GetMonthName(D: TDateTime; Brief: Boolean): String');
  Cl.AddDelphiFunction('Function DayOfTheWeek(const AValue: TDateTime): Word');
  Cl.AddDelphiFunction('function FmtDate(DT: TDateTime): String');
  Cl.AddDelphiFunction('function FillZeros(E: Double; N: Integer): String');
  Cl.AddDelphiFunction('function CalcPeriod(D1, D2: TDateTime; Detail: Integer; Age: Boolean): String');
  Cl.AddDelphiFunction('function BeginYear(D: TDateTime): TDateTime');
  Cl.AddDelphiFunction('function BeginMonth(D: TDateTime): TDateTime');
  Cl.AddDelphiFunction('function BeginWeek(D: TDateTime): TDateTime');
  Cl.AddDelphiFunction('function EndYear(D: TDateTime): TDateTime');
  Cl.AddDelphiFunction('function EndMonth(D: TDateTime): TDateTime');
  Cl.AddDelphiFunction('function EndWeek(D: TDateTime): TDateTime');
  Cl.AddDelphiFunction('function BeginQuarter(D: TDateTime): TDateTime');
  Cl.AddDelphiFunction('function EndQuarter(D: TDateTime): TDateTime');
  Cl.AddDelphiFunction('function QuarterOf(D: TDateTime): Word');
  //Cl.AddDelphiFunction('function DBUnique(Fm: TdxForm; const Fields: String): Boolean');
  {Cl.AddDelphiFunction('function GetCurrentUser: String');
  Cl.AddDelphiFunction('function GetCurrentRole: String');
  Cl.AddDelphiFunction('function GetCurrentDatabase: String');
  Cl.AddDelphiFunction('function GetTemplatesDir: String');
  Cl.AddDelphiFunction('function GetOutputDir: String');   }
  Cl.AddDelphiFunction('function Format(const Fmt: String; Args: array of const): String');
  Cl.AddDelphiFunction('function StringReplace(const S, OldPattern, NewPattern: string;  Flags: TReplaceFlags): string;');
  Cl.AddDelphiFunction('procedure SplitStr(const S: String; Delim: Char; SL: TStrings)');

  //Cl.AddDelphiFunction('function EvalExpr(const Expr: String; Fm: TdxForm): Variant');
  //Cl.AddDelphiFunction('function SQLSelect(const SQL: String): TdxSQLQuery');
  //Cl.AddDelphiFunction('procedure SQLExecute(const SQL: String)');

  Cl.AddDelphiFunction('function FormatFloat(Const Format : String; Value : Double) : String');

  Cl.AddDelphiFunction('function EncodeMD5(const S: String): String');
  Cl.AddDelphiFunction('function EncodeSHA1(const S: String): String');
  Cl.AddDelphiFunction('function EncodeBase64(const S: String): String');
  Cl.AddDelphiFunction('function DecodeBase64(const S: String; Strict: Boolean): String');
  Cl.AddDelphiFunction('function HMACMD5(const AKey, AMessage: string): string');
	Cl.AddDelphiFunction('function HMACSHA1(const AKey, AMessage: string): string');

  Cl.AddDelphiFunction('function VarToStr(const V: Variant): String');
  Cl.AddDelphiFunction('function VarIsNothing(V: Variant): Boolean');
  Cl.AddDelphiFunction('procedure VarCast(var dest : variant;const source : variant;vartype : longint)');
  Cl.AddDelphiFunction('function VarAsType(const V: Variant; aVarType: TVarType): Variant');

  Cl.AddDelphiFunction('function SameValue(const A, B: Double; Epsilon: Double): Boolean');

  Cl.AddDelphiFunction('function Point(X, Y: Integer): TPoint');
  Cl.AddDelphiFunction('function Rect(Left, Top, Right, Bottom: Integer): TRect');

  Cl.AddDelphiFunction('function GetComponentId(C: TComponent): Integer');
  Cl.AddDelphiFunction('function GetComponentFieldName(C: TComponent): String');

  Cl.AddDelphiFunction('function GetFormatSettings: TFormatSettings');
  Cl.AddDelphiFunction('procedure SetFormatSettings(var Settings: TFormatSettings)');

  Cl.AddDelphiFunction('function VarArrayOf(const Values: array of Variant): Variant');
  Cl.AddDelphiFunction('function VarArrayDimCount(const A: Variant) : LongInt');
  Cl.AddDelphiFunction('function VarArrayLowBound(const A: Variant; Dim : LongInt) : LongInt');
  Cl.AddDelphiFunction('function VarArrayHighBound(const A: Variant; Dim : LongInt) : LongInt');

  Cl.AddDelphiFunction('function GetBuildDate: TDateTime');

  Cl.AddDelphiFunction('function ReadXmlFromFile(const FileName: String; Flags: TXMLReaderFlags): TXmlDocument');
  Cl.AddDelphiFunction('function ReadXmlFromStream(Stream: TStream; Flags: TXMLReaderFlags): TXmlDocument');
  Cl.AddDelphiFunction('function ReadXmlFromString(const XmlData: String; Flags: TXMLReaderFlags): TXmlDocument');
  Cl.AddDelphiFunction('procedure ReadXmlNodeFromString(var AParentNode: TDOMNode; const XmlData: String; Flags: TXMLReaderFlags)');
  Cl.AddDelphiFunction('procedure WriteXmlToFile(ADoc: TXmlDocument; const FileName: String; Flags: TXMLWriterFlags)');
  Cl.AddDelphiFunction('procedure WriteXmlToStream(ADoc: TXmlDocument; Stream: TStream; Flags: TXMLWriterFlags)');
  Cl.AddDelphiFunction('procedure WriteXmlToString(ADoc: TXmlDocument; var XmlData: String; Flags: TXMLWriterFlags)');
  Cl.AddDelphiFunction('procedure WriteXmlNodeToString(ANode: TDOMNode; var XmlData: String; Flags: TXMLWriterFlags)');

  Cl.AddDelphiFunction('function ReadJSONFromString(Const JSON : String) : TJSONData');
  Cl.AddDelphiFunction('function ReadJSONFromStream(Const JSON : TStream) : TJSONData');
  Cl.AddDelphiFunction('function ReadJSONFromFile(Const FileName: String) : TJSONData');
  Cl.AddDelphiFunction('function JSONStringToString(const S: String): String');
  Cl.AddDelphiFunction('function StringToJSONString(const S: String; Strict: Boolean): String');

  Cl.AddDelphiFunction('function EncodeURLElement(S: String): String');
  Cl.AddDelphiFunction('function DecodeURLElement(const S: String): String');
  Cl.AddDelphiFunction('procedure DebugFile(const FileName: String; Value: Variant)');

  Cl.AddDelphiFunction('function IIF(Cond, V1, V2: Variant): Variant');
  Cl.AddDelphiFunction('function CreateGUIDString: String');
  Cl.AddDelphiFunction('function GetAppDir: String');

  Cl.AddDelphiFunction('function SetExprVar(const aName: String; aValue: Variant): Variant');
  Cl.AddDelphiFunction('function GetExprVar(const aName: String): Variant');

  Cl.AddDelphiFunction('function CommandExecute(const FileName, Params, WorkDir: String; out OutputString: String; out ExitStatus: Integer): Integer');
  Cl.AddDelphiFunction('function FileExecute(const FileName, Params, WorkDir: String): Integer');
  Cl.AddDelphiFunction('procedure Delay(Milliseconds: Cardinal)');

  with Cl.AddFunction('procedure FreeAndNil;').Decl do
    with AddParam do
    begin
      OrgName := 'x';
      Mode := pmInOut;
    end;
end;

//!!!
procedure SIRegister_All(Cl: TPSPascalCompiler);
begin
  SIRegister_Std(Cl);
  SIRegister_Classes(Cl);
  SIRegister_Graphics(Cl);
  SIRegister_Controls(Cl);
  SIRegister_Timer(Cl);
  RegisterDatetimeLibrary_C(Cl);
  RegisterDll_Compiletime(Cl);

  SIRegister_dxTypes(Cl);
  SIRegister_IniFiles(Cl);
  SIRegister_TField(Cl);
  SIRegister_dxSQLQuery(Cl);
  SIRegister_HttpServer(Cl);
  SIRegister_HttpClient(Cl);
  SIRegister_Template(Cl);
  SIRegister_Xml(Cl);
  SIRegister_Json(Cl);
  SIRegister_CSVData(Cl);
  SIRegister_dxControls(Cl);
  SIRegister_dxForm(Cl);
  SIRegister_Session(Cl);
  SIRegister_Consts(Cl);
  SIRegister_Functions(Cl);
end;

end.

