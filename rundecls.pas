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

unit RunDecls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPSRuntime, DB, LazUtf8, LazFileUtils,
  fphttpserver, httpdefs, httpclient, fphttpclient, webtemplates,
  laz2_dom, fpjson, csvfiles, mytypes, BGRAGraphics;

procedure RegisterDateTimeLibrary_R(S: TPSExec);
procedure RIRegister_Functions(Exec: TPSExec);
procedure RIRegister_All(Cl: TPSRuntimeClassImporter);

implementation

uses
  ScriptFuncs, Math, exprfuncs, DateUtils, dxctrls, apputils, IniFiles,
  BGRABitmap, dxSQLQuery, HMAC, Variants, pivotgrid, dxtypes;

type
  TFileSetDateFunc = function (const FileName : RawByteString;Age : Int64) : Longint;
  TFileAgeFunc = function (const FileName : RawByteString): Int64;

////////////////////////////////////////////////////////////////////////////////
// Std
////////////////////////////////////////////////////////////////////////////////

{procedure TObjectFree(Self: TObject);
begin
  if Self is TdxForm then TObjectDestroyForm(TdxForm(Self))
  else Self.Free;
end;}
procedure TObjectClassName_R(Self: TObject; var T: String); begin T := Self.ClassName; end;

procedure RIRegisterTObject(CL: TPSRuntimeClassImporter);
begin
  with cl.Add(TObject) do
  begin
    RegisterConstructor(@TObject.Create, 'Create');
    RegisterMethod(@TObject.Free, 'Free');
    RegisterPropertyHelper(@TObjectClassName_R, nil, 'ClassName');
  end;
end;

procedure RIRegisterTPersistent(CL: TPSRuntimeClassImporter);
begin
  with cl.Add(TPersistent) do
  begin
    RegisterVirtualMethod(@TPersistent.Assign, 'Assign');
  end;
end;

procedure TComponentName_R(Self: TdxComponent; var T: String); begin T := Self.Name; end;
procedure TComponentName_W(Self: TdxComponent; T: String); begin Self.Name := T; end;
procedure TComponentOwnerR(Self: TdxComponent; var T: TdxComponent); begin T := Self.Owner; end;
procedure TCOMPONENTCOMPONENTS_R(Self: TDXCOMPONENT; var T: TDXCOMPONENT; t1: INTEGER); begin T := Self.COMPONENTS[t1]; end;
procedure TCOMPONENTCOMPONENTCOUNT_R(Self: TDXCOMPONENT; var T: INTEGER); begin t := Self.COMPONENTCOUNT; end;

procedure RIRegisterTComponent(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add2(TdxComponent, 'TCOMPONENT') do
  begin
    RegisterPropertyHelper(@TComponentName_R, @TComponentName_W, 'Name');
    RegisterMethod(@TdxComponent.FindComponent, 'FindComponent');
    RegisterVirtualConstructor(@TdxComponent.Create, 'Create');
    RegisterPropertyHelper(@TComponentOwnerR, nil, 'Owner');
    RegisterPropertyHelper(@TCOMPONENTCOMPONENTS_R, nil, 'Components');
    RegisterPropertyHelper(@TCOMPONENTCOMPONENTCOUNT_R, nil, 'ComponentCount');
  end;
end;

//!!!
procedure RIRegister_Std(Cl: TPSRuntimeClassImporter);
begin
  RIRegisterTObject(Cl);
  RIRegisterTPersistent(CL);
  RIRegisterTComponent(Cl);
end;

////////////////////////////////////////////////////////////////////////////////
// Classes
////////////////////////////////////////////////////////////////////////////////

procedure TStringsCapacityR(Self: TStrings; var T: Longint); begin T := Self.Capacity; end;
procedure TStringsCapacityW(Self: TStrings; T: Longint); begin Self.Capacity := T; end;
procedure TStringsDelimiterR(Self: TStrings; var T: char); begin T := Self.Delimiter; end;
procedure TStringsDelimiterW(Self: TStrings; T: char); begin Self.Delimiter:= T; end;
procedure TStringsDelimitedTextR(Self: TStrings; var T: string); begin T := Self.DelimitedText; end;
procedure TStringsDelimitedTextW(Self: TStrings; T: string); begin Self.DelimitedText:= T; end;
procedure TStringsNameValueSeparatorR(Self: TStrings; var T: char); begin T := Self.NameValueSeparator; end;
procedure TStringsNameValueSeparatorW(Self: TStrings; T: char); begin Self.NameValueSeparator:= T; end;
procedure TStringsQuoteCharR(Self: TStrings; var T: char); begin T := Self.QuoteChar; end;
procedure TStringsQuoteCharW(Self: TStrings; T: char); begin Self.QuoteChar:= T; end;
procedure TStringsCountR(Self: TStrings; var T: Longint); begin T := Self.Count; end;
procedure TStringsTextR(Self: TStrings; var T: string); begin T := Self.Text; end;
procedure TStringsTextW(Self: TStrings; T: string); begin Self.Text:= T; end;
procedure TStringsCommaTextR(Self: TStrings; var T: string); begin T := Self.CommaText; end;
procedure TStringsCommaTextW(Self: TStrings; T: string); begin Self.CommaText:= T; end;
procedure TStringsObjectsR(Self: TStrings; var T: TObject; I: Longint); begin T := Self.Objects[I]; end;
procedure TStringsObjectsW(Self: TStrings; const T: TObject; I: Longint); begin Self.Objects[I]:= T; end;
procedure TStringsStringsR(Self: TStrings; var T: string; I: Longint); begin T := Self.Strings[I]; end;
procedure TStringsStringsW(Self: TStrings; const T: string; I: Longint); begin Self.Strings[I]:= T; end;
procedure TStringsNamesR(Self: TStrings; var T: string; I: Longint); begin T := Self.Names[I]; end;
procedure TStringsValuesR(Self: TStrings; var T: string; const I: string); begin T := Self.Values[I]; end;
procedure TStringsValuesW(Self: TStrings; Const T, I: String); begin Self.Values[I]:= T; end;
procedure TStringsValueFromIndexR(Self: TStrings; var T: string; const I: Longint); begin T := Self.ValueFromIndex[I]; end;
procedure TStringsValueFromIndexW(Self: TStrings; Const T: String; I: Longint); begin Self.ValueFromIndex[I]:= T; end;
procedure TStringsStrictDelimiterR(Self: TStrings; var T: Boolean); begin T := Self.StrictDelimiter; end;
procedure TStringsStrictDelimiterW(Self: TStrings; T: Boolean); begin Self.StrictDelimiter := T; end;
procedure TStringsLoadFromFile(Self: TStrings; const T: String); begin Self.LoadFromFile(T, True); end;
procedure TStringsSaveToFile(Self: TStrings; const T: String); begin Self.SaveToFile(T, True); end;
procedure TStringsLoadFromStream(Self: TStrings; T: TStream); begin Self.LoadFromStream(T, True); end;
procedure TStringsSaveToStream(Self: TStrings; T: TStream); begin Self.SaveToStream(T, True); end;

procedure RIRegisterTStrings(cl: TPSRuntimeClassImporter); // requires TPersistent
begin
  with Cl.Add(TStrings) do
  begin
    RegisterConstructor(@TStrings.Create, 'Create');
    RegisterVirtualMethod(@TStrings.Add, 'Add');
    RegisterVirtualMethod(@TStrings.AddStrings, 'AddStrings');
    RegisterVirtualAbstractMethod(TStringList, @TStringList.Clear, 'Clear');
    RegisterVirtualAbstractMethod(TStringList, @TStringList.Delete, 'Delete');
    RegisterVirtualMethod(@TStrings.IndexOf, 'IndexOf');
    RegisterVirtualAbstractMethod(TStringList, @TStringList.Insert, 'Insert');
    RegisterMethod(@TStringsLoadFromFile, 'LoadFromFile');
    RegisterMethod(@TStringsSaveToFile, 'SaveToFile');
    RegisterMethod(@TStrings.BeginUpdate, 'BeginUpdate');
    RegisterMethod(@TStrings.EndUpdate, 'EndUpdate');
    RegisterMethod(@TStrings.Equals,  'Equals');
    RegisterVirtualMethod(@TStrings.Exchange, 'Exchange');
    RegisterMethod(@TStrings.IndexOfName, 'IndexOfName');
    RegisterMethod(@TStringsLoadFromStream, 'LoadFromStream');
    RegisterVirtualMethod(@TStrings.Move, 'Move');
    RegisterMethod(@TStringsSaveToStream, 'SaveToStream');
    RegisterVirtualMethod(@TSTRINGS.ADDOBJECT, 'AddObject');
    RegisterMethod(@TSTRINGS.INDEXOFOBJECT, 'IndexOfObject');
    RegisterMethod(@TSTRINGS.INSERTOBJECT, 'InsertObject');

    RegisterPropertyHelper(@TStringsCapacityR, @TStringsCapacityW, 'Capacity');
    RegisterPropertyHelper(@TStringsDelimiterR, @TStringsDelimiterW, 'DELIMITER');
    RegisterPropertyHelper(@TStringsDelimitedTextR, @TStringsDelimitedTextW, 'DelimitedText');
    RegisterPropertyHelper(@TStringsNameValueSeparatorR, @TStringsNameValueSeparatorW, 'NameValueSeparator');
    RegisterPropertyHelper(@TStringsQuoteCharR, @TStringsQuoteCharW, 'QuoteChar');
    RegisterPropertyHelper(@TStringsCountR, nil, 'Count');
    RegisterPropertyHelper(@TStringsTextR, @TStringsTextW, 'Text');
    RegisterPropertyHelper(@TStringsCommaTextR, @TStringsCommatextW, 'CommaText');
    RegisterPropertyHelper(@TStringsStringsR, @TStringsStringsW, 'Strings');
    RegisterPropertyHelper(@TStringsObjectsR, @TStringsObjectsW, 'Objects');
    RegisterPropertyHelper(@TStringsNamesR, nil, 'Names');
    RegisterPropertyHelper(@TStringsValuesR, @TStringsValuesW, 'Values');
    RegisterPropertyHelper(@TStringsValueFromIndexR, @TStringsValueFromIndexW, 'ValueFromIndex');
    RegisterPropertyHelper(@TStringsStrictDelimiterR, @TStringsStrictDelimiterW, 'StrictDelimiter');
  end;
end;

procedure TSTRINGLISTCASESENSITIVE_R(Self: TSTRINGLIST; var T: BOOLEAN); begin T := Self.CASESENSITIVE; end;
procedure TSTRINGLISTCASESENSITIVE_W(Self: TSTRINGLIST; const T: BOOLEAN); begin Self.CASESENSITIVE := T; end;
procedure TSTRINGLISTDUPLICATES_R(Self: TSTRINGLIST; var T: TDUPLICATES); begin T := Self.DUPLICATES; end;
procedure TSTRINGLISTDUPLICATES_W(Self: TSTRINGLIST; const T: TDUPLICATES); begin Self.DUPLICATES := T; end;
procedure TSTRINGLISTSORTED_R(Self: TSTRINGLIST; var T: BOOLEAN); begin T := Self.SORTED; end;
procedure TSTRINGLISTSORTED_W(Self: TSTRINGLIST; const T: BOOLEAN); begin Self.SORTED := T; end;
procedure TSTRINGLISTONCHANGE_R(Self: TSTRINGLIST; var T: TNOTIFYEVENT); begin T := Self.ONCHANGE; end;
procedure TSTRINGLISTONCHANGE_W(Self: TSTRINGLIST; const T: TNOTIFYEVENT); begin Self.ONCHANGE := T; end;
procedure TSTRINGLISTONCHANGING_R(Self: TSTRINGLIST; var T: TNOTIFYEVENT); begin T := Self.ONCHANGING; end;
procedure TSTRINGLISTONCHANGING_W(Self: TSTRINGLIST; const T: TNOTIFYEVENT); begin Self.ONCHANGING := T; end;
procedure TStringListUtf8OnCompare_R(Self: TStringListUtf8; var T: TStringListCompareEvent); begin T := Self.OnCompare; end;
procedure TStringListUtf8OnCompare_W(Self: TStringListUtf8; const T: TStringListCompareEvent); begin Self.OnCompare := T; end;

procedure RIRegisterTSTRINGLIST(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSTRINGLIST) do
  begin
    RegisterVirtualMethod(@TSTRINGLIST.FIND, 'Find');
    RegisterVirtualMethod(@TSTRINGLIST.SORT, 'Sort');
    RegisterPropertyHelper(@TSTRINGLISTCASESENSITIVE_R, @TSTRINGLISTCASESENSITIVE_W, 'CaseSensitive');
    RegisterPropertyHelper(@TSTRINGLISTDUPLICATES_R, @TSTRINGLISTDUPLICATES_W, 'Duplicates');
    RegisterPropertyHelper(@TSTRINGLISTSORTED_R, @TSTRINGLISTSORTED_W, 'Sorted');
    RegisterEventPropertyHelper(@TSTRINGLISTONCHANGE_R, @TSTRINGLISTONCHANGE_W, 'OnChange');
    RegisterEventPropertyHelper(@TSTRINGLISTONCHANGING_R, @TSTRINGLISTONCHANGING_W, 'OnChanging');
  end;
  with Cl.Add(TSTRINGLISTUTF8) do
  begin
    RegisterEventPropertyHelper(@TStringListUtf8OnCompare_R, @TStringListUtf8OnCompare_W, 'OnCompare');
  end;
end;

procedure TSTREAMPOSITION_R(Self: TSTREAM; var T: Int64); begin t := Self.POSITION; end;
procedure TSTREAMPOSITION_W(Self: TSTREAM; T: Int64); begin Self.POSITION := t; end;
procedure TSTREAMSIZE_R(Self: TSTREAM; var T: Int64); begin t := Self.SIZE; end;
procedure TSTREAMSIZE_W(Self: TSTREAM; T: Int64); begin Self.SIZE := t; end;
procedure TSTREAMCOPYFROM(Self: TSTREAM; Source: TStream; Count: LongInt);
begin
	Self.CopyFrom(Source, Count);
end;

procedure RIRegisterTSTREAM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSTREAM) do
  begin
    RegisterVirtualMethod(@TStream.READ, 'Read');
    RegisterVirtualMethod(@TStream.WRITE, 'Write');
    RegisterVirtualMethod(@TStream.SEEK, 'Seek');
    RegisterMethod(@TSTREAM.READBUFFER, 'ReadBuffer');
    RegisterMethod(@TSTREAM.WRITEBUFFER, 'WriteBuffer');
    RegisterMethod(@TSTREAMCOPYFROM, 'CopyFrom');
    RegisterPropertyHelper(@TSTREAMPOSITION_R, @TSTREAMPOSITION_W, 'Position');
    RegisterPropertyHelper(@TSTREAMSIZE_R, @TSTREAMSIZE_W, 'Size');
  end;
end;

procedure TFILESTREAMHANDLE_R(Self: THANDLESTREAM; var T: INTEGER); begin T := Self.HANDLE; end;

procedure RIRegisterTFILESTREAM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TFILESTREAM) do
  begin
    RegisterConstructor(@TFileStream.Create, 'Create');
    RegisterPropertyHelper(@TFILESTREAMHANDLE_R, nil, 'Handle');
  end;
end;

function TStringStreamCreate(Self: TClass; CreateNewInstance: Boolean; AStr: string): TObject;
begin
  Result:= TStringStream.Create(AStr);
end;
procedure TStringStreamDataString_R(Self: TStringStream; var T: String); begin T := Self.DataString; end;

procedure RIRegisterTSTRINGSTREAM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSTRINGSTREAM) do
  begin
    RegisterConstructor(@TStringStreamCreate, 'Create');
    RegisterPropertyHelper(@TStringStreamDataString_R, nil, 'DataString');
  end;
end;

procedure RIRegisterTMEMORYSTREAM(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TMEMORYSTREAM) do
  begin
    RegisterMethod(@TMEMORYSTREAM.CLEAR, 'Clear');
    RegisterMethod(@TMEMORYSTREAM.LOADFROMSTREAM, 'LoadFromStream');
    RegisterMethod(@TMEMORYSTREAM.LOADFROMFILE, 'LoadFromFile');
    RegisterMethod(@TMEMORYSTREAM.SAVETOSTREAM, 'SaveToStream');
    RegisterMethod(@TMEMORYSTREAM.SAVETOFILE, 'SaveToFile');
    RegisterMethod(@TMEMORYSTREAM.SETSIZE, 'SetSize');
  end;
end;

procedure TCollectionItemCollection_R(Self: TCollectionItem; var T: TCollection); begin T := Self.Collection; end;
procedure TCollectionItemCollection_W(Self: TCollectionItem; T: TCollection); begin Self.Collection := T; end;
procedure TCollectionItemIndex_R(Self: TCollectionItem; var T: Integer); begin T := Self.Index; end;
procedure TCollectionItemIndex_W(Self: TCollectionItem; T: Integer); begin Self.Index := T; end;


procedure RIRegisterTCollectionItem(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCollectionItem) do
  begin
    RegisterPropertyHelper(@TCollectionItemCollection_R, @TCollectionItemCollection_W, 'Collection');
    RegisterPropertyHelper(@TCollectionItemIndex_R, @TCollectionItemIndex_W, 'Index');
  end;
end;

procedure TCollectionCount_R(Self: TCollection; var T: Integer); begin T := Self.Count; end;
procedure TCollectionItems_R(Self: TCollection; var T: TCollectionItem; I: Longint); begin T := Self.Items[I]; end;

procedure RIRegisterTCollection(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCollection) do
  begin
    RegisterMethod(@TCollection.Clear, 'Clear');
    RegisterPropertyHelper(@TCollectionCount_R, nil, 'Count');
    RegisterPropertyHelper(@TCollectionItems_R, nil, 'Items');
  end;
end;

//!!!
procedure RIRegister_Classes(Cl: TPSRuntimeClassImporter);
begin
  RIRegisterTSTREAM(Cl);
  RIRegisterTStrings(cl);
  RIRegisterTStringList(cl);
  RIRegisterTFILESTREAM(Cl);
  RIRegisterTSTRINGSTREAM(Cl);
  RIRegisterTMEMORYSTREAM(Cl);
  RIRegisterTCollectionItem(Cl);
  RIRegisterTCollection(Cl);
end;

////////////////////////////////////////////////////////////////////////////////
// Graphics
////////////////////////////////////////////////////////////////////////////////

procedure TFontColor_R(Self: TdxFont; var T: TColor); begin T := Self.Color; end;
procedure TFontColor_W(Self: TdxFont; T: TColor); begin Self.Color := T; end;
procedure TFontHeight_R(Self: TdxFont; var T: Integer); begin T := Self.Height; end;
procedure TFontHeight_W(Self: TdxFont; T: Integer); begin Self.Height := T; end;
procedure TFontName_R(Self: TdxFont; var T: String); begin T := Self.Name; end;
procedure TFontName_W(Self: TdxFont; T: String); begin Self.Name := T; end;
procedure TFontSize_R(Self: TdxFont; var T: Integer); begin T := Self.Size; end;
procedure TFontSize_W(Self: TdxFont; T: Integer); begin Self.Size := T; end;
procedure TFontStyle_R(Self: TdxFont; var T: TFontStyles); begin T := Self.Style; end;
procedure TFontStyle_W(Self: TdxFont; T: TFontStyles); begin Self.Style := T; end;

procedure RIRegister_Font(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add2(TdxFont, 'TFONT') do
  begin
    RegisterConstructor(@TdxFont.Create, 'Create');
    RegisterPropertyHelper(@TFontColor_R, @TFontColor_W, 'Color');
    RegisterPropertyHelper(@TFontHeight_R, @TFontHeight_W, 'Height');
    RegisterPropertyHelper(@TFontName_R, @TFontName_W, 'Name');
    RegisterPropertyHelper(@TFontSize_R, @TFontSize_W, 'Size');
    RegisterPropertyHelper(@TFontStyle_R, @TFontStyle_W, 'Style');
    //RegisterPropertyHelper(@TBrush_R, @TBrush_W, '');
  end;
end;

procedure TBrushColor_R(Self: TdxBrush; var T: TColor); begin T := Self.Color; end;
procedure TBrushColor_W(Self: TdxBrush; T: TColor); begin Self.Color := T; end;
procedure TBrushStyle_R(Self: TdxBrush; var T: TBrushStyle); begin T := Self.Style; end;
procedure TBrushStyle_W(Self: TdxBrush; T: TBrushStyle); begin Self.Style := T; end;

procedure RIRegister_Brush(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add2(TdxBrush, 'TBRUSH') do
  begin
    RegisterConstructor(@TdxBrush.Create, 'Create');
    RegisterPropertyHelper(@TBrushColor_R, @TBrushColor_W, 'Color');
    RegisterPropertyHelper(@TBrushStyle_R, @TBrushStyle_W, 'Style');
    //RegisterPropertyHelper(@TBrush_R, @TBrush_W, '');
  end;
end;

procedure TPenColor_R(Self: TdxPen; var T: TColor); begin T := Self.Color; end;
procedure TPenColor_W(Self: TdxPen; T: TColor); begin Self.Color := T; end;
procedure TPenStyle_R(Self: TdxPen; var T: TPenStyle); begin T := Self.Style; end;
procedure TPenStyle_W(Self: TdxPen; T: TPenStyle); begin Self.Style := T; end;
procedure TPenWidth_R(Self: TdxPen; var T: Integer); begin T := Self.Width; end;
procedure TPenWidth_W(Self: TdxPen; T: Integer); begin Self.Width := T; end;

procedure RIRegister_Pen(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add2(TdxPen, 'TPEN') do
  begin
    RegisterConstructor(@TdxPen.Create, 'Create');
    RegisterPropertyHelper(@TPenColor_R, @TPenColor_W, 'Color');
    RegisterPropertyHelper(@TPenStyle_R, @TPenStyle_W, 'Style');
    RegisterPropertyHelper(@TPenWidth_R, @TPenWidth_W, 'Width');
  end;
end;

//!!!
procedure RIRegister_Graphics(Cl: TPSRuntimeClassImporter);
begin
  RIRegister_Font(Cl);
  RIRegister_Brush(Cl);
  RIRegister_Pen(Cl);
end;

////////////////////////////////////////////////////////////////////////////////
// Controls
////////////////////////////////////////////////////////////////////////////////

procedure TControlLeft_R(Self: TdxControl; var T: Integer); begin T := Self.Left; end;
procedure TControlLeft_W(Self: TdxControl; T: Integer); begin Self.Left := T; end;
procedure TControlTop_R(Self: TdxControl; var T: Integer); begin T := Self.Top; end;
procedure TControlTop_W(Self: TdxControl; T: Integer); begin Self.Top := T; end;
procedure TControlWidth_R(Self: TdxControl; var T: Integer); begin T := Self.Width; end;
procedure TControlWidth_W(Self: TdxControl; T: Integer); begin Self.Width := T; end;
procedure TControlHeight_R(Self: TdxControl; var T: Integer); begin T := Self.Height; end;
procedure TControlHeight_W(Self: TdxControl; T: Integer); begin Self.Height := T; end;
procedure TControlVisible_R(Self: TdxControl; var T: Boolean); begin T := Self.Visible; end;
procedure TControlVisible_W(Self: TdxControl; T: Boolean); begin Self.Visible := T; end;
procedure TControlEnabled_R(Self: TdxControl; var T: Boolean); begin T := Self.Enabled; end;
procedure TControlEnabled_W(Self: TdxControl; T: Boolean); begin Self.Enabled := T; end;
procedure TControlBoundsRect_R(Self: TdxControl; var T: TRect); begin T := Self.BoundsRect; end;
procedure TControlBoundsRect_W(Self: TdxControl; T: TRect); begin Self.BoundsRect := T; end;
procedure TControlColor_R(Self: TdxControl; var T: TColor); begin T := Self.Color; end;
procedure TControlColor_W(Self: TdxControl; T: TColor); begin Self.Color := T; end;
procedure TControlFont_R(Self: TdxControl; var T: TdxFont); begin T := Self.Font; end;
procedure TControlFont_W(Self: TdxControl; T: TdxFont); begin Self.Font := T; end;
procedure TControlParentFont_R(Self: TdxControl; var T: Boolean); begin T := Self.ParentFont; end;
procedure TControlParentFont_W(Self: TdxControl; T: Boolean); begin Self.ParentFont := T; end;
procedure TControlTabOrder_R(Self: TdxControl; var T: Integer); begin T := Self.TabOrder; end;
procedure TControlTabOrder_W(Self: TdxControl; T: Integer); begin Self.TabOrder := T; end;
procedure TControlTabStop_R(Self: TdxControl; var T: Boolean); begin T := Self.TabStop; end;
procedure TControlTabStop_W(Self: TdxControl; T: Boolean); begin Self.TabStop := T; end;
procedure TControlParent_R(Self: TdxControl; var T: TdxWinControl); begin T := Self.Parent; end;
procedure TControlParent_W(Self: TdxControl; T: TdxWinControl); begin Self.Parent := T; end;
procedure TControlCaption_R(Self: TdxControl; var T: String); begin T := Self.Caption; end;
procedure TControlCaption_W(Self: TdxControl; T: String); begin Self.Caption := T; end;
procedure TControlOnChangeBounds_R(Self: TdxControl; var T: TNotifyEvent); begin T := Self.OnChangeBounds; end;
procedure TControlOnChangeBounds_W(Self: TdxControl; T: TNotifyEvent); begin Self.OnChangeBounds := T; end;
procedure TControlOnResize_R(Self: TdxControl; var T: TNotifyEvent); begin T := Self.OnResize; end;
procedure TControlOnResize_W(Self: TdxControl; T: TNotifyEvent); begin Self.OnResize := T; end;

procedure RIRegisterTControl(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add2(TdxControl, 'TCONTROL') do
  begin
    RegisterMethod(@TdxControl.Hide, 'Hide');
    RegisterMethod(@TdxControl.Show, 'Show');
    RegisterMethod(@TdxControl.SetBounds, 'SetBounds');
    RegisterPropertyHelper(@TControlLeft_R, @TControlLeft_W, 'Left');
    RegisterPropertyHelper(@TControlTop_R, @TControlTop_W, 'Top');
    RegisterPropertyHelper(@TControlWidth_R, @TControlWidth_W, 'Width');
    RegisterPropertyHelper(@TControlHeight_R, @TControlHeight_W, 'Height');
    RegisterPropertyHelper(@TControlVisible_R, @TControlVisible_W, 'Visible');
    RegisterPropertyHelper(@TControlEnabled_R, @TControlEnabled_W, 'Enabled');
    RegisterPropertyHelper(@TControlBoundsRect_R, @TControlBoundsRect_W, 'BoundsRect');
    RegisterPropertyHelper(@TControlColor_R, @TControlColor_W, 'Color');
    RegisterPropertyHelper(@TControlFont_R, @TControlFont_W, 'Font');
    RegisterPropertyHelper(@TControlParentFont_R, @TControlParentFont_W, 'ParentFont');
    RegisterPropertyHelper(@TControlTabOrder_R, @TControlTabOrder_W, 'TabOrder');
    RegisterPropertyHelper(@TControlTabStop_R, @TControlTabStop_W, 'TabStop');
    RegisterPropertyHelper(@TControlParent_R, @TControlParent_W, 'Parent');
    RegisterPropertyHelper(@TControlCaption_R, @TControlCaption_W, 'Caption');
    RegisterEventPropertyHelper(@TControlOnChangeBounds_R, @TControlOnChangeBounds_W, 'OnChangeBounds');
    RegisterEventPropertyHelper(@TControlOnResize_R, @TControlOnResize_W, 'OnResize');
  end;
end;

procedure TWinControlControls_R(Self: TdxWinControl; var T: TdxControl; t1: INTEGER); begin T := Self.Controls[t1]; end;
procedure TWinControlControlCount_R(Self: TdxWinControl; var T: INTEGER); begin t := Self.ControlCount; end;

procedure RIRegisterTWinControl(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add2(TdxWinControl, 'TWINCONTROL') do
  begin
    RegisterPropertyHelper(@TWinControlControls_R, nil, 'Controls');
    RegisterPropertyHelper(@TWinControlControlCount_R, nil, 'ControlCount');
  end;
end;

//!!!
procedure RIRegister_Controls(Cl: TPSRuntimeClassImporter);
begin
  RIRegisterTControl(Cl);
  RIRegisterTWinControl(Cl);
end;

procedure TTimerEnabled_R(Self: TdxTimer; var T: Boolean); begin T := Self.Enabled; end;
procedure TTimerEnabled_W(Self: TdxTimer; T: Boolean); begin Self.Enabled := T; end;
procedure TTimerInterval_R(Self: TdxTimer; var T: Integer); begin T := Self.Interval; end;
procedure TTimerInterval_W(Self: TdxTimer; T: Integer); begin Self.Interval := T; end;
procedure TTimerOnTimer_R(Self: TdxTimer; var T: TNotifyEvent); begin T := Self.OnTimer; end;
procedure TTimerOnTimer_W(Self: TdxTimer; T: TNotifyEvent); begin Self.OnTimer := T; end;

procedure RIRegister_Timer(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add2(TdxTimer, 'TTIMER') do
  begin
    RegisterPropertyHelper(@TTimerEnabled_R, @TTimerEnabled_W, 'Enabled');
    RegisterPropertyHelper(@TTimerInterval_R, @TTimerInterval_W, 'Interval');
    RegisterEventPropertyHelper(@TTimerOnTimer_R, @TTimerOnTimer_W, 'OnTimer');
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// DateTime
////////////////////////////////////////////////////////////////////////////////

function TryEncodeDate(Year, Month, Day: Word; var Date: TDateTime): Boolean;
begin
  try
    Date := EncodeDate(Year, Month, Day);
    Result := true;
  except
    Result := false;
  end;
end;

function TryEncodeTime(Hour, Min, Sec, MSec: Word; var Time: TDateTime): Boolean;
begin
  try
    Time := EncodeTime(hour, Min, Sec, MSec);
    Result := true;
  except
    Result := false;
  end;
end;

function DateTimeToUnix(D: TDateTime): Int64;
begin
  Result := Round((D - 25569) * 86400);
end;

function UnixToDateTime(U: Int64): TDateTime;
begin
  Result := U / 86400 + 25569;
end;

function MyFormatDateTime(const FormatStr: string; DateTime: TDateTime): String;
begin
  Result := FormatDateTime(FormatStr, DateTime);
end;

function MyStrToDate(const S: String): TDateTime;
begin
  Result := StrToDate(S);
end;

//!!!
procedure RegisterDateTimeLibrary_R(S: TPSExec);
begin
  S.RegisterDelphiFunction(@EncodeDate, 'EncodeDate', cdRegister);
  S.RegisterDelphiFunction(@EncodeTime, 'EncodeTime', cdRegister);
  S.RegisterDelphiFunction(@TryEncodeDate, 'TryEncodeDate', cdRegister);
  S.RegisterDelphiFunction(@TryEncodeTime, 'TryEncodeTime', cdRegister);
  S.RegisterDelphiFunction(@DecodeDate, 'DecodeDate', cdRegister);
  S.RegisterDelphiFunction(@DecodeTime, 'DecodeTime', cdRegister);
  S.RegisterDelphiFunction(@DayOfWeek, 'DayOfWeek', cdRegister);
  S.RegisterDelphiFunction(@Date, 'Date', cdRegister);
  S.RegisterDelphiFunction(@Time, 'Time', cdRegister);
  S.RegisterDelphiFunction(@Now, 'Now', cdRegister);
  S.RegisterDelphiFunction(@DateToStr, 'DateToStr', cdRegister);
  S.RegisterDelphiFunction(@MyFormatDateTime, 'FormatDateTime', cdRegister);
  S.RegisterDelphiFunction(@MyStrToDate, 'StrToDate', cdRegister);
end;

////////////////////////////////////////////////////////////////////////////////
// JSON
////////////////////////////////////////////////////////////////////////////////

function TJSONDataJSONType(Self: TJSONData): TJSONtype; begin Result := Self.JSONType; end;
procedure TJSONDataCount_R(Self: TJSONData; var T: Integer); begin T := Self.Count; end;
procedure TJSONDataItems_R(Self: TJSONData; var T: TJSONData; I: Integer); begin T := Self.Items[I]; end;
procedure TJSONDataItems_W(Self: TJSONData; T: TJSONData; I: Integer); begin Self.Items[I] := T; end;
procedure TJSONDataValue_R(Self: TJSONData; var T: Variant); begin T := Self.Value; end;
procedure TJSONDataValue_W(Self: TJSONData; T: Variant); begin Self.Value := T; end;
procedure TJSONDataAsString_R(Self: TJSONData; var T: String); begin T := Self.AsString; end;
procedure TJSONDataAsString_W(Self: TJSONData; T: String); begin Self.AsString := T; end;
procedure TJSONDataAsUnicodeString_R(Self: TJSONData; var T: UnicodeString); begin T := Self.AsUnicodeString; end;
procedure TJSONDataAsUnicodeString_W(Self: TJSONData; T: UnicodeString); begin Self.AsUnicodeString := T; end;
procedure TJSONDataAsFloat_R(Self: TJSONData; var T: Double); begin T := Self.AsFloat; end;
procedure TJSONDataAsFloat_W(Self: TJSONData; T: Double); begin Self.AsFloat := T; end;
procedure TJSONDataAsInteger_R(Self: TJSONData; var T: Integer); begin T := Self.AsInteger; end;
procedure TJSONDataAsInteger_W(Self: TJSONData; T: Integer); begin Self.AsInteger := T; end;
procedure TJSONDataAsInt64_R(Self: TJSONData; var T: Int64); begin T := Self.AsInt64; end;
procedure TJSONDataAsInt64_W(Self: TJSONData; T: Int64); begin Self.AsInt64 := T; end;
procedure TJSONDataAsQWord_R(Self: TJSONData; var T: QWord); begin T := Self.AsQWord; end;
procedure TJSONDataAsQWord_W(Self: TJSONData; T: QWord); begin Self.AsQWord := T; end;
procedure TJSONDataAsBoolean_R(Self: TJSONData; var T: Boolean); begin T := Self.AsBoolean; end;
procedure TJSONDataAsBoolean_W(Self: TJSONData; T: Boolean); begin Self.AsBoolean := T; end;
procedure TJSONDataIsNull_R(Self: TJSONData; var T: Boolean); begin T := Self.IsNull; end;
procedure TJSONDataAsJSON_R(Self: TJSONData; var T: String); begin T := Self.AsJSON; end;
procedure TJSONDataClear(Self: TJSONData); begin Self.Clear; end;
function TJSONDataClone(Self: TJSONData): TJSONData; begin Result := Self.Clone; end;

procedure SetFormatFloatOption(Json: TJSONData; AValue: Boolean);
var
  i: Integer;
  Item: TJSONData;
begin
  for i := 0 to Json.Count - 1 do
  begin
    Item := Json.Items[i];
    if Item is TJsonFloatNumberEx then
      TJsonFloatNumberEx(Item).JSONFormatFloatEnabled := AValue;
    if Item.Count > 0 then SetFormatFloatOption(Item, AValue);
  end;
end;

{$IFDEF CPU64}
procedure TJSONDataFormatJSON(Self: TJSONData; var Res: String; Options: Integer; IdentSize: Integer);
var
  HasFormatFloatOpt: Boolean;
begin
  // Дополнительная опция foFormatFloat, которой нет в fpjson, но есть у меня в скриптах.
  HasFormatFloatOpt := (Options and 32) = 32;
  if HasFormatFloatOpt then SetFormatFloatOption(Self, True);
  try
    Res := Self.FormatJSON(TFormatOptions(Options), IdentSize);
  finally
    if HasFormatFloatOpt then SetFormatFloatOption(Self, False);
  end;
end;
{$ELSE}
function TJSONDataFormatJSON(Self: TJSONData; Options: Integer; IdentSize: Integer): String;
var
  HasFormatFloatOpt: Boolean;
begin
  // Дополнительная опция foFormatFloat, которой нет в fpjson, но есть у меня в скриптах.
  HasFormatFloatOpt := (Options and 32) = 32;
  if HasFormatFloatOpt then SetFormatFloatOption(Self, True);
  try
    Result := Self.FormatJSON(TFormatOptions(Options), IdentSize);
  finally
    if HasFormatFloatOpt then SetFormatFloatOption(Self, False);
  end;
end;
{$ENDIF}

Function VariantToJSON(V: Variant) : TJSONData;
begin
  Result:=Nil;
  case VarType(V) of
    varnull, varempty: Result := CreateJSON;
    varbyte, varsmallint, varinteger, varword: Result:=CreateJSON(Integer(V));
    varboolean: Result:=CreateJSON(Boolean(V));
    varsingle, vardouble, varcurrency: Result:=CreateJSON(Double(V));
    varint64: Result:=CreateJSON(Int64(V));
    varlongword, varqword: Result:=CreateJSON(QWord(V));
  else
    Result:=CreateJSON(VarToStr(V));
  end;
end;

function TJSONArrayCreate(Self: TClass; CreateNewInstance: Boolean; const Elements: array of const): TJSONArray;
begin Result := TJSONArray.Create(Elements); end;
function TJSONArrayAdd(Self: TJSONArray; Value: Variant): Integer;
begin Result := Self.Add(VariantToJSON(Value)); end;
function TJSONArrayAddArray(Self: TJSONArray; Arr: TJSONArray): Integer;
begin Result := Self.Add(Arr); end;
function TJSONArrayAddObject(Self: TJSONArray; Obj: TJSONObject): Integer;
begin Result := Self.Add(Obj); end;
procedure TJSONArrayInsert(Self: TJSONArray; Index: Integer; Value: Variant);
begin Self.Insert(Index, VariantToJSON(Value)); end;
procedure TJSONArrayInsertArray(Self: TJSONArray; Index: Integer; Arr: TJSONArray);
begin Self.Insert(Index, Arr); end;
procedure TJSONArrayInsertObject(Self: TJSONArray; Index: Integer; Obj: TJSONObject);
begin Self.Insert(Index, Obj); end;

function TJSONObjectCreate(Self: TClass; CreateNewInstance: Boolean; const Elements: array of const): TJSONObject;
begin Result := TJSONObject.Create(Elements); end;
function TJSONObjectAdd(Self: TJSONObject; const AName: String; Value: Variant): Integer;
begin Result := Self.Add(AName, VariantToJSON(Value)); end;
function TJSONObjectAddArray(Self: TJSONObject; const AName: String; Arr: TJSONArray): Integer;
begin Result := Self.Add(AName, Arr); end;
function TJSONObjectAddObject(Self: TJSONObject; const AName: String; Obj: TJSONObject): Integer;
begin Result := Self.Add(AName, Obj); end;
procedure TJSONObjectNames_R(Self: TJSONObject; var T: String; I: Integer); begin T := Self.Names[I]; end;
procedure TJSONObjectElements_R(Self: TJSONObject; var T: TJSONData; I: String); begin T := Self.Elements[I]; end;
procedure TJSONObjectElements_W(Self: TJSONObject; T: TJSONData; I: String); begin Self.Elements[I] := T; end;

procedure RIRegister_Json(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TJSONData) do
  begin
    //RegisterVirtualConstructor(@TJSONData.Create, 'Create');
    RegisterMethod(@TJSONDataClear, 'Clear');
    RegisterMethod(@TJSONData.FindPath, 'FindPath');
    RegisterMethod(@TJSONData.GetPath, 'GetPath');
    RegisterMethod(@TJSONDataClone, 'Clone');
    RegisterMethod(@TJSONDataJSONType, 'JSONType');
    RegisterMethod(@TJSONDataFormatJSON, 'FormatJSON');
    RegisterPropertyHelper(@TJSONDataCount_R, nil, 'Count');
    RegisterPropertyHelper(@TJSONDataItems_R, @TJSONDataItems_W, 'Items');
    RegisterPropertyHelper(@TJSONDataValue_R, @TJSONDataValue_W, 'Value');
    RegisterPropertyHelper(@TJSONDataAsString_R, @TJSONDataAsString_W, 'AsString');
    RegisterPropertyHelper(@TJSONDataAsUnicodeString_R, @TJSONDataAsUnicodeString_W, 'AsUnicodeString');
    RegisterPropertyHelper(@TJSONDataAsFloat_R, @TJSONDataAsFloat_W, 'AsFloat');
    RegisterPropertyHelper(@TJSONDataAsInteger_R, @TJSONDataAsInteger_W, 'AsInteger');
    RegisterPropertyHelper(@TJSONDataAsInt64_R, @TJSONDataAsInt64_W, 'AsInt64');
    //RegisterPropertyHelper(@TJSONDataAsQWord_R, @TJSONDataAsQWord_W, 'AsQWord');
    RegisterPropertyHelper(@TJSONDataAsBoolean_R, @TJSONDataAsBoolean_W, 'AsBoolean');
    RegisterPropertyHelper(@TJSONDataIsNull_R, nil, 'IsNull');
    RegisterPropertyHelper(@TJSONDataAsJSON_R, nil, 'AsJSON');
  end;
  with Cl.Add(TJSONArray) do
  begin
    RegisterConstructor(@TJSONArray.Create, 'Create');
    RegisterConstructor(@TJSONArrayCreate, 'CreateArray');
    //RegisterPropertyHelper(@TJSONArrayItems_R, @TJSONDataItems_W, 'Items');
    RegisterMethod(@TJSONArrayAdd, 'Add');
    RegisterMethod(@TJSONArrayAddArray, 'AddArray');
    RegisterMethod(@TJSONArrayAddObject, 'AddObject');
    RegisterMethod(@TJSONArray.Delete, 'Delete');
    RegisterMethod(@TJSONArray.Exchange, 'Exchange');
    RegisterMethod(@TJSONArrayInsert, 'Insert');
    RegisterMethod(@TJSONArrayInsertArray, 'InsertArray');
    RegisterMethod(@TJSONArrayInsertObject, 'InsertObject');
    RegisterMethod(@TJSONArray.Remove, 'Remove');
    RegisterMethod(@TJSONArray.IndexOf, 'IndexOf');
  end;

  with Cl.Add(TJSONObject) do
  begin
    RegisterConstructor(@TJSONObject.Create, 'Create');
    RegisterConstructor(@TJSONObjectCreate, 'CreateObject');
    RegisterMethod(@TJSONObjectAdd, 'Add');
    RegisterMethod(@TJSONObjectAddArray, 'AddArray');
    RegisterMethod(@TJSONObjectAddObject, 'AddObject');
    RegisterMethod(@TJSONObject.Delete, 'Delete');
    RegisterMethod(@TJSONObject.Remove, 'Remove');
    RegisterMethod(@TJSONObject.IndexOf, 'IndexOf');
    RegisterMethod(@TJSONObject.IndexOfName, 'IndexOfName');
    RegisterPropertyHelper(@TJSONObjectNames_R, nil, 'Names');
    RegisterPropertyHelper(@TJSONObjectElements_R, @TJSONObjectElements_W, 'Elements');
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// XML
////////////////////////////////////////////////////////////////////////////////

procedure TDomNodeNodeName_R(Self: TDOMNode; var T: String); begin T := Self.NodeName; end;
procedure TDomNodeNodeValue_R(Self: TDOMNode; var T: String); begin T := Self.NodeValue; end;
procedure TDomNodeNodeValue_W(Self: TDOMNode; const T: String); begin Self.NodeValue := T; end;
procedure TDomNodeNodeType_R(Self: TDOMNode; var T: Integer); begin T := Self.NodeType; end;
procedure TDomNodeParentNode_R(Self: TDOMNode; var T: TDOMNode); begin T := Self.ParentNode; end;
procedure TDomNodeFirstChild_R(Self: TDOMNode; var T: TDOMNode); begin T := Self.FirstChild; end;
procedure TDomNodeLastChild_R(Self: TDOMNode; var T: TDOMNode); begin T := Self.LastChild; end;
procedure TDomNodeChildNodes_R(Self: TDOMNode; var T: TDOMNodeList); begin T := Self.ChildNodes; end;
procedure TDomNodePreviousSibling_R(Self: TDOMNode; var T: TDOMNode); begin T := Self.PreviousSibling; end;
procedure TDomNodeNextSibling_R(Self: TDOMNode; var T: TDOMNode); begin T := Self.NextSibling; end;
procedure TDomNodeOwnerDocument_R(Self: TDOMNode; var T: TXmlDocument); begin T := TXmlDocument(Self.OwnerDocument); end;
procedure TDomNodeAttrCount_R(Self: TDOMNode; var T: LongWord); begin T := Self.Attributes.Length; end;
procedure TDomNodeAttrs_R(Self: TDOMNode; var T: String; I: String); begin T := (Self as TDOMElement).AttribStrings[I]; end;
procedure TDomNodeAttrs_W(Self: TDOMNode; const T: String; I: String); begin (Self as TDOMElement).AttribStrings[I] := T end;
procedure TDomNodeAttr_R(Self: TDOMNode; var T: TDOMNode; I: LongWord); begin T := Self.Attributes[I]; end;
function TDomNodeCloneNode(Self: TDOMNode; Deep: Boolean): TDOMNode; begin Result := Self.CloneNode(Deep); end;
function TDomNodeRemoveAttr(Self: TDOMNode; const AName: String): TDOMNode; begin Result := Self.Attributes.RemoveNamedItem(AName); end;
function TDomNodeAttrExists(Self: TDOMNode; const AName: String): Boolean; begin Result := Self.Attributes.GetNamedItem(AName) <> nil; end;

procedure TXmlDocumentRoot_R(Self: TXmlDocument; var T: TDomNode); begin T := Self.DocumentElement; end;
function TXmlDocumentCreateNode(Self: TXmlDocument; const T: String): TDOMNode; begin Result := Self.CreateElement(T); end;
function TXmlDocumentCreateText(Self: TXmlDocument; const T: String): TDOMNode; begin Result := Self.CreateTextNode(T); end;
function TXmlDocumentCreateCDATA(Self: TXmlDocument; const T: String): TDOMNode; begin Result := Self.CreateCDATASection(T); end;

procedure TDomNodeListItem_R(Self: TDomNodeList; var T: TDomNode; I: LongWord); begin T := Self[I]; end;
procedure TDomNodeListCount_R(Self: TDomNodeList; var T: LongWord); begin T := Self.Count; end;

procedure RIRegister_Xml(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TDOMNode) do
  begin
    RegisterPropertyHelper(@TDomNodeNodeName_R, nil, 'NodeName');
    RegisterPropertyHelper(@TDomNodeNodeValue_R, @TDomNodeNodeValue_W, 'NodeValue');
    RegisterPropertyHelper(@TDomNodeNodeType_R, nil, 'NodeType');
    RegisterPropertyHelper(@TDomNodeParentNode_R, nil, 'ParentNode');
    RegisterPropertyHelper(@TDomNodeFirstChild_R, nil, 'FirstChild');
    RegisterPropertyHelper(@TDomNodeLastChild_R, nil, 'LastChild');
    RegisterPropertyHelper(@TDomNodeChildNodes_R, nil, 'ChildNodes');
    RegisterPropertyHelper(@TDomNodePreviousSibling_R, nil, 'PreviousSibling');
    RegisterPropertyHelper(@TDomNodeNextSibling_R, nil, 'NextSibling');
    RegisterPropertyHelper(@TDomNodeOwnerDocument_R, nil, 'OwnerDocument');
    RegisterPropertyHelper(@TDomNodeAttrCount_R, nil, 'AttrCount');
    RegisterPropertyHelper(@TDomNodeAttrs_R, @TDomNodeAttrs_W, 'Attrs');
    RegisterPropertyHelper(@TDomNodeAttr_R, nil, 'Attr');

    RegisterVirtualMethod(@TDOMNode.InsertBefore, 'InsertBefore');
    RegisterVirtualMethod(@TDOMNode.ReplaceChild, 'ReplaceChild');
    //RegisterVirtualMethod(@TDOMNode.DetachChild, 'DetachChild');
    RegisterMethod(@TDOMNode.RemoveChild, 'RemoveChild');
    RegisterMethod(@TDOMNode.AppendChild, 'AppendChild');
    RegisterVirtualMethod(@TDOMNode.HasChildNodes, 'HasChildNodes');
    RegisterMethod(@TDOMNodeCloneNode, 'CloneNode');
    RegisterMethod(@TDOMNode.GetLevel, 'GetLevel');
    RegisterVirtualMethod(@TDOMNode.FindNode, 'FindNode');
    RegisterMethod(@TDOMNodeRemoveAttr, 'RemoveAttr');
    RegisterMethod(@TDOMNodeAttrExists, 'AttrExists');
  end;

  with Cl.Add(TXmlDocument) do
  begin
    RegisterPropertyHelper(@TXmlDocumentRoot_R, nil, 'Root');
    RegisterConstructor(@TXmlDocument.Create, 'Create');
    RegisterMethod(@TXmlDocumentCreateNode, 'CreateNode');
    RegisterMethod(@TXmlDocumentCreateText, 'CreateText');
    RegisterMethod(@TXmlDocumentCreateCDATA, 'CreateCDATA');
  end;

  with Cl.Add(TDomNodeList) do
  begin
    RegisterPropertyHelper(@TDomNodeListItem_R, nil, 'Item');
    RegisterPropertyHelper(@TDomNodeListCount_R, nil, 'Count');
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Http
////////////////////////////////////////////////////////////////////////////////

procedure TemplateStartDelimiter_R(Self: TTemplate; var T: String); begin T := Self.StartDelimiter; end;
procedure TemplateStartDelimiter_W(Self: TTemplate; const T: String); begin Self.StartDelimiter := T; end;
procedure TemplateEndDelimiter_R(Self: TTemplate; var T: String); begin T := Self.EndDelimiter; end;
procedure TemplateEndDelimiter_W(Self: TTemplate; const T: String); begin Self.EndDelimiter := T; end;
procedure TemplateParamStartDelimiter_R(Self: TTemplate; var T: String); begin T := Self.ParamStartDelimiter; end;
procedure TemplateParamStartDelimiter_W(Self: TTemplate; const T: String); begin Self.ParamStartDelimiter := T; end;
procedure TemplateParamEndDelimiter_R(Self: TTemplate; var T: String); begin T := Self.ParamEndDelimiter; end;
procedure TemplateParamEndDelimiter_W(Self: TTemplate; const T: String); begin Self.ParamEndDelimiter := T; end;
procedure TemplateParamValueSeparator_R(Self: TTemplate; var T: String); begin T := Self.ParamValueSeparator; end;
procedure TemplateParamValueSeparator_W(Self: TTemplate; const T: String); begin Self.ParamValueSeparator := T; end;
procedure TemplateTags_R(Self: TTemplate; var T: Variant; const I: String); begin T := Self.Tags[I]; end;
procedure TemplateTags_W(Self: TTemplate; const T: Variant; const I: String); begin Self.Tags[I] := T; end;
procedure TemplateTagByIndex_R(Self: TTemplate; var T: Variant; I: Integer); begin T := Self.TagByIndex[I]; end;
procedure TemplateTagByIndex_W(Self: TTemplate; const T: Variant; I: Integer); begin Self.TagByIndex[I] := T; end;
procedure TemplateTagCount_R(Self: TTemplate; var T: Integer); begin T := Self.TagCount; end;
{procedure TemplateOnReplaceTag_R(Self: TTemplate; var T: TReplaceTagEvent); begin T := Self.OnReplaceTag; end;
procedure TemplateOnReplaceTag_W(Self: TTemplate; T: TReplaceTagEvent); begin Self.OnReplaceTag := T; end;  }

procedure RIRegister_Template(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TTemplate) do
  begin
    RegisterConstructor(@TTemplate.Create, 'Create');
    RegisterMethod(@TTemplate.HasContent, 'HasContent');
    RegisterMethod(@TTemplate.GetContent, 'GetContent');
    RegisterMethod(@TTemplate.ClearTags, 'ClearTags');
    RegisterPropertyHelper(@TemplateStartDelimiter_R, @TemplateStartDelimiter_W, 'StartDelimiter');
    RegisterPropertyHelper(@TemplateEndDelimiter_R, @TemplateEndDelimiter_W, 'EndDelimiter');
    RegisterPropertyHelper(@TemplateParamStartDelimiter_R, @TemplateParamStartDelimiter_W, 'ParamStartDelimiter');
    RegisterPropertyHelper(@TemplateParamEndDelimiter_R, @TemplateParamEndDelimiter_W, 'ParamEndDelimiter');
    RegisterPropertyHelper(@TemplateParamValueSeparator_R, @TemplateParamValueSeparator_W, 'ParamValueSeparator');
    RegisterPropertyHelper(@TemplateTags_R, @TemplateTags_W, 'Tags');
    RegisterPropertyHelper(@TemplateTagByIndex_R, @TemplateTagByIndex_W, 'TagByIndex');
    RegisterPropertyHelper(@TemplateTagCount_R, nil, 'TagCount');
    //RegisterEventPropertyHelper(@TemplateOnReplaceTag_R, @TemplateOnReplaceTag_W, 'OnReplaceTag');
  end;
end;

procedure ProxyDataHost_R(Self: TProxyData; var T: String); begin T := Self.Host; end;
procedure ProxyDataHost_W(Self: TProxyData; const T: String); begin Self.Host := T; end;
procedure ProxyDataPort_R(Self: TProxyData; var T: Word); begin T := Self.Port; end;
procedure ProxyDataPort_W(Self: TProxyData; T: Word); begin Self.Port := T; end;
procedure ProxyDataUserName_R(Self: TProxyData; var T: String); begin T := Self.UserName; end;
procedure ProxyDataUserName_W(Self: TProxyData; const T: String); begin Self.UserName := T; end;
procedure ProxyDataPassword_R(Self: TProxyData; var T: String); begin T := Self.Password; end;
procedure ProxyDataPassword_W(Self: TProxyData; const T: String); begin Self.Password := T; end;

procedure HttpClientTerminated_R(Self: THttpClient; var T: Boolean); begin T := Self.Terminated; end;
procedure HttpClientConnectionCount_R(Self: THttpClient; var T: Integer); begin T := Self.ConnectionCount; end;

procedure RIRegister_HttpClient(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TProxyData) do
  begin
    RegisterPropertyHelper(@ProxyDataHost_R, @ProxyDataHost_W, 'Host');
    RegisterPropertyHelper(@ProxyDataPort_R, @ProxyDataPort_W, 'Port');
    RegisterPropertyHelper(@ProxyDataUserName_R, @ProxyDataUserName_W, 'UserName');
    RegisterPropertyHelper(@ProxyDataPassword_R, @ProxyDataPassword_W, 'Password');
  end;

  with Cl.Add(THttpClient) do
  begin
    RegisterConstructor(@THttpClient.Create, 'Create');
    RegisterMethod(@THttpClient.Terminate, 'Terminate');
    RegisterMethod(@THttpClient.MyIndexOfHeader, 'IndexOfHeader');
    RegisterMethod(@THttpClient.MyAddHeader, 'AddHeader');
    RegisterMethod(@THttpClient.MyGetHeader, 'GetHeader');
    RegisterMethod(@THttpClient.Send, 'Send');
    RegisterMethod(@THttpClient.MyFormPost, 'FormPost');
    RegisterMethod(@THttpClient.MyStreamFormPost, 'StreamFormPost');

    RegisterPropertyHelper(@HttpClientTerminated_R, nil, 'Terminated');
    RegisterPropertyHelper(@HttpClientConnectionCount_R, nil, 'ConnectionCount');
  end;
end;

procedure HttpHeaderAccept_R(Self: THttpHeader; var T: String); begin T := Self.Accept; end;
procedure HttpHeaderAccept_W(Self: THttpHeader; const T: String); begin Self.Accept := T; end;
procedure HttpHeaderAcceptCharset_R(Self: THttpHeader; var T: String); begin T := Self.AcceptCharset; end;
procedure HttpHeaderAcceptCharset_W(Self: THttpHeader; const T: String); begin Self.AcceptCharset := T; end;
procedure HttpHeaderAcceptEncoding_R(Self: THttpHeader; var T: String); begin T := Self.AcceptEncoding; end;
procedure HttpHeaderAcceptEncoding_W(Self: THttpHeader; const T: String); begin Self.AcceptEncoding := T; end;
procedure HttpHeaderAcceptLanguage_R(Self: THttpHeader; var T: String); begin T := Self.AcceptLanguage; end;
procedure HttpHeaderAcceptLanguage_W(Self: THttpHeader; const T: String); begin Self.AcceptLanguage := T; end;
procedure HttpHeaderAuthorization_R(Self: THttpHeader; var T: String); begin T := Self.Authorization; end;
procedure HttpHeaderAuthorization_W(Self: THttpHeader; const T: String); begin Self.Authorization := T; end;
procedure HttpHeaderConnection_R(Self: THttpHeader; var T: String); begin T := Self.Connection; end;
procedure HttpHeaderConnection_W(Self: THttpHeader; const T: String); begin Self.Connection := T; end;
procedure HttpHeaderContentEncoding_R(Self: THttpHeader; var T: String); begin T := Self.ContentEncoding; end;
procedure HttpHeaderContentEncoding_W(Self: THttpHeader; const T: String); begin Self.ContentEncoding := T; end;
procedure HttpHeaderContentLanguage_R(Self: THttpHeader; var T: String); begin T := Self.ContentLanguage; end;
procedure HttpHeaderContentLanguage_W(Self: THttpHeader; const T: String); begin Self.ContentLanguage := T; end;
procedure HttpHeaderContentLength_R(Self: THttpHeader; var T: Integer); begin T := Self.ContentLength; end;
procedure HttpHeaderContentLength_W(Self: THttpHeader; const T: Integer); begin Self.ContentLength := T; end;
procedure HttpHeaderContentType_R(Self: THttpHeader; var T: String); begin T := Self.ContentType; end;
procedure HttpHeaderContentType_W(Self: THttpHeader; const T: String); begin Self.ContentType := T; end;
procedure HttpHeaderDate_R(Self: THttpHeader; var T: String); begin T := Self.Date; end;
procedure HttpHeaderDate_W(Self: THttpHeader; const T: String); begin Self.Date := T; end;
procedure HttpHeaderExpires_R(Self: THttpHeader; var T: String); begin T := Self.Expires; end;
procedure HttpHeaderExpires_W(Self: THttpHeader; const T: String); begin Self.Expires := T; end;
procedure HttpHeaderFrom_R(Self: THttpHeader; var T: String); begin T := Self.From; end;
procedure HttpHeaderFrom_W(Self: THttpHeader; const T: String); begin Self.From := T; end;
procedure HttpHeaderHost_R(Self: THttpHeader; var T: String); begin T := Self.Host; end;
procedure HttpHeaderHost_W(Self: THttpHeader; const T: String); begin Self.Host := T; end;
procedure HttpHeaderIfModifiedSince_R(Self: THttpHeader; var T: String); begin T := Self.IfModifiedSince; end;
procedure HttpHeaderIfModifiedSince_W(Self: THttpHeader; const T: String); begin Self.IfModifiedSince := T; end;
procedure HttpHeaderLastModified_R(Self: THttpHeader; var T: String); begin T := Self.LastModified; end;
procedure HttpHeaderLastModified_W(Self: THttpHeader; const T: String); begin Self.LastModified := T; end;
procedure HttpHeaderLocation_R(Self: THttpHeader; var T: String); begin T := Self.Location; end;
procedure HttpHeaderLocation_W(Self: THttpHeader; const T: String); begin Self.Location := T; end;
procedure HttpHeaderPragma_R(Self: THttpHeader; var T: String); begin T := Self.Pragma; end;
procedure HttpHeaderPragma_W(Self: THttpHeader; const T: String); begin Self.Pragma := T; end;
procedure HttpHeaderReferer_R(Self: THttpHeader; var T: String); begin T := Self.Referer; end;
procedure HttpHeaderReferer_W(Self: THttpHeader; const T: String); begin Self.Referer := T; end;
procedure HttpHeaderRetryAfter_R(Self: THttpHeader; var T: String); begin T := Self.RetryAfter; end;
procedure HttpHeaderRetryAfter_W(Self: THttpHeader; const T: String); begin Self.RetryAfter := T; end;
procedure HttpHeaderServer_R(Self: THttpHeader; var T: String); begin T := Self.Server; end;
procedure HttpHeaderServer_W(Self: THttpHeader; const T: String); begin Self.Server := T; end;
procedure HttpHeaderUserAgent_R(Self: THttpHeader; var T: String); begin T := Self.UserAgent; end;
procedure HttpHeaderUserAgent_W(Self: THttpHeader; const T: String); begin Self.UserAgent := T; end;
procedure HttpHeaderWarning_R(Self: THttpHeader; var T: String); begin T := Self.Warning; end;
procedure HttpHeaderWarning_W(Self: THttpHeader; const T: String); begin Self.Warning := T; end;
procedure HttpHeaderWWWAuthenticate_R(Self: THttpHeader; var T: String); begin T := Self.WWWAuthenticate; end;
procedure HttpHeaderWWWAuthenticate_W(Self: THttpHeader; const T: String); begin Self.WWWAuthenticate := T; end;
procedure HttpHeaderVia_R(Self: THttpHeader; var T: String); begin T := Self.Via; end;
procedure HttpHeaderVia_W(Self: THttpHeader; const T: String); begin Self.Via := T; end;
procedure HttpHeaderCookie_R(Self: THttpHeader; var T: String); begin T := Self.Cookie; end;
procedure HttpHeaderCookie_W(Self: THttpHeader; const T: String); begin Self.Cookie := T; end;
procedure HttpHeaderSetCookie_R(Self: THttpHeader; var T: String); begin T := Self.SetCookie; end;
procedure HttpHeaderSetCookie_W(Self: THttpHeader; const T: String); begin Self.SetCookie := T; end;
procedure HttpHeaderHTTPXRequestedWith_R(Self: THttpHeader; var T: String); begin T := Self.HTTPXRequestedWith; end;
procedure HttpHeaderHTTPXRequestedWith_W(Self: THttpHeader; const T: String); begin Self.HTTPXRequestedWith := T; end;
procedure HttpHeaderHttpVersion_R(Self: THttpHeader; var T: String); begin T := Self.HttpVersion; end;
procedure HttpHeaderHttpVersion_W(Self: THttpHeader; const T: String); begin Self.HttpVersion := T; end;
procedure HttpHeaderProtocolVersion_R(Self: THttpHeader; var T: String); begin T := Self.ProtocolVersion; end;
procedure HttpHeaderProtocolVersion_W(Self: THttpHeader; const T: String); begin Self.ProtocolVersion := T; end;
procedure HttpHeaderPathInfo_R(Self: THttpHeader; var T: String); begin T := Self.PathInfo; end;
procedure HttpHeaderPathInfo_W(Self: THttpHeader; const T: String); begin Self.PathInfo := T; end;
procedure HttpHeaderPathTranslated_R(Self: THttpHeader; var T: String); begin T := Self.PathTranslated; end;
procedure HttpHeaderPathTranslated_W(Self: THttpHeader; const T: String); begin Self.PathTranslated := T; end;
procedure HttpHeaderRemoteAddr_R(Self: THttpHeader; var T: String); begin T := Self.RemoteAddr; end;
procedure HttpHeaderRemoteAddr_W(Self: THttpHeader; const T: String); begin Self.RemoteAddr := T; end;
procedure HttpHeaderRemoteHost_R(Self: THttpHeader; var T: String); begin T := Self.RemoteHost; end;
procedure HttpHeaderRemoteHost_W(Self: THttpHeader; const T: String); begin Self.RemoteHost := T; end;
procedure HttpHeaderScriptName_R(Self: THttpHeader; var T: String); begin T := Self.ScriptName; end;
procedure HttpHeaderScriptName_W(Self: THttpHeader; const T: String); begin Self.ScriptName := T; end;
procedure HttpHeaderServerPort_R(Self: THttpHeader; var T: Word); begin T := Self.ServerPort; end;
procedure HttpHeaderServerPort_W(Self: THttpHeader; const T: Word); begin Self.ServerPort := T; end;
procedure HttpHeaderMethod_R(Self: THttpHeader; var T: String); begin T := Self.Method; end;
procedure HttpHeaderMethod_W(Self: THttpHeader; const T: String); begin Self.Method := T; end;
procedure HttpHeaderURL_R(Self: THttpHeader; var T: String); begin T := Self.URL; end;
procedure HttpHeaderURL_W(Self: THttpHeader; const T: String); begin Self.URL := T; end;
procedure HttpHeaderQuery_R(Self: THttpHeader; var T: String); begin T := Self.Query; end;
procedure HttpHeaderQuery_W(Self: THttpHeader; const T: String); begin Self.Query := T; end;
procedure HttpHeaderContent_R(Self: THttpHeader; var T: String); begin T := Self.Content; end;
procedure HttpHeaderContent_W(Self: THttpHeader; const T: String); begin Self.Content := T; end;
procedure HttpHeaderCookieFields_R(Self: THttpHeader; var T: TStrings); begin T := Self.CookieFields; end;
procedure HttpHeaderCookieFields_W(Self: THttpHeader; const T: TStrings); begin Self.CookieFields := T; end;
procedure HttpHeaderContentFields_R(Self: THttpHeader; var T: TStrings); begin T := Self.ContentFields; end;
procedure HttpHeaderQueryFields_R(Self: THttpHeader; var T: TStrings); begin T := Self.QueryFields; end;
procedure HttpHeaderCustomHeaders_R(Self: THttpHeader; var T: TStringList); begin T := Self.CustomHeaders; end;

procedure UploadedFileFieldName_R(Self: TUploadedFile; var T: String); begin T := Self.FieldName; end;
procedure UploadedFileFieldName_W(Self: TUploadedFile; const T: String); begin Self.FieldName := T; end;
procedure UploadedFileFileName_R(Self: TUploadedFile; var T: String); begin T := Self.FileName; end;
procedure UploadedFileFileName_W(Self: TUploadedFile; const T: String); begin Self.FileName := T; end;
procedure UploadedFileStream_R(Self: TUploadedFile; var T: TStream); begin T := Self.Stream; end;
procedure UploadedFileSize_R(Self: TUploadedFile; var T: Int64); begin T := Self.Size; end;
procedure UploadedFileSize_W(Self: TUploadedFile; const T: Int64); begin Self.Size := T; end;
procedure UploadedFileContentType_R(Self: TUploadedFile; var T: String); begin T := Self.ContentType; end;
procedure UploadedFileContentType_W(Self: TUploadedFile; const T: String); begin Self.ContentType := T; end;
procedure UploadedFileDisposition_R(Self: TUploadedFile; var T: String); begin T := Self.Disposition; end;
procedure UploadedFileDisposition_W(Self: TUploadedFile; const T: String); begin Self.Disposition := T; end;
procedure UploadedFileLocalFileName_R(Self: TUploadedFile; var T: String); begin T := Self.LocalFileName; end;
procedure UploadedFileLocalFileName_W(Self: TUploadedFile; const T: String); begin Self.LocalFileName := T; end;
procedure UploadedFileDescription_R(Self: TUploadedFile; var T: String); begin T := Self.Description; end;
procedure UploadedFileDescription_W(Self: TUploadedFile; const T: String); begin Self.Description := T; end;

procedure UploadedFilesFiles_R(Self: TUploadedFiles; var T: TUploadedFile; I: Integer); begin T := Self.Files[I]; end;

procedure FPHTTPConnectionRequestFiles_R(Self: TFPHTTPConnectionRequest; var T: TUploadedFiles); begin T := Self.Files; end;

procedure CookieName_R(Self: TCookie; var T: String); begin T := Self.Name; end;
procedure CookieName_W(Self: TCookie; const T: String); begin Self.Name := T; end;
procedure CookieValue_R(Self: TCookie; var T: String); begin T := Self.Value; end;
procedure CookieValue_W(Self: TCookie; const T: String); begin Self.Value := T; end;
procedure CookieDomain_R(Self: TCookie; var T: String); begin T := Self.Domain; end;
procedure CookieDomain_W(Self: TCookie; const T: String); begin Self.Domain := T; end;
procedure CookiePath_R(Self: TCookie; var T: String); begin T := Self.Path; end;
procedure CookiePath_W(Self: TCookie; const T: String); begin Self.Path := T; end;
procedure CookieExpires_R(Self: TCookie; var T: TDateTime); begin T := Self.Expires; end;
procedure CookieExpires_W(Self: TCookie; const T: TDateTime); begin Self.Expires := T; end;
procedure CookieSecure_R(Self: TCookie; var T: Boolean); begin T := Self.Secure; end;
procedure CookieSecure_W(Self: TCookie; const T: Boolean); begin Self.Secure := T; end;
procedure CookieHttpOnly_R(Self: TCookie; var T: Boolean); begin T := Self.HttpOnly; end;
procedure CookieHttpOnly_W(Self: TCookie; const T: Boolean); begin Self.HttpOnly := T; end;
procedure CookieAsString_R(Self: TCookie; var T: String); begin T := Self.AsString; end;

procedure CookiesItems_R(Self: TCookies; var T: TCookie; I: Integer); begin T := Self.Items[I]; end;

procedure FPHTTPConnectionResponseCode_R(Self: TFPHTTPConnectionResponse; var T: Integer); begin T := Self.Code; end;
procedure FPHTTPConnectionResponseCode_W(Self: TFPHTTPConnectionResponse; const T: Integer); begin Self.Code := T; end;
procedure FPHTTPConnectionResponseCodeText_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.CodeText; end;
procedure FPHTTPConnectionResponseCodeText_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.CodeText := T; end;
procedure FPHTTPConnectionResponseAge_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.Age; end;
procedure FPHTTPConnectionResponseAge_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.Age := T; end;
procedure FPHTTPConnectionResponseAllow_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.Allow; end;
procedure FPHTTPConnectionResponseAllow_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.Allow := T; end;
procedure FPHTTPConnectionResponseCacheControl_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.CacheControl; end;
procedure FPHTTPConnectionResponseCacheControl_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.CacheControl := T; end;
procedure FPHTTPConnectionResponseContentLocation_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.ContentLocation; end;
procedure FPHTTPConnectionResponseContentLocation_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.ContentLocation := T; end;
procedure FPHTTPConnectionResponseContentMD5_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.ContentMD5; end;
procedure FPHTTPConnectionResponseContentMD5_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.ContentMD5 := T; end;
procedure FPHTTPConnectionResponseContentRange_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.ContentRange; end;
procedure FPHTTPConnectionResponseContentRange_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.ContentRange := T; end;
procedure FPHTTPConnectionResponseETag_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.ETag; end;
procedure FPHTTPConnectionResponseETag_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.ETag := T; end;
procedure FPHTTPConnectionResponseProxyAuthenticate_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.ProxyAuthenticate; end;
procedure FPHTTPConnectionResponseProxyAuthenticate_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.ProxyAuthenticate := T; end;
procedure FPHTTPConnectionResponseFirstHeaderLine_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.FirstHeaderLine; end;
procedure FPHTTPConnectionResponseFirstHeaderLine_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.FirstHeaderLine := T; end;
procedure FPHTTPConnectionResponseContentStream_R(Self: TFPHTTPConnectionResponse; var T: TStream); begin T := Self.ContentStream; end;
procedure FPHTTPConnectionResponseContentStream_W(Self: TFPHTTPConnectionResponse; const T: TStream); begin Self.ContentStream := T; end;
procedure FPHTTPConnectionResponseContent_R(Self: TFPHTTPConnectionResponse; var T: String); begin T := Self.Content; end;
procedure FPHTTPConnectionResponseContent_W(Self: TFPHTTPConnectionResponse; const T: String); begin Self.Content := T; end;
procedure FPHTTPConnectionResponseContents_R(Self: TFPHTTPConnectionResponse; var T: TStrings); begin T := Self.Contents; end;
procedure FPHTTPConnectionResponseContents_W(Self: TFPHTTPConnectionResponse; const T: TStrings); begin Self.Contents := T; end;
procedure FPHTTPConnectionResponseHeadersSent_R(Self: TFPHTTPConnectionResponse; var T: Boolean); begin T := Self.HeadersSent; end;
procedure FPHTTPConnectionResponseContentSent_R(Self: TFPHTTPConnectionResponse; var T: Boolean); begin T := Self.ContentSent; end;
procedure FPHTTPConnectionResponseCookies_R(Self: TFPHTTPConnectionResponse; var T: TCookies); begin T := Self.Cookies; end;
procedure FPHTTPConnectionResponseFreeContentStream_R(Self: TFPHTTPConnectionResponse; var T: Boolean); begin T := Self.FreeContentStream; end;
procedure FPHTTPConnectionResponseFreeContentStream_W(Self: TFPHTTPConnectionResponse; const T: Boolean); begin Self.FreeContentStream := T; end;

procedure RIRegister_HttpServer(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(THttpHeader) do
  begin
    RegisterPropertyHelper(@HttpHeaderAccept_R, @HttpHeaderAccept_W, 'Accept');
    RegisterPropertyHelper(@HttpHeaderAcceptCharset_R, @HttpHeaderAcceptCharset_W, 'AcceptCharset');
    RegisterPropertyHelper(@HttpHeaderAcceptEncoding_R, @HttpHeaderAcceptEncoding_W, 'AcceptEncoding');
    RegisterPropertyHelper(@HttpHeaderAcceptLanguage_R, @HttpHeaderAcceptLanguage_W, 'AcceptLanguage');
    RegisterPropertyHelper(@HttpHeaderAuthorization_R, @HttpHeaderAuthorization_W, 'Authorization');
    RegisterPropertyHelper(@HttpHeaderConnection_R, @HttpHeaderConnection_W, 'Connection');
    RegisterPropertyHelper(@HttpHeaderContentEncoding_R, @HttpHeaderContentEncoding_W, 'ContentEncoding');
    RegisterPropertyHelper(@HttpHeaderContentLanguage_R, @HttpHeaderContentLanguage_W, 'ContentLanguage');
    RegisterPropertyHelper(@HttpHeaderContentLength_R, @HttpHeaderContentLength_W, 'ContentLength');
    RegisterPropertyHelper(@HttpHeaderContentType_R, @HttpHeaderContentType_W, 'ContentType');
    RegisterPropertyHelper(@HttpHeaderDate_R, @HttpHeaderDate_W, 'Date');
    RegisterPropertyHelper(@HttpHeaderExpires_R, @HttpHeaderExpires_W, 'Expires');
    RegisterPropertyHelper(@HttpHeaderFrom_R, @HttpHeaderFrom_W, 'From');
    RegisterPropertyHelper(@HttpHeaderHost_R, @HttpHeaderHost_W, 'Host');
    RegisterPropertyHelper(@HttpHeaderIfModifiedSince_R, @HttpHeaderIfModifiedSince_W, 'IfModifiedSince');
    RegisterPropertyHelper(@HttpHeaderLastModified_R, @HttpHeaderLastModified_W, 'LastModified');
    RegisterPropertyHelper(@HttpHeaderLocation_R, @HttpHeaderLocation_W, 'Location');
    RegisterPropertyHelper(@HttpHeaderPragma_R, @HttpHeaderPragma_W, 'Pragma');
    RegisterPropertyHelper(@HttpHeaderReferer_R, @HttpHeaderReferer_W, 'Referer');
    RegisterPropertyHelper(@HttpHeaderRetryAfter_R, @HttpHeaderRetryAfter_W, 'RetryAfter');
    RegisterPropertyHelper(@HttpHeaderServer_R, @HttpHeaderServer_W, 'Server');
    RegisterPropertyHelper(@HttpHeaderUserAgent_R, @HttpHeaderUserAgent_W, 'UserAgent');
    RegisterPropertyHelper(@HttpHeaderWarning_R, @HttpHeaderWarning_W, 'Warning');
    RegisterPropertyHelper(@HttpHeaderWWWAuthenticate_R, @HttpHeaderWWWAuthenticate_W, 'WWWAuthenticate');
    RegisterPropertyHelper(@HttpHeaderVia_R, @HttpHeaderVia_W, 'Via');
    // Headers, not in HTTP spec.
    RegisterPropertyHelper(@HttpHeaderCookie_R, @HttpHeaderCookie_W, 'Cookie');
    RegisterPropertyHelper(@HttpHeaderSetCookie_R, @HttpHeaderSetCookie_W, 'SetCookie');
    RegisterPropertyHelper(@HttpHeaderHTTPXRequestedWith_R, @HttpHeaderHTTPXRequestedWith_W, 'HTTPXRequestedWith');
    RegisterPropertyHelper(@HttpHeaderHttpVersion_R, @HttpHeaderHttpVersion_W, 'HttpVersion');
    RegisterPropertyHelper(@HttpHeaderProtocolVersion_R, @HttpHeaderProtocolVersion_W, 'ProtocolVersion');
    // Specials, mostly from CGI protocol/Apache.
    RegisterPropertyHelper(@HttpHeaderPathInfo_R, @HttpHeaderPathInfo_W, 'PathInfo');
    RegisterPropertyHelper(@HttpHeaderPathTranslated_R, @HttpHeaderPathTranslated_W, 'PathTranslated');
    RegisterPropertyHelper(@HttpHeaderRemoteAddr_R, @HttpHeaderRemoteAddr_W, 'RemoteAddr');
    RegisterPropertyHelper(@HttpHeaderRemoteHost_R, @HttpHeaderRemoteHost_W, 'RemoteHost');
    RegisterPropertyHelper(@HttpHeaderScriptName_R, @HttpHeaderScriptName_W, 'ScriptName');
    RegisterPropertyHelper(@HttpHeaderServerPort_R, @HttpHeaderServerPort_W, 'ServerPort');
    RegisterPropertyHelper(@HttpHeaderMethod_R, @HttpHeaderMethod_W, 'Method');
    RegisterPropertyHelper(@HttpHeaderURL_R, @HttpHeaderURL_W, 'URL');
    RegisterPropertyHelper(@HttpHeaderQuery_R, @HttpHeaderQuery_W, 'Query');
    RegisterPropertyHelper(@HttpHeaderContent_R, @HttpHeaderContent_W, 'Content');
    // Lists
    RegisterPropertyHelper(@HttpHeaderCookieFields_R, @HttpHeaderCookieFields_W, 'CookieFields');
    RegisterPropertyHelper(@HttpHeaderContentFields_R, nil, 'ContentFields');
    RegisterPropertyHelper(@HttpHeaderQueryFields_R, nil, 'QueryFields');
    RegisterPropertyHelper(@HttpHeaderCustomHeaders_R, nil, 'CustomHeaders');
  end;

  with Cl.Add(TUploadedFile) do
  begin
    RegisterPropertyHelper(@UploadedFileFieldName_R, @UploadedFileFieldName_W, 'FieldName');
    RegisterPropertyHelper(@UploadedFileFileName_R, @UploadedFileFileName_W, 'FileName');
    RegisterPropertyHelper(@UploadedFileStream_R, nil, 'Stream');
    RegisterPropertyHelper(@UploadedFileSize_R, @UploadedFileSize_W, 'Size');
    RegisterPropertyHelper(@UploadedFileContentType_R, @UploadedFileContentType_W, 'ContentType');
    RegisterPropertyHelper(@UploadedFileDisposition_R, @UploadedFileDisposition_W, 'Disposition');
    RegisterPropertyHelper(@UploadedFileLocalFileName_R, @UploadedFileLocalFileName_W, 'LocalFileName');
    RegisterPropertyHelper(@UploadedFileDescription_R, @UploadedFileDescription_W, 'Description');
  end;

  with Cl.Add(TUploadedFiles) do
  begin
    RegisterPropertyHelper(@UploadedFilesFiles_R, nil, 'Files');
  end;

  with Cl.Add(TFPHTTPConnectionRequest) do
  begin
    RegisterPropertyHelper(@FPHTTPConnectionRequestFiles_R, nil, 'Files');
  end;

  with Cl.Add(TCookie) do
  begin
    RegisterMethod(@TCookie.Expire, 'Expire');
    RegisterPropertyHelper(@CookieName_R, @CookieName_W, 'Name');
    RegisterPropertyHelper(@CookieValue_R, @CookieValue_W, 'Value');
    RegisterPropertyHelper(@CookieDomain_R, @CookieDomain_W, 'Domain');
    RegisterPropertyHelper(@CookiePath_R, @CookiePath_W, 'Path');
    RegisterPropertyHelper(@CookieExpires_R, @CookieExpires_W, 'Expires');
    RegisterPropertyHelper(@CookieSecure_R, @CookieSecure_W, 'Secure');
    RegisterPropertyHelper(@CookieHttpOnly_R, @CookieHttpOnly_W, 'HttpOnly');
    RegisterPropertyHelper(@CookieAsString_R, nil, 'AsString');
  end;

  with Cl.Add(TCookies) do
  begin
    RegisterMethod(@TCookies.Add, 'Add');
    RegisterMethod(@TCookies.CookieByName, 'CookieByName');
    RegisterMethod(@TCookies.FindCookie, 'FindCookie');
    RegisterMethod(@TCookies.IndexOfCookie, 'IndexOfCookie');
    RegisterPropertyHelper(@CookiesItems_R, nil, 'Items');
  end;

  with Cl.Add(TFPHTTPConnectionResponse) do
  begin
    RegisterMethod(@TFPHTTPConnectionResponse.SendContent, 'SendContent');
    RegisterMethod(@TFPHTTPConnectionResponse.SendHeaders, 'SendHeaders');
    RegisterPropertyHelper(@FPHTTPConnectionResponseCode_R, @FPHTTPConnectionResponseCode_W, 'Code');
    RegisterPropertyHelper(@FPHTTPConnectionResponseCodeText_R, @FPHTTPConnectionResponseCodeText_W, 'CodeText');
    RegisterPropertyHelper(@FPHTTPConnectionResponseAge_R, @FPHTTPConnectionResponseAge_W, 'Age');
    RegisterPropertyHelper(@FPHTTPConnectionResponseAllow_R, @FPHTTPConnectionResponseAllow_W, 'Allow');
    RegisterPropertyHelper(@FPHTTPConnectionResponseCacheControl_R, @FPHTTPConnectionResponseCacheControl_W, 'CacheControl');
    RegisterPropertyHelper(@FPHTTPConnectionResponseContentLocation_R, @FPHTTPConnectionResponseContentLocation_W, 'ContentLocation');
    RegisterPropertyHelper(@FPHTTPConnectionResponseContentMD5_R, @FPHTTPConnectionResponseContentMD5_W, 'ContentMD5');
    RegisterPropertyHelper(@FPHTTPConnectionResponseContentRange_R, @FPHTTPConnectionResponseContentRange_W, 'ContentRange');
    RegisterPropertyHelper(@FPHTTPConnectionResponseETag_R, @FPHTTPConnectionResponseETag_W, 'ETag');
    RegisterPropertyHelper(@FPHTTPConnectionResponseProxyAuthenticate_R, @FPHTTPConnectionResponseProxyAuthenticate_W, 'ProxyAuthenticate');
    //RegisterPropertyHelper(@FPHTTPConnectionResponseRetryAfter_R, @FPHTTPConnectionResponseRetryAfter_W, 'RetryAfter');
    RegisterPropertyHelper(@FPHTTPConnectionResponseFirstHeaderLine_R, @FPHTTPConnectionResponseFirstHeaderLine_W, 'FirstHeaderLine');
    RegisterPropertyHelper(@FPHTTPConnectionResponseContentStream_R, @FPHTTPConnectionResponseContentStream_W, 'ContentStream');
    RegisterPropertyHelper(@FPHTTPConnectionResponseContent_R, @FPHTTPConnectionResponseContent_W, 'Content');
    RegisterPropertyHelper(@FPHTTPConnectionResponseContents_R, @FPHTTPConnectionResponseContents_W, 'Contents');
    RegisterPropertyHelper(@FPHTTPConnectionResponseHeadersSent_R, nil, 'HeadersSent');
    RegisterPropertyHelper(@FPHTTPConnectionResponseContentSent_R, nil, 'ContentSent');
    RegisterPropertyHelper(@FPHTTPConnectionResponseCookies_R, nil, 'Cookies');
    RegisterPropertyHelper(@FPHTTPConnectionResponseFreeContentStream_R, @FPHTTPConnectionResponseFreeContentStream_W, 'FreeContentStream');
  end;

end;

////////////////////////////////////////////////////////////////////////////////
// Ini
////////////////////////////////////////////////////////////////////////////////

procedure TIniFileFileName_R(Self: TIniFile; var T: String); begin T := Self.FileName; end;
procedure TIniFileEscapeLineFeeds_R(Self: TIniFile; var T: Boolean); begin T := Self.EscapeLineFeeds; end;
procedure TIniFileCaseSensitive_R(Self: TIniFile; var T: Boolean); begin T := Self.CaseSensitive; end;
procedure TIniFileCaseSensitive_W(Self: TIniFile; T: Boolean); begin Self.CaseSensitive := T; end;
procedure TIniFileStripQuotes_R(Self: TIniFile; var T: Boolean); begin T := Self.StripQuotes; end;
procedure TIniFileStripQuotes_W(Self: TIniFile; T: Boolean); begin Self.StripQuotes := T; end;
procedure TIniFileCacheUpdates_R(Self: TIniFile; var T: Boolean); begin T := Self.CacheUpdates; end;
procedure TIniFileCacheUpdates_W(Self: TIniFile; T: Boolean); begin Self.CacheUpdates := T; end;

function TIniFileCreate(Self: TClass; CreateNewInstance: Boolean; FileName: string; AOptions: TIniFileOptions): TObject;
begin
  Result := TIniFile.Create(FileName, AOptions);
end;

procedure RIRegister_IniFiles(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TIniFile) do
  begin
    RegisterConstructor(@TIniFileCreate, 'Create');
    RegisterMethod(@TIniFile.SectionExists, 'SectionExists');
    RegisterMethod(@TIniFile.ReadString, 'ReadString');
    RegisterMethod(@TIniFile.WriteString, 'WriteString');
    RegisterMethod(@TIniFile.ReadInteger, 'ReadInteger');
    RegisterMethod(@TIniFile.WriteInteger, 'WriteInteger');
    RegisterMethod(@TIniFile.ReadInt64, 'ReadInt64');
    RegisterMethod(@TIniFile.WriteInt64, 'WriteInt64');
    RegisterMethod(@TIniFile.ReadBool, 'ReadBool');
    RegisterMethod(@TIniFile.WriteBool, 'WriteBool');
    RegisterMethod(@TIniFile.ReadDate, 'ReadDate');
    RegisterMethod(@TIniFile.ReadDateTime, 'ReadDateTime');
    RegisterMethod(@TIniFile.ReadFloat, 'ReadFloat');
    RegisterMethod(@TIniFile.ReadTime, 'ReadTime');
    RegisterMethod(@TIniFile.ReadBinaryStream, 'ReadBinaryStream');
    RegisterMethod(@TIniFile.WriteDate, 'WriteDate');
    RegisterMethod(@TIniFile.WriteDateTime, 'WriteDateTime');
    RegisterMethod(@TIniFile.WriteFloat, 'WriteFloat');
    RegisterMethod(@TIniFile.WriteTime, 'WriteTime');
    RegisterMethod(@TIniFile.WriteBinaryStream, 'WriteBinaryStream');
    RegisterMethod(@TIniFile.ReadSection, 'ReadSection');
    RegisterMethod(@TIniFile.ReadSections, 'ReadSections');
    RegisterMethod(@TIniFile.ReadSectionValues, 'ReadSectionValues');
    RegisterMethod(@TIniFile.EraseSection, 'EraseSection');
    RegisterMethod(@TIniFile.DeleteKey, 'DeleteKey');
    RegisterMethod(@TIniFile.UpdateFile, 'UpdateFile');
    RegisterMethod(@TIniFile.ValueExists, 'ValueExists');
    RegisterPropertyHelper(@TIniFileFileName_R, nil, 'FileName');
    RegisterPropertyHelper(@TIniFileEscapeLineFeeds_R, nil, 'EscapeLineFeeds');
    RegisterPropertyHelper(@TIniFileCaseSensitive_R, @TIniFileCaseSensitive_W, 'CaseSensitive');
    RegisterPropertyHelper(@TIniFileStripQuotes_R, @TIniFileStripQuotes_W, 'StripQuotes');

    RegisterMethod(@TIniFile.ReadSectionRaw, 'ReadSectionRaw');
    RegisterPropertyHelper(@TIniFileCacheUpdates_R, @TIniFileCacheUpdates_W, 'CacheUpdates');
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// SQLQuery
////////////////////////////////////////////////////////////////////////////////

procedure TdxSQLQueryFields_R(Self: TdxSQLQuery; var T: Variant; I: String); begin T := Self.Fields[I]; end;
procedure TdxSQLQueryFields_W(Self: TdxSQLQuery; T: Variant; I: String); begin Self.Fields[I] := T; end;
procedure TdxSQLQueryField_R(Self: TdxSQLQuery; var T: TField; I: Integer); begin T := Self.Field[I]; end;
procedure TdxSQLQueryAsI_R(Self: TdxSQLQuery; var T: Integer; I: String); begin T := Self.AsI[I]; end;
procedure TdxSQLQueryAsI_W(Self: TdxSQLQuery; T: Integer; I: String); begin Self.AsI[I] := T; end;
procedure TdxSQLQueryAsF_R(Self: TdxSQLQuery; var T: Double; I: String); begin T := Self.AsF[I]; end;
procedure TdxSQLQueryAsF_W(Self: TdxSQLQuery; T: Double; I: String); begin Self.AsF[I] := T; end;
procedure TdxSQLQueryAsDT_R(Self: TdxSQLQuery; var T: TDateTime; I: String); begin T := Self.AsDT[I]; end;
procedure TdxSQLQueryAsDT_W(Self: TdxSQLQuery; T: TDateTime; I: String); begin Self.AsDT[I] := T; end;
procedure TdxSQLQueryAsS_R(Self: TdxSQLQuery; var T: String; I: String); begin T := Self.AsS[I]; end;
procedure TdxSQLQueryAsS_W(Self: TdxSQLQuery; T: String; I: String); begin Self.AsS[I] := T; end;
procedure TdxSQLQueryState_R(Self: TdxSQLQuery; var T: TDataSetState); begin T := Self.State; end;
procedure TdxSQLQueryUseGenerator_R(Self: TdxSQLQuery; var T: TUseGeneratorOption); begin T := Self.UseGenerator; end;
procedure TdxSQLQueryUseGenerator_W(Self: TdxSQLQuery; T: TUseGeneratorOption); begin Self.UseGenerator := T; end;
procedure TdxSQLQueryUseExecuteBlock_R(Self: TdxSQLQuery; var T: Boolean); begin T := Self.UseExecuteBlock; end;
procedure TdxSQLQueryUseExecuteBlock_W(Self: TdxSQLQuery; T: Boolean); begin Self.UseExecuteBlock := T; end;

procedure RIRegister_dxSQLQuery(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxSQLQuery) do
  begin
    {RegisterConstructor(@TdxSQLQuery.Create, 'Create');
    RegisterMethod(@TdxSQLQuery.Open, 'Open');
    RegisterMethod(@TdxSQLQuery.Close, 'Close');
    RegisterMethod(@TdxSQLQuery.Opened, 'Opened');}
    RegisterMethod(@TdxSQLQuery.Append, 'Append');
    RegisterMethod(@TdxSQLQuery.Edit, 'Edit');
    RegisterMethod(@TdxSQLQuery.Delete, 'Delete');
    RegisterMethod(@TdxSQLQuery.Cancel, 'Cancel');
    RegisterMethod(@TdxSQLQuery.Post, 'Post');
    RegisterMethod(@TdxSQLQuery.ApplyUpdates, 'ApplyUpdates');
    RegisterMethod(@TdxSQLQuery.CancelUpdates, 'CancelUpdates');
    RegisterMethod(@TdxSQLQuery.MoveFirst, 'MoveFirst');
    RegisterMethod(@TdxSQLQuery.MovePrior, 'MovePrior');
    RegisterMethod(@TdxSQLQuery.MoveNext, 'MoveNext');
    RegisterMethod(@TdxSQLQuery.MoveLast, 'MoveLast');
    RegisterMethod(@TdxSQLQuery.MoveBy, 'MoveBy');
    RegisterMethod(@TdxSQLQuery.MoveTo, 'MoveTo');
    RegisterMethod(@TdxSQLQuery.BOF, 'BOF');
    RegisterMethod(@TdxSQLQuery.EOF, 'EOF');
    RegisterMethod(@TdxSQLQuery.RecNo, 'RecNo');
    RegisterMethod(@TdxSQLQuery.RecordCount, 'RecordCount');
    RegisterMethod(@TdxSQLQuery.FieldCount, 'FieldCount');
    RegisterMethod(@TdxSQLQuery.Locate, 'Locate');
    RegisterMethod(@TdxSQLQuery.LoadFromStream, 'LoadFromStream');
    RegisterMethod(@TdxSQLQuery.SaveToStream, 'SaveToStream');
    RegisterPropertyHelper(@TdxSQLQueryFields_R, @TdxSQLQueryFields_W, 'Fields');
    RegisterPropertyHelper(@TdxSQLQueryField_R, nil, 'Field');
    RegisterPropertyHelper(@TdxSQLQueryAsI_R, @TdxSQLQueryAsI_W, 'AsI');
    RegisterPropertyHelper(@TdxSQLQueryAsF_R, @TdxSQLQueryAsF_W, 'AsF');
    RegisterPropertyHelper(@TdxSQLQueryAsDT_R, @TdxSQLQueryAsDT_W, 'AsDT');
    RegisterPropertyHelper(@TdxSQLQueryAsS_R, @TdxSQLQueryAsS_W, 'AsS');
    RegisterPropertyHelper(@TdxSQLQueryState_R, nil, 'State');
    RegisterPropertyHelper(@TdxSQLQueryUseGenerator_R, @TdxSQLQueryUseGenerator_W, 'UseGenerator');
    RegisterPropertyHelper(@TdxSQLQueryUseExecuteBlock_R, @TdxSQLQueryUseExecuteBlock_W, 'UseExecuteBlock');
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TField
////////////////////////////////////////////////////////////////////////////////

procedure TFIELDREADONLY_W(Self: TFIELD; const T: BOOLEAN); begin Self.READONLY := T; end;
procedure TFIELDREADONLY_R(Self: TFIELD; var T: BOOLEAN); begin T := Self.READONLY; end;
procedure TFIELDALIGNMENT_W(Self: TFIELD; const T: TALIGNMENT); begin Self.ALIGNMENT := T; end;
procedure TFIELDALIGNMENT_R(Self: TFIELD; var T: TALIGNMENT); begin T := Self.ALIGNMENT; end;
procedure TFIELDVALUE_W(Self: TFIELD; const T: VARIANT); begin Self.VALUE := T; end;
procedure TFIELDVALUE_R(Self: TFIELD; var T: VARIANT); begin T := Self.VALUE; end;
//procedure TFIELDTEXT_W(Self: TFIELD; const T: String); begin Self.TEXT := T; end;
//procedure TFIELDTEXT_R(Self: TFIELD; var T: String); begin T := Self.TEXT; end;
procedure TFIELDOLDVALUE_R(Self: TFIELD; var T: VARIANT); begin T := Self.OLDVALUE; end;
procedure TFIELDISNULL_R(Self: TFIELD; var T: BOOLEAN); begin T := Self.ISNULL; end;
procedure TFIELDDATATYPE_R(Self: TFIELD; var T: TFIELDTYPE); begin T := Self.DATATYPE; end;
procedure TFIELDCANMODIFY_R(Self: TFIELD; var T: BOOLEAN); begin T := Self.CANMODIFY; end;
procedure TFIELDASVARIANT_W(Self: TFIELD; const T: VARIANT); begin Self.ASVARIANT := T; end;
procedure TFIELDASVARIANT_R(Self: TFIELD; var T: VARIANT); begin T := Self.ASVARIANT; end;
procedure TFIELDASSTRING_W(Self: TFIELD; const T: String); begin Self.ASSTRING := T; end;
procedure TFIELDASSTRING_R(Self: TFIELD; var T: String); begin T := Self.ASSTRING; end;
procedure TFIELDASINTEGER_W(Self: TFIELD; const T: LONGINT); begin Self.ASINTEGER := T; end;
procedure TFIELDASINTEGER_R(Self: TFIELD; var T: LONGINT); begin T := Self.ASINTEGER; end;
procedure TFIELDASFLOAT_W(Self: TFIELD; const T: DOUBLE); begin Self.ASFLOAT := T; end;
procedure TFIELDASFLOAT_R(Self: TFIELD; var T: DOUBLE); begin T := Self.ASFLOAT; end;
procedure TFIELDASDATETIME_W(Self: TFIELD; const T: TDATETIME); begin Self.ASDATETIME := T; end;
procedure TFIELDASDATETIME_R(Self: TFIELD; var T: TDATETIME); begin T := Self.ASDATETIME; end;
procedure TFIELDASCURRENCY_W(Self: TFIELD; const T: CURRENCY); begin Self.ASCURRENCY := T; end;
procedure TFIELDASCURRENCY_R(Self: TFIELD; var T: CURRENCY); begin T := Self.ASCURRENCY; end;
procedure TFIELDASBOOLEAN_W(Self: TFIELD; const T: BOOLEAN); begin Self.ASBOOLEAN := T; end;
procedure TFIELDASBOOLEAN_R(Self: TFIELD; var T: BOOLEAN); begin T := Self.ASBOOLEAN; end;
procedure TFIELDSTATE_R(Self: TFIELD; var T: TDataSetState); begin T := Self.DataSet.State; end;
procedure TFIELDFIELDNAME_R(Self: TFIELD; var T: String); begin T := Self.FieldName; end;

procedure RIRegister_TFIELD(Cl: TPSRuntimeClassImporter);
Begin
  with Cl.Add(TFIELD) do
  begin
    RegisterVirtualMethod(@TFIELD.CLEAR, 'Clear');
    RegisterMethod(@TFIELD.FOCUSCONTROL, 'FocusControl');
    //RegisterVirtualMethod(@TFIELD.ISVALIDCHAR, 'IsValidChar');
    RegisterPropertyHelper(@TFIELDASBOOLEAN_R,@TFIELDASBOOLEAN_W,'AsBoolean');
    RegisterPropertyHelper(@TFIELDASCURRENCY_R,@TFIELDASCURRENCY_W,'AsCurrency');
    RegisterPropertyHelper(@TFIELDASDATETIME_R,@TFIELDASDATETIME_W,'AsDateTime');
    RegisterPropertyHelper(@TFIELDASFLOAT_R,@TFIELDASFLOAT_W,'AsFloat');
    RegisterPropertyHelper(@TFIELDASINTEGER_R,@TFIELDASINTEGER_W,'AsInteger');
    RegisterPropertyHelper(@TFIELDASSTRING_R,@TFIELDASSTRING_W,'AsString');
    RegisterPropertyHelper(@TFIELDASVARIANT_R,@TFIELDASVARIANT_W,'AsVariant');
    RegisterPropertyHelper(@TFIELDCANMODIFY_R,nil,'CanModify');
    RegisterPropertyHelper(@TFIELDDATATYPE_R,nil,'DataType');
    RegisterPropertyHelper(@TFIELDISNULL_R,nil,'IsNull');
    RegisterPropertyHelper(@TFIELDOLDVALUE_R,nil,'OldValue');
    //RegisterPropertyHelper(@TFIELDTEXT_R,@TFIELDTEXT_W,'Text');
    RegisterPropertyHelper(@TFIELDVALUE_R,@TFIELDVALUE_W,'Value');
    RegisterPropertyHelper(@TFIELDALIGNMENT_R,@TFIELDALIGNMENT_W,'Alignment');
    RegisterPropertyHelper(@TFIELDREADONLY_R,@TFIELDREADONLY_W,'ReadOnly');
    //RegisterPropertyHelper(@TFIELDDISPLAYFORMAT_R, @TFIELDDISPLAYFORMAT_W, 'DisplayFormat');
    //RegisterPropertyHelper(@TFIELDEDITFORMAT_R, @TFIELDEDITFORMAT_W, 'EditFormat');
    RegisterPropertyHelper(@TFIELDSTATE_R, nil, 'State');
    RegisterPropertyHelper(@TFIELDFIELDNAME_R, nil, 'FieldName');
    {RegisterPropertyHelper(@TFIELDINSERTSTATE_R, nil, 'InsertState');
    RegisterPropertyHelper(@TFIELDEDITSTATE_R, nil, 'EditState'); }
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TParamList
////////////////////////////////////////////////////////////////////////////////

procedure TParamListValues_R(Self: TParamList; var T: Variant; I: String); begin T := Self.Values[I]; end;
procedure TParamListValues_W(Self: TParamList; T: Variant; I: String); begin Self.Values[I] := T; end;
procedure TParamListObjects_R(Self: TParamList; var T: TObject; I: String); begin T := Self.Objects[I]; end;
procedure TParamListObjects_W(Self: TParamList; T: TObject; I: String); begin Self.Objects[I] := T; end;
procedure TParamListNames_R(Self: TParamList; var T: String; I: Integer); begin T := Self.Names[I]; end;
procedure TParamListValueFromIndex_R(Self: TParamList; var T: Variant; I: Integer); begin T := Self.ValueFromIndex[I]; end;
procedure TParamListValueFromIndex_W(Self: TParamList; T: Variant; I: Integer); begin Self.ValueFromIndex[I] := T; end;
procedure TParamListObjectFromIndex_R(Self: TParamList; var T: TObject; I: Integer); begin T := Self.ObjectFromIndex[I]; end;
procedure TParamListObjectFromIndex_W(Self: TParamList; T: TObject; I: Integer); begin Self.ObjectFromIndex[I] := T; end;
procedure TParamListCount_R(Self: TParamList; var T: Integer); begin T := Self.Count; end;
procedure TParamListOnGetParam_R(Self: TParamList; var T: TParamNotifyEvent); begin T := Self.OnGetParam; end;
procedure TParamListOnGetParam_W(Self: TParamList; T: TParamNotifyEvent); begin Self.OnGetParam := T; end;
procedure TParamListOnSetParam_R(Self: TParamList; var T: TParamNotifyEvent); begin T := Self.OnSetParam; end;
procedure TParamListOnSetParam_W(Self: TParamList; T: TParamNotifyEvent); begin Self.OnSetParam := T; end;

procedure RIRegister_dxTypes(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TParamList) do
  begin
    RegisterConstructor(@TParamList.Create, 'Create');
    RegisterMethod(@TParamList.Clear, 'Clear');
    RegisterMethod(@TParamList.ParamExists, 'ParamExists');
    RegisterPropertyHelper(@TParamListValues_R, @TParamListValues_W, 'Values');
    RegisterPropertyHelper(@TParamListObjects_R, @TParamListObjects_W, 'Objects');
    RegisterPropertyHelper(@TParamListNames_R, nil, 'Names');
    RegisterPropertyHelper(@TParamListValueFromIndex_R, @TParamListValueFromIndex_W, 'ValueFromIndex');
    RegisterPropertyHelper(@TParamListObjectFromIndex_R, @TParamListObjectFromIndex_W, 'ObjectFromIndex');
    RegisterPropertyHelper(@TParamListCount_R, nil, 'Count');
    RegisterEventPropertyHelper(@TParamListOnGetParam_R, @TParamListOnGetParam_W, 'OnGetParam');
    RegisterEventPropertyHelper(@TParamListOnSetParam_R, @TParamListOnSetParam_W, 'OnSetParam');
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// CSV
////////////////////////////////////////////////////////////////////////////////

procedure TCsvFileRowCount_R(Self: TCsvData; var T: Integer); begin T := Self.RowCount; end;
procedure TCsvFileRowCount_W(Self: TCsvData; T: Integer); begin Self.RowCount := T; end;
procedure TCsvFileColCount_R(Self: TCsvData; var T: Integer); begin T := Self.ColCount; end;
procedure TCsvFileColCount_W(Self: TCsvData; T: Integer); begin Self.ColCount := T; end;
procedure TCsvFileCells_R(Self: TCsvData; var T: String; C, R: Integer); begin T := Self.Cells[C, R]; end;
procedure TCsvFileCells_W(Self: TCsvData; const T: String; C, R: Integer); begin Self.Cells[C, R] := T; end;
procedure TCsvFileDelimiter_R(Self: TCsvData; var T: Char); begin T := Self.Delimiter; end;
procedure TCsvFileDelimiter_W(Self: TCsvData; T: Char); begin Self.Delimiter := T; end;

procedure RIRegister_CsvData(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TCsvData) do
  begin
    RegisterConstructor(@TCsvData.Create, 'Create');
    RegisterMethod(@TCsvData.LoadFromFile, 'LoadFromFile');
    RegisterMethod(@TCsvData.LoadFromStream, 'LoadFromStream');
    RegisterMethod(@TCsvData.SaveToFile, 'SaveToFile');
    RegisterMethod(@TCsvData.SaveToStream, 'SaveToStream');
    RegisterPropertyHelper(@TCsvFileRowCount_R, @TCsvFileRowCount_W, 'RowCount');
    RegisterPropertyHelper(@TCsvFileColCount_R, @TCsvFileColCount_W, 'ColCount');
    RegisterPropertyHelper(@TCsvFileCells_R, @TCsvFileCells_W, 'Cells');
    RegisterPropertyHelper(@TCsvFileDelimiter_R, @TCsvFileDelimiter_W, 'Delimiter');
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// dxControls
////////////////////////////////////////////////////////////////////////////////

procedure TdxLabelFieldName_R(Self: TdxLabel; var T: String); begin T := Self.FieldName; end;
procedure TdxLabelExpression_R(Self: TdxLabel; var T: String); begin T := Self.Expression; end;

procedure RIRegister_dxLabel(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxLabel) do
  begin
    RegisterPropertyHelper(@TdxLabelExpression_R, nil, 'Expression');
    RegisterPropertyHelper(@TdxLabelFieldName_R, nil, 'FieldName');
  end;
end;

procedure TdxEditCheckExpression_R(Self: TdxEdit; var T: String); begin T := Self.CheckExpression; end;
procedure TdxEditDefaultValue_R(Self: TdxEdit; var T: String); begin T := Self.DefaultValue; end;
procedure TdxEditEditable_R(Self: TdxEdit; var T: Boolean); begin T := Self.Editable; end;
procedure TdxEditExpression_R(Self: TdxEdit; var T: String); begin T := Self.Expression; end;
procedure TdxEditFieldName_R(Self: TdxEdit; var T: String); begin T := Self.FieldName; end;
procedure TdxEditFieldSize_R(Self: TdxEdit; var T: Integer); begin T := Self.FieldSize; end;
procedure TdxEditId_R(Self: TdxEdit; var T: Integer); begin T := Self.Id; end;
procedure TdxEditRequired_R(Self: TdxEdit; var T: Boolean); begin T := Self.Required; end;

procedure RIRegister_dxEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxEdit) do
  begin
    RegisterPropertyHelper(@TdxEditCheckExpression_R, nil, 'CheckExpression');
    RegisterPropertyHelper(@TdxEditDefaultValue_R, nil, 'DefaultValue');
    RegisterPropertyHelper(@TdxEditEditable_R, nil, 'Editable');
    RegisterPropertyHelper(@TdxEditExpression_R, nil, 'Expression');
    RegisterPropertyHelper(@TdxEditFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TdxEditFieldSize_R, nil, 'FieldSize');
    RegisterPropertyHelper(@TdxEditId_R, nil, 'Id');
    RegisterPropertyHelper(@TdxEditRequired_R, nil, 'Required');
  end;
end;

procedure TdxCalcEditCheckExpression_R(Self: TdxCalcEdit; var T: String); begin T := Self.CheckExpression; end;
procedure TdxCalcEditDefaultValue_R(Self: TdxCalcEdit; var T: String); begin T := Self.DefaultValue; end;
procedure TdxCalcEditEditable_R(Self: TdxCalcEdit; var T: Boolean); begin T := Self.Editable; end;
procedure TdxCalcEditExpression_R(Self: TdxCalcEdit; var T: String); begin T := Self.Expression; end;
procedure TdxCalcEditFieldName_R(Self: TdxCalcEdit; var T: String); begin T := Self.FieldName; end;
procedure TdxCalcEditId_R(Self: TdxCalcEdit; var T: Integer); begin T := Self.Id; end;
procedure TdxCalcEditMaxValue_R(Self: TdxCalcEdit; var T: Double); begin T := Self.MaxValue; end;
procedure TdxCalcEditMinValue_R(Self: TdxCalcEdit; var T: Double); begin T := Self.MinValue; end;
procedure TdxCalcEditPrecision_R(Self: TdxCalcEdit; var T: Integer); begin T := Self.Precission; end;
procedure TdxCalcEditRequired_R(Self: TdxCalcEdit; var T: Boolean); begin T := Self.Required; end;

procedure RIRegister_dxCalcEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxCalcEdit) do
  begin
    RegisterPropertyHelper(@TdxCalcEditCheckExpression_R, nil, 'CheckExpression');
    RegisterPropertyHelper(@TdxCalcEditDefaultValue_R, nil, 'DefaultValue');
    RegisterPropertyHelper(@TdxCalcEditEditable_R, nil, 'Editable');
    RegisterPropertyHelper(@TdxCalcEditExpression_R, nil, 'Expression');
    RegisterPropertyHelper(@TdxCalcEditFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TdxCalcEditId_R, nil, 'Id');
    RegisterPropertyHelper(@TdxCalcEditMaxValue_R, nil, 'MaxValue');
    RegisterPropertyHelper(@TdxCalcEditMinValue_R, nil, 'MinValue');
    RegisterPropertyHelper(@TdxCalcEditPrecision_R, nil, 'Precision');
    RegisterPropertyHelper(@TdxCalcEditRequired_R, nil, 'Required');
  end;
end;

procedure TdxDateEditCheckExpression_R(Self: TdxDateEdit; var T: String); begin T := Self.CheckExpression; end;
procedure TdxDateEditDateNow_R(Self: TdxDateEdit; var T: Boolean); begin T := Self.DateNow; end;
procedure TdxDateEditDefaultValue_R(Self: TdxDateEdit; var T: String); begin T := Self.DefaultValue; end;
procedure TdxDateEditEditable_R(Self: TdxDateEdit; var T: Boolean); begin T := Self.Editable; end;
procedure TdxDateEditExpression_R(Self: TdxDateEdit; var T: String); begin T := Self.Expression; end;
procedure TdxDateEditFieldName_R(Self: TdxDateEdit; var T: String); begin T := Self.FieldName; end;
procedure TdxDateEditId_R(Self: TdxDateEdit; var T: Integer); begin T := Self.Id; end;
procedure TdxDateEditRequired_R(Self: TdxDateEdit; var T: Boolean); begin T := Self.Required; end;

procedure RIRegister_dxDateEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxDateEdit) do
  begin
    RegisterPropertyHelper(@TdxDateEditCheckExpression_R, nil, 'CheckExpression');
    RegisterPropertyHelper(@TdxDateEditDateNow_R, nil, 'DateNow');
    RegisterPropertyHelper(@TdxDateEditDefaultValue_R, nil, 'DefaultValue');
    RegisterPropertyHelper(@TdxDateEditEditable_R, nil, 'Editable');
    RegisterPropertyHelper(@TdxDateEditExpression_R, nil, 'Expression');
    RegisterPropertyHelper(@TdxDateEditFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TdxDateEditId_R, nil, 'Id');
    RegisterPropertyHelper(@TdxDateEditRequired_R, nil, 'Required');
  end;
end;

procedure TdxTimeEditCheckExpression_R(Self: TdxTimeEdit; var T: String); begin T := Self.CheckExpression; end;
procedure TdxTimeEditCurTime_R(Self: TdxTimeEdit; var T: Boolean); begin T := Self.CurTime; end;
procedure TdxTimeEditDefaultValue_R(Self: TdxTimeEdit; var T: String); begin T := Self.DefaultValue; end;
procedure TdxTimeEditEditable_R(Self: TdxTimeEdit; var T: Boolean); begin T := Self.Editable; end;
procedure TdxTimeEditExpression_R(Self: TdxTimeEdit; var T: String); begin T := Self.Expression; end;
procedure TdxTimeEditFieldName_R(Self: TdxTimeEdit; var T: String); begin T := Self.FieldName; end;
procedure TdxTimeEditId_R(Self: TdxTimeEdit; var T: Integer); begin T := Self.Id; end;
procedure TdxTimeEditRequired_R(Self: TdxTimeEdit; var T: Boolean); begin T := Self.Required; end;

procedure RIRegister_dxTimeEdit(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxTimeEdit) do
  begin
    RegisterPropertyHelper(@TdxTimeEditCheckExpression_R, nil, 'CheckExpression');
    RegisterPropertyHelper(@TdxTimeEditCurTime_R, nil, 'CurTime');
    RegisterPropertyHelper(@TdxTimeEditDefaultValue_R, nil, 'DefaultValue');
    RegisterPropertyHelper(@TdxTimeEditEditable_R, nil, 'Editable');
    RegisterPropertyHelper(@TdxTimeEditExpression_R, nil, 'Expression');
    RegisterPropertyHelper(@TdxTimeEditFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TdxTimeEditId_R, nil, 'Id');
    RegisterPropertyHelper(@TdxTimeEditRequired_R, nil, 'Required');
  end;
end;

procedure TdxCounterCheckExpression_R(Self: TdxCounter; var T: String); begin T := Self.CheckExpression; end;
procedure TdxCounterFieldName_R(Self: TdxCounter; var T: String); begin T := Self.FieldName; end;
procedure TdxCounterId_R(Self: TdxCounter; var T: Integer); begin T := Self.Id; end;
procedure TdxCounterRequired_R(Self: TdxCounter; var T: Boolean); begin T := Self.Required; end;

procedure RIRegister_dxCounter(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxCounter) do
  begin
    RegisterPropertyHelper(@TdxCounterCheckExpression_R, nil, 'CheckExpression');
    RegisterPropertyHelper(@TdxCounterFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TdxCounterId_R, nil, 'Id');
    RegisterPropertyHelper(@TdxCounterRequired_R, nil, 'Required');
  end;
end;

procedure TdxMemoCheckExpression_R(Self: TdxMemo; var T: String); begin T := Self.CheckExpression; end;
procedure TdxMemoDefaultValue_R(Self: TdxMemo; var T: String); begin T := Self.DefaultValue; end;
procedure TdxMemoEditable_R(Self: TdxMemo; var T: Boolean); begin T := Self.Editable; end;
procedure TdxMemoExpression_R(Self: TdxMemo; var T: String); begin T := Self.Expression; end;
procedure TdxMemoFieldName_R(Self: TdxMemo; var T: String); begin T := Self.FieldName; end;
procedure TdxMemoFieldSize_R(Self: TdxMemo; var T: Integer); begin T := Self.FieldSize; end;
procedure TdxMemoId_R(Self: TdxMemo; var T: Integer); begin T := Self.Id; end;
procedure TdxMemoRequired_R(Self: TdxMemo; var T: Boolean); begin T := Self.Required; end;

procedure RIRegister_dxMemo(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxMemo) do
  begin
    RegisterPropertyHelper(@TdxMemoCheckExpression_R, nil, 'CheckExpression');
    RegisterPropertyHelper(@TdxMemoDefaultValue_R, nil, 'DefaultValue');
    RegisterPropertyHelper(@TdxMemoEditable_R, nil, 'Editable');
    RegisterPropertyHelper(@TdxMemoExpression_R, nil, 'Expression');
    RegisterPropertyHelper(@TdxMemoFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TdxMemoFieldSize_R, nil, 'FieldSize');
    RegisterPropertyHelper(@TdxMemoId_R, nil, 'Id');
    RegisterPropertyHelper(@TdxMemoRequired_R, nil, 'Required');
  end;
end;

procedure TdxCheckBoxCheckExpression_R(Self: TdxCheckBox; var T: String); begin T := Self.CheckExpression; end;
procedure TdxCheckBoxCheckedText_R(Self: TdxCheckBox; var T: String); begin T := Self.CheckedText; end;
procedure TdxCheckBoxCheckedText_W(Self: TdxCheckBox; T: String); begin Self.CheckedText := T; end;
procedure TdxCheckBoxDefaultValue_R(Self: TdxCheckBox; var T: String); begin T := Self.DefaultValue; end;
procedure TdxCheckBoxEditable_R(Self: TdxCheckBox; var T: Boolean); begin T := Self.Editable; end;
procedure TdxCheckBoxExpression_R(Self: TdxCheckBox; var T: String); begin T := Self.Expression; end;
procedure TdxCheckBoxFieldName_R(Self: TdxCheckBox; var T: String); begin T := Self.FieldName; end;
procedure TdxCheckBoxId_R(Self: TdxCheckBox; var T: Integer); begin T := Self.Id; end;
procedure TdxCheckBoxUnCheckedText_R(Self: TdxCheckBox; var T: String); begin T := Self.UnCheckedText; end;
procedure TdxCheckBoxUnCheckedText_W(Self: TdxCheckBox; T: String); begin Self.UnCheckedText := T; end;

procedure RIRegister_dxCheckBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxCheckBox) do
  begin
    RegisterPropertyHelper(@TdxCheckBoxCheckExpression_R, nil, 'CheckExpression');
    RegisterPropertyHelper(@TdxCheckBoxCheckedText_R, @TdxCheckBoxCheckedText_W, 'CheckedText');
    RegisterPropertyHelper(@TdxCheckBoxDefaultValue_R, nil, 'DefaultValue');
    RegisterPropertyHelper(@TdxCheckBoxEditable_R, nil, 'Editable');
    RegisterPropertyHelper(@TdxCheckBoxExpression_R, nil, 'Expression');
    RegisterPropertyHelper(@TdxCheckBoxFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TdxCheckBoxId_R, nil, 'Id');
    RegisterPropertyHelper(@TdxCheckBoxUnCheckedText_R, @TdxCheckBoxUnCheckedText_W, 'UnCheckedText');
  end;
end;

procedure TdxComboBoxCheckExpression_R(Self: TdxComboBox; var T: String); begin T := Self.CheckExpression; end;
procedure TdxComboBoxDefaultValue_R(Self: TdxComboBox; var T: String); begin T := Self.DefaultValue; end;
procedure TdxComboBoxEditable_R(Self: TdxComboBox; var T: Boolean); begin T := Self.Editable; end;
procedure TdxComboBoxExpression_R(Self: TdxComboBox; var T: String); begin T := Self.Expression; end;
procedure TdxComboBoxFieldName_R(Self: TdxComboBox; var T: String); begin T := Self.FieldName; end;
procedure TdxComboBoxFieldSize_R(Self: TdxComboBox; var T: Integer); begin T := Self.FieldSize; end;
procedure TdxComboBoxFilter_R(Self: TdxComboBox; var T: String); begin T := Self.Filter; end;
procedure TdxComboBoxFilter_W(Self: TdxComboBox; var T: String); begin Self.Filter := T; end;
procedure TdxComboBoxId_R(Self: TdxComboBox; var T: Integer); begin T := Self.Id; end;
procedure TdxComboBoxRequired_R(Self: TdxComboBox; var T: Boolean); begin T := Self.Required; end;
procedure TdxComboBoxSourceFormName_R(Self: TdxComboBox; var T: String); begin T := Self.SourceFormName; end;
procedure TdxComboBoxSourceFieldName_R(Self: TdxComboBox; var T: String); begin T := Self.SourceFieldName; end;
procedure TdxComboBoxItems_R(Self: TdxComboBox; var T: TStrings); begin T := Self.Items; end;
procedure TdxComboBoxItems_W(Self: TdxComboBox; T: TStrings); begin Self.Items := T; end;

procedure RIRegister_dxComboBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxComboBox) do
  begin
    RegisterPropertyHelper(@TdxComboBoxCheckExpression_R, nil, 'CheckExpression');
    RegisterPropertyHelper(@TdxComboBoxDefaultValue_R, nil, 'DefaultValue');
    RegisterPropertyHelper(@TdxComboBoxEditable_R, nil, 'Editable');
    RegisterPropertyHelper(@TdxComboBoxExpression_R, nil, 'Expression');
    RegisterPropertyHelper(@TdxComboBoxFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TdxComboBoxFieldSize_R, nil, 'FieldSize');
    RegisterPropertyHelper(@TdxComboBoxFilter_R, @TdxComboBoxFilter_W, 'Filter');
    RegisterPropertyHelper(@TdxComboBoxId_R, nil, 'Id');
    RegisterPropertyHelper(@TdxComboBoxRequired_R, nil, 'Required');
    RegisterPropertyHelper(@TdxComboBoxSourceFormName_R, nil, 'SourceFormName');
    RegisterPropertyHelper(@TdxComboBoxSourceFieldName_R, nil, 'SourceFieldName');
    RegisterPropertyHelper(@TdxComboBoxItems_R, @TdxComboBoxItems_W, 'Items');
  end;
end;

procedure TdxLookupComboBoxCheckExpression_R(Self: TdxLookupComboBox; var T: String); begin T := Self.CheckExpression; end;
procedure TdxLookupComboBoxDefaultValue_R(Self: TdxLookupComboBox; var T: String); begin T := Self.DefaultValue; end;
procedure TdxLookupComboBoxEditable_R(Self: TdxLookupComboBox; var T: Boolean); begin T := Self.Editable; end;
procedure TdxLookupComboBoxExpression_R(Self: TdxLookupComboBox; var T: String); begin T := Self.Expression; end;
procedure TdxLookupComboBoxFieldName_R(Self: TdxLookupComboBox; var T: String); begin T := Self.FieldName; end;
procedure TdxLookupComboBoxFilter_R(Self: TdxLookupComboBox; var T: String); begin T := Self.Filter; end;
procedure TdxLookupComboBoxFilter_W(Self: TdxLookupComboBox; T: String); begin Self.Filter := T; end;
procedure TdxLookupComboBoxHideList_R(Self: TdxLookupComboBox; var T: Boolean); begin T := Self.HideList; end;
procedure TdxLookupComboBoxHideList_W(Self: TdxLookupComboBox; T: Boolean); begin Self.HideList := T; end;
procedure TdxLookupComboBoxId_R(Self: TdxLookupComboBox; var T: Integer); begin T := Self.Id; end;
procedure TdxLookupComboBoxRequired_R(Self: TdxLookupComboBox; var T: Boolean); begin T := Self.Required; end;
procedure TdxLookupComboBoxSourceFormName_R(Self: TdxLookupComboBox; var T: String); begin T := Self.SourceFormName; end;
procedure TdxLookupComboBoxSourceFieldName_R(Self: TdxLookupComboBox; var T: String); begin T := Self.SourceFieldName; end;
procedure TdxLookupComboBoxOnCreateForm_R(Self: TdxLookupComboBox; var T: TCreateFormEvent); begin T := Self.OnCreateForm; end;
procedure TdxLookupComboBoxOnCreateForm_W(Self: TdxLookupComboBox; var T: TCreateFormEvent); begin Self.OnCreateForm := T; end;

procedure RIRegister_dxLookupComboBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxLookupComboBox) do
  begin
    RegisterPropertyHelper(@TdxLookupComboBoxCheckExpression_R, nil, 'CheckExpression');
    RegisterPropertyHelper(@TdxLookupComboBoxDefaultValue_R, nil, 'DefaultValue');
    RegisterPropertyHelper(@TdxLookupComboBoxEditable_R, nil, 'Editable');
    RegisterPropertyHelper(@TdxLookupComboBoxExpression_R, nil, 'Expression');
    RegisterPropertyHelper(@TdxLookupComboBoxFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TdxLookupComboBoxFilter_R, @TdxLookupComboBoxFilter_W, 'Filter');
    RegisterPropertyHelper(@TdxLookupComboBoxHideList_R, @TdxLookupComboBoxHideList_W, 'HideList');
    RegisterPropertyHelper(@TdxLookupComboBoxId_R, nil, 'Id');
    RegisterPropertyHelper(@TdxLookupComboBoxRequired_R, nil, 'Required');
    RegisterPropertyHelper(@TdxLookupComboBoxSourceFormName_R, nil, 'SourceFormName');
    RegisterPropertyHelper(@TdxLookupComboBoxSourceFieldName_R, nil, 'SourceFieldName');
    RegisterEventPropertyHelper(@TdxLookupComboBoxOnCreateForm_R, @TdxLookupComboBoxOnCreateForm_W, 'OnCreateForm');
  end;
end;

procedure TdxObjectFieldFieldName_R(Self: TdxObjectField; var T: String); begin T := Self.FieldName; end;
procedure TdxObjectFieldId_R(Self: TdxObjectField; var T: Integer); begin T := Self.Id; end;
procedure TdxObjectFieldObjId_R(Self: TdxObjectField; var T: Integer); begin T := Self.ObjId; end;
procedure TdxObjectFieldFieldId_R(Self: TdxObjectField; var T: Integer); begin T := Self.FieldId; end;

procedure RIRegister_dxObjectField(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxObjectField) do
  begin
    RegisterPropertyHelper(@TdxObjectFieldFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TdxObjectFieldId_R, nil, 'Id');
    RegisterPropertyHelper(@TdxObjectFieldObjId_R, nil, 'ObjId');
    RegisterPropertyHelper(@TdxObjectFieldFieldId_R, nil, 'FieldId');
  end;
end;

procedure TdxImageCenter_R(Self: TdxImage; var T: Boolean); begin T := Self.Center; end;
procedure TdxImageCenter_W(Self: TdxImage; T: Boolean); begin Self.Center := T; end;
procedure TdxImageImageName_R(Self: TdxImage; var T: String); begin T := Self.ImageName; end;
procedure TdxImageImageName_W(Self: TdxImage; T: String); begin Self.ImageName := T; end;
procedure TdxImageKeepSize_R(Self: TdxImage; var T: Boolean); begin T := Self.KeepSize; end;
procedure TdxImageKeepSize_W(Self: TdxImage; T: Boolean); begin Self.KeepSize := T; end;
procedure TdxImageProportional_R(Self: TdxImage; var T: Boolean); begin T := Self.Proportional; end;
procedure TdxImageProportional_W(Self: TdxImage; T: Boolean); begin Self.Proportional := T; end;
procedure TdxImageStretch_R(Self: TdxImage; var T: Boolean); begin T := Self.Stretch; end;
procedure TdxImageStretch_W(Self: TdxImage; T: Boolean); begin Self.Stretch := T; end;

procedure RIRegister_dxImage(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxImage) do
  begin
    RegisterMethod(@TdxImage.Clear, 'Clear');
    RegisterMethod(@TdxImage.LoadFromFile, 'LoadFromFile');
    RegisterMethod(@TdxImage.SaveToFile, 'SaveToFile');
    RegisterMethod(@TdxImage.LoadFromStream, 'LoadFromStream');
    RegisterMethod(@TdxImage.SaveToStream, 'SaveToStream');

    RegisterPropertyHelper(@TdxImageCenter_R, @TdxImageCenter_W, 'Center');
    RegisterPropertyHelper(@TdxImageImageName_R, @TdxImageImageName_W, 'ImageName');
    RegisterPropertyHelper(@TdxImageKeepSize_R, @TdxImageKeepSize_W, 'KeepSize');
    RegisterPropertyHelper(@TdxImageProportional_R, @TdxImageProportional_W, 'Proportional');
    RegisterPropertyHelper(@TdxImageStretch_R, @TdxImageStretch_W, 'Stretch');
  end;
end;

procedure TdxDBImageCheckExpression_R(Self: TdxDBImage; var T: String); begin T := Self.CheckExpression; end;
procedure TdxDBImageFieldName_R(Self: TdxDBImage; var T: String); begin T := Self.FieldName; end;
procedure TdxDBImageId_R(Self: TdxDBImage; var T: Integer); begin T := Self.Id; end;
procedure TdxDBImagePrintSize_R(Self: TdxDBImage; var T: Integer); begin T := Self.PrintSize; end;
procedure TdxDBImagePrintSize_W(Self: TdxDBImage; var T: Integer); begin Self.PrintSize := T; end;
procedure TdxDBImageRequired_R(Self: TdxDBImage; var T: Boolean); begin T := Self.Required; end;
procedure TdxDBImageShowThumbnail_R(Self: TdxDBImage; var T: Boolean); begin T := Self.ShowThumbnail; end;
procedure TdxDBImageSourceFileName_R(Self: TdxDBImage; var T: String); begin T := Self.SourceFileName; end;
procedure TdxDBImageStorageFolder_R(Self: TdxDBImage; var T: String); begin T := Self.StorageFolder; end;
procedure TdxDBImageStorageType_R(Self: TdxDBImage; var T: Integer); begin T := Self.StorageType; end;
procedure TdxDBImageStoredFileName_R(Self: TdxDBImage; var T: String); begin T := Self.StoredFileName; end;
procedure TdxDBImageThumbSize_R(Self: TdxDBImage; var T: Integer); begin T := Self.ThumbSize; end;

procedure RIRegister_dxDBImage(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxDBImage) do
  begin
    RegisterMethod(@TdxDBImage.Clear, 'Clear');
    RegisterMethod(@TdxDBImage.LoadFromFile, 'LoadFromFile');
    RegisterMethod(@TdxDBImage.SaveToFile, 'SaveToFile');
    RegisterMethod(@TdxDBImage.SaveToStream, 'SaveToStream');
    RegisterMethod(@TdxDBImage.WasChanged, 'WasChanged');

    RegisterPropertyHelper(@TdxDBImageCheckExpression_R, nil, 'CheckExpression');
    RegisterPropertyHelper(@TdxDBImageFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TdxDBImageId_R, nil, 'Id');
    RegisterPropertyHelper(@TdxDBImagePrintSize_R, @TdxDBImagePrintSize_W, 'PrintSize');
    RegisterPropertyHelper(@TdxDBImageRequired_R, nil, 'Required');
    RegisterPropertyHelper(@TdxDBImageShowThumbnail_R, nil, 'ShowThumbnail');
    RegisterPropertyHelper(@TdxDBImageSourceFileName_R, nil, 'SourceFileName');
    RegisterPropertyHelper(@TdxDBImageStorageFolder_R, nil, 'StorageFolder');
    RegisterPropertyHelper(@TdxDBImageStorageType_R, nil, 'StorageType');
    RegisterPropertyHelper(@TdxDBImageStoredFileName_R, nil, 'StoredFileName');
    RegisterPropertyHelper(@TdxDBImageThumbSize_R, nil, 'ThumbSize');
  end;
end;

procedure TdxFileCheckExpression_R(Self: TdxFile; var T: String); begin T := Self.CheckExpression; end;
procedure TdxFileDescription_R(Self: TdxFile; var T: String); begin T := Self.Description; end;
procedure TdxFileFieldName_R(Self: TdxFile; var T: String); begin T := Self.FieldName; end;
procedure TdxFileFieldSize_R(Self: TdxFile; var T: Integer); begin T := Self.FieldSize; end;
procedure TdxFileId_R(Self: TdxFile; var T: Integer); begin T := Self.Id; end;
procedure TdxFileRequired_R(Self: TdxFile; var T: Boolean); begin T := Self.Required; end;
procedure TdxFileSourceFileName_R(Self: TdxFile; var T: String); begin T := Self.SourceFileName; end;
procedure TdxFileStorageFolder_R(Self: TdxFile; var T: String); begin T := Self.StorageFolder; end;
procedure TdxFileStorageType_R(Self: TdxFile; var T: Integer); begin T := Self.StorageType; end;
procedure TdxFileStoredFileName_R(Self: TdxFile; var T: String); begin T := Self.StoredFileName; end;

procedure RIRegister_dxFile(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxFile) do
  begin
    RegisterMethod(@TdxFile.Clear, 'Clear');
    RegisterMethod(@TdxFile.LoadFromFile, 'LoadFromFile');
    RegisterMethod(@TdxFile.SaveToFile, 'SaveToFile');
    RegisterMethod(@TdxFile.SaveToStream, 'SaveToStream');
    RegisterMethod(@TdxFile.WasChanged, 'WasChanged');

    RegisterPropertyHelper(@TdxFileCheckExpression_R, nil, 'CheckExpression');
    RegisterPropertyHelper(@TdxFileDescription_R, nil, 'Description');
    RegisterPropertyHelper(@TdxFileFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TdxFileFieldSize_R, nil, 'FieldSize');
    RegisterPropertyHelper(@TdxFileId_R, nil, 'Id');
    RegisterPropertyHelper(@TdxFileRequired_R, nil, 'Required');
    RegisterPropertyHelper(@TdxFileSourceFileName_R, nil, 'SourceFileName');
    RegisterPropertyHelper(@TdxFileStorageFolder_R, nil, 'StorageFolder');
    RegisterPropertyHelper(@TdxFileStorageType_R, nil, 'StorageType');
    RegisterPropertyHelper(@TdxFileStoredFileName_R, nil, 'StoredFileName');
  end;
end;

procedure TdxShapeBrush_R(Self: TdxShape; var T: TdxBrush); begin T := Self.Brush; end;
procedure TdxShapeBrush_W(Self: TdxShape; T: TdxBrush); begin Self.Brush := T; end;
procedure TdxShapePen_R(Self: TdxShape; var T: TdxPen); begin T := Self.Pen; end;
procedure TdxShapePen_W(Self: TdxShape; T: TdxPen); begin Self.Pen := T; end;
procedure TdxShapeShape_R(Self: TdxShape; var T: TShapeType); begin T := Self.Shape; end;
procedure TdxShapeShape_W(Self: TdxShape; T: TShapeType); begin Self.Shape := T; end;
procedure TdxShapeShapeEx_R(Self: TdxShape; var T: TShapeTypeEx); begin T := Self.ShapeEx; end;
procedure TdxShapeShapeEx_W(Self: TdxShape; T: TShapeTypeEx); begin Self.ShapeEx := T; end;

procedure RIRegister_dxShape(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxShape) do
  begin
    RegisterPropertyHelper(@TdxShapeBrush_R, @TdxShapeBrush_W, 'Brush');
    RegisterPropertyHelper(@TdxShapePen_R, @TdxShapePen_W, 'Pen');
    RegisterPropertyHelper(@TdxShapeShape_R, @TdxShapeShape_W, 'Shape');
    RegisterPropertyHelper(@TdxShapeShapeEx_R, @TdxShapeShapeEx_W, 'ShapeEx');
  end;
end;

procedure RIRegister_dxGrid(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxGrid) do
  begin

  end;
end;

procedure TdxQueryGridQueryName_R(Self: TdxQueryGrid; var T: String); begin T := Self.QueryName; end;
procedure TdxQueryGridFields_R(Self: TdxQueryGrid; var T: Variant; I: String); begin T := Self.Fields[I]; end;
procedure TdxQueryGridAsI_R(Self: TdxQueryGrid; var T: Integer; I: String); begin T := Self.AsI[I]; end;
procedure TdxQueryGridAsF_R(Self: TdxQueryGrid; var T: Double; I: String); begin T := Self.AsF[I]; end;
procedure TdxQueryGridAsDT_R(Self: TdxQueryGrid; var T: TDateTime; I: String); begin T := Self.AsDT[I]; end;
procedure TdxQueryGridAsS_R(Self: TdxQueryGrid; var T: String; I: String); begin T := Self.AsS[I]; end;
procedure TdxQueryGridManualRefresh_R(Self: TdxQueryGrid; var T: Boolean); begin T := Self.ManualRefresh; end;
procedure TdxQueryGridManualRefresh_W(Self: TdxQueryGrid; T: Boolean); begin Self.ManualRefresh := T; end;
procedure TdxQueryGridEditable_R(Self: TdxQueryGrid; var T: Boolean); begin T := Self.Editable; end;

procedure TdxQueryGridOnAfterClose_R(Self: TdxQueryGrid; var T: TNotifyEvent); begin T := Self.OnAfterClose; end;
procedure TdxQueryGridOnAfterClose_W(Self: TdxQueryGrid; T: TNotifyEvent); begin Self.OnAfterClose := T; end;
procedure TdxQueryGridOnAfterOpen_R(Self: TdxQueryGrid; var T: TNotifyEvent); begin T := Self.OnAfterOpen; end;
procedure TdxQueryGridOnAfterOpen_W(Self: TdxQueryGrid; T: TNotifyEvent); begin Self.OnAfterOpen := T; end;
procedure TdxQueryGridOnAfterScroll_R(Self: TdxQueryGrid; var T: TNotifyEvent); begin T := Self.OnAfterScroll; end;
procedure TdxQueryGridOnAfterScroll_W(Self: TdxQueryGrid; T: TNotifyEvent); begin Self.OnAfterScroll := T; end;
procedure TdxQueryGridOnBeforeClose_R(Self: TdxQueryGrid; var T: TNotifyEvent); begin T := Self.OnBeforeClose; end;
procedure TdxQueryGridOnBeforeClose_W(Self: TdxQueryGrid; T: TNotifyEvent); begin Self.OnBeforeClose := T; end;
procedure TdxQueryGridOnBeforeOpen_R(Self: TdxQueryGrid; var T: TNotifyEvent); begin T := Self.OnBeforeOpen; end;
procedure TdxQueryGridOnBeforeOpen_W(Self: TdxQueryGrid; T: TNotifyEvent); begin Self.OnBeforeOpen := T; end;
procedure TdxQueryGridOnBeforeScroll_R(Self: TdxQueryGrid; var T: TNotifyEvent); begin T := Self.OnBeforeScroll; end;
procedure TdxQueryGridOnBeforeScroll_W(Self: TdxQueryGrid; T: TNotifyEvent); begin Self.OnBeforeScroll := T; end;
procedure TdxQueryGridOnCreateForm_R(Self: TdxQueryGrid; var T: TCreateFormEvent); begin T := Self.OnCreateForm; end;
procedure TdxQueryGridOnCreateForm_W(Self: TdxQueryGrid; T: TCreateFormEvent); begin Self.OnCreateForm := T; end;

procedure RIRegister_dxQueryGrid(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxQueryGrid) do
  begin
    RegisterMethod(@TdxQueryGrid.MoveFirst, 'MoveFirst');
    RegisterMethod(@TdxQueryGrid.MovePrior, 'MovePrior');
    RegisterMethod(@TdxQueryGrid.MoveNext, 'MoveNext');
    RegisterMethod(@TdxQueryGrid.MoveLast, 'MoveLast');
    RegisterMethod(@TdxQueryGrid.MoveBy, 'MoveBy');
    RegisterMethod(@TdxQueryGrid.MoveTo, 'MoveTo');
    RegisterMethod(@TdxQueryGrid.EOF, 'EOF');
    RegisterMethod(@TdxQueryGrid.BOF, 'BOF');
    RegisterMethod(@TdxQueryGrid.RecId, 'RecId');
    RegisterMethod(@TdxQueryGrid.RecNo, 'RecNo');
    RegisterMethod(@TdxQueryGrid.RecordCount, 'RecordCount');
    RegisterMethod(@TdxQueryGrid.Locate, 'Locate');
    RegisterMethod(@TdxQueryGrid.GotoRecord, 'GotoRecord');
    RegisterMethod(@TdxQueryGrid.Refresh, 'Refresh');
    RegisterMethod(@TdxQueryGrid.Close, 'Close');
    RegisterMethod(@TdxQueryGrid.DisableScrollEvents, 'DisableScrollEvents');
    RegisterMethod(@TdxQueryGrid.EnableScrollEvents, 'EnableScrollEvents');
    RegisterMethod(@TdxQueryGrid.ScrollEventsDisabled, 'ScrollEventsDisabled');
    //RegisterMethod(@TdxQueryGrid.FindColumnByTitle, 'FindColumnByTitle');

    RegisterPropertyHelper(@TdxQueryGridQueryName_R, nil, 'QueryName');
    RegisterPropertyHelper(@TdxQueryGridFields_R, nil, 'Fields');
    RegisterPropertyHelper(@TdxQueryGridAsI_R, nil, 'AsI');
    RegisterPropertyHelper(@TdxQueryGridAsF_R, nil, 'AsF');
    RegisterPropertyHelper(@TdxQueryGridAsDT_R, nil, 'AsDT');
    RegisterPropertyHelper(@TdxQueryGridAsS_R, nil, 'AsS');
    RegisterPropertyHelper(@TdxQueryGridManualRefresh_R, @TdxQueryGridManualRefresh_W, 'ManualRefresh');
    RegisterPropertyHelper(@TdxQueryGridEditable_R, nil, 'Editable');

    RegisterEventPropertyHelper(@TdxQueryGridOnAfterClose_R, @TdxQueryGridOnAfterClose_W, 'OnAfterClose');
    RegisterEventPropertyHelper(@TdxQueryGridOnAfterOpen_R, @TdxQueryGridOnAfterOpen_W, 'OnAfterOpen');
    RegisterEventPropertyHelper(@TdxQueryGridOnAfterScroll_R, @TdxQueryGridOnAfterScroll_W, 'OnAfterScroll');
    RegisterEventPropertyHelper(@TdxQueryGridOnBeforeClose_R, @TdxQueryGridOnBeforeClose_W, 'OnBeforeClose');
    RegisterEventPropertyHelper(@TdxQueryGridOnBeforeOpen_R, @TdxQueryGridOnBeforeOpen_W, 'OnBeforeOpen');
    RegisterEventPropertyHelper(@TdxQueryGridOnBeforeScroll_R, @TdxQueryGridOnBeforeScroll_W, 'OnBeforeScroll');
    RegisterEventPropertyHelper(@TdxQueryGridOnCreateForm_R, @TdxQueryGridOnCreateForm_W, 'OnCreateForm');

    RegisterMethod(@TdxQueryGrid.GetSourceFileName, 'GetSourceFileName');
    RegisterMethod(@TdxQueryGrid.GetStoredFileName, 'GetStoredFileName');
    RegisterMethod(@TdxQueryGrid.SaveBlobToStream, 'SaveToStream');
    RegisterMethod(@TdxQueryGrid.SaveBlobToFile, 'SaveToFile');
    RegisterMethod(@TdxQueryGrid.SaveThumbnailToStream, 'SaveThumbnailToStream');
  end;
end;

procedure TdxButtonOnClick_R(Self: TdxButton; var T: TNotifyEvent); begin T := Self.OnClick; end;
procedure TdxButtonOnClick_W(Self: TdxButton; T: TNotifyEvent); begin Self.OnClick := T; end;

procedure RIRegister_dxButton(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxButton) do
  begin
    RegisterMethod(@TdxButton.Click, 'Click');
    RegisterMethod(@TdxButton.SetClickHandler, 'SetClickHandler');
    RegisterEventPropertyHelper(@TdxButtonOnClick_R, @TdxButtonOnClick_W, 'OnClick');
  end;
end;

procedure RIRegister_dxPivotGrid(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxPivotGrid) do
  begin

  end;
end;

procedure RIRegister_dxChart(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxChart) do
  begin

  end;
end;

procedure TdxPageControlPages_R(Self: TdxPageControl; var T: TdxTabSheet; I: Integer); begin T := Self.Pages[I]; end;
procedure TdxPageControlPageCount_R(Self: TdxPageControl; var T: Integer); begin T := Self.PageCount; end;
procedure TdxPageControlActivePageIndex_R(Self: TdxPageControl; var T: Integer); begin T := Self.ActivePageIndex; end;
procedure TdxPageControlActivePageIndex_W(Self: TdxPageControl; T: Integer); begin Self.ActivePageIndex := T; end;

procedure RIRegister_dxPageControl(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxPageControl) do
  begin
    RegisterPropertyHelper(@TdxPageControlPages_R, nil, 'Pages');
    RegisterPropertyHelper(@TdxPageControlPageCount_R, nil, 'PageCount');
    RegisterPropertyHelper(@TdxPageControlActivePageIndex_R, @TdxPageControlActivePageIndex_W, 'ActivePageIndex');
  end;
end;

procedure TdxTabSheetTabVisible_R(Self: TdxTabSheet; var T: Boolean); begin T := Self.TabVisible; end;
procedure TdxTabSheetTabVisible_W(Self: TdxTabSheet; T: Boolean); begin Self.TabVisible := T; end;
procedure TdxTabSheetPageIndex_R(Self: TdxTabSheet; var T: Integer); begin T := Self.PageIndex; end;

procedure RIRegister_dxTabSheet(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxTabSheet) do
  begin
    RegisterPropertyHelper(@TdxTabSheetTabVisible_R, @TdxTabSheetTabVisible_W, 'TabVisible');
    RegisterPropertyHelper(@TdxTabSheetPageIndex_R, nil, 'PageIndex');
  end;
end;

procedure RIRegister_dxGroupBox(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxGroupBox) do
  begin

  end;
end;

procedure TdxRecordIdFieldName_R(Self: TdxRecordId; var T: String); begin T := Self.FieldName; end;
procedure TdxRecordIdId_R(Self: TdxRecordId; var T: Integer); begin T := Self.Id; end;

procedure RIRegister_dxRecordId(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TdxRecordId) do
  begin
    RegisterPropertyHelper(@TdxRecordIdFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TdxRecordIdId_R, nil, 'Id');
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TdxForm
////////////////////////////////////////////////////////////////////////////////

procedure TFilterFieldFieldName_R(Self: TFilterField; var T: String); begin T := Self.FieldName; end;
procedure TFilterFieldIsNot_R(Self: TFilterField; var T: Boolean); begin T := Self.IsNot; end;
procedure TFilterFieldIsNot_W(Self: TFilterField; T: Boolean); begin Self.IsNot := T; end;
procedure TFilterFieldIsNull_R(Self: TFilterField; var T: Boolean); begin T := Self.IsNull; end;
procedure TFilterFieldIsNull_W(Self: TFilterField; T: Boolean); begin Self.IsNull := T; end;
procedure TFilterFieldValues_R(Self: TFilterField; var T: TStringList); begin T := Self.Values; end;
procedure TFilterFieldValue_R(Self: TFilterField; var T: String; Index: Integer); begin T := Self.Value[Index]; end;
procedure TFilterFieldEndValue_R(Self: TFilterField; var T: String; Index: Integer); begin T := Self.EndValue[Index]; end;

procedure TFilterObjectFields_R(Self: TFilterObject; var T: TFilterField; Index: Integer); begin T := Self.Fields[Index]; end;
procedure TFilterObjectCount_R(Self: TFilterObject; var T: Integer); begin T := Self.Count; end;

procedure MyDestroyForm(Fm: TdxForm);
var
  RS: TSsRecordSet;
begin
  //if Fm.OnDestroy <> nil then Fm.OnDestroy(Fm);
  RS := TSsRecordSet(Fm.RecordSet);
  //if RS.Session.OnDestroyForm <> nil then RS.Session.OnDestroyForm(RS.Session, RS.Form);
  RS.Session.RecordSets.DeleteRecordSet(RS);
end;
procedure TdxFormFields_R(Self: TdxForm; var T: Variant; Index: String); begin T := Self.Fields[Index]; end;
procedure TdxFormFields_W(Self: TdxForm; T: Variant; Index: String); begin Self.Fields[Index] := T; end;
procedure TdxFormField_R(Self: TdxForm; var T: TField; Index: String); begin T := Self.Field[Index]; end;
procedure TdxFormAsI_R(Self: TdxForm; var T: Integer; Index: String); begin T := Self.AsI[Index]; end;
procedure TdxFormAsF_R(Self: TdxForm; var T: Double; Index: String); begin T := Self.AsF[Index]; end;
procedure TdxFormAsDT_R(Self: TdxForm; var T: TDateTime; Index: String); begin T := Self.AsDT[Index]; end;
procedure TdxFormAsS_R(Self: TdxForm; var T: String; Index: String); begin T := Self.AsS[Index]; end;
procedure TdxFormOldValues_R(Self: TdxForm; var T: Variant; Index: String); begin T := Self.OldValues[Index]; end;
procedure TdxFormState_R(Self: TdxForm; var T: TDataSetState); begin T := Self.State; end;
procedure TdxFormForms_R(Self: TdxForm; var T: TdxForm; Index: String); begin T := Self.Forms[Index]; end;
procedure TdxFormFormByIndex_R(Self: TdxForm; var T: TdxForm; Index: Integer); begin T := Self.FormByIndex[Index]; end;
procedure TdxFormFormCount_R(Self: TdxForm; var T: Integer); begin T := Self.FormCount; end;
procedure TdxFormParams_R(Self: TdxForm; var T: TParamList); begin T := Self.Params; end;
procedure TdxFormParentForm_R(Self: TdxForm; var T: TdxForm); begin T := Self.ParentForm; end;
procedure TdxFormId_R(Self: TdxForm; var T: Integer); begin T := Self.Id; end;
procedure TdxFormPId_R(Self: TdxForm; var T: Integer); begin T := Self.PId; end;
procedure TdxFormFormCaption_R(Self: TdxForm; var T: String); begin T := Self.FormCaption; end;
procedure TdxFormFilter_R(Self: TdxForm; var T: TFilterObject); begin T := Self.Filter; end;
procedure TdxFormModified_R(Self: TdxForm; var T: Boolean); begin T := Self.Modified; end;
procedure TdxFormViewType_R(Self: TdxForm; var T: TViewType); begin T := Self.ViewType; end;
procedure TdxFormCustomFilter_R(Self: TdxForm; var T: String); begin T := Self.CustomFilter; end;
procedure TdxFormCustomFilter_W(Self: TdxForm; T: String); begin Self.CustomFilter := T; end;
procedure TdxFormCustomFilterForm_R(Self: TdxForm; var T: TdxForm); begin T := Self.CustomFilterForm; end;
procedure TdxFormCustomFilterForm_W(Self: TdxForm; T: TdxForm); begin Self.CustomFilterForm := T; end;
procedure TdxFormUseSelectCondition_R(Self: TdxForm; var T: Boolean); begin T := Self.UseSelectCondition; end;
procedure TdxFormUseSelectCondition_W(Self: TdxForm; T: Boolean); begin Self.UseSelectCondition := T; end;
procedure TdxFormRecordsCaption_R(Self: TdxForm; var T: String); begin T := Self.RecordsCaption; end;
procedure TdxFormRecordsCaption_W(Self: TdxForm; T: String); begin Self.RecordsCaption := T; end;
procedure TdxFormRecordCaption_R(Self: TdxForm; var T: String); begin T := Self.RecordCaption; end;
procedure TdxFormRecordCaption_W(Self: TdxForm; T: String); begin Self.RecordCaption := T; end;
procedure TdxFormImages_R(Self: TdxForm; var T: TdxDBImage; Index: String); begin T := Self.Images[Index]; end;
procedure TdxFormFiles_R(Self: TdxForm; var T: TdxFile; Index: String); begin T := Self.Files[Index]; end;
procedure TdxFormLockMode_R(Self: TdxForm; var T: TLockMode); begin T := Self.LockMode; end;
procedure TdxFormQueryByIndex_R(Self: TdxForm; var T: TdxQueryGrid; Index: Integer); begin T := Self.QueryByIndex[Index]; end;
procedure TdxFormQueries_R(Self: TdxForm; var T: TdxQueryGrid; Index: String); begin T := Self.Queries[Index]; end;
procedure TdxFormQueryCount_R(Self: TdxForm; var T: Integer); begin T := Self.QueryCount; end;
procedure TdxFormActionResult_R(Self: TdxForm; var T: Variant); begin T := Self.ActionResult; end;
procedure TdxFormActionResult_W(Self: TdxForm; T: Variant); begin Self.ActionResult := T; end;
procedure TdxFormMsgs_R(Self: TdxForm; var T: TStringList); begin T := Self.Errs; end;
procedure TdxFormMsgs_W(Self: TdxForm; T: TStringList); begin Self.Errs.Assign(T); end;

procedure TdxFormOnAfterCancel_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnAfterCancel; end;
procedure TdxFormOnAfterCancel_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnAfterCancel := T; end;
procedure TdxFormOnAfterClose_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnAfterClose; end;
procedure TdxFormOnAfterClose_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnAfterClose := T; end;
procedure TdxFormOnAfterDelete_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnAfterDelete; end;
procedure TdxFormOnAfterDelete_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnAfterDelete := T; end;
procedure TdxFormOnAfterEdit_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnAfterEdit; end;
procedure TdxFormOnAfterEdit_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnAfterEdit := T; end;
procedure TdxFormOnAfterInsert_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnAfterInsert; end;
procedure TdxFormOnAfterInsert_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnAfterInsert := T; end;
procedure TdxFormOnAfterOpen_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnAfterOpen; end;
procedure TdxFormOnAfterOpen_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnAfterOpen := T; end;
procedure TdxFormOnAfterPost_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnAfterPost; end;
procedure TdxFormOnAfterPost_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnAfterPost := T; end;
procedure TdxFormOnAfterScroll_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnAfterScroll; end;
procedure TdxFormOnAfterScroll_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnAfterScroll := T; end;
procedure TdxFormOnAfterDuplicate_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnAfterDuplicate; end;
procedure TdxFormOnAfterDuplicate_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnAfterDuplicate := T; end;
procedure TdxFormOnBeforeCancel_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnBeforeCancel; end;
procedure TdxFormOnBeforeCancel_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnBeforeCancel := T; end;
procedure TdxFormOnBeforeClose_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnBeforeClose; end;
procedure TdxFormOnBeforeClose_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnBeforeClose := T; end;
procedure TdxFormOnBeforeDelete_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnBeforeDelete; end;
procedure TdxFormOnBeforeDelete_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnBeforeDelete := T; end;
procedure TdxFormOnBeforeEdit_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnBeforeEdit; end;
procedure TdxFormOnBeforeEdit_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnBeforeEdit := T; end;
procedure TdxFormOnBeforeInsert_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnBeforeInsert; end;
procedure TdxFormOnBeforeInsert_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnBeforeInsert := T; end;
procedure TdxFormOnBeforeOpen_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnBeforeOpen; end;
procedure TdxFormOnBeforeOpen_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnBeforeOpen := T; end;
procedure TdxFormOnBeforePost_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnBeforePost; end;
procedure TdxFormOnBeforePost_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnBeforePost := T; end;
procedure TdxFormOnBeforeScroll_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnBeforeScroll; end;
procedure TdxFormOnBeforeScroll_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnBeforeScroll := T; end;
procedure TdxFormOnBeforeDuplicate_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnBeforeDuplicate; end;
procedure TdxFormOnBeforeDuplicate_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnBeforeDuplicate := T; end;
procedure TdxFormOnValidate_R(Self: TdxForm; var T: TValidateEvent); begin T := Self.OnValidate; end;
procedure TdxFormOnValidate_W(Self: TdxForm; T: TValidateEvent); begin Self.OnValidate := T; end;
procedure TdxFormOnFieldChange_R(Self: TdxForm; var T: TFieldChangeEvent); begin T := Self.OnFieldChange; end;
procedure TdxFormOnFieldChange_W(Self: TdxForm; T: TFieldChangeEvent); begin Self.OnFieldChange := T; end;
procedure TdxFormOnPrint_R(Self: TdxForm; var T: TPrintEvent); begin T := Self.OnPrint; end;
procedure TdxFormOnPrint_W(Self: TdxForm; T: TPrintEvent); begin Self.OnPrint := T; end;
procedure TdxFormOnStateChange_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnStateChange; end;
procedure TdxFormOnStateChange_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnStateChange := T; end;
procedure TdxFormOnDestroy_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnDestroy; end;
procedure TdxFormOnDestroy_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnDestroy := T; end;
{procedure TdxFormOnMsgButtonClick_R(Self: TdxForm; var T: TMsgButtonClickEvent); begin T := Self.OnMsgButtonClick; end;
procedure TdxFormOnMsgButtonClick_W(Self: TdxForm; T: TMsgButtonClickEvent); begin Self.OnMsgButtonClick := T; end;  }
procedure TdxFormOnShowForm_R(Self: TdxForm; var T: TNotifyEvent); begin T := Self.OnShowForm; end;
procedure TdxFormOnShowForm_W(Self: TdxForm; T: TNotifyEvent); begin Self.OnShowForm := T; end;

procedure RIRegister_dxForm(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TFilterField) do
  begin
    RegisterPropertyHelper(@TFilterFieldFieldName_R, nil, 'FieldName');
    RegisterPropertyHelper(@TFilterFieldIsNot_R, @TFilterFieldIsNot_W, 'IsNot');
    RegisterPropertyHelper(@TFilterFieldIsNull_R, @TFilterFieldIsNull_W, 'IsNull');
    RegisterPropertyHelper(@TFilterFieldValues_R, nil, 'Values');
    RegisterPropertyHelper(@TFilterFieldValue_R, nil, 'Value');
    RegisterPropertyHelper(@TFilterFieldEndValue_R, nil, 'EndValue');
  end;

  with Cl.Add(TFilterObject) do
  begin
    RegisterMethod(@TFilterObject.AddField, 'AddField');
    RegisterMethod(@TFilterObject.FindFieldByName, 'FindField');
    RegisterMethod(@TFilterObject.DeleteField, 'DeleteField');
    RegisterMethod(@TFilterObject.Clear, 'Clear');
    RegisterPropertyHelper(@TFilterObjectFields_R, nil, 'Fields');
    RegisterPropertyHelper(@TFilterObjectCount_R, nil, 'Count');
  end;

  with Cl.Add(TdxForm) do
  begin
    RegisterMethod(@MyDestroyForm, 'Free');
    RegisterMethod(@TdxForm.Append, 'Append');
    RegisterMethod(@TdxForm.Insert, 'Insert');
    RegisterMethod(@TdxForm.Edit, 'Edit');
    RegisterMethod(@TdxForm.Delete, 'Delete');
    RegisterMethod(@TdxForm.Post, 'Post');
    RegisterMethod(@TdxForm.Cancel, 'Cancel');
    RegisterMethod(@TdxForm.Refresh, 'Refresh');
    RegisterMethod(@TdxForm.MoveFirst, 'MoveFirst');
    RegisterMethod(@TdxForm.MovePrior, 'MovePrior');
    RegisterMethod(@TdxForm.MoveNext, 'MoveNext');
    RegisterMethod(@TdxForm.MoveLast, 'MoveLast');
    RegisterMethod(@TdxForm.MoveBy, 'MoveBy');
    RegisterMethod(@TdxForm.MoveTo, 'MoveTo');
    RegisterMethod(@TdxForm.Bof, 'Bof');
    RegisterMethod(@TdxForm.Eof, 'Eof');
    RegisterMethod(@TdxForm.RecNo, 'RecNo');
    RegisterMethod(@TdxForm.RecId, 'RecId');
    RegisterMethod(@TdxForm.RecordCount, 'RecordCount');
    RegisterMethod(@TdxForm.Print, 'Print');
    RegisterMethod(@TdxForm.Locate, 'Locate');
    RegisterMethod(@TdxForm.GotoRecord, 'GotoRecord');
    RegisterMethod(@TdxForm.CanAppend, 'CanAppend');
    RegisterMethod(@TdxForm.CanEdit, 'CanEdit');
    RegisterMethod(@TdxForm.CanDelete, 'CanDelete');
    RegisterMethod(@TdxForm.Open, 'Open');
    RegisterMethod(@TdxForm.OpenRecord, 'OpenRecord');
    RegisterMethod(@TdxForm.OpenRecords, 'OpenRecords');
    RegisterMethod(@TdxForm.Opened, 'Opened');
    RegisterMethod(@TdxForm.Close, 'Close');
    RegisterMethod(@TdxForm.Validate, 'Validate');
    RegisterMethod(@TdxForm.FindComponentByFieldName, 'FindComponentByFieldName');
    RegisterMethod(@TdxForm.DisableScrollEvents, 'DisableScrollEvents');
    RegisterMethod(@TdxForm.EnableScrollEvents, 'EnableScrollEvents');
    RegisterMethod(@TdxForm.ScrollEventsDisabled, 'ScrollEventsDisabled');
    RegisterMethod(@TdxForm.GetRecordsCaption, 'GetRecordsCaption');
    RegisterMethod(@TdxForm.GetRecordCaption, 'GetRecordCaption');
    RegisterMethod(@TdxForm.WhoEdit, 'WhoEdit');
    RegisterMethod(@TdxForm.GotoForm, 'GotoForm');
    RegisterMethod(@TdxForm.GotoReport, 'GotoReport');
    RegisterMethod(@TdxForm.GotoUrl, 'GotoUrl');
    RegisterMethod(@TdxForm.MessageDlg, 'MessageDlg');
    RegisterMethod(@TdxForm.MsgBox, 'MsgBox');

    RegisterPropertyHelper(@TdxFormFields_R, @TdxFormFields_W, 'Fields');
    RegisterPropertyHelper(@TdxFormField_R, nil, 'Field');
    RegisterPropertyHelper(@TdxFormAsI_R, nil, 'AsI');
    RegisterPropertyHelper(@TdxFormAsF_R, nil, 'AsF');
    RegisterPropertyHelper(@TdxFormAsDT_R, nil, 'AsDT');
    RegisterPropertyHelper(@TdxFormAsS_R, nil, 'AsS');
    RegisterPropertyHelper(@TdxFormOldValues_R, nil, 'OldValues');
    RegisterPropertyHelper(@TdxFormState_R, nil, 'State');
    RegisterPropertyHelper(@TdxFormForms_R, nil, 'Forms');
    RegisterPropertyHelper(@TdxFormFormByIndex_R, nil, 'FormByIndex');
    RegisterPropertyHelper(@TdxFormFormCount_R, nil, 'FormCount');
    RegisterPropertyHelper(@TdxFormParams_R, nil, 'Params');
    RegisterPropertyHelper(@TdxFormParentForm_R, nil, 'ParentForm');
    RegisterPropertyHelper(@TdxFormId_R, nil, 'Id');
    RegisterPropertyHelper(@TdxFormPId_R, nil, 'PId');
    RegisterPropertyHelper(@TdxFormFormCaption_R, nil, 'FormCaption');
    RegisterPropertyHelper(@TdxFormFilter_R, nil, 'Filter');
    RegisterPropertyHelper(@TdxFormModified_R, nil, 'Modified');
    RegisterPropertyHelper(@TdxFormViewType_R, nil, 'ViewType');
    RegisterPropertyHelper(@TdxFormCustomFilter_R, @TdxFormCustomFilter_W, 'CustomFilter');
    RegisterPropertyHelper(@TdxFormCustomFilterForm_R, @TdxFormCustomFilterForm_W, 'CustomFilterForm');
    RegisterPropertyHelper(@TdxFormUseSelectCondition_R, @TdxFormUseSelectCondition_W, 'UseSelectCondition');
    RegisterPropertyHelper(@TdxFormRecordsCaption_R, @TdxFormRecordsCaption_W, 'RecordsCaption');
    RegisterPropertyHelper(@TdxFormRecordCaption_R, @TdxFormRecordCaption_W, 'RecordCaption');
    RegisterPropertyHelper(@TdxFormImages_R, nil, 'Images');
    RegisterPropertyHelper(@TdxFormFiles_R, nil, 'Files');
    RegisterPropertyHelper(@TdxFormLockMode_R, nil, 'LockMode');
    RegisterPropertyHelper(@TdxFormQueries_R, nil, 'Queries');
    RegisterPropertyHelper(@TdxFormQueryByIndex_R, nil, 'QueryByIndex');
    RegisterPropertyHelper(@TdxFormQueryCount_R, nil, 'QueryCount');
    RegisterPropertyHelper(@TdxFormActionResult_R, @TdxFormActionResult_W, 'ActionResult');
    RegisterPropertyHelper(@TdxFormMsgs_R, @TdxFormMsgs_W, 'Msgs');

    RegisterEventPropertyHelper(@TdxFormOnAfterCancel_R, @TdxFormOnAfterCancel_W, 'OnAfterCancel');
    RegisterEventPropertyHelper(@TdxFormOnAfterClose_R, @TdxFormOnAfterClose_W, 'OnAfterClose');
    RegisterEventPropertyHelper(@TdxFormOnAfterDelete_R, @TdxFormOnAfterDelete_W, 'OnAfterDelete');
    RegisterEventPropertyHelper(@TdxFormOnAfterEdit_R, @TdxFormOnAfterEdit_W, 'OnAfterEdit');
    RegisterEventPropertyHelper(@TdxFormOnAfterInsert_R, @TdxFormOnAfterInsert_W, 'OnAfterInsert');
    RegisterEventPropertyHelper(@TdxFormOnAfterOpen_R, @TdxFormOnAfterOpen_W, 'OnAfterOpen');
    RegisterEventPropertyHelper(@TdxFormOnAfterPost_R, @TdxFormOnAfterPost_W, 'OnAfterPost');
    RegisterEventPropertyHelper(@TdxFormOnAfterScroll_R, @TdxFormOnAfterScroll_W, 'OnAfterScroll');
    RegisterEventPropertyHelper(@TdxFormOnAfterDuplicate_R, @TdxFormOnAfterDuplicate_W, 'OnAfterDuplicate');
    RegisterEventPropertyHelper(@TdxFormOnBeforeCancel_R, @TdxFormOnBeforeCancel_W, 'OnBeforeCancel');
    RegisterEventPropertyHelper(@TdxFormOnBeforeClose_R, @TdxFormOnBeforeClose_W, 'OnBeforeClose');
    RegisterEventPropertyHelper(@TdxFormOnBeforeDelete_R, @TdxFormOnBeforeDelete_W, 'OnBeforeDelete');
    RegisterEventPropertyHelper(@TdxFormOnBeforeEdit_R, @TdxFormOnBeforeEdit_W, 'OnBeforeEdit');
    RegisterEventPropertyHelper(@TdxFormOnBeforeInsert_R, @TdxFormOnBeforeInsert_W, 'OnBeforeInsert');
    RegisterEventPropertyHelper(@TdxFormOnBeforeOpen_R, @TdxFormOnBeforeOpen_W, 'OnBeforeOpen');
    RegisterEventPropertyHelper(@TdxFormOnBeforePost_R, @TdxFormOnBeforePost_W, 'OnBeforePost');
    RegisterEventPropertyHelper(@TdxFormOnBeforeScroll_R, @TdxFormOnBeforeScroll_W, 'OnBeforeScroll');
    RegisterEventPropertyHelper(@TdxFormOnBeforeDuplicate_R, @TdxFormOnBeforeDuplicate_W, 'OnBeforeDuplicate');
    RegisterEventPropertyHelper(@TdxFormOnValidate_R, @TdxFormOnValidate_W, 'OnValidate');
    RegisterEventPropertyHelper(@TdxFormOnFieldChange_R, @TdxFormOnFieldChange_W, 'OnFieldChange');
    RegisterEventPropertyHelper(@TdxFormOnPrint_R, @TdxFormOnPrint_W, 'OnPrint');
    RegisterEventPropertyHelper(@TdxFormOnStateChange_R, @TdxFormOnStateChange_W, 'OnStateChange');
    RegisterEventPropertyHelper(@TdxFormOnDestroy_R, @TdxFormOnDestroy_W, 'OnDestroy');
    //RegisterEventPropertyHelper(@TdxFormOnMsgButtonClick_R, @TdxFormOnMsgButtonClick_W, 'OnMsgButtonClick');
    RegisterEventPropertyHelper(@TdxFormOnShowForm_R, @TdxFormOnShowForm_W, 'OnShowForm');
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TSession
////////////////////////////////////////////////////////////////////////////////

procedure TSessionFormCount_R(Self: TSession; var T: Integer); begin T := Self.RecordSets.Count; end;
procedure TSessionForms_R(Self: TSession; var T: TdxForm; I: Integer); begin T := Self.RecordSets[I].Form; end;
procedure TSessionOnCreateForm_R(Self: TSession; var T: TCreateFormEvent); begin T := Self.OnCreateForm; end;
procedure TSessionOnCreateForm_W(Self: TSession; T: TCreateFormEvent); begin Self.OnCreateForm := T; end;
procedure TSessionOnDestroyForm_R(Self: TSession; var T: TCreateFormEvent); begin T := Self.OnDestroyForm; end;
procedure TSessionOnDestroyForm_W(Self: TSession; T: TCreateFormEvent); begin Self.OnDestroyForm := T; end;
procedure TSessionOnDatabaseClose_R(Self: TSession; var T: TNotifyEvent); begin T := Self.OnDatabaseClose; end;
procedure TSessionOnDatabaseClose_W(Self: TSession; T: TNotifyEvent); begin Self.OnDatabaseClose := T; end;
procedure TSessionOnHandleRequest_R(Self: TSession; var T: TWebServerRequestHandler); begin T := Self.OnHandleRequest; end;
procedure TSessionOnHandleRequest_W(Self: TSession; T: TWebServerRequestHandler); begin Self.OnHandleRequest := T; end;
procedure TSessionRequest_R(Self: TSession; var T: TFPHTTPConnectionRequest); begin T := Self.Request; end;

procedure RIRegister_Session(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSession) do
  begin
    RegisterMethod(@TSession.CreateForm, 'CreateForm');
    RegisterMethod(@TSession.SQLSelect, 'SQLSelect');
    RegisterMethod(@TSession.SQLExecute, 'SQLExecute');
    RegisterMethod(@TSession.EvalExpr, 'EvalExpr');
    RegisterMethod(@TSession.FindForm, 'FindForm');
    RegisterMethod(@TSession.GetCacheDir, 'GetCacheDir');
    RegisterMethod(@TSession.Debug, 'Debug');
    RegisterMethod(@TSession.GetCurrentUser, 'GetCurrentUser');
    RegisterMethod(@TSession.GetCurrentRole, 'GetCurrentRole');
    RegisterMethod(@TSession.GetCurrentDatabase, 'GetCurrentDatabase');
    RegisterMethod(@TSession.GetTemplatesPath, 'GetTemplatesDir');
    RegisterMethod(@TSession.GetExprVar, 'GetExprVar');
    RegisterMethod(@TSession.SetExprVar, 'SetExprVar');

    RegisterPropertyHelper(@TSessionFormCount_R, nil, 'FormCount');
    RegisterPropertyHelper(@TSessionForms_R, nil, 'Forms');
    RegisterPropertyHelper(@TSessionRequest_R, nil, 'Request');
    RegisterEventPropertyHelper(@TSessionOnCreateForm_R, @TSessionOnCreateForm_W, 'OnCreateForm');
    RegisterEventPropertyHelper(@TSessionOnDestroyForm_R, @TSessionOnDestroyForm_W, 'OnDestroyForm');
    RegisterEventPropertyHelper(@TSessionOnDatabaseClose_R, @TSessionOnDatabaseClose_W, 'OnDatabaseClose');
    RegisterEventPropertyHelper(@TSessionOnHandleRequest_R, @TSessionOnHandleRequest_W, 'OnHandleRequest');
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Functions
////////////////////////////////////////////////////////////////////////////////

procedure MySplitStr(const S: String; D: Char; SL: TStrings);
begin
  SplitStr(S, D, SL);
end;

function RegFunctions86_64(Caller: TPSExec; p: TPSExternalProcRec; Global,
  Stack: TPSStack): Boolean;
var
  R: TRect;
  resData: Pointer;
  val: TPSVariantIFC;
  Obj: TObject;
begin
  Result := True;
  case PtrUInt(p.Ext1) of
    6:                                                                                        // Rect
      begin
        R := Rect(Stack.GetInt(-2), Stack.GetInt(-3), Stack.GetInt(-4), Stack.GetInt(-5));
        resData := @PPSVariantData(Stack.Items[Stack.Count - 1])^.Data;
        resData := Pointer(resData^);
        TRect(resData^) := R; // или, что то же самое, Move(R, resData^, SizeOf(R));
      end;
    7:                                                                                        // FreeAndNil
      begin
        val := NewTPSVariantIFC(Stack[Stack.Count-1],true);
        if val.aType.BaseType = 25 {btClass} then
        begin
          Obj := TObject(val.Dta^);
          if Obj is TdxForm then MyDestroyForm(TdxForm(Obj))
          else Obj.Free;
          Stack.SetClass(-1, nil);
        end
        else
          // Caller.CMD_Err(erInvalidType);
          Result := False;                    // Сделал как в uPSRuntime
      end
    else
      Result := False;
  end;
end;

procedure RIRegister_Functions(Exec: TPSExec);
begin
  Exec.RegisterDelphiFunction(@MyUTF8Length, 'Utf8Length', cdRegister);
  Exec.RegisterDelphiFunction(@UTF8Pos, 'Utf8Pos', cdRegister);
  Exec.RegisterDelphiFunction(@UTF8Copy, 'Utf8Copy', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF8Delete, 'Utf8Delete', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF8Insert, 'Utf8Insert', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF8StringReplace, 'Utf8StringReplace', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF8LowerCase, 'Utf8LowerCase', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF8UpperCase, 'Utf8UpperCase', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF8CompareStr, 'Utf8CompareStr', cdRegister);
  Exec.RegisterDelphiFunction(@MyUtf8CompareText, 'Utf8CompareText', cdRegister);
  Exec.RegisterDelphiFunction(@Utf8ToWinCP, 'Utf8ToWinCP', cdRegister);
  Exec.RegisterDelphiFunction(@WinCPToUtf8, 'WinCPToUtf8', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF8ToUTF16, 'Utf8ToUtf16', cdRegister);
  Exec.RegisterDelphiFunction(@MyUTF16ToUTF8, 'Utf16ToUtf8', cdRegister);

  Exec.RegisterDelphiFunction(@FileExistsUtf8, 'FileExists', cdRegister);
  Exec.RegisterDelphiFunction(@MyFileAge, 'FileAge', cdRegister);
  Exec.RegisterDelphiFunction(@DirectoryExistsUtf8, 'DirectoryExists', cdRegister);
  Exec.RegisterDelphiFunction(@ExpandFileNameUtf8, 'ExpandFileName', cdRegister);
  Exec.RegisterDelphiFunction(@MyFileSetDate, 'FileSetDate', cdRegister);
  Exec.RegisterDelphiFunction(@FileGetAttrUtf8, 'FileGetAttr', cdRegister);
  Exec.RegisterDelphiFunction(@FileSetAttrUtf8, 'FileSetAttr', cdRegister);
  Exec.RegisterDelphiFunction(@DeleteFileUtf8, 'DeleteFile', cdRegister);
  Exec.RegisterDelphiFunction(@RenameFileUtf8, 'RenameFile', cdRegister);
  Exec.RegisterDelphiFunction(@GetCurrentDirUtf8, 'GetCurrentDir', cdRegister);
  Exec.RegisterDelphiFunction(@CreateDirUtf8, 'CreateDir', cdRegister);
  Exec.RegisterDelphiFunction(@RemoveDirUtf8, 'RemoveDir', cdRegister);
  Exec.RegisterDelphiFunction(@ForceDirectoriesUtf8, 'ForceDirectories', cdRegister);
  Exec.RegisterDelphiFunction(@MyCopyFile, 'CopyFile', cdRegister);
  Exec.RegisterDelphiFunction(@MyFindAllFiles, 'FindAllFiles', cdRegister);
  Exec.RegisterDelphiFunction(@MyFindAllDirectories, 'FindAllDirectories', cdRegister);
  Exec.RegisterDelphiFunction(@MyExtractFileName, 'ExtractFileName', cdRegister);
  Exec.RegisterDelphiFunction(@ExtractFileNameOnly, 'ExtractFileNameOnly', cdRegister);
  Exec.RegisterDelphiFunction(@MyExtractFileExt, 'ExtractFileExt', cdRegister);
  Exec.RegisterDelphiFunction(@MyExtractFilePath, 'ExtractFilePath', cdRegister);
  Exec.RegisterDelphiFunction(@MyExtractFileDrive, 'ExtractFileDrive', cdRegister);
  Exec.RegisterDelphiFunction(@MyExtractFileDir, 'ExtractFileDir', cdRegister);
  Exec.RegisterDelphiFunction(@MyChangeFileExt, 'ChangeFileExt', cdRegister);
  Exec.RegisterDelphiFunction(@MyIncludeTrailingPathDelimiter, 'IncludeTrailingPathDelimiter', cdRegister);
  Exec.RegisterDelphiFunction(@MyExcludeLeadingPathDelimiter, 'ExcludeLeadingPathDelimiter', cdRegister);
  Exec.RegisterDelphiFunction(@MyGetTempFilename, 'GetTempFileName', cdRegister);
  Exec.RegisterDelphiFunction(@MyGetTempDir, 'GetTempDir', cdRegister);
  Exec.RegisterDelphiFunction(@ShellExec, 'ShellExecute', cdRegister);
  Exec.RegisterDelphiFunction(@FileDateToDateTime, 'FileDateToDateTime', cdRegister);
  Exec.RegisterDelphiFunction(@DateTimeToFileDate, 'DateTimeToFileDate', cdRegister);
  Exec.RegisterDelphiFunction(@FileSizeUtf8, 'FileSize', cdRegister);

  //Exec.RegisterDelphiFunction(@GetForms, 'GetForms', cdRegister);

  Exec.RegisterDelphiFunction(@MyRandom, 'Random', cdRegister);

  Exec.RegisterDelphiFunction(@DCount, 'DCount', cdRegister);
  Exec.RegisterDelphiFunction(@DSum, 'DSum', cdRegister);
  Exec.RegisterDelphiFunction(@DAvg, 'DAvg', cdRegister);
  Exec.RegisterDelphiFunction(@DMax, 'DMax', cdRegister);
  Exec.RegisterDelphiFunction(@DMin, 'DMin', cdRegister);
  Exec.RegisterDelphiFunction(@DMerge, 'DMerge', cdRegister);
  Exec.RegisterDelphiFunction(@ToWordsRu, 'ToWords', cdRegister);
  Exec.RegisterDelphiFunction(@RurToWords, 'RurToWords', cdRegister);
  Exec.RegisterDelphiFunction(@Nz, 'Nz', cdRegister);
  Exec.RegisterDelphiFunction(@MathRound, 'RoundTo', cdRegister);
  Exec.RegisterDelphiFunction(@_MyFrac, 'Frac', cdRegister);
  Exec.RegisterDelphiFunction(@MyPower, 'Power', cdRegister);
  Exec.RegisterDelphiFunction(@YearsBetweenEx, 'YearsBetween', cdRegister);
  Exec.RegisterDelphiFunction(@MonthsBetweenEx, 'MonthsBetween', cdRegister);
  Exec.RegisterDelphiFunction(@WeeksBetweenEx, 'WeeksBetween', cdRegister);
  Exec.RegisterDelphiFunction(@DaysBetweenEx, 'DaysBetween', cdRegister);
  Exec.RegisterDelphiFunction(@HoursBetweenEx, 'HoursBetween', cdRegister);
  Exec.RegisterDelphiFunction(@MinutesBetweenEx, 'MinutesBetween', cdRegister);
  Exec.RegisterDelphiFunction(@SecondsBetweenEx, 'SecondsBetween', cdRegister);
  Exec.RegisterDelphiFunction(@MilliSecondsBetween, 'MilliSecondsBetween', cdRegister);
  Exec.RegisterDelphiFunction(@IncYear, 'AddYear', cdRegister);
  Exec.RegisterDelphiFunction(@IncMonth, 'AddMonth', cdRegister);
  Exec.RegisterDelphiFunction(@IncWeek, 'AddWeek', cdRegister);
  Exec.RegisterDelphiFunction(@IncDay, 'AddDay', cdRegister);
  Exec.RegisterDelphiFunction(@IncHour, 'AddHour', cdRegister);
  Exec.RegisterDelphiFunction(@IncMinute, 'AddMinute', cdRegister);
  Exec.RegisterDelphiFunction(@IncSecond, 'AddSecond', cdRegister);
  Exec.RegisterDelphiFunction(@YearOf, 'YearOf', cdRegister);
  Exec.RegisterDelphiFunction(@MonthOf, 'MonthOf', cdRegister);
  Exec.RegisterDelphiFunction(@WeekOf, 'WeekOf', cdRegister);
  Exec.RegisterDelphiFunction(@DayOf, 'DayOf', cdRegister);
  Exec.RegisterDelphiFunction(@HourOf, 'HourOf', cdRegister);
  Exec.RegisterDelphiFunction(@MinuteOf, 'MinuteOf', cdRegister);
  Exec.RegisterDelphiFunction(@SecondOf, 'SecondOf', cdRegister);
  Exec.RegisterDelphiFunction(@MyStrToTime, 'StrToTime', cdRegister);
  Exec.RegisterDelphiFunction(@MyTimeToStr, 'TimeToStr', cdRegister);
  Exec.RegisterDelphiFunction(@MyTryStrToDate, 'TryStrToDate', cdRegister);
  Exec.RegisterDelphiFunction(@MyTryStrToTime, 'TryStrToTime', cdRegister);
  Exec.RegisterDelphiFunction(@MyTryStrToDateTime, 'TryStrToDateTime', cdRegister);
  Exec.RegisterDelphiFunction(@TryStrToInt, 'TryStrToInt', cdRegister);
  Exec.RegisterDelphiFunction(@TryStrToInt64, 'TryStrToInt64', cdRegister);
  Exec.RegisterDelphiFunction(@MyTryStrToFloat, 'TryStrToFloat', cdRegister);
  Exec.RegisterDelphiFunction(@MyStrToDateTime, 'StrToDateTime', cdRegister);
  Exec.RegisterDelphiFunction(@MyIntToHex, 'IntToHex', cdRegister);

  Exec.RegisterDelphiFunction(@GetMonthName, 'GetMonthName', cdRegister);
  Exec.RegisterDelphiFunction(@GetWeekName, 'GetWeekName', cdRegister);
  Exec.RegisterDelphiFunction(@DayOfTheWeek, 'DayOfTheWeek', cdRegister);
  Exec.RegisterDelphiFunction(@FmtDate, 'FmtDate', cdRegister);
  Exec.RegisterDelphiFunction(@SetZeros, 'FillZeros', cdRegister);
  Exec.RegisterDelphiFunction(@CalcPeriod, 'CalcPeriod', cdRegister);
  Exec.RegisterDelphiFunction(@BeginYear, 'BeginYear', cdRegister);
  Exec.RegisterDelphiFunction(@BeginQuarter, 'BeginQuarter', cdRegister);
  Exec.RegisterDelphiFunction(@BeginMonth, 'BeginMonth', cdRegister);
  Exec.RegisterDelphiFunction(@BeginWeek, 'BeginWeek', cdRegister);
  Exec.RegisterDelphiFunction(@EndYear, 'EndYear', cdRegister);
  Exec.RegisterDelphiFunction(@EndQuarter, 'EndQuarter', cdRegister);
  Exec.RegisterDelphiFunction(@EndMonth, 'EndMonth', cdRegister);
  Exec.RegisterDelphiFunction(@EndWeek, 'EndWeek', cdRegister);
  Exec.RegisterDelphiFunction(@QuarterOf, 'QuarterOf', cdRegister);
  {Exec.RegisterDelphiFunction(@GetUser, 'GetCurrentUser', cdRegister);
  Exec.RegisterDelphiFunction(@GetRole, 'GetCurrentRole', cdRegister);
  Exec.RegisterDelphiFunction(@GetCurrentDatabase, 'GetCurrentDatabase', cdRegister);
  Exec.RegisterDelphiFunction(@TemplatesPath, 'GetTemplatesDir', cdRegister);
  Exec.RegisterDelphiFunction(@GetOutDir, 'GetOutputDir', cdRegister);   }
  Exec.RegisterDelphiFunction(@MyFormat, 'Format', cdRegister);
  Exec.RegisterDelphiFunction(@MyStringReplace, 'StringReplace', cdRegister);
  Exec.RegisterDelphiFunction(@MySplitStr, 'SplitStr', cdRegister);

	//Exec.RegisterDelphiFunction(@SQLSelect, 'SQLSelect', cdRegister);
  //Exec.RegisterDelphiFunction(@SQLExecute, 'SQLExecute', cdRegister);

  {Exec.RegisterDelphiFunction(@ColorToString, 'ColorToString', cdRegister);
  Exec.RegisterDelphiFunction(@MyStringToColor, 'StringToColor', cdRegister);
  Exec.RegisterDelphiFunction(@RGBToColor, 'RGBToColor', cdRegister);
  Exec.RegisterDelphiFunction(@ColorToRGB, 'ColorToRGB', cdRegister);
  Exec.RegisterDelphiFunction(@RedGreenBlue, 'RedGreenBlue', cdRegister);}
  Exec.RegisterDelphiFunction(@MyFormatFloat, 'FormatFloat', cdRegister);

  Exec.RegisterDelphiFunction(@EncodeMD5, 'EncodeMD5', cdRegister);
  Exec.RegisterDelphiFunction(@EncodeSHA1, 'EncodeSHA1', cdRegister);
  Exec.RegisterDelphiFunction(@EncodeBase64, 'EncodeBase64', cdRegister);
  Exec.RegisterDelphiFunction(@DecodeBase64, 'DecodeBase64', cdRegister);
  Exec.RegisterDelphiFunction(@HMACMD5, 'HMACMD5', cdRegister);
  Exec.RegisterDelphiFunction(@HMACSHA1, 'HMACSHA1', cdRegister);

  Exec.RegisterDelphiFunction(@VarToStr, 'VarToStr', cdRegister);
  Exec.RegisterDelphiFunction(@VarIsNothing, 'VarIsNothing', cdRegister);
  Exec.RegisterDelphiFunction(@VarCast, 'VarCast', cdRegister);
  Exec.RegisterDelphiFunction(@VarAsType, 'VarAsType', cdRegister);

  Exec.RegisterDelphiFunction(@MySameValue, 'SameValue', cdRegister);

  Exec.RegisterDelphiFunction(@Point, 'Point', cdRegister);
  //Exec.RegisterDelphiFunction(@Rect, 'Rect', cdRegister);

  Exec.RegisterDelphiFunction(@GetComponentId, 'GetComponentId', cdRegister);
  {Exec.RegisterDelphiFunction(@ShowExprEditor, 'ShowExprEditor', cdRegister);   }
  Exec.RegisterDelphiFunction(@GetFieldName, 'GetComponentFieldName', cdRegister);
  Exec.RegisterDelphiFunction(@GetFormatSettings, 'GetFormatSettings', cdRegister);
  Exec.RegisterDelphiFunction(@SetFormatSettings, 'SetFormatSettings', cdRegister);

  Exec.RegisterDelphiFunction(@VarArrayOf, 'VarArrayOf', cdRegister);
  Exec.RegisterDelphiFunction(@VarArrayDimCount, 'VarArrayDimCount', cdRegister);
  Exec.RegisterDelphiFunction(@VarArrayLowBound, 'VarArrayLowBound', cdRegister);
  Exec.RegisterDelphiFunction(@VarArrayHighBound, 'VarArrayHighBound', cdRegister);

  Exec.RegisterDelphiFunction(@GetBuildDate, 'GetBuildDate', cdRegister);

  {Exec.RegisterDelphiFunction(@SetPropertyValue, 'SetPropValue', cdRegister);
  Exec.RegisterDelphiFunction(@GetPropertyValue, 'GetPropValue', cdRegister);}

  Exec.RegisterDelphiFunction(@ReadXmlFromFile, 'ReadXmlFromFile', cdRegister);
  Exec.RegisterDelphiFunction(@ReadXmlFromStream, 'ReadXmlFromStream', cdRegister);
  Exec.RegisterDelphiFunction(@ReadXmlFromString, 'ReadXmlFromString', cdRegister);
  Exec.RegisterDelphiFunction(@ReadXmlNodeFromString, 'ReadXmlNodeFromString', cdRegister);
  Exec.RegisterDelphiFunction(@WriteXmlToFile, 'WriteXmlToFile', cdRegister);
  Exec.RegisterDelphiFunction(@WriteXmlToStream, 'WriteXmlToStream', cdRegister);
  Exec.RegisterDelphiFunction(@WriteXmlToString, 'WriteXmlToString', cdRegister);
  Exec.RegisterDelphiFunction(@WriteXmlNodeToString, 'WriteXmlNodeToString', cdRegister);

  Exec.RegisterDelphiFunction(@ReadJSONFromString, 'ReadJSONFromString', cdRegister);
  Exec.RegisterDelphiFunction(@ReadJSONFromStream, 'ReadJSONFromStream', cdRegister);
  Exec.RegisterDelphiFunction(@ReadJSONFromFile, 'ReadJSONFromFile', cdRegister);
  Exec.RegisterDelphiFunction(@JSONStringToString, 'JSONStringToString', cdRegister);
  Exec.RegisterDelphiFunction(@StringToJSONString, 'StringToJSONString', cdRegister);

  //Exec.RegisterDelphiFunction(@Utf8CharToString, 'Utf8CharToString', cdRegister);
  //Exec.RegisterDelphiFunction(@StringToUtf8Char, 'StringToUtf8Char', cdRegister);
  Exec.RegisterDelphiFunction(@EncodeURLElement, 'EncodeURLElement', cdRegister);
  Exec.RegisterDelphiFunction(@DecodeURLElement, 'DecodeURLElement', cdRegister);
  Exec.RegisterDelphiFunction(@DebugFile, 'DebugFile', cdRegister);

  Exec.RegisterDelphiFunction(@IIF, 'IIF', cdRegister);
  Exec.RegisterDelphiFunction(@GenerateId, 'CreateGUIDString', cdRegister);
  Exec.RegisterDelphiFunction(@AppPath, 'GetAppDir', cdRegister);

  Exec.RegisterDelphiFunction(@CommandExecute, 'CommandExecute', cdRegister);
  Exec.RegisterDelphiFunction(@FileExecute, 'FileExecute', cdRegister);
  Exec.RegisterDelphiFunction(@Sleep, 'Delay', cdRegister);

  Exec.RegisterFunctionName('RECT', @RegFunctions86_64, Pointer(6), nil);
  Exec.RegisterFunctionName('FREEANDNIL', @RegFunctions86_64, Pointer(7), nil);
end;

procedure RIRegister_All(Cl: TPSRuntimeClassImporter);
begin
  RIRegister_Std(Cl);
  RIRegister_Classes(Cl);
  RIRegister_Graphics(Cl);
  RIRegister_Controls(Cl);
  RIRegister_Timer(Cl);
  RIRegister_IniFiles(Cl);
  RIRegister_TField(Cl);
  RIRegister_dxTypes(Cl);
  RIRegister_dxSQLQuery(Cl);
  RIRegister_HttpServer(Cl);
  RIRegister_HttpClient(Cl);
  RIRegister_Template(Cl);
  RIRegister_Xml(Cl);
  RIRegister_Json(Cl);
  RIRegister_CsvData(Cl);
  RIRegister_dxLabel(Cl);
  RIRegister_dxEdit(Cl);
  RIRegister_dxCalcEdit(Cl);
  RIRegister_dxDateEdit(Cl);
  RIRegister_dxTimeEdit(Cl);
  RIRegister_dxCounter(Cl);
  RIRegister_dxMemo(Cl);
  RIRegister_dxCheckBox(Cl);
  RIRegister_dxComboBox(Cl);
  RIRegister_dxLookupComboBox(Cl);
  RIRegister_dxObjectField(Cl);
  RIRegister_dxImage(Cl);
  RIRegister_dxDBImage(Cl);
  RIRegister_dxFile(Cl);
  RIRegister_dxShape(Cl);
  RIRegister_dxGrid(Cl);
  RIRegister_dxQueryGrid(Cl);
  RIRegister_dxButton(Cl);
  RIRegister_dxPivotGrid(Cl);
  RIRegister_dxChart(Cl);
  RIRegister_dxPageControl(Cl);
  RIRegister_dxTabSheet(Cl);
  RIRegister_dxGroupBox(Cl);
  RIRegister_dxForm(Cl);
  RIRegister_Session(Cl);
end;

end.

