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

unit DXMains;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mytypes;

type
  { TDXMain }

  TDXMain = class
  private
    FActions: String;
    FDesignTimePPI: Integer;
    FLastModified: TDateTime;
    FTabs: TIntegerList;
    FVersion: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadSettings(St: TStream);
    procedure LoadActions(St: TStream);
    //procedure RunActions;
    property Actions: String read FActions write FActions;
    property DesignTimePPI: Integer read FDesignTimePPI write FDesignTimePPI;
    property LastModified: TDateTime read FLastModified write FLastModified;
    property Tabs: TIntegerList read FTabs;
    property Version: Integer read FVersion;
  end;

var
  DXMain: TDXMain;

implementation

uses
  apputils, SAX, saxbasereader;

type

  { TSettingsReader }

  TSettingsReader = class(TSaxBaseReader)
  private
    FMain: TdxMain;
  protected
    procedure DoStartElement(const NamespaceURI, LocalName, QName: SAXString;
      Atts: TSAXAttributes); override;
  public
    property Main: TdxMain read FMain write FMain;
  end;

{ TSettingsReader }

procedure TSettingsReader.DoStartElement(const NamespaceURI, LocalName,
  QName: SAXString; Atts: TSAXAttributes);
var
  SL: TStringList;
  i: Integer;
begin
  if LocalName = 'designer' then
  begin
    FMain.DesignTimePPI := GetInt(Atts, 'designtimeppi');
    if AttrExists(Atts, 'tabs') then
    begin
      SL := TStringList.Create;
      SplitStr(GetStr(Atts, 'tabs'), ';', SL);
      for i := 0 to SL.Count - 1 do
        FMain.Tabs.AddValue(StrToInt(SL[i]));
      SL.Free;
    end;
  end
  else if LocalName = 'settings' then
  begin
    FMain.FVersion := GetInt(Atts, 'version');
  end;
end;

{ TDXMain }

{procedure TDXMain.RunActions;
var
  DSP: TDataSetProcessor;
begin
  if FActions = '' then Exit;

  DSP := TDataSetProcessor.Create;
  DSP.BindDummyForm;

  with TActionRunner.Create do
  try
    DSProc := DSP;
    DSRi := 0;
    Load(FActions);
    Run;
  finally
    DSP.ClearDummyForm;
    DSP.Free;
    Free;
  end;
end;  }

procedure TDXMain.LoadSettings(St: TStream);
begin
  if St = nil then Exit;
  with TSettingsReader.Create do
  try
    Main := Self;
    ParseStream(St);
  finally
    Free;
  end;
end;

procedure TDXMain.LoadActions(St: TStream);
begin
  if St = nil then Exit;
  SetLength(FActions, St.Size);
  St.ReadBuffer(Pointer(FActions)^, St.Size);
end;

constructor TDXMain.Create;
begin
  FDesignTimePPI := 96;
  FTabs := TIntegerList.Create;
end;

destructor TDXMain.Destroy;
begin
  FTabs.Free;
  inherited Destroy;
end;

end.

