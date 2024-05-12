unit DXMains;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TDXMain }

  TDXMain = class
  private
    FActions: String;
    FDesignTimePPI: Integer;
    FLastModified: TDateTime;
  public
    constructor Create;
    procedure LoadSettings(St: TStream);
    procedure LoadActions(St: TStream);
    //procedure RunActions;
    property Actions: String read FActions write FActions;
    property DesignTimePPI: Integer read FDesignTimePPI write FDesignTimePPI;
    property LastModified: TDateTime read FLastModified write FLastModified;
  end;

var
  DXMain: TDXMain;

implementation

uses
  SAX, saxbasereader;

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
begin
  if LocalName = 'designer' then
  begin
    FMain.DesignTimePPI := GetInt(Atts, 'designtimeppi');
  end
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
end;

end.

