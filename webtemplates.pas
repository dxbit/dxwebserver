unit WebTemplates;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fptemplate, mytypes;

type

  { TTemplate }

  TTemplate = class(TFPTemplate)
  private
    FTags: TVarList;
    function GetTagByIndex(Index: Integer): Variant;
    function GetTagCount: Integer;
    function GetTags(Index: String): Variant;
    procedure SetTagByIndex(Index: Integer; AValue: Variant);
    procedure SetTags(Index: String; AValue: Variant);
  protected
    {procedure GetParam(Sender: TObject; const ParamName: String; out AValue: String);
      override;}
    procedure ReplaceTag(Sender: TObject; const TagName: String; TagParams: TStringList;
      out AValue: String); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearTags;
    property Tags[Index: String]: Variant read GetTags write SetTags;
    property TagByIndex[Index: Integer]: Variant read GetTagByIndex write
      SetTagByIndex;
    property TagCount: Integer read GetTagCount;
  end;

implementation

uses
  Variants, scriptmanager, apputils;

{ TTemplate }

function TTemplate.GetTags(Index: String): Variant;
var
  pV: PVrData;
begin
  pV := FTags.FindVar(Index);
  if pV <> nil then Result := pV^.Value
  else Result := Null;
end;

function TTemplate.GetTagByIndex(Index: Integer): Variant;
begin
  Result := FTags[Index]^.Value;
end;

function TTemplate.GetTagCount: Integer;
begin
  Result := FTags.Count;
end;

procedure TTemplate.SetTagByIndex(Index: Integer; AValue: Variant);
begin
  FTags[Index]^.Value := AValue;
end;

procedure TTemplate.SetTags(Index: String; AValue: Variant);
var
  pV: PVrData;
begin
  pV := FTags.FindVar(Index);
  if pV = nil then FTags.AddVar(Index, AValue)
  else pV^.Value := AValue;
end;

procedure TTemplate.ReplaceTag(Sender: TObject; const TagName: String;
  TagParams: TStringList; out AValue: String);
var
  pV: PVrData;
begin
  pV := FTags.FindVar(TagName);
  if pV <> nil then
    AValue := VarToStr(pV^.Value)
  else
    inherited ReplaceTag(Sender, TagName, TagParams, AValue);
end;

constructor TTemplate.Create;
begin
  FTags := TVarList.Create;
  AllowTagParams:=True;
  StartDelimiter := '{+';
  EndDelimiter := '+}';
end;

destructor TTemplate.Destroy;
begin
  FTags.Free;
  inherited Destroy;
end;

procedure TTemplate.ClearTags;
begin
  FTags.Clear;
end;

end.

