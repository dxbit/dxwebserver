unit FormManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dxctrls;

type

  { TFormManager }

  TFormManager = class
  private
    FForms: TComponentList;
    function GetForms(Index: Integer): TdxForm;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddForm(Fm: TdxForm);
    function FormCount: Integer;
    function FindForm(Id: Integer): TdxForm;
    function FindFormByName(const S: String): TdxForm;
    procedure GetFormList(SL: TStrings);
    procedure GetSubForms(PFmId: Integer; SL: TStrings);
    property Forms[Index: Integer]: TdxForm read GetForms;
  end;

implementation

uses
  apputils, LazUtf8;

{ TFormManager }

function TFormManager.GetForms(Index: Integer): TdxForm;
begin
  Result := TdxForm(FForms[Index]);
end;

constructor TFormManager.Create;
begin
  //FSS := SS;
  FForms := TComponentList.Create;
end;

destructor TFormManager.Destroy;
begin
  FForms.Free;
  inherited Destroy;
end;

procedure TFormManager.AddForm(Fm: TdxForm);
begin
  FForms.Add(Fm);
end;

function TFormManager.FormCount: Integer;
begin
  Result := FForms.Count;
end;

function TFormManager.FindForm(Id: Integer): TdxForm;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FormCount - 1 do
    if Forms[i].Id = Id then Exit(Forms[i]);
end;

function TFormManager.FindFormByName(const S: String): TdxForm;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FormCount - 1 do
    if MyUtf8CompareText(Forms[i].FormCaption, S) = 0 then Exit(Forms[i]);
end;

procedure TFormManager.GetFormList(SL: TStrings);
var
  i: Integer;
begin
  SL.Clear;
  for i := 0 to FormCount - 1 do
    if Forms[i].PId = 0 then
      SL.AddObject(Forms[i].FormCaption, Forms[i]);
end;

procedure TFormManager.GetSubForms(PFmId: Integer; SL: TStrings);
var
  i: Integer;
begin
  SL.Clear;
  for i := 0 to FormCount - 1 do
    if Forms[i].PId = PFmId then
      SL.AddObject(Forms[i].FormCaption, Forms[i]);
end;

end.

