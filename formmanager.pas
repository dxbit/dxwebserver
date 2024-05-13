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

