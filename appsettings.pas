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

unit AppSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mytypes;

type

  TDBItem = class
  public
    Name, DatabasePath, TemplatesPath, DBPwd: String;
    SessionTime: Integer;
  end;

  { TDBList }

  TDBList = class(TList)
  private
    function GetDBItems(Index: Integer): TDBItem;
  public
    function AddItem: TDBItem;
    function FindItem(const AName: String): TDBItem;
    procedure Clear; override;
    property DBItems[Index: Integer]: TDBItem read GetDBItems; default;
  end;

  { TAppSettings }

  TAppSettings = class
  private
    FCertificate: String;
    FDebugMode: Boolean;
    FFirebirdVer: String;
    FLanguage: String;
    //FPassword: String;
    FPort: Integer;
    FDBList: TDBList;
    FPrivateKey: String;
    FUseSSL: Boolean;
    //function GetLanguageId: Integer;
    //procedure SetLanguageId(AValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    //procedure Save;
    procedure Load;
    property Port: Integer read FPort write FPort;
    //property Password: String read FPassword;
    property Language: String read FLanguage;
    property DebugMode: Boolean read FDebugMode;
    property FirebirdVer: String read FFirebirdVer;
    property UseSSL: Boolean read FUseSSL;
    property PrivateKey: String read FPrivateKey;
    property Certificate: String read FCertificate;
    //property LanguageId: Integer read GetLanguageId write SetLanguageId;
    property DBList: TDBList read FDBList;
  end;

var
  AppSet: TAppSettings;

implementation

uses
  IniFiles, apputils, LazUtf8, dxtypes;

//const
//  Langs: array [0..1] of String = ('', 'ru');

function IncludeTrailingSlash(const S: String): String;
begin
  if (Length(S) > 0) and (S[Length(S)] <> '/') then
    Result := S + '/'
  else
    Result := S;
end;

{ TDBList }

function TDBList.GetDBItems(Index: Integer): TDBItem;
begin
  Result := TDBItem(Items[Index]);
end;

function TDBList.AddItem: TDBItem;
begin
  Result := TDBItem.Create;
  Add(Result);
end;

function TDBList.FindItem(const AName: String): TDBItem;
var
  i: Integer;
  Item: TDBItem;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Item := DBItems[i];
    if MyUtf8CompareText(AName, Item.Name) = 0 then Exit(Item);
  end;
end;

procedure TDBList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    DBItems[i].Free;
  inherited Clear;
end;

{ TAppSettings }

{function TAppSettings.GetLanguageId: Integer;
var
  i: Integer;
begin
  for i := 0 to High(Langs) do
    if FLanguage = Langs[i] then Exit(i);
end;

procedure TAppSettings.SetLanguageId(AValue: Integer);
begin
  FLanguage := Langs[AValue];
end; }

constructor TAppSettings.Create;
begin
  FDBList := TDBList.Create;
end;

destructor TAppSettings.Destroy;
begin
  FDBList.Free;
  inherited Destroy;
end;

{procedure TAppSettings.Save;
begin
  with TIniFile.Create(Utf8ToSys(AppPath + 'dxwebserv.cfg')) do
  try
    CacheUpdates:=True;
    WriteString('Server', 'Language', FLanguage);
    WriteInteger('Server', 'Port', FPort);
    WriteString('Server', 'Password', FPassword);
  finally
    Free;
  end;
end;  }

procedure TAppSettings.Load;
var
  Sections: TStringListUtf8;
  Item: TDBItem;
  Sect: String;
  i: Integer;
begin
  FDBList.Clear;
  Sections := TStringListUtf8.Create;
  with TIniFile.Create(Utf8ToSys(AppPath + 'dxwebsrv.cfg')) do
  try
    FLanguage:=ReadString('Server', 'Language', 'ru');
    FPort := ReadInteger('Server', 'Port', 80);
    FUseSSL := ReadBool('Server', 'UseSSL', False);
    FPrivateKey := GetAbsolutePath( ReadString('Server', 'PrivateKey', '') );
    FCertificate := GetAbsolutePath( ReadString('Server', 'Certificate', '') );
    //FPassword := ReadString('Server', 'Password', '');
    FFirebirdVer := ReadString('Server', 'Firebird', '2.5');
    FDebugMode := ReadBool('Server', 'DebugMode', False);

    ReadSections(Sections);
    for i := 0 to Sections.Count - 1 do
    begin
      Sect := Sections[i];
      if CompareText(Sect, 'Server') <> 0 then
      begin
        Item := FDBList.AddItem;
        Item.Name := Sect;
        Item.DatabasePath := ReadString(Sect, 'Database', '');
        Item.TemplatesPath := ReadString(Sect, 'Templates', '');
        Item.SessionTime := ReadInteger(Sect, 'SessionTime', 0);
        Item.DBPwd := ReadString(Sect, 'DBPwd', '');
      end;
    end;
  finally
    Free;
    Sections.Free;
  end;
end;

end.

