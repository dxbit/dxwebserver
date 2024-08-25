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

unit MyLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMyLogger }

  TMyLogger = class
  private
    FDateTimeLog: Boolean;
    FFileName: String;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteToFile(const S: String);
    property FileName: String read FFileName write FFileName;
    property DataTimeLog: Boolean read FDateTimeLog write FDateTimeLog;
  end;

  { TMyLogMan }

  TMyLogMan = class(TThreadList)
  public
    constructor Create;
    function AddLogger(const FileName: String): TMyLogger;
    function FindLogger(const FileName: String): TMyLogger;
    procedure DeleteLogger(Lg: TMyLogger);
    procedure Clear; reintroduce;
    procedure WriteToFile(const FileName, S: String);
  end;

var
  MyLog: TMyLogger;
  LogMan: TMyLogMan;

implementation

uses
  apputils;

{ TMyLogger }

constructor TMyLogger.Create;
begin
  InitCriticalSection(FLock);
  FDateTimeLog := True;
end;

destructor TMyLogger.Destroy;
begin
  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure TMyLogger.WriteToFile(const S: String);
var
  Mode: Integer;
  Buf: String;
  FS: TFileStream;
begin
  EnterCriticalSection(FLock);
  FS := nil;
  try try

    if not FileExists(FFileName) then
      Mode := fmCreate
    else
      Mode := fmOpenWrite + fmShareDenyNone;

    FS := TFileStream.Create(FFileName, Mode);
    Buf := '';
    if FDateTimeLog then Buf := DateTimeToStr(Now) + ' ';
    Buf := Buf + S + LineEnding;
    FS.Position := FS.Size;
    FS.Write(Pointer(Buf)^, Length(Buf));

  except
    ; // Глушим ошибку.
  end;
  finally
    FreeAndNil(FS);
    LeaveCriticalSection(FLock);
  end;
end;

{ TMyLogMan }

constructor TMyLogMan.Create;
begin
  inherited Create;
  Duplicates := dupAccept;
end;

function TMyLogMan.AddLogger(const FileName: String): TMyLogger;
begin
  Result := FindLogger(FileName);
  if Result = nil then
  begin
    Result := TMyLogger.Create;
    Result.FileName := FileName;
    Result.FDateTimeLog := False;
  end;
  Add(Result);      // Да, да. Так и должно быть.
end;

function TMyLogMan.FindLogger(const FileName: String): TMyLogger;
var
  L: TList;
  Lg: TMyLogger;
  i: Integer;
begin
  Result := nil;
  L := LockList;
  try
    for i := 0 to L.Count - 1 do
    begin
      Lg := TMyLogger(L.Items[i]);
      if MyUtf8CompareText(Lg.FileName, FileName) = 0 then Exit(Lg);
    end;
  finally
    UnlockList;
  end;
end;

procedure TMyLogMan.DeleteLogger(Lg: TMyLogger);
var
  L: TList;
begin
  L := LockList;
  try
    L.Remove(Lg);
    if L.IndexOf(Lg) < 0 then Lg.Free;
  finally
    UnlockList;
  end;
end;

procedure TMyLogMan.Clear;
var
  L: TList;
  Lg: TMyLogger;
begin
  L := LockList;

  try
    while L.Count > 0 do
    begin
      Lg := TMyLogger(L.Items[0]);
      while L.IndexOf(Lg) >= 0 do
        L.Remove(Lg);
      Lg.Free;
    end;
  finally
    UnlockList;
  end;
end;

procedure TMyLogMan.WriteToFile(const FileName, S: String);
var
  Lg: TMyLogger;
begin
  Lg := AddLogger(FileName);
  Lg.WriteToFile(S);
  DeleteLogger(Lg);
end;

initialization
  MyLog := TMyLogger.Create;
  LogMan := TMyLogMan.Create;

finalization
  MyLog.Free;
  LogMan.Free;

end.

