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

unit MainDaemon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp, mainserver;

type

  { TMainDaemon }

  TMainDaemon = class(TCustomDaemon)
  Private
    FThread : TMainServerThread;
    Procedure ThreadStopped (Sender : TObject);
  public
    Function Start : Boolean; override;
    Function Stop : Boolean; override;
    Function Pause : Boolean; override;
    Function Continue : Boolean; override;
    Function Execute : Boolean; override;
    Function ShutDown : Boolean; override;
    Function Install : Boolean; override;
    Function UnInstall: boolean; override;
  end;

  { TMainDaemonMapper }

  TMainDaemonMapper = class(TCustomDaemonMapper)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  apputils;

{ TMainDaemonMapper }

constructor TMainDaemonMapper.Create(AOwner: TComponent);
var
  D: TDaemonDef;
begin
  inherited Create(AOwner);
  D := DaemonDefs.Add as TDaemonDef;
  D.DisplayName := 'DataExpress Web Server';
  D.Name := 'dxwebsrv';
  D.DaemonClassName := 'TMainDaemon';
  D.WinBindings.ServiceType := stWin32;
end;

{ TMainDaemon }

procedure TMainDaemon.ThreadStopped(Sender: TObject);
begin
  FreeAndNil(FThread);
end;

function TMainDaemon.Start: Boolean;
begin
  Result:=inherited Start;
  LogString('Daemon start');
  FThread := TMainServerThread.Create;
  FThread.OnTerminate := @ThreadStopped;
  FThread.Start;
end;

function TMainDaemon.Stop: Boolean;
begin
  Result:=inherited Stop;
  LogString('Daemon stop');
  FThread.StopServer;
end;

function TMainDaemon.Pause: Boolean;
begin
  Result:=inherited Pause;
  LogString('Daemon pause');
  FThread.Suspended := True;
end;

function TMainDaemon.Continue: Boolean;
begin
  Result:=inherited Continue;
  LogString('Daemon continue');
  FThread.Start;
end;

function TMainDaemon.Execute: Boolean;
begin
  Result:=inherited Execute;
  LogString('Daemon execute');
end;

function TMainDaemon.ShutDown: Boolean;
begin
  Result:=inherited ShutDown;
  LogString('Daemon shutdown');
  FThread.StopServer;
end;

function TMainDaemon.Install: Boolean;
begin
  Result:=inherited Install;
  LogString('Daemon install');
end;

function TMainDaemon.UnInstall: boolean;
begin
  Result:=inherited UnInstall;
  LogString('Daemon uninstall');
end;

end.

