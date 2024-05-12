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

