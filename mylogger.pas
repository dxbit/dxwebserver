unit MyLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMyLogger }

  TMyLogger = class
  private
    FFileName: String;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteToFile(const S: String);
    property FileName: String read FFileName write FFileName;
  end;

var
  MyLog: TMyLogger;

implementation

{ TMyLogger }

constructor TMyLogger.Create;
begin
  InitCriticalSection(FLock);
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
    Buf := DateTimeToStr(Now) + ' ' + S + LineEnding;
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

initialization
  MyLog := TMyLogger.Create;

finalization
  MyLog.Free;

end.

