program dxwebsrv;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  {cmem, }cthreads, cwstring, clocale,
  {$ENDIF}
  Classes, SysUtils, XmlReport, ToWordsFuncs, StrConsts, SqlGen, SAXBaseReader,
  ReportManager, padeg, MainServer, LfmParser, HtmlShow, FormManager,
  FilterParsers, ExprFuncs, Expressions, DXUsers, DxTypes, DXReports, DXCtrls,
  DBEngine, AppUtils, AppSettings, Translations, QSort, TypesWrapper,
  MainDaemon, PivotGrid, DxActions, DXMains, SQLDB, IBConnection, SQLDBLib,
  ScriptManager, DaemonApp, CompilerDecls, MyTypes, RunDecls,
  HttpClient, ScriptFuncs, CSVFiles, DxSQLQuery, myfpsqlparser, myfpsqltree,
  myfpsqlscanner, webtemplates, PascalScriptFCL, LazLogger, MyLogger;

{$R *.res}

var
  FBLoader: TSQLDBLibraryLoader;

begin
  {$IFDEF HeapTrc}
  if FileExists(AppPath + 'heap.trc') then
    DeleteFile(AppPath + 'heap.trc');
  SetHeapTraceOutput(AppPath + 'heap.trc');
  {$ENDIF}

  Randomize;

  MyLog.FileName := AppPath + 'dxwebsrv.log';

  AppSet := TAppSettings.Create;
  AppSet.Load;
  TranslateUnitResourceStrings('strconsts', AppPath + 'languages' +
    DirectorySeparator + 'dxwebsrv.%s.po', AppSet.Language, '');

  FBLoader := TSQLDBLibraryLoader.Create(nil);
  try
    FBLoader.ConnectionType := 'Firebird';
    {$ifdef windows}
    if AppSet.FirebirdVer = '2.5' then
      FBLoader.LibraryName := AppPath + 'fbclientd.dll'
    else if AppSet.FirebirdVer = '5' then
      FBLoader.LibraryName := AppPath + 'fb5\fbclient.dll'
    else
      raise Exception.Create(rsUnknownFBVersion);
    {$else}
    if AppSet.FirebirdVer = '2.5' then
      FBLoader.LibraryName := AppPath + 'libfbclient.so'
    else if AppSet.FirebirdVer = '5' then
      FBLoader.LibraryName := AppPath + 'fb5/libfbclient.so'
    else
      raise Exception.Create(rsUnknownFBVersion);
    {$endif}
    FBLoader.Enabled := True;
  except
    on E: Exception do
    begin
      LogString('FBLoader. ' + E.ClassName + ': ' + E.Message);
      FBLoader.Free;
      AppSet.Free;
      Exit;
    end;
  end;

  with DefaultFormatSettings do
  begin
    ShortTimeFormat:='hh:mm';
    LongTimeFormat:='hh:mm:ss';
    TimeSeparator:=':';
  end;

  {$IFDEF UNIX}
  with DefaultFormatSettings do
  begin
    DateSeparator:='.';
    DecimalSeparator:=',';
    ShortDateFormat:='dd/mm/yyyy';
    ThousandSeparator:=' ';
  end;

  if ParamCount > 0 then
  begin
    RegisterDaemonClass(TMainDaemon);
    RegisterDaemonMapper(TMainDaemonMapper);
    Application.Title:='dxwebsrv';
    try
      Application.Run;
    except
      on E: Exception do
        LogString('Fatal error: ' + E.Message);
    end;
  end
  else
  {$ENDIF}
  begin
    WriteLn(Format(rsAboutTextConsole, [FormatDateTime('yy.m.d', GetBuildDate)]));
    with TMainServerThread.Create do
    try
      Execute;
    finally
      Free;
    end;
  end;

  AppSet.Free;
  FBLoader.Free;
end.

