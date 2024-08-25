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

unit MainServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, dxtypes, ssockets, strconsts,
  uPSUtils, Variants, IBConnection, openssl;

{const
  APP_VERSION: String = '22.12.5';}

type

  { TMainServer }

  TMainServer = class(TFPHttpServer)
  private
    FLock: TRTLCriticalSection;
    procedure AcceptIdle(Sender: TObject);
    function ParseQueryFields(Fields: TStrings; Ss: TSession): String;
    //function CheckPwd(UserId: Integer; const aPwd: String): Boolean;
    procedure CleanCacheDir;
  protected
    //function GetSocketHandler(const AUseSSL: Boolean): TSocketHandler; override;
    procedure DoConnect(Sender: TObject; Data: TSocketStream); override;
    procedure HandleRequestError(Sender: TObject; E: Exception); override;
    procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
  public
    Sessions: TSessionList;
    MetaMan: TMetaManager;
    //FPwd: String;
    FirstCleanCacheDir: Boolean;
    CertData, PrivateKeyData: TBytes;
    CertFileAge, PrivateKeyFileAge: LongInt;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TMainServerThread }

  TMainServerThread = class(TThread)
  public
    constructor Create;
    procedure Execute; override;
    procedure StopServer;
  end;

var
  MainSrv: TMainServer;

implementation

uses
  htmlshow, LazUtf8, apputils, DateUtils, appsettings, FileUtil, dxusers, md5,
  scriptmanager, dxctrls, HTTPDefs, base64;


{function LoadCertificate(const FileName: String; out Buf: TBytes): Boolean;
var
  SL: TStringList;
  Base64Str, RawStr: String;
  i: Integer;
begin
  Result := False;
  Base64Str := '';
  SL := TStringList.Create;
  try try
    SL.LoadFromFile(FileName);
    for i := 1 to SL.Count - 2 do
      Base64Str := Base64Str + SL[i];
    RawStr := DecodeStringBase64(Base64Str);
    SetLength(Buf, Length(RawStr));
    Move(RawStr[1], Buf[0], Length(RawStr));
    Result := True;
  except
    on E: Exception do
      LogString('LoadCertificate. ' + E.ClassName + ': ' + E.Message);
  end;
  finally
    SL.Free;
  end;
end; }

{function LoadCertificate(const FileName: String; out Buf: TBytes): Boolean;
var
  SL: TStringList;
  Base64Str, RawStr, S: String;
  i, RawStrLen, BufSize: Integer;
  InCert: Boolean;
begin
  Result := False;
  i := 0;
  BufSize := 0;
  InCert := False;

  SL := TStringList.Create;
  try try
    SL.LoadFromFile(FileName);

    for i := 0 to SL.Count - 1 do
    begin
      S := SL[i];
      if (S <> '') and (S[1] = '-') then
      begin
        if not InCert then
        begin
          InCert := True;
          Base64Str := '';
        end
        else
        begin
          RawStr := DecodeStringBase64(Base64Str);
          RawStrLen := Length(RawStr);
          SetLength(Buf, RawStrLen + BufSize);
          Move(RawStr[1], Buf[BufSize], RawStrLen);
          BufSize := BufSize + RawStrLen;
          InCert := False;
        end;
      end
      else
        Base64Str := Base64Str + S;
    end;
    Result := True;
  except
    on E: Exception do
      LogString('LoadCertificate. ' + E.ClassName + ': ' + E.Message);
  end;
  finally
    SL.Free;
  end;
end;   }

{ TMainServerThread }

constructor TMainServerThread.Create;
begin
  inherited Create(True);
end;

procedure TMainServerThread.Execute;
//var
//  LibUtilSSLFile, LibSSLFile: String;
begin
  try try
    MainSrv := TMainServer.Create(nil);
    MainSrv.ServerBanner := 'DataExpress Web Server';
    MainSrv.LookupHostNames := False;
    MainSrv.Port := AppSet.Port;
    MainSrv.Threaded := True;
    MainSrv.AcceptIdleTimeout := 1000;
    if AppSet.UseSSL then
    begin
      if not FileExists(AppSet.Certificate) then
        LogString('Certificate not found: ' + AppSet.Certificate)
      else if not FileExists(AppSet.PrivateKey) then
        LogString('Private key not found: ' + AppSet.PrivateKey)
      else
        MainSrv.UseSSL := True;
    end;
    {MainSrv.UseSSL := AppSet.UseSSL and FileExists(AppSet.PrivateKey) and
      FileExists(AppSet.Certificate); }

    if MainSrv.UseSSL then
    begin
      MainSrv.CertificateData.Certificate.FileName := AppSet.Certificate;
      MainSrv.CertificateData.PrivateKey.FileName := AppSet.PrivateKey;
    end;

    (*{$IFDEF LINUX}
    LibUtilSSLFile := '';// AppPath + 'libcrypto.so';
    LibSSLFile := ''; //AppPath + 'libssl.so';
    {$ELSE}
    LibUtilSSLFile := '';
    LibSSLFile := '';
    {$ENDIF}  *)
    if MainSrv.UseSSL then
      if not InitSSLInterface{(LibUtilSSLFile, LibSSLFile)} then
        LogString('Can not load ssl library.');
    MainSrv.Active := True;
  except
    on E: Exception do
      LogString('MainServer error: ' + E.Message);
  end;
  finally
    MainSrv.Free;
  end;
end;

procedure TMainServerThread.StopServer;
begin
  MainSrv.Active := False;
end;

{ TMainServer }

function TMainServer.ParseQueryFields(Fields: TStrings; Ss: TSession): String;
var
  Tmp, n: integer;
begin
  SS.Clear;
  Result := '';
  if Fields.Count = 0 then Exit;
  n := 1;
  if Fields.Names[0] = 'fm' then
  begin
    if not TryStrToInt(Fields.ValueFromIndex[0], Tmp) then Exit;
    Ss.FormId := Tmp;
    if Fields.Count > 1 then
    begin
      n := 2;
      if Fields.Names[1] = 'pg' then
      begin
        if (not TryStrToInt(Fields.ValueFromIndex[1], Tmp)) or (Tmp < 1) then Exit;
        Ss.Page := Tmp;
        if Fields.Count > 2 then
        begin
          n := 3;
          if Fields[2] = 'sort' then
          else Exit;
        end;
      end
      else if Fields[1] = 'sort' then
      else if Fields.Names[1] = 'rec' then
      begin
        if not TryStrToInt(Fields.ValueFromIndex[1], Tmp) then Exit;
        Ss.RecId := Tmp;
        if Fields.Count > 2 then
        begin
          n := 3;
          if Fields.Names[2] = 'tbl' then
          begin
            if not TryStrToInt(Fields.ValueFromIndex[2], Tmp) then Exit;
            Ss.TableId := Tmp;
            if Fields.Count > 3 then
            begin
              n := 4;
              if Fields.Names[3] = 'row' then
              begin
                if not TryStrToInt(Fields.ValueFromIndex[3], Tmp) then Exit;
                Ss.TableRecId := Tmp;
                if Fields.Count > 4 then
                begin
                  n := 5;
                  if (Fields.Names[4] = 'imgupfm') or (Fields.Names[4] = 'imgvw') or
                    (Fields.Names[4] = 'imgclr') or (Fields.Names[4] = 'imgup') then
                  begin
                    if not TryStrToInt(Fields.ValueFromIndex[4], Tmp) then Exit;
                    Ss.CId := Tmp;
                  end
                  else if (Fields.Names[4] = 'flupfm') or (Fields.Names[4] = 'flup') or
                    (Fields.Names[4] = 'fldl') or (Fields.Names[4] = 'flclr') then
                  begin
                    if not TryStrToInt(Fields.ValueFromIndex[4], Tmp) then Exit;
                    Ss.CId := Tmp;
                  end
                  else if Fields[4] = 'del' then
                  else if Fields[4] = 'post' then
                  else if Fields[4] = 'cancel' then
                  else if Fields[4] = 'fieldchange' then
                  else if Fields[4] = 'queryscroll' then
                  else if Fields[4] = 'getlist' then
                  else if Fields[4] = 'sort' then
                  else if Fields[4] = 'setpage' then
                  else if Fields[4] = 'queryadd' then
                  else if Fields[4] = 'queryedit' then
                  else if Fields[4] = 'querydel' then
                  else if Fields[4] = 'queryfetch' then
                  else if Fields[4] = 'dup' then
                  else if Fields[4] = 'dupall' then
                  else if Fields[4] = 'bnclick' then
                  else if Fields[4] = 'msgbnclick' then
                  else if Fields[4] = 'objadd' then
                  else if Fields[4] = 'objedit' then
                  else Exit;
                end;
              end
              else Exit;
            end;
          end
          else if (Fields.Names[2] = 'imgupfm') or (Fields.Names[2] = 'imgvw') or
            (Fields.Names[2] = 'imgclr') or (Fields.Names[2] = 'imgup') then
          begin
            if not TryStrToInt(Fields.ValueFromIndex[2], Tmp) then Exit;
            Ss.CId := Tmp;
          end
          else if (Fields.Names[2] = 'flupfm') or (Fields.Names[2] = 'flup') or
            (Fields.Names[2] = 'fldl') or (Fields.Names[2] = 'flclr') then
          begin
            if not TryStrToInt(Fields.ValueFromIndex[2], Tmp) then Exit;
            Ss.CId := Tmp;
          end
          else if Fields[2] = 'del' then
          else if Fields[2] = 'formdel' then
          else if Fields[2] = 'post' then
          else if Fields[2] = 'cancel' then
          else if Fields.Names[2] = 'print' then
          begin
            if not TryStrToInt(Fields.ValueFromIndex[2], Tmp) then Exit;
            Ss.TemplateId := Tmp;
          end
          else if Fields[2] = 'fieldchange' then
          else if Fields[2] = 'queryscroll' then
          else if Fields[2] = 'getlist' then
          else if Fields[2] = 'sort' then
          else if Fields[2] = 'setpage' then
          else if Fields[2] = 'queryadd' then
          else if Fields[2] = 'queryedit' then
          else if Fields[2] = 'querydel' then
          else if Fields[2] = 'queryfetch' then
          else if Fields[2] = 'dup' then
          else if Fields[2] = 'dupall' then
          else if Fields[2] = 'bnclick' then
          else if Fields[2] = 'msgbnclick' then
          else if Fields[2] = 'tableadd' then
          else if Fields[2] = 'tableedit' then
          else if Fields[2] = 'tabledel' then
          else if Fields[2] = 'tablescroll' then
          else if Fields[2] = 'tablefetch' then
          else if Fields[2] = 'objadd' then
          else if Fields[2] = 'objedit' then
          else Exit;
        end;
      end
      else if Fields.Names[1] = 'flt' then
      begin
        if not TryStrToInt(Fields.ValueFromIndex[1], Tmp) then Exit;
        Ss.FilterId:=Tmp;
        if Fields.Count > 2 then
        begin
          n := 3;
          if Fields[2] = 'getlist' then
          else if Fields[2] = 'fltok' then
          else Exit;
        end;
      end
      else if Fields.Names[1] = 'fltpr' then
      begin
        if not TryStrToInt(Fields.ValueFromIndex[1], Tmp) then Exit;
        Ss.FilterPrId:=Tmp;
      end
      else if Fields[1] = 'fltclr' then
      else if Fields[1] = 'sort' then
      else Exit;
    end;
  end
  else if Fields.Names[0] = 'rp' then
  begin
    if not TryStrToInt(Fields.ValueFromIndex[0], Tmp) then Exit;
    Ss.RpId := Tmp;
    if Fields.Count > 1 then
    begin
      n := 2;
      if Fields[1] = 'exec' then
      else if Fields[1] = 'getlist' then
      else if Fields.Names[1] = 'print' then
      begin
        if not TryStrToInt(Fields.ValueFromIndex[1], Tmp) then Exit;
        Ss.TemplateId := Tmp;
      end
      else if Fields[1] = 'sort' then
      else if Fields[1] = 'rpfetch' then
      else Exit;
    end;
  end
  else if Fields[0] = 'formadd' then
  else if Fields[0] = 'usermon' then
  else if Fields[0] = 'showdbg' then
  else if Fields[0] = 'closedbg' then
  else if Fields[0] = 'cleardbg' then
  else if Fields[0] = 'compileerror' then
  else if Fields[0] = 'runtimeerror' then
  begin
  end;
  if Fields.Count > n then Exit;
  Result := Fields.Names[n - 1];
  if Result = '' then Result := Fields[n - 1];
end;

procedure TMainServer.AcceptIdle(Sender: TObject);
var
  i: Integer;
  SS: TSession;
  MD: TMetaData;
begin
  if not FirstCleanCacheDir then CleanCacheDir;

  //DebugStr('AcceptIdle Sessions.Lock');
  Sessions.Lock;

  try try
    for i := Sessions.Count - 1 downto 0 do
    begin
      SS := Sessions[i];
      if not SS.Busy and (SS.DBItem.SessionTime > 0) and
        (MinutesBetween(Now, SS.LastTime) > SS.DBItem.SessionTime) then
      begin
        DebugStr('Autoremove session start.', SS);
        MD := SS.MetaData;
        Sessions.DeleteSession(SS);
        MetaMan.DeleteMetaData(MD);
        DebugStr('Autoremove session complete. Session count: ' + IntToStr(Sessions.Count));
      end;
    end;

  except
    on E: Exception do
      LogString('AcceptIdle exception: ' + E.Message);
  end;
  finally
    //DebugStr('AcceptIdle Sessions.Unlock');
    Sessions.Unlock;
  end;
end;

{function TMainServer.CheckPwd(UserId: Integer; const aPwd: String): Boolean;
var
  U: TdxUser;
begin
  if UserMan.Users.Count = 0 then
  begin
    Result := aPwd = FPwd;
  end
  else
  begin
    U := UserMan.Users.FindUser(UserId);
    if U <> nil then
      Result := MD5Print(MD5String(aPwd)) = U.Password
    else Result := False;
  end;
end;}

procedure TMainServer.DoConnect(Sender: TObject; Data: TSocketStream);
begin
  Data.IOTimeout:=5000;
  inherited DoConnect(Sender, Data);
end;

procedure TMainServer.HandleRequestError(Sender: TObject; E: Exception);
begin
  inherited HandleRequestError(Sender, E);
  (*if (E is EHttpServer) { and (E.Message = 'Error reading data from the socket')} then
  else if (E is EHTTP) and (E.Message = 'No REQUEST_METHOD passed from server.') then
  else*) LogString('HandleRequestError. ' + E.ClassName + ': ' + E.Message);        // LogError
end;

constructor TMainServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MetaMan := TMetaManager.Create;
  Sessions := TSessionList.Create;
  OnAcceptIdle :=@AcceptIdle;
  InitCriticalSection(FLock);
  QueueSize := 100;
end;

destructor TMainServer.Destroy;
begin
  DoneCriticalSection(FLock);
  Sessions.Free;
  MetaMan.Free;
  SetLength(CertData, 0);
  SetLength(PrivateKeyData, 0);
  inherited Destroy;
end;

procedure TMainServer.CleanCacheDir;
var
  CacheDir: String;
begin
  EnterCriticalSection(FLock);
  CacheDir := AppPath + 'cache' + DirectorySeparator;
  try
    DeleteDirectory(CacheDir, True);
  finally
    FirstCleanCacheDir:=True;
    LeaveCriticalSection(FLock);
  end;
end;

{function TMainServer.GetSocketHandler(const AUseSSL: Boolean): TSocketHandler;
var
  FlAge: LongInt;
begin
  Result := inherited GetSocketHandler(AUseSSL);
  if Result is TSSLSocketHandler then
    with TSSLSocketHandler(Result).CertificateData do
    begin
      FlAge := FileAge(AppSet.Certificate);
      if (FlAge > 0) and (FlAge <> CertFileAge) then
      begin
        if LoadCertificate(AppSet.Certificate, CertData) then
          CertFileAge := FlAge;
      end;
      FlAge := FileAge(AppSet.PrivateKey);
      if (FlAge > 0) and (FlAge <> PrivateKeyFileAge) then
      begin
        if LoadCertificate(AppSet.PrivateKey, PrivateKeyData) then
          PrivateKeyFileAge := FlAge;
      end;
      Certificate.Value := CertData;
      PrivateKey.Value := PrivateKeyData;
    end;
end;  }

function GetMimeType(const Ext: String): String;
begin
  Result := '';
  if Ext = '.svg' then
    Result := 'image/svg+xml'
  else if Ext = '.css' then
    Result := 'text/css;charset=UTF-8'
  else if Ext = '.js' then
    Result := 'text/javascript;charset=UTF-8'
  else if (Ext = '.docx') or (Ext = '.xml') then
    Result := 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
  else if Ext = '.odt' then
    Result := 'application/vnd.oasis.opendocument.text'
  else if Ext = '.ods' then
    Result := 'application/vnd.oasis.opendocument.spreadsheet'
  else if Ext = '.html' then
    Result := 'text/html;charset=UTF-8'
  else if Ext = '.pdf' then
    Result := 'application/pdf'
  else if Ext = '.zip' then
    Result := 'application/zip'
  else if Ext = '.ico' then
    Result := 'image/vnd.microsoft.icon'
  else if Ext = '.png' then
    Result := 'image/png'
  else if Ext = '.gif' then
    Result := 'image/gif'
  else if (Ext = '.jpg') or (Ext = '.jpeg') then
    Result := 'image/jpeg'
  else if Ext = '.csv' then
    Result := 'text/csv;charset=UTF-8'
  else if Ext = '.json' then
    Result := 'application/json;charset=UTF-8'
  else if Ext = '.xml' then
    Result := 'application/xml;charset=UTF-8'
  else if Ext = '.txt' then
    Result := 'text/plain;charset=UTF-8'
  else
    Result := 'application/' + Copy(Ext, 2, 255);
end;

function ExtractConnectName(const Path: String; out ConnectName: String): Boolean;
var
  i: Integer;
begin
  ConnectName := '';
  Result := False;
  if (Path <> '') and (Path[Length(Path)] = '/') then
  begin
    for i := 2 to Length(Path) - 1 do
      if not (Path[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) then Exit;
    ConnectName := LowerCase(Copy(Path, 2, Length(Path) - 2));
    Result := True;
  end;
end;

function CleanURI(QueryFields: TStrings): String;
begin
  Result := '';
  if (QueryFields.Count > 0) and ((QueryFields.Names[0] = 'fm') or
    (QueryFields.Names[0] = 'rp') or (QueryFields[0] = 'usermon')) then
  begin
    Result := '?' + QueryFields[0];
    if (QueryFields.Count > 1) and ((QueryFields.Names[1] = 'rec') or
      (QueryFields.Names[1] = 'flt')) then
      Result := Result + '&' + QueryFields[1];
  end;
end;

procedure TMainServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  Ss: TSession;
  HS: THtmlShow;
  FlNm, LPm, User, Pwd, URI, ConnectName, StartUrl, Path,
    BrowserId, FlExt, S: String;
  FS: TFileStream;
  UId: Integer;
  MetaLastModified: TDateTime;
  MD: TMetaData;
  U: TdxUser;
  HandleOk, IsService: Boolean;
  DBItem: TDBItem;

  procedure RunMainAction(const ActionData: String);
  var
    DummyRS: TSsRecordSet;
    DummyFm: TdxForm;
  begin
    DummyRS := TSsRecordSet.Create(SS, nil);
    DummyFm := TdxForm.Create(nil);
    DummyFm.Id := DummyForm;
    DummyRS.Form := DummyFm;
    try
      RunAction(ActionData, DummyRS);
    finally
      DummyRS.Free;
      DummyFm.Free;
    end;
  end;

  function CheckPwd(const aUser, aPwd: String): Integer;
  var
    U: TdxUser;
  begin
    if SS.UserMan.Users.Count = 0 then
      Result := -1
    else
    begin
      U := SS.UserMan.Users.FindUserByName(aUser);
      if (U <> nil) and (MD5Print(MD5String(aPwd)) = U.Password) then
        Result := U.Id
      else
        Result := -2;
    end;
  end;

begin
  DebugStr(ARequest.RemoteAddress + ' ' + ARequest.Method + ': ' + ARequest.URI);
  if not FirstCleanCacheDir then CleanCacheDir;

  SS := nil;
  HS := THtmlShow.Create;
  URI := ARequest.URI;
  Path := ARequest.PathInfo;

  try try

  //ShowMessage(ARequest.URI);
  if (Pos('/img/', URI) = 1) or (Pos('/html/', URI) = 1) or (Pos('/cache/', URI) = 1)
    or (URI = '/favicon.ico') then
  begin
    FlNm := DecodeUrlElement(Path);
    FlExt := AnsiLowerCase(ExtractFileExt(Path));
    Delete(FlNm, 1, 1);
    FlNm := AppPath + StringReplace(FlNm, '/', DirectorySeparator, [rfReplaceAll]);
    // ARequest.IfNoneMatch пустой.
    AResponse.ContentType := GetMimeType(FlExt);
    if ARequest.CustomHeaders.Values['if-none-match'] = IntToStr(FileAge(FlNm)) then
    begin
      AResponse.Code:=304;
      Exit;
    end;
    FS := TFileStream.Create(FlNm, fmOpenRead);
    AResponse.FreeContentStream:=True;
    AResponse.ContentStream := FS;
    if Pos('/cache/', URI) = 0 then
    begin
      //AResponse.CacheControl := 'public, max-age=31536000'
      AResponse.ETag := IntToStr(FileAge(FlNm));
    end
    else AResponse.CacheControl := 'no-store, no-cache, must-revalidate';
    Exit;
  end
  else if Pos('/.well-known/acme-challenge/', URI) = 1 then
  begin
    FlNm := StringReplace(DecodeUrlElement(Path), '/', PathDelim, [rfReplaceAll]);
    Delete(FlNm, 1, 1);
    FlNm := AppPath + 'letsencrypt' + PathDelim + FlNm;
    if FileExists(FlNm) then
    begin
      FS := TFileStream.Create(FlNm, fmOpenRead + fmShareDenyNone);
      AResponse.FreeContentStream := True;
      AResponse.ContentStream := FS;
      LogString('Let''s Encrypt verify: ' + FlNm);
    end
    else
      AResponse.Code := rcPageNotFound;
    Exit;
  end;

  DebugStr(ARequest.RemoteAddress + ': ' + ARequest.URI);

  if URI = '/' then
  begin
    AResponse.Content := HS.ShowIndexPage;
    Exit;
  end;

  {$IFDEF HeapTrc}
  if URI = '/exit' then
  begin
    Active:=False;
    Exit;
  end;
  {$ENDIF}

  if Path = '' then
  begin
    AResponse.Content := HS.ShowError2Page(rsError, rsNoConnectionName);
    Exit;
  end
  else if Copy(Path, Length(Path), 1) <> '/' then
  begin
    AResponse.Contents.Text := '<html><head><meta http-equiv=refresh content="1;url=' +
      Path + '/' + IIF(ARequest.QueryString <> '', '?' + ARequest.QueryString, '') +
      '"></head><body></body></html>';
    Exit;
  end
  else if not ExtractConnectName(Path, ConnectName) then
  begin
    AResponse.Content := HS.ShowError2Page(rsError, Format(rsInvalidConnectionName, [Path]));
    Exit;
  end
  else if AppSet.DBList.FindItem(ConnectName) = nil then
  begin
    AResponse.Content := HS.ShowError2Page(rsError, Format(rsConnectionNotFound, [ConnectName]));
    Exit;
  end;

  StartUrl := ARequest.CookieFields.Values[ConnectName + '_starturl'];
  BrowserId := ARequest.CookieFields.Values[ConnectName + '_browserid'];
  if BrowserId = '' then
  begin
    BrowserId := GenerateId;
    with AResponse.Cookies.Add do
    begin
      Name := ConnectName + '_browserid'; Value := BrowserId;
    end;
  end;

  DebugStr('Check timeout Sessions.Lock');
  Sessions.Lock;
  try
    SS := Sessions.FindSessionByBrowserId(BrowserId, ConnectName);
    if SS <> nil then
    begin
      if (SS.MetaData <> nil) and SS.MetaData.LoadComplete and
        (SS.DBItem.SessionTime > 0) and
        (MinutesBetween(Now, SS.LastTime) > SS.DBItem.SessionTime) then
      begin
        MD := SS.MetaData;
        Sessions.DeleteSession(SS);
        MetaMan.DeleteMetaData(MD);
        DebugStr('Session timeout. Session count: ' + IntToStr(Sessions.Count));
      end
      else if (SS.MetaData = nil) or not SS.MetaData.LoadComplete then
      begin
        AResponse.Contents.Text := HS.ShowConnectionProgress(SS.GetCurrentUser);//  ShowError2Page(rsConnectToDB, rsConnectionInProgress);
        //Sessions.Unlock;
        SS := nil;
        Exit;
      end;
    end;
  finally
    DebugStr('Check timeout Sessions.UnLock');
    Sessions.Unlock;
  end;

  //DebugStr(IntToStr(Sessions.Count));
  IsService := False;
  if SS = nil then
  begin
    if (ARequest.QueryFields.Count > 0) and (ARequest.QueryFields.Names[0] = 'serviceid') then
    begin
      DBItem := AppSet.DBList.FindItem(ConnectName);
      if ARequest.QueryFields.ValueFromIndex[0] = DBItem.ServiceId then
      begin
        IsService := True;
        // Если нет параметров, то возвращаем ok
        if (ARequest.QueryFields.Count = 1) and (ARequest.ContentFields.Count = 0)
          and (ARequest.Files.Count = 0) and (ARequest.Content = '') then
        begin
          AResponse.Code := 200;
          AResponse.Content := 'I am ready!';
          Exit;
        end;
      end
      else
      begin
        AResponse.Code := 401;
        AResponse.Content := 'Unauthorized';
        Exit;
      end;
    end;

    if (ARequest.Query = 'login') or IsService then
    begin
      if not IsService then
      begin
        AResponse.Code := rcAjaxError;

        User := ARequest.ContentFields.Values['user'];
        Pwd := ARequest.ContentFields.Values['pwd'];
      end;

      DebugStr('SS = nil Sessions.Lock');
      Sessions.Lock;
      try
        SS := TSession.Create;
        SS.ConnectName := ConnectName;
        SS.BrowserId := BrowserId;
        Sessions.Add(SS);
      finally
        DebugStr('SS = nil Sessions.Unlock');
        Sessions.Unlock;
      end;

      DebugStr('New session SS.LockSession Before');
      SS.LockSession;  // Чтобы AcceptIdle не удалил сессию
      DebugStr('New session SS.LockSession After');
      HS.Session := SS;

      MD := nil;
      try
        DebugStr('ConnectDB', SS);
        SS.ConnectDB(ConnectName);
        MetaLastModified := SS.DBase.GetMetaLastModified;
        DebugStr('ConnectDB MetaMan.Lock', SS);
        MetaMan.Lock;
        try
          MD := MetaMan.FindMetaData(SS.DBase.Database, MetaLastModified);
          if MD = nil then
          begin
            if SS.DBItem.KeepMetaData then
              MetaMan.DeleteOldMetaData(SS.DBase.Database);

            MD := TMetaData.Create;
            MD.Database := SS.DBase.Database;
            MD.LastModified := MetaLastModified;
            MD.ConnectName := ConnectName;
            MD.KeepMetaData := SS.DBItem.KeepMetaData;
            MetaMan.Add(MD);
            DebugStr('Add metadata. Count: ' + IntToStr(MetaMan.Count), SS);
          end
          else
          begin
            MD.IncRef;
          end;
        finally
          DebugStr('ConnectDB MetaMan.UnLock', SS);
          MetaMan.UnLock;
        end;

        SS.MetaData := MD;

        try
          DebugStr('LoadUsers MD.Lock', SS);
          MD.Lock;
          if not MD.UsersLoaded then
          begin
            MD.LoadUsers(SS.DBase);
            MD.UsersLoaded := True;
          end;
        finally
          DebugStr('LoadUsers MD.UnLock', SS);
          MD.Unlock;
        end;

        if not IsService then
          UId := CheckPwd(User, Pwd)
        else
          UId := -1;

        if UId >= -1 then
        begin
          SS.UserId := UId;
          U := SS.UserMan.Users.FindUser(UId);
          if U <> nil then
            SS.RoleId := U.RoleId
          else
            SS.RoleId := -1;

          DebugStr('Load meta MD.Lock', SS);
          MD.Lock;
          try
            if not MD.LoadComplete then
            begin
              MD.LoadMain(SS.DBase);
              MD.LoadForms(SS.DBase);
              MD.LoadReports(SS.DBase);
              MD.LoadImages(SS.DBase);
              MD.LoadScripts(SS.DBase);
              MD.ScriptMan.CompileAll;
              MD.LoadComplete := True;
            end;
          finally
            DebugStr('Load meta MD.UnLock', SS);
            MD.Unlock;
          end;

          SS.IP := ARequest.RemoteAddr;
          SS.LastTime := Now;

          if not IsService then
          begin
            if SS.MainErrorMsg <> '' then StartUrl := '?runtimeerror'
            else if MD.ScriptMan.HasErrors then StartUrl := '?compileerror'
            else if StartUrl = '' then StartUrl := HS.GetFirstForm;
            AResponse.Contents.Text := MakeJsonObjectString(['url', StartUrl]);
          end;

          if not DirectoryExists(GetCachePath(SS)) then
          begin
            if not ForceDirectories(GetCachePath(SS)) then
              DebugStr('Failed to create session directory ' + GetCachePath(SS));
          end;

          if not MD.ScriptMan.HasErrors then
          begin
            SS.ExtRunMan.Init;
            SS.RunScript.SD := MD.ScriptMan.FindScriptByName('WebMain');
            SS.RunScript.LoadBin;
            SS.RunScript.BindVars;
            SS.Request := ARequest;
            SS.IsService := IsService;
            try
              SS.RunScript.TryRunProc('DATABASE_OPEN', []);
              RunMainAction(MD.Main.Actions);
            except
              on E: Exception do
                SS.MainErrorMsg := ExceptionToHtml(E);
            end;
          end;

          DebugStr('Login [' + SS.GetCurrentUser + '] complete. Session count: ' + IntToStr(Sessions.Count), SS);

          if IsService then
          begin
            if SS.OnHandleRequest <> nil then
              SS.OnHandleRequest(SS, ARequest, AResponse);

            try
              if SS.OnDatabaseClose <> nil then SS.OnDatabaseClose(SS);
              SS.RunScript.TryRunProc('DATABASE_CLOSE', []);
            except
              ;
            end;

            SS.UnlockSession;
            Sessions.Lock;
            try
              Sessions.DeleteSession(SS);
              MetaMan.DeleteMetaData(MD);
            finally
              Sessions.Unlock;
            end;
          end
          else
            AResponse.Code := rcAjaxOk;
        end
        else
        begin
          DebugStr('Login failed Sessions.Lock', SS);
          Sessions.Lock;
          try
            Sessions.DeleteSession(SS);
            MetaMan.DeleteMetaData(MD);
          finally
            DebugStr('Login failed Sessions.UnLock');
            Sessions.Unlock;
          end;

          AResponse.Contents.Text := MakeJsonErrString(rcLoginFailed,
            rsIncorrectUserOrPwd);
          DebugStr('Login failed.');
        end;
      except
        on E: Exception do
        begin
          if E is EIBDatabaseError then S := WinCPToUtf8(E.Message)
          else S := E.Message;
          AResponse.Content := MakeJsonErrString(rcConnectError, S);
          AResponse.ContentType := GetMimeType('.json');

          DebugStr('Connect error. ' + E.ClassName + ': ' + E.Message, SS);
          DebugStr('Connect exception Sessions.Lock', SS);
          Sessions.Lock;
          try
            Sessions.DeleteSession(SS);
            if MD <> nil then MetaMan.DeleteMetaData(MD);
          finally
            DebugStr('Connect exception Sessions.UnLock');
            Sessions.Unlock;
          end;
          Exit;
        end;
      end;
    end
    else if ARequest.Method = 'POST' then
    begin
      AResponse.Code:=401;
      Exit;
    end
    else if URI <> '/favicon.ico' then
    begin
      with AResponse.Cookies.Add do
      begin
        Name := ConnectName + '_starturl'; Value := CleanURI(ARequest.QueryFields);
      end;
      AResponse.Contents.Text := HS.ShowLoginUser;
      AResponse.CacheControl := 'no-store, no-cache, must-revalidate';
    end;
    Exit;
  end;
  DebugStr(ARequest.RemoteAddress + ': ' + ARequest.URI, SS);
  DebugStr('Session exists SS.LockSession Before', SS);
  SS.LockSession;
  DebugStr('Session exists SS.LockSession After', SS);
  SS.LastTime:=Now;
  HS.Session := SS;
  SS.Request := ARequest;

  AResponse.CacheControl := 'no-store, no-cache, must-revalidate';

  if ARequest.QueryFields.Count = 0 then
  begin
    AResponse.Contents.Text := HS.ShowFirstForm;
    Exit;
  end;
  LPm := ParseQueryFields(ARequest.QueryFields, Ss);
  if (LPm = 'fm') or (LPm = 'pg') then
  begin
    AResponse.Contents.Text := HS.ShowForm;
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'formadd' then
  begin
    AResponse.Contents.Text := HS.FormAppend(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if (LPm = 'rec') or (LPm = 'row') then
  begin
    AResponse.Contents.Text:=HS.ShowEditForm;
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'tableadd' then
  begin
    AResponse.Contents.Text:=HS.TableAppend(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'tableedit' then
  begin
    AResponse.Contents.Text:=HS.TableEdit(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'tabledel' then
  begin
    AResponse.Contents.Text:=HS.TableDeleteRow(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'tablescroll' then
  begin
    AResponse.Contents.Text:=HS.TableScroll(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'tablefetch' then
  begin
    AResponse.Contents.Text:=HS.TableFetch(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'post' then
  begin
    AResponse.Contents.Text:=HS.PostEditForm(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'imgup' then
  begin
    if ARequest.Files.Count <> 1 then Exit;
    with ARequest.Files[0] do
      AResponse.Contents.Text:=HS.UploadImage(ARequest.ContentFields, FileName, Stream);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'imgvw' then
    AResponse.Contents.Text:=HS.ViewImage
  else if LPm = 'imgclr' then
  begin
    AResponse.Contents.Text:=HS.ClearImage(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'flup' then
  begin
    if ARequest.Files.Count <> 1 then Exit;
    with ARequest.Files[0] do
      AResponse.Contents.Text:=HS.UploadFile(ARequest.ContentFields, FileName, Stream);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'fldl' then
  begin
    AResponse.Contents.Text:=HS.DownloadFile(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'flclr' then
  begin
    AResponse.Contents.Text:=HS.ClearFile(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'del' then
  begin
    AResponse.Contents.Text := HS.DeleteRecord(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'formdel' then
  begin
    AResponse.Contents.Text := HS.FormDeleteRow;
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'flt' then
  begin
    AResponse.Contents.Text := HS.ShowFilter;
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'fltok' then
  begin
    AResponse.Contents.Text := HS.ApplyFilter(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'fltpr' then
  begin
    AResponse.Contents.Text := HS.ApplyFilterPreset;
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'fltclr' then
  begin
    AResponse.Contents.Text := HS.ClearAllFilters;
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'print' then
  begin
    if SS.GetFmId > 0 then
      AResponse.Contents.Text := HS.PrintForm(ARequest.ContentFields)
    else if SS.RpId > 0 then
      AResponse.Contents.Text := HS.PrintReport;
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'rp' then
  begin
    AResponse.Contents.Text := HS.ShowReport;
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'rpfetch' then
  begin
    AResponse.Contents.Text := HS.ReportFetch(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'exec' then
  begin
    AResponse.Contents.Text := HS.ApplyReportFilter(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'sort' then
  begin
    if SS.RecId > 0 then
      AResponse.Contents.Text := HS.ApplyQrySort(ARequest.ContentFields)
    else if SS.RpId > 0 then
      AResponse.Contents.Text := HS.ApplyRpSort(ARequest.ContentFields)
    else if SS.FormId > 0 then
      AResponse.Contents.Text := HS.ApplySort(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'logout' then
  begin
    //Sessions.Lock;
    DebugStr('Logout begin', SS);
    try
      if SS.OnDatabaseClose <> nil then SS.OnDatabaseClose(SS);
      SS.RunScript.TryRunProc('DATABASE_CLOSE', []);
    except
      ;
    end;

    DebugStr('Logout Sessions.Lock', SS);
    Sessions.Lock;
    try
      MD := SS.MetaData;
      Sessions.DeleteSession(SS);
      MetaMan.DeleteMetaData(MD);
    finally
      DebugStr('Logout Sessions.UnLock');
      Sessions.Unlock;
    end;
    AResponse.Contents.Text := '<html><head><meta http-equiv=refresh content="1;url=/' + ConnectName + '/"></head><body></body></html>';
    DebugStr('Logout complete. Session count: ' + IntToStr(Sessions.Count));
  end
  else if LPm = 'fieldchange' then
  begin
    AResponse.Contents.Text := HS.FieldChange(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'queryscroll' then
  begin
    AResponse.Contents.Text := HS.QueryScroll(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'getlist' then
  begin
    if SS.RecId > 0 then
      AResponse.Contents.Text := HS.GetList(ARequest.ContentFields)
    else if SS.FilterId > 0 then
      AResponse.Contents.Text := HS.GetFilterList(ARequest.ContentFields)
    else if SS.RpId > 0 then
      AResponse.Contents.Text := HS.GetRpFilterList(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'cancel' then
  begin
    AResponse.Contents.Text := HS.CancelChanges;
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'setpage' then
  begin
    AResponse.Contents.Text := HS.SetActivePage(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'queryadd' then
  begin
    AResponse.Contents.Text := HS.AppendQueryRecord(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'queryedit' then
  begin
    AResponse.Contents.Text := HS.QueryEdit(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'querydel' then
  begin
    AResponse.Contents.Text := HS.QueryDelRow(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'queryfetch' then
  begin
    AResponse.Contents.Text := HS.QueryFetch(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'objadd' then
  begin
    AResponse.Contents.Text := HS.ObjectAppend(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'objedit' then
  begin
    AResponse.Contents.Text := HS.ObjectEdit(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'dup' then
  begin
    AResponse.Contents.Text := HS.DuplicateRecord(ARequest.ContentFields, dpOne);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'dupall' then
  begin
    AResponse.Contents.Text := HS.DuplicateRecord(ARequest.ContentFields, dpAll);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'bnclick' then
  begin
    AResponse.Contents.Text := HS.BnClick(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'msgbnclick' then
  begin
    AResponse.Contents.Text := HS.MsgBnClick(ARequest.ContentFields);
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'usermon' then
  begin
    AResponse.Contents.Text := HS.ShowUserMonitor;
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'showdbg' then
  begin
    AResponse.Contents.Text := HS.ShowDebugAjax;
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'closedbg' then
  begin
    AResponse.Contents.Text := HS.CloseDebug;
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'cleardbg' then
  begin
    AResponse.Contents.Text := HS.ClearDebug;
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'compileerror' then
  begin
    AResponse.Contents.Text := HS.ShowCompileError;
    AResponse.Code := HS.ResultCode;
  end
  else if LPm = 'runtimeerror' then
  begin
    AResponse.Contents.Text := HS.ShowErrorPage(rsRuntimeError, SS.MainErrorMsg);
    AResponse.Code := HS.ResultCode;
  end
  else
  begin
    try
      HandleOk := False;
      if SS.OnHandleRequest <> nil then
        HandleOk := SS.OnHandleRequest(SS, ARequest, AResponse);
      if not HandleOk then
      begin
        AResponse.Content := HS.ShowErrorPage(rsUnknownRequest, rsUnknownRequestMsg);
        AResponse.Code := rcBadRequest;
      end;
    except
      on E: Exception do
      begin
        AResponse.Contents.Text := HS.ShowErrorPage(rsError, ExceptionToHtml(E));
        AResponse.Code := HS.ResultCode;
      end;
    end;
  end;

  if AResponse.Code = rcAjaxError then
    DebugStr(AResponse.Content, SS);

  except
    on E: Exception do
      LogString('HandleRequest exception. ' + E.ClassName + ': ' + E.Message);  // LogError
  end;
  finally
    if HS.ResultCode = rcAjaxError then
      AResponse.ContentType := GetMimeType('.json');

    HS.Free;
    if SS <> nil then
    begin
      DebugStr('SS.UnlockSession Before', SS);
      SS.UnlockSession;
      DebugStr('SS.UnlockSession After', SS);
    end;
  end;
end;

end.

