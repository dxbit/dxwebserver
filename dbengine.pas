{-------------------------------------------------------------------------------

    Copyright 2016-2025 Pavel Duborkin ( mydataexpress@mail.ru )

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

unit DBEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Db, IBConnection, SQLDb, SQLDbLib, DXReports, MemDs;

type

  { TdxConnection }

  TdxConnection = class(TIBConnection)
  protected
    function ConstructInsertSQL(Query: TCustomSQLQuery; var ReturningClause: Boolean
      ): string; override;
    function ConstructUpdateSQL(Query: TCustomSQLQuery; var ReturningClause: Boolean
      ): string; override;
    //function ConstructDeleteSQL(Query: TCustomSQLQuery): string; override;
    procedure UpdateIndexDefs(IndexDefs: TIndexDefs; TableName: string); override;
  end;

  { TdxDataSet }

  TdxDataSet = class(TSQLQuery)
  private
    FRS: TObject;
  protected
    procedure InternalInitFieldDefs; override;
    function LoadField(FieldDef: TFieldDef; buffer: pointer; out CreateBlob: boolean
      ): boolean; override;
  public
    property RS: TObject read FRS write FRS;
    procedure Delete; override;
  end;

  { TdxMemDataSet }

  TdxMemDataSet = class(TMemDataSet)
  public
    procedure CopyFromDataset(DataSet : TDataSet; CopyData : Boolean); reintroduce;
  end;

  { TDBEngine }

  TDBEngine = class
  private
    FConn: TIBConnection;
    FDatabase: String;
    FDBPwd: String;
    FTrans, FUpdateTrans: TSQLTransaction;
    //FLoader: TSQLDBLibraryLoader;
    procedure SetDatabase(AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure AttachDataSet(DS: TSQLQuery);
    function OpenDataSet(const SQL: String): TSQLQuery;
    function GetMetaLastModified: TDateTime;
    procedure Execute(const aSQL: String);
    procedure ApplyDataSet(DS: TSQLQuery);
    procedure Commit;
    function GenId(const S: String; AValue: Integer = 1): Variant;
    function Connected: Boolean;
    property Database: String read FDatabase write SetDatabase;
    property DBPwd: String read FDBPwd write FDBPwd;
  end;

//var
//  DBase: TDBEngine;

implementation

uses
  LazUtf8, apputils, LazFileUtils, dxctrls, dxtypes;

{ TdxMemDataSet }

// Слегка модифицированный оригинал (добавлена поддержка UTF8)
procedure TdxMemDataSet.CopyFromDataset(DataSet: TDataSet; CopyData: Boolean);
Var
  I  : Integer;
  F,F1,F2 : TField;
  L1,L2  : TList;
  N : String;
  OriginalPosition: TBookMark;

begin
  Clear(True);
  // NOT from FieldDefs. The data may not be available in buffers !!
  For I:=0 to Dataset.FieldCount-1 do
    begin
    F:=Dataset.Fields[I];
    TFieldDef.Create(FieldDefs,F.FieldName,F.DataType,F.Size,F.Required,F.FieldNo, CP_UTF8);
    end;
  CreateTable;
  If CopyData then
  begin
    Open;
    L1:=TList.Create;
    Try
      L2:=TList.Create;
      Try
        For I:=0 to FieldDefs.Count-1 do
          begin
          N:=FieldDefs[I].Name;
          F1:=FieldByName(N);
          F2:=DataSet.FieldByName(N);
          L1.Add(F1);
          L2.Add(F2);
          end;
        DisableControls;
        Dataset.DisableControls;
        OriginalPosition:=Dataset.GetBookmark;
        Try
          Dataset.Open;
          Dataset.First; //make sure we copy from the beginning
          While not Dataset.EOF do
            begin
            Append;
            For I:=0 to L1.Count-1 do
              begin
              F1:=TField(L1[i]);
              F2:=TField(L2[I]);
              if F2.IsNull then
                F1.Clear
              else
                Case F1.DataType of
                  ftFixedChar,
                  ftString   : F1.AsString:=F2.AsString;
                  ftBoolean  : F1.AsBoolean:=F2.AsBoolean;
                  ftFloat    : F1.AsFloat:=F2.AsFloat;
                  ftLargeInt : F1.AsLargeInt:=F2.AsLargeInt;
                  ftSmallInt : F1.AsInteger:=F2.AsInteger;
                  ftInteger  : F1.AsInteger:=F2.AsInteger;
                  ftDate     : F1.AsDateTime:=F2.AsDateTime;
                  ftTime     : F1.AsDateTime:=F2.AsDateTime;
                  ftDateTime : F1.AsDateTime:=F2.AsDateTime;
                  else         F1.AsString:=F2.AsString;
                end;
              end;
            Try
              Post;
            except
              Cancel;
              Raise;
            end;
            Dataset.Next;
            end;
        Finally
          DataSet.GotoBookmark(OriginalPosition); //Return to original record
          Dataset.EnableControls;
          EnableControls;
        end;
      finally
        L2.Free;
      end;
    finally
      l1.Free;
    end;
  end;
end;

{ TdxDataSet }

function RpFieldTypeToFieldType(Tp: TRpFieldType): TFieldType;
begin
  case Tp of
    flText: Result := ftString;
    flNumber: Result := ftFloat;
    flDate: Result := ftDate;
    flTime: Result := ftTime;
    else raise Exception.Create('RpFieldTypeToFieldType: unsupported field type.');
  end;
end;

procedure TdxDataSet.InternalInitFieldDefs;
var
  RD: TReportData;
  i: Integer;
  CF: PRpCalcField;
begin
  inherited InternalInitFieldDefs;
  if (FRS = nil) or (TSsRecordSet(FRS).RD = nil) then Exit;
  RD := TSsRecordSet(FRS).RD;
  for i := 0 to RD.CalcFields.Count - 1 do
  begin
    CF := RD.CalcFields[i];
    FieldDefs.Add('cf' + IntToStr(CF^.Id), RpFieldTypeToFieldType(CF^.Tp), CF^.Size,
      -1, False, False, FieldDefs.Count + 1, CP_UTF8);
  end;
end;

function GetNonCalcFieldsCount(RD: TReportData): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to RD.GetRpSQLFieldCount - 1 do
  begin
    if not RD.GetFieldVisible(i) then Continue;

    if RD.GetFieldType(i) in [flFile, flImage] then Inc(Result, 4)
    else Inc(Result);
  end;
end;

function TdxDataSet.LoadField(FieldDef: TFieldDef; buffer: pointer; out
  CreateBlob: boolean): boolean;
var
  RD: TReportData;
  delta: Integer;
begin
  if (FRS <> nil) and (TSsRecordSet(FRS).RD <> nil) then
  begin
    RD := TSsRecordSet(FRS).RD;
    if RD.IsSimple then
    begin
      if RD.HasParentIdField then delta := 2
      else delta := 1;
    end
    else
      delta := 0;
    if FieldDef.FieldNo > GetNonCalcFieldsCount(RD) + delta then Exit(True);
  end;
  Result:=inherited LoadField(FieldDef, buffer, CreateBlob);
end;

procedure TdxDataSet.Delete;
begin
  inherited Delete;
  // Обход ошибки от 12.03.2022: на печать выводится последняя запись отфильтрованного
  // запроса (выходной фильтр), когда в запросе на самом деле нет записей (запрос пустой).
  // Получается, что внутренний буфер не очищается после удаления записи и
  // обращение к полю вытягивает старые данные из буфера.
  if IsEmpty then InitRecord(ActiveBuffer);
end;

{ TdxConnection }

function TdxConnection.ConstructInsertSQL(Query: TCustomSQLQuery;
  var ReturningClause: Boolean): string;
var
  RS: TSsRecordSet;
  i: Integer;
  L: TList;
  F: TdxField;
  Fields, Values: String;
begin
  if (Query is TdxDataSet) and (TdxDataSet(Query).RS <> nil) then
  begin
    ReturningClause := False;
    RS := TSsRecordSet(TdxDataSet(Query).RS);
    Fields := 'id,';
    Values := ':id,';
    if RS.Form.PId > 0 then
    begin
      Fields := Fields + 'pid,';
      Values := Values + ':pid,';
    end;
    L := TList.Create;
    RS.Form.GetFields(L);
    for i := 0 to L.Count - 1 do
    begin
      F := TdxField(L[i]);
      if (F is TdxObjectField) or (F is TdxRecordId) then Continue;
      Fields := Fields + FieldStr(F.Id) + ',';
      Values := Values + ':' + FieldStr(F.Id) + ',';
      if F is TdxDBImage then
      begin
        Fields := Fields + FieldStr(F.Id) + 'src,' + FieldStr(F.Id) + 'dest,' +
          FieldStr(F.Id) + 'thumb,';
        Values := Values + ':' + FieldStr(F.Id) + 'src,:' + FieldStr(F.Id) + 'dest,:' +
          FieldStr(F.Id) + 'thumb,';
      end
      else if F is TdxFile then
      begin
        Fields := Fields + FieldStr(F.Id) + 'src,' + FieldStr(F.Id) + 'dest,' +
          FieldStr(F.Id) + 'd,';
        Values := Values + ':' + FieldStr(F.Id) + 'src,:' + FieldStr(F.Id) + 'dest,:' +
          FieldStr(F.Id) + 'd,';
      end;
    end;
    SetLength(Fields, Length(Fields) - 1);
    SetLength(Values, Length(Values) - 1);
    Result := 'insert into ' + TableStr(RS.Form.Id) + ' (' + Fields + ') values (' +
      Values + ')';
    L.Free;
  end
  else
    Result:=inherited ConstructInsertSQL(Query, ReturningClause);
end;

function TdxConnection.ConstructUpdateSQL(Query: TCustomSQLQuery;
  var ReturningClause: Boolean): string;
var
  RS: TSsRecordSet;
  L: TList;
  i: Integer;
  F: TdxField;
  FNm: String;
begin
  if (Query is TdxDataSet) and (TdxDataSet(Query).RS <> nil) then
  begin
    ReturningClause := False;
    RS := TSsRecordSet(TdxDataSet(Query).RS);
    Result := 'update ' + TableStr(RS.Form.Id) + ' set id=:id,';
    L := TList.Create;
    RS.Form.GetFields(L);
    for i := 0 to L.Count - 1 do
    begin
      F := TdxField(L[i]);
      if (F is TdxObjectField) or (F is TdxRecordId) then Continue;
      FNm := FieldStr(F.Id);
      if F is TdxDBImage then
      begin
        if RS.FieldChanged(F) then
          Result := Result + FNm + '=:' + FNm + ',' +
            FNm + 'src=:' + FNm + 'src,' + FNm + 'dest=:' +
            FNm + 'dest,' + FNm + 'thumb=:' + FNm + 'thumb,';
      end
      else if F is TdxFile then
      begin
        if RS.FieldChanged(F) then
          Result := Result + FNm + '=:' + FNm + ',' +
            FNm + 'src=:' + FNm + 'src,' + FNm + 'dest=:' +
            FNm + 'dest,' + FNm + 'd=:' + FNm + 'd,';
      end
      else if RS.FieldChanged(F) then
        Result := Result + FieldStr(F.Id) + '=:' + FieldStr(F.Id) + ',';
    end;
    SetLength(Result, Length(Result) - 1);
    Result := Result + ' where id=:id';
    L.Free;
  end
  else
    Result:=inherited ConstructUpdateSQL(Query, ReturningClause);
end;

{function TdxConnection.ConstructDeleteSQL(Query: TCustomSQLQuery): string;
var
  RS: TSsRecordSet;
begin
  if (Query is TdxDataSet) and (TdxDataSet(Query).RS <> nil) then
  begin
    RS := TdxDataSet(Query).RS;
    Result := 'delete from ' + TableStr(RS.Form.Id) + ' where id=:id';
  end
  else
    Result:=inherited ConstructDeleteSQL(Query);
end; }

procedure TdxConnection.UpdateIndexDefs(IndexDefs: TIndexDefs; TableName: string
  );
begin
  //inherited UpdateIndexDefs(IndexDefs, TableName);
end;

{ TdxConnection }

{ TDBEngine }

function IsRelativePath(const Path: String): Boolean;
var
  p: Integer;
begin
  p := Pos(':', Path);
  if p > 2 then Exit(False);
  Result := not FilenameIsAbsolute(Path);
  {if Result then
    for i := 2 to Length(Path) do
    begin
      if Path[i] = ':' then Exit(False)
      else if Path[i] in AllowDirectorySeparators then Exit(True);
    end;}
end;

procedure TDBEngine.SetDatabase(AValue: String);
begin
  if IsRelativePath(AValue) then
    AValue := AppPath + AValue;
  FDatabase := AValue;
  {$ifdef windows}
  FConn.DatabaseName:=Utf8ToWinCP(AValue);
  {$else}
  FConn.DatabaseName := AValue;
  {$endif}
end;

constructor TDBEngine.Create;
begin
  FConn := TdxConnection.Create(nil);
  FConn.UserName:='sysdba';
  FConn.Password:='masterkey';
  FConn.CharSet:='UTF8';;
  FTrans := TSQLTransaction.Create(nil);
  FTrans.Params.AddStrings(['isc_tpb_read', 'isc_tpb_read_committed',
    'isc_rec_version', 'isc_tpb_nowait']);
  FTrans.DataBase := FConn;
  FUpdateTrans := TSQLTransaction.Create(nil);
  FUpdateTrans.Params.AddStrings(['isc_tpb_write', 'isc_tpb_read_committed',
      'isc_tpb_nowait', 'isc_tpb_rec_version']);
  FUpdateTrans.DataBase := FConn;
  //FLoader := TSQLDBLibraryLoader.Create(nil);
  //FLoader.ConnectionType:='Firebird';
end;

destructor TDBEngine.Destroy;
begin
  //FLoader.Free;
  FUpdateTrans.Free;
  FTrans.Free;
  FConn.Free;
  inherited Destroy;
end;

procedure TDBEngine.Connect;
begin
  if FDBPwd = '' then FConn.Password := 'masterkey'
  else FConn.Password := FDBPwd;
  (*{$ifdef windows}
  FLoader.LibraryName := AppPath + 'fbclientd.dll';
  {$else}
  FLoader.LibraryName := AppPath + 'libfbclient.so';
  {$endif}
  FLoader.Enabled:=True;*)
  FConn.Open;
end;

procedure TDBEngine.Disconnect;
begin
  try
    FConn.Close;
    //FLoader.Enabled:=False;
  except
    on E: Exception do
      LogString('Disconnect: ' + E.Message);
  end;
end;

procedure TDBEngine.AttachDataSet(DS: TSQLQuery);
begin
  DS.DataBase := FConn;
  DS.Transaction := FTrans;
  DS.UpdateTransaction := FUpdateTrans;
end;

{function TDBEngine.OpenDataSet(const SQL: String): TSQLQuery;
begin
  Result := TdxDataSet.Create(nil);
  AttachDataSet(Result);
  Result.SQL.Text := SQL;
  try
    Result.Open;
  except
    on E: Exception do
    begin
      FreeAndNil(Result);
      raise;
    end;
  end;
end;    }

function TDBEngine.OpenDataSet(const SQL: String): TSQLQuery;
var
  KeyField: TField;
begin
  //DebugStr(SQL);
  Result := TdxDataSet.Create(nil);
  Result.Transaction := FTrans;
  Result.UpdateTransaction := FUpdateTrans;
  Result.DataBase := FConn;
  Result.SQL.Text := SQL;
  Result.DeleteSQL.Text := 'delete id from rdb$database';
  try
    Result.Open;
    KeyField := Result.FindField('id');
    if KeyField <> nil then KeyField.ProviderFlags := KeyField.ProviderFlags + [pfInKey];
  except
    on E: Exception do
    begin
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

function TDBEngine.GetMetaLastModified: TDateTime;
begin
  try
    with OpenDataSet('select lastmodified from dx_main') do
    begin
      Result := Fields[0].AsDateTime;
      Free;
    end;
  except
    on E: Exception do
      Result := 0;
  end;
end;

procedure TDBEngine.Execute(const aSQL: String);
var
  Trans: TSQLTransaction;
begin
  if aSQL = '' then Exit;
  Trans := FUpdateTrans;

  with TSQLScript.Create(nil) do
    try try
      DataBase := FConn;
      Transaction := Trans;
      Terminator:=';';
      Script.Text := aSQL;
      ExecuteScript;
      Trans.Commit;
    except
      on E: EDatabaseError do
      begin
        Trans.Rollback;
        raise;
      end;
    end;
    finally
      Free;
    end;
end;

{procedure TDBEngine.ApplyDataSet(DS: TSQLQuery);
begin
  try
    if DS.Active then DS.ApplyUpdates;
  except
    on E: EDatabaseError do
    begin
      FTrans.RollbackRetaining;
      raise;
    end;
  end;
end;}


procedure TDBEngine.ApplyDataSet(DS: TSQLQuery);
var
  n: Integer;
begin
  n := 0;
  while True do
    try
      if DS.Active then DS.ApplyUpdates;
      Break;
    except
      on E: EDatabaseError do
      begin
        FUpdateTrans.Rollback;
        if Pos('lock conflict on no wait transaction', E.Message) > 0 then
        begin
          Sleep(50);
          Inc(n);
          if n < 3 then Continue
          else raise;
        end
        else
          raise;
      end;
    end;
end;

procedure TDBEngine.Commit;
begin
  try
    FUpdateTrans.Commit;
  except
    on E: EDatabaseError do
    begin
      FUpdateTrans.Rollback;
      raise;
    end;
  end;
end;

function TDBEngine.GenId(const S: String; AValue: Integer): Variant;
begin
  with OpenDataSet('select gen_id(' + S + ',' + IntToStr(AValue) + ') from rdb$database') do
    try
      Result := Fields[0].Value;
    finally
      Free;
    end;
end;

function TDBEngine.Connected: Boolean;
begin
  Result := FConn.Connected;
end;

end.

