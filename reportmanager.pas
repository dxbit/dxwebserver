unit ReportManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, dxreports;

type

  { TReportManager }

  TReportManager = class
  private
    FReports: TList;
    function GetReports(Index: Integer): TReportData;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddReport(RD: TReportData);
    function ReportCount: Integer;
    function FindReport(Id: Integer): TReportData;
    function FindReportByName(const aName: String): TReportData;
    procedure GetReportList(SL: TStrings);
    property Reports[Index: Integer]: TReportData read GetReports;
  end;

implementation

uses
  apputils, LazUtf8;

{ TReportManager }

function TReportManager.GetReports(Index: Integer): TReportData;
begin
  Result := TReportData(FReports[Index]);
end;

constructor TReportManager.Create;
begin
  //FSS := SS;
  FReports := TList.Create;
end;

destructor TReportManager.Destroy;
begin
  ClearList(FReports);
  FReports.Free;
  inherited Destroy;
end;

procedure TReportManager.AddReport(RD: TReportData);
begin
  FReports.Add(RD);
end;

function TReportManager.ReportCount: Integer;
begin
  Result := FReports.Count;
end;

function TReportManager.FindReport(Id: Integer): TReportData;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FReports.Count - 1 do
    if Reports[i].Id = Id then Exit(Reports[i]);
end;

function TReportManager.FindReportByName(const aName: String): TReportData;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FReports.Count - 1 do
    if MyUtf8CompareText(Reports[i].Name, aName) = 0 then Exit(Reports[i]);
end;

procedure TReportManager.GetReportList(SL: TStrings);
var
  i: Integer;
begin
  SL.Clear;
  for i := 0 to ReportCount - 1 do
    if Reports[i].Kind = rkReport then
      SL.AddObject(Reports[i].Name, Reports[i]);
end;

end.

