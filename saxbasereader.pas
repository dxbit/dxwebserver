unit SAXBaseReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SAX, SAX_XML, LazUtf8;

type

  { TSAXBaseReader }

	TSAXBaseReader = class(TSaxXmlReader)
  public
    function GetBool(Atts: TSAXAttributes; const aName: String): Boolean;
    function GetInt(Atts: TSAXAttributes; const aName: String): Integer;
    function GetStr(Atts: TSAXAttributes; const aName: String): String;
    function AttrExists(Atts: TSAXAttributes; const aName: String): Boolean;
  end;

implementation

uses AppUtils;

{ TSAXBaseReader }

function TSAXBaseReader.GetBool(Atts: TSAXAttributes; const aName: String
  ): Boolean;
var
  S: String;
begin
  Result := False;
  S := GetStr(Atts, aName);
  if S = '1' then Result := True;
end;

function TSAXBaseReader.GetInt(Atts: TSAXAttributes; const aName: String
  ): Integer;
var
  S: String;
begin
  Result := 0;
  S := GetStr(Atts, aName);
  if S <> '' then TryStrToInt(S, Result);
end;

function TSAXBaseReader.GetStr(Atts: TSAXAttributes; const aName: String
  ): String;
var
  i: Integer;
  S: String;
begin
  Result := '';
  for i := 0 to Atts.Length - 1 do
  begin
    S := Atts.GetLocalName(i);
		if CompareText(S, aName) = 0 then
    	Exit( Utf16ToUtf8(Atts.GetValue(i)) );
  end;
end;

function TSAXBaseReader.AttrExists(Atts: TSAXAttributes; const aName: String
  ): Boolean;
var
  i: Integer;
  S: String;
begin
  Result := False;
  for i := 0 to Atts.Length - 1 do
  begin
    S := Atts.GetLocalName(i);
		if CompareText(S, aName) = 0 then
    	Exit( True );
  end;
end;

end.

