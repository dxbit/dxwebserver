unit TypesWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAGraphics;

{type
  TColor = -$7FFFFFFF - 1..$7FFFFFFF;
  TColorRef = Cardinal;  }

const
  (*SYS_COLOR_BASE = TColorRef($80000000);
  COLOR_SCROLLBAR = 0;
  COLOR_BACKGROUND = 1;
  COLOR_ACTIVECAPTION = 2;
  COLOR_INACTIVECAPTION = 3;
  COLOR_MENU = 4;
  COLOR_WINDOW = 5;
  COLOR_WINDOWFRAME = 6;
  COLOR_MENUTEXT = 7;
  COLOR_WINDOWTEXT = 8;
  COLOR_CAPTIONTEXT = 9;
  COLOR_ACTIVEBORDER = 10;
  COLOR_INACTIVEBORDER = 11;
  COLOR_APPWORKSPACE = 12;
  COLOR_HIGHLIGHT = 13;
  COLOR_HIGHLIGHTTEXT = 14;
  COLOR_BTNFACE = 15;
  COLOR_BTNSHADOW = 16;
  COLOR_GRAYTEXT = 17;
  COLOR_BTNTEXT = 18;
  COLOR_INACTIVECAPTIONTEXT = 19;
  COLOR_BTNHIGHLIGHT = 20;
  COLOR_3DDKSHADOW = 21;
  COLOR_3DLIGHT = 22;
  COLOR_INFOTEXT = 23;
  COLOR_INFOBK = 24;
  COLOR_HOTLIGHT = 26;
  COLOR_GRADIENTACTIVECAPTION = 27;
  COLOR_GRADIENTINACTIVECAPTION = 28;
  COLOR_MENUHILIGHT = 29;
  COLOR_MENUBAR = 30;

  COLOR_FORM = 31;

  COLOR_ENDCOLORS = COLOR_FORM;

  COLOR_DESKTOP = COLOR_BACKGROUND;
  COLOR_3DFACE = COLOR_BTNFACE;
  COLOR_3DSHADOW = COLOR_BTNSHADOW;
  COLOR_3DHIGHLIGHT = COLOR_BTNHIGHLIGHT;
  COLOR_3DHILIGHT = COLOR_BTNHIGHLIGHT;
  COLOR_BTNHILIGHT = COLOR_BTNHIGHLIGHT;

  // CLX base, mapped, pseudo, rgb values
  COLOR_clForeground =  32;
  COLOR_clButton =  COLOR_clForeground+1;
  COLOR_clLight =  COLOR_clForeground+2;
  COLOR_clMidlight =  COLOR_clForeground+3;
  COLOR_clDark =  COLOR_clForeground+4;
  COLOR_clMid =  COLOR_clForeground+5;
  COLOR_clText =  COLOR_clForeground+6;
  COLOR_clBrightText =  COLOR_clForeground+7;
  COLOR_clButtonText =  COLOR_clForeground+8;
  COLOR_clBase =  COLOR_clForeground+9;
  //clBackground
  COLOR_clShadow =  COLOR_clForeground+10;
  //clHighlight
  COLOR_clHighlightedText =  COLOR_clForeground+11;

  // CLX normal, mapped, pseudo, rgb values
  COLOR_clNormalForeground =  44;
  COLOR_clNormalButton =  COLOR_clNormalForeground+1;
  COLOR_clNormalLight =  COLOR_clNormalForeground+2;
  COLOR_clNormalMidlight =  COLOR_clNormalForeground+3;
  COLOR_clNormalDark =  COLOR_clNormalForeground+4;
  COLOR_clNormalMid =  COLOR_clNormalForeground+5;
  COLOR_clNormalText =  COLOR_clNormalForeground+6;
  COLOR_clNormalBrightText =  COLOR_clNormalForeground+7;
  COLOR_clNormalButtonText =  COLOR_clNormalForeground+8;
  COLOR_clNormalBase =  COLOR_clNormalForeground+9;
  COLOR_clNormalBackground =  COLOR_clNormalForeground+10;
  COLOR_clNormalShadow =  COLOR_clNormalForeground+11;
  COLOR_clNormalHighlight =  COLOR_clNormalForeground+12;

  // CLX disabled, mapped, pseudo, rgb values
  COLOR_clDisabledForeground =  58;
  // CLX active, mapped, pseudo, rgb values
  COLOR_clActiveForeground =  72;      *)

  // standard colors
  (*clBlack   = TColor($000000);
  clMaroon  = TColor($000080);
  clGreen   = TColor($008000);
  clOlive   = TColor($008080);
  clNavy    = TColor($800000);
  clPurple  = TColor($800080);
  clTeal    = TColor($808000);
  clGray    = TColor($808080);
  clSilver  = TColor($C0C0C0);
  clRed     = TColor($0000FF);
  clLime    = TColor($00FF00);
  clYellow  = TColor($00FFFF);
  clBlue    = TColor($FF0000);
  clFuchsia = TColor($FF00FF);
  clAqua    = TColor($FFFF00);
  clLtGray  = TColor($C0C0C0); // clSilver alias
  clDkGray  = TColor($808080); // clGray alias
  clWhite   = TColor($FFFFFF);*)

  // extended colors
  clMoneyGreen = TColor($C0DCC0);
  clSkyBlue    = TColor($F0CAA6);
  clCream      = TColor($F0FBFF);
  clMedGray    = TColor($A4A0A0);
  //ExtendedColorCount = 4;

  // special colors
  (*clNone    = TColor($1FFFFFFF);
  clDefault = TColor($20000000);

  // system colors
  clScrollBar               = TColor(SYS_COLOR_BASE or COLOR_SCROLLBAR);
  clBackground              = TColor(SYS_COLOR_BASE or COLOR_BACKGROUND);
  clActiveCaption           = TColor(SYS_COLOR_BASE or COLOR_ACTIVECAPTION);
  clInactiveCaption         = TColor(SYS_COLOR_BASE or COLOR_INACTIVECAPTION);
  clMenu                    = TColor(SYS_COLOR_BASE or COLOR_MENU);
  clWindow                  = TColor(SYS_COLOR_BASE or COLOR_WINDOW);
  clWindowFrame             = TColor(SYS_COLOR_BASE or COLOR_WINDOWFRAME);
  clMenuText                = TColor(SYS_COLOR_BASE or COLOR_MENUTEXT);
  clWindowText              = TColor(SYS_COLOR_BASE or COLOR_WINDOWTEXT);
  clCaptionText             = TColor(SYS_COLOR_BASE or COLOR_CAPTIONTEXT);
  clActiveBorder            = TColor(SYS_COLOR_BASE or COLOR_ACTIVEBORDER);
  clInactiveBorder          = TColor(SYS_COLOR_BASE or COLOR_INACTIVEBORDER);
  clAppWorkspace            = TColor(SYS_COLOR_BASE or COLOR_APPWORKSPACE);
  clHighlight               = TColor(SYS_COLOR_BASE or COLOR_HIGHLIGHT);
  clHighlightText           = TColor(SYS_COLOR_BASE or COLOR_HIGHLIGHTTEXT);
  clBtnFace                 = TColor(SYS_COLOR_BASE or COLOR_BTNFACE);
  clBtnShadow               = TColor(SYS_COLOR_BASE or COLOR_BTNSHADOW);
  clGrayText                = TColor(SYS_COLOR_BASE or COLOR_GRAYTEXT);
  clBtnText                 = TColor(SYS_COLOR_BASE or COLOR_BTNTEXT);
  clInactiveCaptionText     = TColor(SYS_COLOR_BASE or COLOR_INACTIVECAPTIONTEXT);
  clBtnHighlight            = TColor(SYS_COLOR_BASE or COLOR_BTNHIGHLIGHT);
  cl3DDkShadow              = TColor(SYS_COLOR_BASE or COLOR_3DDKSHADOW);
  cl3DLight                 = TColor(SYS_COLOR_BASE or COLOR_3DLIGHT);
  clInfoText                = TColor(SYS_COLOR_BASE or COLOR_INFOTEXT);
  clInfoBk                  = TColor(SYS_COLOR_BASE or COLOR_INFOBK);

  clHotLight                = TColor(SYS_COLOR_BASE or COLOR_HOTLIGHT);
  clGradientActiveCaption   = TColor(SYS_COLOR_BASE or COLOR_GRADIENTACTIVECAPTION);
  clGradientInactiveCaption = TColor(SYS_COLOR_BASE or COLOR_GRADIENTINACTIVECAPTION);
  clMenuHighlight           = TColor(SYS_COLOR_BASE or COLOR_MENUHILIGHT);
  clMenuBar                 = TColor(SYS_COLOR_BASE or COLOR_MENUBAR);
  clForm                    = TColor(SYS_COLOR_BASE or COLOR_FORM);

  // synonyms: do not show them in color lists
  clColorDesktop            = TColor(SYS_COLOR_BASE or COLOR_DESKTOP);
  cl3DFace                  = TColor(SYS_COLOR_BASE or COLOR_3DFACE);
  cl3DShadow                = TColor(SYS_COLOR_BASE or COLOR_3DSHADOW);
  cl3DHiLight               = TColor(SYS_COLOR_BASE or COLOR_3DHIGHLIGHT);
  clBtnHiLight              = TColor(SYS_COLOR_BASE or COLOR_BTNHILIGHT);

  clForeground = TColor(-1) deprecated;
  clButton = TColor(-2) deprecated;
  clLight = TColor(-3) deprecated;
  clMidlight = TColor(-4) deprecated;
  clDark = TColor(-5) deprecated;
  clMid = TColor(-6) deprecated;
  clText = TColor(-7) deprecated;
  clBrightText = TColor(-8) deprecated;
  clButtonText = TColor(-9) deprecated;
  clBase = TColor(-10) deprecated;
  clxBackground = TColor(-11) deprecated;
  clShadow = TColor(-12) deprecated;
  clxHighlight = TColor(-13) deprecated;
  clHighlightedText = TColor(-14) deprecated;

  // CLX mapped role offsets
  cloNormal = 32 deprecated;
  cloDisabled = 64 deprecated;
  cloActive = 96 deprecated;

  // CLX normal, mapped, pseudo, rgb values
  clNormalForeground = TColor(clForeground - cloNormal) deprecated;
  clNormalButton = TColor(clButton - cloNormal) deprecated;
  clNormalLight = TColor(clLight - cloNormal) deprecated;
  clNormalMidlight = TColor(clMidlight - cloNormal) deprecated;
  clNormalDark = TColor(clDark - cloNormal) deprecated;
  clNormalMid = TColor(clMid - cloNormal) deprecated;
  clNormalText = TColor(clText - cloNormal) deprecated;
  clNormalBrightText = TColor(clBrightText - cloNormal) deprecated;
  clNormalButtonText = TColor(clButtonText - cloNormal) deprecated;
  clNormalBase = TColor(clBase - cloNormal) deprecated;
  clNormalBackground = TColor(clxBackground - cloNormal) deprecated;
  clNormalShadow = TColor(clShadow - cloNormal) deprecated;
  clNormalHighlight = TColor(clxHighlight - cloNormal) deprecated;
  clNormalHighlightedText = TColor(clHighlightedText - cloNormal) deprecated;

  // CLX disabled, mapped, pseudo, rgb values
  clDisabledForeground = TColor(clForeground - cloDisabled) deprecated;
  clDisabledButton = TColor(clButton - cloDisabled) deprecated;
  clDisabledLight = TColor(clLight - cloDisabled) deprecated;
  clDisabledMidlight = TColor(clMidlight - cloDisabled) deprecated;
  clDisabledDark = TColor(clDark - cloDisabled) deprecated;
  clDisabledMid = TColor(clMid - cloDisabled) deprecated;
  clDisabledText = TColor(clText - cloDisabled) deprecated;
  clDisabledBrightText = TColor(clBrightText - cloDisabled) deprecated;
  clDisabledButtonText = TColor(clButtonText - cloDisabled) deprecated;
  clDisabledBase = TColor(clBase - cloDisabled) deprecated;
  clDisabledBackground = TColor(clxBackground - cloDisabled) deprecated;
  clDisabledShadow = TColor(clShadow - cloDisabled) deprecated;
  clDisabledHighlight = TColor(clxHighlight - cloDisabled) deprecated;
  clDisabledHighlightedText = TColor(clHighlightedText - cloDisabled) deprecated;

  // CLX active, mapped, pseudo, rgb values
  clActiveForeground = TColor(clForeground - cloActive) deprecated;
  clActiveButton = TColor(clButton - cloActive) deprecated;
  clActiveLight = TColor(clLight - cloActive) deprecated;
  clActiveMidlight = TColor(clMidlight - cloActive) deprecated;
  clActiveDark = TColor(clDark - cloActive) deprecated;
  clActiveMid = TColor(clMid - cloActive) deprecated;
  clActiveText = TColor(clText - cloActive) deprecated;
  clActiveBrightText = TColor(clBrightText - cloActive) deprecated;
  clActiveButtonText = TColor(clButtonText - cloActive) deprecated;
  clActiveBase = TColor(clBase - cloActive) deprecated;
  clActiveBackground = TColor(clxBackground - cloActive) deprecated;
  clActiveShadow = TColor(clShadow - cloActive) deprecated;
  clActiveHighlight = TColor(clxHighlight - cloActive) deprecated;
  clActiveHighlightedText = TColor(clHighlightedText - cloActive) deprecated;   *)


type
  TComboBoxStyle = (csDropDown, csSimple, csDropDownList, csOwnerDrawFixed, csOwnerDrawVariable, csOwnerDrawEditableFixed, csOwnerDrawEditableVariable);

  {  TFontStyle = (fsBold, fsItalic, fsUnderline, fsStrikeOut);
  TFontStyles = set of TFontStyle;
  TPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear, psInsideFrame, psPattern);
  TBrushStyle = (bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);
  TTextLayout = (tlTop, tlCenter, tlBottom); }

  {TPen = class
  public
    Width: Integer;
    Color: TColor;
    Style: TPenStyle;
  end;

  TBrush = class
  public
    Color: TColor;
    Style: TBrushStyle;
  end; }

function StringToColor(const S: shortstring): TColor;
function RGBToColor(R, G, B: Byte): TColor;
//function ColorToRGB(Color: TColor): Longint;
function Blue(rgb: TColor): BYTE; // does not work on system color
function Green(rgb: TColor): BYTE; // does not work on system color
function Red(rgb: TColor): BYTE; // does not work on system color
function ColorToIdent(Color: Longint; out Ident: String): Boolean;
function ColorToString(Color: TColor): AnsiString;
function ColorToHtml(AColor: TColor): String;

implementation

const
  Colors: array[0..21{106}] of TIdentMapEntry = (
    // standard colors
    (Value: clBlack; Name: 'clBlack'),
    (Value: clMaroon; Name: 'clMaroon'),
    (Value: clGreen; Name: 'clGreen'),
    (Value: clOlive; Name: 'clOlive'),
    (Value: clNavy; Name: 'clNavy'),
    (Value: clPurple; Name: 'clPurple'),
    (Value: clTeal; Name: 'clTeal'),
    (Value: clGray; Name: 'clGray'),
    (Value: clSilver; Name: 'clSilver'),
    (Value: clRed; Name: 'clRed'),
    (Value: clLime; Name: 'clLime'),
    (Value: clYellow; Name: 'clYellow'),
    (Value: clBlue; Name: 'clBlue'),
    (Value: clFuchsia; Name: 'clFuchsia'),
    (Value: clAqua; Name: 'clAqua'),
    (Value: clWhite; Name: 'clWhite'),

    // extended colors
    (Value: clMoneyGreen; Name: 'clMoneyGreen'),
    (Value: clSkyBlue; Name: 'clSkyBlue'),
    (Value: clCream; Name: 'clCream'),
    (Value: clMedGray; Name: 'clMedGray'),

    // special colors
    (Value: clNone; Name: 'clNone'),
    (Value: clDefault; Name: 'clDefault')

    // system colors
    (*(Value: clScrollBar; Name: 'clScrollBar'),
    (Value: clBackground; Name: 'clBackground'),
    (Value: clActiveCaption; Name: 'clActiveCaption'),
    (Value: clInactiveCaption; Name: 'clInactiveCaption'),
    (Value: clMenu; Name: 'clMenu'),
    (Value: clMenuBar; Name: 'clMenuBar'),
    (Value: clMenuHighlight; Name: 'clMenuHighlight'),
    (Value: clMenuText; Name: 'clMenuText'),
    (Value: clWindow; Name: 'clWindow'),
    (Value: clWindowFrame; Name: 'clWindowFrame'),
    (Value: clWindowText; Name: 'clWindowText'),
    (Value: clCaptionText; Name: 'clCaptionText'),
    (Value: clActiveBorder; Name: 'clActiveBorder'),
    (Value: clInactiveBorder; Name: 'clInactiveBorder'),
    (Value: clAppWorkspace; Name: 'clAppWorkspace'),
    (Value: clHighlight; Name: 'clHighlight'),
    (Value: clHighlightText; Name: 'clHighlightText'),
    (Value: clBtnFace; Name: 'clBtnFace'),
    (Value: clBtnShadow; Name: 'clBtnShadow'),
    (Value: clGrayText; Name: 'clGrayText'),
    (Value: clBtnText; Name: 'clBtnText'),
    (Value: clInactiveCaptionText; Name: 'clInactiveCaptionText'),
    (Value: clBtnHighlight; Name: 'clBtnHighlight'),
    (Value: cl3DDkShadow; Name: 'cl3DDkShadow'),
    (Value: cl3DLight; Name: 'cl3DLight'),
    (Value: clInfoText; Name: 'clInfoText'),
    (Value: clInfoBk; Name: 'clInfoBk'),

    (Value: clHotLight; Name: 'clHotLight'),
    (Value: clGradientActiveCaption; Name: 'clGradientActiveCaption'),
    (Value: clGradientInactiveCaption; Name: 'clGradientInactiveCaption'),

    // one our special color
    (Value: clForm; Name: 'clForm'),

    {$warnings off}
    // CLX base, mapped, pseudo, rgb values
    (Value: clForeground; Name: 'clForeground'),
    (Value: clButton; Name: 'clButton'),
    (Value: clLight; Name: 'clLight'),
    (Value: clMidlight; Name: 'clMidlight'),
    (Value: clDark; Name: 'clDark'),
    (Value: clMid; Name: 'clMid'),
    (Value: clText; Name: 'clText'),
    (Value: clBrightText; Name: 'clBrightText'),
    (Value: clButtonText; Name: 'clButtonText'),
    (Value: clBase; Name: 'clBase'),
    //clBackground
    (Value: clShadow; Name: 'clShadow'),
    //clHighlight
    (Value: clHighlightedText; Name: 'clHighlightedText'),

    // CLX normal, mapped, pseudo, rgb values
    (Value: clNormalForeground; Name: 'clNormalForeground'),
    (Value: clNormalButton; Name: 'clNormalButton'),
    (Value: clNormalLight; Name: 'clNormalLight'),
    (Value: clNormalMidlight; Name: 'clNormalMidlight'),
    (Value: clNormalDark; Name: 'clNormalDark'),
    (Value: clNormalMid; Name: 'clNormalMid'),
    (Value: clNormalText; Name: 'clNormalText'),
    (Value: clNormalBrightText; Name: 'clNormalBrightText'),
    (Value: clNormalButtonText; Name: 'clNormalButtonText'),
    (Value: clNormalBase; Name: 'clNormalBase'),
    (Value: clNormalBackground; Name: 'clNormalBackground'),
    (Value: clNormalShadow; Name: 'clNormalShadow'),
    (Value: clNormalHighlight; Name: 'clNormalHighlight'),
    (Value: clNormalHighlightedText; Name: 'clNormalHighlightedText'),

    // CLX disabled, mapped, pseudo, rgb values
    (Value: clDisabledForeground; Name: 'clDisabledForeground'),
    (Value: clDisabledButton; Name: 'clDisabledButton'),
    (Value: clDisabledLight; Name: 'clDisabledLight'),
    (Value: clDisabledMidlight; Name: 'clDisabledMidlight'),
    (Value: clDisabledDark; Name: 'clDisabledDark'),
    (Value: clDisabledMid; Name: 'clDisabledMid'),
    (Value: clDisabledText; Name: 'clDisabledText'),
    (Value: clDisabledBrightText; Name: 'clDisabledBrightText'),
    (Value: clDisabledButtonText; Name: 'clDisabledButtonText'),
    (Value: clDisabledBase; Name: 'clDisabledBase'),
    (Value: clDisabledBackground; Name: 'clDisabledBackground'),
    (Value: clDisabledShadow; Name: 'clDisabledShadow'),
    (Value: clDisabledHighlight; Name: 'clDisabledHighlight'),
    (Value: clDisabledHighlightedText; Name: 'clDisabledHighlightedText'),

    // CLX active, mapped, pseudo, rgb values
    (Value: clActiveForeground; Name: 'clActiveForeground'),
    (Value: clActiveButton; Name: 'clActiveButton'),
    (Value: clActiveLight; Name: 'clActiveLight'),
    (Value: clActiveMidlight; Name: 'clActiveMidlight'),
    (Value: clActiveDark; Name: 'clActiveDark'),
    (Value: clActiveMid; Name: 'clActiveMid'),
    (Value: clActiveText; Name: 'clActiveText'),
    (Value: clActiveBrightText; Name: 'clActiveBrightText'),
    (Value: clActiveButtonText; Name: 'clActiveButtonText'),
    (Value: clActiveBase; Name: 'clActiveBase'),
    (Value: clActiveBackground; Name: 'clActiveBackground'),
    (Value: clActiveShadow; Name: 'clActiveShadow'),
    (Value: clActiveHighlight; Name: 'clActiveHighlight'),
    (Value: clActiveHighlightedText; Name: 'clActiveHighlightedText')  *)
    );

function IdentToColor(const Ident: string; out Color: Longint): Boolean;
begin
  Result := IdentToInt(Ident, Color, Colors);
end;

function StringToColor(const S: shortstring): TColor;
var
  N: Longint;
begin
  Result := clNone;
  if not IdentToColor(S, Longint(Result)) then
  begin
    if TryStrToInt(S, N) then
      Result := TColor(N);
  end;
end;

function RGBToColor(R, G, B: Byte): TColor;
begin
  Result := (B shl 16) or (G shl 8) or R;
end;

(*function SysColorToSysColorIndex(Color: TColor): integer;
begin
  if (Cardinal(Color) and Cardinal(SYS_COLOR_BASE)) <> 0 then begin
    case Color of
    {$warnings off}
    clHighlightedText..clForeground:   // Deprecated values!
      Result:=clForeground+COLOR_clForeground-Color;
    clNormalHighlightedText..clNormalForeground:
      Result:=clNormalForeground+COLOR_clNormalForeground-Color;
    clDisabledHighlightedText..clDisabledForeground:
      Result:=clDisabledForeground+COLOR_clDisabledForeground-Color;
    clActiveHighlightedText..clActiveForeground:
      Result:=clActiveForeground+COLOR_clActiveForeground-Color;
    {$warnings on}
    else
      Result:=Color and $FF;
    end;
  end else begin
    Result:=-1;
  end;
end;   *)

{function ColorToRGB(Color: TColor): Longint;
begin
  i := SysColorToSysColorIndex(Color);
  if i <> -1 then
    Result := 0//GetSysColor(i)
  else
    Result := Color;
  Result := Result and $FFFFFF;
end;          }

function Blue(rgb: TColor): BYTE;
begin
  Result := (rgb shr 16) and $000000ff;
end;

function Green(rgb: TColor): BYTE;
begin
  Result := (rgb shr 8) and $000000ff;
end;

function Red(rgb: TColor): BYTE;
begin
  Result := rgb and $000000ff;
end;

function ColorToIdent(Color: Longint; out Ident: String): Boolean;
begin
  Result := IntToIdent(Color, Ident, Colors);
end;

function ColorToString(Color: TColor): AnsiString;
begin
  Result := '';
  if not ColorToIdent(Color, Result) then
    Result:='$'+HexStr(Color,8);
end;

{function InvertColor(AColor: TColor): TColor;
var
  R, G, B: Integer;
begin
  R := AColor and $ff;
  G := (AColor shr 8) and $ff;
  B := (AColor shr 16) and $ff;

  if Abs($80 - R) + Abs($80 - G) + Abs($80 - B) < $140 then
  begin
    if R<$80 then
      R:=Min($ff,R+$a0)
    else
      R:=Max(0,R-$a0);
    if G<$80 then
      G:=Min($ff,G+$a0)
    else
      G:=Max(0,G-$a0);
    if B<$80 then
      B:=Min($ff,B+$a0)
    else
      B:=Max(0,B-$a0);
  end
  else
  begin
    R := $ff - R;
    G := $ff - G;
    B := $ff - B;
  end;

  Result := ((B and $ff) shl 16) or ((G and $ff) shl 8) or (R and $ff);
end;                                            }
function ColorToHtml(AColor: TColor): String;
var
  R, G, B: Integer;
begin
  R := AColor and $ff;
  G := (AColor shr 8) and $ff;
  B := (AColor shr 16) and $ff;
  Result := '#' + HexStr(((R and $ff) shl 16) or ((G and $ff) shl 8) or (B and $ff), 6);
end;

end.

