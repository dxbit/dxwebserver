unit QSort;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

type
  TCompareFunc = function (const elem1, elem2): Integer;

procedure AnySort(var Arr; Count: Integer; Stride: Integer; CompareFunc: TCompareFunc);

implementation

type
  TByteArray = array [Word] of byte;
  PByteArray = ^TByteArray;

procedure AnyQuickSort(var Arr; idxL, idxH: Integer;
  Stride: Integer; CompareFunc: TCompareFunc; var SwapBuf, MedBuf);
var
  ls,hs : Integer;
  li,hi : Integer;
  mi    : Integer;
  ms    : Integer;
  pb    : PByteArray;
begin
  pb:=@Arr;
  li:=idxL;
  hi:=idxH;
  mi:=(li+hi) div 2;
  ls:=li*Stride;
  hs:=hi*Stride;
  ms:=mi*Stride;
  Move(pb[ms], medBuf, Stride);
  repeat
    while CompareFunc( pb[ls], medBuf) < 0 do begin
      inc(ls, Stride);
      inc(li);
    end;
    while CompareFunc( medBuf, pb[hs] ) < 0 do begin
      dec(hs, Stride);
      dec(hi);
    end;
    if ls <= hs then begin
      Move(pb[ls], SwapBuf, Stride);
      Move(pb[hs], pb[ls], Stride);
      Move(SwapBuf, pb[hs], Stride);
      inc(ls, Stride); inc(li);
      dec(hs, Stride); dec(hi);
    end;
  until ls>hs;
  if hi>idxL then AnyQuickSort(Arr, idxL, hi, Stride, CompareFunc, SwapBuf, MedBuf);
  if li<idxH then AnyQuickSort(Arr, li, idxH, Stride, CompareFunc, SwapBuf, MedBuf);
end;

procedure AnySort(var Arr; Count: Integer; Stride: Integer; CompareFunc: TCompareFunc);
var
  buf: array of byte;
begin
  if Count <= 1 then Exit; // should be more than 1 to be sortable
  SetLength(buf, Stride*2);
  AnyQuickSort(Arr, 0, Count-1, Stride, compareFunc, buf[0], buf[Stride]);
end;

end.
