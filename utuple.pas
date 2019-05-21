unit uTuple;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uTupleIntf, uToken;

type

  TTuple = class(TInterfacedObject, ITuple)
    private
      FElements: TTupleElements;
      function getElements: TTupleElements;
    public
      property Elements: TTupleElements read getElements;
      constructor Create;
      destructor Destroy; override;
      function toString: String; override;
      function Get(i: Integer; Token: TToken): Variant;
      procedure Put(i: Integer; Value: Variant; Token: TToken);
  end;

implementation
uses Variants, uVariantSupport, uError;

{ TTuple }

function TTuple.getElements: TTupleElements;
begin
  Result := FElements
end;

constructor TTuple.Create;
begin
  FElements := TTupleElements.Create;
end;

destructor TTuple.Destroy;
begin
  FElements.Free;
  inherited Destroy;
end;

function TTuple.toString: String;
var
  i: Integer;

  function getStr(Value: Variant): String;
  begin
    Result := Value.toString;
    if VarType(Value) = varString then
      Result := QuotedStr(Result);
  end;

begin
  Result := '(';
  if FElements.Count > 0 then begin
    for i := 0 to FElements.Count-2 do
      Result += getStr(FElements[i]) + ', ';
    Result += getStr(FElements[FElements.Count-1]);
  end;
  Result += ')';
end;

function TTuple.Get(i: Integer; Token: TToken): Variant;
var
  Index, Count: Integer;
begin
  Index := i-1; // A tuple starts at 1, but FElements starts at 0
  Count := FElements.Count;
  if Count = 0 then
    Result := Unassigned
  else if (Index >= 0) and (Index < Count) then
    Result := FElements[Index]
  else
    Raise ERuntimeError.Create(Token,
      'Index ('+ IntToStr(i) + ') out of range ('+ '1,' + IntToStr(Count) + ').');
end;

procedure TTuple.Put(i: Integer; Value: Variant; Token: TToken);
var
  Index, Count: Integer;
begin
  Index := i-1; // A tuple starts at 1, but FElements starts at 0
  Count := FElements.Count;
  if (Index >= 0) and (Index < Count) then
    FElements[Index] := Value
  else
    Raise ERuntimeError.Create(Token,
      'Index ('+ IntToStr(i) + ') out of range ('+ '1,' + IntToStr(Count) + ').');
end;

end.

