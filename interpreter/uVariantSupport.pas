unit uVariantSupport;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, Variants;

type

  TVariantHelper = type helper for Variant
    function toString: string;
  end;

function VarSupportsIntf(Value: Variant; Intf: array of TGUID): Boolean;

implementation

function TVariantHelper.toString: string;
begin
  if VarSupports(Self, IUnknown) then
    Result := (IUnknown(Self) as TInterfacedObject).toString
  else
    Result := VarToStrDef(Self, 'Null');
end;

function VarSupportsIntf(Value: Variant; Intf: array of TGUID): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(Intf) do
    if VarSupports(Value, Intf[i]) then
      Exit(True);
end;

end.

