unit utils;

{$mode objfpc}{$H+}

interface

function IPlusPlus(var i: Integer): Integer;
function PlusPlusI(var i: Integer): Integer;

function IMinMin(var i: Integer): Integer;
function MinMinI(var i: Integer): Integer;

implementation

function IPlusPlus(var i: Integer): Integer;
begin
  Result := i;
  Inc(i);
end;

function PlusPlusI(var i: Integer): Integer;
begin
  Inc(i);
  Result := i;
end;

function IMinMin(var i: Integer): Integer;
begin
  Result := i;
  Dec(i);
end;

function MinMinI(var i: Integer): Integer;
begin
  Dec(i);
  Result := i;
end;

end.

