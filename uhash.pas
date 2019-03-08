unit uHash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  k_hash = 613;
  N_hash = 1008; //size of hash table (1009 = prime number)


function MakeHash(const Ident: string):LongInt;

implementation

function MakeHash(const Ident: string):LongInt;
var
  i, h, l: Cardinal;
begin
  h := 0;
  l := Length(Ident);
  for i := 1 to l do
    h := k_hash * h + Ord(Ident[i]);
  Result := ((h shl 2) shr 2) Mod N_hash;     //least significant 30 bits
end;

end.

