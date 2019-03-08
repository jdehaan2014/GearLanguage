unit uMemory;

{ This unit defines the memory system of the Gear language. It uses Free Pascal's
  FGL unit, which is used in uCollections.

  Copyright (C) 2018 J. de Haan jdehaan2014@gmail.com

  This source is free software; you can redistribute it and/or modify it under the terms
  of the GNU General Public License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU General Public License for more details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free
  Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCollections, Variants, uAST, uToken, uError;

type

  TMemorySpace = class(specialize TDictionary<String, Variant>)
    private
      FEnclosingSpace: TMemorySpace;
    public
      property EnclosingSpace: TMemorySpace read FEnclosingSpace;
      constructor Create(AEnclosingSpace: TMemorySpace = Nil);
      procedure Store(Name: String; Value: Variant);
      procedure Store(Ident: TIdent; Value: Variant);
      function Load(Name: String; Token: TToken): Variant;
      function Load(Ident: TIdent): Variant;
      function LoadAt(Distance: Integer; Ident: TIdent): Variant;
      function LoadAt(Distance: Integer; Name: String): Variant;
      procedure Update(Name: String; Value: Variant; Token: TToken);
      procedure Update(Ident: TIdent; Value: Variant);
      procedure UpdateAt(Distance: Integer; Ident: TIdent; Value: Variant);
    private
      function MemorySpaceAt(Distance: Integer): TMemorySpace;
  end;

implementation

const
  ErrVarUndefined = 'Variable "%s" is undefined.';


{ TMemorySpace }

constructor TMemorySpace.Create(AEnclosingSpace: TMemorySpace);
begin
  inherited Create;
  Sorted := True;
  FEnclosingSpace := AEnclosingSpace;
end;

procedure TMemorySpace.Store(Name: String; Value: Variant);
begin
  Self[Name] := Value;
end;

procedure TMemorySpace.Store(Ident: TIdent; Value: Variant);
begin
  Store(Ident.Text, Value);
end;

function TMemorySpace.Load(Name: String; Token: TToken): Variant;
var
  i: LongInt = -1;
begin
  if Find(Name, i) then  //Find is a standard FGL function to find a key
    Result := At(i)
  else if Assigned(FEnclosingSpace) then
    Result := FEnclosingSpace.Load(Name, Token)
  else
    Raise ERuntimeError.Create(Token, Format(ErrVarUndefined, [Name]));
end;

function TMemorySpace.Load(Ident: TIdent): Variant;
begin
  Result := Load(Ident.Text, Ident.Token);
end;

function TMemorySpace.LoadAt(Distance: Integer; Name: String): Variant;
var
  MemorySpace: TMemorySpace;
  Index: LongInt = -1;
begin
  MemorySpace := MemorySpaceAt(Distance);
  if MemorySpace.Find(Name, Index) then
    Result := MemorySpace.At(Index)
  else
    Result := Unassigned;
end;

function TMemorySpace.LoadAt(Distance: Integer; Ident: TIdent): Variant;
begin
  Result := LoadAt(Distance, Ident.Text);
end;

procedure TMemorySpace.Update(Name: String; Value: Variant; Token: TToken);
var
  i: LongInt = -1;
begin
  if Find(Name, i) then
    Data[i] := Value
  else if Assigned(FEnclosingSpace) then
    FEnclosingSpace.Update(Name, Value, Token)
  else
    Raise ERuntimeError.Create(Token, Format(ErrVarUndefined, [Name]));
end;

procedure TMemorySpace.Update(Ident: TIdent; Value: Variant);
begin
  Update(Ident.Text, Value, Ident.Token);
end;

procedure TMemorySpace.UpdateAt(Distance: Integer; Ident: TIdent; Value: Variant);
begin
  MemorySpaceAt(Distance)[Ident.Text] := Value
end;

function TMemorySpace.MemorySpaceAt(Distance: Integer): TMemorySpace;
var
  MemorySpace: TMemorySpace;
  i: Integer;
begin
  MemorySpace := Self;
  for i := 1 to Distance do
    MemorySpace := MemorySpace.EnclosingSpace;
  Result := MemorySpace;
end;

end.

