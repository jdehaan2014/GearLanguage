unit uMemory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Variants, uAST, uToken, uError;

type

  TMemorySpace = class(specialize TDictionary<String, Variant>)
    private
      FEnclosingSpace: TMemorySpace;
      function MemorySpaceAt(Distance: Integer): TMemorySpace;
    public
      property EnclosingSpace: TMemorySpace read FEnclosingSpace;
      constructor Create(AEnclosingSpace: TMemorySpace = Nil);
      procedure Store(Name: String; Value: Variant; Token: TToken);
      procedure Store(Ident: TIdent; Value: Variant);
      function Load(Name: String; Token: TToken): Variant;
      function Load(Ident: TIdent): Variant;
      procedure Update(Name: String; Value: Variant; Token: TToken);
      procedure Update(Ident: TIdent; Value: Variant);
      // Distance functions
      function LoadAt(Distance: Integer; Name: String): Variant;
      function LoadAt(Distance: Integer; Ident: TIdent): Variant;
      procedure UpdateAt(Distance: Integer; Ident: TIdent; Value: Variant);
  end;

implementation

const
  ErrVarUndefined = 'Variable "%s" is undefined.';
  ErrVarDefined = 'Variable "%s" is already defined.';


constructor TMemorySpace.Create(AEnclosingSpace: TMemorySpace);
begin
  inherited Create;
  FEnclosingSpace := AEnclosingSpace;
end;

procedure TMemorySpace.Store(Name: String; Value: Variant; Token: TToken);
begin
  if not ContainsKey(Name) then
    Add(Name, Value)
  else
    Raise ERuntimeError.Create(Token, Format(ErrVarDefined, [Name]));
end;

procedure TMemorySpace.Store(Ident: TIdent; Value: Variant);
begin
  Store(Ident.Text, Value, Ident.Token);
end;

function TMemorySpace.Load(Name: String; Token: TToken): Variant;
begin
  if ContainsKey(Name) then
    Result := Items[Name]
  else if Assigned(FEnclosingSpace) then
    Result := FEnclosingSpace.Load(Name, Token)
  else
    Raise ERuntimeError.Create(Token, Format(ErrVarUndefined, [Name]));
end;

function TMemorySpace.Load(Ident: TIdent): Variant;
begin
  Result := Load(Ident.Text, Ident.Token);
end;

procedure TMemorySpace.Update(Name: String; Value: Variant; Token: TToken);
begin
  if ContainsKey(Name) then
    Items[Name] := Value
  else if Assigned(FEnclosingSpace) then
    FEnclosingSpace.Update(Name, Value, Token)
  else
    Raise ERuntimeError.Create(Token, Format(ErrVarUndefined, [Name]));
end;

procedure TMemorySpace.Update(Ident: TIdent; Value: Variant);
begin
  Update(Ident.Text, Value, Ident.Token);
end;

{ Distance functions }

function TMemorySpace.LoadAt(Distance: Integer; Name: String): Variant;
var
  MemorySpace: TMemorySpace;
begin
  MemorySpace := MemorySpaceAt(Distance);
  if not MemorySpace.TryGetValue(Name, Result) then
    Result := Unassigned;
end;

function TMemorySpace.LoadAt(Distance: Integer; Ident: TIdent): Variant;
begin
  Result := LoadAt(Distance, Ident.Text);
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

procedure TMemorySpace.UpdateAt(Distance: Integer; Ident: TIdent; Value: Variant);
begin
  MemorySpaceAt(Distance)[Ident.Text] := Value
end;

end.

