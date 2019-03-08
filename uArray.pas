unit uArray;

{ This unit contains the classes for TArrayClass and TArrayInstance.

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
  Classes, SysUtils, uAST, uError, uToken, Variants, uInterpreter,
  uCallable, uArrayIntf, uMembers;

type

  TArrayClass = class(TInterfacedObject, IArrayable)
    private
      Ident: TIdent;
      Elements: TArrayElements;
      Methods: TMethodTable;
      Values: TValueTable;
      function FindMethod(Instance: IArrayInstance; const AName: String): ICallable;
      function FindValuable(const AName: String): IValuable;
    public
      constructor Create(AIdent: TIdent; AElements: TArrayElements;
        Members: TMembers);
      function Call(Token: TToken; Interpreter: TInterpreter;
        ArgList: TArgList): Variant;
      function toString: string; override;
      procedure ExtendWith(Members: TMembers);
  end;

  TArrayInstance = class(TInterfacedObject, IArrayInstance)
    private
      ArrayClass: TArrayClass;
      FElements: TArrayElements;
      function getCount: LongInt;
      function getElements: TArrayElements;
      function getItem(i: integer): Variant;
      procedure setItem(i: integer; AValue: Variant);
    public
      property Elements: TArrayElements read getElements;
      property Count: LongInt read getCount;
      property Items[i:integer]: Variant read getItem write setItem; default;
      constructor Create(AnArrayClass: TArrayClass);
      destructor Destroy; override;
      function toString: string; override;
      function GetMember(Ident: TIdent): Variant;
      function TypeName: String;
      function Get(i: Integer; Token: TToken): Variant;
      procedure Put(i: Integer; Value: Variant; Token: TToken);
      procedure Add(const AValue: Variant);
      procedure Insert(const i: Integer; const AValue: Variant; Token: TToken);
      procedure Delete(const AValue: Variant; Token: TToken);
      function Contains(const AValue: Variant): Boolean;
      function IndexOf(const AValue: Variant): Variant;
      function Retrieve(const AIndex: Integer; Token: TToken): Variant;
  end;

implementation
uses uFunc, uVariantSupport;

function TArrayClass.FindMethod(Instance: IArrayInstance; const AName: String
  ): ICallable;
var
  Index: integer = -1;
begin
  Result := Nil;
  if Methods.Contains(AName, Index) then
    Result := (ICallable(Methods.At(Index)) as TFunc).Bind(Instance)
end;

function TArrayClass.FindValuable(const AName: String): IValuable;
var
  Index: LongInt = -1;
begin
  Result := Nil;
  if Values.Contains(AName, Index) then
    Result := Values.At(Index);
end;

constructor TArrayClass.Create
  (AIdent: TIdent; AElements: TArrayElements; Members: TMembers);
begin
  Ident := AIdent;
  Elements := AElements;
  Methods := Members.Methods;
  Values := Members.Values;
end;

function TArrayClass.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
var
  Instance: IArrayInstance;
  i: Integer;
begin
  Instance := IArrayInstance(TArrayInstance.Create(Self));

  if ArgList.Count = 0 then
    for i := 0 to Self.Elements.Count-1 do
      Instance.Elements.Add(Self.Elements[i])
  else if ArgList.Count > 1 then
    for i := 0 to ArgList.Count-1 do
      Instance.Elements.Add(ArgList[i].Value)
  else if VarSupports(ArgList[0].Value, IArrayInstance) then
    Instance.Elements.Assign(IArrayInstance(ArgList[0].Value).Elements)
  else if VarIsStr(ArgList[0].Value) then
    for i := 1 to Length(String(ArgList[0].Value)) do
      Instance.Elements.Add(String(ArgList[0].Value)[i])
  else if VarisNumeric(ArgList[0].Value) then
    for i := 1 to ArgList[0].Value do
      Instance.Elements.Add(Null)
  else
    Raise ERuntimeError.Create(Token,
      'Array member of this type not allowed.');

  Result := Instance;
end;

function TArrayClass.toString: string;
begin
  Result := Ident.Text;
end;

procedure TArrayClass.ExtendWith(Members: TMembers);
var
  i: Integer;
begin
  for i := 0 to Members.Methods.Count-1 do
    Methods[Members.Methods.Keys[i]] := Members.Methods.Data[i];
  for i := 0 to Members.Values.Count-1 do
    Values[Members.Values.Keys[i]] := Members.Values.Data[i];
end;

{ TArrayInstance }

function TArrayInstance.getCount: LongInt;
begin
  Result := FElements.Count;
end;

function TArrayInstance.getElements: TArrayElements;
begin
  Result := FElements;
end;

function TArrayInstance.getItem(i: integer): Variant;
begin
  Result := FElements[i];
end;

procedure TArrayInstance.setItem(i: integer; AValue: Variant);
begin
  FElements[i] := AValue;
end;

constructor TArrayInstance.Create(AnArrayClass: TArrayClass);
begin
  ArrayClass := AnArrayClass;
  FElements := TArrayElements.Create;
end;

destructor TArrayInstance.Destroy;
begin
  FElements.Free;
  inherited Destroy;
end;

function TArrayInstance.GetMember(Ident: TIdent): Variant;
var
  Method: ICallable;
  Valuable: IValuable;
begin
  Method := ArrayClass.FindMethod(IArrayInstance(Self), Ident.Text);
  if Method <> Nil then
    Exit(Method);

  Valuable := ArrayClass.FindValuable(Ident.Text);
  if Valuable <> Nil then
    Exit(Valuable);

  Raise ERuntimeError.Create(Ident.Token,
    'Undefined array member "' + Ident.Text + '".');
end;

function TArrayInstance.toString: string;
var
  i: Integer;
begin
  Result := '[';
  if FElements.Count > 0 then begin
    for i := 0 to FElements.Count-2 do
      Result += FElements[i].toString + ', ';
    Result += FElements[FElements.Count-1].toString;
  end;
  Result += ']';
end;

function TArrayInstance.TypeName: String;
begin
  Result := ArrayClass.Ident.Text;
end;

function TArrayInstance.Get(i: Integer; Token: TToken): Variant;
begin
  if getCount = 0 then
    Result := Unassigned
  else if (i >= 0) and (i < getCount) then
    Result := FElements[i]
  else
    Raise ERuntimeError.Create(Token,
      'Index ('+ IntToStr(i) + ') out of range ('+ '0,' + IntToStr(getCount-1) + ').');
end;

procedure TArrayInstance.Put(i: Integer; Value: Variant; Token: TToken);
begin
  if (i >= 0) and (i < getCount) then
    FElements[i] := Value
  else
    Raise ERuntimeError.Create(Token,
      'Index ('+ IntToStr(i) + ') out of range ('+ '0,' + IntToStr(getCount-1) + ').');
end;

procedure TArrayInstance.Add(const AValue: Variant);
begin
  FElements.Add(AValue);
end;

procedure TArrayInstance.Insert(const i: Integer;
  const AValue: Variant; Token: TToken);
begin
  if (i >= 0) and (i <= getCount) then
    FElements.Insert(i, AValue)
  else
    Raise ERuntimeError.Create(Token,
      'Index ('+ IntToStr(i) + ') out of range ('+ '0,' +
      IntToStr(getCount) + ') during insert action.');
end;

procedure TArrayInstance.Delete(const AValue: Variant; Token: TToken);
var
  i: Integer;
begin
  i := FElements.Remove(AValue);
  if i<0 then
    Raise ERuntimeError.Create(Token,
      'Value "' + AValue.toString + '" not found in delete action.');
end;

function TArrayInstance.Contains(const AValue: Variant): Boolean;
begin
  Result := FElements.Contains(AValue);
end;

function TArrayInstance.IndexOf(const AValue: Variant): Variant;
begin
  Result := FElements.IndexOf(AValue);
  if Result < 0 then
    Result := Unassigned;
end;

function TArrayInstance.Retrieve(const AIndex: Integer; Token: TToken): Variant;
begin
  if (AIndex >= 0) and (AIndex < getCount) then
    Result := FElements[AIndex]
  else
    Result := Unassigned;
end;

end.

