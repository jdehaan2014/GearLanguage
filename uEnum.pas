unit uEnum;

{ This unit contains the classes TEnumClass and TEnumInstance.

  Copyright (C) 2019 J. de Haan jdehaan2014@gmail.com

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
  Classes, SysUtils, uAST, uError, uToken, Variants, uCallable,
  uEnumIntf, uMembers, uArrayIntf;

type

  TEnumClass = class(TInterfacedObject, IEnumable)
    private
      Ident: TIdent;
      Elements: TEnumElements;
      Methods: TMethodTable;
      Values: TValueTable;
      CaseTable: TCaseTable;
      function FindMethod(Instance: IEnumInstance; const AName: String): ICallable;
      function FindValuable(const AName: String): IValuable;
    public
      constructor Create(AIdent: TIdent; AElements: TEnumElements;
        ACaseTable: TCaseTable; Members: TMembers);
      destructor Destroy; override;
      function toString: string; override;
      procedure ExtendWith(Members: TMembers);
      function isCase(Name: String): Boolean;
      function ElementList: IArrayInstance;
  end;

  TEnumInstance = class(TInterfacedObject, IEnumInstance)
    private
      EnumClass: TEnumClass;
      Name: String;
      Value: Variant;
      SetName: String;
      Fields: TFieldTable;
    public
      constructor Create(AEnumClass: TEnumClass; Ident: TIdent);
      destructor Destroy; override;
      function toString: string; override;
      function GetMember(Ident: TIdent): Variant;
      function EnumName: String;
      function ElemName: String;
      function ElemValue: Variant;
      function ElemSetName: String;
  end;

implementation
uses uFunc, uLanguage, uArray;

{ TEnumClass }

function TEnumClass.FindMethod(Instance: IEnumInstance; const AName: String
  ): ICallable;
var
  Index: integer = -1;
begin
  Result := Nil;
  if Methods.Contains(AName, Index) then
    Result := (ICallable(Methods.At(Index)) as TFunc).Bind(Instance)
end;

function TEnumClass.FindValuable(const AName: String): IValuable;
var
  Index: LongInt = -1;
begin
  Result := Nil;
  if Values.Contains(AName, Index) then
    Result := Values.At(Index);
end;

constructor TEnumClass.Create(AIdent: TIdent; AElements: TEnumElements;
  ACaseTable: TCaseTable; Members: TMembers);
begin
  Ident := AIdent;
  Elements := AElements;
  Methods := Members.Methods;
  Values := Members.Values;
  CaseTable := ACaseTable;
end;

destructor TEnumClass.Destroy;
begin
  if Assigned(Elements) then Elements.Free;
  inherited Destroy;
end;

function TEnumClass.toString: string;
begin
  Result := Ident.Text;
end;

procedure TEnumClass.ExtendWith(Members: TMembers);
var
  i: Integer;
begin
  for i := 0 to Members.Methods.Count-1 do
    Methods[Members.Methods.Keys[i]] := Members.Methods.Data[i];
  for i := 0 to Members.Values.Count-1 do
    Values[Members.Values.Keys[i]] := Members.Values.Data[i];
end;

function TEnumClass.isCase(Name: String): Boolean;
begin
  Result := CaseTable.Contains(Name);
end;

function TEnumClass.ElementList: IArrayInstance;
var
  ArrayType: IArrayable;
  i: Integer;
  EnumIdent: TIdent;
begin
  // create a array/list of all items
  ArrayType := IArrayable(Language.Interpreter.Globals['Array']);
  Result := IArrayInstance(TArrayInstance.Create(ArrayType as TArrayClass));
  for i := 0 to Elements.Count-1 do begin
    EnumIdent := TIdent.Create(TToken.Create(
      ttIdentifier, Elements.Keys[i], Null, 0, 0));
    Result.Elements.Add(IEnumInstance(TEnumInstance.Create(Self, EnumIdent)));
  end;
end;

{ TEnumInstance }

constructor TEnumInstance.Create(AEnumClass: TEnumClass; Ident: TIdent);
var
  Index: LongInt=-1;
begin
  EnumClass := AEnumClass;
  Name := '';
  SetName := '';
  Fields := TFieldTable.Create;
  if EnumClass.Elements.Contains(Ident.Text, Index) then begin
    Name := Ident.Text;
    Value := EnumClass.Elements.At(Index).Value;
    SetName := EnumClass.Elements.At(Index).SetName;
    Fields['name'] := Name;
    Fields['value'] := Value;
    Fields['set'] := SetName;
  end
  else
    Raise ERuntimeError.Create(Ident.Token,
      'Undefined enum element "' + Ident.Text + '".');
end;

destructor TEnumInstance.Destroy;
begin
  Fields.Free;
  inherited Destroy;
end;

function TEnumInstance.toString: string;
begin
  Result := Name;
end;

function TEnumInstance.GetMember(Ident: TIdent): Variant;
var
  Method: ICallable;
  Valuable: IValuable;
  Index: LongInt = -1;
begin
  if Fields.Contains(Ident.Text, Index) then
    Exit(Fields.At(Index));

  Method := EnumClass.FindMethod(IEnumInstance(Self), Ident.Text);
  if Method <> Nil then
    Exit(Method);

  Valuable := EnumClass.FindValuable(Ident.Text);
  if Valuable <> Nil then
    Exit(Valuable);

  Raise ERuntimeError.Create(Ident.Token,
    'Undefined enum member "' + Ident.Text + '".');
end;

function TEnumInstance.EnumName: String;
begin
  Result := EnumClass.Ident.Text;
end;

function TEnumInstance.ElemName: String;
begin
  Result := Name;
end;

function TEnumInstance.ElemValue: Variant;
begin
  Result := Value;
end;

function TEnumInstance.ElemSetName: String;
begin
  Result := EnumClass.Elements[Name].SetName;
end;

end.

