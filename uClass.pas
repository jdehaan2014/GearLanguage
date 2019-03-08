unit uClass;

{ This unit contains the classes for TGearClass and TGearInstance.

  This code is based on the online book 'Crafting Interpreters',
  written by Bob Nystrom. http://craftinginterpreters.com
  The code is originally written in Java.

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
  Classes, SysUtils, uAST, uCallable, uClassIntf, uInterpreter, uToken,
  uMembers, uError;

type

  TGearClass = class(TInterfacedObject, IClassable)
    private
      Ident: TIdent;
      Parent: Variant;
      Methods: TMethodTable;
      Fields: TFieldTable;
      Constants: TConstTable;
      Values: TValueTable;
      function getName: String;
    public
      property Name: String read getName;
      constructor Create(AIdent: TIdent; AParent: Variant; Members: TMembers);
      function Call(Token: TToken; Interpreter: TInterpreter;
        ArgList: TArgList): Variant;
      function FindMethod(Instance: IGearInstance; const AName: String): ICallable;
      function FindValue(const AName: String): IValuable;
      function toString: String; override;
      procedure ExtendWith(Members: TMembers);
  end;

  TGearInstance = class(TInterfacedObject, IGearInstance)
    private
      GearClass: TGearClass;
      InstanceFields: TFieldTable;
      InstanceConsts: TConstTable;
      function getClassName: String;
    public
      property ClassName: String read getClassName;
      constructor Create(AGearClass: TGearClass);
      function toString: String; override;
      function GetMember(Ident: TIdent): Variant;
      procedure SetField(Ident: TIdent; Value: Variant);
      function isConstant(Ident: TIdent): Boolean;
  end;

  TGearTrait = class(TInterfacedObject, ITraitable)
    private
      FIdent: TIdent;
      FMethods: TMethodTable;
      FValues: TValueTable;
      function getIdent: TIdent;
      function getMethods: TMethodTable;
      function getValues: TValueTable;
    public
      property Ident: TIdent read getIdent;
      property Methods: TMethodTable read getMethods;
      property Values: TValueTable read getValues;
      constructor Create(AIdent: TIdent; Members: TMembers);
      function toString: String; override;
  end;


implementation
uses uFunc, Variants;

{ TGearClass }

function TGearClass.getName: String;
begin
  Result := Ident.Text;
end;

constructor TGearClass.Create
  (AIdent: TIdent; AParent: Variant; Members: TMembers);
var
  ParentClass: TGearClass;
  i: Integer;
begin
  Ident := AIdent;
  Parent := AParent;
  Methods := Members.Methods;
  Fields := Members.Fields;
  Constants := Members.Constants;
  Values := Members.Values;
  if not VarIsNull(Parent) then begin
    ParentClass := ICallable(Parent) as TGearClass;
    for i := 0 to ParentClass.Fields.Count-1 do begin
      if not Fields.Contains(ParentClass.Fields.Keys[i]) then
        Fields[ParentClass.Fields.Keys[i]] := ParentClass.Fields.Data[i];
    end;
    for i := 0 to ParentClass.Constants.Count-1 do begin
      if not Constants.Contains(ParentClass.Constants.Keys[i]) then
        Constants[ParentClass.Constants.Keys[i]] := ParentClass.Constants.Data[i];
    end;
  end;
end;

function TGearClass.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: IGearInstance;
  Init: TFunc;
  Index: LongInt = -1;
  Arity: Integer = 0;
begin
  Instance := IGearInstance(TGearInstance.Create(Self));  // cast to IGearInstance

  if Methods.Contains('init', Index) then begin
    Init := ICallable(Methods.At(Index)) as TFunc;
    Arity := Init.FuncDecl.Params.Count; // Get arity of init(params)

    if (ArgList.Count = 0) and (Arity > 0) then  // Is init called or not
      Exit(Instance)  // init is not called, but Class() directly
    else
      ICallable(Init.Bind(Instance)).Call(Token, Interpreter, ArgList);
  end;

  Result := Instance;
end;

function TGearClass.toString: String;
begin
  Result := Ident.Text;
end;

procedure TGearClass.ExtendWith(Members: TMembers);
var
  i: Integer;
begin
  for i := 0 to Members.Methods.Count-1 do
    Methods[Members.Methods.Keys[i]] := Members.Methods.Data[i];
  for i := 0 to Members.Values.Count-1 do
    Values[Members.Values.Keys[i]] := Members.Values.Data[i];
end;

function TGearClass.FindMethod(Instance: IGearInstance;
  const AName: String): ICallable;
var
  Index: integer = -1;
begin
  Result := Nil;
  if Methods.Contains(AName, Index) then
    Result := (ICallable(Methods.At(Index)) as TFunc).Bind(Instance)
  else if not VarIsNull(Parent) then
    Result := (ICallable(Parent) as TGearClass).FindMethod(Instance, AName);
end;

function TGearClass.FindValue(const AName: String): IValuable;
var
  Index: LongInt = -1;
begin
  Result := Nil;
  if Values.Contains(AName, Index) then
    Result := Values.At(Index)
  else if not VarIsNull(Parent) then
    Result := (ICallable(Parent) as TGearClass).FindValue(AName);
end;

{ TGearInstance }

function TGearInstance.getClassName: String;
begin
  Result := GearClass.Ident.Text;
end;

constructor TGearInstance.Create(AGearClass: TGearClass);
var
  i: Integer;
begin
  GearClass := AGearClass;
  InstanceFields := TFieldTable.Create();
  InstanceFields.Sorted := True;
  for i := 0 to GearClass.Fields.Count-1 do
    InstanceFields.Add(GearClass.Fields.Keys[i], GearClass.Fields.Data[i]);
  InstanceConsts := TConstTable.Create();
  InstanceConsts.Sorted := True;
  for i := 0 to GearClass.Constants.Count-1 do
    InstanceConsts.Add(GearClass.Constants.Keys[i], GearClass.Constants.Data[i]);
  InstanceConsts['className'] := GearClass.Ident.Text;
end;

function TGearInstance.toString: String;
begin
  Result := GearClass.Ident.Text + ' instance';
end;

function TGearInstance.GetMember(Ident: TIdent): Variant;
var
  i: LongInt = -1;
  Method: ICallable;
  Value: IValuable;
begin
  if InstanceFields.Contains(Ident.Text, i) then
    Exit(InstanceFields.At(i));

  if InstanceConsts.Contains(Ident.Text, i) then
    Exit(InstanceConsts.At(i));

  Method := GearClass.FindMethod(Self, Ident.Text);
  if Method <> Nil then
    Exit(Method);

  Value := GearClass.FindValue(Ident.Text);
  if Value <> Nil then
    Exit(Value);

  Raise ERuntimeError.Create(Ident.Token,
    'Undefined class member "' + Ident.Text + '".');
end;

procedure TGearInstance.SetField(Ident: TIdent; Value: Variant);
begin
  InstanceFields[Ident.Text] := Value;
end;

function TGearInstance.isConstant(Ident: TIdent): Boolean;
begin
  Result := InstanceConsts.Contains(Ident.Text);
end;

{ TGearTrait }

function TGearTrait.getIdent: TIdent;
begin
  Result := FIdent;
end;

function TGearTrait.getMethods: TMethodTable;
begin
  Result := FMethods;
end;

function TGearTrait.getValues: TValueTable;
begin
  Result := FValues;
end;

constructor TGearTrait.Create(AIdent: TIdent; Members: TMembers);
begin
  FIdent := AIdent;
  FMethods := Members.Methods;
  FValues := Members.Values;
end;

function TGearTrait.toString: String;
begin
  Result := FIdent.Text;
end;

end.

