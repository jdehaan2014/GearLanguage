unit uDict;

{ This unit contains the classes for TDictClass and TDictInstance.

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
  Classes, SysUtils, uAST, uError, uToken, Variants, uInterpreter,
  uCallable, uDictIntf, uMembers;

type

  TDictClass = class(TInterfacedObject, IDictionable)
    private
      Ident: TIdent;
      Elements: TDictElements;
      Methods: TMethodTable;
      Values: TValueTable;
      function FindMethod(Instance: IDictInstance; const AName: String): ICallable;
      function FindValuable(const AName: String): IValuable;
    public
      constructor Create(AIdent: TIdent; AElements: TDictElements;
        Members: TMembers);
      function Call(Token: TToken; Interpreter: TInterpreter;
        ArgList: TArgList): Variant;
      function toString: string; override;
      procedure ExtendWith(Members: TMembers);
  end;

  TDictInstance = class(TInterfacedObject, IDictInstance)
    private
      DictClass: TDictClass;
      FElements: TDictElements;
      function getCount: LongInt;
      function getElements: TDictElements;
      function getItem(i: integer): Variant;
      procedure setItem(i: integer; AValue: Variant);
    public
      property Elements: TDictElements read getElements;
      property Count: LongInt read getCount;
      property Items[i:integer]: Variant read getItem write setItem; default;
      constructor Create(ADictClass: TDictClass);
      destructor Destroy; override;
      function toString: string; override;
      function GetMember(Ident: TIdent): Variant;
      function TypeName: String;
      function Get(Key: Variant; Token: TToken): Variant;
      procedure Put(Key: Variant; Value: Variant; Token: TToken);
      procedure Add(const AKey, AValue: Variant);
      procedure Insert(const i: Integer; const AKey, AValue: Variant; Token: TToken);
      procedure Delete(const AValue: Variant; Token: TToken);
      function Contains(const AKey: Variant): Boolean;
      function IndexOf(const AKey: Variant): Variant;
      function Retrieve(const AKey: Variant; Token: TToken): Variant;
      function Keys: Variant;
      function Values: Variant;
  end;

implementation
uses uFunc, uVariantSupport, uArrayIntf, uArray, uLanguage;

{ TDictClass }

function TDictClass.FindMethod(Instance: IDictInstance; const AName: String
  ): ICallable;
var
  Index: integer = -1;
begin
  Result := Nil;
  if Methods.Contains(AName, Index) then
    Result := (ICallable(Methods.At(Index)) as TFunc).Bind(Instance)
end;

function TDictClass.FindValuable(const AName: String): IValuable;
var
  Index: LongInt = -1;
begin
  Result := Nil;
  if Values.Contains(AName, Index) then
    Result := Values.At(Index);
end;

constructor TDictClass.Create
  (AIdent: TIdent; AElements: TDictElements; Members: TMembers);
begin
  Ident := AIdent;
  Elements := AElements;
  Methods := Members.Methods;
  Values := Members.Values;
end;

function TDictClass.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: IDictInstance;
begin
  Instance := IDictInstance(TDictInstance.Create(Self));
  if ArgList.Count = 0 then
    Instance.Elements.Assign(Self.Elements)
  else if VarSupports(ArgList[0].Value, IDictInstance) then
    Instance.Elements.Assign(IDictInstance(ArgList[0].Value).Elements)
  else
    Raise ERuntimeError.Create(Token,
      'Dictionary instance of this type not allowed.');
  Result := Instance;
end;

function TDictClass.toString: string;
begin
  Result := Ident.Text;
end;

procedure TDictClass.ExtendWith(Members: TMembers);
var
  i: Integer;
begin
  for i := 0 to Members.Methods.Count-1 do
    Methods[Members.Methods.Keys[i]] := Members.Methods.Data[i];
  for i := 0 to Members.Values.Count-1 do
    Values[Members.Values.Keys[i]] := Members.Values.Data[i];
end;

{ TDictInstance }

function TDictInstance.getCount: LongInt;
begin
  Result := FElements.Count;
end;

function TDictInstance.getElements: TDictElements;
begin
  Result := FElements;
end;

function TDictInstance.getItem(i: integer): Variant;
begin
  Result := FElements[i];
end;

procedure TDictInstance.setItem(i: integer; AValue: Variant);
begin
  FElements[i] := AValue;
end;

constructor TDictInstance.Create(ADictClass: TDictClass);
begin
  DictClass := ADictClass;
  FElements := TDictElements.Create;
end;

destructor TDictInstance.Destroy;
begin
  FElements.Free;
  inherited Destroy;
end;

function TDictInstance.toString: string;
var
  i: Integer;

  function getStr(Value: Variant): String;
  begin
    Result := Value.toString;
    if VarType(Value) = varString then
      Result := QuotedStr(Result);
  end;

begin
  Result := ' [';
  if FElements.Count > 0 then begin
    for i := 0 to FElements.Count-2 do
      Result += getStr(FElements.Keys[i]) + ': ' +
                getStr(FElements.Data[i]) + ', ';
    Result += getStr(FElements.Keys[FElements.Count-1]) + ': ' +
              getStr(FElements.Data[FElements.Count-1]);
  end
  else Result += ':';
  Result += ']';
end;

function TDictInstance.GetMember(Ident: TIdent): Variant;
var
  Method: ICallable;
  Valuable: IValuable;
begin
  Method := DictClass.FindMethod(IDictInstance(Self), Ident.Text);
  if Method <> Nil then
    Exit(Method);

  Valuable := DictClass.FindValuable(Ident.Text);
  if Valuable <> Nil then
    Exit(Valuable);

  Raise ERuntimeError.Create(Ident.Token,
    'Undefined dictionary member "' + Ident.Text + '".');
end;

function TDictInstance.TypeName: String;
begin
  Result := DictClass.Ident.Text;
end;

function TDictInstance.Get(Key: Variant; Token: TToken): Variant;
begin
  if FElements.Contains(Key) then
    Result := FElements[Key]
  else
    Raise ERuntimeError.Create(Token,
      'Key ('+ Key.toString + ') not found.');
end;

procedure TDictInstance.Put(Key: Variant; Value: Variant; Token: TToken);
begin
  try
    FElements[Key] := Value
  except
    Raise ERuntimeError.Create(Token,
      'Key:Value ('+ Key.toString + ', ' + Value.toString + ') wrong.');
  end;
end;

procedure TDictInstance.Add(const AKey, AValue: Variant);
begin
  FElements.Add(AKey, AValue);
end;

procedure TDictInstance.Insert(const i: Integer;
  const AKey, AValue: Variant; Token: TToken);
begin
  if (i >= 0) and (i <= getCount) then
    FElements.InsertKeyData(i, AKey, AValue)
  else
    Raise ERuntimeError.Create(Token,
      'Index ('+ IntToStr(i) + ') out of range ('+ '0,' +
      IntToStr(getCount-1) + ') during insert action.');
end;

procedure TDictInstance.Delete(const AValue: Variant; Token: TToken);
var
  i: Integer;
begin
  i := FElements.Remove(AValue);
  if i<0 then
    Raise ERuntimeError.Create(Token,
      'Value "' + AValue.toString + '" not found in delete action.');
end;

function TDictInstance.Contains(const AKey: Variant): Boolean;
begin
  Result := FElements.Contains(AKey);
end;

function TDictInstance.IndexOf(const AKey: Variant): Variant;
begin
  Result := FElements.IndexOf(AKey);
  if Result < 0 then
    Result := Unassigned;
end;

function TDictInstance.Retrieve(const AKey: Variant; Token: TToken): Variant;
begin
  if FElements.Contains(AKey) then
    Result := FElements[AKey]
  else
    Result := Unassigned;
end;

function TDictInstance.Keys: Variant;
var
  ArrayType: IArrayable;
  Instance: IArrayInstance;
  i: Integer;
begin
  ArrayType := IArrayable(Language.Interpreter.Globals['Array']);

  Instance := IArrayInstance(TArrayInstance.Create(ArrayType as TArrayClass));
  for i := 0 to Elements.Count-1 do
    Instance.Elements.Add(Elements.Keys[i]);
  Result := Instance;
end;

function TDictInstance.Values: Variant;
var
  ArrayType: IArrayable;
  Instance: IArrayInstance;
  i: Integer;
begin
  ArrayType := IArrayable(Language.Interpreter.Globals['Array']);

  Instance := IArrayInstance(TArrayInstance.Create(ArrayType as TArrayClass));
  for i := 0 to Elements.Count-1 do
    Instance.Elements.Add(Elements.Data[i]);
  Result := Instance;
end;

end.

