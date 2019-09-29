unit uArray;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uAST, uError, uToken, Variants, uClass, uInterpreter,
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
      function getTypeName: String;
      function getArrayClass: IArrayable;
      function Get(i: Integer; Token: TToken): Variant;
      procedure Put(i: Integer; Value: Variant; Token: TToken);
      // array standard functions
      procedure Init(const ALength: Integer; const AFillChar: Variant);
      procedure Add(const AValue: Variant);
      procedure AddList(constref AList: TArrayElements);
      procedure Insert(const AIndex: Integer; const AValue: Variant; Token: TToken);
      procedure Delete(const AIndex: Integer; Token: TToken);
      function Remove(const AValue: Variant; Token: TToken): Variant;
      function Extract(const AValue: Variant; Token: TToken): Variant;
      function Contains(const AValue: Variant; Token: TToken): Boolean;
      function IndexOf(const AValue: Variant; Token: TToken): Variant;
      function Retrieve(const AIndex: Integer; Token: TToken): Variant;
      procedure Clear;
      function First: Variant;
      function Last: Variant;
  end;

implementation
uses uFunc , uVariantSupport;

function TArrayClass.FindMethod(Instance: IArrayInstance; const AName: String
  ): ICallable;
begin
  Result := Nil;
  if Methods.ContainsKey(AName) then
    Result := (ICallable(Methods[AName]) as TFunc).Bind(Instance);
end;

function TArrayClass.FindValuable(const AName: String): IValuable;
begin
  Result := Nil;
  if Values.ContainsKey(AName) then
    Result := Values[AName]
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
    Instance.Elements.AddRange(Self.Elements)
  else if ArgList.Count > 1 then
    for i := 0 to ArgList.Count-1 do
      Instance.Elements.Add(ArgList[i].Value)
  else if VarSupports(ArgList[0].Value, IArrayInstance) then
    Instance.Elements.AddRange(IArrayInstance(ArgList[0].Value).Elements)
  else if VarIsStr(ArgList[0].Value) then
    for i := 1 to Length(String(ArgList[0].Value)) do
      Instance.Elements.Add(String(ArgList[0].Value)[i])
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
  Key: String;
begin
  for Key in Members.Methods.Keys do
    Methods.AddOrSetValue(Key, Members.Methods[Key]);
  for Key in Members.Values.Keys do
    Values.AddOrSetValue(Key, Members.Values[Key]);
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

function TArrayInstance.getTypeName: String;
begin
  Result := ArrayClass.Ident.Text;
end;

function TArrayInstance.getArrayClass: IArrayable;
begin
  Result := IArrayable(ArrayClass);
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

procedure TArrayInstance.Init(const ALength: Integer; const AFillChar: Variant);
var
  i: Integer;
begin
  for i := 1 to ALength do
    FElements.Add(AFillChar);
end;

procedure TArrayInstance.Add(const AValue: Variant);
begin
  FElements.Add(AValue);
end;

procedure TArrayInstance.AddList(constref AList: TArrayElements);
begin
  FElements.AddRange(AList);
end;

procedure TArrayInstance.Insert(const AIndex: Integer; const AValue: Variant;
  Token: TToken);
begin
  if (AIndex >= 0) and (AIndex <= getCount) then
    FElements.Insert(AIndex, AValue)
  else
    Raise ERuntimeError.Create(Token,
      'Index ('+ IntToStr(AIndex) + ') out of range ('+ '0,' +
      IntToStr(getCount) + ') during insert action.');
end;

procedure TArrayInstance.Delete(const AIndex: Integer; Token: TToken);
begin
  if (AIndex >= 0) and (AIndex < getCount) then
    FElements.Delete(AIndex)
  else
    Raise ERuntimeError.Create(Token,
      'Index ('+ IntToStr(AIndex) + ') out of range ('+ '0,' +
      IntToStr(getCount-1) + ') during delete action.');
end;

function TArrayInstance.Remove(const AValue: Variant; Token: TToken): Variant;
begin
  Result := FElements.Find(AValue, Token);
  if Result >= 0 then
    FElements.Delete(Result)
  else
    Result := Unassigned;
end;

function TArrayInstance.Extract(const AValue: Variant; Token: TToken): Variant;
begin
  if FElements.Find(AValue, Token) >= 0 then
    Result := FElements.Extract(AValue)
  else
    Result := Unassigned;
end;

function TArrayInstance.Contains(const AValue: Variant; Token: TToken): Boolean;
begin
  Result := FElements.Find(AValue, Token) >= 0;
end;

function TArrayInstance.IndexOf(const AValue: Variant; Token: TToken): Variant;
begin
  Result := FElements.Find(AValue, Token);
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

procedure TArrayInstance.Clear;
begin
  FElements.Clear;
end;

function TArrayInstance.First: Variant;
begin
  if getCount>0 then
    Result := FElements[0]
  else
    Result := Unassigned;
end;

function TArrayInstance.Last: Variant;
begin
  if getCount>0 then
    Result := FElements[Pred(getCount)]
  else
    Result := Unassigned;
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

end.

