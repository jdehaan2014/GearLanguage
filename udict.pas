unit uDict;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uAST, uError, uToken, Variants, uInterpreter,
  uCallable, uDictIntf, uMembers, uArrayIntf;

type

  TDictClass = class(TInterfacedObject, IDictionary)
    private
      Ident: TIdent;
      Elements: TDictElements;
      Methods: TMethodTable;
      Values: TValueTable;
      function FindMethod(Instance: IDictInstance; const AName: String): ICallable;
      function FindValuable(const AName: String): IValuable;
    public
      constructor Create(AIdent: TIdent; AElements: TDictElements; Members: TMembers);
      function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
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
      // standard functions
      procedure Add(const AKey, AValue: Variant);
      procedure AddList(constref AList: TDictElements);
      function ContainsKey(const AKey: Variant; Token: TToken): Boolean;
      function ContainsValue(const AValue: Variant; Token: TToken): Boolean;
      function ValueOf(const AKey: Variant; Token: TToken): Variant;
      function KeyOf(const AValue: Variant; Token: TToken): Variant;
      procedure Delete(const AKey: Variant; Token: TToken);
      procedure Sort;  // sort on Keys
      procedure setSorted(Sorted: Boolean=False);
      procedure Clear;
      function Keys: IArrayInstance;
      function Values: IArrayInstance;
  end;


implementation
uses uFunc, uVariantSupport, uLanguage, uArray;

{  TDictClass  }

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
  Instance.Elements.Assign(Self.Elements);
  Result := Instance;
end;

function TDictClass.toString: string;
begin
  Result := Ident.Text;
end;

procedure TDictClass.ExtendWith(Members: TMembers);
var
  Key: String;
begin
  for Key in Members.Methods.Keys do
    Methods.AddOrSetValue(Key, Members.Methods[Key]);
  for Key in Members.Values.Keys do
    Values.AddOrSetValue(Key, Members.Values[Key]);
end;

function TDictClass.FindMethod(Instance: IDictInstance; const AName: String): ICallable;
begin
  Result := Nil;
  if Methods.ContainsKey(AName) then
    Result := (ICallable(Methods[AName]) as TFunc).Bind(Instance);
end;

function TDictClass.FindValuable(const AName: String): IValuable;
begin
  Result := Nil;
  if Values.ContainsKey(AName) then
    Result := Values[AName]
end;


{  TDictInstance  }

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
  Result := '[';
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
  if ContainsKey(Key, Token) then
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

procedure TDictInstance.AddList(constref AList: TDictElements);
begin
  FElements.AddList(AList);
end;

function TDictInstance.ContainsKey(const AKey: Variant; Token: TToken): Boolean;
begin
  Result := FElements.FindKey(AKey, Token) >= 0;
end;

function TDictInstance.ContainsValue(const AValue: Variant; Token: TToken): Boolean;
begin
  Result := FElements.FindValue(AValue, Token) >= 0;
end;

function TDictInstance.ValueOf(const AKey: Variant; Token: TToken): Variant;
var
  i: SizeInt;
begin
  i := FElements.FindKey(AKey, Token);
  if i>=0 then
    Result := FElements.Data[i]
  else
    Result := Unassigned;
end;

function TDictInstance.KeyOf(const AValue: Variant; Token: TToken): Variant;
var
  i: Integer;
begin
  i := FElements.FindValue(AValue, Token);
  if i >= 0 then
    Result := FElements.Keys[i]
  else
    Result := Unassigned;
end;

procedure TDictInstance.Delete(const AKey: Variant; Token: TToken);
var
  i: Integer;
begin
  i := FElements.FindKey(AKey, Token);
  if i>=0 then
    FElements.Delete(i)
  else
    Raise ERuntimeError.Create(Token, 'Key to delete not found.');
end;

procedure TDictInstance.Sort;
begin
  FElements.Sort;
end;

procedure TDictInstance.setSorted(Sorted: Boolean);
begin
  FElements.Sorted := Sorted;
end;

procedure TDictInstance.Clear;
begin
  FElements.Clear;
end;

function TDictInstance.Keys: IArrayInstance;
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

function TDictInstance.Values: IArrayInstance;
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

