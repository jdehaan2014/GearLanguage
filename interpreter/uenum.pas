unit uEnum;

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
uses uFunc, uVariantSupport, uLanguage, uArray;

{ TEnumClass }

function TEnumClass.FindMethod(Instance: IEnumInstance; const AName: String): ICallable;
begin
  Result := Nil;
  if Methods.ContainsKey(AName) then
    Result := (ICallable(Methods[AName]) as TFunc).Bind(Instance);
end;

function TEnumClass.FindValuable(const AName: String): IValuable;
begin
  Result := Nil;
  if Values.ContainsKey(AName) then
    Result := Values[AName]
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
  Elements.Free;
  inherited Destroy;
end;

function TEnumClass.toString: string;
begin
  Result := Ident.Text;
end;

procedure TEnumClass.ExtendWith(Members: TMembers);
var
  Key: String;
begin
  for Key in Members.Methods.Keys do
    Methods.AddOrSetValue(Key, Members.Methods[Key]);
  for Key in Members.Values.Keys do
    Values.AddOrSetValue(Key, Members.Values[Key]);
end;

function TEnumClass.isCase(Name: String): Boolean;
begin
  Result := CaseTable.Contains(Name);
end;

function TEnumClass.ElementList: IArrayInstance;
var
  ArrayType: IArrayable;
  Key: String;
  EnumIdent: TIdent;
begin
  // create a array/list of all items
  ArrayType := IArrayable(Language.Interpreter.Globals['Array']);
  Result := IArrayInstance(TArrayInstance.Create(ArrayType as TArrayClass));
  for Key in Elements.Keys do begin
    EnumIdent := TIdent.Create(TToken.Create(ttIdentifier, Key, Null, 0, 0));
    Result.Elements.Add(IEnumInstance(TEnumInstance.Create(Self, EnumIdent)));
  end;
end;

{ TEnumInstance }

constructor TEnumInstance.Create(AEnumClass: TEnumClass; Ident: TIdent);
begin
  EnumClass := AEnumClass;
  Name := '';
  SetName := '';
  Fields := TFieldTable.Create;
  if EnumClass.Elements.ContainsKey(Ident.Text) then begin
    Name := Ident.Text;
    Value := EnumClass.Elements[Name].Value;
    SetName := EnumClass.Elements[Name].SetName;
    Fields.Add('name', Name);
    Fields.Add('value', Value);
    Fields.Add('set', SetName);
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
begin
  if Fields.ContainsKey(Ident.Text) then
    Exit(Fields[Ident.Text]);

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

