unit uClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uAST, uCallable, uClassIntf, uInterpreter, uToken, uError,
  uMembers;

type

  TGearClass = class(TInterfacedObject, IClassable)
    private
      Ident: TIdent;
      Methods: TMethodTable;
      Fields: TFieldTable;
      Constants: TConstTable;
      Values: TValueTable;
      StaticMethods: TMethodTable;
      Parent: Variant;
      function getName: String;
    public
      property Name: String read getName;
      constructor Create(AIdent: TIdent; AParent: Variant;
        Members, StaticMembers: TMembers);
      function Call(Token: TToken; Interpreter: TInterpreter;
        ArgList: TArgList): Variant;
      function toString: String; override;
      function FindMethod(Instance: IGearInstance; const AName: String): ICallable;
      function FindValue(const AName: String): IValuable;
      function GetStaticMember(AIdent: TIdent): Variant;
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
      function getMethod(const Name: String): Variant;
  end;

  TGearTrait = class(TInterfacedObject, ITraitable)
    private
      FIdent: TIdent;
      FMethods: TMethodTable;
      function getIdent: TIdent;
      function getMethods: TMethodTable;
    public
      property Ident: TIdent read getIdent;
      property Methods: TMethodTable read getMethods;
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

constructor TGearClass.Create(AIdent: TIdent; AParent: Variant; Members,
  StaticMembers: TMembers);
var
  ParentClass: TGearClass;
  Key: String;
begin
  Ident := AIdent;
  Methods := Members.Methods;
  Fields := Members.Fields;
  Constants := Members.Constants;
  Values := Members.Values;
  StaticMethods := StaticMembers.Methods;
  Parent := AParent;
  if not VarIsNull(Parent) then begin
    ParentClass := IClassable(Parent) as TGearClass;
    for Key in ParentClass.Fields.Keys do
      if not Fields.ContainsKey(Key) then
        Fields.AddOrSetValue(Key, ParentClass.Fields[Key]);
    for Key in ParentClass.Constants.Keys do
      if not Constants.ContainsKey(Key) then
        Constants.AddOrSetValue(Key, ParentClass.Constants[Key]);
  end;
end;

function TGearClass.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
const
  InitName = 'init';
var
  Instance: IGearInstance;
  Init: TFunc;
  Arity: Integer=0;
begin
  Instance := IGearInstance(TGearInstance.Create(Self));  // cast to IGearInstance
  if Methods.ContainsKey(InitName) then begin
    Init := ICallable(Methods[InitName]) as TFunc;
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

function TGearClass.FindMethod(Instance: IGearInstance;
  const AName: String): ICallable;
begin
  Result := Nil;
  if Methods.ContainsKey(AName) then
    Result := (ICallable(Methods[AName]) as TFunc).Bind(Instance)
  else if not VarIsNull(Parent) then
    Result := (ICallable(Parent) as TGearClass).FindMethod(Instance, AName);
end;

function TGearClass.FindValue(const AName: String): IValuable;
begin
  Result := Nil;
  if Values.ContainsKey(AName) then
    Result := Values[AName]
  else if not VarIsNull(Parent) then
    Result := (ICallable(Parent) as TGearClass).FindValue(AName);
end;

function TGearClass.GetStaticMember(AIdent: TIdent): Variant;
var
  Method: ICallable;
begin
  if StaticMethods.ContainsKey(AIdent.Text) then
    Result := ICallable(StaticMethods[AIdent.Text])
  else
    Raise ERuntimeError.Create(AIdent.Token,
      'Undefined static class member "' + AIdent.Text + '".');
end;

procedure TGearClass.ExtendWith(Members: TMembers);
var
  Key: String;
begin
  for Key in Members.Methods.Keys do
    Methods.AddOrSetValue(Key, Members.Methods[Key]);
  for Key in Members.Values.Keys do
    Values.AddOrSetValue(Key, Members.Values[Key]);
end;

{ TGearInstance }

function TGearInstance.getClassName: String;
begin
  Result := GearClass.Ident.Text;
end;

constructor TGearInstance.Create(AGearClass: TGearClass);
var
  Key: String;
begin
  GearClass := AGearClass;
  InstanceFields := TFieldTable.Create();
  for Key in GearClass.Fields.Keys do
    InstanceFields.Add(Key, GearClass.Fields[Key]);
  InstanceConsts := TConstTable.Create();
  InstanceConsts.Add('className', GearClass.Ident.Text);
  for Key in GearClass.Constants.Keys do
    InstanceConsts.Add(Key, GearClass.Constants[Key]);
end;

function TGearInstance.toString: String;
begin
  Result := GearClass.Ident.Text + ' instance';
end;

function TGearInstance.GetMember(Ident: TIdent): Variant;
var
  Method: ICallable;
  Value: IValuable;
begin
  if InstanceFields.ContainsKey(Ident.Text) then
    Exit(InstanceFields[Ident.Text]);

  if InstanceConsts.ContainsKey(Ident.Text) then
    Exit(InstanceConsts[Ident.Text]);

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
  InstanceFields.AddOrSetValue(Ident.Text, Value);
end;

function TGearInstance.isConstant(Ident: TIdent): Boolean;
begin
  Result := InstanceConsts.ContainsKey(Ident.Text);
end;

function TGearInstance.getMethod(const Name: String): Variant;
var
  Method: ICallable;
begin
  Method := GearClass.FindMethod(Self, Name);
  if Method <> Nil then
    Exit(Method);
  Result := Null;
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

constructor TGearTrait.Create(AIdent: TIdent; Members: TMembers);
begin
  FIdent := AIdent;
  FMethods := Members.Methods;
end;

function TGearTrait.toString: String;
begin
  Result := FIdent.Text;
end;

end.

