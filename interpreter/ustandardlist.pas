unit uStandardList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uInterpreter, uCallable, uToken, Variants, uMemory;

type

  TListInit = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TListAdd = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TListAddList = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TListInsert = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TListDelete = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TListRemove = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TListExtract = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TListContains = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TListIndexOf = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TListRetrieve = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TListFirst = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TListLast = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TListClear = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  { for dictionaries the following standard functions:

    procedure Add(const AKey, AValue: Variant);
    procedure AddList(constref AList: TDictElements);
    function ContainsKey(const AKey: Variant): Boolean;
    function ContainsValue(const AValue: Variant): Boolean;
    function ValueOf(const AKey: Variant): Variant;
    function KeyOf(const AValue: Variant): Variant;
    procedure Delete(const AKey: Variant; Token: TToken);
    function Remove(const AKey: Variant; Token: TToken): Variant; // returns Value
    procedure Sort;  // sort on Keys
    procedure setSorted(Sorted: Boolean=False);
    procedure Clear;

  }

  TDictAdd = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TDictAddList = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TDictContainsKey = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TDictContainsValue = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TDictValueOf = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TDictKeyOf = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TDictDelete = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TDictSort = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TDictSetSorted = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TDictClear = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;


  TDictKeys = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TDictValues = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;



procedure StoreStandardListFunctions(GlobalSpace: TMemorySpace);

implementation
uses uArrayIntf, uDictIntf, uFunc, uError;

{ TListInit }

function TListInit.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 3);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then begin
    IArrayInstance(Instance).Init(ArgList[1].Value, ArgList[2].Value);
    Result := IArrayInstance(Instance);
  end
  else
    Raise ERuntimeError.Create(Token,
      'listInit function not possible for this type.');
end;

function TListAdd.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then begin
    IArrayInstance(Instance).Add(ArgList[1].Value);
    Result := IArrayInstance(Instance);
  end
  else
    Raise ERuntimeError.Create(Token,
      'listAdd function not possible for this type.');
end;

function TListAddList.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then begin
    IArrayInstance(Instance).AddList(IArrayInstance(ArgList[1].Value).Elements);
    Result := IArrayInstance(Instance);
  end
  else
    Raise ERuntimeError.Create(Token,
      'listAddRange function not possible for this type.');
end;

function TListInsert.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 3);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then begin
    IArrayInstance(Instance).Insert(ArgList[1].Value, ArgList[2].Value, Token);
    Result := IArrayInstance(Instance);
  end
  else
    Raise ERuntimeError.Create(Token,
      'listInsert function not possible for this type.');
end;

function TListDelete.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then begin
    IArrayInstance(Instance).Delete(ArgList[1].Value, Token);
    Result := IArrayInstance(Instance);
  end
  else
    Raise ERuntimeError.Create(Token,
      'listDelete function not possible for this type.');
end;

function TListRemove.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then
    Result := IArrayInstance(Instance).Remove(ArgList[1].Value, Token)
  else
    Raise ERuntimeError.Create(Token,
      'listRemove function not possible for this type.');
end;

function TListExtract.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then
    Result := IArrayInstance(Instance).Extract(ArgList[1].Value, Token)
  else
    Raise ERuntimeError.Create(Token,
      'listExtract function not possible for this type.');
end;

function TListContains.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then
    Result := IArrayInstance(Instance).Contains(ArgList[1].Value, Token)
  else
    Raise ERuntimeError.Create(Token,
      'listContains function not possible for this type.');
end;

function TListIndexOf.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then
    Result := IArrayInstance(Instance).IndexOf(ArgList[1].Value, Token)
  else
    Raise ERuntimeError.Create(Token,
      'listIndexOf function not possible for this type.');
end;

function TListRetrieve.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then
    Result := IArrayInstance(Instance).Retrieve(ArgList[1].Value, Token)
  else
    Raise ERuntimeError.Create(Token,
      'listRetrieve function not possible for this type.');
end;

function TListClear.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then begin
    IArrayInstance(Instance).Clear;
    Result := IArrayInstance(Instance);
  end
  else
    Raise ERuntimeError.Create(Token,
      'listClear function not possible for this type.');
end;

{ TListFirst }

function TListFirst.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then
    Result := IArrayInstance(Instance).First
  else
    Raise ERuntimeError.Create(Token,
      'listFirst function not possible for this type.');
end;

{ TListLast }

function TListLast.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then
    Result := IArrayInstance(Instance).Last
  else
    Raise ERuntimeError.Create(Token,
      'listLast function not possible for this type.');
end;



{ TDictAdd }

function TDictAdd.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 3);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IDictInstance) then begin
    IDictInstance(Instance).Add(ArgList[1].Value, ArgList[2].Value);
    Result := IDictInstance(Instance);
  end
  else
    Raise ERuntimeError.Create(Token,
      'dictAdd function not possible for this type.');
end;

{ TDictAddList }

function TDictAddList.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IDictInstance) then begin
    IDictInstance(Instance).AddList(IDictInstance(ArgList[1].Value).Elements);
    Result := IDictInstance(Instance);
  end
  else
    Raise ERuntimeError.Create(Token,
      'dictAddList function not possible for this type.');
end;

{ TDictContainsKey }

function TDictContainsKey.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IDictInstance) then
    Result := IDictInstance(Instance).ContainsKey(ArgList[1].Value, Token)
  else
    Raise ERuntimeError.Create(Token,
      'dictContainsKey function not possible for this type.');
end;

{ TDictContainsValue }

function TDictContainsValue.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IDictInstance) then
    Result := IDictInstance(Instance).ContainsValue(ArgList[1].Value, Token)
  else
    Raise ERuntimeError.Create(Token,
      'dictContainsValue function not possible for this type.');
end;

{ TDictValueOf }

function TDictValueOf.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IDictInstance) then
    Result := IDictInstance(Instance).ValueOf(ArgList[1].Value, Token)
  else
    Raise ERuntimeError.Create(Token,
      'dictValueOf function not possible for this type.');
end;

{ TDictKeyOf }

function TDictKeyOf.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IDictInstance) then
    Result := IDictInstance(Instance).KeyOf(ArgList[1].Value, Token)
  else
    Raise ERuntimeError.Create(Token,
      'dictKeyOf function not possible for this type.');
end;

{ TDictDelete }

function TDictDelete.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IDictInstance) then begin
    IDictInstance(Instance).Delete(ArgList[1].Value, Token);
    Result := IDictInstance(Instance);
  end
  else
    Raise ERuntimeError.Create(Token,
      'dictDelete function not possible for this type.');
end;

{ TDictSort }

function TDictSort.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IDictInstance) then begin
    IDictInstance(Instance).Sort;
    Result := IDictInstance(Instance);
  end
  else
    Raise ERuntimeError.Create(Token,
      'dictSort function not possible for this type.');
end;

{ TDictSetSorted }

function TDictSetSorted.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IDictInstance) then begin
    IDictInstance(Instance).SetSorted(ArgList[1].Value);
    Result := IDictInstance(Instance);
  end
  else
    Raise ERuntimeError.Create(Token,
      'dictSorted function not possible for this type.');
end;

{ TDictClear }

function TDictClear.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IDictInstance) then begin
    IDictInstance(Instance).Clear;
    Result := IDictInstance(Instance);
  end
  else
    Raise ERuntimeError.Create(Token,
      'dictClear function not possible for this type.');
end;

{ TDictKeys }

function TDictKeys.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IDictInstance) then
    Result := IArrayInstance(IDictInstance(Instance).Keys)
  else
    Raise ERuntimeError.Create(Token,
      'dictKeys function not possible for this type.');
end;

{ TDictValues }

function TDictValues.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IDictInstance) then
    Result := IArrayInstance(IDictInstance(Instance).Values)
  else
    Raise ERuntimeError.Create(Token,
      'dictValues function not possible for this type.');
end;



procedure StoreStandardListFunctions(GlobalSpace: TMemorySpace);
var
  Token: TToken;
begin
  Token := TToken.Create(ttIdentifier, '', Null, 0, 0);
  GlobalSpace.Store('listAdd', ICallable(TListAdd.Create), Token);
  GlobalSpace.Store('listAddList', ICallable(TListAddList.Create), Token);
  GlobalSpace.Store('listClear', ICallable(TListClear.Create), Token);
  GlobalSpace.Store('listContains', ICallable(TListContains.Create), Token);
  GlobalSpace.Store('listDelete', ICallable(TListDelete.Create), Token);
  GlobalSpace.Store('listExtract', ICallable(TListExtract.Create), Token);
  GlobalSpace.Store('listFirst', ICallable(TListFirst.Create), Token);
  GlobalSpace.Store('listIndexOf', ICallable(TListIndexOf.Create), Token);
  GlobalSpace.Store('listInit', ICallable(TListInit.Create), Token);
  GlobalSpace.Store('listInsert', ICallable(TListInsert.Create), Token);
  GlobalSpace.Store('listLast', ICallable(TListLast.Create), Token);
  GlobalSpace.Store('listRemove', ICallable(TListRemove.Create), Token);
  GlobalSpace.Store('listRetrieve', ICallable(TListRetrieve.Create), Token);

  GlobalSpace.Store('dictAdd', ICallable(TDictAdd.Create), Token);
  GlobalSpace.Store('dictAddList', ICallable(TDictAddList.Create), Token);
  GlobalSpace.Store('dictClear', ICallable(TDictClear.Create), Token);
  GlobalSpace.Store('dictContainsKey', ICallable(TDictContainsKey.Create), Token);
  GlobalSpace.Store('dictContainsValue', ICallable(TDictContainsValue.Create), Token);
  GlobalSpace.Store('dictDelete', ICallable(TDictDelete.Create), Token);
  GlobalSpace.Store('dictKeyOf', ICallable(TDictKeyOf.Create), Token);
  GlobalSpace.Store('dictSetSorted', ICallable(TDictSetSorted.Create), Token);
  GlobalSpace.Store('dictSort', ICallable(TDictSort.Create), Token);
  GlobalSpace.Store('dictValueOf', ICallable(TDictValueOf.Create), Token);
  GlobalSpace.Store('dictKeys', ICallable(TDictKeys.Create), Token);
  GlobalSpace.Store('dictValues', ICallable(TDictValues.Create), Token);
end;

end.

