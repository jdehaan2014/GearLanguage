unit uDictIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCallable, uAST, Variants, uMembers, fgl, uToken, uArrayIntf;

type

  IDictionary = interface(ICallable)
    ['{2D24EA1C-4DFC-78A9-34A9-E4DB262E6B93}']
    procedure ExtendWith(Members: TMembers);
  end;

  // TFPGMap instead of TDictionary from Generics.Collections, since adding variant key and value fail.
  TDictElements = class(specialize TFPGMap<Variant, Variant>)
    function FindKey(const Key: Variant; Token: TToken): SizeInt;
    function FindValue(const Value: Variant; Token: TToken): SizeInt;
  end;

  IDictInstance = Interface
    ['{387857D6-BA13-5615-7256-95843393C4C8}']
    function getCount: LongInt;
    function getElements: TDictElements;
    property Elements: TDictElements read getElements;
    property Count: LongInt read getCount;
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
    procedure setSorted(Sorted: Boolean=False);
    procedure Sort;  // sort on Keys
    procedure Clear;
    function Keys: IArrayInstance;
    function Values: IArrayInstance;
  end;

implementation
uses uError, uClassIntf, uTupleIntf, uEnumIntf;

{ TDictElements }

function TDictElements.FindKey(const Key: Variant; Token: TToken): SizeInt;
var
  i: SizeInt;
begin
  try
    for i := 0 to Count - 1 do
      if Key = Keys[i] then
        Exit(i);
  except
    if VarSupports(Key, IGearInstance) then begin
      for i := 0 to Count - 1 do
        if IGearInstance(Key) = IGearInstance(Keys[i]) then
          Exit(i);
    end
    else if VarSupports(Key, IArrayInstance) then begin
      for i := 0 to Count - 1 do
        if IArrayInstance(Key) = IArrayInstance(Keys[i]) then
          Exit(i);
    end
    else if VarSupports(Key, IDictInstance) then begin
      for i := 0 to Count - 1 do
        if IDictInstance(Key) = IDictInstance(Keys[i]) then
          Exit(i);
    end
    else if VarSupports(Key, ITuple) then begin
      for i := 0 to Count - 1 do
        if ITuple(Key) = ITuple(Keys[i]) then
          Exit(i);
    end
    else if VarSupports(Key, ICallable) then begin
      for i := 0 to Count - 1 do
        if ICallable(Key) = ICallable(Keys[i]) then
          Exit(i);
    end
    else if VarSupports(Key, IEnumInstance) then begin
      for i := 0 to Count - 1 do
         if (IEnumInstance(Key).EnumName = IEnumInstance(Keys[i]).EnumName) and
           (IEnumInstance(Key).ElemName = IEnumInstance(Keys[i]).ElemName) then
          Exit(i);
    end
    else
      Raise ERuntimeError.Create(Token, 'Comparison of dictionary keys failed.');
  end;
  Result := -1;
end;

function TDictElements.FindValue(const Value: Variant; Token: TToken): SizeInt;
var
  i: SizeInt;
begin
  try
    for i := 0 to Count - 1 do
      if Value = Data[i] then
        Exit(i);
  except
    if VarSupports(Value, IGearInstance) then begin
      for i := 0 to Count - 1 do
        if IGearInstance(Value) = IGearInstance(Data[i]) then
          Exit(i);
    end
    else if VarSupports(Value, IArrayInstance) then begin
      for i := 0 to Count - 1 do
        if IArrayInstance(Value) = IArrayInstance(Data[i]) then
          Exit(i);
    end
    else if VarSupports(Value, IDictInstance) then begin
      for i := 0 to Count - 1 do
        if IDictInstance(Value) = IDictInstance(Data[i]) then
          Exit(i);
    end
    else if VarSupports(Value, ITuple) then begin
      for i := 0 to Count - 1 do
        if ITuple(Value) = ITuple(Data[i]) then
          Exit(i);
    end
    else if VarSupports(Value, ICallable) then begin
      for i := 0 to Count - 1 do
        if ICallable(Value) = ICallable(Data[i]) then
          Exit(i);
    end
    else if VarSupports(Value, IEnumInstance) then begin
      for i := 0 to Count - 1 do
        if (IEnumInstance(Value).EnumName = IEnumInstance(Data[i]).EnumName) and
           (IEnumInstance(Value).ElemName = IEnumInstance(Data[i]).ElemName) then
          Exit(i);
    end
    else
      Raise ERuntimeError.Create(Token, 'Comparison of dictionary values failed.');
  end;
  Result := -1;
end;

end.

