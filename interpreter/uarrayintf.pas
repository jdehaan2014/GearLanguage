unit uArrayIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCallable, uAST, Variants, uMembers, Generics.Collections,
  uToken;

type

  IArrayable = Interface(ICallable)
    ['{D7093729-2CB8-6AF9-04EE-E1F296C322ED}']
    procedure ExtendWith(Members: TMembers);
  end;

  TArrayElements = class(specialize TList<Variant>)
    function Find(const Value: Variant; Token: TToken): Integer;
  end;


  IArrayInstance = Interface
    ['{1BB4ACE0-1C5B-4663-4FD2-123CE9CE19A5}']
    function GetMember(Ident: TIdent): Variant;
    function getElements: TArrayElements;
    function getCount: LongInt;
    property Elements: TArrayElements read getElements;
    property Count: LongInt read getCount;
    function getItem(i: integer): Variant;
    procedure setItem(i: integer; AValue: Variant);
    property Items[i:integer]: Variant read getItem write setItem; default;
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
uses uError, uClassIntf, uTupleIntf, uDictIntf, uEnumIntf;

{ TArrayElements }

// work around for failing IndexOf function on Variants in Generics.Collections

function TArrayElements.Find(const Value: Variant; Token: TToken): Integer;
var
  i: SizeInt;
begin
  try
    for i := 0 to Count - 1 do
      if Value = FItems[i] then
        Exit(i);
  except
    if VarSupports(Value, IGearInstance) then begin
      for i := 0 to Count - 1 do
        if IGearInstance(Value) = IGearInstance(FItems[i]) then
          Exit(i);
    end
    else if VarSupports(Value, IArrayInstance) then begin
      for i := 0 to Count - 1 do
        if IArrayInstance(Value) = IArrayInstance(FItems[i]) then
          Exit(i);
    end
    else if VarSupports(Value, ITuple) then begin
      for i := 0 to Count - 1 do
        if ITuple(Value) = ITuple(FItems[i]) then
          Exit(i);
    end
    else if VarSupports(Value, ICallable) then begin
      for i := 0 to Count - 1 do
        if ICallable(Value) = ICallable(FItems[i]) then
          Exit(i);
    end
    else if VarSupports(Value, IDictInstance) then begin
      for i := 0 to Count - 1 do
        if IDictInstance(Value) = IDictInstance(FItems[i]) then
          Exit(i);
    end
    else if VarSupports(Value, IEnumInstance) then begin
      for i := 0 to Count - 1 do
        if (IEnumInstance(Value).EnumName = IEnumInstance(FItems[i]).EnumName) and
           (IEnumInstance(Value).ElemName = IEnumInstance(FItems[i]).ElemName) then
          Exit(i);
    end
    else
      Raise ERuntimeError.Create(Token, 'Comparison of types not allowed.');
  end;
  Result := -1;
end;

end.

