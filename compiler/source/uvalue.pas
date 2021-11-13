unit uValue;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch typehelpers}

{$I uDefines.inc}

interface

uses
  SysUtils;

{$ifdef NAN_BOXING}
const
  SIGN_BIT: UInt64 = UInt64($8000000000000000);
  QNAN:     UInt64 = UInt64($7FFC000000000000);
  CHARBASE: UInt64 = UInt64($7FFC000000000004);

  TagNil = 1;
  TagFalse = 2;
  TagTrue = 3;
  TagChar = 4;
  TagEmpty = 5;

{$endif}

type
  String4 = String[4];

  PValue = ^TValue;

  {$ifdef NAN_BOXING}

  T8Bytes = array[0..7] of Byte;

  TUnion = packed record
    case Byte of
      0: (Bits: UInt64);
      1: (Number: Double);
      2: (Bytes: T8Bytes;);
  end;

  TValue = type UInt64;

  {$else}

  TValueType = (valBool, valNil, valNumber, valChar, valObj, valNan);

  TValue = record
    case Typ: TValueType of
      valBool:   (Bool: Boolean);
      valNumber: (Number: Double);
      valChar:   (Character: String4); // utf8 char
      valObj:    (Obj: Pointer);
  end;

  {$endif}

  TValueHelper = type helper for TValue
    // functions to check if a value is a certain type
    function isBool: Boolean;
    function isNil: Boolean;
    function isNumber: Boolean;
    function isChar: Boolean;
    function isObj: Boolean;
    function isQNAN: Boolean;
    // Conversion from TValue to Pascal value
    function asBool: Boolean;
    function asNumber: Double;
    function asInt: Int64;
    function asChar: String;
    function asObj: Pointer;

    function toString: String;
    procedure Print;
    function Equals(Other: TValue): Boolean;
    function isAssignableFrom(Other: TValue): Boolean;
    function TypeStr: String;
  end;

  PValueArray = ^TValueArray;
  TValueArray = record
    Values: PValue;
    Count,
    Capacity: Longint;
    procedure Init;
    procedure Write(const AValue: TValue);
    procedure Free;
  end;

  // Conversion functions Pascal value to TValue
  function BoolVal(const Value: Boolean): TValue;
  function NilVal: TValue;
  function NanVal: TValue;
  function NumberVal(const Value: Double): TValue;
  function CharVal(const Chars: String4): TValue;
  function ObjVal(Obj: Pointer): TValue;

implementation
uses uObject, uMemory, uCommon;

{ TValueHelper }

{$ifdef NAN_BOXING}

function FalseVal: TValue; inline;
begin
  Result := UInt64(QNAN or TagFalse);
end;

function TrueVal: TValue; inline;
begin
  Result := UInt64(QNAN or TagTrue);
end;

function NilVal: TValue; inline;
begin
  Result := UInt64(QNAN or TagNil);
end;

function NanVal: TValue;
begin
  Result := UInt64(QNAN);
end;

// Conversion functions: promotion of Pascal value to TValue

function BoolVal(const Value: Boolean): TValue; inline;
begin
  if Value then
    Result := TrueVal
  else
    Result := FalseVal;
  //Result := specialize IfThen<TValue>(Value, TrueVal, FalseVal);
end;

function NumberVal(const Value: Double): TValue; inline;
var
  Data: TUnion;
begin
  Data.Number := Value;
  Result := Data.Bits;
end;

function CharVal(const Chars: String4): TValue;
var
  Data: TUnion;
  i, Len: Byte;
begin
  Data.Bytes := T8Bytes(CHARBASE);

  Len  := Length(Chars);
  Data.Bytes[1] := Len; // length byte = max 4
  for i := 1 to Len do    // bytes not used are zero
    Data.Bytes[i+1] := Ord(Chars[i]);

  Result := Data.Bits;
end;


function ObjVal(Obj: Pointer): TValue; inline;
begin
  Result := TValue(SIGN_BIT or QNAN or UInt64(PUInt64(Obj)));
end;


// functions to check if a value is a certain type

function TValueHelper.isBool: Boolean; inline;
begin
  //Result := (Self and FalseVal) = FalseVal;
  Result := (Self or 1) = TrueVal;
end;

function TValueHelper.isNil: Boolean; inline;
begin
  Result := Self = NilVal;
end;

function  TValueHelper.isNumber: Boolean; inline;
begin
  Result := (Self and QNAN) <> QNAN;
end;

function TValueHelper.isChar: Boolean;
begin
  Result := (Self and CHARBASE) = CHARBASE;
end;

function TValueHelper.isObj: Boolean; inline;
begin
  Result := (Self and (QNAN or SIGN_BIT)) = (QNAN or SIGN_BIT);
end;

function TValueHelper.isQNAN: Boolean; inline;
begin
  Result := Self = QNAN;
end;

// Conversion functions: TValue to Pascal value

function TValueHelper.asBool: Boolean; inline;
begin
  Result := Self = TrueVal;
end;

//function TValueHelper.asNumber: Double;
//begin
//  Result := TUnion(Self).Number;
//end;

function TValueHelper.asNumber: Double; inline;
var
  Data: TUnion;
begin
  Data.Bits := Self;
  Result := Data.Number;
end;

function TValueHelper.asInt: Int64;
begin
  Result := Round(Self.asNumber);
end;

function TValueHelper.asChar: String;
var
  Data: TUnion;
  i, Len, b: Byte;
begin
  Data.Bits := Self;
  Len := Data.Bytes[1];
  Result := '';
  for i := 1 to Len do
    begin
      b := Data.Bytes[i+1];
      Result += Chr(b);
    end;
end;

function TValueHelper.asObj: Pointer; inline;
begin
  Result := PUInt64(Self and not(SIGN_BIT or QNAN));
end;

function TValueHelper.toString: String;
begin
  if isBool then
    Result := BoolToStr(asBool, 'true', 'false')
  else if isNil then
    Result := 'nil'
  else if isNumber then
    Result := FloatToStr(asNumber, DefaultFormatSettings)
  else if isChar then
    Result := asChar
  else if isObj then
    WriteStrObj(Result, Self)
  else if isQNAN then
    Result := 'Error: Division by zero.'
  else
    Result := 'Unknown';
end;

procedure TValueHelper.Print;
begin
  Write(Self.toString);
end;


{$else}

// functions to check if a value is a certain type

function TValueHelper.isBool: Boolean;
begin
  Result := Typ = valBool;
end;

function TValueHelper.isNil: Boolean;
begin
  Result := Typ = valNil;
end;

function TValueHelper.isQNAN: Boolean;
begin
  Result := Typ = valNan;
end;

function  TValueHelper.isNumber: Boolean;
begin
  Result := Typ = valNumber;
end;

function TValueHelper.isChar: Boolean;
begin
  Result := Typ = valChar;
end;

function TValueHelper.isObj: Boolean;
begin
  Result := Typ = valObj;
end;

// Conversion functions: TValue to Pascal value

function TValueHelper.asBool: Boolean;
begin
  Result := Bool;
end;

function TValueHelper.asNumber: Double;
begin
  Result := Number;
end;

function TValueHelper.asInt: Int64;
begin
  Result := Round(Number);
end;

function TValueHelper.asChar: String;
begin
  Result := Character;
end;

function TValueHelper.asObj: Pointer;
begin
  Result := Obj;
end;

function TValueHelper.toString: String;
begin
  if isBool then
    WriteStr(Result, asBool)
  else if isNil then
    Result := 'nil'
  else if isNumber then
    Result := Format('%g', [asNumber])
  else if isChar then
    Result := Character
  else if isObj then
    WriteStrObj(Result, Self);
end;

procedure TValueHelper.Print;
begin
  case Self.Typ of
    valBool: Write(specialize IfThen<String>(asBool, 'true', 'false'));
    valNil: Write('nil');
    valNumber: WriteFmt('%g', [asNumber]);
    valChar: Write(asChar);
    valObj: PrintObject(Self);
  end;
end;

// Conversion functions: promotion of Pascal value to TValue

function BoolVal(const Value: Boolean): TValue;
begin
  Result.Typ := valBool;
  Result.Bool := Value;
end;

function NilVal: TValue;
begin
  Result.Typ := valNil;
  Result.Number := 0;
end;

function NanVal: TValue;
begin
  Result.Typ := valNan;
  Result.Number := UInt64($7FFC000000000000);
end;

function NumberVal(const Value: Double): TValue;
begin
  Result.Typ := valNumber;
  Result.Number := Value;
end;

function CharVal(const Chars: String4): TValue;
begin
  Result.Typ := valChar;
  Result.Character := Chars;
end;

function ObjVal(Obj: Pointer): TValue;
begin
  Result.Typ := valObj;
  Result.Obj := Obj;
end;

{$endif}


function TValueHelper.Equals(Other: TValue): Boolean;
begin
  {$ifdef NAN_BOXING}
  if self.isObj then
    Result := EqualsObj(Self, Other)
  else
    Result := Self = Other;
  {$else}
  if Self.Typ <> Other.Typ then
    Exit(False);
  case Self.Typ of
    valBool: Result := Self.asBool = Other.asBool;
    valNil: Result := True;
    valNumber: Result := Self.asNumber = Other.asNumber;
    valChar: Result := Self.asChar = Other.asChar;
    valObj: Result := Self.asObj = Other.asObj;
  end;
  {$endif}
end;

function TValueHelper.isAssignableFrom(Other: TValue): Boolean;
begin
  Result := Self.isNil or Other.isNil or
    (Self.isBool and Other.isBool) or
    (Self.isNumber and Other.isNumber) or
    (Self.isChar and Other.isChar) or
    areAssignable(Self, Other); // in case of object
end;

function TValueHelper.TypeStr: String;
begin
  if isBool then
    Result := 'Boolean'
  else if isNil then
    Result := 'nil'
  else if isNumber then
    Result := 'Number'
  else if isChar then
    Result := 'Character'
  else if isObj then
    TypeStrObj(Result, Self);
end;



{ TValueArray }

procedure TValueArray.Init;
begin
  Count := 0;
  Capacity := 0;
  Values := Nil;
end;

procedure TValueArray.Write(const AValue: TValue);
var
  OldCapacity: LongInt;
begin
  if Capacity < Count + 1 then
    begin
      OldCapacity := Capacity;
      Capacity := Grow_Capacity(OldCapacity);
      Values := PValue(Reallocate(
        Values, SizeOf(TValue)*OldCapacity, SizeOf(TValue)*Capacity));
    end;

  Values[Count] := AValue;
  Inc(Count);
end;

procedure TValueArray.Free;
begin
  Reallocate(Values, SizeOf(TValue)*Capacity, 0); // Free_Array
  Init;
end;

end.


