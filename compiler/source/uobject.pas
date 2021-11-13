unit uObject;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

// The basics for object management are taken from:
// http://craftinginterpreters.com/strings.html#values-and-objects
// The idea is that we can simulate object orientation by using plain records.
// Each record type 'derives' from TObj.

interface

uses
  SysUtils, uValue, uChunk, uCommon, uTable, {fgl,} Generics.Collections;

type

  // all possible object types
  TObjTyp = (otBoundMethod, otClass, otClosure, otFunc, otInstance,
    otNative, otString, otUpValue, otArray, otDictionary,
    otTuple, otSet, otEnumInstance, otRange, otBuffer);

  { TObjTypHelper }

  TObjTypHelper = type helper for TObjTyp
    function toString: String;
  end;

  // base object
  PObj = ^TObj;
  TObj = record
    Typ: TObjTyp;
    isMarked: Boolean; // for garbage collection
    Next: PObj;
  end;

  PPObj = ^PObj;

  // String object consisting of length, chars and a hash
  PObjString = ^TObjString;
  TObjString = record
    Obj: TObj;
    Length: LongInt;
    Chars: PChar;
    Hash: LongWord;   // uint32_t  0 .. 4294967295 -> 4 bytes
  end;

  // A defer statement is elevated to a function without parameters
  // the name of the 'defer' function is: 'defer:Func0' for instance
  // The names of all defer functions are captured in this array and stored in
  // the surrounding function.
  // Just before the func returns the defer funcs are called
  TDeferList = array of String;

  PObjFunc = ^TObjFunc;
  TObjFunc = record
    Obj: TObj;
    Arity: Integer;               // number of parameters
    UpValueCount: Integer;        // number of upvalues
    Chunk: TChunk;                // the code chunk of the function
    Name: PObjString;             // name of the function
    isValProperty: Boolean;       // it might be a value property
    FileIndex: Integer;           // file in which function is defined
    DeferList: TDeferList;        // list of defer:Func names
  end;

  // definition of a native function. See uNative.pas for all native functions
  TNativeFn = function(const ArgCount: Integer; Args: PValue): Boolean;

  PObjNative = ^TObjNative;
  TObjNative = record
    Obj: TObj;
    Func: TNativeFn;
  end;

  // based on: http://craftinginterpreters.com/closures.html#upvalues
  // which is based on Lua upvalue solution.
  // An upvalue refers to a local variable in an enclosing function. Every closure
  // maintains an array of upvalues, one for each surrounding local variable that
  // the closure uses.
  PObjUpValue = ^TObjUpValue;
  TObjUpValue = record
    Obj: TObj;
    Location: PValue;
    Closed: TValue;
    Next: PObjUpValue;
  end;

  PPObjUpValue = ^PObjUpValue;

  // Based on: http://craftinginterpreters.com/closures.html#top
  PObjClosure = ^TObjClosure;
  TObjClosure = record
    Obj: TObj;
    Func: PObjFunc;
    UpValues: PPObjUpValue;
    UpValueCount: Integer;
  end;

  TClassType = (ctClass, {ctRecord,} ctEnum);
  PObjClass = ^TObjClass;
  TObjClass = record
    Obj: TObj;
    Name: PObjString;           // class name
    Typ: TClassType;            // class or enum
    Methods: TTable;            // list of methods in the class
    Statics: TTable;            // list of static methods in the class
    Fields: TTable;             // list of fields and properties
    DefaultName: PObjString;    // name of default value for an instance will be printed
  end;

  // an instance of a class
  PObjInstance = ^TObjInstance;
  TObjInstance = record
    Obj: TObj;
    Klass: PObjClass;           // refernce to the class
    Fields: TTable;             // all class fields are copied to the instance and become state
    DefaultName: PObjString;    // name of default value for an instance will be printed
  end;

  // http://craftinginterpreters.com/methods-and-initializers.html#bound-methods
  PObjBoundMethod = ^TObjBoundMethod;
  TObjBoundMethod = record
    Obj: TObj;
    Receiver: TValue;
    Method: PObjClosure;
  end;

  // An array expression is an instance from class Array
  TValueList = specialize TList<TValue>;
  PObjArray = ^TObjArray;
  TObjArray = record
    Obj: TObj;
    Klass: PObjClass;             // Array class
    Elements: TValueList;         // the array elements
    Iterator: PObjInstance;       // iterator object
  end;

  // A dictionary expression is an instance from class Dictionary
  TKeyValueMap = specialize TDictionary<TValue, TValue>;
  PObjDictionary = ^TObjDictionary;
  TObjDictionary = record
    Obj: TObj;
    Klass: PObjClass;             // Dictionary class
    Elements: TKeyValueMap;       // key:value pair elements
    Iterator: PObjInstance;       // iterator object
  end;

  // A tuple is a list of name:value pairs
  PObjTuple = ^TObjTuple;
  TObjTuple = record
    Obj: TObj;
    Fields: TTable;                // list of name:value pairs
  end;

  // A set expression is an instance of class Set
  TValueSet = specialize TSortedSet<TValue>;
  PObjSet = ^TObjSet;
  TObjSet = record
    Obj: TObj;
    Klass: PObjClass;             // Set class
    Elements: TValueSet;          // unordered set of elements
    Iterator: PObjInstance;       // iterator object
  end;

  // an instance of an enum, which is defined as a class
  PObjEnumInstance = ^TObjEnumInstance;
  TObjEnumInstance = record
    Obj: TObj;
    Enum: PObjClass;              // ref to the enum defintion, which is a class internally
    Name: PObjString;             // name of the enum element
    Value: TValue;                // value of the enum element
  end;

  PObjRange = ^TObjRange;
  TObjRange = record
    Obj: TObj;
    Klass: PObjClass;            // Range class
    From,                        // Numbers  allowed
    UpTo: TValue;                // e.g. 0..9
    Step: TValue;                // step size
    isInclusive: Boolean;        // if true then UpTo is included
    Iterator: PObjInstance;      // iterator object
  end;

  // Buffer is used internally for reading data from files
  PObjBuffer = ^TObjBuffer;
  TObjBuffer = record
    Obj: TObj;
    Bytes: PByte;
    Size: Integer;
  end;

  { TValueObjHelper }

  TValueObjHelper = type helper(TValueHelper) for TValue
    function isFunc: Boolean;
    function asFunc: PObjFunc;
    function isNative: Boolean;
    function asNative: TNativeFn;
    function isClosure: Boolean;
    function asClosure: PObjClosure;
    function isClass: Boolean;
    function asClass: PObjClass;
    function isInstance: Boolean;
    function asInstance: PObjInstance;
    function isBoundMethod: Boolean;
    function asBoundMethod: PObjBoundMethod;
    function isString: Boolean;
    function asString: PObjString;
    function asCString: PChar;
    function asPString: String;
    function isArray: Boolean;
    function asArray: PObjArray;
    function isDictionary: Boolean;
    function asDictionary: PObjDictionary;
    function isTuple: Boolean;
    function asTuple: PObjTuple;
    function isSet: Boolean;
    function asSet: PObjSet;
    function isEnumInstance: Boolean;
    function asEnumInstance: PObjEnumInstance;
    function isRange: Boolean;
    function asRange: PObjRange;
    function isBuffer: Boolean;
    function asBuffer: PObjBuffer;
  end;

  // functions related to an object
  function isObjType(Value: TValue; ObjTyp: TObjTyp): Boolean; inline;
  function Obj_Typ(Value: TValue): TObjTyp;
  generic function Allocate_Obj<T>(const Typ: TObjTyp): Pointer;
  function AllocateObject(const Size: UInt32; const Typ: TObjTyp): PObj;

  function NewFunc(const isValProperty: Boolean = False): PObjFunc;
  procedure NewDefer(const Name: String; Func: PObjFunc);
  function NewNative(Func: TNativeFn): PObjNative;
  function NewClosure(Func: PObjFunc): PObjClosure;
  function NewUpValue(Slot: PValue): PObjUpValue;
  function NewClass(Name: PObjString; const Typ: TClassType=ctClass): PObjClass;
  function NewInstance(Klass: PObjClass): PObjInstance;
  function NewBoundMethod(Receiver: TValue; Method: PObjClosure): PObjBoundMethod;

  function NewArray(Klass: PObjClass): PObjArray;
  function NewDictionary(Klass: PObjClass): PObjDictionary;
  function NewTuple: PObjTuple;
  function NewSet(Klass: PObjClass): PObjSet;
  function NewEnumInstance(Name: PObjString; Value: TValue;
    Enum: PObjClass): PObjEnumInstance;
  function NewRange(From, UpTo, Include: TValue; Klass: PObjClass): PObjRange;

  function NewBuffer(const Size: Integer; newBytes: PByte):PObjBuffer;
  function Bytes2String(Buffer: PObjBuffer): PObjString;
  function String2Bytes(ObjString: PObjString): PObjBuffer;

  function CopyString(AString: String): PObjString; overload;
  function CopyString(Chars: PChar; const Length: LongInt): PObjString; overload;
  function StringVal(const Text: String): TValue;
  function TakeString(Chars: PChar; const Length: LongInt): PObjString;
  function AllocateString(Chars: PChar; const Length: LongInt; const Hash: LongWord): PObjString;

  procedure PrintObject(Value: TValue);
  procedure WriteStrObj(var Result: String; Value: TValue);
  procedure TypeStrObj(var Result: String; Value: TValue);
  function AreAssignable(Left, Right: TValue): Boolean;
  function EqualsObj(Left, Right: TValue): Boolean;


implementation
uses uVM, uMemory;

// returns true if the value is of the requested object type
function isObjType(Value: TValue; ObjTyp: TObjTyp): Boolean; inline;
begin
  Result := Value.isObj and (PObj(Value.asObj)^.Typ = ObjTyp);
end;

// return object type from any given object
function Obj_Typ(Value: TValue): TObjTyp;
begin
  Result := PObj(Value.asObj)^.Typ;
end;

generic function Allocate_Obj<T>(const Typ: TObjTyp): Pointer;
begin
  Result := PObj(AllocateObject(SizeOf(T), Typ));
end;

function AllocateObject(const Size: UInt32; const Typ: TObjTyp): PObj;
begin
  Result := PObj(Reallocate(Nil, 0, Size));
  Result^.Typ := Typ;
  Result^.isMarked := False;
  Result^.Next := VM.Objects;
  VM.Objects := Result;

  {$ifdef DEBUG_LOG_GC}
  WriteLnFmt('%p allocate %d for %s', [Pointer(Result), Size, Typ.toString]);
  {$endif}
end;


// allocate memory for a new function or value property, then initialize fields
function NewFunc(const isValProperty: Boolean): PObjFunc;
begin
  Result := PObjFunc(specialize Allocate_Obj<TObjFunc>(otFunc));
  Result^.Arity := 0;
  Result^.UpValueCount := 0;
  Result^.Name := Nil;
  Result^.isValProperty := isValProperty;
  Result^.Chunk.Init;
  SetLength(Result^.DeferList, 0);
  Result^.FileIndex := 0;
end;

// add a new defer-func name to the list of defers in the respective function
procedure NewDefer(const Name: String; Func: PObjFunc);
var
  Len: Integer;
begin
  Len := Length(Func^.DeferList);
  SetLength(Func^.DeferList, Len+1);
  Func^.DeferList[Len] := Name;          // since the array is zero-based
end;

// allocate memory for a new native function
function NewNative(Func: TNativeFn): PObjNative;
begin
  Result := PObjNative(specialize Allocate_Obj<TObjNative>(otNative));
  Result^.Func := Func;
end;

// Allocate memory for a new closure, and takes a pointer to the ObjFunction it wraps
// http://craftinginterpreters.com/closures.html
// Local variables are limited to a single function’s stack window. Locals from a
// surrounding function are outside of the inner function’s window.
// An upvalue refers to a local variable in an enclosing function. Every closure
// maintains an array of upvalues, one for each surrounding local variable
// that the closure uses.
function NewClosure(Func: PObjFunc): PObjClosure;
var
  Upvalues: PPObjUpValue;
  i: Integer;
begin
  Upvalues := PPObjUpValue(Reallocate(Nil, 0, sizeof(PObjUpValue) * Func^.UpValueCount));
  for i := 0 to Func^.UpValueCount-1 do
    UpValues[i] := Nil;

  Result := PObjClosure(specialize Allocate_Obj<TObjClosure>(otClosure));
  Result^.Func := Func;
  Result^.UpValues := UpValues;
  Result^.UpValueCount := Func^.UpValueCount;
end;

// Allocate memory for a new upvalue
// http://craftinginterpreters.com/closures.html#upvalues
// The upvalue points back into the stack to where the variable it captured lives.
// When the closure needs to access a closed-over variable, it goes through the
// corresponding upvalue to reach it. When a function declaration is first executed
// and we create a closure for it, the VM creates the array of upvalues and wires
// them up to “capture” the surrounding local variables that the closure needs.
function NewUpValue(Slot: PValue): PObjUpValue;
begin
  Result := PObjUpValue(specialize Allocate_Obj<TObjUpValue>(otUpValue));
  Result^.Closed := NilVal;
  Result^.Location := Slot;
  Result^.Next := Nil;
end;

// Allocate memory for a new class with a given name
function NewClass(Name: PObjString; const Typ: TClassType): PObjClass;
begin
  Result := PObjClass(specialize Allocate_Obj<TObjClass>(otClass));
  Result^.Name := Name;
  Result^.Typ := Typ;           // class or enum
  Result^.Methods.Init;         // initialize list of methods
  Result^.Statics.Init;         // initialize list of static methods
  Result^.Fields.Init;          // initialize list of fields
  // each class has a field className, which holds the name of the class
  Result^.Fields.SetValue(CopyString('className'), ObjVal(Name));
  Result^.DefaultName := Nil;   // init default field
end;

// Allocate memory for a new class instance with a given class
function NewInstance(Klass: PObjClass): PObjInstance;
begin
  Result := PObjInstance(specialize Allocate_Obj<TObjInstance>(otInstance));
  Result^.Klass := Klass;
  Result^.Fields.Init;       // initialize fields (state)
  // copy all fields with initial values from the class field list to the instance
  Result^.Fields.AddAll(Result^.Klass^.Fields);
  Result^.DefaultName := Klass^.DefaultName; // copy the default field name
end;

// Allocate memory for a new bound method
function NewBoundMethod(Receiver: TValue; Method: PObjClosure): PObjBoundMethod;
begin
  Result := PObjBoundMethod(specialize Allocate_Obj<TObjBoundMethod>(otBoundMethod));
  Result^.Receiver := Receiver;
  Result^.Method := Method;
end;

// Array functions

// Allocate memory for a new array with the given class Array
function NewArray(Klass: PObjClass): PObjArray;
begin
  Result := PObjArray(specialize Allocate_Obj<TObjArray>(otArray));
  Result^.Klass := Klass;
  Result^.Elements := TValueList.Create;
end;

// Allocate memory for a new dictionary with the given class Dictionary
function NewDictionary(Klass: PObjClass): PObjDictionary;
begin
  Result := PObjDictionary(specialize Allocate_Obj<TObjDictionary>(otDictionary));
  Result^.Klass := Klass;
  Result^.Elements := TKeyValueMap.Create;
end;

// Allocate memory for a new tuple
function NewTuple: PObjTuple;
begin
  Result := PObjTuple(specialize Allocate_Obj<TObjTuple>(otTuple));
  Result^.Fields.Init;  // initialize tuple fields (name:value pairs)
end;

// Allocate memory for a new set with given class Set
function NewSet(Klass: PObjClass): PObjSet;
begin
  Result := PObjSet(specialize Allocate_Obj<TObjSet>(otSet));
  Result^.Klass := Klass;
  Result^.Elements := TValueSet.Create;
end;

// Allocate memory for a new enum instance, with name and possible value
function NewEnumInstance(Name: PObjString; Value: TValue; Enum: PObjClass
  ): PObjEnumInstance;
begin
  Result := PObjEnumInstance(specialize Allocate_Obj<TObjEnumInstance>(otEnumInstance));
  Result^.Enum := Enum;
  Result^.Name := Name;
  Result^.Value := Value;
end;

function NewRange(From, UpTo, Include: TValue; Klass: PObjClass): PObjRange;
begin
  Result := PObjRange(specialize Allocate_Obj<TObjRange>(otRange));
  Result^.Klass := Klass;
  Result^.From := From;
  Result^.UpTo := UpTo;
  Result^.Step := NumberVal(1);
  Result^.isInclusive := Include.asBool;
end;

// Allocate memory for a new byte buffer
function NewBuffer(const Size: Integer; newBytes: PByte): PObjBuffer;
begin
  Result := PObjBuffer(specialize Allocate_Obj<TObjBuffer>(otBuffer));
  Result^.Size := Size;
  Result^.Bytes := PByte(AllocMem(Size));
  Result^.Bytes := newBytes;
end;

function Bytes2String(Buffer: PObjBuffer): PObjString;
var
  newString: String;
begin
  SetString(newString, PChar(Buffer^.Bytes), Buffer^.Size);
  Result := CopyString(newString);
end;

function String2Bytes(ObjString: PObjString): PObjBuffer;
var
  Bytes: PByte;
  Size: Integer;
begin
  Size := ObjString^.Length;
  Bytes := PByte(AllocMem(Size));
  Move(ObjString^.Chars[0], Bytes[0], Size);
  Result := NewBuffer(Size, Bytes);
end;

// create a hash from a string
function HashString(const Key: PChar; const Length: Integer): LongWord;
var
  i: Integer = 0;
  Hash: LongWord = 2166136261;
begin
  while i < Length do
    begin
      Hash := (Hash xor Ord(Key[i])) * 16777619;
      Inc(i);
    end;

  Result := Hash;
end;

// create a new string and allocate memory for it
function AllocateString(Chars: PChar; const Length: LongInt;
  const Hash: LongWord): PObjString;
begin
  Result := PObjString(specialize Allocate_Obj<TObjString>(otString));
  Result^.Obj.Typ := otString;
  Result^.Length := Length;
  Result^.Chars := Chars;
  Result^.Hash := Hash;
  VM.Push(ObjVal(Result));
  VM.Strings.SetValue(Result, NilVal);
  VM.Pop;
end;


function CopyString(AString: String): PObjString;
var
  HeapChars, Chars: PChar;
  Hash: LongWord;
  Interned: PObjString;
  Len: LongInt;
begin
  Chars := PChar(AString);
  Len := Length(AString);
  Hash := HashString(Chars, Len);
  Interned := VM.Strings.FindString(Chars, Len, Hash);
  if Interned <> Nil then
    Exit(Interned);

  HeapChars := StrAlloc(Len+1);
  HeapChars := StrLCopy(HeapChars, Chars, Len);

  Result := AllocateString(HeapChars, Len, Hash);
end;

function CopyString(Chars: PChar; const Length: LongInt): PObjString;
var
  HeapChars: PChar;
  Hash: LongWord;
  Interned: PObjString;
begin
  Hash := HashString(Chars, Length);
  Interned := VM.Strings.FindString(Chars, Length, Hash);
  if Interned <> Nil then
    Exit(Interned);

  HeapChars := StrAlloc(Length+1);
  HeapChars := StrLCopy(HeapChars, Chars, Length);

  Result := AllocateString(HeapChars, Length, Hash);
end;

function StringVal(const Text: String): TValue;
begin
  Result := ObjVal(CopyString(Text));
end;

function TakeString(Chars: PChar; const Length: LongInt): PObjString;
var
  Hash: LongWord;
  Interned: PObjString;
begin
  Hash := HashString(Chars, Length);
  Interned := VM.Strings.FindString(Chars, Length, Hash);
  if Interned <> Nil then
    begin
      StrDispose(Chars);
      Exit(Interned);
    end;
  Result := AllocateString(Chars, Length, Hash);
end;

function FuncToStr(Func: PObjFunc): String;
begin
  if Func^.Name = Nil then
    Result := '<script>'
  else
    Result := Format('<fn %s>', [Func^.Name^.Chars]);
end;

procedure PrintFunc(Func: PObjFunc);
begin
  Write(FuncToStr(Func));
end;

function checkAsString(Value: TValue): String;
begin
  if Value.isString then
    Result := QuotedStr(Value.toString)
  else
    Result := Value.toString;
end;

function ArrayToStr(Elements: TValueList): String;
var
  i: Integer;
begin
  Result := '[';
  if Elements.Count>0 then
    begin
      for i := 0 to Elements.Count-2 do
        Result += checkAsString(Elements[i]) + ', ';
      Result += checkAsString(Elements[Elements.Count-1]);
    end;
  Result += ']';
end;

procedure PrintArray(Elements: TValueList);
begin
  Write(ArrayToStr(Elements));
end;

function DictionaryToStr(Elements: TKeyValueMap): String;
var
  Key: TValue;
  i: Integer=1;
begin
  Result := '[';
  if Elements.Count>0 then
    for Key in Elements.Keys do
      begin
        Result += checkAsString(Key) + ':' + checkAsString(Elements[Key]);
        if i < Elements.Count then Result += ', ';
        Inc(i);
      end
  else
    Result += ':';
  Result += ']';
end;

procedure PrintDictionary(Elements: TKeyValueMap);
begin
  Write(DictionaryToStr(Elements));
end;

function TupleToStr(Tuple: PObjTuple): String;
var
  i, k: Integer;
  Entry: PEntry;
begin
  k := 1;
  with Tuple^ do
    begin
      Result := '(';
      for i := 0 to Fields.Capacity do
        begin
          Entry := @Fields.Entries[i];
          if Entry^.Key <> Nil then
            begin
              Result += PObjString(Entry^.Key)^.Chars + ':';
              Result += Entry^.Value.toString;
              if k < Fields.Count then Result += ', ';
              inc(k);
            end;
        end;
    end;
  Result += ')';
end;

procedure PrintTuple(Tuple: PObjTuple);
begin
  Write(TupleToStr(Tuple));
end;

function SetToStr(SetList: PObjSet): String;
var
  k: Integer=1;
  Value: TValue;
begin
  Result := '{';
  with SetList^ do
  if Elements.Count>0 then
    for Value in Elements do
      begin
        Result += checkAsString(Value);
        if k < Elements.Count then Result += ', ';
        Inc(k);
      end;
  Result += '}';
end;

procedure PrintSet(SetList: PObjSet);
begin
  Write(SetToStr(SetList));
end;

procedure WriteStrObj(var Result: String; Value: TValue);
begin
  case Obj_Typ(Value) of
    otBoundMethod: Result := FuncToStr(Value.asBoundMethod^.Method^.Func);
    otClass: Result := Format('%s', [Value.asClass^.Name^.Chars]);
    otClosure: Result := FuncToStr(Value.asClosure^.Func);
    otFunc: Result := FuncToStr(Value.asFunc);
    otInstance:
      begin
        if Assigned(Value.asInstance^.DefaultName) then
          Result := Value.asInstance^.Fields.Retrieve(Value.asInstance^.DefaultName^.Chars).toString
        else
          Result := Format('%s instance', [Value.asInstance^.Klass^.Name^.Chars]);
      end;
    otNative: Result := '<native fn>';
    otString: Result := Format('%s', [Value.asCString]);
    otUpValue: Result := 'upvalue';
    otArray: Result := ArrayToStr(Value.asArray^.Elements);
    otDictionary: Result := DictionaryToStr(Value.asDictionary^.Elements);
    otTuple: Result := TupleToStr(Value.asTuple);
    otSet: Result := SetToStr(Value.asSet);
    otEnumInstance: with Value.asEnumInstance^ do
      Result := Format('%s', [Name^.Chars]);
    otRange: with Value.asRange^ do Result := Format('[%s,%s%s',
      [From.toString, UpTo.toString, specialize IfThen<Char>(isInclusive, ']', ')')]);
    otBuffer: SetString(Result, PChar(Value.asBuffer^.Bytes),
                Value.asBuffer^.Size);
  end;
end;

procedure TypeStrObj(var Result: String; Value: TValue);
begin
  case Obj_Typ(Value) of
    otBoundMethod: Result := FuncToStr(Value.asBoundMethod^.Method^.Func);
    otClass: Result := Format('%s', [Value.asClass^.Name^.Chars]);
    otClosure: Result := FuncToStr(Value.asClosure^.Func);
    otFunc: Result := FuncToStr(Value.asFunc);
    otInstance: Result := Format('%s', [Value.asInstance^.Klass^.Name^.Chars]);
    otNative: Result := '<native fn>';
    otString: Result := 'String';
    otUpValue: Result := 'upvalue';
    otArray: Result := 'Array';
    otDictionary: Result := 'Dictionary';
    otTuple: Result := 'Tuple';
    otSet: Result := 'Set';
    otEnumInstance: Result := Format('Enum(%s)', [Value.asEnumInstance^.Enum^.Name^.Chars]);
    otRange: Result := 'Range';
    otBuffer: Result := 'ByteBuffer';
  end;
end;

//function AreAssignable(Left, Right: TValue): Boolean;
//begin
//  Result := (Left.isString and (Right.isString or Right.isChar)) or
//    (Left.isArray and Right.isArray) or
//    (Left.isDictionary and Right.isDictionary) or
//    (Left.isSet and Right.isSet) or
//    (Left.isTuple and Right.isTuple) or
//    ((Left.isEnumInstance and Right.isEnumInstance) and
//    (Left.asEnumInstance^.Enum = Right.asEnumInstance^.Enum)) or
//    ((Left.isInstance and Right.isInstance) and
//    (Left.asInstance^.Klass^.Name^.Chars = Right.asInstance^.Klass^.Name^.Chars))
//end;

function AreAssignable(Left, Right: TValue): Boolean;
begin
  if Left.isObj then
    case Obj_Typ(Left) of
      otInstance: Result := Right.isInstance and
        (Left.asInstance^.Klass^.Name^.Chars = Right.asInstance^.Klass^.Name^.Chars);
      otString: Result := Right.isString or Right.isChar;
      otArray: Result := Right.isArray;
      otDictionary: Result := Right.isDictionary;
      otSet: Result := Right.isSet;
      otTuple: Result := Right.isTuple;
      otEnumInstance: Result := Right.isEnumInstance and
        (Left.asEnumInstance^.Enum = Right.asEnumInstance^.Enum);
      otRange: Result := Right.isRange;
      otherwise
        Result := False;
    end
  else
    Result := False;
end;

function EqualsObj(Left, Right: TValue): Boolean;
var
  i: Integer;
begin
  if Left.isNil or Right.isNil then Exit(False);
  if Obj_Typ(Left) <> Obj_Typ(Right) then Exit(False);

  case Obj_Typ(Left) of
    otClass: Result := Left.asClass = Right.asClass;
    otInstance: Result := Left.asInstance = Right.asInstance;
    otString: Result := Left.toString = Right.toString;
    otArray:
      begin
        Result := Left.asArray^.Elements.Count = Right.asArray^.Elements.Count;
        if Result then
          for i := 0 to Left.asArray^.Elements.Count-1 do
            if not Left.asArray^.Elements[i].Equals(Right.asArray^.Elements[i]) then
              Exit(False);
      end;
    otDictionary: Result := False;
    otTuple:
      begin
        Result := Left.asTuple^.Fields.Count = Right.asTuple^.Fields.Count;
        if Result then
          for i := 0 to Left.asTuple^.Fields.Count-1 do
            if not Left.asTuple^.Fields.Entries[i].Value
                   .Equals(Right.asTuple^.Fields.Entries[i].Value) then
              Exit(False);
      end;
    otSet:Result := False;
    otEnumInstance:
      begin
        Result := Left.asEnumInstance^.Enum^.Name^.Chars =
                    Right.asEnumInstance^.Enum^.Name^.Chars;
        if Result then
          Result := Left.asEnumInstance^.Name^.Chars = Right.asEnumInstance^.Name^.Chars;
      end;
    otRange:
      begin
        Result := (Left.asRange^.From.asNumber = Right.asRange^.From.asNumber) and
                  (Left.asRange^.UpTo.asNumber = Right.asRange^.UpTo.asNumber) and
                  (Left.asRange^.isInclusive = Right.asRange^.isInclusive);
      end;
    otherwise Result := False;
  end;
end;

procedure PrintObject(Value: TValue);
begin
  case Obj_Typ(Value) of
    otBoundMethod: PrintFunc(Value.asBoundMethod^.Method^.Func);
    otClass: WriteFmt('%s', [Value.asClass^.Name^.Chars]);
    otClosure: PrintFunc(Value.asClosure^.Func);
    otFunc: PrintFunc(Value.asFunc);
    otInstance: WriteFmt('%s instance', [Value.asInstance^.Klass^.Name^.Chars]);
    otNative: Write('<native fn>');
    otString: WriteFmt('%s', [Value.asCString]);
    otUpValue: Write('upvalue');
    otArray: PrintArray(Value.asArray^.Elements);
    otDictionary: PrintDictionary(Value.asDictionary^.Elements);
    otTuple: PrintTuple(Value.asTuple);
    otSet: PrintSet(Value.asSet);
    otEnumInstance: with Value.asEnumInstance^ do
      WriteFmt('%s(%s)', [Name^.Chars, Value.toString]);
    otRange: with Value.asRange^ do WriteFmt('[%d,%d%s',
      [From.asNumber, UpTo.asNumber, specialize IfThen<Char>(isInclusive, ']', ')')]);
    otBuffer: Write('<Byte buffer>');
  end;
end;

{ TValueObjHelper }

function TValueObjHelper.isFunc: Boolean;
begin
  Result := isObjType(Self, otFunc);
end;

function TValueObjHelper.asFunc: PObjFunc;
begin
  Result := PObjFunc(Self.asObj);
end;

function TValueObjHelper.isNative: Boolean;
begin
  Result := isObjType(Self, otNative);
end;

function TValueObjHelper.asNative: TNativeFn;
begin
  Result := PObjNative(Self.asObj)^.Func;
end;

function TValueObjHelper.isClosure: Boolean;
begin
  Result := isObjType(Self, otClosure);
end;

function TValueObjHelper.asClosure: PObjClosure;
begin
  Result := PObjClosure(Self.asObj);
end;

function TValueObjHelper.isClass: Boolean;
begin
  Result := isObjType(Self, otClass);
end;

function TValueObjHelper.asClass: PObjClass;
begin
  Result := PObjClass(Self.asObj);
end;

function TValueObjHelper.isInstance: Boolean;
begin
  Result := isObjType(Self, otInstance);
end;

function TValueObjHelper.asInstance: PObjInstance;
begin
  Result := PObjInstance(Self.asObj);
end;

function TValueObjHelper.isBoundMethod: Boolean;
begin
  Result := isObjType(Self, otBoundMethod);
end;

function TValueObjHelper.asBoundMethod: PObjBoundMethod;
begin
  Result := PObjBoundMethod(Self.asObj);
end;

function TValueObjHelper.isString: Boolean;
begin
  Result := isObjType(Self, otString);
end;

function TValueObjHelper.asString: PObjString;
var
  NumberAsStr, BoolAsStr: String;
  Hash: LongWord;
begin
  if Self.isNumber then
    begin
      NumberAsStr := Self.toString;
      Hash := HashString(PChar(NumberAsStr), NumberAsStr.Length);
      Result := AllocateString(PChar(NumberAsStr), NumberAsStr.Length, Hash);
    end
  else if Self.isBool then
    begin
      BoolAsStr := Self.toString;
      Hash := HashString(PChar(BoolAsStr), BoolAsStr.Length);
      Result := AllocateString(PChar(BoolAsStr), BoolAsStr.Length, Hash);
    end
  else
    Result := PObjString(Self.asObj);
end;

function TValueObjHelper.asCString: PChar;
begin
  Result := Self.asString^.Chars;
end;

function TValueObjHelper.asPString: String;
begin
  Result := StrPas(Self.asCString);
end;

function TValueObjHelper.isArray: Boolean;
begin
  Result := isObjType(Self, otArray);
end;

function TValueObjHelper.asArray: PObjArray;
begin
  Result := PObjArray(Self.asObj);
end;

function TValueObjHelper.isDictionary: Boolean;
begin
  Result := isObjType(Self, otDictionary);
end;

function TValueObjHelper.asDictionary: PObjDictionary;
begin
  Result := PObjDictionary(Self.asObj);
end;

function TValueObjHelper.isTuple: Boolean;
begin
  Result := isObjType(Self, otTuple);
end;

function TValueObjHelper.asTuple: PObjTuple;
begin
  Result := PObjTuple(Self.asObj);
end;

function TValueObjHelper.isSet: Boolean;
begin
  Result := isObjType(Self, otSet);
end;

function TValueObjHelper.asSet: PObjSet;
begin
  Result := PObjSet(Self.asObj);
end;

function TValueObjHelper.isEnumInstance: Boolean;
begin
  Result := isObjType(Self, otEnumInstance);
end;

function TValueObjHelper.asEnumInstance: PObjEnumInstance;
begin
  Result := PObjEnumInstance(Self.asObj);
end;

function TValueObjHelper.isRange: Boolean;
begin
  Result := isObjType(Self, otRange);
end;

function TValueObjHelper.asRange: PObjRange;
begin
  Result := PObjRange(Self.asObj);
end;

function TValueObjHelper.isBuffer: Boolean;
begin
  Result := isObjType(Self, otBuffer);
end;

function TValueObjHelper.asBuffer: PObjBuffer;
begin
  Result := PObjBuffer(Self.asObj);
end;

{ TObjTypHelper }

function TObjTypHelper.toString: String;
begin
  case Self of
    otBoundMethod: Result := 'BoundMethod';
    otClass: Result := 'Class';
    otClosure: Result := 'Closure';
    otFunc: Result := 'Function';
    otInstance: Result := 'Instance';
    otNative: Result := 'Native';
    otString: Result := 'String';
    otUpValue: Result := 'UpValue';
    otArray: Result := 'Array';
    otDictionary: Result := 'Dictionary';
    otTuple: Result := 'Tuple';
    otSet: Result := 'Set';
    otEnumInstance: Result := 'Enum instance';
    otBuffer: Result := 'Byte buffer';
  end;
end;

end.

