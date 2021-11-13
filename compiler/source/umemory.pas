unit uMemory;

{$mode delphi}{$H+}
{$I uDefines.inc}

// Based on:
// Memory: http://craftinginterpreters.com/strings.html#values-and-objects
// Garbage collection: http://craftinginterpreters.com/garbage-collection.html

interface

uses
  SysUtils, uValue, uCommon;


function Grow_Capacity(OldCapacity: LongInt): LongInt;
function Reallocate(Previous: Pointer; OldSize, NewSize: UInt32): Pointer;
procedure MarkObject(Obj: Pointer);
procedure MarkValue(Value: TValue);
procedure CollectGarbage;
procedure FreeObjects;

// not for pointer types, use reallocate in that case directly
// function Grow_Array<T>(Previous: Pointer; const OldSize, NewSize: size_t): Pointer;
// procedure Free_Array<T>(Ptr: Pointer; OldCount: LongInt);

implementation
uses uVM, uChunk, uObject, uTable, uCodeGen;

const
  GC_HEAP_GROW_FACTOR = 2;

function Grow_Capacity(OldCapacity: LongInt): LongInt;
begin
  if OldCapacity < 8 then
    Result := 8
  else
    Result := OldCapacity * 2;
end;

// function Grow_Array<T>(Previous: Pointer; const OldSize, NewSize: size_t): Pointer;
//begin
//  Result := Reallocate(Previous, SizeOf(T)*OldSize, SizeOf(T)*NewSize);
//end;

//generic function Allocate<T>(VM: PWrenVM): Pointer;
//type PT = ^T;
//begin
//  Result := PT(wrenReallocate(vm, Nil, 0, sizeof(T)));
//end;

function Reallocate(Previous: Pointer; OldSize, NewSize: UInt32): Pointer;
begin
  Inc(VM.BytesAllocated, NewSize - OldSize);

  if NewSize > OldSize then
    begin
      {$ifdef DEBUG_STRESS_GC}
      CollectGarbage;
      {$endif}
      if VM.BytesAllocated > VM.NextGC then
        CollectGarbage;
    end;

  if NewSize = 0 then
    begin
      FreeMem(Previous);
      Exit(Nil);
    end;

  Result := ReAllocMem(Previous, NewSize); // FPC heapmanager auto keeps track of old size.
end;

procedure Free<T>(ptr: Pointer);
begin
  Reallocate(ptr, SizeOf(T), 0);
end;

procedure FreeObject(Obj: PObj);
var
  ObjString: PObjString;
  Func: PObjFunc;
  Closure: PObjClosure;
  Instance: PObjInstance;
  Klass: PObjClass;
  ArrayList: PObjArray;
  Dictionary: PObjDictionary;
  Tuple: PObjTuple;
  SetList: PObjSet;
  Buffer: PObjBuffer;
begin
  {$ifdef DEBUG_LOG_GC}
  WriteLnFmt('%p free type %s', [Pointer(Obj), Obj^.Typ.toString]);
  {$endif}

  case Obj^.Typ of
    otBoundMethod: Free<TObjBoundMethod>(Obj);
    otClass:
      begin
        Klass := PObjClass(Obj);
        Klass^.Methods.Free;
        Klass^.Statics.Free;
        Free<TObjClass>(Obj);
      end;
    otClosure:
      begin
        Closure := PObjClosure(Obj);
        Reallocate(Closure^.UpValues, SizeOf(PObjUpValue)*Closure^.UpValueCount, 0);
        Free<TObjClosure>(Obj);
      end;
    otFunc:
      begin
        Func := PObjFunc(Obj);
        Func^.Chunk.Free;
        Func^.DeferList := Nil;
        Free<TObjFunc>(Obj);
      end;
    otInstance:
      begin
        Instance := PObjInstance(Obj);
        Instance^.Fields.Free;
        Free<TObjInstance>(Obj);
      end;
    otNative: Free<TObjNative>(Obj);
    otString:
      begin
        ObjString := PObjString(Obj);
        StrDispose(ObjString^.Chars);
        Free<TObjString>(Obj);
      end;
    otUpValue:
      Free<TObjUpValue>(Obj);
    otArray:
      begin
        ArrayList := PObjArray(Obj);
        ArrayList^.Elements.Free;
        Free<TObjArray>(Obj);
      end;
    otDictionary:
      begin
        Dictionary := PObjDictionary(Obj);
        Dictionary^.Elements.Free;
        Free<TObjDictionary>(Obj);
      end;
    otTuple:
      begin
        Tuple := PObjTuple(Obj);
        Tuple^.Fields.Free;
        Free<TObjTuple>(Obj);
      end;
    otSet:
      begin
        SetList := PObjSet(Obj);
        SetList^.Elements.Free;
        Free<TObjSet>(Obj);
      end;
    otEnumInstance:
      Free<TObjEnumInstance>(Obj);
    otRange:
      begin
        Free<TObjRange>(Obj);
      end;
    otBuffer:
      begin
        Buffer := PObjBuffer(Obj);
        Buffer^.Bytes := Nil;
        Free<TObjBuffer>(Obj);
      end;
  end;
end;

procedure MarkObject(Obj: Pointer);
var
  TheObj: PObj;
begin
  if Obj = Nil then Exit;
  TheObj := PObj(Obj);
  if TheObj^.isMarked then Exit;

  {$ifdef DEBUG_LOG_GC}
  WriteFmt('%p mark ', [Pointer(TheObj)]);
  ObjVal(TheObj).Print;
  WriteLn;
  {$endif}

  TheObj^.isMarked := True;

  if VM.GrayCapacity < VM.GrayCount + 1 then
    begin
      VM.GrayCapacity := Grow_Capacity(VM.GrayCapacity);
      VM.GrayStack := PPObj(ReAllocMem(VM.GrayStack, SizeOf(PObj)*VM.GrayCapacity));
    end;

  VM.GrayStack[VM.GrayCount] := TheObj;
  Inc(VM.GrayCount);
end;

procedure MarkValue(Value: TValue);
begin
  if Value.isObj then
    MarkObject(Value.asObj);
end;

procedure MarkArray(var ValueArray: TValueArray);
var
  i: Integer;
begin
  for i := 0 to ValueArray.Count-1 do
    MarkValue(ValueArray.Values[i]);
end;

procedure BlackenObject(Obj: PObj);
var
  Func: PObjFunc;
  Closure: PObjClosure;
  i: Integer;
  Class_: PObjClass;
  Instance: PObjInstance;
  Bound: PObjBoundMethod;
  ArrayList: PObjArray;
  Dictionary: PObjDictionary;
  Tuple: PObjTuple;
  SetList: PObjSet;
  Value: TValue;
  EnumInstance: PObjEnumInstance;
begin
  {$ifdef DEBUG_LOG_GC}
  WriteFmt('%p blacken ', [Obj]);
  ObjVal(Obj).Print;
  WriteLn;
  {$endif}

  case Obj^.Typ of
    otBoundMethod:
      begin
        Bound := PObjBoundMethod(Obj);
        MarkValue(Bound^.Receiver);
        MarkObject(PObj(Bound^.Method));
      end;
    otClass:
      begin
        Class_ := PObjClass(Obj);
        MarkObject(PObj(Class_^.Name));
        MarkTable(@Class_^.Methods);
        MarkTable(@Class_^.Statics);
        MarkTable(@Class_^.Fields);
      end;
    otClosure:
      begin
        Closure := PObjClosure(Obj);
        MarkObject(PObj(Closure^.Func));
        for i := 0 to Closure^.UpValueCount-1 do
          MarkObject(PObj(Closure^.UpValues[i]));
      end;
    otFunc:
      begin
        Func := PObjFunc(Obj);
        MarkObject(PObj(Func^.Name));
        MarkArray(Func^.Chunk.Constants);
      end;
    otInstance:
      begin
        Instance := PObjInstance(Obj);
        MarkObject(PObj(Instance^.Klass));
        MarkTable(@Instance^.Fields);
      end;
    otUpValue: MarkValue(PObjUpValue(Obj)^.Closed);
    otNative,
    otString:;
    otArray:
      begin
        ArrayList := PObjArray(Obj);
        for i := 0 to ArrayList^.Elements.Count-1 do
          MarkValue(ArrayList^.Elements[i]);
        MarkObject(PObj(ArrayList^.Iterator));
        MarkObject(PObj(ArrayList^.Klass));
      end;
    otDictionary:
      begin
        Dictionary := PObjDictionary(Obj);
        for Value in Dictionary^.Elements.Values do
          MarkValue(Value);
        MarkObject(PObj(Dictionary^.Iterator));
        MarkObject(PObj(Dictionary^.Klass));
      end;
    otTuple:
      begin
        Tuple := PObjTuple(Obj);
        MarkTable(@Tuple^.Fields);
      end;
    otSet:
      begin
        SetList := PObjSet(Obj);
        for Value in SetList^.Elements do
          MarkValue(Value);
        MarkObject(PObj(SetList^.Iterator));
        MarkObject(PObj(SetList^.Klass));
      end;
    otEnumInstance:
      begin
        EnumInstance := PObjEnumInstance(Obj);
        MarkObject(PObj(EnumInstance^.Enum));
        MarkObject(PObj(EnumInstance^.Name));
        MarkValue(EnumInstance^.Value);
      end;
    otRange: MarkObject(PObj(PObjRange(Obj)^.Iterator));
    otBuffer:;
  end;
end;

procedure MarkRoots;
var
  Slot: PValue;
  i: Integer;
  UpValue: PObjUpValue;
begin
  Slot := @VM.Stack[0];
  while Slot < VM.StackTop do
    begin
      MarkValue(Slot^);
      Inc(Slot);
    end;

  for i := 0 to VM.FrameCount-1 do
    MarkObject(PObj(VM.Frames[i].Closure));

  UpValue := VM.OpenUpValues;
  while UpValue <> Nil do
    begin
      MarkObject(PObj(UpValue));
      UpValue := UpValue^.Next;
    end;

  MarkTable(@VM.Globals);
  CodeGenerator.MarkCompilerRoots;
  MarkObject(PObj(VM.InitString));
end;

procedure TraceReferences;
var
  Obj: PObj;
begin
  while VM.GrayCount > 0 do
    begin
      Dec(VM.GrayCount);
      Obj := VM.GrayStack[VM.GrayCount];
      BlackenObject(Obj);
    end;
end;

procedure Sweep;
var
  Obj, Unreached: PObj;
  Previous: PObj = Nil;
begin
  Obj := VM.Objects;
  while Obj <> Nil do
    begin
      if Obj^.isMarked then
        begin
          Obj^.isMarked := False;
          Previous := Obj;
          Obj := Obj^.Next;
        end
      else
        begin
          Unreached := Obj;

          Obj := Obj^.Next;
          if Previous <> Nil then
            Previous^.Next := Obj
          else
            VM.Objects := Obj;

          FreeObject(Unreached);
        end;
    end;
end;

procedure CollectGarbage;
{$ifdef DEBUG_LOG_GC}
var
  Before: size_t;
  {$endif}
begin
  {$ifdef DEBUG_LOG_GC}
  WriteLn('-- gc begin');
  Before := VM.BytesAllocated;
  {$endif}

  MarkRoots;
  TraceReferences;
  TableRemoveWhite(@VM.Strings);
  Sweep;

  VM.NextGC := VM.BytesAllocated * GC_HEAP_GROW_FACTOR;

  {$ifdef DEBUG_LOG_GC}
  WriteLn('-- gc end');
  WriteLnFmt('   collected %ld bytes (from %ld to %ld) next at %ld',
    [Before - VM.BytesAllocated, Before, VM.BytesAllocated, VM.NextGC]);
  {$endif}
end;

procedure FreeObjects;
var
  Obj, Next: PObj;
begin
  Obj := VM.Objects;
  while Obj <> Nil do
    begin
      Next := Obj^.Next;
      FreeObject(Obj);
      Obj := Next;
    end;

  FreeMem(VM.GrayStack);
end;

// procedure Free_Array<T>(Ptr: Pointer; OldCount: LongInt);
//begin
//  Reallocate(Ptr, SizeOf(T)*OldCount, 0);
//end;

end.


