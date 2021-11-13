unit uVM;

{$mode delphi}{$H+}
{$I uDefines.inc}

interface

uses
  SysUtils, uChunk, uValue, uObject, uDebug, uCommon, uTable, uCodeGen, uAST;

const
  cMaxFrames = 256;
  cMaxStack = cMaxFrames * cByteCount; // 64K stack

type

  PCallFrame = ^TCallFrame;
  TCallFrame = record
    Closure: PObjClosure;
    IP: PByte;
    Slots: PValue;
  end;

  TInterpretResult = (irInterpretOK, irCompileError, irRuntimeError);

  TVM = record
    Frames: array[0..cMaxFrames-1] of TCallFrame;
    FrameCount: Integer;

    Stack: array[0..cMaxStack-1] of TValue;
    StackTop: PValue;
    Globals: TTable;
    Strings: TTable;
    InitString: PObjString;
    OpenUpValues: PObjUpValue;

    BytesAllocated,
    NextGC: UInt32;

    Objects: PObj;
    GrayCount: Integer;
    GrayCapacity: Integer;
    GrayStack: PPObj;

    FormatSettings: TFormatSettings;

    DebugFile: Text;

    function Interpret(Product: TProduct; const DebugFileName: String
      ): TInterpretResult;
    procedure Push(Value: TValue);
    function Pop: TValue;
    procedure Init;
    procedure Free;
  private
    function BinaryOperator(const Op: Byte): Boolean;
    function BindMethod(Klass: PObjClass; Name: PObjString): Boolean;
    function BindStatic(Klass: PObjClass; Name: PObjString): Boolean;
    function Call(Closure: PObjClosure; const ArgCount: Integer): Boolean;
    function CallBinaryOperator(const Op: Byte): Boolean;
    function CallUnaryOperator(const Op: Byte): Boolean;
    function CallValue(Callee: TValue; const ArgCount: Integer): Boolean;
    function CaptureUpValue(Local: PValue): PObjUpValue;
    procedure CloseUpValues(Last: PValue);
    procedure DefineConstant(const Name: String; Value: TValue);
    //function Concatenate: Boolean;
    procedure DefineMethod(Name: PObjString);
    procedure DefineStatic(Name: PObjString);
    procedure DefineField(Name: PObjString; const isDefault: Boolean);
    procedure DefineNative(const Name: String; Func: TNativeFn);
    procedure DefineNatives;
    function Invoke(Name: PObjString; const ArgCount: Integer): Boolean;
    function InvokeFromClass(Klass: PObjClass; Name: PObjString;
      const ArgCount: Integer): Boolean;
    function isFalsey(Value: TValue): Boolean;
    function Peek(const Distance: Integer): TValue;
    procedure ResetStack;
    function Run: TInterpretResult;
    function RuntimeError(const Format: String;
      Args: array of const): TInterpretResult;
    private
      function getArrayClass: PObjClass;
      function getDictClass: PObjClass;
      function getRangeClass: PObjClass;
      function getSetClass: PObjClass;
    public
//      ArrayClass, DictClass, SetClass, RangeClass: PObjClass;
      property ArrayClass: PObjClass read getArrayClass;
      property DictClass: PObjClass read getDictClass;
      property RangeClass: PObjClass read getRangeClass;
      property SetClass: PObjClass read getSetClass;
  end;

var
  VM: TVM;

implementation
uses uMemory, uNative, math;

procedure TVM.ResetStack;
begin
  StackTop := @Stack[0];
  FrameCount := 0;
  OpenUpValues := Nil;
end;

function TVM.RuntimeError(const Format: String; Args: array of const): TInterpretResult;
var
  Instruction: UInt32;
  i: Integer;
  Frame: PCallFrame;
  Func: PObjFunc;
begin
  Write('Runtime error: ');
  WriteLnFmt(Format, Args);

  for i := FrameCount-1 downto 0 do
    begin
      Frame := @Frames[i];
      Func := Frame^.Closure^.Func;
      // -1 because the IP is sitting on the next instruction to be executed.
      Instruction := Frame^.IP - PByte(Func^.Chunk.Code) - 1;
      WriteFmt('[line %d] in ', [Func^.Chunk.Lines[Instruction]]);
      if Func^.Name = Nil then
        WriteLn('script')
      else
        WriteLnFmt('%s() (%s)', [Func^.Name^.Chars, FileNameList[Func^.FileIndex]]);
    end;

  ResetStack;
  Result := irRuntimeError;
end;

function TVM.getArrayClass: PObjClass;
begin
  Result := Globals.Retrieve('Array').asClass;
end;

function TVM.getDictClass: PObjClass;
begin
  Result := Globals.Retrieve('Dictionary').asClass;
end;

function TVM.getRangeClass: PObjClass;
begin
  Result := Globals.Retrieve('Range').asClass;
end;

function TVM.getSetClass: PObjClass;
begin
  Result := Globals.Retrieve('Set').asClass;
end;

procedure TVM.DefineNative(const Name: String; Func: TNativeFn);
begin
  Push(ObjVal(CopyString(Name)));
  Push(ObjVal(NewNative(Func)));
  Globals.SetValue(Stack[0].asString, Stack[1]);
  Pop;
  Pop;
end;

procedure TVM.DefineConstant(const Name: String; Value: TValue);
begin
  Push(ObjVal(CopyString(Name)));
  Globals.SetValue(Stack[0].asString, Value);
  Pop;
end;

procedure TVM.DefineNatives;
begin
  DefineNative('pi', @PiNative);
  DefineNative('sqrt', @SqrtNative);
  DefineNative('cbrt', @CbrtNative);
  DefineNative('sqr', @SqrNative);
  DefineNative('trunc', @TruncNative);
  DefineNative('round', @RoundNative);
  DefineNative('abs', @AbsNative);
  DefineNative('sin', @SinNative);
  DefineNative('cos', @CosNative);
  DefineNative('exp', @ExpNative);
  DefineNative('ln', @LnNative);
  DefineNative('frac', @FracNative);
  DefineNative('arctan', @ArcTanNative);
  DefineNative('milliseconds', @MilliSecondsNative);
  DefineNative('date', @DateNative);
  DefineNative('time', @TimeNative);
  DefineNative('now', @NowNative);
  DefineNative('today', @TodayNative);
  DefineNative('random', @RandomNative);
  DefineNative('randomize', @RandomizeNative);
  DefineNative('length', @LengthNative);
  DefineNative('floor', @FloorNative);
  DefineNative('ceil', @CeilNative);
  DefineNative('readln', @ReadlnNative);
  DefineNative('numberOf', @NumberOfNative);
  DefineNative('number#of:', @NumberOfNative);
  DefineNative('integerOf', @IntegerOfNative);
  DefineNative('integer#of:', @IntegerOfNative);
  DefineNative('stringOf', @StringOfNative);
  DefineNative('string#of:', @StringOfNative);
  DefineNative('succ', @SuccNative);
  DefineNative('pred', @PredNative);
  DefineNative('ord', @OrdNative);
  DefineNative('chr', @ChrNative);
  DefineNative('assigned', @AssignedNative);
  DefineNative('typeOf', @TypeOfNative);

  //DefineNative('hasField', @HasFieldNative);
  //DefineNative('getField', @GetFieldNative);
  //DefineNative('setField', @SetFieldNative);
  //DefineNative('delField', @DelFieldNative);
  DefineNative('error', @ErrorNative);

  // array
  DefineNative('Array:->add', @ArrayAddNative);
  DefineNative('Array:->add#array:', @ArrayAddArrayNative);
  DefineNative('Array:->add#count:#value:', @ArrayAddCountNative);
  DefineNative('Array:->insert', @ArrayInsertNative);
  DefineNative('Array:->contains', @ArrayContainsNative);
  DefineNative('Array:->indexOf', @ArrayIndexOfNative);
  DefineNative('Array:->index', @ArrayIndexOfNative);
  DefineNative('Array:->clear', @ArrayClearNative);
  DefineNative('Array:->delete', @ArrayDeleteNative);
  DefineNative('Array:->remove', @ArrayRemoveNative);
  DefineNative('Array:->swap', @ArrayExchangeNative);
  DefineNative('Array:->count', @ArrayCountNative);
  DefineNative('Array:->first', @ArrayFirstNative);
  DefineNative('Array:->head', @ArrayFirstNative);
  DefineNative('Array:->last', @ArrayLastNative);
  DefineNative('Array:->tail', @ArrayTailNative);
  DefineNative('Array:->concat', @ArrayConcatNative);
  DefineNative('Array:->equals', @ArrayEqualsNative);
  DefineNative('Array:->iterator', @IteratorNative);
  DefineNative('ArrayIterator:->moveNext', @ArrayIteratorMoveNextNative);
  DefineNative('ArrayIterator:->current', @ArrayIteratorCurrentNative);

  // dictionary
  DefineNative('Dictionary:->add', @DictAddNative);
  DefineNative('Dictionary:->contains#key:', @DictContainsKeyNative);
  DefineNative('Dictionary:->contains#value:', @DictContainsValueNative);
  DefineNative('Dictionary:->delete', @DictDeleteNative);
  DefineNative('Dictionary:->clear', @DictClearNative);
  DefineNative('Dictionary:->keys', @DictKeysNative);
  DefineNative('Dictionary:->values', @DictValuesNative);
  DefineNative('Dictionary:->count', @DictCountNative);
  DefineNative('Dictionary:->iterator', @IteratorNative);
  DefineNative('DictionaryIterator:->moveNext', @DictionaryIteratorMoveNextNative);
  DefineNative('DictionaryIterator:->current', @DictionaryIteratorCurrentNative);

  // set
  DefineNative('Set:->add', @SetAddNative);
  DefineNative('Set:->add#set:', @SetAddSetNative);
  DefineNative('Set:->contains', @SetContainsNative);
  DefineNative('Set:->union#with:', @SetUnionWithNative);
  DefineNative('Set:->intersect#with:', @SetIntersectWithNative);
  DefineNative('Set:->except#with:', @SetExceptWithNative);
  DefineNative('Set:->symmetricExcept#with:', @SetSymmetricExceptWithNative);
  DefineNative('Set:->remove', @SetRemoveNative);
  DefineNative('Set:->clear', @SetClearNative);
  DefineNative('Set:->toArray', @SetToArrayNative);
  DefineNative('Set:->count', @SetCountNative);
  DefineNative('Set:->first', @SetFirstNative);
  DefineNative('Set:->head', @SetFirstNative);
  DefineNative('Set:->last', @SetLastNative);
  DefineNative('Set:->tail', @SetTailNative);
  DefineNative('Set:->iterator', @IteratorNative);
  DefineNative('SetIterator:->moveNext', @SetIteratorMoveNextNative);
  DefineNative('SetIterator:->current', @SetIteratorCurrentNative);

  // range
  DefineNative('Range:->iterator', @IteratorNative);
  DefineNative('Range:->step', @RangeStepNative);
  DefineNative('Range:->toArray', @RangeToArrayNative);
  DefineNative('Range:->toSet', @RangeToSetNative);
  DefineNative('Range:->count', @RangeSizeNative);
  DefineNative('Range:->from', @RangeFromNative);
  DefineNative('Range:->to', @RangeToNative);
  DefineNative('Range:->head', @RangeHeadNative);
  DefineNative('Range:->tail', @RangeTailNative);
  DefineNative('RangeIterator:->moveNext', @RangeIteratorMoveNextNative);
  DefineNative('RangeIterator:->current', @RangeIteratorCurrentNative);

  DefineNative('enumTypeOf', @EnumTypeOfNative);
  // in a command window:
  DefineNative('clrScr', @ClrScrNative);
  DefineNative('windowXY', @WindowXYNative);
  DefineNative('gotoXY', @GotoXYNative);
  // get ref to method from method name
  DefineNative('getMethod', @GetMethodNative);

  // file handling
  DefineNative('fileOpen', @FileOpenNative);
  DefineNative('fileRead', @FileReadNative);
  DefineNative('fileClose', @FileCloseNative);
  DefineNative('fileWrite', @FileWriteNative);
  DefineNative('fileSeek', @FileSeekNative);
  DefineNative('fileCreate', @FileCreateNative);
  DefineNative('readFile', @ReadFileNative); // open, read, close file
  // constants for file handling
  DefineConstant('forReading', NumberVal(0));        // fileOpen
  DefineConstant('forWriting', NumberVal(1));        // fileOpen
  DefineConstant('forReadingWriting', NumberVal(2)); // fileOpen
  DefineConstant('fromBeginning', NumberVal(0));     // fileSeek
  DefineConstant('fromCurrent', NumberVal(1));       // fileSeek
  DefineConstant('fromEnd', NumberVal(2));           // fileSeek
  //utility
  DefineNative('arrayOf', @ArrayOfNative); // buffer file to array
  DefineNative('array#of:', @ArrayOfNative); // buffer file to array
  DefineNative('bytesOf', @BytesOfNative); // string to buffer
  DefineNative('bytes#of:', @BytesOfNative); // string to buffer

  // constants for formatting
  //general, exponent, fixed, number or currency
  DefineConstant('fmtGeneral', NumberVal(1));
  DefineConstant('fmtExponent', NumberVal(2));
  DefineConstant('fmtFixed', NumberVal(3));
  DefineConstant('fmtNumber', NumberVal(4));
  DefineConstant('fmtCurrency', NumberVal(5));
end;

procedure TVM.Init;
begin
  FormatSettings := DefaultFormatSettings;
  ResetStack;
  Objects := Nil;
  BytesAllocated := 0;
  NextGC := 1024*1024;
  GrayCount := 0;
  GrayCapacity := 0;
  GrayStack := Nil;
  Globals.Init;
  Strings.Init;
  InitString := Nil;
  InitString := CopyString('init');
  DefineNatives;
end;

procedure TVM.Free;
begin
  Globals.Free;
  Strings.Free;
  InitString := Nil;
  FreeObjects;
end;

procedure TVM.Push(Value: TValue);
begin
  StackTop^ := Value;
  Inc(StackTop);
end;

function TVM.Pop: TValue;
begin
  Dec(StackTop);
  Result := StackTop^;
end;

function TVM.Peek(const Distance: Integer): TValue;
begin
  Result := StackTop[-1-Distance];
end;

function TVM.Call(Closure: PObjClosure; const ArgCount: Integer): Boolean;
var
  Frame: PCallFrame;
begin
  if ArgCount <> Closure^.Func^.Arity then
    begin
      RuntimeError('Expected %d arguments in function call but got %d.',
        [Closure^.Func^.Arity, ArgCount]);
      Exit(False);
    end;

  if FrameCount = cMaxFrames then
    begin
      RuntimeError('Stack overflow.', []);
      Exit(False);
    end;

  Frame := @Frames[FrameCount];
  Inc(FrameCount);
  Frame^.Closure := Closure;
  Frame^.IP := Closure^.Func^.Chunk.Code;

  Frame^.Slots := StackTop-ArgCount-1;
  Result := True;
end;

function TVM.CallValue(Callee: TValue; const ArgCount: Integer): Boolean;
var
  Native: TNativeFn;
  ObjClass: PObjClass;
  Bound: PObjBoundMethod;
  Initializer: TValue;
begin
  if Callee.isObj then
    case Obj_Typ(Callee) of
      otBoundMethod:
        begin
          Bound := Callee.asBoundMethod;
          StackTop[-ArgCount - 1] := Bound^.Receiver;
          Exit(Call(Bound^.Method, ArgCount));
        end;
      otClass:
        begin
          ObjClass := Callee.asClass;
          StackTop[-ArgCount-1] := ObjVal(NewInstance(ObjClass));
          if ObjClass^.Methods.GetValue(InitString, Initializer) then
            begin
              if (ArgCount = 0) and (Initializer.asClosure^.Func^.Arity>0) then
                Exit(True)
              else
                Exit(Call(Initializer.asClosure, ArgCount))
            end
          else if ArgCount <> 0 then
            begin
              RuntimeError('Expected 0 arguments but got %d.', [ArgCount]);
              Exit(False);
            end;
          Exit(True);
        end;
      otClosure: Result := Call(Callee.asClosure, ArgCount);
      otNative:
        begin
          Native := Callee.asNative;
          Result := Native(ArgCount, StackTop-ArgCount);
          if Result then
            Dec(StackTop, ArgCount)
          else
            RuntimeError(StackTop[-ArgCount-1].asString^.Chars, []);
        end
      else // Non-callable object type.
        writeln('CallValue big problem')
    end
  else
    begin
      RuntimeError('Can only call functions and classes.', []);
      Result := False;
    end;
end;

function TVM.InvokeFromClass(Klass: PObjClass; Name: PObjString; const ArgCount: Integer): Boolean;
var
  Method: TValue;
begin
  if not Klass^.Methods.GetValue(Name, Method) then
    begin
      RuntimeError('Undefined method "%s".', [Name^.Chars]);
      Exit(False);
    end;

  Result := Call(Method.asClosure, ArgCount);
end;

function TVM.Invoke(Name: PObjString; const ArgCount: Integer): Boolean;
var
  Receiver, Value: TValue;
  Instance: PObjInstance;
begin
  Receiver := Peek(ArgCount);

  if not Receiver.isInstance then
    begin
      RuntimeError('Only instances have methods.', []);
      Exit(False);
    end;

  Instance := Receiver.asInstance;

  if Instance^.Fields.GetValue(Name, Value) then
    begin
      StackTop[-ArgCount - 1] := Value;
      Exit(CallValue(Value, ArgCount));
    end;

  Result := InvokeFromClass(Instance^.Klass, Name, ArgCount);
end;

function TVM.BindMethod(Klass: PObjClass; Name: PObjString): Boolean;
var
  Method: TValue;
  Bound: PObjBoundMethod;
begin
  if not Klass^.Methods.GetValue(Name, Method) then
    begin
      RuntimeError('Undefined method "%s".', [Name^.Chars]);
      Exit(False);
    end;

  Bound := NewBoundMethod(Peek(0), Method.asClosure);
  Pop;
  Push(ObjVal(Bound));
  Result := True;
end;

function TVM.BindStatic(Klass: PObjClass; Name: PObjString): Boolean;
var
  StaticMethod: TValue;
  Bound: PObjBoundMethod;
begin
  if not Klass^.Statics.GetValue(Name, StaticMethod) then
    begin
      RuntimeError('Undefined static func "%s".', [Name^.Chars]);
      Exit(False);
    end;

  Bound := NewBoundMethod(Peek(0), StaticMethod.asClosure);
  Pop;
  Push(ObjVal(Bound));
  Result := True;
end;

function TVM.CaptureUpValue(Local: PValue): PObjUpValue;
var
  CreatedUpValue, PrevUpValue, UpValue: PObjUpValue;
begin
  PrevUpValue := Nil;
  UpValue := OpenUpValues;

  while (UpValue <> Nil) and (UpValue^.Location > Local) do
    begin
      PrevUpValue := UpValue;
      UpValue := UpValue^.Next;
    end;

  if (UpValue <> Nil) and (UpValue^.Location = Local) then
    Exit(UpValue);

  CreatedUpValue := NewUpValue(Local);
  CreatedUpValue^.Next := UpValue;

  if PrevUpValue = Nil then
    OpenUpValues := CreatedUpValue
  else
    PrevUpValue^.Next := CreatedUpValue;

  Result := CreatedUpValue;
end;

procedure TVM.CloseUpValues(Last: PValue);
var
  UpValue: PObjUpValue;
begin
  while (OpenUpValues <> Nil) and (OpenUpValues^.Location >= Last) do
    begin
      UpValue := OpenUpValues;
      UpValue^.Closed := UpValue^.Location^;
      UpValue^.Location := @UpValue^.Closed;
      OpenUpValues := UpValue^.Next;
    end;
end;

procedure TVM.DefineMethod(Name: PObjString);
var
  Method: TValue;
  Klass: PObjClass;
begin
  Method := Peek(0);
  Klass := Peek(1).asClass;
  Klass^.Methods.SetValue(Name, Method);
  Pop; // method
end;

procedure TVM.DefineStatic(Name: PObjString);
var
  StaticMethod: TValue;
  Klass: PObjClass;
begin
  StaticMethod := Peek(0);
  Klass := Peek(1).asClass;
  Klass^.Statics.SetValue(Name, StaticMethod);
  Pop;
end;

procedure TVM.DefineField(Name: PObjString; const isDefault: Boolean);
var
  Field: TValue;
  Klass: PObjClass;
begin
  Field := Peek(0);
  Klass := Peek(1).asClass;
  Klass^.Fields.SetValue(Name, Field);
  Pop;
  if isDefault then
    Klass^.DefaultName := Name;
  //else
  //  Klass^.DefaultName := Nil;
end;

function TVM.isFalsey(Value: TValue): Boolean;
begin
  Result := Value.isNil or (Value.isBool and not Value.asBool);
end;

function TVM.BinaryOperator(const Op: Byte): Boolean;
var
  a, b: Double;
  V1, V2: TValue;
  s1, s2: String;
begin
  Result := True;
  if Peek(0).isNumber and Peek(1).isNumber then
    begin
      b := Pop.asNumber;
      a := Pop.asNumber;
      case Op of
        op_Equal:        Push(BoolVal(a=b));
        op_NotEqual:     Push(BoolVal(a<>b));
        op_Greater:      Push(BoolVal(a>b));
        op_GreaterEqual: Push(BoolVal(a>=b));
        op_Less:         Push(BoolVal(a<b));
        op_LessEqual:    Push(BoolVal(a<=b));
        op_Add:          Push(NumberVal(a+b));
        op_Subtract:     Push(NumberVal(a-b));
        op_Multiply:     Push(NumberVal(a*b));
        op_Modulo:       Push(NumberVal(a-(b*int(a/b))));
        op_Divide:       if b=0 then
                           Push(NanVal)
                         else
                           Push(NumberVal(a/b));
        op_Power:        Push(NumberVal(Power(a,b)));
        op_Shift_Left:   Push(NumberVal(Trunc(a) << Trunc(b)));
        op_Shift_Right:  Push(NumberVal(Trunc(a) >> Trunc(b)));
        op_Bitwise_And:  Push(NumberVal(Trunc(a) and Trunc(b)));
        op_Bitwise_Or:   Push(NumberVal(Trunc(a) or Trunc(b)));
        op_Bitwise_XOr:  Push(NumberVal(Trunc(a) xor Trunc(b)));
      end;
    end
  else if (Peek(0).isString and Peek(1).isString) or
          (Peek(0).isChar and Peek(1).isChar) then
    begin
      s2 := Pop.toString;
      s1 := Pop.toString;
      case Op of
        op_Equal:        Push(BoolVal(s1 = s2));
        op_NotEqual:     Push(BoolVal(s1 <> s2));
        op_Greater:      Push(BoolVal(s1 > s2));
        op_GreaterEqual: Push(BoolVal(s1 >= s2));
        op_Less:         Push(BoolVal(s1 < s2));
        op_LessEqual:    Push(BoolVal(s1 <= s2));
        op_Add:          Push(ObjVal(CopyString(s1 + s2)));
      end;
    end
  else if Peek(1).isString and (Op = op_Add) then
    begin
      V2 := Pop;
      V1 := Pop;
      Push(ObjVal(PObj(CopyString(V1.toString + V2.toString))));
    end
  else Result := False;
end;

function TVM.CallBinaryOperator(const Op: Byte): Boolean;
var
  Callee, Argument, Method: TValue;
  Bound: PObjBoundMethod;
  LeftClass, RightClass: PObjClass;
begin
  Result := False;
  Argument := Pop;
  Callee := Pop;

  if Callee.isInstance and Argument.isInstance then
    begin
      LeftClass := Callee.asInstance^.Klass;
      RightClass := Argument.asInstance^.Klass;
    end
  else if Callee.isArray and Argument.isArray then
    begin
      LeftClass := Callee.asArray^.Klass;
      RightClass := Argument.asArray^.Klass;
    end
  else if Callee.isSet and Argument.isSet then
    begin
      LeftClass := Callee.asSet^.Klass;
      RightClass := Argument.asSet^.Klass;
    end
  else if Callee.isEnumInstance and Argument.isEnumInstance then
    begin
      if Op = op_Equal then
          begin
            Push(BoolVal(Callee.Equals(Argument)));
            Exit(True);
          end;
      LeftClass := Callee.asEnumInstance^.Enum;
      RightClass := Argument.asEnumInstance^.Enum;
    end
  else if Op = op_Equal then
    begin
      Push(BoolVal(Callee.Equals(Argument)));
      Exit(True);
    end
  else if Op = op_NotEqual then
    begin
      Push(BoolVal(not Callee.Equals(Argument)));
      Exit(True);
    end
  else Exit(False);

  if LeftClass = RightClass then
    begin
      with LeftClass^ do
      if Methods.GetValue(CopyString('infix_'+Operators[Op]), Method) then
        begin
          Bound := NewBoundMethod(Callee, Method.asClosure);
          Push(Bound^.Receiver);
          Push(Argument);

          Result := Call(Bound^.Method, 1);  // always 1 argument: a.infix_+(b)
        end
    end
end;

function TVM.CallUnaryOperator(const Op: Byte): Boolean;
var
  Callee, Method: TValue;
  Bound: PObjBoundMethod;
  Klasse: PObjClass;
begin
  Result := False;
  Callee := Pop;
  if Callee.isInstance then
    Klasse := Callee.asInstance^.Klass
  else if Callee.isArray then
    Klasse := Callee.asArray^.Klass
  else if Callee.isEnumInstance then
    Klasse := Callee.asEnumInstance^.Enum
  else Exit(False);

  // is prefix operator for this class defined?
  with Klasse^ do
  if Methods.GetValue(CopyString('prefix_'+Operators[Op]), Method) then
    begin
      Bound := NewBoundMethod(Callee, Method.asClosure);
      Push(Bound^.Receiver);

      Result := Call(Bound^.Method, 0);  // always 0 arguments: a.prefix_-()
    end
end;

function TVM.Run: TInterpretResult;
var
  Instruction: Byte;
  Constant, Key, Value, FuncResult, SuperClass, Left, Right, Variable: TValue;
  Precision, Digits, Width, FmtType, CCYSymbol: TValue;
  asPrecision, asDigits: Integer;
  Fmt: TFloatFormat;
  FloatStr: String;
  Slot: PValue;
  SlotNum, isLocal, Index: Byte;
  ArgCount, i, k, l: Integer;
  Name, Method, DefaultName: PObjString;
  OffSet: UInt16;
  Frame: PCallFrame;
  Func: PObjFunc;
  Closure: PObjClosure;
  Instance: PObjInstance;
  SubClass, Super, Klass: PObjClass;
  ArrayList, ArraySlice: PObjArray;
  Dictionary: PObjDictionary;
  Tuple: PObjTuple;
  SetList: PObjSet;
  Range: PObjRange;
  EnumInst: PObjEnumInstance;

  function ReadByte: Byte; inline;
  begin
    Result := Frame^.IP^;
    Inc(Frame^.IP); // point to next instruction
  end;

  function ReadShort: UInt16; inline;
  begin
    Result := (Frame^.IP^ shl 8) or Frame^.IP[1];
    Inc(Frame^.IP, 2);
  end;

  function ReadConstant: TValue; inline;
  begin
    Result := Frame^.Closure^.Func^.Chunk.Constants.Values[ReadShort]
  end;

  function ReadString: PObjString; inline;
  begin
    Result := ReadConstant.asString;
  end;

begin
  Frame := @Frames[FrameCount-1];
  while True do
    begin

      {$ifdef DEBUG_TRACE_EXECUTION}
        Write(DebugFile, '          ');
        Slot := @Stack[0];
        while Slot < StackTop do
          begin
            Write(DebugFile, '[ '); Write(DebugFile, Slot^.toString); Write(DebugFile, ' ]');
            Inc(Slot);
          end;
        Writeln(DebugFile);
        DisassembleInstruction(DebugFile, @Frame^.Closure^.Func^.Chunk,
          LongInt(Frame^.IP - PByte(Frame^.Closure^.Func^.Chunk.Code)));
      {$endif}

      Instruction := ReadByte;
      case Instruction of
        op_Constant0..op_Constant5: Push(NumberVal(Instruction)); // 0..5

        op_Constant:
          begin
            Constant := ReadConstant;
            Push(Constant);
          end;

        op_ConstantMin1: Push(NumberVal(-1));

        op_Nil: Push(NilVal);
        op_True: Push(BoolVal(True));
        op_False: Push(BoolVal(False));

        op_Pop: Pop;
        op_Dup: Push(Peek(0));

        op_Get_Local:
          begin
            SlotNum := ReadByte;
            Push(Frame^.Slots[SlotNum]);
          end;

        op_Set_Local:
          begin
            SlotNum := ReadByte;
            Frame^.Slots[SlotNum] := Peek(0);
          end;

        op_Get_Global:
          begin
            Name := ReadString;
            if not Globals.GetValue(Name, Variable) then
              Exit(RuntimeError('Undefined variable "%s".', [Name^.Chars]));

            Push(Variable);
            // Is it a value property?
            if Variable.isClosure and Variable.asClosure^.Func^.isValProperty then
              begin
                if not CallValue(Peek(0), 0) then
                  Exit(irRuntimeError);
                Frame := @Frames[VM.FrameCount-1];
              end;
          end;

        op_Define_Global:
          begin
            Name := ReadString;
            Globals.SetValue(Name, Peek(0));
            Pop;
          end;

        op_Set_Global:
          begin
            Name := ReadString;
            if Globals.GetValue(Name, Variable) then
              begin
                if Variable.isClosure then
                  Exit(RuntimeError('Cannot assign to value property "%s".',
                    [Name^.Chars]))
              end
            else
              Exit(RuntimeError('Undefined variable "%s".', [Name^.Chars]));

            // check if the global class contains a default variable
            if Variable.isInstance and Assigned(Variable.asInstance^.DefaultName) then
              begin
                DefaultName := Variable.asInstance^.DefaultName;
                Variable.asInstance^.Fields.GetValue(DefaultName, Value);
                if Value.isAssignableFrom(Peek(0)) then
                  Variable.asInstance^.Fields.SetValue(DefaultName, Peek(0))
                else
                  Exit(RuntimeError(
                    'Incompatibe types in assignment -> "%s:%s:=%s".',
                    [DefaultName^.Chars, Value.TypeStr, Peek(0).TypeStr]));
              end
            else if Variable.isAssignableFrom(Peek(0)) then
              Globals.SetValue(Name, Peek(0))
            else
              Exit(RuntimeError(
                'Incompatibe types in assignment -> "%s:%s:=%s".',
                [Name^.Chars, Variable.TypeStr, Peek(0).TypeStr]));
          end;

        op_Get_UpValue:
          begin
            SlotNum := ReadByte;
            Push(Frame^.Closure^.UpValues[SlotNum]^.Location^);
          end;

        op_Set_UpValue:
          begin
            SlotNum := ReadByte;
            Frame^.Closure^.UpValues[SlotNum]^.Location^ := Peek(0);
          end;

        op_Get_Property, op_Get_Property_Nil:
          begin
            if Peek(0).isNil then
              begin
                Name := ReadString;
                if Instruction <> op_Get_Property_Nil then
                  begin
                    Pop;
                    RuntimeError('Instance variable is nil.', []);
                  end // otherwise leave the nil value on the stack
              end
            else
            case Obj_Typ(Peek(0)) of
              otInstance:
                begin
                  Instance := Peek(0).asInstance;
                  Name := ReadString;
                  if Instance^.Fields.GetValue(Name, Variable) then
                    begin
                      if Variable.isClosure then  // it is a 'val' property
                        begin
                          if not Call(Variable.asClosure, 0) then   // zero args
                            Exit(irRuntimeError);
                          Frame := @Frames[VM.FrameCount - 1];
                        end
                      else
                        begin // var field property
                          Pop;  // instance;
                          Push(Variable);
                        end
                    end
                  else if not BindMethod(Instance^.Klass, Name) then
                    Exit(irRuntimeError);
                end;
              otArray:
                begin
                  ArrayList := Peek(0).asArray;
                  Name := ReadString;
                  if ArrayClass^.Fields.GetValue(Name, Value) and Value.isClosure then
                    begin
                      if not Call(Value.asClosure, 0) then   // zero args for value property
                        Exit(irRuntimeError);
                      Frame := @Frames[VM.FrameCount - 1];
                    end
                  else if not BindMethod(ArrayList^.Klass, Name) then
                    Exit(irRuntimeError);
                end;
              otDictionary:
                begin
                  Dictionary := Peek(0).asDictionary;
                  Name := ReadString;
                  if DictClass^.Fields.GetValue(Name, Value) and Value.isClosure then
                    begin
                      if not Call(Value.asClosure, 0) then   // zero args for value property
                        Exit(irRuntimeError);
                      Frame := @Frames[VM.FrameCount - 1];
                    end
                  else if not BindMethod(Dictionary^.Klass, Name) then
                    Exit(irRuntimeError);
                end;
              otTuple:
                begin
                  Tuple := Peek(0).asTuple;
                  Name := ReadString;
                  if Tuple^.Fields.GetValue(Name, Value) then
                    begin
                      Pop;  // tuple;
                      Push(Value);
                    end
                  else
                    Exit(RuntimeError('Unknown property "%s" of tuple.', [Name^.Chars]));
                end;
              otSet:
                begin
                  SetList := Peek(0).asSet;
                  Name := ReadString;
                  if SetClass^.Fields.GetValue(Name, Value) and Value.isClosure then
                    begin
                      if not Call(Value.asClosure, 0) then   // zero args for value property
                        Exit(irRuntimeError);
                      Frame := @Frames[VM.FrameCount - 1];
                    end
                  else if not BindMethod(SetList^.Klass, Name) then
                    Exit(irRuntimeError);
                end;
              otRange:
                begin
                  Range := Peek(0).asRange;
                  Name := ReadString;
                  if RangeClass^.Fields.GetValue(Name, Value) and Value.isClosure then
                    begin
                      if not Call(Value.asClosure, 0) then   // zero args for value property
                        Exit(irRuntimeError);
                      Frame := @Frames[VM.FrameCount - 1];
                    end
                  else if not BindMethod(Range^.Klass, Name) then
                    Exit(irRuntimeError);
                end;
              otClass:
                begin
                  Klass := Peek(0).asClass;
                  if Klass^.Typ = ctClass then
                    begin
                      Name := ReadString;
                      if not BindStatic(Klass, Name) then
                        Exit(irRuntimeError);
                    end
                  else if Klass^.Typ = ctEnum then
                    begin
                      Name := ReadString;
                      if Klass^.Fields.GetValue(Name, Value) then
                        begin
                          EnumInst := NewEnumInstance(Name, Value, Klass);
                          Pop;  // class;
                          Push(ObjVal(EnumInst));
                        end
                      else
                        Exit(RuntimeError(
                          'Expected enum value, but got ".%s".', [Name^.Chars]));
                    end
                  else
                    Exit(RuntimeError('Expected enum type.', []));
                end;
              otEnumInstance:
                begin
                  EnumInst := Peek(0).asEnumInstance;
                  Name := ReadString;
                  if Name^.Chars = 'name' then
                    begin
                      Pop; // enum instance
                      Push(ObjVal(EnumInst^.Name));
                    end
                  else if Name^.Chars = 'value' then
                    begin
                      Pop; // enum instance
                      Push(EnumInst^.Value);
                    end
                  else if EnumInst^.Enum^.Fields.GetValue(Name, Value) and Value.isClosure then
                    begin
                      if not Call(Value.asClosure, 0) then   // zero args for value property
                        Exit(irRuntimeError);
                      Frame := @Frames[VM.FrameCount - 1];
                    end
                  else if not BindMethod(EnumInst^.Enum, Name) then
                    Exit(irRuntimeError);
                end;
              otherwise
                Exit(RuntimeError('Only instances have properties.', []));
            end;
          end;

        op_Set_Property:
          begin
            case Obj_Typ(Peek(1)) of
              otInstance:
                begin
                  Instance := Peek(1).asInstance;

                  Name := ReadString;
                  if Instance^.Fields.GetValue(Name, Value) then
                    begin
                      if Value.isClosure then
                        Exit(RuntimeError('Cannot assign to value property "%s".',
                          [Name^.Chars]))
                    end
                  else
                    Exit(RuntimeError('Undefined variable "%s".', [Name^.Chars]));

                  if Value.isAssignableFrom(Peek(0)) then
                    Instance^.Fields.SetValue(Name, Peek(0))
                  else
                    Exit(RuntimeError(
                      'Incompatibe types in assignment -> "%s:%s:=%s".',
                      [Name^.Chars, Value.TypeStr, Peek(0).TypeStr]));

                  Value := Pop;
                  Pop;  // instance
                  Push(Value);
                end;
              otTuple:
                begin
                  Tuple := Peek(1).asTuple;
                  Tuple^.Fields.SetValue(ReadString, Peek(0));
                  Value := Pop;
                  Pop;
                  Push(Value);
                end;
              else
                Exit(RuntimeError('Only instances have properties.', []));
            end;
          end;

        op_Get_Super:
          begin
            Name := ReadString;
            Super := Pop.asClass;
            if not BindMethod(Super, Name) then
              Exit(irRuntimeError);
          end;

        //op_Equal: Push(BoolVal(Pop.Equals(Pop)));
        //op_NotEqual: Push(BoolVal(not Pop.Equals(Pop)));

        {op_Greater}op_Equal..op_LessEqual:
          if not BinaryOperator(Instruction) then
            if not CallBinaryOperator(Instruction) then
              Exit(RuntimeError('Infix operator "%s" for these types undefined.',
                [Operators[Instruction]]))
            else
              Frame := @Frames[VM.FrameCount - 1];

        op_Question: Push(BoolVal(Pop.isNil));  // check if nil

        op_2Questions: // nil coalescing operator
          begin
            Right := Pop;
            if Peek(0).isNil then // left side is nil?
              begin
                Pop;
                Push(Right)
              end;
          end;

        op_Add..op_Shift_Right:
          if not BinaryOperator(Instruction) then
            if not CallBinaryOperator(Instruction) then
                Exit(RuntimeError('Infix operator "%s" for these types undefined.',
                  [Operators[Instruction]]))
            else
              Frame := @Frames[VM.FrameCount - 1];
          //else
          //  if Peek(0).isQNAN then
          //    Exit(RuntimeError('Division by zero.', []));

        op_Not: Push(BoolVal(isFalsey(Pop)));

        op_Negate:
          begin
            if Peek(0).isNumber then
              Push(NumberVal(-Pop.asNumber))
            else if not CallUnaryOperator(op_Negate) then
              Exit(RuntimeError('Prefix operator "-" for this type undefined.', []))
            else
              Frame := @Frames[VM.FrameCount - 1];
          end;

        op_Apply:
          begin
            if Peek(0).isTuple then
              begin
                Tuple := Pop.asTuple;
                Value := Pop;
                if not Value.isNumber then
                  Exit(RuntimeError('Expected number with format.', []));
                asPrecision := 0;
                if Tuple^.Fields.GetValue(CopyString('precision'), Precision) then
                  if Precision.isNumber then
                    asPrecision := Precision.asInt
                  else Exit(RuntimeError('Expected numeric value.', []));

                asDigits := 0;
                if Tuple^.Fields.GetValue(CopyString('digits'), Digits) then
                  if Digits.isNumber then
                    asDigits := Digits.asInt
                  else Exit(RuntimeError('Expected numeric value.', []));

                if Tuple^.Fields.GetValue(CopyString('format'), FmtType) then
                  begin
                    if FmtType.isNumber then
                      begin
                        case FmtType.asInt of
                          1: Fmt := ffGeneral;
                          2: Fmt := ffExponent;
                          3: Fmt := ffFixed;
                          4: Fmt := ffNumber;
                          5:
                            begin
                              Fmt := ffCurrency;
                              if Tuple^.Fields.GetValue(CopyString('currency'), CCYSymbol) then
                                FormatSettings.CurrencyString := CCYSymbol.asPString;
                                //DefaultFormatSettings.CurrencyString := CCYSymbol.asPString;
                            end;
                          else Fmt := ffFixed;
                        end
                      end
                    else Exit(RuntimeError('Expected formatting type (1..5).', []));
                  end
                else Fmt := ffFixed;

                FloatStr := FloatToStrF(Value.asNumber, Fmt, asPrecision, asDigits, FormatSettings);
                //FloatStr := Value.asNumber
                //  .ToString(Fmt, Round(Precision.asNumber), Round(Digits.asNumber));
                if Tuple^.Fields.GetValue(CopyString('width'), Width) then
                  FloatStr := FloatStr.PadLeft(Round(Width.asNumber));
                Push(ObjVal(CopyString(FloatStr)));
              end
            else
              Exit(RuntimeError('Operand must be a format tuple ("%s", "%s", "%s", "%s").',
                ['precision', 'digits', 'format', 'width']));
          end;

        op_Bitwise_Not:
          begin
            if Peek(0).isNumber then
              Push(NumberVal(not Trunc(Pop.asNumber)))
            else if not CallUnaryOperator(op_Bitwise_Not) then
              Exit(RuntimeError('Prefix operator "!" for this type undefined.', []))
            else
              Frame := @Frames[VM.FrameCount - 1];
          end;

        op_Bitwise_And..op_Bitwise_XOr:
          if not BinaryOperator(Instruction) then
            if not CallBinaryOperator(Instruction) then
              Exit(RuntimeError('Infix operator "%s" for these types undefined.',
                [Operators[Instruction]]))
            else
              Frame := @Frames[VM.FrameCount - 1];

        op_Print: Pop.Print;

        op_Jump:
          begin
            OffSet := ReadShort;
            Inc(Frame^.IP, OffSet);
          end;

        op_Jump_If_False:
          begin
            OffSet := ReadShort;
            if isFalsey(Peek(0)) then Inc(Frame^.IP, OffSet);
          end;

        op_Jump_If_False_Pop:
          begin
            OffSet := ReadShort;
            if isFalsey(Peek(0)) then Inc(Frame^.IP, OffSet);
            Pop;
          end;

        op_Loop:
          begin
            OffSet := ReadShort;
            Dec(Frame^.IP, OffSet);
          end;

        op_Call:
          begin
            ArgCount := ReadByte;
            if not CallValue(Peek(ArgCount), ArgCount) then
              Exit(irRuntimeError);
            Frame := @Frames[FrameCount-1];
          end;

        op_Invoke:
          begin
            Method := ReadString;
            ArgCount := ReadByte;
            if not Invoke(Method, ArgCount) then
              Exit(irRuntimeError);
            Frame := @Frames[FrameCount - 1];
          end;

        op_Super_Invoke:
          begin
            Method := ReadString;
            ArgCount := ReadByte;
            Super := Pop.asClass;
            if not InvokeFromClass(Super, Method, ArgCount) then
              Exit(irRuntimeError);
            Frame := @Frames[FrameCount - 1];
          end;

        op_Closure:
          begin
            Func := ReadConstant.asFunc;
            Closure := NewClosure(Func);
            Push(ObjVal(Closure));
            for i := 0 to Closure^.UpValueCount-1 do
              begin
                isLocal := ReadByte;
                Index := ReadByte;
                if isLocal = 1 then
                  Closure^.UpValues[i] := CaptureUpValue(Frame^.Slots + Index)
                else
                  Closure^.UpValues[i] := Frame^.Closure^.UpValues[Index];
              end;
          end;

        op_Close_UpValue:
          begin
            CloseUpValues(StackTop-1);
            Pop;
          end;

        op_Return:
          begin
            FuncResult := Pop;

            CloseUpValues(Frame^.Slots);

            Dec(FrameCount);
            if FrameCount = 0 then
              begin
                Pop;
                Exit(irInterpretOK);
              end;

            StackTop := Frame^.Slots;
            Push(FuncResult);

            Frame := @Frames[VM.FrameCount-1];
          end;

        op_Class: Push(ObjVal(NewClass(ReadString)));

        op_Inherit:
          begin
            SuperClass := Peek(1);
            if not SuperClass.isClass then
              Exit(RuntimeError('Superclass must be a class.', []));

            SubClass := Peek(0).asClass;
            SubClass^.Methods.AddAll(SuperClass.asClass^.Methods);
            SubClass^.Fields.AddAll(SuperClass.asClass^.Fields);
            Pop; // Subclass
          end;

        op_Method: DefineMethod(ReadString);
        op_Static: DefineStatic(ReadString);

        op_Field: DefineField(ReadString, False);
        op_Field_Def: DefineField(ReadString, True);

        op_InstanceOf:
          begin
            if Peek(0).isClass then
              Klass := Pop.asClass    //get class
            else
              RuntimeError('Class type expected.', []);

            Value := Pop;   //get instance of class or array
            if Value.isInstance then
              begin
                Push(BoolVal(Value.asInstance^.Klass = Klass))
              end
            else if Value.isArray then
              Push(BoolVal(Value.asArray^.Klass = Klass))
            else
              Push(BoolVal(False)); // no match so false returned
          end;

        op_Array:
          begin
            ArgCount := ReadByte; // number of elements
            ArrayList := NewArray(ArrayClass); // instance of standard array class
            for i := 1 to ArgCount do
              ArrayList.Elements.Insert(0, Pop); // using add gives reversed order
            Push(ObjVal(ArrayList));
          end;

        op_Dictionary:
          begin
            ArgCount := ReadByte; // number of elements
            Dictionary := NewDictionary(DictClass); // instance of standard dict class
            for i := 1 to ArgCount do
              Dictionary.Elements.Add(Pop, Pop);
            Push(ObjVal(Dictionary));
          end;

        op_Tuple:
          begin
            ArgCount := ReadByte; // number of elements
            Tuple := NewTuple; // instance of standard tuple class
            for i := 1 to ArgCount do
              begin
                Value := Pop;
                Name := Pop.asString;
                Tuple^.Fields.SetValue(Name, Value);
              end;
            Push(ObjVal(Tuple));
          end;

        op_Set:
          begin
            ArgCount := ReadByte; // number of elements
            SetList := NewSet(SetClass); // instance of standard set class
            for i := 1 to ArgCount do
              SetList.Elements.Add(Pop);
            Push(ObjVal(SetList));
          end;

        op_Range:
          begin
            Left := Pop;  // lower bound
            Right := Pop; // higher bound
            Value := Pop; // is inclusive?
            Range := NewRange(Left, Right, Value, RangeClass);
            Push(ObjVal(Range));
          end;

        op_Enum: Push(ObjVal(NewClass(ReadString, ctEnum)));

        op_ElementOf:
          begin
            case Obj_Typ(Peek(0)) of
              otArray:
                begin
                  ArrayList := Pop.asArray;
                  Value := Pop;
                  Push(BoolVal(ArrayList^.Elements.Contains(Value)));
                end;
              otSet:
                begin
                  SetList := Pop.asSet;
                  Value := Pop;
                  Push(BoolVal(SetList^.Elements.Contains(Value)));
                end;
              otRange:
                begin
                  Range := Pop.asRange;
                  Value := Pop;
                  if (Value.asInt >= Range^.From.asInt) then
                    if Range^.isInclusive then
                      Push(BoolVal(Value.asInt <= Range^.UpTo.asInt))
                    else
                      Push(BoolVal(Value.asInt < Range^.UpTo.asInt))
                  else
                    Push(BoolVal(False));
                end;
              else
                RuntimeError('Expected range, array or set type.', []);
            end;
          end;

        op_Index_Get:
          begin
            Key := Pop; // remove index key
            if Peek(0).isChar then
              begin
                Value := Pop;
                try
                  i := Key.asInt;  // the index
                except
                  Exit(RuntimeError('Index is not numeric.', []));
                end;
                if (i<0) or (i>=Length(Value.asChar)) then
                  Exit(RuntimeError('Index %d out of range (0,%d).',
                    [i, Length(Value.asChar)-1]));
                Push(CharVal(Value.asChar[i+1]));
              end
            else
            case Obj_Typ(Peek(0)) of
              otInstance:
                begin
                  Instance := Peek(0).asInstance; // leave instance on stack
                  Method := CopyString('indexer.getter'); // make PObjString
                  if not BindMethod(Instance^.Klass, Method) then // Bound Method on stack
                    Exit(irRuntimeError);
                  Push(Key); // push argument index of method
                  if not CallValue(Peek(1), 1) then // call method with argument
                    Exit(irRuntimeError);
                  Frame := @Frames[VM.FrameCount-1];
                end;
              otArray:
                begin
                  ArrayList := Pop.asArray;
                  if Key.isRange then
                    begin
                      k := Key.asRange^.From.asInt;
                      l := Key.asRange^.UpTo.asInt;
                      if not Key.asRange^.isInclusive then
                        l := l - 1;
                      ArraySlice := NewArray(ArrayClass); // instance of standard array class
                      for i := k to l do
                        ArraySlice^.Elements.Add(ArrayList^.Elements[i]);
                      Push(ObjVal(ArraySlice));
                    end
                  else
                    begin
                      try
                        i := Key.asInt;  // the array index
                      except
                        Exit(RuntimeError('Index is not numeric.', []));
                      end;
                      if (i<0) or (i>=ArrayList^.Elements.Count) then
                        Exit(RuntimeError('Index %d out of range (0,%d).',
                          [i, ArrayList^.Elements.Count-1]));

                      Value := ArrayList^.Elements[i];
                      Push(Value);
                    end;
                end;

              otDictionary:
                begin
                  Dictionary := Pop.asDictionary;
                  if not Dictionary^.Elements.ContainsKey(Key) then
                    Value := NilVal
                    //Exit(RuntimeError('Dictionary key not found.', []))
                  else
                    Value := Dictionary^.Elements[Key];
                  Push(Value);
                end;

              otString:
                begin
                  Name := Pop.asString;
                  if Key.isRange then
                    begin
                      k := Key.asRange^.From.asInt + 1;
                      l := Key.asRange^.UpTo.asInt;
                      if Key.asRange^.isInclusive then
                        l := l + 1;
                      Push(ObjVal(CopyString(String(Name^.Chars)[k..l])));
                    end
                  else
                    begin
                      try
                        i := Key.asInt;  // the index
                      except
                        Exit(RuntimeError('Index is not numeric.', []));
                      end;
                      if (i<0) or (i>=Name^.Length) then
                        Exit(RuntimeError('Index %d out of range (0,%d).',
                          [i, Name^.Length-1]));
                      Push(CharVal(Name^.Chars[i]));
                    end;
                end;

              otherwise
                Exit(RuntimeError('Variable is not indexable.', []));
            end;
          end;

        op_Index_Set:
          begin
            Value := Pop;
            Key := Pop;
            if Peek(0).isObj then
            case Obj_Typ(Peek(0)) of
              otInstance:
                begin
                  Instance := Peek(0).asInstance; // leave instance on stack
                  Method := CopyString('indexer.setter'); // make PObjString
                  if not BindMethod(Instance^.Klass, Method) then // Bound Method on stack
                    Exit(irRuntimeError);
                  Push(Key); // push argument index of method
                  Push(Value); // push expression to assign to indexed var
                  if not CallValue(Peek(2), 2) then // call method with argument
                    Exit(irRuntimeError);
                  Frame := @Frames[VM.FrameCount-1];
                end;

              otArray:
                begin
                  ArrayList := Pop.asArray;
                  try
                    i := Key.asInt;  // the array index
                  except
                    Exit(RuntimeError('Index is not numeric.', []));
                  end;
                  if (i<0) or (i>=ArrayList^.Elements.Count) then
                    Exit(RuntimeError('Index %d out of range (0,%d).',
                      [i, ArrayList^.Elements.Count-1]));

                  ArrayList^.Elements[i] := Value;
                end;

              otDictionary:
                begin
                  Dictionary := Pop.asDictionary;
                  Dictionary^.Elements.AddOrSetValue(Key, Value);
                  //if not Dictionary^.Elements.ContainsKey(Key) then
                  //  Exit(RuntimeError('Dictionary key not found.', []));
                end;

              otString:
                begin
                  Name := Pop.asString;
                  try
                    i := Key.asInt;  // the index
                  except
                    Exit(RuntimeError('Index is not numeric.', []));
                  end;
                  if (i<0) or (i>=Name^.Length) then
                    Exit(RuntimeError('Index %d out of range (0,%d).',
                      [i, Name^.Length-1]));
                  if (Value.isString or Value.isChar) and (Length(Value.toString) = 1) then
                    Name^.Chars[i] := Value.toString[1]
                  else
                    Exit(RuntimeError('Value to assign is not a single character.', []));
                end;
            end
            else
              Exit(RuntimeError('Cannot assign to this value.', []));
          end;

        otherwise
          WriteLn(Format('VM: Unknown opcode %d', [Instruction]));
          Exit(irRuntimeError);
      end;
    end;
end;


function TVM.Interpret(Product: TProduct; const DebugFileName: String
  ): TInterpretResult;
var
  Func: PObjFunc;
  Closure: PObjClosure;
begin
  try
    Assign(DebugFile, DebugFileName);
    Rewrite(DebugFile);
    Writeln(DebugFile, DateToStr(Date), '; ' , TimeToStr(Time));

    //CodeGenerator := TCodeGenerator.Create(Product, DebugFile);
    //Func := CodeGenerator.Compile;
    Func := GenerateCode(Product, DebugFile);

    if Func = Nil then Exit(irCompileError);

    Push(ObjVal(Func));
    Closure := NewClosure(Func);
    Pop;
    Push(ObjVal(Closure));
    CallValue(ObjVal(Closure), 0);

    Result := Run;
  finally
    //CodeGenerator.Free;
    //Flush(DebugFile);
    Close(DebugFile);
  end;
end;

end.


