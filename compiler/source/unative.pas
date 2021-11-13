unit uNative;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uValue, uObject;

function PiNative(const ArgCount: Integer; Args: PValue): Boolean;
function SqrtNative(const ArgCount: Integer; Args: PValue): Boolean; // square root
function CbrtNative(const ArgCount: Integer; Args: PValue): Boolean; // cube root
function SqrNative(const ArgCount: Integer; Args: PValue): Boolean;  // square
function TruncNative(const ArgCount: Integer; Args: PValue): Boolean;
function RoundNative(const ArgCount: Integer; Args: PValue): Boolean;
function AbsNative(const ArgCount: Integer; Args: PValue): Boolean;
function SinNative(const ArgCount: Integer; Args: PValue): Boolean;
function CosNative(const ArgCount: Integer; Args: PValue): Boolean;
function ExpNative(const ArgCount: Integer; Args: PValue): Boolean;
function LnNative(const ArgCount: Integer; Args: PValue): Boolean;
function FracNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArcTanNative(const ArgCount: Integer; Args: PValue): Boolean;
function MilliSecondsNative(const ArgCount: Integer; Args: PValue): Boolean;
function DateNative(const ArgCount: Integer; Args: PValue): Boolean;
function TimeNative(const ArgCount: Integer; Args: PValue): Boolean;
function NowNative(const ArgCount: Integer; Args: PValue): Boolean;
function TodayNative(const ArgCount: Integer; Args: PValue): Boolean;
function RandomNative(const ArgCount: Integer; Args: PValue): Boolean;
function RandomizeNative(const ArgCount: Integer; Args: PValue): Boolean;
function LengthNative(const ArgCount: Integer; Args: PValue): Boolean;
function FloorNative(const ArgCount: Integer; Args: PValue): Boolean;
function CeilNative(const ArgCount: Integer; Args: PValue): Boolean;
function ReadLnNative(const ArgCount: Integer; Args: PValue): Boolean;
function NumberOfNative(const ArgCount: Integer; Args: PValue): Boolean;
function IntegerOfNative(const ArgCount: Integer; Args: PValue): Boolean;
function StringOfNative(const ArgCount: Integer; Args: PValue): Boolean;
function SuccNative(const ArgCount: Integer; Args: PValue): Boolean;
function PredNative(const ArgCount: Integer; Args: PValue): Boolean;
function OrdNative(const ArgCount: Integer; Args: PValue): Boolean;
function ChrNative(const ArgCount: Integer; Args: PValue): Boolean;
//function StringToArrayNative(const ArgCount: Integer; Args: PValue): Boolean;
function AssignedNative(const ArgCount: Integer; Args: PValue): Boolean;

function TypeOfNative(const ArgCount: Integer; Args: PValue): Boolean;


//function HasFieldNative(const ArgCount: Integer; Args: PValue): Boolean;
//function GetFieldNative(const ArgCount: Integer; Args: PValue): Boolean;
//function SetFieldNative(const ArgCount: Integer; Args: PValue): Boolean;
//function DelFieldNative(const ArgCount: Integer; Args: PValue): Boolean;

function ErrorNative(const ArgCount: Integer; Args: PValue): Boolean;

// array standard functions
function ArrayAddNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArrayAddArrayNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArrayAddCountNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArrayInsertNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArrayContainsNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArrayDeleteNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArrayRemoveNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArrayClearNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArrayIndexOfNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArrayExchangeNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArrayCountNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArrayFirstNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArrayLastNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArrayTailNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArrayEqualsNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArrayConcatNative(const ArgCount: Integer; Args: PValue): Boolean;

// dictionary standard functions
function DictAddNative(const ArgCount: Integer; Args: PValue): Boolean;
function DictContainsKeyNative(const ArgCount: Integer; Args: PValue): Boolean;
function DictContainsValueNative(const ArgCount: Integer; Args: PValue): Boolean;
function DictDeleteNative(const ArgCount: Integer; Args: PValue): Boolean;
function DictClearNative(const ArgCount: Integer; Args: PValue): Boolean;
function DictKeysNative(const ArgCount: Integer; Args: PValue): Boolean;
function DictValuesNative(const ArgCount: Integer; Args: PValue): Boolean;
function DictCountNative(const ArgCount: Integer; Args: PValue): Boolean;

// set standard functions
function SetAddNative(const ArgCount: Integer; Args: PValue): Boolean;
function SetAddSetNative(const ArgCount: Integer; Args: PValue): Boolean;
function SetContainsNative(const ArgCount: Integer; Args: PValue): Boolean;
function SetUnionWithNative(const ArgCount: Integer; Args: PValue): Boolean;
function SetIntersectWithNative(const ArgCount: Integer; Args: PValue): Boolean;
function SetExceptWithNative(const ArgCount: Integer; Args: PValue): Boolean;
function SetSymmetricExceptWithNative(const ArgCount: Integer; Args: PValue): Boolean;
function SetRemoveNative(const ArgCount: Integer; Args: PValue): Boolean;
function SetClearNative(const ArgCount: Integer; Args: PValue): Boolean;
function SetToArrayNative(const ArgCount: Integer; Args: PValue): Boolean;
function SetCountNative(const ArgCount: Integer; Args: PValue): Boolean;
function SetFirstNative(const ArgCount: Integer; Args: PValue): Boolean;
function SetLastNative(const ArgCount: Integer; Args: PValue): Boolean;
function SetTailNative(const ArgCount: Integer; Args: PValue): Boolean;

// iterator native functions
function IteratorNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArrayIteratorMoveNextNative(const ArgCount: Integer; Args: PValue): Boolean;
function ArrayIteratorCurrentNative(const ArgCount: Integer; Args: PValue): Boolean;
function SetIteratorMoveNextNative(const ArgCount: Integer; Args: PValue): Boolean;
function SetIteratorCurrentNative(const ArgCount: Integer; Args: PValue): Boolean;
function DictionaryIteratorMoveNextNative(const ArgCount: Integer; Args: PValue): Boolean;
function DictionaryIteratorCurrentNative(const ArgCount: Integer; Args: PValue): Boolean;

// range
function RangeStepNative(const ArgCount: Integer; Args: PValue): Boolean;
function RangeToArrayNative(const ArgCount: Integer; Args: PValue): Boolean;
function RangeToSetNative(const ArgCount: Integer; Args: PValue): Boolean;
function RangeSizeNative(const ArgCount: Integer; Args: PValue): Boolean;
function RangeFromNative(const ArgCount: Integer; Args: PValue): Boolean;
function RangeToNative(const ArgCount: Integer; Args: PValue): Boolean;
function RangeHeadNative(const ArgCount: Integer; Args: PValue): Boolean;
function RangeTailNative(const ArgCount: Integer; Args: PValue): Boolean;
function RangeIteratorMoveNextNative(const ArgCount: Integer; Args: PValue): Boolean;
function RangeIteratorCurrentNative(const ArgCount: Integer; Args: PValue): Boolean;


function EnumTypeOfNative(const ArgCount: Integer; Args: PValue): Boolean;

function ClrScrNative(const ArgCount: Integer; Args: PValue): Boolean;
function WindowXYNative(const ArgCount: Integer; Args: PValue): Boolean;
function GotoXYNative(const ArgCount: Integer; Args: PValue): Boolean;

function GetMethodNative(const ArgCount: Integer; Args: PValue): Boolean;

// file handling
function FileOpenNative(const ArgCount: Integer; Args: PValue): Boolean;
function FileReadNative(const ArgCount: Integer; Args: PValue): Boolean;
function FileCloseNative(const ArgCount: Integer; Args: PValue): Boolean;
function FileWriteNative(const ArgCount: Integer; Args: PValue): Boolean;
function FileSeekNative(const ArgCount: Integer; Args: PValue): Boolean;
function FileCreateNative(const ArgCount: Integer; Args: PValue): Boolean;

// open, read and close in 1
function ReadFileNative(const ArgCount: Integer; Args: PValue): Boolean;

// create Gear array from string, buffer,
function ArrayOfNative(const ArgCount: Integer; Args: PValue): Boolean;
function BytesOfNative(const ArgCount: Integer; Args: PValue): Boolean;

implementation
uses math, uTable, uVM, CRT;

function ReturnError(const Msg: String): TValue;
begin
  Result := ObjVal(PObj(CopyString('Error: ' + Msg)));
end;

function ReturnErrorFmt(const Msg: String; Args: Array of const): TValue;
begin
  Result := ObjVal(PObj(CopyString(Format('Error: ' + Msg, Args))));
end;

function GetString(const Msg: String): TValue;
begin
  Result := ObjVal(PObj(CopyString(Msg)));
end;

function PiNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if ArgCount = 0 then
    begin
      Args[-1] := NumberVal(Pi);
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "pi" cannot have arguments.');
  Result := False;
end;

function SqrtNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if (ArgCount = 1) and Args[0].isNumber then
    begin
      Args[-1] := NumberVal(Sqrt(Args[0].asNumber));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "sqrt" requires a numeric argument.');
  Result := False;
end;

function CbrtNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if (ArgCount = 1) and Args[0].isNumber then
    begin
      Args[-1] := NumberVal(exp(ln(Args[0].asNumber)/3));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "sqrt" requires a numeric argument.');
  Result := False;
end;

function SqrNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if (ArgCount = 1) and Args[0].isNumber then
    begin
      Args[-1] := NumberVal(Sqr(Args[0].asNumber));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "sqr" requires one numeric argument.');
  Result := False;
end;

function TruncNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if (ArgCount = 1) and Args[0].isNumber then
    begin
      Args[-1] := NumberVal(Trunc(Args[0].asNumber));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "trunc" requires one numeric argument.');
  Result := False;
end;

function RoundNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if (ArgCount = 1) and Args[0].isNumber then
    begin
      Args[-1] := NumberVal(Round(Args[0].asNumber));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "round" requires one numeric argument.');
  Result := False;
end;

function AbsNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if (ArgCount = 1) and Args[0].isNumber then
    begin
      Args[-1] := NumberVal(Abs(Args[0].asNumber));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "abs" requires one numeric argument.');
  Result := False;
end;

function SinNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if (ArgCount = 1) and Args[0].isNumber then
    begin
      Args[-1] := NumberVal(Sin(Args[0].asNumber));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "sin" requires one numeric argument.');
  Result := False;
end;

function CosNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if (ArgCount = 1) and Args[0].isNumber then
    begin
      Args[-1] := NumberVal(Cos(Args[0].asNumber));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "cos" requires one numeric argument.');
  Result := False;
end;

function ExpNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if (ArgCount = 1) and Args[0].isNumber then
    begin
      Args[-1] := NumberVal(Exp(Args[0].asNumber));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "exp" requires one numeric argument.');
  Result := False;
end;

function LnNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if (ArgCount = 1) and Args[0].isNumber then
    begin
      Args[-1] := NumberVal(Ln(Args[0].asNumber));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "ln" requires one numeric argument.');
  Result := False;
end;

function FracNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if (ArgCount = 1) and Args[0].isNumber then
    begin
      Args[-1] := NumberVal(Frac(Args[0].asNumber));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "frac" requires one numeric argument.');
  Result := False;
end;

function ArcTanNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if (ArgCount = 1) and Args[0].isNumber then
    begin
      Args[-1] := NumberVal(ArcTan(Args[0].asNumber));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "arctan" requires one numeric argument.');
  Result := False;
end;

function MilliSecondsNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  TS: TTimeStamp;
begin
  if ArgCount = 0 then
    begin
      TS := DateTimeToTimeStamp(Now);
      Args[-1] := NumberVal(Double(TS.Time));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "milliseconds" cannot have arguments.');
  Result := False;
end;

function DateNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if ArgCount = 0 then
    begin
      Args[-1] := GetString(DateToStr(Date));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "date" cannot have arguments.');
  Result := False;
end;

function TimeNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if ArgCount = 0 then
    begin
      Args[-1] := GetString(TimeToStr(Time));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "time" cannot have arguments.');
  Result := False;
end;

function NowNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  ST: TSystemTime;
  S: String;
begin
  if ArgCount = 0 then
    begin
      DateTimeToSystemTime(Now, ST);
      with ST do
        WriteStr(S, Hour:2, ':', Minute:2, ':', Second:2, '.', MilliSecond);
      Args[-1] := GetString(S);
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "now" cannot have arguments.');
  Result := False;
end;

function TodayNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  S: String;
begin
  if ArgCount = 0 then
    begin
      S := FormatSettings.LongDayNames[DayOfWeek(Date)];
      Args[-1] := GetString(S);
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "today" cannot have arguments.');
  Result := False;
end;

function RandomNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Value: LongInt;
begin
  if ArgCount = 0 then
    begin
      Args[-1] := NumberVal(Random);
      Exit(True);
    end
  else if ArgCount = 1 then
    begin
      Value := Args[0].asInt;
      Args[-1] := NumberVal(Random(Value));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "random" has 0 or 1 argument.');
  Result := False;
end;

function RandomizeNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if ArgCount = 0 then
    begin
      Randomize;
      Args[-1] := NilVal;
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "randomize" cannot have arguments.');
  Result := False;
end;

function LengthNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if ArgCount = 1 then
    begin
      if Args[0].isChar then
        begin
          Args[-1] := NumberVal(Length(Args[0].asChar));
          Exit(True);
        end
      else
      case Obj_Typ(Args[0]) of
        otString: Args[-1] := NumberVal(StrLen(Args[0].asCString));
        otArray: Args[-1] := NumberVal(Args[0].asArray^.Elements.Count);
        otDictionary: Args[-1] := NumberVal(Args[0].asDictionary^.Elements.Count);
        otTuple: Args[-1] := NumberVal(Args[0].asTuple^.Fields.Count);
        otSet: Args[-1] := NumberVal(Args[0].asSet^.Elements.Count);
        otBuffer: Args[-1] := NumberVal(Args[0].asBuffer^.Size);
        otherwise
          Args[-1] := ReturnError('Function "length" not allowed for this value.');
          Exit(False);
      end;
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "length" requires one argument.');
  Result := False;
end;

function FloorNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if (ArgCount = 1) and Args[0].isNumber then
    begin
      Args[-1] := NumberVal(Floor(Args[0].asNumber));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "floor" requires one numeric argument.');
  Result := False;
end;

function CeilNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if (ArgCount = 1) and Args[0].isNumber then
    begin
      Args[-1] := NumberVal(Ceil(Args[0].asNumber));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "ceil" requires one numeric argument.');
  Result := False;
end;

function ReadLnNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Input: String;
begin
  try
    Readln(Input);
    Args[-1] := ObjVal(PObj(CopyString(Input)));
    Result := True;
  except
    Args[-1] := NilVal;
    Result := True;
  end;
end;

function NumberOfNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Value: Double;
begin
  if (ArgCount = 1) and Args[0].isString then
    begin
      try
        if TryStrToFloat(Args[0].asPString, Value) then
          Args[-1] := NumberVal(Value)
        else
          Args[-1] := NilVal;
        Exit(True);
      except
        Args[-1] := NilVal;
        Exit(True);
      end;
    end;

  Args[-1] := ReturnError('Function "numberOf" requires one string argument.');
  Result := False;
end;

function IntegerOfNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Value: Int64;
begin
  if ArgCount <> 1 then
    begin
      Args[-1] := ReturnError('Function "integerOf" requires one argument.');
      Exit(False);
    end;

  if Args[0].isString then
    begin
      if TryStrToInt64(Args[0].toString, Value) then
        Args[-1] := NumberVal(Value)
      else
        begin
          Args[-1] := ReturnError('Could not covert string to integer.');
          Exit(False);
        end;
    end
  else if Args[0].isNumber then
    begin
      Value := Args[0].asInt;
      Args[-1] := NumberVal(Value);
    end;
end;

function StringOfNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if ArgCount = 1 then
    begin
      try
        Args[-1] := GetString(Args[0].toString);
        Result := True;
      except
        Args[-1] := ReturnError('Could not convert value to string.');
        Result := False;
      end;
      Exit(Result);
    end;

  Args[-1] := ReturnError('Function "stringOf" requires one argument.');
  Result := False;
end;

function SuccNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if (ArgCount = 1) and Args[0].isNumber then
    begin
      Args[-1] := NumberVal(Args[0].asNumber+1);
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "succ" requires one numeric argument.');
  Result := False;
end;

function PredNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if (ArgCount = 1) and Args[0].isNumber then
    begin
      Args[-1] := NumberVal(Args[0].asNumber-1);
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "pred" requires one numeric argument.');
  Result := False;
end;

//function OrdNative(const ArgCount: Integer; Args: PValue): Boolean;
//var
//  s: String;
//begin
//  if (ArgCount = 1) and Args[0].isString then
//    begin
//      s := Args[0].asPString;
//      Args[-1] := NumberVal(Ord(s[1]));
//      Exit(True);
//    end;
//
//  Args[-1] := ReturnError('Function "ord" requires one character argument.');
//  Result := False;
//end;

function OrdNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  s: String;
begin
  if (ArgCount = 1) and (Args[0].isString or Args[0].isChar) then
    begin
      s := Args[0].toString;
      Args[-1] := NumberVal(Ord(s[1]));
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "ord" requires one character argument.');
  Result := False;
end;

function ChrNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  n: Integer;
begin
  if (ArgCount = 1) and Args[0].isNumber then
    begin
      n := Args[0].asInt;
      if (n>=0) and (n<=255) then
        begin
          Args[-1] := ObjVal(CopyString(Chr(n)));
          Exit(True);
        end
    end;

  Args[-1] := ReturnError('Function "chr" requires one number (0..255) argument.');
  Result := False;
end;

function AssignedNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  isAssigned: Boolean;
begin
  if ArgCount = 1 then
    begin
      isAssigned := not Args[0].isNil;
      Args[-1] := BoolVal(isAssigned);
      Exit(True);
    end;

  Args[-1] := ReturnError('Function "assigned" requires one argument.');
  Result := False;
end;

function TypeOfNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Value, Number: TValue;
begin
  if ArgCount = 1 then
    begin
      Value := Args[0];
      if Value.isNumber then
        Number := NumberVal(0)
      else if Value.isBool then
        Number := NumberVal(1)
      else if Value.isObj then
        case Obj_Typ(Value) of
          otClass: Number := NumberVal(3);
          otInstance: Number := NumberVal(3);
          otString: Number := NumberVal(2);
          otArray: Number := NumberVal(4);
          otDictionary: Number := NumberVal(5);
          otSet: Number := NumberVal(6);
          otEnumInstance: Number := NumberVal(7);
          otBoundMethod, otClosure, otFunc, otNative: Number := NumberVal(8);
          else Number := NumberVal(9);
        end
      else Number := NumberVal(9);

      Args[-1] := Number;
      Result := True;
    end
  else
    begin
      Args[-1] := ReturnError('Function "typeOf" requires one argument.');
      Result := False;
    end;
end;

//function TypeOfNative(const ArgCount: Integer; Args: PValue): Boolean;
//begin
//  if ArgCount = 1 then
//    begin
//      if Args[0].isNumber then
//        Args[-1] := NumberVal(0)
//      else if Args[0].isBool then
//        Args[-1] := NumberVal(1)
//      else if Args[0].isString then
//        Args[-1] := NumberVal(2)
//      else if Args[0].isString then
//        Args[-1] := NumberVal(2)
//      else if Args[0].isClass then
//        Args[-1] := NumberVal(3)
//      else if Args[0].isArray then
//        Args[-1] := NumberVal(4)
//      else if Args[0].isDictionary then
//        Args[-1] := NumberVal(5)
//      else if Args[0].isSet then
//        Args[-1] := NumberVal(6)
//      else if Args[0].isEnumInstance then
//        Args[-1] := NumberVal(7)
//      else if Args[0].isNil then
//        Args[-1] := NumberVal(8)
//      else
//        Args[-1] := NumberVal(8);
//
//      Result := True;
//    end
//  else
//    begin
//      Args[-1] := ReturnError('Function "typeOf" requires one argument.');
//      Result := False;
//    end;
//end;

//function StringToArrayNative(const ArgCount: Integer; Args: PValue): Boolean;
//var
//  ArrayList: PObjArray;
//  s: String;
//begin
//  if (ArgCount = 1) and Args[0].isString then
//    begin
//      ArrayList := NewArray(VM.ArrayClass);
//      s := Args[0].asPString;
//      for i := 1 to length(s) do
//        ArrayList^.Elements.Add();
//    end;
//
//end;

//function HasFieldNative(const ArgCount: Integer; Args: PValue): Boolean;
//var
//  Instance: PObjInstance;
//  Dummy: TValue;
//  Correct: Boolean = True;
//begin
//  if (ArgCount <> 2) or not Args[0].isInstance or not Args[1].isString then
//    Correct := False;
//
//  if not Correct then
//    begin
//      Args[-1] := ReturnError('Wrong number/type of arguments.');
//      Exit(False);
//    end
//  else
//    begin
//      Instance := Args[0].asInstance;
//      Args[-1] := BoolVal(Instance^.Fields.GetValue(Args[1].asString, Dummy));
//    end;
//
//  Result := True;
//end;
//
//function GetFieldNative(const ArgCount: Integer; Args: PValue): Boolean;
//var
//  Instance: PObjInstance;
//  Value: TValue;
//  Correct: Boolean = True;
//begin
//  if (ArgCount <> 2) or not Args[0].isInstance or not Args[1].isString then
//    Correct := False;
//
//  if not Correct then
//    begin
//      Args[-1] := ReturnError('Wrong number/type of arguments.');
//      Exit(False);
//    end
//  else
//    begin
//      Instance := Args[0].asInstance;
//      if Instance^.Fields.GetValue(Args[1].asString, Value) then
//        Args[-1] := Value
//      else
//        Args[-1] := NilVal;
//    end;
//
//  Result := True;
//end;
//
//function SetFieldNative(const ArgCount: Integer; Args: PValue): Boolean;
//var
//  Instance: PObjInstance;
//  Correct: Boolean = True;
//begin
//  if (ArgCount <> 3) or not Args[0].isInstance or not Args[1].isString then
//    Correct := False;
//
//  if not Correct then
//    begin
//      Args[-1] := ReturnError('Wrong number/type of arguments.');
//      Exit(False);
//    end
//  else
//    begin
//      Instance := Args[0].asInstance;
//      Instance^.Fields.SetValue(Args[1].asString, Args[2]);
//      Args[-1] := Args[2]
//    end;
//
//  Result := True;
//end;
//
//function DelFieldNative(const ArgCount: Integer; Args: PValue): Boolean;
//var
//  Instance: PObjInstance;
//  Correct: Boolean = True;
//begin
//  if (ArgCount <> 2) or not Args[0].isInstance or not Args[1].isString then
//    Correct := False;
//
//  if not Correct then
//    begin
//      Args[-1] := ReturnError('Wrong number/type of arguments.');
//      Exit(False);
//    end
//  else
//    begin
//      Instance := Args[0].asInstance;
//      Instance^.Fields.Delete(Args[1].asString);
//      Args[-1] := NilVal;
//    end;
//
//  Result := True;
//end;

function ErrorNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if (ArgCount = 1) and Args[0].isString then
    Args[-1] := ObjVal(Args[0].asString)
  else
    Args[-1] := ObjVal(PObj(CopyString('Error!', 6)));

  Result := False;
end;


function ArrayAddNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[0].asArray^.Elements.Add(Args[1]);
  Args[-1] := Args[0];     // return the array back to the stack
  Result := True;
end;

function ArrayAddArrayNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if Args[1].isArray then
    begin
      Args[0].asArray^.Elements.AddRange(Args[1].asArray^.Elements);
      Args[-1] := Args[0];
      Result := True;
    end
  else
    begin
      Args[-1] := ReturnError('Array value expected.');
      Result := False;
    end;
end;

// add to or initialize a new or existing array with count elements of value
function ArrayAddCountNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  i, Count: Integer;
begin
  Count := Args[1].asInt;
  for i := 1 to Count do
    Args[0].asArray^.Elements.Add(Args[2]);
  Args[-1] := Args[0];     // return the array back to the stack
  Result := True;
end;

function ArrayInsertNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if Args[1].isNumber then
    begin
      Args[0].asArray^.Elements.Insert(Args[1].asInt, Args[2]);
      Args[-1] := Args[0];
      Result := True;
    end
  else
    begin
      Args[-1] := ReturnError('Index must be a number.');
      Result := False;
    end;
end;

function ArrayContainsNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[-1] := BoolVal(Args[0].asArray^.Elements.IndexOf(Args[1]) >= 0);
  Result := True;
end;

function CheckIndex(Min, Max, Number: LongInt): LongInt;
begin
  if (Number < Min) or (Number >= Max) then
    Result := -1
  else
    Result := Number;
end;

function ArrayDeleteNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  ArrayList: PObjArray;
  Index: LongInt;
begin
  if Args[1].isNumber then
    begin
      ArrayList := Args[0].asArray;
      Index := CheckIndex(0, ArrayList^.Elements.Count, Args[1].asInt);
      if Index <> -1 then
        begin
          ArrayList^.Elements.Delete(Index);
          Args[-1] := NilVal;
          Result := True;
        end
      else
        begin
          Args[-1] := ReturnErrorFmt('Array index %d out of range 0..<%d.',
            [Args[1].asInt, ArrayList^.Elements.Count]);
          Result := False;
        end;
    end
  else
    begin
      Args[-1] := ReturnError('Index must be a number.');
      Result := False;
    end;
end;

function ArrayRemoveNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Index: LongInt;
begin
  Index := Args[0].asArray^.Elements.IndexOf(Args[1]);
  if Index = -1 then
    Args[-1] := NilVal
  else
    begin
      Args[0].asArray^.Elements.Delete(Index);
      Args[-1] := Args[1];
    end;
  Result := True;
end;

function ArrayClearNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[0].asArray^.Elements.Clear;
  Args[-1] := NilVal;
  Result := True;
end;

function ArrayIndexOfNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[-1] := NumberVal(Args[0].asArray^.Elements.IndexOf(Args[1]));
  Result := True;
end;

function ArrayExchangeNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  ArrayList: PObjArray;
  Index1, Index2: LongInt;
begin
  if Args[1].isNumber and Args[2].isNumber then
    begin
      ArrayList := Args[0].asArray;
      Index1 := CheckIndex(0, ArrayList^.Elements.Count, Args[1].asInt);
      Index2 := CheckIndex(0, ArrayList^.Elements.Count, Args[2].asInt);
      if (Index1 = -1) or (Index2 = -1) then
        begin
          Args[-1] := ReturnErrorFmt('At least 1 Array index (%d, %d) out of range 0..<%d.',
            [Args[1].asInt, Args[2].asInt, ArrayList^.Elements.Count]);
          Result := False;
        end
      else
        begin
          Args[0].asArray^.Elements.Exchange(Index1, Index2);
          Args[-1] := NilVal;
          Result := True;
        end;
    end
  else
    begin
      Args[-1] := ReturnError('Indices must be numeric.');
      Result := False;
    end;
end;

function ArrayCountNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[-1] := NumberVal(Args[0].asArray^.Elements.Count);
  Result := True;
end;

function ArrayFirstNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  List: PObjArray;
begin
  List := Args[0].asArray;
  if List^.Elements.Count > 0 then
    Args[-1] := List^.Elements.First
  else
    Args[-1] := NilVal;
  Result := True;
end;

function ArrayLastNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  List: PObjArray;
begin
  List := Args[0].asArray;
  if List^.Elements.Count > 0 then
    Args[-1] := List^.Elements.Last
  else
    Args[-1] := NilVal;
  Result := True;
end;

function ArrayTailNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  List, NewList: PObjArray;
begin
  List := Args[0].asArray;
  NewList := NewArray(VM.ArrayClass);
  if List^.Elements.Count > 0 then
    begin
      NewList^.Elements.AddRange(List^.Elements);
      NewList^.Elements.Delete(0);
    end;
  Args[-1] := ObjVal(NewList);
  Result := True;
end;

function ArrayEqualsNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Check: Boolean;
  Left, Right: PObjArray;
  i: Longint=0;
begin
  Left := Args[0].asArray;
  Right := Args[1].asArray;
  Check := Left^.Elements.Count = Right^.Elements.Count;
  if Check then
    while i < Left^.Elements.Count do
      begin
        if not Left^.Elements[i].Equals(Right^.Elements[i]) then
          begin
            Check := False;
            Break;
          end;
        Inc(i);
      end;

  Args[-1] := BoolVal(Check);
  Result := True;
end;

function ArrayConcatNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  ArrayList: PObjArray;
begin
  ArrayList := NewArray(VM.ArrayClass);

  ArrayList^.Elements.AddRange(Args[0].asArray^.Elements);
  ArrayList^.Elements.AddRange(Args[1].asArray^.Elements);

  Args[-1] := ObjVal(ArrayList);
  Result := True;
end;


// dictionaries

function DictAddNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  // Args[1] = Key; Args[2]) = Value
  Args[0].asDictionary^.Elements.Add(Args[1], Args[2]);
  Args[-1] := Args[0]; // return the dictionary back to the stack
  Result := True;
end;

function DictContainsKeyNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[-1] := BoolVal(Args[0].asDictionary^.Elements.ContainsKey(Args[1]));
  Result := True;
end;

function DictContainsValueNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[-1] := BoolVal(Args[0].asDictionary^.Elements.ContainsValue(Args[1]));
  Result := True;
end;

function DictDeleteNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Dictionary: PObjDictionary;
  Key: TValue;
begin
  Dictionary := Args[0].asDictionary;
  Key := Args[1];
  if not Dictionary^.Elements.ContainsKey(Key) then
    begin
      Args[-1] := ReturnErrorFmt('Dictionary key not found: "%s".', [Key.toString]);
      Result := False;
    end
  else
    begin
      Dictionary^.Elements.Remove(Key);
      Args[-1] := NilVal;
      Result := True;
    end;
end;

function DictClearNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[0].asDictionary^.Elements.Clear;
  Args[-1] := NilVal;
  Result := True;
end;

function DictKeysNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Dictionary: PObjDictionary;
  ArrayList: PObjArray;
  Key: TValue;
begin
  Dictionary := Args[0].asDictionary;
  ArrayList := NewArray(VM.ArrayClass);
  for Key in Dictionary^.Elements.Keys do
    ArrayList^.Elements.Add(Key);

  Args[-1] := ObjVal(ArrayList);
  Result := True;
end;

function DictValuesNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Dictionary: PObjDictionary;
  ArrayList: PObjArray;
  Value: TValue;
begin
  Dictionary := Args[0].asDictionary;
  ArrayList := NewArray(VM.ArrayClass);
  for Value in Dictionary^.Elements.Values do
    ArrayList^.Elements.Add(Value);

  Args[-1] := ObjVal(ArrayList);
  Result := True;
end;

function DictCountNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[-1] := NumberVal(Args[0].asDictionary^.Elements.Count);
  Result := True;
end;

function SetAddNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[0].asSet^.Elements.Add(Args[1]);
  Args[-1] := Args[0];
  Result := True;
end;

function SetAddSetNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  if Args[1].isSet then
    begin
      Args[0].asSet^.Elements.AddRange(Args[1].asSet^.Elements);
      Args[-1] := Args[0];
      Result := True;
    end
  else
    begin
      Args[-1] := ReturnError('Set value expected.');
      Result := False;
    end;
end;

function SetContainsNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[-1] := BoolVal(Args[0].asSet^.Elements.Contains(Args[1]));
  Result := True;
end;

function SetUnionWithNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  SetList, OtherSet, ResultSet: PObjSet;
begin
  try
    SetList := Args[0].asSet;
    OtherSet := Args[1].asSet;
    ResultSet := NewSet(VM.SetClass);
    ResultSet^.Elements.AddRange(SetList^.Elements);
    ResultSet^.Elements.UnionWith(OtherSet^.Elements);
    Args[-1] := ObjVal(ResultSet);
    Result := True;
  except
    if (not Args[0].isSet) or (not Args[1].isSet) then
      Args[-1] := ReturnError('Variable must be a set.')
    else
      Args[-1] := ReturnError('Generic Set add error.');
    Result := False;
  end;
end;

function SetIntersectWithNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  SetList, OtherSet, ResultSet: PObjSet;
begin
  try
    SetList := Args[0].asSet;
    OtherSet := Args[1].asSet;
    ResultSet := NewSet(VM.SetClass);
    ResultSet^.Elements.AddRange(SetList^.Elements);
    ResultSet^.Elements.IntersectWith(OtherSet^.Elements);
    Args[-1] := ObjVal(ResultSet);
    Result := True;
  except
    if (not Args[0].isSet) or (not Args[1].isSet) then
      Args[-1] := ReturnError('Variable must be a set.')
    else
      Args[-1] := ReturnError('Generic Set add error.');
    Result := False;
  end;
end;

function SetExceptWithNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  SetList, OtherSet, ResultSet: PObjSet;
begin
  try
    SetList := Args[0].asSet;
    OtherSet := Args[1].asSet;
    ResultSet := NewSet(VM.SetClass);
    ResultSet^.Elements.AddRange(SetList^.Elements);
    ResultSet^.Elements.ExceptWith(OtherSet^.Elements);
    Args[-1] := ObjVal(ResultSet);
    Result := True;
  except
    if (not Args[0].isSet) or (not Args[1].isSet) then
      Args[-1] := ReturnError('Variable must be a set.')
    else
      Args[-1] := ReturnError('Generic Set add error.');
    Result := False;
  end;
end;

function SetSymmetricExceptWithNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  SetList, OtherSet, ResultSet: PObjSet;
begin
  try
    SetList := Args[0].asSet;
    OtherSet := Args[1].asSet;
    ResultSet := NewSet(VM.SetClass);
    ResultSet^.Elements.AddRange(SetList^.Elements);
    ResultSet^.Elements.SymmetricExceptWith(OtherSet^.Elements);
    Args[-1] := ObjVal(ResultSet);
    Result := True;
  except
    if (not Args[0].isSet) or (not Args[1].isSet) then
      Args[-1] := ReturnError('Variable must be a set.')
    else
      Args[-1] := ReturnError('Generic Set add error.');
    Result := False;
  end;
end;

function SetRemoveNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[-1] := BoolVal(Args[0].asSet^.Elements.Remove(Args[1]));
  Result := True;
end;

function SetClearNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[0].asSet^.Elements.Clear;
  Args[-1] := NilVal;
  Result := True;
end;

function SetToArrayNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  SetList: PObjSet;
  ArrayList: PObjArray;
begin
  SetList := Args[0].asSet;
  ArrayList := NewArray(VM.ArrayClass);
  ArrayList^.Elements.AddRange(SetList^.Elements);
  Args[-1] := ObjVal(ArrayList);
  Result := True;
end;

function SetCountNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[-1] := NumberVal(Args[0].asSet^.Elements.Count);
  Result := True;
end;


function SetFirstNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  SetList: PObjSet;
begin
  SetList := Args[0].asSet;
  if SetList^.Elements.Count > 0 then
    Args[-1] := SetList^.Elements.ToArray[0]
  else
    Args[-1] := NilVal;
  Result := True;
end;

function SetLastNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  SetList: PObjSet;
  Position: Integer;
begin
  SetList := Args[0].asSet;
  if SetList^.Elements.Count > 0 then
    begin
      Position := SetList^.Elements.Count - 1;
      Args[-1] := SetList^.Elements.ToArray[Position]
    end
  else
    Args[-1] := NilVal;
  Result := True;
end;

function SetTailNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  SetList: PObjSet;
  Value: TValue;
begin
  SetList := Args[0].asSet;
  if SetList^.Elements.Count > 0 then
    begin
      Value := SetList^.Elements.ToArray[0];
      SetList^.Elements.Remove(Value);
    end;

  Args[-1] := ObjVal(SetList);
  Result := True;
end;


procedure SetUpArrayIterator(var List: PObjArray);
begin
  List^.Iterator := NewInstance(VM.Globals.Retrieve('ArrayIterator').asClass);
  with List^.Iterator^.Fields do
    begin
      SetValue(CopyString('_list'), ObjVal(List));
      SetValue(CopyString('_index'), NumberVal(-1));
      SetValue(CopyString('_count'), NumberVal(List^.Elements.Count));
    end;
end;

procedure SetUpDictionaryIterator(var Dictionary: PObjDictionary);
var
  List: PObjArray;
begin
  Dictionary^.Iterator := NewInstance(VM.Globals.Retrieve('DictionaryIterator').asClass);
  with Dictionary^.Iterator^.Fields do
    begin
      SetValue(CopyString('_table'), ObjVal(Dictionary));
      SetValue(CopyString('_index'), NumberVal(-1));
      SetValue(CopyString('_count'), NumberVal(Dictionary^.Elements.Count));

      List := NewArray(VM.ArrayClass);
      List^.Elements.AddRange(Dictionary^.Elements.Keys);
      SetValue(CopyString('_keys'), ObjVal(List));
    end;
end;

procedure SetUpSetIterator(var SetList: PObjSet);
var
  List: PObjArray;
begin
  SetList^.Iterator := NewInstance(VM.Globals.Retrieve('SetIterator').asClass);

  with SetList^.Iterator^.Fields do
    begin
      List := NewArray(VM.ArrayClass);
      List^.Elements.AddRange(SetList^.Elements);
      SetValue(CopyString('_list'), ObjVal(List));
      SetValue(CopyString('_index'), NumberVal(-1));
      SetValue(CopyString('_count'), NumberVal(List^.Elements.Count));
    end;
end;

procedure SetUpRangeIterator(var Range: PObjRange);
begin
  Range^.Iterator := NewInstance(VM.Globals.Retrieve('RangeIterator').asClass);
  with Range^.Iterator^.Fields do
    begin
      SetValue(CopyString('_index'),
        NumberVal(Range^.From.asNumber - Range^.Step.asNumber));
      SetValue(CopyString('_step'), Range^.Step);
      if Range^.isInclusive then
        SetValue(CopyString('_finish'), Range^.UpTo)
      else
        SetValue(CopyString('_finish'), NumberVal(Range^.UpTo.asNumber-1));
    end;
end;

function IteratorNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Range: PObjRange;
  List: PObjArray;
  Dictionary: PObjDictionary;
  SetList: PObjSet;
begin
  case Obj_Typ(Args[0]) of
    otArray:
      begin
        List := Args[0].asArray;
        SetUpArrayIterator(List);   // set up the iterator.
        Args[-1] := ObjVal(List^.Iterator);
      end;
    otDictionary:
      begin
        Dictionary := Args[0].asDictionary;
        SetUpDictionaryIterator(Dictionary);
        Args[-1] := ObjVal(Dictionary^.Iterator);
      end;
    otSet:
      begin
        SetList := Args[0].asSet;
        SetUpSetIterator(SetList);
        Args[-1] := ObjVal(SetList^.Iterator);
      end;
    otRange:
      begin
        Range := Args[0].asRange;
        SetUpRangeIterator(Range);
        Args[-1] := ObjVal(Range^.Iterator);
      end;
  end;
  Result := True;
end;

function ArrayIteratorMoveNextNative(const ArgCount: Integer; Args: PValue
  ): Boolean;
var
  Iterator: PObjInstance;
  Index, Count: TValue;
begin
  Iterator := Args[0].asInstance;
  Iterator^.Fields.GetValue(CopyString('_index'), Index);
  Iterator^.Fields.GetValue(CopyString('_count'), Count);
  Index := NumberVal(Index.asNumber+1);
  Iterator^.Fields.SetValue(CopyString('_index'), Index);

  Args[-1] := BoolVal(Index.asNumber < Count.asNumber);
  Result := True;
end;

function ArrayIteratorCurrentNative(const ArgCount: Integer; Args: PValue
  ): Boolean;
var
  Iterator: PObjInstance;
  Index, List, Item: TValue;
begin
  Iterator := Args[0].asInstance;
  Iterator^.Fields.GetValue(CopyString('_index'), Index);
  Iterator^.Fields.GetValue(CopyString('_list'), List);

  Item := List.asArray^.Elements[Index.asInt];
  Args[-1] := Item;
  Result := True;
end;

function SetIteratorMoveNextNative(const ArgCount: Integer; Args: PValue
  ): Boolean;
var
  Iterator: PObjInstance;
  Index, Count: TValue;
begin
  Iterator := Args[0].asInstance;
  Iterator^.Fields.GetValue(CopyString('_index'), Index);
  Iterator^.Fields.GetValue(CopyString('_count'), Count);
  Index := NumberVal(Index.asNumber+1);
  Iterator^.Fields.SetValue(CopyString('_index'), Index);

  Args[-1] := BoolVal(Index.asNumber < Count.asNumber);
  Result := True;
end;

function SetIteratorCurrentNative(const ArgCount: Integer; Args: PValue
  ): Boolean;
var
  Iterator: PObjInstance;
  Index, List, Item: TValue;
begin
  Iterator := Args[0].asInstance;
  Iterator^.Fields.GetValue(CopyString('_index'), Index);
  Iterator^.Fields.GetValue(CopyString('_list'), List);

  Item := List.asArray^.Elements[Index.asInt];
  Args[-1] := Item;
  Result := True;
end;

function DictionaryIteratorMoveNextNative(const ArgCount: Integer; Args: PValue
  ): Boolean;
var
  Iterator: PObjInstance;
  Index, Count: TValue;
begin
  Iterator := Args[0].asInstance;
  Iterator^.Fields.GetValue(CopyString('_index'), Index);
  Iterator^.Fields.GetValue(CopyString('_count'), Count);
  Index := NumberVal(Index.asNumber+1);
  Iterator^.Fields.SetValue(CopyString('_index'), Index);

  Args[-1] := BoolVal(Index.asNumber < Count.asNumber);
  Result := True;
end;

function DictionaryIteratorCurrentNative(const ArgCount: Integer; Args: PValue
  ): Boolean;
var
  Iterator: PObjInstance;
  Index, Keys, Key, Table, Value: TValue;
  Tuple: PObjTuple;
begin
  Iterator := Args[0].asInstance;
  Iterator^.Fields.GetValue(CopyString('_index'), Index);
  Iterator^.Fields.GetValue(CopyString('_keys'), Keys);
  Iterator^.Fields.GetValue(CopyString('_table'), Table);
  Key := Keys.asArray^.Elements[Index.asInt];
  Value := Table.asDictionary^.Elements[Key];
  Tuple := NewTuple;
  Tuple^.Fields.SetValue(CopyString('key'), Key);
  Tuple^.Fields.SetValue(CopyString('value'), Value);
  Args[-1] := ObjVal(Tuple);
  Result := True;
end;

function RangeStepNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Range: PObjRange;
begin
  Range := Args[0].asRange;
  Range^.Step := Args[1];
  Args[-1] := ObjVal(Range);
  Result := True;
end;

function RangeToArrayNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Range: PObjRange;
  Left, Right, Index: Int64;
  List: PObjArray;
begin
  Range := Args[0].asRange;
  Left := Range^.From.asInt;
  Right := Range^.UpTo.asInt;
  if not Range^.isInclusive then
    Dec(Right);

  List := NewArray(VM.ArrayClass);
  for Index := Left to Right do
    List^.Elements.Add(NumberVal(Index));

  Args[-1] := ObjVal(List);
  Result := True;
end;

function RangeToSetNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Range: PObjRange;
  Left, Right, Index: Int64;
  SetList: PObjSet;
begin
  Range := Args[0].asRange;
  Left := Range^.From.asInt;
  Right := Range^.UpTo.asInt;
  if not Range^.isInclusive then
    Dec(Right);

  SetList := NewSet(VM.SetClass);
  for Index := Left to Right do
    SetList^.Elements.Add(NumberVal(Index));

  Args[-1] := ObjVal(SetList);
  Result := True;
end;

function RangeSizeNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Range: PObjRange;
  Left, Right, Size: Int64;
begin
  Range := Args[0].asRange;
  Left := Range^.From.asInt;
  Right := Range^.UpTo.asInt;
  if Range^.isInclusive then
    Inc(Right);
  Size := Round((Right-Left)/Range^.Step.asInt);

  if Size <= 0 then
    Args[-1] := NumberVal(0)
  else
    Args[-1] := NumberVal(Size);
  Result := True;
end;

function RangeFromNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[-1] := Args[0].asRange^.From;
  Result := True;
end;

function RangeToNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  begin
    Args[-1] := Args[0].asRange^.UpTo;
    Result := True;
  end;
end;

function RangeHeadNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  Args[-1] := Args[0].asRange^.From;
  Result := True;
end;

function RangeTailNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  NewFrom: TValue;
  Range: PObjRange;
begin
  Range := Args[0].asRange;
  NewFrom := NumberVal(Range^.From.asNumber + Range^.Step.asNumber);
  Range^.From := NewFrom;
  Args[-1] := ObjVal(Range);
  Result := True;
end;

function RangeIteratorMoveNextNative(const ArgCount: Integer; Args: PValue
  ): Boolean;
var
  Iterator: PObjInstance;
  Index, Step, Finish: TValue;
  Counter: TValue;
begin
  Iterator := Args[0].asInstance;
  Iterator^.Fields.GetValue(CopyString('_index'), Index);
  Iterator^.Fields.GetValue(CopyString('_step'), Step);
  Iterator^.Fields.GetValue(CopyString('_finish'), Finish);

  Counter := NumberVal(Index.asNumber + Step.asNumber);
  Iterator^.Fields.SetValue(CopyString('_index'), Counter);

  Args[-1] := BoolVal(Counter.asNumber <= Finish.asNumber);
  Result := True;
end;

function RangeIteratorCurrentNative(const ArgCount: Integer; Args: PValue
  ): Boolean;
var
  Iterator: PObjInstance;
  Index: TValue;
begin
  Iterator := Args[0].asInstance;
  Iterator^.Fields.GetValue(CopyString('_index'), Index);
  Args[-1] := Index;
  Result := True;
end;

function EnumTypeOfNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  EnumInstance: PObjEnumInstance;
  Enum: PObjClass;
begin
  try
    EnumInstance := Args[0].asEnumInstance;
    Enum := EnumInstance^.Enum;
    Args[-1] := ObjVal(Enum);
    Result := True;
  except
    Args[-1] := ReturnError('Expected enum.');
    Result := False;
  end;
end;

function ClrScrNative(const ArgCount: Integer; Args: PValue): Boolean;
begin
  // ignore arguments
  Args[-1] := NilVal;
  Result := True;
  ClrScr;
end;

function WindowXYNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  x1,y1,x2,y2: Byte;
begin
  try
    if ArgCount = 4 then
      begin
        x1 := Byte(Args[0].asInt);
        y1 := Byte(Args[1].asInt);
        x2 := Byte(Args[2].asInt);
        y2 := Byte(Args[3].asInt);
        Args[-1] := NilVal;
        Window(x1,y1,x2,y2);
        Exit(True);
      end
    else
      begin
        Args[-1] := ReturnError('Function "windowXY" requires 4 integer arguments.');
        Result := False;
      end;
  except
    Args[-1] := ReturnError('Function "windowXY" requires 4 integer arguments.');
    Result := False;
  end;
end;

function GotoXYNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  x,y: Byte;
begin
  try
    if ArgCount = 2 then
      begin
        x := Byte(Args[0].asInt);
        y := Byte(Args[1].asInt);
        Args[-1] := NilVal;
        GotoXY(x,y);
        Exit(True);
      end
    else
      begin
        Args[-1] := ReturnError('Function "gotoXY" requires 2 integer arguments.');
        Result := False;
      end;
  except
    Args[-1] := ReturnError('Function "gotoXY" requires 2 integer arguments.');
    Result := False;
  end;
end;

function GetMethodNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Instance: PObjInstance;
  Name: PObjString;
  Method: TValue;
begin
  if ArgCount = 2 then
    begin
      try
        Instance := Args[0].asInstance;
        Name := Args[1].asString;
        if Instance^.Klass^.Methods.GetValue(Name, Method) then
          Args[-1] := ObjVal(NewBoundMethod(Args[0], Method.asClosure))
        else
          Args[-1] := NilVal;
        Result := True;
      except
        if not Args[0].isInstance then
          Args[-1] := ReturnError('Class instance expected.')
        else
          Args[-1] := ReturnError('String expected.');
        Result := False;
      end;
    end
  else
    begin
      Args[-1] := ReturnError('Function "getMethod" requires 2 arguments.');
      Result := False;
    end;
end;

// FILE HANDLING routines

function FileOpenNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  FileName: String;
  Handle, Mode: LongInt;
begin
  if ArgCount <> 2 then
    begin
      Args[-1] := ReturnError('Function "fileOpen" requires 2 arguments.');
      Exit(False);
    end;

  try
    FileName := Args[0].asPString;
    Mode := Args[1].asInt;
    Handle := FileOpen(FileName, Mode); //fmOpenRead, fmOpenWrite, fmOpenReadWrite
    Args[-1] := NumberVal(Handle);
    Result := True;
  except
    Args[-1] := ReturnErrorFmt('Error opening file "%s".', [FileName]);
    Result := False;
  end
end;

function FileReadNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Handle, Size: LongInt;
  Bytes: PByte;
  Buffer: PObjBuffer;

  function ReadFile(Handle: THandle; var Buffer: PByte): Integer;
  var
    FileSize: Integer;
  begin
    FileSize := FileSeek(Handle, 0, fsFromEnd);
    Buffer := PByte(AllocMem(FileSize));
    FileSeek(Handle, 0, fsFromBeginning);
    Result := FileRead(Handle, Buffer[0], FileSize);
  end;

begin
  if ArgCount <> 1 then
    begin
      Args[-1] := ReturnError('Function "fileRead" requires 1 argument.');
      Exit(False);
    end;

  try
    Handle := Args[0].asInt;
    Size := ReadFile(Handle, Bytes);
    Buffer := NewBuffer(Size, Bytes);

    Args[-1] := ObjVal(Buffer);
    Result := True;
  except
    Args[-1] := ReturnError('Error reading from file.');
    Result := False;
  end
end;

function FileCloseNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Handle: LongInt;
begin
  if ArgCount <> 1 then
    begin
      Args[-1] := ReturnError('Function "fileClose" requires 1 argument.');
      Exit(False);
    end;

  try
    Handle := Args[0].asInt;
    FileClose(Handle);

    Args[-1] := NilVal;
    Result := True;
  except
    Args[-1] := ReturnError('Error closing file .');
    Result := False;
  end
end;

function FileWriteNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Handle: LongInt;
  Buffer: PObjBuffer;
  Num: LongInt;
begin
  if ArgCount <> 2 then
    begin
      Args[-1] := ReturnError('Function "fileWrite" requires 2 arguments.');
      Exit(False);
    end;

  try
    Handle := Args[0].asInt;
    Buffer := Args[1].asBuffer;
    // num bytes are returned or -1 for error
    Num := FileWrite(Handle, Buffer^.Bytes[0], Buffer^.Size);

    Args[-1] := NumberVal(Num);
    Result := True;
  except
    Args[-1] := ReturnError('Error writing to file.');
    Result := False;
  end
end;

function FileSeekNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Handle, Offset, Origin, i: LongInt;
begin
  if ArgCount <> 3 then
     begin
       Args[-1] := ReturnError('Function "fileSeek" requires 3 arguments.');
       Exit(False);
     end;

   try
     //FileSeek sets the file pointer on position Offset, starting from Origin.
     //If successful, the function returns the new file position, relative to
     // the beginning of the file. From FPC documentation.
     Handle := Args[0].asInt;
     Offset := Args[1].asInt;
     Origin := Args[2].asInt;
     i := FileSeek(Handle, Offset, Origin);

     Args[-1] := NumberVal(i);
     Result := True;
   except
     Args[-1] := ReturnError('Error seeking in file.');
     Result := False;
   end
end;

function FileCreateNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Handle: LongInt;
  FileName: String;
begin
  if ArgCount <> 1 then
    begin
      Args[-1] := ReturnError('Function "fileCreate" requires 1 argument.');
      Exit(False);
    end;

  try
    FileName := Args[0].asPString;
    Handle := FileCreate(FileName);

    Args[-1] := NumberVal(Handle);
    Result := True;
  except
    Args[-1] := ReturnErrorFmt('Error creating file "%s".', [FileName]);
    Result := False;
  end
end;

function ReadFileNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  FileName: String;
  Handle: THandle;
  FileSize, BytesRead: Integer;
  Bytes: PByte;
  Buffer: PObjBuffer;
begin
  try
    try
      FileName := Args[0].asPString;
      Handle := FileOpen(FileName, fmOpenRead);
      if Handle = -1 then
        begin
          Args[-1] := ReturnErrorFmt('Error opening file "%s".', [FileName]);
          Exit(False);
        end;

      FileSize := FileSeek(Handle, 0, fsFromEnd);
      Bytes := PByte(AllocMem(FileSize));
      FileSeek(Handle, 0, fsFromBeginning);
      BytesRead := FileRead(Handle, Bytes[0], FileSize);
      if BytesRead < FileSize then
        begin
          Args[-1] := ReturnErrorFmt('Error reading file "%s".', [FileName]);
          Exit(False);
        end;

      Buffer := NewBuffer(FileSize, Bytes);
      Args[-1] := ObjVal(Buffer);
      Result := True;
    except
      if not Args[0].isString then
        Args[-1] := ReturnError('Expected file name.')
      else
        Args[-1] := ReturnErrorFmt('Error reading file "%s".', [FileName]);
      Result := False;
    end;

  finally
    FileClose(Handle);
  end;
end;

function ArrayOfNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  List: PObjArray;
  Range: PObjRange;
  Buffer: PObjBuffer;
  StringValue: String;
  i: Integer;
begin
  Result := True;
  //VM.ArrayClass := VM.Globals.Retrieve('Array').asClass;
  List := NewArray(VM.ArrayClass);
  if Args[0].isBuffer then
    begin
      Buffer := Args[0].asBuffer;
      for i:=0 to Buffer^.Size-1 do
        List^.Elements.Add(NumberVal(Buffer^.Bytes[i]));
    end
  else if Args[0].isString then
    begin
      StringValue := Args[0].toString;
      for i:=1 to length(StringValue) do
        List^.Elements.Add(CharVal(StringValue[i]));
    end
  else if Args[0].isNumber then
    begin
      StringValue := Args[0].toString;
      for i:=1 to length(StringValue) do
        List^.Elements.Add(NumberVal(StrToInt(StringValue[i])));
    end
  else if Args[0].isRange then
    begin
      Range := Args[0].asRange;
      for i:= Range^.From.asInt to Range^.UpTo.asInt do
        List^.Elements.Add(NumberVal(i));
    end
  else
    Result := False;

  if Result then
    begin
      // set up the iterator.
      //SetUpArrayIterator(List);
      Args[-1] := ObjVal(List);
    end
  else
    Args[-1] := ReturnError('File buffer or string expected.');
end;

function BytesOfNative(const ArgCount: Integer; Args: PValue): Boolean;
var
  Buffer: PObjBuffer;
begin
  if Args[0].isBuffer then
    begin
      Buffer := Args[0].asBuffer;
      Args[-1] := ObjVal(Buffer);
      Result := True;
    end
  else if Args[0].isString then
    begin
      Buffer := String2Bytes(Args[0].asString);
      Args[-1] := ObjVal(Buffer);
      Result := True;
    end
  else
    begin
      Args[-1] := ReturnError('String or File buffer expected.');
      Result := False;
    end;
end;

end.


