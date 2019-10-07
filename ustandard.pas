unit uStandard;

{ This unit contains the standard functions.

  This code is based on the online book 'Crafting Interpreters',
  written by Bob Nystrom. http://craftinginterpreters.com
  The code is originally written in Java.

  Copyright (C) 2018 J. de Haan jdehaan2014@gmail.com

  This source is free software; you can redistribute it and/or modify it under the terms
  of the GNU General Public License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU General Public License for more details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free
  Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uInterpreter, uCallable, uToken, uFunc, Variants,
  uError, math, uMemory;

type

  TPi = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TSqrt = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TSqr = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TTrunc = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TRound = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TAbs = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TSin = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TCos = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TExp = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TLn = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TFrac = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TArctan = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TOrd = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TChr = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TMilliSeconds = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TDate = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TTime = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TNow = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TToday = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TRandom = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TRandomLimit = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TRandomize = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TLength = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TAssigned = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TReadLn = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TFloor = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TCeil = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TToNum = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TToStr = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TPred = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TSucc = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TGetMethod = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

procedure StoreStandardFunctions(GlobalSpace: TMemorySpace);

implementation
uses uArrayIntf, uDictIntf, uClassIntf;

{ TPi }

function TPi.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 0);
  Result := pi;
end;

{ TSqrt }

function TSqrt.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Result := Sqrt(ArgList[0].Value);
end;

{ TSqr }

function TSqr.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Result := Sqr(ArgList[0].Value);
end;

{ TTrunc }

function TTrunc.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Result := Trunc(ArgList[0].Value);
end;

{ TRound }

function TRound.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Result := Round(ArgList[0].Value);
end;

{ TAbs }

function TAbs.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Result := Abs(ArgList[0].Value);
end;


{ TSin }

function TSin.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Result := Sin(ArgList[0].Value);
end;

{ TCos }

function TCos.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Result := Cos(ArgList[0].Value);
end;

{ TExp }

function TExp.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Result := Exp(ArgList[0].Value);
end;

{ TLn }

function TLn.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Result := Ln(ArgList[0].Value);
end;

{ TFrac }

function TFrac.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Result := Frac(ArgList[0].Value);
end;

{ TArctan }

function TArctan.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Result := ArcTan(ArgList[0].Value);
end;

{ TOrd }

function TOrd.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Result := Ord(Char(ArgList[0].Value));
end;

{ TChr }

function TChr.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Result := Chr(ArgList[0].Value);
end;

{ TMilliSeconds - milliseconds past midnight}

function TMilliSeconds.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  TS : TTimeStamp;
begin
  TFunc.CheckArity(Token, ArgList.Count, 0);
  TS := DateTimeToTimeStamp(Now);
  Result := TS.Time;
end;

{ TDate }

function TDate.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 0);
  Result := DateToStr(Date);
end;

{ TTime }

function TTime.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 0);
  Result := TimeToStr(Time);
end;

{ TNow }

function TNow.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
var
  ST: TSystemTime;
  S: String;
begin
  TFunc.CheckArity(Token, ArgList.Count, 0);
  DateTimeToSystemTime(Now, ST);
  with ST do
    WriteStr(S, Hour,':',Minute,':',Second,'.',MilliSecond);
  Result := S;
end;

{ TToday }

function TToday.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 0);
  Result := FormatSettings.LongDayNames[DayOfWeek(Date)];
end;

{ TRandom }

function TRandom.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 0);
  Result := Random;
end;

{ TRandomLimit }

function TRandomLimit.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Result := Int(Random(ArgList[0].Value));
end;

{ TRandomize }

function TRandomize.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 0);
  Result := Null;
  Randomize;
end;


{ TLength }

function TLength.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Value: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Value := ArgList[0].Value;
  if VarSupports(Value, IArrayInstance) then
    Result := IArrayInstance(Value).Count
  else if VarSupports(Value, IDictInstance) then
    Result := IDictInstance(Value).Count
  else if VarIsStr(Value) then
    Result := Length(Value)
  else
    Raise ERuntimeError.Create(Token,
      'Length function not possible for this type.');
end;


{ TAssigned }

function TAssigned.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
begin
  Result := ArgList[0].Value <> Unassigned;
end;

{ TReadLn }

function TReadLn.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Input: String;
begin
  try
    Readln(input);
    Result := input;
  except
    Result := Null;
  end;
end;

{ TFloor }

function TFloor.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  Result := Floor(ArgList[0].Value);
end;

{ TCeil }

function TCeil.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  Result := Ceil(ArgList[0].Value);
end;

{ TToNum }

function TToNum.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
var
  Success: Boolean = False;
  Value: Double;
begin
  try
    Success := TryStrToFloat(ArgList[0].Value, Value);
    if Success then
      Result := Value
    else
      Result := Null;
  except
    Result := Null;
  end;
end;

{ TToStr }

function TToStr.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
begin
  try
    Result := VarToStr(ArgList[0].Value);
  except
    Result := Null;
  end;
end;

{ TPred }

function TPred.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
var
  x: Byte;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  x := Byte(ArgList[0].Value);
  Result := Pred(x);
end;

{ TSucc }

function TSucc.Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList
  ): Variant;
var
  x: Byte;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  x := Byte(ArgList[0].Value);
  Result := Succ(x);
end;

{ TGetMethod }

function TGetMethod.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Name: String;
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  Name := ArgList[1].Value;
  if VarSupports(Instance, IGearInstance) then
    begin
      Result := IGearInstance(Instance).getMethod(Name);
      if Result = Null then
        Raise ERuntimeError.Create(Token, Format(
          'Function signature "%s" not found.', [Name]));
    end
  else
    Raise ERuntimeError.Create(Token,
      'Function method() not possible for this type.');
end;


procedure StoreStandardFunctions(GlobalSpace: TMemorySpace);
var
  Token: TToken;
begin
  Token := TToken.Create(ttIdentifier, '', Null, 0, 0);
  GlobalSpace.Store('abs', ICallable(TAbs.Create), Token);
  GlobalSpace.Store('arctan', ICallable(TArctan.Create), Token);
  GlobalSpace.Store('chr', ICallable(TChr.Create), Token);
  GlobalSpace.Store('cos', ICallable(TCos.Create), Token);
  GlobalSpace.Store('date', ICallable(TDate.Create), Token);
  GlobalSpace.Store('exp', ICallable(TExp.Create), Token);
  GlobalSpace.Store('frac', ICallable(TFrac.Create), Token);
  GlobalSpace.Store('length', ICallable(TLength.Create), Token);
  GlobalSpace.Store('ln', ICallable(TLn.Create), Token);
  GlobalSpace.Store('milliseconds', ICallable(TMilliSeconds.Create), Token);
  GlobalSpace.Store('now', ICallable(TNow.Create), Token);
  GlobalSpace.Store('ord', ICallable(TOrd.Create), Token);
  GlobalSpace.Store('pi', ICallable(TPi.Create), Token);
  GlobalSpace.Store('random', ICallable(TRandom.Create), Token);
  GlobalSpace.Store('randomLimit', ICallable(TRandomLimit.Create), Token);
  GlobalSpace.Store('randomize', ICallable(TRandomize.Create), Token);
  GlobalSpace.Store('round', ICallable(TRound.Create), Token);
  GlobalSpace.Store('sin', ICallable(TSin.Create), Token);
  GlobalSpace.Store('sqr', ICallable(TSqr.Create), Token);
  GlobalSpace.Store('sqrt', ICallable(TSqrt.Create), Token);
  GlobalSpace.Store('time', ICallable(TTime.Create), Token);
  GlobalSpace.Store('today', ICallable(TToday.Create), Token);
  GlobalSpace.Store('trunc', ICallable(TTrunc.Create), Token);
  GlobalSpace.Store('assigned', ICallable(TAssigned.Create), Token);
  GlobalSpace.Store('readln', ICallable(TReadLn.Create), Token);
  GlobalSpace.Store('floor', ICallable(TFloor.Create), Token);
  GlobalSpace.Store('ceil', ICallable(TCeil.Create), Token);
  GlobalSpace.Store('toNum', ICallable(TToNum.Create), Token);
  GlobalSpace.Store('toStr', ICallable(TToStr.Create), Token);
  GlobalSpace.Store('pred', ICallable(TPred.Create), Token);
  GlobalSpace.Store('succ', ICallable(TSucc.Create), Token);
  GlobalSpace.Store('method', ICallable(TGetMethod.Create), Token);
end;

end.

