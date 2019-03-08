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
  uError, math;

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

  TLength = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TListAdd = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TListInsert = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TListDelete = class(TInterfacedObject, ICallable)
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

  TListKeys = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TListValues = class(TInterfacedObject, ICallable)
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

  TToNum =class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TToStr =class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

implementation
uses uArrayIntf, uDictIntf;

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

{ TLength }

function TLength.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then
    Result := IArrayInstance(Instance).Count
  else if VarSupports(Instance, IDictInstance) then
    Result := IDictInstance(Instance).Count
  else if VarIsStr(Instance) then
    Result := Length(Instance)
  else
    Raise ERuntimeError.Create(Token,
      'Length function not possible for this type.');
end;

{ TListAdd }


function TListAdd.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then begin
    TFunc.CheckArity(Token, ArgList.Count, 2);
    IArrayInstance(Instance).Add(ArgList[1].Value);
    Result := IArrayInstance(Instance);
  end
  else if VarSupports(Instance, IDictInstance) then begin
    TFunc.CheckArity(Token, ArgList.Count, 3);
    IDictInstance(Instance).Add(ArgList[1].Value, ArgList[2].Value);
    Result := IDictInstance(Instance);
  end
  else
    Raise ERuntimeError.Create(Token,
      'listAdd function not possible for this type.');
end;

{ TListInsert }

function TListInsert.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then begin
    TFunc.CheckArity(Token, ArgList.Count, 3);
    IArrayInstance(Instance).Insert(ArgList[1].Value, ArgList[2].Value, Token);
    Result := IArrayInstance(Instance);
  end
  else if VarSupports(Instance, IDictInstance) then begin
    TFunc.CheckArity(Token, ArgList.Count, 4);
    IDictInstance(Instance).Insert(
      ArgList[1].Value, ArgList[2].Value, ArgList[3].Value, Token);
    Result := IDictInstance(Instance);
  end
  else
    Raise ERuntimeError.Create(Token,
      'listInsert function not possible for this type.');
end;

{ TListDelete }

function TListDelete.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then begin
    TFunc.CheckArity(Token, ArgList.Count, 2);
    IArrayInstance(Instance).Delete(ArgList[1].Value, Token);
    Result := IArrayInstance(Instance);
  end
  else if VarSupports(Instance, IDictInstance) then begin
    TFunc.CheckArity(Token, ArgList.Count, 2);
    IDictInstance(Instance).Delete(ArgList[1].Value, Token);
    Result := IDictInstance(Instance);
  end
  else
    Raise ERuntimeError.Create(Token,
      'listDelete function not possible for this type.');
end;

{ TListContains }

function TListContains.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then
    Result := IArrayInstance(Instance).Contains(ArgList[1].Value)
  else if VarSupports(Instance, IDictInstance) then
    Result := IDictInstance(Instance).Contains(ArgList[1].Value)
  else
    Raise ERuntimeError.Create(Token,
      'listContains function not possible for this type.');
end;

{ TListIndexOf }

function TListIndexOf.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then
    Result := IArrayInstance(Instance).IndexOf(ArgList[1].Value)
  else if VarSupports(Instance, IDictInstance) then
    Result := IDictInstance(Instance).IndexOf(ArgList[1].Value)
  else
    Raise ERuntimeError.Create(Token,
      'listIndexOf function not possible for this type.');
end;

{ TListRetrieve }

function TListRetrieve.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then
    Result := IArrayInstance(Instance).Retrieve(ArgList[1].Value, Token)
  else if VarSupports(Instance, IDictInstance) then
    Result := IDictInstance(Instance).Retrieve(ArgList[1].Value, Token)
  else
    Raise ERuntimeError.Create(Token,
      'listRetrieve function not possible for this type.');
end;

{ TListFirst }

function TListFirst.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then begin
    if IArrayInstance(Instance).Count > 0 then
      Result := IArrayInstance(Instance).Elements.First
    else Result := Unassigned;
  end
  else if VarSupports(Instance, IDictInstance) then begin
    if IDictInstance(Instance).Count > 0 then
      Result := IDictInstance(Instance).Elements.Keys[0]
    else Result := Unassigned;
  end
  else if VarIsStr(Instance) then
    Result := String(Instance)[1]
  else
    Raise ERuntimeError.Create(Token,
      'listFirst function not possible for this type.');
end;

{ TListLast }

function TListLast.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IArrayInstance) then begin
    if IArrayInstance(Instance).Count > 0 then
      Result := IArrayInstance(Instance).Elements.Last
    else Result := Unassigned;
  end
  else if VarSupports(Instance, IDictInstance) then begin
    if IDictInstance(Instance).Count > 0 then
      Result := IDictInstance(Instance).Elements.Keys[IDictInstance(Instance).Count-1]
    else Result := Unassigned;
  end
  else if VarIsStr(Instance) then
    Result := String(Instance)[Length(Instance)]
  else
    Raise ERuntimeError.Create(Token,
      'listLast function not possible for this type.');
end;

{ TListKeys }

function TListKeys.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IDictInstance) then begin
    Result := IDictInstance(Instance).Keys;
  end
  else
    Raise ERuntimeError.Create(Token,
      'listKeys function not possible for this type.');
end;

{ TListValues }

function TListValues.Call
  (Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
var
  Instance: Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Instance := ArgList[0].Value;
  if VarSupports(Instance, IDictInstance) then begin
    Result := IDictInstance(Instance).Values;
  end
  else
    Raise ERuntimeError.Create(Token,
      'listValues function not possible for this type.');
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

initialization
  Randomize;
end.

