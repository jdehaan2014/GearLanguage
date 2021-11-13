unit uMath;

{ This unit contains standard math functions used in the interpreter.

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
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, uToken, uError, math, Variants;

type
  TMath = record
    private const
      ErrIncompatibleOperands = 'Incompatible operand types for "%s" operation.';
      ErrMustBeBothNumber  = 'Both operands must be a number for "%s" operation.';
      ErrMustBeNumber = 'Operand must be a number for "%s" operation.';
      ErrMustBeBoolean = 'Operand must be a boolean for "%s" operation.';
      ErrMustBeBothBoolean = 'Both operands must be a boolean.';
      ErrDivByZero = 'Division by zero.';
      ErrConcatNotAllowed = 'Both types must be array for concat operation.';
      ErrArrayWrongTypes = 'Both variables must be array of same type.';
      ErrArrayMismatchElem = 'Mismatch in number of array elements in array operation.';
      ErrIncompatibleTypes = 'Incompatible types in assignment: %s vs. %s.';

   public
    class function _Add(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _Sub(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _Mul(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _Div(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _Rem(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _Or(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _And(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _XOr(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _Shl(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _Shr(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _Pow(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _Neg(const Value: Variant; Op: TToken): Variant; static;
    class function _Not(const Value: Variant; Op: TToken): Variant; static;

    class function _EQ(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _NEQ(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _GT(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _GE(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _LT(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _LE(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _In(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _Is(const Left, Right: Variant; Op: TToken): Variant; static;

    // boolean checks
    class function areBothNumber(const Value1, Value2: Variant): Boolean; static;
    class function areBothString(const Value1, Value2: Variant): Boolean; static;
    class function areBothBoolean(const Value1, Value2: Variant): Boolean; static;
    class function oneOfBothBoolean(const Value1, Value2: Variant): Boolean; static;
    class function oneOfBothNull(const Value1, Value2: Variant): Boolean; static;

    // array math
    class function areBothArray(const Left, Right: Variant): Boolean; static;
    class function _Concat(const Left, Right: Variant; Op: TToken): Variant; static;
    class function sameArrayTypes(const Left, Right: Variant): Boolean; static;
    class function addTwoArrays(const Left, Right: Variant; Op: TToken
      ): Variant; static;
    class function addToArray(const Left, Right: Variant; Op: TToken): Variant; static;
    class function subTwoArrays(const Left, Right: Variant; Op: TToken
      ): Variant;static;
    class function subNumberFromArray(const Left, Right: Variant; Op: TToken
      ): Variant;static;
    class function mulTwoArrays(const Left, Right: Variant; Op: TToken
      ): Variant;static;
    class function mulNumberToArray(const Left, Right: Variant; Op: TToken
      ): Variant;static;
    class function divTwoArrays(const Left, Right: Variant; Op: TToken
      ): Variant;static;
    class function divArrayByNumber(const Left, Right: Variant; Op: TToken
      ): Variant;static;
    class function negArray(const Value: Variant; Op: TToken): Variant; static;
    class function eqTwoArrays(const Left, Right: Variant; Op: TToken): Variant; static;
    class function _Dot(const Left, Right: Variant; Op: TToken): Variant;static;
    // enum
    class procedure CheckSameEnumTypes(const Left, Right: Variant; Token: TToken
      ); static;
    class function areBothEnum(const Left, Right: Variant): Boolean; static;
    class function sameEnumTypes(const Left, Right: Variant): Boolean; static;
  end;

implementation
uses uClassIntf, uArrayIntf, uArray, uVariantSupport, uEnumIntf;

{ TMath }

class function TMath._Add(const Left, Right: Variant; Op: TToken): Variant;
begin
  if VarIsStr(Left) then
    Exit(Left + Right.toString);
  if VarIsBool(Left) then
    Raise ERuntimeError.Create(Op, Format(ErrIncompatibleOperands, ['+']));
  if areBothNumber(Left, Right) then
    Exit(Left + Right);
  if areBothArray(Left, Right) then
    Exit(addTwoArrays(Left, Right, Op));
  if VarSupports(Left, IArrayInstance) then
    Exit(addToArray(Left, Right, Op));
  Raise ERuntimeError.Create(Op, Format(ErrIncompatibleOperands, ['+']));
end;

class function TMath._Sub(const Left, Right: Variant; Op: TToken): Variant;
begin
  if oneOfBothBoolean(Left, Right) then
    Raise ERuntimeError.Create(Op, Format(ErrIncompatibleOperands, ['-']));
  if areBothNumber(Left, Right) then
    Exit(Left - Right);
  if areBothArray(Left, Right) then
    Exit(subTwoArrays(Left, Right, Op));
  if VarSupports(Left, IArrayInstance) then
    Exit(subNumberFromArray(Left, Right, Op));
  Raise ERuntimeError.Create(Op, Format(ErrMustBeBothNumber, ['-']));
end;

class function TMath._Mul(const Left, Right: Variant; Op: TToken): Variant;
begin
  if oneOfBothBoolean(Left, Right) then
    Raise ERuntimeError.Create(Op, Format(ErrIncompatibleOperands, ['*']));
  if areBothNumber(Left, Right) then
    Exit(Left * Right);
  if areBothArray(Left, Right) then
    Exit(mulTwoArrays(Left, Right, Op));
  if VarSupports(Left, IArrayInstance) then
    Exit(mulNumberToArray(Left, Right, Op));
  Raise ERuntimeError.Create(Op, Format(ErrMustBeBothNumber, ['*']));
end;

class function TMath._Div(const Left, Right: Variant; Op: TToken): Variant;
begin
  if oneOfBothBoolean(Left, Right) then
    Raise ERuntimeError.Create(Op, Format(ErrIncompatibleOperands, ['/']));
  if areBothNumber(Left, Right) then begin
    if Right <> 0 then
      Exit(Left / Right)
    else
      Raise ERuntimeError.Create(Op, ErrDivByZero);
  end;
  if areBothArray(Left, Right) then
    Exit(divTwoArrays(Left, Right, Op));
  if VarSupports(Left, IArrayInstance) then
    Exit(divArrayByNumber(Left, Right, Op));
  Raise ERuntimeError.Create(Op, Format(ErrMustBeBothNumber, ['/']));
end;

class function TMath._Rem(const Left, Right: Variant; Op: TToken): Variant;
begin
  if oneOfBothBoolean(Left, Right) then
    Raise ERuntimeError.Create(Op, Format(ErrIncompatibleOperands, ['%']));
  try
    Result := Left mod Right;
    Exit(Result);
  except
    Raise ERuntimeError.Create(Op, Format(ErrMustBeBothNumber, ['%']));
  end;
end;

class function TMath._Or(const Left, Right: Variant; Op: TToken): Variant;
begin
  if areBothBoolean(Left, Right) then begin
    if Left then
      Result := Left
    else
      Result := Right;
  end
  else if oneOfBothBoolean(Left, Right) then
    Raise ERuntimeError.Create(Op, ErrMustBeBothBoolean)
  else if areBothNumber(Left, Right) then
    Result := Left or Right
  else
    Raise ERuntimeError.Create(Op, ErrMustBeBothBoolean);
end;

class function TMath._And(const Left, Right: Variant; Op: TToken): Variant;
begin
  if areBothBoolean(Left, Right) then begin
    if not Left then
      Result := Left
    else
      Result := Right;
  end
  else if oneOfBothBoolean(Left, Right) then
    Raise ERuntimeError.Create(Op, ErrMustBeBothBoolean)
  else if areBothNumber(Left, Right) then
    Result := Left and Right
  else
    Raise ERuntimeError.Create(Op, ErrMustBeBothBoolean);
end;

class function TMath._XOr(const Left, Right: Variant; Op: TToken): Variant;
begin
  if areBothBoolean(Left, Right) then
    Result := Left xor Right
  else if oneOfBothBoolean(Left, Right) then
    Raise ERuntimeError.Create(Op, ErrMustBeBothBoolean)
  else if areBothNumber(Left, Right) then
    Result := Left xor Right
  else
    Raise ERuntimeError.Create(Op, ErrMustBeBothBoolean);
end;

class function TMath._Shl(const Left, Right: Variant; Op: TToken): Variant;
begin
  if oneOfBothBoolean(Left, Right) then
    Raise ERuntimeError.Create(Op, Format(ErrIncompatibleOperands, ['<<']));
  try
    Result := Left shl Right;
  except
    Raise ERuntimeError.Create(Op, Format(ErrMustBeBothNumber, ['<<']));
  end;
end;

class function TMath._Shr(const Left, Right: Variant; Op: TToken): Variant;
begin
  if oneOfBothBoolean(Left, Right) then
    Raise ERuntimeError.Create(Op, Format(ErrIncompatibleOperands, ['>>']));
  try
    Result := Left shr Right;
  except
    Raise ERuntimeError.Create(Op, Format(ErrMustBeBothNumber, ['>>']));
  end;
end;

class function TMath._Pow(const Left, Right: Variant; Op: TToken): Variant;
begin
  if oneOfBothBoolean(Left, Right) then
    Raise ERuntimeError.Create(Op, Format(ErrIncompatibleOperands, ['^']));
  try
    Result := Power(Left, Right);
  except
    Raise ERuntimeError.Create(Op, Format(ErrMustBeBothNumber, ['^']));
  end;
end;

class function TMath._Neg(const Value: Variant; Op: TToken): Variant;
begin
  if VarIsBool(Value) then
    Raise ERuntimeError.Create(Op, Format(ErrMustBeNumber, ['-']));
  if VarType(Value) = varDouble then
    Exit( -Value);
  if VarSupports(Value, IArrayInstance) then
    Exit(negArray(Value, Op));
  Raise ERuntimeError.Create(Op, Format(ErrMustBeNumber, ['-']));
end;

class function TMath._Not(const Value: Variant; Op: TToken): Variant;
begin
  if VarType(Value) = varBoolean then
    Result := not Value
  else
    Raise ERuntimeError.Create(Op, Format(ErrMustBeBoolean, ['!']));
end;

class function TMath._EQ(const Left, Right: Variant; Op: TToken): Variant;
begin
  if oneOfBothNull(Left, Right) then
    Exit(Left = Right);
  if areBothBoolean(Left, Right) then
    Exit(Left = Right);
  if areBothNumber(Left, Right) then
    Exit(Left = Right);
  if areBothString(Left, Right) then
    Exit(Left = Right);
  if areBothArray(Left, Right) then
    Exit(eqTwoArrays(Left, Right, Op));
  if areBothEnum(Left, Right) and sameEnumTypes(Left, Right) then
    Exit(IEnumInstance(Left).ElemName = IEnumInstance(Right).ElemName);
  Raise ERuntimeError.Create(Op, Format(ErrIncompatibleOperands, ['equal']));
end;

class function TMath._NEQ(const Left, Right: Variant; Op: TToken): Variant;
begin
  Result := not _EQ(Left, Right, Op);
end;

class function TMath._GT(const Left, Right: Variant; Op: TToken): Variant;
begin
  if oneOfBothNull(Left, Right) then
    Exit(False);
  if areBothNumber(Left, Right) then
    Exit(Left > Right);
  if areBothString(Left, Right) then
    Exit(Left > Right);
  Raise ERuntimeError.Create(Op, Format(ErrIncompatibleOperands, ['>']));
end;

class function TMath._GE(const Left, Right: Variant; Op: TToken): Variant;
begin
  if oneOfBothNull(Left, Right) then
    Exit(False);
  if areBothNumber(Left, Right) then
    Exit(Left >= Right);
  if areBothString(Left, Right) then
    Exit(Left >= Right);
  Raise ERuntimeError.Create(Op, Format(ErrIncompatibleOperands, ['>=']));
end;

class function TMath._LT(const Left, Right: Variant; Op: TToken): Variant;
begin
  if oneOfBothNull(Left, Right) then
    Exit(False);
  if areBothNumber(Left, Right) then
    Exit(Left < Right);
  if areBothString(Left, Right) then
    Exit(Left < Right);
  Raise ERuntimeError.Create(Op, Format(ErrIncompatibleOperands, ['<']));
end;

class function TMath._LE(const Left, Right: Variant; Op: TToken): Variant;
begin
  if oneOfBothNull(Left, Right) then
    Exit(False);
  if areBothNumber(Left, Right) then
    Exit(Left <= Right);
  if areBothString(Left, Right) then
    Exit(Left <= Right);
  Raise ERuntimeError.Create(Op, Format(ErrIncompatibleOperands, ['<=']));
end;

class function TMath._In(const Left, Right: Variant; Op: TToken): Variant;
begin
  if oneOfBothNull(Left, Right) then
    Exit(False);
  if VarSupports(Right, IArrayInstance) then
    Exit(IArrayInstance(Right).Elements.Find(Left, Op) >= 0);
  if VarSupports(Left, IEnumInstance) then
    Exit(IEnumInstance(Left).ElemSetName = Right);
  Raise ERuntimeError.Create(Op, Format(ErrMustBeBoolean, ['in']));
end;

class function TMath._Is(const Left, Right: Variant; Op: TToken): Variant;
begin
  if oneOfBothNull(Left, Right) then
    Exit(False);
  if VarSupports(Left, IGearInstance) and VarSupports(Right, IClassable) then
    Exit(IGearInstance(Left).ClassName = IClassable(Right).Name);
  Raise ERuntimeError.Create(Op, Format(ErrIncompatibleOperands, ['is']));
end;


class function TMath.areBothNumber(const Value1, Value2: Variant): Boolean;
begin
  Result := VarIsNumeric(Value1) and VarIsNumeric(Value2);
end;

class function TMath.areBothString(const Value1, Value2: Variant): Boolean;
begin
  Result := VarIsStr(Value1) and VarIsStr(Value2);
end;

class function TMath.areBothBoolean(const Value1, Value2: Variant): Boolean;
begin
  Result := VarIsBool(Value1) and VarIsBool(Value2);
end;

class function TMath.oneOfBothBoolean(const Value1, Value2: Variant): Boolean;
begin
  Result := VarIsBool(Value1) or VarIsBool(Value2);
end;

class function TMath.oneOfBothNull(const Value1, Value2: Variant): Boolean;
begin
  Result := VarIsNull(Value1) or VarIsNull(Value2);
end;


// ARRAY MATH

class function TMath.areBothArray(const Left, Right: Variant): Boolean;
begin
  Result := VarSupports(Left, IArrayInstance) and
            VarSupports(Right, IArrayInstance);
end;

class function TMath.sameArrayTypes(const Left, Right: Variant): Boolean;
begin
  Result := IArrayInstance(Left).getTypeName = IArrayInstance(Right).getTypeName;
end;

class function TMath._Concat(const Left, Right: Variant; Op: TToken): Variant;
var
  newArray: IArrayInstance;
begin
  if not areBothArray(Left, Right) then
    Raise ERuntimeError.Create(Op, ErrConcatNotAllowed);

  if not sameArrayTypes(Left, Right) then
    Raise ERuntimeError.Create(Op, ErrArrayWrongTypes);

  newArray := IArrayInstance(TArrayInstance.Create(
                IArrayInstance(Left).getArrayClass as TArrayClass));
  newArray.Elements.AddRange(IArrayInstance(Left).Elements);
  newArray.Elements.AddRange(IArrayInstance(Right).Elements);
  Result := newArray;
end;

class function TMath.addTwoArrays(const Left, Right: Variant; Op: TToken):Variant;
var
  i: Integer;
  newArray: IArrayInstance;
begin
  if not sameArrayTypes(Left, Right) then
    Raise ERuntimeError.Create(Op, ErrArrayWrongTypes);
  if IArrayInstance(Left).Count <> IArrayInstance(Right).Count then
    Raise ERuntimeError.Create(Op, ErrArrayMismatchElem);

  newArray := IArrayInstance(TArrayInstance.Create(
                IArrayInstance(Left).getArrayClass as TArrayClass));
  for i := 0 to IArrayInstance(Left).Count-1 do
    newArray.Elements.Add(
      _Add(IArrayInstance(Left)[i], IArrayInstance(Right)[i], Op));

  Result := newArray;
end;

class function TMath.addToArray(const Left, Right: Variant; Op: TToken): Variant;
var
  i: Integer;
  newArray: IArrayInstance;
begin
  newArray := IArrayInstance(TArrayInstance.Create(
                IArrayInstance(Left).getArrayClass as TArrayClass));
  for i := 0 to IArrayInstance(Left).Count-1 do
    newArray.Elements.Add(_Add(IArrayInstance(Left)[i], Right, Op));

  Result := newArray;
end;

class function TMath.subTwoArrays(
  const Left, Right: Variant; Op: TToken): Variant;
var
  i: Integer;
  newArray: IArrayInstance;
begin
  if not sameArrayTypes(Left, Right) then
    Raise ERuntimeError.Create(Op, ErrArrayWrongTypes);
  if IArrayInstance(Left).Count <> IArrayInstance(Right).Count then
    Raise ERuntimeError.Create(Op, ErrArrayMismatchElem);

  newArray := IArrayInstance(TArrayInstance.Create(
                IArrayInstance(Left).getArrayClass as TArrayClass));
  for i := 0 to IArrayInstance(Left).Count-1 do
    newArray.Elements.Add(
      _Sub(IArrayInstance(Left)[i], IArrayInstance(Right)[i], Op));

  Result := newArray;
end;

class function TMath.subNumberFromArray(
  const Left, Right: Variant; Op: TToken): Variant;
var
  i: Integer;
  newArray: IArrayInstance;
begin
  newArray := IArrayInstance(TArrayInstance.Create(
                IArrayInstance(Left).getArrayClass as TArrayClass));
  for i := 0 to IArrayInstance(Left).Count-1 do
    newArray.Elements.Add(_Sub(IArrayInstance(Left)[i], Right, Op));

  Result := newArray;
end;


class function TMath.mulNumberToArray(
  const Left, Right: Variant; Op: TToken): Variant;
var
  i: Integer;
  newArray: IArrayInstance;
begin
  newArray := IArrayInstance(TArrayInstance.Create(
                IArrayInstance(Left).getArrayClass as TArrayClass));
  for i := 0 to IArrayInstance(Left).Count-1 do
    newArray.Elements.Add(_Mul(IArrayInstance(Left)[i], Right, Op));

  Result := newArray;
end;

class function TMath.mulTwoArrays(
  const Left, Right: Variant; Op: TToken): Variant;
var
  i: Integer;
  newArray: IArrayInstance;
begin
  if not sameArrayTypes(Left, Right) then
    Raise ERuntimeError.Create(Op, ErrArrayWrongTypes);
  if IArrayInstance(Left).Count <> IArrayInstance(Right).Count then
    Raise ERuntimeError.Create(Op, ErrArrayMismatchElem);

  newArray := IArrayInstance(TArrayInstance.Create(
                IArrayInstance(Left).getArrayClass as TArrayClass));
  for i := 0 to IArrayInstance(Left).Count-1 do
    newArray.Elements.Add(
      _Mul(IArrayInstance(Left)[i], IArrayInstance(Right)[i], Op));

  Result := newArray;
end;

class function TMath.divArrayByNumber(
  const Left, Right: Variant; Op: TToken): Variant;
var
  i: Integer;
  newArray: IArrayInstance;
begin
  newArray := IArrayInstance(TArrayInstance.Create(
                IArrayInstance(Left).getArrayClass as TArrayClass));
  for i := 0 to IArrayInstance(Left).Count-1 do
    newArray.Elements.Add(_Div(IArrayInstance(Left)[i], Right, Op));

  Result := newArray;
end;

class function TMath.divTwoArrays(
  const Left, Right: Variant; Op: TToken): Variant;
var
  i: Integer;
  newArray: IArrayInstance;
begin
  if not sameArrayTypes(Left, Right) then
    Raise ERuntimeError.Create(Op, ErrArrayWrongTypes);
  if IArrayInstance(Left).Count <> IArrayInstance(Right).Count then
    Raise ERuntimeError.Create(Op, ErrArrayMismatchElem);

  newArray := IArrayInstance(TArrayInstance.Create(
                IArrayInstance(Left).getArrayClass as TArrayClass));
  for i := 0 to IArrayInstance(Left).Count-1 do
    newArray.Elements.Add(
      _Div(IArrayInstance(Left)[i], IArrayInstance(Right)[i], Op));

  Result := newArray;
end;

class function TMath.negArray(const Value: Variant; Op: TToken): Variant;
var
  i: Integer;
  newArray: IArrayInstance;
begin
  newArray := IArrayInstance(TArrayInstance.Create(
                IArrayInstance(Value).getArrayClass as TArrayClass));
  for i := 0 to IArrayInstance(Value).Count-1 do
    newArray.Elements.Add(_Neg(IArrayInstance(Value)[i], Op));

  Result := newArray;
end;

class function TMath.eqTwoArrays(const Left, Right: Variant; Op: TToken
  ): Variant;
var
  i: Integer;
begin
  if not sameArrayTypes(Left, Right) then
    Exit(False);
  if IArrayInstance(Left).Count <> IArrayInstance(Right).Count then
    Exit(False);

  Result := True;
  for i := 0 to IArrayInstance(Left).Count-1 do
    if _NEQ(IArrayInstance(Left)[i], IArrayInstance(Right)[i], Op) then
      Exit(False);
end;

class function TMath._Dot(const Left, Right: Variant; Op: TToken): Variant;
var
  temp: IArrayInstance;
  i: Integer;
begin
  if areBothArray(Left, Right) then begin
    temp := _Mul(IArrayInstance(Left), IArrayInstance(Right), Op);
    Result := 0;
    for i := 0 to temp.Count-1 do
      Result += temp[i];
  end
  else
    Raise ERuntimeError.Create(Op, Format(ErrArrayWrongTypes, ['::']));
end;

{ ENUM }

class function TMath.areBothEnum(const Left, Right: Variant): Boolean;
begin
  Result := VarSupports(Left, IEnumInstance) and VarSupports(Right, IEnumInstance);
end;

class function TMath.sameEnumTypes(const Left, Right: Variant): Boolean;
begin
  Result := IEnumInstance(Left).EnumName = IEnumInstance(Right).EnumName;
end;

class procedure TMath.CheckSameEnumTypes(const Left, Right: Variant; Token: TToken);
begin
  if not VarSupports(Right, IEnumInstance) then
    Raise ERuntimeError.Create(Token, Format(ErrIncompatibleTypes,
          [IEnumInstance(Left).EnumName, VarTypeAsText(VarType(Right))]));

  if not sameEnumTypes(Left, Right) then
    Raise ERuntimeError.Create(Token, Format(ErrIncompatibleTypes,
            [IEnumInstance(Left).EnumName, IEnumInstance(Right).EnumName]));
end;

end.

