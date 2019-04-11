unit uInterpreter;

{ This unit contains the interpreter of the Gear language.

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
  Classes, SysUtils, uVisitor, uAST, uToken, uError, Variants,
  uMemory, uMembers;

type

  TInterpreter = class(TVisitor)
    private
      CurrentSpace: TMemorySpace;
      FGlobals: TMemorySpace;
    public
      property Globals: TMemorySpace read FGlobals;
      property Memory: TMemorySpace read CurrentSpace;
      constructor Create;
      destructor Destroy; override;
      procedure Execute(Tree: TProduct);
      procedure Execute(Block: TBlock; MemorySpace: TMemorySpace);
    published
      //expressions
      function VisitBinaryExpr(BinaryExpr: TBinaryExpr): Variant;
      function VisitConstExpr(ConstExpr: TConstExpr): Variant;
      function VisitUnaryExpr(UnaryExpr: TUnaryExpr): Variant;
      procedure VisitIdent(Ident: TIdent);
      function VisitVariable(Variable: TVariable): Variant;
      function VisitCallExpr(CallExpr: TCallExpr): Variant;
      function VisitIfExpr(IfExpr: TIfExpr): Variant;
      function VisitMatchExpr(MatchExpr: TMatchExpr): Variant;
      function VisitFuncDeclExpr(FuncDeclExpr: TFuncDeclExpr): Variant;
      function VisitGetExpr(GetExpr: TGetExpr): Variant;
      function VisitSelfExpr(SelfExpr: TSelfExpr): Variant;
      function VisitInheritedExpr(InheritedExpr: TInheritedExpr): Variant;
      function VisitArrayDeclExpr(ArrayDeclExpr: TArrayDeclExpr): Variant;
      function VisitDictDeclExpr(DictDeclExpr: TDictDeclExpr): Variant;
      function VisitIndexedExpr(IndexedExpr: TIndexedExpr): Variant;
      function VisitInterpolatedExpr(InterpolatedExpr: TInterpolatedExpr): Variant;
      //statements
      procedure VisitPrintStmt(PrintStmt: TPrintStmt);
      procedure VisitAssignStmt(AssignStmt: TAssignStmt);
      procedure VisitCallExprStmt(CallExprStmt: TCallExprStmt);
      procedure VisitSetStmt(SetStmt: TSetStmt);
      procedure VisitIndexedExprStmt(IndexedExprStmt: TIndexedExprStmt);
      procedure VisitIfStmt(IfStmt: TIfStmt);
      procedure VisitWhileStmt(WhileStmt: TWhileStmt);
      procedure VisitRepeatStmt(RepeatStmt: TRepeatStmt);
      procedure VisitEnsureStmt(EnsureStmt: TEnsureStmt);
      procedure VisitSwitchStmt(SwitchStmt: TSwitchStmt);
      procedure VisitBreakStmt(BreakStmt: TBreakStmt);
      procedure VisitContinueStmt(ContinueStmt: TContinueStmt);
      procedure VisitReturnStmt(ReturnStmt: TReturnStmt);
      procedure VisitUseStmt(UseStmt: TUseStmt);
      //declarations
      procedure VisitVarDecl(VarDecl: TVarDecl);
      procedure VisitFuncDecl(FuncDecl: TFuncDecl);
      procedure VisitValDecl(ValDecl: TValDecl);
      function getMembers(DeclList: TDeclList): TMembers;
      procedure VisitClassDecl(ClassDecl: TClassDecl);
      procedure VisitExtensionDecl(ExtensionDecl: TExtensionDecl);
      function ApplyTraits(Traits: TExprList): TMembers;
      function CombineMembers(Traits: TMembers; DeclList: TDeclList): TMembers;
      procedure VisitTraitDecl(TraitDecl: TTraitDecl);
      procedure VisitArrayDecl(ArrayDecl: TArrayDecl);
      procedure VisitDictDecl(DictDecl: TDictDecl);
      procedure VisitEnumDecl(EnumDecl: TEnumDecl);
      //block
      procedure VisitBlock(Block: TBlock);
      procedure VisitProduct(Product: TProduct);

    private
      procedure CheckDuplicate(AIdent: TIdent; const TypeName: String);
      procedure Assign(Variable: TVariable; Value: Variant);
      function Lookup(Variable: TVariable): Variant;
      procedure CheckNumericIndex(Value: Variant; Token: TToken);
  end;

implementation
uses uCallable, uFunc, uStandard, uClassIntf, uClass, uArrayIntf, uArray,
  uDictIntf, uDict, uEnumIntf, uEnum, uMath, uVariantSupport;

const
  ErrDuplicateID = '%s cannot be defined. Identifier "%s" is already declared.';
  ErrIncompatibleTypes = 'Incompatible types in assignment: %s vs. %s.';
  ErrConditionMustBeBool = 'Condition must return a Boolean value.';
  ErrNotAFunction = '"%s" is not defined as function.';
  ErrExpectedInstance = 'Expected declared type instance.';
  ErrUndefVarOrSelfMissing = 'Variable is undefined or missing "self".';
  ErrClassMemberImmutable = 'Class member "%s" is immutable.';
  ErrUndefinedMethod = 'Undefined method "%s".';
  ErrAssignToValue = 'Assignment to Value property not allowed.';
  ErrExtIdNoType = 'Extension identifier "%s" is not a type.';
  ErrNotDeclaredAsTrait = '"%s" is not declared as trait.';
  ErrTraitIsDeclared = 'Trait "%s" "%s" is already declared.';
  ErrTraitDeclared = 'Trait already declared for member "%s".';
  ErrIndexMustBeNumeric = 'Index must be a numeric value.';
  ErrArrayTypeExpected = 'Array or string type expected.';

constructor TInterpreter.Create;
begin
  FGlobals := TMemorySpace.Create();
  FGlobals.Sorted := True;
  FGlobals.Store('assigned', ICallable(TAssigned.Create));
  FGlobals.Store('abs', ICallable(TAbs.Create));
  FGlobals.Store('arctan', ICallable(TArctan.Create));
  FGlobals.Store('ceil', ICallable(TCeil.Create));
  FGlobals.Store('chr', ICallable(TChr.Create));
  FGlobals.Store('cos', ICallable(TCos.Create));
  FGlobals.Store('date', ICallable(TDate.Create));
  FGlobals.Store('exp', ICallable(TExp.Create));
  FGlobals.Store('floor', ICallable(TFloor.Create));
  FGlobals.Store('frac', ICallable(TFrac.Create));
  FGlobals.Store('length', ICallable(TLength.Create));
  FGlobals.Store('ln', ICallable(TLn.Create));
  FGlobals.Store('milliseconds', ICallable(TMilliSeconds.Create));
  FGlobals.Store('now', ICallable(TNow.Create));
  FGlobals.Store('today', ICallable(TToday.Create));
  FGlobals.Store('ord', ICallable(TOrd.Create));
  FGlobals.Store('pi', ICallable(TPi.Create));
  FGlobals.Store('random', ICallable(TRandom.Create));
  FGlobals.Store('randomLimit', ICallable(TRandomLimit.Create));
  FGlobals.Store('readln', ICallable(TReadLn.Create));
  FGlobals.Store('round', ICallable(TRound.Create));
  FGlobals.Store('sin', ICallable(TSin.Create));
  FGlobals.Store('sqr', ICallable(TSqr.Create));
  FGlobals.Store('sqrt', ICallable(TSqrt.Create));
  FGlobals.Store('time', ICallable(TTime.Create));
  FGlobals.Store('toNum', ICallable(TToNum.Create));
  FGlobals.Store('toStr', ICallable(TToStr.Create));
  FGlobals.Store('trunc', ICallable(TTrunc.Create));
  // list functions
  Globals.Store('listAdd', ICallable(TListAdd.Create));
  Globals.Store('listContains', ICallable(TListContains.Create));
  Globals.Store('listDelete', ICallable(TListDelete.Create));
  Globals.Store('listIndexOf', ICallable(TListIndexOf.Create));
  Globals.Store('listInsert', ICallable(TListInsert.Create));
  Globals.Store('listRetrieve', ICallable(TListRetrieve.Create));
  Globals.Store('listFirst', ICallable(TListFirst.Create));
  Globals.Store('listLast', ICallable(TListLast.Create));
  Globals.Store('listKeys', ICallable(TListKeys.Create));
  Globals.Store('listValues', ICallable(TListValues.Create));

  // store base Array type
  Globals.Store('Array', IArrayable(
    TArrayClass.Create(
      TIdent.Create(TToken.Create(ttIdentifier, 'Array', Null, 0, 0)),
      TArrayElements.Create(),
      TMembers.Create(TFieldTable.Create, TConstTable.Create,
        TMethodTable.Create, TValueTable.Create
      )
    ))
  );

  // store base Dictionary type
  Globals.Store('Dictionary', IDictionable(
    TDictClass.Create(
      TIdent.Create(TToken.Create(ttIdentifier, 'Dictionary', Null, 0, 0)),
      TDictElements.Create(),
      TMembers.Create(TFieldTable.Create, TConstTable.Create,
        TMethodTable.Create, TValueTable.Create
      )
    ))
  );

  CurrentSpace:= FGlobals;
end;

destructor TInterpreter.Destroy;
begin
  CurrentSpace.Free;
  inherited Destroy;
end;

procedure TInterpreter.Execute(Tree: TProduct);
begin
  try
    Visit(Tree);
  except
    on E: ERuntimeError do
      RuntimeError(E);
  end;
end;

procedure TInterpreter.Execute(Block: TBlock; MemorySpace: TMemorySpace);
var
  SavedSpace: TMemorySpace;
begin
  SavedSpace := CurrentSpace;
  try
    CurrentSpace := MemorySpace;
    Visit(Block);
  finally
    CurrentSpace := SavedSpace;
  end;
end;

function TInterpreter.VisitBinaryExpr(BinaryExpr: TBinaryExpr): Variant;
var
  Left, Right: Variant;
  Op: TToken;
begin
  Left := Visit(BinaryExpr.Left);
  Right := Visit(BinaryExpr.Right);
  Op := BinaryExpr.Op;
  case BinaryExpr.Op.Typ of
    ttPlus: Result := TMath._Add(Left, Right, Op);
    ttMin:  Result := TMath._Sub(Left, Right, Op);
    ttMul:  Result := TMath._Mul(Left, Right, Op);
    ttDiv:  Result := TMath._Div(Left, Right, Op);
    ttRem:  Result := TMath._Rem(Left, Right, Op);
    ttOr:   Result := TMath._Or(Left, Right, Op);
    ttAnd:  Result := TMath._And(Left, Right, Op);
    ttXor:  Result := TMath._Xor(Left, Right, Op);
    ttShl:  Result := TMath._Shl(Left, Right, Op);
    ttShr:  Result := TMath._Shr(Left, Right, Op);
    ttPow:  Result := TMath._Pow(Left, Right, Op);
    ttConcat: Result := TMath._Concat(Left, Right, Op);
    ttEQ:   Result := TMath._EQ(Left, Right, Op);
    ttNEQ:  Result := TMath._NEQ(Left, Right, Op);
    ttGT:   Result := TMath._GT(Left, Right, Op);
    ttGE:   Result := TMath._GE(Left, Right, Op);
    ttLT:   Result := TMath._LT(Left, Right, Op);
    ttLE:   Result := TMath._LE(Left, Right, Op);
    ttIn:   Result := TMath._In(Left, Right, Op);
    ttIs:   Result := TMath._Is(Left, Right, Op);
    ttColons: Result := TMath._Dot(Left, Right, Op);
  end;
end;

function TInterpreter.VisitConstExpr(ConstExpr: TConstExpr): Variant;
begin
  Result := ConstExpr.Value;
end;

function TInterpreter.VisitUnaryExpr(UnaryExpr: TUnaryExpr): Variant;
var
  Expr: Variant;
begin
  Expr := Visit(UnaryExpr.Expr);
  case UnaryExpr.Op.Typ of
    ttNot: Result := TMath._Not(Expr, UnaryExpr.Op);
    ttMin: Result := TMath._Neg(Expr, UnaryExpr.Op);
    ttQuestion: Result := Expr <> Unassigned;
    else Result := Expr;
  end;
end;

procedure TInterpreter.VisitIdent(Ident: TIdent);
begin
  // do nothing
end;

function TInterpreter.VisitVariable(Variable: TVariable): Variant;
begin
  Result := Lookup(Variable);
  if VarSupports(Result, IValuable) then
    Result := IValuable(Result).Call(Variable.Token, Self, TArgList.Create());
  if VarIsEmpty(Result) then
    Raise ERuntimeError.Create(Variable.Token, ErrUndefVarOrSelfMissing);
end;

function TInterpreter.VisitCallExpr(CallExpr: TCallExpr): Variant;
var
  Callee: Variant;
  Args: TArgList;
  i: Integer;
  Func: ICallable;
  Msg: String;
  CallArg: TCallArg;
  Token: TToken;
begin
  Callee := Visit(CallExpr.Callee);
  if VarSupportsIntf(Callee,
       [ICallable, IClassable, IArrayable, IDictionable]) then
    Func := ICallable(Callee)
  else begin
    Msg := Format(ErrNotAFunction, [CallExpr.Callee.Token.Lexeme]);
    Raise ERuntimeError.Create(CallExpr.Token, Msg);
  end;
  Args := TArgList.Create();
  for i := 0 to CallExpr.Args.Count-1 do begin
    if Assigned(CallExpr.Args[i].Ident) then
      Token := CallExpr.Args[i].Ident.Token
    else
      Token := CallExpr.Args[i].Expr.Token;
    CallArg := TCallArg.Create(Visit(CallExpr.Args[i].Expr),
      CallExpr.Args[i].Ident, Token);
    Args.Add(CallArg);
  end;
  Result := Func.Call(CallExpr.Token, Self, Args);
end;

function TInterpreter.VisitIfExpr(IfExpr: TIfExpr): Variant;
var
  Condition: Variant;
begin
  Condition := Visit(IfExpr.Condition);
  if VarIsBool(Condition) then begin
    if Condition then
      Result := Visit(IfExpr.TrueExpr)
    else
      Result := Visit(IfExpr.FalseExpr)
  end
  else
    Raise ERuntimeError.Create(IfExpr.Token, ErrConditionMustBeBool);
end;

function TInterpreter.VisitMatchExpr(MatchExpr: TMatchExpr): Variant;
var
  MatchValue, IfValue: Variant;
  i, j: integer;
begin
  MatchValue := Visit(MatchExpr.Expr);
  for i := 0 to MatchExpr.IfLimbs.Count-1 do begin
    for j := 0 to MatchExpr.IfLimbs[i].Values.Count-1 do begin
      IfValue := Visit(MatchExpr.IfLimbs[i].Values[j]);
      if TMath._EQ(MatchValue, IfValue, MatchExpr.IfLimbs[i].Values[j].Token) then
        Exit(Visit(MatchExpr.IfLimbs[i].Expr));
    end;
  end;
  Result := Visit(MatchExpr.ElseLimb);
end;

function TInterpreter.VisitFuncDeclExpr(FuncDeclExpr: TFuncDeclExpr): Variant;
begin
  Result := ICallable(TFunc.Create(FuncDeclExpr.FuncDecl, CurrentSpace));
end;

function TInterpreter.VisitGetExpr(GetExpr: TGetExpr): Variant;
var
  Instance: Variant;
begin
  Instance := Visit(GetExpr.Instance);
  if VarSupportsIntf(Instance,
       [IGearInstance, IArrayInstance, IDictInstance, IEnumInstance]) then
  begin
    if VarSupports(Instance, IGearInstance) then
      Result := IGearInstance(Instance).GetMember(GetExpr.Ident)
    else if VarSupports(Instance, IArrayInstance) then
      Result := IArrayInstance(Instance).GetMember(GetExpr.Ident)
    else if VarSupports(Instance, IDictInstance) then
      Result := IDictInstance(Instance).GetMember(GetExpr.Ident)
    else if VarSupports(Instance, IEnumInstance) then
      Result := IEnumInstance(Instance).GetMember(GetExpr.Ident);

    if VarSupports(Result, IValuable) then
      Result := ICallable((IValuable(Result) as TVal)
                          .Bind(Instance))
                          .Call(GetExpr.Token, Self, TArgList.Create());
  end
  else if VarSupports(Instance, IEnumable) then begin
    if IEnumable(Instance).isCase(GetExpr.Ident.Text) then
      Result := GetExpr.Ident.Text
    else if GetExpr.Ident.Text = 'Elements' then
      Result := IEnumable(Instance).ElementList
    else
      Result := IEnumInstance(TEnumInstance.Create(
        IEnumable(Instance) as TEnumClass, GetExpr.Ident));
  end
  else
    Raise ERuntimeError.Create(GetExpr.Ident.Token, ErrExpectedInstance);
end;

function TInterpreter.VisitSelfExpr(SelfExpr: TSelfExpr): Variant;
begin
  Result := Lookup(SelfExpr.Variable);
end;

function TInterpreter.VisitInheritedExpr(InheritedExpr: TInheritedExpr): Variant;
var
  Distance: Integer;
  Parent, Method: ICallable;
  Instance: IGearInstance;
begin
  Distance := InheritedExpr.Variable.Distance;
  Parent := ICallable(CurrentSpace.LoadAt(Distance, 'inherited'));
  Instance := IGearInstance(CurrentSpace.LoadAt(Distance-1, 'self'));
  Method := (Parent as TGearClass).FindMethod(Instance, InheritedExpr.Method.Text);
  if Method = Nil then
    Raise ERuntimeError.Create(InheritedExpr.Method.Token, Format(
      ErrUndefinedMethod, [InheritedExpr.Method.Text]));
  Result := Method;
end;

function TInterpreter.VisitArrayDeclExpr(ArrayDeclExpr: TArrayDeclExpr): Variant;
var
  ArrayType: IArrayable;
  Instance: IArrayInstance;
  i: Integer;
begin
  ArrayType := IArrayable(Globals['Array']);

  Instance := IArrayInstance(TArrayInstance.Create(ArrayType as TArrayClass));
  for i := 0 to ArrayDeclExpr.ArrayDecl.Elements.Count-1 do
    Instance.Elements.Add(Visit(ArrayDeclExpr.ArrayDecl.Elements[i]));

  Result := Instance;
end;

function TInterpreter.VisitDictDeclExpr(DictDeclExpr: TDictDeclExpr): Variant;
var
  DictType: IDictionable;
  Instance: IDictInstance;
  i: Integer;
begin
  DictType := IDictionable(Globals['Dictionary']);

  Instance := IDictInstance(TDictInstance.Create(DictType as TDictClass));
  for i := 0 to DictDeclExpr.DictDecl.Elements.Count-1 do
    Instance.Elements.Add(
      Visit(DictDeclExpr.DictDecl.Elements[i].Key),
      Visit(DictDeclExpr.DictDecl.Elements[i].Value));

  Result := Instance;
end;

function TInterpreter.VisitIndexedExpr(IndexedExpr: TIndexedExpr): Variant;
var
  Instance: Variant;
  Index: Variant;
begin
  Instance := Visit(IndexedExpr.Variable);
  Index := Visit(IndexedExpr.Index);
  if VarSupports(Instance, IArrayInstance) then begin
    CheckNumericIndex(Index, IndexedExpr.Index.Token);
    Result := IArrayInstance(Instance).get(Index, IndexedExpr.Index.Token);
  end
  else if VarSupports(Instance, IDictInstance) then
    Result := IDictInstance(Instance).get(Index, IndexedExpr.Index.Token)
  else if VarIsStr(Instance) then begin
    CheckNumericIndex(Index, IndexedExpr.Index.Token);
    Result := String(Instance)[Index+1];  // strings start at index 1
  end
  else
    Raise ERuntimeError.Create(IndexedExpr.Variable.Token, ErrArrayTypeExpected);
end;

function TInterpreter.VisitInterpolatedExpr(InterpolatedExpr: TInterpolatedExpr
  ): Variant;
var
  Expr: TExpr;
begin
  Result := '';
  for Expr in InterpolatedExpr.ExprList do
    Result := TMath._Add(Result, Visit(Expr), Expr.Token);
end;

procedure TInterpreter.VisitPrintStmt(PrintStmt: TPrintStmt);
var
  i: Integer;
begin
  for i := 0 to PrintStmt.ExprList.Count-1 do
    Write((Visit(PrintStmt.ExprList[i])).toString);
  Write((Visit(PrintStmt.Terminator)).toString);
end;

function TypeOf(Value: Variant): String;
begin
  case VarType(Value) of
    varNull: Result := 'Null';
    varSingle, varDouble: Result := 'Number';
    varString: Result := 'String';
    varBoolean: Result := 'Boolean';
    varShortInt, varSmallInt, varInteger, varInt64,
    varByte, varWord, varLongWord, varQWord: Result := 'Number';
    else
      begin
          if VarSupports(Value, IArrayInstance) then Exit('Array');
          if VarSupports(Value, IDictInstance) then Exit('Dictionary');
          if VarSupports(Value, IEnumInstance) then Exit('Enum');
          if VarSupports(Value, IGearInstance) then Exit('Class');
          if VarSupports(Value, ICallable) then Exit('Func');
      end;
  end;
end;

function getAssignValue(OldValue, NewValue: Variant; ID, Op: TToken): Variant;
var
  OldType, NewType: String;
begin
  OldType := TypeOf(OldValue);
  NewType := TypeOf(NewValue);
  if VarIsNull(OldValue) and (Op.Typ = ttAssign) then
    Exit(NewValue);
  if VarIsNull(NewValue) and (Op.Typ = ttAssign) then
    Exit(Null);

  if OldType <> NewType then
    Raise ERuntimeError.Create(ID,
      Format(ErrIncompatibleTypes, [OldType, NewType]));

  if VarSupports(OldValue, IEnumInstance) then begin
    if not TMath.sameEnumTypes(OldValue, NewValue) then
      Raise ERuntimeError.Create(ID,
        Format('Incompatible enum types in assignment: %s vs. %s.',
          [IEnumInstance(OldValue).EnumName, IEnumInstance(NewValue).EnumName]));
     if Op.Typ = ttAssign then
       Exit(NewValue)
     else
       Raise ERuntimeError.Create(Op, 'Illegal assignment operator, ":=" expected.');
  end;

  if not VarIsNull(OldValue) then begin
    if Op.Typ <> ttAssign then
    case Op.Typ of
      ttPlusIs: NewValue := TMath._Add(OldValue, NewValue, Op);
      ttMinIs:  NewValue := TMath._Sub(OldValue, NewValue, Op);
      ttMulIs:  NewValue := TMath._Mul(OldValue, NewValue, Op);
      ttDivIs:  NewValue := TMath._Div(OldValue, NewValue, Op);
      ttRemIs:  NewValue := TMath._Rem(OldValue, NewValue, Op);
      ttConcatIs: NewValue := TMath._Concat(OldValue, NewValue, Op);
    end;
    Exit(NewValue);
  end;

  Raise ERuntimeError.Create(ID,
    Format(ErrIncompatibleTypes, [OldType, NewType]));
end;

procedure TInterpreter.VisitAssignStmt(AssignStmt: TAssignStmt);
var
  OldValue, NewValue, Value: Variant;
begin
  with AssignStmt do begin
    OldValue := Lookup(Variable);
    NewValue := Visit(Expr);
    Value := getAssignValue(OldValue, NewValue, Variable.Token, Op);
    Assign(Variable, Value);
  end;
end;

procedure TInterpreter.VisitCallExprStmt(CallExprStmt: TCallExprStmt);
begin
  Visit(CallExprStmt.CallExpr);
end;

procedure TInterpreter.VisitSetStmt(SetStmt: TSetStmt);
var
  Instance, OldValue, NewValue, Value: Variant;
begin
  Instance := Visit(SetStmt.Instance);
  if not VarSupports(Instance, IGearInstance) then
    Raise ERuntimeError.Create(SetStmt.Token, ErrExpectedInstance);

  OldValue := IGearInstance(Instance).GetMember(SetStmt.Ident);
  if VarSupports(OldValue, IValuable) then
    Raise ERuntimeError.Create(SetStmt.Token, ErrAssignToValue);

  if IGearInstance(Instance).isConstant(SetStmt.Ident) and
     (not VarIsNull(OldValue)) then
    Raise ERuntimeError.Create(SetStmt.Ident.Token, Format(
      ErrClassMemberImmutable, [SetStmt.Ident.Text]));

  NewValue := Visit(SetStmt.Expr);
  Value := getAssignValue(OldValue, NewValue, SetStmt.Ident.Token, SetStmt.Op);
  IGearInstance(Instance).SetField(SetStmt.Ident, Value);
end;

procedure TInterpreter.VisitIndexedExprStmt(IndexedExprStmt: TIndexedExprStmt);
var
  Variable, OldValue, NewValue, Value: Variant;
  Index: Variant;
  VarAsStr: String;
begin
  Variable := Visit(IndexedExprStmt.IndexedExpr.Variable);
  Index := Visit(IndexedExprStmt.IndexedExpr.Index);
  NewValue := Visit(IndexedExprStmt.Expr);

  with IndexedExprStmt do
  if VarSupports(Variable, IArrayInstance) then begin
    CheckNumericIndex(Index, IndexedExpr.Index.Token);
    OldValue := IArrayInstance(Variable).get(Index, IndexedExpr.Index.Token);
    Value := getAssignValue(OldValue, NewValue, IndexedExpr.Variable.Token, Op);
    IArrayInstance(Variable).Put(Index, Value, IndexedExpr.Index.Token);
  end
  else if VarSupports(Variable, IDictInstance) then begin
    if IDictInstance(Variable).Contains(Index) then
      OldValue := IDictInstance(Variable).get(Index, IndexedExpr.Index.Token)
    else
      OldValue := Null;
    Value := getAssignValue(OldValue, NewValue, IndexedExpr.Variable.Token, Op);
    IDictInstance(Variable).Put(Index, Value, IndexedExpr.Index.Token);
  end
  else if VarIsStr(Variable) then begin
    CheckNumericIndex(Index, IndexedExpr.Index.Token);
    VarAsStr := Variable;
    OldValue := VarAsStr[Index+1];
    VarAsStr[Index+1] := getAssignValue(OldValue, NewValue,
      IndexedExpr.Variable.Token, Op);
    Assign(IndexedExpr.Variable as TVariable, VarAsStr);
  end
  else
    Raise ERuntimeError.Create(IndexedExpr.Variable.Token, ErrArrayTypeExpected);
end;

procedure TInterpreter.VisitIfStmt(IfStmt: TIfStmt);
var
  SavedSpace: TMemorySpace;
  i: Integer;
  ElseIfExecuted: Boolean = False;

  function isBooleanAndTrue(Condition: TExpr): Boolean;
  var Value: Variant;
  begin
    Value := Visit(Condition);
    if VarIsBool(Value) then
      Result := Boolean(Value)
    else
      Raise ERuntimeError.Create(IfStmt.Token, ErrConditionMustBeBool);
  end;

begin
  try
    if Assigned(IfStmt.VarDecl) then begin
      SavedSpace := CurrentSpace;
      CurrentSpace := TMemorySpace.Create(SavedSpace);
      Visit(IfStmt.VarDecl);
    end;

    if isBooleanAndTrue(IfStmt.Condition) then
      Visit(IfStmt.ThenPart)
    else if Assigned(IfStmt.ElseIfs) then begin
      for i := 0 to IfStmt.ElseIfs.Count-1 do begin
        if isBooleanAndTrue(IfStmt.ElseIfs[i]) then begin
          Visit(IfStmt.ElseIfParts[i]);
          ElseIfExecuted := True;
          Break;
        end;
      end;
      if not ElseIfExecuted then
        if Assigned(IfStmt.ElsePart) then
          Visit(IfStmt.ElsePart);
    end
    else if Assigned(IfStmt.ElsePart) then
      Visit(IfStmt.ElsePart);

  finally
    if Assigned(IfStmt.VarDecl) then begin
      CurrentSpace.Free;
      CurrentSpace := SavedSpace;
    end;
  end;
end;

procedure TInterpreter.VisitWhileStmt(WhileStmt: TWhileStmt);
var
  Condition: Variant;
  SavedSpace: TMemorySpace;
begin
  try
    if Assigned(WhileStmt.VarDecl) then begin
      SavedSpace := CurrentSpace;
      CurrentSpace := TMemorySpace.Create(SavedSpace);
      Visit(WhileStmt.VarDecl);
    end;
    try
      Condition := Visit(WhileStmt.Condition);
      if VarIsBool(Condition) then begin
        while Condition do begin
          try
            Visit(WhileStmt.Block);
          except
            on EContinueException do;
          end;
          Condition := Visit(WhileStmt.Condition);
        end;
      end
      else
        Raise ERuntimeError.Create(WhileStmt.Token, ErrConditionMustBeBool);
    except
      on EBreakException do;
    end;
  finally
    if Assigned(WhileStmt.VarDecl) then begin
      CurrentSpace.Free;
      CurrentSpace := SavedSpace;
    end;
  end;
end;

procedure TInterpreter.VisitRepeatStmt(RepeatStmt: TRepeatStmt);
var
  Condition: Variant;
begin
  try
    Condition := Visit(RepeatStmt.Condition);
    if VarIsBool(Condition) then begin
      repeat
        try
          Visit(RepeatStmt.Block);
        except
          on EContinueException do;
        end;
        Condition := Visit(RepeatStmt.Condition);
      until Condition;
    end
    else
      Raise ERuntimeError.Create(RepeatStmt.Token, ErrConditionMustBeBool);
  except
    on EBreakException do;
  end;
end;

procedure TInterpreter.VisitEnsureStmt(EnsureStmt: TEnsureStmt);
var
  Condition: Variant;
begin
  if Assigned(EnsureStmt.VarDecl) then
    Visit(EnsureStmt.VarDecl);
  Condition := Visit(EnsureStmt.Condition);
  if VarIsBool(Condition) then begin
    if not Condition then
      Visit(EnsureStmt.ElsePart)
  end
  else
    Raise ERuntimeError.Create(EnsureStmt.Token, ErrConditionMustBeBool);
end;

procedure TInterpreter.VisitSwitchStmt(SwitchStmt: TSwitchStmt);
var
  SwitchValue, CaseValue: Variant;
  IsObjValue: Boolean;
  i, j: integer;
begin
  SwitchValue := Visit(SwitchStmt.Expr);
  for i := 0 to SwitchStmt.CaseLimbs.Count-1 do begin
    for j := 0 to SwitchStmt.CaseLimbs[i].Values.Count-1 do begin
      CaseValue := Visit(SwitchStmt.CaseLimbs[i].Values[j]);
      IsObjValue := SwitchStmt.CaseLimbs[i].IsObj;
      if IsObjValue then begin
        if TMath._Is(SwitchValue, CaseValue,
          SwitchStmt.CaseLimbs[i].Values[j].Token) then
        begin
          Visit(SwitchStmt.CaseLimbs[i].Block);
          Exit;
        end;
      end
      else if TMath._EQ(SwitchValue, CaseValue,
        SwitchStmt.CaseLimbs[i].Values[j].Token) then
      begin
        Visit(SwitchStmt.CaseLimbs[i].Block);
        Exit;
      end;
    end;
  end;
  // if no match found => execute Else block
  Visit(SwitchStmt.ElseLimb);
end;

procedure TInterpreter.VisitBreakStmt(BreakStmt: TBreakStmt);
var
  Condition: Variant;
begin
  Condition := True;
  if Assigned(BreakStmt.Condition) then
    Condition := Visit(BreakStmt.Condition);

  if not VarIsBool(Condition) then
    Raise ERuntimeError.Create(BreakStmt.Token, ErrConditionMustBeBool);

  if Condition then
    raise EBreakException.Create('');
end;

procedure TInterpreter.VisitContinueStmt(ContinueStmt: TContinueStmt);
begin
  raise EContinueException.Create('');
end;

procedure TInterpreter.VisitReturnStmt(ReturnStmt: TReturnStmt);
begin
  raise EReturnFromFunc.Create(Visit(ReturnStmt.Expr));
end;

procedure TInterpreter.VisitUseStmt(UseStmt: TUseStmt);
begin
  // do nothing
end;

procedure TInterpreter.VisitVarDecl(VarDecl: TVarDecl);
begin
  CheckDuplicate(VarDecl.Ident, 'Variable');
  CurrentSpace.Store(VarDecl.Ident, Visit(VarDecl.Expr));
end;

procedure TInterpreter.VisitFuncDecl(FuncDecl: TFuncDecl);
var
  Func: TFunc;
begin
  CheckDuplicate(FuncDecl.Ident, 'Func');
  Func := TFunc.Create(FuncDecl, CurrentSpace);
  CurrentSpace.Store(FuncDecl.Ident, ICallable(Func));
end;

procedure TInterpreter.VisitValDecl(ValDecl: TValDecl);
var
  Value: TVal;
begin
  CheckDuplicate(ValDecl.Ident, 'Val');
  Value := TVal.Create(ValDecl.FuncDecl, CurrentSpace);
  CurrentSpace.Store(ValDecl.Ident, IValuable(Value));
end;

function TInterpreter.getMembers(DeclList: TDeclList): TMembers;
var
  Decl: TDecl;
  Func: TFunc;
  Value: TVal;
begin
  Result.Init;
  for Decl in DeclList do begin
    case Decl.Kind of
      dkFunc: begin
        Func := TFunc.Create(Decl as TFuncDecl, CurrentSpace);
        Result.Methods[Decl.Ident.Text] := ICallable(Func);
      end;
      dkVal: begin
        Value := TVal.Create((Decl as TValDecl).FuncDecl, CurrentSpace);
        Result.Values[Decl.Ident.Text] := IValuable(Value);
      end;
      dkVar:
        if (Decl as TVarDecl).Mutable then
          Result.Fields[Decl.Ident.Text] := Visit((Decl as TVarDecl).Expr)
        else
          Result.Constants[Decl.Ident.Text] := Visit((Decl as TVarDecl).Expr);
    end;
  end;
end;

procedure TInterpreter.VisitClassDecl(ClassDecl: TClassDecl);
var
  GearClass: TGearClass;
  Members, Traits: TMembers;
  Parent: Variant;
begin
  CheckDuplicate(ClassDecl.Ident, 'Class');
  CurrentSpace.Store(ClassDecl.Ident, Nil);
  if Assigned(ClassDecl.Parent) then begin
    Parent := Visit(ClassDecl.Parent);
    CurrentSpace := TMemorySpace.Create(CurrentSpace);
    CurrentSpace.Store('inherited', Parent);
  end
  else Parent := Null;
  Traits := ApplyTraits(ClassDecl.Traits);
  Members := CombineMembers(Traits, ClassDecl.DeclList);
  if Assigned(ClassDecl.Parent) then
    CurrentSpace := CurrentSpace.EnclosingSpace;
  GearClass := TGearClass.Create(ClassDecl.Ident, Parent, Members);
  CurrentSpace.Update(ClassDecl.Ident, IClassable(GearClass));
end;

procedure TInterpreter.VisitExtensionDecl(ExtensionDecl: TExtensionDecl);
var
  TypeDecl: Variant;
  Members: TMembers;
begin
  TypeDecl := CurrentSpace.Load(ExtensionDecl.Ident);  // get actual type
  if not VarSupportsIntf(TypeDecl, [IClassable, IArrayable, IDictionable]) then
    Raise ERuntimeError.Create(ExtensionDecl.Ident.Token,
      Format(ErrExtIdNoType, [ExtensionDecl.Ident.Text]));
  Members := getMembers(ExtensionDecl.DeclList);
  if VarSupports(TypeDecl, IClassable) then
    IClassable(TypeDecl).ExtendWith(Members)
  else if VarSupports(TypeDecl, IArrayable) then
    IClassable(TypeDecl).ExtendWith(Members)
  else if VarSupports(TypeDecl, IDictionable) then
    IDictionable(TypeDecl).ExtendWith(Members);
end;

function TInterpreter.ApplyTraits(Traits: TExprList): TMembers;
var
  Trait: TExpr;
  TraitValue: Variant;
  GearTrait: ITraitable;
  i: Integer;
  Name: String;
begin
  Result.Init;
  for Trait in Traits do begin
    TraitValue := Visit(Trait);
    if not VarSupports(TraitValue, ITraitable) then
      Raise ERuntimeError.Create(Trait.Token,
        Format(ErrNotDeclaredAsTrait, [Trait.Token.Lexeme]));
    GearTrait := ITraitable(TraitValue);
    for i := 0 to GearTrait.Methods.Count-1 do begin
      Name := GearTrait.Methods.Keys[i];
      if Result.Methods.Contains(Name) then
        Raise ERuntimeError.Create(GearTrait.Ident.Token,
          Format(ErrTraitIsDeclared, ['function', Name]));
      Result.Methods[Name] := GearTrait.Methods[Name];
    end;
    for i := 0 to GearTrait.Values.Count-1 do begin
      Name := GearTrait.Values.Keys[i];
      if Result.Values.Contains(Name) then
        Raise ERuntimeError.Create(GearTrait.Ident.Token,
          Format(ErrTraitIsDeclared, ['value', Name]));
      Result.Values[Name] := GearTrait.Values[Name];
    end;
  end;
end;

function TInterpreter.CombineMembers(Traits: TMembers; DeclList: TDeclList): TMembers;
var
  Decl: TDecl;
  Func: TFunc;
  Value: TVal;
begin
  Result := Traits;
  for Decl in DeclList do begin
    if Traits.Methods.Contains(Decl.Ident.Text) or
       Traits.Values.Contains(Decl.Ident.Text) then
      Raise ERuntimeError.Create(Decl.Ident.Token,
        Format(ErrTraitDeclared, [Decl.Ident.Text]));
    case Decl.Kind of
      dkFunc: begin
        Func := TFunc.Create(Decl as TFuncDecl, CurrentSpace);
        Result.Methods[Decl.Ident.Text] := ICallable(Func);
      end;
      dkVal: begin
        Value := TVal.Create((Decl as TValDecl).FuncDecl, CurrentSpace);
        Result.Values[Decl.Ident.Text] := IValuable(Value);
      end;
      dkVar:
        if (Decl as TVarDecl).Mutable then
          Result.Fields[Decl.Ident.Text] := Visit((Decl as TVarDecl).Expr)
        else
          Result.Constants[Decl.Ident.Text] := Visit((Decl as TVarDecl).Expr);
    end;
  end;
end;

procedure TInterpreter.VisitTraitDecl(TraitDecl: TTraitDecl);
var
  Members, Traits: TMembers;
  Trait: TGearTrait;
begin
  CheckDuplicate(TraitDecl.Ident, 'Trait');
  CurrentSpace.Store(TraitDecl.Ident, Nil);
  Traits := ApplyTraits(TraitDecl.Traits);
  Members := CombineMembers(Traits, TraitDecl.DeclList);
  Trait := TGearTrait.Create(TraitDecl.Ident, Members);
  CurrentSpace.Update(TraitDecl.Ident, ITraitable(Trait));
end;

procedure TInterpreter.VisitArrayDecl(ArrayDecl: TArrayDecl);
var
  ArrayClass: TArrayClass;
  Expr: TExpr;
  Members: TMembers;
  Elements: TArrayElements;
begin
  CheckDuplicate(ArrayDecl.Ident, 'Array');
  Elements := TArrayElements.Create;
  for Expr in ArrayDecl.Elements do
    Elements.Add(Visit(Expr));
  Members := getMembers(ArrayDecl.DeclList);
  ArrayClass := TArrayClass.Create(ArrayDecl.Ident, Elements, Members);
  CurrentSpace.Store(ArrayDecl.Ident, IArrayable(ArrayClass));
end;

procedure TInterpreter.VisitDictDecl(DictDecl: TDictDecl);
var
  DictClass: TDictClass;
  Element: TKeyValuePair;
  Elements: TDictElements;
  Members: TMembers;
begin
  CheckDuplicate(DictDecl.Ident, 'Dictionary');
  Elements := TDictElements.Create;
  for Element in DictDecl.Elements do
    Elements[Visit(Element.Key)] := Visit(Element.Value);
  Members := getMembers(DictDecl.DeclList);
  DictClass := TDictClass.Create(DictDecl.Ident, Elements, Members);
  CurrentSpace.Store(DictDecl.Ident, IDictionable(DictClass));
end;

procedure TInterpreter.VisitEnumDecl(EnumDecl: TEnumDecl);
var
  Enum: TEnumClass;
  Members: TMembers;
begin
  CheckDuplicate(EnumDecl.Ident, 'Enum');
  Members := getMembers(EnumDecl.DeclList);
  Enum := TEnumClass.Create(EnumDecl.Ident, EnumDecl.Elements.Copy,
    EnumDecl.CaseTable, Members);
  CurrentSpace.Store(EnumDecl.Ident, IEnumable(Enum));
end;

procedure TInterpreter.VisitBlock(Block: TBlock);
var
  Node: TNode;
  EnclosingSpace: TMemorySpace;
begin
  EnclosingSpace := CurrentSpace;
  try
    CurrentSpace := TMemorySpace.Create(EnclosingSpace);
    for Node in Block.Nodes do
      Visit(Node);
  finally
    CurrentSpace := EnclosingSpace;
  end;
end;

procedure TInterpreter.VisitProduct(Product: TProduct);
var
  Node: TNode;
begin
  for Node in Product.Nodes do
    Visit(Node);
end;

procedure TInterpreter.CheckDuplicate(AIdent: TIdent; const TypeName: String);
begin
  if CurrentSpace.Contains(AIdent.Text) then
    Raise ERuntimeError.Create(AIdent.Token,
      Format(ErrDuplicateID, [TypeName, AIdent.Text]));
end;

procedure TInterpreter.Assign(Variable: TVariable; Value: Variant);
begin
  if Variable.Distance >= 0 then
    CurrentSpace.UpdateAt(Variable.Distance, Variable.Ident, Value)
  else
    Globals.Update(Variable.Ident, Value);
end;

function TInterpreter.Lookup(Variable: TVariable): Variant;
begin
  if Variable.Distance >= 0 then
    Result := CurrentSpace.LoadAt(Variable.Distance, Variable.Ident)
  else
    Result := Globals.Load(Variable.Ident);
end;

procedure TInterpreter.CheckNumericIndex(Value: Variant; Token: TToken);
begin
  if not VarIsNumeric(Value) then
    Raise ERuntimeError.Create(Token, ErrIndexMustBeNumeric);
end;

end.

