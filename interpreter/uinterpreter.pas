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
  Classes, SysUtils, uVisitor, uAST, uToken, uError, Variants, uMath, uMemory,
  uMembers;

type

  TInterpreter = class(TVisitor)
    private
      CurrentSpace: TMemorySpace;
      FGlobals: TMemorySpace;
      procedure CheckDuplicate(AIdent: TIdent; const TypeName: String);
      function Lookup(Variable: TVariable): Variant;
      procedure Assign(Variable: TVariable; Value: Variant);
      function getMembers(DeclList: TDeclList): TMembers;
      function ApplyTraits(Traits: TExprList): TMembers;
      function CombineMembers(Traits: TMembers; DeclList: TDeclList): TMembers;
      procedure CheckNumericIndex(Value: Variant; Token: TToken);
    public
      property Globals: TMemorySpace read FGlobals;
      constructor Create;
      destructor Destroy; override;
      procedure Execute(Tree: TProduct);
      procedure Execute(Block: TBlock; MemorySpace: TMemorySpace);    published
      procedure VisitIdent(Ident: TIdent);
      // Expressions
      function VisitBinaryExpr(BinaryExpr: TBinaryExpr): Variant;
      function VisitConstExpr(ConstExpr: TConstExpr): Variant;
      function VisitUnaryExpr(UnaryExpr: TUnaryExpr): Variant;
      function VisitCallExpr(CallExpr: TCallExpr): Variant;
      function VisitVariable(Variable: TVariable): Variant;
      function VisitIfExpr(IfExpr: TIfExpr): Variant;
      function VisitMatchExpr(MatchExpr: TMatchExpr): Variant;
      function VisitTupleExpr(TupleExpr: TTupleExpr): Variant;
      function VisitGetExpr(GetExpr: TGetExpr): Variant;
      function VisitSelfExpr(SelfExpr: TSelfExpr): Variant;
      function VisitInheritedExpr(InheritedExpr: TInheritedExpr): Variant;
      function VisitArrayDeclExpr(ArrayDeclExpr: TArrayDeclExpr): Variant;
      function VisitDictDeclExpr(DictDeclExpr: TDictDeclExpr): Variant;
      function VisitIndexedExpr(IndexedExpr: TIndexedExpr): Variant;
      function VisitInterpolatedExpr(InterpolatedExpr: TInterpolatedExpr): Variant;
      // Statements
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
      // Declarations
      procedure VisitVarDecl(VarDecl: TVarDecl);
      procedure VisitVarDecls(VarDecls: TVarDecls);
      procedure VisitFuncDecl(FuncDecl: TFuncDecl);
      function VisitFuncDeclExpr(FuncDeclExpr: TFuncDeclExpr): Variant;
      procedure VisitClassDecl(ClassDecl: TClassDecl);
      procedure VisitValDecl(ValDecl: TValDecl);
      procedure VisitExtensionDecl(ExtensionDecl: TExtensionDecl);
      procedure VisitTraitDecl(TraitDecl: TTraitDecl);
      procedure VisitArrayDecl(ArrayDecl: TArrayDecl);
      procedure VisitDictDecl(DictDecl: TDictDecl);
      procedure VisitEnumDecl(EnumDecl: TEnumDecl);
      // Blocks
      procedure VisitBlock(Block: TBlock);
      procedure VisitProduct(Product: TProduct);
  end;

implementation
uses uCallable, uFunc, uStandard, uStandardList, uStandardCRT, uStandardFiles, uVariantSupport,
  uTupleIntf, uTuple, uClassIntf, uClass, uArrayIntf, uArray, uDictIntf, uDict, uEnumIntf, uEnum;

const
  ErrDuplicateID = 'Duplicate identifier: %s "%s" is already declared.';
  ErrIncompatibleTypes = 'Incompatible types in assignment: %s vs. %s.';
  ErrConditionNotBoolean = 'Condition is not Boolean.';
  ErrNotAFunction = '"%s" is not defined as function.';
  ErrExpectedInstance = 'Expected declared type instance.';
  ErrUndefVarOrSelfMissing = 'Variable is undefined or missing "self".';
  ErrClassMemberImmutable = 'Class member "%s" is immutable.';
  ErrUndefinedMethod = 'Undefined method "%s".';
  ErrExtIdNoType = 'Extension identifier "%s" is not a type.';
  ErrNotDeclaredAsTrait = '"%s" is not declared as trait.';
  ErrTraitIsDeclared = 'Trait "%s" "%s" is already declared.';
  ErrTraitDeclared = 'Trait already declared for member "%s".';
  ErrArrayTypeExpected = 'Array or string type expected.';
  ErrIllegalAssignOp = 'Illegal assignment operator, ":=" expected.';

constructor TInterpreter.Create;
begin
  FGlobals := TMemorySpace.Create();
  StoreStandardFunctions(FGlobals);
  StoreStandardListFunctions(FGlobals);
  StoreStandardCRTFunctions(FGlobals);
  StoreStandardFileFunctions(FGlobals);

  // store base Array type
  FGlobals.Store(
    'Array',
    IArrayable(
      TArrayClass.Create(
        TIdent.Create(TToken.Create(ttIdentifier, 'Array', Null, 0, 0)),
        TArrayElements.Create(),
        TMembers.Create(TFieldTable.Create, TConstTable.Create,
          TMethodTable.Create, TValueTable.Create))),
    TToken.Create(ttNone, '', Null, 0, 0));

  // store base Dictionary type
  FGlobals.Store(
    'Dictionary',
    IDictionary(
      TDictClass.Create(
        TIdent.Create(TToken.Create(ttIdentifier, 'Dictionary', Null, 0, 0)),
        TDictElements.Create(),
        TMembers.Create(TFieldTable.Create, TConstTable.Create,
          TMethodTable.Create, TValueTable.Create))),
    TToken.Create(ttNone, '', Null, 0, 0));

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
    VisitProc(Tree);
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
    VisitProc(Block);
  finally
    CurrentSpace := SavedSpace;
  end;
end;

procedure TInterpreter.VisitIdent(Ident: TIdent);
begin
  // do nothing
end;

function TInterpreter.VisitBinaryExpr(BinaryExpr: TBinaryExpr): Variant;
var
  Left, Right: Variant;
  Op: TToken;
begin
  Left := VisitFunc(BinaryExpr.Left);
  Right := VisitFunc(BinaryExpr.Right);
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
  Expr := VisitFunc(UnaryExpr.Expr);
  case UnaryExpr.Op.Typ of
    ttNot: Result := TMath._Not(Expr, UnaryExpr.Op);
    ttMin: Result := TMath._Neg(Expr, UnaryExpr.Op);
    ttQuestion: Result := Expr <> Unassigned;
    else Result := Expr;
  end;
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
  if CallExpr.FromClass then
    Callee := VisitFunc(CallExpr.Callee)
  else
    Callee := VisitFunc(CallExpr.Signature);
  if VarSupportsIntf(Callee, [ICallable, IClassable, IArrayable, IDictionary]) then
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
    CallArg := TCallArg.Create(VisitFunc(CallExpr.Args[i].Expr),
               CallExpr.Args[i].Ident, Token);
    Args.Add(CallArg);
  end;
  Result := Func.Call(CallExpr.Token, Self, Args);
end;

function TInterpreter.VisitVariable(Variable: TVariable): Variant;
begin
  Result := Lookup(Variable);
  if VarSupports(Result, IValuable) then
    Result := IValuable(Result).Call(Variable.Token, Self, TArgList.Create());
  if VarIsEmpty(Result) then
    Raise ERuntimeError.Create(Variable.Token, ErrUndefVarOrSelfMissing);
end;

function TInterpreter.VisitIfExpr(IfExpr: TIfExpr): Variant;
var
  Condition: Variant;
begin
  Condition := VisitFunc(IfExpr.Condition);
  if VarIsBool(Condition) then begin
    if Condition then
      Result := VisitFunc(IfExpr.TrueExpr)
    else
      Result := VisitFunc(IfExpr.FalseExpr)
  end
  else
    Raise ERuntimeError.Create(IfExpr.Token, ErrConditionNotBoolean);
end;

function TInterpreter.VisitMatchExpr(MatchExpr: TMatchExpr): Variant;
var
  MatchValue, IfValue: Variant;
  Key: TExpr;
begin
  MatchValue := VisitFunc(MatchExpr.Expr);
  for Key in MatchExpr.IfLimbs.Keys do begin
    IfValue := VisitFunc(Key);
    if TMath._EQ(MatchValue, IfValue, Key.Token) then
      Exit(VisitFunc(MatchExpr.IfLimbs[Key]));
  end;
  Result := VisitFunc(MatchExpr.ElseLimb);
end;

function TInterpreter.VisitTupleExpr(TupleExpr: TTupleExpr): Variant;
var
  Expr: TExpr;
  Tuple: ITuple;
begin
  Tuple := ITuple(TTuple.Create);

  for Expr in TupleExpr.ExprList do
    Tuple.Elements.Add(VisitFunc(Expr));

  Result := Tuple;
end;

function TInterpreter.VisitGetExpr(GetExpr: TGetExpr): Variant;
var
  Instance, Index: Variant;
  Ident: TIdent;
begin
  Instance := VisitFunc(GetExpr.Instance);
  if VarSupports(Instance, ITuple) then begin
    Index := VisitFunc(GetExpr.Member);
    if not VarIsNumeric(Index) then
      Raise ERuntimeError.Create(GetExpr.Member.Token, 'Integer number expected.');
    Result := ITuple(Instance).Get(Index, GetExpr.Member.Token);
  end
  else if VarSupportsIntf(Instance,
    [IGearInstance, IArrayInstance, IDictInstance, IEnumInstance]) then
  begin
    Ident := TVariable(GetExpr.Member).Ident;
    if VarSupports(Instance, IGearInstance) then
      Result := IGearInstance(Instance).GetMember(Ident)
    else if VarSupports(Instance, IArrayInstance) then
      Result := IArrayInstance(Instance).GetMember(Ident)
    else if VarSupports(Instance, IDictInstance) then
      Result := IDictInstance(Instance).GetMember(Ident)
    else if VarSupports(Instance, IEnumInstance) then
      Result := IEnumInstance(Instance).GetMember(Ident);
    if VarSupports(Result, IValuable) then
      Result := ICallable((IValuable(Result) as TVal)
                          .Bind(Instance))
                          .Call(GetExpr.Token, Self, TArgList.Create());
  end
  else if VarSupports(Instance, IClassable) then begin
    Ident := TVariable(GetExpr.Member).Ident;
    Result := IClassable(Instance).GetStaticMember(Ident);
  end
  else if VarSupports(Instance, IEnumable) then begin
    Ident := TVariable(GetExpr.Member).Ident;
    if IEnumable(Instance).isCase(Ident.Text) then
      Result := Ident.Text
    else if Ident.Text = 'Elements' then
      Result := IEnumable(Instance).ElementList
    else
      Result := IEnumInstance(TEnumInstance.Create(
                  IEnumable(Instance) as TEnumClass, Ident));
  end
  else
    Raise ERuntimeError.Create(GetExpr.Token, ErrExpectedInstance);
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
  MethodID: TIdent;
begin
  Distance := InheritedExpr.Variable.Distance;
  Parent := ICallable(CurrentSpace.LoadAt(Distance, 'inherited'));
  Instance := IGearInstance(CurrentSpace.LoadAt(Distance-1, 'self'));
  MethodID := InheritedExpr.Method.Ident;
  if MethodID.Text.Contains('init') then MethodID.Text := 'init';
  Method := (Parent as TGearClass).FindMethod(Instance, MethodID.Text);
  if Method = Nil then
    Raise ERuntimeError.Create(MethodID.Token, Format(
      ErrUndefinedMethod, [MethodID.Text]));
  Result := Method;
end;

function TInterpreter.VisitArrayDeclExpr(ArrayDeclExpr: TArrayDeclExpr): Variant;
var
  ArrayExpr: IArrayable;
  Instance: IArrayInstance;
  Expr: TExpr;
begin
  ArrayExpr := IArrayable(FGlobals['Array']);
  Instance := IArrayInstance(TArrayInstance.Create(ArrayExpr as TArrayClass));
  for Expr in ArrayDeclExpr.ArrayDecl.Elements do
    Instance.Elements.Add(VisitFunc(Expr));
  Result := Instance;
end;

function TInterpreter.VisitDictDeclExpr(DictDeclExpr: TDictDeclExpr): Variant;
var
  DictExpr: IDictionary;
  Instance: IDictInstance;
  Key: TExpr;
begin
  DictExpr := IDictionary(FGlobals['Dictionary']);

  Instance := IDictInstance(TDictInstance.Create(DictExpr as TDictClass));
  for Key in DictDeclExpr.DictDecl.KeyValueList.Keys do
    Instance.Elements.Add(VisitFunc(Key),
                          VisitFunc(DictDeclExpr.DictDecl.KeyValueList[Key]));

  Result := Instance;
end;

function TInterpreter.VisitIndexedExpr(IndexedExpr: TIndexedExpr): Variant;
var
  Instance: Variant;
  Index: Variant;
begin
  Instance := VisitFunc(IndexedExpr.Variable);
  Index := VisitFunc(IndexedExpr.Index);
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
    Result := TMath._Add(Result, VisitFunc(Expr), Expr.Token);
end;


//
// STATEMENTS
//

procedure TInterpreter.VisitPrintStmt(PrintStmt: TPrintStmt);
var
  Value: String='';
  Terminator: String='';
  Expr: TExpr;
begin
  for Expr in PrintStmt.ExprList do begin
    Value := VisitFunc(Expr).toString;
    Write(Value);
  end;
  Terminator := VisitFunc(PrintStmt.Terminator).toString;
  Write(Terminator);
end;

// Helper functions for assignment

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
      Result := 'Unknown';
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

  if VarSupports(OldValue, IEnumInstance) then begin
    TMath.CheckSameEnumTypes(OldValue, NewValue, ID);
    if Op.Typ = ttAssign then
      Exit(NewValue)
    else Raise ERuntimeError.Create(Op, ErrIllegalAssignOp);
  end;

  if OldType <> NewType then
    Raise ERuntimeError.Create(ID,
      Format(ErrIncompatibleTypes, [OldType, NewType]));

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
    NewValue := VisitFunc(Expr);
    Value := getAssignValue(OldValue, NewValue, Variable.Token, Op);
    Assign(Variable, Value);
  end;
end;

procedure TInterpreter.VisitCallExprStmt(CallExprStmt: TCallExprStmt);
begin
  // Since a CallExpr returns a value we have to use VisitFunc here, but we
  // may ignore the result.
  VisitFunc(CallExprStmt.CallExpr);
end;

procedure TInterpreter.VisitSetStmt(SetStmt: TSetStmt);
var
  Instance, OldValue, NewValue, Value, Index: Variant;
  Member: TExpr;
  Ident: TIdent;
begin
  Instance := VisitFunc(SetStmt.GetExpr.Instance);
  Member := SetStmt.GetExpr.Member;
  if VarSupports(Instance, ITuple) then begin
    Index := VisitFunc(Member);
    if not VarIsNumeric(Index) then
      Raise ERuntimeError.Create(Member.Token, 'Integer number expected.');
    OldValue := ITuple(Instance).Get(Index, Member.Token);
    NewValue := VisitFunc(SetStmt.Expr);
    Value := getAssignValue(OldValue, NewValue, Member.Token, SetStmt.Op);
    ITuple(Instance).Put(Index, Value, Member.Token);
  end
  else if VarSupports(Instance, IGearInstance) then begin
    Ident := TVariable(Member).Ident;
    OldValue := IGearInstance(Instance).GetMember(Ident);
    if (IGearInstance(Instance).isConstant(Ident) and
       (not VarIsNull(OldValue))) or (VarSupports(OldValue, IValuable)) then
      Raise ERuntimeError.Create(Ident.Token, Format(
        ErrClassMemberImmutable, [Ident.Text]));
    NewValue := VisitFunc(SetStmt.Expr);
    Value := getAssignValue(OldValue, NewValue, Member.Token, SetStmt.Op);
    IGearInstance(Instance).SetField(Ident, Value);
  end
  else
    Raise ERuntimeError.Create(SetStmt.GetExpr.Token, ErrExpectedInstance);
end;

procedure TInterpreter.VisitIndexedExprStmt(IndexedExprStmt: TIndexedExprStmt);
var
  Variable, OldValue, NewValue, Value: Variant;
  Index: Variant;
  VarAsStr: String;
begin
  Variable := VisitFunc(IndexedExprStmt.IndexedExpr.Variable);
  Index := VisitFunc(IndexedExprStmt.IndexedExpr.Index);
  NewValue := VisitFunc(IndexedExprStmt.Expr);

  with IndexedExprStmt do
  if VarSupports(Variable, IArrayInstance) then begin
    CheckNumericIndex(Index, IndexedExpr.Index.Token);
    OldValue := IArrayInstance(Variable).Get(Index, IndexedExpr.Index.Token);
    Value := getAssignValue(OldValue, NewValue, IndexedExpr.Variable.Token, Op);
    IArrayInstance(Variable).Put(Index, Value, IndexedExpr.Index.Token);
  end
  else if VarSupports(Variable, IDictInstance) then begin
    OldValue := IDictInstance(Variable).get(Index, IndexedExpr.Index.Token);
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
    Value := VisitFunc(Condition);
    if VarIsBool(Value) then
      Result := Boolean(Value)
    else
      Raise ERuntimeError.Create(IfStmt.Token, ErrConditionNotBoolean);
  end;

begin
  try
    if Assigned(IfStmt.VarDecl) then begin
      SavedSpace := CurrentSpace;
      CurrentSpace := TMemorySpace.Create(SavedSpace);
      VisitProc(IfStmt.VarDecl);
    end;

    if isBooleanAndTrue(IfStmt.Condition) then
      VisitProc(IfStmt.ThenPart)
    else if Assigned(IfStmt.ElseIfs) then begin
      for i := 0 to IfStmt.ElseIfs.Count-1 do begin
        if isBooleanAndTrue(IfStmt.ElseIfs[i]) then begin
          VisitProc(IfStmt.ElseIfParts[i]);
          ElseIfExecuted := True;
          Break;
        end;
      end;
      if not ElseIfExecuted then
        if Assigned(IfStmt.ElsePart) then
          VisitProc(IfStmt.ElsePart);
    end
    else if Assigned(IfStmt.ElsePart) then
      VisitProc(IfStmt.ElsePart);

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
      VisitProc(WhileStmt.VarDecl);
    end;
    try
      Condition := VisitFunc(WhileStmt.Condition);
      if VarIsBool(Condition) then begin
        while Condition do begin
          try
            VisitProc(WhileStmt.Block);
          except
            on EContinueException do;
          end;
          Condition := VisitFunc(WhileStmt.Condition);
        end;
      end
      else
        Raise ERuntimeError.Create(WhileStmt.Token, ErrConditionNotBoolean);
    except
      on EBreakException do; // nothing
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
    Condition := VisitFunc(RepeatStmt.Condition);
    if VarIsBool(Condition) then begin
      repeat
        try
          VisitProc(RepeatStmt.Block);
        except
          on EContinueException do;
        end;
        Condition := VisitFunc(RepeatStmt.Condition);
      until Condition;
    end
    else
      Raise ERuntimeError.Create(RepeatStmt.Token, ErrConditionNotBoolean);
  except
    on EBreakException do;
  end;
end;

procedure TInterpreter.VisitEnsureStmt(EnsureStmt: TEnsureStmt);
var
  Condition: Variant;
begin
  if Assigned(EnsureStmt.VarDecl) then
    VisitProc(EnsureStmt.VarDecl);
  Condition := VisitFunc(EnsureStmt.Condition);
  if VarIsBool(Condition) then begin
    if not Condition then
      VisitProc(EnsureStmt.ElsePart)
  end
  else
    Raise ERuntimeError.Create(EnsureStmt.Token, ErrConditionNotBoolean);
end;

procedure TInterpreter.VisitSwitchStmt(SwitchStmt: TSwitchStmt);
var
  SwitchValue, CaseValue: Variant;
  Key: TCaseItem;
  MatchCase: Boolean;
begin
  SwitchValue := VisitFunc(SwitchStmt.Expr);
  for Key in SwitchStmt.CaseLimbs.Keys do begin
    CaseValue := VisitFunc(Key.Expr);
    if Key.isObj then
      MatchCase := TMath._Is(SwitchValue, CaseValue, Key.Expr.Token)
    else
      MatchCase := TMath._EQ(SwitchValue, CaseValue, Key.Expr.Token);
    if MatchCase then begin
      VisitProc(SwitchStmt.CaseLimbs[Key]);
      Exit;
    end;
  end;
  VisitProc(SwitchStmt.ElseLimb);
end;

procedure TInterpreter.VisitBreakStmt(BreakStmt: TBreakStmt);
var
  Condition: Variant;
begin
  Condition := True;
  if Assigned(BreakStmt.Condition) then
    Condition := VisitFunc(BreakStmt.Condition);

  if not VarIsBool(Condition) then
    Raise ERuntimeError.Create(BreakStmt.Token, ErrConditionNotBoolean);

  if Condition then
    raise EBreakException.Create('');
end;

procedure TInterpreter.VisitContinueStmt(ContinueStmt: TContinueStmt);
begin
  raise EContinueException.Create('');
end;

procedure TInterpreter.VisitReturnStmt(ReturnStmt: TReturnStmt);
begin
  raise EReturnFromFunc.Create(VisitFunc(ReturnStmt.Expr));
end;

procedure TInterpreter.VisitUseStmt(UseStmt: TUseStmt);
begin
  //do nothing
end;


procedure TInterpreter.VisitVarDecl(VarDecl: TVarDecl);
begin
  CheckDuplicate(VarDecl.Ident, 'Variable');
  CurrentSpace.Store(VarDecl.Ident, VisitFunc(VarDecl.Expr));
end;

procedure TInterpreter.VisitVarDecls(VarDecls: TVarDecls);
var
  Decl: TDecl;
begin
  for Decl in VarDecls.List do
    VisitProc(Decl);
end;

procedure TInterpreter.VisitFuncDecl(FuncDecl: TFuncDecl);
var
  Func: TFunc;
begin
  CheckDuplicate(FuncDecl.Ident, 'Func');
  Func := TFunc.Create(FuncDecl, CurrentSpace);
  CurrentSpace.Store(FuncDecl.Ident, ICallable(Func));
end;

function TInterpreter.VisitFuncDeclExpr(FuncDeclExpr: TFuncDeclExpr): Variant;
begin
  Result := ICallable(TFunc.Create(FuncDeclExpr.FuncDecl, CurrentSpace));
end;

procedure TInterpreter.VisitClassDecl(ClassDecl: TClassDecl);
var
  GearClass: TGearClass;
  Members, StaticMembers, Traits: TMembers;
  Decl: TDecl;
  Func: TFunc;
  Parent: Variant;
begin
  CheckDuplicate(ClassDecl.Ident, 'Class');
  CurrentSpace.Store(ClassDecl.Ident, Nil);
  if Assigned(ClassDecl.Parent) then begin
    Parent := VisitFunc(ClassDecl.Parent);
    CurrentSpace := TMemorySpace.Create(CurrentSpace);
    CurrentSpace.Store('inherited', Parent, ClassDecl.Parent.Token);
  end
  else Parent := Null;
  Traits := ApplyTraits(ClassDecl.Traits);
  Members := CombineMembers(Traits, ClassDecl.DeclList);
  StaticMembers := getMembers(ClassDecl.StaticList);
  if Assigned(ClassDecl.Parent) then
    CurrentSpace := CurrentSpace.EnclosingSpace;
  GearClass := TGearClass.Create(ClassDecl.Ident, Parent, Members, StaticMembers);
  CurrentSpace.Update(ClassDecl.Ident, IClassable(GearClass));
end;

procedure TInterpreter.VisitValDecl(ValDecl: TValDecl);
var
  Value: TVal;
begin
  CheckDuplicate(ValDecl.Ident, 'Val');
  Value := TVal.Create(ValDecl.FuncDecl, CurrentSpace);
  CurrentSpace.Store(ValDecl.Ident, IValuable(Value));
end;

procedure TInterpreter.VisitExtensionDecl(ExtensionDecl: TExtensionDecl);
var
  TypeDecl: Variant;
  Members: TMembers;
begin
  TypeDecl := CurrentSpace.Load(ExtensionDecl.Ident);  // get actual type
  if not VarSupportsIntf(TypeDecl, [IClassable, IArrayable, IDictionary]) then
    Raise ERuntimeError.Create(ExtensionDecl.Ident.Token,
      Format(ErrExtIdNoType, [ExtensionDecl.Ident.Text]));
  Members := getMembers(ExtensionDecl.DeclList);
  if VarSupports(TypeDecl, IClassable) then
    IClassable(TypeDecl).ExtendWith(Members)
  else if VarSupports(TypeDecl, IArrayable) then
    IArrayable(TypeDecl).ExtendWith(Members)
  else if VarSupports(TypeDecl, IDictionary) then
    IDictionary(TypeDecl).ExtendWith(Members)
  else if VarSupports(TypeDecl, IEnumable) then
    IEnumable(TypeDecl).ExtendWith(Members);
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
    Elements.Add(VisitFunc(Expr));
  Members := getMembers(ArrayDecl.DeclList);
  ArrayClass := TArrayClass.Create(ArrayDecl.Ident, Elements, Members);
  CurrentSpace.Store(ArrayDecl.Ident, IArrayable(ArrayClass));
end;

procedure TInterpreter.VisitDictDecl(DictDecl: TDictDecl);
var
  DictClass: TDictClass;
  Key: TExpr;
  Elements: TDictElements;
  Members: TMembers;
begin
  CheckDuplicate(DictDecl.Ident, 'Dictionary');
  Elements := TDictElements.Create;
  for Key in DictDecl.KeyValueList.Keys do
    Elements.Add(VisitFunc(Key), VisitFunc(DictDecl.KeyValueList[Key]));
  Members := getMembers(DictDecl.DeclList);
  DictClass := TDictClass.Create(DictDecl.Ident, Elements, Members);
  CurrentSpace.Store(DictDecl.Ident, IDictionary(DictClass));
end;

procedure TInterpreter.VisitEnumDecl(EnumDecl: TEnumDecl);
var
  Enum: TEnumClass;
  Members: TMembers;
begin
  CheckDuplicate(EnumDecl.Ident, 'Enum');
  Members := getMembers(EnumDecl.DeclList);
  Enum := TEnumClass.Create(EnumDecl.Ident, EnumDecl.Elements.Copy, EnumDecl.CaseTable, Members);
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
      VisitProc(Node);
  finally
    CurrentSpace := EnclosingSpace;
  end;
end;

procedure TInterpreter.VisitProduct(Product: TProduct);
var
  Node: TNode;
begin
  for Node in Product.Nodes do
    VisitProc(Node);
end;

//
// Helper functions
//

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
        Result.Methods.Add(Decl.Ident.Text, ICallable(Func));
      end;
      dkVal: begin
        Value := TVal.Create((Decl as TValDecl).FuncDecl, CurrentSpace);
        Result.Values.Add(Decl.Ident.Text, IValuable(Value));
      end;
      dkVar:
        if (Decl as TVarDecl).Mutable then
          Result.Fields.Add(Decl.Ident.Text, VisitFunc((Decl as TVarDecl).Expr))
        else
          Result.Constants.Add(Decl.Ident.Text, VisitFunc((Decl as TVarDecl).Expr));
    end;
  end;
end;

function TInterpreter.ApplyTraits(Traits: TExprList): TMembers;
var
  Trait: TExpr;
  TraitValue: Variant;
  GearTrait: ITraitable;
  Name: String;
begin
  Result.Init;
  for Trait in Traits do begin
    TraitValue := VisitFunc(Trait);
    if not VarSupports(TraitValue, ITraitable) then
      Raise ERuntimeError.Create(Trait.Token,
        Format(ErrNotDeclaredAsTrait, [Trait.Token.Lexeme]));
    GearTrait := ITraitable(TraitValue);
    for Name in GearTrait.Methods.Keys do begin
      if Result.Methods.ContainsKey(Name) then
        Raise ERuntimeError.Create(GearTrait.Ident.Token,
          Format(ErrTraitIsDeclared, ['function', Name]));
      Result.Methods.Add(Name, GearTrait.Methods[Name]);
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
    if Traits.Methods.ContainsKey(Decl.Ident.Text) then
      Raise ERuntimeError.Create(Decl.Ident.Token,
        Format(ErrTraitDeclared, [Decl.Ident.Text]));
    case Decl.Kind of
      dkFunc: begin
        Func := TFunc.Create(Decl as TFuncDecl, CurrentSpace);
        Result.Methods.Add(Decl.Ident.Text, ICallable(Func));
      end;
      dkVal: begin
        Value := TVal.Create((Decl as TValDecl).FuncDecl, CurrentSpace);
        Result.Values.Add(Decl.Ident.Text, IValuable(Value));
      end;
      dkVar:
        if (Decl as TVarDecl).Mutable then
          Result.Fields.Add(Decl.Ident.Text, VisitFunc((Decl as TVarDecl).Expr))
        else
          Result.Constants.Add(Decl.Ident.Text, VisitFunc((Decl as TVarDecl).Expr));
    end;
  end;
end;


procedure TInterpreter.CheckDuplicate(AIdent: TIdent; const TypeName: String);
begin
  if CurrentSpace.ContainsKey(AIdent.Text) then
    Raise ERuntimeError.Create(AIdent.Token,
      Format(ErrDuplicateID, [TypeName, AIdent.Text]));
end;

function TInterpreter.Lookup(Variable: TVariable): Variant;
begin
  if Variable.Distance >= 0 then
    Result := CurrentSpace.LoadAt(Variable.Distance, Variable.Ident)
  else
    Result := Globals.Load(Variable.Ident);
end;

procedure TInterpreter.Assign(Variable: TVariable; Value: Variant);
begin
  if Variable.Distance >= 0 then
    CurrentSpace.UpdateAt(Variable.Distance, Variable.Ident, Value)
  else
    Globals.Update(Variable.Ident, Value);
end;

procedure TInterpreter.CheckNumericIndex(Value: Variant; Token: TToken);
begin
  if not VarIsNumeric(Value) then
    Raise ERuntimeError.Create(Token, 'Index must be a number.');
end;

end.

