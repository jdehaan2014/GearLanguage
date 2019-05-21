unit uResolver;

{ This unit defines the semantic analyzer (symbol table, resolver).

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
  Classes, SysUtils, uVisitor, uCollections, uAST, uToken, uError,
  Variants;

type

  TStatus = (Declared, Enabled);

  TFuncKind = (fkNone, fkFunc, fkInit, fkMethod);
  TClassKind = (ckNone, ckClass, ckSubClass, ckExtension, ckTrait,
    ckArray, ckDictionary, ckEnum);

  TSymbol = class
    Name: String;
    Status: TStatus;
    Mutable: Boolean;
    IsNull: Boolean;
    constructor Create(const AName: String; const AStatus: TStatus;
      const AMutable: Boolean=False);
  end;

  TScope = class(specialize TDictionary<String, TSymbol>)
    private
      FEnclosing: TScope;
    public
      property Enclosing: TScope read FEnclosing;
      constructor Create(AEnclosing: TScope=Nil);
      procedure Enter(ASymbol: TSymbol);
      function Lookup(AName: String): TSymbol;
  end;

  TScopes = specialize TStack<TScope>;
  TClassList = specialize TArray<String>;

  TResolver = class(TVisitor)
    public
      constructor Create;
      destructor Destroy; override;
      procedure Resolve(Tree: TProduct);
    published
      procedure VisitNode(Node: TNode);
      procedure VisitIdent(Ident: TIdent);
      // Expr
      procedure VisitBinaryExpr(BinaryExpr: TBinaryExpr);
      procedure VisitConstExpr(ConstExpr: TConstExpr);
      procedure VisitUnaryExpr(UnaryExpr: TUnaryExpr);
      procedure VisitVariable(Variable: TVariable);
      procedure VisitCallExpr(CallExpr: TCallExpr);
      procedure VisitIfExpr(IfExpr: TIfExpr);
      procedure VisitMatchExpr(MatchExpr: TMatchExpr);
      procedure VisitFuncDeclExpr(FuncDeclExpr: TFuncDeclExpr);
      procedure VisitGetExpr(GetExpr: TGetExpr);
      procedure VisitSelfExpr(SelfExpr: TSelfExpr);
      procedure VisitInheritedExpr(InheritedExpr: TInheritedExpr);
      procedure VisitArrayDeclExpr(ArrayDeclExpr: TArrayDeclExpr);
      procedure VisitIndexedExpr(IndexedExpr: TIndexedExpr);
      procedure VisitDictDeclExpr(DictDeclExpr: TDictDeclExpr);
      procedure VisitInterpolatedExpr(InterpolatedExpr: TInterpolatedExpr);
      procedure VisitTupleExpr(TupleExpr: TTupleExpr);
      // Stmt
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
      // Decl
      procedure VisitVarDecl(VarDecl: TVarDecl);
      procedure VisitVarDecls(VarDecls: TVarDecls);
      procedure VisitFuncDecl(FuncDecl: TFuncDecl);
      procedure VisitValDecl(ValDecl: TValDecl);
      procedure VisitClassDecl(ClassDecl: TClassDecl);
      procedure VisitExtensionDecl(ExtensionDecl: TExtensionDecl);
      procedure VisitTraitDecl(TraitDecl: TTraitDecl);
      procedure VisitArrayDecl(ArrayDecl: TArrayDecl);
      procedure VisitDictDecl(DictDecl: TDictDecl);
      procedure VisitEnumDecl(EnumDecl: TEnumDecl);
      // Blocks
      procedure VisitBlock(Block: TBlock);
      procedure VisitProduct(Product: TProduct);
    private
      Scopes: TScopes;
      GlobalScope: TScope;
      CurrentScope: TScope;
      CurrentFuncKind: TFuncKind;
      CurrentClassKind: TClassKind;
      ClassList: TClassList;
      procedure BeginScope;
      procedure EndScope;
      procedure Declare(Ident: TIdent; const isMutable: Boolean=False);
      procedure Enable(Ident: TIdent);
      procedure ResolveLocal(Variable: TVariable);
      procedure ResolveFunction(Func: TFuncDecl; FuncKind: TFuncKind);
      procedure ResolveClass(ClassDecl: TClassDecl);
      function Retrieve(Ident: TIdent): TSymbol;
      procedure EnableStandardFunctions;
  end;

implementation

const
  ErrCannotReadLocalVar = 'Cannot read local variable in its own declaration.';
  ErrCannotAssignToConstant = 'Cannot assign value to constant "%s".';
  ErrDuplicateIdInScope = 'Duplicate identifier "%s" in this scope.';
  ErrUndeclaredVar = 'Undeclared variable "%s".';
  ErrReturnFromFunc = 'Return can only be used from a function.';
  ErrSelfOutsideClass = 'Cannot use "self" outside a class.';
  ErrParentNoClass = 'Parent "%s" is not defined as class.';
  ErrInheritedInClass = 'Can only use "inherited" inside a class.';
  ErrInheritedNotInSubClass = 'Cannot use "inherited" in a class without parent class.';

constructor TResolver.Create;
begin
  GlobalScope := TScope.Create();
  GlobalScope.Sorted := True;
  EnableStandardFunctions;
  CurrentScope := GlobalScope;
  Scopes := TScopes.Create;
  CurrentFuncKind := fkNone;
  CurrentClassKind := ckNone;
  ClassList := TClassList.Create;
end;

destructor TResolver.Destroy;
begin
  GlobalScope.Free;
  Scopes.Free;
  ClassList.Free;
  inherited Destroy;
end;

procedure TResolver.Resolve(Tree: TProduct);
begin
  Visit(Tree);
end;

procedure TResolver.VisitNode(Node: TNode);
begin
  // do nothing
end;

procedure TResolver.VisitIdent(Ident: TIdent);
begin
  // do nothing
end;

procedure TResolver.VisitBinaryExpr(BinaryExpr: TBinaryExpr);
begin
  Visit(BinaryExpr.Left);
  Visit(BinaryExpr.Right);
end;

procedure TResolver.VisitConstExpr(ConstExpr: TConstExpr);
begin
  // do nothing
end;

procedure TResolver.VisitUnaryExpr(UnaryExpr: TUnaryExpr);
begin
  Visit(UnaryExpr.Expr);
end;

procedure TResolver.VisitVariable(Variable: TVariable);
var
  Symbol: TSymbol;
begin
  Symbol := Retrieve(Variable.Ident);
  if Symbol <> Nil then begin
    if Symbol.Status = Declared then
      Errors.Append(Variable.Token, ErrCannotReadLocalVar);
    ResolveLocal(Variable);
  end;
end;

procedure TResolver.VisitCallExpr(CallExpr: TCallExpr);
var
  i: Integer;
begin
  Visit(CallExpr.Callee);
  for i := 0 to CallExpr.Args.Count-1 do
    Visit(CallExpr.Args[i].Expr);
end;

procedure TResolver.VisitIfExpr(IfExpr: TIfExpr);
begin
  Visit(IfExpr.Condition);
  Visit(IfExpr.TrueExpr);
  Visit(IfExpr.FalseExpr);
end;

procedure TResolver.VisitMatchExpr(MatchExpr: TMatchExpr);
var
  i, j: Integer;
begin
  Visit(MatchExpr.Expr);
  for i := 0 to MatchExpr.IfLimbs.Count-1 do begin
    for j := 0 to MatchExpr.IfLimbs[i].Values.Count-1 do
      Visit(MatchExpr.IfLimbs[i].Values[j]);
    Visit(MatchExpr.IfLimbs[i].Expr);
  end;
  Visit(MatchExpr.ElseLimb);
end;

procedure TResolver.VisitFuncDeclExpr(FuncDeclExpr: TFuncDeclExpr);
begin
  Visit(FuncDeclExpr.FuncDecl);
end;

procedure TResolver.VisitGetExpr(GetExpr: TGetExpr);
begin
  Visit(GetExpr.Instance);
end;

procedure TResolver.VisitSelfExpr(SelfExpr: TSelfExpr);
begin
  if CurrentClassKind = ckNone then
    Errors.Append(SelfExpr.Token, ErrSelfOutsideClass);
  ResolveLocal(SelfExpr.Variable);
end;

procedure TResolver.VisitInheritedExpr(InheritedExpr: TInheritedExpr);
begin
  if CurrentClassKind in [ckNone, ckTrait, ckArray, ckDictionary, ckEnum] then
    Errors.Append(InheritedExpr.Token, ErrInheritedInClass)
  else if CurrentClassKind <> ckSubClass then
    Errors.Append(InheritedExpr.Token, ErrInheritedNotInSubClass);
  ResolveLocal(InheritedExpr.Variable);
end;

procedure TResolver.VisitArrayDeclExpr(ArrayDeclExpr: TArrayDeclExpr);
begin
  Visit(ArrayDeclExpr.ArrayDecl);
end;

procedure TResolver.VisitIndexedExpr(IndexedExpr: TIndexedExpr);
begin
  Visit(IndexedExpr.Variable);
  Visit(IndexedExpr.Index);
end;

procedure TResolver.VisitDictDeclExpr(DictDeclExpr: TDictDeclExpr);
begin
  Visit(DictDeclExpr.DictDecl);
end;

procedure TResolver.VisitInterpolatedExpr(InterpolatedExpr: TInterpolatedExpr);
var
  Expr: TExpr;
begin
  for Expr in InterpolatedExpr.ExprList do
    Visit(Expr);
end;

procedure TResolver.VisitTupleExpr(TupleExpr: TTupleExpr);
var
  Expr: TExpr;
begin
  for Expr in TupleExpr.ExprList do
    Visit(Expr);
end;

procedure TResolver.VisitPrintStmt(PrintStmt: TPrintStmt);
var
  Expr: TExpr;
begin
  for Expr in PrintStmt.ExprList do
    Visit(Expr);
end;

procedure TResolver.VisitAssignStmt(AssignStmt: TAssignStmt);
var
  Symbol: TSymbol;
  Ident: TIdent;
begin
  Visit(AssignStmt.Expr);
  Ident := AssignStmt.Variable.Ident;
  Symbol := Retrieve(Ident);
  if Symbol <> Nil then begin
    if (not Symbol.Mutable) and (not Symbol.isNull) then
      Errors.Append(Ident.Token,
        Format(ErrCannotAssignToConstant, [Ident.Text]))
    else if Symbol.isNull then
      Symbol.isNull := False;
    ResolveLocal(AssignStmt.Variable);
  end;
end;

procedure TResolver.VisitCallExprStmt(CallExprStmt: TCallExprStmt);
begin
  Visit(CallExprStmt.CallExpr);
end;

procedure TResolver.VisitSetStmt(SetStmt: TSetStmt);
begin
  Visit(SetStmt.Instance);
  Visit(SetStmt.Expr);
end;

procedure TResolver.VisitIndexedExprStmt(IndexedExprStmt: TIndexedExprStmt);
begin
  Visit(IndexedExprStmt.IndexedExpr);
  Visit(IndexedExprStmt.Expr);
end;

procedure TResolver.VisitIfStmt(IfStmt: TIfStmt);
var
  i: Integer;
begin
  if Assigned(IfStmt.VarDecl) then begin
    BeginScope;
    Visit(IfStmt.VarDecl);
  end;
  Visit(IfStmt.Condition);
  Visit(IfStmt.ThenPart);
  if Assigned(IfStmt.ElseIfs) then begin
    for i := 0 to IfStmt.ElseIfs.Count-1 do begin;
      Visit(IfStmt.ElseIfs[i]);
      Visit(IfStmt.ElseIfParts[i]);
    end;
  end;
  if IfStmt.ElsePart <> Nil then
    Visit(IfStmt.ElsePart);
  if Assigned(IfStmt.VarDecl) then
    EndScope;
end;

procedure TResolver.VisitWhileStmt(WhileStmt: TWhileStmt);
begin
  if Assigned(WhileStmt.VarDecl) then begin
    BeginScope;
    Visit(WhileStmt.VarDecl);
  end;
  Visit(WhileStmt.Condition);
  Visit(WhileStmt.Block);
  if Assigned(WhileStmt.VarDecl) then
    EndScope;
end;

procedure TResolver.VisitRepeatStmt(RepeatStmt: TRepeatStmt);
begin
  Visit(RepeatStmt.Condition);
  Visit(RepeatStmt.Block);
end;

procedure TResolver.VisitEnsureStmt(EnsureStmt: TEnsureStmt);
begin
  if Assigned(EnsureStmt.VarDecl) then
    Visit(EnsureStmt.VarDecl);
  Visit(EnsureStmt.Condition);
  Visit(EnsureStmt.ElsePart);
end;

procedure TResolver.VisitSwitchStmt(SwitchStmt: TSwitchStmt);
var
  i, j: Integer;
begin
  Visit(SwitchStmt.Expr);
  for i := 0 to SwitchStmt.CaseLimbs.Count-1 do begin
    for j := 0 to SwitchStmt.CaseLimbs[i].Values.Count-1 do
      Visit(SwitchStmt.CaseLimbs[i].Values[j]);
    Visit(SwitchStmt.CaseLimbs[i].Block);
  end;
  Visit(SwitchStmt.ElseLimb);
end;

procedure TResolver.VisitBreakStmt(BreakStmt: TBreakStmt);
begin
  if Assigned(BreakStmt.Condition) then
    Visit(BreakStmt.Condition);
end;

procedure TResolver.VisitContinueStmt(ContinueStmt: TContinueStmt);
begin
  // nothing to do
end;

procedure TResolver.VisitReturnStmt(ReturnStmt: TReturnStmt);
begin
  if CurrentFuncKind in [fkNone, fkInit] then
    Errors.Append(ReturnStmt.Token, ErrReturnFromFunc);
  Visit(ReturnStmt.Expr);
end;

procedure TResolver.VisitUseStmt(UseStmt: TUseStmt);
begin
  // do nothing
end;

procedure TResolver.VisitVarDecl(VarDecl: TVarDecl);
var
  Symbol: TSymbol;
begin
  Declare(VarDecl.Ident, VarDecl.Mutable);
  Visit(VarDecl.Expr);
  Enable(VarDecl.Ident);

  if (VarDecl.Expr is TConstExpr) and
     ((VarDecl.Expr as TConstExpr).Token.Typ = ttNull) then
  begin
    Symbol := Retrieve(VarDecl.Ident);
    Symbol.isNull := True;
  end;
end;

procedure TResolver.VisitVarDecls(VarDecls: TVarDecls);
var
  Decl: TDecl;
begin
  for Decl in VarDecls.List do
    Visit(Decl);
end;

procedure TResolver.VisitFuncDecl(FuncDecl: TFuncDecl);
begin
  if FuncDecl.Ident <> Nil then begin
    Declare(FuncDecl.Ident);
    Enable(FuncDecl.Ident);
  end;
  ResolveFunction(FuncDecl, fkFunc);
end;

procedure TResolver.VisitValDecl(ValDecl: TValDecl);
begin
  Visit(ValDecl.FuncDecl);
end;

procedure TResolver.VisitClassDecl(ClassDecl: TClassDecl);
var
  EnclosingClassKind: TClassKind;
begin
  Declare(ClassDecl.Ident);
  Enable(ClassDecl.Ident);
  EnclosingClassKind := CurrentClassKind;
  CurrentClassKind := ckClass;
  if Assigned(ClassDecl.Parent) then begin
    Visit(ClassDecl.Parent);
    CurrentClassKind := ckSubClass;
    if not ClassList.Contains(ClassDecl.Parent.Ident.Text) then
      with ClassDecl.Parent.Ident do
        Errors.Append(Token, Format(ErrParentNoClass, [Text]));
    BeginScope;
    Scopes.Top.Enter(TSymbol.Create('inherited', Enabled));
  end;
  ClassList.Add(ClassDecl.Ident.Text);  // add this class to list
  ResolveClass(ClassDecl);
  if Assigned(ClassDecl.Parent) then EndScope;
  CurrentClassKind := EnclosingClassKind;
end;

procedure TResolver.VisitExtensionDecl(ExtensionDecl: TExtensionDecl);
var
  Decl: TDecl;
  EnclosingClassKind: TClassKind;
begin
  EnclosingClassKind := CurrentClassKind;
  CurrentClassKind := ckExtension;
  BeginScope;
  Scopes.Top.Enter(TSymbol.Create('self', Enabled));
  for Decl in ExtensionDecl.DeclList do begin
    case Decl.Kind of
      dkFunc: ResolveFunction(Decl as TFuncDecl, fkMethod);
      dkVal: ResolveFunction((Decl as TValDecl).FuncDecl, fkMethod);
    end;
  end;
  EndScope;
  CurrentClassKind := EnclosingClassKind;
end;

procedure TResolver.VisitTraitDecl(TraitDecl: TTraitDecl);
var
  Decl: TDecl;
  EnclosingClassKind: TClassKind;
  Trait: TExpr;
begin
  Declare(TraitDecl.Ident);
  Enable(TraitDecl.Ident);
  EnclosingClassKind := CurrentClassKind;
  CurrentClassKind := ckTrait;
  for Trait in TraitDecl.Traits do
    Visit(Trait);
  BeginScope;
  Scopes.Top.Enter(TSymbol.Create('self', Enabled));
  for Decl in TraitDecl.DeclList do begin
    if Decl.Kind = dkFunc then
      ResolveFunction(Decl as TFuncDecl, fkMethod);
  end;
  EndScope;
  CurrentClassKind := EnclosingClassKind;
end;

procedure TResolver.VisitArrayDecl(ArrayDecl: TArrayDecl);
var
  Expr: TExpr;
  Decl: TDecl;
  EnclosingClassKind: TClassKind;
begin
  for Expr in ArrayDecl.Elements do
    Visit(Expr);
  if Assigned(ArrayDecl.Ident) then begin
    Declare(ArrayDecl.Ident);
    Enable(ArrayDecl.Ident);
    EnclosingClassKind := CurrentClassKind;
    CurrentClassKind := ckArray;
    BeginScope;
    Scopes.Top.Enter(TSymbol.Create('self', Enabled));
    for Decl in ArrayDecl.DeclList do begin
      case Decl.Kind of
        dkFunc: ResolveFunction(Decl as TFuncDecl, fkMethod);
        dkVal: ResolveFunction((Decl as TValDecl).FuncDecl, fkMethod);
      end;
    end;
    EndScope;
    CurrentClassKind := EnclosingClassKind;
  end;
end;

procedure TResolver.VisitDictDecl(DictDecl: TDictDecl);
var
  Element: TKeyValuePair;
  Decl: TDecl;
  EnclosingClassKind: TClassKind;
begin
  for Element in DictDecl.Elements do begin
    Visit(Element.Key);
    Visit(Element.Value);
  end;
  if Assigned(DictDecl.Ident) then begin
    Declare(DictDecl.Ident);
    Enable(DictDecl.Ident);
    EnclosingClassKind := CurrentClassKind;
    CurrentClassKind := ckDictionary;
    BeginScope;
    Scopes.Top.Enter(TSymbol.Create('self', Enabled));
    for Decl in DictDecl.DeclList do begin
      case Decl.Kind of
        dkFunc: ResolveFunction(Decl as TFuncDecl, fkMethod);
        dkVal: ResolveFunction((Decl as TValDecl).FuncDecl, fkMethod);
      end;
    end;
    EndScope;
    CurrentClassKind := EnclosingClassKind;
  end;
end;

procedure TResolver.VisitEnumDecl(EnumDecl: TEnumDecl);
var
  Decl: TDecl;
  EnclosingClassKind: TClassKind;
begin
  Declare(EnumDecl.Ident);
  Enable(EnumDecl.Ident);
  EnclosingClassKind := CurrentClassKind;
  CurrentClassKind := ckEnum;
  BeginScope;
  Scopes.Top.Enter(TSymbol.Create('self', Enabled));
  for Decl in EnumDecl.DeclList do begin
    case Decl.Kind of
      dkFunc: ResolveFunction(Decl as TFuncDecl, fkMethod);
      dkVal: ResolveFunction((Decl as TValDecl).FuncDecl, fkMethod);
    end;
  end;
  EndScope;
  CurrentClassKind := EnclosingClassKind;
end;

procedure TResolver.VisitBlock(Block: TBlock);
var
  Node: TNode;
begin
  BeginScope;
  for Node in Block.Nodes do
    Visit(Node);
  EndScope;
end;

procedure TResolver.VisitProduct(Product: TProduct);
var
  Node: TNode;
begin
  for Node in Product.Nodes do
    Visit(Node);
end;

procedure TResolver.ResolveLocal(Variable: TVariable);
var
  i: LongInt;
begin
  for i := Scopes.Count-1 downto 0 do
    if Scopes[i].Contains(Variable.Ident.Text) then begin
      Variable.Distance := Scopes.Count-1-i;
      Exit;
    end;
end;

procedure TResolver.ResolveFunction(Func: TFuncDecl; FuncKind: TFuncKind);
var
  EnclosingFuncKind: TFuncKind;
  i: Integer;
begin
  EnclosingFuncKind := CurrentFuncKind;
  CurrentFuncKind := FuncKind;
  BeginScope;
  for i := 0 to Func.Params.Count-1 do begin
    Declare(Func.Params[i].Ident);
    Enable(Func.Params[i].Ident);
  end;
  Visit(Func.Body);
  EndScope;
  CurrentFuncKind := EnclosingFuncKind;
end;

procedure TResolver.ResolveClass(ClassDecl: TClassDecl);
var
  Decl: TDecl;
  FuncKind: TFuncKind;
  Trait: TExpr;
begin
  for Trait in ClassDecl.Traits do
    Visit(Trait);
  BeginScope;
  Scopes.Top.Enter(TSymbol.Create('self', Enabled));
  for Decl in ClassDecl.DeclList do begin
    case Decl.Kind of
      dkFunc: begin
        FuncKind := fkMethod;
        if Decl.Ident.Text = 'init' then
          FuncKind := fkInit;
        ResolveFunction(Decl as TFuncDecl, FuncKind);
      end;
      dkVal: ResolveFunction((Decl as TValDecl).FuncDecl, fkMethod);
      dkVar: Visit(Decl as TVarDecl);
    end;
  end;
  EndScope;
end;

procedure TResolver.BeginScope;
begin
  Scopes.Push(TScope.Create(CurrentScope));
  CurrentScope := Scopes.Top;
end;

procedure TResolver.EndScope;
begin
  CurrentScope := Scopes.Top.Enclosing;
  Scopes.Pop;
end;

procedure TResolver.Declare(Ident: TIdent; const isMutable: Boolean);
begin
  if CurrentScope.Contains(Ident.Text) then
    Errors.Append(Ident.Token, Format(
      ErrDuplicateIdInScope, [Ident.Text]));
  CurrentScope.Enter(TSymbol.Create(Ident.Text, Declared, isMutable));
end;

procedure TResolver.Enable(Ident: TIdent);
var
  Symbol: TSymbol;
begin
  Symbol := Retrieve(Ident);
  if Symbol <> Nil then
    Symbol.Status := Enabled;
end;

function TResolver.Retrieve(Ident: TIdent): TSymbol;
begin
  Result := CurrentScope.Lookup(Ident.Text);

  if Result = Nil then
    Errors.Append(Ident.Token, Format(ErrUndeclaredVar, [Ident.Text]));
end;

procedure TResolver.EnableStandardFunctions;
begin
  with GlobalScope do begin
    Enter(TSymbol.Create('assigned', Enabled, False));
    Enter(TSymbol.Create('abs', Enabled, False));
    Enter(TSymbol.Create('arctan', Enabled, False));
    Enter(TSymbol.Create('chr', Enabled, False));
    Enter(TSymbol.Create('ceil', Enabled, False));
    Enter(TSymbol.Create('cos', Enabled, False));
    Enter(TSymbol.Create('date', Enabled, False));
    Enter(TSymbol.Create('exp', Enabled, False));
    Enter(TSymbol.Create('floor', Enabled, False));
    Enter(TSymbol.Create('frac', Enabled, False));
    Enter(TSymbol.Create('length', Enabled, False));
    Enter(TSymbol.Create('ln', Enabled, False));
    Enter(TSymbol.Create('milliseconds', Enabled, False));
    Enter(TSymbol.Create('now', Enabled, False));
    Enter(TSymbol.Create('ord', Enabled, False));
    Enter(TSymbol.Create('pi', Enabled, False));
    Enter(TSymbol.Create('random', Enabled, False));
    Enter(TSymbol.Create('randomLimit', Enabled, False));
    Enter(TSymbol.Create('readln', Enabled, False));
    Enter(TSymbol.Create('round', Enabled, False));
    Enter(TSymbol.Create('sin', Enabled, False));
    Enter(TSymbol.Create('sqr', Enabled, False));
    Enter(TSymbol.Create('sqrt', Enabled, False));
    Enter(TSymbol.Create('time', Enabled, False));
    Enter(TSymbol.Create('today', Enabled, False));
    Enter(TSymbol.Create('toNum', Enabled, False));
    Enter(TSymbol.Create('toStr', Enabled, False));
    Enter(TSymbol.Create('trunc', Enabled, False));
    Enter(TSymbol.Create('Array', Enabled, False));
    Enter(TSymbol.Create('Dictionary', Enabled, False));
    Enter(TSymbol.Create('listAdd', Enabled, False));
    Enter(TSymbol.Create('listContains', Enabled, False));
    Enter(TSymbol.Create('listDelete', Enabled, False));
    Enter(TSymbol.Create('listIndexOf', Enabled, False));
    Enter(TSymbol.Create('listInsert', Enabled, False));
    Enter(TSymbol.Create('listRetrieve', Enabled, False));
    Enter(TSymbol.Create('listFirst', Enabled, False));
    Enter(TSymbol.Create('listLast', Enabled, False));
    Enter(TSymbol.Create('listKeys', Enabled, False));
    Enter(TSymbol.Create('listValues', Enabled, False));
  end;
end;

{ TSymbol }

constructor TSymbol.Create(const AName: String; const AStatus: TStatus;
  const AMutable: Boolean);
begin
  Name := AName;
  Status := AStatus;
  Mutable := AMutable;
  isNull := False;
end;

{ TScope }

constructor TScope.Create(AEnclosing: TScope);
begin
  inherited Create;
  FEnclosing := AEnclosing;
end;

procedure TScope.Enter(ASymbol: TSymbol);
begin
  Self[ASymbol.Name] := ASymbol;
end;

function TScope.Lookup(AName: String): TSymbol;
var
  i: Integer;
begin
  i := IndexOf(AName);
  if i >= 0 then
    Result := Data[i]
  else if FEnclosing <> Nil then
    Result := FEnclosing.Lookup(AName)
  else
    Result := Nil;
end;

end.

