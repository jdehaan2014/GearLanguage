unit uPrinter;

{ This unit contains the visitor functions that print the AST nodes.

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
  Classes, SysUtils, Variants, uAST, uVisitor, uToken;

type

  { TPrinter }

  TPrinter = class(TVisitor)
    private
      const Increase = 2;
      var
        Indent: string;
        Tree: TProduct;
      procedure IncIndent;
      procedure DecIndent;
    public
      constructor Create(ATree: TProduct);
      procedure Print;
    published
      procedure VisitIdent(Ident: TIdent);
      procedure VisitNode(Node: TNode);
      // Expressions
      procedure VisitBinaryExpr(BinaryExpr: TBinaryExpr);
      procedure VisitConstExpr(ConstExpr: TConstExpr);
      procedure VisitUnaryExpr(UnaryExpr: TUnaryExpr);
      procedure VisitCallExpr(CallExpr: TCallExpr);
      procedure VisitVariable(Variable: TVariable);
      procedure VisitIfExpr(IfExpr: TIfExpr);
      procedure VisitMatchExpr(MatchExpr: TMatchExpr);
      procedure VisitTupleExpr(TupleExpr: TTupleExpr);
      procedure VisitGetExpr(GetExpr: TGetExpr);
      procedure VisitSelfExpr(SelfExpr: TSelfExpr);
      procedure VisitInheritedExpr(InheritedExpr: TInheritedExpr);
      procedure VisitArrayDeclExpr(ArrayDeclExpr: TArrayDeclExpr);
      procedure VisitIndexedExpr(IndexedExpr: TIndexedExpr);
      procedure VisitDictDecExprl(DictDeclExpr: TDictDeclExpr);
      procedure VisitInterpolatedExpr(InterpolatedExpr: TInterpolatedExpr);
      // Statements
      procedure VisitPrintStmt(Node: TPrintStmt);
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
      procedure VisitFuncDeclExpr(FuncDeclExpr: TFuncDeclExpr);
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

procedure TPrinter.IncIndent;
begin
  Indent := StringOfChar(' ', Length(Indent) + Increase);
end;

procedure TPrinter.DecIndent;
begin
  Indent := StringOfChar(' ', Length(Indent) - Increase);
end;

constructor TPrinter.Create(ATree: TProduct);
begin
  Indent := '  ';
  Tree := ATree;
end;

procedure TPrinter.Print;
begin
  VisitProc(Tree);
  Writeln;
end;

procedure TPrinter.VisitNode(Node: TNode);
begin
  Writeln(Indent + String(Node.ClassName).Substring(1));
end;

procedure TPrinter.VisitIdent(Ident: TIdent);
begin
  IncIndent;
  WriteLn(Indent + 'Ident: ' + Ident.Text);
  DecIndent;
end;

procedure TPrinter.VisitBinaryExpr(BinaryExpr: TBinaryExpr);
begin
  IncIndent;
  WriteLn(Indent, '(', BinaryExpr.Op.Typ.toString, ')');
  VisitProc(BinaryExpr.Left);
  VisitProc(BinaryExpr.Right);
  DecIndent;
end;

procedure TPrinter.VisitConstExpr(ConstExpr: TConstExpr);
begin
  IncIndent;
  WriteLn(Indent, VarToStrDef(ConstExpr.Value, 'Null'));
  DecIndent;
end;

procedure TPrinter.VisitUnaryExpr(UnaryExpr: TUnaryExpr);
begin
  IncIndent;
  WriteLn(Indent, '(', UnaryExpr.Op.Typ.toString, ')');
  VisitProc(UnaryExpr.Expr);
  DecIndent;
end;

procedure TPrinter.VisitCallExpr(CallExpr: TCallExpr);
var
  i: Integer;
begin
  IncIndent;
  VisitNode(CallExpr);
  VisitProc(CallExpr.Signature);
  IncIndent;
  Writeln(Indent, 'Arguments:');
  for i := 0 to CallExpr.Args.Count-1 do
    VisitProc(CallExpr.Args[i].Expr);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitVariable(Variable: TVariable);
begin
  IncIndent;
  WriteLn(Indent, 'Var: ', Variable.Ident.Text);
  DecIndent;
end;

procedure TPrinter.VisitIfExpr(IfExpr: TIfExpr);
begin
  IncIndent;
  VisitNode(IfExpr);
  VisitProc(IfExpr.Condition);
  IncIndent;
  Writeln(Indent, 'True:');
  VisitProc(IfExpr.TrueExpr);
  Writeln(Indent, 'False:');
  VisitProc(IfExpr.FalseExpr);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitMatchExpr(MatchExpr: TMatchExpr);
var
  Key: TExpr;
begin
  IncIndent;
  VisitNode(MatchExpr);
  VisitProc(MatchExpr.Expr);
  IncIndent;
  Writeln(Indent, 'If Limbs:');
  for Key in MatchExpr.IfLimbs.Keys do begin
    IncIndent;
    WriteLn(Indent, 'IF:');
    VisitProc(Key);
    VisitProc(MatchExpr.IfLimbs[Key]);
    DecIndent;
  end;
  Writeln(Indent, 'Else:');
  VisitProc(MatchExpr.ElseLimb);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitTupleExpr(TupleExpr: TTupleExpr);
var
  Expr: TExpr;
begin
  IncIndent;
  VisitNode(TupleExpr);
  for Expr in TupleExpr.ExprList do
    VisitProc(Expr);
  DecIndent;
end;

procedure TPrinter.VisitGetExpr(GetExpr: TGetExpr);
begin
  IncIndent;
  VisitNode(GetExpr);
  VisitProc(GetExpr.Instance);
  VisitProc(GetExpr.Member);
  DecIndent;
end;

procedure TPrinter.VisitSelfExpr(SelfExpr: TSelfExpr);
begin
  IncIndent;
  VisitNode(SelfExpr);
  VisitProc(SelfExpr.Variable);
  DecIndent;
end;

procedure TPrinter.VisitInheritedExpr(InheritedExpr: TInheritedExpr);
begin
  IncIndent;
  VisitNode(InheritedExpr);
  VisitProc(InheritedExpr.Variable);
  VisitProc(InheritedExpr.Method);
  DecIndent;
end;

procedure TPrinter.VisitArrayDeclExpr(ArrayDeclExpr: TArrayDeclExpr);
begin
  IncIndent;
  VisitNode(ArrayDeclExpr);
  VisitProc(ArrayDeclExpr.ArrayDecl);
  DecIndent;
end;

procedure TPrinter.VisitIndexedExpr(IndexedExpr: TIndexedExpr);
begin
  IncIndent;
  VisitNode(IndexedExpr);
  VisitProc(IndexedExpr.Variable);
  Writeln(Indent, 'Index: ');
  IncIndent;
  VisitProc(IndexedExpr.Index);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitInterpolatedExpr(InterpolatedExpr: TInterpolatedExpr);
var
  Expr: TExpr;
begin
  IncIndent;
  VisitNode(InterpolatedExpr);
  for Expr in InterpolatedExpr.ExprList do
    VisitProc(Expr);
  DecIndent;
end;


procedure TPrinter.VisitPrintStmt(Node: TPrintStmt);
var
  Expr: TExpr;
begin
  IncIndent;
  VisitNode(Node);
  for Expr in Node.ExprList do
    VisitProc(Expr);
  DecIndent;
end;

procedure TPrinter.VisitAssignStmt(AssignStmt: TAssignStmt);
begin
  IncIndent;
  VisitNode(AssignStmt);
  WriteLn(Indent, '(', AssignStmt.Op.Typ.toString, ')');
  VisitProc(AssignStmt.Variable);
  VisitProc(AssignStmt.Expr);
  DecIndent;
end;

procedure TPrinter.VisitCallExprStmt(CallExprStmt: TCallExprStmt);
begin
  IncIndent;
  VisitNode(CallExprStmt);
  VisitProc(CallExprStmt.CallExpr);
  DecIndent;
end;

procedure TPrinter.VisitSetStmt(SetStmt: TSetStmt);
begin
  IncIndent;
  VisitNode(SetStmt);
  WriteLn(Indent, '(', SetStmt.Op.Typ.toString, ')');
  VisitProc(SetStmt.GetExpr);
  VisitProc(SetStmt.Expr);
  DecIndent;
end;

procedure TPrinter.VisitIndexedExprStmt(IndexedExprStmt: TIndexedExprStmt);
begin
  IncIndent;
  VisitNode(IndexedExprStmt);
  VisitProc(IndexedExprStmt.IndexedExpr);
  VisitProc(IndexedExprStmt.Expr);
  DecIndent;
end;

procedure TPrinter.VisitIfStmt(IfStmt: TIfStmt);
var
  i: Integer;
begin
  IncIndent;
  VisitNode(IfStmt);
  if Assigned(IfStmt.VarDecl) then
    VisitProc(IfStmt.VarDecl);
  VisitProc(IfStmt.Condition);
  IncIndent;
  WriteLn(Indent, 'ThenPart:');
  VisitProc(IfStmt.ThenPart);
  if Assigned(IfStmt.ElseIfs) then begin
    WriteLn(Indent, 'ElseIfParts:');
    for i := 0 to IfStmt.ElseIfs.Count-1 do begin;
      VisitProc(IfStmt.ElseIfs[i]);
      VisitProc(IfStmt.ElseIfParts[i]);
    end;
  end;
  if Assigned(IfStmt.ElsePart) then begin
    WriteLn(Indent, 'ElsePart:');
    VisitProc(IfStmt.ElsePart);
  end;
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitWhileStmt(WhileStmt: TWhileStmt);
begin
  IncIndent;
  VisitNode(WhileStmt);
  if Assigned(WhileStmt.VarDecl) then
    VisitProc(WhileStmt.VarDecl);
  VisitProc(WhileStmt.Condition);
  IncIndent;
  WriteLn(Indent, 'Loop:');
  VisitProc(WhileStmt.Block);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitRepeatStmt(RepeatStmt: TRepeatStmt);
begin
  IncIndent;
  VisitNode(RepeatStmt);
  VisitProc(RepeatStmt.Condition);
  IncIndent;
  WriteLn(Indent, 'Loop:');
  VisitProc(RepeatStmt.Block);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitEnsureStmt(EnsureStmt: TEnsureStmt);
begin
  IncIndent;
  VisitNode(EnsureStmt);
  if Assigned(EnsureStmt.VarDecl) then
    VisitProc(EnsureStmt.VarDecl);
  VisitProc(EnsureStmt.Condition);
  IncIndent;
  WriteLn(Indent, 'ElsePart:');
  VisitProc(EnsureStmt.ElsePart);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitSwitchStmt(SwitchStmt: TSwitchStmt);
var
  Key: TCaseItem;
begin
  IncIndent;
  VisitNode(SwitchStmt);
  VisitProc(SwitchStmt.Expr);
  IncIndent;
  Writeln(Indent, 'Case Limbs:');
  for Key in SwitchStmt.CaseLimbs.Keys do begin
    IncIndent;
    WriteLn(Indent, 'Case:');
    Write(Indent, '  Is object: ', Key.isObj);
    VisitProc(Key.Expr);
    VisitProc(SwitchStmt.CaseLimbs[Key]);
    DecIndent;
  end;
  Writeln(Indent, 'Else:');
  VisitProc(SwitchStmt.ElseLimb);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitBreakStmt(BreakStmt: TBreakStmt);
begin
  IncIndent;
  VisitNode(BreakStmt);
  if Assigned(BreakStmt.Condition) then
    VisitProc(BreakStmt.Condition);
  DecIndent;
end;

procedure TPrinter.VisitContinueStmt(ContinueStmt: TContinueStmt);
begin
  IncIndent;
  VisitNode(ContinueStmt);
  DecIndent;
end;

procedure TPrinter.VisitReturnStmt(ReturnStmt: TReturnStmt);
begin
  IncIndent;
  VisitNode(ReturnStmt);
  VisitProc(ReturnStmt.Expr);
  DecIndent;
end;

procedure TPrinter.VisitUseStmt(UseStmt: TUseStmt);
begin
  IncIndent;
  VisitNode(UseStmt);
  WriteLn(Indent + 'Filename: ' + UseStmt.FileName);
  DecIndent;
end;


procedure TPrinter.VisitVarDecl(VarDecl: TVarDecl);
begin
  IncIndent;
  VisitNode(VarDecl);
  VisitProc(VarDecl.Ident);
  VisitProc(VarDecl.Expr);
  DecIndent;
end;

procedure TPrinter.VisitVarDecls(VarDecls: TVarDecls);
var
  Decl: TDecl;
begin
  for Decl in VarDecls.List do
    VisitProc(Decl);
end;

procedure TPrinter.VisitFuncDecl(FuncDecl: TFuncDecl);
var
  i: Integer;
begin
  IncIndent;
  VisitNode(FuncDecl);  // Print FuncDecl
  if Assigned(FuncDecl.Ident) then
    VisitProc(FuncDecl.Ident);
  IncIndent;
  WriteLn(Indent, 'Parameters:');
  for i := 0 to FuncDecl.Params.Count-1 do begin
    if Assigned(FuncDecl.Params[i].ExtIdent) then
      Write(Indent, 'ExtIdent: ', FuncDecl.Params[i].ExtIdent.Text);
    VisitProc(FuncDecl.Params[i].Ident);
  end;
  DecIndent;
  VisitProc(FuncDecl.Body);
  DecIndent;
end;

procedure TPrinter.VisitFuncDeclExpr(FuncDeclExpr: TFuncDeclExpr);
begin
  IncIndent;
  VisitNode(FuncDeclExpr);
  VisitProc(FuncDeclExpr.FuncDecl);
  DecIndent;
end;

procedure TPrinter.VisitClassDecl(ClassDecl: TClassDecl);
var
  Decl: TDecl;
  Trait: TExpr;
begin
  IncIndent;
  VisitNode(ClassDecl);
  VisitProc(ClassDecl.Ident);
  if Assigned(ClassDecl.Parent) then begin
    Write(indent, '  Parent: ');
    VisitProc(ClassDecl.Parent.Ident);
  end;
  WriteLn(Indent + '  Traits:');
  for Trait in ClassDecl.Traits do
    VisitProc(Trait);
  WriteLn(Indent + '  Declarations:');
  for Decl in ClassDecl.DeclList do
    VisitProc(Decl);
  Writeln(Indent + '  Static:');
  for Decl in ClassDecl.StaticList do
    VisitProc(Decl);
  DecIndent;
end;

procedure TPrinter.VisitValDecl(ValDecl: TValDecl);
begin
  IncIndent;
  VisitNode(ValDecl);
  VisitProc(ValDecl.Ident);
  VisitProc(ValDecl.FuncDecl);
  DecIndent;
end;

procedure TPrinter.VisitExtensionDecl(ExtensionDecl: TExtensionDecl);
var
  Decl: TDecl;
begin
  IncIndent;
  VisitNode(ExtensionDecl);
  VisitProc(ExtensionDecl.Ident);
  for Decl in ExtensionDecl.DeclList do
    VisitProc(Decl);
  DecIndent;
end;

procedure TPrinter.VisitTraitDecl(TraitDecl: TTraitDecl);
var
  Decl: TDecl;
  Trait: TExpr;
begin
  IncIndent;
  VisitNode(TraitDecl);
  VisitProc(TraitDecl.Ident);
  WriteLn(Indent + 'Traits:');
  for Trait in TraitDecl.Traits do
    VisitProc(Trait);
  WriteLn(Indent + 'Declarations:');
  for Decl in TraitDecl.DeclList do
    VisitProc(Decl);
  DecIndent;
end;

procedure TPrinter.VisitArrayDecl(ArrayDecl: TArrayDecl);
var
  Expr: TExpr;
  Decl: TDecl;
begin
  IncIndent;
  VisitNode(ArrayDecl);
  if Assigned(ArrayDecl.Ident) then
    VisitProc(ArrayDecl.Ident);
  WriteLn(Indent + 'Elements:');
  IncIndent;
  for Expr in ArrayDecl.Elements do
    VisitProc(Expr);
  for Decl in ArrayDecl.DeclList do
    VisitProc(Decl);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitDictDecl(DictDecl: TDictDecl);
var
  Key: TExpr;
  Decl: TDecl;
begin
  IncIndent;
  VisitNode(DictDecl);
  VisitProc(DictDecl.Ident);
  IncIndent;
  WriteLn(Indent + 'Elements:');
  IncIndent;
  for Key in DictDecl.KeyValueList.Keys do begin
    Write(Indent + 'Key: '); VisitProc(Key);
    Write(Indent + 'Val: '); VisitProc(DictDecl.KeyValueList[Key]);
  end;
  DecIndent;
  WriteLn(Indent + 'Declarations:');
  for Decl in DictDecl.DeclList do
    VisitProc(Decl);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitDictDecExprl(DictDeclExpr: TDictDeclExpr);
begin
  IncIndent;
  VisitNode(DictDeclExpr);
  VisitProc(DictDeclExpr.DictDecl);
  DecIndent;
end;

procedure TPrinter.VisitEnumDecl(EnumDecl: TEnumDecl);
var
  Key: String;
  Decl: TDecl;
begin
  IncIndent;
  VisitNode(EnumDecl);
  VisitProc(EnumDecl.Ident);
  IncIndent;
  IncIndent;
  WriteLn('Elements:');
  for Key in EnumDecl.Elements.Keys do
    WriteLn(Indent, 'Name: ', Key, ', Value: ', EnumDecl.Elements[Key].Value);
  DecIndent;
  WriteLn(Indent + 'Declarations:');
  for Decl in EnumDecl.DeclList do
    VisitProc(Decl);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitBlock(Block: TBlock);
var
  Node: TNode;
begin
  IncIndent;
  VisitNode(Block);
  for Node in Block.Nodes do
    VisitProc(Node);
  DecIndent;
end;

procedure TPrinter.VisitProduct(Product: TProduct);
var
  Node: TNode;
begin
  IncIndent;
  VisitNode(Product);
  for Node in Product.Nodes do
    VisitProc(Node);
  DecIndent;
end;


end.

