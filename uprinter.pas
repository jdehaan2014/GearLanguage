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
      procedure VisitNode(Node: TNode);
      procedure VisitIdent(Ident: TIdent);
      //expressions
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
      procedure VisitSwitchStmt(Node: TSwitchStmt);
      procedure VisitBreakStmt(BreakStmt: TBreakStmt);
      procedure VisitContinueStmt(ContinueStmt: TContinueStmt);
      procedure VisitReturnStmt(ReturnStmt: TReturnStmt);
      procedure VisitUseStmt(UseStmt: TUseStmt);
      //declarations
      procedure VisitVarDecl(VarDecl: TVarDecl);
      procedure VisitFuncDecl(FuncDecl: TFuncDecl);
      procedure VisitValDecl(ValDecl: TValDecl);
      procedure VisitClassDecl(ClassDecl: TClassDecl);
      procedure VisitExtensionDecl(ExtensionDecl: TExtensionDecl);
      procedure VisitTraitDecl(TraitDecl: TTraitDecl);
      procedure VisitArrayDecl(ArrayDecl: TArrayDecl);
      procedure VisitDictDecl(DictDecl: TDictDecl);
      procedure VisitEnumDecl(EnumDecl: TEnumDecl);
      //block
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
  Visit(Tree);
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
  Visit(BinaryExpr.Left);
  Visit(BinaryExpr.Right);
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
  Visit(UnaryExpr.Expr);
  DecIndent;
end;

procedure TPrinter.VisitVariable(Variable: TVariable);
begin
  IncIndent;
  WriteLn(Indent, 'Var: ', Variable.Ident.Text);
  DecIndent;
end;

procedure TPrinter.VisitCallExpr(CallExpr: TCallExpr);
var
  i: Integer;
begin
  IncIndent;
  VisitNode(CallExpr);
  Visit(CallExpr.Callee);
  IncIndent;
  Writeln(Indent, 'Arguments:');
  for i := 0 to CallExpr.Args.Count-1 do
    Visit(CallExpr.Args[i].Expr);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitIfExpr(IfExpr: TIfExpr);
begin
  IncIndent;
  VisitNode(IfExpr);
  Visit(IfExpr.Condition);
  IncIndent;
  Writeln(Indent, 'True:');
  Visit(IfExpr.TrueExpr);
  Writeln(Indent, 'False:');
  Visit(IfExpr.FalseExpr);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitMatchExpr(MatchExpr: TMatchExpr);
var
  i, j: integer;
begin
  IncIndent;
  VisitNode(MatchExpr);
  Visit(MatchExpr.Expr);
  IncIndent;
  Writeln(Indent, 'If Limbs:');
  for i := 0 to MatchExpr.IfLimbs.Count-1 do begin
    IncIndent;
    WriteLn(Indent, 'IF:');
    for j := 0 to MatchExpr.IfLimbs[i].Values.Count-1 do
      Visit(MatchExpr.IfLimbs[i].Values[j]);
    Visit(MatchExpr.IfLimbs[i].Expr);
    DecIndent;
  end;
  Writeln(Indent, 'Else:');
  Visit(MatchExpr.ElseLimb);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitFuncDeclExpr(FuncDeclExpr: TFuncDeclExpr);
begin
  IncIndent;
  VisitNode(FuncDeclExpr);
  Visit(FuncDeclExpr.FuncDecl);
  DecIndent;
end;

procedure TPrinter.VisitGetExpr(GetExpr: TGetExpr);
begin
  IncIndent;
  VisitNode(GetExpr);
  Visit(GetExpr.Instance);
  Visit(GetExpr.Ident);
  DecIndent;
end;

procedure TPrinter.VisitSelfExpr(SelfExpr: TSelfExpr);
begin
  IncIndent;
  VisitNode(SelfExpr);
  Visit(SelfExpr.Variable);
  DecIndent;
end;

procedure TPrinter.VisitInheritedExpr(InheritedExpr: TInheritedExpr);
begin
  IncIndent;
  VisitNode(InheritedExpr);
  Visit(InheritedExpr.Variable);
  Visit(InheritedExpr.Method);
  DecIndent;
end;

procedure TPrinter.VisitArrayDeclExpr(ArrayDeclExpr: TArrayDeclExpr);
begin
  IncIndent;
  VisitNode(ArrayDeclExpr);
  Visit(ArrayDeclExpr.ArrayDecl);
  DecIndent;
end;

procedure TPrinter.VisitIndexedExpr(IndexedExpr: TIndexedExpr);
begin
  IncIndent;
  VisitNode(IndexedExpr);
  Visit(IndexedExpr.Variable);
  Writeln(Indent, 'Index: ');
  IncIndent;
  Visit(IndexedExpr.Index);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitDictDeclExpr(DictDeclExpr: TDictDeclExpr);
begin
  IncIndent;
  VisitNode(DictDeclExpr);
  Visit(DictDeclExpr.DictDecl);
  DecIndent;
end;

procedure TPrinter.VisitInterpolatedExpr(InterpolatedExpr: TInterpolatedExpr);
var
  Expr: TExpr;
begin
  IncIndent;
  VisitNode(InterpolatedExpr);
  for Expr in InterpolatedExpr.ExprList do
    Visit(Expr);
  DecIndent;
end;

procedure TPrinter.VisitPrintStmt(PrintStmt: TPrintStmt);
var
  Expr: TExpr;
begin
  IncIndent;
  VisitNode(PrintStmt);
  for Expr in PrintStmt.ExprList do
    Visit(Expr);
  DecIndent;
end;

procedure TPrinter.VisitAssignStmt(AssignStmt: TAssignStmt);
begin
  IncIndent;
  VisitNode(AssignStmt);
  WriteLn(Indent, '(', AssignStmt.Op.Typ.toString, ')');
  Visit(AssignStmt.Variable);
  Visit(AssignStmt.Expr);
  DecIndent;
end;

procedure TPrinter.VisitCallExprStmt(CallExprStmt: TCallExprStmt);
begin
  IncIndent;
  VisitNode(CallExprStmt);
  Visit(CallExprStmt.CallExpr);
  DecIndent;
end;

procedure TPrinter.VisitSetStmt(SetStmt: TSetStmt);
begin
  IncIndent;
  VisitNode(SetStmt);
  Visit(SetStmt.Instance);
  Visit(SetStmt.Ident);
  Visit(SetStmt.Expr);
  DecIndent;
end;

procedure TPrinter.VisitIndexedExprStmt(IndexedExprStmt: TIndexedExprStmt);
begin
  IncIndent;
  VisitNode(IndexedExprStmt);
  Visit(IndexedExprStmt.IndexedExpr);
  Visit(IndexedExprStmt.Expr);
  DecIndent;
end;

procedure TPrinter.VisitIfStmt(IfStmt: TIfStmt);
var
  i: Integer;
begin
  IncIndent;
  VisitNode(IfStmt);
  if Assigned(IfStmt.VarDecl) then
    Visit(IfStmt.VarDecl);
  Visit(IfStmt.Condition);
  IncIndent;
  WriteLn(Indent, 'ThenPart:');
  Visit(IfStmt.ThenPart);
  if Assigned(IfStmt.ElseIfs) then begin
    WriteLn(Indent, 'IfElseParts:');
    for i := 0 to IfStmt.ElseIfs.Count-1 do begin;
      Visit(IfStmt.ElseIfs[i]);
      Visit(IfStmt.ElseIfParts[i]);
    end;
  end;
  if Assigned(IfStmt.ElsePart) then begin
    WriteLn(Indent, 'ElsePart:');
    Visit(IfStmt.ElsePart);
  end;
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitWhileStmt(WhileStmt: TWhileStmt);
begin
  IncIndent;
  VisitNode(WhileStmt);
  if Assigned(WhileStmt.VarDecl) then
    Visit(WhileStmt.VarDecl);
  Visit(WhileStmt.Condition);
  IncIndent;
  WriteLn(Indent, 'Loop:');
  Visit(WhileStmt.Block);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitRepeatStmt(RepeatStmt: TRepeatStmt);
begin
  IncIndent;
  VisitNode(RepeatStmt);
  Visit(RepeatStmt.Condition);
  IncIndent;
  WriteLn(Indent, 'Loop:');
  Visit(RepeatStmt.Block);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitEnsureStmt(EnsureStmt: TEnsureStmt);
begin
  IncIndent;
  VisitNode(EnsureStmt);
  if Assigned(EnsureStmt.VarDecl) then
    Visit(EnsureStmt.VarDecl);
  Visit(EnsureStmt.Condition);
  IncIndent;
  WriteLn(Indent, 'ElsePart:');
  Visit(EnsureStmt.ElsePart);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitSwitchStmt(Node: TSwitchStmt);
var
  i,j: integer;
begin
  IncIndent;
  VisitNode(Node);
  Visit(Node.Expr);
  IncIndent;
  Writeln(Indent, 'Case Limbs:');
  for i := 0 to Node.CaseLimbs.Count-1 do begin
    IncIndent;
    WriteLn(Indent, 'CASE:');
    for j := 0 to Node.CaseLimbs[i].Values.Count-1 do
      Visit(Node.CaseLimbs[i].Values[j]);
    Visit(Node.CaseLimbs[i].Block);
    DecIndent;
  end;
  Writeln(Indent, 'Else:');
  Visit(Node.ElseLimb);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitBreakStmt(BreakStmt: TBreakStmt);
begin
  IncIndent;
  VisitNode(BreakStmt);
  if Assigned(BreakStmt.Condition) then
    Visit(BreakStmt.Condition);
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
  Visit(ReturnStmt.Expr);
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
  Visit(VarDecl.Ident);
  Visit(VarDecl.Expr);
  DecIndent;
end;

procedure TPrinter.VisitFuncDecl(FuncDecl: TFuncDecl);
var
  i: Integer;
begin
  IncIndent;
  VisitNode(FuncDecl);  // Print FuncDecl
  if Assigned(FuncDecl.Ident) then
    Visit(FuncDecl.Ident);
  IncIndent;
  WriteLn(Indent, 'Parameters:');
  for i := 0 to FuncDecl.Params.Count-1 do begin
    if Assigned(FuncDecl.Params[i].ExtIdent) then
      Write(Indent, 'ExtIdent: ', FuncDecl.Params[i].ExtIdent.Text);
    Visit(FuncDecl.Params[i].Ident);
  end;
  DecIndent;
  Visit(FuncDecl.Body);
  DecIndent;
end;

procedure TPrinter.VisitValDecl(ValDecl: TValDecl);
begin
  IncIndent;
  VisitNode(ValDecl);
  Visit(ValDecl.Ident);
  Visit(ValDecl.FuncDecl);
  DecIndent;
end;

procedure TPrinter.VisitClassDecl(ClassDecl: TClassDecl);
var
  Decl: TDecl;
  Trait: TExpr;
begin
  IncIndent;
  VisitNode(ClassDecl);
  Visit(ClassDecl.Ident);
  if Assigned(ClassDecl.Parent) then
    Visit(ClassDecl.Parent);
  WriteLn(Indent + 'Traits:');
  for Trait in ClassDecl.Traits do
    Visit(Trait);
  WriteLn(Indent + 'Declarations:');
  for Decl in ClassDecl.DeclList do
    Visit(Decl);
  DecIndent;
end;

procedure TPrinter.VisitExtensionDecl(ExtensionDecl: TExtensionDecl);
var
  Decl: TDecl;
begin
  IncIndent;
  VisitNode(ExtensionDecl);
  Visit(ExtensionDecl.Ident);
  for Decl in ExtensionDecl.DeclList do
    Visit(Decl);
  DecIndent;
end;

procedure TPrinter.VisitTraitDecl(TraitDecl: TTraitDecl);
var
  Decl: TDecl;
  Trait: TExpr;
begin
  IncIndent;
  VisitNode(TraitDecl);
  Visit(TraitDecl.Ident);
  WriteLn(Indent + 'Traits:');
  for Trait in TraitDecl.Traits do
    Visit(Trait);
  WriteLn(Indent + 'Declarations:');
  for Decl in TraitDecl.DeclList do
    Visit(Decl);
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
    Visit(ArrayDecl.Ident);
  WriteLn(Indent + 'Elements:');
  IncIndent;
  for Expr in ArrayDecl.Elements do
    Visit(Expr);
  for Decl in ArrayDecl.DeclList do
    Visit(Decl);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitDictDecl(DictDecl: TDictDecl);
var
  Element: TKeyValuePair;
  Decl: TDecl;
begin
  IncIndent;
  VisitNode(DictDecl);
  Visit(DictDecl.Ident);
  WriteLn(Indent + 'Elements:');
  IncIndent;
  for Element in DictDecl.Elements do begin
    Write(Indent + 'Key: '); Visit(Element.Key);
    Write(Indent + 'Val: '); Visit(Element.Value);
  end;
  WriteLn(Indent + 'Declarations:');
  for Decl in DictDecl.DeclList do
    Visit(Decl);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitEnumDecl(EnumDecl: TEnumDecl);
var
  i: Integer;
  Decl: TDecl;
begin
  IncIndent;
  VisitNode(EnumDecl);
  Visit(EnumDecl.Ident);
  IncIndent;
  WriteLn(Indent + 'Elements:');
  IncIndent;
  for i := 0 to EnumDecl.Elements.Count-1 do begin
    Writeln(Indent, 'Name: ', EnumDecl.Elements.Keys[i], ', Value: ',
            EnumDecl.Elements.Data[i].Value);
  end;
  DecIndent;
  WriteLn(Indent + 'Declarations:');
  for Decl in EnumDecl.DeclList do
    Visit(Decl);
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
    Visit(Node);
  DecIndent;
end;

procedure TPrinter.VisitProduct(Product: TProduct);
var
  Node: TNode;
begin
  IncIndent;
  VisitNode(Product);
  for Node in Product.Nodes do
    Visit(Node);
  DecIndent;
end;

end.

