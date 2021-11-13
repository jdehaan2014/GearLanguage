unit uPrinter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uVisitor, uAST, uToken;

type

  TPrinter = class(TVisitor)
    public
      constructor Create(const IndentSize: Integer);
      procedure Print(Tree: TProduct; const DebugFileName: TFileName);
    published
      procedure VisitTNode(Node: TNode);
      // Expressions
      procedure VisitTLiteralExpr(LiteralExpr: TLiteralExpr);
      procedure VisitTNumberExpr(NumberExpr: TNumberExpr);
      procedure VisitTStringExpr(StringExpr: TStringExpr);
      procedure VisitTCharExpr(CharExpr: TCharExpr);
      procedure VisitTUnaryExpr(UnaryExpr: TUnaryExpr);
      procedure VisitTBinaryExpr(BinaryExpr: TBinaryExpr);
      procedure VisitTAndExpr(AndExpr: TAndExpr);
      procedure VisitTOrExpr(OrExpr: TOrExpr);
      procedure VisitTTernaryExpr(TernaryExpr: TTernaryExpr);
      //procedure VisitTPostfixExpr(PostfixExpr: TPostfixExpr);
      procedure VisitTIdent(Ident: TIdent);
      procedure VisitTVariable(Variable: TVariable);
      procedure VisitTCallExpr(CallExpr: TCallExpr);
      procedure VisitTMatchExpr(MatchExpr: TMatchExpr);
      procedure VisitTSelfExpr(SelfExpr: TSelfExpr);
      procedure VisitTInheritedExpr(InheritedExpr: TInheritedExpr);
      procedure VisitTGetExpr(GetExpr: TGetExpr);
      procedure VisitTLambdaExpr(LambdaExpr: TLambdaExpr);
      procedure VisitTArrayExpr(ArrayExpr: TArrayExpr);
      procedure VisitTDictionaryExpr(DictionaryExpr: TDictionaryExpr);
      procedure VisitTIndexedExpr(IndexedExpr: TIndexedExpr);
      procedure VisitTTupleExpr(TupleExpr: TTupleExpr);
      procedure VisitTSetExpr(SetExpr: TSetExpr);
      procedure VisitTRangeExpr(RangeExpr: TRangeExpr);
      procedure VisitTListBuilderExpr(ListBuilderExpr: TListBuilderExpr);

       // Statements
      procedure VisitTExprStmt(ExprStmt: TExprStmt);
      procedure VisitTPrintStmt(PrintStmt: TPrintStmt);
      procedure VisitTAssignStmt(AssignStmt: TAssignStmt);
      procedure VisitTSetStmt(SetStmt: TSetStmt);
      procedure VisitTIndexedStmt(IndexedStmt: TIndexedStmt);
      procedure VisitTCallExprStmt(CallExprStmt: TCallExprStmt);
      procedure VisitTReturnStmt(ReturnStmt: TReturnStmt);
      procedure VisitTIfStmt(IfStmt: TIfStmt);
      procedure VisitTWhileStmt(WhileStmt: TWhileStmt);
      procedure VisitTForStmt(ForStmt: TForStmt);
      procedure VisitTForInStmt(ForInStmt: TForInStmt);
      procedure VisitTRepeatStmt(RepeatStmt: TRepeatStmt);
      procedure VisitTLoopDoStmt(LoopDoStmt: TLoopDoStmt);
      procedure VisitTEnsureStmt(EnsureStmt: TEnsureStmt);
      procedure VisitTSwitchStmt(SwitchStmt: TSwitchStmt);
      procedure VisitTBreakStmt(BreakStmt: TBreakStmt);
      procedure VisitTContinueStmt(ContinueStmt: TContinueStmt);
      procedure VisitTUseStmt(UseStmt: TUseStmt);
      procedure VisitTDeferStmt(DeferStmt: TDeferStmt);
      // Declarations
      procedure VisitTVarDecl(VarDecl: TVarDecl);
      procedure VisitTVarDecls(VarDecls: TVarDecls);
      procedure VisitTBody(Body: TBody);
      procedure VisitTFuncDecl(FuncDecl: TFuncDecl);
      procedure VisitTOperatorDecl(OperatorDecl: TOperatorDecl);
      procedure VisitTValDecl(ValDecl: TValDecl);
      procedure VisitTClassDecl(ClassDecl: TClassDecl);
      procedure VisitTTraitDecl(TraitDecl: TTraitDecl);
      procedure VisitTExtensionDecl(ExtensionDecl: TExtensionDecl);
      procedure VisitTEnumDecl(EnumDecl: TEnumDecl);
      //procedure VisitTIndexerDecl(IndexerDecl: TIndexerDecl);
      // Blocks
      procedure VisitTBlock(Block: TBlock);
      procedure VisitTProduct(Product: TProduct);
    private
      Increase: Integer;
      Indent: string;
      ASTFile: TextFile;
      procedure IncIndent;
      procedure DecIndent;
  end;


implementation
uses uCommon;

{ TPrinter }

constructor TPrinter.Create(const IndentSize: Integer);
begin
  Increase := IndentSize;
  Indent := Space(IndentSize);
end;

procedure TPrinter.Print(Tree: TProduct; const DebugFileName: TFileName);
begin
  try
    Assign(ASTFile, DebugFileName);
    Rewrite(ASTFile);
    Writeln(ASTFile, DateToStr(Date), '; ' , TimeToStr(Time));
    VisitProc(Tree);
    Writeln(ASTFile);
  finally
    Close(ASTFile);
  end;
end;

procedure TPrinter.VisitTNode(Node: TNode);
begin
  Writeln(ASTFile, Indent + String(Node.ClassName).Substring(1));
end;

procedure TPrinter.VisitTLiteralExpr(LiteralExpr: TLiteralExpr);
var
  Literal: String;
begin
  IncIndent;
  WriteStr(Literal, LiteralExpr.LiteralType);
  WriteLn(ASTFile, Indent, Literal.Substring(2));
  DecIndent;
end;

procedure TPrinter.VisitTNumberExpr(NumberExpr: TNumberExpr);
begin
  IncIndent;
  WriteLn(ASTFile, Indent, NumberExpr.Value);
  DecIndent;
end;

procedure TPrinter.VisitTStringExpr(StringExpr: TStringExpr);
begin
  IncIndent;
  WriteLn(ASTFile, Indent, StringExpr.Value.QuotedString);
  DecIndent;
end;

procedure TPrinter.VisitTCharExpr(CharExpr: TCharExpr);
begin
  IncIndent;
  WriteLn(ASTFile, Indent, '"', CharExpr.Value, '"');
  DecIndent;
end;

procedure TPrinter.VisitTUnaryExpr(UnaryExpr: TUnaryExpr);
begin
  IncIndent;
  WriteLn(ASTFile, Indent, '(', UnaryExpr.Op.toString, ')');
  VisitProc(UnaryExpr.Expr);
  DecIndent;
end;

procedure TPrinter.VisitTBinaryExpr(BinaryExpr: TBinaryExpr);
begin
  IncIndent;
  WriteLn(ASTFile, Indent, '(', BinaryExpr.Op.toString, ')');
  VisitProc(BinaryExpr.Left);
  VisitProc(BinaryExpr.Right);
  DecIndent;
end;

procedure TPrinter.VisitTAndExpr(AndExpr: TAndExpr);
begin
  IncIndent;
  WriteLn(ASTFile, Indent, '(', AndExpr.Op.toString, ')');
  VisitProc(AndExpr.Left);
  VisitProc(AndExpr.Right);
  DecIndent;
end;

procedure TPrinter.VisitTOrExpr(OrExpr: TOrExpr);
begin
  IncIndent;
  WriteLn(ASTFile, Indent, '(', OrExpr.Op.toString, ')');
  VisitProc(OrExpr.Left);
  VisitProc(OrExpr.Right);
  DecIndent;
end;

procedure TPrinter.VisitTTernaryExpr(TernaryExpr: TTernaryExpr);
begin
  IncIndent;
  WriteLn(ASTFile, Indent, '(?)');
  VisitProc(TernaryExpr.Condition);
  Write(ASTFile, Indent, 'TRUE : '); VisitProc(TernaryExpr.TrueExpr);
  Write(ASTFile, Indent, 'FALSE: '); VisitProc(TernaryExpr.FalseExpr);
  DecIndent;
end;

procedure TPrinter.VisitTIdent(Ident: TIdent);
begin
  IncIndent;
  WriteLn(ASTFile, Indent, 'ID: ', Ident.Text);
  DecIndent;
end;

procedure TPrinter.VisitTVariable(Variable: TVariable);
begin
  IncIndent;
  VisitTNode(Variable);
  VisitProc(Variable.Name);
  DecIndent;
end;

procedure TPrinter.VisitTCallExpr(CallExpr: TCallExpr);
var
  Argument: TArgument;
begin
  IncIndent;
  VisitTNode(CallExpr);
  VisitProc(CallExpr.Callee);
  WriteLn(ASTFile, Indent, '  Arguments(', CallExpr.ArgCount, '):');
  for Argument in CallExpr.Arguments do
    VisitProc(Argument.Expr);
  DecIndent;
end;

procedure TPrinter.VisitTMatchExpr(MatchExpr: TMatchExpr);
var
  Key: TBinaryExpr;
begin
  IncIndent;
  VisitTNode(MatchExpr);
  VisitProc(MatchExpr.Expr);
  IncIndent;
  Writeln(ASTFile, Indent, 'If Limbs:');
  for Key in MatchExpr.IfLimbs.Keys do
    begin
      IncIndent;
      WriteLn(ASTFile, Indent, 'IF:');
      Write(ASTFile, Indent, Key.Op.toString, ' ');
      VisitProc(Key.Right);
      VisitProc(MatchExpr.IfLimbs[Key]);
      DecIndent;
    end;
  Writeln(ASTFile, Indent, 'Else:');
  VisitProc(MatchExpr.ElseLimb);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitTSelfExpr(SelfExpr: TSelfExpr);
begin
  IncIndent;
  VisitTNode(SelfExpr);
  DecIndent;
end;

procedure TPrinter.VisitTInheritedExpr(InheritedExpr: TInheritedExpr);
var
  Argument: TExpr;
begin
  IncIndent;
  VisitTNode(InheritedExpr);
  WriteLn(ASTFile, Indent, '  Method: ', InheritedExpr.Method.Name.Text);
  WriteLn(ASTFile, Indent, '  Arguments(', InheritedExpr.ArgCount, '):');
  IncIndent;
  for Argument in InheritedExpr.Arguments do
    VisitProc(Argument);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitTGetExpr(GetExpr: TGetExpr);
begin
  IncIndent;
  VisitTNode(GetExpr);
  VisitProc(GetExpr.Instance);
  VisitProc(GetExpr.Member);
  DecIndent;
end;

procedure TPrinter.VisitTLambdaExpr(LambdaExpr: TLambdaExpr);
begin
  IncIndent;
  VisitTNode(LambdaExpr);
  VisitProc(LambdaExpr.Func);
  DecIndent;
end;

procedure TPrinter.VisitTArrayExpr(ArrayExpr: TArrayExpr);
var
  Element: TExpr;
begin
  IncIndent;
  VisitTNode(ArrayExpr);
  for Element in ArrayExpr.Elements do
    VisitProc(Element);
  DecIndent;
end;

procedure TPrinter.VisitTDictionaryExpr(DictionaryExpr: TDictionaryExpr);
var
  Key: TExpr;
begin
  IncIndent;
  VisitTNode(DictionaryExpr);
  for Key in DictionaryExpr.Elements.Keys do
    begin
      VisitProc(Key);
      VisitProc(DictionaryExpr.Elements[Key]);
    end;
  DecIndent;
end;

procedure TPrinter.VisitTIndexedExpr(IndexedExpr: TIndexedExpr);
begin
  IncIndent;
  VisitTNode(IndexedExpr);
  VisitProc(IndexedExpr.Variable);
  IncIndent;
  Writeln(ASTFile, Indent, 'Index: ');
  VisitProc(IndexedExpr.Index);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitTTupleExpr(TupleExpr: TTupleExpr);
var
  Element: TArgument;
begin
  IncIndent;
  VisitTNode(TupleExpr);
  for Element in TupleExpr.Elements do
    begin
      VisitProc(Element.Name);
      VisitProc(Element.Expr);
    end;
  DecIndent;
end;

procedure TPrinter.VisitTSetExpr(SetExpr: TSetExpr);
var
  Element: TExpr;
begin
  IncIndent;
  VisitTNode(SetExpr);
  for Element in SetExpr.Elements do
    VisitProc(Element);
  DecIndent;
end;

procedure TPrinter.VisitTRangeExpr(RangeExpr: TRangeExpr);
begin
  IncIndent;
  VisitTNode(RangeExpr);
  VisitProc(RangeExpr.From);
  VisitProc(RangeExpr.UpTo);
  Writeln(ASTFile, Indent, 'is inclusive: ', RangeExpr.IsInclusive);
  DecIndent;
end;

procedure TPrinter.VisitTListBuilderExpr(ListBuilderExpr: TListBuilderExpr);
begin
  IncIndent;
  VisitTNode(ListBuilderExpr);
  VisitProc(ListBuilderExpr.Map);
  VisitProc(ListBuilderExpr.Sequence);
  VisitProc(ListBuilderExpr.Filter);
  DecIndent;
end;


procedure TPrinter.VisitTExprStmt(ExprStmt: TExprStmt);
begin
  IncIndent;
  VisitTNode(ExprStmt);
  VisitProc(ExprStmt.Expr);
  DecIndent;
end;

procedure TPrinter.VisitTPrintStmt(PrintStmt: TPrintStmt);
var
  Expr: TExpr;
begin
  IncIndent;
  VisitTNode(PrintStmt);
  for Expr in PrintStmt.ExprList do
    VisitProc(Expr);
  DecIndent;
end;

procedure TPrinter.VisitTAssignStmt(AssignStmt: TAssignStmt);
begin
  IncIndent;
  VisitTNode(AssignStmt);
  WriteLn(ASTFile, Indent, '(', AssignStmt.AssignOp.toString, ')');
  VisitProc(AssignStmt.Variable);
  VisitProc(AssignStmt.Expr);
  DecIndent
end;

procedure TPrinter.VisitTSetStmt(SetStmt: TSetStmt);
begin
  IncIndent;
  VisitTNode(SetStmt);
  WriteLn(ASTFile, Indent, '(', SetStmt.Op.toString, ')');
  VisitProc(SetStmt.GetExpr);
  VisitProc(SetStmt.Expr);
  DecIndent
end;

procedure TPrinter.VisitTIndexedStmt(IndexedStmt: TIndexedStmt);
begin
  IncIndent;
  VisitTNode(IndexedStmt);
  VisitProc(IndexedStmt.IndexedExpr);
  VisitProc(IndexedStmt.Expr);
  DecIndent;
end;

procedure TPrinter.VisitTCallExprStmt(CallExprStmt: TCallExprStmt);
begin
  IncIndent;
  VisitTNode(CallExprStmt);
  VisitProc(CallExprStmt.CallExpr);
  DecIndent
end;

procedure TPrinter.VisitTReturnStmt(ReturnStmt: TReturnStmt);
begin
  IncIndent;
  VisitTNode(ReturnStmt);
  if Assigned(ReturnStmt.Expr) then
    VisitProc(ReturnStmt.Expr);
  DecIndent
end;

procedure TPrinter.VisitTIfStmt(IfStmt: TIfStmt);
var
  ElseIfItem: TElseIfItem;
begin
  IncIndent;
  VisitTNode(IfStmt);
  if Assigned(IfStmt.VarDecl) then
    VisitProc(IfStmt.VarDecl);
  VisitProc(IfStmt.Condition);
  IncIndent;
  WriteLn(ASTFile, Indent, 'ThenPart:');
  VisitProc(IfStmt.ThenPart);

  if Assigned(IfStmt.ElseIfList) then
    for ElseIfItem in IfStmt.ElseIfList do
      begin
        VisitProc(ElseIfItem.Condition);
        VisitProc(ElseIfItem.Block);
      end;

  if Assigned(IfStmt.ElsePart) then
    begin
      WriteLn(ASTFile, Indent, 'ElsePart:');
      VisitProc(IfStmt.ElsePart);
    end;
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitTWhileStmt(WhileStmt: TWhileStmt);
begin
  IncIndent;
  VisitTNode(WhileStmt);
  if Assigned(WhileStmt.VarDecl) then
    VisitProc(WhileStmt.VarDecl);
  VisitProc(WhileStmt.Condition);
  IncIndent;
  WriteLn(ASTFile, Indent, 'Loop:');
  VisitProc(WhileStmt.Block);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitTForStmt(ForStmt: TForStmt);
begin
  IncIndent;
  VisitTNode(ForStmt);
  VisitProc(ForStmt.VarDecl);
  VisitProc(ForStmt.Condition);
  VisitProc(ForStmt.Iterator);
  VisitProc(ForStmt.Block);
  DecIndent;
end;

procedure TPrinter.VisitTForInStmt(ForInStmt: TForInStmt);
begin
  IncIndent;
  VisitTNode(ForInStmt);
  VisitProc(ForInStmt.LoopVar);
  VisitProc(ForInStmt.Sequence);
  if Assigned(ForInStmt.Where) then
    VisitProc(ForInStmt.Where);
  VisitProc(ForInStmt.Block);
  DecIndent;
end;

procedure TPrinter.VisitTRepeatStmt(RepeatStmt: TRepeatStmt);
begin
  IncIndent;
  VisitTNode(RepeatStmt);
  IncIndent;
  WriteLn(ASTFile, Indent, 'Loop:');
  VisitProc(RepeatStmt.Block);
  DecIndent;
  WriteLn(ASTFile, Indent, 'Until:');
  VisitProc(RepeatStmt.Condition);
  DecIndent;
end;

procedure TPrinter.VisitTLoopDoStmt(LoopDoStmt: TLoopDoStmt);
var
  ConditionalBlock: TConditionalBlock;
begin
  IncIndent;
  VisitTNode(LoopDoStmt);
  if Assigned(LoopDoStmt.VarDecls) then
    VisitProc(LoopDoStmt.VarDecls);
  VisitProc(LoopDoStmt.Block);

  for ConditionalBlock in LoopDoStmt.ConditionalBlocks do
    begin
      VisitProc(ConditionalBlock.Condition);
      if Assigned(ConditionalBlock.Block) then
        VisitProc(ConditionalBlock.Block);
    end;

  DecIndent;
end;

procedure TPrinter.VisitTEnsureStmt(EnsureStmt: TEnsureStmt);
begin
  IncIndent;
  VisitTNode(EnsureStmt);
  if Assigned(EnsureStmt.VarDecl) then
    VisitProc(EnsureStmt.VarDecl);
  VisitProc(EnsureStmt.Condition);
  IncIndent;
  WriteLn(ASTFile, Indent, 'ElsePart:');
  VisitProc(EnsureStmt.ElsePart);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitTSwitchStmt(SwitchStmt: TSwitchStmt);
var
  Key: TCaseItem;
begin
  IncIndent;
  VisitTNode(SwitchStmt);
  VisitProc(SwitchStmt.Expr);
  IncIndent;
  Writeln(ASTFile, Indent, 'Case Limbs:');
  for Key in SwitchStmt.Cases.Keys do begin
    IncIndent;
    WriteLn(ASTFile, Indent, 'Case:');
    VisitProc(Key.Expr);
    VisitProc(SwitchStmt.Cases[Key]);
    DecIndent;
  end;
  Writeln(ASTFile, Indent, 'Default:');
  VisitProc(SwitchStmt.DefaultCase);
  DecIndent;
  DecIndent;
end;

//procedure TPrinter.VisitTBreakStmt(BreakStmt: TBreakStmt);
//begin
//  IncIndent;
//  VisitTNode(BreakStmt);
//  if Assigned(BreakStmt.Condition) then
//    VisitProc(BreakStmt.Condition);
//  DecIndent;
//end;

procedure TPrinter.VisitTBreakStmt(BreakStmt: TBreakStmt);
begin
  IncIndent;
  VisitTNode(BreakStmt);
  DecIndent;
end;

procedure TPrinter.VisitTContinueStmt(ContinueStmt: TContinueStmt);
begin
  IncIndent;
  VisitTNode(ContinueStmt);
  DecIndent;
end;

procedure TPrinter.VisitTUseStmt(UseStmt: TUseStmt);
begin
  IncIndent;
  VisitTNode(UseStmt);
  Writeln(ASTFile, Indent, '  File: ', UseStmt.FileName);
  DecIndent;
end;

procedure TPrinter.VisitTDeferStmt(DeferStmt: TDeferStmt);
begin
  IncIndent;
  VisitTNode(DeferStmt);
  VisitProc(DeferStmt.Closure);
  DecIndent;
end;

procedure TPrinter.VisitTVarDecl(VarDecl: TVarDecl);
begin
  IncIndent;
  VisitTNode(VarDecl);
  VisitProc(VarDecl.Name);
  if Assigned(VarDecl.Expr) then
    VisitProc(VarDecl.Expr);
  DecIndent;
end;

procedure TPrinter.VisitTVarDecls(VarDecls: TVarDecls);
var
  Decl: TDecl;
begin
  for Decl in VarDecls.List do
    VisitProc(Decl);
end;

procedure TPrinter.VisitTBody(Body: TBody);
var
  Node: TNode;
begin
  IncIndent;
  VisitTNode(Body);
  for Node in Body.Nodes do
    VisitProc(Node);
  DecIndent;
end;

procedure TPrinter.VisitTFuncDecl(FuncDecl: TFuncDecl);
var
  Parameter: TParameter;
begin
  IncIndent;
  VisitTNode(FuncDecl);
  if Assigned(FuncDecl.Name) then
    VisitProc(FuncDecl.Name)
  else
    WriteLn(ASTFile, Indent, '  ID: <empty>');
  IncIndent;
  WriteLn(ASTFile, Indent, 'Parameters:');
  if Assigned(FuncDecl.Parameters) then
    for Parameter in FuncDecl.Parameters do
      VisitProc(Parameter.Variable)
  else
    WriteLn(ASTFile, Indent, '  None');
  DecIndent;
  VisitProc(FuncDecl.Body);
  DecIndent;
end;

procedure TPrinter.VisitTOperatorDecl(OperatorDecl: TOperatorDecl);
var
  Parameter: TParameter;
begin
  IncIndent;
  VisitTNode(OperatorDecl);
  VisitProc(OperatorDecl.Name);
  IncIndent;
  WriteLn(ASTFile, Indent, 'Parameters:');
  if Assigned(OperatorDecl.Parameters) then
    for Parameter in OperatorDecl.Parameters do
      VisitProc(Parameter.Variable)
  else
    WriteLn(ASTFile, Indent, '  None');
  DecIndent;
  VisitProc(OperatorDecl.Body);
  DecIndent;
end;

procedure TPrinter.VisitTValDecl(ValDecl: TValDecl);
begin
  IncIndent;
  VisitTNode(ValDecl);
  VisitProc(ValDecl.Name);
  VisitProc(ValDecl.Func);
  DecIndent;
end;

procedure TPrinter.VisitTClassDecl(ClassDecl: TClassDecl);
var
  Decl: TDecl;
begin
  IncIndent;
  VisitTNode(ClassDecl);
  VisitProc(ClassDecl.Name);
  if Assigned(ClassDecl.Parent) then
    Writeln(ASTFile, Indent, '  Parent ID: ', ClassDecl.Parent.Name.Text);

  WriteLn(ASTFile, Indent + '  Declarations:');
  IncIndent;
  for Decl in ClassDecl.Members.Values do
    VisitProc(Decl);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitTTraitDecl(TraitDecl: TTraitDecl);
var
  Decl: TDecl;
  Trait: TExpr;
begin
  IncIndent;
  VisitTNode(TraitDecl);
  VisitProc(TraitDecl.Name);
  IncIndent;
  WriteLn(ASTFile, Indent + 'Traits:');
  if Assigned(TraitDecl.Traits) then
    for Trait in TraitDecl.Traits do
      VisitProc(Trait);
  DecIndent;
  IncIndent;
  WriteLn(ASTFile, Indent + 'Declarations:');
  for Decl in TraitDecl.Members.Values do
    VisitProc(Decl);
  DecIndent;
  DecIndent;
end;

procedure TPrinter.VisitTExtensionDecl(ExtensionDecl: TExtensionDecl);
var
  Member: TDecl;
begin
  IncIndent;
  VisitTNode(ExtensionDecl);
  VisitProc(ExtensionDecl.Name);
  for Member in ExtensionDecl.Members.Values do
    VisitProc(Member);
  DecIndent;
end;

procedure TPrinter.VisitTEnumDecl(EnumDecl: TEnumDecl);
var
  Key: String;
  Decl: TDecl;
begin
  IncIndent;
  VisitTNode(EnumDecl);
  VisitProc(EnumDecl.Name);
  IncIndent;
  WriteLn(ASTFile, Indent, 'Elements:');
  IncIndent;
  for Key in EnumDecl.Elements.Keys do
    begin
      Write(ASTFile, Indent, 'Name: ', Key);
      if EnumDecl.Elements[Key] <> Nil then
        VisitProc(EnumDecl.Elements[Key]);
    end;
  DecIndent;
  WriteLn(ASTFile, Indent, 'Declarations:');
  for Decl in EnumDecl.Members.Values do
    VisitProc(Decl);
  DecIndent;
  DecIndent;
end;

//procedure TPrinter.VisitTIndexerDecl(IndexerDecl: TIndexerDecl);
//begin
//  IncIndent;
//  VisitTNode(IndexerDecl);
//  VisitProc(IndexerDecl.Getter);
//  if Assigned(IndexerDecl.Setter) then
//    VisitProc(IndexerDecl.Setter);
//  DecIndent;
//end;

procedure TPrinter.VisitTBlock(Block: TBlock);
var
  Node: TNode;
begin
  IncIndent;
  VisitTNode(Block);
  for Node in Block.Nodes do
    VisitProc(Node);
  DecIndent;
end;

procedure TPrinter.VisitTProduct(Product: TProduct);
var
  Node: TNode;
begin
  VisitTNode(Product);
  for Node in Product.Nodes do
    VisitProc(Node);
end;

procedure TPrinter.IncIndent;
begin
  Indent := Space(Length(Indent) + Increase);
end;

procedure TPrinter.DecIndent;
begin
  Indent := Space(Length(Indent) - Increase);
end;

end.


