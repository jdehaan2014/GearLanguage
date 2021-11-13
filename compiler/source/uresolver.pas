unit uResolver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, uVisitor, uAST, uToken, uScopes,
  uError;

// The resolver resolves identifiers and validates whether they are declared before
// they are used.
// The resolver also performs basic type checking, function argument vs. parameter
// checking, check if 'self' and 'inherited' are properly used.
// It processes the extensions and traits, and adds respective members to their classes.
// Mutability of variables and constants is checked.
// The use of private clas properties is checked.
// The usage of 'defer' is checked.
type

  // A function can be a global function, a constructor or an object method
  TFuncKind = (fkNone, fkFunc, fkInit, fkMethod);

  TClassKind = (ckNone, ckClass, ckSubClass, ckExtension, ckEnum, ckTrait);

  TResolver = class(TVisitor)
    public
      constructor Create;
      destructor Destroy; override;
      procedure Resolve(Product: TProduct);
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
      // Blocks
      procedure VisitTBlock(Block: TBlock);
      procedure VisitTProduct(Product: TProduct);
    private
      GlobalScope: TScope;
      CurrentScope: TScope;
      Scopes: TScopes;
      CurrentFuncKind: TFuncKind;
      CurrentClassKind: TClassKind;
      CurrentClass: TSymbol;
      DeferActive: Boolean;
      procedure BeginScope;
      procedure EndScope;
      procedure Declare(Ident: TIdent; const isMutable: Boolean);
      procedure Enable(Ident: TIdent);
      procedure Define(Ident: TIdent);
      function Retrieve(Ident: TIdent): TSymbol;
      procedure ResolveLocal(Variable: TVariable);
      procedure ResolveFunction(FuncDecl: TFuncDecl; FuncKind: TFuncKind);
      procedure EnableStandardFunctions;
      procedure ResolveClass(ClassDecl: TClassDecl);
      function ApplyTraits(TraitList: TExprList): TMemberMap;
  end;


implementation
uses uCommon;

const
  ErrCannotReadLocalVar = 'Cannot read local variable in its own declaration.';
  ErrCannotAssignToConstant = 'Cannot assign value to constant "%s".';
  ErrCannotAccessPrivateVar = 'Undeclared or access to private member "%s".';
  ErrDuplicateIdInScope = 'Duplicate identifier "%s" in this scope.';
  ErrUndeclaredVar = 'Undeclared variable "%s".';
  ErrReturnFromFunc = 'Return can only be used from a function.';
  ErrSelfOutsideClass = 'Cannot use "self" outside a class.';
  ErrParentNoClass = 'Parent "%s" is not defined as class.';
  ErrInheritedInClass = 'Can only use "inherited" inside a class.';
  ErrInheritedNotInSubClass = 'Cannot use "inherited" in a class without parent class.';
  ErrUndefinedClassMember = 'Class member "%s" is undefined.';

{ TResolver }

constructor TResolver.Create;
begin
  GlobalScope := TScope.Create;
  CurrentScope := GlobalScope;
  Scopes := TScopes.Create();
  CurrentFuncKind := fkNone;
  CurrentClassKind := ckNone;
  CurrentClass := Nil;
  DeferActive := False;
  EnableStandardFunctions;
end;

destructor TResolver.Destroy;
begin
  GlobalScope.Free;
  Scopes.Free;
  inherited Destroy;
end;

procedure TResolver.Resolve(Product: TProduct);
begin
  VisitProc(Product);
end;

procedure TResolver.VisitTNode(Node: TNode);
begin
  // nothing
end;

procedure TResolver.VisitTLiteralExpr(LiteralExpr: TLiteralExpr);
begin
  // nothing
end;

procedure TResolver.VisitTNumberExpr(NumberExpr: TNumberExpr);
begin
  // nothing
end;

procedure TResolver.VisitTStringExpr(StringExpr: TStringExpr);
begin
  // nothing
end;

procedure TResolver.VisitTCharExpr(CharExpr: TCharExpr);
begin
  // nothing
end;

procedure TResolver.VisitTUnaryExpr(UnaryExpr: TUnaryExpr);
begin
  VisitProc(UnaryExpr.Expr);
end;

procedure TResolver.VisitTBinaryExpr(BinaryExpr: TBinaryExpr);
begin
  VisitProc(BinaryExpr.Left);
  VisitProc(BinaryExpr.Right);
  //if BinaryExpr.Op = ttIs then // to prevent runtime error
  //  begin
  //    if not ((BinaryExpr.Right is TVariable) and
  //       (ClassMap.ContainsKey(TVariable(BinaryExpr.Right).Name.Text))) then
  //       AddError(BinaryExpr.Right.Location, 'Class type expected.');
  //  end;
end;

procedure TResolver.VisitTAndExpr(AndExpr: TAndExpr);
begin
  VisitProc(AndExpr.Left);
  VisitProc(AndExpr.Right);
end;

procedure TResolver.VisitTOrExpr(OrExpr: TOrExpr);
begin
  VisitProc(OrExpr.Left);
  VisitProc(OrExpr.Right);
end;

procedure TResolver.VisitTTernaryExpr(TernaryExpr: TTernaryExpr);
begin
  VisitProc(TernaryExpr.Condition);
  VisitProc(TernaryExpr.TrueExpr);
  VisitProc(TernaryExpr.FalseExpr);
end;

procedure TResolver.VisitTIdent(Ident: TIdent);
begin
  // nothing
end;

procedure TResolver.VisitTVariable(Variable: TVariable);
var
  Symbol: TSymbol;
begin
  Symbol := Retrieve(Variable.Name);
  if Assigned(Symbol) then
    begin
      if Symbol.Status = Declared then
        AddError(Variable.Location, ErrCannotReadLocalVar);
      ResolveLocal(Variable);
    end;
end;

procedure TResolver.VisitTCallExpr(CallExpr: TCallExpr);
var
  Argument: TArgument;
  ClassDecl: TClassDecl;
  Init: TFuncDecl;
  Parameter: TParameter;
  i: Integer=0;
begin
  if (CallExpr.Callee is TVariable) and
     (ClassMap.ContainsKey(TVariable(CallExpr.Callee).Name.Text)) then
    begin
      CallExpr.IsClassInit := True; // class instance; set for reuse in codegen
      VisitProc(CallExpr.Callee); // just the class ident
      ClassDecl := ClassMap[TVariable(CallExpr.Callee).Name.Text] as TClassDecl;
      if CallExpr.Arguments.Count>0 then
        if ClassDecl.Members.ContainsKey('init') then
          begin
            // check if parameter alternate names are used in call expr
            Init := ClassDecl.Members['init'] as TFuncDecl;
            for Parameter in Init.Parameters do
              begin
                if (Parameter.Name = Nil) and (CallExpr.Arguments[i].Name = Nil) then
                  Continue;
                if (Parameter.Name = Nil) and (CallExpr.Arguments[i].Name <> Nil) then
                  AddError(CallExpr.Arguments[i].Name.Location,
                    Format('Did not expect parameter name "%s:" in call.', [CallExpr.Arguments[i].Name.Text]))
                else if (Parameter.Name <> Nil) and (CallExpr.Arguments[i].Name = Nil) then
                  AddError(Parameter.Name.Location,
                    Format('Expected parameter name "%s:" in call.', [Parameter.Name.Text]))
                else if Parameter.Name.Text <> CallExpr.Arguments[i].Name.Text then
                  AddError(Init.Location,
                    Format('Parameter name mismatch in call. Expected "%s:" but got "%s:".',
                    [Parameter.Name.Text, CallExpr.Arguments[i].Name.Text]));
                Inc(i);
              end;
          end;
    end
  else
    VisitProc(CallExpr.Signature);
  // visit the arguments
  for Argument in CallExpr.Arguments do
    VisitProc(Argument.Expr);
end;


procedure TResolver.VisitTMatchExpr(MatchExpr: TMatchExpr);
var
  Key: TBinaryExpr;
begin
  VisitProc(MatchExpr.Expr);
  for Key in MatchExpr.IfLimbs.Keys do
    begin
      VisitProc(Key.Right);
      VisitProc(MatchExpr.IfLimbs[Key]);
    end;
  VisitProc(MatchExpr.ElseLimb);
end;

procedure TResolver.VisitTSelfExpr(SelfExpr: TSelfExpr);
begin
  if CurrentClassKind = ckNone then
    AddError(SelfExpr.Location, ErrSelfOutsideClass);
  VisitTVariable(SelfExpr);
end;

procedure TResolver.VisitTInheritedExpr(InheritedExpr: TInheritedExpr);
begin
  if CurrentClassKind in [ckNone, ckExtension, ckEnum, ckTrait] then
    AddError(InheritedExpr.Location, ErrInheritedInClass)
  else if CurrentClassKind <> ckSubClass then
    AddError(InheritedExpr.Location, ErrInheritedNotInSubClass);
  VisitTVariable(InheritedExpr);
end;

procedure TResolver.VisitTGetExpr(GetExpr: TGetExpr);
var
  Name: String;
begin
  VisitProc(GetExpr.Instance);
  if not (GetExpr.Instance is TSelfExpr) then
    begin
      Name := TVariable(GetExpr.Member).Name.Text;
      if Name[1] = '_' then
        AddError(GetExpr.Member.Location, Format(ErrCannotAccessPrivateVar, [Name]));
    end;
end;


procedure TResolver.VisitTLambdaExpr(LambdaExpr: TLambdaExpr);
begin
  VisitProc(LambdaExpr.Func);
end;

procedure TResolver.VisitTArrayExpr(ArrayExpr: TArrayExpr);
var
  Element: TExpr;
begin
  for Element in ArrayExpr.Elements do
    VisitProc(Element);
end;

procedure TResolver.VisitTDictionaryExpr(DictionaryExpr: TDictionaryExpr);
var
  Key: TExpr;
begin
  for Key in DictionaryExpr.Elements.Keys do
    begin
      VisitProc(Key);
      VisitProc(DictionaryExpr.Elements[Key]);
    end;
end;

procedure TResolver.VisitTIndexedExpr(IndexedExpr: TIndexedExpr);
begin
  VisitProc(IndexedExpr.Variable);
  VisitProc(IndexedExpr.Index);
end;

procedure TResolver.VisitTTupleExpr(TupleExpr: TTupleExpr);
var
  Element: TArgument;
begin
  for Element in TupleExpr.Elements do
    VisitProc(Element.Expr);
end;

procedure TResolver.VisitTSetExpr(SetExpr: TSetExpr);
var
  Element: TExpr;
begin
  for Element in SetExpr.Elements do
    VisitProc(Element);
end;

procedure TResolver.VisitTRangeExpr(RangeExpr: TRangeExpr);
begin
  VisitProc(RangeExpr.From);
  VisitProc(RangeExpr.UpTo);
end;

procedure TResolver.VisitTListBuilderExpr(ListBuilderExpr: TListBuilderExpr);
begin
  VisitProc(ListBuilderExpr.Map);
  VisitProc(ListBuilderExpr.Sequence);
  VisitProc(ListBuilderExpr.Filter);
end;

//
// STATEMENTS
//

procedure TResolver.VisitTExprStmt(ExprStmt: TExprStmt);
begin
  VisitProc(ExprStmt.Expr);
end;

procedure TResolver.VisitTPrintStmt(PrintStmt: TPrintStmt);
var
  Expr: TExpr;
begin
  for Expr in PrintStmt.ExprList do
    VisitProc(Expr);
end;

procedure TResolver.VisitTAssignStmt(AssignStmt: TAssignStmt);
var
  Symbol: TSymbol;
  Name: TIdent;
begin
  VisitProc(AssignStmt.Expr);
  Name := AssignStmt.Variable.Name;
  Symbol := Retrieve(Name);
  if Assigned(Symbol) then
    begin
      if (not Symbol.Mutable) and (not Symbol.isNil) then
        AddError(Name.Location, Format(ErrCannotAssignToConstant, [Name.Text]))
      else if Symbol.isNil then
        Symbol.isNil := False;
      ResolveLocal(AssignStmt.Variable);
    end;
end;

procedure TResolver.VisitTSetStmt(SetStmt: TSetStmt);
var
  Member: String;
  Instance: TIdent;
  Symbol: TSymbol;
begin
  VisitProc(SetStmt.GetExpr);
  VisitProc(SetStmt.Expr);
  // check if field is mutable.
  Instance := TVariable(SetStmt.GetExpr.Instance).Name;
  Member := TVariable(SetStmt.GetExpr.Member).Name.Text;
  // are we inside a class declaration?
  if (Instance.Text = 'self') and Assigned(CurrentClass) then   // self.field := value
    begin
      if CurrentClass.Members.IndexOf(Member) = -1 then
        AddError(SetStmt.GetExpr.Member.Location,
                Format('Class member "%s" is undefined.', [Member]));
    end
  else
    begin  // instance.field := value
      Symbol := Retrieve(Instance);
      if Assigned(Symbol) then
        begin
          if (Symbol.Members.Count > 0) and (Symbol.Members.IndexOf(Member) >= 0) then
            if Symbol.Members[Member] = False then
              AddError(SetStmt.Expr.Location,
                Format(ErrCannotAssignToConstant, [Member]));
        end
      else AddError(SetStmt.GetExpr.Member.Location,
        Format('Undefined class member "%s".', [Member]));
    end;
end;

procedure TResolver.VisitTIndexedStmt(IndexedStmt: TIndexedStmt);
begin
  VisitProc(IndexedStmt.IndexedExpr);
  VisitProc(IndexedStmt.Expr);
end;

procedure TResolver.VisitTCallExprStmt(CallExprStmt: TCallExprStmt);
begin
  VisitProc(CallExprStmt.CallExpr);
end;

procedure TResolver.VisitTReturnStmt(ReturnStmt: TReturnStmt);
begin
  if CurrentFuncKind in [fkNone, fkInit] then
    AddError(ReturnStmt.Location, ErrReturnFromFunc);
  if DeferActive then
    AddError(ReturnStmt.Location, 'Cannot use "return" inside defer statement.');
  if Assigned(ReturnStmt.Expr) then
    VisitProc(ReturnStmt.Expr);
end;

procedure TResolver.VisitTIfStmt(IfStmt: TIfStmt);
var
  ElseIfItem: TElseIfItem;
begin
  if Assigned(IfStmt.VarDecl) then
    begin
      BeginScope;
      VisitProc(IfStmt.VarDecl);
    end;

  VisitProc(IfStmt.Condition);

  VisitProc(IfStmt.ThenPart);

  if Assigned(IfStmt.ElseIfList) then
    for ElseIfItem in IfStmt.ElseIfList do
      begin
        VisitProc(ElseIfItem.Condition);
        VisitProc(ElseIfItem.Block);
      end;

  if Assigned(IfStmt.ElsePart) then
    VisitProc(IfStmt.ElsePart);

  if Assigned(IfStmt.VarDecl) then
    EndScope;
end;

procedure TResolver.VisitTWhileStmt(WhileStmt: TWhileStmt);
begin
  if Assigned(WhileStmt.VarDecl) then
    begin
      BeginScope;
      VisitProc(WhileStmt.VarDecl);
    end;

  VisitProc(WhileStmt.Condition);

  VisitProc(WhileStmt.Block);

  if Assigned(WhileStmt.VarDecl) then
    EndScope;
end;

procedure TResolver.VisitTForStmt(ForStmt: TForStmt);
begin
  BeginScope;
  VisitProc(ForStmt.VarDecl);
  VisitProc(ForStmt.Condition);
  VisitProc(ForStmt.Iterator);
  VisitProc(ForStmt.Block);
  EndScope;
end;

procedure TResolver.VisitTForInStmt(ForInStmt: TForInStmt);
begin
  // enter the loopvar to the current scope so that the Block can be resolved
  CurrentScope.Enter(TSymbol.Create(ForInStmt.LoopVar.Text, Enabled));

  VisitProc(ForInStmt.Sequence);
  if Assigned(ForInStmt.Where) then
    VisitProc(ForInStmt.Where);
  VisitProc(ForInStmt.Block);
end;

procedure TResolver.VisitTRepeatStmt(RepeatStmt: TRepeatStmt);
begin
  VisitProc(RepeatStmt.Block);
  VisitProc(RepeatStmt.Condition);
end;

procedure TResolver.VisitTLoopDoStmt(LoopDoStmt: TLoopDoStmt);
var
  ConditionalBlock: TConditionalBlock;
begin
  if Assigned(LoopDoStmt.VarDecls) then
    begin
      BeginScope;
      VisitProc(LoopDoStmt.VarDecls);
    end;

  VisitProc(LoopDoStmt.Block);

  for ConditionalBlock in LoopDoStmt.ConditionalBlocks do
    begin
      VisitProc(ConditionalBlock.Condition);
      if Assigned(ConditionalBlock.Block) then
        VisitProc(ConditionalBlock.Block);
    end;

  if Assigned(LoopDoStmt.VarDecls) then
    EndScope;
end;

procedure TResolver.VisitTEnsureStmt(EnsureStmt: TEnsureStmt);
begin
  if Assigned(EnsureStmt.VarDecl) then // don't define new scope here
    VisitProc(EnsureStmt.VarDecl);     // variable must be available outside ensure stmt

  VisitProc(EnsureStmt.Condition);

  VisitProc(EnsureStmt.ElsePart);
end;

procedure TResolver.VisitTSwitchStmt(SwitchStmt: TSwitchStmt);
var
  Key: TCaseItem;
begin
  VisitProc(SwitchStmt.Expr);

  for Key in SwitchStmt.Cases.Keys do
    begin
      if Key.Typ <> citEnum then
        VisitProc(Key.Expr);
      VisitProc(SwitchStmt.Cases[Key]); // resolve statement block of the case
    end;

  VisitProc(SwitchStmt.DefaultCase);
end;

procedure TResolver.VisitTBreakStmt(BreakStmt: TBreakStmt);
begin
  //if Assigned(BreakStmt.Condition) then
  //  VisitProc(BreakStmt.Condition);
  // do nothing
  if DeferActive then
    AddError(BreakStmt.Location, 'Cannot use "break" inside defer statement.');
end;

procedure TResolver.VisitTContinueStmt(ContinueStmt: TContinueStmt);
begin
  // do nothing
  if DeferActive then
    AddError(ContinueStmt.Location, 'Cannot use "continue" inside defer statement.');
end;

procedure TResolver.VisitTUseStmt(UseStmt: TUseStmt);
begin
  // do nothing
  if DeferActive then
    AddError(UseStmt.Location, 'Cannot "use module" inside defer statement.');
end;

procedure TResolver.VisitTDeferStmt(DeferStmt: TDeferStmt);
begin
  if CurrentFuncKind = fkNone then
    AddError(DeferStmt.Location, 'Defer statement only allowed inside functions.');

  // if defer is active then inside a defer stmt
  //   break, continue, return, use stmts cannot be used
  DeferActive := True;
  VisitProc(DeferStmt.Closure);

  DeferActive := False; // defer stmt is not active anymore
end;

//
// DECLARATIONS
//

procedure TResolver.VisitTVarDecl(VarDecl: TVarDecl);
var
  Symbol: TSymbol;
  ClassDecl: TClassDecl;
  Decl: TDecl;
begin
  Declare(VarDecl.Name, VarDecl.Mutable);
  VisitProc(VarDecl.Expr);
  Enable(VarDecl.Name);

  Symbol := Retrieve(VarDecl.Name);
  if VarDecl.Expr is TLiteralExpr then
    with VarDecl.Expr as TLiteralExpr do
      begin
        if LiteralType = ltNil then
          Symbol.isNil := True;
      end
  else if VarDecl.Expr is TCallExpr then with VarDecl.Expr as TCallExpr do
    if IsClassInit then
      begin
        ClassDecl :=
          ClassMap[TVariable((VarDecl.Expr as TCallExpr).Callee).Name.Text] as TClassDecl;
        for Decl in ClassDecl.Members.Values do
          if (Decl is TVarDecl) then with Decl as TVarDecl do
            Symbol.Members[Decl.Name.Text] := (Decl as TVarDecl).Mutable;
      end;
end;

procedure TResolver.VisitTVarDecls(VarDecls: TVarDecls);
var
  Decl: TDecl;
begin
  for Decl in VarDecls.List do
    VisitProc(Decl);
end;

procedure TResolver.VisitTBody(Body: TBody);
var
  Node: TNode;
begin
  for Node in Body.Nodes do
    VisitProc(Node);
end;

procedure TResolver.VisitTFuncDecl(FuncDecl: TFuncDecl);
begin
  if Assigned(FuncDecl.Name) then
    Define(FuncDecl.Name);
  ResolveFunction(FuncDecl, fkFunc);
end;

procedure TResolver.VisitTOperatorDecl(OperatorDecl: TOperatorDecl);
begin
  Define(OperatorDecl.Name);
  ResolveFunction(OperatorDecl, fkMethod);
end;

procedure TResolver.VisitTValDecl(ValDecl: TValDecl);
begin
  VisitProc(ValDecl.Func);
end;

procedure TResolver.VisitTClassDecl(ClassDecl: TClassDecl);
var
  EnclosingClassKind: TClassKind;
  Parent: TClassDecl;
  EnclosingClass: TSymbol;
begin
  Define(ClassDecl.Name);
  EnclosingClass := CurrentClass;
  CurrentClass := Retrieve(ClassDecl.Name);
  EnclosingClassKind := CurrentClassKind;
  CurrentClassKind := ckClass;
  if Assigned(ClassDecl.Parent) then
    begin
      VisitProc(ClassDecl.Parent);
      if ClassMap.ContainsKey(ClassDecl.Parent.Name.Text) then
        begin
          Parent := ClassMap[ClassDecl.Parent.Name.Text] as TClassDecl;
          if Parent.IsRecord then
            AddError(Parent.Location,
              Format('Cannot inherit from "%s".', [ClassDecl.Parent.Name.Text]));
        end
      else
        AddError(ClassDecl.Parent.Location,
          Format('Identifier "%s" is not a class.', [ClassDecl.Parent.Name.Text]));
      CurrentClassKind := ckSubClass;
      BeginScope;
      Scopes.Top.Enter(TSymbol.Create('inherited', Enabled));
    end;
  ResolveClass(ClassDecl);
  if Assigned(ClassDecl.Parent) then EndScope;
  CurrentClassKind := EnclosingClassKind;
  CurrentClass := EnclosingClass;
end;

procedure TResolver.VisitTTraitDecl(TraitDecl: TTraitDecl);
var
  Decl, Member: TDecl;
  EnclosingClassKind: TClassKind;
  Trait: TExpr;
  TraitMembers: TMemberMap=Nil;
  MemberName: String;
begin
  Define(TraitDecl.Name);
  EnclosingClassKind := CurrentClassKind;
  CurrentClassKind := ckTrait;

  // process traits and add functions to members
  if Assigned(TraitDecl.Traits) then
    begin
      for Trait in TraitDecl.Traits do
        VisitProc(Trait);
      TraitMembers := ApplyTraits(TraitDecl.Traits);
      for Member in TraitMembers.Values do
        begin
          MemberName := Member.Name.Text;
          if TraitDecl.Members.ContainsKey(MemberName) then
            AddError(Member.Location, Format('Trait collision: duplicate "%s".',[MemberName]));
          TraitDecl.Members.AddOrSetValue(MemberName, Member);
        end;
    end;

  BeginScope;
  Scopes.Top.Enter(TSymbol.Create('self', Enabled));
  for Decl in TraitDecl.Members.Values do
    if Decl.Kind = dkFunc then
      ResolveFunction(Decl as TFuncDecl, fkMethod);
  EndScope;
  CurrentClassKind := EnclosingClassKind;
end;

procedure TResolver.VisitTExtensionDecl(ExtensionDecl: TExtensionDecl);
var
  Member, Decl: TDecl;
  EnclosingClassKind: TClassKind;
  Key, MemberName: String;
  TraitMembers: TMemberMap=Nil;
begin
  // add members to original class
  if not ClassMap.TryGetValue(ExtensionDecl.Name.Text, Decl) then
    AddError(ExtensionDecl.Location,
      Format('Extension "%s" doesn''t refer to a class/enum.', [ExtensionDecl.Name.Text]))
  else
    begin
      // process traits and add functions to members
      if Assigned(ExtensionDecl.Traits) then
        begin
          TraitMembers := ApplyTraits(ExtensionDecl.Traits);
          for Member in TraitMembers.Values do
            begin
              MemberName := Member.Name.Text;
              if (Decl as TClassDecl).Members.ContainsKey(MemberName) then
                AddError(Member.Location, Format('Trait collision: duplicate "%s".',[MemberName]));
              (Decl as TClassDecl).Members.AddOrSetValue(MemberName, Member);
            end;
        end;

      EnclosingClassKind := CurrentClassKind;
      CurrentClassKind := ckExtension;
      BeginScope;
      Scopes.Top.Enter(TSymbol.Create('self', Enabled));
      for Key in ExtensionDecl.Members.Keys do
        begin
          Member := ExtensionDecl.Members[Key];
          case Decl.Kind of
            dkClass: (Decl as TClassDecl).Members.AddOrSetValue(Key, Member);
            dkEnum:  (Decl as TEnumDecl).Members.AddOrSetValue(Key, Member);
            else
              AddError(Decl.Location, 'Extension member not allowed.');
          end;

          Define(Member.Name);
          case Member.Kind of
            dkFunc: ResolveFunction(Member as TFuncDecl, fkMethod);
            dkVal: ResolveFunction((Member as TValDecl).Func, fkMethod);
          end;
        end;
      EndScope;
      CurrentClassKind := EnclosingClassKind;
    end;
end;

procedure TResolver.VisitTEnumDecl(EnumDecl: TEnumDecl);
var
  Decl: TDecl;
  EnclosingClassKind: TClassKind;
begin
  Define(EnumDecl.Name);
  EnclosingClassKind := CurrentClassKind;
  CurrentClassKind := ckEnum;
  BeginScope;
  Scopes.Top.Enter(TSymbol.Create('self', Enabled));
  for Decl in EnumDecl.Members.Values do begin
    case Decl.Kind of
      dkFunc: ResolveFunction(Decl as TFuncDecl, fkMethod);
      dkVal: ResolveFunction((Decl as TValDecl).Func, fkMethod);
    end;
  end;
  EndScope;
  CurrentClassKind := EnclosingClassKind;
end;

procedure TResolver.VisitTBlock(Block: TBlock);
var
  Node: TNode;
begin
  BeginScope;
  for Node in Block.Nodes do
    begin
      if Node is TDeferStmt then
        AddError(Node.Location, 'Cannot use "defer" inside a local block scope.');
      VisitProc(Node);
    end;
  EndScope;
end;

procedure TResolver.VisitTProduct(Product: TProduct);
var
  Node: TNode;
begin
  for Node in Product.Nodes do
    VisitProc(Node);
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
  if CurrentScope.IndexOf(Ident.Text) >= 0 then
    AddError(Ident.Location, Format(ErrDuplicateIdInScope, [Ident.Text]))
  else
    CurrentScope.Enter(TSymbol.Create(Ident.Text, Declared, isMutable));
end;

procedure TResolver.Enable(Ident: TIdent);
var
  Symbol: TSymbol;
begin
  Symbol := Retrieve(Ident);
  if Assigned(Symbol) then
    Symbol.Status := Enabled;
end;

procedure TResolver.Define(Ident: TIdent);
begin
  if CurrentScope.IndexOf(Ident.Text) >= 0 then
    AddError(Ident.Location, Format(ErrDuplicateIdInScope, [Ident.Text]))
  else
    CurrentScope.Enter(TSymbol.Create(Ident.Text, Enabled));
end;

function TResolver.Retrieve(Ident: TIdent): TSymbol;
begin
  Result := CurrentScope.Lookup(Ident.Text);

  if Result = Nil then
    AddError(Ident.Location, Format(ErrUndeclaredVar, [Ident.Text]));
end;

procedure TResolver.ResolveLocal(Variable: TVariable);
var
  i, NumberOfScopes: Integer;
begin
  NumberOfScopes := Scopes.Count-1;
  for i := NumberOfScopes downto 0 do
    if Scopes[i].IndexOf(Variable.Name.Text) >= 0 then
      Exit;
  // Must be Global scope here
  if GlobalScope.IndexOf(Variable.Name.Text) < 0 then
    AddError(Variable.Location, Format('Variable "%s" not found.', [Variable.Name.Text]));
end;

procedure TResolver.ResolveFunction(FuncDecl: TFuncDecl; FuncKind: TFuncKind);
var
  EnclosingFuncKind: TFuncKind;
  Parameter: TParameter;
begin
  EnclosingFuncKind := CurrentFuncKind;
  CurrentFuncKind := FuncKind;
  BeginScope;
  for Parameter in FuncDecl.Parameters do
    begin
      Declare(Parameter.Variable.Name, True);
      Enable(Parameter.Variable.Name);
    end;
  VisitProc(FuncDecl.Body);
  EndScope;
  CurrentFuncKind := EnclosingFuncKind;
end;

procedure TResolver.ResolveClass(ClassDecl: TClassDecl);
var
  Member: TDecl;
  FuncKind: TFuncKind;
  Key, MemberName: String;
  TraitMembers: TMemberMap=Nil;
begin
  // process traits and add functions to members
  if Assigned(ClassDecl.Traits) then
    begin
      TraitMembers := ApplyTraits(ClassDecl.Traits);
      for Member in TraitMembers.Values do
        begin
          MemberName := Member.Name.Text;
          if ClassDecl.Members.ContainsKey(MemberName) then
            AddError(Member.Location, Format('Trait collision: duplicate "%s".',[MemberName]));
          ClassDecl.Members.AddOrSetValue(MemberName, Member);
        end;
    end;

  BeginScope;
  Scopes.Top.Enter(TSymbol.Create('self', Enabled));

  // first resolve class variables
  if Assigned(ClassDecl.DefaultValue) then
    begin
      VisitProc(ClassDecl.DefaultValue);
      CurrentClass.Members.Add(ClassDecl.DefaultValue.Name.Text, True);
    end;
  for Key in ClassDecl.Members.Keys do
    begin
      Member := ClassDecl.Members[Key];
      if Member.Kind = dkVar then
        begin
          VisitProc(Member as TVarDecl);
          CurrentClass.Members.Add(Member.Name.Text, (Member as TVarDecl).Mutable);
        end;
    end;
  // other members
  for Key in ClassDecl.Members.Keys do
    begin
      Member := ClassDecl.Members[Key];
      case Member.Kind of
        dkFunc:
          begin
            Define(Member.Name);
            if Member.Name.Text = 'init' then
              FuncKind := fkInit
            else
              FuncKind := fkMethod;
            ResolveFunction(Member as TFuncDecl, FuncKind);
          end;
        dkVal:
          begin
            Define(Member.Name);
            ResolveFunction((Member as TValDecl).Func, fkMethod);
          end;
      end;
    end;

  // resolve static members
  for Key in ClassDecl.Statics.Keys do
    begin
      BeginScope;
      Scopes.Top.Enter(TSymbol.Create('self', Enabled));
      Member := ClassDecl.Statics[Key];
      case Member.Kind of
        dkFunc:
          begin
            Define(Member.Name);
            ResolveFunction(Member as TFuncDecl, fkMethod);
          end;
        else
          AddError(Member.Location, 'Static function expected.');
      end;
      EndScope;
    end;

  EndScope;
end;

function TResolver.ApplyTraits(TraitList: TExprList): TMemberMap;
var
  Trait: TExpr;
  TraitDecl: TTraitDecl;
  Name, MemberName: String;
  Member: TDecl;
begin
  Result := TMemberMap.create;
  for Trait in TraitList do
    begin
      // check if Trait exists
      //VisitProc(Trait);
      Name := TVariable(Trait).Name.Text;
      if not TraitMap.ContainsKey(Name) then
        AddError(Trait.Location, Format('Identifier "%s" is not a trait.', [Name]));
      TraitDecl := TraitMap[Name];
      //add trait members to result
      for Member in TraitDecl.Members.Values do
        begin
          MemberName := Member.Name.Text;
          if Result.ContainsKey(MemberName) then
            AddError(Member.Location,
              Format('Func "%s" collision in trait "%s".',[MemberName, Name]));
          Result.AddOrSetValue(MemberName, Member);
        end;
    end;
end;

procedure TResolver.EnableStandardFunctions;
begin
  with GlobalScope do
    begin
      Enter(TSymbol.Create('abs', Enabled));
      Enter(TSymbol.Create('arctan', Enabled));
      Enter(TSymbol.Create('cbrt', Enabled));
      Enter(TSymbol.Create('cos', Enabled));
      Enter(TSymbol.Create('date', Enabled));
      Enter(TSymbol.Create('exp', Enabled));
      Enter(TSymbol.Create('frac', Enabled));
      Enter(TSymbol.Create('length', Enabled));
      Enter(TSymbol.Create('ln', Enabled));
      Enter(TSymbol.Create('milliseconds', Enabled));
      Enter(TSymbol.Create('now', Enabled));
      Enter(TSymbol.Create('pi', Enabled));
      Enter(TSymbol.Create('random', Enabled));
      Enter(TSymbol.Create('randomize', Enabled));
      Enter(TSymbol.Create('round', Enabled));
      Enter(TSymbol.Create('sin', Enabled));
      Enter(TSymbol.Create('sqr', Enabled));
      Enter(TSymbol.Create('sqrt', Enabled));
      Enter(TSymbol.Create('time', Enabled));
      Enter(TSymbol.Create('today', Enabled));
      Enter(TSymbol.Create('trunc', Enabled));
      Enter(TSymbol.Create('readln', Enabled));
      Enter(TSymbol.Create('floor', Enabled));
      Enter(TSymbol.Create('ceil', Enabled));
      Enter(TSymbol.Create('numberOf', Enabled));
      Enter(TSymbol.Create('number#of:', Enabled));
      Enter(TSymbol.Create('integerOf', Enabled));
      Enter(TSymbol.Create('integer#of:', Enabled));
      Enter(TSymbol.Create('stringOf', Enabled));
      Enter(TSymbol.Create('string#of:', Enabled));
      Enter(TSymbol.Create('pred', Enabled));
      Enter(TSymbol.Create('succ', Enabled));
      Enter(TSymbol.Create('ord', Enabled));
      Enter(TSymbol.Create('chr', Enabled));
      Enter(TSymbol.Create('assigned', Enabled));
      Enter(TSymbol.Create('typeOf', Enabled));

      //Enter(TSymbol.Create('hasField', Enabled));
      //Enter(TSymbol.Create('getField', Enabled));
      //Enter(TSymbol.Create('setField', Enabled));
      //Enter(TSymbol.Create('delField', Enabled));
      Enter(TSymbol.Create('error', Enabled));

      // array handling
      Enter(TSymbol.Create('Array:->add', Enabled));
      Enter(TSymbol.Create('Array:->add#array:', Enabled));
      Enter(TSymbol.Create('Array:->add#count:#value:', Enabled));
      Enter(TSymbol.Create('Array:->insert', Enabled));
      Enter(TSymbol.Create('Array:->contains', Enabled));
      Enter(TSymbol.Create('Array:->indexOf', Enabled));
      Enter(TSymbol.Create('Array:->index', Enabled));
      Enter(TSymbol.Create('Array:->clear', Enabled));
      Enter(TSymbol.Create('Array:->delete', Enabled));
      Enter(TSymbol.Create('Array:->remove', Enabled));
      Enter(TSymbol.Create('Array:->swap', Enabled));
      Enter(TSymbol.Create('Array:->count', Enabled));
      Enter(TSymbol.Create('Array:->first', Enabled));
      Enter(TSymbol.Create('Array:->head', Enabled));
      Enter(TSymbol.Create('Array:->last', Enabled));
      Enter(TSymbol.Create('Array:->tail', Enabled));
      Enter(TSymbol.Create('Array:->concat', Enabled));
      Enter(TSymbol.Create('Array:->equals', Enabled));
      Enter(TSymbol.Create('Array:->iterator', Enabled));
      Enter(TSymbol.Create('ArrayIterator:->moveNext', Enabled));
      Enter(TSymbol.Create('ArrayIterator:->current', Enabled));

      // dictionary handling
      Enter(TSymbol.Create('Dictionary:->add', Enabled));
      Enter(TSymbol.Create('Dictionary:->contains#key:', Enabled));
      Enter(TSymbol.Create('Dictionary:->contains#value:', Enabled));
      Enter(TSymbol.Create('Dictionary:->delete', Enabled));
      Enter(TSymbol.Create('Dictionary:->clear', Enabled));
      Enter(TSymbol.Create('Dictionary:->keys', Enabled));
      Enter(TSymbol.Create('Dictionary:->values', Enabled));
      Enter(TSymbol.Create('Dictionary:->count', Enabled));
      Enter(TSymbol.Create('Dictionary:->iterator', Enabled));
      Enter(TSymbol.Create('DictionaryIterator:->moveNext', Enabled));
      Enter(TSymbol.Create('DictionaryIterator:->current', Enabled));

      // set handling
      Enter(TSymbol.Create('Set:->add', Enabled));
      Enter(TSymbol.Create('Set:->add#set:', Enabled));
      Enter(TSymbol.Create('Set:->contains', Enabled));
      Enter(TSymbol.Create('Set:->union#with:', Enabled));
      Enter(TSymbol.Create('Set:->intersect#with:', Enabled));
      Enter(TSymbol.Create('Set:->except#with:', Enabled));
      Enter(TSymbol.Create('Set:->symmetricExcept#with:', Enabled));
      Enter(TSymbol.Create('Set:->remove', Enabled));
      Enter(TSymbol.Create('Set:->clear', Enabled));
      Enter(TSymbol.Create('Set:->toArray', Enabled));
      Enter(TSymbol.Create('Set:->count', Enabled));
      Enter(TSymbol.Create('Set:->first', Enabled));
      Enter(TSymbol.Create('Set:->head', Enabled));
      Enter(TSymbol.Create('Set:->last', Enabled));
      Enter(TSymbol.Create('Set:->tail', Enabled));
      Enter(TSymbol.Create('Set:->iterator', Enabled));
      Enter(TSymbol.Create('SetIterator:->moveNext', Enabled));
      Enter(TSymbol.Create('SetIterator:->current', Enabled));

      // range handling
      Enter(TSymbol.Create('Range:->iterator', Enabled));
      Enter(TSymbol.Create('Range:->step', Enabled));
      Enter(TSymbol.Create('Range:->toArray', Enabled));
      Enter(TSymbol.Create('Range:->toSet', Enabled));
      Enter(TSymbol.Create('Range:->count', Enabled));
      Enter(TSymbol.Create('Range:->from', Enabled));
      Enter(TSymbol.Create('Range:->to', Enabled));
      Enter(TSymbol.Create('Range:->head', Enabled));
      Enter(TSymbol.Create('Range:->tail', Enabled));
      Enter(TSymbol.Create('RangeIterator:->moveNext', Enabled));
      Enter(TSymbol.Create('RangeIterator:->current', Enabled));

      // CRT
      Enter(TSymbol.Create('clrScr', Enabled));
      Enter(TSymbol.Create('windowXY', Enabled));
      Enter(TSymbol.Create('gotoXY', Enabled));

      // get method pointer based on instance and method name
      Enter(TSymbol.Create('getMethod', Enabled));

      // file handling routines
      Enter(TSymbol.Create('fileOpen', Enabled));
      Enter(TSymbol.Create('fileRead', Enabled));
      Enter(TSymbol.Create('fileClose', Enabled));
      Enter(TSymbol.Create('fileWrite', Enabled));
      Enter(TSymbol.Create('fileSeek', Enabled));
      Enter(TSymbol.Create('fileCreate', Enabled));
      Enter(TSymbol.Create('readFile', Enabled));
      Enter(TSymbol.Create('arrayOf', Enabled));
      Enter(TSymbol.Create('array#of:', Enabled));
      Enter(TSymbol.Create('bytesOf', Enabled));
      Enter(TSymbol.Create('bytes#of:', Enabled));

      // file handling
      Enter(TSymbol.Create('forReading', Enabled));   // fileOpen
      Enter(TSymbol.Create('forWriting', Enabled));   // fileOpen
      Enter(TSymbol.Create('forReadingWriting', Enabled));  // fileOpen
      Enter(TSymbol.Create('fromBeginning', Enabled));  // fileSeek
      Enter(TSymbol.Create('fromCurrent', Enabled));  // fileSeek
      Enter(TSymbol.Create('fromEnd', Enabled));   // fileSeek

      // formatting
      Enter(TSymbol.Create('fmtGeneral', Enabled));
      Enter(TSymbol.Create('fmtExponent', Enabled));
      Enter(TSymbol.Create('fmtFixed', Enabled));
      Enter(TSymbol.Create('fmtNumber', Enabled));
      Enter(TSymbol.Create('fmtCurrency', Enabled));

    end;
end;

end.


