unit uResolver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uVisitor, Generics.Collections, uAST, uToken, uError,
  Variants;

type
  TStatus = (Declared, Enabled);
  TFuncKind = (fkNone, fkFunc, fkInit, fkMethod);
  TClassKind = (ckNone, ckClass, ckSubClass, ckExtension, ckTrait, ckArray,
                ckDictionary, ckEnum);

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

  TScopes = class(specialize TObjectList<TScope>)
    procedure Push(Item: TScope);
    procedure Pop;
    function Top: TScope;
  end;


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
      procedure VisitCallExpr(CallExpr: TCallExpr);
      procedure VisitVariable(Variable: TVariable);
      procedure VisitIfExpr(IfExpr: TIfExpr);
      procedure VisitMatchExpr(MatchExpr: TMatchExpr);
      procedure VisitTupleExpr(TupleExpr: TTupleExpr);
      procedure VisitGetExpr(GetExpr: TGetExpr);
      procedure VisitSelfExpr(SelfExpr: TSelfExpr);
      procedure VisitInheritedExpr(InheritedExpr: TInheritedExpr);
      procedure VisitArrayDeclExpr(ArrayDeclExpr: TArrayDeclExpr);
      procedure VisitDictDeclExpr(DictDeclExpr: TDictDeclExpr);
      procedure VisitIndexedExpr(IndexedExpr: TIndexedExpr);
      procedure VisitInterpolatedExpr(InterpolatedExpr: TInterpolatedExpr);
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
    private
      GlobalScope: TScope;
      CurrentScope: TScope;
      Scopes: TScopes;
      CurrentFuncKind: TFuncKind;
      CurrentClassKind: TClassKind;
      procedure BeginScope;
      procedure EndScope;
      procedure Declare(Ident: TIdent; const isMutable: Boolean=False);
      procedure Enable(Ident: TIdent);
      function Retrieve(Ident: TIdent): TSymbol;
      procedure ResolveLocal(Variable: TVariable);
      procedure ResolveFunction(Func: TFuncDecl; FuncKind: TFuncKind);
      procedure EnableStandardFunctions;
      procedure ResolveClass(ClassDecl: TClassDecl);
  end;

implementation
uses uStandard;

const
  ErrCannotReadLocalVar = 'Cannot read local variable in its own declaration.';
  ErrCannotAssignToConstant = 'Cannot assign value to constant "%s".';
  ErrDuplicateIdInScope = 'Duplicate identifier "%s" in this scope.';
  ErrUndeclaredVar = 'Undeclared variable or function signature "%s".';
  ErrReturnFromFunc = 'Return can only be used from a function.';
  ErrSelfOutsideClass = 'Cannot use "self" outside a class.';
  ErrParentNoClass = 'Parent "%s" is not defined as class.';
  ErrInheritedInClass = 'Can only use "inherited" inside a class.';
  ErrInheritedNotInSubClass = 'Cannot use "inherited" in a class without parent class.';

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
  Add(ASymbol.Name, ASymbol);
end;

function TScope.Lookup(AName: String): TSymbol;
begin
  if ContainsKey(AName) then
    Result := Items[AName]
  else if Assigned(FEnclosing) then
    Result := FEnclosing.Lookup(AName)
  else
    Result := Nil;
end;

{ TScopes }

procedure TScopes.Push(Item: TScope);
begin
  Add(Item); // This item becomes the top of the stack
end;

procedure TScopes.Pop;
begin
  Remove(Last); // Throw away the top of the stack
end;

function TScopes.Top: TScope;
begin
  Result := Last; // Last is the lastly added item and thus the top of the stack
end;

{ TResolver }

constructor TResolver.Create;
begin
  GlobalScope := TScope.Create;
  CurrentScope := GlobalScope;
  Scopes := TScopes.Create();
  CurrentFuncKind := fkNone;
  EnableStandardFunctions;
  CurrentClassKind := ckNone;
end;

destructor TResolver.Destroy;
begin
  GlobalScope.Free;
  Scopes.Free;
  inherited Destroy;
end;

procedure TResolver.Resolve(Tree: TProduct);
begin
  VisitProc(Tree);
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
  VisitProc(BinaryExpr.Left);
  VisitProc(BinaryExpr.Right);
end;

procedure TResolver.VisitConstExpr(ConstExpr: TConstExpr);
begin
  // do nothing
end;

procedure TResolver.VisitUnaryExpr(UnaryExpr: TUnaryExpr);
begin
  VisitProc(UnaryExpr.Expr);
end;

procedure TResolver.VisitCallExpr(CallExpr: TCallExpr);
var
  i: Integer;
begin
  if (CallExpr.Callee is TVariable) and
      ClassNameList.Contains(TVariable(CallExpr.Callee).Ident.Text) then
  begin
    CallExpr.FromClass := True; // set for reuse in interpreter
    VisitProc(CallExpr.Callee); // just the class ident
  end
  else
    VisitProc(CallExpr.Signature); // func ident + argument names
  // visit argument expressions
  for i := 0 to CallExpr.Args.Count-1 do
    VisitProc(CallExpr.Args[i].Expr);
end;

procedure TResolver.VisitVariable(Variable: TVariable);
var
  Symbol: TSymbol;
begin
  Symbol := Retrieve(Variable.Ident);
  if Symbol <> Nil then begin
    if Symbol.Status = Declared then
      with Variable.Token do
        Errors.Append(Variable.Token, ErrCannotReadLocalVar);
    ResolveLocal(Variable);
  end;
end;

procedure TResolver.VisitIfExpr(IfExpr: TIfExpr);
begin
  VisitProc(IfExpr.Condition);
  VisitProc(IfExpr.TrueExpr);
  VisitProc(IfExpr.FalseExpr);
end;

procedure TResolver.VisitMatchExpr(MatchExpr: TMatchExpr);
var
  Key: TExpr;
begin
  VisitProc(MatchExpr.Expr);
  for Key in MatchExpr.IfLimbs.Keys do begin
    VisitProc(Key);
    VisitProc(MatchExpr.IfLimbs[Key]);
  end;
  VisitProc(MatchExpr.ElseLimb);
end;

procedure TResolver.VisitTupleExpr(TupleExpr: TTupleExpr);
var
  Expr: TExpr;
begin
  for Expr in TupleExpr.ExprList do
    VisitProc(Expr);
end;

procedure TResolver.VisitGetExpr(GetExpr: TGetExpr);
begin
  VisitProc(GetExpr.Instance);
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
  VisitProc(ArrayDeclExpr.ArrayDecl);
end;

procedure TResolver.VisitDictDeclExpr(DictDeclExpr: TDictDeclExpr);
begin
  VisitProc(DictDeclExpr.DictDecl);
end;

procedure TResolver.VisitIndexedExpr(IndexedExpr: TIndexedExpr);
begin
  VisitProc(IndexedExpr.Variable);
  VisitProc(IndexedExpr.Index);
end;

procedure TResolver.VisitInterpolatedExpr(InterpolatedExpr: TInterpolatedExpr);
var
  Expr: TExpr;
begin
  for Expr in InterpolatedExpr.ExprList do
    VisitProc(Expr);
end;

procedure TResolver.VisitPrintStmt(PrintStmt: TPrintStmt);
var
  Expr: TExpr;
begin
  for Expr in PrintStmt.ExprList do
    VisitProc(Expr);
end;

procedure TResolver.VisitAssignStmt(AssignStmt: TAssignStmt);
var
  Symbol: TSymbol;
  Ident: TIdent;
begin
  VisitProc(AssignStmt.Expr);
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
  VisitProc(CallExprStmt.CallExpr);
end;

procedure TResolver.VisitSetStmt(SetStmt: TSetStmt);
begin
  VisitProc(SetStmt.GetExpr);
  VisitProc(SetStmt.Expr);
end;

procedure TResolver.VisitIndexedExprStmt(IndexedExprStmt: TIndexedExprStmt);
begin
  VisitProc(IndexedExprStmt.IndexedExpr);
  VisitProc(IndexedExprStmt.Expr);
end;

procedure TResolver.VisitIfStmt(IfStmt: TIfStmt);
var
  i: Integer;
begin
  if Assigned(IfStmt.VarDecl) then begin
    BeginScope;
    VisitProc(IfStmt.VarDecl);
  end;
  VisitProc(IfStmt.Condition);
  VisitProc(IfStmt.ThenPart);
  if Assigned(IfStmt.ElseIfs) then begin
    for i := 0 to IfStmt.ElseIfs.Count-1 do begin;
      VisitProc(IfStmt.ElseIfs[i]);
      VisitProc(IfStmt.ElseIfParts[i]);
    end;
  end;
  if IfStmt.ElsePart <> Nil then
    VisitProc(IfStmt.ElsePart);
  if Assigned(IfStmt.VarDecl) then
    EndScope;
end;

procedure TResolver.VisitWhileStmt(WhileStmt: TWhileStmt);
begin
  if Assigned(WhileStmt.VarDecl) then begin
    BeginScope;
    VisitProc(WhileStmt.VarDecl);
  end;
  VisitProc(WhileStmt.Condition);
  VisitProc(WhileStmt.Block);
  if Assigned(WhileStmt.VarDecl) then
    EndScope;
end;

procedure TResolver.VisitRepeatStmt(RepeatStmt: TRepeatStmt);
begin
  VisitProc(RepeatStmt.Condition);
  VisitProc(RepeatStmt.Block);
end;

procedure TResolver.VisitEnsureStmt(EnsureStmt: TEnsureStmt);
begin
  if Assigned(EnsureStmt.VarDecl) then
    VisitProc(EnsureStmt.VarDecl);
  VisitProc(EnsureStmt.Condition);
  VisitProc(EnsureStmt.ElsePart);
end;

procedure TResolver.VisitSwitchStmt(SwitchStmt: TSwitchStmt);
var
  Key: TCaseItem;
begin
  VisitProc(SwitchStmt.Expr);
  for Key in SwitchStmt.CaseLimbs.Keys do begin
    VisitProc(Key.Expr);
    VisitProc(SwitchStmt.CaseLimbs[Key]);
  end;
  VisitProc(SwitchStmt.ElseLimb);
end;

procedure TResolver.VisitBreakStmt(BreakStmt: TBreakStmt);
begin
  if Assigned(BreakStmt.Condition) then
    VisitProc(BreakStmt.Condition);
end;

procedure TResolver.VisitContinueStmt(ContinueStmt: TContinueStmt);
begin
  // nothing to resolve
end;

procedure TResolver.VisitReturnStmt(ReturnStmt: TReturnStmt);
begin
  if CurrentFuncKind in [fkNone, fkInit] then
    Errors.Append(ReturnStmt.Token, ErrReturnFromFunc);
  VisitProc(ReturnStmt.Expr);
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
  VisitProc(VarDecl.Expr);
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
    VisitProc(Decl);
end;

procedure TResolver.VisitFuncDecl(FuncDecl: TFuncDecl);
begin
  if FuncDecl.Ident <> Nil then begin
    Declare(FuncDecl.Ident);
    Enable(FuncDecl.Ident);
  end;
  ResolveFunction(FuncDecl, fkFunc);
end;

procedure TResolver.VisitFuncDeclExpr(FuncDeclExpr: TFuncDeclExpr);
begin
  VisitProc(FuncDeclExpr.FuncDecl);
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
    VisitProc(ClassDecl.Parent);
    CurrentClassKind := ckSubClass;
    if (not ClassNameList.Contains(ClassDecl.Parent.Ident.Text)) or
        (ClassDecl.Ident.Text = ClassDecl.Parent.Ident.Text) then
      with ClassDecl.Parent.Ident do
        Errors.Append(Token, Format(ErrParentNoClass, [Text]));
    BeginScope;
    Scopes.Top.Enter(TSymbol.Create('inherited', Enabled));
  end;
  ResolveClass(ClassDecl);
  if Assigned(ClassDecl.Parent) then EndScope;
  CurrentClassKind := EnclosingClassKind;
end;

procedure TResolver.VisitValDecl(ValDecl: TValDecl);
begin
  VisitProc(ValDecl.FuncDecl);
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
    VisitProc(Trait);
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
    VisitProc(Expr);
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
  Key: TExpr;
  Decl: TDecl;
  EnclosingClassKind: TClassKind;
begin
  for Key in DictDecl.KeyValueList.Keys do begin
    VisitProc(Key);
    VisitProc(DictDecl.KeyValueList[Key]);
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
    VisitProc(Node);
  EndScope;
end;

procedure TResolver.VisitProduct(Product: TProduct);
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
  if CurrentScope.ContainsKey(Ident.Text) then
    Errors.Append(Ident.Token, Format(
      ErrDuplicateIdInScope, [Ident.Text]))
  else
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
    Errors.Append(Ident.Token, Format(
      ErrUndeclaredVar, [Ident.Text]));
end;

procedure TResolver.ResolveLocal(Variable: TVariable);
var
  i: Integer;
begin
  for i := Scopes.Count-1 downto 0 do
    if Scopes[i].ContainsKey(Variable.Ident.Text) then begin
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
  VisitProc(Func.Body);
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
    VisitProc(Trait);
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
      dkVar: VisitProc(Decl as TVarDecl);
    end;
  end;
  // resolve static declarations
  for Decl in ClassDecl.StaticList do begin
    BeginScope;
    Scopes.Top.Enter(TSymbol.Create('self', Enabled));
    case Decl.Kind of
      //dkVar: Visit(Decl as TVarDecl);
      dkFunc: ResolveFunction(Decl as TFuncDecl, fkMethod);
    end;
    EndScope;
  end;
  EndScope;
end;


procedure TResolver.EnableStandardFunctions;
begin
  with GlobalScope do begin
    Enter(TSymbol.Create('abs', Enabled, False));
    Enter(TSymbol.Create('arctan', Enabled, False));
    Enter(TSymbol.Create('chr', Enabled, False));
    Enter(TSymbol.Create('cos', Enabled, False));
    Enter(TSymbol.Create('date', Enabled, False));
    Enter(TSymbol.Create('exp', Enabled, False));
    Enter(TSymbol.Create('frac', Enabled, False));
    Enter(TSymbol.Create('length', Enabled, False));
    Enter(TSymbol.Create('ln', Enabled, False));
    Enter(TSymbol.Create('milliseconds', Enabled, False));
    Enter(TSymbol.Create('now', Enabled, False));
    Enter(TSymbol.Create('ord', Enabled, False));
    Enter(TSymbol.Create('pi', Enabled, False));
    Enter(TSymbol.Create('random', Enabled, False));
    Enter(TSymbol.Create('randomLimit', Enabled, False));
    Enter(TSymbol.Create('randomize', Enabled, False));
    Enter(TSymbol.Create('round', Enabled, False));
    Enter(TSymbol.Create('sin', Enabled, False));
    Enter(TSymbol.Create('sqr', Enabled, False));
    Enter(TSymbol.Create('sqrt', Enabled, False));
    Enter(TSymbol.Create('time', Enabled, False));
    Enter(TSymbol.Create('today', Enabled, False));
    Enter(TSymbol.Create('trunc', Enabled, False));
    Enter(TSymbol.Create('assigned', Enabled, False));
    Enter(TSymbol.Create('readln', Enabled, False));
    Enter(TSymbol.Create('floor', Enabled, False));
    Enter(TSymbol.Create('ceil', Enabled, False));
    Enter(TSymbol.Create('toNum', Enabled, False));
    Enter(TSymbol.Create('toStr', Enabled, False));
    Enter(TSymbol.Create('pred', Enabled, False));
    Enter(TSymbol.Create('succ', Enabled, False));
    Enter(TSymbol.Create('method', Enabled, False));

    Enter(TSymbol.Create('Array', Enabled, False));
    Enter(TSymbol.Create('Dictionary', Enabled, False));
    // list functions
    Enter(TSymbol.Create('listAdd', Enabled, False));
    Enter(TSymbol.Create('listAddList', Enabled, False));
    Enter(TSymbol.Create('listClear', Enabled, False));
    Enter(TSymbol.Create('listContains', Enabled, False));
    Enter(TSymbol.Create('listDelete', Enabled, False));
    Enter(TSymbol.Create('listExtract', Enabled, False));
    Enter(TSymbol.Create('listFirst', Enabled, False));
    Enter(TSymbol.Create('listIndexOf', Enabled, False));
    Enter(TSymbol.Create('listInit', Enabled, False));
    Enter(TSymbol.Create('listInsert', Enabled, False));
    Enter(TSymbol.Create('listLast', Enabled, False));
    Enter(TSymbol.Create('listRemove', Enabled, False));
    Enter(TSymbol.Create('listRetrieve', Enabled, False));
    // dictionary functions
    Enter(TSymbol.Create('dictAdd', Enabled, False));
    Enter(TSymbol.Create('dictAddList', Enabled, False));
    Enter(TSymbol.Create('dictClear', Enabled, False));
    Enter(TSymbol.Create('dictContainsKey', Enabled, False));
    Enter(TSymbol.Create('dictContainsValue', Enabled, False));
    Enter(TSymbol.Create('dictDelete', Enabled, False));
    Enter(TSymbol.Create('dictKeyOf', Enabled, False));
    Enter(TSymbol.Create('dictSetSorted', Enabled, False));
    Enter(TSymbol.Create('dictSort', Enabled, False));
    Enter(TSymbol.Create('dictValueOf', Enabled, False));
    Enter(TSymbol.Create('dictKeys', Enabled, False));
    Enter(TSymbol.Create('dictValues', Enabled, False));

    // CRT functions
    Enter(TSymbol.Create('window', Enabled, False));
    Enter(TSymbol.Create('gotoXY', Enabled, False));
    Enter(TSymbol.Create('clrScr', Enabled, False));

    // files
    Enter(TSymbol.Create('forReading', Enabled, False));
    Enter(TSymbol.Create('forWriting', Enabled, False));
    Enter(TSymbol.Create('forReadingAndWriting', Enabled, False));
    Enter(TSymbol.Create('fromBeginning', Enabled, False));
    Enter(TSymbol.Create('fromCurrent', Enabled, False));
    Enter(TSymbol.Create('fromEnd', Enabled, False));
    Enter(TSymbol.Create('fileOpen', Enabled, False));
    Enter(TSymbol.Create('fileRead', Enabled, False));
    Enter(TSymbol.Create('fileClose', Enabled, False));
    Enter(TSymbol.Create('fileWrite', Enabled, False));
    Enter(TSymbol.Create('fileSeek', Enabled, False));
    Enter(TSymbol.Create('fileCreate', Enabled, False));
  end;
end;

end.

