unit uCodeGen;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

{$I uDefines.inc}

interface

uses
  Classes, SysUtils, uCommon, uVisitor, uValue, uChunk, uObject, uAST, uToken;

type

  PLocal = ^TLocal;
  TLocal = record
    Name: TIdent;
    Depth: Integer;
    isCaptured: Boolean;
  end;

  PUpValue = ^TUpValue;
  TUpValue = record
    Index: Byte;
    isLocal: Boolean;
  end;

  PLoop = ^TLoop;
  TLoop = record
    // Index of the instruction that the loop should jump back to.
    Start: Integer;
    // Index of the argument for the op_Jump_If_False instruction used to exit the
    // loop. Stored so we can patch it once we know where the loop ends.
    ExitJump: Integer;
    // Depth of the scope(s) that need to be exited if a break is hit inside the loop.
    ScopeDepth: Integer;
    // The loop enclosing this one, or NULL if this is the outermost loop.
    Enclosing: PLoop;
    // True if break is used inside a loop
    BreakLoop: Boolean;
    // index of where to jump to when break is encountered
    BreakExit: Integer;
  end;

  PCompiler = ^TCompiler;
  TCompiler = record
    Enclosing: PCompiler;
    Func: PObjFunc;
    Form: TFuncForm;
    Locals: array[0..cByteMax] of TLocal;
    LocalCount: Integer;
    UpValues: array[0..cByteMax] of TUpValue;
    ScopeDepth: Integer;
    Loop: PLoop; // current innnermost loop being compiled or Nil if not in a loop
  end;

  PClassCompiler = ^TClassCompiler;
  TClassCompiler = record
    Enclosing: PClassCompiler;
    Name: TIdent;
    hasSuperClass: Boolean;
  end;

  { TCodeGenerator }

  TCodeGenerator = class(TVisitor)
    public
      constructor Create(Product: TProduct; var ADebugFile: Text);
      function Compile: PObjFunc;
      procedure MarkCompilerRoots;
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
      procedure VisitTTupleExpr(TupleExpr: TTupleExpr);
      procedure VisitTArrayExpr(ArrayExpr: TArrayExpr);
      procedure VisitTDictionaryExpr(DictionaryExpr: TDictionaryExpr);
      procedure VisitTIndexedExpr(IndexedExpr: TIndexedExpr);
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
      //procedure VisitTOperatorDecl(OperatorDecl: TOperatorDecl);
      procedure VisitTValDecl(ValDecl: TValDecl);
      procedure VisitTClassDecl(ClassDecl: TClassDecl);
      procedure VisitTTraitDecl(TraitDecl: TTraitDecl);
      procedure VisitTExtensionDecl(ExtensionDecl: TExtensionDecl);
      procedure VisitTEnumDecl(EnumDecl: TEnumDecl);
      // Blocks
      procedure VisitTBlock(Block: TBlock);
      procedure VisitTProduct(Product: TProduct);
    private
      AST: TProduct;  // tree containing the full program
      hasCompileError: Boolean; // stop execution if true
      DebugFile: Text; // used for debugging output

      CurrentLoc: TLocation; // keep track of runerror line and generated code lines
      Current: PCompiler;  // currently being compiled function
      CurrentClass: PClassCompiler; // currently being compiled class

      function FoldBinaryConstants(Left, Right: TNumberExpr; Op: TTokenTyp
        ): Boolean;
      function FoldUnaryConstants(Expr: TNumberExpr; Op: TTokenTyp): Boolean;
      procedure SetLocation(const NewLocation: TLocation);
      function MakeID(const Text: String): TIdent;

      procedure ErrorAt(const Location: TLocation; const Msg: String);
      procedure Error(const Msg: String);
      function CurrentChunk: PChunk;

      // compiler stuff
      procedure InitCompiler(var Compiler: TCompiler; const Form: TFuncForm;
        const FuncName: TIdent);
      function EndCompiler: PObjFunc;
      procedure CompileFunction(FuncDecl: TFuncDecl);
      procedure BeginScope;
      procedure EndScope;

      // code emitters
      procedure EmitByte(const AByte: Byte);
      procedure EmitBytes(const Opcode1, Opcode2: Byte);
      procedure Emit3Bytes(const Opcode1: Byte; const Address: UInt16);
      procedure EmitLoop(const LoopStart: Integer);
      function EmitJump(const Instruction: Byte): Integer;
      procedure EmitReturn;
      function MakeConstant(Value: TValue): UInt16;
      procedure EmitConstant(Value: TValue);
      procedure PatchJump(const OffSet: Integer);
      function IdentifierConstant(Name: TIdent): UInt16;

      // resolving locals and/or upvalues
      function IdentifiersEqual(a, b: TIdent): Boolean;
      procedure AddLocal(Name: TIdent);
      function AddNewLocal(Name: TIdent): Integer;
      procedure DeclareVariable(Name: TIdent);
      function MakeVariable(Name: TIdent): UInt16;
      procedure DefineVariable(const Global: UInt16);
      function ResolveLocal(Compiler: PCompiler; Name: TIdent): Integer;
      function AddUpValue(Compiler: PCompiler; Index: Byte; isLocal: Boolean): Integer;
      function ResolveUpValue(Compiler: PCompiler; Name: TIdent): Integer;
      function ScopedVariable(Name: TIdent; var getOp, setOp: Byte): Integer;
      procedure NamedVariable(Name: TIdent; const AssignOp: TTokenTyp; Expr: TExpr;
        const canAssign: Boolean);
      procedure MarkInitialized; // mark variable as initialized

      procedure CompileElseIfClauses(IfStmt: TIfStmt; const Address: Integer);
      procedure CallDeferred; // deferred statements
      function DiscardLocals(const Depth: Integer): Integer; // remove locals after break/continue
      // code for loops
      procedure EndLoop;
      procedure StartLoop(var Loop: TLoop);
      procedure TestLoopExit;
  end;

var
  CodeGenerator: TCodeGenerator;

function GenerateCode(Product: TProduct; var DebugFile: Text): PObjFunc;

implementation
uses uDebug, uMemory, uParser, uPrinter, math;

function GenerateCode(Product: TProduct; var DebugFile: Text): PObjFunc;
begin
  try
    CodeGenerator := TCodeGenerator.Create(Product, DebugFile);
    Result := CodeGenerator.Compile;
  finally
    CodeGenerator.Free;
  end;
end;

function TCodeGenerator.CurrentChunk: PChunk;
begin
  Result := @Current^.Func^.Chunk;
end;

procedure TCodeGenerator.SetLocation(const NewLocation: TLocation);
begin
  CurrentLoc := NewLocation;
end;



//=========================
// ERROR ROUTIUNES
//=========================

procedure TCodeGenerator.ErrorAt(const Location: TLocation; const Msg: String);
begin
  with Location do
    WriteFmt('[line %d, col %d] Code Generation Error', [Line, Col]);
  WriteLnFmt(': %s', [Msg]);
  hasCompileError := True;
end;

procedure TCodeGenerator.Error(const Msg: String);
begin
  ErrorAt(CurrentLoc, Msg);
end;

function TCodeGenerator.MakeID(const Text: String): TIdent;
begin
  Result := TIdent.Create(TToken.Create(
    ttIdentifier, Text, CurrentLoc));
end;

//=========================
// CODE GENERATION ROUTINES
//=========================

procedure TCodeGenerator.EmitByte(const AByte: Byte);
begin
  CurrentChunk^.Write(AByte, CurrentLoc.Line);
end;

procedure TCodeGenerator.EmitBytes(const Opcode1, Opcode2: Byte);
begin
  EmitByte(Opcode1);
  EmitByte(Opcode2);
end;

procedure TCodeGenerator.Emit3Bytes(const Opcode1: Byte; const Address: UInt16);
begin
  EmitByte(Opcode1);
  EmitByte(Hi(Address));
  EmitByte(Lo(Address));
end;

procedure TCodeGenerator.EmitLoop(const LoopStart: Integer);
var
  OffSet: Integer;
begin
  EmitByte(op_Loop);

  OffSet := CurrentChunk^.Count - LoopStart + 2;
  if OffSet > UInt16.MaxValue then
    Error('Loop body too large.');

  EmitByte((OffSet >> 8) and $FF); // >> is shr
  EmitByte(OffSet and $FF);
end;

function TCodeGenerator.EmitJump(const Instruction: Byte): Integer;
begin
  EmitByte(Instruction);
  EmitByte($FF);
  EmitByte($FF);
  Result := CurrentChunk^.Count-2;
end;

procedure TCodeGenerator.EmitReturn;
begin
  case Current^.Form of
    ffFuncArrow, ffAnonymArrow, ffMethodArrow, ffMethodVal, ffVal: Exit;
    ffInit: EmitBytes(op_Get_Local, 0);
    else
      EmitByte(op_Nil);
  end;

  EmitByte(op_Return);
end;

// max 65535 constants in 1 chunk
function TCodeGenerator.MakeConstant(Value: TValue): UInt16;
var
  Constant: UInt16;
begin
  Constant := CurrentChunk^.AddConstant(Value);
  if Constant = UInt16.MaxValue then
    begin
      Error('Too many constants in one chunk.');
      Exit(0);
    end;
  Result := Constant;
end;

procedure TCodeGenerator.EmitConstant(Value: TValue);
var
  Constant: UInt16;
begin
  Constant := MakeConstant(Value);
  Emit3Bytes(op_Constant, Constant); // Constant is 2 bytes
end;

procedure TCodeGenerator.PatchJump(const OffSet: Integer);
var
  Jump: Integer;
begin
  // -2 to adjust for the bytecode for the jump offset itself.
  Jump := CurrentChunk^.Count - OffSet - 2;

  if Jump > UINT16.MaxValue then
    Error('Too much code to jump over.');

  CurrentChunk^.Code[OffSet] := (Jump shr 8) and $FF;
  CurrentChunk^.Code[OffSet+1] := Jump and $FF;
end;


//====================
// COMPILER ROUTINES
//====================

procedure TCodeGenerator.InitCompiler(var Compiler: TCompiler; const Form: TFuncForm;
  const FuncName: TIdent);
var
  Local: PLocal;
begin
  Compiler.Enclosing := Current;
  Compiler.Func := Nil;
  Compiler.Form := Form;
  Compiler.LocalCount := 0;
  Compiler.ScopeDepth := 0;
  Compiler.Loop := Nil;
  Compiler.Func := NewFunc(Form=ffVal);
  Compiler.Func^.FileIndex := CurrentLoc.FileIndex;
  Current := @Compiler;

  //TFuncTyp = (ftAnonym, ftAnonymArrow, ftFuncArrow, ftFunc, ftInit,
  //            ftMethod, ftMethodArrow, ftScript, ftVal, ftMethodVal);
  case Form of
    ffFuncArrow, ffFunc, ffInit, ffMethod, ffMethodArrow, ffVal, ffMethodVal:
      Current^.Func^.Name := CopyString(FuncName.Text);
    ffAnonym, ffAnonymArrow: Current^.Func^.Name := CopyString('lambda');
  end;

  Local := @Current^.Locals[Current^.LocalCount];
  Inc(Current^.LocalCount);
  Local^.Depth := 0;
  Local^.isCaptured := False;

  if Form in [ffInit, ffMethod, ffMethodArrow, {ffScript,} ffMethodVal] then
    Local^.Name := TIdent.Create('self', CurrentLoc)
  else
    Local^.Name := TIdent.Create('', CurrentLoc);
end;

function TCodeGenerator.EndCompiler: PObjFunc;
var
  Func: PObjFunc;
  Chars: String = '<script>';
begin
  CallDeferred;
  EmitReturn;
  Func := Current^.Func;

  {$ifdef DEBUG_PRINT_CODE}
    if not hasCompileError then
      begin
        if Func^.Name <> Nil then
          Chars := Func^.Name^.Chars;
        DisassembleChunk(DebugFile, CurrentChunk, Chars);
      end;
  {$endif}

  Current := Current^.Enclosing;
  Result := Func;
end;

procedure TCodeGenerator.CallDeferred;
var
  Name: String;
  Count: Integer;
begin
  // add deferred call if any
  Count := Length(Current^.Func^.DeferList);
  while Count > 0 do
    begin
      Name := Current^.Func^.DeferList[Count-1];
      VisitProc(TVariable.Create(TIdent.Create(Name, CurrentLoc)));
      EmitBytes(op_Call, 0); // defer closure has 0 arguments
      Dec(Count);
    end;
end;

// removes any local variables after breaking out of a loop
function TCodeGenerator.DiscardLocals(const Depth: Integer): Integer;
var
  i: Integer;
begin
  i := Current^.LocalCount;
  while (i > 0) and (Current^.Locals[i-1].Depth > Depth) do
    begin
      // If the local was closed over, make sure the upvalue gets closed when it
      // goes out of scope on the stack. Otherwise pop the local.
      if Current^.Locals[i-1].isCaptured then
        EmitByte(op_Close_UpValue)
      else
        EmitByte(op_Pop);
      Dec(i);
    end;

  // number of local variables that were closed
  Result := Current^.LocalCount - i;
end;

// Marks the beginning of a loop. Keeps track of the current instruction so we
// know what to loop back to at the end of the body.
procedure TCodeGenerator.StartLoop(var Loop: TLoop);
begin
  Loop.Enclosing := Current^.Loop;
  Loop.Start := CurrentChunk^.Count;
  Loop.ScopeDepth := Current^.ScopeDepth;
  Loop.BreakExit := 0;
  Loop.BreakLoop := False;
  Loop.ExitJump := 0;
  Current^.Loop := @Loop; // address of Loop
end;

// Emits the [op_Jump_If_False] instruction used to test the loop condition and
// potentially exit the loop. Keeps track of the instruction so we can patch it
// later once we know where the end of the body is.
procedure TCodeGenerator.TestLoopExit;
begin
  Current^.Loop^.ExitJump := EmitJump(op_Jump_If_False_Pop);
end;

// Ends the current innermost loop. Patches up all jumps and breaks now that
// we know where the end of the loop is.
procedure TCodeGenerator.EndLoop;
begin
  EmitLoop(Current^.Loop^.Start);
  PatchJump(Current^.Loop^.ExitJump);

  // Find any break and patch it
  if Current^.Loop^.BreakLoop then
    begin
      PatchJump(Current^.Loop^.BreakExit);
      Current^.Loop^.BreakLoop := False;
    end;

  Current^.Loop := Current^.Loop^.Enclosing;
end;


procedure TCodeGenerator.BeginScope;
begin
  Inc(Current^.ScopeDepth);
end;

//procedure TCodeGenerator.EndScope;
//begin
//  Dec(Current^.ScopeDepth);
//
//  // Remove local variables from the stack
//  while (Current^.LocalCount > 0) and
//        (Current^.Locals[Current^.LocalCount-1].Depth > Current^.ScopeDepth) do
//    begin
//      if Current^.Locals[Current^.LocalCount-1].isCaptured then
//        EmitByte(op_Close_UpValue)
//      else
//        EmitByte(op_Pop);
//      Dec(Current^.LocalCount);
//    end;
//end;

procedure TCodeGenerator.EndScope;
var
  Removed: Integer;
begin
  Dec(Current^.ScopeDepth);

  // Remove local variables from the stack
  Removed := DiscardLocals(Current^.ScopeDepth);
  Dec(Current^.LocalCount, Removed);
end;


//
// IDENTIFIERS AND VARIABLES
//

function TCodeGenerator.IdentifierConstant(Name: TIdent): UInt16;
begin
  SetLocation(Name.Location);
  Result := MakeConstant(ObjVal(PObj(CopyString(Name.Text))));
end;

function TCodeGenerator.IdentifiersEqual(a, b: TIdent): Boolean;
begin
  if a.Text.Length <> b.Text.Length then Exit(False);
  Result := CompareStr(a.Text, b.Text) = 0;
end;

procedure TCodeGenerator.AddLocal(Name: TIdent);
var
  Local: PLocal;
begin
  if Current^.LocalCount = cByteCount then
    begin
      ErrorAt(Name.Location, 'Too many local variables in this scope.');
      Exit;
    end;

  Local := @Current^.Locals[Current^.LocalCount];
  Inc(Current^.LocalCount);
  Local^.Name := Name;
  Local^.Depth := -1;
  Local^.isCaptured := False;
end;

// add local var and return position in locals array. Initialize it as well.
function TCodeGenerator.AddNewLocal(Name: TIdent): Integer;
begin
  Result := Current^.LocalCount;
  AddLocal(Name);
  MarkInitialized;
end;


procedure TCodeGenerator.DeclareVariable(Name: TIdent);
var
  i: Integer;
  Local: PLocal;
begin
  // Global variables are implicitly declared.
  if Current^.ScopeDepth = 0 then Exit;

  for i := Current^.LocalCount-1 downto 0 do
    begin
      Local := @Current^.Locals[i];
      if (Local^.Depth <> -1) and (Local^.Depth < Current^.ScopeDepth) then
        Break;

      if IdentifiersEqual(Name, Local^.Name) then
        ErrorAt(Name.Location, 'Variable with this name already declared in this scope.');
    end;
  AddLocal(Name);
end;

function TCodeGenerator.MakeVariable(Name: TIdent): UInt16;
begin
  DeclareVariable(Name);
  if Current^.ScopeDepth > 0 then
    Result := 0
  else
    Result := IdentifierConstant(Name);
end;

procedure TCodeGenerator.MarkInitialized;
begin
  if Current^.ScopeDepth = 0 then Exit;
  Current^.Locals[Current^.LocalCount-1].Depth := Current^.ScopeDepth;
end;

procedure TCodeGenerator.DefineVariable(const Global: UInt16);
begin
  if Current^.ScopeDepth > 0 then
    begin
      MarkInitialized;
      Exit;
    end;
  Emit3Bytes(op_Define_Global, Global);
end;

function TCodeGenerator.ResolveLocal(Compiler: PCompiler; Name: TIdent): Integer;
var
  i: Integer;
  Local: PLocal;
begin
  for i := Compiler^.LocalCount-1 downto 0 do
    begin
      Local := @Compiler^.Locals[i];
      if IdentifiersEqual(Name, Local^.Name) then
        begin
          if Local^.Depth = -1 then
            ErrorAt(Name.Location, 'Cannot read local variable in its own initializer.');;
          Exit(i);
        end;
    end;
  Result := -1;
end;


function TCodeGenerator.AddUpValue(Compiler: PCompiler; Index: Byte; isLocal: Boolean): Integer;
var
  UpValueCount, i: Integer;
  UpValue: PUpValue;
begin
  UpValueCount := Compiler^.Func^.UpValueCount;

  for i:=0 to UpValueCount-1 do
    begin
      UpValue := @Compiler^.UpValues[i];
      if (UpValue^.Index = Index) and (UpValue^.isLocal = isLocal) then
        Exit(i);
    end;

  if UpValueCount = cByteCount then
    begin
      Error('Too many closure variables in function.');
      Exit(0);
    end;

  Compiler^.UpValues[UpValueCount].isLocal := isLocal;
  Compiler^.UpValues[UpValueCount].Index := Index;
  Result := Compiler^.Func^.UpValueCount;
  Inc(Compiler^.Func^.UpValueCount);
end;

function TCodeGenerator.ResolveUpValue(Compiler: PCompiler; Name: TIdent): Integer;
var
  Local, UpValue: Integer;
begin
  if Compiler^.Enclosing = Nil then Exit(-1);

  Local := ResolveLocal(Compiler^.Enclosing, Name);
  if Local <> -1 then
    begin
      Compiler^.Enclosing^.Locals[Local].isCaptured := True;
      Exit(AddUpValue(Compiler, Byte(Local), True));
    end;

  UpValue := ResolveUpValue(Compiler^.Enclosing, Name);
  if UpValue <> -1 then
    Exit(AddUpValue(Compiler, Byte(UpValue), False));

  Result := -1;
end;

function TCodeGenerator.ScopedVariable(Name: TIdent; var getOp, setOp: Byte): Integer;
var
  Arg: Integer;
begin
  Arg := ResolveLocal(Current, Name);
  if Arg <> -1 then
    begin
      getOp := op_Get_Local;
      setOp := op_Set_Local;
    end
  else
    begin
      Arg := ResolveUpvalue(Current, Name);
      if Arg <> -1 then
        begin
          getOp := op_Get_UpValue;
          setOp := op_Set_UpValue;
        end
      else
        begin
          Arg := IdentifierConstant(Name);
          getOp := op_Get_Global;
          setOp := op_Set_Global;
        end;
    end;
  Result := Arg;
end;

procedure TCodeGenerator.NamedVariable(Name: TIdent; const AssignOp: TTokenTyp;
  Expr: TExpr; const canAssign: Boolean);
var
  getOp, setOp: Byte;
  Arg: Integer;
  isGlobal: Boolean = False;
begin
  Arg := ScopedVariable(Name, getOp, setOp);
  isGlobal := getOp = op_Get_Global;

  // assignable? check which assignment operator
  if canAssign and (AssignOp in AssignSet) then
    begin
      if AssignOp = ttAssign then
        begin
          VisitProc(Expr);
          if isGlobal then
            Emit3Bytes(setOp, UInt16(Arg))
          else
            EmitBytes(setOp, UInt8(Arg));
        end
      else
        begin
          if isGlobal then
            Emit3Bytes(getOp, UInt16(Arg))
          else
            EmitBytes(getOp, UInt8(Arg));
          VisitProc(Expr);
          case AssignOp of
            ttPlusIs:    EmitByte(op_Add);
            ttMinusIs:   EmitByte(op_Subtract);
            ttStarIs:    EmitByte(op_Multiply);
            ttSlashIs:   EmitByte(op_Divide);
            ttPercentIs: EmitByte(op_Modulo);
          end;
          if isGlobal then
            Emit3Bytes(setOp, UInt16(Arg))
          else
            EmitBytes(setOp, UInt8(Arg));
        end;
    end
  else
    if isGlobal then
      Emit3Bytes(getOp, UInt16(Arg))
    else
      EmitBytes(getOp, UInt8(Arg));
end;


//
// CODE GENERATION
//

{ TCodeGenerator }

constructor TCodeGenerator.Create(Product: TProduct; var ADebugFile: Text);
begin
  DebugFile := ADebugFile;
  AST := Product;

  hasCompileError := False;
  SetLocation(TLocation.Create(0, 0));

  Current := Nil;
  CurrentClass := Nil;
end;

function TCodeGenerator.Compile: PObjFunc;
var
  Compiler: TCompiler;
  Func: PObjFunc;
  NoName: TIdent;
begin
  NoName := TIdent.Create(TToken.Create(ttNone, '', CurrentLoc));
  InitCompiler(Compiler, ffScript, NoName);  //main script doesn't have a name

  VisitProc(AST);  // generate code by visiting the tree

  Func :=  EndCompiler;

  if hasCompileError then
    Result := Nil
  else
    Result := Func;
end;

procedure TCodeGenerator.MarkCompilerRoots;
var
  Compiler: PCompiler;
begin
  Compiler := Current;
  while Compiler <> Nil do
    begin
      MarkObject(PObj(Compiler^.Func));
      Compiler := Compiler^.Enclosing;
    end;
end;

procedure TCodeGenerator.VisitTNode(Node: TNode);
begin
  // do nothing
end;

procedure TCodeGenerator.VisitTLiteralExpr(LiteralExpr: TLiteralExpr);
begin
  SetLocation(LiteralExpr.Location);
  case LiteralExpr.LiteralType of // ltNil, ltFalse, ltTrue
    ltNil: EmitByte(op_Nil);
    ltFalse: EmitByte(op_False);
    ltTrue: EmitByte(op_True);
  end;
end;

procedure TCodeGenerator.VisitTNumberExpr(NumberExpr: TNumberExpr);
begin
  SetLocation(NumberExpr.Location);
  with NumberExpr do
    if (Value>=0) and (Value<=5) then
      EmitByte(op_Constant0 + Round(Value))
    else
      EmitConstant(NumberVal(Value));
end;

procedure TCodeGenerator.VisitTStringExpr(StringExpr: TStringExpr);
begin
  SetLocation(StringExpr.Location);
  EmitConstant(ObjVal(PObj(CopyString(StringExpr.Value))));
end;

procedure TCodeGenerator.VisitTCharExpr(CharExpr: TCharExpr);
begin
  SetLocation(CharExpr.Location);
  EmitConstant(CharVal(CharExpr.Value));
end;

function TCodeGenerator.FoldUnaryConstants(Expr: TNumberExpr; Op: TTokenTyp): Boolean;
var
  e: Double;
begin
  e := Expr.Value;
  Result := True;
  case Op of
    ttExclamation: EmitConstant(NumberVal(not Trunc(e)));
    ttPlus:        EmitConstant(NumberVal(e));
    ttMinus:
      if e = 1 then
        EmitByte(op_ConstantMin1)
      else
        EmitConstant(NumberVal(-e));
    otherwise
      Result := False;
  end;
end;

procedure TCodeGenerator.VisitTUnaryExpr(UnaryExpr: TUnaryExpr);
var
  Folded: Boolean=False;
begin
  SetLocation(UnaryExpr.Location);

  if UnaryExpr.Expr is TNumberExpr then
    with UnaryExpr do
      Folded := FoldUnaryConstants(Expr as TNumberExpr, Op);

  if not Folded then
    begin
      VisitProc(UnaryExpr.Expr);
      case UnaryExpr.Op of
        ttQuestion: EmitByte(op_Question);
        ttExclamation: EmitByte(op_Bitwise_Not);
        ttMinus: EmitByte(op_Negate);
        ttPlus: begin {do nothing} end;
        ttNot: EmitByte(op_Not);
        else
          Exit; // unreachable
      end;
    end;
end;

function TCodeGenerator.FoldBinaryConstants(Left, Right: TNumberExpr;
  Op: TTokenTyp): Boolean;
var
  a, b: Double;
begin
  a := Left.Value;
  b := Right.Value;
  Result := True;
  case Op of
    ttPlus:        EmitConstant(NumberVal(a + b));
    ttMinus:       EmitConstant(NumberVal(a - b));
    ttStar:        EmitConstant(NumberVal(a * b));
    ttSlash:       EmitConstant(NumberVal(a / b));
    ttPercent:     EmitConstant(NumberVal(a-(b*Int(a/b))));
    ttShl:         EmitConstant(NumberVal(Trunc(a) shl Trunc(b)));
    ttShr:         EmitConstant(NumberVal(Trunc(a) shr Trunc(b)));
    ttCaret:       EmitConstant(NumberVal(Power(a, b)));
    ttEQ:          EmitConstant(BoolVal(a = b));
    ttNEQ:         EmitConstant(BoolVal(a <> b));
    ttGT:          EmitConstant(BoolVal(a > b));
    ttGE:          EmitConstant(BoolVal(a >= b));
    ttLT:          EmitConstant(BoolVal(a < b));
    ttLE:          EmitConstant(BoolVal(a <= b));
    ttAmpersand:   EmitConstant(NumberVal(Trunc(a) and Trunc(b)));
    ttVertBar:     EmitConstant(NumberVal(Trunc(a) or Trunc(b)));
    ttTilde:       EmitConstant(NumberVal(Trunc(a) xor Trunc(b)));
    otherwise
      Result := False;
  end;
end;

procedure TCodeGenerator.VisitTBinaryExpr(BinaryExpr: TBinaryExpr);
var
  Folded: Boolean=False;
begin
  SetLocation(BinaryExpr.Location);

  if (BinaryExpr.Left is TNumberExpr) and (BinaryExpr.Right is TNumberExpr) then
    with BinaryExpr do
      Folded := FoldBinaryConstants(Left as TNumberExpr, Right as TNumberExpr, Op);

  if not Folded then
    begin
      VisitProc(BinaryExpr.Left);
      VisitProc(BinaryExpr.Right);
      // Emit the operator instruction.
      case BinaryExpr.Op of
        ttPlus:        EmitByte(op_Add);
        ttMinus:       EmitByte(op_Subtract);
        ttStar:        EmitByte(op_Multiply);
        ttSlash:       EmitByte(op_Divide);
        ttPercent:     EmitByte(op_Modulo);
        ttShl:         EmitByte(op_Shift_Left);
        ttShr:         EmitByte(op_Shift_Right);
        ttCaret:       EmitByte(op_Power);
        ttEQ:          EmitByte(op_Equal);
        ttNEQ:         EmitByte(op_NotEqual);
        ttGT:          EmitByte(op_Greater);
        ttGE:          EmitByte(op_GreaterEqual);
        ttLT:          EmitByte(op_Less);
        ttLE:          EmitByte(op_LessEqual);
        ttIn:          EmitByte(op_ElementOf);
        ttNotIn:       begin EmitByte(op_ElementOf); EmitByte(op_Not); end;
        ttIs:          EmitByte(op_InstanceOf);
        ttLeftArrow:   EmitByte(op_Apply);
        ttAmpersand:   EmitByte(op_Bitwise_And);
        ttVertBar:     EmitByte(op_Bitwise_Or);
        ttTilde:       EmitByte(op_Bitwise_XOr);
        tt2Questions:  EmitByte(op_2Questions);
        else
          Exit;  // unreachable
      end;
    end;
end;

procedure TCodeGenerator.VisitTAndExpr(AndExpr: TAndExpr);
var
  EndJump: Integer;
begin
  SetLocation(AndExpr.Location);

  VisitProc(AndExpr.Left);                   // evaluate left side
  EndJump := EmitJump(op_Jump_If_False);     // if false then don't evaluate right side
  EmitByte(op_Pop);
  // if left side = true then Pop and evaluate right side as well
  VisitProc(AndExpr.Right); // right side will have the result

  PatchJump(EndJump);
end;

procedure TCodeGenerator.VisitTOrExpr(OrExpr: TOrExpr);
var
  ElseJump, EndJump: Integer;
begin
  SetLocation(OrExpr.Location);

  VisitProc(OrExpr.Left);
  ElseJump := EmitJump(op_Jump_If_False);
  EndJump := EmitJump(op_Jump);   // if left side = true then skip right side

  PatchJump(ElseJump);  // left side is false so evaluate right side
  EmitByte(op_Pop);

  VisitProc(OrExpr.Right);
  PatchJump(EndJump);
end;

procedure TCodeGenerator.VisitTTernaryExpr(TernaryExpr: TTernaryExpr);
var
  IfJump, ElseJump: Integer;
begin
  SetLocation(TernaryExpr.Location);

  VisitProc(TernaryExpr.Condition);
  // Jump to else branch if condition is false
  IfJump := EmitJump(op_Jump_If_False_Pop);

  // Compile the then branch.
  VisitProc(TernaryExpr.TrueExpr);

  // Jump over the else branch when the if branch is taken.
  ElseJump := EmitJump(op_Jump);

  // Compile the else branch.
  PatchJump(IfJump);
  VisitProc(TernaryExpr.FalseExpr);

  // Patch the jump over the else.
  PatchJump(ElseJump);
end;

procedure TCodeGenerator.VisitTIdent(Ident: TIdent);
begin
  // do nothing
end;

procedure TCodeGenerator.VisitTVariable(Variable: TVariable);
begin
  SetLocation(Variable.Location);
  NamedVariable(Variable.Name, ttNone, Nil, False);
end;

procedure TCodeGenerator.VisitTCallExpr(CallExpr: TCallExpr);
var
  Argument: TArgument;
begin
  SetLocation(CallExpr.Location);
  if CallExpr.IsClassInit then
    VisitProc(CallExpr.Callee)
  else
    VisitProc(CallExpr.Signature);

  for Argument in CallExpr.Arguments do
    VisitProc(Argument.Expr);
  EmitBytes(op_Call, CallExpr.ArgCount);
end;

procedure TCodeGenerator.VisitTMatchExpr(MatchExpr: TMatchExpr);
var
  Key: TBinaryExpr;
  IfEnds: array[0..cMaxCases-1] of Integer;
  IfCount: Integer = 0;
  PreviousIfSkip: Integer = -1;
  i: Integer;
begin
  SetLocation(MatchExpr.Location);

  for Key in MatchExpr.IfLimbs.Keys do
    begin
      VisitProc(Key);

      PreviousIfSkip := EmitJump(op_Jump_If_False); // jump to next match key

      // Pop the comparison result.
      EmitByte(op_Pop);

      VisitProc(MatchExpr.IfLimbs[Key]); // execute return expression

      // At the end of the previous if, jump over the others.
      IfEnds[IfCount] := EmitJump(op_Jump); // jump to end of switch stmt
      Inc(IfCount);

      // Patch its condition to jump to the next case (this one).
      PatchJump(PreviousIfSkip);
      EmitByte(op_Pop);
    end;

  // else clause
  VisitProc(MatchExpr.ElseLimb);
  // Patch all the if jumps to the end.
  for i := 0 to IfCount-1 do
    PatchJump(IfEnds[i]);
end;

procedure TCodeGenerator.VisitTSelfExpr(SelfExpr: TSelfExpr);
begin
  SetLocation(SelfExpr.Location);
  //if CurrentClass = Nil then
  //  begin
  //    ErrorAt(SelfExpr.Location, 'Cannot use "self" outside of a class.');
  //    Exit;
  //  end;

  NamedVariable(SelfExpr.Name, ttNone, Nil, False);
end;

procedure TCodeGenerator.VisitTInheritedExpr(InheritedExpr: TInheritedExpr);
var
  Name: UInt16;
  ArgCount: Byte;
  Argument: TExpr;
begin
  SetLocation(InheritedExpr.Location);
  if CurrentClass = Nil then
    ErrorAt(InheritedExpr.Location, 'Cannot use "inherited" outside of a class.')
  else if not CurrentClass^.hasSuperClass then
    ErrorAt(InheritedExpr.Location, 'Cannot use "inherited" in a class without parent class.');

  Name := IdentifierConstant(InheritedExpr.Method.Name);

  NamedVariable(TIdent.Create('self', CurrentLoc), ttNone, Nil, False);

  if InheritedExpr.ArgCount >= 0 then
    begin
      for Argument in InheritedExpr.Arguments do
        VisitProc(Argument);
      ArgCount := InheritedExpr.ArgCount;
      NamedVariable(TIdent.Create('inherited', CurrentLoc), ttNone, Nil, False);
      Emit3Bytes(op_Super_Invoke, Name);
      EmitByte(ArgCount);
    end
  else
    begin
      NamedVariable(TIdent.Create('inherited', CurrentLoc), ttNone, Nil, False);
      Emit3Bytes(op_Get_Super, Name);
    end;
end;

procedure TCodeGenerator.VisitTGetExpr(GetExpr: TGetExpr);
var
  Member: UInt16;
begin
  SetLocation(GetExpr.Location);
  VisitProc(GetExpr.Instance);

  Member := IdentifierConstant(TVariable(GetExpr.Member).Name);
  if GetExpr.SafetyOn then
    Emit3Bytes(op_Get_Property_Nil, Member)
  else
    Emit3Bytes(op_Get_Property, Member);
end;


procedure TCodeGenerator.VisitTLambdaExpr(LambdaExpr: TLambdaExpr);
begin
  SetLocation(LambdaExpr.Location);
  CompileFunction(LambdaExpr.Func);
end;

procedure TCodeGenerator.VisitTTupleExpr(TupleExpr: TTupleExpr);
var
  Element: TArgument;
begin
  SetLocation(TupleExpr.Location);
  // the arguments are the elements
  for Element in TupleExpr.Elements do
    begin
      EmitConstant(ObjVal(PObj(CopyString(Element.Name.Text))));
      VisitProc(Element.Expr);
    end;
  EmitBytes(op_Tuple, Byte(TupleExpr.Elements.Count));
end;

procedure TCodeGenerator.VisitTArrayExpr(ArrayExpr: TArrayExpr);
var
  Element: TExpr;
begin
  SetLocation(ArrayExpr.Location);

  // the arguments are the elements
  for Element in ArrayExpr.Elements do
    VisitProc(Element);
  EmitBytes(op_Array, Byte(ArrayExpr.Elements.Count));
end;

procedure TCodeGenerator.VisitTDictionaryExpr(DictionaryExpr: TDictionaryExpr);
var
  Key: TExpr;
begin
  SetLocation(DictionaryExpr.Location);

  // the arguments are the elements, first the value then the key
  for Key in DictionaryExpr.Elements.Keys do
    begin
      VisitProc(Key);
      VisitProc(DictionaryExpr.Elements[Key]);
    end;

  EmitBytes(op_Dictionary, Byte(DictionaryExpr.Elements.Count));
end;

procedure TCodeGenerator.VisitTIndexedExpr(IndexedExpr: TIndexedExpr);
begin
  SetLocation(IndexedExpr.Location);

  // the array, dictionary, string or indexed class
  VisitProc(IndexedExpr.Variable);
  // index or key
  VisitProc(IndexedExpr.Index);

  EmitByte(op_Index_Get);
end;

procedure TCodeGenerator.VisitTSetExpr(SetExpr: TSetExpr);
var
  Element: TExpr;
begin
  SetLocation(SetExpr.Location);

  // the arguments are the elements
  for Element in SetExpr.Elements do
    VisitProc(Element);

  EmitBytes(op_Set, Byte(SetExpr.Elements.Count));
end;

procedure TCodeGenerator.VisitTRangeExpr(RangeExpr: TRangeExpr);
begin
  SetLocation(RangeExpr.Location);

  // in reverse order
  if RangeExpr.IsInclusive then
    EmitByte(op_True)
  else
    EmitByte(op_False);

  VisitProc(RangeExpr.UpTo);
  VisitProc(RangeExpr.From);

  EmitByte(op_Range);
end;

procedure TCodeGenerator.VisitTListBuilderExpr(ListBuilderExpr: TListBuilderExpr);
begin
  SetLocation(ListBuilderExpr.Location);

  // create callee listBuilder
  VisitProc(TVariable.Create(MakeID('listBuilder')));

  // first argument is an empty array or set
  if ListBuilderExpr.Typ = lbtArray then
    EmitBytes(op_Array, 0) // create an empty array
  else
    EmitBytes(op_Set, 0); // create an empty set

  // the map argument
  VisitProc(ListBuilderExpr.Map);
  // the sequence argument
  VisitProc(ListBuilderExpr.Sequence);
  // the filter argument
  VisitProc(ListBuilderExpr.Filter);

  // call func listBuilder with 4 arguments
  EmitBytes(op_Call, 4);
end;


//procedure TCodeGenerator.VisitTListBuilderExpr(ListBuilderExpr: TListBuilderExpr);
//var
//  IterSlot, ResultSlot, SkipLoop, TransformSlot, LoopvarSlot: Integer;
//  Loop: TLoop;
//begin
//  SetLocation(ListBuilderExpr.Location);
//
//  // Create a scope for the hidden local variables used for the iterator.
//  BeginScope;
//  // first hidden variable is the empty result array or set based on type
//  if ListBuilderExpr.Typ = lbtArray then
//    EmitBytes(op_Array, 0) // create an empty array
//  else
//    EmitBytes(op_Set, 0); // create an empty set
//
//  ResultSlot := AddNewLocal(MakeID('result$')); // store in local var result$
//
//  BeginScope;
//  VisitProc(ListBuilderExpr.Transform); // call transform function
//  TransformSlot := 2;
//
//  // create a local variable 'iterator', which represents the sequence.iterator property
//  VisitProc(ListBuilderExpr.Sequence); // evaluate sequence expr and put on the stack
//  Emit3Bytes(op_Get_Property, MakeConstant(StringVal('iterator'))); // get iterator property
//  IterSlot := AddNewLocal(MakeID('iter$')); // add '$' so that it can't collide with user-defined
//
//  StartLoop(Loop);
//
//  EmitBytes(op_Get_Local, IterSlot); // retrieve the iterator object
//  Emit3Bytes(op_Get_Property, MakeConstant(StringVal('moveNext'))); // advance iterator
//
//  TestLoopExit; // check if moveNext returns false
//
//  // Retrieve current value of the iterator
//  EmitBytes(op_Get_Local, IterSlot);
//  Emit3Bytes(op_Get_Property, MakeConstant(StringVal('current'))); // iterator value
//
//  // the loop variable gets its own scope
//  BeginScope;
//  LoopvarSlot := AddNewLocal(ListBuilderExpr.LoopVar); // bind current value to the Loop variable
//
//  // check condition if not nil
//  if Assigned(ListBuilderExpr.Condition) then
//    begin
//      VisitProc(ListBuilderExpr.Condition);
//      SkipLoop := EmitJump(op_Jump_If_False_Pop);
//    end;
//
//  // call add function
//  EmitBytes(op_Get_Local, ResultSlot); // get result instance
//  Emit3Bytes(op_Get_Property, MakeConstant(StringVal('add'))); // add method
//
//  //VisitProc(ListBuilderExpr.Transform); // call transform function
//  EmitBytes(op_Get_Local, TransformSlot);
//  EmitBytes(op_Get_Local, LoopvarSlot);
//  EmitBytes(op_Call, 1);
//  EmitBytes(op_Set_Local, ResultSlot);
//
//  EmitBytes(op_Call, 1);  // call 'add' with 1 argument, i.e. the transformed loop var
//  EmitByte(op_Pop);
//
//  if Assigned(ListBuilderExpr.Condition) then
//    PatchJump(SkipLoop);
//
//  EndScope;  // loop variable is released again
//  EndLoop;
//
//  EndScope;  // release hidden variables
//
//  Dec(Current^.ScopeDepth);
//  Dec(Current^.LocalCount);
//
////
////  EmitByte(op_Dup); // make duplicate so that the result stays on the stack
////  EndScope;  // release hidden variables
//end;


procedure TCodeGenerator.VisitTExprStmt(ExprStmt: TExprStmt);
begin
  SetLocation(ExprStmt.Location);
  VisitProc(ExprStmt.Expr);
  EmitByte(op_Pop);
end;

procedure TCodeGenerator.VisitTPrintStmt(PrintStmt: TPrintStmt);
var
  Expr: TExpr;
begin
  SetLocation(PrintStmt.Location);
  for Expr in PrintStmt.ExprList do
    begin
      VisitProc(Expr);
      EmitByte(op_Print);
    end;
  VisitProc(PrintStmt.Terminator);
  EmitByte(op_Print);
end;

procedure TCodeGenerator.VisitTAssignStmt(AssignStmt: TAssignStmt);
begin
  SetLocation(AssignStmt.Location);
  NamedVariable(AssignStmt.Variable.Name, AssignStmt.AssignOp, AssignStmt.Expr, True);
  EmitByte(op_Pop);
end;

procedure TCodeGenerator.VisitTSetStmt(SetStmt: TSetStmt);
var
  Member: UInt16;
begin
  SetLocation(SetStmt.Location);
  VisitProc(SetStmt.GetExpr.Instance);
  Member := IdentifierConstant(TVariable(SetStmt.GetExpr.Member).Name);

  if SetStmt.Op in [ttPlusIs, ttMinusIs, ttStarIs, ttSlashIs] then
    begin
      VisitProc(SetStmt.GetExpr.Instance);
      Emit3Bytes(op_Get_Property, Member);
      VisitProc(SetStmt.Expr);
      case SetStmt.Op of
        ttPlusIs:  EmitByte(op_Add);
        ttMinusIs: EmitByte(op_Subtract);
        ttStarIs:  EmitByte(op_Multiply);
        ttSlashIs: EmitByte(op_Divide);
      end;
      Emit3Bytes(op_Set_Property, Member);
    end
  else
    begin
      VisitProc(SetStmt.Expr);
      Emit3Bytes(op_Set_Property, Member);
    end;
end;

procedure TCodeGenerator.VisitTIndexedStmt(IndexedStmt: TIndexedStmt);
begin
  SetLocation(IndexedStmt.Location);

  // the array, dictionary, string or indexed class
  VisitProc(IndexedStmt.IndexedExpr.Variable);
  // index or key
  VisitProc(IndexedStmt.IndexedExpr.Index);
  // value to add to var[index]
  VisitProc(IndexedStmt.Expr);

  EmitByte(op_Index_Set);
end;

procedure TCodeGenerator.VisitTCallExprStmt(CallExprStmt: TCallExprStmt);
begin
  SetLocation(CallExprStmt.Location);
  VisitProc(CallExprStmt.CallExpr);
  EmitByte(op_Pop);
end;

procedure TCodeGenerator.VisitTReturnStmt(ReturnStmt: TReturnStmt);
begin
  SetLocation(ReturnStmt.Location);

  CallDeferred;   // add deferred call if any

  if Assigned(ReturnStmt.Expr) then
    VisitProc(ReturnStmt.Expr)
  else
    // it's a return without expression, so return nil
    EmitByte(op_Nil);

  EmitByte(op_Return);
end;

procedure TCodeGenerator.CompileElseIfClauses(IfStmt: TIfStmt; const Address: Integer);
var
  ElseIfJump, i: Integer;
  ElseIfs: array[0..cMaxCases-1] of Integer;
  Count: Integer = 0;
  ElseIfItem: TElseIfItem;
begin
  ElseIfs[Count] := EmitJump(op_Jump);
  PatchJump(Address);
  for ElseIfItem in IfStmt.ElseIfList do
    begin
      SetLocation(ElseIfItem.Condition.Location);

      VisitProc(ElseIfItem.Condition);
      ElseIfJump := EmitJump(op_Jump_If_False_Pop);

      VisitProc(ElseIfItem.Block);

      Inc(Count);
      if Count >= cMaxCases then
        Error('Maximum number "elseif" cases exceeded.');
      ElseIfs[Count] := EmitJump(op_Jump);

      PatchJump(ElseIfJump);
    end;

  if Assigned(IfStmt.ElsePart) then
    VisitProc(IfStmt.ElsePart);

  for i := 0 to Count do
    PatchJump(ElseIfs[i]);
end;

procedure TCodeGenerator.VisitTIfStmt(IfStmt: TIfStmt);
var
  ThenJump, ElseJump: Integer;
begin
  SetLocation(IfStmt.Location);

  if Assigned(IfStmt.VarDecl) then
    begin
      BeginScope;
      VisitProc(IfStmt.VarDecl);
    end;

  VisitProc(IfStmt.Condition);
  ThenJump := EmitJump(op_Jump_If_False_Pop);

  VisitProc(IfStmt.ThenPart);

  if Assigned(IfStmt.ElseIfList) then
    CompileElseIfClauses(IfStmt, ThenJump)
  else if Assigned(IfStmt.ElsePart) then
    begin
      ElseJump := EmitJump(op_Jump);
      PatchJump(ThenJump);
      VisitProc(IfStmt.ElsePart);
      PatchJump(ElseJump);
    end
  else
    PatchJump(ThenJump);

  if Assigned(IfStmt.VarDecl) then
    EndScope;
end;

//procedure TCodeGenerator.VisitTWhileStmt(WhileStmt: TWhileStmt);
//var
//  SurroundingLoopStart,
//  SurroundingLoopScopeDepth: Integer;
//  ExitJump: Integer;
//begin
//  SetLocation(WhileStmt.Location);
//
//  if Assigned(WhileStmt.VarDecl) then
//    begin
//      BeginScope;
//      VisitProc(WhileStmt.VarDecl);
//    end;
//
//  SurroundingLoopStart := InnermostLoopStart;
//  SurroundingLoopScopeDepth := InnermostLoopScopeDepth;
//  InnermostLoopStart := CurrentChunk^.Count;
//  InnermostLoopScopeDepth := Current^.ScopeDepth;
//
//  VisitProc(WhileStmt.Condition);
//  ExitJump := EmitJump(op_Jump_If_False_Pop);
//
//  VisitProc(WhileStmt.Block);
//
//  EmitLoop(InnermostLoopStart);
//  PatchJump(ExitJump);
//
//  if BreakLoop then
//    begin
//      PatchJump(BreakExit);
//      BreakLoop := False;
//    end;
//
//  InnermostLoopStart := SurroundingLoopStart;
//  InnermostLoopScopeDepth := SurroundingLoopScopeDepth;
//
//  if Assigned(WhileStmt.VarDecl) then
//    EndScope;
//end;

procedure TCodeGenerator.VisitTWhileStmt(WhileStmt: TWhileStmt);
var
  Loop: TLoop;
begin
  SetLocation(WhileStmt.Location);

  if Assigned(WhileStmt.VarDecl) then
    begin
      BeginScope;
      VisitProc(WhileStmt.VarDecl);
    end;

  StartLoop(Loop);

  VisitProc(WhileStmt.Condition);
  TestLoopExit;

  VisitProc(WhileStmt.Block);

  EndLoop;

  if Assigned(WhileStmt.VarDecl) then
    EndScope;
end;

//procedure TCodeGenerator.VisitTForStmt(ForStmt: TForStmt);
//var
//  SurroundingLoopStart,
//  SurroundingLoopScopeDepth: Integer;
//  ExitJump, IteratorStart, BodyJump: Integer;
//begin
//  SetLocation(ForStmt.Location);
//  BeginScope;
//  VisitProc(ForStmt.VarDecl);
//
//  SurroundingLoopStart := InnermostLoopStart;
//  SurroundingLoopScopeDepth := InnermostLoopScopeDepth;
//  InnermostLoopStart := CurrentChunk^.Count;
//  InnermostLoopScopeDepth := Current^.ScopeDepth;
//
//  VisitProc(ForStmt.Condition);
//  ExitJump := EmitJump(op_Jump_If_False_Pop);
//
//  BodyJump := EmitJump(op_Jump);
//  IteratorStart := CurrentChunk^.Count;
//  VisitProc(ForStmt.Iterator);
//  EmitLoop(InnermostLoopStart);
//  InnermostLoopStart := IteratorStart;
//
//  PatchJump(BodyJump);
//  VisitProc(ForStmt.Block);
//
//  EmitLoop(InnermostLoopStart);
//  PatchJump(ExitJump);
//
//  if BreakLoop then
//    begin
//      PatchJump(BreakExit);
//      BreakLoop := False;
//    end;
//
//  InnermostLoopStart := SurroundingLoopStart;
//  InnermostLoopScopeDepth := SurroundingLoopScopeDepth;
//
//  EndScope;
//end;

procedure TCodeGenerator.VisitTForStmt(ForStmt: TForStmt);
var
  Loop: TLoop;
  IteratorStart: LongInt;
  BodyJump: Integer;
begin
  SetLocation(ForStmt.Location);
  BeginScope;
  VisitProc(ForStmt.VarDecl);

  StartLoop(Loop);
  VisitProc(ForStmt.Condition);
  TestLoopExit;

  BodyJump := EmitJump(op_Jump);
  IteratorStart := CurrentChunk^.Count;
  VisitProc(ForStmt.Iterator);
  EmitLoop(Loop.Start);
  Loop.Start := IteratorStart;

  PatchJump(BodyJump);
  VisitProc(ForStmt.Block);

  EndLoop;

  EndScope;
end;

procedure TCodeGenerator.VisitTForInStmt(ForInStmt: TForInStmt);
var
  IterSlot , SkipLoop: Integer;
  Loop: TLoop;
begin
  SetLocation(ForInStmt.Location);

  // Create a scope for the hidden local variable used for the iterator.
  BeginScope;

  // create a local variable 'iterator', which represents the sequence.iterator property
  VisitProc(ForInStmt.Sequence); // evaluate sequence expr and put on the stack
  Emit3Bytes(op_Get_Property, MakeConstant(StringVal('iterator'))); // get iterator property
  IterSlot := AddNewLocal(MakeID('iter$')); // add '$' so that it can't collide with user-defined

  StartLoop(Loop);

  EmitBytes(op_Get_Local, IterSlot); // retrieve the iterator object
  Emit3Bytes(op_Get_Property, MakeConstant(StringVal('moveNext'))); // advance iterator

  TestLoopExit; // check if moveNext returns false

  // Retrieve current value of the iterator
  EmitBytes(op_Get_Local, IterSlot);
  Emit3Bytes(op_Get_Property, MakeConstant(StringVal('current'))); // iterator value

  // the loop variable gets its own scope
  BeginScope;
  AddNewLocal(ForInStmt.LoopVar); // bind current value to the Loop variable

  // check where-expr if not nil then evaluate and put jump if false
  if Assigned(ForInStmt.Where) then
    begin
      VisitProc(ForInStmt.Where);
      SkipLoop := EmitJump(op_Jump_If_False_Pop);
    end;

  VisitProc(ForInStmt.Block);   // evaluate the loop's block

  if Assigned(ForInStmt.Where) then
    PatchJump(SkipLoop);

  EndScope;  // loop variable is released again

  EndLoop;
  EndScope;  // release hidden variable iter$
end;

procedure TCodeGenerator.VisitTRepeatStmt(RepeatStmt: TRepeatStmt);
var
  LoopStart, ExitJump: Integer;
begin
  SetLocation(RepeatStmt.Location);
  LoopStart := CurrentChunk^.Count;
  VisitProc(RepeatStmt.Block);
  VisitProc(RepeatStmt.Condition);
  ExitJump := EmitJump(op_Jump_If_False);
  EmitByte(op_Pop);
  EmitLoop(LoopStart);
  PatchJump(ExitJump);
  EmitByte(op_Pop);
end;

procedure TCodeGenerator.VisitTLoopDoStmt(LoopDoStmt: TLoopDoStmt);
var
  LoopStart, i: Integer;
  ConditionalBlock: TConditionalBlock;
  ExitJumps: array[0..cMaxExits-1] of Integer;
  ExitCount: Integer = 0;
begin
  SetLocation(LoopDoStmt.Location);

  if Assigned(LoopDoStmt.VarDecls) then
    begin
      BeginScope;
      VisitProc(LoopDoStmt.VarDecls);
    end;

  LoopStart := CurrentChunk^.Count;

  VisitProc(LoopDoStmt.Block);

  for ConditionalBlock in LoopDoStmt.ConditionalBlocks do
    begin
      VisitProc(ConditionalBlock.Condition);
      EmitByte(op_Not);
      ExitJumps[ExitCount] := EmitJump(op_Jump_If_False_Pop); // jump to end of loop stmt
      Inc(ExitCount);

      if Assigned(ConditionalBlock.Block) then
        VisitProc(ConditionalBlock.Block);
    end;

  EmitLoop(LoopStart);

  for i := 0 to ExitCount-1 do
    PatchJump(ExitJumps[i]);

  if Assigned(LoopDoStmt.VarDecls) then
    EndScope;
end;

procedure TCodeGenerator.VisitTEnsureStmt(EnsureStmt: TEnsureStmt);
var
  OKJump: Integer;
begin
  SetLocation(EnsureStmt.Location);

  if Assigned(EnsureStmt.VarDecl) then
    VisitProc(EnsureStmt.VarDecl);

  VisitProc(EnsureStmt.Condition);
  EmitByte(op_Not); // test for inequality

  OKJump := EmitJump(op_Jump_If_False);
  EmitByte(op_Pop);
  // if not ok then execute else part
  VisitProc(EnsureStmt.ElsePart);

  PatchJump(OKJump);
  EmitByte(op_Pop);
end;

procedure TCodeGenerator.VisitTSwitchStmt(SwitchStmt: TSwitchStmt);
var
  Key: TCaseItem;
  CaseEnds: array[0..cMaxCases-1] of Integer;
  CaseCount: Integer = 0;
  PreviousCaseSkip: Integer = -1;
  i: Integer;
  Callee: TVariable;
  Enum: UInt16;
begin
  SetLocation(SwitchStmt.Location);

  VisitProc(SwitchStmt.Expr); // load case expression

  for Key in SwitchStmt.Cases.Keys do
    begin
      // See if the case is equal to the value.
      EmitByte(op_Dup); // duplicate case expression

      if Key.Typ = citEnum then
        begin
          // callee returns the type of the enum instance
          Callee := TVariable.Create(TIdent.Create('enumTypeOf', SwitchStmt.Location));
          VisitProc(Callee);
          // the argument is the enum instance
          VisitProc(SwitchStmt.Expr);
          EmitBytes(op_Call, 1);

          Enum := IdentifierConstant(TVariable(Key.Expr).Name);
          Emit3Bytes(op_Get_Property, Enum);
        end
      else
        VisitProc(Key.Expr);  // load the case key

      if Key.Typ = citObject then
        EmitByte(op_InstanceOf)
      else if Key.Typ = citElemOf then
        EmitByte(op_ElementOf)
      else
        EmitByte(op_Equal);

      PreviousCaseSkip := EmitJump(op_Jump_If_False); // jump to next case key

      // Pop the comparison result.
      EmitByte(op_Pop);

      VisitProc(SwitchStmt.Cases[Key]); // execute statement block of the case

      // At the end of the previous case, jump over the others.
      CaseEnds[CaseCount] := EmitJump(op_Jump); // jump to end of switch stmt
      Inc(CaseCount);

      // Patch its condition to jump to the next case (this one).
      PatchJump(PreviousCaseSkip);
      EmitByte(op_Pop);
    end;

  // default case
  VisitProc(SwitchStmt.DefaultCase);
  // Patch all the case jumps to the end.
  for i := 0 to CaseCount-1 do
    PatchJump(CaseEnds[i]);

  EmitByte(op_Pop); // remove initial case expression
end;

procedure TCodeGenerator.VisitTBreakStmt(BreakStmt: TBreakStmt);
begin
  SetLocation(BreakStmt.Location);
  if Current^.Loop = Nil then
    ErrorAt(BreakStmt.Location, 'Cannot use "break" outside of a loop.');

  Current^.Loop^.BreakLoop := True;

  // Discard any locals created inside the loop.
  DiscardLocals(Current^.Loop^.ScopeDepth);

  // Jump to past end of current innermost loop.
  Current^.Loop^.BreakExit := EmitJump(op_Jump);
end;

procedure TCodeGenerator.VisitTContinueStmt(ContinueStmt: TContinueStmt);
var
  i: Integer;
begin
  SetLocation(ContinueStmt.Location);
  if Current^.Loop = Nil then
    ErrorAt(ContinueStmt.Location, 'Cannot use "continue" outside of a loop.');

  // Discard any locals created inside the loop.
  DiscardLocals(Current^.Loop^.ScopeDepth);

  // Jump to top of current innermost loop.
  EmitLoop(Current^.Loop^.Start);
end;

//procedure TCodeGenerator.VisitTBreakStmt(BreakStmt: TBreakStmt);
//var
//  i: Integer;
//begin
//  SetLocation(BreakStmt.Location);
//  if InnermostLoopStart = -1 then
//    ErrorAt(BreakStmt.Location, 'Cannot use "break" outside of a loop.');
//
//  BreakLoop := True;
//
//  // Discard any locals created inside the loop.
//  i := Current^.LocalCount - 1;
//  while (i >= 0) and (Current^.Locals[i].Depth > InnermostLoopScopeDepth) do
//    begin
//      EmitByte(op_Pop);
//      Dec(i);
//    end;
//
//  // Jump to past end of current innermost loop.
//  BreakExit := EmitJump(op_Jump);
//end;
//
//procedure TCodeGenerator.VisitTContinueStmt(ContinueStmt: TContinueStmt);
//var
//  i: Integer;
//begin
//  SetLocation(ContinueStmt.Location);
//  if InnermostLoopStart = -1 then
//    ErrorAt(ContinueStmt.Location, 'Cannot use "continue" outside of a loop.');
//
//  // Discard any locals created inside the loop.
//  i := Current^.LocalCount - 1;
//  while (i >= 0) and (Current^.Locals[i].Depth > InnermostLoopScopeDepth) do
//    begin
//      EmitByte(op_Pop);
//      Dec(i);
//    end;
//
//  // Jump to top of current innermost loop.
//  EmitLoop(InnermostLoopStart);
//end;

procedure TCodeGenerator.VisitTUseStmt(UseStmt: TUseStmt);
begin
  // do nothing
end;

procedure TCodeGenerator.VisitTDeferStmt(DeferStmt: TDeferStmt);
begin
  SetLocation(DeferStmt.Location);
  VisitProc(DeferStmt.Closure);
  NewDefer(DeferStmt.Closure.Name.Text, Current^.Func);
end;

procedure TCodeGenerator.VisitTVarDecl(VarDecl: TVarDecl);
var
  Slot: UInt16;
begin
  SetLocation(VarDecl.Location);
  Slot := MakeVariable(VarDecl.Name);
  if Assigned(VarDecl.Expr) then
    VisitProc(VarDecl.Expr)
  else
    EmitByte(op_Nil);
  DefineVariable(Slot);
end;

procedure TCodeGenerator.VisitTVarDecls(VarDecls: TVarDecls);
var
  Decl: TDecl;
begin
  for Decl in VarDecls.List do
    VisitProc(Decl);
end;

procedure TCodeGenerator.VisitTBody(Body: TBody);
var
  Node: TNode;
begin
  for Node in Body.Nodes do
    VisitProc(Node);
end;

procedure TCodeGenerator.CompileFunction(FuncDecl: TFuncDecl);
var
  Compiler: TCompiler;
  Func: PObjFunc;
  ParamConstant: UInt16;
  i: Integer;
  Parameter: TParameter;
begin
  SetLocation(FuncDecl.Location);

  InitCompiler(Compiler, FuncDecl.Form, FuncDecl.Name);
  BeginScope;

  // Compile the parameter list
  Current^.Func^.Arity := FuncDecl.Parameters.Count;
  for Parameter in FuncDecl.Parameters do
    begin
      ParamConstant := MakeVariable(Parameter.Variable.Name);
      DefineVariable(ParamConstant)
    end;

  VisitProc(FuncDecl.Body);

  // Create the function object
  Func := EndCompiler; // and closes scope

  Emit3Bytes(op_Closure, MakeConstant(ObjVal(Func)));

  for i := 0 to Func^.UpValueCount-1 do
    begin
      EmitByte(specialize IfThen<Byte>(Compiler.UpValues[i].isLocal, 1, 0));
      EmitByte(Compiler.UpValues[i].Index);
    end;
end;

procedure TCodeGenerator.VisitTFuncDecl(FuncDecl: TFuncDecl);
var
  Global: UInt16;
begin
  SetLocation(FuncDecl.Location);
  Global := MakeVariable(FuncDecl.Name);
  MarkInitialized;
  CompileFunction(FuncDecl);
  DefineVariable(Global);
end;

procedure TCodeGenerator.VisitTValDecl(ValDecl: TValDecl);
var
  Slot: UInt16;
begin
  SetLocation(ValDecl.Location);
  Slot := MakeVariable(ValDecl.Name);
  MarkInitialized;
  CompileFunction(ValDecl.Func);
  DefineVariable(Slot);
end;

procedure TCodeGenerator.VisitTClassDecl(ClassDecl: TClassDecl);
var
  NameConstant, MethodConstant, VarConstant: UInt16;
  ClassCompiler: TClassCompiler;
  Member: TDecl;
  Key: String;
begin
  SetLocation(ClassDecl.Location);
  NameConstant := IdentifierConstant(ClassDecl.Name);
  DeclareVariable(ClassDecl.Name);

  Emit3Bytes(op_Class, NameConstant);
  DefineVariable(NameConstant);

  ClassCompiler.Name := ClassDecl.Name;
  ClassCompiler.hasSuperClass := False;
  ClassCompiler.Enclosing := CurrentClass;
  CurrentClass := @ClassCompiler;

  if Assigned(ClassDecl.Parent) then
    begin
      NamedVariable(ClassDecl.Parent.Name, ttNone, Nil, False);

      BeginScope;
      AddLocal(TIdent.Create('inherited', CurrentLoc));
      DefineVariable(0);

      NamedVariable(ClassDecl.Name, ttNone, Nil, False);
      EmitByte(op_Inherit);
      ClassCompiler.hasSuperClass := True;
    end;

  NamedVariable(ClassDecl.Name, ttNone, Nil, False);

  if Assigned(ClassDecl.DefaultValue) then
    begin
      VarConstant := IdentifierConstant(ClassDecl.DefaultValue.Name);
      VisitProc(ClassDecl.DefaultValue.Expr);
      Emit3Bytes(op_Field_Def, VarConstant);
    end;
  for Key in ClassDecl.Members.Keys do
    begin
      Member := ClassDecl.Members[Key];
      case Member.Kind of
        dkFunc:
          begin
            MethodConstant := IdentifierConstant(Member.Name);
            CompileFunction(Member as TFuncDecl);
            Emit3Bytes(op_Method, MethodConstant);
          end;
        dkVal:
          begin
            VarConstant := IdentifierConstant(Member.Name);
            CompileFunction((Member as TValDecl).Func);
            Emit3Bytes(op_Field, VarConstant); // a 'val' goes into a field
          end;
        dkVar:
          begin
            VarConstant := IdentifierConstant(Member.Name);
            if Assigned((Member as TVarDecl).Expr) then
              VisitProc((Member as TVarDecl).Expr)
            else
              EmitByte(op_Nil);
            Emit3Bytes(op_Field, VarConstant);
          end;
      end;
    end;

  // static functions
  for Key in ClassDecl.Statics.Keys do
    begin
      Member := ClassDecl.Statics[Key];
      MethodConstant := IdentifierConstant(Member.Name);
      CompileFunction(Member as TFuncDecl);
      Emit3Bytes(op_Static, MethodConstant);
    end;

  EmitByte(op_Pop);

  if ClassCompiler.hasSuperClass then
    EndScope;

  CurrentClass := CurrentClass^.Enclosing;
end;


procedure TCodeGenerator.VisitTTraitDecl(TraitDecl: TTraitDecl);
begin
  // do nothing
end;

procedure TCodeGenerator.VisitTExtensionDecl(ExtensionDecl: TExtensionDecl);
begin
  // do nothing
end;

procedure TCodeGenerator.VisitTEnumDecl(EnumDecl: TEnumDecl);
var
  NameConstant, MethodConstant, VarConstant, EnumConstant: UInt16;
  ClassCompiler: TClassCompiler;
  Member: TDecl;
  ElementName: String;
begin
  SetLocation(EnumDecl.Location);
  NameConstant := IdentifierConstant(EnumDecl.Name);
  DeclareVariable(EnumDecl.Name);

  Emit3Bytes(op_Enum, NameConstant);
  DefineVariable(NameConstant);

  ClassCompiler.Name := EnumDecl.Name;
  ClassCompiler.hasSuperClass := False;
  ClassCompiler.Enclosing := CurrentClass;
  CurrentClass := @ClassCompiler;

  NamedVariable(EnumDecl.Name, ttNone, Nil, False);

  for ElementName in EnumDecl.Elements.Keys do
    begin
      EnumConstant := IdentifierConstant(TIdent.Create(ElementName, CurrentLoc));
      if Assigned(EnumDecl.Elements[ElementName]) then
        VisitProc(EnumDecl.Elements[ElementName])
      else
        EmitByte(op_Nil);
      Emit3Bytes(op_Field, EnumConstant);
    end;

  for Member in EnumDecl.Members.Values do
    begin
      if Member is TFuncDecl then
        begin
          MethodConstant := IdentifierConstant(Member.Name);
          CompileFunction(Member as TFuncDecl);
          Emit3Bytes(op_Method, MethodConstant);
        end
      else
        begin
          VarConstant := IdentifierConstant(Member.Name);
          CompileFunction((Member as TValDecl).Func);
          Emit3Bytes(op_Field, VarConstant); // a 'val' goes into a field
        end;
    end;

  EmitByte(op_Pop);
  CurrentClass := CurrentClass^.Enclosing;
end;

procedure TCodeGenerator.VisitTBlock(Block: TBlock);
var
  Node: TNode;
begin
  SetLocation(Block.Location);
  BeginScope;
  for Node in Block.Nodes do
    VisitProc(Node);
  EndScope;
end;

procedure TCodeGenerator.VisitTProduct(Product: TProduct);
var
  Node: TNode;
begin
  for Node in Product.Nodes do
    VisitProc(Node);
end;


end.


