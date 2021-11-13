unit uParser;

{ This unit contains the parser that processes all tokens; input from the Lexer.
  The parser also checks the language grammar. The result of the parsing is
  an AST.

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
  Classes, SysUtils, uLexer, uToken, uAST, uError, Variants;

type

  TParser = class
    private
      type
        TFuncForm = (ffFunction, ffAnonym, ffInit);
    private
      Tokens: TTokens;
      Current: integer;
      LoopDepth: Integer;
      function CurrentToken: TToken;
      function Peek: TToken;
      procedure Error(Token: TToken; Msg: string);
      procedure Expect(const TokenTyp: TTokenTyp);
      procedure Next;
      procedure Synchronize(Types: TTokenTypSet);
      function isLastToken: Boolean;
      function MakeID(Name: String): TIdent;
      function ParseExprList: TExprList;
    public
      constructor Create(Lexer: TLexer);
      function Parse: TProduct;
    private
      function ParseIdent: TIdent;
      // Expressions
      function ParseExpr: TExpr;
      function isRelOp: Boolean;
      function ParseAddExpr: TExpr;
      function isAddOp: Boolean;
      function ParseMulExpr: TExpr;
      function isMulOp: Boolean;
      function ParseShiftExpr: TExpr;
      function isShiftOp: Boolean;
      function ParseRangeExpr: TExpr;
      function ParseUnaryExpr: TExpr;
      function ParseCallExpr: TExpr;
      function ParseCallArgs(Callee: TExpr): TExpr;
      function ParseIfExpr: TExpr;
      function ParseMatchExpr: TExpr;
      function ParseIdentifierExpr: TExpr;
      function ParseParenExpr: TExpr;
      function ParseSelfExpr: TExpr;
      function ParseInheritedExpr: TExpr;
      function ParseArrayDeclExpr(FirstExpr: TExpr): TExpr;
      function ParseDictDeclExpr(KeyExpr: TExpr): TExpr;
      function ParseBracketExpr: TExpr;
      function ParseSetBuilderExpr: TExpr;
      function ParseInterpolatedExpr: TExpr;
      function ParseFactor: TExpr;
      // Statements
      function ParseStmt: TStmt;
      function ParsePrintStmt: TStmt;
      function ParseAssignStmt: TStmt;
      function ParseIfStmt: TStmt;
      function ParseWhileStmt: TStmt;
      function ParseRepeatStmt: TStmt;
      function ParseForStmt: TStmt;
      function ParseForEachStmt: TStmt;
      function ParseEnsureStmt: TStmt;
      function ParseSwitchStmt: TStmt;
      function ParseBreakStmt: TStmt;
      function ParseContinueStmt: TStmt;
      function ParseReturnStmt: TStmt;
      function ParseUseStmt: TStmt;
      // Declarations
      function ParseDecl: TDecl;
      function ParseDeclList(DeclSet: TTokenTypSet; const TypeName: String): TDeclList;
      function ParseVarDecl(Mutable: Boolean): TDecl;
      function ParseVarDecls(Mutable: Boolean): TDecl;
      function ParseFuncDecl(FuncForm: TFuncForm): TDecl;
      function ParseClassDecl: TDecl;
      function ParseValDecl: TDecl;
      function ParseExtensionDecl: TDecl;
      function ParseTraitDecl: TDecl;
      function ParseArrayDecl: TDecl;
      function ParseKeyValueList: TKeyValueList;
      function ParseDictDecl: TDecl;
      function ParseEnumDecl: TDecl;
      // Blocks
      function ParseNode: TNode;
      function ParseBlock: TBlock;
      function ParseProduct: TProduct;
  end;


implementation
uses uLanguage, uReader;

const
  ErrSyntax = 'Syntax error, "%s" expected.';
  ErrUnexpectedToken = 'Unexpected token: %s.';
  ErrDuplicateTerminator = 'Duplicate terminator not allowed.';
  ErrUnexpectedAttribute = 'Unexpected attribute "%s:".';
  ErrInvalidAssignTarget = 'Invalid assignment target.';
  ErrExpectedAssignOpFunc = 'Expected assignment operator, or function call.';
  ErrUnrecognizedDeclOrStmt = 'Unrecognized declaration or statement.';
  ErrBreakInLoop = 'Break can only be used from inside a loop.';
  ErrExpectedArrow = 'Expected arrow "=>".';
  ErrIncorrectInherit = 'Incorrect inheritance expression.';
  ErrUnallowedDeclIn = 'Unallowed declaration in "%s".';
  ErrNotExistsUseFile = 'Used file "%s" does not exist.';
  ErrIncorrectUseFile = 'Used file "%s" is incorrect or corrupt.';
  ErrNotAllowedInRange = 'Token "%s" not allowed in range expression.';
  ErrAtLeastOneEnum = 'At least one enum is required in declaration.';
  ErrDuplicateEnum = 'Duplicate enum name "%s".';
  ErrNotAllowedInEnum = 'Constant value "%s" not allowed in enum declaration.';
  ErrDuplicateSetName = 'Duplicate enum set name "%s".';

function TParser.CurrentToken: TToken;
begin
  Result := Tokens[Current];
end;

function TParser.Peek: TToken;
begin
  if not isLastToken then
    Result := Tokens[Current+1];
end;

procedure TParser.Error(Token: TToken; Msg: string);
begin
  Errors.Append(Token, Msg);
  Raise EParseError.Create(Msg);
end;

procedure TParser.Expect(const TokenTyp: TTokenTyp);
begin
  if CurrentToken.Typ = TokenTyp then
    Next
  else
    Error(CurrentToken, Format(ErrSyntax, [TokenTyp.toString]));
end;

procedure TParser.Next;
begin
  Current +=1;
end;

procedure TParser.Synchronize(Types: TTokenTypSet);
begin
  while not (CurrentToken.Typ in Types) do
    Next;
end;

function TParser.isLastToken: Boolean;
begin
  Result := Current = Tokens.Count-1;
end;

function TParser.MakeID(Name: String): TIdent;
begin
  with CurrentToken do
    Result := TIdent.Create(TToken.Create(
      ttIdentifier, Name, Null, Line, Col, FileIndex));
end;

constructor TParser.Create(Lexer: TLexer);
begin
  Tokens := Lexer.Tokens;
  Current := 0;
  LoopDepth := 0;
end;

function TParser.Parse: TProduct;
begin
  Result := ParseProduct;
end;

function TParser.ParseExprList: TExprList;
begin
  Result := TExprList.Create();
  Result.Add(ParseExpr);
  while CurrentToken.Typ = ttComma do begin
    Next;  // skip ,
    Result.Add(ParseExpr);
  end;
end;

function TParser.ParseIdent: TIdent;
var
  Token: TToken;
begin
  Token := CurrentToken;
  Expect(ttIdentifier);
  Result := TIdent.Create(Token);
end;

function TParser.ParseExpr: TExpr;
var
  RelOp: TToken;
begin
  Result := ParseAddExpr;
  if isRelOp then begin
    RelOp := CurrentToken;
    Next;
    Result := TBinaryExpr.Create(Result, RelOp, ParseAddExpr);
  end;
end;

function TParser.isRelOp: Boolean;
begin
  Result := CurrentToken.Typ in [ttEQ, ttNEQ, ttGT, ttGE, ttLT, ttLE, ttIn, ttIs];
  //                               =     <>    >     >=     <    <=    in    is
end;

function TParser.ParseAddExpr: TExpr;
var
  AddOp: TToken;
begin
  Result := ParseMulExpr;
  while isAddOp do begin
    AddOp := CurrentToken;
    Next;
    Result := TBinaryExpr.Create(Result, AddOp, ParseMulExpr);
  end;
end;

function TParser.isAddOp: Boolean;
begin
  Result := CurrentToken.Typ in [ttPlus, ttMin, ttOr, ttXOr, ttConcat];
  //                               +       -      |     ~       ><
end;

function TParser.ParseMulExpr: TExpr;
var
  MulOp: TToken;
begin
  Result := ParseShiftExpr;
  while isMulOp do begin
    MulOp := CurrentToken;
    Next;
    Result := TBinaryExpr.Create(Result, MulOp, ParseShiftExpr);
  end;
end;

function TParser.isMulOp: Boolean;
begin
  Result := CurrentToken.Typ in [ttMul, ttDiv, ttRem, ttAnd, ttColons];
  //                               *      /      %      &       ::
end;

function TParser.ParseShiftExpr: TExpr;
var
  ShiftOp: TToken;
begin
  Result := ParseRangeExpr;
  while isShiftOp do begin
    ShiftOp := CurrentToken;
    Next;
    Result := TBinaryExpr.Create(Result, ShiftOp, ParseUnaryExpr);
  end;
end;

function TParser.isShiftOp: Boolean;
begin
  Result := CurrentToken.Typ in [ttShl, ttShr, ttPow];
  //                               <<     >>     ^
end;

function TParser.ParseRangeExpr: TExpr;
var
  UpperBound: TExpr;
  Range: TCallExpr;
  Exclude: Boolean=False;
  Minus: TToken;
  One: TConstExpr;

  procedure CheckUnaryOp(Expr: TExpr);
  begin
    if Expr is TUnaryExpr then
      if (Expr as TUnaryExpr).Op.Typ in [ttNot, ttQuestion] then
        Error((Expr as TUnaryExpr).Op,
          Format(ErrNotAllowedInRange, [(Expr as TUnaryExpr).Op.Typ.toString]));
  end;

begin
  Result := ParseUnaryExpr;
  if CurrentToken.Typ = ttDotDot then begin
    CheckUnaryOp(Result);
    Next; // skip ..
    if CurrentToken.Typ = ttLT then begin  // ..<
      Exclude := True;
      Next;  // skip <
    end;
    UpperBound := ParseUnaryExpr;
    CheckUnaryOp(UpperBound);
    if Exclude then begin
      Minus := TToken.Create(ttMin, '-', Null, 0, 0);
      One := TConstExpr.Create(1, TToken.Create(ttNumber, '1', 1, 0, 0));
      UpperBound := TBinaryExpr.Create(UpperBound, Minus, One);
    end;
    // build call to Range(Lower, Upper)
    Range := TCallExpr.Create(TVariable.Create(MakeID('Range')), Nil);
    Range.AddArgument(Result, Nil);
    Range.AddArgument(UpperBound, Nil);
    Result := Range;
  end;
end;

function TParser.ParseUnaryExpr: TExpr;
var
  Op: TToken;
begin
  if CurrentToken.Typ in [ttPlus, ttMin, ttNot, ttQuestion] then begin
    Op := CurrentToken;
    Next;
    Result := TUnaryExpr.Create(Op, ParseUnaryExpr());
  end
  else
    Result := ParseCallExpr;
end;

function TParser.ParseCallExpr: TExpr;
var
  Token: TToken;
begin
  Token := CurrentToken;
  Result := ParseFactor;
  while CurrentToken.Typ in [ttDot, ttOpenParen, ttOpenBrack] do
    case CurrentToken.Typ of
      ttDot: begin
          Next; // skip .
          Result := TGetExpr.Create(Result, ParseFactor, Token);
        end;
      ttOpenParen: Result := ParseCallArgs(Result);
      ttOpenBrack: begin
        Next; // skip [
        Result := TIndexedExpr.Create(Result, ParseExpr);
        Expect(ttCloseBrack);
      end;
    end;
end;

function TParser.ParseCallArgs(Callee: TExpr): TExpr;
var
  CallExpr: TCallExpr;
  Token: TToken;
  Signature: TExpr;
  ParamNames: String = '';

  function ParseArgument: String;
  var
    Expr: TExpr;
    Ident: TIdent = Nil;
  begin
    Result := '';
    Expr := ParseExpr;
    if CurrentToken.Typ = ttColon then begin
      Ident := TVariable(Expr).Ident; // ExtIdent parsed as Expr
      Result := '#' + Ident.Text;
      Next; // skip :
      Expr := ParseExpr;
    end;
    CallExpr.AddArgument(Expr, Ident);
  end;

begin
  Token := CurrentToken;
  Next;  // skip (
  CallExpr := TCallExpr.Create(Callee, Token);
  if CurrentToken.Typ <> ttCloseParen then begin
    ParamNames := ParseArgument;
    while CurrentToken.Typ = ttComma do begin
      Next;  // skip ,
      ParamNames += ParseArgument;
    end;
  end;
  if ParamNames <> '' then begin
    if Callee is TVariable then with Callee as TVariable do begin
      Signature := TVariable.Create(TIdent.Create(Ident.Token.Copy));
      TVariable(Signature).Ident.Text := Ident.Text + ParamNames;
    end
    else if Callee is TGetExpr then with Callee as TGetExpr do begin
      Signature := TGetExpr.Create(Instance, TVariable.Create(
        TIdent.Create(TVariable(Member).Ident.Token.Copy)), Token.Copy);
      TVariable(TGetExpr(Signature).Member).Ident.Text :=
        TVariable(Member).Ident.Text + ParamNames;
    end
    else if Callee is TInheritedExpr then with Callee as TInheritedExpr do begin
      Signature := TInheritedExpr.Create(Variable, TVariable.Create(
        TIdent.Create(TVariable(Method).Ident.Token.Copy)));
      TVariable(TInheritedExpr(Signature).Method).Ident.Text :=
        TVariable(Method).Ident.Text + ParamNames;
    end
  end
  else Signature := Callee;
  CallExpr.Signature := Signature;
  Expect(ttCloseParen);
  Result := CallExpr;
end;

function TParser.ParseIfExpr: TExpr;
var
  Condition, TrueExpr, FalseExpr: TExpr;
  Token: TToken;
begin
  Token := CurrentToken;
  Next; // skip 'if'
  Condition := ParseExpr;
  Expect(ttThen);
  TrueExpr := ParseExpr;
  Expect(ttElse);
  FalseExpr := ParseExpr;
  Result := TIfExpr.Create(Condition, TrueExpr, FalseExpr, Token);
end;

function TParser.ParseMatchExpr: TExpr;
var
  Token: TToken;
  Value, Expr: TExpr;
  MatchExpr: TMatchExpr;
  Values: TExprList;
begin
  Token := CurrentToken;
  Next; // skip 'match'
  MatchExpr := TMatchExpr.Create(ParseExpr, Token);
  Expect(ttIf); // one 'if' is mandatory
  Values := ParseExprList;
  Expect(ttThen);
  Expr := ParseExpr;
  for Value in Values do
    MatchExpr.AddLimb(Value, Expr);
  while CurrentToken.Typ = ttIf do begin
    Next;  // skip if
    Values := ParseExprList;
    Expect(ttThen);
    Expr := ParseExpr;
    for Value in Values do
      MatchExpr.AddLimb(Value, Expr);
  end;
  Expect(ttElse);  // else is mandatory
  MatchExpr.ElseLimb := ParseExpr;
  Result := MatchExpr;
end;

function TParser.ParseIdentifierExpr: TExpr;
var
  Ident: TIdent;
  FuncDecl: TFuncDecl;
begin
  Ident := ParseIdent;
  if CurrentToken.Typ = ttArrow then begin
    FuncDecl := TFuncDecl.Create(Nil, CurrentToken);
    FuncDecl.AddParam(Ident, Nil);
    FuncDecl.Body := TBlock.Create(TNodeList.Create(), CurrentToken);
    FuncDecl.Body.Nodes.Add(ParseReturnStmt);
    Result := TFuncDeclExpr.Create(FuncDecl);
  end
  else Result := TVariable.Create(Ident);
end;

function TParser.ParseParenExpr: TExpr;
var
  Expr: TExpr;
  ExprList: TExprList;
  FuncDecl: TFuncDecl;
begin
  Expect(ttOpenParen);
  if CurrentToken.Typ <> ttCloseParen then begin
    Expr := ParseExpr;
    if CurrentToken.Typ = ttComma then begin  // check for a list
      ExprList := TExprList.Create();
      ExprList.Add(Expr);
      while CurrentToken.Typ = ttComma do begin
        Next; // skip ,
        ExprList.Add(ParseExpr);
      end;
      Expect(ttCloseParen);
      if CurrentToken.Typ = ttArrow then begin  // func: ExprList contains params
        FuncDecl := TFuncDecl.Create(Nil, CurrentToken);
        for Expr in ExprList do
          FuncDecl.AddParam((Expr as TVariable).Ident, Nil);
        FuncDecl.Body := TBlock.Create(TNodeList.Create(), CurrentToken);
        FuncDecl.Body.Nodes.Add(ParseReturnStmt);
        Result := TFuncDeclExpr.Create(FuncDecl);
      end
      else  // must be a tuple expr
        Result := TTupleExpr.Create(ExprList, CurrentToken);
    end
    else begin
      Result := Expr;  // it’s a regular parenthesised expression
      Expect(ttCloseParen);
    end;
  end
  else begin
    Next; // skip )
    if CurrentToken.Typ = ttArrow then begin  // func: zero params
      FuncDecl := TFuncDecl.Create(Nil, CurrentToken);
      FuncDecl.Body := TBlock.Create(TNodeList.Create(), CurrentToken);
      FuncDecl.Body.Nodes.Add(ParseReturnStmt);
      Result := TFuncDeclExpr.Create(FuncDecl);
    end
    else // empty tuple
      Result := TTupleExpr.Create(TExprList.Create(), CurrentToken);
  end;
end;

function TParser.ParseSelfExpr: TExpr;
var
  Variable: TVariable;
begin
  Variable := TVariable.Create(TIdent.Create(CurrentToken));
  Next;  // skip self
  Result := TSelfExpr.Create(Variable);
end;

function TParser.ParseInheritedExpr: TExpr;
var
  Variable, Method: TVariable;
begin
  Variable := TVariable.Create(TIdent.Create(CurrentToken));
  Next;  // skip inherited
  if CurrentToken.Typ in [ttIdentifier, ttInit] then begin
    Method := TVariable.Create(TIdent.Create(CurrentToken));
    Next; // skip method name
  end
  else if CurrentToken.Typ = ttOpenParen then with CurrentToken do
    Method := TVariable.Create(TIdent.Create(TToken.Create(
      ttIdentifier, 'init', Null, Line, Col, FileIndex)))
  else
    Error(CurrentToken, ErrIncorrectInherit);
  Result := TInheritedExpr.Create(Variable, Method);
end;


function TParser.ParseArrayDeclExpr(FirstExpr: TExpr): TExpr;
var
  ArrayDecl: TArrayDecl;
begin
  ArrayDecl := TArrayDecl.Create(Nil, TDeclList.Create(false), CurrentToken);
  ArrayDecl.AddElement(FirstExpr);
  while CurrentToken.Typ = ttComma do begin
    Next;  // skip ,
    ArrayDecl.AddElement(ParseExpr);
  end;
  Result := TArrayDeclExpr.Create(ArrayDecl);
end;

function TParser.ParseDictDeclExpr(KeyExpr: TExpr): TExpr;
var
  DictDecl: TDictDecl;
  Key: TExpr;
begin
  DictDecl := TDictDecl.Create(Nil, TKeyValueList.Create,
                TDeclList.Create(false), CurrentToken);
  Expect(ttColon);
  DictDecl.AddElement(KeyExpr, ParseExpr);
  while CurrentToken.Typ = ttComma do begin
    Next;  // skip ,
    Key := ParseExpr;
    Expect(ttColon);
    DictDecl.AddElement(Key, ParseExpr);
  end;
  Result := TDictDeclExpr.Create(DictDecl);
end;

function TParser.ParseBracketExpr: TExpr;
var
  Expr: TExpr;
  ArrayDecl: TArrayDecl;
  DictDecl: TDictDecl;
begin
  Next; // skip [
  case CurrentToken.Typ of
    ttCloseBrack: begin    // an empty array
      ArrayDecl := TArrayDecl.Create(Nil, TDeclList.Create(false), CurrentToken);
      Result := TArrayDeclExpr.Create(ArrayDecl);
    end;
    ttColon: begin        // an empty dictionary
      DictDecl := TDictDecl.Create(Nil, TKeyValueList.Create,
                    TDeclList.Create(false), CurrentToken);
      Result := TDictDeclExpr.Create(DictDecl);
      Next;  // skip :
    end
    else                 // a filled array or dictionary
      begin
        Expr := ParseExpr;       // parse the first expr
        if CurrentToken.Typ = ttColon then    // then check for ‘:’
          Result := ParseDictDeclExpr(Expr)   // a dictionary; pass the first expr
        else
          Result := ParseArrayDeclExpr(Expr); // an array; pass the first expr
      end;
  end;
  Expect(ttCloseBrack);   // final closing bracket
end;



function TParser.ParseSetBuilderExpr: TExpr;
var
  OutputExpr, InputSet: TExpr;
  PredicateExpr: TExpr = Nil;
  Ident: TIdent;
  OutputFuncExpr, PredicateFuncExpr: TFuncDeclExpr;
  ListBuilder: TCallExpr;
begin
  // first step: parse all elements of set builder expression
  Next; // skip '{'
  OutputExpr := ParseExpr;
  Expect(ttFor);
  Ident := ParseIdent;
  Expect(ttIn);
  InputSet := ParseExpr;
  if CurrentToken.Typ = ttWhere then begin
    Next; // skip where
    PredicateExpr := ParseExpr;
  end;
  Expect(ttCloseBrace);

  // second step: create the functions
  OutputFuncExpr := TFuncDeclExpr.Create(TFuncDecl.Create(Nil, Nil));
  with OutputFuncExpr do begin
    FuncDecl.AddParam(Ident, Nil);
    FuncDecl.Body := TBlock.Create(TNodeList.Create(), Nil);
    FuncDecl.Body.Nodes.Add(TReturnStmt.Create(OutputExpr, Nil));
  end;

  if Assigned(PredicateExpr) then begin
    PredicateFuncExpr := TFuncDeclExpr.Create(TFuncDecl.Create(Nil, Nil));
    with PredicateFuncExpr do begin
      FuncDecl.AddParam(Ident, Nil);
      FuncDecl.Body := TBlock.Create(TNodeList.Create(), Nil);
      FuncDecl.Body.Nodes.Add(TReturnStmt.Create(PredicateExpr, Nil));
    end;
  end;

  // third step: create call to func listBuilder(transform, input, include)
  ListBuilder := TCallExpr.Create(TVariable.Create(MakeID('listBuilder')), Nil);
  ListBuilder.AddArgument(OutputFuncExpr, Nil);
  ListBuilder.AddArgument(InputSet, Nil);
  if Assigned(PredicateExpr) then
    ListBuilder.AddArgument(PredicateFuncExpr, Nil)
  else
    ListBuilder.AddArgument(TConstExpr.Create(Null, Nil), Nil);
  Result := ListBuilder;
end;

function TParser.ParseInterpolatedExpr: TExpr;
var
  ExprList: TExprList;
  Token: TToken;
begin
  Token := CurrentToken;
  ExprList := TExprList.Create();
  while CurrentToken.Typ = ttInterpolated do begin
    ExprList.Add(TConstExpr.Create(CurrentToken.Value, CurrentToken)); // Opening string
    Next;
    ExprList.Add(ParseExpr);          // Interpolated expression
  end;
  if CurrentToken.Typ = ttString then
    ExprList.Add(ParseFactor)
  else
    Error(CurrentToken, 'Expected end of string interpolation.');
  Result := TInterpolatedExpr.Create(ExprList, Token);
end;

function TParser.ParseFactor: TExpr;
begin
  case CurrentToken.Typ of
    ttFunc: Result := TFuncDeclExpr.Create(ParseFuncDecl(ffAnonym) as TFuncDecl);
    ttIf: Result := ParseIfExpr;
    ttMatch: Result := ParseMatchExpr;
    ttInherited: Result := ParseInheritedExpr;
    ttSelf: Result := ParseSelfExpr;
    ttIdentifier: Result := ParseIdentifierExpr;
    ttFalse, ttTrue: begin
      Result := TConstExpr.Create(CurrentToken.Typ = ttTrue, CurrentToken);
      Next;
    end;
    ttNull: begin
      Result := TConstExpr.Create(Null, CurrentToken); // Null is variant enum
      Next;
    end;
    ttNumber, ttString, ttChar: begin
      Result := TConstExpr.Create(CurrentToken.Value, CurrentToken);
      Next;
    end;
    ttInterpolated: Result := ParseInterpolatedExpr;
    ttOpenParen: Result := ParseParenExpr;
    ttOpenBrace: Result := ParseSetBuilderExpr;
    ttOpenBrack: Result := ParseBracketExpr; // [ array or dictionary
    else begin
      Result := TExpr.Create(CurrentToken);
      Error(CurrentToken,Format(ErrUnexpectedToken, [CurrentToken.toString]));
    end;
  end;
end;

const
  StmtStartSet: TTokenTypSet = [ttIf, ttWhile, ttRepeat, ttFor, ttSwitch,
    ttReturn, ttEnsure, ttPrint, ttInherited , ttSelf, ttUse, ttBreak,
    ttContinue, ttIdentifier];

function TParser.ParseStmt: TStmt;
begin
  case CurrentToken.Typ of
    ttIf: Result := ParseIfStmt;
    ttWhile: Result := ParseWhileStmt;
    ttRepeat: Result := ParseRepeatStmt;
    ttFor: Result := ParseForStmt;
    ttReturn: Result := ParseReturnStmt;
    ttSwitch: Result := ParseSwitchStmt;
    ttEnsure: Result := ParseEnsureStmt;
    ttPrint: Result := ParsePrintStmt;
    ttUse: Result := ParseUseStmt;
    ttBreak: Result := ParseBreakStmt;
    ttContinue: Result := ParseContinueStmt;
    else
      Result := ParseAssignStmt;
  end;
end;

function TParser.ParsePrintStmt: TStmt;
var
  ExprList: TExprList;
  Token: TToken;
  Terminator: TExpr;
  SawTerminator: Boolean = False;

  procedure ParseItem;
  var
    PrintPretty: TIdent;
  begin
    if (CurrentToken.Typ = ttIdentifier) and (Peek.Typ = ttColon) then begin
      PrintPretty := ParseIdent;
      if PrintPretty.Text = 'terminator' then begin
        if SawTerminator then
          Error(PrintPretty.Token, ErrDuplicateTerminator);
        SawTerminator := True;
        Next; // skip :
        Terminator := ParseExpr;
      end
      else
        Error(PrintPretty.Token,
          Format(ErrUnexpectedAttribute, [PrintPretty.Text]));
    end
    else ExprList.Add(ParseExpr);
  end;

begin
  Terminator := TConstExpr.Create(LineEnding, CurrentToken);
  Token := CurrentToken;
  Next; // skip print
  Expect(ttOpenParen);
  ExprList := TExprList.Create(false);
  if CurrentToken.Typ <> ttCloseParen then begin
    ParseItem;
    while CurrentToken.Typ = ttComma do begin
      Next; // skip ,
      ParseItem;
    end;
  end;
  Expect(ttCloseParen);
  Result := TPrintStmt.Create(ExprList, Terminator, Token);
end;

const
  AssignSet: TTokenTypSet =
    [ttPlusIs, ttMinIs, ttMulIs, ttDivIs, ttRemIs, ttConcatIs, ttAssign];

function TParser.ParseAssignStmt: TStmt;
var
  Token, Op: TToken;
  Left, Right: TExpr;
begin
  Token := CurrentToken;
  Left := ParseExpr;
  if CurrentToken.Typ in AssignSet then begin
    Op := CurrentToken;
    Next; // skip assign token
    Right := ParseExpr;
    if Left is TVariable then
      Result := TAssignStmt.Create(Left as TVariable, Op, Right)
    else if Left is TGetExpr then
      Result := TSetStmt.Create(Left as TGetExpr, Op, Right)
    else if Left is TIndexedExpr then
      Result := TIndexedExprStmt.Create(Left as TIndexedExpr, Op, Right)
    else
      Error(Token, ErrInvalidAssignTarget);
  end
  else if Left is TCallExpr then
    Result := TCallExprStmt.Create(Left as TCallExpr, Token)
  else
    Error(CurrentToken, ErrExpectedAssignOpFunc);
end;

function TParser.ParseIfStmt: TStmt;
var
  Token, VarToken: TToken;
  VarDecl: TVarDecl = Nil;
  Condition: TExpr;
  ThenPart: TBlock;
  ElsePart: TBlock = Nil;
  ElseIfExprs: TExprList = Nil;
  ElseIfParts: TBlocks = Nil;
begin
  Token := CurrentToken;
  Next; // skip if
  if CurrentToken.Typ in [ttVar, ttLet] then begin
    VarToken := CurrentToken;
    Next; // skip var or let
    VarDecl := ParseVarDecl(VarToken.Typ = ttVar) as TVarDecl;
    Expect(ttWhere);
  end;
  Condition := ParseExpr;
  Expect(ttThen);
  ThenPart := ParseBlock;
  if CurrentToken.Typ = ttElseif then begin
    ElseIfExprs := TExprList.Create();
    ElseIfParts := TBlocks.Create();
    repeat
      Next;  // skip elseif
      ElseIfExprs.Add(ParseExpr);
      Expect(ttThen);
      ElseIfParts.Add(ParseBlock);
    until CurrentToken.Typ <> ttElseIf;
  end;
  if CurrentToken.Typ = ttElse then begin
    Next; // skip else
    ElsePart := ParseBlock;
  end;
  Expect(ttEnd);
  Result := TIfStmt.Create(VarDecl, Condition, ElseIfExprs,
    ThenPart, ElsePart, ElseIfParts, Token);
end;

function TParser.ParseWhileStmt: TStmt;
var
  Token: TToken;
  VarDecl: TVarDecl = Nil;
  Condition: TExpr;
  Block: TBlock;
begin
  try
    Inc(LoopDepth);
    Token := CurrentToken;
    Next; // skip while
    if CurrentToken.Typ = ttVar then begin
      Next;
      VarDecl := ParseVarDecl(True) as TVarDecl;
      Expect(ttWhere);
    end;
    Condition := ParseExpr;
    Expect(ttDo);
    Block := ParseBlock;
    Expect(ttEnd);
    Result := TWhileStmt.Create(VarDecl, Condition, Block, Token);
  finally
    Dec(LoopDepth);
  end;
end;

function TParser.ParseRepeatStmt: TStmt;
var
  Token: TToken;
  Condition: TExpr;
  Block: TBlock;
begin
  Token := CurrentToken;
  Next; // skip repeat
  Block := ParseBlock;
  Expect(ttUntil);
  Condition := ParseExpr;
  Result := TRepeatStmt.Create(Condition, Block, Token);
end;

function TParser.ParseForStmt: TStmt;
var
  Token: TToken;
  VarDecl: TVarDecl;
  Condition: TExpr;
  Iterator: TStmt;
  Block: TBlock;
begin
  try
    Inc(LoopDepth);
    Token := CurrentToken;
    Next; // skip for
    if CurrentToken.Typ = ttEach then
      Result := ParseForEachStmt
    else begin
      Expect(ttVar);
      VarDecl := ParseVarDecl(True) as TVarDecl;
      Expect(ttWhere);
      Condition := ParseExpr;
      Expect(ttComma);
      Iterator := ParseAssignStmt;
      Expect(ttDo);
      Block := ParseBlock;
      Block.Nodes.Add(Iterator);
      Result := TWhileStmt.Create(VarDecl, Condition, Block, Token);
    end;
    Expect(ttEnd);
  finally
    Dec(LoopDepth);
  end;
end;

function TParser.ParseForEachStmt: TStmt;
var
  Condition: TGetExpr;
  IterDecl, VarDecl: TVarDecl;
  Block: TBlock;
  LoopIdent: TIdent;
  ListExpr: TExpr;
  WhereExpr: TExpr = Nil;
  IfStmt: TIfStmt = Nil;
begin
  Next; // skip each
  // parse the for-loop variable
  LoopIdent := ParseIdent;
  Expect(ttIn);
  // parse list type expression
  ListExpr := ParseExpr;
  if CurrentToken.Typ = ttWhere then begin
    Next;
    WhereExpr := ParseExpr;
  end;
  Expect(ttDo);

  // create initial iterator vardecl: var iterator := ListExpr.iterator
  IterDecl := TVarDecl.Create(MakeID('iterator'),
    TGetExpr.Create(ListExpr, TVariable.Create(MakeID('iterator')), Nil), Nil);

  // build condition: iterator.hasNext
  Condition := TGetExpr.Create(
    TVariable.Create(MakeID('iterator')), TVariable.Create(MakeID('hasNext')), Nil);

  // build var LoopIdent := iterator.next
  VarDecl := TVarDecl.Create(LoopIdent,
    TGetExpr.Create(TVariable.Create(MakeID('iterator')), TVariable.Create(MakeID('next')), Nil), Nil);

  // build where-expression into if-statement
  if Assigned(WhereExpr) then begin
    IfStmt := TIfStmt.Create(Nil, WhereExpr, Nil,
                TBlock.Create(TNodeList.Create(), Nil), Nil, Nil, Nil);
    IfStmt.ThenPart := ParseBlock;
    Block := TBlock.Create(TNodeList.Create(), Nil);
    Block.Nodes.Add(VarDecl);
    Block.Nodes.Add(IfStmt);
  end else begin
    // parse the block
    Block := ParseBlock;
    Block.Nodes.Insert(0, VarDecl);   // insert VarDecl at start of block
  end;

  Result := TWhileStmt.Create(IterDecl, Condition, Block, Nil);
end;

function TParser.ParseEnsureStmt: TStmt;
var
  Token, VarToken: TToken;
  VarDecl: TVarDecl = Nil;
  Condition: TExpr;
  ElsePart: TBlock;
begin
  Token := CurrentToken;
  Next; // skip ensure
  if CurrentToken.Typ in [ttVar, ttLet] then begin
    VarToken := CurrentToken;
    Next;
    VarDecl := ParseVarDecl(VarToken.Typ = ttVar) as TVarDecl;
    Expect(ttWhere);
  end;
  Condition := ParseExpr;
  Expect(ttElse);
  ElsePart := ParseBlock;
  Expect(ttEnd);
  Result := TEnsureStmt.Create(VarDecl, Condition, ElsePart, Token);
end;

function TParser.ParseSwitchStmt: TStmt;
var
  Token: TToken;
  Value: TExpr;
  IsObj: Boolean=False;
  SwitchStmt: TSwitchStmt;
  Values: TExprList;
  Block: TBlock;
begin
  Token := CurrentToken;
  Next; // skip 'switch'
  SwitchStmt := TSwitchStmt.Create(ParseExpr, Token);
  repeat
    Expect(ttCase); // one 'case' is mandatory
    if CurrentToken.Typ = ttIs then begin
      Next; // skip is
      IsObj := True;
    end;
    Values := ParseExprList;
    Expect(ttColon);
    Block := ParseBlock;
    for Value in Values do
      SwitchStmt.AddLimb(Value, IsObj, Block);
    IsObj := False;
  until CurrentToken.Typ <> ttCase;
  Expect(ttElse);  // else is mandatory
  SwitchStmt.ElseLimb := ParseBlock;
  Expect(ttEnd);
  Result := SwitchStmt;
end;

function TParser.ParseBreakStmt: TStmt;
var
  Token: TToken;
  Condition: TExpr=Nil;
begin
  if LoopDepth = 0 then
    Error(CurrentToken, ErrBreakInLoop);
  Token := CurrentToken;
  Next; // skip Break
  if CurrentToken.Typ = ttOn then begin
    Next; // skip On
    Condition := ParseExpr;
  end;
  Result := TBreakStmt.Create(Condition, Token);
end;

function TParser.ParseContinueStmt: TStmt;
begin
  Result := TContinueStmt.Create(CurrentToken);
  Next; // skip continue
end;

function TParser.ParseReturnStmt: TStmt;
var
  Token: TToken;
  Expr: TExpr;
begin
  Token := CurrentToken;
  Next; // skip return
  Expr := ParseExpr;
  Result := TReturnStmt.Create(Expr, Token);
end;

function TParser.ParseUseStmt: TStmt;
const Ext = '.gear';
var
  Token: TToken;
  Ident: TIdent;
  Index: LongInt;
  FileName, UseFile: String;
  Lexer: TLexer = Nil;
  Reader: TReader = Nil;
begin
  Token := CurrentToken;
  Next; // skip use
  Ident := ParseIdent;
  FileName := Ident.Text;
  Result := TUseStmt.Create(FileName, Token);
  if FileExists(GearProdFolder + FileName + Ext) then
    UseFile := GearProdFolder + FileName + Ext
  else if FileExists(GearLibFolder + FileName + Ext) then
    UseFile := GearLibFolder + FileName + Ext
  else Error(Ident.Token, Format(ErrNotExistsUseFile, [FileName]));
  if not FileNameArray.Contains(UseFile) then begin
    try
      Reader := TReader.Create(UseFile, itFile);
    except
      Error(Ident.Token, Format(ErrIncorrectUseFile, [FileName]));
    end;
    Lexer := TLexer.Create(Reader);
    //Insert the new tokens in the main tokens list, minus the last EOF token
    Index := Current;
    Lexer.Tokens.Delete(Lexer.Tokens.Count-1);
    Tokens.InsertRange(Index, Lexer.Tokens);
  end;
end;


{ DECLARATIONS }

const
  DeclStartSet: TTokenTypSet = [ttArray, ttClass, ttDictionary, ttEnum, ttExtension,
    ttFunc, ttLet, ttVal, ttVar, ttTrait];

function TParser.ParseDecl: TDecl;
begin
  case CurrentToken.Typ of
    ttArray: Result := ParseArrayDecl;
    ttClass: Result := ParseClassDecl;
    ttDictionary: Result := ParseDictDecl;
    ttEnum: Result := ParseEnumDecl;
    ttExtension: Result := ParseExtensionDecl;
    ttFunc: Result := ParseFuncDecl(ffFunction);
    ttVal: Result := ParseValDecl;
    ttLet, ttVar: Result := ParseVarDecls(CurrentToken.Typ = ttVar);
    ttTrait: Result := ParseTraitDecl;
  end;
end;

function TParser.ParseDeclList
  (DeclSet: TTokenTypSet; const TypeName: String): TDeclList;
begin
  Result := TDeclList.Create();
  while CurrentToken.Typ in DeclStartSet do
    if CurrentToken.Typ in DeclSet then
      Result.Add(ParseDecl)
    else
      Error(CurrentToken, Format(ErrUnallowedDeclIn, [TypeName]));
end;

function TParser.ParseVarDecl(Mutable: Boolean): TDecl;
var
  Ident: TIdent;
  Token: TToken;
begin
  Token := CurrentToken;
  Ident := ParseIdent;
  Expect(ttAssign);
  Result := TVarDecl.Create(Ident, ParseExpr, Token, Mutable);
end;

function TParser.ParseVarDecls(Mutable: Boolean): TDecl;
var
  VarDecls: TVarDecls;
begin
  VarDecls := TVarDecls.Create(TDeclList.Create(), CurrentToken);
  Next; // skip var or Let
  VarDecls.List.Add(ParseVarDecl(Mutable));
  while CurrentToken.Typ = ttComma do begin
    Next; // skip ,
    VarDecls.List.Add(ParseVarDecl(Mutable));
  end;
  Result := VarDecls;
end;

function TParser.ParseFuncDecl(FuncForm: TFuncForm): TDecl;
var
  FuncDecl: TFuncDecl;
  Token: TToken;
  Name: TIdent = Nil;
  Signature: String = '';

  procedure ParseParameters(var FuncHeader: String);

    procedure ParseParam;
    var Ident: TIdent;
    begin
      case CurrentToken.Typ of
        ttDot: begin
          Next;
          Ident := ParseIdent;
          FuncDecl.AddParam(Ident, Ident);
        end;
        ttIdentifier: begin
          Ident := ParseIdent;
          if CurrentToken.Typ = ttIdentifier then
            FuncDecl.AddParam(ParseIdent, Ident)
          else
            FuncDecl.AddParam(Ident, Nil);
        end
        else Error(CurrentToken, 'Invalid parameter.');
      end;
      if FuncDecl.Params.Last.ExtIdent <> Nil then
        FuncHeader += '#' + FuncDecl.Params.Last.ExtIdent.Text;
    end;

  begin
    if CurrentToken.Typ <> ttCloseParen then begin
      ParseParam;
      while CurrentToken.Typ = ttComma do begin
        Next; // skip comma
        ParseParam;
      end;
    end;
  end;

begin
  Token := CurrentToken;
  case FuncForm of
    ffFunction: begin Next; Name := ParseIdent; Signature := Name.Text;  end;
    ffAnonym: Next;
    ffInit: begin Name := TIdent.Create(Token); Next; end;
  end;
  FuncDecl := TFuncDecl.Create(Name, Token);
  Expect(ttOpenParen);
  ParseParameters(Signature);
  if FuncForm = ffFunction then FuncDecl.Ident.Text := Signature;
  Expect(ttCloseParen);
  if CurrentToken.Typ = ttArrow then begin
    FuncDecl.Body := TBlock.Create(TNodeList.Create(), CurrentToken);
    FuncDecl.Body.Nodes.Add(ParseReturnStmt);
  end
  else begin
    FuncDecl.Body := ParseBlock;
    Expect(ttEnd);
  end;
  Result := FuncDecl;
end;

const
  ClassStartSet: TTokenTypSet = [ttFunc, ttInit, ttLet, ttVal, ttVar, ttStatic];

function TParser.ParseClassDecl: TDecl;
var
  Ident: TIdent;
  DeclList, StaticList: TDeclList;
  VarDecls: TVarDecls;
  Decl: TDecl;
  Token: TToken;
  Parent: TVariable = Nil;
  Traits: TExprList;
begin
  Token := CurrentToken;
  Next;  // skip class
  Ident := ParseIdent;
  if CurrentToken.Typ = ttOpenParen then begin
    Next;  // skip (
    Parent := TVariable.Create(ParseIdent);
    Expect(ttCloseParen);
  end;
  Traits := TExprList.Create();
  if CurrentToken.Typ = ttColon then begin
    Next; // skip :
    Traits := ParseExprList;
  end;
  DeclList := TDeclList.Create(false);
  StaticList := TDeclList.Create(false);
  while ClassStartSet.Contains(CurrentToken.Typ) do begin
    case CurrentToken.Typ of
      ttFunc: DeclList.Add(ParseFuncDecl(ffFunction));
      ttInit: DeclList.Add(ParseFuncDecl(ffInit));
      ttStatic: begin
        Next; // skip static
        if CurrentToken.Typ = ttFunc then
          StaticList.Add(ParseFuncDecl(ffFunction))
        else
          Error(CurrentToken, Format(ErrSyntax, ['Func']));
      end;
      ttVal: DeclList.Add(ParseValDecl);
      else begin // ttLet, ttVar
        VarDecls := ParseVarDecls(CurrentToken.Typ = ttVar) as TVarDecls;
        for Decl in VarDecls.List do
          DeclList.Add(Decl);
      end;
    end;
  end;
  Result := TClassDecl.Create(Ident, Parent, Traits, DeclList, StaticList, Token);
  Expect(ttEnd);
end;

function TParser.ParseValDecl: TDecl;
var
  Ident: TIdent;
  FuncDecl: TFuncDecl;
  Token: TToken;
begin
  Token := CurrentToken;
  Next; // skip val
  Ident := ParseIdent;
  FuncDecl := TFuncDecl.Create(Ident, Ident.Token);
  if CurrentToken.Typ = ttAssign then begin
    FuncDecl.Body := TBlock.Create(TNodeList.Create(), CurrentToken);
    FuncDecl.Body.Nodes.Add(ParseReturnStmt);
  end
  else begin
    FuncDecl.Body := ParseBlock;
    Expect(ttEnd);
  end;
  Result := TValDecl.Create(Ident, FuncDecl, Token);
end;

function TParser.ParseExtensionDecl: TDecl;
var
  Token: TToken;
  Extension: TExtensionDecl;
  Ident: TIdent;
begin
  Token := CurrentToken;
  Next;  // skip extension
  Ident := ParseIdent;
  Extension :=
    TExtensionDecl.Create(Ident, ParseDeclList([ttFunc, ttVal], 'extension'), Token);
  Expect(ttEnd);
  Result := Extension;
end;


function TParser.ParseTraitDecl: TDecl;
var
  Token: TToken;
  Ident: TIdent;
  Traits: TExprList;
begin
  Token := CurrentToken;
  Next; // skip trait
  Ident := ParseIdent;
  Traits := TExprList.Create();
  if CurrentToken.Typ = ttColon then begin
    Next; // skip :
    Traits := ParseExprList;  // parse list of traits separated by comma’s
  end;
  Result := TTraitDecl.Create(Ident, Traits, ParseDeclList([ttFunc], 'trait'), Token);
  Expect(ttEnd);
end;

function TParser.ParseArrayDecl: TDecl;
var
  Token: TToken;
  Ident: TIdent;
  ArrayDecl: TArrayDecl;
  ExprList: TExprList;
begin
  Token := CurrentToken;
  Next;  // skip array
  Ident := ParseIdent;
  Expect(ttOpenBrack);
  if CurrentToken.Typ <> ttCloseBrack then
    ExprList := ParseExprList;
  Expect(ttCloseBrack);
  ArrayDecl := TArrayDecl.Create(Ident, ParseDeclList([ttFunc, ttVal], 'array'), Token);
  ArrayDecl.Elements.AddRange(ExprList);
  Expect(ttEnd);
  Result := ArrayDecl;
end;

function TParser.ParseKeyValueList: TKeyValueList;
var
  Key, Value: TExpr;
begin
  Result := TKeyValueList.Create();
  Key := ParseExpr;
  Expect(ttColon);
  Value := ParseExpr;
  Result.Add(Key, Value);
  while CurrentToken.Typ = ttComma do begin
    Next;  // skip ,
    Key := ParseExpr;
    Expect(ttColon);
    Value := ParseExpr;
    Result.Add(Key, Value);
  end;
end;

function TParser.ParseDictDecl: TDecl;
var
  Token: TToken;
  Ident: TIdent;
  DictDecl: TDictDecl;
  KeyValueList: TKeyValueList;
begin
  Token := CurrentToken;
  Next;  // skip dictionary
  Ident := ParseIdent;
  Expect(ttOpenBrack);
  case CurrentToken.Typ of
    ttColon: begin Next; KeyValueList := TKeyValueList.Create(); end;
    ttCloseBrack: Expect(ttColon);
    else KeyValueList := ParseKeyValueList;
  end;
  Expect(ttCloseBrack);
  DictDecl :=
    TDictDecl.Create(Ident, KeyValueList, ParseDeclList([ttFunc, ttVal], 'dictionary'), Token);
  Expect(ttEnd);
  Result := DictDecl;
end;

function TParser.ParseEnumDecl: TDecl;
var
  EnumDecl: TEnumDecl;
  Token: TToken;
  SetName: String = '';

  procedure AddNameValue(const ASetName: String);
  var
    Name: TIdent;
    Value: Variant;
  begin
    Name := ParseIdent;
    if EnumDecl.Elements.ContainsKey(Name.Text) then
      Error(Name.Token, Format(ErrDuplicateEnum, [Name.Text]));
    Value := Null;
    if CurrentToken.Typ = ttEQ then begin
      Next;  // skip =
      if CurrentToken.Typ in [ttNumber, ttString, ttChar] then begin
        Value := CurrentToken.Value;
        Next;
      end
      else
        Error(CurrentToken, Format(ErrNotAllowedInEnum, [CurrentToken.Lexeme]));
    end;
    EnumDecl.AddElement(ASetName, Name.Text, Value);
  end;

begin
  Token := CurrentToken;
  Next;  // skip enum
  EnumDecl := TEnumDecl.Create(ParseIdent, TDeclList.Create(False), Token);
  Expect(ttOpenParen);
  if CurrentToken.Typ <> ttCloseParen then begin
    while CurrentToken.Typ <> ttCloseParen do begin
      if CurrentToken.Typ = ttCase then begin
        Next;
        SetName := ParseIdent.Text;
        if EnumDecl.CaseTable.Contains(SetName) then
          Error(CurrentToken, Format(ErrDuplicateSetName, [SetName]))
        else EnumDecl.CaseTable.Add(SetName);
        Expect(ttColon);
      end;
      AddNameValue(SetName);   // at least expect 1 enumeration
      while CurrentToken.Typ = ttComma do begin
        Next;  // skip ,
        AddNameValue(SetName);
      end;
      SetName := '';
    end;
  end
  else Error(CurrentToken, ErrAtLeastOneEnum);
  Expect(ttCloseParen);
  // other declarations: val and func
  EnumDecl.DeclList.AddRange(ParseDeclList([ttFunc, ttVal], 'enum'));
  Expect(ttEnd);
  Result := EnumDecl;
end;

function TParser.ParseNode: TNode;
begin
  try
    if DeclStartSet.Contains(CurrentToken.Typ) then
      Result := ParseDecl
    else if StmtStartSet.Contains(CurrentToken.Typ) then
      Result := ParseStmt
    else
      Error(CurrentToken, ErrUnrecognizedDeclOrStmt);
  except
    Synchronize(DeclStartSet + StmtStartSet + [ttEOF]);
    Result := TNode.Create(CurrentToken);
  end;
end;

const
  BlockEndSet: TTokenTypSet = [ttElse, ttElseif, ttUntil, ttEnd, ttCase, ttEOF];

function TParser.ParseBlock: TBlock;
begin
  Result := TBlock.Create(TNodeList.Create(), CurrentToken);
  while not BlockEndSet.Contains(CurrentToken.Typ) do
    Result.Nodes.Add(ParseNode);
end;


function TParser.ParseProduct: TProduct;
var
  Token: TToken;
begin
  Token := CurrentToken;
  Result := TProduct.Create(ParseBlock.Nodes, Token);
end;


end.


