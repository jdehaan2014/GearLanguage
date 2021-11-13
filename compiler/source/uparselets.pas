unit uParselets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uParser, uAST, uToken, uPrecedence, uError, uCommon;

type

  {Generic infix parselet for a binary arithmetic operator. The only
   difference when parsing, "+", "-", "*", "/", and "^" is precedence and
   associativity, so we can use a single parselet class for all of those.}
   TBinaryOperatorParselet = class(IInfixParselet)
     Precedence: Integer;
     IsRight: Boolean;
     constructor Create(APrecedence: Integer; AIsRight: Boolean);
     function Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
     function getPrecedence: Integer;
   end;

   // parse binary 'and' operator
   TAndParselet = class(IInfixParselet)
     function Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
     function getPrecedence: Integer;
   end;

   // parse binary 'or' operator
   TOrParselet = class(IInfixParselet)
     function Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
     function getPrecedence: Integer;
   end;

   //Parselet for the condition or "ternary" operator, like "a ? b : c".
   TConditionalParselet = class(IInfixParselet)
     function Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
     function getPrecedence: Integer;
   end;

   //Parselet to parse a function call like "a(b, c, d)".
   TCallParselet = class(IInfixParselet)
     function Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
     function getPrecedence: Integer;
     private
       function getSignature(Callee: TExpr; const Signature: String): TExpr;
   end;

   //Parses parentheses used to group an expression, like "a * (b + c)".
   TParenthesisParselet = class(IPrefixParselet)
     function Parse(Parser: TParser; Token: TToken): TExpr;
   end;

   // Generic prefix parselet for an unary arithmetic operator. Parses prefix
   // unary "-", "+", "not" and "?" expressions.
   TPrefixOperatorParselet = class(IPrefixParselet)
     Precedence: Integer;
     constructor Create(APrecedence: Integer);
     function Parse(Parser: TParser; Token: TToken): TExpr;
     function getPrecedence: Integer;
   end;

   //Generic infix parselet for an unary arithmetic operator. Parses postfix unary "?" expressions.
   //TPostfixOperatorParselet = class(IInfixParselet)
   //  Precedence: Integer;
   //  constructor Create(APrecedence: Integer);
   //  function Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
   //  function getPrecedence: Integer;
   //end;

   //Simple parselet for a named variable like "abc".
   TVariableParselet = class(IPrefixParselet)
     function Parse(Parser: TParser; Token: TToken): TExpr;
   end;

   //Simple parselet for a constant values: numbers, strings, False, True, Null.
   TConstantParselet = class(IPrefixParselet)
     function Parse(Parser: TParser; Token: TToken): TExpr;
   end;

   // parses '=' and '<>' operators
   TEqualityOperatorParselet = class(IInfixParselet)
     function Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
     function getPrecedence: Integer;
   end;

   // parses '>', '>=', '<' and '<=' operators
   TComparisonOperatorParselet = class(IInfixParselet)
     function Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
     function getPrecedence: Integer;
   end;

   //Parses 'if' expression used in if Condition then Expr else Expr.
   TIfThenParselet = class(IPrefixParselet)
     function Parse(Parser: TParser; Token: TToken): TExpr;
   end;

   //Parses 'match' used in match Expr { if Expr then Expr }.
   TMatchParselet = class(IPrefixParselet)
     function Parse(Parser: TParser; Token: TToken): TExpr;
   end;

   // parses 'instance.member'
   TGetExprParselet = class(IInfixParselet)
     function Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
     function getPrecedence: Integer;
   end;

   //Parses 'self'  .
   TSelfParselet = class(IPrefixParselet)
     function Parse(Parser: TParser; Token: TToken): TExpr;
   end;

   //Parses '@' to expand to 'self.' used in GetExpr .
   TAtSignParselet = class(IPrefixParselet)
     function Parse(Parser: TParser; Token: TToken): TExpr;
   end;

   //Parses 'inherited' used in inherited method .
   TInheritedParselet = class(IPrefixParselet)
     function Parse(Parser: TParser; Token: TToken): TExpr;
   end;

   // parses interpolated strings 'text $(value)'
   TInterpolatedParselet = class(IPrefixParselet)
     function Parse(Parser: TParser; Token: TToken): TExpr;
   end;

   // parses an anonymous function as an expression, i.e. lambda expression
   TLambdaParselet = class(IPrefixParselet)
     function Parse(Parser: TParser; Token: TToken): TExpr;
   end;

   // parses indexed expressions: e.g. a[i]
   TIndexedParselet = class(IInfixParselet)
     function Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
     function getPrecedence: Integer;
   end;

   // parses array or dictionary as an expression, [a,b,c] or [a:aa, b:bb, c:cc]
   // or parses [ x for x in [1,2,3,4,5,6,7,8,9] where x%2=0 ] list builder expression
   TBracketParselet = class(IPrefixParselet)
     function Parse(Parser: TParser; Token: TToken): TExpr;
     private
       function ParseArrayExpr(Parser: TParser; FirstExpr: TExpr): TExpr;
       function ParseDictionaryExpr(Parser: TParser; KeyExpr: TExpr): TExpr;
       function ParseListBuilderExpr(Parser: TParser; Transform: TExpr): TExpr;
   end;

   // parses range (or dotted) expr: a..b
   TDotDotParselet = class(IInfixParselet)
     function Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
     function getPrecedence: Integer;
     private
       procedure CheckExpr(Parser: TParser; Expr: TExpr);
   end;

   // parses a set expression or a set builder expression
   TBracesParselet = class(IPrefixParselet)
     function Parse(Parser: TParser; Token: TToken): TExpr;
     private
       function ParseSetExpr(Parser: TParser; FirstExpr: TExpr): TExpr;
       function ParseSetBuilderExpr(Parser: TParser; Transform: TExpr): TExpr;
   end;


implementation

{ TBinaryOperatorParselet }

constructor TBinaryOperatorParselet.Create(APrecedence: Integer; AIsRight: Boolean);
begin
  Precedence := APrecedence;
  IsRight := AIsRight;
end;

// To handle right-associative operators like "^", we allow a slightly
// lower precedence when parsing the right-hand side. This will let a
// parselet with the same precedence appear on the right, which will then
// take *this* parselet's result as its left-hand argument.

function TBinaryOperatorParselet.Parse(Parser: TParser; Left: TExpr;
  Token: TToken): TExpr;
var
  Right: TExpr;
begin
  Parser.IgnoreNewlines;
  Right := Parser.ParseExpr(Precedence - specialize IfThen<Integer>(IsRight, 1, 0));
  Result := TBinaryExpr.Create(Left, Token.Typ, Right, Token.Location);
end;

function TBinaryOperatorParselet.getPrecedence: Integer;
begin
  Result := Precedence;
end;

{ TAndParselet }
// parses LeftExpr 'and' RightExpr
function TAndParselet.Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
var
  Right: TExpr;
begin
  Parser.IgnoreNewlines;
  Right := Parser.ParseExpr;
  Result := TAndExpr.Create(Left, Right, Token.Location);
end;

function TAndParselet.getPrecedence: Integer;
begin
  Result := precAnd;
end;

{ TOrParselet }
// parses LeftExpr 'or' RightExpr
function TOrParselet.Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
var
  Right: TExpr;
begin
  Parser.IgnoreNewlines;
  Right := Parser.ParseExpr;
  Result := TOrExpr.Create(Left, Right, Token.Location);
end;

function TOrParselet.getPrecedence: Integer;
begin
  Result := precOr;
end;

{ TConditionalParselet
  Parses the true-or-false expression. The expression checks whether a boolean
  expression is true or false, and returns a value (or expression) based on the
  truthness or falseness of the boolean expression evaluation. e.g.:

  let heads := 1
  let tails  := 0
  randomize()
  var toss := random(2) // number between 0 and 2, excluding 2

  var throw := toss = heads?
    true: 'heads thrown'
    false: 'tails thrown

  ebnf:
  TrueFalseExpr = Condition '?' 'true:' Expr 'false:' Expr .
  The true and false part order may be switched, so is not fixed.
}

function TConditionalParselet.Parse(Parser: TParser; Left: TExpr; Token: TToken
  ): TExpr;
var
  ThenArm, ElseArm: TExpr;
begin
  Parser.IgnoreNewlines;
  if Parser.Match(ttTrue) then // first true then false
   begin
     Parser.Expect(ttColon);
     Parser.IgnoreNewlines;
     ThenArm := Parser.ParseExpr;
     Parser.IgnoreNewlines;
     Parser.Expect(ttFalse);
     Parser.Expect(ttColon);
     Parser.IgnoreNewlines;
     ElseArm := Parser.ParseExpr(precConditional-1);
   end
  else if Parser.Match(ttFalse) then // first false then true
   begin
     Parser.Expect(ttColon);
     Parser.IgnoreNewlines;
     ElseArm := Parser.ParseExpr;
     Parser.IgnoreNewlines;
     Parser.Expect(ttTrue);
     Parser.Expect(ttColon);
     Parser.IgnoreNewlines;
     ThenArm := Parser.ParseExpr(precConditional-1);
   end
  else
    Parser.Error('Expected true or false indicator.');

  Result := TTernaryExpr.Create(Left, ThenArm, ElseArm);
end;

function TConditionalParselet.getPrecedence: Integer;
begin
  Result := precConditional;
end;


{ TCallParselet }

// Args = Empty | Arg { ',' Arg } .
// Arg = [ Ident ':' ] Expr .
function TCallParselet.Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
var
  CallExpr: TCallExpr;
  Arguments: TArgumentList;
  Name: TIdent;                 // alternative parameter name defined in TParameter
  Expr: TExpr;                  // actual expression argument
  ParamNames: String = '';
begin
  Arguments := TArgumentList.Create();
  // Parse the comma-separated arguments until we hit, ")".
  Parser.IgnoreNewlines;
  if not Parser.Match(ttRightParen) then
    begin
      repeat
        Parser.IgnoreNewlines;
        Name := Nil;                        // start with nil
        Expr := Parser.ParseExpr;           // parse expression, which might be an ident
        if Parser.Match(ttColon) then       // possible parameter name detected
          begin
            if Expr is TVariable then       // then it must be a variable identifier
              begin
                Name := TVariable(Expr).Name; // Name parsed as Expr
                ParamNames += '#' + Name.Text + ':'; // build the call signature
              end
            else
              Parser.Error('Expected variable name of parameter.');
            Expr := Parser.ParseExpr;         // parse actual argument expression
          end;
        Arguments.Add(TArgument.Create(Name, Expr)); // add to the argument list
      until not Parser.Match(ttComma);
      Parser.IgnoreNewlines;
      Parser.Expect(ttRightParen);
    end;
  // create the call expr with the callee (Left) and the arguments
  CallExpr := TCallExpr.Create(Left, Arguments, Token.Location);
  // create call signature, consisting of callee + named arguments, separated by #
  CallExpr.Signature := getSignature(Left, ParamNames);
  Result := CallExpr;
  Parser.AllowLineBeforeDot;
end;

function TCallParselet.getPrecedence: Integer;
begin
  Result := precCall;
end;

function TCallParselet.getSignature(Callee: TExpr; const Signature: String): TExpr;
begin
  if Signature <> '' then
    begin
      // a callee with parameter names can be a function or a method
      // signature = callee#name1:#name2:#name3:
      // the signature is the basis for overloading functions with the same callee
      if Callee is TVariable then with Callee as TVariable do
        Result := TVariable.Create(TIdent.Create(Name.Text+Signature, Name.Location))
      else if Callee is TGetExpr then with Callee as TGetExpr do
        Result := TGetExpr.Create(Instance, TVariable.Create(
          TIdent.Create(TVariable(Member).Name.Text+Signature, Member.Location)));
    end
  else
    Result := Callee;
end;

{ TParenthesisParselet }
// Tuple = '(' (Ident ':' | '.') Expr { ',' (Ident ':' | '.') Expr } ')' .
function TParenthesisParselet.Parse(Parser: TParser; Token: TToken): TExpr;
var
  Expr: TExpr;
  Name: TIdent;
  Arguments: TArgumentList;
begin
  Parser.IgnoreNewlines;
  if (Parser.Current.Typ = ttDot) or (Parser.Peek.Typ = ttColon) then   // check for a tuple list
    begin
      Arguments := TArgumentList.Create();
      repeat
        Parser.IgnoreNewlines;
        if Parser.Match(ttDot) then // variable name becomes the tuple name
          begin
            Expr := Parser.ParseExpr;  // the expr must be a variable identifier
            if Expr is TVariable then
              Arguments.Add(TArgument.Create((Expr as TVariable).Name, Expr))
            else
              Parser.Error('Identifier expected.');
          end
        else
          begin // otherwise we expect a name:expr pair
            Name := Parser.ParseIdent;
            Parser.Expect(ttColon);
            Parser.IgnoreNewlines;
            Arguments.Add(TArgument.Create(Name, Parser.ParseExpr));
          end;
      until not Parser.Match(ttComma);
      Result := TTupleExpr.Create(Arguments, Token.Location);
      Parser.IgnoreNewlines;
      Parser.Expect(ttRightParen);
    end
  else  // it is a grouped expression '(' Expr ')'
    begin
      Result := Parser.ParseExpr;
      Parser.IgnoreNewlines;
      Parser.Expect(ttRightParen, 'Expect ")" after expression.');
    end;
end;


{ TPrefixOperatorParselet }

constructor TPrefixOperatorParselet.Create(APrecedence: Integer);
begin
  Precedence := APrecedence;
end;

function TPrefixOperatorParselet.Parse(Parser: TParser; Token: TToken): TExpr;
var
  Right: TExpr;
begin
  Right := Parser.parseExpr(Precedence);
  Result := TUnaryExpr.Create(Token.Typ, Right, Token.Location);
end;

function TPrefixOperatorParselet.getPrecedence: Integer;
begin
  Result := Precedence;
end;


{ TPostfixOperatorParselet }

//constructor TPostfixOperatorParselet.Create(APrecedence: Integer);
//begin
//  Precedence := APrecedence;
//end;
//
//function TPostfixOperatorParselet.Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
//begin
//  Result := TPostfixExpr.Create(Left, Token.Typ, Token);
//end;
//
//function TPostfixOperatorParselet.getPrecedence: Integer;
//begin
//  Result := Precedence;
//end;

{ TVariableParselet }
// parse a variable or a single variable lambda expression e.g. x=>x^2
// If a variable starts with an underscore, e.g. '_name' then we check if it
// is a private class field, and whether it's part of 'self._name' or stand-alone.
// If stand-alone and class field, we expand it to 'self._name'.
function TVariableParselet.Parse(Parser: TParser; Token: TToken): TExpr;
var
  Ident: TIdent;
  Params: TParameterList;
  Body: TBody;
begin
  Ident := TIdent.Create(Token); // create variable identifier from the token: x
  if Parser.Current.Typ = ttArrow then // is it a lambda expression: x =>
    begin
      Params := TParameterList.Create();
      Params.Add(TParameter.Create(TVariable.Create(Ident), Nil)); // add parameter: 'x'
      Body := TBody.Create(TNodeList.Create(), Parser.Current.Location);
      Body.Nodes.Add(Parser.ParseReturnStmt); // add return stmt: x^2
      Result := TLambdaExpr.Create(
        TFuncDecl.Create(Params, ffAnonymArrow, Body, Body.Location)); // x=>x^2
    end
  else // it is a variable
    begin
      // check if it starts with an underscore. If so, expand to self._field
      if (not Parser.SelfUsed) and (Ident.Text[1] = '_') and Parser.InsideClassDecl then
        Result := TGetExpr.Create(
          TSelfExpr.Create(TIdent.Create('self', Ident.Location)),
          TVariable.Create(Ident))
      else
        Result := TVariable.Create(Ident);
    end;
end;

{ TConstantParselet }
// parse constants and literals
function TConstantParselet.Parse(Parser: TParser; Token: TToken): TExpr;
begin
  case Token.Typ of
    ttString: Result := TStringExpr.Create(Token);
    ttNumber: Result := TNumberExpr.Create(Token);
    ttCharacter: Result := TCharExpr.Create(Token);
    ttFalse: Result := TLiteralExpr.Create(ltFalse, Token.Location);
    ttTrue: Result := TLiteralExpr.Create(ltTrue, Token.Location);
    ttNil: Result := TLiteralExpr.Create(ltNil, Token.Location);
  end;
end;

{ TEqualityOperatorParselet }
// parse equal: Left = Right  or not equal: Left <> Right
function TEqualityOperatorParselet.Parse(Parser: TParser; Left: TExpr;
  Token: TToken): TExpr;
var
  Right: TExpr;
begin
  Parser.IgnoreNewlines;
  Right := Parser.ParseExpr(precEquality);
  if Parser.Current.Typ in [ttEQ, ttNEQ] then
    AddError(Token.Location, 'Multiple equality operators in one expression not allowed.');
  Result := TBinaryExpr.Create(Left, Token.Typ, Right, Token.Location);
end;

function TEqualityOperatorParselet.getPrecedence: Integer;
begin
  Result := precEquality;
end;

{ TComparisonOperatorParselet }
// parse greater: Left > Right; greater equal: Left >= Right;
// less: Left < Right; less equal: Left <= Right;
function TComparisonOperatorParselet.Parse(Parser: TParser; Left: TExpr;
  Token: TToken): TExpr;
var
  Right: TExpr;
begin
  Parser.IgnoreNewlines;
  Right := Parser.ParseExpr(precComparison);
  if Parser.Current.Typ in [ttGT, ttGE, ttLT, ttLE] then
    AddError(Token.Location, 'Multiple comparison operators in one expression not allowed.');
  Result := TBinaryExpr.Create(Left, Token.Typ, Right, Token.Location);
end;

function TComparisonOperatorParselet.getPrecedence: Integer;
begin
  Result := precComparison;
end;

{ TIfThenParselet }
// parse if-expression: ebnf:
// if-expr = 'if' Condition 'then' TrueExpr 'else' FalseExpr .
function TIfThenParselet.Parse(Parser: TParser; Token: TToken): TExpr;
var
  Condition, TrueExpr, FalseExpr: TExpr;
begin
  Parser.IgnoreNewlines;
  Condition := Parser.ParseExpr;
  Parser.IgnoreNewlines;
  Parser.Expect(ttThen);
  Parser.IgnoreNewlines;
  TrueExpr := Parser.ParseExpr;
  Parser.IgnoreNewlines;
  Parser.Expect(ttElse);
  Parser.IgnoreNewlines;
  FalseExpr := Parser.ParseExpr(precConditional-1);
  Result := TTernaryExpr.Create(Condition, TrueExpr, FalseExpr);
end;

{ TMatchParselet }

{ parse match-expr, ebnf:
  match-expr = 'match' Expr 'if' Expr ':' Expr { 'if' Expr ':' Expr } 'else' ':' Expr .
  example:
  func fib(n) =>
    match n
      if 0: 0
      if 1: 1
      else: fib(n-1) + fib(n-2

  it's also possible to use comparison operators, like: =, <>, <, <=, >, >=
  as well as 'in' and 'is'
  match n
    if <= 1: expression
    if in list: expression
    if is Class: expression
}
function TMatchParselet.Parse(Parser: TParser; Token: TToken): TExpr;
var
  Match, Value, Expr, ElseLimb: TExpr;
  IfLimbs: TBinaryExprMap;
  Values: TExprList;
  Op: TTokenTyp;
begin
  Parser.IgnoreNewlines;
  Match := Parser.ParseExpr;
  Parser.IgnoreNewlines;
  IfLimbs := TBinaryExprMap.Create;
  Parser.Expect(ttIf, 'Expect "if" after match expression.'); // minimum 1 if required
  repeat
    // check for operator
    if Parser.Current.Typ in ComparisonOperators then
      begin
        Op := Parser.Current.Typ;
        Parser.Next;
      end
    else // if no operator used, the default is '='
      Op := ttEq;

    Values := Parser.ParseExprListUntil(ttColon); // comma separated expressions
    Parser.IgnoreNewlines;
    if Values.Count = 0 then
      Parser.Error('Minimum 1 value expected in if clause.')
    else
      begin // after the colon :
        Expr := Parser.ParseExpr;
        for Value in Values do // create list of binary expressions
          IfLimbs.Add(TBinaryExpr.Create(Match, Op, Value, Value.Location), Expr);
      end;
    Parser.ExpectLine;
  until not Parser.Match(ttIf);
  Parser.Expect(ttElse);  // else is mandatory
  Parser.IgnoreNewlines;
  ElseLimb := Parser.ParseExpr;
  Result := TMatchExpr.Create(Match, IfLimbs, ElseLimb);
end;

{ TGetExprParselet }
// parse instance.member
function TGetExprParselet.Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
var
  Member: TExpr;
  SafetyOn: Boolean;
begin
  // Token contains '.' or '?.'
  SafetyOn := Token.Typ = ttQuestionDot;
  Parser.IgnoreNewlines;

  // SelfUsed initiates special processing in Variable.Parse
  if Left is TSelfExpr then
    Parser.SelfUsed := True;
  Member := Parser.ParseExpr(precPrimary);
  Parser.SelfUsed := False;
  Result := TGetExpr.Create(Left, Member, SafetyOn);
end;

function TGetExprParselet.getPrecedence: Integer;
begin
  Result := precCall;
end;

{ TSelfParselet }
// parse 'self'
function TSelfParselet.Parse(Parser: TParser; Token: TToken): TExpr;
begin
  Result := TSelfExpr.Create(TIdent.Create(Token));
end;

{ TAtSignParselet }
// expand '@' sign to 'self.' and return TGetExpr.
function TAtSignParselet.Parse(Parser: TParser; Token: TToken): TExpr;
var
  Member, SelfExpr: TExpr;
begin
  SelfExpr := TSelfExpr.Create(Parser.MakeID('self'));
  Member := Parser.ParseExpr(precPrimary);
  Result := TGetExpr.Create(SelfExpr, Member, False);
end;

{ TInheritedParselet }
// parse: InheritedExpr = 'inherited' (MethodIdent | 'init' | empty )  '(' Parameters ')' .
function TInheritedParselet.Parse(Parser: TParser; Token: TToken): TExpr;
var
  Method: TVariable;
  Arguments: TExprList=Nil;
  ArgCount: Integer=-1;
begin
  // if directly after the keyword inherited we detect parentheses, it is
  // expanded to "inherited init()"
  if Parser.Current.Typ = ttLeftParen then
    Method := TVariable.Create(TIdent.Create('init', Token.Location))
  else // we parse a method identifier, which may also be init
    Method := TVariable.Create(Parser.ParseIdent);

  Parser.IgnoreNewlines;
  Parser.Expect(ttLeftParen);     // parens required
  Arguments := Parser.ParseExprListUntil(ttRightParen);  // parse possible arguments
  ArgCount := Arguments.Count;

  Result := TInheritedExpr.Create(Token, Method, Arguments, ArgCount);
end;

{ TInterpolatedParselet }

// Parse the interpolated string as a binary + expression
// After fully parsing the result is a binary addition tree
// An interpolated string consists of
// - an opening string
// - multiple interpolations
// - a closing string
function TInterpolatedParselet.Parse(Parser: TParser; Token: TToken): TExpr;
var
  Expr: TExpr;
begin
  Result := TStringExpr.Create(Token); // Opening string
  Expr := Parser.ParseExpr;
  Result := TBinaryExpr.Create(Result, ttPlus, Expr, Expr.Location);
  Token := Parser.Consume;

  while Token.Typ = ttInterpolated do // multiple non-nested interpolations
    begin
      Parser.IgnoreNewlines;
      Result := TBinaryExpr.Create(Result, ttPlus, TStringExpr.Create(Token), Token.Location);
      Expr := Parser.ParseExpr;
      Parser.IgnoreNewlines;
      Result := TBinaryExpr.Create(Result, ttPlus, Expr, Expr.Location);

      Token := Parser.Consume;
    end;

  if Token.Typ = ttString then // closing string
    Result := TBinaryExpr.Create(Result, ttPlus, TStringExpr.Create(Token), Token.Location)
  else
    Parser.Error('Expected end of string interpolation.');
end;

{ TLambdaParselet }
// A lambda is an anonymous function expression defined as follows:
// LambdaExpr = ('lambda' | '/\' ) Parameters ( '=>' Expr | 'do' Body 'end' ) .
function TLambdaParselet.Parse(Parser: TParser; Token: TToken): TExpr;
var
  Parameters: TParameterList;
  Body: TBody;
  Return: TToken;
  FuncForm: TFuncForm=ffAnonymArrow;
begin
  if Parser.Match(ttLeftParen) then // after lambda we have a left paren
    begin
      Parameters := TParameterList.Create(); // initialize parameters
      if Parser.Current.Typ <> ttRightParen then
      repeat // add parameters
        Parser.IgnoreNewlines;
        // lambda's do not have alternative names for the parameters, hence Nil
        Parameters.Add(TParameter.Create(TVariable.Create(Parser.ParseIdent), Nil));
      until not Parser.Match(ttComma);
      Parser.IgnoreNewlines;
      Parser.Expect(ttRightParen);
    end
  else
    begin // if no parens, then the variable IS the parameters
      Parameters := TParameterList.Create();
      Parameters.Add(TParameter.Create(TVariable.Create(Parser.ParseIdent), Nil));
    end;

  if Parser.Match(ttArrow) then // is it an arrow function?
    begin // parse return stmt, and add to func body
      Parser.IgnoreNewlines;
      Return := Parser.Current;
      Body := TBody.Create(TNodeList.Create(), Return.Location);
      Body.Nodes.Add(TReturnStmt.Create(Parser.ParseExpr, Return.Location));
    end
  else if Parser.Match(ttDo) then // parse: 'do' Body 'end'
    begin
      Parser.IgnoreNewlines;
      Body := Parser.ParseBody;
      Parser.IgnoreNewlines;
      Parser.Expect(ttEnd);
      FuncForm := ffAnonym;
    end
  else
    Parser.Error('Expected "=>" for function expression or "do" before function body.');

  Result := TLambdaExpr.Create(TFuncDecl.Create(Parameters, FuncForm, Body, Token.Location));
end;


{ TIndexedParselet }
// parse: variable[index] or dictionary[key]
function TIndexedParselet.Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
begin
  Result := TIndexedExpr.Create(Left, Parser.ParseExpr);
  Parser.Expect(ttRightBracket);
  Parser.AllowLineBeforeDot;
end;

function TIndexedParselet.getPrecedence: Integer;
begin
  Result := precCall;
end;

{ TBracketParselet }
// parse empty array []; empty dictionary [:] or
// parse array or dictionary expressions or
// parse list builder expression
function TBracketParselet.Parse(Parser: TParser; Token: TToken): TExpr;
var
  Expr: TExpr;
begin
  case Parser.Current.Typ of
    // an empty array []
    ttRightBracket: Result := TArrayExpr.Create(TExprList.Create(), Token.Location);
    // an empty dictionary [:]
    ttColon:
      begin
        Result := TDictionaryExpr.Create(TKeyValueMap.Create, Token.Location);
        Parser.Next;
      end
    else // a filled array or dictionary
      Parser.IgnoreNewlines;
      Expr := Parser.ParseExpr; // parse the first expr
      Parser.IgnoreNewlines;
      case Parser.Current.Typ of
        ttColon: Result := ParseDictionaryExpr(Parser, Expr); // a dictionary
        ttFor: Result := ParseListBuilderExpr(Parser, Expr) // a list builder
        else
          Result := ParseArrayExpr(Parser, Expr); // an array
      end;
  end;
  Parser.IgnoreNewlines;
  Parser.Expect(ttRightBracket);   // final closing bracket
end;

// parses [a,b,c,d,...]
function TBracketParselet.ParseArrayExpr(Parser: TParser; FirstExpr: TExpr): TExpr;
var
  Elements: TExprList;
begin
  Elements := TExprList.Create();
  Elements.Add(FirstExpr);
  while Parser.Match(ttComma) do
    begin
        Parser.IgnoreNewlines;
        Elements.Add(Parser.ParseExpr);
    end;
  Result := TArrayExpr.Create(Elements, FirstExpr.Location);
end;

// parses [a:aa,b:bb,c:cc,d:dd,...]
function TBracketParselet.ParseDictionaryExpr(Parser: TParser; KeyExpr: TExpr): TExpr;
var
  Elements: TKeyValueMap;
  Key: TExpr;
begin
  Elements := TKeyValueMap.Create;
  Parser.Expect(ttColon);
  Parser.IgnoreNewlines;
  Elements.Add(KeyExpr, Parser.ParseExpr);
  while Parser.Match(ttComma) do
    begin
      Parser.IgnoreNewlines;
      Key := Parser.ParseExpr;
      Parser.Expect(ttColon);
      Parser.IgnoreNewlines;
      Elements.Add(Key, Parser.ParseExpr);
    end;
  Result := TDictionaryExpr.Create(Elements, KeyExpr.Location);
end;

// parses [mapExpr for loopVar in sequenceExpr where filterExpr]
function TBracketParselet.ParseListBuilderExpr(Parser: TParser; Transform: TExpr): TExpr;
var
  Sequence: TExpr;
  Where: TExpr = Nil;
  Variable: TVariable;
  Map, Filter: TLambdaExpr; // the transform and predicate functions
  Location: TLocation;
begin
  // first step: parse all elements of set builder expression
  Parser.IgnoreNewlines;
  Parser.Expect(ttFor);
  Variable := TVariable.Create(Parser.ParseIdent);
  Parser.Expect(ttIn);
  Parser.IgnoreNewlines;
  Sequence := Parser.ParseExpr;
  Parser.IgnoreNewlines;
  Location := Parser.Current.Location;
  if Parser.Match(ttWhere) then // if where clause then parse predicate expr
    begin
      Parser.IgnoreNewlines;
      Where := Parser.ParseExpr;
      Parser.IgnoreNewlines;
    end
  else
    Where := TLiteralExpr.Create(ltTrue, Location);

  // second step: create the functions map and filter
  Map := TLambdaExpr.Create(TFuncDecl.Create(TParameterList.Create(),
    ffAnonymArrow, TBody.Create(TNodeList.Create(), Location), Location));
  Map.Func.Parameters.Add(TParameter.Create(Variable, Nil));
  Map.Func.Body.Nodes.Add(TReturnStmt.Create(Transform, Location));

  Filter := TLambdaExpr.Create(TFuncDecl.Create(TParameterList.Create(),
    ffAnonymArrow, TBody.Create(TNodeList.Create(), Location), Location));
  Filter.Func.Parameters.Add(TParameter.Create(Variable, Nil));
  Filter.Func.Body.Nodes.Add(TReturnStmt.Create(Where, Location));

  Result := TListBuilderExpr.Create(lbtArray, Map, Sequence, Filter, Location);
end;

//function TBracketParselet.ParseListBuilderExpr(Parser: TParser; Transform: TExpr): TExpr;
//var
//  Sequence: TExpr;
//  Where: TExpr = Nil;
//  Variable: TVariable;
//  Map, Filter: TLambdaExpr; // the transform and predicate functions
//  ListBuilder: TCallExpr;
//  Arguments: TArgumentList;
//  Location: TLocation;
//begin
//  // first step: parse all elements of set builder expression
//  Parser.IgnoreNewlines;
//  Parser.Expect(ttFor);
//  Variable := TVariable.Create(Parser.ParseIdent);
//  Parser.Expect(ttIn);
//  Parser.IgnoreNewlines;
//  Sequence := Parser.ParseExpr;
//  Parser.IgnoreNewlines;
//  Location := Parser.Current.Location;
//  if Parser.Match(ttWhere) then // if where clause then parse predicate expr
//    begin
//      Parser.IgnoreNewlines;
//      Where := Parser.ParseExpr;
//      Parser.IgnoreNewlines;
//    end
//  else
//    Where := TLiteralExpr.Create(ltTrue, Location);
//
//  // second step: create the functions map and filter
//  Map := TLambdaExpr.Create(TFuncDecl.Create(TParameterList.Create(),
//    ffAnonymArrow, TBody.Create(TNodeList.Create(), Location), Location));
//  Map.Func.Parameters.Add(TParameter.Create(Variable, Nil));
//  Map.Func.Body.Nodes.Add(TReturnStmt.Create(Transform, Location));
//
//  Filter := TLambdaExpr.Create(TFuncDecl.Create(TParameterList.Create(),
//    ffAnonymArrow, TBody.Create(TNodeList.Create(), Location), Location));
//  Filter.Func.Parameters.Add(TParameter.Create(Variable, Nil));
//  Filter.Func.Body.Nodes.Add(TReturnStmt.Create(Where, Location));
//
//  // third step: create call to func listBuilder([], map, sequence, filter)
//  Arguments := TArgumentList.Create(); // first create arguments to call
//  Arguments.Add(TArgument.Create(Nil, TArrayExpr.Create(TExprList.Create(), Location))); // []
//  Arguments.Add(TArgument.Create(Nil, Map));
//  Arguments.Add(TArgument.Create(Nil, Sequence));
//  Arguments.Add(TArgument.Create(Nil, Filter));
//
//  // the function 'listBuilder' is a Gear func defined in the parser
//  ListBuilder := TCallExpr.Create(
//    TVariable.Create(Parser.MakeID('listBuilder')), Arguments, Location);
//
//  Result := ListBuilder;
//end;

{ TDotDotParselet }
// parses a..b or a..<b
function TDotDotParselet.Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
var
  Right: TExpr;
  isInclusive: Boolean;
begin
  CheckExpr(Parser, Left);                      // check lowerbound
  isInclusive := Token.Typ = ttDotDot;          // check if .. or ..< is used
  Right := Parser.ParseExpr;                    // parse upperbound
  CheckExpr(Parser, Right);                     // check upperbound

  Result := TRangeExpr.Create(Left, Right, isInclusive, Token.Location);
end;


//function TDotDotParselet.Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
//var
//  Right: TExpr;
//  Exclude: Boolean;
//  One: TNumberExpr;
//  Args: TArgumentList;
//begin
//  CheckExpr(Parser, Left);                      // check lowerbound
//  Exclude := Token.Typ = ttDotDotLeft;          // check if ..< is used
//  Right := Parser.ParseExpr;                    // parse upperbound
//  CheckExpr(Parser, Right);                     // check upperbound
//  if Exclude then
//    begin
//      One := TNumberExpr.Create(1, Right.Location);
//      Right := TBinaryExpr.Create(Right, ttMinus, One, Right.Location);
//    end;
//  // build call to Range(Lower, Upper)
//  Args := TArgumentList.Create();
//  Args.Add(TArgument.Create(Nil, Left));
//  Args.Add(TArgument.Create(Nil, Right));
//  // The class Range is defined in ranges.gear, and the call inits the Range class
//  Result := TCallExpr.Create(TVariable.Create(Parser.MakeID('Range')), Args, Token.Location);
//end;

function TDotDotParselet.getPrecedence: Integer;
begin
  Result := precRange;
end;

// The range boundaries can be any expression, but if they are unary expressions, then
// there are a few limitations, e.g. 'not' and '?' cannot be used.
procedure TDotDotParselet.CheckExpr(Parser: TParser; Expr: TExpr);
const
  ErrNotAllowedInRange = '"%s" not allowed in range expression.';
var
  Op: TTokenTyp;
begin
  if Expr is TUnaryExpr then
    begin
      Op := (Expr as TUnaryExpr).Op;
      if Op in [ttNot, ttQuestion] then
        Parser.Error(Format(ErrNotAllowedInRange, [Op.toString]));
    end;
end;

{ TBracesParselet }
// Parses an empty set {} or a set expression {expr, expr, expr, ... } or
// parses a set builder expression { 'for' Expr 'in' ListExpr ['where' Expr] }
function TBracesParselet.Parse(Parser: TParser; Token: TToken): TExpr;
var
  Expr: TExpr;
begin
  Parser.IgnoreNewlines;
  if Parser.Match(ttRightBrace) then
    Result := TSetExpr.Create(TExprList.Create(), Token.Location) // an empty set
  else
    begin
      Expr := Parser.ParseExpr; // parse the first expr
      if Parser.Current.Typ = ttFor then
        Result := ParseSetBuilderExpr(Parser, Expr) // a set builder
      else
        Result := ParseSetExpr(Parser, Expr);
      Parser.IgnoreNewlines;
      Parser.Expect(ttRightBrace);   // final closing bracket
    end;
end;

function TBracesParselet.ParseSetExpr(Parser: TParser; FirstExpr: TExpr): TExpr;
var
  Elements: TExprList;
begin
  Elements := TExprList.Create();
  Elements.Add(FirstExpr);
  while Parser.Match(ttComma) do
    begin
      Parser.IgnoreNewlines;
      Elements.Add(Parser.ParseExpr);
    end;
  Result := TSetExpr.Create(Elements, FirstExpr.Location);
end;

// parses a set builder expression { 'for' Expr 'in' ListExpr ['where' Expr] }
// parses {mapExpr for loopVar in sequenceExpr where filterExpr}
function TBracesParselet.ParseSetBuilderExpr(Parser: TParser; Transform: TExpr): TExpr;
var
  Sequence: TExpr;
  Where: TExpr = Nil;
  Variable: TVariable;
  Map, Filter: TLambdaExpr; // the transform and predicate functions
  Location: TLocation;
begin
  Parser.IgnoreNewlines;
  Parser.Expect(ttFor);
  Variable := TVariable.Create(Parser.ParseIdent);
  Parser.Expect(ttIn);
  Parser.IgnoreNewlines;
  Sequence := Parser.ParseExpr;
  Parser.IgnoreNewlines;
  Location := Parser.Current.Location;
  if Parser.Match(ttWhere) then // if where clause then parse predicate expr
    begin
      Parser.IgnoreNewlines;
      Where := Parser.ParseExpr;
      Parser.IgnoreNewlines;
    end
  else
    Where := TLiteralExpr.Create(ltTrue, Location);

  Map := TLambdaExpr.Create(TFuncDecl.Create(TParameterList.Create(),
    ffAnonymArrow, TBody.Create(TNodeList.Create(), Location), Location));
  Map.Func.Parameters.Add(TParameter.Create(Variable, Nil));
  Map.Func.Body.Nodes.Add(TReturnStmt.Create(Transform, Location));

  Filter := TLambdaExpr.Create(TFuncDecl.Create(TParameterList.Create(),
    ffAnonymArrow, TBody.Create(TNodeList.Create(), Location), Location));
  Filter.Func.Parameters.Add(TParameter.Create(Variable, Nil));
  Filter.Func.Body.Nodes.Add(TReturnStmt.Create(Where, Location));

  Result := TListBuilderExpr.Create(lbtSet, Map, Sequence, Filter, Location);
end;



//function TBracesParselet.ParseSetBuilderExpr(Parser: TParser; Transform: TExpr): TExpr;
//var
//  Sequence: TExpr;
//  Where: TExpr = Nil;
//  Variable: TVariable;
//  Map, Filter: TLambdaExpr; // the transform and predicate functions
//  ListBuilder: TCallExpr;
//  Arguments: TArgumentList;
//  Location: TLocation;
//begin
//  // first step: parse all elements of set builder expression
//  Parser.IgnoreNewlines;
//  Parser.Expect(ttFor);
//  Variable := TVariable.Create(Parser.ParseIdent);
//  Parser.Expect(ttIn);
//  Parser.IgnoreNewlines;
//  Sequence := Parser.ParseExpr;
//  Parser.IgnoreNewlines;
//  Location := Parser.Current.Location;
//  if Parser.Match(ttWhere) then // if where clause then parse predicate expr
//    begin
//      Parser.IgnoreNewlines;
//      Where := Parser.ParseExpr;
//      Parser.IgnoreNewlines;
//    end
//  else
//    Where := TLiteralExpr.Create(ltTrue, Location);
//
//  // second step: create the functions map and filter
//  Map := TLambdaExpr.Create(TFuncDecl.Create(TParameterList.Create(),
//    ffAnonymArrow, TBody.Create(TNodeList.Create(), Location), Location));
//  Map.Func.Parameters.Add(TParameter.Create(Variable, Nil));
//  Map.Func.Body.Nodes.Add(TReturnStmt.Create(Transform, Location));
//
//  Filter := TLambdaExpr.Create(TFuncDecl.Create(TParameterList.Create(),
//    ffAnonymArrow, TBody.Create(TNodeList.Create(), Location), Location));
//  Filter.Func.Parameters.Add(TParameter.Create(Variable, Nil));
//  Filter.Func.Body.Nodes.Add(TReturnStmt.Create(Where, Location));
//
//  // third step: create call to func listBuilder([], map, sequence, filter)
//  Arguments := TArgumentList.Create(); // first create arguments to call
//  Arguments.Add(TArgument.Create(Nil, TSetExpr.Create(TExprList.Create(), Location))); // {}
//  Arguments.Add(TArgument.Create(Nil, Map));
//  Arguments.Add(TArgument.Create(Nil, Sequence));
//  Arguments.Add(TArgument.Create(Nil, Filter));
//
//  // the function 'listBuilder' is a Gear func defined in the parser
//  ListBuilder := TCallExpr.Create(
//    TVariable.Create(Parser.MakeID('listBuilder')), Arguments, Location);
//
//  Result := ListBuilder;
//end;

end.


