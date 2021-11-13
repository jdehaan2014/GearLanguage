unit uExprParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uParser, uLexer, uToken, uPrecedence;

type

{ The expression parser is a Pratt parser, already partly described in unit uParser.
  It's based on the following article:
  http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
  Unit uParselets contains all the respective mini-parsers for the expression types.
}

  TExprParser = class(TParser)
    public
      constructor Create(Lexer: TLexer);
    private
      procedure RegisterParselets;
      //procedure Postfix(const Typ: TTokenTyp; const Precedence: Integer);
      procedure Prefix(const Typ: TTokenTyp; const Precedence: Integer);
      procedure InfixLeft(const Typ: TTokenTyp; const Precedence: Integer);
      procedure InfixRight(const Typ: TTokenTyp; const Precedence: Integer);
  end;

implementation
uses uParselets;

{ TExprParser }

constructor TExprParser.Create(Lexer: TLexer);
begin
  inherited Create(Lexer);

  //register all parselets for the grammar
  RegisterParselets;
end;


procedure TExprParser.RegisterParselets;
begin
  // Register the ones that need special parselets.
  RegisterParselet(ttIdentifier, TVariableParselet.Create);
  RegisterParselet(ttNumber, TConstantParselet.Create);
  RegisterParselet(ttString, TConstantParselet.Create);
  RegisterParselet(ttCharacter, TConstantParselet.Create);
  RegisterParselet(ttFalse, TConstantParselet.Create);
  RegisterParselet(ttTrue, TConstantParselet.Create);
  RegisterParselet(ttNil, TConstantParselet.Create);

  RegisterParselet(ttQuestion, TConditionalParselet.Create);
  RegisterParselet(ttAtSign, TAtSignParselet.Create);
  RegisterParselet(ttIf, TIfThenParselet.Create);
  RegisterParselet(ttMatch, TMatchParselet.Create);
  RegisterParselet(ttLeftParen, TParenthesisParselet.Create);
  RegisterParselet(ttLeftParen, TCallParselet.Create);
  RegisterParselet(ttDot, TGetExprParselet.Create);
  RegisterParselet(ttQuestionDot, TGetExprParselet.Create);
  RegisterParselet(ttSelf, TSelfParselet.Create);
  RegisterParselet(ttInherited, TInheritedParselet.Create);
  RegisterParselet(ttInterpolated, TInterpolatedParselet.Create);
  RegisterParselet(ttLambda, TLambdaParselet.Create);
  RegisterParselet(ttLeftBracket, TIndexedParselet.Create);
  RegisterParselet(ttLeftBracket, TBracketParselet.Create);
  RegisterParselet(ttDotDot, TDotDotParselet.Create); // ..
  RegisterParselet(ttDotDotLeft, TDotDotParselet.Create); // ..<
  RegisterParselet(ttLeftBrace, TBracesParselet.Create);

  // Register the prefix / postfix operator parselets.
  Prefix(ttQuestion, precUnary);
  Prefix(ttPlus, precUnary);
  Prefix(ttMinus, precUnary);
  Prefix(ttNot, precUnary);
  Prefix(ttExclamation, precUnary); // bitwise not !

  //PostFix(ttQuestion, precPostfix);
  //PostFix(ttBang,     precPostfix);

  // Register the infix operator parselets.
  // Equality: =, <>
  RegisterParselet(ttEQ,  TEqualityOperatorParselet.Create);
  RegisterParselet(ttNEQ,  TEqualityOperatorParselet.Create);
  //InfixLeft(ttEQ,  precEquality);
  //InfixLeft(ttNEQ, precEquality);

  // Comparison: >, >=, <, <=
  RegisterParselet(ttGT,  TComparisonOperatorParselet.Create);
  RegisterParselet(ttGE,  TComparisonOperatorParselet.Create);
  RegisterParselet(ttLT,  TComparisonOperatorParselet.Create);
  RegisterParselet(ttLE,  TComparisonOperatorParselet.Create);
  //InfixLeft(ttGT, precComparison);
  //InfixLeft(ttGE, precComparison);
  //InfixLeft(ttLT, precComparison);
  //InfixLeft(ttLE, precComparison);
  //InfixLeft(ttIn, precComparison);

  // Sum: +, -, or
  InfixLeft(ttPlus,  precTerm);
  InfixLeft(ttMinus, precTerm);
  //InfixLeft(ttOr,    precOr);
  RegisterParselet(ttOr, TOrParselet.Create);

  // Product: *, /, %, and, <<, >>, is
  InfixLeft(ttStar,    precFactor);
  InfixLeft(ttSlash,   precFactor);
  InfixLeft(ttPercent, precFactor);
  InfixLeft(ttShl,     precFactor);
  InfixLeft(ttShr,     precFactor);
  InfixLeft(ttIn,      precFactor);
  InfixLeft(ttNotIn,   precFactor);
  InfixLeft(ttIs,      precFactor);
  //InfixLeft(ttAnd,     precAnd);
  RegisterParselet(ttAnd, TAndParselet.Create);
  InfixLeft(tt2Questions, precFactor);
  //RegisterParselet(tt2Questions, TNilCoescalatingParselet.Create);

  // bitwise operators &, |, ~ for and, or, xor
  InfixLeft(ttAmpersand, precFactor);
  InfixLeft(ttVertBar, precFactor);
  InfixLeft(ttTilde, precFactor);

  //InfixLeft(ttColons,  precFactor);
  InfixLeft(ttLeftArrow, precPrecision);

  InfixRight(ttCaret,  precExponent); // ^
end;

//Registers a postfix unary operator parselet for the given token and precedence.
//procedure TExprParser.Postfix(const Typ: TTokenTyp; const Precedence: Integer);
//begin
//  RegisterParselet(Typ, TPostfixOperatorParselet.Create(Precedence));
//end;

//Registers a prefix unary operator parselet for the given token and precedence.
procedure TExprParser.Prefix(const Typ: TTokenTyp; const Precedence: Integer);
begin
  RegisterParselet(Typ, TPrefixOperatorParselet.Create(Precedence));
end;

//Registers a left-associative binary operator parselet for the given token and precedence.
procedure TExprParser.InfixLeft(const Typ: TTokenTyp; const Precedence: Integer);
begin
  RegisterParselet(Typ, TBinaryOperatorParselet.Create(Precedence, False));
end;

//Registers a right-associative binary operator parselet for the given token and precedence.
procedure TExprParser.InfixRight(const Typ: TTokenTyp; const Precedence: Integer);
begin
  RegisterParselet(Typ, TBinaryOperatorParselet.Create(Precedence, True));
end;


end.


