unit uBaseParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uToken, uAST;

type

  // The base parser defines basic parser functionality. The language syntax is
  // parsed in the main TParser class in unit uParser.
  TBaseParser = class
  private
    protected
      // the list of tokens received from the Lexer
      Tokens: TTokens;
      // the index keeping track of the current token being parsed
      Index: integer;
    public
      // the current token being parsed
      function Current: TToken;
      // the next token, not yet being parsed
      function Peek: TToken;
      // diverse error routines
      procedure Error(const Location: TLocation; const Msg: string);
      procedure Error(const Msg: string);
      procedure Error(const Msg: string; SynchSet: TTokenTypSet);
      procedure ErrorFmt(const Fmt: String; const Args: array of const);
      // the expect method expects a certain token in a given syntax
      procedure Expect(const TokenTyp: TTokenTyp; const Message: String);
      procedure Expect(const TokenTyp: TTokenTyp);
      procedure Expect(const TokenTyp: TTokenTyp; SynchSet: TTokenTypSet);
      // advance to the next token
      procedure Next;
      // consume and advance to the next token, returning the previous token
      function Consume: TToken;
      // match and consume the current token
      function Match(const Expected: TTokenTyp): Boolean;
      function Match(const Expected: TTokenTypSet): Boolean;
      // handle newLines as token
      function MatchLine: Boolean;
      procedure IgnoreNewlines;
      procedure ExpectLine;
      procedure ExpectLine(const Msg: String);
      procedure AllowLineBeforeDot;
      // after an error synchronize to statement or declaration start token
      procedure Synchronize(Types: TTokenTypSet);
      // determine if it is the last token in the list
      function isLastToken: Boolean;
      // parse an identifier
      function ParseIdent: TIdent;
      // make an identifier at the current location
      function MakeID(Name: String): TIdent;
      // parse an operator for operator overloading
      function ParseOperator: TIdent;
    public
      constructor Create(ATokens: TTokens);
  end;


const
  ErrSyntax = 'Syntax error, "%s" expected.';

implementation
uses uError;

// return the current token, at Index in the list of tokens.
// if any error occurs then return the EOF End-of-file token
function TBaseParser.Current: TToken;
begin
  try
    Result := Tokens[Index];
  except
    Result := TToken.Create(ttEOF, 'EOF', TLocation.Create(0,0));
  end;
end;

// peek at the next token
function TBaseParser.Peek: TToken;
begin
  if not isLastToken then
    Result := Tokens[Index+1]
  else
    Result := TToken.Create(ttEOF, 'EOF', TLocation.Create(0,0));
end;

// add an error with the location and message to the error list
// then synchronize to synch point
procedure TBaseParser.Error(const Location: TLocation; const Msg: string);
begin
  AddError(Location, Msg);
  Synchronize(SynchronizeSet); // the set of all statement and declaration start tokens
  //Raise EParseError.Create(Msg);
end;

// add an error at the current location
procedure TBaseParser.Error(const Msg: string);
begin
  Error(Current.Location, Msg);
end;

procedure TBaseParser.Error(const Msg: string; SynchSet: TTokenTypSet);
begin
  Error(Current.Location, Msg);
  Synchronize(SynchSet);
end;

// add a formatted error with arguments at the current location
procedure TBaseParser.ErrorFmt(const Fmt: String; const Args: array of const);
begin
  Error(Current.Location, Format(Fmt, Args));
end;

// if the tokentype is as expected then continue to the next token
// otherwise add an error to the list and try to synchronize
procedure TBaseParser.Expect(const TokenTyp: TTokenTyp; const Message: String);
begin
  if Current.Typ = TokenTyp then
    Next
  else
    begin
      Error(Current.Location, Message);
      Synchronize(SynchronizeSet);
    end;
end;

// expect a certain tokentype. if not found then add a syntax error
procedure TBaseParser.Expect(const TokenTyp: TTokenTyp);
begin
  Expect(TokenTyp, Format(ErrSyntax, [TokenTyp.toString]));
end;

// expect a tokentype. if not found then synchronize on a given synch set
procedure TBaseParser.Expect(const TokenTyp: TTokenTyp; SynchSet: TTokenTypSet);
begin
  try
    Expect(TokenTyp);
  except
    Synchronize(SynchSet);
  end;
end;

// increase the index of the current token
// if the token is an error token received from the lexer then report it
// to the error list and increase index again, skipping the error token.
procedure TBaseParser.Next;
begin
  while True do
    begin
      Inc(Index);

      if Current.Typ <> ttError then
        Break;
      AddError(Current.Location, Format('Error at %s.', [Current.Lexeme]));
      Synchronize(SynchronizeSet);
    end;
end;

// return the current token and move on to the next index
function TBaseParser.Consume: TToken;
begin
  Result := Current;
  Next;
end;

// try to match the current token with the expected token
// if true then move to the next token
function TBaseParser.Match(const Expected: TTokenTyp): Boolean;
begin
  Result := Current.Typ = Expected;
  if Result then
    Next;
end;

// try to match the current token with a token in the expected token set
// if true then move to the next token
function TBaseParser.Match(const Expected: TTokenTypSet): Boolean;
begin
  Result := Current.Typ in Expected;
  if Result then
    Next;
end;

// Matches one or more newlines. Returns true if at least one was found.
function TBaseParser.MatchLine: Boolean;
begin
  if not Match(ttLine) then Exit(False);
  while Current.Typ = ttLine do
    Next;
  Result := True;
end;

// Discards any newlines starting at the current token.
procedure TBaseParser.IgnoreNewlines;
begin
  MatchLine;
end;

// Consumes the current token. Emits an error if it is not a newline. Then
// discards any duplicate newlines following it.
procedure TBaseParser.ExpectLine(const Msg: String);
begin
  Expect(ttLine, Msg);
  IgnoreNewlines;
end;

// expects a newLine with the standard syntax error message
procedure TBaseParser.ExpectLine;
begin
  ExpectLine(Format(ErrSyntax, [ttLine.toString]));
end;


// skip line token if next token is a dot
procedure TBaseParser.AllowLineBeforeDot;
begin
  if (Current.Typ = ttLine) and (Peek.Typ in [ttDot, ttQuestionDot]) then
    Next;
end;

// skip tokens until we can synchronize, then continue parsing from there
procedure TBaseParser.Synchronize(Types: TTokenTypSet);
begin
  while not (Current.Typ in Types) do
    Next;
end;

// the last token is found if Index is equal to Count -1
function TBaseParser.isLastToken: Boolean;
begin
  Result := Index = Tokens.Count-1;
end;

// create the parser
constructor TBaseParser.Create(ATokens: TTokens);
begin
  Tokens := ATokens;
  Index := 0;
end;

// this method expects the current token to be an identifier
// if found then return the identifier as a TIdent type
function TBaseParser.ParseIdent: TIdent;
var
  Token: TToken;
begin
  Token := Current;
  Expect(ttIdentifier);
  Result := TIdent.Create(Token);
end;

// create an identifier with a given string name, and use the current location
function TBaseParser.MakeID(Name: String): TIdent;
begin
  with Current do
    Result := TIdent.Create(TToken.Create(
      ttIdentifier, Name, Location));
end;

// only operators (token types) part of the set AllOperators can be overloaded
// if found then create an identifier from the operator token and return it
// otherwise report an error
function TBaseParser.ParseOperator: TIdent;
var
  Token: TToken;
begin
  Token := Consume;
  if Token.Typ in AllOperators then
    Result := TIdent.Create(Token)
  else
    Error(Token.Location, 'Expected operator.');
end;

end.


