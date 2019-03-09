unit uLexer;

{ This unit contains the Lexer that produces all tokens form input to the parser.

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
  Classes, SysUtils, uReader, uToken, uError;

const
  Space  = #32;
  BackSpace = #8;
  Tab    = #9;
  Quote1 = #39; // '
  Quote2 = #34; // "

  WhiteSpace   = [Tab, LineEnding, Space];  // LineEnding is predefined in FPC
  Underscore   = ['_'];
  LoCaseLetter = ['a'..'z'];
  UpCaseLetter = ['A'..'Z'];
  Letters      = UpCaseLetter + LoCaseLetter;
  AlphaChars   = UpCaseLetter + LoCaseLetter + Underscore;
  NumberChars  = ['0'..'9'];
  SpecialChar  = '#';
  IdentChars   = NumberChars + AlphaChars;

type

  TLexer = class
    private
      FLook : char;              // next input character (still unprocessed)
      FLine, FCol : integer;     // line and column number of the input character
      FReader: TReader;          // contains the text to scan
      FTokens: TTokens;          // list of mined tokens
      EndOfFile: Boolean;
      NumParens: Integer;
      procedure Error(Token: TToken; const Msg: String);
      function getChar: Char;
      procedure doKeywordOrIdentifier(const Line, Col: Integer);
      procedure doNumber(const Line, Col: Integer);
      procedure doString(const Line, Col: Integer);
      procedure doChar(const Line, Col: Integer);
      procedure ScanToken(const Line, Col: Integer);
      Procedure ScanTokens;
      Procedure SingleLineComment;
      Procedure MultiLineComment;
    public
      property Tokens: TTokens read FTokens;
      constructor Create(Reader: TReader);
      destructor Destroy; override;
  end;

implementation

constructor TLexer.Create(Reader: TReader);
begin
  FReader := Reader;
  FTokens := TTokens.Create();
  FLine := 1;
  FCol := 0;
  EndOfFile := false;
  NumParens := 0;
  FLook := getChar;   // get first character
  ScanTokens;  // scan all tokens
end;

destructor TLexer.Destroy;
begin
  FTokens.Free;
  inherited Destroy;
end;

procedure TLexer.Error(Token: TToken; const Msg: String);
begin
  Errors.Append(Token, Msg);
end;

// reads the next character from the input stream
function TLexer.getChar: Char;
begin
  Result := FReader.NextChar;
  Inc(FCol);
  if Result = LineEnding then begin
    Inc(FLine);
    FCol := 0;
  end;
end;

procedure TLexer.ScanTokens;
begin
  while not EndOfFile do begin
    while FLook in WhiteSpace do
      FLook := getChar;   // skip white space
    ScanToken(FLine, FCol);
  end;
  Tokens.Add(TToken.Create(ttEOF, 'End of file', Nil, FLine, FCol, FReader.FileIndex));
end;

procedure TLexer.ScanToken(const Line, Col: Integer);

  procedure AddToken(const Typ: TTokenTyp; const Lexeme: String);
  var Token: TToken;
  begin
    Token := TToken.Create(Typ, Lexeme, Null, Line, Col, FReader.FileIndex);
    Tokens.Add(Token);
    FLook := getChar;
  end;

begin
  case FLook of
    '+' : if FReader.PeekChar = '=' then begin
            FLook := getChar;
            AddToken(ttPlusIs, '+=');
          end else AddToken(ttPlus, '+');
    '-' : if FReader.PeekChar = '=' then begin
            FLook := getChar;
            AddToken(ttMinIs, '-=');
          end else AddToken(ttMin, '-');
    '/' : case FReader.PeekChar of
            '/': begin FLook := getChar; SingleLineComment end;
            '*': begin FLook := getChar; MultiLineComment end;
            '=': begin FLook := getChar; AddToken(ttDivIs, '/=') end;
            else AddToken(ttDiv, '/');
          end;
    '*' : if FReader.PeekChar = '=' then begin
            FLook := getChar;
            AddToken(ttMulIs, '*=');
          end else AddToken(ttMul, '*');
    '%' : if FReader.PeekChar = '=' then begin
            FLook := getChar;
            AddToken(ttRemIs, '%=');
          end else AddToken(ttRem, '%');
    ':' : case FReader.PeekChar of
            '=' : begin FLook := getChar; AddToken(ttAssign, ':='); end;
            ':' : begin FLook := getChar; AddToken(ttColons, '::'); end;
            else AddToken(ttColon, ':');
          end;
    '&' : AddToken(ttAnd, '&');
    '|' : AddToken(ttOr, '|');
    '~' : AddToken(ttXor, '~');
    '!' : AddToken(ttNot, '!');
    '^' : AddToken(ttPow, '^');
    '(' : begin
            if NumParens>0 then NumParens +=1;
            AddToken(ttOpenParen, '(');
          end;
    ')' : if NumParens>0 then begin
            NumParens -= 1;
            if NumParens = 0 then doString(Line, Col)
            else AddToken(ttCloseParen, ')');
          end
          else AddToken(ttCloseParen, ')');
    '{' : AddToken(ttOpenBrace, '{');
    '}' : AddToken(ttCloseBrace, '}');
    '[' : AddToken(ttOpenBrack, '[');
    ']' : AddToken(ttCloseBrack, ']');
    ',' : AddToken(ttComma, ',');
    '.' : if FReader.PeekChar = '.' then begin
            FLook := getChar;
            AddToken(ttDotDot, '..');
          end else AddToken(ttDot, '.');
    '=' : if FReader.PeekChar = '>' then begin
            FLook := getChar;
            AddToken(ttArrow, '=>');
          end else AddToken(ttEQ, '=');
    '<' : case FReader.PeekChar of
            '<' : begin FLook := getChar; AddToken(ttShl, '<<'); end;
            '=' : begin FLook := getChar; AddToken(ttLE, '<='); end;
            '>' : begin FLook := getChar; AddToken(ttNEQ, '<>'); end;
            else AddToken(ttLT, '<');
          end;
    '>' : case FReader.PeekChar of
            '>' : begin FLook := getChar; AddToken(ttShr, '>>'); end;
            '=' : begin FLook := getChar; AddToken(ttGE, '>='); end;
            '<' : begin
                    FLook := getChar;
                    if FReader.PeekChar = '=' then begin
                      FLook := getChar; AddToken(ttConcatIs, '><=');
                    end
                    else AddToken(ttConcat, '><');
                  end
            else AddToken(ttGT, '>');
          end;
    '?' : AddToken(ttQuestion, '?');
    '0'..'9': doNumber(Line, Col);
    '_', 'A'..'Z', 'a'..'z': doKeywordOrIdentifier(Line, Col);
    Quote1: doString(Line, Col);
    Quote2: doChar(Line, Col);
    FileEnding: EndOfFile := true;
    else
      EndOfFile := true;
  end;
end;

// process an identifier
procedure TLexer.doKeywordOrIdentifier(const Line, Col: Integer);
var
  Index: integer = -1;
  Lexeme: String = '';
  TokenTyp: TTokenTyp = ttIdentifier;
  Token: TToken;
begin
  Lexeme := FLook;
  FLook := getChar;
  while FLook in IdentChars do begin
    Lexeme += FLook;
    FLook := getChar;
  end;

  //Match the keyword and return its type, otherwise it's an identifier
  if Keywords.Contains(Lexeme, Index) then
    TokenTyp := Keywords.At(Index);

  Token := TToken.Create(TokenTyp, Lexeme, Null, Line, Col, FReader.FileIndex);
  Tokens.Add(Token);
end;

// process a number literal int or float.
procedure TLexer.doNumber(const Line, Col: Integer);
var
  Lexeme: String = '';
  Value: Double;
  Token: TToken;
  FoundDotDot: Boolean = False;
begin
  Lexeme := FLook;
  // first read integer part of number
  FLook := getChar;
  while FLook in NumberChars do begin
    Lexeme += FLook;
    FLook := getChar;
  end;

  // did we encounter a '..'
  FoundDotDot := (FLook = '.') and (FReader.PeekChar = '.');

  // read the fraction if any
  if (FLook = '.') and not FoundDotDot then begin
    Lexeme += FLook;
    FLook := getChar;
    while FLook in NumberChars do begin
      Lexeme += FLook;
      FLook := getChar;
    end;
  end;

  // is there an exponent?
  if (Upcase(FLook) = 'E') and not FoundDotDot then begin
    Lexeme += FLook;
    FLook := getChar;
    if FLook in ['+', '-'] then begin
      Lexeme += FLook;
      FLook := getChar;
    end;
    while FLook in NumberChars do begin
      Lexeme += FLook;
      FLook := getChar;
    end;
  end;

  Value := Lexeme.ToDouble;
  Token := TToken.Create(ttNumber, Lexeme, Value, Line, Col, FReader.FileIndex);
  Tokens.Add(Token);
end;

{ Process a string 'string' literal }
procedure TLexer.doString(const Line, Col: Integer);
var
  Lexeme: string = '';
  Value: String;
  Token: TToken;
  Typ: TTokenTyp;
begin
  Typ := ttString;

  while True do begin
    FLook := getChar;
    case FLook of
      Quote1:
        if FReader.PeekChar = Quote1 then
          FLook := getChar
        else begin
          FLook := getChar;   // consume quote '
          Break;
        end;
      LineEnding:
        begin
          Token := TToken.Create(ttNone, '', Null, Line, Col, FReader.FileIndex);
          if NumParens > 0 then
            Error(Token, 'Lexer error: Expected closing paren ")" in expression.')
          else
            Error(Token, 'Lexer error: String exceeds line.');
          Break;
        end;
      '\':
        if FReader.PeekChar = '(' then begin
          FLook := getChar;
          Typ := ttInterpolated;
          NumParens := 1;
          FLook := getChar;
          Break;
        end;
    end;
    Lexeme += FLook;
  end;

  Lexeme := StringReplace(Lexeme, '\n', LineEnding, [rfReplaceAll]);
  Lexeme := StringReplace(Lexeme, '\t', Tab, [rfReplaceAll]);

  Value := Lexeme;
  Lexeme := '''' + Lexeme + '''';   // including the quotes
  Token := TToken.Create(Typ, Lexeme, Value, Line, Col, FReader.FileIndex);
  Tokens.Add(Token);
end;

procedure TLexer.doChar(const Line, Col: Integer);
var
  Value: Char;
  Token: TToken;
begin
  FLook := getChar;
  Value := FLook;
  Token := TToken.Create(ttChar, '"'+FLook, Value, Line, Col, FReader.FileIndex);
  Tokens.Add(Token);
  FLook := getChar;
end;

// multi line comment starts with '/*' and ends with '*/'
procedure TLexer.MultiLineComment;
var
  Token: TToken;
begin
  Repeat
    Repeat
      FLook := getChar;
    Until (FLook = '*') or (FLook = FileEnding);
    FLook := getChar;
  Until (FLook = '/') or (FLook = FileEnding);
  if FLook = FileEnding then begin
    Token := TToken.Create(ttNone, '', Null, FLine, FCol, FReader.FileIndex);
    Error(Token, 'Lexer error: Comment exceeds file.');
  end
  else
    FLook := getChar;
end;

procedure TLexer.SingleLineComment;
begin
  Repeat
    FLook := getChar;
  until FLook = LineEnding;
  FLook := getChar;
end;

end.


