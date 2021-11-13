unit uLexer;

{$mode objfpc}{$H+}
{$ModeSwitch typehelpers}
{$ModeSwitch arrayoperators}

interface

uses
  Classes, SysUtils, uToken, uError, uReader;

type

  TLexer = class
    private
      Look: char;                // next input character (still unprocessed)
      Lexeme: String;            // current lexeme
      Line, Col: Integer;        // line and column of the input character
      Reader: TReader;           // contains the text to scan
      OpenParens: Integer;       // track if inside an interpolated string
      FTokens: TTokens;
      function getChar: Char;
      function peekChar: Char;
      function peek(const OffSet: Integer): Char;
      function CurrentLocation: TLocation;
      procedure SkipWhiteSpace;
      procedure SingleLineComment;
      procedure MultiLineComment;
      function ErrorToken(const Message: String): TToken;
      function Identifier: TToken;
      function Number: TToken;
      function HexaDecimal: TToken;
      function BinaryNumber: TToken;
      function OctalNumber: TToken;
      function Characters: TToken;
      function Character: TToken;
      function SpecialCharacter: TToken;
      function ScanToken: TToken;
      procedure ScanTokens;
      function UnicodeNumber: TToken;
      procedure InsertUseFiles;
    public
      property Tokens: TTokens read FTokens;
      constructor Create(AReader: TReader; const EmptyTokens: Boolean=True);
      destructor Destroy; override;
  end;

  TCharHelper = type helper for Char
    function isWhiteSpace: Boolean;
    function isUnderscore: Boolean;
    function isLowCaseLetter: Boolean;
    function isUpCaseLetter: Boolean;
    function isAlpha: Boolean;
    function isDigit: Boolean;
    function isAlphaNum: Boolean;
    function isHexaDecimal: Boolean;
    function isBinaryDecimal: Boolean;
    function isOctoDecimal: Boolean;
    function isSingleQuote: Boolean;
    function isDoubleQuote: Boolean;
    function isDot: Boolean;
    function isFileEnding: Boolean;
    function isLineEnding: Boolean;
  end;

const
  Null = #0;
  Bell = #7;
  BackSpace = #8;
  Tab = #9;
  VTab = #11;
  FormFeed = #12;
  CR = #13;
  Escape = #27;
  Space = #32;
  SingleQuote = #39; // single quote
  DoubleQuote = #34; // double quote

implementation
uses strutils;

{ TLexer }

constructor TLexer.Create(AReader: TReader; const EmptyTokens: Boolean);
begin
  Line := 1;
  Col := 0;

  OpenParens := 0;
  Reader := AReader;

  FTokens := TTokens.Create(EmptyTokens);

  Look := getChar;   // get first character
  ScanTokens;
end;

destructor TLexer.Destroy;
begin
  FTokens.Free;
  inherited Destroy;
end;

// reads the next character from the input stream
function TLexer.getChar: Char;
begin
  Result := Reader.NextChar;
  Inc(Col);
  if Result = LineEnding then
    begin
      Inc(Line);
      Col := 0;
    end;
end;

function TLexer.peekChar: Char; inline;
begin
  Result := Reader.PeekChar;
end;

function TLexer.peek(const OffSet: Integer): Char;
begin
  Result := Reader.PeekChar(OffSet);
end;

function TLexer.CurrentLocation: TLocation;
begin
  Result := TLocation.Create(Line, Col, Reader.FileIndex);
end;

procedure TLexer.SkipWhiteSpace;
begin
  while True do
    begin
      case Look of
        // Line endings are NOT whitespace, they are tokens
        Tab, Space:
          Look := getChar;
        '/':
          case PeekChar of
            '/': SingleLineComment;
            '*': MultiLineComment;
            else
              Break;
          end;
        else
          Break;
      end;
    end;
end;

// single line comment starts with '//'
procedure TLexer.SingleLineComment;
begin
  // skip characters until the end of the current line
  Look := getChar;
  while (Look <> LineEnding) do //and (Look <> FileEnding) do
    Look := getChar;
  //Look := getChar;
end;

// nested comments are allowed
procedure TLexer.MultiLineComment;
var
  Nesting: Integer = 1;
begin
  Look := getChar;
  while Nesting > 0 do
    begin
      case Look of
        FileEnding:
          begin
            Tokens.Add(ErrorToken('comment block not terminated'));
            Exit;
          end;
        '/':
          if PeekChar = '*' then
            begin
              Look := getChar;
              Inc(Nesting);
            end;
        '*':
          if PeekChar = '/' then
            begin
              Look := getChar;
              Dec(Nesting);
            end;
      end;
      Look := getChar;
    end;
end;

// create an error token, with the message as lexeme
function TLexer.ErrorToken(const Message: String): TToken;
begin
  Result := TToken.Create(ttError, Message, CurrentLocation);
  Look := getChar;
end;

// process an identifier or a keyword
function TLexer.Identifier: TToken;
var
  TokenTyp: TTokenTyp = ttIdentifier;  // default to identifier
  Location: TLocation;            // Position of lexeme location
  Index: Integer;
begin
  Location := CurrentLocation;
  Lexeme := Look;

  Look := getChar;
  while Look.isAlphaNum do
    begin
      Lexeme += Look;
      Look := getChar;
    end;

  //Match the keyword and return its type, otherwise it's an identifier
  if Keywords.Find(Lexeme, Index) then
    begin
      TokenTyp := Keywords.Data[Index];
      if TokenTyp = ttNot then
      begin
        //test for "not in"
        if (Peek(0) = 'i') and (Peek(1) = 'n') and (Peek(2) = ' ') then
          begin
            TokenTyp := ttNotIn;
            Look := getChar; Look := getChar; Look := getChar; // skip "in "
            Lexeme := 'not in';
          end;
      end;
    end;

  Result := TToken.Create(TokenTyp, Lexeme, Location);
end;

// process a number literal int or float.
function TLexer.Number: TToken;
var
  FoundDotDot: Boolean = False;
  Location: TLocation;            // Position of lexeme location
begin
  Location := CurrentLocation;
  Lexeme := Look;

  // first read integer part of number
  Look := getChar;

  while Look.isDigit do
    begin
      Lexeme += Look;
      Look := getChar;
    end;

  // did we encounter a '..'
  FoundDotDot := Look.isDot and PeekChar.isDot;

  // read the fraction if any
  if Look.isDot and not FoundDotDot then
    begin
      Lexeme += Look;
      Look := getChar;
      while Look.isDigit do
        begin
          Lexeme += Look;
          Look := getChar;
        end;
    end;

  // is there an exponent?
  if (Upcase(Look) = 'E') and not FoundDotDot then
    begin
      Lexeme += Look;
      Look := getChar;
      if Look in ['+', '-'] then
        begin
          Lexeme += Look;
          Look := getChar;
        end;
      while Look.isDigit do
        begin
          Lexeme += Look;
          Look := getChar;
        end;
    end;

  Result := TToken.Create(ttNumber, Lexeme, Location);
end;

// Process a hexadecimal number
function TLexer.HexaDecimal: TToken;
var
  Location: TLocation;            // Position of lexeme location
begin
  Location := CurrentLocation;
  getChar; // 0x
  Lexeme := '$';  // a Pascal hex number starts with '$'
  Look := getChar;
  if Look.isHexaDecimal then
    while Look.isHexaDecimal do
      begin
        Lexeme += Look;
        Look := getChar;
      end
  else
    Exit(ErrorToken('non hexadecimal character'));

  Result := TToken.Create(ttNumber, Lexeme, Location);
end;

// Process a binary number
function TLexer.BinaryNumber: TToken;
var
  Location: TLocation;            // Position of lexeme location
begin
  Location := CurrentLocation;
  getChar; // 0b
  Lexeme := '%';   // a Pascal binary number starts with '%'
  Look := getChar;
  if Look.isBinaryDecimal then
    while Look.isBinaryDecimal do
      begin
        Lexeme += Look;
        Look := getChar;
      end
  else
    Exit(ErrorToken('non binary decimal character'));

  Result := TToken.Create(ttNumber, Lexeme, Location);
end;

// Process a octal number
function TLexer.OctalNumber: TToken;
var
  Location: TLocation;            // Position of lexeme location
begin
  Location := CurrentLocation;
  getChar; // 0o
  Lexeme := '&';     // a Pascal octal number starts with '&'
  Look := getChar;
  if Look.isOctoDecimal then
    while Look.isOctoDecimal do
      begin
        Lexeme += Look;
        Look := getChar;
      end
  else
    Exit(ErrorToken('non octodecimal character'));

  Result := TToken.Create(ttNumber, Lexeme, Location);
end;

// Returns the number of bytes needed to encode [value] in UTF-8.
//
// Returns 0 if [value] is too large to encode.
function Utf8EncodeNumBytes(const Value: LongWord): Integer;
begin
  if Value <= $7F then Result := 1
  else if Value <= $7FF then Result := 2
  else if Value <= $FFFF then Result := 3
  else if Value <= $10FFFF then Result := 4
  else Result := 0;
end;

// Encodes value as a series of bytes in [bytes], which is assumed to be large
// enough to hold the encoded result.
//
// Returns the number of written bytes.
function Utf8Encode(Value: LongWord; Bytes: PByte): Integer; // PByte = pointer to byte
begin
  if Value <= $7F then
    begin
      // Single byte (i.e. fits in ASCII).
      Bytes^ := Value and $7F;
      Result := 1;
    end
  else if Value <= $7FF then
    begin
      // Two byte sequence: 110xxxxx 10xxxxxx.
      Bytes^ := $C0 or ((Value and $7C0) shr 6);
      Inc(Bytes);
      Bytes^ := $80 or (Value and $3F);
      Result := 2;
    end
  else if Value <= $FFFF then
    begin
      // Three byte sequence: 1110xxxx 10xxxxxx 10xxxxxx.
      Bytes^ := $E0 or ((Value and $F000) shr 12);
      Inc(Bytes);
      Bytes^ := $80 or ((Value and $FC0) shr 6);
      Inc(Bytes);
      Bytes^ := $80 or (Value and $3F);
      Result := 3;
    end
  else if Value <= $10FFFF then
    begin
      // Four byte sequence: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx.
      Bytes^ := $F0 or ((Value and $1C0000) shr 18);
      Inc(Bytes);
      Bytes^ := $80 or ((Value and $3F000) shr 12);
      Inc(Bytes);
      Bytes^ := $80 or ((Value and $FC0) shr 6);
      Inc(Bytes);
      Bytes^ := $80 or (Value and $3F);
      Result := 4;
    end
  else
    Result := 0;
end;

function IntToUTF8(const Value: LongWord): String;
var
  NumBytes, NumWritten: Integer;
  Bytes: PByte;
begin
  NumBytes := Utf8EncodeNumBytes(Value);
  if NumBytes <> 0 then
    begin
      Bytes := PByte(AllocMem(NumBytes));
      NumWritten := Utf8Encode(Value, Bytes);
      SetString(Result, PChar(Bytes), NumWritten);
    end
  else
    Result := '';
end;

function TLexer.UnicodeNumber: TToken;
var
  Location: TLocation;            // Position of lexeme location
  Chars: String;                  // characters of the hex byte
  Bytes: TBytes;
  CodePoint: LongWord;
  i: Byte;
begin
  Location := CurrentLocation;
  Look := getChar; // 0u
  Look := getChar;
  Lexeme := '';
  if Look = '+' then
    begin
      Look := getChar;
      // read hex numbers
      Chars := '$';
      if Look.isHexaDecimal then
        while Look.isHexaDecimal do
          begin
            Chars += Look;
            Look := getChar;
          end
      else
        Exit(ErrorToken('non hexadecimal character'));
      CodePoint := StrToInt(Chars);
      Lexeme := IntToUTF8(CodePoint);
    end
  else
    begin
      Bytes := [];   // initialize byte array, start with length zero
      if Look.isHexaDecimal then
        while Look.isHexaDecimal do
          begin
            Chars := '';
            i := 0;
            while Look.isHexaDecimal and (i<2) do
              begin
                Chars += Look;
                Look := getChar;
                i+=1;
              end;
            // Add '$' to make it a hexadecimal, and return integer Value
            Bytes += [Byte(StrToInt('$'+Chars))];
          end
      else
        Exit(ErrorToken('non hexadecimal character'));

      if Length(Bytes) > 4 then
        Exit(ErrorToken('exceeded maximum length of character (max 4 bytes)'));

      if Length(Bytes) = 1 then
        if (Bytes[0] < 32) or (Bytes[0] > 126) then
          Lexeme := '?'
        else
          Lexeme := Chr(Bytes[0])
      else
        Lexeme := PChar(@Bytes[0]);
    end;

  Result := TToken.Create(ttCharacter, Lexeme, Location);
end;

procedure TLexer.InsertUseFiles;
begin
  // arrays
  Tokens.Add(TToken.Create(ttUse, 'use', TLocation.Create(0, 0)));
  Tokens.Add(TToken.Create(ttIdentifier, 'arrays', TLocation.Create(0, 0)));
  Tokens.Add(TToken.Create(ttLine, '', TLocation.Create(0, 0)));
  // ranges
  Tokens.Add(TToken.Create(ttUse, 'use', TLocation.Create(0, 0)));
  Tokens.Add(TToken.Create(ttIdentifier, 'ranges', TLocation.Create(0, 0)));
  Tokens.Add(TToken.Create(ttLine, '', TLocation.Create(0, 0)));
  // sets
  Tokens.Add(TToken.Create(ttUse, 'use', TLocation.Create(0, 0)));
  Tokens.Add(TToken.Create(ttIdentifier, 'sets', TLocation.Create(0, 0)));
  Tokens.Add(TToken.Create(ttLine, '', TLocation.Create(0, 0)));
end;


// Process a string "string of chars" literal
function TLexer.Characters: TToken;
var
  Typ: TTokenTyp = ttString;
  Location: TLocation;            // Position of lexeme location
begin
  Location := CurrentLocation;
  Lexeme := '';

  while True do
    begin
      Look := getChar;

      if Look.isSingleQuote then
        begin
          if peekChar.isSingleQuote then
            Look := getChar
          else
            begin
              Look := getChar; // consume quote '
              if OpenParens > 0 then
                Exit(ErrorToken('missing closing paren ")"'));
              Break;
            end;
        end;

      if Look = LineEnding then
        if OpenParens > 0 then
          Exit(ErrorToken('missing closing paren ")"'))
        else
          Exit(ErrorToken('unterminated single quoted string'));

      if Look = '$' then
        begin
          if peekChar = '(' then
            begin
              Look := getChar;
              Typ := ttInterpolated;
              OpenParens := 1;
              Look := getChar;
              Break;
            end;
        end;

      Lexeme += Look;
    end;

  //Lexeme := StringsReplace(Lexeme, ['\t', '\n'], [Tab, LineEnding], [rfReplaceAll]);
  Lexeme := StringsReplace(Lexeme,
    ['\0', '\a', '\b', '\t', '\v', '\n', '\r', '\f', '\e'],
    [Null, Bell, BackSpace, Tab, VTab, LineEnding, CR, FormFeed, Escape],
    [rfReplaceAll]);

  Result := TToken.Create(Typ, Lexeme, Location);
end;

// process a character
function TLexer.Character: TToken;
var
  Location: TLocation;            // Position of lexeme location
begin
  // a character can be up to four bytes long, e.g. an emoji
  Location := CurrentLocation;
  Lexeme := '';

  while True do
    begin
      Look := getChar;

      if Look.isDoubleQuote then
        begin
          Look := getChar; // consume quote "
          Break;
        end;

      if Look = LineEnding then
        Exit(ErrorToken('unterminated double quoted string'));

      Lexeme += Look;
    end;

  if Length(Lexeme) > 4 then
    Exit(ErrorToken('exceeded maximum length of character (max 4)'));

  Result := TToken.Create(ttCharacter, Lexeme, Location);
end;

// process a special deciamal character preceded by #
function TLexer.SpecialCharacter: TToken;
var
  Location: TLocation;            // Position of lexeme location
  Value, Code: Integer;
  Chars: String;
begin
  // a special character sequence can be up to four bytes long, e.g. an emoji
  Location := CurrentLocation;
  Lexeme := '';

  while Look = '#' do
    begin
      Look := getChar;
      Chars := '';
      if Look.isDigit then
        begin
          while Look.isDigit do
            begin
              Chars += Look;
              Look := getChar;
            end;
          Val(Chars, Value, Code);
          if (Value < 0) or (Value > 255) then
            Exit(ErrorToken('character value exceeding size limit (0..255)'));
          Lexeme += Chr(Byte(Value));
        end
      else
        Exit(ErrorToken(Format('unexpected character "%s"', [Look])));
    end;

  if Length(Lexeme) > 4 then
    Exit(ErrorToken('exceeded maximum length of character (max 4)'));

  Result := TToken.Create(ttCharacter, Lexeme, Location);
end;

// get the next token
function TLexer.ScanToken: TToken;
  // make a token from a token type
  function Token(const Typ: TTokenTyp): TToken;
  begin
    Result := TToken.Create(Typ, Typ.toString, CurrentLocation);
    Look := getChar;
  end;

  // simulates the ifthen() function
  function Iff(Check: Boolean; const IfTrue, IfFalse: TTokenTyp): TTokenTyp;
  begin
    Result := specialize IfThen<TTokenTyp>(Check, IfTrue, IfFalse);
  end;

  // return true if expected char equals next char
  function EQ(const Expected: Char): Boolean;
  begin
    Result := peekChar = Expected;
    if Result then
      begin
        Look := getChar;
        Lexeme += Look;
      end;
  end;

var
  Typ: TTokenTyp;
begin
  SkipWhitespace;
  Lexeme := Look;

  case Look of
    FileEnding: Exit(Token(ttEOF));
    LineEnding: Exit(Token(ttLine));
    '!': Exit(Token(ttExclamation));
    DoubleQuote: Exit(Character); // single character
    '#': Exit(SpecialCharacter);
    '%': Exit(Token(Iff(EQ('='), ttPercentIs, ttPercent)));
    '&': Exit(Token(ttAmpersand));
    SingleQuote: Exit(Characters); // string

    '(': // If we are inside an interpolated expression, count the unmatched "(".
      begin
        if OpenParens > 0 then Inc(OpenParens);
        Exit(Token(ttLeftParen));
      end;

    ')': // If we are inside an interpolated expression, count the ")".
      if OpenParens > 0 then
        begin
          Dec(OpenParens);
          if OpenParens = 0 then
            Exit(Characters)
          else
            Exit(Token(ttRightParen));
        end
      else Exit(Token(ttRightParen));

    '*': Exit(Token(Iff(EQ('='), ttStarIs, ttStar)));
    '+': Exit(Token(Iff(EQ('='), ttPlusIs, ttPlus)));
    ',': Exit(Token(ttComma));
    '-': Exit(Token(Iff(EQ('='), ttMinusIs, ttMinus)));
    '.':
      begin
        Typ := Iff(EQ('.'), ttDotDot, ttDot);
        if Typ = ttDotDot then
          Typ := Iff(EQ('<'), ttDotDotLeft, Typ);
        Exit(Token(Typ));
      end;
    '/': Exit(Token(Iff(EQ('='), ttSlashIs, Iff(EQ('\'), ttLambda , ttSlash))));
    '0':
      case PeekChar of
        'U', 'u': Exit(UnicodeNumber);
        'b': Exit(BinaryNumber);
        'o': Exit(OctalNumber);
        'x': Exit(HexaDecimal);
        else Exit(Number);
      end;
    '1'..'9': Exit(Number);
    ':': Exit(Token(Iff(EQ('='), ttAssign, ttColon)));
    '<': Exit(Token(Iff(EQ('='), ttLE, Iff(EQ('>'), ttNEQ,
           Iff(EQ('<'), ttShl, Iff(EQ('~'), ttLeftArrow, ttLT))))));
    '=': Exit(Token(Iff(EQ('>'), ttArrow, ttEQ)));
    '>': Exit(Token(Iff(EQ('='), ttGE, Iff(EQ('>'), ttShr, ttGT))));
    '?': Exit(Token(Iff(EQ('?'), tt2Questions, Iff(EQ('.'), ttQuestionDot, ttQuestion))));
    '@': Exit(Token(ttAtSign));
    '[': Exit(Token(ttLeftBracket));
    ']': Exit(Token(ttRightBracket));
    '^': Exit(Token(ttCaret));
    '{': Exit(Token(ttLeftBrace));
    '|': Exit(Token(ttVertBar));
    '}': Exit(Token(ttRightBrace));
    '~': Exit(Token(ttTilde));
    otherwise
      if Look.isAlpha then Exit(Identifier);
  end;

  if (Ord(Look) >= 32) and (Ord(Look) <= 126) then
    Result := ErrorToken(Format('unexpected character "%s"', [Look]))
  else
    Result := ErrorToken(Format('unexpected byte "0x%X"', [Ord(Look)]));
end;

// scan all tokens and add them to the token list
procedure TLexer.ScanTokens;
var
  Token: TToken;
begin
  InsertUseFiles;
  repeat
    Token := ScanToken;
    Tokens.Add(Token);
  until Token.Typ = ttEOF;
end;

{ TCharHelper }

function TCharHelper.isWhiteSpace: Boolean;
begin
  Result := Self in [Tab, {LineEnding,} Space];
end;

function TCharHelper.isUnderscore: Boolean;
begin
  Result := Self = '_';
end;

function TCharHelper.isLowCaseLetter: Boolean;
begin
  Result := Self in ['a'..'z'];
end;

function TCharHelper.isUpCaseLetter: Boolean;
begin
  Result := Self in ['A'..'Z'];
end;

function TCharHelper.isAlpha: Boolean;
begin
  Result := isUnderscore or isLowCaseLetter or isUpCaseLetter;
end;

function TCharHelper.isDigit: Boolean;
begin
  Result := Self in ['0'..'9'];
end;

function TCharHelper.isAlphaNum: Boolean;
begin
  Result := isAlpha or isDigit;
end;

function TCharHelper.isHexaDecimal: Boolean;
begin
  Result := Self in ['0'..'9', 'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f'];
end;

function TCharHelper.isBinaryDecimal: Boolean;
begin
  Result := Self in ['0', '1'];
end;

function TCharHelper.isOctoDecimal: Boolean;
begin
  Result := Self in ['0'..'7'];
end;

function TCharHelper.isSingleQuote: Boolean;
begin
  Result := Self = #39;
end;

function TCharHelper.isDoubleQuote: Boolean;
begin
  Result := Self = #34;
end;

function TCharHelper.isDot: Boolean;
begin
  Result := Self = '.';
end;

function TCharHelper.isLineEnding: Boolean; inline;
begin
  Result := Self = LineEnding;
end;

function TCharHelper.isFileEnding: Boolean; inline;
begin
  Result := Self = FileEnding;
end;

end.


