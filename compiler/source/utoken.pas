unit uToken;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch typehelpers}

interface

uses
  SysUtils, Generics.Collections, fgl;

type

  TTokenTyp = (
    // Math operators.
    ttPlus, ttMinus, ttStar, ttSlash, ttPercent, ttShl{<<}, ttShr{>>}, ttCaret{^},
    ttExclamation{bitwise not}, ttVertBar{bitwise or}, ttAmpersand{bitwise and},
    ttTilde{bitwise xor},

    // Symbol tokens.
    ttLeftParen, ttRightParen, ttLeftBrace, ttRightBrace,
    ttLeftBracket, ttRightBracket,
    ttComma, ttDot, ttDotDot{..}, ttDotDotLeft{..<},
    ttQuestionDot{?.}, ttQuestion{?}, tt2Questions{??},
    ttAtSign{@}, ttColon{:}, ttArrow{=>}, ttLeftArrow{<~},

    // Assignment
    ttAssign{:=}, ttPlusIs{+=}, ttMinusIs{-=}, ttStarIs{*=}, ttSlashIs{/=}, ttPercentIs{%=},

    // Comparison tokens.
    ttEQ{=}, ttNEQ{<>}, ttGT{>}, ttGE{>=}, ttLT{<}, ttLE{<=},

    // Literals.
    ttIdentifier, ttString, ttInterpolated, ttNumber, ttCharacter, ttLine,

    // Keywords.
    ttAnd, ttBreak, ttCase, ttClass, ttContinue, ttDefault, ttDefer, ttDo,
    ttElse,ttElseIf,
    ttEnd, ttEnsure, ttEnum, ttExit, ttExtension, ttFalse, ttFor, ttFunc,
    ttIf, ttIn, {ttIndexer,} ttInfix, ttInherited, ttIs, ttLambda, ttLet, ttLoop, ttMatch,
    ttNil, ttNot, ttNotIn, ttOr, ttPrefix, ttPrint, ttRecord, ttReturn, ttSelf, ttStatic,
    ttSubscript,
    ttSwitch, ttThen, ttTrait, ttTrue, ttUse, ttVal, ttVar, ttWhen, ttWhere, ttWhile,

    ttError, ttEOF, ttNone
  );

  // convert enum tokentyp to a string representation
  TTokenTypHelper = type helper for TTokenTyp
    function toString: String;
  end;

  TTokenTypSet = set of TTokenTyp;

  // Keep track of token position
  TLocation = record
    Line, Col: Integer;
    FileIndex: Integer;
    constructor Create(const ALine, ACol: Integer; const AFileIndex: Integer = 0);
    function toString: String;
  end;

  // a token holds a type, a lexeme and a location
  TToken = class
    Typ: TTokenTyp;
    Lexeme: String;
    Location: TLocation;
    constructor Create(ATyp: TTokenTyp; ALexeme: String; ALocation: TLocation);
    function toString: String; override;
  end;

  // the ordered map of keywords
  TKeywords = specialize TFPGMap<String, TTokenTyp>;

  // a list of tokens
  TTokens = specialize TObjectList<TToken>;


const

  AssignSet: TTokenTypSet = // set of all assign token types
    [ttAssign, ttPlusIs, ttMinusIs, ttStarIs, ttSlashIs, ttPercentIs];

  BlockEndSet: TTokenTypSet = // set of possible block end token types
    [ttCase, ttDefault, ttElse, ttElseIf, ttEnd, ttExit, ttEOF];

  StmtStartSet: TTokenTypSet = // set of possible statement start token types
    [ttBreak, ttContinue, ttDo, ttDefer, ttEnsure, ttFor, ttIdentifier,
    ttIf, ttInherited, ttLoop, ttPrint, ttReturn, ttSelf, ttAtSign,
    ttSwitch, ttUse, ttWhile];

  DeclStartSet: TTokenTypSet = // set of possible declaration start token types
    [ttClass, ttEnum, ttExtension, ttFunc, ttLet, ttRecord, ttStatic,
    ttTrait, ttVal, ttVar];

  SynchronizeSet: TTokenTypSet = // set of possible token types for synch after error
    [ttClass, ttDefer, ttDo, ttEnd, ttEnsure, ttEnum, ttExtension,
    ttFor, ttFunc, ttIf, {ttIndexer,} ttLet, ttLoop, ttPrint, ttRecord,
    ttReturn, ttStatic, ttSubscript, ttSwitch, ttTrait, ttVar, ttWhile, ttEOF];

  ComparisonOperators: TTokenTypSet = // set of possible comparison operators
    [ttEQ, ttNEQ, ttGT, ttGE, ttLT, ttLE, ttIn, ttIs];

  AllOperators: TTokenTypSet = // set of all operators that can be overloaded
    [ttPlus, ttMinus, ttStar, ttSlash, ttPercent,
    ttShl, ttShr, ttCaret, ttExclamation, ttVertBar, ttAmpersand, ttTilde,
    //ttPlusIs, ttMinusIs, ttStarIs, ttSlashIs, ttPercentIs,
    ttQuestion, ttLeftArrow,
    ttEQ, ttNEQ, ttGT, ttGE, ttLT, ttLE,
    ttAnd, ttNot, ttOr, ttIn, ttIs];
  //, ttNilCoesc, ttElvis, ttSpaceShip


var
  Keywords: TKeywords;

implementation
uses uCommon;

{ TLocation }

constructor TLocation.Create(const ALine, ACol: Integer; const AFileIndex: Integer);
begin
  Line := ALine;
  Col := ACol;
  FileIndex := AFileIndex;
end;

function TLocation.toString: String;
begin
  Result := Format('[%d,%d] in "%s"', [Line, Col, FileNameList[FileIndex]]);
end;

const
  // holds string representations of the enums of tokentype
  TokenTypStr: array[TTokenTyp] of String = (
    '+', '-', '*', '/', '%', '<<', '>>', '^',
    '!', '|', '&', '~',
    '(', ')', '{', '}', '[', ']',
    ',', '.', '..', '..<', '?.', '?', '??', '@', ':', '=>', '<~',
    ':=', '+=', '-=', '*=', '/=', '%=',
    '=', '<>', '>', '>=', '<', '<=',
    'identifier', 'string', 'interpolated', 'number', 'character', 'newLine',

    'and', 'break', 'case', 'class', 'continue', 'default', 'defer', 'do', 'else',
    'elseif', 'end', 'ensure', 'enum', 'exit', 'extension', 'false', 'for', 'func',
    'if', 'in', {'indexer',} 'infix', 'inherited', 'is', 'lambda', 'let', 'loop', 'match',
    'nil', 'not', 'not in', 'or', 'prefix', 'print', 'record', 'return', 'self', 'static',
    'subscript',
    'switch', 'then', 'trait', 'true', 'use', 'val', 'var', 'when', 'where', 'while',
    'error', 'EOF', 'none'
  );

{ TTokenTypHelper }

function TTokenTypHelper.toString: String;
begin
  Result := TokenTypStr[Self];
end;


{ TToken }

constructor TToken.Create(ATyp: TTokenTyp; ALexeme: String; ALocation: TLocation);
begin
  Typ := ATyp;
  Lexeme := ALexeme;
  Location := ALocation;
  if ATyp = ttLine then // line token appears on line containing newLine
    Dec(Location.Line);
end;

function TToken.toString: String;
var StrTyp: String;
begin
  WriteStr(StrTyp, Typ);
  Result := 'Type: ' +  StrTyp + ' Lexeme: ' + Lexeme + ' Loc: ' + Location.toString;
end;


initialization

  Keywords := TKeywords.Create;
  Keywords.Sorted := True;

  // the keywords
  Keywords['and'] := ttAnd;
  Keywords['break'] := ttBreak;
  Keywords['case'] := ttCase;
  Keywords['class'] := ttClass;
  Keywords['continue'] := ttContinue;
  Keywords['default'] := ttDefault;
  Keywords['defer'] := ttDefer;
  Keywords['do'] := ttDo;
  Keywords['else'] := ttElse;
  Keywords['elseif'] := ttElseIf;
  Keywords['end'] := ttEnd;
  Keywords['ensure'] := ttEnsure;
  Keywords['enum'] := ttEnum;
  Keywords['exit'] := ttExit;
  Keywords['extension'] := ttExtension;
  Keywords['false'] := ttFalse;
  Keywords['for'] := ttFor;
  Keywords['func'] := ttFunc;
  Keywords['if'] := ttIf;
  Keywords['in'] := ttIn;
  //Keywords['indexer'] := ttIndexer;
  Keywords['infix'] := ttInfix;
  Keywords['inherited'] := ttInherited;
  Keywords['is'] := ttIs;
  Keywords['lambda'] := ttLambda;
  Keywords['let'] := ttLet;
  Keywords['loop'] := ttLoop;
  Keywords['match'] := ttMatch;
  Keywords['nil'] := ttNil;
  Keywords['not'] := ttNot;
  Keywords['or'] := ttOr;
  Keywords['prefix'] := ttPrefix;
  Keywords['print'] := ttPrint;
  Keywords['record'] := ttRecord;
  Keywords['return'] := ttReturn;
  Keywords['self'] := ttSelf;
  Keywords['static'] := ttStatic;
  Keywords['subscript'] := ttSubscript;
  Keywords['switch'] := ttSwitch;
  Keywords['then'] := ttThen;
  Keywords['trait'] := ttTrait;
  Keywords['true'] := ttTrue;
  Keywords['use'] := ttUse;
  Keywords['var'] := ttVar;
  Keywords['val'] := ttVal;
  Keywords['when'] := ttWhen;
  Keywords['where'] := ttWhere;
  Keywords['while'] := ttWhile;

finalization
  Keywords.Free;

end.



