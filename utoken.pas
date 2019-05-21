unit uToken;

{ This unit contains the token class and the definition of all token types.

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
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, uCollections, Variants;

type

  TTokenTyp = (
    //Expressions - operators
    ttPlus, ttMin, ttMul, ttDiv, ttRem,
    ttPlusIs, ttMinIs, ttMulIs, ttDivIs, ttRemIs,
    ttOr, ttAnd, ttNot, ttXor,
    ttShl, ttShr, ttPow, ttConcat, ttConcatIs ,
    ttEQ, ttNEQ, ttGT, ttGE, ttLT, ttLE,

    //Keywords declarations
    ttArray, ttClass, ttDictionary, ttEach, ttEnum, ttExtension, ttFunc,
    ttInit, ttLet, ttVar, ttVal, ttTrait,
    //Keywords statements and expressions
    ttIf, ttThen, ttElse, ttElseif, ttWhile, ttDo, ttRepeat, ttUntil,
    ttFor, ttIn, ttIs, ttReturn, ttEnd, ttMatch, ttWhere, ttSwitch, ttCase,
    ttEnsure, ttPrint, ttInherited, ttSelf, ttUse, ttBreak, ttOn, ttContinue,
    ttIdentifier,

    //Constant values
    ttFalse, ttTrue, ttNull, ttNumber, ttString, ttChar, ttInterpolated,

    //Symbols and punctuation marks
    ttComma, ttDot, ttDotDot, ttAssign, ttQuestion, ttArrow, ttColon, ttColons,
    ttOpenParen, ttCloseParen, ttOpenBrace, ttCloseBrace,
    ttOpenBrack, ttCloseBrack, ttComment, ttEOF, ttNone
  );

  TTokenTypHelper = type helper for TTokenTyp
    function toString: string;
  end;

  TTokenTypSet = set of TTokenTyp;
  TTokenTypSetHelper = type helper for TTokenTypSet
    function Contains(TokenTyp: TTokenTyp): Boolean;
  end;

  TToken = class
    private
      FTyp: TTokenTyp;
      FLexeme: String;
      FValue: Variant;
      FLine, FCol: LongInt;
      FFileIndex: Integer;
    public
      property Typ: TTokenTyp read FTyp;
      property Lexeme: String read FLexeme;
      property Value: Variant read FValue;
      property Line: LongInt read FLine;
      property Col: LongInt read FCol;
      property FileIndex: Integer read FFileIndex;
      constructor Create(ATyp: TTokenTyp; ALexeme: String;
        AValue: Variant; ALine, ACol: LongInt; AFileIndex: Integer = 0);
      function toString: String; override;
      function Copy: TToken;
  end;

  TTokens = specialize TArrayObj<TToken>;
  TTokensHelper = class helper for TTokens
    function toText: String;
  end;

  TKeywords = specialize TDictionary<string, TTokenTyp>;

var
  Keywords: TKeywords;

implementation

constructor TToken.Create
  (ATyp: TTokenTyp; ALexeme: String; AValue: Variant; ALine, ACol: LongInt; AFileIndex: Integer
  );
begin
  FTyp := ATyp;
  FLexeme := ALexeme;
  FValue := AValue;
  FLine := ALine;
  FCol := ACol;
  FFileIndex := AFileIndex;
end;

function TToken.toString: String;
var
  TypStr: String;
begin
  WriteStr(TypStr, FTyp);
  Result := TypStr.Substring(2) + ' ("' + FLexeme + '")';
  Result += ' ' + VarToStr(FValue);  // special function to print variant value
end;

function TToken.Copy: TToken;
begin
  Result := TToken.Create(FTyp, FLexeme, FValue, FLine, FCol);
end;

{ TTokenTypHelper }

function TTokenTypHelper.toString: string;
begin
  case Self of
    ttPlus : Result := '+';
    ttMin  : Result := '-';
    ttMul  : Result := '*';
    ttDiv  : Result := '/';
    ttRem  : Result := '%';
    ttPlusIs : Result := '+=';
    ttMinIs  : Result := '-=';
    ttMulIs  : Result := '*=';
    ttDivIs  : Result := '/=';
    ttRemIs  : Result := '%=';
    ttOr : Result := '|';
    ttAnd : Result := '&';
    ttNot : Result := '!';
    ttXor : Result := '~';
    ttShl : Result := '<<';
    ttShr : Result := '>>';
    ttPow : Result := '^';
    ttConcat: Result := '><';
    ttConcatIs: Result := '><=';
    ttEQ : Result := '=';
    ttNEQ : Result := '<>';
    ttGT : Result := '>';
    ttGE : Result := '>=';
    ttLT : Result := '<';
    ttLE : Result := '<=';
    ttComma: Result := ',';
    ttDot: Result := '.';
    ttDotDot: Result := '..';
    ttAssign: Result := ':=';
    ttQuestion: Result := '?';
    ttArrow: Result := '=>';
    ttColon: Result := ':';
    ttColons: Result := '::';
    ttOpenParen: Result := '(';
    ttCloseParen: Result := ')';
    ttOpenBrace: Result := '{';
    ttCloseBrace: Result := '}';
    ttOpenBrack: Result := '[';
    ttCloseBrack: Result := ']';
    ttEOF: Result := 'End of file';
    else begin
      WriteStr(Result, Self);
      Result := Result.Substring(2).ToLower;
    end;
  end;
end;

{ TTokensHelper }

function TTokensHelper.toText: String;
var
  Token: TToken;
begin
  Result := '';
  for Token in Self do
    Result += Token.toString + LineEnding;
end;

{ TTokenTypSetHelper }

function TTokenTypSetHelper.Contains(TokenTyp: TTokenTyp): Boolean;
begin
  Result := TokenTyp in Self;
end;

initialization

  Keywords := TKeywords.Create;
  Keywords.Sorted := True;

  // the constant values
  Keywords['False'] := ttFalse;
  Keywords['Null'] := ttNull;
  Keywords['True'] := ttTrue;

  // the keywords
  Keywords['array'] := ttArray;
  Keywords['break'] := ttBreak;
  Keywords['case'] := ttCase;
  Keywords['class'] := ttClass;
  Keywords['continue'] := ttContinue;
  Keywords['dictionary'] := ttDictionary;
  Keywords['do'] := ttDo;
  Keywords['each'] := ttEach;
  Keywords['else'] := ttElse;
  Keywords['elseif'] := ttElseif;
  Keywords['end'] := ttEnd;
  Keywords['ensure'] := ttEnsure;
  Keywords['enum'] := ttEnum;
  Keywords['extension'] := ttExtension;
  Keywords['for'] := ttFor;
  Keywords['func'] := ttFunc;
  Keywords['if'] := ttIf;
  Keywords['in'] := ttIn;
  Keywords['is'] := ttIs;
  Keywords['inherited'] := ttInherited;
  Keywords['init'] := ttInit;
  Keywords['let'] := ttLet;
  Keywords['match'] := ttMatch;
  Keywords['on'] := ttOn;
  Keywords['print'] := ttPrint;
  Keywords['repeat'] := ttRepeat;
  Keywords['return'] := ttReturn;
  Keywords['self'] := ttSelf;
  Keywords['switch'] := ttSwitch;
  Keywords['then'] := ttThen;
  Keywords['trait'] := ttTrait;
  Keywords['until'] := ttUntil;
  Keywords['use'] := ttUse;
  Keywords['val'] := ttVal;
  Keywords['var'] := ttVar;
  Keywords['where'] := ttWhere;
  Keywords['while'] := ttWhile;

finalization
  Keywords.Free;

end.

