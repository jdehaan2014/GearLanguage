unit uPrecedence;

{$mode objfpc}{$H+}

interface

{
 Defines the different precendence levels used by the infix parsers. These
 determine how a series of infix expressions will be grouped. For example,
 "a + b * c - d" will be parsed as "(a + (b * c)) - d" because "*" has higher
 precedence than "+" and "-". Here, bigger numbers mean higher precedence.

}

const
  precNone        = 0;
  precPrecision   = 1;  // <~
  precConditional = 2;  // ?: IfExpr
  precOr          = 3;  // or
  precAnd         = 4;  // and
  precEquality    = 5;  // = <>
  precComparison  = 6;  // > >= < <=
  precTerm        = 7;  // + - or xor ~
  precFactor      = 8;  // * / % and << >> is in
  precExponent    = 9;  // ^
  precRange       = 10;  // ..  ..<
  precUnary       = 11; // + - not
  precCall        = 12; // . () []
  precPrimary     = 13; // literals

implementation

end.


