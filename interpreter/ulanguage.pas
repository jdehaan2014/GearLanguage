unit uLanguage;

{ This unit defines basic execution functions.

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
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, uReader, uLexer, uParser, uError, uAST,
  uPrinter, uInterpreter, uResolver;

type
  Language = record
    private
      class procedure PrintAST(Tree: TProduct); static;
      class procedure Execute(const Source: String; InputType: TInputType); static;
    public
      class var Interpreter: TInterpreter;
      class procedure ExecuteFromFile(const Source: String); static;
      class procedure ExecuteFromPrompt; static;
      class procedure CompileFromFile(const Source: String); static;
      class procedure ExecutePrintAST(const Source: String); static;
  end;

const
  GearVersion = 'v0.1';

var
  AppFolder: String = '';
  GearLibFolder: String = '';
  GearProdFolder: String = '';

implementation

class procedure Language.ExecuteFromFile(const Source: String);
begin
  WriteLn('Gear Interpreter ', GearVersion, ' - (c) J. de Haan 2018', LineEnding);
  Language.Execute(Source, itFile);
end;

class procedure Language.ExecuteFromPrompt;
var
  Source: String = '';
  Quit: Boolean = False;
begin
  WriteLn('Gear REPL ', GearVersion, ' - (c) J. de Haan 2018', LineEnding);
  while not Quit do begin
    Write('Gear> ');
    ReadLn(Source);
    Quit := LowerCase(Source) = 'quit';
    if not Quit then
      Language.Execute(Source, itPrompt);
    Errors.Reset;
  end;
end;

class procedure Language.CompileFromFile(const Source: String);
begin
  WriteLn('Gear Compiler ', GearVersion, ' - (c) J. de Haan 2018', LineEnding);
  WriteLn('Not implemented yet.', LineEnding);
end;

class procedure Language.Execute(const Source: String; InputType: TInputType);
var
  Parser: TParser;
  Tree: TProduct = Nil;
  Resolver: TResolver;
  Reader: TReader;
  Lexer: TLexer;
begin
  try
    Reader := TReader.Create(Source, InputType);
    Lexer := TLexer.Create(Reader);
    Parser := TParser.Create(Lexer);
    Tree := Parser.Parse;
    Resolver := TResolver.Create;
    Resolver.Resolve(Tree);
    if not Errors.isEmpty then
      WriteLn(Errors.toString)
    else
      Interpreter.Execute(Tree);
  finally
    if Assigned(Tree) then Tree.Free;
    Reader.Free;
    Lexer.Free;
    Parser.Free;
    Resolver.Free;
  end;
end;


class procedure Language.PrintAST(Tree: TProduct);
var
  Printer: TPrinter = Nil;
begin
  try
    try
      Printer := TPrinter.Create(Tree);
      Printer.Print;
    except
      on E: Exception do begin
        Writeln('Unable to print the AST due to:');
        Writeln(E.Message);
      end;
    end;
  finally
    if Assigned(Printer) then Printer.Free;
  end;
end;

class procedure Language.ExecutePrintAST(const Source: String);
var
  Parser: TParser;
  Tree: TProduct;
begin
  WriteLn('Gear AST ', GearVersion, ' - (c) J. de Haan 2018', LineEnding);
  try
    Parser := TParser.Create(TLexer.Create(TReader.Create(Source, itFile)));
    Tree := Parser.Parse;
    if not Errors.isEmpty then
      WriteLn(Errors.toString);
    PrintAST(Tree);

  finally
    Parser.Free;
    if Assigned(Tree) then Tree.Free;
  end;
end;


initialization
  Language.Interpreter := TInterpreter.Create;

finalization
  Language.Interpreter.Free;

end.

