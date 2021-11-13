unit uMain;

{$mode objfpc}{$H+}

interface

{ Structure of the Gear parser and compiler
  =========================================

      characters     tokens        AST          valid AST      bytecode
  Source ----> Lexer ----> Parser ----> Resolver ----> Compiler ----> VM

  Source consists of a text string.
  Lexer produces a list of tokens.
  Parser checks syntax and creates the AST.
  Resolver checks AST semantics, identifiers, and context.
  Compiler generates byte code from the AST.
  VM executes the byte code.

  The visitor pattern is used to walk the AST. For this an external visitor is used.
  See unit uVisitor.
}

uses
  Classes, SysUtils;

procedure Execute(const InputFile: TFileName);
procedure Compile(const InputFile: TFileName);
procedure PrintAST(const InputFile: TFileName);

implementation
uses uCommon, uReader, uLexer, uExprParser, uAST, uError, uPrinter, uResolver
  {, uInterpreter}, uVM;

function ParseInput(const InputFile: TFileName): TProduct;
var
  Reader: TReader;
  Lexer: TLexer;
  Parser: TExprParser=Nil;
begin
  try
    Reader := TReader.Create(InputFile);
    Lexer := TLexer.Create(Reader);
    Parser := TExprParser.Create(Lexer);
    Result := Parser.Parse;
  finally
    Parser.Free;
    Lexer.Free;
    Reader.Free;
  end;
end;

procedure Execute(const InputFile: TFileName);
//var
//  Product: TProduct;
//  Resolver: TResolver;
//  Interpreter: TInterpreter;
begin
  try
    WriteLn('Gear Interpreter ', GearVersion, ' - (c) J. de Haan ', Year, LineEnding);
    //Product := ParseInput(InputFile);
    //
    //// Analyze the AST and Symbol table
    //Resolver := TResolver.Create;
    //Resolver.Resolve(Product);
    //
    //if Warnings.Count > 0 then
    //  WriteLn(Warnings.toString);
    //
    //if Errors.Count > 0 then
    //  WriteLn(Errors.toString)
    //else
    //  begin
    //     Interpreter := TInterpreter.Create;
    //     Interpreter.Run(Product);
    //     Interpreter.Free;
    //  end;
  finally
    writeln(LineEnding, 'Gear Interpreter done.', LineEnding);
    //Resolver.Free;
  end;
end;

procedure Compile(const InputFile: TFileName);
var
  Product: TProduct;
  Resolver: TResolver=Nil;
  InterpretResult: TInterpretResult;
  DebugFileName: TFileName;
begin
  DebugFileName := ExtractFilePath(ExcludeTrailingPathDelimiter(InputFile)) + 'debug.txt';
  WriteLn('Gear Compiler ', GearVersion, ' - (c) J. de Haan ', Year);
  WriteLn('File: ', InputFile, LineEnding);
  try
    Product := ParseInput(InputFile);

    if Errors.Count > 0 then
      begin
        WriteLn(Errors.toString);
        Exit;
      end
    else
      begin
         // Analyze the AST and resolve symbols
         Resolver := TResolver.Create;
         Resolver.Resolve(Product);
         if Errors.Count > 0 then
           begin
             WriteLn(Errors.toString);
             Exit;
           end
      end;

    VM.Init;
    InterpretResult := VM.Interpret(Product, DebugFileName);
    VM.Free;

    if InterpretResult = irCompileError then Halt(65);
    if InterpretResult = irRuntimeError then Halt(70);

  finally
    if Assigned(Resolver) then Resolver.Free;
    Write(LineEnding, 'Gear Compiler & Interpreter done with ', Errors.Count);
    writeln(specialize IfThen<String>(Errors.Count=1 , ' error.', ' errors.'), LineEnding);
  end;
end;

procedure PrintAST(const InputFile: TFileName);
const
  IndentSize = 2;
var
  Product: TProduct;
  Printer: TPrinter;
  ASTFileName: TFileName;
begin
  try
    ASTFileName := ExtractFilePath(ExcludeTrailingPathDelimiter(InputFile)) + 'ast.txt';
    WriteLn('Gear AST printer', GearVersion, ' - (c) J. de Haan ', Year, LineEnding);
    Product := ParseInput(InputFile);

    if Errors.Count > 0 then
        WriteLn(Errors.toString)
    else
      begin
         Printer := TPrinter.Create(IndentSize);
         Printer.Print(Product, ASTFileName);
         Printer.free;
       end;
  finally
    WriteLn('Look for AST in "ast.txt".');
    writeln(LineEnding, 'Gear AST printer done.', LineEnding);
  end;
end;


end.


