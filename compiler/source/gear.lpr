program gear;

{$mode objfpc}{$H+}

{ This program (including its units) is a parser, compiler and interpreter
  for the Gear programming language.

  Copyright (C) 2020 J. de Haan jdehaan2014@gmail.com

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
{
To do list:
* replace generics.collections with extended TTable
* base obj contains a class
* new module system with pre-compiled units
* commenting/explaining code
* extensions to basic types
* error handling
* range of characters, i.e. 'a'..'z'
* graphics
* program arguments
* debugger
* 'is' operator also for parent class

}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { you can add units after this },
  SysUtils,
  uCommon, uMain;

const
  PossibleParams = ['c', 'h', 'p', 'x'];

type
  TOperation = (opCompile, opHelp, opPrintAST, opRun);
  TOperations = array['a'..'z'] of TOperation;

const
  Usage = 'Usage: "gear" "h(elp)" | ( "x(ecute)" | "c(ompile)" | "p(rint)" ) "f" <path/filename>';

var
  Operations: TOperations;
  Operation: TOperation;
  CmdParam: Char;
  InputFile: String = '';

procedure WriteHelp(Msg: String='');
begin
  if Msg <> '' then
    WriteLn(LineEnding, Msg);
  WriteLn(LineEnding, Usage);
  WriteLn;
end;

function GetFlag(const Parameter: ShortString): Char;
begin
  if Parameter[1] = '-' then
    Result := Parameter[2]
  else
    Result := Parameter[1];
end;

begin
  //get the application directory from the executable
  AppFolder := ExtractFilePath(ExcludeTrailingPathDelimiter(ParamStr(0)));
  //AppFolder := ExtractFilePath(ParamStr(0));
  LibraryFolder := AppFolder + 'gearlib/';

  // init operations
  Operations['c'] := opCompile;
  Operations['h'] := opHelp;
  Operations['p'] := opPrintAST;
  Operations['x'] := opRun;

  //check if operation exists
  if ParamStr(1) <> '' then
    begin
      CmdParam := GetFlag(ParamStr(1));
      if not (CmdParam in PossibleParams) then
        begin
          WriteHelp(Format('Error: Operation "%s" not supported.', [CmdParam]));
          Halt;
        end;
    end
  else
    begin
      WriteHelp;
      Halt;
    end;

  //get operation
  Operation := Operations[CmdParam];

  //2nd parameter must be an '[-]f' followed by filename
  if Operation <> opHelp then
    begin
      CmdParam := GetFlag(ParamStr(2));
      if CmdParam = 'f' then
        InputFile := ParamStr(3);
      if InputFile = '' then
        begin
          WriteHelp('Error: No input file provided.');
          Halt;
        end
      else if not FileExists(InputFile) then
        begin
          WriteHelp('Error: Input file does not exist.');
          Halt;
        end;
      ProductFolder := ExtractFilePath(ExcludeTrailingPathDelimiter(InputFile));
    end;

  case Operation of
    opCompile:  Compile(InputFile);
    opHelp:     WriteHelp;
    opPrintAST: PrintAst(InputFile);
    opRun:      Execute(InputFile);
  end;
end.

