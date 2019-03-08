program gear;

{ This program is an interpreter/compiler for the Gear language, described in the
  accompanying document GearLang - An interpreter in Free Pascal using an
  external visitor pattern.

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

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, uLanguage;
type

  { TGearLang }

  TGearLang = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TGearLang }

procedure TGearLang.DoRun;
var
  ErrorMsg: String;
  InputFile: String='';
  FileNeeded: Boolean = False;

begin
  // quick check parameters
  ErrorMsg := CheckOptions('hxcaf:', 'help execute compile ast file:');
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { File and filename required for a, x, c }
  FileNeeded := HasOption('a', 'ast') or       // print AST
                HasOption('x', 'execute') or   // execute file
                HasOption('c', 'compile');     // compile file

  if FileNeeded and not HasOption('f', 'file') then begin
    ShowException(Exception.Create('Input file name is required.'));
    Terminate;
    Exit;
  end;

  if HasOption('f', 'file') then begin
    InputFile := GetOptionValue('f', 'file');
    if not FileExists(InputFile) then begin
      ShowException(Exception.Create('Input file does not exist.'));
      Terminate;
      Exit;
    end;
    if not (ExtractFileExt(InputFile) = '.gear') then begin
      ShowException(Exception.Create('File extension must be ".gear".'));
      Terminate;
      Exit;
    end;
    GearProdFolder := ExtractFilePath(ExcludeTrailingPathDelimiter(InputFile));
    //writeln(GearProdFolder);
  end;

  { Execute a file }
  if HasOption('a', 'ast') then
    Language.ExecutePrintAST(InputFile)
  else if HasOption('x', 'execute') then
    Language.ExecuteFromFile(InputFile)
  else if HasOption('c', 'compile') then
    Language.CompileFromFile(InputFile)
  else  // gear REPL started
    Language.ExecuteFromPrompt;

  // stop program loop
  Terminate;
end;

constructor TGearLang.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TGearLang.Destroy;
begin
  inherited Destroy;
end;

procedure TGearLang.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExtractFileName(ExeName), ' -h -(a|x|c) -f filename.gear');
  writeln('Option:    -h --help               Show help');
  writeln('Option:    -a --ast                Print AST');
  writeln('Option:    -x --execute            Execute product');
  writeln('Option:    -c --compile            Compile product');
  writeln('Required:  -f --file= filename     Input product');
  writeln('No parameters: start REPL');
end;

var
  Application: TGearLang;
begin
  //get the application directory from the executable
  AppFolder := ExtractFilePath(ExcludeTrailingPathDelimiter(ParamStr(0)));
  GearLibFolder := AppFolder + 'gearlib/';
  //writeln(GearLibFolder);
  Application := TGearLang.Create(nil);
  Application.Title := 'Gear Language';
  Application.Run;
  Application.Free;
end.

