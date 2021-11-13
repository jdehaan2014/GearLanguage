unit uFunc;

{ This unit contains the TFunc class.

  The code is based on the online book 'Crafting Interpreters',
  written by Bob Nystrom. http://craftinginterpreters.com
  The code is originally written in Java.

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
  Classes, SysUtils, uCallable, uInterpreter, uAst, uMemory, uError, uToken;

type

  TFunc = class(TInterfacedObject, ICallable)
    private
      FFuncDecl: TFuncDecl;
      FClosure: TMemorySpace;
      procedure CheckIdents(Token: TToken; IdentCalled, IdentDefined: TIdent);
    public
      property FuncDecl: TFuncDecl read FFuncDecl;
      constructor Create(AFuncDecl: TFuncDecl; AClosure: TMemorySpace);
      function Call(Token: TToken; Interpreter: TInterpreter;
        ArgList: TArgList): Variant;
      function toString: String; override;
      function Bind(Instance: Variant): Variant;
      class procedure CheckArity(Token: TToken; NumArgs, NumParams: Integer); static;
  end;

  TVal = class(TFunc, IValuable)
    // this is empty
  end;

implementation

const
  ErrInvalidNumberOfArgs = 'Invalid number of arguments. Expected %d arguments.';
  ErrExpectedNoExtId = 'Did not expect external identifier "%s:".';
  ErrExpectedExtId = 'Expected external identifier: "%s:".';
  ErrExtIdMismatch = 'External identifier mismatch: expected "%s:".';


constructor TFunc.Create(AFuncDecl: TFuncDecl; AClosure: TMemorySpace);
begin
  FFuncDecl := AFuncDecl;
  FClosure := AClosure;
end;

function TFunc.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  FuncSpace: TMemorySpace;
  i: Integer;
begin
  CheckArity(Token, ArgList.Count, FFuncDecl.Params.Count);
  Result := Null;
  FuncSpace := TMemorySpace.Create(FClosure);
  for i := 0 to FFuncDecl.Params.Count-1 do begin
    CheckIdents(ArgList[i].Token, ArgList[i].Ident, FFuncDecl.Params[i].ExtIdent);
    FuncSpace.Store(FFuncDecl.Params[i].Ident, ArgList[i].Value);
  end;
  try
    Interpreter.Execute(FFuncDecl.Body, FuncSpace);
  except
    on E: EReturnFromFunc do
      Result := E.Value;
    //on E: Exception do
    //  Raise ERuntimeError.Create(Token, Format(
    //    'Problem in function body "%s". Check missing "self".' + LineEnding + E.Message,
    //    [FFuncDecl.Ident.Text]));;
  end;
end;

function TFunc.toString: String;
begin
  Result := '<func ' + FFuncDecl.Ident.Text+ '>';
end;

class procedure TFunc.CheckArity(Token: TToken; NumArgs, NumParams: Integer);
begin
  if NumArgs <> NumParams then
    Raise ERuntimeError.Create(Token, Format(
      ErrInvalidNumberOfArgs, [NumParams]));
end;

function TFunc.Bind(Instance: Variant): Variant;
var
  MemorySpace: TMemorySpace;
begin
  MemorySpace := TMemorySpace.Create(FClosure);
  MemorySpace.Store('self', Instance);
  Result := ICallable(TFunc.Create(FuncDecl, MemorySpace));
end;

procedure TFunc.CheckIdents(Token: TToken; IdentCalled, IdentDefined: TIdent);
begin
  if (IdentCalled = Nil) and (IdentDefined = Nil) then Exit;
  if IdentDefined = Nil then
    Raise ERuntimeError.Create(Token, Format(
      ErrExpectedNoExtId, [IdentCalled.Text]));
  if IdentCalled = Nil then
    Raise ERuntimeError.Create(Token, Format(
      ErrExpectedExtId, [IdentDefined.Text]));
  if IdentCalled.Text <> IdentDefined.Text then
    Raise ERuntimeError.Create(Token, Format(
      ErrExtIdMismatch, [IdentDefined.Text]));
end;

end.

