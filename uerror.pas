unit uError;

{ This unit defines the Error functionality. During parsing all errors are
  gathered and shown. During runtime each error stops execution.

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
  Classes, SysUtils, uToken, uCollections;

type

  TErrorItem = class
    Line, Col: Integer;
    FileIndex: Integer;
    Msg: String;
    constructor Create(const ALine, ACol: Integer; const AMsg: String);
    constructor Create(AToken: TToken; const AMsg: String);
    function toString: String; override;
  end;

  TErrors = class(specialize TArrayObj<TErrorItem>)
    procedure Append(const ALine, ACol: Integer; const AMsg: String);
    procedure Append(AToken: TToken; const AMsg: String);
    function isEmpty: Boolean;
    procedure Reset;
    function toString: String; override;
  end;

  EParseError = class(Exception);

  EBreakException = class(Exception);
  EContinueException = class(Exception);

  ERuntimeError = class(Exception)
    Token: TToken;
    constructor Create(AToken: TToken; AMessage: String);
  end;

  EReturnFromFunc = class(Exception)
    Value: Variant;
    constructor Create(AValue: Variant);
  end;

procedure RuntimeError(E: ERuntimeError);
//procedure RuntimeWarning(E: ERuntimeError);

var
  Errors: TErrors;

implementation
uses uReader;

{ TErrorItem }

constructor TErrorItem.Create(const ALine, ACol: Integer; const AMsg: String);
begin
  Line := ALine;
  Col := ACol;
  Msg := AMsg;
  FileIndex := 0;
end;

constructor TErrorItem.Create(AToken: TToken; const AMsg: String);
begin
  Line := AToken.Line;
  Col := AToken.Col;
  Msg := AMsg;
  FileIndex := AToken.FileIndex;
end;

function TErrorItem.toString: String;
begin
  Result := Format('@[%d,%d] in %s: %s',
    [Line, Col, ExtractFileName(FileNameArray[FileIndex]), Msg]);
end;

{ TErrors }

procedure TErrors.Append(const ALine, ACol: Integer; const AMsg: String);
begin
  Add(TErrorItem.Create(ALine, ACol, AMsg));
end;

procedure TErrors.Append(AToken: TToken; const AMsg: String);
begin
  Add(TErrorItem.Create(AToken, AMsg));
end;

function TErrors.isEmpty: Boolean;
begin
  Result := Count = 0;
end;

procedure TErrors.Reset;
begin
  Clear;
end;

function TErrors.toString: String;
var
  Item: TErrorItem;
begin
  Result := 'Errors:' + LineEnding;
  for Item in Self do
    Result += Item.toString + LineEnding;
end;

{ proc RuntimeError }

procedure RuntimeError(E: ERuntimeError);
const
  ErrString = '@[%d,%d] in %s - Runtime error: %s';
begin
  with E.Token do
    Writeln(Format(ErrString,
      [Line, Col, ExtractFileName(FileNameArray[FileIndex]), E.Message]));
  Exit;
end;

//procedure RuntimeWarning(E: ERuntimeError);
//begin
//  WriteLn('@[' + IntToStr(E.Token.Line) + ',' + IntToStr(E.Token.Col) + '] ' +
//    'Runtime warning: ', E.Message);
//end;

{ ERuntimeError }

constructor ERuntimeError.Create(AToken: TToken; AMessage: String);
begin
  Token := AToken;
  inherited Create(AMessage);
end;

{ EReturnFromFunc }

constructor EReturnFromFunc.Create(AValue: Variant);
begin
  Value := AValue;
  inherited Create('return');
end;

initialization
  Errors := TErrors.Create();

finalization
  Errors.Free;
end.


