unit uCallable;

{ This unit contains the ICallable interface, needed for function calls.

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
  Classes, SysUtils, uCollections, uInterpreter, uToken, uAST, Variants;

type
  TCallArg = class
    Value: Variant;
    Ident: TIdent;
    Token: TToken;
    constructor Create(AValue: Variant; AIdent: TIdent; AToken: TToken);
  end;

  TArgList = specialize TArrayObj<TCallArg>;

  ICallable = interface
    ['{EBA5469A-1CA1-FB54-0D6F-60889AFA4679}']
    function Call(Token: TToken; Interpreter: TInterpreter;
      ArgList: TArgList): Variant;
    function toString: String;
  end;

  IValuable = interface(ICallable)
    ['{C5B7F7F5-3E31-AE0F-C7E2-FC4B56B77224}']
  end;

implementation

constructor TCallArg.Create(AValue: Variant; AIdent: TIdent; AToken: TToken);
begin
  Value := AValue;
  Ident := AIdent;
  Token := AToken;
end;

end.

