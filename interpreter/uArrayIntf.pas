unit uArrayIntf;

{ This unit contains the interfaces needed for TArrayClass and TArrayInstance.

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
  Classes, SysUtils, uCallable, uAST, Variants, uMembers, uCollections, uToken;

type

  IArrayable = Interface(ICallable)
    ['{1A6EC506-9318-F69B-0563-830506C351F7}']
    procedure ExtendWith(Members: TMembers);
  end;

  TArrayElements = specialize TArray<Variant>;

  IArrayInstance = Interface
    ['{5C2DF1A9-88BC-8FFE-B6F2-6C3DCF535601}']
    function GetMember(Ident: TIdent): Variant;
    function getElements: TArrayElements;
    function getCount: LongInt;
    property Elements: TArrayElements read getElements;
    property Count: LongInt read getCount;
    function getItem(i: integer): Variant;
    procedure setItem(i: integer; AValue: Variant);
    property Items[i:integer]: Variant read getItem write setItem; default;
    function TypeName: String;
    function Get(i: Integer; Token: TToken): Variant;
    procedure Put(i: Integer; Value: Variant; Token: TToken);
    procedure Add(const AValue: Variant);
    procedure Insert(const i: Integer; const AValue: Variant; Token: TToken);
    procedure Delete(const AValue: Variant; Token: TToken);
    function Contains(const AValue: Variant): Boolean;
    function IndexOf(const AValue: Variant): Variant;
    function Retrieve(const AIndex: Integer; Token: TToken): Variant;
  end;

implementation

end.

