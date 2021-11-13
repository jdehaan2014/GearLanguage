unit uDictIntf;

{ This unit contains the interfaces needed for TDictClass and TDictInstance.

  Copyright (C) 2019 J. de Haan jdehaan2014@gmail.com

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

  IDictionable = interface(ICallable)
    ['{F461B1D0-B563-695D-455C-A3606F6DE48F}']
    procedure ExtendWith(Members: TMembers);
  end;

  TDictElements = specialize TDictionary<Variant, Variant>;

  IDictInstance = Interface
    ['{A467BC62-AC8F-F5F2-6691-E2586512EE21}']
    function getCount: LongInt;
    function getElements: TDictElements;
    property Elements: TDictElements read getElements;
    property Count: LongInt read getCount;
    function GetMember(Ident: TIdent): Variant;
    function TypeName: String;
    function Get(Key: Variant; Token: TToken): Variant;
    procedure Put(Key: Variant; Value: Variant; Token: TToken);
    procedure Add(const AKey, AValue: Variant);
    procedure Insert(const i: Integer; const AKey, AValue: Variant; Token: TToken);
    procedure Delete(const AValue: Variant; Token: TToken);
    function Contains(const AKey: Variant): Boolean;
    function IndexOf(const AKey: Variant): Variant;
    function Retrieve(const AKey: Variant; Token: TToken): Variant;
    function Keys: Variant;
    function Values: Variant;
  end;

implementation

end.

