unit uEnumIntf;

{ This unit contains the interfaces needed for TEnumClass and TEnumInstance.

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
  Classes, SysUtils, uAST, Variants, uMembers, uToken, uArrayIntf;

type

  IEnumable = Interface
    ['{0C787B09-3EB8-C197-D539-47B40762797D}']
    procedure ExtendWith(Members: TMembers);
    function isCase(Name: String): Boolean;
    function ElementList: IArrayInstance;
  end;

  IEnumInstance = Interface
    ['{1F7D0C58-2866-4A85-7FA2-E36590303912}']
    function GetMember(Ident: TIdent): Variant;
    function EnumName: String;
    function ElemName: String;
    function ElemValue: Variant;
    function ElemSetName: String;
  end;

implementation

end.

