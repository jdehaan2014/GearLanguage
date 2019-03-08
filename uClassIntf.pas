unit uClassIntf;

{ This unit contains the IClassable interface, needed for class calls.

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
  Classes, SysUtils, uCallable, uAST, uMembers;

type

  IClassable = interface(ICallable)
    ['{230E841B-D57B-F5F8-4DD3-224B74645A17}']
    procedure ExtendWith(Members: TMembers);
    function getName: String;
    property Name: String read getName;
  end;

  IGearInstance = Interface
    ['{6E9625B1-8DAD-EC3B-57E5-80D360CC7B67}']
    function toString: String;
    function GetMember(Ident: TIdent): Variant;
    procedure SetField(Ident: TIdent; Value: Variant);
    function isConstant(Ident: TIdent): Boolean;
    function getClassName: String;
    property ClassName: String read getClassName;
  end;

  ITraitable = interface
    ['{4CEE3AA1-2D73-E3C0-DA25-6C6E52BCD1D1}']
    function getIdent: TIdent;
    function getMethods: TMethodTable;
    function getValues: TValueTable;
    property Ident: TIdent read getIdent;
    property Methods: TMethodTable read getMethods;
    property Values: TValueTable read getValues;
  end;

implementation

end.

