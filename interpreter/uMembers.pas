unit uMembers;

{ This unit contains the transport record TMembers.

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
{$modeswitch typehelpers}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, uCollections, variants;

type

  TMemberTable = specialize TDictionary<String, Variant>;
  TFieldTable = TMemberTable;
  TConstTable = TMemberTable;
  TMethodTable = TMemberTable;
  TValueTable = TMemberTable;

  TMembers = record
    Fields: TFieldTable;
    Constants: TConstTable;
    Methods: TMethodTable;
    Values: TValueTable;
    constructor Create(AFields: TFieldTable; AConstants: TConstTable;
      AMethods: TMethodTable; AValues: TValueTable);
    procedure Init;
  end;

implementation

{ TMembers }

constructor TMembers.Create(AFields: TFieldTable; AConstants: TConstTable;
  AMethods: TMethodTable; AValues: TValueTable);
begin
  Fields := AFields;
  Constants := AConstants;
  Methods := AMethods;
  Values := AValues;
end;

procedure TMembers.Init;
begin
  Self.Fields := TFieldTable.Create;
  Self.Constants := TConstTable.Create;
  Self.Methods := TMethodTable.Create;
  Self.Values := TValueTable.Create;
end;

end.

