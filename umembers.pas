unit uMembers;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Generics.Collections, variants;

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
      AValues: TValueTable; AMethods: TMethodTable);
    procedure Init;
  end;

implementation

{ TMembers }

constructor TMembers.Create(AFields: TFieldTable; AConstants: TConstTable;
  AValues: TValueTable; AMethods: TMethodTable);
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

