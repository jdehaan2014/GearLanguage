unit uEnumIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uAST, Variants, uMembers, Generics.Collections, uToken, uArrayIntf;

type

  IEnumable = Interface
    ['{45644E35-4C74-AB4C-25EF-BF65930F7911}']
    procedure ExtendWith(Members: TMembers);
    function isCase(Name: String): Boolean;
    function ElementList: IArrayInstance;
  end;

  IEnumInstance = Interface
    ['{B51D4D8B-73F7-CD1B-8D70-A98E1C86910D}']
    function GetMember(Ident: TIdent): Variant;
    function EnumName: String;
    function ElemName: String;
    function ElemValue: Variant;
    function ElemSetName: String;
  end;

implementation

end.

