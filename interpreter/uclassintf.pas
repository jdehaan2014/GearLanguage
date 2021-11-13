unit uClassIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCallable, uAST, uMembers;

type

  IClassable = interface(ICallable)
    ['{1A8EC89F-5203-7091-61FA-ED5058712D86}']
    function GetStaticMember(Ident: TIdent): Variant;
    procedure ExtendWith(Members: TMembers);
    function getName: String;
    property Name: String read getName;
  end;

  IGearInstance = Interface
    ['{9DE49CA1-F5DF-4F52-244C-604A29E52659}']
    function toString: String;
    function GetMember(Ident: TIdent): Variant;
    procedure SetField(Ident: TIdent; Value: Variant);
    function isConstant(Ident: TIdent): Boolean;
    function getClassName: String;
    property ClassName: String read getClassName;
    function getMethod(const Name: String): Variant;
  end;

  ITraitable = interface
    ['{B0CE851D-2ECF-7FD8-C6CE-AF0CCA7533A9}']
    function getIdent: TIdent;
    function getMethods: TMethodTable;
    property Ident: TIdent read getIdent;
    property Methods: TMethodTable read getMethods;
  end;


implementation

end.

