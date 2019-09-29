unit uTupleIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, uToken;

type

  TTupleElements = specialize TList<Variant>;

  ITuple = interface
    ['{D30402C8-77E2-88D5-21A1-7CCB1CA12D9B}']
    function getElements: TTupleElements;
    property Elements: TTupleElements read getElements;
    function Get(i: Integer; Token: TToken): Variant;
    procedure Put(i: Integer; Value: Variant; Token: TToken);
    function toString: String;
  end;

implementation

end.

