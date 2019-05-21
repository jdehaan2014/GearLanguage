unit uTupleIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCollections, uToken;

type

  TTupleElements = specialize TArray<Variant>;

  ITuple = interface
    ['{547A44F8-846C-09C1-958D-609414F777DC}']
    function getElements: TTupleElements;
    property Elements: TTupleElements read getElements;
    function Get(i: Integer; Token: TToken): Variant;
    procedure Put(i: Integer; Value: Variant; Token: TToken);
    function toString: String;
  end;

implementation

end.

