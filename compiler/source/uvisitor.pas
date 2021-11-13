unit uVisitor;

{ This unit contains the base and parent class of the visitor. Each visitor class
  inherits from this basic TVisitor class.

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
  Classes, SysUtils, typinfo{, uValue};

{
There’s lots of documentation to find on internet on how the visitor pattern theoretically works.
It uses Visits and Accepts and is good to understand, however requires quite some work to implement.
There’s an easier way that uses the principle of reflection or realtime type information (RTTI)
in Free Pascal.

We are able to visit every tree node without adding code to the tree, and still be able to perform
actions based on the tree contents. We can print the tree, or generate a filled symbol table, or
generate code or interpret the tree directly. We just have to create a descendent from the
main visitor class that performs all actions.

Important here is to include standard Free Pascal unit ‘typinfo’ and add {$M+}
}

type

  {$M+}

  { This simple class forms the basis for our visitor system. The class has two published
    functions Visit, VisitFunc for functions and VisitProc for procedures, which have a node
    of TObject as a parameter. This only works for published methods
    Here, the return type Variant is used, however, this can be any value, e.g.

      TValueType = (vtNull, vtBool, vtInt, vtFloat, vtStr, vtChar, vtObj);
      TValue = record
        case Typ: TValueType of
          vtNull:  (Null: Pointer);
          vtBool:  (Bool: Boolean);
          vtInt:   (Int: Int64);
          vtFloat: (Float: Double);
          vtStr:   (Count: LongInt; Str: PString);
          vtChar:  (Character: Char);
          vtObj:   (Obj: Pointer);
      end;

  }

  TVisitor = class
    published
      //function VisitFunc(Node: TObject): TValue; virtual;
      function VisitFunc(Node: TObject): TObject; virtual;
      procedure VisitProc(Node: TObject); virtual;
  end;

implementation

{ We define a private (to the unit) type TVisit, which more or less is a template copy of the
  previous mentioned Visit function. Since all visitor methods are inside classes, we add
  ‘of object’.}

type
  //TVisitFunc = function(Node: TObject): TValue of object;
  TVisitFunc = function(Node: TObject): TObject of object;
  TVisitProc = procedure(Node: TObject) of object;


{ Since Node is a descendant of TObject, it’s possible to get its class name from Node.ClassName.
  We add ‘Visit’. Then, Self is the class that executes
  the visitor, e.g. TPrinter, and we have to set the reference to the class to the Data field
  of the TMethod record. So, Data points to the visiting class. Next, we have to search for the
  actual visitor code (is method address) by calling Self.MethodAddress(VisitName).
  If all goes well, variable VisitMethod now contains everything we need to execute it. We do this
  in two steps: first, cast VisitMethod to the TVisit template type and assign it to ‘doVisit’,
  followed by the actual execution ‘Result := doVisit(Node);’.
  If the respective method cannot be found, a runtime error is generated. This usually means you
  forgot to create the respective method in the visitor class.
}

// Retrieve the method record of the method to visit
function GetVisitor(constref Obj: TObject; const NameToVisit: shortstring): TMethod;
begin
  Result.Data := Obj;
  Result.Code := Obj.MethodAddress(NameToVisit);
end;

//apply function to return a result
//function TVisitor.VisitFunc(Node: TObject): TValue;
//var
//  Method: TMethod;
//  doVisit: TVisitFunc;
//begin
//  // Build visitor name: e.g. VisitTBinaryExpr from 'Visit' and TBinaryExpr
//  Method := GetVisitor(Self, 'Visit' + Node.ClassName);
//  if Assigned(Method.Code) then
//    begin
//      doVisit := TVisitFunc(Method);
//      Result := doVisit(Node);
//    end
//  else
//    Raise
//      Exception.Create(Format('No %s.Visit%s method found.', [Self.ClassName, Node.ClassName]));
//end;

//apply function to return a result
function TVisitor.VisitFunc(Node: TObject): TObject;
var
  Method: TMethod;
  doVisit: TVisitFunc;
begin
  // Build visitor name: e.g. VisitTBinaryExpr from 'Visit' and TBinaryExpr
  Method := GetVisitor(Self, 'Visit' + Node.ClassName);
  if Assigned(Method.Code) then
    begin
      doVisit := TVisitFunc(Method);
      Result := doVisit(Node);
    end
  else
    Raise
      Exception.Create(Format('No %s.Visit%s method found.', [Self.ClassName, Node.ClassName]));
end;

//apply procedure returning nothing
procedure TVisitor.VisitProc(Node: TObject);
var
  Method: TMethod;
  doVisit: TVisitProc;
begin
  // Build visitor name: e.g. VisitTBinaryExpr from 'Visit' and TBinaryExpr
  Method := GetVisitor(Self, 'Visit' + Node.ClassName);
  if Assigned(Method.Code) then
    begin
      doVisit := TVisitProc(Method);
      doVisit(Node);
    end
  else
    Raise
      Exception.Create(Format('No %s.Visit%s method found.', [Self.ClassName, Node.ClassName]));
end;

{
All visitor classes that you create from here descend from TVisitor, and the visitor methods
that you create must be published. For example:

TPrinter = class(TVisitor)
  published
    procedure VisitTBinaryExpr(BinaryExpr: TBinaryExpr);
    methods…
end;

TInterpreter = class(TVisitor)
  published
    function VisitTBinaryExpr(BinaryExpr: TBinaryExpr): TValue;
    methods…
end;

It is important to note the difference between the visitor for functions, which require a
result back from the function, and the visitor for procedures, which should not return a value.}

end.


