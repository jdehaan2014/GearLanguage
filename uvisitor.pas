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
  Classes, SysUtils, typinfo;

type

  {$M+}

  TVisitor = class
    published
      function VisitFunc(Node: TObject): Variant; virtual;
      procedure VisitProc(Node: TObject); virtual;
  end;

implementation

type
  TVisitFunc = function(Node: TObject): Variant of object;
  TVisitProc = procedure(Node: TObject) of object;

function TVisitor.VisitFunc(Node: TObject): Variant;
var
  VisitName: shortstring;
  VisitMethod: TMethod;
  doVisit: TVisitFunc;
  SelfName: string = '';
begin
  // Build visitor name: e.g. VisitBinaryExpr from 'Visit' and TBinaryExpr
  VisitName := 'Visit' + Copy(Node.ClassName, 2, 255);  // remove 'T'
  SelfName := Self.ClassName;
  VisitMethod.Data := Self;
  VisitMethod.Code := Self.MethodAddress(VisitName);
  if Assigned(VisitMethod.Code) then begin
    doVisit := TVisitFunc(VisitMethod);
    Result := doVisit(Node);
  end
  else
    Raise
      Exception.Create(Format('No %s.%s method found.', [SelfName, VisitName]));
end;

procedure TVisitor.VisitProc(Node: TObject);
var
  VisitName: shortstring;
  VisitMethod: TMethod;
  doVisit: TVisitProc;
  SelfName: string = '';
begin
  // Build visitor name: e.g. VisitBinaryExpr from 'Visit' and TBinaryExpr
  VisitName := 'Visit' + Copy(Node.ClassName, 2, 255);  // remove 'T'
  SelfName := Self.ClassName;
  VisitMethod.Data := Self;
  VisitMethod.Code := Self.MethodAddress(VisitName);
  if Assigned(VisitMethod.Code) then begin
    doVisit := TVisitProc(VisitMethod);
    doVisit(Node);
  end
  else
    Raise
      Exception.Create(Format('No %s.%s method found.', [SelfName, VisitName]));
end;

end.


