unit uStandardCRT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uInterpreter, uCallable, uToken, Variants, uMemory;

type

  TClrScr = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TWindow = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  TGotoXY = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;


procedure StoreStandardCRTFunctions(GlobalSpace: TMemorySpace);


implementation
uses uFunc, CRT;

{ TWindow }

function TWindow.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  x1,y1,x2,y2: Byte;
begin
  TFunc.CheckArity(Token, ArgList.Count, 4);
  x1 := Byte(ArgList[0].Value);
  y1 := Byte(ArgList[1].Value);
  x2 := Byte(ArgList[2].Value);
  y2 := Byte(ArgList[3].Value);

  Result := Null;
  Window(x1,y1,x2,y2);
end;

{ TClrScr }

function TClrScr.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
begin
  TFunc.CheckArity(Token, ArgList.Count, 0);
  Result := Null;
  ClrScr;
end;

{ TGotoXY }

function TGotoXY.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  x,y: Byte;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  x := Byte(ArgList[0].Value);
  y := Byte(ArgList[1].Value);

  Result := Null;
  GotoXY(x,y);
end;



procedure StoreStandardCRTFunctions(GlobalSpace: TMemorySpace);
var
  Token: TToken;
begin
  Token := TToken.Create(ttIdentifier, '', Null, 0, 0);
  GlobalSpace.Store('window', ICallable(TWindow.Create), Token);
  GlobalSpace.Store('gotoXY', ICallable(TGotoXY.Create), Token);
  GlobalSpace.Store('clrScr', ICallable(TClrScr.Create), Token);
end;

end.

