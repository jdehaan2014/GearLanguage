unit uStandardFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uInterpreter, uCallable, uToken, Variants, uMemory;

type

  //function FileOpen(const FileName: UnicodeString; Mode: Integer):THandle;
  TFileOpen = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  //function FileRead(Handle: THandle; out Buffer; Count: LongInt):LongInt;
  TFileRead = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  //procedure FileClose(Handle: THandle);
  TFileClose = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  //function FileWrite(Handle: THandle; const Buffer; Count: LongInt):LongInt;
  TFileWrite = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  //function FileSeek(Handle: THandle; FOffset: LongInt; Origin: LongInt):LongInt;
  TFileSeek = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

  //function FileCreate(const FileName: UnicodeString):THandle;
  TFileCreate = class(TInterfacedObject, ICallable)
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
  end;

procedure StoreStandardFileFunctions(GlobalSpace: TMemorySpace);

implementation
uses uFunc, uArrayIntf, uArray;

{ TFileOpen }

function TFileOpen.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Name: String;
  Mode, Handle: LongInt;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Name := ArgList[0].Value;
  Mode := ArgList[1].Value;
  Handle := FileOpen(Name, Mode);
  Result := Handle;
end;

type
  TBuffer = array of Byte;

procedure ReadFile(Handle: THandle; var Buffer: TBuffer);
const
  BufSize = 1024;
  MaxGrow = 1 shl 29;
var
  BytesRead: LongInt;
  i, BufLen, BufDelta: LongInt;
begin
  BufLen := 1;
  i := 1;
  repeat
    BufDelta := BufSize * i;
    SetLength(Buffer, BufLen+BufDelta);
    BytesRead := FileRead(Handle, Buffer[BufLen], BufDelta);
    inc(BufLen, BufDelta);
    If i < MaxGrow then
      i := i shl 1;
  Until BytesRead <> BufDelta;
  SetLength(Buffer, BufLen-BufDelta+BytesRead);
end;

{ TFileRead }

function TFileRead.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Handle: LongInt;
  ArrayExpr: IArrayable;
  Instance: IArrayInstance;
  Buffer: TBuffer;
  i: LongInt;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Handle := ArgList[0].Value;
  ArrayExpr := IArrayable(Interpreter.Globals['Array']);
  Instance := IArrayInstance(TArrayInstance.Create(ArrayExpr as TArrayClass));
  ReadFile(Handle, Buffer);
  for i:=0 to Length(Buffer)-1 do
    Instance.Elements.Add(Buffer[i]);
  Result := Instance;
end;

{ TFileClose }

function TFileClose.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Handle: LongInt;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Handle := ArgList[0].Value;
  FileClose(Handle);
  Result := Null;
end;

{ TFileWrite }

function TFileWrite.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Handle: LongInt;
  Buffer: TBuffer;
  Count, i: LongInt;
  Instance: IArrayInstance;
begin
  TFunc.CheckArity(Token, ArgList.Count, 2);
  Handle := ArgList[0].Value;
  Instance := IArrayInstance(ArgList[1].Value);
  Count := Instance.Count;
  SetLength(Buffer, Count);
  for i:=0 to Count-1 do
    Buffer[i] := Byte(Instance.Elements[i]);
  Result := FileWrite(Handle, Buffer[0], Count); // num bytes are returned or -1 for error
end;

{ TFileSeek }

function TFileSeek.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Handle, Offset, Origin: LongInt;
begin
  TFunc.CheckArity(Token, ArgList.Count, 3);
  Handle := ArgList[0].Value;
  Offset := ArgList[1].Value;
  Origin := ArgList[2].Value;
  Result := FileSeek(Handle, Offset, Origin);
end;

{ TFileCreate }

function TFileCreate.Call(Token: TToken; Interpreter: TInterpreter;
  ArgList: TArgList): Variant;
var
  Handle: LongInt;
  Name: String;
begin
  TFunc.CheckArity(Token, ArgList.Count, 1);
  Name := ArgList[0].Value;
  Handle := FileCreate(Name);
  Result := Handle;
end;


procedure StoreStandardFileFunctions(GlobalSpace: TMemorySpace);
var
  Token: TToken;
begin
  Token := TToken.Create(ttIdentifier, '', Null, 0, 0);

  //open file for
  GlobalSpace.Store('forReading', $0000, Token);
  GlobalSpace.Store('forWriting', $0001, Token);
  GlobalSpace.Store('forReadingAndWriting', $0002, Token);

  //seek position from
  GlobalSpace.Store('fromBeginning', 0, Token);
  GlobalSpace.Store('fromCurrent', 1, Token);
  GlobalSpace.Store('fromEnd', 2, Token);

  GlobalSpace.Store('fileOpen', ICallable(TFileOpen.Create), Token);
  GlobalSpace.Store('fileRead', ICallable(TFileRead.Create), Token);
  GlobalSpace.Store('fileClose', ICallable(TFileClose.Create), Token);
  GlobalSpace.Store('fileWrite', ICallable(TFileWrite.Create), Token);
  GlobalSpace.Store('fileSeek', ICallable(TFileSeek.Create), Token);
  GlobalSpace.Store('fileCreate', ICallable(TFileCreate.Create), Token);
end;

end.

