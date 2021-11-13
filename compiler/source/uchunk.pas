unit uChunk;

{$mode delphi}{$H+}
{$modeswitch typehelpers}
{$PointerMath ON}

interface

uses
  SysUtils,
  uMemory, uValue;

const
  // Opcode constants
  // no gaps between constant numbers 0..74 in order for fpc compiler to create
  // a jump table in the run function (uVM.pas). Executes ~15% faster.
  op_Constant0     = 0;
  op_Constant1     = 1;
  op_Constant2     = 2;
  op_Constant3     = 3;
  op_Constant4     = 4;
  op_Constant5     = 5;
  op_Constant      = 6;
  op_ConstantMin1  = 7;
  op_Nil           = 8;
  op_True          = 9;
  op_False         = 10;

  op_Pop           = 11;
  op_Dup           = 12;

  //Variables
  op_Get_Local     = 13;
  op_Set_Local     = 14;
  op_Get_Global    = 15;
  op_Define_Global = 16;
  op_Set_Global    = 17;
  op_Get_UpValue   = 18;
  op_Set_UpValue   = 19;
  op_Get_Property  = 20;
  op_Get_Property_Nil  = 21;
  op_Set_Property  = 22;
  op_Get_Super     = 23;

  //Equality
  op_Equal         = 24;
  op_NotEqual      = 25;
  op_Greater       = 26;
  op_GreaterEqual  = 27;
  op_Less          = 28;
  op_LessEqual     = 29;
  op_Question      = 30;
  op_2Questions    = 31; // ?? is nil coalescing operator

  //Math
  op_Add           = 32;
  op_Subtract      = 33;
  op_Multiply      = 34;
  op_Modulo        = 35;
  op_Divide        = 36;
  op_Power         = 37;
  op_Shift_Left    = 38;
  op_Shift_Right   = 39;
  op_Not           = 40;
  op_Negate        = 41;
  op_Apply         = 42; // <~ for precision
  op_Bitwise_Not   = 43;
  op_Bitwise_And   = 44;
  op_Bitwise_Or    = 45;
  op_Bitwise_XOr   = 46;

  op_Print         = 47;

  //Jumps
  op_Jump          = 48;
  op_Jump_If_False = 49;
  op_Jump_If_False_Pop = 50;
  op_Loop          = 51;

  //Functions
  op_Call          = 52;
  op_Invoke        = 53;
  op_Super_Invoke  = 54;
  op_Closure       = 55;
  op_Close_UpValue = 56;
  op_Return        = 57;

  //Class
  op_Class         = 58;
  op_Inherit       = 59;
  op_Method        = 60;
  op_Static        = 61;
  op_Field         = 62;
  op_Field_Def     = 63;
  op_InstanceOf    = 64;

  //Special types
  op_Array         = 65;
  op_Dictionary    = 66;
  op_Tuple         = 67;
  op_Set           = 68;
  op_Range         = 69;
  op_Enum          = 71;
  op_ElementOf     = 72;
  op_Index_Get     = 73; // access to array, dictionary, class indexer
  op_Index_Set     = 74;


  Operators: packed array[0..74] of String[2] = (
    '','','','','','','','','','',                 // 00 .. 09
    '','','','','','','','','','',                 // 10 .. 19
    '','','','','=','<>','>','>=','<','<=',        // 20 .. 29
    '','','+','-','*','%','/','^','<<','>>',       // 30 .. 39
    '','-','<~','!','&','|','~','','','',          // 40 .. 49
    '','','','','','','','','','',                 // 50 .. 59
    '','','','','','','','','','',                 // 60 .. 69
    '','','','','');                               // 70 .. 74

type

  PChunk = ^TChunk;

  { TChunk }

  TChunk = record
    Code: PByte;  // holds dynamic array of byte
    Count,
    Capacity: Longint;
    Constants: TValueArray;
    Lines: PInteger;
    procedure Init;
    procedure Write(const AByte: Byte; const Line: Integer); overload;
    procedure WriteConstant(const Value: TValue; const Line: Integer);
    function AddConstant(Value: TValue): UInt16;
    procedure Free;
  end;

implementation
uses uVM, uCommon;

{ TChunk }

procedure TChunk.Init;
begin
  Count := 0;
  Capacity := 0;
  Code := Nil;
  Lines := Nil;
  Constants.Init;
end;

procedure TChunk.Write(const AByte: Byte; const Line: Integer);
var
  OldCapacity: LongInt;
begin
  if Capacity < Count + 1 then
    begin
      OldCapacity := Capacity;
      Capacity := Grow_Capacity(OldCapacity);

      Code := PByte(Reallocate(Code, cByteSize*OldCapacity, cByteSize*Capacity));
      Lines := PInteger(Reallocate(Lines, cIntSize*OldCapacity, cIntSize*Capacity));
    end;

  Code[Count] := AByte;
  Lines[Count] := Line;
  Inc(Count);
end;

procedure TChunk.WriteConstant(const Value: TValue; const Line: Integer);
var
  Index: UInt16;
begin
  Index := AddConstant(Value);
  Write(op_Constant, Line); // always 2 bytes = 65535 constants
  Write(Hi(Index), Line);   // write high byte
  Write(Lo(Index), Line);   // write low byte
end;

function TChunk.AddConstant(Value: TValue): UInt16;
begin
  VM.Push(Value);
  Constants.Write(Value);
  VM.Pop;
  Result := Constants.Count - 1;
end;

procedure TChunk.Free;
begin
  Reallocate(Code, cByteSize*Capacity, 0); // Free Array
  Reallocate(Lines, cIntSize*Capacity, 0); // Free Array
  Constants.Free;
  Init;
end;

end.


