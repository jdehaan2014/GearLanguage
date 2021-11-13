unit uDebug;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, uChunk, uValue;

procedure DisassembleChunk(var DebugFile: Text; Chunk: PChunk; const Name: String);
function DisassembleInstruction(var DebugFile: Text; Chunk: PChunk; Offset: Longint): Longint;

implementation
uses uCommon, uObject;

procedure DisassembleChunk(var DebugFile: Text; Chunk: PChunk; const Name: String);
var
  Offset: Longint = 0;
begin
  WriteLnFmt(DebugFile, '== %s ==', [Name]);
  while Offset < Chunk^.Count do
    Offset := DisassembleInstruction(DebugFile, Chunk, Offset);
  WriteLnFmt(DebugFile, '== End %s ==' + LineEnding, [Name]);
  Flush(DebugFile);
end;

function SimpleInstruction(var DebugFile: Text; const Name: String; Offset: Longint): Longint;
begin
  WriteLnFmt(DebugFile, '%s', [Name]);
  Result := Offset+1;
end;

function ConstantInstruction(var DebugFile: Text; const Name: String; Chunk: PChunk; Offset: Longint): Longint;
var
  Constant: UInt16;
begin
  Constant := Chunk^.Code[Offset+2] or
              (Chunk^.Code[Offset+1] shl 8);
  WriteFmt(DebugFile, '%-16s %4d "', [Name, Constant]);
  WriteLn(DebugFile, Chunk^.Constants.Values[Constant].toString, '"');
  Result := Offset+3;
end;

function InvokeInstruction(var DebugFile: Text; const Name: String; Chunk: PChunk; Offset: Longint): Longint;
var
  Constant, ArgCount: Byte;
begin
  Constant := Chunk^.Code[Offset+1];
  ArgCount := Chunk^.Code[Offset+2];
  WriteFmt(DebugFile, '%-5s (%d args) %2d "', [Name, ArgCount, Constant]);
  WriteLn(DebugFile, Chunk^.Constants.Values[Constant].toString, '"');
  Result := Offset + 3;
end;

function ByteInstruction(var DebugFile: Text; const Name: String; Chunk: PChunk; Offset: Longint): Longint;
var
  Slot: UInt8;
begin
  Slot := Chunk^.Code[Offset+1];
  WriteLnFmt(DebugFile, '%-16s %4d', [Name, Slot]);
  Result := Offset + 2;
end;

function JumpInstruction(var DebugFile: Text; const Name: String; Sign: Integer; Chunk: PChunk;
  Offset: Longint): Integer;
var
  Jump: UInt16;
begin
  Jump := UInt16(Chunk^.Code[Offset+1] shl 8);
  Jump := Jump or Chunk^.Code[Offset+2];
  WriteLnFmt(DebugFile, '%-16s %4d -> %d', [Name, Offset, Offset+3 + Sign * Jump]);
  Result := Offset + 3;
end;

function DisassembleInstruction(var DebugFile: Text; Chunk: PChunk; Offset: Longint): Longint;
var
  Instruction: Byte;
  Constant, isLocal, Index: Byte;
  Func: PObjFunc;
  j: Integer;
begin
  WriteFmt(DebugFile, '%.4d ', [Offset]);
  if (Offset > 0) and (Chunk^.Lines[Offset] = Chunk^.Lines[Offset-1]) then
    Write(DebugFile, '   | ')
  else
    Write(DebugFile, Format('%4d ', [Chunk^.Lines[Offset]]));

  Instruction := Chunk^.Code[Offset];
  case Instruction of
    op_Constant0..op_Constant5:
      Exit(SimpleInstruction(DebugFile, 'CONST'+IntToStr(Instruction), Offset));
    //op_Constant0:    Exit(SimpleInstruction(DebugFile, 'CONST0', Offset));
    //op_Constant1:    Exit(SimpleInstruction(DebugFile, 'CONST1', Offset));
    //op_Constant2:    Exit(SimpleInstruction(DebugFile, 'CONST2', Offset));
    //op_Constant3:    Exit(SimpleInstruction(DebugFile, 'CONST3', Offset));
    op_Constant:     Exit(ConstantInstruction(DebugFile, 'CONSTANT', Chunk, Offset));
    op_ConstantMin1: Exit(SimpleInstruction(DebugFile, 'CONST_MIN1', Offset));
    op_Nil:          Exit(SimpleInstruction(DebugFile, 'NIL', Offset));
    op_True:         Exit(SimpleInstruction(DebugFile, 'TRUE', Offset));
    op_False:        Exit(SimpleInstruction(DebugFile, 'FALSE', Offset));
    op_Pop:          Exit(SimpleInstruction(DebugFile, 'POP', Offset));
    op_Dup:          Exit(SimpleInstruction(DebugFile, 'DUP', Offset));
    op_Get_Local:    Exit(ByteInstruction(DebugFile, 'GET_LOCAL', Chunk, Offset));
    op_Set_Local:    Exit(ByteInstruction(DebugFile, 'SET_LOCAL', Chunk, Offset));
    op_Get_Global:   Exit(ConstantInstruction(DebugFile, 'GET_GLOBAL', Chunk, Offset));
    op_Define_Global: Exit(ConstantInstruction(DebugFile, 'DEFINE_GLOBAL', Chunk, Offset));
    op_Set_Global:   Exit(ConstantInstruction(DebugFile, 'SET_GLOBAL', Chunk, Offset));
    op_Get_UpValue:  Exit(ByteInstruction(DebugFile, 'GET_UPVALUE', Chunk, Offset));
    op_Set_UpValue:  Exit(ByteInstruction(DebugFile, 'SET_UPVALUE', Chunk, Offset));
    op_Get_Property: Exit(ConstantInstruction(DebugFile, 'GET_PROPERTY', Chunk, Offset));
    op_Get_Property_Nil: Exit(ConstantInstruction(DebugFile, 'GET_PROPERTY_NIL', Chunk, Offset));
    op_Set_Property: Exit(ConstantInstruction(DebugFile, 'SET_PROPERTY', Chunk, Offset));
    op_Get_Super:    Exit(ConstantInstruction(DebugFile, 'GET_SUPER', Chunk, Offset));
    op_Equal:        Exit(SimpleInstruction(DebugFile, 'EQUAL', Offset));
    op_NotEqual:     Exit(SimpleInstruction(DebugFile, 'NOT_EQUAL', Offset));
    op_Greater:      Exit(SimpleInstruction(DebugFile, 'GREATER_THAN', Offset));
    op_GreaterEqual: Exit(SimpleInstruction(DebugFile, 'GREATER_EQUAL', Offset));
    op_Less:         Exit(SimpleInstruction(DebugFile, 'LESS_THAN', Offset));
    op_LessEqual:    Exit(SimpleInstruction(DebugFile, 'LESS_EQUAL', Offset));
    op_Question:     Exit(SimpleInstruction(DebugFile, 'QUESTION', Offset));
    op_2Questions:   Exit(SimpleInstruction(DebugFile, '2QUESTIONS', Offset));
    op_Add:          Exit(SimpleInstruction(DebugFile, 'ADD', Offset));
    op_Subtract:     Exit(SimpleInstruction(DebugFile, 'SUBTRACT', Offset));
    op_Multiply:     Exit(SimpleInstruction(DebugFile, 'MULTIPLY', Offset));
    op_Modulo:       Exit(SimpleInstruction(DebugFile, 'MODULO', Offset));
    op_Divide:       Exit(SimpleInstruction(DebugFile, 'DIVIDE', Offset));
    op_Power:        Exit(SimpleInstruction(DebugFile, 'POWER', Offset));
    op_Shift_Left:   Exit(SimpleInstruction(DebugFile, 'SHIFT_LEFT', Offset));
    op_Shift_Right:  Exit(SimpleInstruction(DebugFile, 'SHIFT_RIGHT', Offset));
    op_Not:          Exit(SimpleInstruction(DebugFile, 'NOT', Offset));
    op_Negate:       Exit(SimpleInstruction(DebugFile, 'NEGATE', Offset));
    op_Apply:        Exit(SimpleInstruction(DebugFile, 'APPLY', Offset));
    op_Bitwise_Not:  Exit(SimpleInstruction(DebugFile, 'BITWISE_NOT', Offset));
    op_Bitwise_And:  Exit(SimpleInstruction(DebugFile, 'BITWISE_AND', Offset));
    op_Bitwise_Or:   Exit(SimpleInstruction(DebugFile, 'BITWISE_OR', Offset));
    op_Bitwise_XOr:  Exit(SimpleInstruction(DebugFile, 'BITWISE_XOR', Offset));
    op_Print:        Exit(SimpleInstruction(DebugFile, 'PRINT', Offset));
    op_Jump:         Exit(JumpInstruction(DebugFile, 'JUMP', 1, Chunk, Offset));
    op_Jump_If_False: Exit(JumpInstruction(DebugFile, 'JUMP_IF_FALSE', 1, Chunk, Offset));
    op_Jump_If_False_Pop: Exit(JumpInstruction(DebugFile, 'JUMP_IF_FALSE_POP', 1, Chunk, Offset));
    op_Loop:          Exit(JumpInstruction(DebugFile, 'LOOP', -1, Chunk, Offset));
    op_Call:          Exit(ByteInstruction(DebugFile, 'CALL', Chunk, Offset));
    op_Invoke:        Exit(InvokeInstruction(DebugFile, 'INVOKE', Chunk, Offset));
    op_Super_Invoke:  Exit(InvokeInstruction(DebugFile, 'SUPER_INVOKE', Chunk, Offset));
    op_Closure:
      begin
        Inc(Offset);
        Constant := Chunk^.Code[Offset+1] or
                    (Chunk^.Code[Offset] shl 8);

        Inc(Offset,2);
        WriteFmt(DebugFile, '%-16s %4d ', ['CLOSURE', Constant]);
        WriteLn(DebugFile, Chunk^.Constants.Values[Constant].toString);

        Func := Chunk^.Constants.Values[Constant].asFunc;
        for j := 0 to Func^.UpValueCount-1 do
          begin
            isLocal := Chunk^.Code[Offset];
            Inc(Offset);
            Index := Chunk^.Code[Offset];
            Inc(Offset);
            WriteLnFmt(DebugFile, '%.4d    |                     %s %d',
              [Offset-2, specialize IfThen<String>(isLocal=1, 'local', 'upvalue'), Index]);
          end;

        Result := Offset;
      end;
    op_Close_UpValue: Exit(SimpleInstruction(DebugFile, 'CLOSE_UPVALUE', Offset));
    op_Return:        Exit(SimpleInstruction(DebugFile, 'RETURN', Offset));
    op_Class:         Exit(ConstantInstruction(DebugFile, 'CLASS', Chunk, Offset));
    op_Inherit:       Exit(SimpleInstruction(DebugFile, 'INHERIT', Offset));
    op_Method:        Exit(ConstantInstruction(DebugFile, 'METHOD', Chunk, Offset));
    op_Static:        Exit(ConstantInstruction(DebugFile, 'STATIC', Chunk, Offset));
    op_Field:         Exit(ConstantInstruction(DebugFile, 'FIELD', Chunk, Offset));
    op_Field_Def:     Exit(ConstantInstruction(DebugFile, 'FIELD_DEF', Chunk, Offset));
    op_InstanceOf:    Exit(SimpleInstruction(DebugFile, 'INSTANCE_OF', Offset));
    op_Array:         Exit(ByteInstruction(DebugFile, 'ARRAY', Chunk, Offset));
    op_Dictionary:    Exit(ByteInstruction(DebugFile, 'DICTIONARY', Chunk, Offset));
    op_Tuple:         Exit(ByteInstruction(DebugFile, 'TUPLE', Chunk, Offset));
    op_Set:           Exit(ByteInstruction(DebugFile, 'SET', Chunk, Offset));
    op_Range:         Exit(SimpleInstruction(DebugFile, 'RANGE', Offset));
    op_Enum:          Exit(ConstantInstruction(DebugFile, 'ENUM', Chunk, Offset));
    op_ElementOf:     Exit(SimpleInstruction(DebugFile, 'ELEMENT_OF', Offset));
    op_Index_Get:     Exit(SimpleInstruction(DebugFile, 'INDEX_GET', Offset));
    op_Index_Set:     Exit(SimpleInstruction(DebugFile, 'INDEX_SET', Offset));
    otherwise
      WriteLnFmt(DebugFile, 'Unknown opcode %d', [Instruction]);
      Exit(Offset+1);
  end;
end;


end.


