unit uCommon;

{$mode objfpc}{$H+}
{$ModeSwitch typehelpers}

// Common constants, variables and functions
interface

uses
  SysUtils, fgl;

const
  cByteMax = Byte.MaxValue;   // 255
  cByteCount = cByteMax + 1;  // 256
  cMaxCases = 256; // max no of cases in switch statement
  cMaxExits = 256; // max no of exits in a loop exit do statement

  cIntSize = SizeOf(Integer);
  cByteSize = SizeOf(Byte);

  FileExtension = '.gear';

  GearVersion = 'version 0.3';
  Year = 2021;


type

  TFileNameList = specialize TFPGList<TFileName>;


procedure WriteFmt(const Fmt: String; const Args: array of const);
procedure WriteLnFmt(const Fmt: String; const Args: array of const);
procedure WriteFmt(var TextFile: Text; const Fmt: String; const Args: array of const);
procedure WriteLnFmt(var TextFile: Text; const Fmt: String; const Args: array of const);

// This operator overload allows to check if a class instance has an assigned value
// Instead of doing
//   if Assigned(Instance) then ....
// One can now do
//   if Instance then ...
//operator := (Obj: TObject): Boolean;


var
  // folder system
  AppFolder,
  ProductFolder,
  LibraryFolder: TFileName;
  FileNameList: TFileNameList;

implementation

//operator := (Obj: TObject): Boolean; inline;
//begin
//  Result := Assigned(Obj);
//end;

procedure WriteFmt(const Fmt: String; const Args: array of const);
begin
  Write(Format(Fmt, Args));
end;

procedure WriteLnFmt(const Fmt: String; const Args: array of const);
begin
  WriteLn(Format(Fmt, Args));
end;

procedure WriteFmt(var TextFile: Text; const Fmt: String;
  const Args: array of const);
begin
  Write(TextFile, Format(Fmt, Args));
end;

procedure WriteLnFmt(var TextFile: Text; const Fmt: String;
  const Args: array of const);
begin
  WriteLn(TextFile, Format(Fmt, Args));
end;


initialization
  FileNameList := TFileNameList.Create;
finalization
  FileNameList.Free;
end.



