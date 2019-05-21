unit uReader;

{ This unit contains the reader class that reads a file and provides characters
  to the Lexer.

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
  Classes, SysUtils, uCollections;

const
  //on Unix '^D' on windows ^Z (#26)
  //FileEnding = ^D;
  {$IFDEF UNIX}
    FileEnding = ^D;
  {$ENDIF}
  {$IFDEF WINDOWS}
    FileEnding = ^Z (#26)
  {$ENDIF}

type

  TInputType = (itPrompt, itFile);

  TReader = class
    private
      FFileName: TFileName;
      FFileIndex: Integer;
      FSource: string;
      FIndex: LongInt;
      FCount: LongInt;
      function getPeekChar: char;
    public
      property Count: LongInt read FCount;
      property Index: LongInt read FIndex;
      property FileName: TFileName read FFileName;
      property FileIndex: Integer read FFileIndex;
      property PeekChar: char read getPeekChar;
      constructor Create(Source: String; InputType: TInputType);
      function NextChar: char;
  end;

  TFileNameArray = specialize TArray<TFileName>;

var
  FileNameArray: TFileNameArray;

implementation

constructor TReader.Create(Source: String; InputType: TInputType);
var
  SourceCode: TStringList = Nil;
begin
  FFileName := '';
  FIndex := 1;
  FFileIndex := -1;
  case InputType of
    itPrompt: FSource := Source;
    itFile:
      try
        FFileName := Source;
        FFileIndex := FileNameArray.Add(FFileName);
        SourceCode := TStringList.Create;
        SourceCode.LoadFromFile(FFileName);
        FSource := SourceCode.Text;
      finally
        if Assigned(SourceCode) then SourceCode.Free;
      end;
  end;
  FCount := FSource.Length;
end;

function TReader.NextChar: char;
begin
  try
    Result := FSource[FIndex];
    Inc(FIndex);
  except
    Result := FileEnding;
  end;
end;

function TReader.getPeekChar: char;  //peek at next character, but don't process it
begin
  try
    Result := FSource[FIndex];
  except
    Result := FileEnding;
  end;
end;

initialization
  FileNameArray := TFileNameArray.Create;
finalization
  FileNameArray.Free;
end.

end.

