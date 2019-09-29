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
  Classes, SysUtils, Generics.Collections;

const
  //on Unix '^D' on windows ^Z (#26)
  {$IFDEF UNIX}
    FileEnding = ^D;
  {$ENDIF}
  {$IFDEF WINDOWS}
    FileEnding = ^Z //(#26)
  {$ENDIF}

type

  TInputType = (itPrompt, itFile);

  { TReader }

  TReader = class(TStringList)
    private
      FFileName: TFileName;
      FFileIndex: Integer;
      Index: LongInt;
      function getPeekChar: char;
    public
      property FileName: TFileName read FFileName;
      property FileIndex: Integer read FFileIndex;
      property PeekChar: char read getPeekChar;
      constructor Create(Source: String; InputType: TInputType);
      function NextChar: char;
  end;

  TFileNameArray = specialize TList<TFileName>;

var
  FileNameArray: TFileNameArray;

implementation

{ TReader }

function TReader.getPeekChar: char; //peek at next character, but don't process it
begin
  try
    Result := Text[Index];
  except
    Result := FileEnding;
  end;
end;

constructor TReader.Create(Source: String; InputType: TInputType);
begin
  inherited Create;
  FFileName := '';
  FFileIndex := -1;
  Index := 1;
  case InputType of
    itPrompt: add(Source);
    itFile:
      begin
        FFileName := Source;
        FFileIndex := FileNameArray.Add(FFileName);
        LoadFromFile(FFileName);
      end;
  end;
end;

function TReader.NextChar: char;
begin
  try
    Result := Text[Index];
    Inc(Index);
  except
    Result := FileEnding;
  end;
end;

initialization
  FileNameArray := TFileNameArray.Create;
finalization
  FileNameArray.Free;

end.

