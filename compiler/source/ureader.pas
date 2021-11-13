unit uReader;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, uCommon;

const
  //on Unix '^D' on windows ^Z (#26)
  {$IFDEF UNIX}
    FileEnding = ^D;
  {$ENDIF}
  {$IFDEF WINDOWS}
    FileEnding = ^Z //(#26)
  {$ENDIF}

type

  // the Reader reads a source text file, and upon request of the Lexer returns
  // characters to be processed by the Lexer. Every time a character is requested,
  // the index is increased by 1. This continues until an end-of-file character is found.
  // The filename is stored in an array for location data in case of errors.
  TReader = record
    private
      FFileName: TFileName;
      FFileIndex: Integer;
      FText: String;
      FIndex, FSize: Integer;
      function ReadFile: String;
    public
      property Text: String read FText;     // the source text
      property Index: Integer read FIndex;  // current index of the source text
      property FileName: TFileName read FFileName;
      property FileIndex: Integer read FFileIndex; // index in file array list
      constructor Create(Source: String; isFile: Boolean = True);
      procedure Free;
      function NextChar: Char;  // get next character and increase index
      function PeekChar: Char;  // peek next character, do not update index
      function PeekChar(const OffSet: Integer): Char;
  end;

  EFileOpenException = class(Exception);
  EFileReadException = class(Exception);

implementation

{ TReader }

function TReader.ReadFile: String;
var
  InputFile: THandle;
  FileSize, BytesRead: Integer;
begin
  try
    InputFile := FileOpen(FileName, fmOpenRead); // open file for reading

    if InputFile = -1 then
      raise EFileOpenException.CreateFmt('Error opening file "%s".', [FileName]);

    FileSize := FileSeek(InputFile, 0, fsFromEnd); // get filesize
    SetLength(Result, FileSize); // initialize source text string
    FileSeek(InputFile, 0, fsFromBeginning); // reset file pointer to 0

    // read file and save contents in result string; number of bytes read is returned.
    BytesRead := FileRead(InputFile, Result[1], FileSize);
    if BytesRead < FileSize then
      raise EFileReadException.CreateFmt('Error reading file "%s".', [FileName]);

  finally
    FileClose(InputFile);  // always close the file
  end;
end;

// source can be a file or a REPL input
constructor TReader.Create(Source: String; isFile: Boolean);
begin
  if isFile then
    begin
      FFileName := Source;
      FText := ReadFile;
      FFileIndex := FileNameList.Add(ExtractFileName(FFileName));
    end
  else
    begin
      FFileName := '';
      FText := Source + FileEnding;
      FFileIndex := -1;
    end;
  FIndex := 1;
  FSize := Length(FText);
end;

procedure TReader.Free;
begin
  SetLength(FText, 0);
end;

// return next character of the source string; update index
function TReader.NextChar: Char;
begin
  if FIndex < FSize then
    begin
      Result := FText[FIndex];
      Inc(FIndex);                // move to next char
    end
  else
    Result := FileEnding;
end;


// return next character of the source string; do not update index
function TReader.PeekChar: Char;
begin
  if FIndex <= FSize then
    Result := FText[FIndex] // read next char, don't move index
  else
    Result := FileEnding;
end;

// return a character of the source string at a certain offset
function TReader.PeekChar(const OffSet: Integer): Char;
begin
  if FIndex + OffSet <= FSize then
    Result := FText[FIndex + OffSet]
  else
    Result := FileEnding;
end;

end.


