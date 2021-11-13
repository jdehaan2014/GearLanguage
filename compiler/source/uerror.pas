unit uError;

{$mode objfpc}{$H+}

// Contains the definition of the error class and error list.

interface

uses
  Classes, SysUtils, uToken, fgl;

type

  TErrorItem = class
    Loc: TLocation; // an error has a location: Line, Col, File
    Msg: String;
    constructor Create(const Location: TLocation; const Message: String);
    function toString: String; override;
  end;

  TErrors = class(specialize TFPGObjectList<TErrorItem>)
    private
      FHeader: String;
    public
      constructor Create(const Header: String);
      function toString: String; override;
  end;

  EParseError = class(Exception);

procedure AddError(const Location: TLocation; const Message: String);

var
  Errors: TErrors;

implementation

{ TErrorItem }

constructor TErrorItem.Create(const Location: TLocation; const Message: String);
begin
  Loc := Location;
  Msg := Message;
end;

function TErrorItem.toString: String;
begin
  Result := '@' + Loc.toString + ': ' + Msg;
end;

{ TErrors }

constructor TErrors.Create(const Header: String);
begin
  inherited Create();
  FHeader := Header;
end;

function TErrors.toString: String;
var
  Item: TErrorItem;
begin
  Result := FHeader + LineEnding;
  for Item in Self do
    Result += Item.toString + LineEnding;
end;


procedure AddError(const Location: TLocation; const Message: String);
begin
  Errors.Add(TErrorItem.Create(Location, Message));
  if Errors.Count>30 then
    begin
      Writeln(Errors.toString);
      Halt(65);
    end;
end;


initialization
  Errors := TErrors.Create('[Errors]');

finalization
  Errors.Free;
end.


