unit uCallable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, uInterpreter, uAST, uToken;

type
  TCallArg = class
    Value: Variant;
    Ident: TIdent;
    Token: TToken;
    constructor Create(AValue: Variant; AIdent: TIdent; AToken: TToken);
  end;

  TArgList = specialize TObjectList<TCallArg>;

  ICallable = interface
    ['{4E512927-63FA-6E3B-0312-B27878C144EB}']
    function Call(Token: TToken; Interpreter: TInterpreter; ArgList: TArgList): Variant;
    function toString: String;
  end;

  IValuable = interface(ICallable)
    ['{047FA5CB-C090-11B2-5FDB-5E3EA6245717}']
  end;

implementation

constructor TCallArg.Create(AValue: Variant; AIdent: TIdent; AToken: TToken);
begin
  Value := AValue;
  Ident := AIdent;
  Token := AToken;
end;

end.

