unit uScopes;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl;

type
  // Status of a Symbol: Variables need to be Declared or Enabled
  // Methods need to be Declared
  TStatus = (Declared, Enabled);

  // A symbol can only be used if it is enabled.
  // Only variables declared with 'var' are mutable.
  // If a variable id declared with 'let' and the initial value is nil,
  // then a one time assignment is allowed.

  TMembers = specialize TFPGMap<String, Boolean>;

  TSymbol = class
    Name: String;
    Status: TStatus;
    Mutable: Boolean;
    IsNil: Boolean;
    isPrivate: Boolean;  // field starting with underscore '_field'
    Members: TMembers;
    constructor Create(const AName: String; const AStatus: TStatus;
      const AMutable: Boolean = False);
    destructor Destroy; override;
  end;

  // A scope maintains all symbols declared in that scope. Symbols can be
  // entered into the scope and they can be looked up again.
  // A scope usually has an enclosing scope. Only the global scope doesn't have one.
  TScope = class(specialize TFPGMapObject<String, TSymbol>)
    private
      FEnclosing: TScope;
    public
      property Enclosing: TScope read FEnclosing;
      constructor Create(AEnclosing: TScope=Nil);
      procedure Enter(ASymbol: TSymbol);
      function Lookup(AName: String): TSymbol;
  end;

  // Everytime a new scope is entered, it is pushed on top of the stack.
  // The top of stack is the current scope.
  TScopes = class(specialize TFPGObjectList<TScope>)
    procedure Push(Item: TScope);
    function Pop: TScope;
    function Top: TScope;
  end;

implementation

{ TSymbol }

constructor TSymbol.Create(const AName: String; const AStatus: TStatus;
  const AMutable: Boolean);
begin
  Name := AName;
  Status := AStatus;
  Mutable := AMutable;
  isNil := False;
  isPrivate := AName[1] = '_';
  Members := TMembers.Create;
end;

destructor TSymbol.Destroy;
begin
  Members.Free;
  inherited Destroy;
end;


{ TScope }

constructor TScope.Create(AEnclosing: TScope);
begin
  inherited Create();
  Sorted := True;
  FEnclosing := AEnclosing;
end;

// Enter a symbol to the current scope
procedure TScope.Enter(ASymbol: TSymbol);
begin
  Add(ASymbol.Name, ASymbol);
end;

// First lookup the symbol in the current scope.
// If not found also lookup the enclosing scope(s) recursively.
function TScope.Lookup(AName: String): TSymbol;
var
  i: Integer;
begin
  if Find(AName, i) then
    Result := Data[i]
  else if Assigned(FEnclosing) then
    Result := FEnclosing.Lookup(AName)
  else
    Result := Nil;
end;

{ TScopes }

procedure TScopes.Push(Item: TScope);
begin
  Add(Item); // This scope becomes the top of the stack
end;

function TScopes.Pop: TScope;
begin
  Result := Extract(Last); // Throw away the top of the stack
end;

function TScopes.Top: TScope;
begin
  Result := Last; // Last is the lastly added scope and thus the top of the stack
end;


end.


