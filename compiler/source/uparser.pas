unit uParser;

{$mode objfpc}{$H+}
{$Interfaces Corba}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, uLexer, uBaseParser, uToken, Generics.Collections, uAST, uCommon;

type

  TParser = class;

  { This is a Pratt parser, whereby expressions are parsed conform Pratt's principle. In unit
    uParselets, the expression types are parsed in so-called parselet classes. These classes
    use either prefix or infix operators. The respective parselets are stored in two dictionaries,
    one for prefix parselets and one for infix parselets.
    Procedure ParseExpr is the main expression parser. A parselet is found by looking up a
    tokentype in the corresponding dictionary.
    Also, check for more info:
    http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
  }

  { An InfixParselet is associated with a token that appears in the middle of the
    expression it parses. Its parse() method will be called after the left-hand
    side has been parsed, and it in turn is responsible for parsing everything
    that comes after the token. This is also used for postfix expressions, in
    which case it simply doesn't consume any more tokens in its parse() call.}

  IInfixParselet = interface
    ['IInfixParselet']
    function Parse(Parser: TParser; Left: TExpr; Token: TToken): TExpr;
    function getPrecedence: Integer;
  end;

  { A PrefixParselet is associated with a token that appears at the beginning of an
    expression. Its parse() method will be called with the consumed leading token,
    and the parselet is responsible for parsing anything that comes after that token.
    This interface is also used for single-token expressions like variables, in
    which case parse() simply doesn't consume any more tokens. }

  IPrefixParselet = interface
    ['IPrefixParselet']
    function Parse(Parser: TParser; Token: TToken): TExpr;
  end;

  // the dictionary of infix parselets with token type as the key
  TInfixParselets = specialize TDictionary<TTokenTyp, IInfixParselet>;
  // the dictionary of prefix parselets with token type as the key
  TPrefixParselets = specialize TDictionary<TTokenTyp, IPrefixParselet>;

  // determines the kind of method in a core class
  TMethodKind = (mkMethod, mkProperty, mkOperator);

  // Main parser class that inherits from the base parser
  TParser = class(TBaseParser)
    private
      InfixParselets: TInfixParselets;
      PrefixParselets: TPrefixParselets;
    public
      SelfUsed: Boolean;
      InsideClassDecl: Boolean;
      constructor Create(Lexer: TLexer); // create parser and copy tokens from the Lexer
      destructor Destroy; override;
      procedure RegisterParselet(Typ: TTokenTyp; Parselet: IPrefixParselet);
      procedure RegisterParselet(Typ: TTokenTyp; Parselet: IInfixParselet);

      function ParseExpr(Precedence: Integer): TExpr; // main expression parser
      function ParseExpr: TExpr;
      function getPrecedence: Integer;     // get operator precedence
      // parse list of comma separated variable identifiers
      function ParseVarList: TExprList;
      // parse list of comma separated expressions until a given tokentype is reached
      function ParseExprListUntil(const EndTokenTyp: TTokenTyp): TExprList;
      // main product parse function
      function Parse: TProduct;
      // TProduct is the AST node, containing the main program
      function ParseProduct: TProduct;
      // parse a single AST node
      function ParseNode(const TokenTyp: TTokenTyp): TNode;
      // parse a block of statements and/or declarations
      function ParseBlock: TBlock;
      // parse the function's return statement
      function ParseReturnStmt: TStmt;
    private
      // Declarations
      function Declaration: TDecl;
      // parse a variable declaration, ident := expr
      function ParseVarDecl(const Mutable: Boolean): TDecl;
      // parse ident, ident, ident, ... := Expr
      function ParseVariableDecl(const Mutable: Boolean): TVarDecls;
      // parse a list of variable declarations
      function ParseVarDecls: TDecl;
      // parse a value property declarations, var identList := Expr, identList := Expr
      function ParseValDecl(const inClass: Boolean=False): TDecl;
      // parse a class or record declarations
      function ParseClassDecl(const isClass: Boolean = True): TDecl;
      // parse a trait declarations
      function ParseTraitDecl: TDecl;
      // parse an extension to a type declarations
      function ParseExtensionDecl: TDecl;
      // parse an enum declarations
      function ParseEnumDecl: TDecl;

      // function declarations
    public
      // parse the list of formal parameters
      function ParameterList(out Signature: String): TParameterList;
      function ParseBody: TBody;  // parse the function body
    private
      // define core classes
      ArrayDecl, DictionaryDecl, SetDecl, RangeDecl,
      ArrayIteratorDecl, RangeIteratorDecl, DictionaryIteratorDecl, SetIteratorDecl: TClassDecl;
      // define listBuilder function
      ListBuilder: TFuncDecl;
      // counter for keeping track of defer statements, to create unique function names
      DeferCount: Integer;
      // parse the function details
      function ParseFuncDetails(Name: TIdent; Form: TFuncForm): TFuncDecl;
      function ParseFuncDecl: TDecl;
      function ParseMethodDecl: TDecl;
      // parse an operator overload declaration
      function ParseOperatorDecl: TDecl;
      // parse the getter and setter used in class indexers
      function ParseGetterDecl(IndexVar: TVariable): TFuncDecl;
      function ParseSetterDecl(IndexVar: TVariable; newValue: TVariable): TFuncDecl;

      // Statements
      function Statement(const NewLine: Boolean = True): TStmt;
      function ParsePrintStmt: TStmt;
      function ParseAssignStmt: TStmt;
      function ParseIfStmt: TStmt;
      function ParseWhileStmt: TStmt;
      function ParseDoStmt: TStmt;
      function ParseLoopDoStmt: TStmt;
      function ParseForStmt: TStmt;
      function ParseForInStmt: TStmt;
      function ParseEnsureStmt: TStmt;
      function ParseSwitchStmt: TStmt;
      function ParseBreakStmt: TStmt;
      function ParseContinueStmt: TStmt;
      function ParseUseStmt: TStmt;
      function ParseDeferStmt: TStmt;

      // helper functions
      function withMethod(Func, Typ: String; Vars: array of String; const useAltName,
        needsSignature: Boolean; const Kind: TMethodKind; const OpType: String=''): TDecl;
      procedure BindClass(const NameClass: String; MethodDecl: TDecl);
      procedure InitializeSystem;
  end;

implementation
uses uReader;

{ TParser }

constructor TParser.Create(Lexer: TLexer);
begin
  inherited Create(Lexer.Tokens);  // get the tokens from the Lexer

  InfixParselets := TInfixParselets.Create;
  PrefixParselets := TPrefixParselets.Create;
  // for every defer stmt a func is created with a unique name based on the counter
  DeferCount := 0;
  SelfUsed := False;
  InsideClassDecl := False;
end;

destructor TParser.Destroy;
begin
  InfixParselets.Free;
  PrefixParselets.Free;
  inherited Destroy;
end;

// register is add a new prefix parselet
procedure TParser.RegisterParselet(Typ: TTokenTyp; Parselet: IPrefixParselet);
begin
  PrefixParselets.Add(Typ, Parselet);
end;

// register is add a new infix parselet
procedure TParser.RegisterParselet(Typ: TTokenTyp; Parselet: IInfixParselet);
begin
  InfixParselets.Add(Typ, Parselet);
end;

{ ParseExpr() starts at the current token and parses any expression at the
  given precedence level or higher. See unit uPrecedence for the precedence levels.
  Read the next token and look up the corresponding prefix Parselet. If there is no
  prefix parselet, then the token must be a syntax error. We report that and return
  to the caller.
  The prefix parser parses the rest of the prefix expression, consuming any other
  tokens it needs, and returns back the result.
  Next, look for an infix parselet for the next token. If the next token is
  too low precedence, or isn’t an infix operator at all, we’re done.
  Otherwise, we consume the operator and hand off control to the infix parselet.
  Loop back around and see if the next token is also a valid infix operator
  that can take the entire preceding expression as its operand.
  Loop until the token isn't an infix operator or has too low precedence.
  More info at: http://craftinginterpreters.com/compiling-expressions.html#a-pratt-parser
}
function TParser.ParseExpr(Precedence: Integer): TExpr;
var
  Token: TToken;
  Prefix: IPrefixParselet;
  Infix: IInfixParselet;
begin
  Token := Consume;

  if not PrefixParselets.TryGetValue(Token.Typ, Prefix) then
    ErrorFmt('Unable to parse "%s" in expression.', [Token.Lexeme]);

  Result := Prefix.Parse(Self, Token);  // parse left hand side

  while Precedence < getPrecedence do
    begin
      IgnoreNewlines;
      Token := Consume;
      Infix := InfixParselets[Token.Typ];
      Result := Infix.Parse(Self, Result, Token);   // parse right hand side
    end;
end;

function TParser.ParseExpr: TExpr;
begin
  Result := ParseExpr(0);
end;

// returns the precedence of an infix parselet
function TParser.getPrecedence: Integer;
var
  Parselet: IInfixParselet;
begin
  if InfixParselets.TryGetValue(Current.Typ, Parselet) then
    Result := Parselet.getPrecedence
  else
    Result := 0;
end;

// parse a comma separated list of variable identifiers
function TParser.ParseVarList: TExprList;
begin
  Result := TExprList.Create();
  repeat
    IgnoreNewlines;
    Result.Add(TVariable.Create(ParseIdent));
    if Result.Count > 255 then
      Error('Cannot have more that 255 items.');
  until not Match(ttComma);
end;

// parse a comma separated list of expressions, until given end token
function TParser.ParseExprListUntil(const EndTokenTyp: TTokenTyp): TExprList;
begin
  Result := TExprList.Create();
  if Current.Typ <> EndTokenTyp then
    repeat
      IgnoreNewlines;
      Result.Add(ParseExpr);
      if Result.Count > 255 then
        Error('Cannot have more that 255 items.');
    until not Match(ttComma);
  //IgnoreNewlines;
  Expect(EndTokenTyp, SynchronizeSet);
end;

// main parse function, returning the complete AST
function TParser.Parse: TProduct;
begin
  IgnoreNewlines;
  try
    Result := ParseProduct;
  except
    on E: Exception do
      Error(E.Message);
  end;
end;

// parse the product/program
function TParser.ParseProduct: TProduct;
var
  Token: TToken;
begin
  // initialize core classes with their methods
  InitializeSystem;

  Token := Current;
  // Create the product node with the parsed block nodes
  Result := TProduct.Create(ParseBlock.Nodes, Token.Location);
  // insert them as an array at the start of the node list of the product/program
  Result.Nodes.InsertRange(0,
    [ListBuilder, ArrayDecl, DictionaryDecl, SetDecl, RangeDecl,
     RangeIteratorDecl, ArrayIteratorDecl, DictionaryIteratorDecl, SetIteratorDecl]);
end;

// parse a single node, ebnf:
// Node = ( Statement | Declaration ) .
function TParser.ParseNode(const TokenTyp: TTokenTyp): TNode;
begin
  IgnoreNewlines;
  try
    // a node can be a declaration
    if TokenTyp in DeclStartSet then
      Result := Declaration
    // or it can be a statement
    else //if TokenTyp in StmtStartSet then
      Result := Statement
    //else
    //  Error('Unrecognized declaration or statement.');
  except
    Synchronize(SynchronizeSet);
    Result := TNode.Create(Current.Location);
  end;
end;

// a block consists of statement and/or declaration nodes
// the nodes are parsed until we hit a block-end token, such as 'end', 'else', 'case',...
// Block = { Node } .
function TParser.ParseBlock: TBlock;
var
  Token: TToken;
begin
  Token := Current;
  Result := TBlock.Create(TNodeList.Create(), Token.Location);
  while not (Token.Typ in BlockEndSet) do
    begin
      Result.Nodes.Add(ParseNode(Token.Typ));
      Token := Current;
    end;
end;

// a return statement of a function must contain an expression. ebnf:
// ReturnStmt = 'return' Expr .
function TParser.ParseReturnStmt: TStmt;
var
  Token: TToken;
begin
  Token := Consume; // return
  if Current.Typ = ttLine then
    // it's a return without expression
    Result := TReturnStmt.Create(Nil, Token.Location)
  else
    // it's a return with expression
    Result := TReturnStmt.Create(ParseExpr, Token.Location);
end;


//
// DECLARATIONS
//

// Declaration = ClassDecl | EnumDecl | ExtensionDecl | FuncDecl | TraitDecl
//               | RecordDecl | ValDecl | VarDecl .
function TParser.Declaration: TDecl;
begin
  case Current.Typ of
    ttClass: Result := ParseClassDecl(True);
    ttEnum: Result := ParseEnumDecl;
    ttExtension: Result := ParseExtensionDecl;
    ttFunc: Result := ParseFuncDecl;
    ttRecord: Result := ParseClassDecl(False);
    ttTrait: Result := ParseTraitDecl;
    ttVal: Result := ParseValDecl;
    ttLet, ttVar: Result := ParseVarDecls;
  end;
  ExpectLine('Expect newline after declaration.');
end;


// VarDecl = Name ':=' Expr .
function TParser.ParseVarDecl(const Mutable: Boolean): TDecl;
var
  Name: TIdent;
  Expr: TExpr;
begin
  IgnoreNewlines;
  Name := ParseIdent;                // Variable name
  Expect(ttAssign, SynchronizeSet);  // :=
  IgnoreNewlines;
  Expr := ParseExpr;
  Result := TVarDecl.Create(Name, Expr, Mutable);
end;

// VariableDecl = Ident { ',' Ident } ':=' Expr.
function TParser.ParseVariableDecl(const Mutable: Boolean): TVarDecls;
var
  VariableList: TExprList;
  VarDecls: TVarDecls;
  Expr, Variable: TExpr;
begin
  VarDecls := TVarDecls.Create(TDeclList.Create(), Current.Location);
  VariableList := ParseVarList;
  Expect(ttAssign, SynchronizeSet);  // :=
  IgnoreNewlines;
  Expr := ParseExpr;
  for Variable in VariableList do
    VarDecls.List.Add(TVarDecl.Create(TVariable(Variable).Name, Expr, Mutable));

  Result := VarDecls;
end;

// VarDecls = ['var' | 'let'] VarsDecl { ',' VarsDecl } .
function TParser.ParseVarDecls: TDecl;
var
  VarDecls: TVarDecls;
  isMutable: Boolean;
begin
  VarDecls := TVarDecls.Create(TDeclList.Create(), Current.Location);
  isMutable := Current.Typ = ttVar; // if ttLet then constant and not mutable
  Next; // skip var, let
  repeat
    VarDecls.List.AddRange(ParseVariableDecl(isMutable).List);
  until not Match(ttComma);
  Result := VarDecls;
end;

// ValDecl = 'val' Name ( ':=' Expr | 'do' Body 'end' )
// the Body must contain a return statement
function TParser.ParseValDecl(const inClass: Boolean): TDecl;
var
  Name: TIdent;
  FuncDecl: TFuncDecl;
  Body: TBody;
  FuncForm: TFuncForm = ffVal;
begin
  Next; // skip val
  Name := ParseIdent;
  if Current.Typ = ttAssign then // val identifier := expr
    begin
      IgnoreNewlines;
      Body := TBody.Create(TNodeList.Create(), Current.Location); // empty body
      Body.Nodes.Add(ParseReturnStmt);  // add a single return statemen
    end
  else                          // val identifier do code return expr end
    begin
      IgnoreNewlines;
      Expect(ttDo);
      IgnoreNewlines;
      Body := ParseBody;  // parse complete body
      IgnoreNewlines;
      Expect(ttEnd);
    end;
  if inClass then FuncForm := ffMethodVal; // are we inside a class decl?
  FuncDecl := TFuncDecl.Create(Name, TParameterList.Create(), FuncForm, Body);
  Result := TValDecl.Create(Name, FuncDecl);
end;

// ebnf of class:
// ClassDecl = 'class' Ident [ '<' Parent ] [':' Traits] 'is' {Member} 'end' .
// Parent = Ident .
// Member = FuncDecl | ValDecl | VarDecl | Indexer | DefaultDecl .
// Traits = Ident { ',' Ident } .
// A record is the same as a class but without parent or super class
function TParser.ParseClassDecl(const isClass: Boolean): TDecl;
var
  Name: TIdent;                     // name of the class
  Parent: TVariable=Nil;            // parent variable identifier, default = nil
  Members, Statics: TMemberMap;     // the class (static) members
  TokenTyp: TTokenTyp;
  ClassDecl: TClassDecl;
  Traits: TExprList=Nil;            // list of traits, if any
  Decl: TDecl;
  isMutable: Boolean;               // mutability of variable declarations
  DefaultVar: TVarDecl=Nil;         // the default variable of a class
  DefaultDefined: Boolean=False;    // set to true if a default variable is defined
  newValue: TVariable=Nil;          // the new value in an indexer setter
  IndexVar: TVariable;              // the index variable of an indexer
  IndexerDefined: Boolean=False;    // set to true if an indexer is defined
begin
  InsideClassDecl := True;
  Next; // skip class or record
  Name := ParseIdent;  // parse class name
  if Match(ttLT) then  // is there a parent class inheritance?
    begin
      if not isClass then // then it must be a record
        Error('A record cannot have a parent/super class.');
      Parent := TVariable.Create(ParseIdent);
      if Name.Text = Parent.Name.Text then
        Error('A class cannot inherit from itself.');
    end;

  // check is there are traits listed
  if Match(ttColon) then
    Traits := ParseVarList; // parse the list of traits

  Expect(ttIs, 'Expected "is" after class header. ');
  ExpectLine('Expect newLine after "is".');

  Members := TMemberMap.Create;   // initialize members
  Statics := TMemberMap.Create;   // initialize static members: only functions allowed
  TokenTyp := Current.Typ;
  while not (TokenTyp in [ttEnd, ttEOF]) do
    begin
      case TokenTyp of
        ttDefault:         // parse a default variable
          begin
            if DefaultDefined then  // it can only appear once
              Error('Duplicate default variable not allowed.', // synchonize with
                [ttEnd, ttFunc, {ttIndexer,} ttLet, ttStatic, ttSubscript, ttVal, ttVar])
            else begin
              Next; // skip default
              DefaultVar := ParseVarDecl(True) as TVarDecl; // always mutable
              DefaultDefined := True;
            end;
          end;
        //ttIndexer:
        ttSubscript:
          begin
            if IndexerDefined then  // it can only appear once
              Error('Duplicate subscript not allowed.',
                [ttDefault, ttEnd, ttFunc, ttLet, ttStatic, ttVal, ttVar])
            else begin
              Next;  // skip subscript
              Expect(ttLeftBracket, 'Expected "[" after subscript.');
              IndexVar := TVariable.Create(ParseIdent); // parse index variable
              Expect(ttRightBracket, 'Expected "]" after index variable.');
              if Match(ttLeftParen) then  // is there a setter value?
                begin
                  newValue := TVariable.Create(ParseIdent); // parse setter variable
                  Expect(ttRightParen);
                end;
              IgnoreNewlines;
              Expect(ttFor, 'Expected "for" before getter.');
              // create name (dot makes it unique) and parse getter declaration
              Members.Add('indexer.getter', ParseGetterDecl(IndexVar));
              if Assigned(newValue) then
                begin
                  IgnoreNewlines;
                  Expect(ttComma, 'Expected "," after getter.');
                  IgnoreNewlines;
                  // create unique name and parse setter declaration
                  Members.Add('indexer.setter', ParseSetterDecl(IndexVar, newValue));
                end;
              IndexerDefined := True;
            end;
          end;
        ttVal:  // parse value property
          begin
            Decl := ParseValDecl(True);
            Members.Add(Decl.Name.Text, Decl);
          end;
        ttLet, ttVar: // parse constant or variable field
          begin
            isMutable := TokenTyp = ttVar;
            Next; // skip var, let
            repeat // parse comma separated list of declarations
              Decl := ParseVarDecl(isMutable);
              Members.Add(Decl.Name.Text, Decl);
            until not Match(ttComma);
          end;
        ttStatic:  // parse static member: only func allowed
          begin
            Next; // skip static
            Expect(ttFunc, 'Expected "func" after "static".');
            Decl := ParseFuncDetails(ParseIdent, ffFunc);
            Statics.AddOrSetValue(Decl.Name.Text, Decl);
          end;
        otherwise // it's a method or init
          Decl := ParseMethodDecl;
          Members.Add(Decl.Name.Text, Decl);
      end; {case}
      ExpectLine('Expect newline after definition in class.');
      Tokentyp := Current.Typ;
    end; {while}

  Expect(ttEnd, 'Expect "end" after class body.');

  ClassDecl := TClassDecl.Create(Name, Parent, Traits, DefaultVar, Members, Statics);
  ClassDecl.IsRecord := isClass = False; // true if not a class
  Result := ClassDecl;

  ClassMap.Add(Name.Text, ClassDecl); // store class/record in class list
  InsideClassDecl := False;
end;

// A trait contains a number of functions that can be used by a class/record to
// extend functionality. A trait is not a type!
// Traits can contain other traits. Functions cannot be overwritten in traits.
// A collision results in an error. The ebnf of traits:
// TraitDecl = 'trait' Ident [ ':' Traits ] 'is' {Member} 'end' .
// Traits = Ident { ',' Ident } .
function TParser.ParseTraitDecl: TDecl;
var
  Name: TIdent;                  // trait name
  TraitDecl: TTraitDecl;
  Traits: TExprList=Nil;         // list of traits
  Members: TMemberMap;
  TokenTyp: TTokenTyp;
  Decl: TDecl;
begin
  Next; // skip trait
  Name := ParseIdent;            // parse trait name

  if Match(ttColon) then         // check if this trait includes other traits
    Traits := ParseVarList;      // parse list of traits

  Expect(ttIs, 'Expected "is" after trait header. ');
  ExpectLine('Expect newLine after "is".');

  // only functions are allowed
  Members := TMemberMap.Create();
  TokenTyp := Current.Typ;
  while not (TokenTyp in [ttEnd, ttEOF]) do
    begin
      Expect(ttFunc);    // only functions are allowed in traits
      Decl := ParseFuncDetails(ParseIdent, ffMethod); // trait functions always methods
      Members.AddOrSetValue(Decl.Name.Text, Decl); // overwrite existing func names
      ExpectLine('Expect newline after definition in trait.');
      TokenTyp := Current.Typ;
    end;

  Expect(ttEnd);

  TraitDecl := TTraitDecl.Create(Name, Traits, Members);
  Result := TraitDecl;
  TraitMap.add(Name.Text, TraitDecl); // add to global trait map
end;

// With an extension the functionality of classes and enums can be extended.
// Only functions and value properties can be added to an extension.
// Also, extensions are the place to add operator overloads.
// The ebnf of extensionm:
// ExtensionDecl = 'extension' Ident 'is' {Member} 'end' .
// Member = FuncDecl | ValDecl | OperatorDecl
function TParser.ParseExtensionDecl: TDecl;
var
  Name: TIdent;
  Members: TMemberMap;
  TokenTyp: TTokenTyp;
  Decl: TDecl;
  Traits: TExprList=Nil;
begin
  Next; // skip extension
  Name := ParseIdent; // name of extension (must match existing type)

  // check is there are traits listed
  if Match(ttColon) then
    Traits := ParseVarList; // parse the list of traits

  Expect(ttIs, 'Expected "is" after extension header.');
  ExpectLine('Expect newLine after "is".');

  Members := TMemberMap.Create();
  TokenTyp := Current.Typ;
  while not (TokenTyp in [ttEnd, ttEOF]) do
    begin
      if TokenTyp = ttVal then          // parse value property
        Decl := ParseValDecl(True)
      else if TokenTyp in [ttInfix, ttPrefix] then // parse operator
        Decl := ParseOperatorDecl
      else                               // parse method
        Decl := ParseMethodDecl;

      Members.AddOrSetValue(Decl.Name.Text, Decl);
      ExpectLine('Expect newline after definition in extension.');
      Tokentyp := Current.Typ;
    end;

  Result := TExtensionDecl.Create(Name, Members, Traits);
  Expect(ttEnd, 'Expected "end" after extension declaration.');
end;

// The ebnf of an enum declaration is:
// EnumDecl = 'enum' EnumElements {Member} 'end' .
// The elements are comma separated, and may be succeeded by '=' Expr
// EnumElements = EnumElement { ',' EnumElement } .
// EnumElement = Ident [ '=' Expr ]
// An enum declaration may contain functions and value properties.
function TParser.ParseEnumDecl: TDecl;
var
  EnumDecl: TEnumDecl;
  TokenTyp: TTokenTyp;
  Decl: TDecl;
  Name: TIdent;    // name of an enum element
  Value: TExpr;    // value of an enum element
begin
  Next;  // skip enum
  // declare enum and parse name; initialize members
  EnumDecl := TEnumDecl.Create(ParseIdent, TMemberMap.Create());
  Expect(ttIs, 'Expected "is" after enum header.');
  ExpectLine('Expect newLine after "is".');

  repeat
    IgnoreNewlines;
    Name := ParseIdent;  // enum element name
    if EnumDecl.Elements.ContainsKey(Name.Text) then   // duplicates not allowed
      ErrorFmt('Duplicate enum name "%s".', [Name.Text]);
    if Match(ttEQ) then   // check for optional value
      Value := ParseExpr  // parse value
    else
      Value := TLiteralExpr.Create(ltNil, Current.Location); // else nil
    EnumDecl.AddElement(Name.Text, Value);
  until not Match(ttComma);
  ExpectLine('Expect newLine after last enum item.');

  // other declarations: functions and value properties are allowed
  TokenTyp := Current.Typ;
  while not (TokenTyp in [ttEnd, ttEOF]) do
    begin
      IgnoreNewlines;
      if TokenTyp = ttVal then
        Decl := ParseValDecl(True)   // parse value property
      else
        Decl := ParseMethodDecl;     // parse method

      EnumDecl.Members.AddOrSetValue(Decl.Name.Text, Decl);
      ExpectLine('Expect newline after definition in enum.');
      Tokentyp := Current.Typ;
    end;

  Expect(ttEnd);
  Result := EnumDecl;

  ClassMap.Add(EnumDecl.Name.Text, EnumDecl); // store enum in class list
end;


// Parameters = '(' [ Parameter { ',' Parameter } ] ')' .
// Parameter = [ ( ExternalIdent | '.' ) ] Ident .
// a parameter can have an external identifier. This identifier must be used used
// when a func is called with the respective matching argument. Examples:
// func add(a, to b) do return a+b end // here 'to:' required in the call:
// var a := add(7, to: 9)
// func sub(.a, .b) do return a-b end // here both 'a:' and 'b:' required:
// var b := sub(a:9, b:8)
// The signature of a function is its name + named parameters:, separated by #
// Different signatures makes function overloading possible.
function TParser.ParameterList(out Signature: String): TParameterList;
var
  Name: TIdent;
begin
  IgnoreNewlines; // allow newLine before open paren
  Signature := '';
  Result := TParameterList.Create();
  Expect(ttLeftParen);
  if Current.Typ <> ttRightParen then
    repeat
      IgnoreNewlines; // allow newLine before each parameter
      case Current.Typ of
        ttDot:
          begin
            Next;
            Name := ParseIdent;
            Result.Add(TParameter.Create(TVariable.Create(Name), Name));
          end;
        ttIdentifier:
          begin
            Name := ParseIdent;
            if Current.Typ = ttIdentifier then
              Result.Add(TParameter.Create(TVariable.Create(ParseIdent), Name))
            else
              Result.Add(TParameter.Create(TVariable.Create(Name), Nil));
          end;
        otherwise
          Error('Invalid parameter syntax.');
      end;
      if Result.Count > 255 then
        Error('Cannot have more than 255 parameters.');
      if Assigned(TParameter(Result.Last).Name) then
        Signature += '#' + Name.Text + ':';
    until not Match(ttComma);
  IgnoreNewlines; // allow neLine before closing paren
  Expect(ttRightParen);
end;

// parse the function body, between the 'do' and 'end'.
function TParser.ParseBody: TBody;
var
  Token: TToken;
begin
  Token := Current;
  Result := TBody.Create(ParseBlock.Nodes, Token.Location);
end;

// FuncDetails = Parameters ( '=>' Expr | 'for' SelfList | 'do' Body 'end' ) .
// a function can have an arrow => + expression if only a single expression returned
// an init function can have a for clause to simplify initialization of fields
// Otherwise it contains a normal body with do ... end.
function TParser.ParseFuncDetails(Name: TIdent; Form: TFuncForm): TFuncDecl;
var
  Parameters: TParameterList;
  Body: TBody;
  Left, Right: TExpr;
  i: Integer = 0;
  Signature: String='';
begin
  // Parse the parameter list
  Parameters := ParameterList(Signature);
  if Form in [ffFunc, ffMethod] then
    Name.Text := Name.Text + Signature;

  if Match(ttArrow) then    // func(parameters) => Expr
    begin
      if Form = ffInit then
        Error('Cannot return a value from an initializer.');
      IgnoreNewlines;
      Body := TBody.Create(TNodeList.Create(), Current.Location);
      Body.Nodes.Add(TReturnStmt.Create(ParseExpr, Body.Location));
      Form := specialize IfThen<TFuncForm>(Form = ffMethod, ffMethodArrow, ffFuncArrow);
    end
  else if Match(ttFor) then  // init(param1, param2) for self.param1, self.param2
    begin
      if Form <> ffInit then
        Error('Can use "for" only from an init().');
      Body := TBody.Create(TNodeList.Create(), Current.Location);
      repeat
        IgnoreNewlines;
        Left := ParseExpr;
        Right := Parameters[i].Variable;
        Inc(i);
        // it must either be self.variable or variable (automatic expansion with self)
        if (Left is TGetExpr) and ((Left as TGetExpr).Instance is TSelfExpr) then
          Body.Nodes.Add(TSetStmt.Create(Left as TGetExpr, ttAssign, Right))
        else if Left is TVariable then // automatically expand to self.variable
          Body.Nodes.Add(TSetStmt.Create(
            TGetExpr.Create(TSelfExpr.Create(TIdent.Create('self', Current.Location)), Left),
            ttAssign, Right))
        else
          Error('Only "self.property" allowed.')
      until not Match(ttComma);
    end
  else  // The body
    begin
      Expect(ttDo, 'Expect "do" before function body.');
      ExpectLine('Expect newLine after "do".');
      Body := ParseBody;
      Expect(ttEnd, 'Expect "end" after function body.');
    end;
  Result := TFuncDecl.Create(Name, Parameters, Form, Body);
end;

// FuncDecl = 'func' Name FuncDetails
function TParser.ParseFuncDecl: TDecl;
var
  Name: TIdent;
begin
  Next; // skip func
  Name := ParseIdent;
  Result := ParseFuncDetails(Name, ffFunc);
end;

// starts with 'func' or 'init'
function TParser.ParseMethodDecl: TDecl;
var
  FuncForm: TFuncForm = ffMethod;
  Name: TIdent;
begin
  if Current.Lexeme = 'init' then
    FuncForm := ffInit
  else
    Expect(ttFunc);

  Name := ParseIdent;

  Result := ParseFuncDetails(Name, FuncForm);
end;

// an operator can be either infix for binary expr or prefix for unary expr,
// and operator overloads can only appear in extension declarations. ebnf:
// OperatorDecl = ('prefix' | 'infix') Operator '(' Parameters ')' Body .
// Body = ( '=>' Expr | 'do' {Node} 'end' ) .
function TParser.ParseOperatorDecl: TDecl;
var
  xFixToken: TToken;             // ttInfix or ttPrefix
  OpType: TOperatorType;         // otInfix or otPrefix
  Op, Name: TIdent;              // the used operator, and new name of operator func
  Parameters: TParameterList;
  Body: TBody;
  FuncForm: TFuncForm=ffMethod;
begin
  xFixToken := Consume;
  IgnoreNewlines;
  OpType := specialize IfThen<TOperatorType>(xFixToken.Typ = ttInfix, otInfix, otPrefix);
  Op := ParseOperator;
  Name := TIdent.Create(xFixToken.Typ.toString+'_'+Op.Text, Op.Location); // e.g. infix_+
  IgnoreNewlines;

  // parse parameters
  Parameters := TParameterList.Create();
  Expect(ttLeftParen);
  IgnoreNewlines;
  if OpType = otInfix then // max 1 parameter allowed
    Parameters.Add(TParameter.Create(TVariable.Create(ParseIdent), Nil));
  IgnoreNewlines;
  Expect(ttRightParen);

  // parse body, check if arrow function
  if Match(ttArrow) then
    begin
      Body := TBody.Create(TNodeList.Create(), Current.Location);
      IgnoreNewlines;
      Body.Nodes.Add(TReturnStmt.Create(ParseExpr, Body.Location));
      FuncForm := ffMethodArrow;
    end
  else
    begin
      Expect(ttDo, 'Expect "do" before operator body.');
      IgnoreNewlines;
      Body := ParseBody;
      IgnoreNewlines;
      Expect(ttEnd, 'Expect "end" after operator body.');
    end;

  Result := TOperatorDecl.Create(Name, OpType, Parameters, Body, FuncForm);
end;

// parse the getter used in class indexers
function TParser.ParseGetterDecl(IndexVar: TVariable): TFuncDecl;
var
  Expr: TExpr;
  Body: TBody;
  Name: TIdent;
  Params: TParameterList;
begin
  IgnoreNewlines;
  Expr := ParseExpr;
  // create the body of the getter function
  Body := TBody.Create(TNodeList.Create(), Current.Location);
  // create "return expr" and add to getter body
  Body.Nodes.Add(TReturnStmt.Create(Expr, Body.Location));
  // create parameter "indexVar" for the getter function: indexer.getter(indexVar)
  Params := TParameterList.Create();
  Params.Add(TParameter.Create(IndexVar, Nil));
  //Create getter function
  Name := MakeID('indexer.getter'); // name of getter function
  Result := TFuncDecl.Create(Name, Params, ffMethodArrow, Body);
end;

// parse the setter used in class indexers
function TParser.ParseSetterDecl(IndexVar: TVariable; newValue: TVariable): TFuncDecl;
var
  Assignment: TStmt;
  Name: TIdent;
  Params: TParameterList;
  Body: TBody;
begin
  IgnoreNewlines;
  Assignment := ParseAssignStmt;
  // create parameter "index" and "value" for the setter function:
  // indexer.setter(index, value)
  Params := TParameterList.Create();
  Params.Add(TParameter.Create(IndexVar, Nil));
  Params.Add(TParameter.Create(newValue, Nil));
  // create the body of the setter function
  Body := TBody.Create(TNodeList.Create(), Current.Location);
  // add the set statement "self.items[i] := newValue"
  Body.Nodes.Add(Assignment);
  //Create setter function
  Name := MakeID('indexer.setter');  // name of setter function
  Result := TFuncDecl.Create(Name, Params, ffMethod, Body);
end;

function TParser.Statement(const NewLine: Boolean): TStmt;
begin
  case Current.Typ of
    ttBreak: Result := ParseBreakStmt;
    ttContinue: Result := ParseContinueStmt;
    ttDefer: Result := ParseDeferStmt;
    ttDo: Result := ParseDoStmt;
    ttEnsure: Result := ParseEnsureStmt;
    ttFor: Result := ParseForStmt;
    ttIf: Result := ParseIfStmt;
    ttLoop: Result := ParseLoopDoStmt;
    ttPrint: Result := ParsePrintStmt;
    ttReturn: Result := ParseReturnStmt;
    ttSwitch: Result := ParseSwitchStmt;
    ttUse: Result := ParseUseStmt;
    ttWhile: Result := ParseWhileStmt;
    otherwise
      Result := ParseAssignStmt;
  end;
  if NewLine then
    ExpectLine('Expect newline after statement.');
end;

function TParser.ParsePrintStmt: TStmt;
var
  Token: TToken;
  ExprList: TExprList;
  Terminator: TExpr;
  SawTerminator: Boolean = False;

  procedure ParseItem;
  var
    PrintPretty: TIdent;
  begin
    if (Current.Typ = ttIdentifier) and (Peek.Typ = ttColon) then
      begin
        PrintPretty := ParseIdent;
        if PrintPretty.Text = 'terminator' then
          begin
            if SawTerminator then
              Error(PrintPretty.Location, 'Duplicate terminator attribute.');
            SawTerminator := True;
            Next; // skip :
            IgnoreNewlines;
            Terminator := ParseExpr;
          end
        else
          begin
            ErrorFmt('Unexpected attribute "%s:".', [PrintPretty.Text]);
            Next;
            Terminator := ParseExpr;
          end;
      end
    else
      ExprList.Add(ParseExpr);
  end;

begin
  Token := Consume; // print
  Terminator := TCharExpr.Create(LineEnding, Token.Location);
  //Terminator := TStringExpr.Create(LineEnding, Token.Location);
  ExprList := TExprList.Create();
  IgnoreNewlines;
  Expect(ttLeftParen);
  if Current.Typ <> ttRightParen then
    repeat
      IgnoreNewlines;
      ParseItem;
    until not Match(ttComma);
  IgnoreNewlines;
  Expect(ttRightParen);
  Result := TPrintStmt.Create(ExprList, Terminator, Token.Location);
end;

// An assignment statement can have the following possibilities:
//  variable AssignOp value, whereby AssignOp can be :=, +=, -=, *=, /=, %=
//  instance.field AssignOp value
//  array[index] AssignOp value
//  call(arguments)
//  inherited method(arguments)
function TParser.ParseAssignStmt: TStmt;
var
  AssignOp: TTokenTyp;
  Left, Right: TExpr;
begin
  Left := ParseExpr;
  if Current.Typ in AssignSet then
    begin
      AssignOp := Consume.Typ;
      IgnoreNewlines;
      Right := ParseExpr;
      if Left is TVariable then
        Result := TAssignStmt.Create(Left as TVariable, Right, AssignOp)
      else if Left is TGetExpr then
        Result := TSetStmt.Create(Left as TGetExpr, AssignOp, Right)
      else if Left is TIndexedExpr then
        Result := TIndexedStmt.Create(Left as TIndexedExpr, AssignOp, Right);
    end
  else if Left is TCallExpr then
    Result := TCallExprStmt.Create(Left as TCallExpr)
  else if Left is TInheritedExpr then
    Result := TExprStmt.Create(Left)
  else
    Error(Left.Location, 'Expected assignment operator.');
end;

// IfStmt = 'if' [VarDecl 'where'] Condition 'then' Block [{ElseIf}] ['else' Block] .
// ElseIf = 'elseif' Condition 'then' Block .
function TParser.ParseIfStmt: TStmt;
var
  VarDecl: TVarDecl = Nil;
  Condition, ElseIfCondition: TExpr;
  ThenPart: TBlock;
  ElsePart: TBlock = Nil;
  ElseIfList: TElseIfList = Nil;
  Token, VarToken: TToken;
begin
  Token := Consume; // if
  // is there a variable declaration
  if Current.Typ in [ttLet, ttVar] then
    begin
      VarToken := Consume; // skip var or let
      VarDecl := ParseVarDecl(VarToken.Typ = ttVar) as TVarDecl;
      Expect(ttWhere, 'Expect "where" after "variable declaration".');
    end;

  Condition := ParseExpr;   // parse boolean expression

  Expect(ttThen, 'Expect "then" after if condition.');
  ExpectLine('Expect newLine after "then".');
  ThenPart := ParseBlock;
  IgnoreNewlines;
  // check for elseif parts and store any in the list of elseifs
  if Current.Typ = ttElseIf then
    begin
      ElseIfList := TElseIfList.Create();
      repeat
        Next;  // skip elseif
        ElseIfCondition := ParseExpr;
        Expect(ttThen, 'Expect "then" after elseif condition.');
        ExpectLine('Expect newLine after "then".');
        ElseIfList.Add(TElseIfItem.Create(ElseIfCondition, ParseBlock));
      until Current.Typ <> ttElseIf;
    end;
  IgnoreNewlines;

  if Match(ttElse) then  // check for final 'else' block
    begin
      IgnoreNewlines;
      ElsePart := ParseBlock;
    end;

  IgnoreNewlines;
  Expect(ttEnd, 'Expect "end" after if statement.');

  Result := TIfStmt.Create(VarDecl, Condition, ThenPart, ElsePart,
    ElseIfList, Token.Location);
end;

// WhileStmt = 'while' ['var' VarDecl 'where'] Condition 'do' Block 'end' .
function TParser.ParseWhileStmt: TStmt;
var
  VarDecl: TVarDecl = Nil;
  Condition: TExpr;
  Token: TToken;
  Block: TBlock;
begin
  Token := Consume; // while
  // is there a variable declaration
  if Match(ttVar) then
    begin
      VarDecl := ParseVarDecl(True) as TVarDecl;
      Expect(ttWhere, 'Expect "where" after "variable declaration".');
    end;

  Condition := ParseExpr; // parse boolean expression

  Expect(ttDo, 'Expect "do" after while loop condition.');
  ExpectLine('Expect newLine after "do".');
  Block := ParseBlock;
  Expect(ttEnd);

  Result := TWhileStmt.Create(VarDecl, Condition, Block, Token.Location);
end;

// DoStmt = 'do' ( ['(' | '[' | '{'] Expr [')' | ']' | '}']
//               | statements 'end' 'while' Expr ) .
// The DoStmt has 2 faces. Firstly, it is used to create expression statements of
// the form 'do' (Expr) or 'do' [Expr] or 'do' {Expr}. The other form is the
// well-known 'do .. end while' statement.
function TParser.ParseDoStmt: TStmt;
var
  Condition: TExpr;
  Token: TToken;
  Block: TBlock;
begin
  Token := Consume; // do
  if Current.Typ in [ttLeftParen, ttLeftBrace, ttLeftBracket] then
    Result := TExprStmt.Create(ParseExpr)
  else
    begin
      Block := ParseBlock;
      Expect(ttEnd);
      Expect(ttWhile);
      Condition := ParseExpr; // parse boolean expression
      Result := TRepeatStmt.Create(Block, Condition, Token.Location);
    end;
end;

// The LoopDoStmt is an experimental loop statement, which may contain multiple exits.
// LoopDoStmt = 'loop' [VarDecl] 'do' Block ExitBlock {',' ExitBlock} 'end' .
// ExitBlock = 'exit when' Condition Block .
// A minimum of 1 exit block is required.
function TParser.ParseLoopDoStmt: TStmt;
var
  Token: TToken;
  VarDecls: TVarDecls=Nil;
  Block: TBlock;
  ConditionalBlocks: TConditionalBlocks;
  Condition: TExpr;
begin
  Token := Consume;  // loop
  IgnoreNewlines;
  if Current.Typ in [ttLet, ttVar] then     // is there a VarDecl?
    VarDecls := ParseVarDecls as TVarDecls;
  IgnoreNewlines;
  Expect(ttDo);
  ExpectLine;
  Block := ParseBlock; // 1st mandatory block
  IgnoreNewlines;
  ConditionalBlocks := TConditionalBlocks.Create(); // initialize exitblocks
  repeat
    Expect(ttExit, 'Expected "exit" after statements.');
    Expect(ttWhen, 'Expected "when" after exit.');
    Condition := ParseExpr; // parse boolean condition, and if true then exit loop
    ExpectLine;
    ConditionalBlocks.Add(TConditionalBlock.Create(Condition, ParseBlock));
    IgnoreNewlines;
  until Current.Typ <> ttExit;
  Expect(ttEnd);
  Result := TLoopDoStmt.Create(VarDecls, Block, ConditionalBlocks, Token.Location);
end;

// ForStmt = 'for' 'var' VarDecl 'where' Condition, Iterator 'do' Block' 'end' .
// ForStmt = 'for' 'var' Ident 'in' Expr ['where' Condition] 'do' Block' 'end' .
function TParser.ParseForStmt: TStmt;
var
  VarDecl: TVarDecl;
  Condition: TExpr;
  Iterator: TStmt;
  Token: TToken;
  Block: TBlock;
begin
  Token := Consume;  // for
  Expect(ttVar);
  if Peek.Typ = ttIn then
    Exit(ParseForInStmt);

  VarDecl := ParseVarDecl(True) as TVarDecl;
  Expect(ttWhere, 'Expect "where" after "variable declaration".');

  Condition := ParseExpr; // parse boolean expression
  Expect(ttComma, 'Expect "," after loop condition.');

  Iterator := ParseAssignStmt;

  Expect(ttDo, 'Expect "do" after for loop conditions.');
  ExpectLine('Expect newLine after "do".');
  Block := ParseBlock;
  Expect(ttEnd);

  Result := TForStmt.Create(VarDecl, Condition, Iterator, Block, Token.Location);
end;

{
  ForInStmt = 'for' 'var' Ident 'in' IterableExpr ['where' Condition] 'do' Block' 'end' .

  Only works on iterable types. An iterable type must implement the following:
  - an iterator with the following properties:
    - moveNext, a boolean value property that advances and checks if there are more
    - current, a value property that returns the current item

  extension ListType is
    val iterator := ListTypeIterator(self)
  end

  record ListTypeIterator is
    init(ListType)...
    val moveNext...
    val current...
  end

  for item in list do
    print(item)
  end

  is translated in:

  while var iterator := list.iterator where iterator.moveNext do
    var item := iterator.current
    print(item) // statement using item
  end

  for item in list where condition do
    print(item)
  end

  is translated in:

  while var iterator := list.iterator where iterator.moveNext do
    if var item := iterator.current where condition then
      print(item) // statement using item
    end
  end
}
function TParser.ParseForInStmt: TStmt;
var
  LoopVar: TIdent;
  SequenceExpr: TExpr;
  WhereExpr: TExpr = Nil;
  IfStmt: TIfStmt;
  Body: TBody;
begin
  LoopVar := ParseIdent;  // parse the for-loop variable
  Expect(ttIn);
  SequenceExpr := ParseExpr;  // parse sequence type expression

  if Match(ttWhere) then
    WhereExpr := ParseExpr; // parse where expression if available

  Expect(ttDo, 'Expect "do" after for-in expression.');
  ExpectLine('Expect newLine after "do".');

  Body := ParseBody;
  Expect(ttEnd);

  Result := TForInStmt.Create(LoopVar, SequenceExpr, WhereExpr, Body, LoopVar.Location);
end;

{ Many languages have some form of assertion or guarding that certain conditions
  are met. If a condition is not met, usually an error is generated, or at least an
  early escape from a function is possible. We want to ensure that a certain condition
  is met with the ensure statement. The general form of the ensure statement is:
  ensure Boolean_expression else
    statements
  end

  It’s also possible to declare a variable in the ensure statement.
  ensure let variable := value where Boolean_expression else
    statements
  end

  ebnf:
  EnsureStmt = 'ensure' [('var'|'let') VarDecl 'where'] Condition 'else' Block 'end' .
}
function TParser.ParseEnsureStmt: TStmt;
var
  VarDecl: TVarDecl = Nil;
  Condition: TExpr;
  ElsePart: TBlock;
  Token, VarToken: TToken;
begin
  Token := Consume; // ensure
  IgnoreNewlines;
  if Current.Typ in [ttLet, ttVar] then
    begin
      VarToken := Consume;
      VarDecl := ParseVarDecl(VarToken.Typ = ttVar) as TVarDecl;
      Expect(ttWhere, 'Expect "where" after "variable declaration".');
    end;
  IgnoreNewlines;
  Condition := ParseExpr; // parse boolean expression
  IgnoreNewlines;
  Expect(ttElse, 'Expect "else" after ensure condition.');
  IgnoreNewlines;
  ElsePart := ParseBlock;
  Expect(ttEnd);

  Result := TEnsureStmt.Create(VarDecl, Condition, ElsePart, Token.Location);
end;

{
  A switch statement can have multiple cases and requires a final default: clause.
  The general for of the Gear switch statement is:
  switch expression
    case [‘in’ | ‘is’ | '.'] value [, value]: statement(s)
    case [‘in’ | ‘is’ | '.'] value [, value]: statement(s)
    default: statement(s)
  end

  Note that after a case clause is executed, the switch statement is finished
  automatically. There is no ‘break’ needed, nor can you fall through to the next case.
  This is the standard usage we have see in many other languages, whether it is
  called ‘switch’ or ‘case of’. What is not so common, is that it supports further
  pattern matching, e.g. checking if a class instance belongs to a certain class
  type by using ‘is’ right after ‘case’.
  You can check whether a value is in a certain range, part of an array or part
  of a set. You can even combine those in one switch statement.
}
function TParser.ParseSwitchStmt: TStmt;
var
  Token: TToken;
  Expr, CaseValue: TExpr;
  ItemTyp: TCaseItemType = citNormal;
  Cases: TSwitchCases;
  DefaultCase: TBlock;
  CaseValues: TExprList;
  CaseBlock: TBlock;
begin
  Token := Consume; // switch
  Expr := ParseExpr;
  ExpectLine('Expect newLine after switch expression.');
  Cases := TSwitchCases.Create();
  Expect(ttCase, 'Expect "case" after switch condition.'); // minimum 1 case required
  repeat
    if Match(ttIs) then
      ItemTyp := citObject        // is class type
    else if Match(ttDot) then
      ItemTyp := citEnum          // = enum type
    else if Match(ttIn) then
      ItemTyp := citElemOf;       // in set, array or range
    CaseValues := ParseExprListUntil(ttColon);
    IgnoreNewlines;
    CaseBlock := ParseBlock;
    for CaseValue in CaseValues do // each case value has its own entry in the dictionary 'cases'
      Cases.Add(TCaseItem.Create(CaseValue, ItemTyp), CaseBlock);
    ItemTyp := citNormal;
  until not Match(ttCase);
  Expect(ttDefault, 'Expect "default" after final case.');
  Expect(ttColon, 'Expect ":" after default.');
  IgnoreNewlines;
  DefaultCase := ParseBlock;
  IgnoreNewlines;
  Expect(ttEnd, 'Expect "end" after default case.');
  Result := TSwitchStmt.Create(Expr, Cases, DefaultCase, Token.Location);
end;

//function TParser.ParseBreakStmt: TStmt;
//var
//  Token: TToken;
//  Condition: TExpr=Nil;
//begin
//  Token := Consume;  // skip break
//  if Match(ttIf) then
//    Condition := ParseExpr;
//  Result := TBreakStmt.Create(Condition, Token.Location);
//end;

// break, which effect is to terminate the current loop immediately, and transfer
// control to the statement immediately after that loop.
function TParser.ParseBreakStmt: TStmt;
begin
  Result := TBreakStmt.Create(Current.Location);
  Next;
end;

//  Continue is used in order to stop the current iteration and continue with the
//  next one. Contrary to break, where you leave the iteration, with continue you
//  perform the next iteration.
function TParser.ParseContinueStmt: TStmt;
begin
  Result := TContinueStmt.Create(Current.Location);
  Next;
end;

// The file to use is scanned/lexed and its tokens are inserted at the location of the use stmt.
function TParser.ParseUseStmt: TStmt;
var
  Token: TToken;
  FileName, UseName, ProdFileName, LibFileName: String;
  Lexer: TLexer=Nil;
  Reader: TReader;
  AtIndex: LongInt;
begin
  Token := Consume; // use
  UseName := ParseIdent.Text; //the text of the identifier contains the file to use

  Result := TUseStmt.Create(UseName, Token);

  ProdFileName := ProductFolder + UseName + FileExtension;
  LibFileName := LibraryFolder + UseName + FileExtension;

  // First check in current product folder, otherwise in /gearlib/ folder
  if FileExists(ProdFileName) then
    FileName := ProdFileName
  else if FileExists(LibFileName) then
    FileName := LibFileName
  else
    ErrorFmt('Use file "%s" does not exist.', [UseName]);

  // If file is already used then no need to re-use
  if FileNameList.IndexOf(UseName) < 0 then
    begin
      FileNameList.Add(UseName); //+FileExtension);
      try
        try
          Reader := TReader.Create(FileName);
        except
          ErrorFmt('Used file "%s" is incorrect or doesn''t exist.', [UseName]);
        end;

        // Create new Lexer with given filename, resulting in a list of tokens
        Lexer := TLexer.Create(Reader, False); // Don't free the tokens
        AtIndex := Index;

        // convert the EOF token to a newLine token
        with Lexer.Tokens[Lexer.Tokens.Count-1] do
          begin
            Typ := ttLine;
            Lexeme := 'line';
          end;
        //Lexer.Tokens.Delete(Lexer.Tokens.Count-1);
        // insert a newLine at the start of the to-be imported tokens
        Lexer.Tokens.Insert(0, TToken.Create(ttLine, 'line', Current.Location));

        //Insert the new tokens in the main tokens list
        Tokens.InsertRange(AtIndex, Lexer.Tokens);
      finally
        if Assigned(Lexer) then Lexer.Free;
      end;
    end;
end;

// DeferStmt = 'defer' ( statement | 'do' statements 'end' ) .
// A DeferStmt is wrapped into a tempary function + it's call:
//   func deferFunc() do
//     statements
//   end
//   deferFunc()
// The func must be called on all surrounding func returns
function TParser.ParseDeferStmt: TStmt;
var
  Token: TToken;
  Body: TBody;
  DeferID: TIdent;
  Closure: TFuncDecl;
begin
  Token := Consume; // defer

  if Match(ttDo) then
    begin
      ExpectLine('Expect newLine after "do".');
      Body := ParseBody;
      Expect(ttEnd);
    end
  else
    begin
      IgnoreNewlines;
      Body := TBody.Create(TNodeList.Create(), Token.Location);
      Body.Nodes.Add(Statement(False)); // false means no newLine expected
    end;

  // Create unique defer name: deferFunc0, deferFunc1, deferFunc2, etc
  DeferID := MakeID('defer:Func' + IntToStr(DeferCount));
  Inc(DeferCount);
  // create closure with access to surrounding func scope and zero parameters
  Closure := TFuncDecl.Create(DeferID, TParameterList.Create(), ffFunc, Body);

  Result := TDeferStmt.Create(Closure, Token.Location);
end;


// generic function to create a method from a primitive.
// e.g. Method := withMethod('add', 'List', ['value']);
// will create a method 'add(value)' with body: return List:->add(self, value)
// e.g. Method := withMethod('insert', 'List', ['index','value']);
// will create a method 'insert(index, value)' with body: return List:->insert(self, index, value)

// The created method calls contains the string ':->' which makes sure they're not
// callable from Gear code, but only inside the compiler.
// A method call can be a real method function, a value property or an operator.
// In case of an operator, the last argument should be e.g. 'infix_+'.
function TParser.withMethod(Func, Typ: String; Vars: array of String;
  const useAltName, needsSignature: Boolean; const Kind: TMethodKind;
  const OpType: String): TDecl;
var
  Body: TBody;
  Arguments: TArgumentList;
  Parameters: TParameterList;
  CallID, VarID: TIdent;
  CallExpr: TCallExpr;
  Location: TLocation;
  i: Integer;
  Variable: TVariable;
  Signature: String='';
begin
  Location := TLocation.Create(0, 0);  // location can be zero

  Parameters := TParameterList.Create(); // create list of parameters to the function declaration
  Arguments := TArgumentList.Create();   // create list of arguments to the function call
  // add 'self' as 1st argument
  Arguments.Add(TArgument.Create(Nil, TVariable.Create(MakeID('self'))));
  for i := 0 to High(Vars) do
    begin
      VarID := MakeID(Vars[i]);
      Variable := TVariable.Create(VarID);
      Arguments.Add(TArgument.Create(Nil, Variable));
      if useAltName then
        begin
          Parameters.Add(TParameter.Create(Variable, Variable.Name));
          Signature += '#' + Variable.Name.Text + ':';
        end
      else
        Parameters.Add(TParameter.Create(Variable, Nil));
    end;

  // create the call expression e.g. List:->add. The :-> makes sure it can only
  // be used by the compiler and not by the pogrammer in Gear.
  CallID := MakeID(Typ+':->'+Func);
  if needsSignature then
    CallID.Text := CallID.Text + Signature;
  CallExpr := TCallExpr.Create(TVariable.Create(CallID), Arguments, Location);

  // create the body of the function
  Body := TBody.Create(TNodeList.Create(), Location);
  Body.Nodes.Add(TReturnStmt.Create(CallExpr, Location));

  case Kind of
    mkMethod:
      begin
        // create the function declaration
        Func += Signature; // needed in case of named parameters
        Result := TFuncDecl.Create(MakeID(Func), Parameters, ffMethodArrow, Body);
      end;
    mkProperty:
      begin
        // create the value property declaration
        Result := TValDecl.Create(MakeID(Func),
          TFuncDecl.Create(MakeID(Func), TParameterList.Create(), ffMethodVal, Body))
      end;
    mkOperator:
      begin
        // create an operator declaration, e.g. infix_+
        Func := OpType;
        Result := TOperatorDecl.Create(MakeID(Func), Parameters, Body);
      end;
  end;
end;

procedure TParser.BindClass(const NameClass: String; MethodDecl: TDecl);
var
  Decl: TDecl;
begin
  if not ClassMap.TryGetValue(NameClass, Decl) then
    Error(TLocation.Create(0,0), Format('Class not defined: "%s".', [NameClass]))
  else
    (Decl as TClassDecl).Members.AddOrSetValue(MethodDecl.Name.Text, MethodDecl);
end;

procedure TParser.InitializeSystem;
var
  Location: TLocation;
  ForInStmt: TForInStmt;
  GetExpr: TGetExpr;
  Args0, Args1, Args2: TArgumentList;
  CallFilter, CallAdd, CallTransform: TCallExpr;
  CallExprStmt: TCallExprStmt;

  function DeclareClass(const Name: String; const isRecord: Boolean): TClassDecl;
  begin
    Result := TClassDecl.Create(TIdent.Create(Name, Location),
      Nil, TExprList.Create(), Nil, TMemberMap.Create, TMemberMap.Create);
    Result.IsRecord := isRecord;
    ClassMap.Add(Name, Result);
  end;

begin
  Location := TLocation.Create(0,0);

  // add standard classes Array, Dictionary, Set, Range as core classes

  ArrayDecl := DeclareClass('Array', True);
  // array methods
  BindClass('Array', withMethod('add', 'Array', ['value'], False, False, mkMethod));
  BindClass('Array', withMethod('add', 'Array', ['value'], True, False, mkMethod));
  BindClass('Array', withMethod('add', 'Array', ['array'], True, True, mkMethod));
  BindClass('Array', withMethod('add', 'Array', ['count', 'value'], True, True, mkMethod));
  BindClass('Array', withMethod('insert', 'Array', ['index','value'], False, False, mkMethod));
  BindClass('Array', withMethod('insert', 'Array', ['at','value'], True, False, mkMethod));
  BindClass('Array', withMethod('contains', 'Array', ['value'], False, False, mkMethod));
  BindClass('Array', withMethod('contains', 'Array', ['value'], True, False, mkMethod));
  BindClass('Array', withMethod('indexOf', 'Array', ['value'], False, False, mkMethod));
  BindClass('Array', withMethod('index', 'Array', ['of'], True, False, mkMethod));
  BindClass('Array', withMethod('clear', 'Array', [], False, False, mkMethod));
  BindClass('Array', withMethod('delete', 'Array', ['index'], False, False, mkMethod));
  BindClass('Array', withMethod('delete', 'Array', ['index'], True, False, mkMethod));
  BindClass('Array', withMethod('remove', 'Array', ['value'], False, False, mkMethod));
  BindClass('Array', withMethod('remove', 'Array', ['value'], True, False, mkMethod));
  BindClass('Array', withMethod('swap', 'Array', ['index1','index2'], False, False, mkMethod));
  BindClass('Array', withMethod('swap', 'Array', ['index1','index2'], True, False, mkMethod));
  BindClass('Array', withMethod('count', 'Array', [], False, False, mkProperty));
  BindClass('Array', withMethod('first', 'Array', [], False, False, mkProperty));
  BindClass('Array', withMethod('head', 'Array', [], False, False, mkProperty));
  BindClass('Array', withMethod('last', 'Array', [], False, False, mkProperty));
  BindClass('Array', withMethod('tail', 'Array', [], False, False, mkProperty));
  BindClass('Array', withMethod('concat', 'Array', ['other'], False, False, mkOperator, 'infix_+'));
  BindClass('Array', withMethod('equals', 'Array', ['other'], False, False, mkOperator, 'infix_='));
  BindClass('Array', withMethod('iterator', 'Array', [], False, False, mkProperty));
  // ArrayIterator
  ArrayIteratorDecl := DeclareClass('ArrayIterator', True);
  BindClass('ArrayIterator', withMethod('moveNext', 'ArrayIterator', [], False, False, mkProperty));
  BindClass('ArrayIterator', withMethod('current', 'ArrayIterator', [], False, False, mkProperty));

  // dictionary
  DictionaryDecl := DeclareClass('Dictionary', True);
  // dictionary methods
  BindClass('Dictionary', withMethod('add', 'Dictionary', ['key','value'], False, False, mkMethod));
  BindClass('Dictionary', withMethod('add', 'Dictionary', ['key','value'], True, False, mkMethod));
  BindClass('Dictionary', withMethod('delete', 'Dictionary', ['key'], False, False, mkMethod));
  BindClass('Dictionary', withMethod('delete', 'Dictionary', ['key'], True, False, mkMethod));
  BindClass('Dictionary', withMethod('count', 'Dictionary', [], False, False, mkProperty));
  BindClass('Dictionary', withMethod('clear', 'Dictionary', [], False, False, mkMethod));
  BindClass('Dictionary', withMethod('contains', 'Dictionary', ['key'], True, True, mkMethod));
  BindClass('Dictionary', withMethod('contains', 'Dictionary', ['value'], True, True, mkMethod));
  BindClass('Dictionary', withMethod('keys', 'Dictionary', [], False, False, mkProperty));
  BindClass('Dictionary', withMethod('values', 'Dictionary', [], False, False, mkProperty));
  BindClass('Dictionary', withMethod('iterator', 'Dictionary', [], False, False, mkProperty));
  // ArrayIterator
  DictionaryIteratorDecl := DeclareClass('DictionaryIterator', True);
  BindClass('DictionaryIterator', withMethod('moveNext', 'DictionaryIterator', [], False, False, mkProperty));
  BindClass('DictionaryIterator', withMethod('current', 'DictionaryIterator', [], False, False, mkProperty));

  // set
  SetDecl := DeclareClass('Set', True);
  // set methods
  BindClass('Set', withMethod('add', 'Set', ['value'], False, False, mkMethod));
  BindClass('Set', withMethod('add', 'Set', ['value'], True, False, mkMethod));
  BindClass('Set', withMethod('add', 'Set', ['set'], True, True, mkMethod));
  BindClass('Set', withMethod('contains', 'Set', ['value'], False, False, mkMethod));
  BindClass('Set', withMethod('contains', 'Set', ['value'], True, False, mkMethod));
  BindClass('Set', withMethod('union', 'Set', ['with'], True, True, mkMethod));
  BindClass('Set', withMethod('intersect', 'Set', ['with'], True, True, mkMethod));
  BindClass('Set', withMethod('except', 'Set', ['with'], True, True, mkMethod));
  BindClass('Set', withMethod('symmetricExcept', 'Set', ['with'], True, True, mkMethod));
  BindClass('Set', withMethod('remove', 'Set', ['value'], False, False, mkMethod));
  BindClass('Set', withMethod('remove', 'Set', ['value'], True, False, mkMethod));
  BindClass('Set', withMethod('clear', 'Set', [], False, False, mkMethod));
  BindClass('Set', withMethod('toArray', 'Set', [], False, False, mkMethod));
  BindClass('Set', withMethod('count', 'Set', [], False, False, mkProperty));
  BindClass('Set', withMethod('first', 'Set', [], False, False, mkProperty));
  BindClass('Set', withMethod('head', 'Set', [], False, False, mkProperty));
  BindClass('Set', withMethod('last', 'Set', [], False, False, mkProperty));
  BindClass('Set', withMethod('tail', 'Set', [], False, False, mkProperty));
  BindClass('Set', withMethod('union', 'Set', ['with'], True, True, mkOperator, 'infix_+'));
  BindClass('Set', withMethod('intersect', 'Set', ['with'], True, True, mkOperator, 'infix_*'));
  BindClass('Set', withMethod('except', 'Set', ['with'], True, True, mkOperator, 'infix_-'));
  BindClass('Set', withMethod('symmetricExcept', 'Set', ['with'], True, True, mkOperator, 'infix_~'));
  BindClass('Set', withMethod('iterator', 'Set', [], False, False, mkProperty));
  // setIterator
  SetIteratorDecl := DeclareClass('SetIterator', True);
  BindClass('SetIterator', withMethod('moveNext', 'SetIterator', [], False, False, mkProperty));
  BindClass('SetIterator', withMethod('current', 'SetIterator', [], False, False, mkProperty));

  // range
  RangeDecl := DeclareClass('Range', True);
  // range methods
  BindClass('Range', withMethod('iterator', 'Range', [], False, False, mkProperty));
  BindClass('Range', withMethod('step', 'Range', ['by'], False, False, mkMethod));
  BindClass('Range', withMethod('step', 'Range', ['by'], True, False, mkMethod));
  BindClass('Range', withMethod('toArray', 'Range', [], False, False, mkMethod));
  BindClass('Range', withMethod('toSet', 'Range', [], False, False, mkMethod));
  BindClass('Range', withMethod('count', 'Range', [], False, False, mkProperty));
  BindClass('Range', withMethod('from', 'Range', [], False, False, mkProperty));
  BindClass('Range', withMethod('to', 'Range', [], False, False, mkProperty));
  BindClass('Range', withMethod('head', 'Range', [], False, False, mkProperty));
  BindClass('Range', withMethod('tail', 'Range', [], False, False, mkProperty));

  RangeIteratorDecl := DeclareClass('RangeIterator', True);
  // rangeIterator methods
  BindClass('RangeIterator', withMethod('moveNext', 'RangeIterator', [], False, False, mkProperty));
  BindClass('RangeIterator', withMethod('current', 'RangeIterator', [], False, False, mkProperty));

  // list builder function:
  //  func listBuilder(result, transform, sequence, include) do
  //  for var item in sequence where include(item) do
  //    result.add(transform(item))
  //  end
  //  return result
  //  end
  ListBuilder := TFuncDecl.Create(MakeID('listBuilder'), TParameterList.Create(), ffFunc,
    TBody.Create(TNodeList.Create(), Location));
  // the result parameter expects either an empty [] or {} as value
  ListBuilder.Parameters.Add(TParameter.Create(TVariable.Create(MakeID('result')), Nil));
  // transform is a function that transforms an item of the sequence
  ListBuilder.Parameters.Add(TParameter.Create(TVariable.Create(MakeID('transform')), Nil));
  // sequence is the input list, which must be iterable
  ListBuilder.Parameters.Add(TParameter.Create(TVariable.Create(MakeID('sequence')), Nil));
  // include is a function that returns true or false on each item
  ListBuilder.Parameters.Add(TParameter.Create(TVariable.Create(MakeID('include')), Nil));

  // create call filter: include(item), which returns true or false
  Args0 := TArgumentList.Create();
  Args0.Add(TArgument.Create(Nil, TVariable.Create(MakeID('item'))));
  CallFilter := TCallExpr.Create(TVariable.Create(MakeID('include')), Args0, Location);

  // create the ForInStmt
  ForInStmt := TForInStmt.Create(MakeID('item'), TVariable.Create(MakeID('sequence')),
    CallFilter, TBody.Create(TNodeList.Create(), Location), Location);

  // create call: transform(item)
  Args1 := TArgumentList.Create();
  Args1.Add(TArgument.Create(Nil, TVariable.Create(MakeID('item'))));
  CallTransform := TCallExpr.Create(TVariable.Create(MakeID('transform')), Args1, Location);

  // create the call stmt result.add(transform(item))
  // create getExpr result.add
  GetExpr := TGetExpr.Create(TVariable.Create(MakeID('result')),TVariable.Create(MakeID('add')));
  Args2 := TArgumentList.Create();
  Args2.Add(TArgument.Create(Nil, CallTransform));
  CallAdd := TCallExpr.Create(GetExpr, Args2, Location);
  CallExprStmt := TCallExprStmt.Create(CallAdd);
  ForInStmt.Block.Nodes.Add(CallExprStmt);
  ListBuilder.Body.Nodes.Add(ForInStmt);
  // create the return expr
  ListBuilder.Body.Nodes.Add(TReturnStmt.Create(TVariable.Create(MakeID('result')), Location));

end;

end.


