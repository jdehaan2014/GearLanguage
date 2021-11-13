unit uAST;

{$mode objfpc}{$H+}

{
  This unit defines the nodes of the AST structure.
  The root node of all expressions, statements and declarations is TNode.
  There's also base TExpr node, from which all expressions inherite. The same
  goes for the TStmt node for all statements and TDecl for all declarations.
}

interface

uses
  Classes, SysUtils, uToken, Generics.Collections, uValue;

type

  // Root of all nodes in the AST
  TNode = class
    private
      // The location: row, column and file index of the node. All nodes provide this.
      FLocation: TLocation;
    public
      property Location: TLocation read FLocation;
      constructor Create(ALocation: TLocation);
  end;

  { EXPRESSIONS }

  // TExpr is the base class for all expression types.
  TExpr = class(TNode)
    // Base node for expressions.
  end;

  // A list of expressions in an object list
  TExprList = specialize TObjectList<TExpr>;

  // for defining literals
  TLiteralType = (ltNil, ltFalse, ltTrue);

  // A literal such as 'nil', 'true' or 'false'
  TLiteralExpr = class(TExpr)
    private
      FLiteralType: TLiteralType;
    public
      property LiteralType: TLiteralType read FLiteralType;
      constructor Create(ALiteralType: TLiteralType; ALocation: TLocation);
  end;

  // Node for constant numbers. Both integer types and floating point types
  // are represented in a Double.
  TNumberExpr = class(TExpr)
    private
      FValue: Double;
    public
      property Value: Double read FValue;
      constructor Create(NumberToken: TToken);
      constructor Create(Number: Double; ALocation: TLocation);
  end;

  // for constant strings
  TStringExpr = class(TExpr)
    private
      FValue: String;
    public
      property Value: String read FValue;
      constructor Create(StringToken: TToken);
      constructor Create(AValue: String; ALocation: TLocation);
  end;

  // for constant char. A char value may contain up to 4 bytes
  TCharExpr = class(TExpr)
    private
      FValue: String4;
    public
      property Value: String4 read FValue;
      constructor Create(CharToken: TToken);
      constructor Create(AValue: String4; ALocation: TLocation);
  end;

  // Used for prefix operators: -a, +a, not a
  TUnaryExpr = class(TExpr)
    private
      FOp: TTokenTyp;      //operator Not, Minus, Plus
      FExpr: TExpr;        //single expression
    public
      property Op: TTokenTyp read FOp;
      property Expr: TExpr read FExpr;
      constructor Create(AOp: TTokenTyp; AExpr: TExpr; ALocation: TLocation);
      destructor Destroy; override;
  end;

  // Used for all infix operators, e.g. a+b, a*b, a/b, a-b, a%b, a~b (concat)
  TBinaryExpr = class(TExpr)
    private
      // The left side of a binary expression
      FLeft: TExpr;
      // The operator of the binary expression
      FOp: TTokenTyp;
      // The right hand side of a binary expression
      FRight: TExpr;
    public
      property Left: TExpr read FLeft;
      property Op: TTokenTyp read FOp;
      property Right: TExpr read FRight;
      constructor Create(ALeft: TExpr; AOp: TTokenTyp; ARight: TExpr; ALocation: TLocation);
      destructor Destroy; override;
  end;

  // the 'and' expression is a special case of binary expr
  TAndExpr = class(TBinaryExpr)
    constructor Create(ALeft, ARight: TExpr; ALocation: TLocation);
  end;

  // the 'or' expression is special case of binary expr
  TOrExpr = class(TBinaryExpr)
    constructor Create(ALeft, ARight: TExpr; ALocation: TLocation);
  end;

  // Used for conditional or if-expressions
  TTernaryExpr = class(TExpr)
    private
      // the condition  of the if-expression
      FCondition,
      // the expression returned when the condition is true
      FTrueExpr,
      // the expression returned when the condition is false
      FFalseExpr: TExpr;
    public
      property Condition: TExpr read FCondition;
      property TrueExpr: TExpr read FTrueExpr;
      property FalseExpr: TExpr read FFalseExpr;
      constructor Create(ACondition, ATrueExpr, AFalseExpr: TExpr);
      destructor Destroy; override;
  end;

  // base for all identifiers, such as variables and declarations
  TIdent = class(TNode)
    private
      // an identifier is built from a string
      FText: String;
    public
      property Text: String read FText write FText;
      constructor Create(Token: TToken);
      constructor Create(const AText: String; const ALocation: TLocation);
  end;

  // the variable expression is used for all variables in the code
  TVariable = class(TExpr)
    private
      // a variable consists of an identifier name
      FName: TIdent;
    public
      property Name: TIdent read FName;
      constructor Create(const AName: TToken);
      constructor Create(AName: TIdent);
  end;

  // an argument is uswd in a call to a function, and matches the defined parameter
  // Argument = [ Ident ':' ] Expr .
  TArgument = class
    private
      // an argument can have a name
      FName: TIdent;
      // the expression of the argument
      FExpr: TExpr;
    public
      property Name: TIdent read FName;
      property Expr: TExpr read FExpr;
      constructor Create(AName: TIdent; AExpr: TExpr);
      destructor Destroy; override;
  end;

  // the list of arguments in a call to a function
  TArgumentList = specialize TObjectList<TArgument>;

  // A CallExpr respresents the call of a function
  TCallExpr = class(TExpr)
    private
      // the name of the called function: the callee
      FCallee: TExpr;
      // the argument expressions (arg1, arg2, arg3, ... arg256)
      FArguments: TArgumentList;
      // the number of arguments
      FArgCount: Byte;
    public
      // the signature is the callee + any named arguments, needed for overloading funcs
      Signature: TExpr;
      // if the callee refers to a class name, this is set to true, else false
      IsClassInit: Boolean;
      property Callee: TExpr read FCallee;
      property Arguments: TArgumentList read FArguments;
      property ArgCount: Byte read FArgCount;
      constructor Create(ACallee: TExpr; ArgList: TArgumentList; ALocation: TLocation);
      destructor Destroy; override;
  end;

  // respresents the 'self' variable
  TSelfExpr = class(TVariable)
    // no other fields
  end;

  // the 'inherited' variable followed by a method + arguments
  //   inherited method(arguments)
  // calls the method with that name in the parent class
  TInheritedExpr = class(TVariable)
    private
      // the inherited method
      FMethod: TVariable;
      // arguments of the inherited method
      FArguments: TExprList;
      // number of arguments
      FArgCount: Integer;
    public
      property Method: TVariable read FMethod;
      property Arguments: TExprList read FArguments;
      property ArgCount: Integer read FArgCount;
      constructor Create(AName: TToken; AMethod: TVariable; ArgList: TExprList;
        const Count: Integer);
      destructor Destroy; override;
  end;

  // a GetExpr consists of a class instance and a member (method or field) separates
  // by either a dot or a questionmark dot .
  //   instance.member   using regular 'dot' operator
  //   instance?.member  using savety operator ?. prevends crash if instance = nil
  TGetExpr = class(TExpr)
    private
      // the instance of a class, record, array, dictionary, set
      FInstance: TExpr;
      // the member field or method or value property
      FMember: TExpr;
      // if safety operator ?. used then the value is 'true' else 'false'
      FSafetyOn: Boolean;
    public
      property Instance: TExpr read FInstance;
      property Member: TExpr read FMember;
      property SafetyOn: Boolean read FSafetyOn;
      constructor Create(AInstance, AMember: TExpr; ASafetyOn: Boolean=False);
      destructor Destroy; override;
  end;

  // an array expression is a list of expressions between [ and ], separated by commas
  // e.g. var a := [1,2,3,4,5]
  TArrayExpr = class(TExpr)
    private
      FElements: TExprList;
    public
      property Elements: TExprList read FElements;
      constructor Create(AElements : TExprList; ALocation: TLocation);
      destructor Destroy; override;
  end;

  // an unordered map of key:value pairs
  TKeyValueMap =  specialize TObjectDictionary<TExpr, TExpr>;

  // a dictionary expression is a list of key:value pairs separated by comma's and
  // surrounded by square brackets [ and ]
  // e.g. var d := [1:'one', 2:'two', 3:'three']
  // the key and value are separated by a colon :
  TDictionaryExpr = class(TExpr)
    private
      FElements: TKeyValueMap;
    public
      property Elements: TKeyValueMap read FElements;
      constructor Create(AElements : TKeyValueMap; ALocation: TLocation);
      destructor Destroy; override;
  end;

  // an indexed expression is a variable name followed by an index between square
  // brackets. e.g. variable[index]
  TIndexedExpr = class(TExpr)
    private
      // the variable, an indexable expression, array or dictionary
      FVariable: TExpr;
      // the index, a number when array, or any expression when a ditionary
      FIndex: TExpr;
    public
      property Variable: TExpr read FVariable;
      property Index: TExpr read FIndex;
      constructor Create(AVariable: TExpr; AIndex: TExpr);
      destructor Destroy; override;
  end;

  // an unordered map of binary expr and expr, used in the match expr
  TBinaryExprMap = specialize TObjectDictionary<TBinaryExpr, TExpr>;

  // the match expression is used for pattern matching
  TMatchExpr = class(TExpr)
    private
      // the expression to match
      FExpr: TExpr;
      // the list of patterns that could match the expression
      FIfLimbs: TBinaryExprMap;
      // mandatory default expression if none of the patterns match
      FElseLimb: TExpr;
    public
      property Expr: TExpr read FExpr;
      property IfLimbs: TBinaryExprMap read FIfLimbs;
      property ElseLimb: TExpr read FElseLimb write FElseLimb;
      constructor Create(AExpr: TExpr; AIfLimbs: TBinaryExprMap; AElseLimb: TExpr);
      destructor Destroy; override;
  end;

  // A tuple is a finite ordered list of elements. A tuple is written
  // by listing the elements within parentheses (), and separated by commas.
  // A Gear tuple consists of name:expr pairs.
  // e.g. let one2four := (one: 'one', two: 2, three: 'three', four: 4)
  // Tuple values are accessed using the dot operator.
  // e.g. print( one2four.two )
  TTupleExpr = class(TExpr)
    private
      FElements: TArgumentList;
    public
      property Elements: TArgumentList read FElements;
      constructor Create(AElements: TArgumentList; ALocation: TLocation);
      destructor Destroy; override;
  end;

  // A set is unordered (like a dictionary), and duplicates are automatically
  // removed when creating a new set or when adding to the set. The elements of
  // a set expression are separated by commas, and enclosed by curly braces.
  // e.g. var s := {1,1,2,3,5,8,13,21,34,55}
  TSetExpr = class(TExpr)
    private
      FElements: TExprList;
    public
      property Elements: TExprList read FElements;
      constructor Create(AElements: TExprList; ALocation: TLocation);
      destructor Destroy; override;
  end;

  // A range is depicted by 2 constants/variables separated by the range operator.
  // The range operator can be 2 dots: 0..9
  // or can be 2 dots followed by a less than token: 0..<list.count
  // In the latter case the value list.count is excluded from the range.
  // In effect, 0..<list.count is the same as 0..(list.count-1)
  TRangeExpr = class(TExpr)
    private
      FFrom,
      FUpTo: TExpr;
      FIsInclusive: Boolean;
    public
      property From: TExpr read FFrom;
      property UpTo: TExpr read FUpTo;
      property IsInclusive: Boolean read FIsInclusive;
      constructor Create(AFrom, AUpTo: TExpr; AIsInclusive: Boolean; ALocation: TLocation);
      destructor Destroy; override;
  end;

  { STATEMENTS }

  // forward definitions
  TBlock = class;
  TBlockList = specialize TObjectList<TBlock>;
  TNodeList = specialize TObjectList<TNode>;
  TVarDecl = class;

  // TStmt is the base class for all statements
  TStmt = class(TNode)
    // Base class for statements
  end;

  // Expression as a statement, e.g. inherited method() is an ExprStmt
  TExprStmt = class(TStmt)
    private
      FExpr: TExpr;
    public
      property Expr: TExpr read FExpr;
      constructor Create(AExpr: TExpr);
      destructor Destroy; override;
  end;

  // The assignment is a variable followed by := and an expression
  // e.g. variable := expression
  TAssignStmt = class(TStmt)
    private
      // the variable expression of the assignment
      FVariable: TVariable;
      // the expression to be assigned to the variable
      FExpr: TExpr;
      // the assignment operator: :=, +=, -=, *=, /=, %=
      FAssignOp: TTokenTyp;
    public
      property Variable: TVariable read FVariable;
      property Expr: TExpr read FExpr;
      property AssignOp: TTokenTyp read FAssignOp;
      constructor Create(AVariable: TVariable; AExpr: TExpr; const AAssignOp: TTokenTyp);
      destructor Destroy; override;
  end;

  // A call expr stmt calls a function but doesn't return a result
  TCallExprStmt = class(TStmt)
    private
      FCallExpr: TCallExpr;
    public
      property CallExpr: TCallExpr read FCallExpr;
      constructor Create(ACallExpr: TCallExpr);
      destructor Destroy; override;
  end;

  // a set statement consists of a class instance, a dot, and a member variable,
  // followed by an assignment operator and an expression.
  // e.g. instance.member := expression
  TSetStmt = class(TStmt)
    private
      // a GetExpr: instance.member
      FGetExpr: TGetExpr;
      // assignment operator
      FOp: TTokenTyp;
      // expression to be assigned to the member field of the instance
      FExpr: TExpr;
    public
      property GetExpr: TGetExpr read FGetExpr;
      property Op: TTokenTyp read FOp;
      property Expr: TExpr read FExpr;
      constructor Create(AGetExpr: TGetExpr; AOp: TTokenTyp; AExpr: TExpr);
      destructor Destroy; override;
  end;

  // An indexed statement consists of a variable expression follewed by an index,
  // then an assignment operator and finally an expression.
  // e.g.  arrray[index] := expression
  // e.g.  dictionary[key] := value
  TIndexedStmt = class(TStmt)
    private
      // the variable + index or key
      FIndexedExpr: TIndexedExpr;
      // the assignment operator
      FOp: TTokenTyp;
      // the expression to be assigned to the indexed expression
      FExpr: TExpr;
    public
      property IndexedExpr: TIndexedExpr read FIndexedExpr;
      property Expr: TExpr read FExpr;
      property Op: TTokenTyp read FOp;
      constructor Create(AIndexedExpr: TIndexedExpr; AOp: TTokenTyp; AExpr: TExpr);
      destructor Destroy; override;
  end;

  // Print can do multiple expressions and may use a terminator
  // e.g. print(expr1, expr2, terminator: '\t\n')
  TPrintStmt = class(TStmt)
    private
      // the list of expressions to print on 1 line
      FExprList: TExprList;
      // the terminator expression printed after the last expression is printed
      FTerminator: TExpr;
    public
      property ExprList: TExprList read FExprList;
      property Terminator: TExpr read FTerminator;
      constructor Create(AExprList: TExprList; ATerminator: TExpr; ALocation: TLocation);
      destructor Destroy; override;
  end;

  // The if-statement is defined in ebnf as follows:
  // 'if' [VarDecl 'where'] Condition 'then' Block
  // {'elseif' Condition 'then' Block}                 zero or more
  // ['else' Block]                                    zero or one
  // 'end' .
  TElseIfItem = class
    private
      // Boolean expression of the elseif condition
      FCondition: TExpr;
      // Block of statements and declarations to be executed if condition = true
      FBlock: TBlock;
    public
      property Condition: TExpr read FCondition;
      property Block: TBlock read FBlock;
      constructor Create(ACondition: TExpr; ABlock: TBlock);
      destructor Destroy; override;
  end;

  // the list of elseif conditions: zero or more
  TElseIfList = specialize TObjectList<TElseIfItem>;

  TIfStmt = class(TStmt)
    private
      // optional variable declaration. The variable scope is the full if-statement.
      FVarDecl: TVarDecl;
      // Boolean expression
      FCondition: TExpr;
      // Block of statements and declarations executed of condition is true
      FThenPart: TBlock;
      // Block of statements and declarations executed of condition is false
      FElsePart: TBlock;
      // List of elseif conditions
      FElseIfList: TElseIfList;
    public
      property VarDecl: TVarDecl read FVarDecl;
      property Condition: TExpr read FCondition;
      property ThenPart: TBlock read FThenPart write FThenPart;
      property ElsePart: TBlock read FElsePart;
      property ElseIfList: TElseIfList read FElseIfList;
      constructor Create(AVarDecl: TVarDecl; ACondition: TExpr; AThenPart, AElsePart: TBlock;
        AElseIfList: TElseIfList; ALocation: TLocation);
      destructor Destroy; override;
  end;

  // The while-statement is defined in ebnf as follows:
  // 'while' [VarDecl 'where'] Condition 'do' Block 'end' .
  TWhileStmt = class(TStmt)
    private
      // optional variable declaration. The variable scope is the full while-statement.
      FVarDecl: TVarDecl;
      // Boolean expression
      FCondition: TExpr;
      // Block of statements and declarations executed while condition is true
      FBlock: TBlock;
    public
      property VarDecl: TVarDecl read FVarDecl;
      property Condition: TExpr read FCondition;
      property Block: TBlock read FBlock;
      constructor Create(AVarDecl: TVarDecl; ACondition: TExpr;
        ABlock: TBlock; ALocation: TLocation);
      destructor Destroy; override;
  end;

  // The for-statement is defined in ebnf as follows:
  // 'for' VarDecl 'where' Condition ',' Iterator 'do' Block 'end' .
  // It's based on the While statement, however the VarDecl is mndatory,
  // and an Iterator is needed.
  TForStmt = class(TWhileStmt)
    private
      // The iterator is a statement, such as i+=1, or i := i + 1
      FIterator: TStmt;
    public
      property Iterator: TStmt read FIterator;
      constructor Create(AVarDecl: TVarDecl; ACondition: TExpr; AIterator: TStmt;
        ABlock: TBlock; ALocation: TLocation);
      destructor Destroy; override;
  end;

  // The body of the function
  TBody = class;  // forward declaration

  // for-in-stmt = 'for' Ident 'in' SequenceExpr ['where' Condition] 'do' Stmt 'end'
  // The where-clause is added to the statemens block during parsing.
  TForInStmt = class(TStmt)
    private
      FLoopVar: TIdent;
      FSequence: TExpr;
      FWhere: TExpr;
      FBlock: TBody;  // TBody to prevend automatic scoping
    public
      property LoopVar: TIdent read FLoopVar;
      property Sequence: TExpr read FSequence;
      property Where: TExpr read FWhere;
      property Block: TBody read FBlock;
      constructor Create(ALoopVar: TIdent; ASequence: TExpr; AWhere: TExpr;
        ABlock: TBody; ALocation: TLocation);
      destructor Destroy; override;
  end;

  TListBuilderTyp = (lbtArray, lbtSet);
  // ListBuilder = '[' TransformExpr 'for' Ident 'in' SequenceExpr ['where' Condition] ']'
  // SetBuilder = '{' TransformExpr 'for' Ident 'in' SequenceExpr ['where' Condition] '}'

  //TListBuilderExpr = class(TExpr)
  //  private
  //    FTyp: TListBuilderTyp;
  //    FForInStmt: TForInStmt;
  //  public
  //    property Typ: TListBuilderTyp read FTyp;
  //    property ForInStmt: TForInStmt read FForInStmt;
  //    constructor Create(ATyp: TListBuilderTyp; AForInStmt: TForInStmt;
  //      ALocation: TLocation);
  //    destructor Destroy; override;
  //end;


  TListBuilderExpr = class(TExpr)
    private
      FTyp: TListBuilderTyp;
      FMap,
      FSequence,
      FFilter: TExpr;
    public
      property Typ: TListBuilderTyp read FTyp;
      property Map: TExpr read FMap;
      property Sequence: TExpr read FSequence;
      property Filter: TExpr read FFilter;
      constructor Create(ATyp: TListBuilderTyp;  AMap, ASequence, AFilter: TExpr;
        ALocation: TLocation);
      destructor Destroy; override;
  end;

  // The repeat-statement is defined in ebnf as follows:
  // 'do' Block 'end while' Condition .
  TRepeatStmt = class(TStmt)
    private
      // Block of statements to be executed while condition is true. Minimum
      // number of executions is one.
      FBlock: TBlock;
      // Boolean expression
      FCondition: TExpr;
    public
      property Block: TBlock read FBlock;
      property Condition: TExpr read FCondition;
      constructor Create(ABlock: TBlock; ACondition: TExpr;
        ALocation: TLocation);
      destructor Destroy; override;
  end;

  // Test of alternative loops with exits. Not to be published.
  { example:

  func factorial(n) do
    var result := 1
    loop var counter := n do
      result *= counter
      counter -=1
      exit when counter=0
    end
    return result
  end

  Multiple exits are allowed:

  randomize()
  var y := 0
  loop var x := 0 do
    x := random(10)
    exit when x = 0
      y := x^2
      print(y)
    exit when y > 70
  end

  }
  TConditionalBlock = class
    private
      FCondition: TExpr;
      FBlock: TBlock;
    public
      property Condition: TExpr read FCondition;
      property Block: TBlock read FBlock;
      constructor Create(ACondition: TExpr; ABlock: TBlock);
      destructor Destroy; override;
  end;

  TVarDecls = class;

  TConditionalBlocks = specialize TObjectList<TConditionalBlock>;

  TLoopDoStmt = class(TStmt)
    private
      FVarDecls: TVarDecls;
      FBlock: TBlock;
      FConditionalBlocks: TConditionalBlocks;
    public
      property VarDecls: TVarDecls read FVarDecls;
      property Block: TBlock read FBlock;
      property ConditionalBlocks: TConditionalBlocks read FConditionalBlocks;
      constructor Create(AVarDecls: TVarDecls; ABlock: TBlock;
         AConditionalBlocks: TConditionalBlocks; ALocation: TLocation);
      destructor Destroy; override;
  end;

  // the ebnf of the Ensure-statement is defined as follows:
  // 'ensure' [VarDecl 'where'] Condiion 'else' Block 'end' .
  TEnsureStmt = class(TStmt)
    private
      // optional variable declaration. Scope of variable is the urrounding scope.
      FVarDecl: TVarDecl;
      // Boolean expression
      FCondition: TExpr;
      // Block to be executed if condition is false
      FElsePart: TBlock;
    public
      property VarDecl: TVarDecl read FVarDecl;
      property Condition: TExpr read FCondition;
      property ElsePart: TBlock read FElsePart;
      constructor Create(AVarDecl: TVarDecl; ACondition: TExpr;
        AElsePart: TBlock; ALocation: TLocation);
      destructor Destroy; override;
  end;

  // to return from a function
  TReturnStmt = class(TStmt)
    private
      // expression to be returned from the function
      FExpr: TExpr;
    public
      property Expr: TExpr read FExpr;
      constructor Create(AExpr: TExpr; ALocation: TLocation);
      destructor Destroy; override;
  end;

  //TBreakStmt = class(TStmt)
  //  private
  //    FCondition: TExpr;
  //  public
  //    property Condition: TExpr read FCondition;
  //    constructor Create(ACondition: TExpr; ALocation: TLocation);
  //    destructor Destroy; override;
  //end;

  // to break out of the current loop and give control to the surrounding scope
  TBreakStmt = class(TStmt)
    //nothing in here
  end;

  // stop the current iteration and start the next one
  TContinueStmt = class(TStmt)
    //nothing in here
  end;

  // type of expression used in the pattern matching of the switch statement
  TCaseItemType = (citNormal, citObject, citEnum, citElemOf);

  // the expression to match and accompanying type
  TCaseItem = class
    Expr: TExpr;
    Typ: TCaseItemType;
    constructor Create(AExpr: TExpr; const ATyp: TCaseItemType);
    destructor Destroy; override;
  end;

  // the list of cases in the switch statement
  TSwitchCases = specialize TObjectDictionary<TCaseItem, TBlock>;

  // the ebnf of the switch-statemnt is as follows:
  // 'switch' Expr CaseBlock {CaseBlock} 'default' Block .
  // CaseBlock = 'case' CaseItem ':' Block .
  // CaseItem = [ ( 'is' | 'in' | '.') ] Expr .
  TSwitchStmt = class(TStmt)
    private
      // Expression to match: 'value' or 'is class' or 'in array' or '.enum'
      FExpr: TExpr;
      // list of cases that could match the pattern
      FCases: TSwitchCases;
      // mandatory default case if all matches are false
      FDefaultCase: TBlock;
    public
      property Expr: TExpr read FExpr;
      property Cases: TSwitchCases read FCases;
      property DefaultCase: TBlock read FDefaultCase;
      constructor Create(AExpr: TExpr; ACases: TSwitchCases; ADefault: TBlock;
        ALocation: TLocation);
      destructor Destroy; override;
  end;

  // to import library files
  TUseStmt = class(TStmt)
    private
      // name of the fil without file extension
      // name will be automatically expanded to '.gear'
      FFileName: String;
    public
      property FileName: String read FFileName;
      constructor Create(AFileName: String; AToken: TToken);
  end;

  TFuncDecl = class;

  // With the defer-statement it is possible to defer any activity to when a function
  // returns. A defer-statement is transformed into a nested function declaration, i.e.
  // a closure, and a call to this closure. The call is guaranteed executed upon return.
  // ebnf: 'defer' ( Statement | 'do' Block 'end' ) .
  TDeferStmt = class(TStmt)
    private
      FClosure: TFuncDecl;
    public
      property Closure: TFuncDecl read FClosure;
      constructor Create(AClosure: TFuncDecl; ALocation: TLocation);
      destructor Destroy; override;
  end;

  { DECLARATIONS }

  // Each declaration has a declaration kind
  TDeclKind = (dkClass, dkExtension, dkFunc, dkEnum, dkParam,
               dkTrait, dkVal, dkVar, dkVars, dkIndexer);
  TDecl = class(TNode)
    // Base class for declarations
    private
      // name of the declaration
      FName: TIdent;
      // kind of declaration
      FKind: TDeclKind;
    public
      property Name: TIdent read FName;
      property Kind: TDeclKind read FKind;
      constructor Create(AName: TIdent; AKind: TDeclKind; ALocation: TLocation);
      destructor Destroy; override;
  end;

  TDeclList = specialize TObjectList<TDecl>;

  // A variable declaration: 'var' ident ':=' expression .
  // the ident name is inherited from TDecl
  TVarDecl = class(TDecl)
    private
      // initial expression of the variable
      FExpr: TExpr;
      // is it a constant or a variable
      FMutable: Boolean;
    public
      property Expr: TExpr read FExpr;
      property Mutable: Boolean read FMutable;
      constructor Create(AName: TIdent; AExpr: TExpr;
        const AMutable: Boolean);
      destructor Destroy; override;
  end;

  // container for multiple VarDecl
  // var a:=1, b:=2, c:=3
  TVarDecls = class(TDecl)
    private
      // list of multiple variable declarations
      FList: TDeclList;
    public
      property List: TDeclList read FList;
      constructor Create(AList: TDeclList; ALocation: TLocation);
      destructor Destroy; override;
  end;

  // the possible forms a function can have
  TFuncForm = (ffAnonym, ffAnonymArrow, ffFuncArrow, ffFunc, ffInit,
               ffMethod, ffMethodArrow, ffScript, ffVal, ffMethodVal);

  // The parameter of a function may have a name, e.g.
  // func test(name1 parameter1, name2 parameter2) do
  // The parameter may be its own name if preceded by a dot, e.g.
  // func test(.parameter)
  // When the function is called the alternative name when available must be used
  TParameter = class
    private
      // the formal parameter
      FVariable: TVariable;
      // the alternative name of the parameter also to be used in the argument to the call
      FName: TIdent;
    public
      property Variable: TVariable read FVariable;
      property Name: TIdent read FName;
      constructor Create(AVariable: TVariable; AName: TIdent);
      destructor Destroy; override;
  end;

  // the list of formal parameters with their alternative names
  TParameterList = specialize TObjectList<TParameter>;

  // ebnf of the standard function declaration:
  // 'func' Ident '(' Parameters ')' 'do' Body 'end' .
  // However, whenonly 1 expression is returned, the arrow notation may be used:
  // 'func' Ident '(' Parameters ')' '=>' Expr  .
  TFuncDecl = class(TDecl)
    private
      // the form of the function: normal, arrow, method, value property, etc
      FForm: TFuncForm;
      // the list of formal parameters
      FParameters: TParameterList;
      // the body of the function
      FBody: TBody;
    public
      property Form: TFuncForm read FForm;
      property Parameters: TParameterList read FParameters;
      property Body: TBody read FBody;
      constructor Create(AName: TIdent; AParameters: TParameterList;
        const AForm: TFuncForm; ABody: TBody);
      constructor Create(AParameters: TParameterList;
        const AForm: TFuncForm; ABody: TBody; ALocation: TLocation);
      destructor Destroy; override;
  end;

  // A lambda expression is an anonymous function, so has no identifier
  // ebnf of the lmbda expression is:
  // ('lambda' | '/\' ) '(' Parameters ')' ( '=>' Expr | 'do' Block 'end' ) .
  TLambdaExpr = class(TExpr)
    private
      FFunc: TFuncDecl;
    public
      property Func: TFuncDecl read FFunc;
      constructor Create(AFunc: TFuncDecl);
      destructor Destroy; override;
  end;

  // type of the operator definition
  TOperatorType = (otInfix, otPostfix, otPrefix);

  // An operator declaration is actually a function declaration with special processing
  TOperatorDecl = class(TFuncDecl)
    private
      // Infix, Prefix or Postfix
      FOperatorType: TOperatorType;
    public
      property OperatorType: TOperatorType read FOperatorType;
      constructor Create(AOpID: TIdent; AOperatorType: TOperatorType;
        AParameters: TParameterList; ABody: TBody; AFuncForm: TFuncForm);
      constructor Create(AOpID: TIdent; AParameters: TParameterList; ABody: TBody);
  end;

  // The value property (often called a 'getter') is in fact a function without parameters
  // ebnf is:
  // ValDecl = 'val' Ident ( ':=' Expr  | 'do' Block 'end' ) .
  // It must always return a value. If only a single expression is returned the
  // shorthand ':=' may be used.
  // val field := _field
  // is the same as
  // val field do return _field end
  TValDecl = class(TDecl)
    private
      FFunc: TFuncDecl;
    public
      property Func: TFuncDecl read FFunc;
      constructor Create(const AName: TIdent; AFunc: TFuncDecl);
      destructor Destroy; override;
  end;

  // unordered list of members of a class
  TMemberMap = specialize TObjectDictionary<String, TDecl>;

  // ebnf of class:
  // ClassDecl = 'class' Ident [ '<' Parent ] [':' Traits] 'is' {Member} 'end' .
  TClassDecl = class(TDecl)
    private
      // optional parent of the class (also called super class)
      FParent: TVariable;
      // optional available trait functions for the class
      FTraits: TExprList;
      // if a field is defined as default then a reference to the class instance is
      // redirected to the respective field.
      FDefaultValue: TVarDecl;
      // the members (fields, methods, value properties)
      FMembers: TMemberMap;
      // static members of the class
      FStatics: TMemberMap;
      // if IsRecord is true then inheritance is not allowed
      FIsRecord: Boolean;
    public
      property Parent: TVariable read FParent;
      property Traits: TExprList read FTraits;
      property DefaultValue: TVarDecl read FDefaultValue;
      property Members: TMemberMap read FMembers;
      property Statics: TMemberMap read FStatics;
      property IsRecord: Boolean read FIsRecord write FIsRecord;
      constructor Create(AName: TIdent; AParent: TVariable; ATraits: TExprList;
        ADefaultValue: TVarDecl; AMembers, AStatics: TMemberMap);
      destructor Destroy; override;
  end;

  // list of declared classes
  TClassMap = specialize TObjectDictionary<String, TDecl>;

  // extension to a class
  // ebnf:
  // 'extension' Ident 'is' {Member} 'end' .
  // Ident must be an existing class, all members are added to the class or record
  // during compilation, not at runtime. If a member has the same name as an exiting
  // member, than the original one is overwritten.
  TExtensionDecl = class(TDecl)
    private
      // members can only be func or val
      FMembers: TMemberMap;
      // optional available trait functions for the class
      FTraits: TExprList;
    public
      property Members: TMemberMap read FMembers;
      property Traits: TExprList read FTraits;
      constructor Create(AName: TIdent; AMembers: TMemberMap; ATraits: TExprList);
      destructor Destroy; override;
  end;

  // The elements of an enumeration, in ebnf:
  // EnumElements = EnumElement { ',' EnumElement } .
  // EnumElement = Ident [ '=' Expr ]
  TEnumElements = specialize TObjectDictionary<String, TExpr>;

  // The ebnf of an enum declaration is:
  // EnumDecl = 'enum' EnumElements {Member} 'end' .
  // The elements comma separated, and may be succeeded by '=' Expr
  // An enum declaration may contain functions and value properties.
  TEnumDecl = class(TDecl)
    private
      FElements: TEnumElements;
      FMembers: TMemberMap;
    public
      property Elements: TEnumElements read FElements;
      property Members: TMemberMap read FMembers;
      constructor Create(AName: TIdent; AMembers: TMemberMap);
      destructor Destroy; override;
      procedure AddElement(const AName: String; AExpr: TExpr);
  end;

  // A trait contains a number of functions that can be used by a class/record to
  // extend functionality. A trait is not a type!
  // Traits can contain other traits. Functions cannot be overwritten in traits.
  // A collision results in an error. The ebnf of traits:
  // TraitDecl = 'trait' Ident [ ':' Traits ] 'is' {Member} 'end' .
  // Traits = Ident { ',' Ident } .
  TTraitDecl = class(TDecl)
    private
      FTraits: TExprList;
      FMembers: TMemberMap;
    public
      property Traits: TExprList read FTraits;
      property Members: TMemberMap read FMembers;
      constructor Create(AName: TIdent; ATraits: TExprList; AMembers: TMemberMap);
      destructor Destroy; override;
  end;

  // an unoprdered map of trait declarations
  TTraitMap = specialize TObjectDictionary<String, TTraitDecl>;

  // A block of statements and/or declarations. A block has a separate scope.
  TBlock = class(TNode)
    private
      FNodes: TNodeList;
    public
      property Nodes: TNodeList read FNodes;
      constructor Create(ANodes: TNodeList; ALocation: TLocation);
      destructor Destroy; override;
  end;

  // The body of a function is also a block, but scope is handled differently.
  TBody = class(TBlock)
    // used for function bodies
  end;

  // the product or program is also a block.
  TProduct = class(TBlock)
  end;

var
  // all class declarations are stored in the class map, used to check if parent classes
  // exist, as well as to immediately add extensions.
  ClassMap: TClassMap;
  // all trait declarations are stored in a trait map, used to check if trait lists
  // are actual traits
  TraitMap: TTraitMap;

implementation
uses strutils, uCommon;

{ TNode }

constructor TNode.Create(ALocation: TLocation);
begin
  FLocation := ALocation;
end;

{ TLiteralExpr }

constructor TLiteralExpr.Create(ALiteralType: TLiteralType; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FLiteralType := ALiteralType;
end;

{ TNumberExpr }

constructor TNumberExpr.Create(NumberToken: TToken);
begin
  inherited Create(NumberToken.Location);
  if NumberToken.Lexeme[1] in ['$', '%', '&'] then
    FValue := StrToInt64(NumberToken.Lexeme)
  else
    FValue := StrToFloat(NumberToken.Lexeme);
end;

constructor TNumberExpr.Create(Number: Double; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FValue := Number;
end;

{ TStringExpr }

constructor TStringExpr.Create(StringToken: TToken);
begin
  inherited Create(StringToken.Location);
  FValue := StringToken.Lexeme;
end;

constructor TStringExpr.Create(AValue: String; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FValue := AValue;
end;

{ TCharExpr }

constructor TCharExpr.Create(CharToken: TToken);
begin
  inherited Create(CharToken.Location);
  FValue := CharToken.Lexeme;
end;

constructor TCharExpr.Create(AValue: String4; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FValue := AValue;
end;

{ TUnaryExpr }

constructor TUnaryExpr.Create(AOp: TTokenTyp; AExpr: TExpr; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FOp := AOp;
  FExpr := AExpr;
end;

destructor TUnaryExpr.Destroy;
begin
  if Assigned(FExpr) then FExpr.Free;
  inherited Destroy;
end;

{ TBinaryExpr }

constructor TBinaryExpr.Create(ALeft: TExpr; AOp: TTokenTyp; ARight: TExpr;
  ALocation: TLocation);
begin
  inherited Create(ALocation);
  FOp := AOp;
  FLeft := ALeft;
  FRight := ARight;
end;

destructor TBinaryExpr.Destroy;
begin
  if Assigned(FLeft) then FLeft.Free;
  if Assigned(FRight) then FRight.Free;
  inherited Destroy;
end;

{ TAndExpr }

constructor TAndExpr.Create(ALeft, ARight: TExpr; ALocation: TLocation);
begin
  inherited Create(ALeft, ttAnd, ARight, ALocation);
end;

{ TOrExpr }

constructor TOrExpr.Create(ALeft, ARight: TExpr; ALocation: TLocation);
begin
  inherited Create(ALeft, ttOr, ARight, ALocation);
end;

{ TTernaryExpr }

constructor TTernaryExpr.Create(ACondition, ATrueExpr, AFalseExpr: TExpr);
begin
  inherited Create(ACondition.Location);
  FCondition := ACondition;
  FTrueExpr := ATrueExpr;
  FFalseExpr := AFalseExpr;
end;

destructor TTernaryExpr.Destroy;
begin
  if Assigned(FCondition) then FCondition.Free;
  if Assigned(FTrueExpr) then FTrueExpr.Free;
  if Assigned(FFalseExpr) then FFalseExpr.Free;
  inherited Destroy;
end;

{ TIdent }

constructor TIdent.Create(Token: TToken);
begin
  inherited Create(Token.Location);
  FText := Token.Lexeme;
end;

constructor TIdent.Create(const AText: String; const ALocation: TLocation);
begin
  inherited Create(ALocation);
  FText := AText;
end;

{ TVariable }

constructor TVariable.Create(const AName: TToken);
begin
  inherited Create(AName.Location);
  FName := TIdent.Create(AName);
end;

constructor TVariable.Create(AName: TIdent);
begin
  inherited Create(AName.Location);
  FName := AName;
end;

{ TArgument }

constructor TArgument.Create(AName: TIdent; AExpr: TExpr);
begin
  FName := AName;
  FExpr := AExpr;
end;

destructor TArgument.Destroy;
begin
  if Assigned(FName) then FName.Free;
  if Assigned(FExpr) then FExpr.Free;
  inherited Destroy;
end;

{ TCallExpr }

constructor TCallExpr.Create(ACallee: TExpr; ArgList: TArgumentList; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FCallee := ACallee;
  FArguments := ArgList;
  FArgCount := Byte(ArgList.Count);
  Signature := FCallee;
  IsClassInit := False;
end;

destructor TCallExpr.Destroy;
begin
  if Assigned(FCallee) then FCallee.Free;
  if Assigned(FArguments) then FArguments.Free;
  inherited Destroy;
end;

{ TInheritedExpr }

constructor TInheritedExpr.Create(AName: TToken; AMethod: TVariable; ArgList: TExprList;
  const Count: Integer);
begin
  inherited Create(AName);
  FMethod := AMethod;
  FArguments := ArgList;
  FArgCount := Count;
end;

destructor TInheritedExpr.Destroy;
begin
  if Assigned(FMethod) then FMethod.Free;
  if Assigned(FArguments) then FArguments.Free;
  inherited Destroy;
end;

{ TGetExpr }

constructor TGetExpr.Create(AInstance, AMember: TExpr; ASafetyOn: Boolean);
begin
  inherited Create(AInstance.Location);
  FInstance := AInstance;
  FMember := AMember;
  FSafetyOn := ASafetyOn;
end;

destructor TGetExpr.Destroy;
begin
  if Assigned(FInstance) then FInstance.Free;
  if Assigned(FMember) then FMember.Free;
  inherited Destroy;
end;

{ TArrayExpr }

constructor TArrayExpr.Create(AElements: TExprList; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FElements := AElements;
end;

destructor TArrayExpr.Destroy;
begin
  if Assigned(FElements) then FElements.Free;
  inherited Destroy;
end;

{ TDictionaryExpr }

constructor TDictionaryExpr.Create(AElements: TKeyValueMap; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FElements := AElements;
end;

destructor TDictionaryExpr.Destroy;
begin
  if Assigned(FElements) then FElements.Free;
  inherited Destroy;
end;

{ TIndexedExpr }

constructor TIndexedExpr.Create(AVariable: TExpr; AIndex: TExpr);
begin
  inherited Create(AVariable.Location);
  FVariable := AVariable;
  FIndex := AIndex;
end;

destructor TIndexedExpr.Destroy;
begin
  if Assigned(FVariable) then FVariable.Free;
  if Assigned(FIndex) then FIndex.Free;
  inherited Destroy;
end;

{ TMatchExpr }

constructor TMatchExpr.Create(AExpr: TExpr; AIfLimbs: TBinaryExprMap;
  AElseLimb: TExpr);
begin
  inherited Create(AExpr.Location);
  FExpr := AExpr;
  FIfLimbs := AIfLimbs;
  FElseLimb := AElseLimb;
end;

destructor TMatchExpr.Destroy;
begin
  if Assigned(FExpr) then FExpr.Free;
  if Assigned(FIfLimbs) then FIfLimbs.Free;
  if Assigned(FElseLimb) then FElseLimb.Free;
  inherited Destroy;
end;

{ TTupleExpr }

constructor TTupleExpr.Create(AElements: TArgumentList; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FElements := AElements;
end;

destructor TTupleExpr.Destroy;
begin
  if Assigned(FElements) then FElements.Free;
  inherited Destroy;
end;

{ TSetExpr }

constructor TSetExpr.Create(AElements: TExprList; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FElements := AElements;
end;

destructor TSetExpr.Destroy;
begin
  if Assigned(FElements) then FElements.Free;
  inherited Destroy;
end;

{ TRangeExpr }

constructor TRangeExpr.Create(AFrom, AUpTo: TExpr; AIsInclusive: Boolean;
  ALocation: TLocation);
begin
  inherited Create(ALocation);
  FFrom := AFrom;
  FUpTo := AUpTo;
  FIsInclusive := AIsInclusive
end;

destructor TRangeExpr.Destroy;
begin
  if Assigned(FFrom) then FFrom.Free;
  if Assigned(FUpTo) then FUpTo.Free;
  inherited Destroy;
end;



// ==========
// STATEMENTS
// ==========

{ TAssignStmt }

constructor TAssignStmt.Create(AVariable: TVariable; AExpr: TExpr;
  const AAssignOp: TTokenTyp);
begin
  inherited Create(AVariable.Location);
  FVariable := AVariable;
  FExpr := AExpr;
  FAssignOp := AAssignOp;
end;

destructor TAssignStmt.Destroy;
begin
  if Assigned(FVariable) then FVariable.Free;
  if Assigned(FExpr) then FExpr.Free;
  inherited Destroy;
end;

{ TCallExprStmt }

constructor TCallExprStmt.Create(ACallExpr: TCallExpr);
begin
  inherited Create(ACallExpr.Location);
  FCallExpr := ACallExpr;
end;

destructor TCallExprStmt.Destroy;
begin
  if Assigned(FCallExpr) then FCallExpr.Free;
  inherited Destroy;
end;

{ TExprStmt }

constructor TExprStmt.Create(AExpr: TExpr);
begin
  inherited Create(AExpr.Location);
  FExpr := AExpr;
end;

destructor TExprStmt.Destroy;
begin
  if Assigned(FExpr) then FExpr.Free;
  inherited Destroy;
end;

{ TSetStmt }

constructor TSetStmt.Create(AGetExpr: TGetExpr; AOp: TTokenTyp; AExpr: TExpr);
begin
  inherited Create(AGetExpr.Location);
  FGetExpr := AGetExpr;
  FOp := AOp;
  FExpr := AExpr;
end;

destructor TSetStmt.Destroy;
begin
  if Assigned(FGetExpr) then FGetExpr.Free;
  if Assigned(FExpr) then FExpr.Free;
  inherited Destroy;
end;

{ TIndexedStmt }

constructor TIndexedStmt.Create(AIndexedExpr: TIndexedExpr; AOp: TTokenTyp;
  AExpr: TExpr);
begin
  Inherited Create(AIndexedExpr.Location);
  FIndexedExpr := AIndexedExpr;
  FOp := AOp;
  FExpr := AExpr;
end;

destructor TIndexedStmt.Destroy;
begin
  if Assigned(FIndexedExpr) then FIndexedExpr.Free;
  if Assigned(FExpr) then FExpr.Free;
  inherited Destroy;
end;

{ TPrintStmt }

constructor TPrintStmt.Create(AExprList: TExprList; ATerminator: TExpr;
  ALocation: TLocation);
begin
  inherited Create(ALocation);
  FExprList := AExprList;
  FTerminator := ATerminator;
end;

destructor TPrintStmt.Destroy;
begin
  if Assigned(FExprList) then FExprList.Free;
  inherited Destroy;
end;

{ TElseIfItem }

constructor TElseIfItem.Create(ACondition: TExpr; ABlock: TBlock);
begin
  FCondition := ACondition;
  FBlock := ABlock;
end;

destructor TElseIfItem.Destroy;
begin
  if Assigned(FCondition) then FCondition.Free;
  if Assigned(FBlock) then FBlock.Free;
  inherited Destroy;
end;

{ TIfStmt }

constructor TIfStmt.Create(AVarDecl: TVarDecl; ACondition: TExpr; AThenPart,
  AElsePart: TBlock; AElseIfList: TElseIfList; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FVarDecl := AVarDecl;
  FCondition := ACondition;
  FThenPart := AThenPart;
  FElsePart := AElsePart;
  FElseIfList := AElseIfList;
end;

destructor TIfStmt.Destroy;
begin
  if Assigned(FVarDecl) then FVarDecl.Free;
  if Assigned(FCondition) then FCondition.Free;
  if Assigned(FThenPart) then FThenPart.Free;
  if Assigned(FElsePart) then FElsePart.Free;
  if Assigned(FElseIfList) then FElseIfList.Free;
  inherited Destroy;
end;

{ TWhileStmt }

constructor TWhileStmt.Create(AVarDecl: TVarDecl; ACondition: TExpr; ABlock: TBlock;
  ALocation: TLocation);
begin
  inherited Create(ALocation);
  FVarDecl := AVarDecl;
  FCondition := ACondition;
  FBlock := ABlock;
end;

destructor TWhileStmt.Destroy;
begin
  if Assigned(FVarDecl) then FVarDecl.Free;
  if Assigned(FCondition) then FCondition.Free;
  if Assigned(FBlock) then FBlock.Free;
  inherited Destroy;
end;

{ TForStmt }

constructor TForStmt.Create(AVarDecl: TVarDecl; ACondition: TExpr;
  AIterator: TStmt; ABlock: TBlock; ALocation: TLocation);
begin
  inherited Create(AVarDecl, ACondition, ABlock, ALocation);
  FIterator := AIterator;
end;

destructor TForStmt.Destroy;
begin
  if Assigned(FIterator) then FIterator.Free;
  inherited Destroy;
end;

{ TForInStmt }

constructor TForInStmt.Create(ALoopVar: TIdent; ASequence: TExpr;
  AWhere: TExpr; ABlock: TBody; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FLoopVar := ALoopVar;
  FSequence := ASequence;
  FWhere := AWhere;
  FBlock := ABlock;
end;

destructor TForInStmt.Destroy;
begin
  if Assigned(FLoopVar) then FLoopVar.Free;
  if Assigned(FSequence) then FSequence.Free;
  if Assigned(FWhere) then FWhere.Free;
  if Assigned(FBlock) then FBlock.Free;
  inherited Destroy;
end;

{ TListBuilderExpr }

//constructor TListBuilderExpr.Create(ATyp: TListBuilderTyp;
//  AForInStmt: TForInStmt; ALocation: TLocation);
//begin
//  inherited Create(ALocation);
//  FTyp := ATyp;
//  FForInStmt := AForInStmt;
//end;
//
//destructor TListBuilderExpr.Destroy;
//begin
//  if Assigned(FForInStmt) then FForInStmt.Free;
//  inherited Destroy;
//end;

constructor TListBuilderExpr.Create(ATyp: TListBuilderTyp; AMap, ASequence,
  AFilter: TExpr; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FTyp := ATyp;
  FMap := AMap;
  FSequence := ASequence;
  FFilter := AFilter;
end;

destructor TListBuilderExpr.Destroy;
begin
  if Assigned(FMap) then FMap.Free;
  if Assigned(FSequence) then FSequence.Free;
  if Assigned(FFilter) then FFilter.Free;
  inherited Destroy;
end;

{ TRepeatStmt }

constructor TRepeatStmt.Create(ABlock: TBlock; ACondition: TExpr;
  ALocation: TLocation);
begin
  inherited Create(ALocation);
  FBlock := ABlock;
  FCondition := ACondition;
end;

destructor TRepeatStmt.Destroy;
begin
  if Assigned(FBlock) then FBlock.Free;
  if Assigned(FCondition) then FCondition.Free;
  inherited Destroy;
end;

{ TConditionalBlock }

constructor TConditionalBlock.Create(ACondition: TExpr; ABlock: TBlock);
begin
  FCondition := ACondition;
  FBlock := ABlock;
end;

destructor TConditionalBlock.Destroy;
begin
  if Assigned(FCondition) then FCondition.Free;
  if Assigned(FBlock) then FBlock.Free;
  inherited Destroy;
end;

{ TLoopDoStmt }

constructor TLoopDoStmt.Create(AVarDecls: TVarDecls; ABlock: TBlock;
  AConditionalBlocks: TConditionalBlocks; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FVarDecls := AVarDecls;
  FBlock := ABlock;
  FConditionalBlocks := AConditionalBlocks;
end;

destructor TLoopDoStmt.Destroy;
begin
  if Assigned(FVarDecls) then FVarDecls.Free;
  if Assigned(FBlock) then FBlock.Free;
  if Assigned(FConditionalBlocks) then FConditionalBlocks.Free;
  inherited Destroy;
end;

{ TEnsureStmt }

constructor TEnsureStmt.Create(AVarDecl: TVarDecl; ACondition: TExpr; AElsePart: TBlock;
  ALocation: TLocation);
begin
  inherited Create(ALocation);
  FVarDecl := AVarDecl;
  FCondition := ACondition;
  FElsePart := AElsePart;
end;

destructor TEnsureStmt.Destroy;
begin
  if Assigned(FVarDecl) then FVarDecl.Free;
  if Assigned(FCondition) then FCondition.Free;
  if Assigned(FElsePart) then FElsePart.Free;
  inherited Destroy;
end;


{ TReturnStmt }

constructor TReturnStmt.Create(AExpr: TExpr; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FExpr := AExpr;
end;

destructor TReturnStmt.Destroy;
begin
  if Assigned(FExpr) then FExpr.Free;
  inherited Destroy;
end;

{ TBreakStmt }

//constructor TBreakStmt.Create(ACondition: TExpr; ALocation: TLocation);
//begin
//  inherited Create(ALocation);
//  FCondition := ACondition;
//end;
//
//destructor TBreakStmt.Destroy;
//begin
//  if Assigned(FCondition) then FCondition.Free;
//  inherited Destroy;
//end;

{ TCaseItem }

constructor TCaseItem.Create(AExpr: TExpr; const ATyp: TCaseItemType);
begin
  Expr := AExpr;
  Typ := ATyp;
end;

destructor TCaseItem.Destroy;
begin
  if Assigned(Expr) then Expr.Free;
  inherited Destroy;
end;

{ TSwitchStmt }

constructor TSwitchStmt.Create(AExpr: TExpr; ACases: TSwitchCases; ADefault: TBlock;
  ALocation: TLocation);
begin
  inherited Create(ALocation);
  FExpr := AExpr;
  FCases := ACases;
  FDefaultCase := ADefault;
end;

destructor TSwitchStmt.Destroy;
begin
  if Assigned(FExpr) then FExpr.Free;
  if Assigned(FCases) then FCases.Free;
  if Assigned(FDefaultCase) then FDefaultCase.Free;
  inherited Destroy;
end;

{ TUseStmt }

constructor TUseStmt.Create(AFileName: String; AToken: TToken);
begin
  inherited Create(AToken.Location);
  FFileName := AFileName;
end;

{ TDeferStmt }

constructor TDeferStmt.Create(AClosure: TFuncDecl; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FClosure := AClosure;
end;

destructor TDeferStmt.Destroy;
begin
  if Assigned(FClosure) then FClosure.Free;
  inherited Destroy;
end;


// ============
// DECLARATIONS
// ============

{ TDecl }

constructor TDecl.Create(AName: TIdent; AKind: TDeclKind; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FName := AName;
  FKind := AKind;
end;

destructor TDecl.Destroy;
begin
  if Assigned(FName) then FName.Free;
  inherited Destroy;
end;


{ TVarDecl }

constructor TVarDecl.Create(AName: TIdent; AExpr: TExpr; const AMutable: Boolean
  );
begin
  inherited Create(AName, dkVar, AName.Location);
  FExpr := AExpr;
  FMutable := AMutable;
  //FIsPrivate := AIsPrivate;
end;

destructor TVarDecl.Destroy;
begin
  if Assigned(FExpr) then FExpr.Free;
  inherited Destroy;
end;

{ TVarDecls }

constructor TVarDecls.Create(AList: TDeclList; ALocation: TLocation);
begin
  Inherited Create(Nil, dkVars, ALocation);
  FList := AList;
end;

destructor TVarDecls.Destroy;
begin
  if Assigned(FList) then FList.Free;
  inherited Destroy;
end;

{ TParameter }

constructor TParameter.Create(AVariable: TVariable; AName: TIdent);
begin
  FVariable := AVariable;
  FName := AName;
end;

destructor TParameter.Destroy;
begin
  if Assigned(FVariable) then
    begin
      if not FVariable.FName.Equals(FName) then
        begin
          FVariable.Free;
          if Assigned(FName) then FName.Free;
        end
      else
        FVariable.Free
    end;
  inherited Destroy;
end;

{ TFuncDecl }

constructor TFuncDecl.Create(AName: TIdent; AParameters: TParameterList;
  const AForm: TFuncForm; ABody: TBody);
begin
  inherited Create(AName, dkFunc, AName.Location);
  FParameters := AParameters;
  FForm := AForm;
  FBody := ABody;
end;

constructor TFuncDecl.Create(AParameters: TParameterList; const AForm: TFuncForm;
  ABody: TBody; ALocation: TLocation);
begin
  inherited Create(Nil, dkFunc, ALocation);
  FParameters := AParameters;
  FForm := AForm;
  FBody := ABody;
end;

destructor TFuncDecl.Destroy;
begin
  if Assigned(FParameters) then FParameters.Free;
  if Assigned(FBody) then FBody.Free;
  inherited Destroy;
end;

{ TLambdaExpr }

constructor TLambdaExpr.Create(AFunc: TFuncDecl);
begin
  inherited Create(AFunc.Location);
  FFunc := AFunc;
end;

destructor TLambdaExpr.Destroy;
begin
  if Assigned(FFunc) then FFunc.Free;
  inherited Destroy;
end;

{ TOperatorDecl }

constructor TOperatorDecl.Create(AOpID: TIdent; AOperatorType: TOperatorType;
  AParameters: TParameterList; ABody: TBody; AFuncForm: TFuncForm);
begin
  inherited Create(AOpID, AParameters, AFuncForm, ABody);
  FOperatorType := AOperatorType;
end;

// specifically used in creating core class methods/operators
constructor TOperatorDecl.Create(AOpID: TIdent; AParameters: TParameterList;
  ABody: TBody);
begin
  inherited Create(AOpID, AParameters, ffMethodArrow, ABody);
  if AOpID.Text.Contains('infix') then
    FOperatorType := otInfix
  else if AOpID.Text.Contains('prefix') then
    FOperatorType := otPrefix
  else
    FOperatorType := otPostfix;
end;


{ TValDecl }

constructor TValDecl.Create(const AName: TIdent; AFunc: TFuncDecl);
begin
  inherited Create(AName, dkVal, AName.Location);
  FFunc := AFunc;
end;

destructor TValDecl.Destroy;
begin
  if Assigned(FFunc) then FFunc.Free;
  inherited Destroy;
end;

{ TClassDecl }

constructor TClassDecl.Create(AName: TIdent; AParent: TVariable;
  ATraits: TExprList; ADefaultValue: TVarDecl; AMembers, AStatics: TMemberMap);
begin
  inherited Create(AName, dkClass, AName.Location);
  FParent := AParent;
  FTraits := ATraits;
  FDefaultValue := ADefaultValue;
  FMembers := AMembers;
  FStatics := AStatics;
  FIsRecord := False;
end;

destructor TClassDecl.Destroy;
begin
  if Assigned(FParent) then FParent.Free;
  if Assigned(FTraits) then FTraits.Free;
  if Assigned(FDefaultValue) then FDefaultValue.Free;
  if Assigned(FMembers) then FMembers.Free;
  if Assigned(FStatics) then FStatics.Free;
  inherited Destroy;
end;

{ TExtensionDecl }

constructor TExtensionDecl.Create(AName: TIdent; AMembers: TMemberMap;
  ATraits: TExprList);
begin
  inherited Create(AName, dkExtension, AName.Location);
  FMembers := AMembers;
  FTraits := ATraits;
end;

destructor TExtensionDecl.Destroy;
begin
  if Assigned(FMembers) then FMembers.Free;
  inherited Destroy;
end;

{ TEnumDecl }

constructor TEnumDecl.Create(AName: TIdent; AMembers: TMemberMap);
begin
  inherited Create(AName, dkEnum, AName.Location);
  FMembers := AMembers;
  FElements := TEnumElements.create;
end;

destructor TEnumDecl.Destroy;
begin
  if Assigned(FMembers) then FMembers.Free;
  if Assigned(FElements) then FElements.Free;
  inherited Destroy;
end;

procedure TEnumDecl.AddElement(const AName: String; AExpr: TExpr);
begin
  FElements.Add(AName, AExpr);
end;

{ TTraitDecl }

constructor TTraitDecl.Create
  (AName: TIdent; ATraits: TExprList; AMembers: TMemberMap);
begin
  inherited Create(AName, dkTrait, AName.Location);
  FTraits := ATraits;
  FMembers := AMembers;
end;

destructor TTraitDecl.Destroy;
begin
  if Assigned(FTraits) then FTraits.Free;
  if Assigned(FMembers) then FMembers.Free;
  inherited Destroy;
end;


{ TBlock }

constructor TBlock.Create(ANodes: TNodeList; ALocation: TLocation);
begin
  inherited Create(ALocation);
  FNodes := ANodes;
end;

destructor TBlock.Destroy;
begin
  if Assigned(FNodes) then FNodes.Free;
  inherited Destroy;
end;

initialization
  ClassMap := TClassMap.Create;
  TraitMap := TTraitMap.create;
finalization
  ClassMap.Free;
  TraitMap.Free;
end.


