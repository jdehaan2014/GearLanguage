unit uAST;

{ This unit defines the nodes of the AST structure.

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
  Classes, SysUtils, uToken, uCollections;

type

  TNode = class
    private
      FToken: TToken;
    public
      property Token: TToken read FToken;
      constructor Create(AToken: TToken);
  end;

  TExpr = class(TNode)
    // Base node for expressions.
  end;

  TExprList = specialize TArrayObj<TExpr>;

  TBinaryExpr = class(TExpr)
    private
      FLeft: TExpr;
      FOp: TToken;
      FRight: TExpr;
    public
      property Left: TExpr read FLeft;
      property Op: TToken read FOp;
      property Right: TExpr read FRight;
      constructor Create(ALeft: TExpr; AOp: TToken; ARight: TExpr);
      destructor Destroy; override;
  end;

  TFactorExpr = class(TExpr)
    // Base node for parsing a factor
  end;

  TUnaryExpr = class(TFactorExpr)
    private
      FOp: TToken;         //operator Not, Minus, Plus
      FExpr: TExpr;        //single expression
    public
      property Op: TToken read FOp;
      property Expr: TExpr read FExpr;
      constructor Create(AOp: TToken; AExpr: TExpr);
      destructor Destroy; override;
  end;

  TConstExpr = class(TFactorExpr)
    private
      FValue: Variant;
    public
      property Value: Variant read FValue;
      constructor Create(Constant: Variant; AToken: TToken);
  end;

  TInterpolatedExpr = class(TFactorExpr)
    private
      FExprList: TExprList;
    public
      property ExprList: TExprList read FExprList;
      constructor Create(AExprList: TExprList; AToken: TToken);
      destructor Destroy; override;
  end;

  TIdent = class(TNode)
    private
      FText: String;
    public
      property Text: String read FText;
      constructor Create(AToken: TToken);
  end;

  TVariable = class(TFactorExpr)
    private
      FIdent: TIdent;
      FDistance: Integer;
    public
      property Ident: TIdent read FIdent;
      property Distance: Integer read FDistance write FDistance;
      constructor Create(AIdent: TIdent);
      destructor Destroy; override;
  end;

  TIfExpr = class(TFactorExpr)
    private
      FCondition,
      FTrueExpr,
      FFalseExpr: TExpr;
    public
      property Condition: TExpr read FCondition;
      property TrueExpr: TExpr read FTrueExpr;
      property FalseExpr: TExpr read FFalseExpr;
      constructor Create(ACondition, ATrueExpr, AFalseExpr: TExpr; AToken: TToken);
      destructor Destroy; override;
  end;

  TMatchExpr = class(TFactorExpr)
    private
      type
        TIfLimb = class
          Values: TExprList;
          Expr: TExpr;
          constructor Create(AValues: TExprList; AExpr: TExpr);
          destructor Destroy; override;
        end;
        TIfLimbs = specialize TArrayObj<TIfLimb>;
    private
      FExpr: TExpr;
      FIfLimbs: TIfLimbs;
      FElseLimb: TExpr;
    public
      property Expr: TExpr read FExpr;
      property IfLimbs: TIfLimbs read FIfLimbs;
      property ElseLimb: TExpr read FElseLimb write FElseLimb;
      constructor Create(aExpr: TExpr; AToken: TToken);
      destructor Destroy; override;
      procedure AddLimb(AValues: TExprList; AExpr: TExpr);
  end;

  TCallExpr = class(TExpr)
    private
      type
        TArg = class
          Expr: TExpr;
          Ident: TIdent;
          constructor Create(AExpr: TExpr; AIdent: TIdent);
          destructor Destroy; override;
        end;
        TArgList = specialize TArrayObj<TArg>;
    private
      FCallee: TExpr;
      FArgs: TArgList;
    public
      property Callee: TExpr read FCallee;
      property Args: TArgList read FArgs;
      constructor Create(ACallee: TExpr; AToken: TToken);
      destructor Destroy; override;
      procedure AddArgument(Expr: TExpr; Ident: TIdent);
  end;

  TGetExpr = class(TExpr)
    private
      FInstance: TExpr;
      FIdent: TIdent;
    public
      property Instance: TExpr read FInstance;
      property Ident: TIdent read FIdent;
      constructor Create(AInstance: TExpr; AIdent: TIdent);
      destructor Destroy; override;
  end;

  TSelfExpr = class(TFactorExpr)
    private
      FVariable: TVariable;
    public
      property Variable: TVariable read FVariable;
      constructor Create(AVariable: TVariable);
      destructor Destroy; override;
  end;

  TInheritedExpr = class(TFactorExpr)
    private
      FVariable: TVariable;
      FMethod: TIdent;
    public
      property Variable: TVariable read FVariable;
      property Method: TIdent read FMethod;
      constructor Create(AVariable: TVariable; AMethod: TIdent);
      destructor Destroy; override;
  end;

  TIndexedExpr = class(TFactorExpr)
    private
      FVariable: TExpr;
      FIndex: TExpr;
    public
      property Variable: TExpr read FVariable;
      property Index: TExpr read FIndex;
      constructor Create(AVariable: TExpr; AIndex: TExpr);
      destructor Destroy; override;
  end;

  TTupleExpr = class(TFactorExpr)
    private
      FExprList: TExprList;
    public
      property ExprList: TExprList read FExprList;
      constructor Create(AExprList: TExprList; AToken: TToken);
      destructor Destroy; override;
  end;

  TStmt = class(TNode)
    // Base class for statements
  end;

  TBlock = class;
  TVarDecl = class;

  TPrintStmt = class(TStmt)
    private
      FExprList: TExprList;
      FTerminator: TExpr;
    public
      property ExprList: TExprList read FExprList;
      property Terminator: TExpr read FTerminator;
      constructor Create(AExprList: TExprList;
        ATerminator: TExpr; AToken: TToken);
      destructor Destroy; override;
  end;

  TAssignStmt = class(TStmt)
    private
      FVariable: TVariable;
      FOp: TToken;
      FExpr: TExpr;
    public
      property Variable: TVariable read FVariable;
      property Op: TToken read FOp;
      property Expr: TExpr read FExpr;
      constructor Create(AVariable: TVariable; AOp: TToken; AExpr: TExpr);
      destructor Destroy; override;
  end;

  TCallExprStmt = class(TStmt)
    private
      FCallExpr: TCallExpr;
    public
      property CallExpr: TCallExpr read FCallExpr;
      constructor Create(ACallExpr: TCallExpr; AToken: TToken);
      destructor Destroy; override;
  end;

  TSetStmt = class(TStmt)
    private
      FInstance: TExpr;
      FIdent: TIdent;
      FExpr: TExpr;
      FOp: TToken;
    public
      property Instance: TExpr read FInstance;
      property Ident: TIdent read FIdent;
      property Expr: TExpr read FExpr;
      property Op: TToken read FOp;
      constructor Create(AInstance: TExpr; AIdent: TIdent; AOp: TToken;
        AExpr: TExpr);
      destructor Destroy; override;
  end;

  TIndexedExprStmt = class(TStmt)
    private
      FIndexedExpr: TIndexedExpr;
      FOp: TToken;
      FExpr: TExpr;
    public
      property IndexedExpr: TIndexedExpr read FIndexedExpr;
      property Expr: TExpr read FExpr;
      property Op: TToken read FOp;
      constructor Create(AIndexedExpr: TIndexedExpr; AOp: TToken; AExpr: TExpr);
      destructor Destroy; override;
  end;

  TBlockList = specialize TArrayObj<TBlock>;

  TIfStmt = class(TStmt)
    private
      FVarDecl: TVarDecl;
      FCondition: TExpr;
      FThenPart: TBlock;
      FElsePart: TBlock;
      FElseIfs: TExprList;
      FElseIfParts: TBlockList;
    public
      property VarDecl: TVarDecl read FVarDecl;
      property Condition: TExpr read FCondition;
      property ThenPart: TBlock read FThenPart write FThenPart;
      property ElsePart: TBlock read FElsePart;
      property ElseIfs: TExprList read FElseIfs;
      property ElseIfParts: TBlockList read FElseIfParts;
      constructor Create(AVarDecl: TVarDecl; ACondition: TExpr;
        AElseIfs: TExprList; AThenPart, AElsePart: TBlock;
        AElseIfParts: TBlockList; AToken: TToken);
      destructor Destroy; override;
  end;

  TWhileStmt = class(TStmt)
    private
      FVarDecl: TVarDecl;
      FCondition: TExpr;
      FBlock: TBlock;
    public
      property VarDecl: TVarDecl read FVarDecl;
      property Condition: TExpr read FCondition;
      property Block: TBlock read FBlock;
      constructor Create(AVarDecl: TVarDecl; ACondition: TExpr;
        ABlock: TBlock; AToken: TToken);
      destructor Destroy; override;
  end;

  TRepeatStmt = class(TStmt)
    private
      FCondition: TExpr;
      FBlock: TBlock;
    public
      property Condition: TExpr read FCondition;
      property Block: TBlock read FBlock;
      constructor Create(ACondition: TExpr; ABlock: TBlock; AToken: TToken);
      destructor Destroy; override;
  end;

  TEnsureStmt = class(TStmt)
     private
       FVarDecl: TVarDecl;
       FCondition: TExpr;
       FElsePart: TBlock;
     public
       property VarDecl: TVarDecl read FVarDecl;
       property Condition: TExpr read FCondition;
       property ElsePart: TBlock read FElsePart;
       constructor Create(AVarDecl: TVarDecl; ACondition: TExpr;
         AElsePart: TBlock; AToken: TToken);
       destructor Destroy; override;
   end;

   TSwitchStmt = class(TStmt)
     private
       type
         TCaseLimb = class
           Values: TExprList;
           IsObj: Boolean;
           Block: TBlock;
           constructor Create(AValues: TExprList; AIsObj: Boolean; ABlock: TBlock);
           destructor Destroy; override;
         end;
         TCaseLimbs = specialize TArrayObj<TCaseLimb>;
     private
       FExpr: TExpr;
       FCaseLimbs: TCaseLimbs;
       FElseLimb: TBlock;
     public
       property Expr: TExpr read FExpr;
       property CaseLimbs: TCaseLimbs read FCaseLimbs write FCaseLimbs;
       property ElseLimb: TBlock read FElseLimb write FElseLimb;
       constructor Create(aExpr: TExpr; AToken: TToken);
       destructor Destroy; override;
       procedure AddLimb(AValues: TExprList; AIsObj: Boolean; ABlock: TBlock);
   end;

   TBreakStmt = class(TStmt)
     private
       FCondition: TExpr;
     public
       property Condition: TExpr read FCondition;
       constructor Create(ACondition: TExpr; AToken: TToken);
       destructor Destroy; override;
   end;

   TContinueStmt = class(TStmt)
     //nothing in here
   end;

   TReturnStmt = class(TStmt)
     private
       FExpr: TExpr;
     public
       property Expr: TExpr read FExpr;
       constructor Create(AExpr: TExpr; AToken: TToken);
       destructor Destroy; override;
   end;

   TUseStmt = class(TStmt)
     private
       FFileName: String;
     public
       property FileName: String read FFileName;
       constructor Create(AFileName: String; AToken: TToken);
   end;

  // Base class for declarations
  TDecl = class(TNode)
    private
       type TDeclKind = (dkNone, dkArray, dkClass, dkDict, dkEnum, dkExtension,
         dkFunc, dkTrait, dkVal, dkVar, dkTuple);
    private
       FKind: TDeclKind;
       FIdent: TIdent;
    public
      property Ident: TIdent read FIdent;
      property Kind: TDeclKind read FKind;
      constructor Create(AIdent: TIdent; AKind: TDeclKind; AToken: TToken);
      destructor Destroy; override;
  end;

  TVarDecl = class(TDecl)
    private
      FExpr: TExpr;
      FMutable: Boolean;
    public
      property Expr: TExpr read FExpr;
      property Mutable: Boolean read FMutable;
      constructor Create(AIdent: TIdent; AExpr: TExpr; AToken: TToken;
        AMutable: Boolean=True);
      destructor Destroy; override;
  end;

  TDeclList = specialize TArrayObj<TDecl>;

  TVarDecls = class(TDecl)
    private
      FList: TDeclList;
    public
      property List: TDeclList read FList;
      constructor Create(AList: TDeclList; AToken: TToken);
      destructor Destroy; override;
  end;


  TFuncDecl = class(TDecl)
    private
      type
        TParam = class
          Ident: TIdent;
          ExtIdent: TIdent;
          constructor Create(AIdent, AExtIdent: TIdent);
          destructor Destroy; override;
        end;
        TParamList = specialize TArrayObj<TParam>;
      var
        FParams: TParamList;
        FBody: TBlock;
    public
      property Params: TParamList read FParams;
      property Body: TBlock read FBody write FBody;
      constructor Create(AIdent: TIdent; AToken: TToken);
      destructor Destroy; override;
      procedure AddParam(AIdent, AExtIdent: TIdent);
  end;

  TFuncDeclExpr = class(TFactorExpr)
    private
      FFuncDecl: TFuncDecl;
    public
      property FuncDecl: TFuncDecl read FFuncDecl;
      constructor Create(AFuncDecl: TFuncDecl);
      destructor Destroy; override;
  end;

  TValDecl = class(TDecl)
    private
      FFuncDecl: TFuncDecl;
    public
      property FuncDecl: TFuncDecl read FFuncDecl;
      constructor Create(AIdent: TIdent; AFuncDecl: TFuncDecl; AToken: TToken);
  end;

  TClassDecl = class(TDecl)
    private
      FDeclList: TDeclList;
      FParent: TVariable;
      FTraits: TExprList;
    public
      property DeclList: TDeclList read FDeclList;
      property Parent: TVariable read FParent;
      property Traits: TExprList read FTraits;
      constructor Create(AIdent: TIdent; AParent: TVariable; ATraits: TExprList;
        ADeclList: TDeclList; AToken: TToken);
      destructor Destroy; override;
  end;

  TExtensionDecl = class(TDecl)
    private
      FDeclList: TDeclList;
    public
      property DeclList: TDeclList read FDeclList;
      constructor Create(AIdent: TIdent; ADeclList: TDeclList; AToken: TToken);
      destructor Destroy; override;
  end;

  TTraitDecl = class(TDecl)
    private
      FTraits: TExprList;
      FDeclList: TDeclList;
    public
      property Traits: TExprList read FTraits;
      property DeclList: TDeclList read FDeclList;
      constructor Create(AIdent: TIdent; ATraits: TExprList;
        ADeclList: TDeclList; AToken: TToken);
      destructor Destroy; override;
  end;

  TArrayDecl = class(TDecl)
    private
      FElements: TExprList;
      FDeclList: TDeclList;
    public
      property Elements: TExprList read FElements;
      property DeclList: TDeclList read FDeclList;
      constructor Create(AIdent: TIdent; ADeclList: TDeclList; AToken: TToken);
      destructor Destroy; override;
      procedure AddElement(Expr: TExpr);
  end;

  TArrayDeclExpr = class(TFactorExpr)
    private
      FArrayDecl: TArrayDecl;
    public
      property ArrayDecl: TArrayDecl read FArrayDecl;
      constructor Create(AnArrayDecl: TArrayDecl);
      destructor Destroy; override;
  end;

  TKeyValuePair = class
    Key, Value: TExpr;
    constructor Create(AKey, AValue: TExpr);
    destructor Destroy; override;
  end;

  TKeyValueList = specialize TArrayObj<TKeyValuePair>;

  TDictDecl = class(TDecl)
    private
      FElements: TKeyValueList;
      FDeclList: TDeclList;
    public
      property Elements: TKeyValueList read FElements write FElements;
      property DeclList: TDeclList read FDeclList;
      constructor Create(AIdent: TIdent; ADeclList: TDeclList; AToken: TToken);
      destructor Destroy; override;
      procedure AddElement(KeyExpr, ValueExpr: TExpr);
  end;

  TDictDeclExpr = class(TFactorExpr)
    private
      FDictDecl: TDictDecl;
    public
      property DictDecl: TDictDecl read FDictDecl;
      constructor Create(ADictDecl: TDictDecl);
      destructor Destroy; override;
  end;

  TEnumElement = class
    Value: Variant;
    SetName: String;
    constructor Create(AValue: Variant; ASetName: String);
    function Copy: TEnumElement;
  end;

  TEnumElements = specialize TDictionaryObj<String, TEnumElement>;
  TEnumElementsHelper = class helper for TEnumElements
    function Copy: TEnumElements;
  end;

  TCaseTable = specialize TArray<String>;

  TEnumDecl = class(TDecl)
    private
      FElements: TEnumElements;
      FDeclList: TDeclList;
      FCaseTable: TCaseTable;
    public
      property Elements: TEnumElements read FElements;
      property DeclList: TDeclList read FDeclList;
      property CaseTable: TCaseTable read FCaseTable;
      constructor Create(AIdent: TIdent; ADeclList: TDeclList; AToken: TToken);
      destructor Destroy; override;
      procedure AddElement(const SetName, Name: String; Value: Variant);
  end;

  TNodeList = specialize TArrayObj<TNode>;

  TBlock = class(TNode)
    private
      FNodes: TNodeList;
    public
      property Nodes: TNodeList read FNodes;
      constructor Create(ANodes: TNodeList; AToken: TToken);
      destructor Destroy; override;
  end;

  TProduct = class(TBlock)
  end;

implementation

{ TNode }

constructor TNode.Create(AToken: TToken);
begin
  FToken := AToken;
end;

{ TBinaryExpr }

constructor TBinaryExpr.Create(ALeft: TExpr; AOp: TToken; ARight: TExpr);
begin
  Inherited Create(AOp);
  FLeft :=  ALeft;
  FOp := AOp;
  FRight := ARight;
end;

destructor TBinaryExpr.Destroy;
begin
  if Assigned(FLeft) then FLeft.Free;
  if Assigned(FRight) then FRight.Free;
  inherited Destroy;
end;

{ TUnaryExpr }

constructor TUnaryExpr.Create(AOp: TToken; AExpr: TExpr);
begin
  Inherited Create(AOp);
  FOp := AOp;
  FExpr :=  AExpr;
end;

destructor TUnaryExpr.Destroy;
begin
  if Assigned(FExpr) then FExpr.Free;
  inherited Destroy;
end;

{ TConstExpr }

constructor TConstExpr.Create(Constant: Variant; AToken: TToken);
begin
  Inherited Create(AToken);
  FValue := Constant;
end;

{ TInterpolatedExpr }

constructor TInterpolatedExpr.Create(AExprList: TExprList; AToken: TToken);
begin
  inherited Create(AToken);
  FExprList := AExprList;
end;

destructor TInterpolatedExpr.Destroy;
begin
  if Assigned(FExprList) then FExprList.Free;
  inherited Destroy;
end;

{ TIdent }

constructor TIdent.Create(AToken: TToken);
begin
  inherited Create(AToken);
  FText := AToken.Lexeme;
end;

{ TVariable }

constructor TVariable.Create(AIdent: TIdent);
begin
  Inherited Create(AIdent.Token);
  FIdent := AIdent;
  FDistance := -1;
end;

destructor TVariable.Destroy;
begin
  if Assigned(FIdent) then FIdent.Free;
  inherited Destroy;
end;

{ TIfExpr }

constructor TIfExpr.Create
  (ACondition, ATrueExpr, AFalseExpr: TExpr; AToken: TToken);
begin
  inherited Create(AToken);
  FCondition := ACondition;
  FTrueExpr := ATrueExpr;
  FFalseExpr := AFalseExpr;
end;

destructor TIfExpr.Destroy;
begin
  if Assigned(FCondition) then FCondition.Free;
  if Assigned(FTrueExpr) then FTrueExpr.Free;
  if Assigned(FFalseExpr) then FFalseExpr.Free;
  inherited Destroy;
end;

{ TMatchExpr }

constructor TMatchExpr.TIfLimb.Create(AValues: TExprList; AExpr: TExpr);
begin
  Values := AValues;
  Expr := AExpr;
end;

destructor TMatchExpr.TIfLimb.Destroy;
begin
  if Assigned(Values) then Values.Free;
  if Assigned(Expr) then Expr.Free;
  inherited Destroy;
end;

constructor TMatchExpr.Create(aExpr: TExpr; AToken: TToken);
begin
  Inherited Create(AToken);
  FExpr := AExpr;
  FElseLimb := Nil;
  FIfLimbs := TIfLimbs.Create();
end;

destructor TMatchExpr.Destroy;
begin
  if Assigned(FExpr) then FExpr.Free;
  if Assigned(FIfLimbs) then FIfLimbs.Free;
  if Assigned(FElseLimb) then FElseLimb.Free;
  inherited Destroy;
end;

procedure TMatchExpr.AddLimb(AValues: TExprList; AExpr: TExpr);
begin
  FIfLimbs.Add(TIfLimb.Create(AValues, AExpr));
end;

{ TCallExpr }

constructor TCallExpr.TArg.Create(AExpr: TExpr; AIdent: TIdent);
begin
  Expr := AExpr;
  Ident := AIdent;
end;

destructor TCallExpr.TArg.Destroy;
begin
  if Assigned(Expr) then Expr.Free;
  if Assigned(Ident) then Ident.Free;
  inherited Destroy;
end;

constructor TCallExpr.Create(ACallee: TExpr; AToken: TToken);
begin
  inherited Create(AToken);
  FCallee := ACallee;
  FArgs := TArgList.Create();
end;

destructor TCallExpr.Destroy;
begin
  FArgs.Free;
  if Assigned(FCallee) then FCallee.Free;
  inherited Destroy;
end;

procedure TCallExpr.AddArgument(Expr: TExpr; Ident: TIdent);
begin
  FArgs.Add(TArg.Create(Expr, Ident));
end;

{ TGetExpr }

constructor TGetExpr.Create(AInstance: TExpr; AIdent: TIdent);
begin
  Inherited Create(AIdent.Token);
  FInstance := AInstance;
  FIdent := AIdent;
end;

destructor TGetExpr.Destroy;
begin
  if Assigned(FInstance) then FInstance.Free;
  if Assigned(FIdent) then FIdent.Free;
  inherited Destroy;
end;

{ TSelfExpr }

constructor TSelfExpr.Create(AVariable: TVariable);
begin
  inherited Create(AVariable.Ident.Token);
  FVariable := AVariable;
end;

destructor TSelfExpr.Destroy;
begin
  if Assigned(FVariable) then FVariable.Free;
  inherited Destroy;
end;

constructor TInheritedExpr.Create(AVariable: TVariable; AMethod: TIdent);
begin
  inherited Create(AVariable.Ident.Token);
  FVariable := AVariable;
  FMethod := AMethod;
end;

destructor TInheritedExpr.Destroy;
begin
  if Assigned(FVariable) then FVariable.Free;
  if Assigned(FMethod) then FMethod.Free;
  inherited Destroy;
end;

{ TIndexedExpr }

constructor TIndexedExpr.Create(AVariable: TExpr; AIndex: TExpr);
begin
  inherited Create(AVariable.Token);
  FVariable := AVariable;
  FIndex := AIndex;
end;

destructor TIndexedExpr.Destroy;
begin
  if Assigned(FVariable) then FVariable.Free;
  if Assigned(FIndex) then FIndex.Free;
  inherited Destroy;
end;

{ TTupleExpr }

constructor TTupleExpr.Create(AExprList: TExprList; AToken: TToken);
begin
  inherited Create(AToken);
  FExprList := AExprList;
end;

destructor TTupleExpr.Destroy;
begin
  if Assigned(FExprList) then FExprList.Free;
  inherited Destroy;
end;


{ TPrintStmt }

constructor TPrintStmt.Create
  (AExprList: TExprList; ATerminator: TExpr; AToken: TToken);
begin
  Inherited Create(AToken);
  FExprList := AExprList;
  FTerminator := ATerminator;
end;

destructor TPrintStmt.Destroy;
begin
  if Assigned(FExprList) then FExprList.Free;
  if Assigned(FTerminator) then FTerminator.Free;
  inherited Destroy;
end;

{ TAssignStmt }

constructor TAssignStmt.Create(AVariable: TVariable; AOp: TToken; AExpr: TExpr);
begin
  inherited Create(AOp);
  FVariable := AVariable;
  FOp := AOp;
  FExpr := AExpr;
end;

destructor TAssignStmt.Destroy;
begin
  if Assigned(FVariable) then FVariable.Free;
  if Assigned(FExpr) then FExpr.Free;
  inherited Destroy;
end;

{ TCallExprStmt }

constructor TCallExprStmt.Create(ACallExpr: TCallExpr; AToken: TToken);
begin
  inherited Create(AToken);
  FCallExpr := ACallExpr;
end;

destructor TCallExprStmt.Destroy;
begin
  if Assigned(FCallExpr) then FCallExpr.Free;
  inherited Destroy;
end;

{ TSetStmt }

constructor TSetStmt.Create
  (AInstance: TExpr; AIdent: TIdent; AOp: TToken; AExpr: TExpr);
begin
  Inherited Create(AIdent.Token);
  FInstance := AInstance;
  FIdent := AIdent;
  FOp := AOp;
  FExpr := AExpr;
end;

destructor TSetStmt.Destroy;
begin
  if Assigned(FInstance) then FInstance.Free;
  if Assigned(FIdent) then FIdent.Free;
  if Assigned(FExpr) then FExpr.Free;
  inherited Destroy;
end;

{ TIndexedExprStmt }

constructor TIndexedExprStmt.Create
  (AIndexedExpr: TIndexedExpr; AOp: TToken; AExpr: TExpr);
begin
  Inherited Create(AOp);
  FIndexedExpr := AIndexedExpr;
  FOp := AOp;
  FExpr := AExpr;
end;

destructor TIndexedExprStmt.Destroy;
begin
  if Assigned(FIndexedExpr) then FIndexedExpr.Free;
  if Assigned(FExpr) then FExpr.Free;
  inherited Destroy;
end;


{ TIfStmt }

constructor TIfStmt.Create(AVarDecl: TVarDecl; ACondition: TExpr;
  AElseIfs: TExprList; AThenPart, AElsePart: TBlock;
  AElseIfParts: TBlockList; AToken: TToken);
begin
  inherited Create(AToken);
  FVarDecl := AVarDecl;
  FCondition := ACondition;
  FThenPart := AThenPart;
  FElsePart := AElsePart;
  FElseIfs := AElseIfs;
  FElseIfParts := AElseIfParts;
end;

destructor TIfStmt.Destroy;
begin
  if Assigned(FVarDecl) then FVarDecl.Free;
  if Assigned(FCondition) then FCondition.Free;
  if Assigned(FThenPart) then FThenPart.Free;
  if Assigned(FElsePart) then FElsePart.Free;
  if Assigned(FElseIfs) then FElseIfs.Free;
  if Assigned(FElseIfParts) then FElseIfParts.Free;
  inherited Destroy;
end;

{ TWhileStmt }

constructor TWhileStmt.Create(AVarDecl: TVarDecl; ACondition: TExpr;
  ABlock: TBlock; AToken: TToken);
begin
  inherited Create(AToken);
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

{ TRepeatStmt }

constructor TRepeatStmt.Create(ACondition: TExpr; ABlock: TBlock; AToken: TToken);
begin
  inherited Create(AToken);
  FCondition := ACondition;
  FBlock := ABlock;
end;

destructor TRepeatStmt.Destroy;
begin
  if Assigned(FCondition) then FCondition.Free;
  if Assigned(FBlock) then FBlock.Free;
  inherited Destroy;
end;

{ TEnsureStmt }

constructor TEnsureStmt.Create(AVarDecl: TVarDecl; ACondition: TExpr;
  AElsePart: TBlock; AToken: TToken);
begin
  inherited Create(AToken);
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

{ TSwitchStmt }

constructor TSwitchStmt.TCaseLimb.Create
  (AValues: TExprList; AIsObj: Boolean; ABlock: TBlock);
begin
  Values := AValues;
  IsObj := AIsObj;
  Block := ABlock;
end;

destructor TSwitchStmt.TCaseLimb.Destroy;
begin
  if Assigned(Values) then Values.Free;
  if Assigned(Block) then Block.Free;
  inherited Destroy;
end;

constructor TSwitchStmt.Create(aExpr: TExpr; AToken: TToken);
begin
  Inherited Create(AToken);
  FExpr := AExpr;
  FElseLimb := Nil;
  FCaseLimbs := TCaseLimbs.Create(True);
end;

destructor TSwitchStmt.Destroy;
begin
  if Assigned(FExpr) then FExpr.Free;
  if Assigned(FElseLimb) then FElseLimb.Free;
  FCaseLimbs.Free;
  inherited Destroy;
end;

procedure TSwitchStmt.AddLimb
  (AValues: TExprList; AIsObj: Boolean; ABlock: TBlock);
begin
  FCaseLimbs.Add(TCaseLimb.Create(AValues, AIsObj, ABlock));
end;

{ TBreakStmt }

constructor TBreakStmt.Create(ACondition: TExpr; AToken: TToken);
begin
  inherited Create(AToken);
  FCondition := ACondition;
end;

destructor TBreakStmt.Destroy;
begin
  if Assigned(FCondition) then FCondition.Free;
  inherited Destroy;
end;

{ TReturnStmt }

constructor TReturnStmt.Create(AExpr: TExpr; AToken: TToken);
begin
  inherited Create(AToken);
  FExpr := AExpr;
end;

destructor TReturnStmt.Destroy;
begin
  if Assigned(FExpr) then FExpr.Free;
  inherited Destroy;
end;

{ TUseStmt }

constructor TUseStmt.Create(AFileName: String; AToken: TToken);
begin
  Inherited Create(AToken);
  FFileName := AFileName;
end;

{ TDecl }

constructor TDecl.Create(AIdent: TIdent; AKind: TDeclKind; AToken: TToken);
begin
  Inherited Create(AToken);
  FIdent := AIdent;
  FKind := AKind;
end;

destructor TDecl.Destroy;
begin
  if Assigned(FIdent) then FIdent.Free;
  inherited Destroy;
end;

{ TVarDecl }

constructor TVarDecl.Create
  (AIdent: TIdent; AExpr: TExpr; AToken: TToken; AMutable: Boolean);
begin
  inherited Create(AIdent, dkVar, AToken);
  FExpr := AExpr;
  FMutable := AMutable;
end;

destructor TVarDecl.Destroy;
begin
  if Assigned(FExpr) then FExpr.Free;
  inherited Destroy;
end;

{ TVarDecls }

constructor TVarDecls.Create(AList: TDeclList; AToken: TToken);
begin
  inherited Create(Nil, dkNone, AToken);
  FList := AList;
end;

destructor TVarDecls.Destroy;
begin
  if Assigned(FList) then FList.Free;
  inherited Destroy;
end;

{ TFuncDecl }

constructor TFuncDecl.TParam.Create(AIdent, AExtIdent: TIdent);
begin
  Ident := AIdent;
  ExtIdent := AExtIdent;
end;

destructor TFuncDecl.TParam.Destroy;
begin
  if Assigned(Ident) then
    if Ident.Equals(ExtIdent) then
      Ident.Free
    else if Assigned(ExtIdent) then begin
      Ident.Free;
      ExtIdent.Free;
    end;
  inherited Destroy;
end;

constructor TFuncDecl.Create(AIdent: TIdent; AToken: TToken);
begin
  inherited Create(AIdent, dkFunc, AToken);
  FBody := Nil;
  FParams := TParamList.Create();
end;

destructor TFuncDecl.Destroy;
begin
  FParams.Free;
  if Assigned(FBody) then FBody.Free;
  inherited Destroy;
end;

procedure TFuncDecl.AddParam(AIdent, AExtIdent: TIdent);
begin
  FParams.Add(TParam.Create(AIdent, AExtIdent));
end;

{ TFuncDeclExpr }

constructor TFuncDeclExpr.Create(AFuncDecl: TFuncDecl);
begin
  inherited Create(AFuncDecl.Token);
  FFuncDecl := AFuncDecl;
end;

destructor TFuncDeclExpr.Destroy;
begin
  if Assigned(FFuncDecl) then FFuncDecl.Free;
  inherited Destroy;
end;

{ TValDecl }

constructor TValDecl.Create(AIdent: TIdent; AFuncDecl: TFuncDecl;
  AToken: TToken);
begin
  Inherited Create(AIdent, dkVal, AToken);
  FFuncDecl := AFuncDecl;
end;

{ TClassDecl }

constructor TClassDecl.Create(AIdent: TIdent; AParent: TVariable;
  ATraits: TExprList; ADeclList: TDeclList; AToken: TToken);
begin
  Inherited Create(AIdent, dkClass, AToken);
  FDeclList := ADeclList;
  FParent := AParent;
  FTraits := ATraits;
end;

destructor TClassDecl.Destroy;
begin
  if Assigned(FDeclList) then FDeclList.Free;
  if Assigned(FParent) then FParent.Free;
  if Assigned(FTraits) then FTraits.Free;
  inherited Destroy;
end;

{ TExtensionDecl }

constructor TExtensionDecl.Create
  (AIdent: TIdent; ADeclList: TDeclList; AToken: TToken);
begin
  inherited Create(AIdent, dkExtension, AToken);
  FDeclList := ADeclList;
end;

destructor TExtensionDecl.Destroy;
begin
  if Assigned(FDeclList) then FDeclList.Free;
  inherited Destroy;
end;

{ TTraitDecl }

constructor TTraitDecl.Create
  (AIdent: TIdent; ATraits: TExprList; ADeclList: TDeclList; AToken: TToken);
begin
  inherited Create(AIdent, dkTrait, AToken);
  FTraits := ATraits;
  FDeclList := ADeclList;
end;

destructor TTraitDecl.Destroy;
begin
  if Assigned(FDeclList) then FDeclList.Free;
  if Assigned(FTraits) then FTraits.Free;
  inherited Destroy;
end;

{ TArrayDecl }

constructor TArrayDecl.Create
  (AIdent: TIdent; ADeclList: TDeclList; AToken: TToken);
begin
  inherited Create(AIdent, dkArray, AToken);
  FElements := TExprList.Create();
  FDeclList := ADeclList;
end;

destructor TArrayDecl.Destroy;
begin
  if Assigned(FDeclList) then FDeclList.Free;
  FElements.Free;
  inherited Destroy;
end;

procedure TArrayDecl.AddElement(Expr: TExpr);
begin
  FElements.Add(Expr);
end;

{ TArrayDeclExpr }

constructor TArrayDeclExpr.Create(AnArrayDecl: TArrayDecl);
begin
  inherited Create(AnArrayDecl.Token);
  FArrayDecl := AnArrayDecl;
end;

destructor TArrayDeclExpr.Destroy;
begin
  if Assigned(FArrayDecl) then FArrayDecl.Free;
  inherited Destroy;
end;

constructor TKeyValuePair.Create(AKey, AValue: TExpr);
begin
  Key := AKey;
  Value := AValue;
end;

destructor TKeyValuePair.Destroy;
begin
  if Assigned(Key) then Key.Free;
  if Assigned(Value) then Value.Free;
  inherited Destroy;
end;

constructor TDictDecl.Create
  (AIdent: TIdent; ADeclList: TDeclList; AToken: TToken);
begin
  inherited Create(AIdent, dkDict, AToken);
  FElements := TKeyValueList.Create;
  FDeclList := ADeclList;
end;

destructor TDictDecl.Destroy;
begin
  if Assigned(FDeclList) then FDeclList.Free;
  FElements.Free;
  inherited Destroy;
end;

procedure TDictDecl.AddElement(KeyExpr, ValueExpr: TExpr);
begin
  FElements.Add(TKeyValuePair.Create(KeyExpr, ValueExpr));
end;

{ TDictDeclExpr }

constructor TDictDeclExpr.Create(ADictDecl: TDictDecl);
begin
  inherited Create(ADictDecl.Token);
  FDictDecl := ADictDecl;
end;

destructor TDictDeclExpr.Destroy;
begin
  if Assigned(FDictDecl) then FDictDecl.Free;
  inherited Destroy;
end;

{ TEnumDecl }

constructor TEnumElement.Create(AValue: Variant; ASetName: String);
begin
  Value := AValue;
  SetName := ASetName;
end;

function TEnumElement.Copy: TEnumElement;
begin
  Result := TEnumElement.Create(Value, SetName);
end;

function TEnumElementsHelper.Copy: TEnumElements;
var
  i: Integer;
begin
  Result := TEnumElements.Create();
  for i := 0 to self.Count-1 do
    Result.Add(self.Keys[i], TEnumElement(self.Data[i]).Copy);
end;

constructor TEnumDecl.Create(AIdent: TIdent; ADeclList: TDeclList; AToken: TToken);
begin
  inherited Create(AIdent, dkEnum, AToken);
  FElements := TEnumElements.Create;
  FDeclList := ADeclList;
  FCaseTable := TCaseTable.Create;
end;

destructor TEnumDecl.Destroy;
begin
  if Assigned(FDeclList) then FDeclList.Free;
  FCaseTable.Free;
  FElements.Free;
  inherited Destroy;
end;

procedure TEnumDecl.AddElement(const SetName, Name: String; Value: Variant);
begin
  FElements.Add(Name, TEnumElement.Create(Value, SetName));
end;

{ TBlock }

constructor TBlock.Create(ANodes: TNodeList; AToken: TToken);
begin
  inherited Create(AToken);
  FNodes := ANodes;
end;

destructor TBlock.Destroy;
begin
  if Assigned(FNodes) then FNodes.Free;
  inherited Destroy;
end;


end.

