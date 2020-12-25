unit Statements;

interface

uses SysUtils, Generics.Collections, Symbols, Expressions, Contnrs;

type
  TIODevice = (iodUnknown = 0, iodPrinter, iodPunch, iodReader, iodConsole, iodSeqFile );

  TStatement = class
  end;

  TForStatement = class(TStatement)
  public
    AsgExpressions: TObjectList;
    IncrExpression: TExpression;
    TestExpression: TExpression;
    LoopVar: TSymbol;
    LoopLblNum: Integer;
    EndLblNum: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TGotoStatement = class(TStatement)
  public
    Expr: TExpression;
    destructor Destroy; override;
  end;

  TIfStatement = class(TStatement)
  public
    Expr: TExpression;
    ElseLblNum: Integer;
    EndLblNum: Integer;
    destructor Destroy; override;
  end;

  TWriteStatement = class(TStatement)
  public
    Device: TIODevice;
    Params: TObjectList;
    constructor Create;
    destructor Destroy; override;
  end;

  TBlock = class(TStatement)
  public
    IsProgram: Boolean;
    IsMain: Boolean;
    IsProc: Boolean;
    BlockNum: Integer;
    Symbols: TSymbolTable;
    constructor Create;
    destructor Destroy; override;
  end;

  TBlockStack  = class(TStack<TBlock>)
  end;



implementation

{ TBlock }

constructor TBlock.Create;
begin
    Symbols := TSymbolTable.Create;
end;

destructor TBlock.Destroy;
begin
    FreeAndNil(Symbols);
    inherited;
end;

{ TGotoStatement }

destructor TGotoStatement.Destroy;
begin
    FreeAndNil(Expr);
    inherited;
end;

{ TIfStatement }

destructor TIfStatement.Destroy;
begin
    FreeAndNil(Expr);
    inherited;
end;

{ TForStatement }

constructor TForStatement.Create;
begin
    AsgExpressions := TObjectList.Create;
end;

destructor TForStatement.Destroy;
begin
    FreeAndNil(AsgExpressions);
    // Do not free IncrExpression since it is included as part of
    // TestExpression.
    // FreeAndNil(IncrExpression);
    FreeAndNil(TestExpression);
    inherited;
end;

{ TWriteStatement }

constructor TWriteStatement.Create;
begin
    Params := TObjectList.Create;
end;

destructor TWriteStatement.Destroy;
begin
    FreeAndNil(Params);
    inherited;
end;

end.
