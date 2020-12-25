unit Expressions;

interface

uses SysUtils, Generics.Collections, Symbols, Literals;

type
  TExprOperatorType = ( otExponent = 0, otUnaryMinus, otMultiply, otDivide,
                        otIntDivide, otPlus, otMinus, otLess, otLessEqual,
                        otGreater, otGreaterEqual, otEqual, otNotEqual,
                        otNot, otAnd, otOr, otXor, otImpl, otEquiv,
                        otIf, otThen, otElse, otGetArray, otPush,
                        otSign, otUnknown );

  TExpressionType = ( etInteger, etReal, etLogical, etDesignational, etUnknown, etNotExpression );

  TExprUnaryTypes = set of TExprOperatorType;

//  TExpressionItemClass = class of TExpressionItem;

  TExpressionTerm = class
  public
    ErrorCount: Integer;
    TargetDecl: TDeclType;
    Left, Right: TExpressionTerm;
    destructor Destroy; override;
//    class function NewItem(cls: TExpressionItemClass): TExpressionItem;
    procedure Assign(src: TExpressionTerm); virtual;
    function DeclType: TDeclType; virtual; abstract;
    function IsLogical: Boolean; virtual; abstract;
    function IsNumeric: Boolean; virtual; abstract;
    function IsReal: Boolean; virtual; abstract;
    function TargetIsLogical: Boolean; virtual;
    function TargetIsNumeric: Boolean; virtual;
    function TargetIsReal: Boolean; virtual;
    function TargetIsString: Boolean; virtual;
  end;

  TExprVariable = class(TExpressionTerm)
  public
    Symbol: TSymbol;
    StringStart: TExpressionTerm;
    StringLength: TExpressionTerm;
    destructor Destroy; override;
    procedure Assign(src: TExpressionTerm); override;
    function DeclType: TDeclType; override;
    function IsLogical: Boolean; override;
    function IsNumeric: Boolean; override;
    function IsReal: Boolean; override;
  end;

  TExprLabel = class(TExprVariable)
  end;

  TExprSwitch = class(TExprVariable)
  public
    Index: TExpressionTerm;
    destructor Destroy; override;
  end;

  TExprArray = class(TExprVariable)
  public
    Subscripts: TList<TExpressionTerm>;
    constructor Create;
    destructor Destroy; override;
  end;

  TExprLiteral = class(TExpressionTerm)
  public
    Literal: TLiteral;
    procedure Assign(src: TExpressionTerm); override;
    function DeclType: TDeclType; override;
    function IsLogical: Boolean; override;
    function IsNumeric: Boolean; override;
    function IsReal: Boolean; override;
  end;

  TExprOperator = class(TExpressionTerm)
  public
    OType: TExprOperatorType;
    procedure Assign(src: TExpressionTerm); override;
    function DeclType: TDeclType; override;
    function IsArithmetic: Boolean;
    function IsLogical: Boolean; override;
    function IsNumeric: Boolean; override;
    function IsReal: Boolean; override;
    function IsRelational: Boolean;
    function TargetIsNumeric: Boolean; override;
  end;

  TExprResult = class(TExpressionTerm)
  public
    DType: TDeclType;
    procedure Assign(src: TExpressionTerm); override;
    function DeclType: TDeclType; override;
    function IsLogical: Boolean; override;
    function IsNumeric: Boolean; override;
    function IsReal: Boolean; override;
  end;

  TExpression = class(TExpressionTerm)
  end;

  TExprIf = class(TExprOperator)
  public
    ElseLblNum: Integer;
    EndLblNum: Integer;
    ConditionalDepth: Integer;
    IfExpr: TExpressionTerm;
    ThenExpr: TExpressionTerm;
    ElseExpr: TExpressionTerm;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(src: TExpressionTerm); override;
  end;

  TExpressionList = class(TList<TExpression>)
  end;

const
  ExprUnaryOperators: TExprUnaryTypes = [ otUnaryMinus, otNot, otIf, otThen, otElse,
                                          otPush ];

  ExprOprPriorities: array [0..9, 0..5] of AnsiString = (
    ( 'EQUIV', '', '', '', '', '' ),
    ( 'IMPL', '', '', '', '', '' ),
    ( 'OR', 'XOR', '', '', '', '' ),
    ( 'AND', '', '', '', '', '' ),
    ( 'NOT', '', '', '', '', '' ),
    ( 'LSS', 'LEQ', 'GTR', 'GEQ', 'EQL', 'NEQ' ),
    ( '+', '-', '', '', '', '' ),
    ( '*', '/', '//', '', '', '' ),
    ( '-', '', '', '', '', '' ),
    ( '**', '', '', '', '', '' )
  );

implementation

{ TExprVariable }

procedure TExprVariable.Assign(src: TExpressionTerm);
begin
    inherited;
    Symbol := TExprVariable(src).Symbol;
end;

function TExprVariable.DeclType: TDeclType;
begin
    Result := Symbol.DeclType;
end;

destructor TExprVariable.Destroy;
begin
    FreeAndNil(StringStart);
    FreeAndNil(StringLength);
    inherited Destroy;
end;

function TExprVariable.IsLogical: Boolean;
begin
    Result := Symbol.DeclType = dtLogical;
end;

function TExprVariable.IsNumeric: Boolean;
begin
    Result := (Symbol.DeclType = dtInteger) or (Symbol.DeclType = dtReal);
end;

function TExprVariable.IsReal: Boolean;
begin
    Result := (Symbol.DeclType = dtReal);
end;

{ TExprLiteral }

procedure TExprLiteral.Assign(src: TExpressionTerm);
begin
    inherited;
    Literal := TExprLiteral(src).Literal;
end;

function TExprLiteral.DeclType: TDeclType;
begin
    Result := Literal.DeclType;
end;

function TExprLiteral.IsLogical: Boolean;
begin
    Result := Literal.DeclType = dtLogical;
end;

function TExprLiteral.IsNumeric: Boolean;
begin
    Result := (Literal.DeclType = dtInteger) or (Literal.DeclType = dtReal);
end;

function TExprLiteral.IsReal: Boolean;
begin
    Result := (Literal.DeclType = dtReal);
end;

{ TExprOperator }

procedure TExprOperator.Assign(src: TExpressionTerm);
begin
    inherited;
end;

function TExprOperator.DeclType: TDeclType;
begin
    Result := TargetDecl;
end;

function TExprOperator.IsArithmetic: Boolean;
begin
    Result := ((Integer(OType) >= 0) and (Integer(OType) <= 6)) or
               (OType = otThen) or (OType = otElse);
end;

function TExprOperator.IsLogical: Boolean;
begin
    Result := ((Integer(OType) >= 13) and (Integer(OType) <= 18)) or
               (OType = otIf);
end;

function TExprOperator.IsNumeric: Boolean;
begin
    Result := (TargetDecl = dtInteger) or (TargetDecl = dtReal);
end;

function TExprOperator.IsReal: Boolean;
begin
    Result := False;
end;

function TExprOperator.IsRelational: Boolean;
begin
    Result := (Integer(OType) >= 7) and (Integer(OType) <= 12);
end;

function TExprOperator.TargetIsNumeric: Boolean;
begin
    if (TargetDecl <> dtUnknown) then
        Result := inherited TargetIsNumeric
    else
        Result := IsArithmetic;
end;

{ TExprResult }

procedure TExprResult.Assign(src: TExpressionTerm);
begin
    inherited;
    DType := TExprResult(src).DType;
end;

function TExprResult.DeclType: TDeclType;
begin
    Result := DType;
end;

function TExprResult.IsLogical: Boolean;
begin
    Result := DType = dtLogical;
end;

function TExprResult.IsNumeric: Boolean;
begin
    Result := (DType = dtInteger) or (DType = dtReal);
end;

function TExprResult.IsReal: Boolean;
begin
    Result := (DType = dtReal);
end;

{ TExpressionItem }

procedure TExpressionTerm.Assign(src: TExpressionTerm);
begin
    TargetDecl := src.TargetDecl;
end;

//class function TExpressionItem.NewItem(cls: TExpressionItemClass): TExpressionItem;
//begin
//    Result := cls.Create;
//end;

destructor TExpressionTerm.Destroy;
begin
    FreeAndNil(Left);
    FreeAndNil(Right);
    inherited;
end;

function TExpressionTerm.TargetIsLogical: Boolean;
begin
    Result := TargetDecl = dtLogical;
end;

function TExpressionTerm.TargetIsNumeric: Boolean;
begin
    Result := (TargetDecl = dtInteger) or (TargetDecl = dtReal);
end;

function TExpressionTerm.TargetIsReal: Boolean;
begin
    Result := TargetDecl = dtReal;
end;

function TExpressionTerm.TargetIsString: Boolean;
begin
    Result := TargetDecl = dtString;
end;

{ TExprIf }

procedure TExprIf.Assign(src: TExpressionTerm);
begin
    inherited;
    OType := TExprOperator(src).OType;
    ElseLblNum := TExprIf(src).ElseLblNum;
    EndLblNum := TExprIf(src).EndLblNum;
    ConditionalDepth := TExprIf(src).ConditionalDepth;
end;

constructor TExprIf.Create;
begin
    OType := otIf;
end;

destructor TExprIf.Destroy;
begin
    FreeAndNil(IfExpr);
    FreeAndNil(ThenExpr);
    FreeAndNil(ElseExpr);
    inherited;
end;

{ TExprArray }

constructor TExprArray.Create;
begin
    Subscripts := TList<TExpressionTerm>.Create;
end;

destructor TExprArray.Destroy;
begin
    FreeAndNil(Subscripts);
    inherited;
end;

{ TExprSwitch }

destructor TExprSwitch.Destroy;
begin
    FreeAndNil(Index);
    inherited;
end;

end.
