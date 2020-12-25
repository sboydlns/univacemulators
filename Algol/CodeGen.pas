unit CodeGen;

interface

uses SysUtils, AnsiStrings, SrcFile, Statements, Symbols, Expressions,
     Literals;

type
  TCodeGen = class
  protected
    FOutFile: TSrcFile;
    FStarted: Boolean;
    FLabelIndent: Integer;
    FOpcodeIndent: Integer;
    FOperandIndent: Integer;
    FCommentIndent: Integer;
    procedure Emit(lbl: AnsiString = ''; opcode: AnsiString = ''; operand: AnsiString = '';
                   cmnt: AnsiString = ''); virtual;
  public
    constructor Create(outFile: TSrcFile); virtual;
    destructor Destroy; override;
    procedure AssignArrayEnd(sym: TSymbol);virtual; abstract;
    procedure Assignment(sym: TSymbol); virtual; abstract;
    procedure BlockBegin(block: TBlock); virtual; abstract;
    procedure BlockEnd(block: TBlock); virtual; abstract;
    procedure BlockStart(block: TBlock); virtual; abstract;
    procedure CallSwitch(sym: TSymbol; isGoto: Boolean); virtual; abstract;
    procedure Comment(s: AnsiString); virtual; abstract;
    procedure Expression(expr: TExpression); virtual; abstract;
    procedure ForEnd(inclbl, endlbl: Integer); virtual; abstract;
    procedure ForInit; virtual; abstract;
    procedure ForListEnd(count, looplbl, endlbl: Integer; loopvar: TSymbol); virtual; abstract;
    procedure ForListExpr; virtual; abstract;
    procedure ForListInit(count, looplbl, endlbl: Integer; loopvar: TSymbol); virtual; abstract;
    procedure ForStep(inclbl, looplbl: Integer; sym: TSymbol); virtual; abstract;
    procedure ForStepTest(endlbl: Integer); virtual; abstract;
    procedure ForUntil(looplbl: Integer; sym: TSymbol); virtual; abstract;
    procedure ForWhile(looplbl: Integer; sym: TSymbol); virtual; abstract;
    procedure ForWhileTest(endlbl: Integer); virtual; abstract;
    procedure GotoIfFalse(target: Integer); virtual; abstract;
    procedure Gotoo(target: TSymbol); overload; virtual; abstract;
    procedure Gotoo(target: Integer); overload; virtual; abstract;
    procedure Labell(sym: TSymbol); overload; virtual; abstract;
    procedure Labell(sym: Integer); overload; virtual; abstract;
    procedure NewArrayStart(sym: TSymbol); virtual; abstract;
    procedure NewArrayEnd(sym: TSymbol); virtual; abstract;
    procedure PgmEnd(isMain: Boolean; lits: TLiteralList); virtual; abstract;
    procedure PgmStart(isMain: Boolean); virtual; abstract;
    procedure PushExprRslt; virtual; abstract;
    procedure PushValue(value: Integer); overload; virtual; abstract;
    procedure PushValue(sym: TSymbol); overload; virtual; abstract;
    procedure StackVar(sym: TSymbol); virtual; abstract;
    procedure StaticVar(sym: TSymbol); virtual; abstract;
    procedure SwitchEnd(lbl: Integer); virtual; abstract;
    procedure SwitchItem(idx, lbl: Integer); virtual; abstract;
    procedure SwitchStart(sym: TSymbol); virtual; abstract;
  end;

var
    StackSize: Integer;

implementation

uses Math;

{ TCodeGen }

constructor TCodeGen.Create(outFile: TSrcFile);
begin
    FOutFile := outFile;
    FLabelIndent := 1;
    FOpcodeIndent := 8;
    FOperandIndent := 16;
    FCommentIndent := 24;
end;

destructor TCodeGen.Destroy;
begin
  inherited;
end;

procedure TCodeGen.Emit(lbl, opcode, operand, cmnt: AnsiString);
var
    s: AnsiString;
    spaces: Integer;
begin
    s := StringOfChar(AnsiChar(' '), FLabelIndent - 1) + lbl;
    spaces := Max(1, FOpcodeIndent - Length(s) - 1);
    s := s + StringOfChar(AnsiChar(' '), spaces) + opcode;
    spaces := Max(1, FOperandIndent - Length(s) - 1);
    s := s + StringOfChar(AnsiChar(' '), spaces) + operand;
    spaces := Max(1, FCommentIndent - Length(s) - 1);
    s := s + StringOfChar(AnsiChar(' '), spaces) + cmnt;
    FOutFile.PrintLine(s);
end;

end.
