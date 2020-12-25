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
    procedure Assignment(tgt:TExprVariable; expr: TExpressionTerm; depth: Integer); overload; virtual; abstract;
    procedure Assignment(sym: TSymbol; expr: TExpressionTerm; depth: Integer); overload; virtual; abstract;
    procedure BlockBegin(block: TBlock); virtual; abstract;
    procedure BlockEnd(block: TBlock); virtual; abstract;
    procedure BlockStart(block: TBlock); virtual; abstract;
    procedure Comment(s: AnsiString); virtual; abstract;
    procedure Expression(expr: TExpressionTerm); virtual; abstract;
    procedure ForEnd(inclbl, endlbl: Integer); virtual; abstract;
    procedure ForInit; virtual; abstract;
    procedure ForListEnd(stmt: TForStatement); virtual; abstract;
    procedure ForListInit(stmt: TForStatement); virtual; abstract;
    procedure ForStep(stmt: TForStatement); virtual; abstract;
    procedure ForStepIncr(stmt: TForStatement); virtual; abstract;
    procedure ForWhile(stmt: TForStatement); virtual; abstract;
    procedure ForWhileEnd(stmt: TForStatement); virtual; abstract;
    procedure GotoIfFalse(target: Integer); virtual; abstract;
    procedure Gotoo(target: TExpressionTerm); overload; virtual; abstract;
    procedure Gotoo(target: Integer); overload; virtual; abstract;
    procedure IfElse(iff: TIfStatement); virtual; abstract;
    procedure IfEnd(iff: TIfStatement); virtual; abstract;
    procedure IfThen(iff: TIfStatement); virtual; abstract;
    procedure Labell(sym: TSymbol); overload; virtual; abstract;
    procedure Labell(sym: Integer); overload; virtual; abstract;
    procedure NewArray(arr: TSymbol); virtual; abstract;
    procedure PgmEnd(isMain: Boolean; lits: TLiteralList; tmpVars: TSymbolTable); virtual; abstract;
    procedure PgmStart(isMain: Boolean); virtual; abstract;
    procedure PopExprRslt(expr: TExpressionTerm); virtual; abstract;
    procedure PushExprRslt(expr: TExpressionTerm); virtual; abstract;
    procedure PushValue(value: Integer); overload; virtual; abstract;
    procedure PushValue(sym: TSymbol); overload; virtual; abstract;
    procedure PushSubstring(expr: TExpressionTerm); virtual; abstract;
    procedure StackVar(sym: TSymbol); virtual; abstract;
    procedure StaticVar(sym: TSymbol); virtual; abstract;
    procedure Switch(sym: TSwitchSymbol); virtual; abstract;
    procedure Write(wr: TWriteStatement); virtual; abstract;
  end;

var
    StackSize: Integer;
    MinHeapSize: Integer;
    ReqHeapSize: Integer;

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
