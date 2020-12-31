unit U494CodeGen;

interface

uses SysUtils, Generics.Collections, CodeGen, AnsiStrings, SrcFile, Statements, Symbols,
     Expressions, Literals, Math;

type
  TEmitType = ( emtOther = 0, emtPushA, emtPushQ, emtPushAQ, emtPopA, emtPopQ, emtPopAQ );

  T494CodeGen = class(TCodeGen)
  protected
    FLastEmit: TEmitType;
    procedure ApplyOperator(opr: TExprOperator; right: TExpressionTerm; dtype: TDeclType);
    procedure CallSwitch(sym: TSymbol; isGoto: Boolean);
    procedure DesignationalExpr(expr: TExpressionTerm; isGoto: Boolean);
    procedure Emit(lbl: AnsiString = ''; opcode: AnsiString = ''; operand: AnsiString = '';
                   cmnt: AnsiString = ''); override;
    procedure FreeStackVar(sym: TSymbol);
    procedure FreeStaticVar(sym: TSymbol);
    procedure InitStackVar(sym: TSymbol);
    procedure InitStaticVar(sym: TSymbol);
    function LiteralOperand(lit: TLiteral; j: AnsiString = ''): AnsiString;
    procedure LoadQ(term: TExpressionTerm);
    function VariableOperand(sym: TSymbol; j: AnsiString = ''): AnsiString;
  public
    constructor Create(outFile: TSrcFile); override;
    destructor Destroy; override;
    procedure Assignment(tgt:TExprVariable; expr: TExpressionTerm; depth: Integer); override;
    procedure Assignment(sym: TSymbol; expr: TExpressionTerm; depth: Integer); override;
    procedure BlockBegin(block: TBlock); override;
    procedure BlockEnd(block: TBlock); override;
    procedure BlockStart(block: TBlock); override;
    procedure Comment(s: AnsiString); override;
    procedure Expression(expr: TExpressionTerm); override;
    procedure ForEnd(looplbl, endlbl: Integer); override;
    procedure ForInit; override;
    procedure ForListEnd(stmt: TForStatement); override;
    procedure ForListInit(stmt: TForStatement); override;
    procedure ForStep(stmt: TForStatement); override;
    procedure ForStepIncr(stmt: TForStatement); override;
    procedure ForWhile(stmt: TForStatement); override;
    procedure ForWhileEnd(stmt: TForStatement); override;
    procedure GotoIfFalse(target: Integer); override;
    procedure Gotoo(target: TExpressionTerm); overload; override;
    procedure Gotoo(target: Integer); overload; override;
    procedure IfElse(iff: TIfStatement); override;
    procedure IfEnd(iff: TIfStatement); override;
    procedure IfThen(iff: TIfStatement); override;
    procedure Labell(sym: TSymbol); overload; override;
    procedure Labell(sym: Integer); overload; override;
    procedure NewArray(arr: TSymbol); override;
    procedure PgmEnd(isMain: Boolean; lits: TLiteralList; tmpVars: TSymbolTable); override;
    procedure PgmStart(isMain: Boolean); override;
    procedure PopExprRslt(expr: TExpressionTerm); override;
    procedure PushExprRslt(expr: TExpressionTerm); override;
    procedure PushValue(value: Integer); override;
    procedure PushValue(sym: TSymbol); override;
    procedure PushSubstring(expr: TExpressionTerm); override;
    procedure StackVar(sym: TSymbol); override;
    procedure StaticVar(sym: TSymbol); override;
    procedure Switch(sym: TSwitchSymbol); override;
    procedure Write(wr: TWriteStatement); override;
  end;

implementation

{ T494CodeGen }

procedure T494CodeGen.ApplyOperator(opr: TExprOperator; right: TExpressionTerm; dtype: TDeclType);
var
    k, operand, comment: AnsiString;
    lit: TExprLiteral;
    sym: TExprVariable;
    itemp: Integer;
begin
    k := '';
    operand := '';
    comment := '';
    if (right is TExprLiteral) then
    begin
        lit := TExprLiteral(right);
        case lit.Literal.DeclType of
          dtInteger,
          dtLogical:
          begin
            if (not TryStrToInt(String(lit.Literal.Value), itemp)) then
                itemp := 0;
            if ((itemp >= 0) and (itemp <= 32767)) then
            begin
                operand := lit.Literal.Value + 'D';
                Dec(lit.Literal.RefCount);
            end else if ((itemp < 0) and (Abs(itemp) <= 16383)) then
            begin
                k := ',X';
                operand := lit.Literal.Value + 'D';
                Dec(lit.Literal.RefCount);
            end else
            begin
                Emit('', 'LA,W', LiteralOperand(lit.Literal), '. ' + lit.Literal.Value);
                k := ',A';
            end;
          end;
          dtReal:
          begin
            operand := LiteralOperand(lit.Literal);
            comment := '. ' + lit.Literal.Value;
          end;
          dtString:
          begin
            Emit('', 'LQ', LiteralOperand(lit.Literal), Format(AnsiString('. %-10.10s'), [lit.Literal.Value]));
            Emit('', 'LBPJB1', 'PUSHQ');
            PushValue(1);
            PushValue(Length(lit.Literal.Value));
          end;
        end;
    end else if ((right is TExprVariable) and (not (right is TExprArray))) then
    begin
        sym := TExprVariable(right);
        case sym.Symbol.DeclType of
          dtInteger,
          dtLogical:
          begin
            if (sym.TargetDecl = dtReal) then
            begin
                Emit('', 'DPS', 'TEMP$AQ2');
                Emit('', 'LA,W', VariableOperand(sym.Symbol), '. ' + sym.Symbol.ID);
                Emit('', 'LBPJB1', 'INT2FLOATA');
                Emit('', 'DPS', 'TEMP$AQ');
                Emit('', 'DPL', 'TEMP$AQ2');
                operand := 'TEMP$AQ';
            end else
            begin
                k := ',W';
                operand  := VariableOperand(sym.Symbol);
                comment := '. ' + sym.Symbol.ID;
            end;
          end;
          dtReal:
          begin
            operand := VariableOperand(sym.Symbol);
            comment := '. ' + sym.Symbol.ID;
          end;
          dtString:
          begin
            Emit('',
                 'LQ,W',
                 VariableOperand(sym.Symbol),
                 Format(AnsiString('. %s'), [sym.Symbol.ID]));
            Emit('', 'LBPJB1', 'PUSHQ');
            PushSubstring(sym);
          end;
        end;
    end else
    begin
        case opr.TargetDecl of
          dtInteger,
          dtLogical:
          begin
            k := ',A';
            operand := '';
          end;
          dtReal:
          begin
            operand := 'TEMP$AQ';
          end;
        end;
    end;

    case opr.OType of
      otExponent:
      begin
        Emit('', 'DPS', '$+4');
        Emit('', 'DPL', operand, comment);
        Emit('', 'DPS', '$+4');
        Emit('', 'LBPJB1', 'POWER');
        Emit('', 'RES', '4D');
      end;
      otUnaryMinus:
      begin
        case dtype of
          dtInteger:
          begin
            Emit('', 'LQ' + k, operand, comment);
            Emit('', 'NQ');
          end;
          dtReal:
          begin
            Emit('', 'DPL', operand, comment);
            Emit('', 'DPN');
          end;
        end;
      end;
      otMultiply:
      begin
        case dtype of
          dtInteger:
            Emit('', 'M' + k, operand, comment);
          dtReal:
            Emit('', 'FM', operand, comment);
        end;
      end;
      otDivide:
      begin
        case dtype of
          dtInteger: ;
          dtReal:
            Emit('', 'FD', operand, comment);
        end;
      end;
      otIntDivide:
      begin
        case dtype of
          dtInteger:
          begin
            if (k = ',A') then
            begin
                Emit('', 'SA,W', 'TEMP$AQ');
                k := ',W';
                operand := 'TEMP$AQ';
            end;
            Emit('', 'ZA');
            Emit('', 'D' + k, operand, comment);
          end;
        end;
      end;
      otPlus:
      begin
        case dtype of
          dtInteger:
            Emit('', 'AQ' + k, operand, comment);
          dtReal:
            Emit('', 'FA', operand, comment);
        end;
      end;
      otMinus:
      begin
        case dtype of
          dtInteger:
            Emit('', 'ANQ' + k, operand, comment);
          dtReal:
            Emit('', 'FAN', operand, comment);
        end;
      end;
      otLess:
      begin
        case right.TargetDecl of
          dtInteger,
          dtLogical:
          begin
            Emit('', 'ANQ' + k, operand, '. LSS' + comment);
            Emit('', 'SQ,A', '0,,ANEG');
            Emit('', 'LQ', '0,,QPOS', '. FALSE');
            Emit('', 'LQ', '1', '. TRUE');
          end;
          dtReal:
          begin
            Emit('', 'DPTL', operand, '. LSS' + comment);
            Emit('', 'LQ', '0,,APOS', '. FALSE');
            Emit('', 'LQ', '1', '. TRUE');
          end;
          dtString:
          begin
            Emit('', 'LBPJB1', 'STRCOMP');
            Emit('', 'LA,A', '0,,APOS');
            Emit('', 'LQ', '1,,QPOS', '. TRUE');
            Emit('', 'LQ', '0', '. FALSE');
          end;
        end;
      end;
      otLessEqual:
      begin
        case right.TargetDecl of
          dtInteger,
          dtLogical:
          begin
            Emit('', 'ANQ' + k, operand, '. LEQ' + comment);
            Emit('', 'SQ,A');
            Emit('', 'LQ', '1', '. TRUE');
            Emit('', 'JT', '$+3,,ANEG');
            Emit('', 'JT', '$+2,,AZERO');
            Emit('', 'LQ', '0', '. FALSE');
          end;
          dtReal:
          begin
            Emit('', 'DPTL', operand, '. GTR' + comment);
            Emit('', 'J', '$+2');
            Emit('', 'J', '$+3');
            Emit('', 'DPTE', operand);
            Emit('', 'LQ', '0,,QPOS', '. FALSE');
            Emit('', 'LQ', '1', '. TRUE');
          end;
          dtString:
          begin
            Emit('', 'LBPJB1', 'STRCOMP');
            Emit('', 'LA,A', '0,,APOS');
            Emit('', 'LQ', '1,,QPOS', '. TRUE');
            Emit('', 'LA,A', '0,,ANOT');
            Emit('', 'LQ', '1,,QPOS', '. TRUE');
            Emit('', 'LQ', '0', '. FALSE');
          end;
        end;
      end;
      otGreater:
      begin
        case right.TargetDecl of
          dtInteger,
          dtLogical:
          begin
            Emit('', 'ANQ' + k, operand, '. GTR' + comment);
            Emit('', 'SQ,A');
            Emit('', 'LQ', '0', '. FALSE');
            Emit('', 'JT', '$+3,,ANEG');
            Emit('', 'JT', '$+2,,AZERO');
            Emit('', 'LQ', '1', '. TRUE');
          end;
          dtReal:
          begin
            Emit('', 'DPTL', operand, '. GTR' + comment);
            Emit('', 'J', '$+2');
            Emit('', 'J', '$+3');
            Emit('', 'DPTE', operand);
            Emit('', 'LQ', '1,,QPOS', '. TRUE');
            Emit('', 'LQ', '0', '. FALSE');
          end;
          dtString:
          begin
            Emit('', 'LBPJB1', 'STRCOMP');
            Emit('', 'LA,A', '0,,APOS');
            Emit('', 'LQ', '0,,QPOS', '. FALSE');
            Emit('', 'LA,A', '0,,ANOT');
            Emit('', 'LQ', '0,,QPOS', '. FALSE');
            Emit('', 'LQ', '1', '. TRUE');
          end;
        end;
      end;
      otGreaterEqual:
      begin
        case right.TargetDecl of
          dtInteger,
          dtLogical:
          begin
            Emit('', 'ANQ' + k, operand, '. GTE' + comment);
            Emit('', 'SQ,A', '0,,APOS');
            Emit('', 'LQ', '0,,QPOS', '. FALSE');
            Emit('', 'LQ', '1', '. TRUE');
          end;
          dtReal:
          begin
            Emit('', 'DPTL', operand, '. GTE' + comment);
            Emit('', 'LQ', '1,,APOS', '. FALSE');
            Emit('', 'LQ', '0', '. TRUE');
          end;
          dtString:
          begin
            Emit('', 'LBPJB1', 'STRCOMP');
            Emit('', 'LA,A', '0,,APOS');
            Emit('', 'LQ', '0,,QPOS', '. FALSE');
            Emit('', 'LQ', '1', '. TRUE');
          end;
        end;
      end;
      otEqual:
      begin
        case right.TargetDecl of
          dtInteger,
          dtLogical:
          begin
            Emit('', 'ANQ' + k, operand, '. EQL' + comment);
            Emit('', 'SQ,A', '0,,ANOT');
            Emit('', 'LQ', '1,,QPOS', '. TRUE');
            Emit('', 'LQ', '0', '. FALSE');
          end;
          dtReal:
          begin
            Emit('', 'DPTE', operand, '. EQL' + comment);
            Emit('', 'LQ', '0,,QPOS', '. FALSE');
            Emit('', 'LQ', '1', '. TRUE');
          end;
          dtString:
          begin
            Emit('', 'LBPJB1', 'STRCOMP');
            Emit('', 'LA,A', '0,,AZERO');
            Emit('', 'LQ', '0,,QPOS', '. FALSE');
            Emit('', 'LQ', '1', '. TRUE');
          end;
        end;
      end;
      otNotEqual:
      begin
        case right.TargetDecl of
          dtInteger,
          dtLogical:
          begin
            Emit('', 'ANQ' + k, operand, '. NEQ' + comment);
            Emit('', 'SQ,A', '0,,AZERO');
            Emit('', 'LQ', '1,,QPOS', '. TRUE');
            Emit('', 'LQ', '0', '. FALSE');
          end;
          dtReal:
          begin
            Emit('', 'DPTE', operand, '. NEQ' + comment);
            Emit('', 'LQ', '1,,QPOS', '. TRUE');
            Emit('', 'LQ', '0', '. FALSE');
          end;
          dtString:
          begin
            Emit('', 'LBPJB1', 'STRCOMP');
            Emit('', 'LA,A', '0,,ANOT');
            Emit('', 'LQ', '0,,QPOS', '. FALSE');
            Emit('', 'LQ', '1', '. TRUE');
          end;
        end;
      end;
      otNot:
      begin
        Emit('', 'LA', '1', '. NOT');
        Emit('', 'XOR' + k, operand, comment);
        Emit('', 'LQ,A');
      end;
      otAnd:
      begin
        Emit('', 'LLP' + k, operand, '. AND' + comment);
        Emit('', 'LQ,A');
      end;
      otOr:
      begin
        Emit('', 'SQ,W', 'TEMP$AQ', '. OR');
        Emit('', 'LA' + k, operand, comment);
        Emit('', 'OR,W', 'TEMP$AQ');
        Emit('', 'LQ,A');
      end;
      otXor:
      begin
        Emit('', 'SQ,W', 'TEMP$AQ', '. XOR');
        Emit('', 'LA' + k, operand, comment);
        Emit('', 'XOR,W', 'TEMP$AQ');
        Emit('', 'LQ,A');
      end;
      otImpl:
      begin
        Emit('', 'LA' + k, operand, comment);
        Emit('', 'JT', '$+5,,ANOT', '. IMPL');
        Emit('', 'SQ,A');
        Emit('', 'JT', '$+3,,AZERO');
        Emit('', 'LQ', '0', '. FALSE');
        Emit('', 'J', '$+2');
        Emit('', 'LQ', '1', '. TRUE');
      end;
      otEquiv:
      begin
        Emit('', 'ANQ' + k, operand + ',,ANOT', '. EQUIV');
        Emit('', 'J', '$+3');
        Emit('', 'LQ', '0', '. FALSE');
        Emit('', 'J', '$+2');
        Emit('', 'LQ', '1', '. TRUE');
      end;
      otIf:
      begin
        Emit('', 'SQ,A', '0,,ANOT');
        Emit('', 'J', Format(AnsiString('L$%d'), [TExprIf(opr).ElseLblNum]));
      end;
      otThen:
      begin
        Emit('', 'J', Format(AnsiString('L$%d'), [TExprIf(opr).EndLblNum]));
        Emit(Format(AnsiString('L$%d'), [TexprIf(opr).ElseLblNum]));
      end;
      otElse:
      begin
        if (TexprIf(opr).ConditionalDepth = 0) then
        begin
            Emit(Format(AnsiString('L$%d'), [TexprIf(opr).EndLblNum]));
        end;
      end;
      otGetArray:
      begin
        case dtype of
          dtInteger,
          dtLogical:
            Emit('', 'LBPJB1', 'GETARRAYQ');
          dtReal:
            Emit('', 'LBPJB1', 'GETARRAYAQ');
          dtString:
          begin
            Emit('', 'LBPJB1', 'GETSARRAY');
            Emit('', 'LBPJB1', 'PUSHQ');
            PushSubstring(right);
          end;
        end;
      end;
      otSign:
      begin
        case dtype of
          dtInteger:
          begin
            Emit('', 'LQ' + k, operand, comment);
            Emit('', 'LBPJB1', 'SIGNQ');
          end;
          dtReal:
          begin
            Emit('', 'DPL', operand, comment);
            Emit('', 'LBPJB1', 'SIGNAQ');
          end;
        end;
      end;
    end;
end;

procedure T494CodeGen.Assignment(tgt:TExprVariable; expr: TExpressionTerm; depth: Integer);
var
    p: Pointer;
    subscript: TExpressionTerm;
begin
    if (tgt is TExprArray) then
    begin
        case tgt.DeclType of
          dtInteger,
          dtLogical:
          begin
            if (Assigned(expr) and (expr.TargetDecl = dtReal)) then
                Emit('', 'LBPJB1', 'INTEGER');
            Emit('', 'LBPJB1', 'PUSHQ');
          end;
          dtReal:
          begin
            if (Assigned(expr) and (expr.TargetDecl = dtInteger)) then
                Emit('', 'LBPJB1', 'INT2FLOATQ');
            Emit('', 'LBPJB1', 'PUSHAQ');
          end;
          dtString:
          begin
          end;
        end;
        for p in TExprArray(tgt).Subscripts do
        begin
            subscript := TExpressionTerm(p);
            Expression(subscript);
            PushExprRslt(subscript);
        end;
        Emit('', 'LQ,W', VariableOperand(tgt.Symbol), '. ' + tgt.Symbol.ID);
        Emit('', 'LBPJB1', 'PUSHQ');
        PushValue(TExprArray(tgt).Subscripts.Count);
        case tgt.DeclType of
          dtInteger,
          dtLogical:
            Emit('', 'LBPJB1', 'PUTARRAYQ');
          dtReal:
            Emit('', 'LBPJB1', 'PUTARRAYAQ');
          dtString:
          begin
            Emit('', 'LBPJB1', 'GETSARRAY');
            Emit('', 'LBPJB1', 'PUSHQ');
            PushSubstring(tgt);
            Emit('', 'LBPJB1', 'STRCPY');
            if (depth <> 0) then
            begin
                // Reset stack ptr for next assignment
                Emit('', 'SB,A', 'B4');
                Emit('', 'AN', '3D');
                Emit('', 'LB,A', 'B4');
            end;
          end;
        end;
    end else
    begin
        case tgt.DeclType of
          dtInteger,
          dtLogical:
          begin
            if (Assigned(expr) and (expr.TargetDecl = dtReal)) then
                Emit('', 'LBPJB1', 'INTEGER');
            if (tgt.Symbol.IsStatic) then
                Emit('',
                     'SQ,W',
                     VariableOperand(tgt.Symbol),
                     Format(AnsiString('. %s'), [tgt.Symbol.ID]))
            else
                Emit();
          end;
          dtReal:
          begin
            if (Assigned(expr) and (expr.TargetDecl = dtInteger)) then
                Emit('', 'LBPJB1', 'INT2FLOATQ');
            if (tgt.Symbol.IsStatic) then
                Emit('',
                     'DPS',
                     VariableOperand(tgt.Symbol),
                     Format(AnsiString('. %s'), [tgt.Symbol.ID]))
            else
                Emit();
          end;
          dtString:
          begin
            Emit('', 'LQ,W', VariableOperand(tgt.Symbol), '. ' + tgt.Symbol.ID);
            Emit('', 'LBPJB1', 'PUSHQ');
            PushSubstring(tgt);
            Emit('', 'LBPJB1', 'STRCPY');
            if (depth <> 0) then
            begin
                // Reset stack ptr for next assignment
                Emit('', 'SB,A', 'B4');
                Emit('', 'AN', '3D');
                Emit('', 'LB,A', 'B4');
            end;
          end;
        end;
    end;
end;

procedure T494CodeGen.Assignment(sym: TSymbol; expr: TExpressionTerm; depth: Integer);
var
    asg: TExprVariable;
begin
    asg := TExprVariable.Create;
    try
        asg.Symbol := sym;
        Assignment(asg, expr, depth);
    finally
        asg.Free;
    end;
end;

procedure T494CodeGen.BlockBegin(block: TBlock);
begin
    if (not FStarted) then
    begin
        PgmStart(block.IsMain);
        FStarted := True;
    end;
    if (block.IsMain) then
    begin
        Emit('', 'EDEF', 'ALG$STACK');
        Emit('.');
        Emit('L$MAIN', '.');
        Emit('', 'LBPJB1', 'ALG$INIT');
        Emit('', Format(AnsiString('+%dD'), [StackSize]));
    end;
    Emit('', 'J', Format(AnsiString('B$%d'), [block.BlockNum]));
end;

procedure T494CodeGen.BlockEnd(block: TBlock);
var
    i: Integer;
    sym: TSymbol;
begin
    for i := 0 to block.Symbols.Count - 1 do
    begin
        sym := block.Symbols.Symbols[i];
        if (sym.SymbolType = stVariable) then
        begin
            if (sym.IsStatic) then
                FreeStaticVar(sym)
            else
                FreeStackVar(sym);
        end;
    end;
end;

procedure T494CodeGen.BlockStart(block: TBlock);
var
    i: Integer;
    sym: TSymbol;
begin
    Emit(Format(AnsiString('B$%d'), [block.BlockNum]), '.');
    if (block.IsProc) then
    begin
        Emit('', 'SB,A', 'B4', '. SET BLOCK STACK TOP PTR');
        Emit('', 'LB,A', 'B5');
    end;
    for i := 0 to block.Symbols.Count - 1 do
    begin
        sym := block.Symbols.Symbols[i];
        if (sym.SymbolType = stVariable) then
        begin
            if (sym.IsStatic) then
                InitStaticVar(sym)
            else
                InitStackVar(sym);
        end;
    end;
end;

procedure T494CodeGen.CallSwitch(sym: TSymbol; isGoto: Boolean);
begin
    if (isGoto) then
        Emit('',
             'LBPJB2',
             Format(AnsiString('L$%d'), [sym.SymbolNum]),
             '. ' + sym.ID)
    else
        Emit('',
             'J',
             Format(AnsiString('L$%d'), [sym.SymbolNum]),
             '. ' + sym.ID);
end;

procedure T494CodeGen.Comment(s: AnsiString);
begin
    Emit('.', s);
end;

constructor T494CodeGen.Create(outFile: TSrcFile);
begin
    inherited;
    FLabelIndent := 8;
    FOpcodeIndent := 20;
    FOperandIndent := 30;
    FCommentIndent := 50;
end;

procedure T494CodeGen.DesignationalExpr(expr: TExpressionTerm; isGoto: Boolean);
var
    iff: TExprIf;
begin
    if (not Assigned(expr)) then
        Exit;

    if (expr is TExprLabel) then
    begin
        Emit('',
             'J',
             Format(AnsiString('L$%d'), [TExprLabel(expr).Symbol.SymbolNum]),
             '. ' + TExprLabel(expr).Symbol.ID);
    end else if (expr is TExprSwitch) then
    begin
        Expression(TExprSwitch(expr).Index);
        CallSwitch(TExprSwitch(expr).Symbol, isGoto);
    end else if (expr is TExprIf) then
    begin
        iff := TExprIf(expr);
        Expression(iff.IfExpr);
        Emit('', 'SQ,A');
        Emit('', 'JT', Format(AnsiString('L$%d,,AZERO'), [iff.ElseLblNum]));
        DesignationalExpr(iff.ThenExpr, isGoto);
        if (not (iff.ThenExpr is TExprLabel)) then
            Emit('', 'J', Format(AnsiString('L$%d'), [iff.EndLblNum]));
        Labell(iff.ElseLblNum);
        DesignationalExpr(iff.ElseExpr, isGoto);
        Labell(iff.EndLblNum);
    end;
end;

destructor T494CodeGen.Destroy;
begin
    inherited;
end;

procedure T494CodeGen.Emit(lbl, opcode, operand, cmnt: AnsiString);
begin
    case FLastEmit of
      emtPushA:
        inherited Emit('', 'LBPJB1', 'PUSHA');
      emtPushQ:
        inherited Emit('', 'LBPJB1', 'PUSHQ');
      emtPushAQ:
        inherited Emit('', 'LBPJB1', 'PUSHAQ');
    end;
    FLastEmit := emtOther;
    inherited;
end;

procedure T494CodeGen.Expression(expr: TExpressionTerm);
var
    iff: TExprIf;
    arr: TExprArray;
    subscript: TExpressionTerm;
begin
    if (expr.ErrorCount <> 0) then
        Exit;

    if (expr is TExprIf) then
    begin
        iff := TExprIf(expr);
        Expression(iff.IfExpr);
        ApplyOperator(iff, iff.IfExpr, dtLogical);
        Expression(iff.ThenExpr);
        iff.OType := otThen;
        ApplyOperator(iff, iff.ThenExpr, dtLogical);
        Expression(iff.ElseExpr);
        iff.OType := otElse;
        ApplyOperator(iff, iff.ElseExpr, dtLogical);
    end else if (expr is TExprOperator) then
    begin
        if (Assigned(expr.Left)) then
        begin
            if (expr.Left is TExprOperator) then
            begin
                Expression(expr.Left);
                if (expr.Right is TExprOperator) then
                begin
                    PushExprRslt(expr.Left);
                    Expression(expr.Right);
                    PopExprRslt(expr.Left);
                    ApplyOperator(TExprOperator(expr), expr.Right, expr.TargetDecl);
                end else
                begin
                    ApplyOperator(TExprOperator(expr), expr.Right, expr.TargetDecl);
                end;
            end else
            begin
                if (expr.Right is TExprOperator) then
                begin
                    Expression(expr.Right);
                    case expr.Right.TargetDecl of
                      dtInteger,
                      dtLogical:
                        Emit('', 'SQ,A');
                      dtReal:
                        Emit('', 'DPS', 'TEMP$AQ');
                    end;
                    LoadQ(expr.Left);
                    ApplyOperator(TExprOperator(expr), expr.Right, expr.TargetDecl);
                end else
                begin
                    LoadQ(expr.Left);
                    ApplyOperator(TExprOperator(expr), expr.Right, expr.TargetDecl);
                end;
            end;
        end else if (Assigned(expr.Right)) then
        begin
            if (TExprOperator(expr).OType = otGetArray) then
            begin
                arr := TExprArray(expr.Right);
                for subscript in arr.Subscripts do
                begin
                    Expression(subscript);
                    PushExprRslt(subscript);
                end;
                Emit('', 'LQ,W',
                     VariableOperand(arr.Symbol),
                     '. ' + arr.Symbol.ID);
                Emit('', 'LBPJB1', 'PUSHQ');
                PushValue(arr.Subscripts.Count);
                ApplyOperator(TExprOperator(expr), expr.Right, expr.TargetDecl);
            end else if (expr.Right is TExprOperator) then
            begin
                Expression(expr.Right);
                ApplyOperator(TExprOperator(expr), expr.Right, expr.TargetDecl);
            end else
            begin
                ApplyOperator(TExprOperator(expr), expr.Right, expr.TargetDecl);
            end;
        end;
    end else
        LoadQ(expr);
end;

procedure T494CodeGen.ForEnd(looplbl, endlbl: Integer);
begin
    Emit('', 'J', Format(AnsiString('L$%d'), [looplbl]));
    Labell(endlbl);
    Comment('FOR DO');
end;

procedure T494CodeGen.ForInit;
begin
end;

procedure T494CodeGen.ForListEnd(stmt: TForStatement);
begin
    Emit('', 'J', Format(AnsiString('L$%d'), [stmt.LoopLblNum]));
    Labell(stmt.EndLblNum);
    if (stmt.AsgExpressions.Count > 0) then
    begin
        Emit('', 'SB,A', 'B4');
        if (stmt.LoopVar.DeclType = dtReal) then
            Emit('', 'A', Format(AnsiString('%dD'), [stmt.AsgExpressions.Count * 2]))
        else
            Emit('', 'A', Format(AnsiString('%dD'), [stmt.AsgExpressions.Count]));
        Emit('', 'LB,A', 'B4');
    end;
end;

procedure T494CodeGen.ForListInit(stmt: TForStatement);
var
    p: Pointer;
begin
    Comment('FOR LIST');
    for p in stmt.AsgExpressions do
    begin
        Expression(TExpression(p));
        PushExprRslt(TExpression(p));
    end;
    if (stmt.LoopVar.DeclType = dtReal) then
        Emit('', 'LA', Format(AnsiString('%dD'), [(stmt.AsgExpressions.Count * 2)]))
    else
        Emit('', 'LA', Format(AnsiString('%dD'), [stmt.AsgExpressions.Count]));
    Emit('', 'LBPJB1', 'PUSHA');
    Labell(stmt.LoopLblNum);
    Emit('', 'LBPJB1', 'POPA');
    Emit('', 'JT', Format(AnsiString('L$%d,,AZERO'), [stmt.EndLblNum]));
    Emit('', 'A', '1');
    Emit('', 'SA,L', '$+3');
    if (stmt.LoopVar.DeclType = dtReal) then
        Emit('', 'AN', '3')
    else
        Emit('', 'AN', '2');
    Emit('', 'LBPJB1', 'PUSHA');
    if (stmt.LoopVar.DeclType = dtReal) then
        Emit('', 'DPL', '0,B4')
    else
        Emit('', 'LQ,W', '0,B4');
    Assignment(stmt.LoopVar, nil, 0);
end;

procedure T494CodeGen.ForStep(stmt: TForStatement);
begin
    if ((stmt.AsgExpressions.Count = 0) or
        (not Assigned(stmt.LoopVar)) or
        (not Assigned(stmt.TestExpression))) then
            Exit;

    Comment('FOR STEP');
    Expression(TExpression(stmt.AsgExpressions[0]));
    Assignment(stmt.LoopVar, TExpression(stmt.AsgExpressions[0]), 0);
    Labell(stmt.LoopLblNum);
    Expression(stmt.TestExpression);
    Emit('', 'SQ,A');
    Emit('', 'JT', Format(AnsiString('L$%d,,ANOT'), [stmt.EndLblNum]));
end;

procedure T494CodeGen.ForStepIncr(stmt: TForStatement);
begin
    if (not Assigned(stmt.IncrExpression)) then
        Exit;

    Expression(stmt.IncrExpression);
    if (stmt.LoopVar.DeclType = dtReal) then
    begin
        Emit('', 'FA', VariableOperand(stmt.LoopVar), '. ' + stmt.LoopVar.ID);
        Emit('', 'DPS', VariableOperand(stmt.LoopVar), '. ' + stmt.LoopVar.ID);
    end else
    begin
        Emit('', 'AQ,W', VariableOperand(stmt.LoopVar), '. ' + stmt.LoopVar.ID);
        Emit('', 'SQ,W', VariableOperand(stmt.LoopVar), '. ' + stmt.LoopVar.ID);
    end;
    Emit('', 'J', Format(AnsiString('L$%d'), [stmt.LoopLblNum]));
    Labell(stmt.EndLblNum);
end;

procedure T494CodeGen.ForWhile(stmt: TForStatement);
begin
    if ((stmt.AsgExpressions.Count = 0) or (not Assigned(stmt.TestExpression))) then
        Exit;

    Comment('FOR WHILE');
    Labell(stmt.LoopLblNum);
    Expression(TExpression(stmt.AsgExpressions[0]));
    Assignment(stmt.LoopVar, TExpression(stmt.AsgExpressions[0]), 0);
    Expression(stmt.TestExpression);
    Emit('', 'SQ,A');
    Emit('', 'JT', Format(AnsiString('L$%d,,AZERO'), [stmt.EndLblNum]));
end;

procedure T494CodeGen.ForWhileEnd(stmt: TForStatement);
begin
    Gotoo(stmt.LoopLblNum);
    Labell(stmt.EndLblNum);
end;

procedure T494CodeGen.FreeStackVar(sym: TSymbol);
begin
    ;
end;

procedure T494CodeGen.FreeStaticVar(sym: TSymbol);
begin
    if (sym.IsArray) then
    begin
        Emit('', 'LQ,W', VariableOperand(sym), Format(AnsiString('. %s'), [sym.ID]));
        Emit('', 'LBPJB1', 'FREEARRAY');
    end else if (sym.DeclType = dtString) then
    begin
        if (not (sym is TSubstringSymbol)) then
        begin
            Emit('', 'LQ,W', VariableOperand(sym), Format(AnsiString('. %s'), [sym.ID]));
            Emit('', 'LBPJB1', 'FREESTRING');
        end;
    end;
end;

procedure T494CodeGen.GotoIfFalse(target: Integer);
begin
    Emit('', 'SQ,A');
    Emit('', 'JT', Format(AnsiString('L$%d,,AZERO'), [target]));
end;

procedure T494CodeGen.Gotoo(target: Integer);
begin
    Emit('', 'J', Format(AnsiString('L$%d'), [target]));
end;

procedure T494CodeGen.Gotoo(target: TExpressionTerm);
begin
    DesignationalExpr(target, True);
end;

procedure T494CodeGen.IfElse(iff: TIfStatement);
begin
    Emit('', 'J', Format(AnsiString('L$%d'), [iff.EndLblNum]));
    Labell(iff.ElseLblNum);
end;

procedure T494CodeGen.IfEnd(iff: TIfStatement);
begin
    Labell(iff.EndLblNum);
end;

procedure T494CodeGen.IfThen(iff: TIfStatement);
begin
    if (not Assigned(iff)) then
        Exit;

    Expression(iff.Expr);
    Emit('', 'SQ,A');
    Emit('', 'JT', Format(AnsiString('L$%d,,AZERO'), [iff.ElseLblNum]));
end;

procedure T494CodeGen.InitStackVar(sym: TSymbol);
begin
end;

procedure T494CodeGen.InitStaticVar(sym: TSymbol);
begin
    if (sym.IsArray) then
    begin
        if (not (sym is TSubstringSymbol)) then
        begin
            Emit('', 'LBPJB2', Format(AnsiString('L$%d'), [sym.NewArrayLabelNum]));
            Emit('', 'SQ,W', VariableOperand(sym), '. ' + sym.ID);
        end;
    end else
    begin
        case sym.DeclType of
          dtInteger,
          dtLogical:
          begin
            Emit('', 'SZ,W', VariableOperand(sym));
          end;
          dtReal:
          begin
            Emit('', 'SZ,W', VariableOperand(sym));
            Emit('', 'SZ,W', Format(AnsiString('V$%d+1'), [sym.SymbolNum]));
          end;
          dtString:
          begin
            if (not (sym is TSubstringSymbol)) then
            begin
                Emit('', 'LQ', Format(AnsiString('%dD'), [sym.StringLength]));
                Emit('', 'LBPJB1', 'NEWSTRING');
                Emit('', 'SQ,W', Format(AnsiString('V$%d'), [sym.SymbolNum]), '. ' + sym.ID);
            end;
          end;
        end;
    end;
end;

procedure T494CodeGen.Labell(sym: TSymbol);
begin
    Emit(Format(AnsiString('L$%d'), [sym.SymbolNum]), '',  '', '. ' + sym.ID);
end;

procedure T494CodeGen.Labell(sym: Integer);
begin
    Emit(Format(AnsiString('L$%d'), [sym]));
end;

function T494CodeGen.LiteralOperand(lit: TLiteral; j: AnsiString): AnsiString;
begin
    if (j <> '') then
        j := ',,' + j;
    Result := Format(AnsiString('C$%d%s'), [lit.LiteralNum, j]);
end;

procedure T494CodeGen.LoadQ(term: TExpressionTerm);
var
    v: TExprVariable;
    lit: TExprLiteral;
    itemp: Integer;
begin
    if (term is TExprVariable) then
    begin
        v := TExprVariable(term);
        case v.Symbol.DeclType of
          dtInteger,
          dtLogical:
          begin
            Emit('', 'LQ,W', VariableOperand(v.Symbol), Format(AnsiString('. %s'), [v.Symbol.ID]));
            if (v.TargetDecl = dtReal) then
                Emit('', 'LBPJB1', 'INT2FLOATQ');
          end;
          dtReal:
          begin
            Emit('', 'DPL', VariableOperand(v.Symbol), Format(AnsiString('. %s'), [v.Symbol.ID]));
          end;
          dtString:
          begin
            Emit('', 'LQ,W', VariableOperand(v.Symbol), Format(AnsiString('. %s'), [v.Symbol.ID]));
            Emit('', 'LBPJB1', 'PUSHQ');
            PushSubstring(v);
          end;
        end;
    end else if (term is TExprLiteral) then
    begin
        lit := TExprLiteral(term);
        case lit.Literal.DeclType of
          dtInteger,
          dtLogical:
          begin
            if (not TryStrToInt(String(lit.Literal.Value), itemp)) then
                itemp := 0;
            if ((itemp >= 0) and (itemp <= 32767)) then
            begin
                Emit('', 'LQ', lit.Literal.Value + 'D');
                Dec(lit.Literal.RefCount);
            end else if ((itemp < 0) and (Abs(itemp) <= 16383)) then
            begin
                Emit('', 'LQ,X', lit.Literal.Value + 'D');
                Dec(lit.Literal.RefCount);
            end else
            begin
                Emit('', 'LQ,W', LiteralOperand(lit.Literal), Format(AnsiString('. %s'), [lit.Literal.Value]));
            end;
            if (lit.TargetDecl = dtReal) then
                Emit('', 'LBPJB1', 'INT2FLOATQ');
          end;
          dtReal:
          begin
            Emit('', 'DPL', LiteralOperand(lit.Literal), Format(AnsiString('. %s'), [lit.Literal.Value]));
          end;
          dtString:
          begin
            Emit('', 'LQ', LiteralOperand(lit.Literal), Format(AnsiString('. %-10.10s'), [lit.Literal.Value]));
            Emit('', 'LBPJB1', 'PUSHQ');
            PushValue(1);
            PushValue(Length(lit.Literal.Value));
          end;
        end;
    end else if (term is TExprOperator) then
    begin
        ;
    end;
end;

procedure T494CodeGen.NewArray(arr: TSymbol);
var
    p: Pointer;
    expr: TExpression;
begin
    Labell(arr.NewArrayLabelNum);
    for p in arr.Indices do
    begin
        expr := TExpression(p);
        Expression(expr);
        PushExprRslt(expr);
    end;
    PushValue(arr.ArraySubscripts);
    case arr.DeclType of
      dtInteger,
      dtLogical:
      begin
        PushValue(1);
        Emit('', 'LBPJB1', 'NEWARRAY');
      end;
      dtReal:
      begin
        PushValue(2);
        Emit('', 'LBPJB1', 'NEWARRAY');
      end;
      dtString:
      begin
        PushValue(((arr.StringLength - 1) div 5) + 2);
        PushValue(arr.StringLength);
        Emit('', 'LBPJB1', 'STRARRAY');
      end
      else
        PushValue(1);
    end;
    Emit('', 'J', '0,B2');
end;

procedure T494CodeGen.PgmEnd(isMain: Boolean; lits: TLiteralList; tmpVars: TSymbolTable);
var
    lit: TLiteral;
    i: Integer;
    heapSize: Integer;
    stemp: AnsiString;
begin
    Emit('', 'HALT', '0');
    Emit('', '.');
    Emit('TEMP$AQ', 'RES', '2');
    Emit('TEMP$AQ2', 'RES', '2');
    Emit('SPACES', '+''     ''');
    for lit in lits do
    begin
        if (lit.RefCount > 0) then
        begin
            case lit.DeclType of
              dtInteger,
              dtLogical:
              begin
                Emit(Format(AnsiString('C$%d'), [lit.LiteralNum]),
                     Format(AnsiString('%sD'), [lit.Value]));
              end;
              dtReal:
              begin
                Emit(Format(AnsiString('C$%d'), [lit.LiteralNum]),
                     'DLD',
                     lit.Value);
              end;
              dtString:
              begin
                Emit(Format(AnsiString('C$%d'), [lit.LiteralNum]),
                     Format(AnsiString('+%dD'), [Length(lit.Value)]));
                stemp := AnsiString(lit.Value);
                repeat
                    Emit('', Format(AnsiString('+''%s'''), [Copy(stemp, 1, 50)]));
                    stemp := Copy(stemp, 51);
                until Length(stemp) <= 0;
              end;
            end;
        end;
    end;
    for i := 0 to tmpVars.Count - 1 do
    begin
        StaticVar(tmpVars.Symbols[i]);
    end;
    Emit('', '.');
    if (isMain) then
    begin
        Emit('ALG$STACK', 'RES', Format(AnsiString('%dD'), [StackSize]));
        heapSize := Max(MinHeapSize, ReqHeapSize);
        Emit('ALG$HEAP', 'RES', Format(AnsiString('%dD'), [heapSize]));
        Emit('', '.');
    end;
    Emit('', 'END');
end;

procedure T494CodeGen.PgmStart(isMain: Boolean);
begin
    if (isMain) then
        Emit('', 'START', 'L$MAIN')
    else
        Emit('', 'START');
    Emit('.');
    Emit('', 'ER$DEF');
    Emit('.');
    Emit('', 'XREF', 'ALG$INIT');
    Emit('', 'XREF', 'POPA, POPQ, POPAQ, PUSHA, PUSHQ, PUSHAQ');
    Emit('', 'XREF', 'NEWARRAY, FREEARRAY, GETARRAYQ, GETARRAYAQ');
    Emit('', 'XREF', 'GETSARRAY');
    Emit('', 'XREF', 'PUTARRAYQ, PUTARRAYAQ, INT2FLOATA, INT2FLOATQ');
    Emit('', 'XREF', 'NEWSTRING, FREESTRING, STRARRAY, STRCPY');
    Emit('', 'XREF', 'STRCOMP, SETSYSFILE, WR$FLUSH');
    Emit('', 'XREF', 'WR$INT, WR$REAL, WR$BOOL, WR$STR');
    Emit('', 'XREF', 'WR$IARRAY, WR$RARRAY, WR$BARRAY, WR$SARRAY');
    Emit('', 'XREF', 'SIGNQ, SIGNAQ, TRUNC, FRAC, ENTIER, INTEGER');
    Emit('', 'XREF', 'POWER');
    FStarted := True;
end;

procedure T494CodeGen.PopExprRslt(expr: TExpressionTerm);
begin
    case expr.TargetDecl of
      dtInteger,
      dtLogical:
      begin
        Emit('', 'SQ,A');
        Emit('', 'LBPJB1', 'POPQ');
      end;
      dtReal:
      begin
        Emit('', 'DPS', 'TEMP$AQ');
        Emit('', 'LBPJB1', 'POPAQ');
      end;
    end;
end;

procedure T494CodeGen.PushExprRslt(expr: TExpressionTerm);
begin
    case expr.TargetDecl of
      dtInteger,
      dtLogical:
        Emit('', 'LBPJB1', 'PUSHQ');
      dtReal:
        Emit('', 'LBPJB1', 'PUSHAQ');
    end;
end;

procedure T494CodeGen.PushSubstring(expr: TExpressionTerm);
var
    send: Integer;
begin
    with TExprVariable(expr) do
    begin
        if (Assigned(StringStart)) then
        begin
            Expression(StringStart);
            PushExprRslt(StringStart);
            // if the substring starts at position 1 then the length is equal to
            // the ending position of the string, which is what we want. Otherwise,
            // we need to calculate the ending position of the substring.
            if ((StringStart is TExprLiteral) and
                (TExprLiteral(StringStart).Literal.AsInteger = 1)) then
            begin
                Expression(StringLength);
                PushExprRslt(StringLength);
            end else if ((StringStart is TExprLiteral) and (StringLength is TExprLiteral)) then
            begin
                send := TExprLiteral(StringStart).Literal.AsInteger +
                        TExprLiteral(StringLength).Literal.AsInteger -
                        1;
                PushValue(send);
            end else
            begin
                Expression(StringStart);        // StringStart + StringLength - 1
                PushExprRslt(StringStart);
                Expression(StringLength);
                PopExprRslt(StringStart);
                Emit('', 'AQ,A');
                Emit('', 'LA', '1D');
                Emit('', 'ANQ,A');
                Emit('', 'LBPJB1', 'PUSHQ');
            end;
        end else
        begin
            if (Symbol is TSubstringSymbol) then
            begin
                with TSubstringSymbol(Symbol) do
                begin
                    PushValue(Start);
                    PushValue(Start + Length - 1);
                end;
            end else
            begin
                PushValue(1);
                PushValue(Symbol.StringLength);
            end;
        end;
    end;
end;

procedure T494CodeGen.PushValue(sym: TSymbol);
begin
    Emit('', 'LQ,W', VariableOperand(sym), Format(AnsiString('. %s'), [sym.ID]));
    Emit('', 'LBPJB1', 'PUSHQ');
end;

procedure T494CodeGen.PushValue(value: Integer);
begin
    Emit('', 'LQ', Format(AnsiString('%dD'), [value]));
    Emit('', 'LBPJB1', 'PUSHQ');
end;

procedure T494CodeGen.StackVar(sym: TSymbol);
begin
end;

procedure T494CodeGen.StaticVar(sym: TSymbol);
var
    opcode: AnsiString;
    operand: AnsiString;
begin
    case sym.DeclType of
      dtInteger,
      dtLogical:
      begin
        opcode := '+0';
        operand := '';
      end;
      dtReal:
      begin
        opcode := 'DLD';
        operand := '0.0';
      end;
      dtArray,
      dtString:
      begin
        opcode := '+0';
        operand := '';
      end;
    end;
    Emit(Format(AnsiString('V$%d'), [sym.SymbolNum]), opcode, operand,
         Format(AnsiString('. %s'), [sym.ID]));
end;

procedure T494CodeGen.Switch(sym: TSwitchSymbol);
var
    i: Integer;
    expr: TExpressionTerm;
begin
    Emit(Format(AnsiString('L$%d'), [sym.SymbolNum]));
    Emit('', 'ANQ', '1,,QPOS');
    Emit('', 'J', '0,B2');
    for i := 0 to sym.Targets.Count - 1 do
    begin
        expr := TExpressionTerm(sym.Targets[i]);
        if (i <> 0) then
            Emit(Format(AnsiString('S$%d'), [sym.FirstSwitchNum + i]));
        Emit('', 'TQ', Format(AnsiString('%dD,,YMORE'), [i + 1]));
        Emit('', 'J', Format(AnsiString('S$%d'), [sym.FirstSwitchNum + i + 1]));
        DesignationalExpr(expr, False);
    end;
    Emit(Format(AnsiString('S$%d'), [sym.FirstSwitchNum + sym.Targets.Count]));
    Emit('', 'J', '0,B2');
end;

function T494CodeGen.VariableOperand(sym: TSymbol; j: AnsiString): AnsiString;
begin
    if (sym.IsStatic) then
    begin
        if (j <> '') then
            j := ',,' + j;
        Result := Format(AnsiString('V$%d%s'), [sym.SymbolNum, j]);
    end else
    begin
        Result := '';
    end;
end;

procedure T494CodeGen.Write(wr: TWriteStatement);
var
    i: Integer;
    expr: TExpression;
begin
    Emit('', 'LQ', Format(AnsiString('%dD'), [Integer(wr.Device)]));
    Emit('', 'LBPJB1', 'SETSYSFILE');
    expr := nil;
    for i := 0 to wr.Params.Count - 1 do
    begin
        expr := TExpression(wr.Params.Items[i]);
        if (TExpressionTerm(expr) is TExprArray) then
        begin
            if (TExprArray(expr).Subscripts.Count = 0) then
            begin
                Emit('',
                     'LQ,W',
                     VariableOperand(TExprArray(expr).Symbol),
                     '. ' + TExprArray(expr).Symbol.ID);
                case expr.TargetDecl of
                  dtInteger:
                    Emit('', 'LBPJB1', 'WR$IARRAY');
                  dtReal:
                    Emit('', 'LBPJB1', 'WR$RARRAY');
                  dtArray: ;
                  dtString:
                    Emit('', 'LBPJB1', 'WR$SARRAY');
                  dtLogical:
                    Emit('', 'LBPJB1', 'WR$BARRAY');
                end;
                Continue;
            end;
        end;
        Expression(expr);
        case expr.TargetDecl of
          dtInteger:
            Emit('', 'LBPJB1', 'WR$INT');
          dtReal:
            Emit('', 'LBPJB1', 'WR$REAL');
          dtArray: ;
          dtString:
            Emit('', 'LBPJB1', 'WR$STR');
          dtLogical:
            Emit('', 'LBPJB1', 'WR$BOOL');
        end;
    end;
    if (Assigned(expr) and (expr.TargetDecl <> dtString)) then
        Emit('', 'LBPJB1', 'WR$FLUSH');
end;

end.
