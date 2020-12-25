unit U494CodeGen;

interface

uses SysUtils, Generics.Collections, CodeGen, AnsiStrings, SrcFile, Statements, Symbols,
     Expressions, Literals;

type
  TEmitType = ( emtOther = 0, emtPushA, emtPushQ, emtPushAQ, emtPopA, emtPopQ, emtPopAQ );

  T494CodeGen = class(TCodeGen)
  protected
    FLastEmit: TEmitType;
    procedure ApplyOperator(opr: TExprOperator; dtype: TDeclType);
    procedure Emit(lbl: AnsiString = ''; opcode: AnsiString = ''; operand: AnsiString = '';
                   cmnt: AnsiString = ''); override;
    procedure EmitPop(reg: AnsiString);
    procedure EmitPush(reg: AnsiString);
    procedure FreeStackVar(sym: TSymbol);
    procedure FreeStaticVar(sym: TSymbol);
    procedure InitStackVar(sym: TSymbol);
    procedure InitStaticVar(sym: TSymbol);
    procedure LoadLeftTerm(term: TExpressionItem; decl: TDeclType = dtUnknown);
    procedure LoadRightTerm(term: TExpressionItem);
  public
    constructor Create(outFile: TSrcFile); override;
    destructor Destroy; override;
    procedure AssignArrayEnd(sym: TSymbol); override;
    procedure Assignment(sym: TSymbol); override;
    procedure BlockBegin(block: TBlock); override;
    procedure BlockEnd(block: TBlock); override;
    procedure BlockStart(block: TBlock); override;
    procedure CallSwitch(sym: TSymbol; isGoto: Boolean); override;
    procedure Comment(s: AnsiString); override;
    procedure Expression(expr: TExpression); override;
    procedure ForEnd(looplbl, endlbl: Integer); override;
    procedure ForInit; override;
    procedure ForListEnd(count, looplbl, endlbl: Integer; loopvar: TSymbol); override;
    procedure ForListExpr; override;
    procedure ForListInit(count, looplbl, endlbl: Integer; loopvar: TSymbol); override;
    procedure ForStep(inclbl, looplbl: Integer; sym: TSymbol); override;
    procedure ForStepTest(endlbl: Integer); override;
    procedure ForUntil(looplbl: Integer; sym: TSymbol); override;
    procedure ForWhile(looplbl: Integer; sym: TSymbol); override;
    procedure ForWhileTest(endlbl: Integer); override;
    procedure GotoIfFalse(target: Integer); override;
    procedure Gotoo(target: TSymbol); overload; override;
    procedure Gotoo(target: Integer); overload; override;
    procedure Labell(sym: TSymbol); overload; override;
    procedure Labell(sym: Integer); overload; override;
    procedure NewArrayStart(sym: TSymbol); override;
    procedure NewArrayEnd(sym: TSymbol); override;
    procedure PgmEnd(isMain: Boolean; lits: TLiteralList); override;
    procedure PgmStart(isMain: Boolean); override;
    procedure PushExprRslt; override;
    procedure PushValue(value: Integer); override;
    procedure PushValue(sym: TSymbol); override;
    procedure StackVar(sym: TSymbol); override;
    procedure StaticVar(sym: TSymbol); override;
    procedure SwitchEnd(lbl: Integer); override;
    procedure SwitchItem(idx, lbl: Integer); override;
    procedure SwitchStart(sym: TSymbol); override;
  end;

implementation

{ T494CodeGen }

procedure T494CodeGen.ApplyOperator(opr: TExprOperator; dtype: TDeclType);
var
    noPush: Boolean;
begin
    noPush := False;
    case opr.OType of
      otExponent:       raise Exception.Create('Not implmented');
      otUnaryMinus:
      begin
        case dtype of
          dtInteger:
            Emit('', 'NQ');
          dtReal:
          begin
            Emit('', 'DPL', 'TEMP$AQ');
            Emit('', 'DPN');
          end;
          dtArray: ;
          dtString: ;
          dtLogical: ;
          dtUnknown: ;
        end;
      end;
      otMultiply:
      begin
        case dtype of
          dtInteger:
            Emit('', 'M,A');
          dtReal:
            Emit('', 'FM', 'TEMP$AQ');
          dtArray: ;
          dtString: ;
          dtLogical: ;
          dtUnknown: ;
        end;
      end;
      otDivide:
      begin
        case dtype of
          dtInteger: ;
          dtReal:
            Emit('', 'FD', 'TEMP$AQ');
          dtArray: ;
          dtString: ;
          dtLogical: ;
          dtUnknown: ;
        end;
      end;
      otIntDivide:
      begin
        case dtype of
          dtInteger:
          begin
            Emit('', 'SA,W', 'TEMP$AQ');
            Emit('', 'ZA');
            Emit('', 'D,W', 'TEMP$AQ');
          end;
          dtReal: ;
          dtArray: ;
          dtString: ;
          dtLogical: ;
          dtUnknown: ;
        end;
      end;
      otPlus:
      begin
        case dtype of
          dtInteger:
            Emit('', 'AQ,A');
          dtReal:
            Emit('', 'FA', 'TEMP$AQ');
          dtArray: ;
          dtString: ;
          dtLogical: ;
          dtUnknown: ;
        end;
      end;
      otMinus:
      begin
        case dtype of
          dtInteger:
            Emit('', 'ANQ,A');
          dtReal:
            Emit('', 'FAN', 'TEMP$AQ');
          dtArray: ;
          dtString: ;
          dtLogical: ;
          dtUnknown: ;
        end;
      end;
      otLess:
      begin
        Emit('', 'ANQ,A', '', '. LSS');
        Emit('', 'SQ,A');
        Emit('', 'LQ', '1', '. TRUE');
        Emit('', 'JT', '$+2,,ANEG');
        Emit('', 'LQ', '0', '. FALSE');
      end;
      otLessEqual:
      begin
        Emit('', 'ANQ,A', '', '. LEQ');
        Emit('', 'SQ,A');
        Emit('', 'LQ', '1', '. TRUE');
        Emit('', 'JT', '$+3,,ANEG');
        Emit('', 'JT', '$+2,,AZERO');
        Emit('', 'LQ', '0', '. FALSE');
      end;
      otGreater:
      begin
        Emit('', 'ANQ,A', '', '. GTR');
        Emit('', 'SQ,A');
        Emit('', 'LQ', '0', '. FALSE');
        Emit('', 'JT', '$+3,,ANEG');
        Emit('', 'JT', '$+2,,AZERO');
        Emit('', 'LQ', '1', '. TRUE');
      end;
      otGreaterEqual:
      begin
        Emit('', 'ANQ,A', '', '. GTE');
        Emit('', 'SQ,A');
        Emit('', 'LQ', '1', '. TRUE');
        Emit('', 'JT', '$+2,,APOS');
        Emit('', 'LQ', '0', '. FALSE');
      end;
      otEqual:
      begin
        Emit('', 'ANQ,A', '', '. EQL');
        Emit('', 'SQ,A');
        Emit('', 'LQ', '1', '. TRUE');
        Emit('', 'JT', '$+2,,AZERO');
        Emit('', 'LQ', '0', '. FALSE');
      end;
      otNotEqual:
      begin
        Emit('', 'ANQ,A', '', '. NEQ');
        Emit('', 'SQ,A');
        Emit('', 'LQ', '1', '. TRUE');
        Emit('', 'JT', '$+2,,ANOT');
        Emit('', 'LQ', '0', '. FALSE');
      end;
      otNot:
      begin
        Emit('', 'SQ,A', '', '. NOT');
        Emit('', 'XOR', '1');
        Emit('', 'LQ,A');
      end;
      otAnd:
      begin
        Emit('', 'LLP,A', '', '. AND');
        Emit('', 'LQ,A');
      end;
      otOr:
      begin
        Emit('', 'SQ,W', 'TEMP$AQ', '. OR');
        Emit('', 'OR,W', 'TEMP$AQ');
        Emit('', 'LQ,A');
      end;
      otXor:
      begin
        Emit('', 'SQ,W', 'TEMP$AQ', '. XOR');
        Emit('', 'XOR,W', 'TEMP$AQ');
        Emit('', 'LQ,A');
      end;
      otImpl:
      begin
        Emit('', 'JT', '$+5,,ANOT', '. IMPL');
        Emit('', 'SQ,A');
        Emit('', 'JT', '$+3,,AZERO');
        Emit('', 'LQ', '0', '. FALSE');
        Emit('', 'J', '$+2');
        Emit('', 'LQ', '1', '. TRUE');
      end;
      otEquiv:
      begin
        Emit('', 'ANQ,A', '0,,ANOT', '. EQUIV');
        Emit('', 'J', '$+3');
        Emit('', 'LQ', '0', '. FALSE');
        Emit('', 'J', '$+2');
        Emit('', 'LQ', '1', '. TRUE');
      end;
      otIf:
      begin
        Emit('', 'SQ,A', '0,,ANOT');
        Emit('', 'J', Format(AnsiString('L$%d'), [opr.ElseLblNum]));
        noPush := True;
      end;
      otThen:
      begin
        Emit('', 'J', Format(AnsiString('L$%d'), [opr.EndLblNum]));
        Emit(Format(AnsiString('L$%d'), [opr.ElseLblNum]));
        noPush := True;
      end;
      otElse:
      begin
        if (opr.ConditionalDepth = 1) then
        begin
            Emit(Format(AnsiString('L$%d'), [opr.EndLblNum]));
            case dtype of
              dtInteger,
              dtLogical:
                EmitPush('Q');
              dtReal:
                EmitPush('AQ');
              dtArray: ;
              dtString: ;
              dtUnknown: ;
            end;
        end;
        noPush := True;
      end;
      otGetArray:
      begin
        case dtype of
          dtInteger,
          dtLogical:
            Emit('', 'LBPJB1', 'GETARRAYQ');
          dtReal:
            Emit('', 'LBPJB1', 'GETARRAYAQ');
          dtString: ;
        end;
      end;
    end;
    if (not noPush) then
    begin
        case dtype of
          dtInteger,
          dtLogical:
            EmitPush('Q');
          dtReal:
            EmitPush('AQ');
          dtArray: ;
          dtString: ;
          dtUnknown: ;
        end;
    end;
end;

procedure T494CodeGen.AssignArrayEnd(sym: TSymbol);
begin
    PushValue(sym);
    PushValue(sym.ArraySubscripts);
end;

procedure T494CodeGen.Assignment(sym: TSymbol);
begin
    case sym.DeclType of
      dtInteger,
      dtLogical:
      begin
        if (sym.IsStatic) then
            Emit('',
                 'SQ,W',
                 Format(AnsiString('V$%d'), [sym.SymbolNum]),
                 Format(AnsiString('. %s'), [sym.ID]))
        else
            Emit();
      end;
      dtReal:
      begin
        if (sym.IsStatic) then
            Emit('',
                 'DPS',
                 Format(AnsiString('V$%d'), [sym.SymbolNum]),
                 Format(AnsiString('. %s'), [sym.ID]))
        else
            Emit();
      end;
      dtArray: ;
      dtString: ;
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
        if (sym.IsStatic) then
            FreeStaticVar(sym)
        else
            FreeStackVar(sym);
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
        if (sym.IsStatic) then
            InitStaticVar(sym)
        else
            InitStackVar(sym);
    end;
end;

procedure T494CodeGen.CallSwitch(sym: TSymbol; isGoto: Boolean);
begin
    if (isGoto) then
        Emit('', 'LBPJB2', Format(AnsiString('L$%d'), [sym.SymbolNum]))
    else
        Emit('', 'J', Format(AnsiString('L$%d'), [sym.SymbolNum]));
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

procedure T494CodeGen.EmitPop(reg: AnsiString);
begin
    if (reg = 'A') then
    begin
        if (FLastEmit <> emtPushA) then
            Emit('', 'LBPJB1', Format(AnsiString('POP%s'), [reg]));
        FLastEmit := emtPopA;
    end else if (reg = 'Q') then
    begin
        if (FLastEmit <> emtPushQ) then
            Emit('', 'LBPJB1', Format(AnsiString('POP%s'), [reg]));
        FLastEmit := emtPopQ;
    end else if (reg = 'AQ') then
    begin
        if (FLastEmit <> emtPushAQ) then
            Emit('', 'LBPJB1', Format(AnsiString('POP%s'), [reg]));
        FLastEmit := emtPopAQ;
    end;
end;

procedure T494CodeGen.EmitPush(reg: AnsiString);
begin
    if (reg = 'A') then
        FLastEmit := emtPushA
    else if (reg = 'Q') then
        FLastEmit := emtPushQ
    else if (reg = 'AQ') then
        FLastEmit := emtPushAQ;
end;

procedure T494CodeGen.Expression(expr: TExpression);
var
    terms: TArray<TExpressionItem>;
    term, left, right: TExpressionItem;
    opr: TExprOperator;
    rslt: TExprResult;
    stack: TExpression;
begin
    terms := expr.ToArray;
    stack := TExpression.Create;
    try
        stack.OwnsObjects := False;
        for term in terms do
        begin
            if (term is TExprOperator) then
            begin
                opr := TExprOperator(term);
                if (opr.OType = otGetArray) then
                begin
                    ApplyOperator(opr, opr.TargetDecl);
                    rslt := TExprResult.Create;
                    rslt.TargetDecl := opr.TargetDecl;
                    stack.Push(rslt);
                end else if (opr.OType in ExprUnaryOperators) then
                begin
                    left := stack.Pop;
                    if (opr.OType = otPush) then
                        LoadLeftTerm(left, dtInteger)
                    else
                        LoadLeftTerm(left);
                    ApplyOperator(opr, left.TargetDecl);
                    if (opr.OType <> otPush) then
                    begin
                        rslt := TExprResult.Create;
                        rslt.TargetDecl := left.TargetDecl;
                        stack.Push(rslt);
                    end;
                    if (left is TExprResult) then
                        left.Free;
                end else
                begin
                    right := stack.Pop;
                    left := stack.Pop;
                    LoadRightTerm(right);
                    LoadLeftTerm(left);
                    ApplyOperator(opr, left.TargetDecl);
                    if ((opr.OType <> otIf) and (opr.OType <> otThen)) then
                    begin
                        rslt := TExprResult.Create;
                        rslt.TargetDecl := left.TargetDecl;
                        stack.Push(rslt);
                    end;
                    if (right is TExprResult) then
                        right.Free;
                    if (left is TExprResult) then
                        left.Free;
                end;
            end else
            begin
                stack.Push(term);
            end;
        end;
        Assert(stack.Count = 1, 'Stack count <> 1 at end of expression generation');
        right := stack.Pop;
        LoadLeftTerm(right);
        if (right is TExprResult) then
            right.Free;
    finally
        stack.Free;
    end;
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

procedure T494CodeGen.ForListEnd(count, looplbl, endlbl: Integer; loopvar: TSymbol);
begin
    Emit('', 'J', Format(AnsiString('L$%d'), [looplbl]));
    Labell(endlbl);
    if (count > 0) then
    begin
        Emit('', 'SB,A', 'B4');
        if (loopvar.DeclType = dtReal) then
            Emit('', 'A', Format(AnsiString('%dD'), [count * 2]))
        else
            Emit('', 'A', Format(AnsiString('%dD'), [count]));
        Emit('', 'LB,A', 'B4');
    end;
end;

procedure T494CodeGen.ForListExpr;
begin
    Emit('', 'LBPJB1', 'PUSHQ');
end;

procedure T494CodeGen.ForListInit(count, looplbl, endlbl: Integer; loopvar: TSymbol);
begin
    if (loopvar.DeclType = dtReal) then
        Emit('', 'LA', Format(AnsiString('%dD'), [(count * 2)]))
    else
        Emit('', 'LA', Format(AnsiString('%dD'), [count]));
    Emit('', 'LBPJB1', 'PUSHA');
    Labell(looplbl);
    Emit('', 'LBPJB1', 'POPA');
    Emit('', 'JT', Format(AnsiString('L$%d,,AZERO'), [endlbl]));
    Emit('', 'A', '1');
    Emit('', 'SA,L', '$+3');
    if (loopvar.DeclType = dtReal) then
        Emit('', 'AN', '3')
    else
        Emit('', 'AN', '2');
    Emit('', 'LBPJB1', 'PUSHA');
    if (loopvar.DeclType = dtReal) then
        Emit('', 'DPL', '0,B4')
    else
        Emit('', 'LQ,W', '0,B4');
    Assignment(loopvar);
    Comment('FOR LIST');
end;

procedure T494CodeGen.ForStep(inclbl, looplbl: Integer; sym: TSymbol);
begin
    Assignment(sym);
    Emit('', 'J', Format(AnsiString('L$%d'), [looplbl]));
    Labell(inclbl);
    Comment('FOR STEP');
end;

procedure T494CodeGen.ForStepTest(endlbl: Integer);
begin
    Emit('', 'SQ,A');
    Emit('', 'JT', Format(AnsiString('L$%d,,ANOT'), [endlbl]));
end;

procedure T494CodeGen.ForUntil(looplbl: Integer; sym: TSymbol);
begin
    if (sym.DeclType = dtReal) then
    begin
        Emit('', 'FA', Format(AnsiString('V$%d'), [sym.SymbolNum]));
        Emit('', 'DPS', Format(AnsiString('V$%d'), [sym.SymbolNum]));
    end else
    begin
        Emit('', 'AQ,W', Format(AnsiString('V$%d'), [sym.SymbolNum]));
        Emit('', 'SQ,W', Format(AnsiString('V$%d'), [sym.SymbolNum]));
    end;
    Labell(looplbl);
    Comment('FOR UNTIL');
end;

procedure T494CodeGen.ForWhile(looplbl: Integer; sym: TSymbol);
begin
    Assignment(sym);
    Labell(looplbl);
    Comment('FOR UNTIL');
end;

procedure T494CodeGen.ForWhileTest(endlbl: Integer);
begin
    Emit('', 'SQ,A');
    Emit('', 'JT', Format(AnsiString('L$%d,,AZERO'), [endlbl]));
end;

procedure T494CodeGen.FreeStackVar(sym: TSymbol);
begin
    ;
end;

procedure T494CodeGen.FreeStaticVar(sym: TSymbol);
begin
    if (sym.IsArray) then
    begin
        Emit('', 'LQ,W',
             Format(AnsiString('V$%d'), [sym.SymbolNum]),
             Format(AnsiString('. %s'), [sym.ID]));
        Emit('', 'LBPJB1', 'FREEARRAY');
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

procedure T494CodeGen.Gotoo(target: TSymbol);
begin
    Emit('', 'J', Format(AnsiString('L$%d'), [target.SymbolNum]));
end;

procedure T494CodeGen.InitStackVar(sym: TSymbol);
begin
end;

procedure T494CodeGen.InitStaticVar(sym: TSymbol);
begin
    if (sym.IsArray) then
    begin
        Emit('', 'LBPJB2', Format(AnsiString('L$%d'), [sym.NewArrayLabelNum]));
        Emit('', 'SQ,W', Format(AnsiString('V$%d'), [sym.SymbolNum]));
    end else
    begin
        case sym.DeclType of
          dtInteger,
          dtLogical:
          begin
            Emit('', 'SZ,W', Format(AnsiString('V$%d'), [sym.SymbolNum]));
          end;
          dtReal:
          begin
            Emit('', 'SZ,W', Format(AnsiString('V$%d'), [sym.SymbolNum]));
            Emit('', 'SZ,W', Format(AnsiString('V$%d+1'), [sym.SymbolNum]));
          end;
          dtString: ;
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

procedure T494CodeGen.LoadLeftTerm(term: TExpressionItem; decl: TDeclType);
var
    v: TExprVariable;
    lit: TExprLiteral;
begin
    if (term is TExprVariable) then
    begin
        v := TExprVariable(term);
        if (decl = dtUnknown) then
            decl := v.DeclType;
        case decl of
          dtInteger,
          dtLogical:
          begin
            Emit('',
                 'LQ,W',
                 Format(AnsiString('V$%d'), [v.Symbol.SymbolNum]),
                 Format(AnsiString('. %s'), [v.Symbol.ID]));
            if (v.TargetDecl = dtReal) then
                Emit('', 'LBPJB1', 'INT2FLOATQ');
          end;
          dtReal:
            Emit('',
                 'DPL',
                 Format(AnsiString('V$%d'), [v.Symbol.SymbolNum]),
                 Format(AnsiString('. %s'), [v.Symbol.ID]));
          dtArray: ;
          dtString: ;
          dtUnknown: ;
        end;
    end else if (term is TExprLiteral) then
    begin
        lit := TExprLiteral(term);
        if (decl = dtUnknown) then
            decl := lit.DeclType;
        case decl of
          dtInteger,
          dtLogical:
          begin
            Emit('',
                 'LQ,W',
                 Format(AnsiString('C$%d'), [lit.Literal.LiteralNum]),
                 Format(AnsiString('. %s'), [lit.Literal.Value]));
            if (lit.TargetDecl = dtReal) then
                Emit('', 'LBPJB1', 'INT2FLOATQ');
          end;
          dtReal:
            Emit('',
                 'DPL',
                 Format(AnsiString('C$%d'), [lit.Literal.LiteralNum]),
                 Format(AnsiString('. %s'), [lit.Literal.Value]));
          dtString: ;
        end;
    end else if (term is TExprResult) then
    begin
        if (decl = dtUnknown) then
            decl := term.TargetDecl;
        case decl of
          dtInteger,
          dtLogical:
            EmitPop('Q');
          dtReal:
            EmitPop('AQ');
          dtArray: ;
          dtString: ;
          dtUnknown: ;
        end;
    end;
end;

procedure T494CodeGen.LoadRightTerm(term: TExpressionItem);
var
    v: TExprVariable;
    lit: TExprLiteral;
begin
    if (term is TExprVariable) then
    begin
        v := TExprVariable(term);
        case v.Symbol.DeclType of
          dtInteger,
          dtLogical:
          begin
            Emit('',
                 'LA,W',
                 Format(AnsiString('V$%d'), [v.Symbol.SymbolNum]),
                 Format(AnsiString('. %s'), [v.Symbol.ID]));
            if (v.TargetDecl = dtReal) then
            begin
                Emit('', 'LBPJB1', 'INT2FLOATA');
                Emit('', 'DPS', 'TEMP$AQ');
            end;
          end;
          dtReal:
          begin
            Emit('',
                 'DPL',
                 Format(AnsiString('V$%d'), [v.Symbol.SymbolNum]),
                 Format(AnsiString('. %s'), [v.Symbol.ID]));
            Emit('', 'DPS', 'TEMP$AQ');
          end;
          dtArray: ;
          dtString: ;
          dtUnknown: ;
        end;
    end else if (term is TExprLiteral) then
    begin
        lit := TExprLiteral(term);
        case lit.Literal.DeclType of
          dtInteger,
          dtLogical:
          begin
            Emit('',
                 'LA,W',
                 Format(AnsiString('C$%d'), [lit.Literal.LiteralNum]),
                 Format(AnsiString('. %s'), [lit.Literal.Value]));
            if (lit.TargetDecl = dtReal) then
            begin
                Emit('', 'LBPJB1', 'INT2FLOATA');
                Emit('', 'DPS', 'TEMP$AQ');
            end;
          end;
          dtReal:
            Emit('',
                 'DPL',
                 Format(AnsiString('C$%d'), [lit.Literal.LiteralNum]),
                 Format(AnsiString('. %s'), [lit.Literal.Value]));
          dtString: ;
        end;
    end else if (term is TExprResult) then
    begin
        case term.TargetDecl of
          dtInteger,
          dtLogical:
            EmitPop('A');
          dtReal:
          begin
            EmitPop('AQ');
            Emit('', 'DPS', 'TEMP$AQ');
          end;
          dtArray: ;
          dtString: ;
          dtUnknown: ;
        end;
    end;
end;

procedure T494CodeGen.NewArrayEnd(sym: TSymbol);
begin
    PushValue(sym.ArraySubscripts);
    PushValue(Integer(sym.DeclType));
    Emit('', 'LBPJB1', 'NEWARRAY');
    Emit('', 'J', '0,B2');
end;

procedure T494CodeGen.NewArrayStart(sym: TSymbol);
begin
    Labell(sym.NewArrayLabelNum);
end;

procedure T494CodeGen.PgmEnd(isMain: Boolean; lits: TLiteralList);
var
    lit: TLiteral;
begin
    Emit('', 'EXIT', '0');
    Emit('', '.');
    Emit('TEMP$AQ', 'RES', '2');
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
              dtString: ;
            end;
        end;
    end;
    Emit('', '.');
    if (isMain) then
    begin
        Emit('ALG$STACK', 'RES', Format(AnsiString('%dD'), [StackSize]));
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
    Emit('', 'XREF', 'INT2FLOATA, INT2FLOATQ');
    FStarted := True;
end;

procedure T494CodeGen.PushExprRslt;
begin
    Emit('', 'LBPJB1', 'PUSHQ');
end;

procedure T494CodeGen.PushValue(sym: TSymbol);
begin
    Emit('', 'LQ,W',
         Format(AnsiString('V$%d'), [sym.SymbolNum]),
         Format(AnsiString('. %s'), [sym.ID]));
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
      dtArray:
      begin
        opcode := '+0';
        operand := '';
      end;
      dtString:
      begin
        opcode := '+0';
        operand := '';
      end;
    end;
    Emit(Format(AnsiString('V$%d'), [sym.SymbolNum]), opcode, operand,
         Format(AnsiString('. %s'), [sym.ID]));
end;

procedure T494CodeGen.SwitchEnd(lbl: Integer);
begin
    Emit(Format(AnsiString('S$%d'), [lbl]));
    Emit('', 'J', '0,B2');
end;

procedure T494CodeGen.SwitchItem(idx,lbl: Integer);
begin
    if (idx <> 1) then
        Emit(Format(AnsiString('S$%d'), [lbl - 1]));
    Emit('', 'TQ', Format(AnsiString('%d,,YMORE'), [idx]));
    Emit('', 'J', Format(AnsiString('S$%d'), [lbl]));
end;

procedure T494CodeGen.SwitchStart(sym: TSymbol);
begin
    Emit(Format(AnsiString('L$%d'), [sym.SymbolNum]));
    Emit('', 'ANQ', '1,,QPOS');
    Emit('', 'J', '0,B2');
end;

end.
