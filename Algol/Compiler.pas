unit Compiler;

interface

uses SysUtils, Classes, Generics.Collections, SrcFile, Tokens, CodeGen, U494CodeGen,
     Statements, Symbols, Expressions, Literals;

type
  TTargetType = ( tgtUnknown, tgt494 );

  TCompiler = class
  private
    FSrcFile: TSrcFile;
    FAsmFile: TSrcFile;
    FTokenGen: TTokenGen;
    FBlocks: TBlockStack;
    FLiterals: TLiteralList;
    FExpressionStack: TExpression;
    FCodeGen: TCodeGen;
    FTokenTrace: Boolean;
    FInFile: String;
    FOutDir: String;
    FAsmFileName: String;
    FErrorCount: Integer;
    FTargetType: TTargetType;
    FSymbolNum: Integer;
    FSwitchNum: Integer;
    FBlockNum: Integer;
    FConditionalDepth: Integer;
    FConditionalEndLblNum: Integer;
    function Assignment(var depth: Integer): Boolean;
    function Block(isPgm, isMain, isProc: Boolean): Boolean;
    function Declaration: Boolean;
    function DesignationalExpr(isGoto: Boolean): TExpressionType;
    procedure Endd;
    procedure Error(e: String);
    function Expression(var depth: Integer; var parenCount: Integer): TExpressionType;
    function FindSymbol(id: AnsiString; st: TSymbolType): TSymbol;
    function Forr: Boolean;
    function Gotoo: Boolean;
    function Iff: Boolean;
    function Labell: Boolean;
    function NewBlock(isPgm, isMain, isProc: Boolean): TBlock;
    function NewSymbol(id: AnsiString; st: TSymbolType): TSymbol;
    function ProcedureDecl: Boolean;
    function Programm: Boolean;
    function Statement: Boolean;
  public
    constructor Create;
    function Compile(InFile: String; tokenTrace: Boolean; outDir: String;
                     tgt: TTargetType): Integer;
  end;

implementation

uses IOUtils, Math;

{ TCompiler }

function TCompiler.Assignment(var depth: Integer): Boolean;
var
    ttype1, ttype2: TTokenType;
    token1, token2: AnsiString;
    edepth, eparenCount: Integer;
    sym: TSymbol;
    rslt: TExpressionType;

    procedure Arrayy;
    var
        count: Integer;
    begin
        FTokenGen.Get(ttype1, token1);
        if (ttype1 <> ttLeftParen) then
            raise Exception.CreateFmt('Expected left bracket following array identifier, got %s', [token1]);
        edepth := 0;
        eparenCount := 0;
        count := 0;
        repeat
            rslt := Expression(edepth, eparenCount);
            if (rslt <> etInteger) then
                Error('Subscript expression must be integer');
            FCodeGen.PushExprRslt;
            Inc(count);
            FTokenGen.Get(ttype1, token1);
        until ttype1 <> ttComma;
        if (ttype1 <> ttRightParen) then
        begin
            Error('Subscript list missing closing bracket');
            FTokenGen.Unget(ttype1, token1);
        end;
        if (count <> sym.ArraySubscripts) then
            Error('Incorrect number of array subscripts');
        FCodeGen.AssignArrayEnd(sym);
    end;

begin
    Result := False;
    FTokenGen.Get(ttype1, token1);
    if (ttype1 <> ttIdentifier) then
    begin
        FTokenGen.Unget(ttype1, token1);
        Exit;
    end;
    sym := FindSymbol(token1, stVariable);
    if (not Assigned(sym)) then
    begin
        Error(Format('%s is undefined', [token1]));
        Exit;
    end;
    if (sym.IsArray) then
    begin
        try
            Arrayy;
        except
          on E: Exception do
          begin
            Error(E.Message);
            FTokenGen.FlushLine;
            Exit;
          end;
        end;
    end;

    FTokenGen.Get(ttype2, token2);
    if (ttype2 <> ttAssignment) then
    begin
        Result := False;
        FTokenGen.Unget(ttype2, token2);
        FTokenGen.Unget(ttype1, token1);
        Exit;
    end;

    Result := True;

    Inc(depth);
    if (not Assignment(depth)) then
    begin
        edepth := 0;
        rslt := Expression(edepth, eparenCount);
        if (rslt = etNotExpression) then
        begin
            Error('Expected an assignment or an expression');
            FTokenGen.Get(ttype1, token1);
            while ((ttype1 <>  ttLineSeparator) and (ttype1 <> ttEof)) do
                FTokenGen.Get(ttype1, token1);
            Exit;
        end else if (Assigned(sym) and
                     (((sym.DeclType = dtInteger) and (rslt <> etInteger)) or
                      ((sym.DeclType = dtReal) and (rslt <> etReal)))) then
        begin
            Error('Target variable type does not match expression type');
            Exit;
        end else if (Assigned(sym) and (sym.DeclType = dtLogical) and (rslt <> etLogical)) then
        begin
            Error('Not a boolean expression');
            Exit;
        end;
        FTokenGen.Get(ttype1, token1);
        if (ttype1 <> ttLineSeparator) then
        begin
            FTokenGen.Unget(ttype1, token1);
            Error(Format('Line separator expected, got %s', [token1]));
        end;
    end;

    if (depth = 0) then
    begin
        FTokenGen.Get(ttype1, token1);
        if (ttype1 <> ttLineSeparator) then
        begin
            Error(Format('Expected line separator, got %s', [token1]));
            FTokenGen.Unget(ttype1, token1);
        end;
    end;

    if (Assigned(sym)) then
        FCodeGen.Assignment(sym);
end;

function TCompiler.Block(isPgm, isMain, isProc: Boolean): Boolean;
var
    ttype: TTokenType;
    token: AnsiString;
    b: TBlock;
    I: Integer;
begin
    FTokenGen.Get(ttype, token);
    if ((ttype = ttLabel) or (ttype = ttBegin)) then
    begin
        if (isPgm) then
            FCodeGen.PgmStart(isMain);
    end;
    FTokenGen.Unget(ttype, token);
    while (Labell) do
        ;

    FTokenGen.Get(ttype, token);
    if (ttype <> ttBegin) then
    begin
        Result := False;
        FTokenGen.Unget(ttype, token);
        Exit;
    end;

    FTokenGen.Get(ttype, token);
    if (ttype = ttComment) then
        FTokenGen.FlushLine
    else
        FTokenGen.Unget(ttype, token);

    Result := True;
    b := NewBlock(False, isMain, isProc);
    FBlocks.Push(b);
    try
        FCodeGen.BlockBegin(b);

        while (Declaration) do
            ;

        FCodeGen.BlockStart(b);

        while (Statement) do
            ;

        FTokenGen.Get(ttype, token);
        if (ttype = ttEnd) then
        begin
            Endd;
            for i := 0 to b.Symbols.Count - 1 do
            begin
                if (b.Symbols.Symbols[i].IsForward) then
                    Error(Format('Forward symbol %s not defined', [b.Symbols.Symbols[i].ID]));
            end;
            FCodeGen.BlockEnd(b);
        end else
        begin
            Error(Format('Expected END, got %s', [token]));
            FTokenGen.Unget(ttype, token);
        end;
    finally
        FBlocks.Pop.Free;
    end;
end;

function TCompiler.Compile(InFile: String; tokenTrace: Boolean; outDir: String;
                           tgt: TTargetType): Integer;
var
    ispgm: Boolean;
begin
    try
        FInFile := InFile;
        FTokenTrace := tokenTrace;
        FOutDir := outDir;
        FTargetType := tgt;
        FSrcFile := TSrcFile.Create(InFile);
        FAsmFileName := TPath.GetDirectoryName(InFile) + '\' +
                        TPath.GetFileNameWithoutExtension(InFile) + '.s';
        FAsmFile := TSrcFile.Create(FAsmFileName, fmCreate);
        case FTargetType of
          tgtUnknown:   raise Exception.Create('Unknown target type');
          tgt494:       FCodeGen := T494CodeGen.Create(FAsmFile);
        end;
        FTokenGen := TTokenGen.Create(FSrcFile, FCodeGen, FTokenTrace);

        ispgm := Programm;
        if (not ispgm) then
        begin
            while (ProcedureDecl) do                    // If not a program, do external subroutines
                ;
        end;

        FCodeGen.PgmEnd(ispgm, FLiterals);
    except
      on E: Exception do
      begin
        WriteLn(E.Message);
        Inc(FErrorCount);
      end;
    end;

    FreeAndNil(FCodeGen);
    FreeAndNil(FSrcFile);
    FreeAndNil(FAsmFile);
    WriteLn(Format('%-20.20s: %d error(s) encountered', [TPath.GetFileName(FInFIle), FErrorCount]));
    Result := FErrorCount;
end;

constructor TCompiler.Create;
begin
    FBlocks := TBlockStack.Create;
    FLiterals := TLiteralList.Create;
    FExpressionStack := TExpression.Create;
    FExpressionStack.OwnsObjects := True;
end;

function TCompiler.Declaration: Boolean;
var
    ttype: TTokenType;
    token: AnsiString;
    d: TDeclaration;
    sym: TSymbol;
    b: TBlock;
    done: Boolean;

    function Variable: TSymbol;
    begin
        Result := nil;
        if (b.Symbols.TryGetValue(token, stAny, sym)) then
        begin
            Error(Format('%s is multipy defined', [token]));
            Exit;
        end;
        sym := NewSymbol(token, stAny);
        Result := sym;
        b.Symbols.Add(token, sym);
        sym.DeclType := d.DType;
        if (not b.IsProc) then
        begin
            sym.SymbolType := stVariable;
            sym.IsStatic := True;
            FCodeGen.StaticVar(sym);
        end else
        begin
            sym.SymbolType := stVariable;
            sym.IsStatic := False;
            FCodeGen.StackVar(sym);
        end;
    end;

    procedure Local;
    var
        st: TSymbolType;
    begin
        FTokenGen.Get(ttype, token);
        if (token = 'LABEL') then
            st := stLabel
        else
        begin
            Error(Format('%s is not valid for LOCAL', [token]));
            FTokenGen.FlushLine;
            Exit;
        end;

        b := FBlocks.Peek;
        done := False;
        while (not done) do
        begin
            FTokenGen.Get(ttype, token);
            case ttype of
              ttIdentifier:
              begin
                if (b.Symbols.TryGetValue(token, stAny, sym)) then
                begin
                    Error(Format('%s is multipy defined', [token]));
                    Exit;
                end;
                sym := NewSymbol(token, st);
                b.Symbols.Add(token, sym);
                sym.IsForward := True;
              end;
              ttEof:
              begin
                Error('Unexpected end-of-file');
                Exit;
              end;
              ttLineSeparator:
                done := True;
              ttComma:
                Continue;
              else
              begin
                FTokenGen.Unget(ttype, token);
                Error(Format('Expected identifier or comma, got %s', [token]));
                Exit;
              end;
            end;
        end;
    end;

    procedure Switch;
    var
        idx: Integer;
    begin
        FTokenGen.Get(ttype, token);
        if (ttype <> ttIdentifier) then
        begin
            Error(Format('Identifier expected got %s', [token]));
            FTokenGen.FlushLine;
            Exit;
        end;
        b := FBlocks.Peek;
        if (b.Symbols.TryGetValue(token, stSwitch, sym)) then
        begin
            Error(Format('%s is multiply defined', [token]));
            FTokenGen.FlushLine;
            Exit;
        end;
        sym := NewSymbol(token, stSwitch);
        b.Symbols.Add(token, sym);

        FTokenGen.Get(ttype, token);
        if (ttype <> ttAssignment) then
        begin
            Error(Format('Equal sign expected got %s', [token]));
            FTokenGen.FlushLine;
            Exit;
        end;

        FCodeGen.SwitchStart(sym);
        done := False;
        idx := 0;
        while (not done) do
        begin
            Inc(idx);
            Inc(FSwitchNum);
            FCodeGen.SwitchItem(idx, FSwitchNum);
            if (DesignationalExpr(False) <> etDesignational) then
                Error('Designational expression expected.');
            FTokenGen.Get(ttype, token);
            if (ttype = ttComma) then
                Continue
            else if (ttype = ttLineSeparator) then
                done := True
            else
            begin
                Error(Format('Comma or line separator expected, got %s', [token]));
                FTokenGen.FlushLine;
            end;
        end;
        FCodeGen.SwitchEnd(FSwitchNum);
    end;

    procedure ArraySubscripts;
    var
        et: TExpressionType;
        depth, parencount: Integer;
    begin
        sym.ArraySubscripts := 0;
        depth := 0;
        parenCount := 0;
        Inc(FSymbolNum);
        sym.NewArrayLabelNum := FSymbolNum;
        FCodeGen.NewArrayStart(sym);
        repeat
            et := Expression(depth, parencount);
            if (et = etInteger) then
                FCodeGen.PushExprRslt
            else
                Error('Subscript expression must be integer');
            FTokenGen.Get(ttype, token);
            if (ttype = ttColon) then
            begin
                et := Expression(depth, parencount);
                if (et = etInteger) then
                    FCodeGen.PushExprRslt
                else
                    Error('Subscript expression must be integer');
            end else
                raise Exception.Create('Subscript not of form ll:uu');
            Inc(sym.ArraySubscripts);
            FTokenGen.Get(ttype, token);
        until ttype <> ttComma;
        if (ttype <> ttRightParen) then
        begin
            FTokenGen.Unget(ttype, token);
            Error('Subscript list missing closing bracket');
        end;
        if (sym.ArraySubscripts > 10) then
            Error('Too many array subscripts (max. 10)');
        FCodeGen.NewArrayEnd(sym);
    end;

    procedure Arrayy;
    var
        syms: TList<TSymbol>;
        sym1: TSymbol;
    begin
        syms := TList<TSymbol>.Create;
        try
            FTokenGen.Get(ttype, token);
            while (ttype = ttIdentifier) do
            begin
                sym := Variable;
                syms.Add(sym);
                sym.IsArray := True;
                FTokenGen.Get(ttype, token);
                if (ttype = ttComma) then
                begin
                    FTokenGen.Get(ttype, token);
                    Continue;
                end else if (ttype <> ttLeftParen) then
                begin
                    FTokenGen.Unget(ttype, token);
                    Error('An array declaration must specify some subscripts');
                    Break;
                end;
                try
                    ArraySubscripts;
                except
                  on E: Exception do
                  begin
                    Error(E.Message);
                    FTokenGen.FlushLine;
                  end;
                end;
                // Make all preceeding variables reference the same
                // array defintion.
                for sym1 in syms do
                begin
                    sym1.ArraySubscripts := sym.ArraySubscripts;
                    sym1.NewArrayLabelNum := sym.NewArrayLabelNum;
                end;
                syms.Clear;
                FTokenGen.Get(ttype, token);
                if (ttype = ttComma) then
                    FTokenGen.Get(ttype, token);
            end;
            if (ttype <> ttLineSeparator) then
            begin
                Error(Format('Line separator expected, got %s', [token]));
                FTokenGen.Unget(ttype, token);
            end;
        finally
            syms.Free;
        end;
    end;

begin
    FTokenGen.Get(ttype, token);
    if (token = 'LOCAL') then
    begin
        Result := True;
        Local;
        Exit;
    end else if (token = 'SWITCH') then
    begin
        Result := True;
        Switch;
        Exit;
    end;

    if (ttype = ttDeclarator) then
    begin
        for d in DeclTypes do
        begin
            if (d.ID = token) then
                Break;
        end;
        if (d.DType = dtArray) then
        begin
            Result := True;
            Error('Variable type required before ARRAY');
            Exit;
        end;
        if (d.DType = dtUnknown) then
        begin
            Result := False;
            FTokenGen.Unget(ttype, token);
            Exit;
        end;
    end else
    begin
        Result := False;
        FTokenGen.Unget(ttype, token);
        Exit;
    end;

    Result := True;
    b := FBlocks.Peek;

    FTokenGen.Get(ttype, token);
    if (token = 'ARRAY') then
    begin
        Result := True;
        Arrayy;
        Exit;
    end else
        FTokenGen.Unget(ttype, token);

    done := False;
    while (not done) do
    begin
        FTokenGen.Get(ttype, token);
        case ttype of
          ttIdentifier:
            Variable;
          ttEof:
          begin
            Error('Unexpected end-of-file');
            Exit;
          end;
          ttLineSeparator:
            done := True;
          ttComma:
            Continue;
          else
          begin
            FTokenGen.Unget(ttype, token);
            Error(Format('Expected identifier or comma, got %s', [token]));
            Exit;
          end;
        end;
    end;
end;

function TCompiler.DesignationalExpr(isGoto: Boolean): TExpressionType;
var
    ttype: TTokenType;
    token: AnsiString;
    sym: TSymbol;
    depth, parenCount: Integer;
    elseLbl, endLbl: Integer;
    et: TExpressionType;
begin
    Result := etDesignational;
    depth := 0;
    parenCount := 0;
    FTokenGen.Get(ttype, token);
    case ttype of
      ttIdentifier:
      begin
        sym := FindSymbol(token, stLabel);
        if (Assigned(sym)) then
        begin
            FCodeGen.Gotoo(sym);
        end else
        begin
            sym := FindSymbol(token, stSwitch);
            if (Assigned(sym)) then
            begin
                FTokenGen.Get(ttype, token);
                if (ttype <> ttLeftParen) then
                    Error(Format('Expected left bracket, got %s', [token]));
                et := Expression(depth, parenCount);
                if (et <> etInteger) then
                    Error('Expression is not an integer expression');
                FTokenGen.Get(ttype, token);
                if (ttype <> ttRightParen) then
                    Error(Format('Expected right bracket, got %s', [token]));
                FCodeGen.CallSwitch(sym, isGoto);
            end else
            begin
                Result := etNotExpression;
                Error(Format('%s is undefined', [token]));
            end;
        end;
      end;

      ttIf:
      begin
        if (Expression(depth, parenCount) = etLogical) then
        begin
            FTokenGen.Get(ttype, token);
            if (token <> 'THEN') then
            begin
                Error(Format('THEN expected, got %s', [token]));
                FTokenGen.Unget(ttype, token);
            end;
            elseLbl := FSymbolNum + 1;
            endLbl := FSymbolNum + 2;
            Inc(FSymbolNum, 2);
            FCodeGen.GotoIfFalse(elseLbl);
            if (DesignationalExpr(isGoto) = etNotExpression)  then
                Error('Designational expression expected');
            FTokenGen.Get(ttype, token);
            if (token <> 'ELSE') then
            begin
                Error(Format('ELSE expected, got %s', [token]));
                FTokenGen.Unget(ttype, token);
            end;
            FCodeGen.Gotoo(endLbl);
            FCodeGen.Labell(elseLbl);
            if (DesignationalExpr(isGoto) = etNotExpression)  then
                Error('Designational expression expected');
            FCodeGen.Labell(endLbl);
        end else
        begin
            Error('Relational expression expected');
        end;
      end;
    end;
end;

procedure TCompiler.Endd;
var
    ttype: TTokenType;
    token: AnsiString;
begin
    FTokenGen.Get(ttype, token);
    while ((ttype <> ttEof) and (ttype <> ttEnd) and
           (ttype <> ttLineSeparator) and (token <> 'ELSE')) do
        FTokenGen.Get(ttype, token);
    FTokenGen.Unget(ttype, token);
end;

procedure TCompiler.Error(e: String);
var
    s: String;
begin
    s := Format('%d: %s', [FSrcFile.LineNum, e]);
    WriteLn(s);
    FCodeGen.Comment(AnsiString('**** ' + s));
    Inc(FerrorCount);
end;

function TCompiler.Expression(var depth: Integer; var parenCount: Integer): TExpressionType;

    procedure Expression12(var parenCount: Integer; sym: TSymbol);
    var
        ttype: TTokenType;
        token: AnsiString;
        et: TExpressionType;
        opr: TExprOperator;
        elit: TExprLiteral;
        evar: TExprVariable;
        count, i: Integer;
    // process array items by treating the array bracket as a high priority operator
    begin
        FTokenGen.Get(ttype, token);
        if (ttype <> ttLeftParen) then
        begin
            FTokenGen.Unget(ttype, token);
            Error(Format('Expected left bracket following array identifier, got %s', [token]));
            Exit;
        end;
        count := 0;
        repeat
            et := Expression(depth, parenCount);
            if (et = etNotExpression) then
            begin
                Error('Expression expected for array subscript');
                Exit;
            end;
            opr := TExprOperator.Create;
            opr.OType := otPush;
            FExpressionStack.Push(opr);
            Inc(count);
            FTokenGen.Get(ttype, token);
        until ttype <> ttComma;
        if (ttype <> ttRightParen) then
        begin
            FTokenGen.Unget(ttype, token);
            Error(Format('Expected right bracket, got %s', [token]));
        end;
        if (count <> sym.ArraySubscripts) then
            Error('Incorrect number of array subscripts');
        evar := TExprVariable.Create;                               // add array reference
        evar.Symbol := sym;
        FExpressionStack.Push(evar);
        opr := TExprOperator.Create;
        opr.OType := otPush;
        FExpressionStack.Push(opr);
        i := FLiterals.Add(dtInteger, AnsiString(IntToStr(count))); // add subscript count
        elit := TExprLiteral.Create;
        elit.Literal := FLiterals[i];
        FExpressionStack.Push(elit);
        opr := TExprOperator.Create;
        opr.OType := otPush;
        FExpressionStack.Push(opr);
        opr := TExprOperator.Create;                                // add 'get array' psuedo op
        opr.OType := otGetArray;
        FExpressionStack.Push(opr);
    end;

    procedure Expression11(var parenCount: Integer);
    // top of the priority hierarchy. Looking for identifiers, literals
    // and parentheses.
    var
        ttype: TTokenType;
        token: AnsiString;
        i, itemp: Integer;
        ftemp: Double;
        sym: TSymbol;
        evar: TExprVariable;
        elit: TExprLiteral;
        ifOpr, opr: TExprOperator;
        et: TExpressionType;
    begin
        FTokenGen.Get(ttype, token);
        case ttype of
          ttIf:
          begin
            et := Expression(depth, parenCount);
            if (et = etNotExpression) then
            begin
                Error('Logical expression expected following IF');
                Inc(FExpressionStack.ErrorCount);
                Abort;
            end;
            ifOpr := TExprOperator.Create;
            ifOpr.OType := otIf;
            Inc(FSymbolNum);
            ifOpr.ElseLblNum := FSymbolNum;
            if (FConditionalDepth = 0) then
            begin
                Inc(FConditionalDepth);
                Inc(FSymbolNum);
                FConditionalEndLblNum := FSymbolNum;
            end;
            ifOpr.EndLblNum := FConditionalEndLblNum;
            ifOpr.ConditionalDepth := FConditionalDepth;
            FExpressionStack.Push(ifOpr);
            FTokenGen.Get(ttype, token);
            if (token <> 'THEN') then
            begin
                Error(Format('THEN expected, got %s', [token]));
                Inc(FExpressionStack.ErrorCount);
            end;
            et := Expression(depth, parencount);
            if (et = etNotExpression) then
            begin
                Error('Expression expected following THEN');
                Inc(FExpressionStack.ErrorCount);
                Abort;
            end;
            opr := TExprOperator.Create;
            opr.OType := otThen;
            opr.ElseLblNum := ifOpr.ElseLblNum;
            opr.EndLblNum := FConditionalEndLblNum;
            opr.ConditionalDepth := FConditionalDepth;
            FExpressionStack.Push(opr);
            FTokenGen.Get(ttype, token);
            if (token <> 'ELSE') then
            begin
                Error(Format('ELSE expected, got %s', [token]));
                Inc(FExpressionStack.ErrorCount);
            end;
            et := Expression(depth, parencount);
            if (et = etNotExpression) then
            begin
                Error('Expression expected following ELSE');
                Inc(FExpressionStack.ErrorCount);
                Abort;
            end;
            opr := TExprOperator.Create;
            opr.OType := otElse;
            opr.ElseLblNum := ifOpr.ElseLblNum;
            opr.EndLblNum := FConditionalEndLblNum;
            opr.ConditionalDepth := FConditionalDepth;
            FExpressionStack.Push(opr);
            Dec(FConditionalDepth);
          end;
          ttIdentifier:
          begin
            sym := FindSymbol(token, stVariable);
            if (not Assigned(sym)) then
            begin
                Error(Format('%s is undefined', [token]));
                Inc(FExpressionStack.ErrorCount);
            end;
            if (sym.IsArray) then
                Expression12(parenCount, sym)
            else
            begin
                evar := TExprVariable.Create;
                evar.Symbol := sym;
                FExpressionStack.Push(evar);
            end;
            Exit;
          end;
          ttInteger:
          begin
            if (not TryStrToInt(String(token), itemp)) then
            begin
                Error(Format('%s is not a valid integer', [token]));
                Inc(FExpressionStack.ErrorCount);
            end;
            i := FLiterals.Add(dtInteger, token);
            elit := TExprLiteral.Create;
            elit.Literal := FLiterals[i];
            FExpressionStack.Push(elit);
            Exit;
          end;
          ttReal:
          begin
            if (not TryStrToFloat(String(token), ftemp)) then
            begin
                Error(Format('%s is not a valid real number', [token]));
                Inc(FExpressionStack.ErrorCount);
            end;
            i := FLiterals.Add(dtReal, token);
            elit := TExprLiteral.Create;
            elit.Literal := FLiterals[i];
            FExpressionStack.Push(elit);
            Exit;
          end;
          ttEof:
          begin
            FTokenGen.Unget(ttype, token);
            raise Exception.Create('Unexpected end-of-file');
          end;
          ttLineSeparator:
          begin
            FTokenGen.Unget(ttype, token);
            raise Exception.Create('Expression syntax error');
          end;
          ttLeftParen:
          begin
            Inc(parenCount);
            Result := Expression(depth, parenCount);
            if (Result = etNotExpression) then
            begin
                Error('Subexpression syntax error');
                Inc(FExpressionStack.ErrorCount);
                Abort;
            end;
            FTokenGen.Get(ttype, token);
            if (ttype = ttRightParen) then
                Dec(parenCount);
          end;
          ttLogicalValue:
          begin
            if (token = 'TRUE') then
                i := FLiterals.Add(dtLogical, '1')
            else
                i := FLiterals.Add(dtLogical, '0');
            elit := TExprLiteral.Create;
            elit.Literal := FLiterals[i];
            FExpressionStack.Push(elit);
            Exit;
          end;
          ttString:
          begin
            i := FLiterals.Add(dtString, token);
            elit := TExprLiteral.Create;
            elit.Literal := FLiterals[i];
            FExpressionStack.Push(elit);
            Exit;
          end;
          else
          begin
            FTokenGen.Unget(ttype, token);
            raise Exception.Create('Expression Syntax error');
          end;
        end;
    end;

    procedure Expression10(var parenCount: Integer);
    // Exponentiation
    var
        ttype: TTokenType;
        token: AnsiString;
        opr: TExprOperator;
    begin
        Expression11(parenCount);
        FTokenGen.Get(ttype, token);
        while (token = '**') do
        begin
            Expression11(parenCount);
            opr := TExprOperator.Create;
            opr.OType := otExponent;
            FExpressionStack.Push(opr);
            FTokenGen.Get(ttype, token);
        end;
        FTokenGen.Unget(ttype, token);
    end;

    procedure Expression9(var parenCount: Integer);
    // unary minus
    var
        ttype: TTokenType;
        token: AnsiString;
        opr: TExprOperator;
        count: Integer;
    begin
        count := 0;
        FTokenGen.Get(ttype, token);
        while (token = '-') do
        begin
            Inc(count);
            FTokenGen.Get(ttype, token);
        end;
        if (count > 0) then
        begin
            FTokenGen.Unget(ttype, token);
            Expression10(parenCount);
            if ((count mod 2) <> 0) then
            begin
                opr := TExprOperator.Create;
                opr.OType := otUnaryMinus;
                FExpressionStack.Push(opr);
            end;
        end else
        begin
            FTokenGen.Unget(ttype, token);
            Expression10(parenCount);
        end;
    end;

    procedure Expression8(var parenCount: Integer);
    // multiply  & divide
    var
        ttype: TTokenType;
        token: AnsiString;
        opr: TExprOperator;
    begin
        Expression9(parenCount);
        FTokenGen.Get(ttype, token);
        while ((token = '*') or (token = '/') or (token = '//')) do
        begin
            Expression9(parenCount);
            opr := TExprOperator.Create;
            if (token = '*') then
                opr.OType := otMultiply
            else if (token = '/') then
                opr.OType := otDivide
            else
                opr.OType := otIntDivide;
            FExpressionStack.Push(opr);
            FTokenGen.Get(ttype, token);
        end;
        FTokenGen.Unget(ttype, token);
    end;

    procedure Expression7(var parenCount: Integer);
    var
        ttype: TTokenType;
        token: AnsiString;
        opr: TExprOperator;
    begin
        Expression8(parenCount);
        FTokenGen.Get(ttype, token);
        while ((token = '+') or (token = '-')) do
        begin
            Expression8(parenCount);
            opr := TExprOperator.Create;
            if (token = '+') then
                opr.OType := otPlus
            else
                opr.OType := otMinus;
            FExpressionStack.Push(opr);
            FTokenGen.Get(ttype, token);
        end;
        FTokenGen.Unget(ttype, token);
    end;

    procedure Expression6(var parenCount: Integer);
    var
        ttype: TTokenType;
        token: AnsiString;
        opr: TExprOperator;
    begin
        Expression7(parenCount);
        FTokenGen.Get(ttype, token);
        while ((token = 'LSS') or (token = 'LEQ') or
               (token = 'GTR') or (token = 'GEQ') or
               (token = 'EQL') or (token = 'NEQ')) do
        begin
            Expression7(parenCount);
            opr := TExprOperator.Create;
            if (token = 'LSS') then
                opr.OType := otLess
            else if (token = 'LEQ') then
                opr.OType := otLessEqual
            else if (token = 'GTR') then
                opr.OType := otGreater
            else if (token = 'GEQ') then
                opr.OType := otGreaterEqual
            else if (token = 'EQL') then
                opr.OType := otEqual
            else
                opr.OType := otNotEqual;
            FExpressionStack.Push(opr);
            FTokenGen.Get(ttype, token);
        end;
        FTokenGen.Unget(ttype, token);
    end;

    procedure Expression5(var parenCount: Integer);
    var
        ttype: TTokenType;
        token: AnsiString;
        opr: TExprOperator;
        count: Integer;
    begin
        count := 0;
        FTokenGen.Get(ttype, token);
        while (token = 'NOT') do
        begin
            Inc(count);
            FTokenGen.Get(ttype, token);
        end;
        if (count > 0) then
        begin
            FTokenGen.Unget(ttype, token);
            Expression6(parenCount);
            if ((count mod 2) <> 0) then
            begin
                opr := TExprOperator.Create;
                opr.OType := otNot;
                FExpressionStack.Push(opr);
            end;
        end else
        begin
            FTokenGen.Unget(ttype, token);
            Expression6(parenCount);
        end;
    end;

    procedure Expression4(var parenCount: Integer);
    var
        ttype: TTokenType;
        token: AnsiString;
        opr: TExprOperator;
    begin
        Expression5(parenCount);
        FTokenGen.Get(ttype, token);
        while (token = 'AND') do
        begin
            Expression5(parenCount);
            opr := TExprOperator.Create;
            opr.OType := otAnd;
            FExpressionStack.Push(opr);
            FTokenGen.Get(ttype, token);
        end;
        FTokenGen.Unget(ttype, token);
    end;

    procedure Expression3(var parenCount: Integer);
    var
        ttype: TTokenType;
        token: AnsiString;
        opr: TExprOperator;
    begin
        Expression4(parenCount);
        FTokenGen.Get(ttype, token);
        while ((token = 'OR') or (token = 'XOR')) do
        begin
            Expression4(parenCount);
            opr := TExprOperator.Create;
            if (token = 'OR') then
                opr.OType := otOr
            else
                opr.OType := otXor;
            FExpressionStack.Push(opr);
            FTokenGen.Get(ttype, token);
        end;
        FTokenGen.Unget(ttype, token);
    end;

    procedure Expression2(var parenCount: Integer);
    var
        ttype: TTokenType;
        token: AnsiString;
        opr: TExprOperator;
    begin
        Expression3(parenCount);
        FTokenGen.Get(ttype, token);
        while (token = 'IMPL') do
        begin
            Expression3(parenCount);
            opr := TExprOperator.Create;
            opr.OType := otImpl;
            FExpressionStack.Push(opr);
            FTokenGen.Get(ttype, token);
        end;
        FTokenGen.Unget(ttype, token);
    end;

    procedure Expression1(var parenCount: Integer);
    var
        ttype: TTokenType;
        token: AnsiString;
        opr: TExprOperator;
    begin
        Expression2(parenCount);
        FTokenGen.Get(ttype, token);
        while (token = 'EQUIV') do
        begin
            Expression2(parenCount);
            opr := TExprOperator.Create;
            opr.OType := otEquiv;
            FExpressionStack.Push(opr);
            FTokenGen.Get(ttype, token);
        end;
        FTokenGen.Unget(ttype, token);
    end;

    function TypeAndReduce: TExpressionType;
    // Take a quick pass through the expression determining what
    // type the terms need to cast to (and hence the type of the final
    // result) and reducing and constant epressions to a single term
    var
        expr: TExpression;
        terms: TArray<TExpressionItem>;
        term, newTerm, subTerm, left, right: TExpressionItem;
        opr: TExprOperator;
        leftLit, rightLit: TExprLiteral;
        i, icount, lint, rint: Integer;
        lflt, rflt: Double;
        lbool: Boolean;
        inCondition: Boolean;
        condDecl: TDeclType;
        subs: TExpression;
    begin
        Result := etUnknown;
        expr := TExpression.Create;
        expr.OwnsObjects := False;
        inCondition := False;
        condDecl := dtUnknown;
        terms := FExpressionStack.ToArray;
        try
            for term in terms do
            begin
                if (term is TExprOperator) then
                begin
                    opr := TExprOperator(term);
                    if (opr.OType = otGetArray) then
                    begin
                        subs := TExpression.Create;
                        try
                            subs.OwnsObjects := False;
                            subs.Push(expr.Pop);                    // get subscript count
                            right := expr.Pop;
                            subs.Push(right);
                            subs.Push(expr.Pop);                    // get array reference
                            left := expr.Pop;
                            left.TargetDecl := dtInteger;           // force to single word (array address)
                            subs.Push(left);
                            // Make sure that all subscripts are of type integer;
                            Assert(right is TExprLiteral, 'OOPS! Subscript count not a literal');
                            icount := TExprLiteral(right).Literal.AsInteger * 2;
                            while (icount > 0) do
                            begin
                                subTerm := expr.Pop;
                                subs.Push(subTerm);
                                if (subTerm.TargetDecl <> dtInteger) then
                                    Error('Array subscripts must be integer');
                                Dec(icount);
                            end;
                            while (subs.Count > 0) do
                            begin
                                subTerm := subs.Pop;
                                expr.Push(subTerm);
                            end;
                        finally
                            subs.Free;
                        end;
                        // Return everything else to the stack
                        newTerm := TExprOperator.Create;
                        newTerm.Assign(opr);
                        newTerm.TargetDecl := left.DeclType;        // force result to type of array
                        expr.Push(newTerm);
                    end else if (opr.OType in ExprUnaryOperators) then
                    begin
                        left := expr.Pop;
                        if (opr.IsArithmetic and (not left.TargetIsNumeric)) then
                            raise Exception.Create('Non-numeric operand specified for unary minus, THEN or ELSE');
                        if (opr.IsLogical and (not left.TargetIsLogical)) then
                            raise Exception.Create('Non-logical operand specified for NOT or IF');
                        if (opr.OType = otThen) then
                        begin
                            if (inCondition) then
                            begin
                                if (left.TargetDecl <> condDecl) then
                                    raise Exception.Create('Operands in a conditional expression must all be the same type');
                            end else
                                inCondition := True;
                            condDecl := left.TargetDecl;
                        end else if (opr.OType = otElse) then
                        begin
                            if (inCondition) then
                            begin
                                if (opr.TargetDecl <> condDecl) then
                                    raise Exception.Create('Operands in a conditional expression must all be the same type');
                            end;
                            inCondition := False;
                        end;
                        // Reduce constant expressions to a single node
                        if (left is TExprLiteral) then
                        begin
                            leftLit := TExprLiteral(left);
                            case leftLit.TargetDecl of
                              dtInteger,
                              dtLogical:
                                if (not TryStrToInt(String(leftLit.Literal.Value), lint)) then
                                    lint := 0;
                              dtReal:
                                if (not TryStrToFloat(String(leftLit.Literal.Value), lflt)) then
                                    lflt := 0;
                            end;
                            case opr.OType of
                              otUnaryMinus:
                              begin
                                case leftLit.TargetDecl of
                                  dtInteger:
                                  begin
                                    Dec(leftLit.Literal.RefCount);
                                    i := FLiterals.Add(dtInteger, AnsiString(IntToStr(-lint)));
                                    leftLit.Literal := FLiterals[i];
                                  end;
                                  dtReal:
                                  begin
                                    Dec(leftLit.Literal.RefCount);
                                    i := FLiterals.Add(dtInteger, AnsiString(FormatFloat('0.0E', -lflt)));
                                    leftLit.Literal := FLiterals[i];
                                  end;
                                end;
                                expr.Push(leftLit);
                                Continue;
                              end;
                              otNot:
                              begin
                                case leftLit.TargetDecl of
                                  dtInteger:
                                  begin
                                    lint := not lint;
                                  end;
                                  dtLogical:
                                  begin
                                    if (lint = 0) then
                                        lint := 1
                                    else
                                        lint := 0;
                                  end;
                                end;
                                Dec(leftLit.Literal.RefCount);
                                i := FLiterals.Add(dtLogical, AnsiString(IntToStr(lint)));
                                leftLit.Literal := FLiterals[i];
                                expr.Push(leftLit);
                                Continue;
                              end;
                            end;
                        end;    // (left is TExprLiteral)
                        expr.Push(left);
                        if ((opr.OType <> otElse) or
                            ((opr.OType = otElse) and (opr.ConditionalDepth = 1))) then
                        begin
                            newTerm := TExprOperator.Create;
                            newTerm.Assign(opr);
                            newTerm.TargetDecl := left.TargetDecl;
                            expr.Push(newTerm);
                        end;
                    end else   // (opr.OType in ExprUnaryOperators)
                    begin
                        right := expr.Pop;
//                        right.TargetDecl := right.DeclType;
                        left := expr.Pop;
//                        left.TargetDecl := left.DeclType;
                        opr.TargetDecl := left.TargetDecl;
                        if ((right.DeclType = dtInteger) and (left.DeclType = dtReal)) then
                        begin
                            right.TargetDecl := dtReal;
                            opr.TargetDecl := dtReal;
                        end;
                        if ((left.DeclType = dtInteger) and (right.DeclType = dtReal)) then
                        begin
                            left.TargetDecl := dtReal;
                            opr.TargetDecl := dtReal;
                        end;
                        if (opr.IsLogical or opr.IsRelational) then
                            opr.TargetDecl := dtLogical;
                        if ((opr.OType = otExponent) or (opr.OType = otDivide)) then
                        begin
                            left.TargetDecl := dtReal;
                            right.TargetDecl := dtReal;
                            opr.TargetDecl := dtReal;
                        end else if (opr.OType = otIntDivide) then
                        begin
                            if ((left.DeclType <> dtInteger) or (right.DeclType <> dtInteger)) then
                                raise Exception.Create('Both operands of integer divide must be integer');
                        end;
                        if (opr.IsArithmetic and (not (left.TargetIsNumeric and right.TargetIsNumeric))) then
                            raise Exception.Create('Non-numeric operand specified for arithmetic operator');
                        if (opr.IsRelational and (not (left.TargetIsNumeric and right.TargetIsNumeric))) then
                            raise Exception.Create('Non-numeric operand specified for relational operator');
                        if (opr.IsLogical and (not (left.TargetIsLogical and right.TargetIsLogical))) then
                            raise Exception.Create('Non-logical operand specified for logical operator');
                        if ((left is TExprLiteral) and (right is TExprLiteral)) then
                        begin
                            leftLit := TExprLiteral(left);
                            rightLit := TExprLiteral(right);
                            case leftLit.TargetDecl of
                              dtInteger,
                              dtLogical:
                              begin
                                if (not TryStrToInt(String(leftLit.Literal.Value), lint)) then
                                    lint := 0;
                                lflt := lint;
                              end;
                              dtReal:
                              begin
                                if (not TryStrToFloat(String(leftLit.Literal.Value), lflt)) then
                                    lflt := 0;
                              end;
                            end;
                            case rightLit.TargetDecl of
                              dtInteger,
                              dtLogical:
                              begin
                                if (not TryStrToInt(String(rightLit.Literal.Value), rint)) then
                                    rint := 0;
                                rflt := rint;
                              end;
                              dtReal:
                              begin
                                if (not TryStrToFloat(String(rightLit.Literal.Value), rflt)) then
                                    rflt := 0;
                              end;
                            end;
                            case opr.OType of
                              otExponent:
                              begin
                                lflt := Power(lflt, rflt);
                              end;
                              otMultiply:
                              begin
                                if (opr.TargetDecl = dtInteger) then
                                    lint := lint * rint
                                else
                                    lflt := lflt * rflt;
                              end;
                              otDivide:
                              begin
                                lflt := lflt / rflt;
                              end;
                              otIntDivide:
                              begin
                                lint := lint div rint;
                              end;
                              otPlus:
                              begin
                                if (opr.TargetDecl = dtInteger) then
                                    lint := lint + rint
                                else
                                    lflt := lflt + rflt;
                              end;
                              otMinus:
                              begin
                                if (opr.TargetDecl = dtInteger) then
                                    lint := lint - rint
                                else
                                    lflt := lflt - rflt;
                              end;
                              otLess:
                              begin
                                if (opr.TargetDecl = dtInteger) then
                                    lbool := lint < rint
                                else
                                    lbool := lflt < rflt;
                              end;
                              otLessEqual:
                              begin
                                if (opr.TargetDecl = dtInteger) then
                                    lbool := lint <= rint
                                else
                                    lbool := lflt <= rflt;
                              end;
                              otGreater:
                              begin
                                if (opr.TargetDecl = dtInteger) then
                                    lbool := lint > rint
                                else
                                    lbool := lflt > rflt;
                              end;
                              otGreaterEqual:
                              begin
                                if (opr.TargetDecl = dtInteger) then
                                    lbool := lint >= rint
                                else
                                    lbool := lflt >= rflt;
                              end;
                              otEqual:
                              begin
                                if (opr.TargetDecl = dtInteger) then
                                    lbool := lint = rint
                                else
                                    lbool := lflt = rflt;
                              end;
                              otNotEqual:
                              begin
                                if (opr.TargetDecl = dtInteger) then
                                    lbool := lint <> rint
                                else
                                    lbool := lflt <> rflt;
                              end;
                              otAnd:
                              begin
                                lbool := (lint and rint) <> 0;
                              end;
                              otOr:
                              begin
                                lbool := (lint or rint) <> 0;
                              end;
                              otXor:
                              begin
                                lbool := (lint xor rint) <> 0;
                              end;
                              otImpl:
                              begin
                                lbool := (lint = 1) and (rint = 0);
                              end;
                              otEquiv:
                              begin
                                lbool := lint = rint;
                              end;
                            end;
                            Dec(leftLit.Literal.RefCount);
                            Dec(rightLit.Literal.RefCount);
                            case opr.TargetDecl of
                              dtInteger:
                                i := FLiterals.Add(dtInteger, AnsiString(IntToStr(lint)));
                              dtReal:
                                i := FLiterals.Add(dtReal, AnsiString(FormatFloat('0.0E', lflt)));
                              dtLogical:
                                if (lbool) then
                                    i := FLiterals.Add(dtLogical, '1')
                                else
                                    i := FLiterals.Add(dtLogical ,'0');
                              else
                                raise Exception.Create('OOPS! This should never happen');
                            end;
                            leftLit.Literal := FLiterals[i];
                            expr.Push(leftLit);
                            rightLit.Free;
                        end else   // (left is TExprLiteral) and (right is TExprLiteral)
                        begin
                            expr.Push(left);
                            expr.Push(right);
                            newTerm := TExprOperator.Create;
                            newTerm.Assign(opr);
                            expr.Push(newTerm);
                        end;
                    end;
                end else  // (term is TExprOperator)
                begin
                    newTerm := TExpressionItem(term.ClassType.Create);
                    newTerm.Assign(term);
                    newTerm.TargetDecl := newTerm.DeclType;
                    expr.Push(newTerm);
                end;
            end;
            FExpressionStack.Clear;
            terms := expr.ToArray;
            term := nil;
            for term in terms do
                FExpressionStack.Push(term);
            if (Assigned(term)) then
            begin
                if (term is TExprOperator) then
                begin
                    with TExprOperator(term) do
                    begin
                        if (IsArithmetic) then
                        begin
                            if (TargetDecl = dtInteger) then
                                Result := etInteger
                            else
                                Result := etReal;
                        end else if (IsLogical or IsRelational) then
                            Result := etLogical
                        else
                        begin
                            if (term.TargetDecl = dtInteger) then
                                Result := etInteger
                            else if (term.TargetDecl = dtReal) then
                                Result := etReal
                            else if (term.IsLogical) then
                                Result := etLogical
                            else
                                Result := etUnknown;
                        end;
                    end;
                end else
                begin
                    if (term.TargetDecl = dtInteger) then
                        Result := etInteger
                    else if (term.TargetDecl = dtReal) then
                        Result := etReal
                    else if (term.IsLogical) then
                        Result := etLogical
                    else
                        Result := etUnknown;
                end;
            end else
                Result := etNotExpression;
        finally
            expr.Free;
        end;
    end;
const
    oprs: array [TExprOperatorType] of String = (
        '**', '-', '*', '/', '//', '+', '-', 'LSS', 'LEQ',
        'GTR', 'GEQ', 'EQL', 'NEQ', 'NOT', 'AND', 'OR',
        'XOR', 'IMPL', 'EQUIV', 'IF', 'THEN', 'ELSE',
        'GET_ARRAY', 'PUSH'
    );
var
    terms: TArray<TExpressionItem>;
    term: TExpressionItem;
    post: String;
    ttype: TTokenType;
    token: AnsiString;
begin
    Inc(depth);
    if (depth = 1) then
    begin
        FConditionalDepth := 0;
        FExpressionStack.Clear;
        parenCount := 0;
    end;
    try
        FTokenGen.Get(ttype, token);
        if (not ((ttype = ttIdentifier) or (ttype = ttInteger) or (ttype = ttReal) or
                 (ttype = ttLogicalValue) or (ttype = ttString) or // (ttype = ttLabel) or
                 (ttype = ttIf) or (ttype = ttLeftParen) or (token = '-') or
                 (token = 'NOT'))) then
        begin
            Result := etNotExpression;
            Exit;
        end else
            FTokenGen.Unget(ttype, token);
        Expression1(parenCount);
        if (depth = 1) then
        begin
            if (parenCount <> 0) then
                raise Exception.Create('Unbalanced parentheses');
            if (FExpressionStack.ErrorCount = 0) then
            begin
                //**********************
                // for debugging
                // *********************
                post := '';
                terms := FExpressionStack.ToArray;
                for term in terms do
                begin
                    if (term is TExprVariable) then
                        post := post + Format('%s ', [TExprVariable(term).Symbol.ID])
                    else if (term is TExprLiteral) then
                        post := post + Format('%s ', [TExprLiteral(term).Literal.Value])
                    else if (term is TExprOperator) then
                        post := post + Format('%s ', [oprs[TExprOperator(term).OType]]);
                end;
                // ***********************
                Result := TypeAndReduce;
                //**********************
                // for debugging
                // *********************
                post := '';
                terms := FExpressionStack.ToArray;
                for term in terms do
                begin
                    if (term is TExprVariable) then
                        post := post + Format('%s ', [TExprVariable(term).Symbol.ID])
                    else if (term is TExprLiteral) then
                        post := post + Format('%s ', [TExprLiteral(term).Literal.Value])
                    else if (term is TExprOperator) then
                        post := post + Format('%s ', [oprs[TExprOperator(term).OType]]);
                end;
                // ***********************
                FCodeGen.Expression(FExpressionStack);
            end;
        end;
        Dec(depth);
    except
      on E: EAbort do
      begin
        Result := etNotExpression;
      end;
      on E: Exception do
      begin
        Result := etUnknown;
        Error(E.Message);
      end;
    end;
end;

function TCompiler.FindSymbol(id: AnsiString; st: TSymbolType): TSymbol;
var
    blocks: TArray<TBlock>;
    i: Integer;
begin
    Result := nil;
    blocks := FBlocks.ToArray;
    for i := High(blocks) downto Low(blocks) do
    begin
        if (blocks[i].Symbols.TryGetValue(id, st, Result)) then
            Exit;
    end;
end;

function TCompiler.Forr: Boolean;
var
    ttype: TTokenType;
    token: AnsiString;
    loopVar: TSymbol;
    et, et1: TExpressionType;
    depth, parenCount, ecount: Integer;
    loopLbl, endLbl, incLbl: Integer;
begin
    FTokenGen.Get(ttype, token);
    if (ttype <> ttFor) then
    begin
        FTokenGen.Unget(ttype, token);
        Result := False;
        Exit;
    end;

    Result := True;
    FTokenGen.Get(ttype, token);
    if (ttype <> ttIdentifier) then
    begin
        Error(Format('Identifier expected, got %s', [token]));
        FTokenGen.Unget(ttype, token);
    end;
    loopVar := FindSymbol(token, stVariable);
    if (not Assigned(loopVar)) then
    begin
        Error(Format('%s is undefined', [token]));
        FTokenGen.Unget(ttype, token);
    end;
    FTokenGen.Get(ttype, token);
    if (ttype <> ttAssignment) then
    begin
        Error(Format('Assignment expected, got %s', [token]));
        FTokenGen.Unget(ttype, token);
    end;
    // I := e1[, e2[, e3 ...]]]
    FCodeGen.ForInit;
    Inc(FSymbolNum);
    loopLbl := FSymbolNum;
    Inc(FSymbolNum);
    endLbl := FSymbolNum;
    depth := 0;
    parenCount := 0;
    ecount := 0;
    et := Expression(depth, parenCount);
    if (et = etNotExpression) then
        Error(Format('Expression expected after assignment', [token]))
    else
        Inc(ecount);
    FTokenGen.Get(ttype, token);
    if (ttype = ttComma) then
    begin
        FCodeGen.PushExprRslt;
        repeat
            et1 := Expression(depth, parenCount);
            if (et1 = etNotExpression) then
                Error(Format('Expression expected after comma', [token]))
            else if (et1 <> et) then
                Error(Format('All expression in FOR must be same type', [token]))
            else
            begin
                Inc(ecount);
                FCodeGen.PushExprRslt;
                FTokenGen.Get(ttype, token);
            end;
        until ttype <> ttComma;
        FTokenGen.Unget(ttype, token);
    end else
    begin
        FTokenGen.Unget(ttype, token);
    end;
    // STEP or WHILE
    FTokenGen.Get(ttype, token);
    if (token = 'DO') then
    begin
        FCodeGen.ForListInit(ecount, loopLbl, endLbl, loopVar);
        if (not Statement) then
            Error('Statement expected following DO');
        FCodeGen.ForListEnd(ecount, looplbl, endLbl, loopVar);
    end else
    begin
        // STEP
        if (token = 'STEP') then
        begin
            Inc(FSymbolNum);
            incLbl := FSymbolNum;
            FCodeGen.ForStep(inclbl, looplbl, loopVar);
            et1 := Expression(depth, parenCount);
            if (et1 = etNotExpression) then
                Error('Expression expected following STEP')
            else if (et1 <> et) then
                Error('Assignment and STEP expressions must the same type')
            else begin
        // UNTIL
                FTokenGen.Get(ttype, token);
                if (token <> 'UNTIL') then
                begin
                    Error(Format('UNTIL expected, got %s', [token]));
                    FTokenGen.Unget(ttype, token);
                end;
                FCodeGen.ForUntil(loopLbl, loopVar);
                et1 := Expression(depth, parenCount);
                if (et1 <> etLogical) then
                    Error('Boolean expression expected follwing UNTIL');
                FCodeGen.ForStepTest(endlbl);
                FTokenGen.Get(ttype, token);
                if (token <> 'DO') then
                begin
                    Error(Format('Expected DO or WHILE, got %s', [token]));
                    FTokenGen.Unget(ttype, token);
                end;
                if (not Statement) then
                    Error('Statement expected following DO');
                FCodeGen.ForEnd(incLbl, endLbl);
            end;
        end else if (token = 'WHILE') then
        begin
        // WHILE
            FCodeGen.ForWhile(loopLbl, loopVar);
            et1 := Expression(depth, parenCount);
            if (et1 <> etLogical) then
                Error('Boolean expression expected after WHILE');
            FCodeGen.ForWhileTest(endLbl);
            FTokenGen.Get(ttype, token);
            if (token <> 'DO') then
            begin
                Error(Format('Expected DO or WHILE, got %s', [token]));
                FTokenGen.Unget(ttype, token);
            end;
            if (not Statement) then
                Error('Statement expected following DO');
            FCodeGen.ForEnd(loopLbl, endLbl);
        end else
        begin
            Error(Format('Expected STEP or WHILE, got %s', [token]));
            FTokenGen.Unget(ttype, token);
        end;
    end;
end;

function TCompiler.Gotoo: Boolean;
var
    ttype: TTokenType;
    token: AnsiString;
begin
    FTokenGen.Get(ttype, token);
    if (ttype <> ttGoto) then
    begin
        FTokenGen.Unget(ttype, token);
        Result := False;
        Exit;
    end;

    Result := True;
    if (DesignationalExpr(True) = etNotExpression) then
        Error('Designational expression expected');
end;

function TCompiler.Iff: Boolean;
var
    ttype: TTokenType;
    token: AnsiString;
    et: TExpressionType;
    depth, parenCount: Integer;
    elseLbl, endLbl: Integer;
begin
    FTokenGen.Get(ttype, token);
    if (ttype <> ttIf) then
    begin
        FTokenGen.Unget(ttype, token);
        Result := False;
        Exit;
    end;

    Result := True;
    depth := 0;
    parenCount := 0;
    et := Expression(depth, parenCount);
    if (et <> etLogical) then
    begin
        Error('Boolean expression expected');
        Exit;
    end;
    FTokenGen.Get(ttype, token);
    if (token <> 'THEN') then
    begin
        FTokenGen.Unget(ttype, token);
        Error(Format('THEN expected, got %s', [token]));
        Exit;
    end;
    Inc(FSymbolNum);
    elseLbl := FSymbolNum;
    Inc(FSymbolNum);
    endLbl := FSymbolNum;
    FCodeGen.GotoIfFalse(elseLbl);
    if (not Statement) then
    begin
        Error('Statement expected');
        Exit;
    end;
    FCodeGen.Gotoo(endLbl);
    FCodeGen.Labell(elseLbl);
    FTokenGen.Get(ttype, token);
    if (token = 'ELSE') then
    begin
        if (not Statement) then
            Error('Statement expected');
    end;
    FCodeGen.Labell(endLbl);
end;

function TCompiler.Labell: Boolean;
var
    ttype: TTokenType;
    token: AnsiString;
    b: TBlock;
    sym: TSymbol;
begin
    b := FBlocks.Peek;
    FTokenGen.Get(ttype, token);
    if (ttype = ttLabel) then
    begin
        sym := NewSymbol(token, stLabel);
        if (b.Symbols.TryGetValue(token, stLabel, sym)) then
        begin
            if (sym.IsForward) then
            begin
                sym.IsForward := False;
                FCodeGen.Labell(sym);
            end else
                Error(Format('Duplicate label %s', [token]));
        end else
        begin
            sym := NewSymbol(token, stLabel);
            b.Symbols.Add(token, sym);
            FCodeGen.Labell(sym);
        end;
        Result := True;
    end else
    begin
        FTokenGen.Unget(ttype, token);
        Result := False;
    end;
end;

function TCompiler.NewBlock(isPgm, isMain, isProc: Boolean): TBlock;
begin
    Result := TBlock.Create;
    Result.IsMain := isMain;
    Result.IsProc := isProc;
    Result.IsProgram := ispgm;
    Inc(FBlockNum);
    Result.BlockNum := FBlockNum;
end;

function TCompiler.NewSymbol(id: AnsiString; st: TSymbolType): TSymbol;
begin
    Result := TSymbol.Create;
    Result.ID := id;
    Result.SymbolType :=st;
    Inc(FSymbolNum);
    Result.SymbolNum := FSymbolNum;
end;

function TCompiler.ProcedureDecl: Boolean;
var
    ttypeDecl, ttypeProc: TTokenType;
    tokenDecl, tokenProc: AnsiString;
begin
    FTokenGen.Get(ttypeDecl, tokenDecl);
    if ((ttypeDecl = ttDeclarator) and
        ((tokenDecl = 'INTEGER') or (tokenDecl = 'REAL'))) then
    begin
        FTokenGen.Get(ttypeProc, tokenProc);
        if ((ttypeProc <> ttDeclarator) or (tokenProc <> 'PROCEDURE')) then
        begin
            Result := False;
            FTokenGen.Unget(ttypeProc, tokenProc);
            FTokenGen.Unget(ttypeDecl, tokenDecl);
            Exit;
        end;
    end else
    begin
        if ((ttypeDecl <> ttDeclarator) or (tokenDecl <> 'PROCEDURE')) then
        begin
            Result := False;
            FTokenGen.Unget(ttypeDecl, tokenDecl);
            Exit;
        end;
        ttypeDecl := ttUnknown;
    end;

    Result := True;
end;

function TCompiler.Programm: Boolean;
var
    b: TBlock;
begin
    b := NewBlock(True, False, False);
    FBlocks.Push(b);
    Result := Block(True, True, False);
    if (not Result) then
        FBlocks.Pop.Free;
end;

function TCompiler.Statement: Boolean;
var
    ttype: TTokenType;
    token: AnsiString;
    adepth: Integer;
    parent: TBlock;
begin
    Result := False;

    while (Labell) do
        ;

    FTokenGen.Get(ttype, token);
    case ttype of
      ttEof:
      begin
        Result := False;
        Error('Unexpected enf-of-file');
      end;
      ttLineSeparator:
      begin
        Result := True;
      end;
      ttEnd:
      begin
        FTokenGen.Unget(ttype, token);
        Result := False;
      end;
      ttComment:
      begin
        FTokenGen.FlushLine;
        Result := True;
      end;
      ttIdentifier:
      begin
        FTokenGen.Unget(ttype, token);
        adepth := 0;
        Result := Assignment(adepth);
      end;
      ttBegin:
      begin
        FTokenGen.Unget(ttype, token);
        parent := FBlocks.Peek;
        Result := Block(False, False, parent.IsProc);
      end
      else
      begin
        FTokenGen.Unget(ttype, token);
        if (Gotoo) then
        begin
            Result := True;
        end else if (Iff) then
        begin
            Result := True;
        end else if (Forr) then
        begin
            Result := True;
        end;
      end;
    end;
end;

end.
