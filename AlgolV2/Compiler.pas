unit Compiler;

interface

uses SysUtils, Classes, Generics.Collections, SrcFile, Tokens, CodeGen, U494CodeGen,
     Statements, Symbols, Expressions, Literals, Contnrs, AnsiStrings;

type
  TTargetType = ( tgtUnknown, tgt494 );

  TCompiler = class
  private
    FSrcFile: TSrcFile;
    FAsmFile: TSrcFile;
    FTokenGen: TTokenGen;
    FBlocks: TBlockStack;
    FLiterals: TLiteralList;
    FTempVars: TSymbolTable;
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
    FConditionalEndLblNum: Integer;
    function Assignment(depth: Integer; dt: TDeclType): Boolean;
    function Block(isPgm, isMain, isProc: Boolean): Boolean;
    function Declaration: Boolean;
    function DesignationalExpr: TExpression;
    procedure Endd;
    procedure Error(e: String);
    function Expression: TExpression;
    function FindSymbol(id: AnsiString; st: TSymbolType): TSymbol;
    function Forr: Boolean;
    function Gotoo: Boolean;
    function Iff: Boolean;
    function Labell: Boolean;
    function NewBlock(isPgm, isMain, isProc: Boolean): TBlock;
    function NewSymbol(id: AnsiString; st: TSymbolType; isArray: Boolean): TSymbol;
    function ProcedureDecl: Boolean;
    function Programm: Boolean;
    function Statement: Boolean;
    function Write: Boolean;
  public
    constructor Create;
    function Compile(InFile: String; tokenTrace: Boolean; outDir: String;
                     tgt: TTargetType): Integer;
  end;

implementation

uses IOUtils, Math;

{ TCompiler }

function TCompiler.Assignment(depth: Integer; dt: TDeclType): Boolean;
var
    ttype1: TTokenType;
    token1: AnsiString;
    sym: TSymbol;
    expr: TExpression;
    asg: TExprVariable;

    function InAssignment: Boolean;
    // We need to scan ahead here to see what the next operator is. If it is := then we are part
    // part of an assignment. Otherwise we are part of an expression.
    var
        save: TTokenStack;
        ttype: TTokenType;
        token: AnsiString;
        t: TToken;
    begin
        save := TTokenStack.Create;
        try
            ttype := ttUnknown;
            while (ttype <> ttLineSeparator) and (ttype <> ttEof) and (ttype <> ttAssignment) do
            begin
                FTokenGen.Get(ttype, token);
                t.TType := ttype;
                t.Value := token;
                save.Push(t);
            end;
            Result := (ttype = ttAssignment);
            while (save.Count > 0) do
            begin
                t := save.Pop;
                FTokenGen.Unget(t.TType, t.Value);
            end;
        finally
            save.Free;
        end;
    end;

    procedure Arrayy;
    var
        ttype: TTokenType;
        token: AnsiString;
    begin
        FTokenGen.Get(ttype, token);
        if (ttype <> ttLeftParen) then
            raise Exception.CreateFmt('Expected left bracket following array identifier, got %s', [token]);

        //
        TExprArray(asg).Subscripts := TList<TExpressionTerm>.Create;
        repeat
            expr := Expression;
            if ((not Assigned(expr)) or (expr.TargetDecl <> dtInteger)) then
                Error('Integer expression expected for array subscript');
            TExprArray(asg).Subscripts.Add(expr);
            FTokenGen.Get(ttype1, token1);
        until ttype1 <> ttComma;
        if (ttype1 <> ttRightParen) then
        begin
            Error('Subscript list missing closing bracket');
            FTokenGen.Unget(ttype1, token1);
        end;
        if (TExprArray(asg).Subscripts.Count <> sym.ArraySubscripts) then
            Error(Format('Incorrect number of array subscripts for %s', [sym.ID]));
    end;

    procedure Stringg;
    // Check to see if substring specified and return the start and end values of
    // the substring.
    var
        ttype: TTokenType;
        token: AnsiString;
        elit: TExprLiteral;
        lit: TLiteral;
    begin
        // Check for left paren. If not present, we are done.
        FTokenGen.Get(ttype, token);
        if (sym.IsArray and (ttype <> ttLeftParen)) then
            raise Exception.CreateFmt('Expected left bracket following array identifier, got %s', [token]);
        if (ttype <> ttLeftParen) then
        begin
            FTokenGen.Unget(ttype, token);
            Exit;
        end;
        // This gets complicated. Values given inside [] may be either
        // substring start / length or array subscripts or both.
        //
        // First gather all values until we see something that is not a
        // comma. If variable is not an array then values are substring
        // start / length and we are done.
        //
        // If variable is an array and next token is not a colon then
        // values are array subscripts and we are done.
        //
        // If next value is a token, copy assumed array subscripts to
        // substring start / length and continue parsing to find array
        // subscripts.
        if (asg) is TExprArray then
            TExprArray(asg).Subscripts := TList<TExpressionTerm>.Create;
        repeat
            expr := Expression;
            if ((not Assigned(expr)) or (expr.TargetDecl <> dtInteger)) then
                Error('Integer expression expected for substring / array subscript');
            if (sym.IsArray) then
                TExprArray(asg).Subscripts.Add(expr)
            else if (not Assigned(TExprVariable(asg).StringStart)) then
                TExprVariable(asg).StringStart := expr
            else if (not Assigned(TExprVariable(asg).StringLength)) then
                TExprVariable(asg).StringLength := expr
            else
            begin
                Error('Too many values given for substring');
            end;
            FTokenGen.Get(ttype, token);
        until ttype <> ttComma;
        //
        if (ttype = ttColon) then
        begin
            if (sym.IsArray) then
            begin
                if (TExprArray(asg).Subscripts.Count > 2) then
                begin
                    Error('Too many values given for substring');
                end else
                begin
                    with TExprArray(asg) do
                    begin
                        StringStart := Subscripts[0];
                        Subscripts.Delete(0);
                        if (Subscripts.Count > 0) then
                        begin
                            StringLength := Subscripts[1];
                            Subscripts.Delete(0);
                        end;
                    end;
                end;
                repeat
                    expr := Expression;
                    if ((not Assigned(expr)) or (expr.TargetDecl <> dtInteger)) then
                    begin
                        Error('Integer expression expected for array subscript');
                    end;
                    TExprArray(asg).Subscripts.Add(expr);
                    FTokenGen.Get(ttype, token);
                until ttype <> ttComma;
            end else
            begin
                Error('Colon only allowed for string arrays');
            end;
        end;
        //
        if (ttype <> ttRightParen) then
        begin
            Error(Format('Right bracket expected, got %s', [token]));
            FTokenGen.Unget(ttype, token);
        end;
        if (sym.IsArray) then
        begin
            if (TExprArray(asg).Subscripts.Count <> sym.ArraySubscripts) then
                Error('Incorrect number of array subscripts');
        end;
        with TExprVariable(asg) do
        begin
            if (Assigned(StringStart) and (not Assigned(StringLength))) then
            begin
                elit := TExprLiteral.Create;
                lit := TLiteral.Create;
                lit.Value := '1';
                lit.DeclType := dtInteger;
                elit.Literal := lit;
                StringLength := elit;
            end;
        end;
    end;

begin
    Result := False;
    FTokenGen.Get(ttype1, token1);
    if (ttype1 <> ttIdentifier) then
    begin
        FTokenGen.Unget(ttype1, token1);
        Exit;
    end;
    if (not InAssignment) then
    begin
        FTokenGen.Unget(ttype1, token1);
        Exit;
    end;

    Result := True;
    expr := nil;
    asg := nil;

    sym := FindSymbol(token1, stVariable);
    if (not Assigned(sym)) then
    begin
        Error(Format('%s is undefined', [token1]));
        FTokenGen.FlushLine;
        Exit;
    end;

    try
        if (Assigned(sym)) then
        begin
            if ((sym.DeclType <> dt) and (dt <> dtUnknown)) then
                Error('All variables in an assignment must be the same type');
            if (sym.IsArray) then
                asg := TExprArray.Create
            else
                asg := TExprVariable.Create;
            asg.Symbol := sym;
            if (sym.DeclType = dtString) then
            begin
                try
                    Stringg;
                except
                  on E: Exception do
                  begin
                    Error(E.Message);
                    FTokenGen.FlushLine;
                    Exit;
                  end;
                end;
            end else
            begin
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
            end;
        end;
        // burn to := operator
        FTokenGen.Get(ttype1, token1);
        if (ttype1 <> ttAssignment) then
            raise Exception.CreateFmt('OOPS! Internal error. Expected := got %s', [token1]);

        if (not Assignment(depth + 1, sym.DeclType)) then
        begin
            expr := Expression;
            if (not Assigned(expr)) then
            begin
                Error('Expected an assignment or an expression following :=');
                FTokenGen.Get(ttype1, token1);
                while ((ttype1 <>  ttLineSeparator) and (ttype1 <> ttEof)) do
                    FTokenGen.Get(ttype1, token1);
                Exit;
            end else if (expr.ErrorCount <> 0) then
            begin
                Exit;
            end else if (Assigned(sym) and
                         ((sym.DeclType = dtReal) and (not expr.IsNumeric)) or
                         ((sym.DeclType = dtInteger) and ( not expr.IsNumeric))) then
            begin
                Error('Target variable type does not match expression type');
                Exit;
            end else if (Assigned(sym) and (sym.DeclType = dtLogical) and (expr.TargetDecl <> dtLogical)) then
            begin
                Error('Not a boolean expression');
                Exit;
            end;
            FCodeGen.Expression(expr);
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

        if (Assigned(asg)) then
            FCodeGen.Assignment(asg, expr, depth);
    finally
        expr.Free;
        asg.Free;
    end;
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

        FCodeGen.PgmEnd(ispgm, FLiterals, FTempVars);
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
    FTempVars := TSymbolTable.Create;
end;

function TCompiler.Declaration: Boolean;
var
    ttype: TTokenType;
    token: AnsiString;
    d: TDeclaration;
    sym: TSymbol;
    b: TBlock;
    done: Boolean;

    procedure ArraySubscripts; forward;

    function SubString(id: AnsiString; sym: TSymbol): Integer;
    var
        ttype: TTokenType;
        token: AnsiString;
        len: Integer;
        sub: TSubstringSymbol;
        sym1: TSymbol;
    begin
        Result := 0;
        if (b.Symbols.TryGetValue(id, stAny, sym1)) then
        begin
            Error(Format('%s is multipy defined', [id]));
            Exit;
        end;
        sub := TSubstringSymbol.Create;
        sub.ID := id;
        sub.SymbolType := stVariable;
        sub.DeclType := dtString;
        sub.IsStatic := sym.IsStatic;
        if (sym is TSubstringSymbol) then
        begin
            sub.Start := TSubstringSymbol(sym).Start +
                         TSubstringSymbol(sym).Length;
            sub.Container := TSubstringSymbol(sym).Container;
            sub.SymbolNum := TSubstringSymbol(sym).SymbolNum;
        end else
        begin
            sub.Start := sym.StringLength + 1;
            sub.Container := sym;
            sub.SymbolNum := sym.SymbolNum;
        end;
        b.Symbols.Add(id, sub);
        //
        FTokenGen.Get(ttype, token);
        if (ttype <> ttLeftParen) then
        begin
            Error(Format('Left bracket expected for substring, got %s', [token]));
            FTokenGen.Unget(ttype, token);
        end;
        while ((ttype <> ttRightParen) and (ttype <> ttLineSeparator) and (ttype <> ttEof)) do
        begin
            FTokenGen.Get(ttype, token);
            if (ttype = ttInteger) then
            begin
                if (not TryStrToInt(String(token), len)) then
                    len := 0;
                if ((len < 1) or (len > 32767)) then
                    Error('String length must be >= 1 and < 32K');
                Inc(sub.Length, len);
            end else if (ttype = ttIdentifier) then
            begin
                Inc(sub.Length, SubString(token, sub));
            end else if (ttype = ttComma) then
            begin
                ;
            end else
                Break;
        end;
        if (ttype <> ttRightParen) then
        begin
            FTokenGen.Unget(ttype, token);
            Error(Format('Expected right bracket to end substring, got %s', [token]));
        end {else
            FTokenGen.Get(ttype, token)};
        Result := sub.Length;
    end;

    procedure SubscriptsToSubstrings(sym: TSymbol);
    var
        i: Integer;
        sym1: TSymbol;
    begin
        for i := 0 to b.Symbols.Count - 1 do
        begin
            sym1 := b.Symbols.Symbols[i];
            if (sym1 is TSubstringSymbol) then
            begin
                if (TSubstringSymbol(sym1).Container = sym) then
                begin
                    sym1.IsArray := True;
                    sym1.ArraySubscripts := sym.ArraySubscripts;
                    sym1.ArraySize := sym.ArraySize;
                end;
            end;
        end;
    end;

    procedure Stringg(sym: TSymbol);
    var
        len: Integer;
    begin
        sym.StringLength := 0;
        FTokenGen.Get(ttype, token);
        if (ttype <> ttLeftParen) then
        begin
            FTokenGen.Unget(ttype, token);
            Error(Format('Left bracket expected following STRING declaration, got %s', [token]));
            Exit;
        end;
        while ((ttype <> ttRightParen) and (ttype <> ttLineSeparator) and (ttype <> ttEof)) do
        begin
            FTokenGen.Get(ttype, token);
            if (ttype = ttInteger) then
            begin
                if (not TryStrToInt(String(token), len)) then
                    len := 0;
                if ((len < 1) or (len > 32767)) then
                    Error('String length must be >= 1 and < 32K');
                Inc(sym.StringLength, len);
            end else if (ttype = ttIdentifier) then
            begin
                Inc(sym.StringLength, SubString(token, sym));
            end else if (ttype = ttComma) then
            begin
                ;
            end else if (ttype = ttColon) then
            begin
                sym.IsArray := True;
                ArraySubscripts;
                SubscriptsToSubstrings(sym);
                Inc(MinHeapSize, sym.ArraySize);
                FCodeGen.NewArray(sym);
            end else
                Break;
        end;
        if (ttype <> ttRightParen) then
        begin
            FTokenGen.Unget(ttype, token);
            Error(Format('Expected right bracket to end STRING, got %s', [token]));
        end;
        Inc(MinHeapSize, sym.StringLength + 1);
    end;

    function Variable(isArray: Boolean): TSymbol;
    begin
        Result := nil;
        if (b.Symbols.TryGetValue(token, stAny, sym)) then
        begin
            Error(Format('%s is multipy defined', [token]));
            Exit;
        end;
        sym := NewSymbol(token, stVariable, isArray);
        Result := sym;
        b.Symbols.Add(token, sym);
        sym.DeclType := d.DType;
        if (b.IsProc) then
            sym.IsStatic := False
        else
            sym.IsStatic := True;
        if (sym.DeclType = dtString) then
            Stringg(sym);
        if (sym.IsStatic) then
            FCodeGen.StaticVar(sym)
        else
            FCodeGen.StackVar(sym);
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
                sym := NewSymbol(token, st, False);
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
        swtch: TSwitchSymbol;
        expr: TExpression;
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
        sym := NewSymbol(token, stSwitch, False);
        b.Symbols.Add(token, sym);
        swtch := (TSwitchSymbol(sym));

        FTokenGen.Get(ttype, token);
        if (ttype <> ttAssignment) then
        begin
            Error(Format('Equal sign expected got %s', [token]));
            FTokenGen.FlushLine;
            Exit;
        end;

        done := False;
        while (not done) do
        begin
            Inc(FSwitchNum);
            if (swtch.FirstSwitchNum = 0) then
                swtch.FirstSwitchNum := FSwitchNum;
            expr := DesignationalExpr;
            if (Assigned(expr)) then
                swtch.Targets.Add(expr)
            else
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
        FCodeGen.Switch(swtch);
    end;

    procedure ArraySubscripts;
    var
        expr: TExpression;
        i, len, i1, i2: Integer;
        allLiterals: Boolean;
    begin
        allLiterals := True;
        sym.ArraySubscripts := 0;
        Inc(FSymbolNum);
        sym.NewArrayLabelNum := FSymbolNum;
        repeat
            expr := Expression;
            if ((not Assigned(expr)) or (expr.TargetDecl <> dtInteger)) then
                Error('Subscript expressions must be of type integer');
            sym.Indices.Add(expr);
            FTokenGen.Get(ttype, token);
            if (ttype = ttColon) then
            begin
                expr := Expression;
                if ((not Assigned(expr)) or (expr.TargetDecl <> dtInteger)) then
                    Error('Subscript expressions must be of type integer');
                if (Assigned(expr) and (not (TExpressionTerm(expr) is TExprLiteral))) then
                    allLiterals := False;
                sym.Indices.Add(expr);
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
        // Calculate total length of static arrays
        sym.ArraySize := 0;
        if (allLiterals) then
        begin
            case sym.DeclType of
              dtInteger,
              dtLogical:
                len := 1;
              dtReal:
                len := 2;
              dtString:
                len := ((sym.StringLength - 1) div 5) + 2;
              else
                len := 1;
            end;
            for i := sym.Indices.Count - 1 downto 0 do
            begin
                if ((i mod 2) = 0) then
                begin
                    if (not TryStrToInt(String(TExprLiteral(sym.Indices[i]).Literal.Value), i1)) then
                        i1 := 0;
                    len := ((i2 - i1) + 1) * len
                end else
                begin
                    if (not TryStrToInt(String(TExprLiteral(sym.Indices[i]).Literal.Value), i2)) then
                        i2 := 0;
                end;
            end;
            sym.ArraySize := len + sym.Indices.Count + 1;
        end;
    end;

    procedure SubscriptsToPrior(syms: TList<TSymbol>);
    // Make all preceeding variables reference the same
    // array defintion.
    var
        sym1: TSymbol;
    begin
        for sym1 in syms do
        begin
            if (sym1 <> sym) then
            begin
                sym1.ArraySubscripts := sym.ArraySubscripts;
                sym1.NewArrayLabelNum := sym.NewArrayLabelNum;
                sym1.ArraySize := sym.ArraySize;
                Inc(MinHeapSize, sym.ArraySize);
            end;
        end;
        syms.Clear;
    end;

    procedure Arrayy;
    var
        syms: TList<TSymbol>;
    begin
        syms := TList<TSymbol>.Create;
        try
            FTokenGen.Get(ttype, token);
            while (ttype = ttIdentifier) do
            begin
                sym := Variable(True);
                syms.Add(sym);
                if ((sym.DeclType = dtString) and (sym.ArraySubscripts <> 0)) then
                    SubscriptsToPrior(syms);
                FTokenGen.Get(ttype, token);
                if (ttype = ttComma) then
                begin
                    FTokenGen.Get(ttype, token);
                    Continue;
                end else if (sym.DeclType = dtString) then
                begin
                    Continue;
                end else if (ttype <> ttLeftParen) then
                begin
                    FTokenGen.Unget(ttype, token);
                    Error('An array declaration must specify some subscripts');
                    Break;
                end;
                try
                    ArraySubscripts;
                    Inc(MinHeapSize, sym.ArraySize);
                    FCodeGen.NewArray(sym);
                except
                  on E: Exception do
                  begin
                    Error(E.Message);
                    FTokenGen.FlushLine;
                  end;
                end;
                SubscriptsToPrior(syms);
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
    if (ProcedureDecl) then
    begin
        Result := True;
        Exit;
    end;

    FTokenGen.Get(ttype, token);
    if (ttype = ttComment) then
    begin
        FTokenGen.FlushLine;
        Result := True;
        Exit;
    end else if (token = 'LOCAL') then
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
            Variable(False);
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

function TCompiler.DesignationalExpr: TExpression;
var
    ttype: TTokenType;
    token: AnsiString;
    sym: TSymbol;
    elbl: TExprLabel;
    eswtch: TExprSwitch;
    iff: TExprIf;
begin
    Result := nil;
    FTokenGen.Get(ttype, token);
    case ttype of
      ttIdentifier:
      begin
        sym := FindSymbol(token, stLabel);
        if (Assigned(sym)) then
        begin
            elbl := TExprLabel.Create;
            elbl.Symbol := sym;
            Result := TExpression(elbl);
        end else
        begin
            sym := FindSymbol(token, stSwitch);
            if (Assigned(sym)) then
            begin
                eswtch := TExprSwitch.Create;
                eswtch.Symbol := sym;
                FTokenGen.Get(ttype, token);
                if (ttype <> ttLeftParen) then
                    Error(Format('Expected left bracket, got %s', [token]));
                eswtch.Index := Expression;
                if (eswtch.Index.TargetDecl <> dtInteger) then
                    Error(Format('Integer expression expected for SWITCH %s', [eswtch.Symbol.ID]));
                FTokenGen.Get(ttype, token);
                if (ttype <> ttRightParen) then
                    Error(Format('Expected right bracket, got %s', [token]));
                Result := TExpression(eswtch);
            end else
            begin
                Error(Format('%s is not a LABEL or a SWITCH', [token]));
            end;
        end;
      end;

      ttIf:
      begin
        iff := TExprIf.Create;
        iff.IfExpr := Expression;
        if (iff.IfExpr.TargetDecl = dtLogical) then
        begin
            FTokenGen.Get(ttype, token);
            if (token <> 'THEN') then
            begin
                Error(Format('THEN expected, got %s', [token]));
                FTokenGen.Unget(ttype, token);
            end;
            iff.ElseLblNum := FSymbolNum + 1;
            iff.EndLblNum := FSymbolNum + 2;
            Inc(FSymbolNum, 2);
            iff.ThenExpr := DesignationalExpr;
            if (not Assigned(iff.ThenExpr))  then
                Error('Designational expression expected following THEN');
            if (Assigned(iff.ThenExpr) and (iff.ThenExpr is TExprIf)) then
                Error('IF not allowed following THEN');
            FTokenGen.Get(ttype, token);
            if (token <> 'ELSE') then
            begin
                Error(Format('ELSE expected, got %s', [token]));
                FTokenGen.Unget(ttype, token);
            end;
            iff.ElseExpr := DesignationalExpr;
            if (not Assigned(iff.ElseExpr)) then
                Error('Designational expression expected following ELSE');
            Result := TExpression(iff);
        end else
        begin
            Error('Relational expression expected following IF');
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

function TCompiler.Expression: TExpression;
const
    oprs: array [TExprOperatorType] of AnsiString = (
        '**', '-', '*', '/', '//', '+', '-', 'LSS', 'LEQ',
        'GTR', 'GEQ', 'EQL', 'NEQ', 'NOT', 'AND', 'OR',
        'XOR', 'IMPL', 'EQUIV', 'IF', 'THEN', 'ELSE',
        'GET_ARRAY', 'PUSH', 'SIGN', 'UNK'
    );

    function Factor(priorityLevel, condDepth: Integer; var errCount: Integer): TExpressionTerm; forward;

    function OperatorType(opr: AnsiString): TExprOperatorType;
    var
        i: TExprOperatorType;
    begin
        for i := Low(oprs) to High(oprs) do
        begin
            if (opr = oprs[i]) then
            begin
                Result := i;
                Exit;
            end;
        end;
        Result := otUnknown;
    end;

    function PriorityMatch(opr: AnsiString; priorityLevel: Integer): Boolean;
    var
        s: AnsiString;
    begin
        Result := False;
        for s in ExprOprPriorities[priorityLevel] do
        begin
            if (s = opr) then
            begin
                Result := True;
                Break;
            end;
        end;
    end;

    function Arrayy(var errCount: Integer; condDepth: Integer; sym: TSymbol): TExpressionTerm;
    var
        ttype: TTokenType;
        token: AnsiString;
        expr: TExpressionTerm;
        arr: TExprArray;
        opr: TExprOperator;
    // process array items by treating the array bracket as a high priority operator
    begin
        Result := nil;
        FTokenGen.Get(ttype, token);
        if (ttype <> ttLeftParen) then
        begin
            FTokenGen.Unget(ttype, token);
            Error(Format('Expected left bracket following array identifier, got %s', [token]));
            Inc(errCount);
            Exit;
        end;
        arr := TExprArray.Create;
        arr.Symbol := sym;
        repeat
            expr := Factor(0, condDepth, errCount);
            if ((not Assigned(expr)) or (expr.TargetDecl <> dtInteger)) then
            begin
                Error('Integer expression expected for array subscript');
                Inc(errCount);
            end;
            arr.Subscripts.Add(expr);
            FTokenGen.Get(ttype, token);
        until ttype <> ttComma;
        if (ttype <> ttRightParen) then
        begin
            FTokenGen.Unget(ttype, token);
            Error(Format('Expected right bracket, got %s', [token]));
        end;
        if (arr.Subscripts.Count <> sym.ArraySubscripts) then
        begin
            Error('Incorrect number of array subscripts');
            Inc(errCount);
        end;
        opr := TExprOperator.Create;
        opr.OType := otGetArray;
        opr.Right := arr;
        opr.TargetDecl := sym.DeclType;
        Result := opr;
    end;

    function Stringg(var errCount: Integer; condDepth: Integer; sym: TSymbol): TExpressionTerm;
    // Check to see if substring specified and return the start and end values of
    // the substring.
    var
        ttype: TTokenType;
        token: AnsiString;
        expr: TExpressionTerm;
        opr: TExprOperator;
        elit: TExprLiteral;
        lit: TLiteral;
    begin
        if (sym.IsArray) then
            Result := TExprArray.Create
        else
            Result := TExprVariable.Create;
        TExprVariable(Result).Symbol := sym;
        TExprVariable(Result).TargetDecl := sym.DeclType;
        // Check for left paren. If not present, we are done.
        FTokenGen.Get(ttype, token);
        if (ttype <> ttLeftParen) then
        begin
            if (sym.IsArray) then
            begin
                Error(Format('Left bracket expected for array reference, got %s', [token]));
                Inc(errCount);
            end;
            FTokenGen.Unget(ttype, token);
            Exit;
        end;
        // This gets complicated. Values given inside [] may be either
        // substring start / length or array subscripts or both.
        //
        // First gather all values until we see something that is not a
        // comma. If variable is not an array then values are substring
        // start / length and we are done.
        //
        // If variable is an array and next token is not a colon then
        // values are array subscripts and we are done.
        //
        // If next value is a token, copy assumed array subscripts to
        // substring start / length and continue parsing to find array
        // subscripts.
        repeat
            expr := Factor(0, condDepth, errCount);
            if ((not Assigned(expr)) or (expr.TargetDecl <> dtInteger)) then
            begin
                Error('Integer expression expected for substring / array subscript');
                Inc(errCount);
            end;
            if (sym.IsArray) then
                TExprArray(Result).Subscripts.Add(expr)
            else if (not Assigned(TExprVariable(Result).StringStart)) then
                TExprVariable(Result).StringStart := expr
            else if (not Assigned(TExprVariable(Result).StringLength)) then
                TExprVariable(Result).StringLength := expr
            else
            begin
                Error('Too many values given for substring');
                Inc(errCount);
            end;
            FTokenGen.Get(ttype, token);
        until ttype <> ttComma;
        //
        if (ttype = ttColon) then
        begin
            if (sym.IsArray) then
            begin
                if (TExprArray(Result).Subscripts.Count > 2) then
                begin
                    Error('Too many values given for substring');
                    Inc(errCount);
                end else
                begin
                    with TExprArray(Result) do
                    begin
                        StringStart := Subscripts[0];
                        Subscripts.Delete(0);
                        if (Subscripts.Count > 0) then
                        begin
                            StringLength := Subscripts[1];
                            Subscripts.Delete(0);
                        end;
                    end;
                end;
                repeat
                    expr := Factor(0, condDepth, errCount);
                    if ((not Assigned(expr)) or (expr.TargetDecl <> dtInteger)) then
                    begin
                        Error('Integer expression expected for array subscript');
                        Inc(errCount);
                    end;
                    TExprArray(Result).Subscripts.Add(expr);
                    FTokenGen.Get(ttype, token);
                until ttype <> ttComma;
            end else
            begin
                Error('Colon only allowed for string arrays');
                Inc(errCount);
            end;
        end;
        //
        if (ttype <> ttRightParen) then
        begin
            Error(Format('Right bracket expected, got %s', [token]));
            Inc(errCount);
            FTokenGen.Unget(ttype, token);
        end;
        with TExprVariable(Result) do
        begin
            if (Assigned(StringStart) and (not Assigned(StringLength))) then
            begin
                elit := TExprLiteral.Create;
                lit := TLiteral.Create;
                lit.Value := '1';
                lit.DeclType := dtInteger;
                elit.Literal := lit;
                StringLength := elit;
            end;
        end;
        if (sym.IsArray) then
        begin
            if (TExprArray(Result).Subscripts.Count <> sym.ArraySubscripts) then
            begin
                Error('Incorrect number of array subscripts');
                Inc(errCount);
            end;
            opr := TExprOperator.Create;
            opr.OType := otGetArray;
            opr.Right := Result;
            opr.TargetDecl := sym.DeclType;
            Result := opr;
        end;
    end;

    function Operand(var errCount: Integer; condDepth: Integer): TExpressionTerm;
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
        ifOpr: TExprIf;
    begin
        Result := nil;
        try
            FTokenGen.Get(ttype, token);
            case ttype of
              ttIf:
              begin
                ifOpr := TExprIf.Create;
                ifOpr.IfExpr := Factor(0, condDepth, errCount);
                if ((not Assigned(ifOpr.IfExpr)) or (ifOpr.IfExpr.DeclType <> dtLogical)) then
                begin
                    Error('Boolean expression expected following IF');
                    Inc(errCount);
                end;
                Inc(FSymbolNum);
                ifOpr.ElseLblNum := FSymbolNum;
                // If we are parsing the outermost IF then we need to set the label number
                // of the label marking the end of the entire IF chain.
                if (condDepth = 0) then
                begin
                    Inc(FSymbolNum);
                    FConditionalEndLblNum := FSymbolNum;
                end;
                ifOpr.EndLblNum := FConditionalEndLblNum;
                ifOpr.ConditionalDepth := condDepth;
                Inc(condDepth);
                FTokenGen.Get(ttype, token);
                if (token <> 'THEN') then
                begin
                    Error(Format('THEN expected, got %s', [token]));
                    Inc(errCount);
                    FTokenGen.Unget(ttype, token);
                end;
                ifOpr.ThenExpr := Factor(0, condDepth, errCount);
                if (not Assigned(ifOpr.ThenExpr)) then
                    raise Exception.Create('Expression expected following THEN');
                if (Assigned(ifOpr.ThenExpr) and (ifOpr.ThenExpr is TExprIf)) then
                begin
                    Error('IF not allowed following THEN');
                    Inc(errCount);
                end;
                FTokenGen.Get(ttype, token);
                if (token <> 'ELSE') then
                begin
                    Error(Format('ELSE expected, got %s', [token]));
                    Inc(errCount);
                    FTokenGen.Unget(ttype, token);
                end;
                IfOpr.ElseExpr := Factor(0, condDepth, errCount);
                if (not Assigned(ifOpr.ElseExpr)) then
                    raise Exception.Create('Expression expected following ELSE');
                Result := ifOpr;
                Exit;
              end;
              ttIdentifier:
              begin
                sym := FindSymbol(token, stVariable);
                if (not Assigned(sym)) then
                begin
                    Error(Format('%s is undefined', [token]));
                    sym := NewSymbol(token, stVariable, False);
                    sym.DeclType := dtInteger;
                    Inc(errCount);
                end;
                if (sym.DeclType = dtString) then
                    Result := Stringg(errCount, condDepth, sym)
                else if (sym.IsArray) then
                    Result := Arrayy(errCount, condDepth, sym)
                else
                begin
                    evar := TExprVariable.Create;
                    evar.Symbol := sym;
                    evar.TargetDecl := sym.DeclType;
                    Result := evar;
                end;
                Exit;
              end;
              ttInteger:
              begin
                if (not TryStrToInt(String(token), itemp)) then
                begin
                    Error(Format('%s is not a valid integer', [token]));
                    Inc(errCount);
                end;
                i := FLiterals.Add(dtInteger, token);
                elit := TExprLiteral.Create;
                elit.Literal := FLiterals[i];
                elit.TargetDecl := dtInteger;
                Result := elit;
                Exit;
              end;
              ttReal:
              begin
                if (not TryStrToFloat(String(token), ftemp)) then
                begin
                    Error(Format('%s is not a valid real number', [token]));
                    Inc(errCount);
                end;
                if (token[1] = '.') then
                    token := '0' + token;
                i := FLiterals.Add(dtReal, token);
                elit := TExprLiteral.Create;
                elit.Literal := FLiterals[i];
                elit.TargetDecl := dtReal;
                Result := elit;
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
                Result := Expression;
                Inc(errCount, Result.ErrorCount);
                FTokenGen.Get(ttype, token);
                if (ttype <> ttRightParen) then
                    raise Exception.Create('Unbalanced parenthese');
              end;
              ttLogicalValue:
              begin
                if (token = 'TRUE') then
                    i := FLiterals.Add(dtLogical, '1')
                else
                    i := FLiterals.Add(dtLogical, '0');
                elit := TExprLiteral.Create;
                elit.Literal := FLiterals[i];
                elit.TargetDecl := dtLogical;
                Result := elit;
                Exit;
              end;
              ttString:
              begin
                i := FLiterals.Add(dtString, token);
                elit := TExprLiteral.Create;
                elit.Literal := FLiterals[i];
                elit.TargetDecl := dtString;
                Result := elit;
                Exit;
              end;
              else
              begin
                FTokenGen.Unget(ttype, token);
                raise Exception.Create('Expression Syntax error');
              end;
            end;
        except
          on E: Exception do
          begin
            Error(E.Message);
            Inc(errCount);
            FreeAndNil(Result);
          end;
        end;
    end;

    procedure CheckOperands(opr: TExprOperator; var errCount: Integer);
    // Validate operand types against the operator type.
    begin
        if ((not Assigned(opr.Left)) and (not Assigned(opr.Right)))  then
            Exit;

        if (Assigned(opr.Left)) then
        begin
            opr.TargetDecl := opr.Left.DeclType;
            opr.Left.TargetDecl := opr.Left.DeclType;
            opr.Right.TargetDecl := opr.Right.DeclType;
            if ((opr.Right.DeclType = dtInteger) and (opr.Left.DeclType = dtReal)) then
            begin
                opr.Right.TargetDecl := dtReal;
                opr.TargetDecl := dtReal;
            end;
            if ((opr.Left.DeclType = dtInteger) and (opr.Right.DeclType = dtReal)) then
            begin
                opr.Left.TargetDecl := dtReal;
                opr.TargetDecl := dtReal;
            end;
            if (opr.IsLogical or opr.IsRelational) then
                opr.TargetDecl := dtLogical;
            if ((opr.OType = otExponent) or (opr.OType = otDivide)) then
            begin
                opr.Left.TargetDecl := dtReal;
                opr.Right.TargetDecl := dtReal;
                opr.TargetDecl := dtReal;
            end else if (opr.OType = otIntDivide) then
            begin
                if ((opr.Left.DeclType <> dtInteger) or (opr.Right.DeclType <> dtInteger)) then
                begin
                    Error('Both operands of integer divide must be integer');
                    Inc(errCount);
                end;
            end;
            if (opr.IsArithmetic and (not (opr.Left.TargetIsNumeric and opr.Right.TargetIsNumeric))) then
            begin
                Error('Non-numeric operand specified for arithmetic operator');
                Inc(errCount);
            end;
            if (opr.IsRelational and
                (not ((opr.Left.TargetIsNumeric and opr.Right.TargetIsNumeric) or
                      (opr.Left.TargetIsString and opr.Right.TargetIsString)))) then
            begin
                Error('Non-numeric or non-string operand(s) specified for relational operator');
                Inc(errCount);
            end;
            if (opr.IsLogical and (not (opr.Left.TargetIsLogical and opr.Right.TargetIsLogical))) then
            begin
                Error('Non-logical operand specified for logical operator');
                Inc(errCount);
            end;
        end else
        begin
            opr.Right.TargetDecl := opr.Right.DeclType;
            if (opr.IsArithmetic and (not opr.Right.TargetIsNumeric)) then
            begin
                Error('Non-numeric operand specified for unary minus');
                Inc(errCount);
            end;
            if (opr.IsLogical and (not opr.Right.TargetIsLogical)) then
            begin
                Error('Non-logical operand specified for NOT');
                Inc(errCount);
            end;
        end;
    end;

    function Reduce(opr: TExprOperator): TExpressionTerm;
    // Reduce expressions consisting of only literals to a single literal.
    var
        lint, rint, irslt, i: Integer;
        lflt, rflt, rrslt: Double;
        tgt: TDeclType;
        lit: TExprLiteral;
        iff: TExprIf;
        stemp: AnsiString;
    begin
        Result := opr;
        irslt := 0;
        rrslt := 0;
        if (opr is TExprIf) then
        begin
            // Special processing for IF expressions
            iff := TExprIf(opr);
            if ((iff.ThenExpr.TargetDecl = dtReal) and (iff.ElseExpr.TargetDecl = dtInteger)) then
                iff.ElseExpr.TargetDecl := dtReal;
            if ((iff.ThenExpr.TargetDecl = dtInteger) and (iff.ElseExpr.TargetDecl = dtReal)) then
                iff.ThenExpr.TargetDecl := dtReal;
            iff.TargetDecl := iff.ThenExpr.TargetDecl;
            Exit;
        end;

        if ((not Assigned(opr.Left)) and (not Assigned(opr.Right))) then
            Exit;
        if (Assigned(opr.Left)) then
        begin
            if (opr.Left is TExprLiteral) then
            begin
                if ((opr.Left.TargetDecl = dtReal) and (opr.Left.DeclType = dtInteger)) then
                begin
                    stemp := TExprLiteral(opr.Left).Literal.Value + '.0';
                    if (stemp[1] = '.') then
                        stemp := '0' + stemp;
                    i := FLiterals.Add(dtReal, stemp);
                    Dec(TExprLiteral(opr.Left).Literal.RefCount);
                    TExprLiteral(opr.Left).Literal := FLiterals[i];
                end;
            end;
            if (opr.Right is TExprLiteral) then
            begin
                if ((opr.Right.TargetDecl = dtReal) and (opr.Right.DeclType = dtInteger)) then
                begin
                    stemp := TExprLiteral(opr.Right).Literal.Value + '.0';
                    if (stemp[1] = '.') then
                        stemp := '0' + stemp;
                    i := FLiterals.Add(dtReal, stemp);
                    Dec(TExprLiteral(opr.Right).Literal.RefCount);
                    TExprLiteral(opr.Right).Literal := FLiterals[i];
                end;
            end;
            if ((not (opr.Left is TExprLiteral)) or (not (opr.Right is TExprLiteral))) then
                Exit;
            if ((opr.Left.DeclType = dtReal) or (opr.Right.DeclType = dtReal)) then
            begin
                tgt := dtReal;
                if (not TryStrToFloat(String(TExprLiteral(opr.Left).Literal.Value), lflt)) then
                    lflt := 0.0;
                if (not TryStrToFloat(String(TExprLiteral(opr.Right).Literal.Value), rflt)) then
                    rflt := 0.0;
            end else
            begin
                tgt := dtInteger;
                if (not TryStrToInt(String(TExprLiteral(opr.Left).Literal.Value), lint)) then
                    lint := 0;
                if (not TryStrToInt(String(TExprLiteral(opr.Right).Literal.Value), rint)) then
                    rint := 0;
            end;
        end else
        begin
            if (not (opr.Right is TExprLiteral)) then
                Exit;
            if (opr.Right.DeclType = dtReal) then
            begin
                tgt := dtReal;
                if (not TryStrToFloat(String(TExprLiteral(opr.Right).Literal.Value), rflt)) then
                    rflt := 0.0;
            end else
            begin
                tgt := dtInteger;
                if (not TryStrToInt(String(TExprLiteral(opr.Right).Literal.Value), rint)) then
                    rint := 0;
            end;
        end;
        case opr.OType of
          otExponent:
          begin
            if (tgt = dtReal) then
                rrslt := Power(lflt, rflt)
            else
                rrslt := Power(lint, rint);
            tgt := dtReal;
          end;
          otUnaryMinus:
          begin
            if (tgt = dtReal) then
                rrslt := -rflt
            else
                irslt := -rint;
          end;
          otMultiply:
          begin
            if (tgt = dtReal) then
                rrslt := lflt * rflt
            else
                irslt := lint * rint;
          end;
          otDivide:
          begin
            if (tgt = dtReal) then
                rrslt := lflt / rflt
            else
                rrslt := lint / rint;
            tgt := dtReal;
          end;
          otIntDivide:
          begin
            if (tgt = dtReal) then
                irslt := 0
            else
                irslt := lint div rint;
          end;
          otPlus:
          begin
            if (tgt = dtReal) then
                rrslt := lflt + rflt
            else
                irslt := lint + rint;
          end;
          otMinus:
          begin
            if (tgt = dtReal) then
                rrslt := lflt - rflt
            else
                irslt := lint - rint;
          end;
          otLess:
          begin
            if (tgt = dtReal) then
                irslt := Integer(lflt < rflt)
            else
                irslt := Integer(lint < rint);
            tgt := dtLogical;
          end;
          otLessEqual:
          begin
            if (tgt = dtReal) then
                irslt := Integer(lflt <= rflt)
            else
                irslt := Integer(lint <= rint);
            tgt := dtLogical;
          end;
          otGreater:
          begin
            if (tgt = dtReal) then
                irslt := Integer(lflt > rflt )
            else
                irslt := Integer(lint > rint);
            tgt := dtLogical;
          end;
          otGreaterEqual:
          begin
            if (tgt = dtReal) then
                irslt := Integer(lflt >= rflt)
            else
                irslt := Integer(lint >= rint);
            tgt := dtLogical;
          end;
          otEqual:
          begin
            if (tgt = dtReal) then
                irslt := Integer(lflt = rflt )
            else
                irslt := Integer(lint = rint);
            tgt := dtLogical;
          end;
          otNotEqual:
          begin
            if (tgt = dtReal) then
                irslt := Integer(lflt <> rflt)
            else
                irslt := Integer(lint <> rint);
            tgt := dtLogical;
          end;
          otNot:
          begin
            if (tgt = dtReal) then
                irslt := 0
            else
            begin
                if (rint = 0) then
                    rint := 1
                else
                    rint := 0;
            end;
            tgt := dtLogical;
          end;
          otAnd:
          begin
            if (tgt = dtReal) then
                irslt := 0
            else
            begin
                if ((lint <> 0) and (rint <> 0)) then
                    irslt := 1
                else
                    irslt := 0;
            end;
            tgt := dtLogical;
          end;
          otOr:
          begin
            if (tgt = dtReal) then
                irslt := 0
            else
            begin
                if ((lint <> 0) or (rint <> 0)) then
                    irslt := 1
                else
                    irslt := 0;
            end;
            tgt := dtLogical;
          end;
          otXor:
          begin
            if (tgt = dtReal) then
                irslt := 0
            else
            begin
                if (((lint <> 0) and (rint <> 0)) or ((lint = 0) and (rint = 0))) then
                    irslt := 0
                else
                    irslt := 1;
            end;
            tgt := dtLogical;
          end;
          otImpl:
          begin
            if (tgt = dtReal) then
                irslt := 0
            else
            begin
                if ((lint <> 0) and (rint = 0)) then
                    irslt := 0
                else
                    irslt := 1;
            end;
            tgt := dtLogical;
          end;
          otEquiv:
          begin
            if (tgt = dtReal) then
                irslt := 0
            else
            begin
                if (((lint <> 0) and (rint <> 0)) or ((lint = 0) and (rint = 0))) then
                    irslt := 1
                else
                    irslt := 0;
            end;
            tgt := dtLogical;
          end;
        end;

        if (tgt = dtReal) then
        begin
            stemp := AnsiString(FloatToStr(rrslt));
            i := FLiterals.Add(dtReal, stemp);
            lit := TExprLiteral.Create;
            lit.TargetDecl := dtReal;
            lit.Literal := FLiterals[i];
        end else if (tgt = dtInteger) then
        begin
            stemp := AnsiString(IntToStr(irslt));
            i := FLiterals.Add(dtInteger, stemp);
            lit := TExprLiteral.Create;
            lit.TargetDecl := dtInteger;
            lit.Literal := FLiterals[i];
        end else if (tgt = dtLogical) then
        begin
            if (irslt = 0) then
                stemp := '0'
            else
                stemp := '1';
            i := FLiterals.Add(dtLogical, stemp);
            lit := TExprLiteral.Create;
            lit.TargetDecl := dtLogical;
            lit.Literal := FLiterals[i];
        end else
        begin
            lit := nil;
        end;
        if (Assigned(opr.Left)) then
            Dec(TExprLiteral(opr.Left).Literal.RefCount);
        Dec(TExprLiteral(opr.Right).Literal.RefCount);
        opr.Free;
        Result := lit;
    end;

    function Factor(priorityLevel, condDepth: Integer; var errCount: Integer): TExpressionTerm;
    var
        ttype: TTokenType;
        token: AnsiString;
        done: Boolean;
        opr: TExprOperator;
        left, right: TExpressionTerm;
        ot: TExprOperatorType;
    begin
        if (FTokenTrace) then
            WriteLn(Format('Factor (priority %s)', [priorityLevel]));

        Result := nil;
        left := nil;
        right := nil;
        opr := nil;
        try
            FTokenGen.Get(ttype, token);
            if ((token = '-') or (token = 'NOT')) then
            begin
                if (PriorityMatch(token, priorityLevel)) then
                begin
                    right := Factor(priorityLevel + 1, condDepth, errCount);
                    if (not Assigned(right)) then
                        Exit;
                    opr := TExprOperator.Create;
                    opr.Right := right;
                    opr.TargetDecl := right.DeclType;
                    if (token = '-') then
                    begin
                        opr.OType := otUnaryMinus;
                        if (not right.IsNumeric) then
                        begin
                            Error('Integer or real value expected for unary minus');
                            Inc(errCount);
                        end;
                        CheckOperands(opr, errCount);
                        Result := Reduce(opr);
                    end else
                    begin
                        opr.OType := otNot;
                        if (not right.IsLogical) then
                        begin
                            Error('Boolean value expected for NOT');
                            Inc(errCount);
                        end;
                        CheckOperands(opr, errCount);
                        Result := Reduce(opr);
                    end;
                    Exit;
                end else
                begin
                    FTokenGen.Unget(ttype, token);
                end;
            end else
            begin
                FTokenGen.Unget(ttype, token);
            end;

            if (priorityLevel >= High(ExprOprPriorities)) then
                left := Operand(errCount, condDepth)
            else
                left := Factor(priorityLevel + 1, condDepth, errCount);
            if (not Assigned(left)) then
                Exit;

            done := False;
            while (not done) do
            begin
                FTokenGen.Get(ttype, token);
                ot := OperatorType(token);
                if (ot <> otUnknown) then
                begin
                    if (PriorityMatch(token, priorityLevel)) then
                    begin
                        if (priorityLevel >= High(ExprOprPriorities)) then
                            right := Operand(errCount, condDepth)
                        else
                            right := Factor(priorityLevel + 1, condDepth, errCount);
                        if (not Assigned(right)) then
                        begin
                            FreeAndNil(left);
                            Exit;
                        end;
                        opr := TExprOperator.Create;
                        opr.OType := ot;
                        opr.Left := left;
                        opr.Right := right;
                        CheckOperands(opr, errCount);
                        left := Reduce(opr);
                    end else
                    begin
                        FTokenGen.Unget(ttype, token);
                        done := True;
                    end;
                end else
                begin
                    FTokenGen.Unget(ttype, token);
                    done := True;
                end;
            end;
            if ((priorityLevel = 0) and (left is TExprIf)) then
                Reduce(TExprIf(left));
            Result := left;
        except
          on E: Exception do
          begin
            Error(E.Message);
            FreeAndNil(left);
            FreeAndNil(right);
            FreeAndNil(opr);
            Result := nil;
          end;
        end;
    end;

    function PostFix(term: TExpressionTerm): AnsiString;
    // Traverse the expression tree and generate the eqivalent postfix
    // expression. Used for debugging purposes only.
    begin
        if (not Assigned(term)) then
        begin
            Result := '';
        end else if (term is TExprLiteral) then
        begin
            Result := TExprLiteral(term).Literal.Value + ' ';
        end else if (term is TExprVariable) then
        begin
            Result := TExprVariable(term).Symbol.ID + ' ';
        end else if (term is TExprIf) then
        begin
            Result := PostFix(TExprIf(term).IfExpr) +
                      oprs[otIf] + ' ' +
                      PostFix(TExprIf(term).ThenExpr) +
                      oprs[otThen] + ' ' +
                      PostFix(TExprIf(term).ElseExpr);
            if (TExprIf(term).ConditionalDepth = 0) then
                Result := Result + oprs[otElse] + ' ';
        end else if (term is TExprOperator) then
        begin
            Result := PostFix(TExprOperator(term).Left) +
                      PostFix(TExprOperator(term).Right) +
                      oprs[TExprOperator(term).OType] + ' ';
        end;
    end;

var
    post: AnsiString;
    ttype: TTokenType;
    token: AnsiString;
    errCount: Integer;
begin
    Result := nil;
    FTokenGen.Get(ttype, token);
    if (not ((ttype = ttIdentifier) or (ttype = ttInteger) or (ttype = ttReal) or
             (ttype = ttLogicalValue) or (ttype = ttString) or // (ttype = ttLabel) or
             (ttype = ttIf) or (ttype = ttLeftParen) or (token = '-') or
             (token = 'NOT'))) then
        Exit;

    FTokenGen.Unget(ttype, token);
    try
        errCount := 0;
        Result := TExpression(Factor(0, 0, errCount));
        if (Assigned(Result)) then
        begin
            Result.ErrorCount := errCount;
            // **********************
            // for debugging
            post := PostFix(Result);
            // **********************
        end;
    except
      on E: Exception do
      begin
        Error(E.Message);
        FreeAndNil(Result);
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
    expr: TExpression;
    stmt: TForStatement;
    asgType: TDeclType;
    mult, minus, gt, sign: TExprOperator;
    lv: TExprVariable;
    zero: TExprLiteral;
    i: Integer;
begin
    asgType := dtUnknown;
    FTokenGen.Get(ttype, token);
    if (ttype <> ttFor) then
    begin
        FTokenGen.Unget(ttype, token);
        Result := False;
        Exit;
    end;

    Result := True;
    stmt := TForStatement.Create;
    FTokenGen.Get(ttype, token);
    if (ttype <> ttIdentifier) then
    begin
        Error(Format('Identifier expected, got %s', [token]));
        FTokenGen.Unget(ttype, token);
    end;
    stmt.LoopVar := FindSymbol(token, stVariable);
    if (not Assigned(stmt.LoopVar)) then
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
    Inc(FSymbolNum);
    stmt.LoopLblNum := FSymbolNum;
    Inc(FSymbolNum);
    stmt.EndLblNum := FSymbolNum;
    expr := Expression;
    if (Assigned(expr)) then
    begin
        stmt.AsgExpressions.Add(expr);
        asgType := expr.TargetDecl;
    end else
        Error(Format('Expression expected after assignment', [token]));
    FTokenGen.Get(ttype, token);
    if (ttype = ttComma) then
    begin
        repeat
            expr := Expression;
            if (Assigned(expr)) then
            begin
                if (expr.TargetDecl <> stmt.LoopVar.DeclType) then
                    Error('All expression must be same type as loop variable');
                stmt.AsgExpressions.Add(expr);
            end else
                Error('Expression expected after comma');
            FTokenGen.Get(ttype, token);
        until ttype <> ttComma;
        FTokenGen.Unget(ttype, token);
    end else
        FTokenGen.Unget(ttype, token);
    if (stmt.LoopVar.DeclType <> asgType) then
        Error('Loop variable type and expression type must match');
    // DO, STEP or WHILE
    FTokenGen.Get(ttype, token);
    if (token = 'DO') then
    begin
        FCodeGen.ForListInit(stmt);
        if (not Statement) then
            Error('Statement expected following DO');
        FCodeGen.ForListEnd(stmt);
    end else
    begin
        if (token = 'STEP') then
        begin
            // STEP
            stmt.IncrExpression := Expression;
            if (Assigned(stmt.IncrExpression)) then
            begin
                if (stmt.IncrExpression.TargetDecl <> asgType) then
                    Error('Assignment and STEP expressions must the same type');
            end else
                Error('Expression expected following STEP');
            // UNTIL
            FTokenGen.Get(ttype, token);
            if (token <> 'UNTIL') then
            begin
                Error(Format('UNTIL expected, got %s', [token]));
                FTokenGen.Unget(ttype, token);
            end;
            expr := Expression;
            if (Assigned(expr)) then
            begin
                if (expr.TargetDecl <> asgType) then
                    Error('Assignment and UNTIL expressions must be the same type');
            end else
                Error('Expression expected following UNTIL');
            // Generate test expression (SIGN(step) * (LoopVar - TestExpression)) > 0
            lv := TExprVariable.Create;
            lv.Symbol := stmt.LoopVar;
            lv.TargetDecl := stmt.LoopVar.DeclType;
            //
            sign := TExprOperator.Create;
            sign.OType := otSign;
            sign.TargetDecl := stmt.LoopVar.DeclType;
            sign.Right := stmt.IncrExpression;
            //
            minus := TExprOperator.Create;
            minus.OType := otMinus;
            minus.TargetDecl := stmt.LoopVar.DeclType;
            minus.Left := lv;
            minus.Right := expr;
            //
            mult := TExprOperator.Create;
            mult.OType := otMultiply;
            mult.TargetDecl := stmt.LoopVar.DeclType;
            mult.Left := sign;
            mult.Right := minus;
            //
            zero := TExprLiteral.Create;
            i := FLiterals.Add(stmt.LoopVar.DeclType, '0');
            zero.Literal := FLiterals[i];
            zero.TargetDecl := stmt.LoopVar.DeclType;
            //
            gt := TExprOperator.Create;
            gt.OType := otGreater;
            gt.TargetDecl := dtLogical;
            gt.Left := mult;
            gt.Right := zero;
            //
            stmt.TestExpression := TExpression(gt);
            FCodeGen.ForStep(stmt);
            FTokenGen.Get(ttype, token);
            if (token <> 'DO') then
            begin
                Error(Format('Expected DO or WHILE, got %s', [token]));
                FTokenGen.Unget(ttype, token);
            end;
            if (not Statement) then
                Error('Statement expected following DO');
            FCodeGen.ForStepIncr(stmt);
        end else if (token = 'WHILE') then
        begin
            // WHILE
            stmt.TestExpression := Expression;
            if (Assigned(stmt.TestExpression)) then
            begin
                if (stmt.TestExpression.TargetDecl <> dtLogical) then
                    Error('Boolean expression expected following WHILE');
            end else
                Error('Expression expected following WHILE');
            FTokenGen.Get(ttype, token);
            if (token <> 'DO') then
            begin
                Error(Format('Expected DO or WHILE, got %s', [token]));
                FTokenGen.Unget(ttype, token);
            end;
            FCodeGen.ForWhile(stmt);
            if (not Statement) then
                Error('Statement expected following DO');
            FCodeGen.ForWhileEnd(stmt);
        end else
        begin
            Error(Format('Expected DO, STEP or WHILE, got %s', [token]));
            FTokenGen.Unget(ttype, token);
        end;
    end;

    stmt.Free;
end;

function TCompiler.Gotoo: Boolean;
var
    ttype: TTokenType;
    token: AnsiString;
    gt: TGotoStatement;
begin
    FTokenGen.Get(ttype, token);
    if (ttype <> ttGoto) then
    begin
        FTokenGen.Unget(ttype, token);
        Result := False;
        Exit;
    end;

    Result := True;
    gt := TGotoStatement.Create;
    gt.Expr := DesignationalExpr;
    if (Assigned(gt.Expr)) then
        FCodeGen.Gotoo(gt.Expr)
    else
        Error('Designational expression expected following GOTO');
    gt.Free;
end;

function TCompiler.Iff: Boolean;
var
    ttype: TTokenType;
    token: AnsiString;
    stmt: TIfStatement;
begin
    FTokenGen.Get(ttype, token);
    if (ttype <> ttIf) then
    begin
        FTokenGen.Unget(ttype, token);
        Result := False;
        Exit;
    end;

    Result := True;
    stmt := TIfStatement.Create;
    stmt.Expr := Expression;
    if (stmt.Expr.TargetDecl <> dtLogical) then
    begin
        Error('Boolean expression expected following IF');
        Exit;
    end;
    FTokenGen.Get(ttype, token);
    if (token <> 'THEN') then
    begin
        FTokenGen.Unget(ttype, token);
        Error(Format('THEN expected, got %s', [token]));
    end;
    Inc(FSymbolNum);
    stmt.ElseLblNum := FSymbolNum;
    Inc(FSymbolNum);
    stmt.EndLblNum := FSymbolNum;
    FCodeGen.IfThen(stmt);
    if (not Statement) then
    begin
        Error('Statement expected following THEN');
        Exit;
    end;
    FCodeGen.IfElse(stmt);
    FTokenGen.Get(ttype, token);
    if (token = 'ELSE') then
    begin
        if (not Statement) then
            Error('Statement expected');
    end else
        FTokenGen.Unget(ttype, token);
    FCodeGen.IfEnd(stmt);

    stmt.Free;
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
        sym := NewSymbol(token, stLabel, False);
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
            sym := NewSymbol(token, stLabel, False);
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

function TCompiler.NewSymbol(id: AnsiString; st: TSymbolType; isArray: Boolean): TSymbol;
begin
    if (st = stSwitch) then
        Result := TSwitchSymbol.Create
    else
        Result := TSymbol.Create;
    Result.ID := id;
    Result.SymbolType := st;
    Result.IsArray := isArray;
    Inc(FSymbolNum);
    Result.SymbolNum := FSymbolNum;
end;

function TCompiler.ProcedureDecl: Boolean;
var
    ttypeDecl, ttypeProc: TTokenType;
    tokenDecl, tokenProc: AnsiString;
begin
    FTokenGen.Get(ttypeDecl, tokenDecl);
    if (ttypeDecl = ttComment) then
    begin
        FTokenGen.FlushLine;
        Result := True;
        Exit;
    end else if ((ttypeDecl = ttDeclarator) and
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
        Result := Assignment(0, dtUnknown);
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
            Result := True
        else if (Iff) then
            Result := True
        else if (Forr) then
            Result := True
        else if (Write) then
            Result := True;
      end;
    end;
end;

function TCompiler.Write: Boolean;
var
    ttype, ttype2: TTokenType;
    token, token2: AnsiString;
    expr: TExpression;
    arr: TExprArray;
    sym: TSymbol;
    wr: TWriteStatement;
begin
    FTokenGen.Get(ttype, token);
    if (ttype <> ttWrite) then
    begin
        FTokenGen.Unget(ttype, token);
        Result := False;
        Exit;
    end;

    Result := True;
    wr := TWriteStatement.Create;
    try
        FTokenGen.Get(ttype, token);
        if (ttype <> ttLeftParen) then
            Error(Format('Left bracket expected, got %s', [token]));
        FTokenGen.Get(ttype, token);
        if (token = 'PRINTER') then
            wr.Device := iodPrinter
        else if (token = 'PUNCH') then
            wr.Device := iodPunch
        else if (token = 'CONSOLE') then
            wr.Device := iodConsole
        else
            FTokenGen.Unget(ttype, token);
        if (wr.Device <> iodUnknown) then
        begin
            // Trash comma following device name, if present
            FTokenGen.Get(ttype, token);
            if (ttype <> ttComma) then
                FTokenGen.Unget(ttype, token);
        end else
            // Default device to printer
            wr.Device := iodPrinter;
        repeat
            // We need to look ahead a bit here to see if the parameter is
            // an array with no subscripts.
            FTokenGen.Get(ttype, token);
            if (ttype = ttIdentifier) then
            begin
                sym := FindSymbol(token, stVariable);
                if (Assigned(sym) and (sym.IsArray)) then
                begin
                    FTokenGen.Get(ttype2, token2);
                    if (ttype2 <> ttLeftParen) then
                    begin
                        arr := TExprArray.Create;
                        arr.Symbol := sym;
                        arr.TargetDecl := sym.DeclType;
                        wr.Params.Add(arr);
                        ttype := ttype2;
                        token := token2;
                        Continue;
                    end else
                        FTokenGen.Unget(ttype2, token2);
                end;
            end;
            FTokenGen.Unget(ttype, token);
            expr := Expression;
            if (Assigned(expr)) then
                wr.Params.Add(expr)
            else
            begin
                FTokenGen.Unget(ttype, token);
                Error('Expression or array expected for WRITE');
            end;
            FTokenGen.Get(ttype, token);
        until ttype <> ttComma;
        if (ttype <> ttRightParen) then
        begin
            Error(Format('Right bracket expected, got %s', [token]));
            FTokenGen.Unget(ttype, token);
        end;
        FCodeGen.Write(wr);
    finally
        wr.Free;
    end;
end;

end.
