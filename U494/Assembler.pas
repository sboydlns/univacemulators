unit Assembler;

interface

uses Windows, SysUtils, IOUtils, Classes, Generics.Collections, SrcFile, ListFile, U494Opcodes,
     ObjFile, AsmTypes;

type
  TTokenType = ( ttUnknown, ttEndOfLine, ttEndOfFile, ttComment, ttLabel, ttIdentifier,
                 ttComma, ttNumber, ttString, ttLiteral, ttOperator, ttOperatorComment,
                 ttRealNumber, ttBcdNumber
                );

  TAssembler = class(TObject)
  private
    FInFile: String;
    FPrintFile: String;
    FObjectFile: String;
    FProcDir: String;
    FOutDir: String;
    FTokenTrace: Boolean;
    FPrintXref: Boolean;
    FPrintProcs: Boolean;
    FPass: Integer;
    FTransferAddr: UInt64;
    FObjCodeSize: Uint64;
    FErrorCount: Integer;
    FLastTokenType: TTokenType;
    FTokenUngot: Boolean;
    FUngotToken: AnsiString;
    FUngotTokenType: TTokenType;
    FListFile: TListFileStream;
    FOutFile: TObjFileStream;
    FOpcodes: TOpcodeList;
    FSymbols: TSymbolList;
    FStmtLabel: TSymbol;
    FEntryLabel: TSymbol;
    FFormats: TWordFormatList;
    FLocationCounter: TSymbol;
    FCurInst: TInstruction;
    FProcs: TProcList;
    FParenCount: Integer;
    FOutputType: TOutputType;
    procedure Do77(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoBLOCKDATA(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoBRegister(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoCOMMON(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoDLD(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoDO(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoEDEF(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoENTRY(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoEND(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoEQU(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoEXIT(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoEXPRESSION(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoFORM(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoFormWord(srcFile: TSrcFileStream; fmt: TWordFormat);
    procedure DoGeneral(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoINPUT(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoINPUTFORM(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoLET(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoLIST(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoLIT(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoProc(srcFile: TSrcFileStream; proc: TProc; entry: AnsiString);
    procedure DoRES(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoSTART(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoUNLIST(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoUTAG(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoXREF(srcFile: TSrcFileStream; op: TOpcode);
    procedure DoString(srcFile: TSrcFileStream; s: AnsiString);
    procedure DoWord(srcFile: TSrcFileStream; plusMinus: AnsiString);
    function Expression(srcFile: TSrcFileStream): TExpressionResult;
    function Expression0(srcFile: TSrcFileStream): TExpressionResult;
    function Expression1(srcFile: TSrcFileStream): TExpressionResult;
    function Expression2(srcFile: TSrcFileStream): TExpressionResult;
    function Expression3(srcFile: TSrcFileStream): TExpressionResult;
    function Expression4(srcFile: TSrcFileStream): TExpressionResult;
    function Expression5(srcFile: TSrcFileStream): TExpressionResult;
    function Expression6(srcFile: TSrcFileStream): TExpressionResult;
    function Expression7(srcFile: TSrcFileStream): TExpressionResult;
    function FindDesignator(op: TOpcode; token: AnsiString): TSymbol;
    procedure FlushToEndOfLine(srcFile: TSrcFileStream);
    function GetB(srcFile: TSrcFileStream; op: TOpcode): Byte;
    function GetJ(srcFile: TSrcFileStream; op: TOpcode; commaReqd: Boolean): Byte;
    function GetK(srcFile: TSrcFileStream; op: TOpcode): Byte;
    function GetY(srcFile: TSrcFileStream; commaReqd: Boolean): TExpressionResult;
    procedure GetToken(srcFile: TSrcFileStream; var ttype: TTokenType; var token: AnsiString);
    function LoadProc(fname: String): Boolean;
    function LoadProcs: Boolean;
    function NumberToBcd(num: AnsiString): UInt64;
    function NumberToInt(num: AnsiString): Int64;
    function NumberToReal(num: AnsiString): UInt64;
    function Pass0: Boolean;
    procedure Pass1(srcFile: TSrcFileStream);
    procedure Pass2(srcFile: TSrcFileStream);
    procedure PrintXref;
    function StringToWord(s: AnsiString): UInt32;
    function StringToDblWord(s: AnsiString): UInt64;
    procedure UngetToken(ttype: TTokenType; token: AnsiString);
  public
    constructor Create;
    destructor Destroy; override;
    function Assemble(inFile: String; ttrace: Boolean; xref: Boolean; procDir: String;
                      noproc: Boolean; otype: TOutputType; outDir: String): Integer;
  end;

implementation

uses Math, Soap.HTTPUtil, AnsiStrings, U494Util, EmulatorTypes;

function Opcode(mnem: String; op: Byte; inst: T494InstructionType; opt: T494OperandType): TOpcode;
begin
    Result := TOpcode.Create;
    Result.InstType := inst;
    Result.Mnemonic := mnem;
    Result.Opcode := op;
    Result.OperandType := opt;
    Result.Proc := nil;
end;

function Symbol(id: AnsiString; value: UInt64; rel: Boolean; st: TSymbolType): TSymbol;
begin
    Result := TSymbol.Create;
    Result.ID := id;
    Result.Value := value;
    Result.Relocatable := rel;
    Result.SymbolType := st;
    Result.DefCount := 1;
end;

{ TAssembler }

function TAssembler.Assemble(inFile: String; ttrace: Boolean; xref: Boolean; procDir: String;
                             noproc: Boolean; otype: TOutputType; outDir: String): Integer;
var
    srcFile: TSrcFileStream;
    extn: String;
begin
    FInFile := infile;
    FPrintFile := TPath.GetDirectoryName(FInFile) + '\' +
                  TPath.GetFileNameWithoutExtension(FInFile) + '.lst';
    FListFile := TListFileStream.Create(FPrintFile, fmCreate);
    FListFile.Enabled := True;
    FTokenTrace := ttrace;
    FPrintXref := xref;
    FPrintProcs := not noproc;
    FProcDir := procDir;
    FOutputType := otype;
    FOutDir := outDir;
    if (FOutDir = '') then
        FOutDir := TPath.GetDirectoryName(FInFile) + '\'
    else if (FOutDir[Length(FOutDir)] <> '\') then
        FOutDir := FOutDir + '\';
    case FOutputType of
      otImage,
      otExecutable: extn := '.mem';
      otObject:     extn := '.obj';
    end;
    FObjectFile := FOutDir +
                   TPath.GetFileNameWithoutExtension(FInFile) + extn;
    if (FOutputType = otExecutable) then
        FLocationCounter.Value := 96
    else
        FLocationCounter.Value := 0;
    if (Pass0) then
    begin
        srcFile := TSrcFileStream.Create(FInFile);
        srcFile.Column := 81;
        Pass1(srcFile);
        Pass2(srcFile);
        PrintXref;
        srcFile.Free;
    end;
    WriteLn(Format('%-20.20s: %d error(s) encountered', [TPath.GetFileName(FInFIle), FErrorCount]));
    Result := FErrorCount;
end;

constructor TAssembler.Create;
var
    i: Integer;
    op: TOpcode;
begin
    inherited;
    FLastTokenType := ttUnknown;
    FOpcodes := TOpcodeList.Create;
    FSymbols := TSymbolList.Create;
    FFormats := TWordFormatList.Create;
    FProcs := TProcList.Create;
    for i := Low(U494StdOpcodes) to High(U494StdOpcodes) do
    begin
        if (U494StdOpcodes[i].AsmMnemonic <> 'UNK') then
        begin
            op := Opcode(U494StdOpcodes[i].AsmMnemonic, U494StdOpcodes[i].Opcode,
                         U494StdOpcodes[i].InstType, U494StdOpcodes[i].OperandType);
            FOpcodes.Add(U494StdOpcodes[i].AsmMnemonic, op);
            if ((op.OperandType = otGeneral) or (op.OperandType = otIO)) then
                op.Proc := DoGeneral
            else
                op.Proc := DoBRegister;
        end;
    end;
    for i := Low(U494ExtOpcodes) to High(U494ExtOpcodes) do
    begin
        if (U494ExtOpcodes[i].AsmMnemonic <> 'UNK') then
        begin
            op := Opcode(U494ExtOpcodes[i].AsmMnemonic, U494ExtOpcodes[i].Opcode,
                         U494ExtOpcodes[i].InstType, U494ExtOpcodes[i].OperandType);
            FOpcodes.Add(U494ExtOpcodes[i].AsmMnemonic, op);
            op.Proc := Do77;
        end;
    end;
    for i := Low(U494PsuedoOps) to High(U494PsuedoOps) do
    begin
        if (U494PsuedoOps[i].AsmMnemonic <> 'UNK') then
        begin
            op := Opcode(U494PsuedoOps[i].AsmMnemonic, U494PsuedoOps[i].Opcode,
                         U494PsuedoOps[i].InstType, U494PsuedoOps[i].OperandType);
            FOpcodes.Add(U494PsuedoOps[i].AsmMnemonic, op);
            if ((op.OperandType = otGeneral) or (op.OperandType = otIO)) then
                op.Proc := DoGeneral
            else
                op.Proc := DoBRegister;
        end;
    end;
    for i := Low(AsmDirectives) to High(AsmDirectives) do
        if (AsmDirectives[i].AsmMnemonic <> 'UNK') then
            FOpcodes.Add(AsmDirectives[i].AsmMnemonic,
                         Opcode(AsmDirectives[i].AsmMnemonic, AsmDirectives[i].Opcode,
                         AsmDirectives[i].InstType, AsmDirectives[i].OperandType));
    //
    FOpcodes.Items['EQU'].Proc := DoEQU;
    FOpcodes.Items['RES'].Proc := DoRES;
    FOpcodes.Items['LIT'].Proc := DoLIT;
    FOpcodes.Items['FORM'].Proc := DoFORM;
    FOpcodes.Items['START'].Proc := DoSTART;
    FOpcodes.Items['END'].Proc := DoEND;
    FOpcodes.Items['DLD'].Proc := DoDLD;
    FOpcodes.Items['UTAG'].Proc := DoUTAG;
    FOpcodes.Items['DO'].Proc := DoDO;
    FOpcodes.Items['COMMON'].Proc := DoCOMMON;
    FOpcodes.Items['BLOCKDATA'].Proc := DoBLOCKDATA;
    FOpcodes.Items['XREF'].Proc := DoXREF;
    FOpcodes.Items['EDEF'].Proc := DoEDEF;
    FOpcodes.Items['EXPRESSION'].Proc := DoEXPRESSION;
    FOpcodes.Items['INPUT'].Proc := DoINPUT;
    FOpcodes.Items['INPUTFORM'].Proc := DoINPUTFORM;
    FOpcodes.Items['LET'].Proc := DoLET;
    FOpcodes.Items['UNLIST'].Proc := DoUNLIST;
    FOpcodes.Items['LIST'].Proc := DoLIST;
    FOpcodes.Items['ENTRY'].Proc := DoENTRY;
    FOpcodes.Items['EXIT'].Proc := DoEXIT;
    // Create system defined identifiers
    FLocationCounter := Symbol('$', 0, True, stSystem);
    FSymbols.Add('$', FLocationCounter);
    // b registers
    FSymbols.Add('B0', Symbol('B0', 0, False, stSystem));
    FSymbols.Add('B1', Symbol('B1', 1, False, stSystem));
    FSymbols.Add('B2', Symbol('B2', 2, False, stSystem));
    FSymbols.Add('B3', Symbol('B3', 3, False, stSystem));
    FSymbols.Add('B4', Symbol('B4', 4, False, stSystem));
    FSymbols.Add('B5', Symbol('B5', 5, False, stSystem));
    FSymbols.Add('B6', Symbol('B6', 6, False, stSystem));
    FSymbols.Add('B7', Symbol('B7', 7, False, stSystem));
    // k designators for read instructions
    FSymbols.Add('R$O', Symbol('R$O', 0, False, stSystem));
    FSymbols.Add('R$L', Symbol('R$L', 1, False, stSystem));
    FSymbols.Add('R$U', Symbol('R$U', 2, False, stSystem));
    FSymbols.Add('R$W', Symbol('R$W', 3, False, stSystem));
    FSymbols.Add('R$X', Symbol('R$X', 4, False, stSystem));
    FSymbols.Add('R$LX', Symbol('R$LX', 5, False, stSystem));
    FSymbols.Add('R$UX', Symbol('R$UX', 6, False, stSystem));
    FSymbols.Add('R$A', Symbol('R$A', 7, False, stSystem));
    // k designators for store instructions
    FSymbols.Add('S$Q', Symbol('S$Q', 0, False, stSystem));
    FSymbols.Add('S$L', Symbol('S$L', 1, False, stSystem));
    FSymbols.Add('S$U', Symbol('S$U', 2, False, stSystem));
    FSymbols.Add('S$W', Symbol('S$W', 3, False, stSystem));
    FSymbols.Add('S$A', Symbol('S$A', 4, False, stSystem));
    FSymbols.Add('S$CPL', Symbol('S$CPL', 5, False, stSystem));
    FSymbols.Add('S$CPU', Symbol('S$CPU', 6, False, stSystem));
    FSymbols.Add('S$CPW', Symbol('S$CPW', 7, False, stSystem));
    // k designators for replace instructions
    FSymbols.Add('RP$L', Symbol('RP$L', 1, False, stSystem));
    FSymbols.Add('RP$U', Symbol('RP$U', 2, False, stSystem));
    FSymbols.Add('RP$W', Symbol('RP$W', 3, False, stSystem));
    FSymbols.Add('RP$LX', Symbol('RP$LX', 5, False, stSystem));
    FSymbols.Add('RP$UX', Symbol('RP$UX', 6, False, stSystem));
    // k designators for I/O instructions
    FSymbols.Add('I$L', Symbol('I$L', 1, False, stSystem));
    FSymbols.Add('I$U', Symbol('I$U', 2, False, stSystem));
    FSymbols.Add('I$W', Symbol('I$W', 3, False, stSystem));
    // Standard j designators
    FSymbols.Add('SKIP', Symbol('SKIP', 1, False, stSystem));
    FSymbols.Add('QPOS', Symbol('QPOS', 2, False, stSystem));
    FSymbols.Add('QNEG', Symbol('QNEG', 3, False, stSystem));
    FSymbols.Add('AZERO', Symbol('AZERO', 4, False, stSystem));
    FSymbols.Add('ANOT', Symbol('ANOT', 5, False, stSystem));
    FSymbols.Add('APOS', Symbol('APOS', 6, False, stSystem));
    FSymbols.Add('ANEG', Symbol('ANEG', 7, False, stSystem));
    // Special j designators
    FSymbols.Add('TA$YLESS', Symbol('TA$YLESS', 6, False, stSystem));
    FSymbols.Add('TA$YMORE', Symbol('TA$YMORE', 7, False, stSystem));
    //
    FSymbols.Add('TQ$YLESS', Symbol('TQ$YLESS', 2, False, stSystem));
    FSymbols.Add('TQ$YMORE', Symbol('TQ$YMORE', 3, False, stSystem));
    //
    FSymbols.Add('TR$YIN', Symbol('TR$YIN', 4, False, stSystem));
    FSymbols.Add('TR$YOUT', Symbol('TR$YOUT', 4, False, stSystem));
    //
    FSymbols.Add('J$KEY1', Symbol('J$KEY1', 1, False, stSystem));
    FSymbols.Add('J$KEY2', Symbol('J$KEY2', 2, False, stSystem));
    FSymbols.Add('J$KEY3', Symbol('J$KEY3', 3, False, stSystem));
    FSymbols.Add('J$STOP', Symbol('J$STOP', 4, False, stSystem));
    FSymbols.Add('J$STOP5', Symbol('J$STOP5', 5, False, stSystem));
    FSymbols.Add('J$STOP6', Symbol('J$STOP6', 6, False, stSystem));
    FSymbols.Add('J$STOP7', Symbol('J$STOP7', 7, False, stSystem));
    //
    FSymbols.Add('SLJ$KEY1', Symbol('SLJ$KEY1', 1, False, stSystem));
    FSymbols.Add('SLJ$KEY2', Symbol('SLJ$KEY2', 2, False, stSystem));
    FSymbols.Add('SLJ$KEY3', Symbol('SLJ$KEY3', 3, False, stSystem));
    FSymbols.Add('SLJ$STOP', Symbol('SLJ$STOP', 4, False, stSystem));
    FSymbols.Add('SLJ$KEY5', Symbol('SLJ$KEY5', 5, False, stSystem));
    FSymbols.Add('SLJ$KEY6', Symbol('SLJ$KEY6', 6, False, stSystem));
    FSymbols.Add('SLJ$KEY7', Symbol('SLJ$KEY7', 7, False, stSystem));
    //
    FSymbols.Add('JT$RIL', Symbol('JT$RIL', 0, False, stSystem));
    FSymbols.Add('JT$RILJP', Symbol('JT$RILJP', 1, False, stSystem));
    FSymbols.Add('JT$QPOS', Symbol('JT$QPOS', 2, False, stSystem));
    FSymbols.Add('JT$QNEG', Symbol('JT$QNEG', 3, False, stSystem));
    FSymbols.Add('JT$AZERO', Symbol('JT$AZERO', 4, False, stSystem));
    FSymbols.Add('JT$ANOT', Symbol('JT$ANOT', 5, False, stSystem));
    FSymbols.Add('JT$APOS', Symbol('JT$APOS', 6, False, stSystem));
    FSymbols.Add('JT$ANEG', Symbol('JT$ANEG', 7, False, stSystem));
    //
    FSymbols.Add('SLJT$SIL', Symbol('SLJT$SIL', 0, False, stSystem));
    FSymbols.Add('SLJT$SILJP', Symbol('SLJT$SILJP', 1, False, stSystem));
    FSymbols.Add('SLJT$QPOS', Symbol('SLJT$QPOS', 2, False, stSystem));
    FSymbols.Add('SLJT$QNEG', Symbol('SLJT$QNEG', 3, False, stSystem));
    FSymbols.Add('SLJT$AZERO', Symbol('SLJT$AZERO', 4, False, stSystem));
    FSymbols.Add('SLJT$ANOT', Symbol('SLJT$ANOT', 5, False, stSystem));
    FSymbols.Add('SLJT$APOS', Symbol('SLJT$APOS', 6, False, stSystem));
    FSymbols.Add('SLJT$ANEG', Symbol('SLJT$ANEG', 7, False, stSystem));
    //
    FSymbols.Add('R$ADV', Symbol('R$ADV', 1, False, stSystem));
    FSymbols.Add('R$BACK', Symbol('R$BACK', 2, False, stSystem));
    FSymbols.Add('R$ADDB', Symbol('R$ADDV', 3, False, stSystem));
    FSymbols.Add('R$R', Symbol('R$R', 4, False, stSystem));
    FSymbols.Add('R$ADVR', Symbol('R$ADVR', 5, False, stSystem));
    FSymbols.Add('R$BACKRNE', Symbol('R$BACKRNE', 6, False, stSystem));
    FSymbols.Add('R$ADDBR', Symbol('R$ADDBR', 7, False, stSystem));
    //
    FSymbols.Add('AQ$SKIP', Symbol('AQ$SKIP', 1, False, stSystem));
    FSymbols.Add('AQ$APOS', Symbol('AQ$APOS', 2, False, stSystem));
    FSymbols.Add('AQ$ANEG', Symbol('AQ$ANEG', 3, False, stSystem));
    FSymbols.Add('AQ$QZERO', Symbol('AQ$QZERO', 4, False, stSystem));
    FSymbols.Add('AQ$QNOT', Symbol('AQ$QNOT', 5, False, stSystem));
    FSymbols.Add('AQ$QPOS', Symbol('AQ$QPOS', 6, False, stSystem));
    FSymbols.Add('AQ$QNEG', Symbol('AQ$QNEG', 7, False, stSystem));
    //
    FSymbols.Add('ANQ$SKIP', Symbol('ANQ$SKIP', 1, False, stSystem));
    FSymbols.Add('ANQ$APOS', Symbol('ANQ$APOS', 2, False, stSystem));
    FSymbols.Add('ANQ$ANEG', Symbol('ANQ$ANEG', 3, False, stSystem));
    FSymbols.Add('ANQ$QZERO', Symbol('ANQ$QZERO', 4, False, stSystem));
    FSymbols.Add('ANQ$QNOT', Symbol('ANQ$QNOT', 5, False, stSystem));
    FSymbols.Add('ANQ$QPOS', Symbol('ANQ$QPOS', 6, False, stSystem));
    FSymbols.Add('ANQ$QNEG', Symbol('ANQ$QNEG', 7, False, stSystem));
    //
    FSymbols.Add('LLP$SKIP', Symbol('LLP$SKIP', 1, False, stSystem));
    FSymbols.Add('LLP$EVAN', Symbol('LLP$EVAN', 2, False, stSystem));
    FSymbols.Add('LLP$ODD', Symbol('LLP$ODD', 3, False, stSystem));
    FSymbols.Add('LLP$AZERO', Symbol('LLP$AZERO', 4, False, stSystem));
    FSymbols.Add('LLP$ANOT', Symbol('LLP$ANOT', 5, False, stSystem));
    FSymbols.Add('LLP$APOS', Symbol('LLP$APOS', 6, False, stSystem));
    FSymbols.Add('LLP$ANEG', Symbol('LLP$ANEG', 7, False, stSystem));
    //
    FSymbols.Add('RLP$SKIP', Symbol('RLP$SKIP', 1, False, stSystem));
    FSymbols.Add('RLP$EVAN', Symbol('RLP$EVAN', 2, False, stSystem));
    FSymbols.Add('RLP$ODD', Symbol('RLP$ODD', 3, False, stSystem));
    FSymbols.Add('RLP$AZERO', Symbol('RLP$AZERO', 4, False, stSystem));
    FSymbols.Add('RLP$ANOT', Symbol('RLP$ANOT', 5, False, stSystem));
    FSymbols.Add('RLP$APOS', Symbol('RLP$APOS', 6, False, stSystem));
    FSymbols.Add('RLP$ANEG', Symbol('RLP$ANEG', 7, False, stSystem));
    //
    FSymbols.Add('D$SKIP', Symbol('D$SKIP', 1, False, stSystem));
    FSymbols.Add('D$NOOF', Symbol('D$NOOF', 2, False, stSystem));
    FSymbols.Add('D$OF', Symbol('D$OF', 3, False, stSystem));
    FSymbols.Add('D$AZERO', Symbol('D$AZERO', 4, False, stSystem));
    FSymbols.Add('D$ANOT', Symbol('D$ANOT', 5, False, stSystem));
    FSymbols.Add('D$APOS', Symbol('D$APOS', 6, False, stSystem));
    FSymbols.Add('D$ANEG', Symbol('D$ANEG', 7, False, stSystem));
    //
    FSymbols.Add('OUT$EXF', Symbol('OUT$EXF', 1, False, stSystem));
end;

destructor TAssembler.Destroy;
begin
    FreeAndNil(FSymbols);
    FreeAndNil(FFormats);
    FreeAndNil(FProcs);
    FreeAndNil(FListFile);
    inherited;
end;

procedure TAssembler.Do77(srcFile: TSrcFileStream; op: TOpcode);
var
    y: TExpressionResult;
    rel: TRelocatableType;
begin
    try
        y := GetY(srcFile, False);
        if (y.Relocatable) then
            rel := rtH2
        else
            rel := rtNone;
        FCurInst.y := y.Value;
        FCurInst.b := GetB(srcFile, op);
    finally
        FListFile.Value := FCurInst.Value;
    end;
    //
    if (FPass = 2) then
    begin
        FListFile.Print;
        FOutFile.EmitSingleWord(FLocationCounter.Value, rel, FCurInst.Value);
    end;
end;

procedure TAssembler.DoBLOCKDATA(srcFile: TSrcFileStream; op: TOpcode);
// This has to do with making FORTRAN compatible data elements.
// Not needed.
begin
    raise Exception.Create('BLOCK-DATA is not implemented.');
end;

procedure TAssembler.DoBRegister(srcFile: TSrcFileStream; op: TOpcode);
var
    y: TExpressionResult;
    rel: TRelocatableType;
begin
    try
        FCurInst.k := GetK(srcFile, op);
        FCurInst.j := GetJ(srcFile, op, False);
        y := GetY(srcFile, True);
        if (y.Relocatable) then
            rel := rtH2
        else
            rel := rtNone;
        FCurInst.y := y.Value;
        FCurInst.b := GetB(srcFile, op);
        if (op.Mnemonic = 'ZB') then
        begin
            FCurInst.k := 0;
            FCurInst.b := 0;
            FCurInst.y := 0;
            rel := rtNone;
        end else if (op.Mnemonic = 'ZQ') then
        begin
            FCurInst.j := 0;
            FCurInst.k := 0;
            FCurInst.b := 0;
            FCurInst.y := 0;
            rel := rtNone;
        end else if (op.Mnemonic = 'NOP') then
        begin
            FCurInst.j := 0;
            FCurInst.k := 0;
            FCurInst.b := 0;
            FCurInst.y := 0;
            rel := rtNone;
        end;
    finally
        FListFile.Value := FCurInst.Value;
    end;
    //
    if (FPass = 2) then
    begin
        FListFile.Print;
        FOutFile.EmitSingleWord(FLocationCounter.Value, rel, FCurInst.Value);
    end;
end;

procedure TAssembler.DoCOMMON(srcFile: TSrcFileStream; op: TOpcode);
begin
    raise Exception.Create('COMMON is not implemented.');
end;

procedure TAssembler.DoDLD(srcFile: TSrcFileStream; op: TOpcode);
var
    dword: Int64;
    addr: UInt32;
    sign: AnsiString;
    ttype: TTokenType;
    token: AnsiString;
begin
    sign := '';
    addr := FLocationCounter.Value;
    Inc(FLocationCounter.Value, 2);
    if (FPass = 2) then
    begin
        GetToken(srcFile, ttype, token);
        if ((ttype = ttOperator) and ((token = '-') or (token = '+'))) then
        begin
            sign := token;
            GetToken(srcFile, ttype, token);
        end;
        if (ttype = ttNumber) then
        begin
            dword := NumberToInt(sign + token);
            if (dword < 0) then
                dword := dword - 1;
        end else if (ttype = ttRealNumber) then
        begin
            dword := NumberToReal(sign + token);
        end else if (ttype = ttBcdNumber) then
        begin
            dword := NumberToBcd(sign + token)
        end else if (ttype = ttString) then
            dword := StringToDblWord(token)
        else
            raise Exception.CreateFmt('Expected a number but got (%s)', [token]);
        FOutFile.EmitDoubleWord(addr, dword);
        FListFile.Value := dword shr 30;
        if ((dword < 0) and (FListFile.Value = 0)) then
            FListFile.Value := UInt32(-1);
        FListFile.Print;
        Inc(addr);
        FListFile.Address := addr;
        FListFile.Source := '';
        FListFile.Value := dword and $3fffffff;
        FListFile.Print;
    end;
end;

procedure TAssembler.DoDO(srcFile: TSrcFileStream; op: TOpcode);
begin
    { TODO : Needs to be implemented as part of the macro processing pass. }
    raise Exception.Create('DO is not implemented.');
end;

procedure TAssembler.DoEDEF(srcFile: TSrcFileStream; op: TOpcode);
var
    token: AnsiString;
    ttype: TTokenType;
    sym: TSymbol;
begin
    if (FPass = 2) then
    begin
        FListFile.Print;
        Exit;
    end;

    GetToken(srcFile, ttype, token);
    while ((ttype <> ttEndOfLine) and (ttype <> ttEndOfFile)) do
    begin
        case ttype of
          ttIdentifier:
          begin
            if (FSymbols.TryGetValue(token, sym)) then
            begin
                sym.IsEntry := True;
            end else
            begin
                sym := TSymbol.Create;
                sym.IsEntry := True;
                sym.ID := token;
                sym.SymbolType := stIdentifier;
                sym.Relocatable := False;
                FSymbols.Add(token, sym);
            end;
          end;
          ttComma:
            ;
          else
            raise Exception.CreateFmt('Identifier expected. Got %s.', [token]);
        end;
        GetToken(srcFile, ttype, token);
    end;
    UngetToken(ttype, token);
end;

procedure TAssembler.DoEND(srcFile: TSrcFileStream; op: TOpcode);
var
    sym: TSymbol;
    addr: Integer;
begin
    if (FPass = 2) then
    begin
        FListFile.Print;
        FOutFile.EmitObjEnd(FLocationCounter.Value);
        for sym in FSymbols.Values do
        begin
            if (sym.IsExternal) then
            begin
                FOutFile.EmitExternalID(sym.ID);
                for addr in sym.ExternRef do
                    FOutFile.EmitExternalReference(addr);
                FOutFile.EmitExternalReference($3fffffff);
            end;
        end;
        FOutFile.EmitExternalID('');
    end;
end;

procedure TAssembler.DoENTRY(srcFile: TSrcFileStream; op: TOpcode);
begin
    if (not Assigned(FStmtLabel)) then
        raise Exception.Create('ENTRY directive must have a label');

    if (FPass = 2) then
    begin
        FEntryLabel := FStmtLabel;
        FCurInst.Value := 0;
        FCurInst.f := 49;               // Emit a J instruction
        FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, FCurInst.Value);
        FListFile.Value := FCurInst.Value;
        FListFile.Print;
    end;

    Inc(FLocationCounter.Value);
end;

procedure TAssembler.DoEQU(srcFile: TSrcFileStream; op: TOpcode);
var
    rslt: TExpressionResult;
begin
    if (not Assigned(FStmtLabel)) then
        raise Exception.Create('EQU must have a label.');

    if ((FPass = 2) and FStmtLabel.EquUndefPass1) then
        raise Exception.Create('Error evalulating expression in Pass 1. Foward referneces not allowed.');
    try
        rslt := Expression(srcFile);
    except
        if (FPass = 1) then
            FStmtLabel.EquUndefPass1 := True;
    end;
    FStmtLabel.Value := rslt.Value;
    FStmtLabel.Relocatable := rslt.Relocatable;
    if (FPass = 2) then
    begin
        if (Integer(FStmtLabel.Value) < 0) then
            FListFile.Value := Integer(FStmtLabel.Value) - 1
        else
            FListFile.Value := FStmtLabel.Value;
        FListFile.Print;
        Exit;
    end;
end;

procedure TAssembler.DoEXIT(srcFile: TSrcFileStream; op: TOpcode);
var
    ttype: TTokenType;
    sym: TSymbol;
    token, s: AnsiString;
begin
    if (not Assigned(FEntryLabel)) then
        raise Exception.Create('EXIT without a preceeding ENTRY');

    if (FPass = 2) then
    begin
        FCurInst.Value := 0;
        FCurInst.f := 49;               // Emit a J,L instruction
        FCurInst.k := 1;
        // Check for j designator
        GetToken(srcFile, ttype, token);
        if (ttype = ttIdentifier) then
        begin
            s := AnsiString(Format('J$%s', [token]));
            if (FSymbols.TryGetValue(s, sym)) then
            begin
                FCurInst.j := sym.Value;
            end else
            begin
                if (FSymbols.TryGetValue(token, sym)) then
                    FCurInst.j := sym.Value
                else if (token <> '') then
                    raise Exception.Createfmt('%s is not a valid j designator', [token]);
            end;
        end else
            UngetToken(ttype, token);
        //
        FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, FCurInst.Value);
        FListFile.Value := FCurInst.Value;
        FListFile.Print;
    end;

    Inc(FLocationCounter.Value);
end;

procedure TAssembler.DoEXPRESSION(srcFile: TSrcFileStream; op: TOpcode);
begin
    // Has to do with specifying binary constants.
    // Not needed.
    raise Exception.Create('EXPRESSION is not implemented.');
end;

procedure TAssembler.DoFORM(srcFile: TSrcFileStream; op: TOpcode);
var
    fmt: TWordFormat;
    rslt: TExpressionResult;
    ttype: TTokenType;
    token: AnsiString;
    i, ttl: UInt64;
begin
    //
    if (not Assigned(FStmtLabel)) then
    begin
        if (FPass = 1) then
            Exit
        else
            raise Exception.Create('FORM must have a label.');
    end;
    //
    if (FPass = 1) then
    begin
        FStmtLabel.SymbolType := stForm;
        fmt := TWordFormat.Create;
        fmt.ID := FStmtLabel.ID;
        FFormats.Add(fmt.ID, fmt);
        // Process all expressions found on this line and add each result
        // to the list of fields.
        repeat
            rslt := Expression(srcFile);
            fmt.Fields.Add(rslt.Value);
            GetToken(srcFile, ttype, token);
        until ttype <> ttComma;
        UngetToken(ttype, token);
    end else
    begin
        fmt := FFormats[FStmtLabel.ID];
        // Size of all fields cannot exceed 30
        ttl := 0;
        for i in fmt.Fields do
            Inc(ttl, i);
        if (ttl > 30) then
            raise Exception.Create('Total word size exceeds 30 bits');
        //
        FListFile.Print;
    end;
end;

procedure TAssembler.DoFormWord(srcFile: TSrcFileStream; fmt: TWordFormat);
var
    count: Integer;
    bitsRemaining: Integer;
    rslt: TExpressionResult;
    value, mask: UInt32;
    ttype: TTokenType;
    token: AnsiString;
    rels: array of Boolean;
begin
    bitsRemaining := 30;
    value := 0;
    SetLength(rels, fmt.Fields.Count);
    for count := 0 to fmt.Fields.Count - 1 do
    begin
        mask := Trunc(Power(2, fmt.Fields[count])) - 1;
        rslt := Expression(srcFile);
        if (rslt.Value < 0) then
            rslt.Value := rslt.Value - 1;
        rslt.Value := rslt.Value and mask;
        value := value or (rslt.Value shl (bitsRemaining - fmt.Fields[count]));
        rels[count] := rslt.Relocatable;
        Dec(bitsRemaining, fmt.Fields[count]);
        if (count <> (fmt.Fields.Count - 1)) then
        begin
            GetToken(srcFile, ttype, token);
            if (token <> ',') then
            begin
                UngetToken(ttype, token);
                Break;
            end;
        end;
    end;
    FListFile.Value := value;
    FListFile.Print;
    { TODO : How to handle odd sized relocatable fields }
    FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, value);
end;

procedure TAssembler.DoGeneral(srcFile: TSrcFileStream; op: TOpcode);
var
    y: TExpressionResult;
    rel: TRelocatableType;
begin
    try
        if (op.OperandType = otIO) then
            FCurInst.khat := GetK(srcFile, op)
        else
            FCurInst.k := GetK(srcFile, op);
        y := GetY(srcFile, False);
        if (y.Relocatable) then
            rel := rtH2
        else
            rel := rtNone;
        FCurInst.y := y.Value;
        FCurInst.b := GetB(srcFile, op);
        if (op.OperandType = otIO) then
            FCurInst.jhat := GetJ(srcFile, op, True)
        else
            FCurInst.j := GetJ(srcFile, op, True);
        if (op.Mnemonic = 'NQ') then
        begin
            FCurInst.k := 0;
        end else if (op.Mnemonic = 'NA') then
        begin
            FCurInst.k := 4;
        end else if (op.Mnemonic = 'SZ') then
        begin
            FCurInst.j := 0;
        end else if (op.Mnemonic = 'ZA') then
        begin
            FCurInst.b := 0;
            FCurInst.j := 0;
            FCurInst.k := 7;
            FCurInst.y := 0;
        end;
    finally
        FListFile.Value := FCurInst.Value;
    end;
    //
    if (FPass = 2) then
    begin
        FListFile.Print;
        FOutFile.EmitSingleWord(FLocationCounter.Value, rel, FCurInst.Value);
    end;
end;

procedure TAssembler.DoINPUT(srcFile: TSrcFileStream; op: TOpcode);
begin
    // For redefining the column layout of source cards.
    // Not needed.
    raise Exception.Create('INPUT is not implemented.');
end;

procedure TAssembler.DoINPUTFORM(srcFile: TSrcFileStream; op: TOpcode);
// A synonym for INPUT
begin
    DoInput(srcFile, op);
end;

procedure TAssembler.DoLET(srcFile: TSrcFileStream; op: TOpcode);
begin
    { TODO : Needs to be implemented as part of the macro processing pass. }
    raise Exception.Create('LET is not implemented.');
end;

procedure TAssembler.DoLIST(srcFile: TSrcFileStream; op: TOpcode);
begin
    FListFile.Enabled := True;
    if (FPass = 2) then
        FListFile.Print;
end;

procedure TAssembler.DoLIT(srcFile: TSrcFileStream; op: TOpcode);
begin
    raise Exception.Create('LIT not implemented');
end;

procedure TAssembler.DoProc(srcFile: TSrcFileStream; proc: TProc; entry: AnsiString);
var
    pname: TProcName;
    params: array of array of AnsiString;
    src: AnsiString;
    i: Integer;
    done, holdListEnabled: Boolean;
    tempFile: TSrcStringStream;

    procedure ParseParams;
    var
        peek: AnsiString;
    begin
        src := srcFile.Line + ' ';
        SetLength(params, 1);                                       // make room for operand subfields
        // Find the beginning of any subfields of the operand
        i := AnsiPos(' ' + entry + ',', src);
        if (i > 0) then
        begin
            src := Copy(src, i + Length(entry) + 2);                // discard the label and operand
            done := False;
            while (not done) do
            begin
                if (Copy(src, 1, 1) = '''') then
                begin
                    src := Copy(src, 2);
                    i := FirstDelimiter('''', String(src));
                    if (i > 0) then
                    begin
                        SetLength(params[0], Length(params[0]) + 1);
                        params[0, High(params[0])] := '''' + Copy(src, 1, i - 1) + '''';
                        src := Copy(src, i + 1);
                        if (AnsiPos(AnsiString(','), src) = 1) then
                            src := Copy(src, 2)
                        else
                            done := True;
                    end;
                end else
                begin
                    i := FirstDelimiter(', ', String(src));            // find the next comma or space
                    if (i > 0) then
                    begin
                        done := (src[i] = ' ');
                        SetLength(params[0], Length(params[0]) + 1);
                        params[0, High(params[0])] := Copy(src, 1, i - 1);
                        src := Copy(src, i + 1);
                    end else
                        done := True;
                end;
            end;
        end else
        begin
            i := AnsiPos(' ' + entry, src);
            src := TrimLeft(Copy(src, i + Length(entry) + 1));
        end;
        // Find all remaining fields and subfields
        src := TrimLeft(src);
        while (src <> '') do
        begin
            SetLength(params, Length(params) + 1);
            done := False;
            while (not done) do
            begin
                if (src[1] = '''') then
                begin
                    src := Copy(src, 2);
                    i := FirstDelimiter('''', String(src));
                    if (i > 0) then
                    begin
                        SetLength(params[High(params)], Length(params[High(params)]) + 1);
                        params[High(params), High(params[High(params)])] := '''' + Copy(src, 1, i - 1) + '''';
                        src := Copy(src, i + 1);
                        if (AnsiPos(AnsiString(','), src) = 1) then
                            src := Copy(src, 2)
                        else
                            done := True;
                    end;
                end else
                begin
                    i := FirstDelimiter(', ', String(src));
                    if (i > 0) then
                    begin
                        done := (src[i] = ' ');
                        SetLength(params[High(params)], Length(params[High(params)]) + 1);
                        params[High(params), High(params[High(params)])] := Copy(src, 1, i - 1);
                        src := Copy(src, i + 1);
                    end else
                        done := True;
                end;
            end;
            src := TrimLeft(src);
            if (src = '') then
            begin
                peek := srcFile.PeekLine;
                if (peek[7] = '-') then
                begin
                    if (FPass = 2) then
                        FListFile.Print;
                    srcFile.Line := AnsiString(UpperCase(String(srcFile.ReadLine)));
                    srcFile.Column := 8;
                    if (FPass = 2) then
                        FListFile.InitLine(srcFile.LineNumber, FLocationCounter.Value, srcFile.Line);
                    src := TrimLeft(Copy(srcFile.Line + ' ', 8));
                end;
            end;
        end;
        if ((High(params) <> 0) and (Length(params[High(params)]) = 0)) then
            SetLength(params, Length(params) - 1);
    end;

    procedure ExpandProc;
    var
        src: TProcSource;
        seg: TProcSegment;
        target: TProcName;
        s: AnsiString;
        i: Integer;
    begin
        i := pname.FirstSrcLine;
        while (i <= proc.SrcLinesMax) do
        begin
            src := proc.SrcLines[i];
            s := '';
            for seg in src.Segments do
            begin
                case seg.SType of
                  stConstant:
                  begin
                    s := s + seg.Constant;
                  end;
                  stNoIndex:
                  begin
                    if (proc.IsProcName(pname)) then
                        s := s + AnsiString(IntToStr(Length(params) - 1))
                    else
                        s := s + AnsiString(IntToStr(Length(params)));
                  end;
                  st1Index:
                  begin
                    if (seg.Index1 < Length(params)) then
                    begin
                        if (seg.Index1 = 0) then
                        begin
                            if (proc.IsProcName(pname)) then
                                s := s + '0'
                            else
                                s := s + AnsiString(IntToStr(Length(params[0])));
                        end else
                            s := s + AnsiString(IntToStr(Length(params[seg.Index1])));
                    end else
                        s := s + '0';
                  end;
                  st2Index:
                  begin
                    if ((seg.Index1 < Length(params)) and (seg.Index2 <= Length(params[seg.Index1]))) then
                    begin
                        if ((seg.Index1 = 0) and (seg.Index2 = 0)) then
                        begin
                            if (proc.IsProcName(pname)) then
                                s := s + '0'
                            else
                                s := s + AnsiString(IntToStr(pname.Param)) + 'D';
                        end else
                        begin
                            if (seg.Index2 > 0) then
                                s := s + params[seg.Index1, seg.Index2 - 1];
                        end;
                    end else
                        s := s + '0';
                  end;
                  st2IndexStar:
                  begin
                    if ((seg.Index1 <= Length(params)) and
                        (seg.Index2 <= Length(params[seg.Index1])) and
                        (seg.Index2 > 0)) then
                    begin
                        if (Pos('*', String(params[seg.Index1, seg.Index2 - 1])) = 1) then
                            s := s + '1'
                        else
                            s := s + '0';
                    end;
                  end;
                  stGo:
                  begin
                    if (not proc.FindName(seg.Constant, target)) then
                        raise Exception.CreateFmt('GO target %s is undefined', [seg.Constant]);
                    i := target.FirstSrcLine - 1;
                    s := '';
                  end;
                end;
            end;
            if (s <> '') then
                tempFile.AddLine(s);
            Inc(i);
        end;
    end;

begin
    proc.FindName(entry, pname);
    ParseParams;
    FLushToEndOfLine(srcFile);
    if (FPass = 2) then
        FListFile.Print;
    holdListEnabled := FListFile.Enabled;
    FListFile.Enabled := FListFile.Enabled and FPrintProcs;
    tempFile := TSrcStringStream.Create(srcFile.LineNumber);
    try
        ExpandProc;
        tempFile.Reset;
        if (FPass = 1) then
        begin
            Pass1(tempFile);
            FPass := 1;
        end else
        begin
            Pass2(tempFile);
            FPass := 2;
        end;
    finally
        tempFile.Free;
        FListFile.Enabled := holdListEnabled;
    end;
end;

procedure TAssembler.DoRES(srcFile: TSrcFileStream; op: TOpcode);
var
    rslt: TExpressionResult;
begin
    rslt := Expression(srcFile);
    FLocationCounter.Value := FLocationCounter.Value + rslt.Value;
    if (FPass = 2) then
        FListFile.Print;
end;

procedure TAssembler.DoSTART(srcFile: TSrcFileStream; op: TOpcode);
var
    sym: TSymbol;
    ttype: TTokenType;
    token: AnsiString;
begin
    if (FPass = 1) then
        Exit;

    FTransferAddr := $3fffffff;
    GetToken(srcFile, ttype, token);
    if (ttype = ttIdentifier) then
    begin
        if ((not FSymbols.TryGetValue(token, sym)) or (sym.DefCount = 0)) then
        begin
            FOutFile.EmitTransferAddr(0, FObjCodeSize);
            raise Exception.CreateFmt('Label (%s) is undefined.', [token]);
        end;
        FTransferAddr := sym.Value;
        sym.Xref.Add(srcFile.LineNumber);
    end;
    FListFile.Print;
    FOutFile.EmitTransferAddr(FTransferAddr, FObjCodeSize);
    for sym in FSymbols.Values do
    begin
        if (sym.IsEntry) then
            FOutFile.EmitEntryPoint(sym.ID, sym.Value);
    end;
    FOutFile.EmitEntryPoint('', 0);
end;

procedure TAssembler.DoString(srcFile: TSrcFileStream; s: AnsiString);
var
    c: AnsiChar;
    word: UInt32;
    count: Integer;
begin
    s := TCodeTranslator.AsciiToFieldata(s);
    count := 0;
    word := 0;
    for c in s do
    begin
        word := (word shl 6) or Ord(c);
        Inc(count);
        if (count >= 5) then
        begin
            FListFile.Value := word;
            FLIstFile.Print;
            FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, word);
            Inc(FLocationCounter.Value);
            FListFile.Address := FLocationCounter.Value;
            FListFile.Source := '';
            count := 0;
            word := 0;
        end;
    end;
    if (count > 0) then
    begin
        while (count < 5) do
        begin
            word := (word shl 6) or Ord(TCodeTranslator.AsciiToFieldata(' '));
            Inc(count);
        end;
        FListFile.Value := word;
        FLIstFile.Print;
        FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, word);
        Inc(FLocationCounter.Value);
    end;
end;

procedure TAssembler.DoUNLIST(srcFile: TSrcFileStream; op: TOpcode);
begin
    if (FPass = 2) then
        FListFile.Print;
    FListFile.Enabled := False;
end;

procedure TAssembler.DoUTAG(srcFile: TSrcFileStream; op: TOpcode);
var
    word, addr: UInt32;
    rslt: TExpressionResult;
    h1, h2: Integer;
    h1rel, h2rel: Boolean;
    rel: TRelocatableType;
    ttype: TTokenType;
    token: AnsiString;
begin
    addr := FLocationCounter.Value;
    Inc(FLocationCounter.Value, 1);
    if (FPass = 2) then
    begin
        rslt := Expression(srcFile);
        h1 := rslt.Value;
        h1rel := rslt.Relocatable;
        GetToken(srcFile, ttype, token);
        if (token <> ',') then
        begin
            UngetToken(ttype, token);
            raise Exception.CreateFmt('Comma expected but %s found.', [token]);
        end;
        rslt := Expression(srcFile);
        h2 := rslt.Value;
        h2rel := rslt.Relocatable;
        if (h1 < 0) then
            word := (h1 - 1) shl 15
        else
            word := h1 shl 15;
        if (h2 < 0) then
            word := word or ((UInt32(h2) - 1) and $7fff)
        else
            word := word or (UInt32(h2) and $7fff);
        FListFile.Value := word;
        FListFile.Print;
        if (h1rel and h2rel) then
            rel := rtH1H2
        else if (h1rel) then
            rel := rtH1
        else if (h2rel) then
            rel := rth2
        else
            rel := rtNone;
        FOutFile.EmitSingleWord(addr, rel, word);
    end;
end;

procedure TAssembler.DoWord(srcFile: TSrcFileStream; plusMinus: AnsiString);
var
    nums: array of Int64;
    rels: array of Boolean;
    rslt: TExpressionResult;
    ttype: TTokenType;
    token: AnsiString;
    word, addr: UInt32;
    wrel, h1rel, h2rel: Boolean;
    rel: TRelocatableType;
begin
    wrel := False;
    h1rel := False;
    h2rel := False;
    addr := FLocationCounter.Value;
    try
        SetLength(nums, 0);
        repeat
            rslt := Expression(srcFile);
            if ((Length(nums) > 0) and (rslt.IsExternal)) then
                raise Exception.Create('Externals may only be referenced by whole words');
            SetLength(nums, Length(nums) + 1);
            nums[High(nums)] := rslt.Value;
            if (plusMinus = '-') then
                nums[High(nums)] := -nums[High(nums)];
            SetLength(rels, Length(rels) + 1);
            rels[High(rels)] := rslt.Relocatable;
            plusMinus := '+';
            GetToken(srcFile, ttype, token);
        until token <> ',';
        UngetToken(ttype, token);
        case (Length(nums)) of
          1:
          begin
            if (nums[0] < 0) then
                word := nums[0] - 1
            else
                word := nums[0];
            wrel := rels[0];
          end;
          2:
          begin
            if (nums[0] < 0) then
                word := (nums[0] - 1) shl 15
            else
                word := nums[0] shl 15;
            h1rel := rels[0];
            if (nums[1] < 0) then
                word := word or ((nums[1] - 1) and $7fff)
            else
                word := word or (nums[1] and $7fff);
            h2rel := rels[1];
          end;
          3:
          begin
            if (nums[0] < 0) then
                word := (nums[0] - 1) shl 20
            else
                word := nums[0] shl 20;
            if (nums[1] < 0) then
                word := word or (((nums[1] - 1) and $3ff) shl 10)
            else
                word := word or ((nums[1] and $3ff) shl 10);
            if (nums[2] < 0) then
                word := word or ((nums[2] - 1) and $3ff)
            else
                word := word or (nums[2] and $3ff);
          end;
          5:
          begin
            if (nums[0] < 0) then
                word := (nums[0] - 1) shl 24
            else
                word := nums[0] shl 24;
            if (nums[1] < 0) then
                word := word or (((nums[1] - 1) and $3f) shl 18)
            else
                word := word or ((nums[1] and $3f) shl 18);
            if (nums[2] < 0) then
                word := word or (((nums[2] - 1) and $3f) shl 12)
            else
                word := word or ((nums[2] and $3f) shl 12);
            if (nums[3] < 0) then
                word := word or (((nums[3] - 1) and $3f) shl 6)
            else
                word := word or ((nums[3] and $3f) shl 6);
            if (nums[4] < 0) then
                word := word or ((nums[4] - 1) and $3f)
            else
                word := word or (nums[4] and $3f);
          end;
          else
          begin
            raise Exception.Create('There must be 1, 2, 3 or 5 sub-expressions');
          end;
        end;
        FListFile.Value := word;
        FListFile.Print;
        if (wrel) then
            rel := rtWord
        else if (h1rel and h2rel) then
            rel := rtH1H2
        else if (h1rel) then
            rel := rtH1
        else if (h2rel) then
            rel := rtH2
        else
            rel := rtNone;
        FOutFile.EmitSingleWord(addr, rel, word);
    finally
        Inc(FLocationCounter.Value);
    end;
end;

procedure TAssembler.DoXREF(srcFile: TSrcFileStream; op: TOpcode);
var
    token: AnsiString;
    ttype: TTokenType;
    sym: TSymbol;
begin
    if (FPass = 2) then
    begin
        if (FOutputType <> otObject) then
            raise Exception.Create('XREF only allowed when compiling to object.');
        FListFile.Print;
        Exit;
    end;

    GetToken(srcFile, ttype, token);
    while ((ttype <> ttEndOfLine) and (ttype <> ttEndOfFile)) do
    begin
        case ttype of
          ttIdentifier:
          begin
            if (FSymbols.TryGetValue(token, sym)) then
            begin
                if (sym.DefCount > 0) then
                    raise Exception.CreateFmt('%s is defined multiple times', [token])
                else
                begin
                    sym.DefCount := 1;
                    sym.IsExternal := True;
                end;
            end else
            begin
                sym := TSymbol.Create;
                sym.IsExternal := True;
                sym.ID := token;
                sym.SymbolType := stIdentifier;
                sym.Relocatable := False;
                sym.DefCount := 1;
                sym.DefLine := srcFile.LineNumber;
                FSymbols.Add(token, sym);
            end;
          end;
          ttComma:
            ;
          else
            raise Exception.CreateFmt('Identifier expected. Got %s.', [token]);
        end;
        GetToken(srcFile, ttype, token);
    end;
    UngetToken(ttype, token);
end;

function TAssembler.Expression(srcFile: TSrcFileStream): TExpressionResult;
var
    token: AnsiString;
    ttype: TTokenType;
begin
    Result := Expression0(srcFile);
    GetToken(srcFile, ttype, token);
    if ((token = ')') and (FParenCount <> 0)) then
        raise Exception.Create('Mismatched Parentheses');
    UngetToken(ttype, token);
end;

function TAssembler.Expression0(srcFile: TSrcFileStream): TExpressionResult;
begin
    FParenCount := 0;
    Result := Expression1(srcFile);
end;

function TAssembler.Expression1(srcFile: TSrcFileStream): TExpressionResult;
var
    op1, op2: TExpressionResult;
    ttype: TTokenType;
    token: AnsiString;
begin
    Result.IsExternal := False;
    op1 := Expression2(srcFile);
    GetToken(srcFile, ttype, token);
    while ((token = '=') or (token = '>') or (token = '<') or
           (token = '<=') or (token = '>=') or (token = '/=')) do
    begin
        if (op1.IsExternal) then
            raise Exception.Create('Externals are not allowed in expressions.');
        op2 := Expression2(srcFile);
        if (op2.IsExternal) then
            raise Exception.Create('Externals are not allowed in expressions.');
        if (token = '=') then
        begin
            if (op1.Value = op2.Value) then
                op1.Value := 1
            else
                op1.Value := 0;
            op1.Relocatable := False;
        end else if (token = '>') then
        begin
            if (op1.Value > op2.Value) then
                op1.Value := 1
            else
                op1.Value := 0;
            op1.Relocatable := False;
        end else if (token = '<') then
        begin
            if (op1.Value < op2.Value) then
                op1.Value := 1
            else
                op1.Value := 0;
            op1.Relocatable := False;
        end else if (token = '<=') then
        begin
            if (op1.Value <= op2.Value) then
                op1.Value := 1
            else
                op1.Value := 0;
            op1.Relocatable := False;
        end else if (token = '>=') then
        begin
            if (op1.Value >= op2.Value) then
                op1.Value := 1
            else
                op1.Value := 0;
            op1.Relocatable := False;
        end else if (token = '/=') then
        begin
            if (op1.Value <> op2.Value) then
                op1.Value := 1
            else
                op1.Value := 0;
            op1.Relocatable := False;
        end;
        GetToken(srcFile, ttype, token);
    end;
    UngetToken(ttype, token);
    Result := op1;
end;

function TAssembler.Expression2(srcFile: TSrcFileStream): TExpressionResult;
var
    op1, op2: TExpressionResult;
    ttype: TTokenType;
    token: AnsiString;
begin
    Result.IsExternal := False;
    op1 := Expression3(srcFile);
    GetToken(srcFile, ttype, token);
    while ((token = '++') or (token = '--')) do
    begin
        if (op1.IsExternal) then
            raise Exception.Create('Externals are not allowed in expressions.');
        op2 := Expression3(srcFile);
        if (op2.IsExternal) then
            raise Exception.Create('Externals are not allowed in expressions.');
        if (token = '++') then
        begin
            op1.Value := op1.Value or op2.Value;
            op1.Relocatable := False;
        end else if (token = '--') then
        begin
            op1.Value := op1.Value or op2.Value;
            op1.Relocatable := False;
        end;
        GetToken(srcFile, ttype, token);
    end;
    UngetToken(ttype, token);
    Result := op1;
end;

function TAssembler.Expression3(srcFile: TSrcFileStream): TExpressionResult;
var
    op1, op2: TExpressionResult;
    ttype: TTokenType;
    token: AnsiString;
begin
    Result.IsExternal := False;
    op1 := Expression4(srcFile);
    GetToken(srcFile, ttype, token);
    while (token = '**') do
    begin
        if (op1.IsExternal) then
            raise Exception.Create('Externals are not allowed in expressions.');
        op2 := Expression4(srcFile);
        if (op2.IsExternal) then
            raise Exception.Create('Externals are not allowed in expressions.');
        op1.Value := op1.Value and op2.Value;
        op1.Relocatable := False;
        GetToken(srcFile, ttype, token);
    end;
    UngetToken(ttype, token);
    Result := op1;
end;

function TAssembler.Expression4(srcFile: TSrcFileStream): TExpressionResult;
var
    op1, op2: TExpressionResult;
    ttype: TTokenType;
    token: AnsiString;
begin
    Result.IsExternal := False;
    op1 := Expression5(srcFile);
    GetToken(srcFile, ttype, token);
    while ((ttype = ttOperator) and ((token = '+') or (token = '-'))) do
    begin
        if (op1.IsExternal) then
            raise Exception.Create('Externals are not allowed in expressions.');
        op2 := Expression5(srcFile);
        if (op2.IsExternal) then
            raise Exception.Create('Externals are not allowed in expressions.');
        if (token = '+') then
        begin
            op1.Value := op1.Value + op2.Value;
            op1.Relocatable := op1.Relocatable <> op2.Relocatable;
        end else if (token = '-') then
        begin
            op1.Value := op1.Value - op2.Value;
            op1.Relocatable := op1.Relocatable and (not op2.Relocatable);
        end;
        GetToken(srcFile, ttype, token);
    end;
    UngetToken(ttype, token);
    Result := op1;
end;

function TAssembler.Expression5(srcFile: TSrcFileStream): TExpressionResult;
var
    op1, op2: TExpressionResult;
    ttype: TTokenType;
    token: AnsiString;
begin
    Result.IsExternal := False;
    op1 := Expression6(srcFile);
    GetToken(srcFile, ttype, token);
    while ((token = '*') or (token = '/') or (token = '//')) do
    begin
        if (op1.IsExternal) then
            raise Exception.Create('Externals are not allowed in expressions.');
        op2 := Expression6(srcFile);
        if (op2.IsExternal) then
            raise Exception.Create('Externals are not allowed in expressions.');
        if (token = '*') then
        begin
            op1.Value := op1.Value * op2.Value;
            op1.Relocatable := False;
        end else if (token = '/') then
        begin
            op1.Value := op1.Value div op2.Value;
            op1.Relocatable := False;
        end else if (token = '//') then
        begin
            op1.Value := (op1.Value + op2.Value - 1) div op2.Value;
            op1.Relocatable := False;
        end;
        GetToken(srcFile, ttype, token);
    end;
    UngetToken(ttype, token);
    Result := op1;
end;

function TAssembler.Expression6(srcFile: TSrcFileStream): TExpressionResult;
var
    op1, op2: TExpressionResult;
    ttype: TTokenType;
    token: AnsiString;
begin
    Result.IsExternal := False;
    op1 := Expression7(srcFile);
    GetToken(srcFile, ttype, token);
    while (token = '*/') do
    begin
        if (op1.IsExternal) then
            raise Exception.Create('Externals are not allowed in expressions.');
        op2 := Expression7(srcFile);
        if (op2.IsExternal) then
            raise Exception.Create('Externals are not allowed in expressions.');
        if (op2.Value < 0) then
            op1.Value := op1.Value shr -op2.Value
        else
            op1.Value := op1.Value shl op2.Value;
        op1.Relocatable := False;
        GetToken(srcFile, ttype, token);
    end;
    begin
        UngetToken(ttype, token);
        Result := op1;
    end;
end;

function TAssembler.Expression7(srcFile: TSrcFileStream): TExpressionResult;
// Highest priority operator. We are looking for a simple
// operand or another expression.
var
    ttype: TTokenType;
    token: AnsiString;
    sym: TSymbol;
    negative: Boolean;
begin
    GetToken(srcFile, ttype, token);
    // Skip and embedded comments
    while (ttype = ttOperatorComment) do
        GetToken(srcFile, ttype, token);
    // Unary minus?
    negative := False;
    if ((ttype = ttOperator) and (token = '-')) then
    begin
        negative := True;
        GetToken(srcFile, ttype, token);
    end else if (token = '+') then
    begin
        GetToken(srcFile, ttype, token);
    end;
    // Do we have to parse a sub-expression?
    if (token = '(') then
    begin
        Inc(FParenCount);
        Result := Expression0(srcFile);
        if (negative) then
            Result.Value := -Result.Value;
        GetToken(srcFile, ttype, token);
        // Skip and embedded comments
        while (ttype = ttOperatorComment) do
            GetToken(srcFile, ttype, token);
        if (token <> ')') then
            raise Exception.Create('Unbalanced parentheses');
        Dec(FParenCount);
    end else
    begin
        case ttype of
          ttIdentifier:
          begin
            if ((not FSymbols.TryGetValue(token, sym)) or (sym.DefCount = 0)) then
                raise Exception.CreateFmt('%s is undefined', [token]);
            if (FPass = 2) then
            begin
                sym.Xref.Add(srcFile.LineNumber);
                if (sym.IsExternal) then
                    sym.ExternRef.Add(FLocationCounter.Value);
            end;
            Result.Value := sym.Value;
            if (negative) then
                Result.Value := -Result.Value;
            Result.Relocatable := sym.Relocatable;
            Result.IsExternal := sym.IsExternal;
          end;
          ttNumber:
          begin
            Result.Value := NumberToInt(token);
            if (negative) then
                Result.Value := -Result.Value;
            Result.Relocatable := False;
            Result.IsExternal := False;
          end;
          ttString:
          begin
            { TODO : A string with a leading minus sign should be left justified. }
            Result.Value := StringToWord(token);
            Result.Relocatable := False;
            Result.IsExternal := False;
           end;
          ttLiteral:
            raise Exception.Create('Literals are not implemented');
          else
          begin
            raise Exception.Create('Expression syntax error');
          end;
        end;
    end;
end;

function TAssembler.FindDesignator(op: TOpcode; token: AnsiString): TSymbol;
var
    pfx: AnsiString;
    s: AnsiString;
begin
    s := AnsiString(Format('%s$%s', [op.Mnemonic, token]));
    if ((not FSymbols.TryGetValue(s, Result)) or (Result.DefCount = 0)) then
    begin
        if ((not FSymbols.TryGetValue(token, Result)) or (Result.DefCount = 0)) then
        begin
            case op.InstType of
              itRead:    pfx := 'R';
              itStore:   pfx := 'S';
              itReplace: pfx := 'RP';
              itIO:      pfx := 'I';
            end;
            s := AnsiString(Format('%s$%s', [pfx, token]));
            if ((not FSymbols.TryGetValue(s, Result)) or (Result.DefCount = 0)) then
                raise Exception.CreateFmt('Identifier (%s) undefined.', [token]);
        end;
    end;
end;

procedure TAssembler.FlushToEndOfLine(srcFile: TSrcFileStream);
begin
    srcFile.Column := 81;
    FTokenUngot := False;
end;

function TAssembler.GetB(srcFile: TSrcFileStream; op: TOpcode): Byte;
// Get the b designator
var
    token: AnsiString;
    ttype: TTokenType;
    sym: TSymbol;
begin
    Result := 0;
    GetToken(srcFile, ttype, token);
    if (token = ',') then
    begin
        GetToken(srcFile, ttype, token);
        if (token = ',') then                   // b designator not specified
        begin
            UngetToken(ttype, token);
            Exit;
        end;
        if (ttype = ttNumber) then
        begin
            Result := NumberToInt(token);
        end else if (ttype = ttIdentifier) then
        begin
            sym := FindDesignator(op, token);
            Result := sym.Value;
        end else
    end else
        UngetToken(ttype, token);
    if (Result > 7) then
        raise Exception.Create('Invalid B register. 0<= b <= 7.');
end;

function TAssembler.GetJ(srcFile: TSrcFileStream; op: TOpcode; commaReqd: Boolean): Byte;
// Get the j designator
var
    token: AnsiString;
    ttype: TTokenType;
    sym: TSymbol;
begin
    Result := 0;
    GetToken(srcFile, ttype, token);
    if (commaReqd) then
    begin
        if (token = ',') then
        begin
            GetToken(srcFile, ttype, token);
            if (ttype <> ttIdentifier) then
                raise Exception.CreateFmt('j designator expected but found (%s).', [token]);
            sym := FindDesignator(op, token);
            Result := sym.Value;
        end else
            UngetToken(ttype, token);
    end else
    begin
        if (ttype = ttIdentifier) then
        begin
            sym := FindDesignator(op, token);
            Result := sym.Value;
        end else
            UngetToken(ttype, token);
    end;
    if (Result > 7) then
        raise Exception.Create('Invalid j designator. 0<= j <= 7.');
end;

function TAssembler.GetK(srcFile: TSrcFileStream; op: TOpcode): Byte;
// Get the k designator
var
    token: AnsiString;
    ttype: TTokenType;
    sym: TSymbol;
begin
    Result := 0;
    GetToken(srcFile, ttype, token);
    if (token = ',') then
    begin
        GetToken(srcFile, ttype, token);
        if (ttype = ttNumber) then
        begin
            Result := NumberToInt(token);
        end else if (ttype = ttIdentifier) then
        begin
            sym := FindDesignator(op, token);
            Result := sym.Value;
        end else
            raise Exception.CreateFmt('k designator expected but found (%s).', [token]);
    end else
        UngetToken(ttype, token);
    if (Result > 7) then
        raise Exception.Create('Invalid k designator. 0<= k <= 7.');
end;

procedure TAssembler.GetToken(srcFile: TSrcFileStream; var ttype: TTokenType; var token: AnsiString);
var
    c: AnsiChar;
    peek: AnsiString;

    function NextLine: Boolean;
    begin
        if (srcFile.Eof) then
        begin
            ttype := ttEndOfFile;
            FLastTokenType := ttEndOfFile;
            Result := False;
            Exit;
        end;
        srcFile.Line := AnsiString(UpperCase(String(srcFile.ReadLine)));
        srcFile.Column := 8;
        if (FPass = 2) then
            FListFile.InitLine(srcFile.LineNumber, FLocationCounter.Value, srcFile.Line);
        Result := True;
    end;

    function DoComment: Boolean;
    begin
        if (srcFile.Column <= 79) then
        begin
            if ((srcFile.Line[srcFile.Column - 1] = ' ') and            // Did we find " . "?
                (srcFile.Line[srcFile.Column + 1] = ' ')) then
            begin
                if (srcFile.Column = 8) then                        // Whole line comment?
                    ttype := ttComment                          // yes
                else
                    ttype := ttEndOfLine;                       // no, report as eol
                FLastTokenType := ttype;
                Result := True;
                srcFile.Column := 81;
            end else
            begin
                Result := False;                                // No
                srcFile.Column := srcFile.Column + 1;
            end;
        end else
        begin
            ttype := ttComment;
            FLastTokenType := ttComment;
            Result := True;
            srcFile.Column := 81;
        end;
    end;

    procedure DoIdentifier;
    begin
        if ((srcFile.Line[7] <> '-') and (srcFile.Column = 8)) then
            ttype := ttLabel
        else
            ttype := ttIdentifier;
        FLastTokenType := ttype;
        repeat
            token := token + c;
            srcFile.Column := srcFile.Column + 1;
            if (srcFile.Column <= 80) then
                c := srcFile.Line[srcFile.Column];
        until ((srcFile.Column > 80) or
               (not (((c >= 'A') and (c <= 'Z')) or ((c >= '0') and (c <= '9')) or (c = '$'))));
        if (ttype = ttLabel) then
        begin
            while ((srcFile.Column <= 80) and (srcFile.Line[srcFile.Column] = '*')) do
            begin
                token := token + c;
                srcFile.Column := srcFile.Column + 1;
            end;
        end;
    end;

    procedure DoNumber;
    begin
        ttype := ttNumber;
        repeat
            if (c = '.') then
                ttype := ttRealNumber;
            token := token + c;
            srcFile.Column := srcFile.Column + 1;
            if (srcFile.Column <= 80) then
                c := srcFile.Line[srcFile.Column];
        until ((srcFile.Column > 80) or
               (not (((c >= '0') and (c <= '9')) or (c = 'D') or (c = '.') or (c = 'I') or (c = 'E'))));
        if (token[Length(token)] = 'I') then
            ttype := ttBcdNumber;
        FLastTokenType := ttype;
    end;

    procedure DoComma;
    begin
        token := c;
        ttype := ttComma;
        FLastTokenType := ttComma;
        srcFile.Column := srcFile.Column + 1;
    end;

    procedure DoString;
    begin
        ttype := ttString;
        FLastTokenType := ttString;
        srcFile.Column := srcFile.Column + 1;
        while ((srcFile.Column <= 80) and (srcFile.Line[srcFile.Column] <> '''')) do
        begin
            token := token + srcFile.Line[srcFile.Column];
            srcFile.Column := srcFile.Column + 1;
        end;
        if (srcFile.Line[srcFile.Column] <> '''') then
            raise Exception.Create('Unterminated string');
        srcFile.Column := srcFile.Column + 1;
    end;

    procedure DoLiteral;
    begin
        ttype := ttLiteral;
        FLastTokenType := ttLiteral;
        srcFile.Column := srcFile.Column + 1;
        while ((srcFile.Column <= 80) and (srcFile.Line[srcFile.Column] <> ';')) do
        begin
            token := token + srcFile.Line[srcFile.Column];
            srcFile.Column := srcFile.Column + 1;
        end;
        srcFile.Column := srcFile.Column + 1;
    end;

    procedure DoOperator;
    begin
        ttype := ttOperator;
        FLastTokenType := ttOperator;
        token := token + c;
        srcFile.Column := srcFile.Column + 1;
        if (srcFile.Column <= 80) then
        begin
            c := srcFile.Line[srcFile.Column];
            case token[1] of
              '*':
              begin
                if ((c = '/') or (c = '*')) then
                begin
                    token := token + c;
                    srcFile.Column := srcFile.Column + 1;
                end;
              end;
              '/':
              begin
                if ((c = '/') or (c = '=')) then
                begin
                    token := token + c;
                    srcFile.Column := srcFile.Column + 1;
                end;
                if (c = '.') then                               // /. = comment within expression
                begin
                    repeat
                        srcFile.Column := srcFile.Column + 1;
                    until ((srcFile.Column > 79) or
                           ((srcFile.Line[srcFile.Column] = '.') and (srcFile.Line[srcFile.Column + 1] = '/')));
                    srcFile.Column := srcFile.Column + 2;
                    token := '';
                    ttype := ttOperatorComment;
                    FLastTokenType := ttOperatorComment;
                end;
              end;
              '+':
              begin
                if (c = '+') then
                begin
                    token := token + c;
                    srcFile.Column := srcFile.Column + 1;
                end;
              end;
              '-':
              begin
                if (c = '-') then
                begin
                    token := token + c;
                    srcFile.Column := srcFile.Column + 1;
                end;
              end;
              '>',
              '<':
              begin
                if (c = '=') then
                begin
                    token := token + c;
                    srcFile.Column := srcFile.Column + 1;
                end;
              end;
            end;
        end;
    end;

begin
    ttype := ttUnknown;
    token := '';

    if (FTokenUngot) then
    begin
        ttype := FUngotTokenType;
        FLastTokenType := FUngotTokenType;
        token := FUngotToken;
        FTokenUngot := False;
        Exit;
    end;

    if (srcFile.Column > 80) then
    begin
        peek := srcFile.PeekLine;
        if ((peek[7] = '-') and (FLastTokenType = ttComment)) then
        begin
            ttype := ttComment;
            srcFile.Column := 81;
            Exit;
        end;
        if (not NextLine) then
            Exit;
    end;
    // Skip leading spaces
    while ((srcFile.Column <= 80) and (srcFile.Line[srcFile.Column] = ' ')) do
        srcFile.Column := srcFile.Column + 1;
    if (srcFile.Column > 80) then
    begin
        // Check for line continuation
        peek := srcFile.PeekLine;
        if (peek[7] <> '-') then
        begin
            ttype := ttEndOfLine;
            FLastTokenType := ttEndOfLine;
            Exit;
        end;
        if (not NextLine) then
            Exit;
    end;
    // Decide what to do based on the first character of the token
    c := srcFile.Line[srcFile.Column];
    if (c = '.') then
    begin
        if (DoComment) then
            Exit;
    end;
    if (((c >= 'A') and (c <= 'Z')) or (c = '$')) then
        DoIdentifier
    else if ((c >= '0') and (c <= '9')) then
        DoNumber
    else if (c = ',') then
        DoComma
    else if (c = '''') then
        DoString
    else if (c = ':') then
        DoLiteral
    else if ((c = '*') or (c = '/') or (c = '+') or (c = '-') or
             (c = '=') or (c = '>') or (c = '<') or (c = '(') or
             (c = ')')) then
        DoOperator;
end;

function TAssembler.GetY(srcFile: TSrcFileStream; commaReqd: Boolean): TExpressionResult;
// Get the y portion
var
    token: AnsiString;
    ttype: TTokenType;
begin
    Result.Value := 0;
    Result.Relocatable := False;
    GetToken(srcFile, ttype, token);
    if (commaReqd) then
    begin
        if (token = ',') then
            Result := Expression(srcFile)
        else
            UngetToken(ttype, token);
    end else
    begin
        UngetToken(ttype, token);
        if ((ttype <> ttComma) and (ttype <> ttEndOfLIne) and (ttype<> ttEndOfFile)) then
        begin
            Result := Expression(srcFile);
        end;
    end;
end;

function TAssembler.LoadProc(fname: String): Boolean;
var
    ttype: TTokenType;
    proc: TProc;
    token, lbl: AnsiString;
    rslt: TExpressionResult;
    srcFile: TSrcFileStream;
begin
    Result := True;
    proc := nil;
    srcFile := TSrcFileStream.Create(Format('%s\%s', [FProcDir, fname]));
    srcFile.Column := 81;
    repeat
        GetToken(srcFile, ttype, token);
        if (FTokenTrace) then
            WriteLn(Integer(ttype), '-', token);
        try
            if (ttype = ttLabel) then
            begin
                lbl := token;
            end else if (ttype = ttIdentifier) then
            begin
                if (token = 'PROC') then
                begin
                    if (Assigned(proc)) then
                        raise Exception.Create('PROCs may not be nested');
                    GetToken(srcFile, ttype, token);
                    UngetToken(ttype, token);
                    if ((ttype <> ttEndOfLine) and (ttype <> ttEndOfFile)) then
                        rslt := Expression(srcFile)
                    else
                        rslt.Value := 0;
                    proc := TProc.Create;
                    if (lbl <> '') then
                        proc.AddName(lbl, rslt.Value, proc.SrcLinesMin);
                    if (lbl = '') then
                        raise Exception.Create('PROC must have a label');
                end else if (token = 'NAME') then
                begin
                    if (lbl = '') then
                        raise Exception.Create('NAME must have a label');
                    if (not Assigned(proc)) then
                        raise Exception.Create('NAME without PROC');
                    GetToken(srcFile, ttype, token);
                    UngetToken(ttype, token);
                    if ((ttype <> ttEndOfLine) and (ttype <> ttEndOfFile)) then
                        rslt := Expression(srcFile)
                    else
                        rslt.Value := 0;
                    proc.AddName(lbl, rslt.Value, proc.SrcLinesLength);
                end else if (token = 'END') then
                begin
                    if (Assigned(proc)) then
                        FProcs.Add(proc)
                    else
                        raise Exception.Create('No PROC statement');
                    proc := nil;
                end else if (token = 'GO') then
                begin
                    GetToken(srcFile, ttype, token);
                    if (ttype <> ttIdentifier) then
                        raise Exception.Create('GO must be followed by an identifier');
                    proc.AddGoto(token);
                end else
                begin
                    if (Assigned(proc)) then
                        proc.AddSource(srcFile.Line);
                    FlushToEndOfLine(srcFile);
                end;
            end else if (ttype = ttEndOfLine) then
            begin
                lbl := '';
            end else if (ttype = ttComment) then
            begin
                lbl := '';
            end else if (ttype <> ttEndOfFile) then
            begin
                if (Assigned(proc)) then
                    proc.AddSource(srcFile.Line);
                FlushToEndOfLine(srcFile);
            end;
        except
          on E: Exception do
          begin
            Result := False;
            FListFile.Print;
            FListFile.Print(Format('%s: Line %d: %s', [fname, srcFile.LineNumber, E.Message]));
            FlushToEndOfLine(srcFile);
            FStmtLabel := nil;
            Inc(FErrorCount);
          end;
        end;
    until ttype = ttEndOfFile;
    FreeAndNil(srcFile);
end;

function TAssembler.LoadProcs: Boolean;
// Load all procs found in the proc library into memory tables. Not
// terribly efficient but easy.
var
    sr: TSearchRec;
    path: String;
    stat: Integer;
begin
    Result := True;
    path := Format('%s\*.proc', [FProcDir]);
    stat := FindFirst(path, faAnyFile, sr);
    while (stat = 0) do
    begin
        if (not LoadProc(sr.Name)) then
            Result := False;
        stat := FindNext(sr);
    end;
    FindClose(sr);
end;

function TAssembler.NumberToBcd(num: AnsiString): UInt64;
var
    i: Integer;
    isneg: Boolean;
begin
    Result := 0;
    isneg := False;
    // Init result to all zeros
    for i := 1 to 10 do
        Result := (Result shl 6) or $30;
    // Convert num to BCD
    i := 1;
    if (num[1] = '-') then
    begin
        isneg := True;
        i := 2;
    end;
    while (i <= Length(num)) do
    begin
        if (num[i] <> 'I') then
        begin
            if ((num[i] < '0') or (num[i] > '9')) then
                raise Exception.CreateFmt('%s is not a valid decimal number', [num]);
            Result := (Result shl 6) or (Ord(num[i]) - Ord('0') + $30);
        end;
        Inc(i);
    end;
    if (isneg) then
        Result := Result xor $10;
end;

function TAssembler.NumberToInt(num: AnsiString): Int64;
begin
    Result := 0;
    if (num = '') then
        Exit;
    //
    if (num[Length(num)] = 'D') then
    begin
        num := Copy(num, 1, Length(num) - 1);
        if (not TryStrToInt64(String(num), Result)) then
            raise Exception.CreateFmt('%s is not a valid decimal number', [num]);
    end else
    begin
        try
            Result := Octal(String(num));
        except
            raise Exception.CreateFmt('%s is not a valid octal number', [num]);
        end;
    end;
end;

function TAssembler.NumberToReal(num: AnsiString): UInt64;
var
    r: Double;
    pi: PUint64;
    sign, exp, mantissa: UInt64;
begin
    if (not TryStrToFloat(String(num), r)) then
        raise Exception.CreateFmt('%s is not a valid real number', [num]);
    pi := PUint64(@r);
    // The following bit of cruft converts from IEEE floating point format
    // to 494 format.
    //
    // 60-bit instead of 64-bit
    // exponent biased by 1024 rather than 1023
    // mantissa has zero to left of decimal instead of 1
    sign := pi^ shr 63;
    exp := (pi^ shr 52) and $7ff;
    mantissa := (pi^ and $fffffffffffff) shr 4;

    if ((exp <> 0) or (mantissa <> 0)) then
    begin
        exp := exp + 2;
        mantissa := (mantissa shr 1) or $800000000000;
    end;

    Result := (exp shl 48) or mantissa;
    if (sign <> 0) then
        Result := not Result;
end;

function TAssembler.Pass0: Boolean;
// Load the proc library and create a new source file with
// all procs expanded. Subsequent passes will read read this
// file to finish the compilation.
begin
    Result := True;
    FPass := 0;
    if (FProcDir = '') then
        Exit;

    if (not LoadProcs) then
    begin
        Result := False;
        Exit;
    end;
end;

procedure TAssembler.Pass1(srcFile: TSrcFileStream);
var
    ttype: TTokenType;
    token: AnsiString;
    sym: TSymbol;
    fmt: TWordFormat;
    proc: TProc;
    op: TOpcode;
    opSeen: Boolean;
    star: Integer;
begin
    FPass := 1;
    opSeen := False;
    FStmtLabel := nil;
    repeat
        GetToken(srcFile, ttype, token);
        if (FTokenTrace) then
            WriteLn(Integer(ttype), '-', token);
        try
            if (ttype = ttEndOfFile) then
            begin
                Continue
            end else if (ttype = ttUnknown) then
            begin
                raise Exception.CreateFmt('Invalid character(s) in source (%s)', [token]);
            end else if (ttype = ttLabel) then
            begin
                if (FSymbols.TryGetValue(token, sym)) then
                begin
                    if (sym.DefCount = 0) then
                    begin
                        sym.Value := FLocationCounter.Value;
                        sym.DefLine := srcFile.LineNumber;
                    end;
                    Inc(sym.DefCount);
                    sym.Relocatable := True;
                    FStmtLabel := sym;
                end else
                begin
                    sym := TSymbol.Create;
                    star := Pos('*', String(token));
                    if (star > 0) then
                    begin
                        sym.IsEntry := True;
                        token := Copy(token, 1, star - 1);
                    end;
                    sym.ID := token;
                    sym.SymbolType := stIdentifier;
                    sym.DefLine := srcFile.LineNumber;
                    sym.Value := FLocationCounter.Value;
                    sym.Relocatable := True;
                    sym.DefCount := 1;
                    FSymbols.Add(token, sym);
                    FStmtLabel := sym;
                end;
                Continue;
            end else if (ttype = ttComment) then
            begin
                opSeen := False;
                FStmtLabel := nil;
                Continue;
            end else if (ttype = ttEndOfLine) then
            begin
                opSeen := False;
                FStmtLabel := nil;
                Continue;
            end;
            if (not opSeen) then
            begin
                // Check for opcodes or directives
                if ((ttype = ttIdentifier) and FOpcodes.TryGetValue(String(token), op)) then
                begin
                    opSeen := True;
                    if (op.OperandType = otDirective) then
                        op.Proc(srcFile, op)
                    else
                        Inc(FLocationCounter.Value);
                // Check for storage generation
                end else if ((ttype = ttIdentifier) and FFormats.TryGetValue(token, fmt)) then
                begin
                    opSeen := True;
                    Inc(FLocationCounter.Value)
                end else if ((ttype = ttOperator) and ((token = '+') or (token = '-'))) then
                begin
                    opSeen := True;
                    GetToken(srcFile, ttype, token);
                    if (ttype = ttString) then
                        Inc(FLocationCounter.Value, (Length(token) + 4) div 5)
                    else
                        Inc(FLocationCounter.Value);
                // Check for procs
                end else if (FProcs.Find(token, proc)) then
                begin
                    DoProc(srcFile, proc, token);
                end else
                begin
                    opSeen := True;
                    Inc(FLocationCounter.Value);
                end;
            end;
        except
          on E: Exception do
          begin
//            WriteLn(Format('**** %s Line %d', [E.Message, FSrcFile.LineNumber]));
            FlushToEndOfLine(srcFile);
            opSeen := False;
            FStmtLabel := nil;
          end;
        end;
    until ttype = ttEndOfFile;
    FObjCodeSize := FLocationCounter.Value;
end;

procedure TAssembler.Pass2(srcFile: TSrcFileStream);
var
    ttype: TTokenType;
    token: AnsiString;
    fmt: TWordFormat;
    proc: TProc;
    op: TOpcode;
    opSeen: Boolean;
    plusMinus: AnsiString;
    star: Integer;
begin
    FPass := 2;
    if (not Assigned(FOutFile)) then
    begin
        case FOutputType of
          otImage,
          otExecutable: FOutFile := TMemImageStream.Create(FObjectFile, fmCreate);
          otObject:     FOutFile := TRelocatableStream.Create(FObjectFile, fmCreate);
        end;
        // Reset source file to beginning only if we are not being
        // called recursively.
        srcFile.Reset;
        srcFile.Column := 81;
        FStmtLabel := nil;
        if (FOutputType = otExecutable) then
            FLocationCounter.Value := 96
        else
            FLocationCounter.Value := 0;
        FListFile.Enabled := True;
    end;
    //
    opSeen := False;
    FStmtLabel := nil;
    repeat
        GetToken(srcFile, ttype, token);
        if (FTokenTrace) then
            WriteLn(Integer(ttype), '-', token);
        try
            if (ttype = ttEndOfFile) then
            begin
                Exit
            end else if (ttype = ttUnknown) then
            begin
                raise Exception.CreateFmt('Invalid character(s) in source (%s)', [token]);
            end else if (ttype = ttLabel) then
            begin
                star := Pos('*', String(token));
                if (star > 0) then
                    token := Copy(token, 1, star - 1);
                FStmtLabel := FSymbols.Items[token];
                if (FStmtLabel.DefCount > 1) then
                    raise Exception.CreateFmt('%s is defined multiple times', [token]);
                Continue;
            end else if (ttype = ttComment) then
            begin
                FListFile.Print;
                opSeen := False;
                FStmtLabel := nil;
                Continue;
            end else if (ttype = ttEndOfLine) then
            begin
                if (not opSeen) then
                    FListFile.Print;
                opSeen := False;
                FStmtLabel := nil;
                Continue;
            end;
            if (not opSeen) then
            begin
                // Check for opcodes or directives
                if ((ttype = ttIdentifier) and FOpcodes.TryGetValue(String(token), op)) then
                begin
                    opSeen := True;
                    FCurInst.Clear;
                    if (op.OperandType = otDirective) then
                    begin
                        op.Proc(srcFile, op);
                    end else
                    begin
                        FCurInst.f := op.Opcode;
                        if (op.InstType = it77) then
                        begin
                            FCurInst.f := $3f;
                            FCurInst.g := op.Opcode;
                        end;
                        try
                            op.Proc(srcFile, op);
                        finally
                            Inc(FLocationCounter.Value);
                        end;
                    end;
                // Check for storage generation
                end else if ((ttype = ttIdentifier) and FFormats.TryGetValue(token, fmt)) then
                begin
                    opSeen := True;
                    DoFormWord(srcFile, fmt);
                    Inc(FLocationCounter.Value)
                end else if ((ttype = ttOperator) and ((token = '+') or (token = '-'))) then
                begin
                    opSeen := True;
                    plusMinus := token;
                    GetToken(srcFile, ttype, token);
                    if (ttype = ttString) then
                    begin
                        DoString(srcFile, token);
                    end else
                    begin
                        UngetToken(ttype, token);
                        DoWord(srcFile, plusMinus);
                    end;
                // Check for procs
                end else if (FProcs.Find(token, proc)) then
                begin
                    DoProc(srcFile, proc, token);
                end else
                begin
                    opSeen := True;
                    UngetToken(ttype, token);
                    DoWord(srcFile, '+');
                end;
            end;
        except
          on E: Exception do
          begin
            FListFile.Print;
            FListFile.Print(Format('**** %s', [E.Message]));
            WriteLn(Format('%s: %s', [FListFile.LineNum, E.Message]));
            FlushToEndOfLine(srcFile);
            opSeen := False;
            FStmtLabel := nil;
            Inc(FErrorCount);
          end;
        end;
    until ttype = ttEndOfFile;
end;

procedure TAssembler.PrintXref;
var
    syms: TStringList;
    sym: TSymbol;
    val: String;
    i, l, count: Integer;
    s, ref, id: String;
begin
    if (not FPrintXref) then
        Exit;

    syms := TStringList.Create;
    try
        syms.Sorted := True;
        syms.OwnsObjects := False;
        for sym in FSymbols.Values do
            syms.AddObject(String(sym.ID), sym);
        FListFile.Print('');
        FListFile.Print('Symbol Cross Reference');
        FListFile.Print('ID         Addr  Line References');
        FListFile.Print('');
        for i := 0 to syms.Count - 1 do
        begin
            sym := TSymbol(syms.Objects[i]);
            if ((sym.SymbolType <> stSystem) and
                (sym.SymbolType <> stBDesignator) and
                (sym.SymbolType <> stJDesignator) and
                (sym.SymbolType <> stJDesignator) and
                (sym.SymbolType <> stForm)) then
            begin
                if (sym.IsEntry) then
                    id := String(sym.ID) + '*'
                else
                    id := String(sym.ID);
                val := Copy(FormatOctal(sym.Value), 6);
                if (sym.IsExternal) then
                    val := '?????';
                s := Format('%-10.10s %s %4d ', [id, val, sym.DefLine]);
                ref := '';
                count := 0;
                for l in sym.Xref do
                begin
                    ref := ref + Format('%4d ', [l]);
                    Inc(count);
                    if (count >= 15) then
                    begin
                        FListFile.Print(Format('%s %s', [s, ref]));
                        s := StringOfChar(' ', 22);
                        ref := '';
                        count := 0;
                    end;
                end;
                if (count > 0) then
                    FListFile.Print(Format('%s %s', [s, ref]));
            end;
        end;
    finally
        syms.Free;
    end;
end;

function TAssembler.StringToDblWord(s: AnsiString): UInt64;
var
    c: AnsiChar;
    count: Integer;
begin
    s := TCodeTranslator.AsciiToFieldata(Copy(s + '          ', 1, 10));
    Result := 0;
    count := 0;
    for c in s do
    begin
        Result := (Result shl 6) or Ord(c);
        Inc(count);
        if (count >= 10) then
            Break;
    end;
end;

function TAssembler.StringToWord(s: AnsiString): UInt32;
var
    c: AnsiChar;
    count: Integer;
begin
    s := TCodeTranslator.AsciiToFieldata(s);
    Result := 0;
    count := 0;
    for c in s do
    begin
        Result := (Result shl 6) or Ord(c);
        Inc(count);
        if (count >= 5) then
            Break;
    end;
end;

procedure TAssembler.UngetToken(ttype: TTokenType; token: AnsiString);
begin
    FUngotTokenType := ttype;
    FUngotToken := token;
    FTokenUngot := True;
end;

end.
