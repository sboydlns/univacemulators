unit Spurt;

interface

uses SysUtils, Classes, IOUtils, AsmTypes, SrcFile, ObjFile, ListFile,
    U494Opcodes;

type
    TSpurtStringList = class(TStringList)
    private
        function GetDelimitedText: String;
        procedure SetDelimitedText(const Value: String);
    public
        constructor Create;
        property DelimitedText: String read GetDelimitedText
          write SetDelimitedText;
    end;

    TAssembler = class(TObject)
    private
        FInFile: String;
        FPrintFile: String;
        FObjectFile: String;
        FListFile: TListFileStream;
        FOutFile: TObjFileStream;
        FProcDir: String;
        FOutDir: String;
        FTokenTrace: Boolean;
        FPrintXref: Boolean;
        FPrintProcs: Boolean;
        FTab: AnsiChar;
        FSeparator: AnsiChar;
        FIOSeparator: AnsiChar;
        FPass: Integer;
        FOutputType: TOutputType;
        FLocationCounter: TSymbol;
        FObjCodeSize: UInt64;
        FTransferAddrEmitted: Boolean;
        FErrorCount: Integer;
        FOpcodes: TOpcodeList;
        FSymbols: TSymbolList;
        FStmtLabel: TSymbol;
        FEntryLabel: TSymbol;
        FEntryJ: Integer;
        FFormats: TWordFormatList;
        FCurInst: TInstruction;
        FProcs: TProcList;
        FPrograms: TProgramList;
        FCrntProgram: TProgram;
        FTransferAddr: UInt32;
        FAllocationType: TAllocationType;
        FIOLibRequired: Boolean;
        function AdjustIdent(l: AnsiString): AnsiString; overload;
        function AdjustIdent(l: String): String; overload;
        procedure CallTypeCR(lineNum: Integer);
        procedure CallTypeDec(lineNum: Integer);
        procedure CallTypeSP(lineNum: Integer);
        procedure CallTypeTAB(lineNum: Integer);
        procedure AllocValue(var ops: AnsiString);
        procedure Do77(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoACONTROL(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoALLOCATION(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoCLEAR(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoCCONTROL(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoCOMMENT(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoDEC(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoDOTDOT(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoENDIT(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoENDLOCDD(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoENDPROC(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoENTRY(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoEQUALS(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoEXIT(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoFD(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoGeneral(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoINCREMENT(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoINDRALLOC(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoIO(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoLOCDD(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoMEANS(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoMOVE(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoORG(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoOUTPUTS(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoPROCEDURE(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoPROGRAM(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoPUT(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoRELALLOC(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoRESERVE(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoRIL(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoRILEX(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoSIL(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoSILEX(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoTERM(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoTYPEDEC(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoTYPET(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoUTAG(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoVRBL(lineNum: Integer; ops: AnsiString; op: TOpcode);
        procedure DoWord(lineNum: Integer; var ops: AnsiString);
        procedure GetChannel(var ops: AnsiString; var channel: AnsiString);
        procedure GetFields(sline: AnsiString;
          var lbl, operands, cmnt: AnsiString);
        procedure GetToken(var ops: AnsiString; var token: AnsiString);
        function GetNumber(var ops: AnsiString; var num: Integer): Boolean;
        function GetOpcode(var ops: AnsiString; var rslt: TOpcode;
          var b: Integer): Boolean;
        function GetY(lineNum: Integer; var ops: AnsiString;
          var rel: TRelocatableType; var b: Byte): Integer;
        function NumberToInt(num: String): Integer;
        function Pass0: Boolean;
        procedure Pass1(SrcFile: TSrcFileStream);
        procedure Pass2(SrcFile: TSrcFileStream);
        procedure PrintXref;
        procedure ReadStatement(SrcFile: TSrcFileStream;
          var lbl, operands, cmnt: AnsiString);
    public
        constructor Create;
        destructor Destroy; override;
        function Assemble(inFile: String; ttrace: Boolean; xref: Boolean;
          procDir: String; noproc: Boolean; otype: TOutputType; outDir: String;
          tabc, sepc, iosepc: String): Integer;
    end;

implementation

uses Math, U494Util, AnsiStrings, EmulatorTypes;

const
    // I/O library entry points
    TYPEA_ENTRY = $7E00; // Octal 77000
    TYPET_ENTRY = $7E01; // Octal 77001
    TYPESP_ENTRY = $7E1F; // Octal 77037
    TYPECR_ENTRY = $7E1A; // Octal 77032
    TYPETAB_ENTRY = $7E24; // Octal 77044
    TYPEDEC_ENTRY = $7E27; // Octal 77047

function Opcode(mnem: String; op: Byte; inst: T494InstructionType;
  opt: T494OperandType): TOpcode;
begin
    Result := TOpcode.Create;
    Result.InstType := inst;
    Result.Mnemonic := mnem;
    Result.Opcode := op;
    Result.OperandType := opt;
    Result.Proc := nil;
end;

function Symbol(id: AnsiString; Value: UInt64; rel: Boolean;
  st: TSymbolType): TSymbol;
begin
    Result := TSymbol.Create;
    Result.id := id;
    Result.Value := Value;
    Result.Relocatable := rel;
    Result.SymbolType := st;
    Result.DefCount := 1;
end;

{ TAssembler }

function TAssembler.AdjustIdent(l: AnsiString): AnsiString;
// Replace all alphbetic 'O' with zero as per SPURT reference page 5-C-1.
begin
    Result := StringReplace(l, AnsiString('0'), AnsiString('O'),
      [rfReplaceAll]);
end;

function TAssembler.AdjustIdent(l: String): String;
begin
    Result := StringReplace(l, '0', 'O', [rfReplaceAll]);
end;

procedure TAssembler.AllocValue(var ops: AnsiString);
// Attempt to get a numeric value from the operand and set the
// value of the current statement label to that value.
var
    val: Integer;
begin
    if (not Assigned(FStmtLabel)) then
        raise Exception.Create
          ('Statements following allocation directive must have a label');
    if (not GetNumber(ops, val)) then
        raise Exception.Create
          ('Statements following allocation directive must have a numeric operand');
    FStmtLabel.Value := val;
    FStmtLabel.AllocationType := FAllocationType;
    if (FPass = 2) then
    begin
        if (val < 0) then
            Dec(val);
        FListFile.Value := val;
        FListFile.Print;
    end;
end;

function TAssembler.Assemble(inFile: String; ttrace, xref: Boolean;
  procDir: String; noproc: Boolean; otype: TOutputType;
  outDir, tabc, sepc, iosepc: String): Integer;
var
    SrcFile: TSrcFileStream;
    extn: String;
begin
    FInFile := inFile;
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
      otImage, otExecutable:
        extn := '.mem';
      otObject:
        extn := '.obj';
      otAbsolute:
        extn := '.pt';
    end;
    FObjectFile := FOutDir + TPath.GetFileNameWithoutExtension(FInFile) + extn;
    if (FOutputType = otExecutable) then
        FLocationCounter.Value := 96
    else
        FLocationCounter.Value := 0;
    FTab := FirstChar(AnsiString(tabc));
    FSeparator := FirstChar(AnsiString(sepc));
    FIOSeparator := FirstChar(AnsiString(iosepc));
    if (Pass0) then
    begin
        SrcFile := TSrcFileStream.Create(FInFile);
        SrcFile.Column := 81;
        Pass1(SrcFile);
        Pass2(SrcFile);
        PrintXref;
        SrcFile.Free;
    end;
    WriteLn(Format('%-20.20s: %d error(s) encountered',
      [TPath.GetFileName(FInFile), FErrorCount]));
    Result := FErrorCount;
    FOutFile.Free;
    FListFile.Free;
end;

procedure TAssembler.CallTypeCR(lineNum: Integer);
var
    opcode: TOpcode;
begin
    if (FPass = 2) then
    begin
        Opcode := FOpcodes['RJP.MAN'];
        FCurInst.Value := 0;
        FCurInst.f := Opcode.Opcode;
        FCurInst.y := TYPECR_ENTRY;
        FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone,
          FCurInst.Value);
        FListFile.Value := FCurInst.Value;
        FListFile.Print;
        FListFile.InitLine(lineNum, FLocationCounter.Value + 1, '');
    end;
    Inc(FLocationCounter.Value);
end;

procedure TAssembler.CallTypeDec(lineNum: Integer);
var
    opcode: TOpcode;
begin
    if (FPass = 2) then
    begin
        Opcode := FOpcodes['RJP.MAN'];
        FCurInst.Value := 0;
        FCurInst.f := Opcode.Opcode;
        FCurInst.y := TYPEDEC_ENTRY;
        FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone,
          FCurInst.Value);
        FListFile.Value := FCurInst.Value;
        FListFile.Print;
        FListFile.InitLine(lineNum, FLocationCounter.Value + 1, '');
    end;
    Inc(FLocationCounter.Value);
end;

procedure TAssembler.CallTypeSP(lineNum: Integer);
var
    opcode: TOpcode;
begin
    if (FPass = 2) then
    begin
        Opcode := FOpcodes['RJP.MAN'];
        FCurInst.Value := 0;
        FCurInst.f := Opcode.Opcode;
        FCurInst.y := TYPESP_ENTRY;
        FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone,
          FCurInst.Value);
        FListFile.Value := FCurInst.Value;
        FListFile.Print;
        FListFile.InitLine(lineNum, FLocationCounter.Value + 1, '');
    end;
    Inc(FLocationCounter.Value);
end;

procedure TAssembler.CallTypeTAB(lineNum: Integer);
var
    opcode: TOpcode;
begin
    if (FPass = 2) then
    begin
        Opcode := FOpcodes['RJP.MAN'];
        FCurInst.Value := 0;
        FCurInst.f := Opcode.Opcode;
        FCurInst.y := TYPETAB_ENTRY;
        FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone,
          FCurInst.Value);
        FListFile.Value := FCurInst.Value;
        FListFile.Print;
        FListFile.InitLine(lineNum, FLocationCounter.Value + 1, '');
    end;
    Inc(FLocationCounter.Value);
end;

constructor TAssembler.Create;
var
    i: Integer;
    op: TOpcode;
begin
    inherited;
    FOpcodes := TOpcodeList.Create;
    FSymbols := TSymbolList.Create;
    FFormats := TWordFormatList.Create;
    FProcs := TProcList.Create;
    FPrograms := TProgramList.Create;
    for i := Low(U494StdOpcodes) to High(U494StdOpcodes) do
    begin
        if (U494StdOpcodes[i].SpurtMnemonic <> 'UNK') then
        begin
            op := Opcode(U494StdOpcodes[i].SpurtMnemonic,
              U494StdOpcodes[i].Opcode, U494StdOpcodes[i].InstType,
              U494StdOpcodes[i].OperandType);
            FOpcodes.Add(U494StdOpcodes[i].SpurtMnemonic, op);
            case op.OperandType of
              otGeneral:
                op.SpurtProc := DoGeneral;
              otBRegister:
                op.SpurtProc := DoGeneral;
              ot77:
                op.SpurtProc := Do77;
              otIO:
                op.SpurtProc := DoIO;
            end;
        end;
    end;
    for i := Low(U1230ExtOpcodes) to High(U1230ExtOpcodes) do
    begin
        if (U1230ExtOpcodes[i].SpurtMnemonic <> 'UNK') then
        begin
            op := Opcode(U1230ExtOpcodes[i].SpurtMnemonic,
              U1230ExtOpcodes[i].Opcode, U1230ExtOpcodes[i].InstType,
              U1230ExtOpcodes[i].OperandType);
            FOpcodes.Add(U1230ExtOpcodes[i].SpurtMnemonic, op);
            op.SpurtProc := Do77;
        end;
    end;
    for i := Low(U494PsuedoOps) to High(U494PsuedoOps) do
    begin
        if (U494PsuedoOps[i].SpurtMnemonic <> 'UNK') then
        begin
            op := Opcode(U494PsuedoOps[i].SpurtMnemonic,
              U494PsuedoOps[i].Opcode, U494PsuedoOps[i].InstType,
              U494PsuedoOps[i].OperandType);
            FOpcodes.Add(U494PsuedoOps[i].SpurtMnemonic, op);
            case op.OperandType of
              otGeneral:
                op.SpurtProc := DoGeneral;
              otBRegister:
                op.SpurtProc := DoGeneral;
              ot77:
                op.SpurtProc := Do77;
              otIO:
                op.SpurtProc := DoIO;
            end;
        end;
    end;
    for i := Low(SpurtDirectives) to High(SpurtDirectives) do
        if (SpurtDirectives[i].SpurtMnemonic <> 'UNK') then
            FOpcodes.Add(SpurtDirectives[i].SpurtMnemonic,
              Opcode(SpurtDirectives[i].SpurtMnemonic,
              SpurtDirectives[i].Opcode, SpurtDirectives[i].InstType,
              SpurtDirectives[i].OperandType));
    //
    FOpcodes.Items['A-CONTROL'].SpurtProc := DoACONTROL;
    FOpcodes.Items['C-CONTROL'].SpurtProc := DoCCONTROL;
    FOpcodes.Items['OUTPUTS'].SpurtProc := DoOUTPUTS;
    FOpcodes.Items['ALLOCATION'].SpurtProc := DoALLOCATION;
    FOpcodes.Items['REL-ALLOC'].SpurtProc := DoRELALLOC;
    FOpcodes.Items['INDR-ALLOC'].SpurtProc := DoINDRALLOC;
    FOpcodes.Items['PROGRAM'].SpurtProc := DoPROGRAM;
    FOpcodes.Items['SYSTEM'].SpurtProc := DoPROGRAM;
    FOpcodes.Items['SYS-PROC'].SpurtProc := DoPROGRAM;
    FOpcodes.Items['SYS-DD'].SpurtProc := DoPROGRAM;
    FOpcodes.Items['MEANS'].SpurtProc := DoMEANS;
    FOpcodes.Items['EQUALS'].SpurtProc := DoEQUALS;
    FOpcodes.Items['ENTRY'].SpurtProc := DoENTRY;
    FOpcodes.Items['EXIT'].SpurtProc := DoEXIT;
    FOpcodes.Items['U-TAG'].SpurtProc := DoUTAG;
    FOpcodes.Items['PUT'].SpurtProc := DoPUT;
    FOpcodes.Items['INCREMENT'].SpurtProc := DoINCREMENT;
    FOpcodes.Items['COMMENT'].SpurtProc := DoCOMMENT;
    FOpcodes.Items['RESERVE'].SpurtProc := DoRESERVE;
    FOpcodes.Items['CLEAR'].SpurtProc := DoCLEAR;
    FOpcodes.Items['MOVE'].SpurtProc := DoMOVE;
    FOpcodes.Items['TERM'].SpurtProc := DoTERM;
    FOpcodes.Items['SIL.ALL'].SpurtProc := DoSIL;
    FOpcodes.Items['SIL-EX'].SpurtProc := DoSILEX;
    FOpcodes.Items['RIL'].SpurtProc := DoRIL;
    FOpcodes.Items['RIL-EX'].SpurtProc := DoRILEX;
    FOpcodes.Items['FD'].SpurtProc := DoFD;
    FOpcodes.Items['ORG'].SpurtProc := DoORG;
    FOpcodes.Items['ENDIT'].SpurtProc := DoENDIT;
    FOpcodes.Items['DEC'].SpurtProc := DoDEC;
    FOpcodes.Items['TYPET'].SpurtProc := DoTYPET;
    FOpcodes.Items['TYPE-DEC'].SpurtProc := DoTYPEDEC;
    FOpcodes.Items['END-DATA'].SpurtProc := DoDOTDOT;
    FOpcodes.Items['LOC-DD'].SpurtProc := DoLOCDD;
    FOpcodes.Items['END-LOC-DD'].SpurtProc := DoENDLOCDD;
    FOpcodes.Items['VRBL'].SpurtProc := DoVRBL;
    FOpcodes.Items['PROCEDURE'].SpurtProc := DoPROCEDURE;
    FOpcodes.Items['END-PROC'].SpurtProc := DoENDPROC;
    FOpcodes.Items['RETURN'].SpurtProc := DoEXIT;
    FOpcodes.Items['..'].SpurtProc := DoDOTDOT;
    // Create system defined identifiers
    FLocationCounter := Symbol('$', 0, True, stSystem);
    FSymbols.Add('$', FLocationCounter);
    FAllocationType := atNone;
    // b registers
    FSymbols.Add('B0', Symbol('B0', 0, False, stSystem));
    FSymbols.Add('B1', Symbol('B1', 1, False, stSystem));
    FSymbols.Add('B2', Symbol('B2', 2, False, stSystem));
    FSymbols.Add('B3', Symbol('B3', 3, False, stSystem));
    FSymbols.Add('B4', Symbol('B4', 4, False, stSystem));
    FSymbols.Add('B5', Symbol('B5', 5, False, stSystem));
    FSymbols.Add('B6', Symbol('B6', 6, False, stSystem));
    FSymbols.Add('B7', Symbol('B7', 7, False, stSystem));
    // channel identifiers
    FSymbols.Add('C0', Symbol('C0', 0, False, stSystem));
    FSymbols.Add('C1', Symbol('C1', 1, False, stSystem));
    FSymbols.Add('C2', Symbol('C2', 2, False, stSystem));
    FSymbols.Add('C3', Symbol('C3', 3, False, stSystem));
    FSymbols.Add('C4', Symbol('C4', 4, False, stSystem));
    FSymbols.Add('C5', Symbol('C5', 5, False, stSystem));
    FSymbols.Add('C6', Symbol('C6', 6, False, stSystem));
    FSymbols.Add('C7', Symbol('C7', 7, False, stSystem));
    FSymbols.Add('C10', Symbol('C10', 8, False, stSystem));
    FSymbols.Add('C11', Symbol('C11', 9, False, stSystem));
    FSymbols.Add('C12', Symbol('C12', 10, False, stSystem));
    FSymbols.Add('C13', Symbol('C13', 11, False, stSystem));
    FSymbols.Add('C14', Symbol('C14', 12, False, stSystem));
    FSymbols.Add('C15', Symbol('C15', 13, False, stSystem));
    FSymbols.Add('C16', Symbol('C16', 14, False, stSystem));
    FSymbols.Add('C17', Symbol('C17', 15, False, stSystem));
    // k designators for read instructions
    FSymbols.Add('R$O(', Symbol('R$O(', 0, False, stSystem));
    FSymbols.Add('R$L(', Symbol('R$L(', 1, False, stSystem));
    FSymbols.Add('R$U(', Symbol('R$U(', 2, False, stSystem));
    FSymbols.Add('R$W(', Symbol('R$W(', 3, False, stSystem));
    FSymbols.Add('R$X(', Symbol('R$X(', 4, False, stSystem));
    FSymbols.Add('R$LX(', Symbol('R$LX(', 5, False, stSystem));
    FSymbols.Add('R$UX(', Symbol('R$UX(', 6, False, stSystem));
    FSymbols.Add('R$A', Symbol('R$A', 7, False, stSystem));
    // k designators for store instructions
    FSymbols.Add('S$Q', Symbol('S$Q', 0, False, stSystem));
    FSymbols.Add('S$L(', Symbol('S$L(', 1, False, stSystem));
    FSymbols.Add('S$U(', Symbol('S$U(', 2, False, stSystem));
    FSymbols.Add('S$W(', Symbol('S$W(', 3, False, stSystem));
    FSymbols.Add('S$A', Symbol('S$A', 4, False, stSystem));
    FSymbols.Add('S$CPL(', Symbol('S$CPL(', 5, False, stSystem));
    FSymbols.Add('S$CPU(', Symbol('S$CPU(', 6, False, stSystem));
    FSymbols.Add('S$CPW(', Symbol('S$CPW(', 7, False, stSystem));
    // k designators for replace instructions
    FSymbols.Add('RP$L(', Symbol('RP$L(', 1, False, stSystem));
    FSymbols.Add('RP$U(', Symbol('RP$U(', 2, False, stSystem));
    FSymbols.Add('RP$W(', Symbol('RP$W(', 3, False, stSystem));
    FSymbols.Add('RP$LX(', Symbol('RP$LX(', 5, False, stSystem));
    FSymbols.Add('RP$UX(', Symbol('RP$UX(', 6, False, stSystem));
    // k designators for I/O instructions
    FSymbols.Add('I$L(', Symbol('I$L(', 1, False, stSystem));
    FSymbols.Add('I$U(', Symbol('I$U(', 2, False, stSystem));
    FSymbols.Add('I$W(', Symbol('I$W(', 3, False, stSystem));
    // Standard j designators
    FSymbols.Add('SKIP', Symbol('SKIP', 1, False, stSystem));
    FSymbols.Add('QPOS', Symbol('QPOS', 2, False, stSystem));
    FSymbols.Add('QNEG', Symbol('QNEG', 3, False, stSystem));
    FSymbols.Add('AZERO', Symbol('AZERO', 4, False, stSystem));
    FSymbols.Add('ANOT', Symbol('ANOT', 5, False, stSystem));
    FSymbols.Add('APOS', Symbol('APOS', 6, False, stSystem));
    FSymbols.Add('ANEG', Symbol('ANEG', 7, False, stSystem));
    // Special j designators
    FSymbols.Add('COM.A$YLESS', Symbol('COM.A$YLESS', 6, False, stSystem));
    FSymbols.Add('COM.A$YMORE', Symbol('COM.A$YMORE', 7, False, stSystem));
    //
    FSymbols.Add('COM.Q$YLESS', Symbol('COM.Q$YLESS', 2, False, stSystem));
    FSymbols.Add('COM.Q$YMORE', Symbol('COM.Q$YMORE', 3, False, stSystem));
    //
    FSymbols.Add('COM.AQ$YIN', Symbol('COM.AQ$YIN', 4, False, stSystem));
    FSymbols.Add('COM.AQ$YOUT', Symbol('COM.AQ$YOUT', 5, False, stSystem));
    //
    FSymbols.Add('JP$RIL', Symbol('JP$RIL', 0, False, stSystem));
    FSymbols.Add('JP$RILJP', Symbol('JP$RILJP', 1, False, stSystem));
    FSymbols.Add('JP$QPOS', Symbol('JP$QPOS', 2, False, stSystem));
    FSymbols.Add('JP$QNEG', Symbol('JP$QNEG', 3, False, stSystem));
    FSymbols.Add('JP$AZERO', Symbol('JP$AZERO', 4, False, stSystem));
    FSymbols.Add('JP$ANOT', Symbol('JP$ANOT', 5, False, stSystem));
    FSymbols.Add('JP$APOS', Symbol('JP$APOS', 6, False, stSystem));
    FSymbols.Add('JP$ANEG', Symbol('JP$ANEG', 7, False, stSystem));
    // j designators for JP.MAN. Opcode for JP.MAN = opcode for JP + 1 which is stored
    // in the upper half word of Value.
    FSymbols.Add('JP$', Symbol('JP$', 0 + (1 shl 15), False, stSystem));
    FSymbols.Add('JP$KEY1', Symbol('JP$KEY1', 1 + (1 shl 15), False, stSystem));
    FSymbols.Add('JP$KEY2', Symbol('JP$KEY2', 2 + (1 shl 15), False, stSystem));
    FSymbols.Add('JP$KEY3', Symbol('JP$KEY3', 3 + (1 shl 15), False, stSystem));
    FSymbols.Add('JP$STOP', Symbol('JP$STOP', 4 + (1 shl 15), False, stSystem));
    FSymbols.Add('JP$STOP5', Symbol('JP$STOP5', 5 + (1 shl 15), False,
      stSystem));
    FSymbols.Add('JP$STOP6', Symbol('JP$STOP6', 6 + (1 shl 15), False,
      stSystem));
    FSymbols.Add('JP$STOP7', Symbol('JP$STOP7', 7 + (1 shl 15), False,
      stSystem));
    // j designator for JP.ACTI. Opcode for JP.ACTI = opcode for JP + 2.
    FSymbols.Add('JP$ACTIVEIN', Symbol('JP$ACTIVEIN', 0 + (2 shl 15), False,
      stSystem));
    // j designator for JP.ACTO. Opcode for JP.ACTO = opcode for JP + 3.
    FSymbols.Add('JP$ACTIVEOUT', Symbol('JP$ACTIVEOUT', 0 + (3 shl 15), False,
      stSystem));
    //
    FSymbols.Add('RJP$SIL', Symbol('RJP$SIL', 0, False, stSystem));
    FSymbols.Add('RJP$SILJP', Symbol('RJP$SILJP', 1, False, stSystem));
    FSymbols.Add('RJP$QPOS', Symbol('RJP$QPOS', 2, False, stSystem));
    FSymbols.Add('RJP$QNEG', Symbol('RJP$QNEG', 3, False, stSystem));
    FSymbols.Add('RJP$AZERO', Symbol('RJP$AZERO', 4, False, stSystem));
    FSymbols.Add('RJP$ANOT', Symbol('RJP$ANOT', 5, False, stSystem));
    FSymbols.Add('RJP$APOS', Symbol('RJP$APOS', 6, False, stSystem));
    FSymbols.Add('RJP$ANEG', Symbol('RJP$ANEG', 7, False, stSystem));
    // j designators for RJP.MAN. Opcode for RJP.MAN = opcode for RJP + 1 which is stored
    // in the upper half word of Value.
    FSymbols.Add('RJP$', Symbol('RJP$', 0 + (1 shl 15), False, stSystem));
    FSymbols.Add('RJP$KEY1', Symbol('RJP$KEY1', 1 + (1 shl 15), False,
      stSystem));
    FSymbols.Add('RJP$KEY2', Symbol('RJP$KEY2', 2 + (1 shl 15), False,
      stSystem));
    FSymbols.Add('RJP$KEY3', Symbol('RJP$KEY3', 3 + (1 shl 15), False,
      stSystem));
    FSymbols.Add('RJP$STOP', Symbol('RJP$STOP', 4 + (1 shl 15), False,
      stSystem));
    FSymbols.Add('RJP$KEY5', Symbol('RJP$KEY5', 5 + (1 shl 15), False,
      stSystem));
    FSymbols.Add('RJP$KEY6', Symbol('RJP$KEY6', 6 + (1 shl 15), False,
      stSystem));
    FSymbols.Add('RJP$KEY7', Symbol('RJP$KEY7', 7 + (1 shl 15), False,
      stSystem));
    //
    FSymbols.Add('RPT$ADV', Symbol('RPT$ADV', 1, False, stSystem));
    FSymbols.Add('RPT$BACK', Symbol('RPT$BACK', 2, False, stSystem));
    FSymbols.Add('RPT$ADDB', Symbol('RPT$ADDV', 3, False, stSystem));
    FSymbols.Add('RPT$R', Symbol('RPT$R', 4, False, stSystem));
    FSymbols.Add('RPT$ADVR', Symbol('RPT$ADVR', 5, False, stSystem));
    FSymbols.Add('RPT$BACKRNE', Symbol('RPT$BACKRNE', 6, False, stSystem));
    FSymbols.Add('RPT$ADDBR', Symbol('RPT$ADDBR', 7, False, stSystem));
    //
    FSymbols.Add('ADD.Q$SKIP', Symbol('ADD.Q$SKIP', 1, False, stSystem));
    FSymbols.Add('ADD.Q$APOS', Symbol('ADD.Q$APOS', 2, False, stSystem));
    FSymbols.Add('ADD.Q$ANEG', Symbol('ADD.Q$ANEG', 3, False, stSystem));
    FSymbols.Add('ADD.Q$QZERO', Symbol('ADD.Q$QZERO', 4, False, stSystem));
    FSymbols.Add('ADD.Q$QNOT', Symbol('ADD.Q$QNOT', 5, False, stSystem));
    FSymbols.Add('ADD.Q$QPOS', Symbol('ADD.Q$QPOS', 6, False, stSystem));
    FSymbols.Add('ADD.Q$QNEG', Symbol('ADD.Q$QNEG', 7, False, stSystem));
    //
    FSymbols.Add('SUB.Q$SKIP', Symbol('SUB.Q$SKIP', 1, False, stSystem));
    FSymbols.Add('SUB.Q$APOS', Symbol('SUB.Q$APOS', 2, False, stSystem));
    FSymbols.Add('SUB.Q$ANEG', Symbol('SUB.Q$ANEG', 3, False, stSystem));
    FSymbols.Add('SUB.Q$QZERO', Symbol('SUB.Q$QZERO', 4, False, stSystem));
    FSymbols.Add('SUB.Q$QNOT', Symbol('SUB.Q$QNOT', 5, False, stSystem));
    FSymbols.Add('SUB.Q$QPOS', Symbol('SUB.Q$QPOS', 6, False, stSystem));
    FSymbols.Add('SUB.Q$QNEG', Symbol('SUB.Q$QNEG', 7, False, stSystem));
    //
    FSymbols.Add('ENT.LP$SKIP', Symbol('ENT.LP$SKIP', 1, False, stSystem));
    FSymbols.Add('ENT.LP$EVAN', Symbol('ENT.LP$EVAN', 2, False, stSystem));
    FSymbols.Add('ENT.LP$ODD', Symbol('ENT.LP$ODD', 3, False, stSystem));
    FSymbols.Add('ENT.LP$AZERO', Symbol('ENT.LP$AZERO', 4, False, stSystem));
    FSymbols.Add('ENT.LP$ANOT', Symbol('ENT.LP$ANOT', 5, False, stSystem));
    FSymbols.Add('ENT.LP$APOS', Symbol('ENT.LP$APOS', 6, False, stSystem));
    FSymbols.Add('ENT.LP$ANEG', Symbol('ENT.LP$ANEG', 7, False, stSystem));
    //
    FSymbols.Add('RPL.LP$SKIP', Symbol('RPL.LP$SKIP', 1, False, stSystem));
    FSymbols.Add('RPL.LP$EVAN', Symbol('RPL.LP$EVAN', 2, False, stSystem));
    FSymbols.Add('RPL.LP$ODD', Symbol('RPL.LP$ODD', 3, False, stSystem));
    FSymbols.Add('RPL.LP$AZERO', Symbol('RPL.LP$AZERO', 4, False, stSystem));
    FSymbols.Add('RPL.LP$ANOT', Symbol('RPL.LP$ANOT', 5, False, stSystem));
    FSymbols.Add('RPL.LP$APOS', Symbol('RPL.LP$APOS', 6, False, stSystem));
    FSymbols.Add('RPL.LP$ANEG', Symbol('RPL.LP$ANEG', 7, False, stSystem));
    //
    FSymbols.Add('DIV$SKIP', Symbol('DIV$SKIP', 1, False, stSystem));
    FSymbols.Add('DIV$NOOF', Symbol('DIV$NOOF', 2, False, stSystem));
    FSymbols.Add('DIV$OF', Symbol('DIV$OF', 3, False, stSystem));
    FSymbols.Add('DIV$AZERO', Symbol('DIV$AZERO', 4, False, stSystem));
    FSymbols.Add('DIV$ANOT', Symbol('DIV$ANOT', 5, False, stSystem));
    FSymbols.Add('DIV$APOS', Symbol('DIV$APOS', 6, False, stSystem));
    FSymbols.Add('DIV$ANEG', Symbol('DIV$ANEG', 7, False, stSystem));
    //
    FSymbols.Add('SQRT$SKIP', Symbol('SQRT$SKIP', 1, False, stSystem));
    FSymbols.Add('SQRT$REM', Symbol('SQRT$REM', 2, False, stSystem));
    FSymbols.Add('SQRT$NOREM', Symbol('SQRT$NOREM', 3, False, stSystem));
    // Special khat designators for EX-COM / EX-FCT
    FSymbols.Add('EX-COM$', Symbol('EX-COM$', 2, False, stSystem));
    FSymbols.Add('EX-COM$FORCE', Symbol('EX-COM$FORCE', 3, False, stSystem));
    FSymbols.Add('EX-COM$MONITOR', Symbol('EX-COM$MONITOR', 0, False,
      stSystem));
    FSymbols.Add('EX-COM$MONFORCE', Symbol('EX-COM$MONFORCE', 1, False,
      stSystem));
    FSymbols.Add('EX-FCT$', Symbol('EX-FCT$', 2, False, stSystem));
    FSymbols.Add('EX-FCT$FORCE', Symbol('EX-FCT$FORCE', 3, False, stSystem));
    FSymbols.Add('EX-FCT$MONITOR', Symbol('EX-FCT$MONITOR', 0, False,
      stSystem));
    FSymbols.Add('EX-FCT$MONFORCE', Symbol('EX-FCT$MONFORCE', 1, False,
      stSystem));
    // Special khat designators for EX-COM-MW
    FSymbols.Add('EX-COM-MW$W(', Symbol('EX-COM-MW$W(', 2, False, stSystem));
    //
    FSymbols.Add('OUT$EXF', Symbol('OUT$EXF', 1, False, stSystem));
end;

destructor TAssembler.Destroy;
begin
    FreeAndNil(FSymbols);
    FreeAndNil(FFormats);
    FreeAndNil(FProcs);
    FreeAndNil(FListFile);
    FreeAndNil(FPrograms);
    inherited;
end;

procedure TAssembler.Do77(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    token: AnsiString;
    reg: Integer;
    y: UInt32;
    b: Byte;
    rel: TRelocatableType;
    sym: TSymbol;
begin
    if (op.Mnemonic = 'NORM.AQ') then
    begin
        y := GetY(lineNum, ops, rel, b);
        FCurInst.y := y;
        FCurInst.b := b;
    end else if ((op.Mnemonic = 'ENTSR') or (op.Mnemonic = 'STRSR')) then
    begin
        GetToken(ops, token);
        if (token = '.') then
            GetToken(ops, token);
        if (FirstChar(token) = 'C') then
        begin
            if ((not FSymbols.TryGetValue(token, sym)) or (sym.DefCount = 0))
            then
                raise Exception.CreateFmt('%s is undefined', [token]);
            FCurInst.j77 := sym.Value;
            GetToken(ops, token);
            // Get new register value
            if (token <> '.') then
                raise Exception.Create('ENTSR.C requires 2 operands');
            y := GetY(lineNum, ops, rel, b);
            // Get INPUT / OUTPUT
            GetToken(ops, token);
            if (token <> '.') then
                raise Exception.Create('ENTSR.C requires 2 operands');
            if (token = 'INPUT') then
                FCurInst.g := FCurInst.g + 1
            else if (token = 'OUTPUT') then
                FCurInst.g := FCurInst.g + 2
            else if (token = 'EF') then
                FCurInst.g := FCurInst.g + 3
            else
                raise Exception.Create('INPUT or OUTPUT or EF expected');
            FCurInst.b := b;
            FCurInst.j77 := sym.Value;
            FCurInst.y77 := y;
        end else
        begin
            GetNumber(token, reg);
            if ((reg < 0) or (reg > 2)) then
                raise Exception.Create('Invalid SR register number');
            FCurInst.j77 := reg;
            GetToken(ops, token);
            if (token <> '.') then
                ops := token + ops;
            y := GetY(lineNum, ops, rel, b);
            FCurInst.b := b;
            FCurInst.y77 := y;
        end;
    end else if ((op.Mnemonic = 'ENTSR0') or (op.Mnemonic = 'STRSR0')) then
    begin
        FCurInst.j77 := 0;
        GetToken(ops, token);
        if (token <> '.') then
            ops := token + ops;
        y := GetY(lineNum, ops, rel, b);
        FCurInst.b := b;
        FCurInst.y77 := y;
    end else if ((op.Mnemonic = 'ENTSR1') or (op.Mnemonic = 'STRSR1')) then
    begin
        FCurInst.j77 := 1;
        GetToken(ops, token);
        if (token <> '.') then
            ops := token + ops;
        y := GetY(lineNum, ops, rel, b);
        FCurInst.b := b;
        FCurInst.y77 := y;
    end else if ((op.Mnemonic = 'ENTSR2') or (op.Mnemonic = 'STRSR2')) then
    begin
        FCurInst.j77 := 2;
        GetToken(ops, token);
        if (token <> '.') then
            ops := token + ops;
        y := GetY(lineNum, ops, rel, b);
        FCurInst.b := b;
        FCurInst.y77 := y;
    end else if ((op.Mnemonic = 'ECDM') or (op.Mnemonic = 'DCDM')) then
    begin
        GetToken(ops, token);
        if (token = '.') then
            GetToken(ops, token);
        if (FirstChar(token) = 'C') then
        begin
            if ((not FSymbols.TryGetValue(token, sym)) or (sym.DefCount = 0))
            then
                raise Exception.CreateFmt('%s is undefined', [token]);
            FCurInst.j77 := sym.Value;
            GetToken(ops, token);
            if (token = '.') then
                GetToken(ops, token);
            if (token = 'OUTPUT') then
                FCurInst.g := FCurInst.g + 1
            else if (token <> 'INPUT') then
                raise Exception.Create('INPUT or OUTPUT EXPECTED');
        end else
        begin
            raise Exception.CreateFmt
              ('%s is not a valid channel identifier', [token]);
        end;
    end;
    FOutFile.EmitSingleWord(FLocationCounter.Value, rel, FCurInst.Value);
    FListFile.Value := FCurInst.Value;
    FListFile.Print;
end;

procedure TAssembler.DoACONTROL(lineNum: Integer; ops: AnsiString; op: TOpcode);
// As far as I can tell this is a fairly useless header which tells us that we are
// processing AS-1 source code.
begin
    if (FPass = 2) then
        FListFile.Print;
end;

procedure TAssembler.DoALLOCATION(lineNum: Integer; ops: AnsiString;
  op: TOpcode);
begin
    FAllocationType := atDirect;
    if (FOutputType = otObject) then
        raise Exception.Create
          ('Direct allocation not valid when compiling relocatable objec files');
    if (FPass = 1) then
    begin
        if (Assigned(FStmtLabel)) then
            Dec(FStmtLabel.DefCount);
    end else
        FListFile.Print;
end;

procedure TAssembler.DoCCONTROL(lineNum: Integer; ops: AnsiString; op: TOpcode);
// As far as I can tell this is a fairly useless header which tells us that we are
// processing CS-1 source code.
begin
    if (FPass = 2) then
        FListFile.Print;
end;

procedure TAssembler.DoCLEAR(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    op1: TOpcode;
    fields: TSpurtStringList;
begin
    if (FPass = 1) then
    begin
        Inc(FLocationCounter.Value, 2);
    end else
    begin
        fields := TSpurtStringList.Create;
        try
            // separate ops into count and destination operand
            fields.DelimitedText := String(ops);
            if (fields.Count <> 2) then
                raise Exception.Create('CLEAR directive requires 2 operands');
            fields[0] := fields[0] + '.ADV';
            op1 := FOpcodes.Items['RPT'];
            FCurInst.Value := 0;
            FCurInst.f := op1.Opcode;
            DoGeneral(lineNum, AnsiString(fields[0]), op1);
            Inc(FLocationCounter.Value);
            FListFile.InitLine(lineNum, FLocationCounter.Value, '');
            if (Pos(AnsiString('('), fields[1]) <> 0) then
                raise Exception.Create('k designator not allowed in operand 2');
            fields[1] := 'W(' + fields[1] + ')';
            op1 := FOpcodes.Items['CL'];
            FCurInst.Value := 0;
            FCurInst.f := op1.Opcode;
            DoGeneral(lineNum, AnsiString(fields[1]), op1);
            Inc(FLocationCounter.Value);
        finally
            fields.Free;
        end;
    end;
end;

procedure TAssembler.DoCOMMENT(lineNum: Integer; ops: AnsiString; op: TOpcode);
begin
    if (FPass = 2) then
        FListFile.Print;
end;

procedure TAssembler.DoDEC(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    val: Double;
    split, exp, ival: Integer;
begin
    if (FPass = 1) then
    begin
        Inc(FLocationCounter.Value);
    end else
    begin
        if (ops = '') then
        begin
            val := 0;
        end else
        begin
            split := AnsiPos(AnsiString('B'), ops);
            if (split > 0) then
            begin
                val := StrToFloat(String(Copy(ops, 1, split - 1)));
                exp := StrToInt(String(Copy(ops, split + 1)));
                val := val * Power(2, exp);
            end else
                val := StrToFloat(String(ops));
            ival := Trunc(val);
            FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone,
              UInt32(ival));
            FListFile.Value := UInt32(ival);
            FListFile.Print;
            Inc(FLocationCounter.Value);
        end;
    end;
end;

procedure TAssembler.DoDOTDOT(lineNum: Integer; ops: AnsiString; op: TOpcode);
begin
    FAllocationType := atNone;
    if (FPass = 2) then
        FListFile.Print;
end;

procedure TAssembler.DoENDIT(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    token: AnsiString;
    sym: TSymbol;
begin
    GetToken(ops, token);
    if (token = '') then
        Exit;
    if ((not FSymbols.TryGetValue(AdjustIdent(token), sym)) or
      (sym.DefCount = 0)) then
        raise Exception.CreateFmt('%s is undefined', [token]);
    FTransferAddr := sym.Value;
    if (FPass = 2) then
        FListFile.Print;
end;

procedure TAssembler.DoENDLOCDD(lineNum: Integer; ops: AnsiString; op: TOpcode);
begin
    if (FPass = 2) then
        FListFile.Print;
end;

procedure TAssembler.DoENDPROC(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    token, s: AnsiString;
    proc, sym: TSymbol;
    op1: TOpcode;
    opAdjust: Integer;
begin
    GetToken(ops, token);
    if (token = '') then
        raise Exception.Create('Procedure name expected');
    if (FPass = 2) then
    begin
        if ((not FSymbols.TryGetValue(AdjustIdent(token), proc)) or (proc.DefCount = 0)) then
            raise Exception.CreateFmt('%s is undefined', [token]);
        op1 := FOpcodes.Items['JP'];
        FCurInst.Value := 0;
        FCurInst.f := op1.Opcode; // Emit a JP L() instruction
        if (FEntryJ = 0) then
            FCurInst.k := 1;
        FCurInst.y := proc.Value;
        // Check for j designator
        GetToken(ops, token);
        if (token = '.') then
            GetToken(ops, token);
        s := AnsiString(Format('%s$%s', [op1.Mnemonic, token]));
        if (FSymbols.TryGetValue(s, sym)) then
        begin
            if (sym.DefCount < 1) then
                raise Exception.CreateFmt('%s is undefined', [token]);
            // See if opcode needs to be adjusted for this designator
            opAdjust := sym.Value shr 15;
            FCurInst.f := FCurInst.f + opAdjust;
            FCurInst.j := sym.Value and $7;
        end else
        begin
            if (FSymbols.TryGetValue(token, sym)) then
            begin
                if (sym.DefCount < 1) then
                    raise Exception.CreateFmt('%s is undefined', [token]);
                FCurInst.j := sym.Value;
            end else if (token <> '') then
                raise Exception.CreateFmt
                  ('%s is not a valid j designator', [token]);
        end;
        //
        FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, FCurInst.Value);
        FListFile.Value := FCurInst.Value;
        FListFile.Print;
    end;
    Inc(FLocationCounter.Value);
end;

procedure TAssembler.DoENTRY(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    token: AnsiString;
    sym: TSymbol;
begin
    if (not Assigned(FStmtLabel)) then
        raise Exception.Create('ENTRY directive must have a label');

    if (FPass = 2) then
    begin
        sym := nil;
        GetToken(ops, token);
        if (token = '.') then
        begin
            GetToken(ops, token);
            if (Copy(token, 1, 4) = 'STOP') then
            begin
                if (not FSymbols.TryGetValue('JP$' + token, sym)) then
                    raise Exception.CreateFmt('%s not valid for ENTRY',
                      [token]);
            end else
                raise Exception.CreateFmt('%s not valid for ENTRY', [token]);
        end;
        FEntryLabel := FStmtLabel;
        FEntryJ := 0;
        FCurInst.Value := 0;
        if (Assigned(sym)) then
        begin
            FCurInst.f := 49; // JP instruction
            FCurInst.j := sym.Value; // with approp. j designator
            FEntryJ := sym.Value;
        end;
        FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, FCurInst.Value);
        FListFile.Value := FCurInst.Value;
        FListFile.Print;
    end;

    Inc(FLocationCounter.Value);
end;

procedure TAssembler.DoEQUALS(lineNum: Integer; ops: AnsiString; op: TOpcode);
// Implmented as defined in the 1230 programmer reference, which is a
// superset of the EQUALS directive for 490 SPURT. Relocatability of
// result is as defined for 490 SPURT since the 1230 assembler seems to
// treat everything as absolute.
var
    rslt: Integer;
    rel: Boolean;

    function IsOperator(c: AnsiChar): Boolean;
    begin
        Result := (c = '+') or (c = '-') or (c = '/') or (c = '(') or (c = ')');
    end;

    function GetToken: AnsiString;
    var
        i: Integer;
    begin
        Result := '';
        if (ops = '') then
            Exit;

        if (ops[1] = '(') then // beginning of multiplicand / multiplier
        begin
            Result := '(';
            ops := Copy(ops, 2);
        end;

        i := 1;
        while ((i <= Length(ops)) and (not IsOperator(ops[i]))) do
        begin
            Result := Result + ops[i];
            Inc(i);
        end;

        ops := Copy(ops, i);
    end;

    procedure Expression(var rslt: Integer; var Relocatable: Boolean);
    var
        token: AnsiString;
        op1, op2: Integer;
        op1Seen, op2Seen, hasMulDiv: Boolean;
        op: AnsiChar;
        rel: Boolean;
        sym: TSymbol;
    begin
        token := GetToken;
        hasMulDiv := False;
        rel := False;
        op := ' ';
        op1 := 0;
        op2 := 0;
        op1Seen := False;
        op2Seen := False;
        while (token <> '') do
        begin
            if (token = '(') then
            begin
                // Start of multiplication. (expr)(expr)
                Expression(op1, rel);
                if (GetToken <> ')') then
                    raise Exception.Create('Unbalanced parentheses');
                if (GetToken <> '(') then
                    raise Exception.Create('Missing multiplier');
                Expression(op2, rel);
                if (GetToken <> ')') then
                    raise Exception.Create('Unbalanced parentheses');
                op1 := op1 * op2;
                rel := False;
                hasMulDiv := True;
            end else if (IsOperator(token[1])) then
            begin
                if (op <> ' ') then
                    raise Exception.Create('Expression syntax error');
                op := token[1];
                if (op = '/') then
                begin
                    Expression(op2, rel);
                    if (op2 = 0) then
                        raise Exception.Create('Attempt to divide by zero');
                    op1 := op1 div op2;
                    rel := False;
                    hasMulDiv := True;
                end;
            end else if ((token[1] < '0') or (token[1] > '9')) then
            begin
                if (not FSymbols.TryGetValue(AdjustIdent(token), sym)) then
                    raise Exception.CreateFmt('%s is undefined', [token]);
                if (sym.DefCount < 1) then
                    raise Exception.CreateFmt('%s is undefined', [token]);
                sym.xref.Add(lineNum);
                rel := sym.Relocatable;
                if (op = ' ') then
                begin
                    if (op1Seen) then
                        raise Exception.Create('Expression syntax error');
                    op1 := sym.Value;
                    op1Seen := True;
                end else
                begin
                    if (op2Seen) then
                        raise Exception.Create('Expression syntax error');
                    op2 := sym.Value;
                    op2Seen := True;
                end;
            end else
            begin
                if (op = ' ') then
                begin
                    if (op1Seen) then
                        raise Exception.Create('Expression syntax error');
                    op1 := NumberToInt(String(token));
                    op1Seen := True;
                end else
                begin
                    if (op2Seen) then
                        raise Exception.Create('Expression syntax error');
                    op2 := NumberToInt(String(token));
                    op2Seen := True;
                end;
            end;
            if (op1Seen and op2Seen) then
            begin
                case op of
                  '+':
                    op1 := op1 + op2;
                  '-':
                    op2 := op1 - op2;
                end;
                op2Seen := False;
                op := ' ';
            end;
            token := GetToken;
        end;
        if ((op <> ' ') and (not op2Seen)) then
            raise Exception.Create('Expression syntax error');
        rslt := op1;
        if (hasMulDiv) then
            Relocatable := False
        else
            Relocatable := rel;
    end;

begin
    if (not Assigned(FStmtLabel)) then
        raise Exception.Create('EQUALS directive must have a label');

    if (FPass = 1) then
    begin
        Expression(rslt, rel);
        FStmtLabel.Value := rslt;
        FStmtLabel.Relocatable := rel;
    end else
    begin
        if (FStmtLabel.Value < 0) then
            FListFile.Value := Integer(FStmtLabel.Value) - 1
        else
            FListFile.Value := FStmtLabel.Value;
        FListFile.Print;
        Exit;
    end;

end;

procedure TAssembler.DoEXIT(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    op1: TOpcode;
    sym: TSymbol;
    token, s: AnsiString;
    opAdjust: Integer;
begin
    if (FPass = 2) then
    begin
        if (not Assigned(FEntryLabel)) then
            raise Exception.Create('EXIT without a preceeding ENTRY');

        op1 := FOpcodes.Items['JP'];
        FCurInst.Value := 0;
        FCurInst.f := op1.Opcode; // Emit a JP L() instruction
        if (FEntryJ = 0) then
            FCurInst.k := 1;
        FCurInst.y := FEntryLabel.Value;
        // Check for j designator
        GetToken(ops, token);
        s := AnsiString(Format('%s$%s', [op1.Mnemonic, token]));
        if (FSymbols.TryGetValue(s, sym)) then
        begin
            if (sym.DefCount < 1) then
                raise Exception.CreateFmt('%s is undefined', [token]);
            // See if opcode needs to be adjusted for this designator
            opAdjust := sym.Value shr 15;
            FCurInst.f := FCurInst.f + opAdjust;
            FCurInst.j := sym.Value and $7;
        end else
        begin
            if (FSymbols.TryGetValue(token, sym)) then
            begin
                if (sym.DefCount < 1) then
                    raise Exception.CreateFmt('%s is undefined', [token]);
                FCurInst.j := sym.Value;
            end else if (token <> '') then
                raise Exception.CreateFmt
                  ('%s is not a valid j designator', [token]);
        end;
        //
        FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, FCurInst.Value);
        FListFile.Value := FCurInst.Value;
        FListFile.Print;
    end;

    Inc(FLocationCounter.Value);
end;

procedure TAssembler.DoFD(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    token: AnsiString;
    len, i, bytes: Integer;
    word: UInt32;
begin
    GetToken(ops, token);
    GetNumber(token, len);
    if (len < 0) then
        raise Exception.Create('Illegal string size');
    GetToken(ops, token);
    if (token = '.') then
    begin
        ops := TrimRight(ops);
        if (len = 0) then
            len := (Length(ops) + 4) div 5;
        word := 0;
        i := 0;
        while ((len > 0) and (i < Length(ops))) do
        begin
            word := (word shl 6) or
              Byte(TCodeTranslator.AsciiToFieldata(ops[i + 1]));
            Inc(i);
            if ((i mod 5) = 0) then
            begin
                if (FPass = 2) then
                begin
                    FOutFile.EmitSingleWord(FLocationCounter.Value,
                      rtNone, word);
                    FListFile.Value := word;
                    FListFile.Print;
                    Inc(FLocationCounter.Value);
                    FListFile.InitLine(lineNum, FLocationCounter.Value, '');
                end else
                    Inc(FLocationCounter.Value);
                word := 0;
                Dec(len);
            end;
        end;
        bytes := i mod 5;
        if (bytes <> 0) then
        begin
            // Emit last word
            Dec(len);
            if (FPass = 2) then
            begin
                word := word shl (30 - (bytes * 6));
                FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, word);
                FListFile.Value := word;
                FListFile.Print;
                Inc(FLocationCounter.Value);
                FListFile.InitLine(lineNum, FLocationCounter.Value, '');
            end else
                Inc(FLocationCounter.Value);
        end;
        while (len > 0) do
        begin
            // Pad to end of specified number of words
            Dec(len);
            if (FPass = 2) then
            begin
                FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, 0);
                FListFile.Value := word;
                FListFile.Print;
                Inc(FLocationCounter.Value);
                FListFile.InitLine(lineNum, FLocationCounter.Value, '');
            end else
                Inc(FLocationCounter.Value);
        end;
    end else
    begin
        // No string given, just output the given # of words of zeros
        while (len > 0) do
        begin
            if (FPass = 2) then
            begin
                FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, 0);
                FListFile.Value := 0;
                FListFile.Print;
                Inc(FLocationCounter.Value);
                FListFile.InitLine(lineNum, FLocationCounter.Value, '');
            end else
                Inc(FLocationCounter.Value);
        end;
    end;
end;

procedure TAssembler.DoGeneral(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    token, token1, pfx, s: AnsiString;
    sym: TSymbol;
    rel: TRelocatableType;
    opAdjust, chan, chanLen: Integer;
    b: Byte;
begin
    rel := rtNone;
    GetToken(ops, token);
    // Check for k specifier
    case op.InstType of
      itRead:
        pfx := 'R$';
      itStore:
        pfx := 'S$';
      itReplace:
        pfx := 'RP$';
      itIO:
        pfx := 'I$';
    else
        pfx := '';
    end;
    if ((pfx <> '') and (FSymbols.TryGetValue(pfx + token, sym))) then
    begin
        if (sym.DefCount < 1) then
            raise Exception.CreateFmt('%s is undefined', [token]);
        FCurInst.k := sym.Value;
        GetToken(ops, token);
    end else if ((op.InstType = itRead) and (FirstChar(token) = 'X')) then
    begin
        FCurInst.k := 4;
        token := Copy(token, 2);
    end;
    ops := token + ops;
    // k specifier, if any, processed, evaluate address expression
    FCurInst.y := GetY(lineNum, ops, rel, b);
    FCurInst.b := b;
    // check for j designator
    GetToken(ops, token);
    if ((token = ')') or (token = '.') or (token = ').')) then
        GetToken(ops, token);
    token := AdjustIdent(token);
    // This bit of cruft checks to see if we are trying to change a JP instruction into
    // a jump on buffer active instruction.
    chan := -1;
    if ((op.Mnemonic = 'JP') and (token <> '')) then
    begin
        // Check for special ACTIVEIN AND ACTIVEOUT j designators for JP instruction
        if (FSymbols.TryGetValue(token, sym)) then
        begin
            // perform MEANS substitution
            if (sym.SubstituteValue <> '') then
            begin
                token := AdjustIdent(sym.SubstituteValue);
                sym.xref.Add(lineNum);
            end;
        end;
        // Is token possibly a channel identifier of some kind?
        if ((token[1] = 'C') or (token[1] = 'F')) then
        begin
            chanLen := Pos('ACTIVEIN', String(token)) - 2;
            if (chanLen < 0) then
            begin
                chanLen := Pos('ACTIVEOUT', String(token)) - 2;
                if (chanLen < 0) then
                    chanLen := Length(token) - 1;
            end;
            try
                chan := Octal(Copy(String(token), 2, chanLen));
                if ((chan >= 0) and (chan <= 15)) then
                begin
                    token1 := Copy(token, chanLen + 2);
                    if (token1 = '') then
                    begin
                        GetToken(ops, token1);
                        if (token1 = '.') then
                            GetToken(ops, token1);
                        token1 := AdjustIdent(token1);
                        if ((token1 = 'ACTIVEIN') or (token1 = 'ACTIVEOUT'))
                        then
                        begin
                            token := token1;
                        end else if (token1 = 'COMACTIVE') then
                        begin
                            op := FOpcodes.Items['STR'];
                            FCurInst.f := op.Opcode;
                            FCurInst.jhat := chan;
                            token := '';
                        end;
                    end else
                        token := token1;
                end else
                    chan := -1;
            except
                ;
            end;
        end;
    end;
    //
    s := AnsiString(Format('%s$%s', [op.Mnemonic, token]));
    if (FSymbols.TryGetValue(s, sym)) then
    begin
        if (sym.DefCount < 1) then
            raise Exception.CreateFmt('%s is undefined', [token]);
        // See if opcode needs to be adjusted for this designator
        opAdjust := sym.Value shr 15;
        FCurInst.f := FCurInst.f + opAdjust;
        //
        if (chan <> -1) then
            FCurInst.jhat := chan
        else
            FCurInst.j := sym.Value and $7;
    end else
    begin
        if (FSymbols.TryGetValue(token, sym)) then
        begin
            if (sym.DefCount < 1) then
                raise Exception.CreateFmt('%s is undefined', [token]);
            FCurInst.j := sym.Value
        end else if (token <> '') then
            raise Exception.CreateFmt('%s is not a valid j designator',
              [token]);
    end;
    // process psuedo ops
    if (op.Mnemonic = 'CP.Q') then
    begin
        FCurInst.k := 0;
    end else if (op.Mnemonic = 'CP') then
    begin
        FCurInst.k := 4;
    end else if (op.Mnemonic = 'CL') then
    begin
        FCurInst.j := 0;
    end else if (op.Mnemonic = 'CL.A') then
    begin
        FCurInst.b := 0;
        FCurInst.j := 0;
        FCurInst.k := 7;
        FCurInst.y := 0;
    end else if (op.Mnemonic = 'CL.B') then
    begin
        FCurInst.k := 0;
        FCurInst.b := 0;
        FCurInst.y := 0;
        rel := rtNone;
    end else if (op.Mnemonic = 'CL.Q') then
    begin
        FCurInst.j := 0;
        FCurInst.k := 0;
        FCurInst.b := 0;
        FCurInst.y := 0;
        rel := rtNone;
    end else if (op.Mnemonic = 'NO.OP') then
    begin
        FCurInst.j := 0;
        FCurInst.k := 0;
        FCurInst.b := 0;
        FCurInst.y := 0;
        rel := rtNone;
    end else if (op.Mnemonic = 'RILJP') then
    begin
        FCurInst.j := 1;
    end else if (op.Mnemonic = 'SQRT') then
    begin
        FCurInst.k := 7;
    end;
    FOutFile.EmitSingleWord(FLocationCounter.Value, rel, FCurInst.Value);
    FListFile.Value := FCurInst.Value;
    FListFile.Print;
end;

procedure TAssembler.DoINCREMENT(lineNum: Integer; ops: AnsiString;
  op: TOpcode);
var
    token: AnsiString;
    ksym, bsym: TSymbol;
    y: Integer;
    b: Byte;
    rel: TRelocatableType;
    op1: TOpcode;
begin
    bsym := nil;
    ksym := nil;
    // get b register id
    GetToken(ops, token);
    if (not FSymbols.TryGetValue(AdjustIdent(token), bsym)) then
        raise Exception.CreateFmt('%s is undefined', [token]);
    if (bsym.DefCount < 1) then
        raise Exception.CreateFmt('%s is undefined', [token]);
    // get dot separator
    GetToken(ops, token);
    if (token <> '.') then
        raise Exception.Create('INCREMENT directive requires 2 operands');
    // get possible k designator
    GetToken(ops, token);
    if (Pos('(', String(token)) = 0) or
      (not FSymbols.TryGetValue(AdjustIdent('R$' + token), ksym)) then
        ops := token + ops;
    y := GetY(lineNum, ops, rel, b);
    if ((b = 0) and (not Assigned(ksym))) then
    begin
        if (y = -1) then
        begin
            if (FPass = 2) then
            begin
                op1 := FOpcodes.Items['BJP.B'];
                FCurInst.Value := 0;
                FCurInst.f := op1.Opcode;
                FCurInst.j := bsym.Value;
                FCurInst.y := FLocationCounter.Value + 1;
                FOutFile.EmitSingleWord(FLocationCounter.Value, rel,
                  FCurInst.Value);
                FListFile.Value := FCurInst.Value;
                FListFile.Print;
            end;
            Inc(FLocationCounter.Value);
        end else if (y < -1) then
        begin
            if (FPass = 2) then
            begin
                op1 := FOpcodes.Items['ENT.A'];
                FCurInst.Value := 0;
                FCurInst.f := op1.Opcode;
                FCurInst.b := bsym.Value;
                FOutFile.EmitSingleWord(FLocationCounter.Value, rel,
                  FCurInst.Value);
                FListFile.Value := FCurInst.Value;
                FListFile.Print;
                Inc(FLocationCounter.Value);
                FListFile.InitLine(lineNum, FLocationCounter.Value, '');
                op1 := FOpcodes.Items['ADD.A'];
                FCurInst.Value := 0;
                FCurInst.f := op1.Opcode;
                FCurInst.k := 4;
                FCurInst.y := y;
                FOutFile.EmitSingleWord(FLocationCounter.Value, rel,
                  FCurInst.Value);
                FListFile.Value := FCurInst.Value;
                FListFile.Print;
                Inc(FLocationCounter.Value);
                FListFile.InitLine(lineNum, FLocationCounter.Value, '');
                op1 := FOpcodes.Items['ENT.B'];
                FCurInst.Value := 0;
                FCurInst.f := op1.Opcode;
                FCurInst.j := bsym.Value;
                FCurInst.k := 7;
                FOutFile.EmitSingleWord(FLocationCounter.Value, rel,
                  FCurInst.Value);
                FListFile.Value := FCurInst.Value;
                FListFile.Print;
                Inc(FLocationCounter.Value);
            end else
                Inc(FLocationCounter.Value, 3);
        end else if (y > 0) then
        begin
            if (FPass = 2) then
            begin
                op1 := FOpcodes.Items['ENT.B'];
                FCurInst.Value := 0;
                FCurInst.f := op1.Opcode;
                FCurInst.b := bsym.Value;
                FCurInst.j := bsym.Value;
                FCurInst.y := y;
                FOutFile.EmitSingleWord(FLocationCounter.Value, rel,
                  FCurInst.Value);
                FListFile.Value := FCurInst.Value;
                FListFile.Print;
            end;
            Inc(FLocationCounter.Value);
        end;
    end else
    begin
        if (FPass = 2) then
        begin
            op1 := FOpcodes.Items['ENT.A'];
            FCurInst.Value := 0;
            FCurInst.f := op1.Opcode;
            FCurInst.b := bsym.Value;
            FOutFile.EmitSingleWord(FLocationCounter.Value, rel,
              FCurInst.Value);
            FListFile.Value := FCurInst.Value;
            FListFile.Print;
            Inc(FLocationCounter.Value);
            FListFile.InitLine(lineNum, FLocationCounter.Value, '');
            op1 := FOpcodes.Items['ADD.A'];
            FCurInst.Value := 0;
            FCurInst.f := op1.Opcode;
            FCurInst.k := ksym.Value;
            // Force sign extension
            if ((FCurInst.k = 1) or (FCurInst.k = 2)) then
                FCurInst.k := FCurInst.k + 4;
            FCurInst.y := y;
            FOutFile.EmitSingleWord(FLocationCounter.Value, rel,
              FCurInst.Value);
            FListFile.Value := FCurInst.Value;
            FListFile.Print;
            Inc(FLocationCounter.Value);
            FListFile.InitLine(lineNum, FLocationCounter.Value, '');
            op1 := FOpcodes.Items['ENT.B'];
            FCurInst.Value := 0;
            FCurInst.f := op1.Opcode;
            FCurInst.j := bsym.Value;
            FCurInst.k := 7;
            FOutFile.EmitSingleWord(FLocationCounter.Value, rel,
              FCurInst.Value);
            FListFile.Value := FCurInst.Value;
            FListFile.Print;
            Inc(FLocationCounter.Value);
        end else
            Inc(FLocationCounter.Value, 3);
    end;
end;

procedure TAssembler.DoINDRALLOC(lineNum: Integer; ops: AnsiString;
  op: TOpcode);
begin
    FAllocationType := atIndirect;
    if (FPass = 1) then
    begin
        if (Assigned(FStmtLabel)) then
            Dec(FStmtLabel.DefCount);
    end else
        FListFile.Print;
end;

procedure TAssembler.DoIO(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    token: AnsiString;
    sym: TSymbol;
    rel: TRelocatableType;
    b: Byte;
    op1: TOpcode;
begin
    rel := rtNone;
    // Check for channel identifier
    GetToken(ops, token);
    if (token = '') then
        raise Exception.Create('Channel number expected');
    if (FSymbols.TryGetValue(AdjustIdent(token), sym)) then
    begin
        if ((sym.SymbolType <> stSystem) and (sym.SubstituteValue <> '')) then
        begin
            token := sym.SubstituteValue;
            sym.xref.Add(lineNum);
        end;
    end;
    if ((not FSymbols.TryGetValue(token, sym)) or (sym.Value < 0) or
      (sym.Value > 15)) then
        raise Exception.CreateFmt
          ('%s is not a valid channel identifier', [token]);
    FCurInst.jhat := sym.Value;
    GetToken(ops, token);
    if (token <> '.') then
        raise Exception.Create('Syntax error');
    // Check for k specifier
    GetToken(ops, token);
    if (FSymbols.TryGetValue(AnsiString(op.Mnemonic + '$' + String(token)), sym))
    then
    begin
        if (sym.DefCount < 1) then
            raise Exception.CreateFmt('%s is undefined', [token]);
        FCurInst.khat := sym.Value;
    end else if (FSymbols.TryGetValue('I$' + token, sym)) then
    begin
        if (sym.DefCount < 1) then
            raise Exception.CreateFmt('%s is undefined', [token]);
        FCurInst.khat := sym.Value;
    end else if (token <> '') then
        raise Exception.Create('Invalid k designator');
    //
    FCurInst.y := GetY(lineNum, ops, rel, b);
    FCurInst.b := b;
    // Get the modifier if any
    GetToken(ops, token);
    if (token = '.') then
        GetToken(ops, token);
    // Special code for some instructions
    if ((op.Mnemonic = 'EX-COM') or (op.Mnemonic = 'EX-FCT')) then
    begin
        if ((not FSymbols.TryGetValue(AnsiString(op.Mnemonic + '$' +
          String(token)), sym)) or (sym.DefCount = 0)) then
            raise Exception.CreateFmt('%s is undefined', [token]);
        FCurInst.khat := sym.Value;
    end else if ((op.Mnemonic = 'EX-COM-MW') and (token = 'MONITOR')) then
    begin
        op1 := FOpcodes.Items['OUT.MON'];
        FCurInst.f := op1.Opcode;
    end else if ((op.Mnemonic = 'IN') and (token = 'MONITOR')) then
    begin
        op1 := FOpcodes.Items['IN.MON'];
        FCurInst.f := op1.Opcode;
    end else if ((op.Mnemonic = 'OUT') and (token = 'MONITOR')) then
    begin
        op1 := FOpcodes.Items['OUT.MON'];
        FCurInst.f := op1.Opcode;
    end;
    //
    FOutFile.EmitSingleWord(FLocationCounter.Value, rel, FCurInst.Value);
    FListFile.Value := FCurInst.Value;
    FListFile.Print;
end;

procedure TAssembler.DoLOCDD(lineNum: Integer; ops: AnsiString; op: TOpcode);
begin
    if (FPass = 2) then
        FListFile.Print;
end;

procedure TAssembler.DoMEANS(lineNum: Integer; ops: AnsiString; op: TOpcode);
begin
    if (not Assigned(FStmtLabel)) then
        raise Exception.Create('MEANS directive must have a label');

    if (FPass = 1) then
    begin
        GetChannel(ops, FStmtLabel.SubstituteValue);
        // A label can be defined by either MEANS or another method. This results
        // in "multiple definition" errors unless we reduce the DecCount here.
        Dec(FStmtLabel.DefCount);
    end else
        FListFile.Print;
end;

procedure TAssembler.DoMOVE(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    fields: TStringList;
    op1: TOpcode;
    token, operand: AnsiString;
    ksym: TSymbol;
    rel: TRelocatableType;
    b: Byte;
begin
    { TODO : This needs work to be able to handle operands which specify an index register }
    if (FPass = 1) then
    begin
        Inc(FLocationCounter.Value, 4);
        Exit;
    end;

    fields := TSpurtStringList.Create;
    try
        // Separate operands
        fields.DelimitedText := String(ops);
        if (fields.Count <> 3) then
            raise Exception.Create('MOVE directive requires 3 operands');
        // Initialize the B register
        op1 := FOpcodes.Items['ENT.B'];
        FCurInst.Value := 0;
        FCurInst.f := op1.Opcode;
        FCurInst.j := 7;
        // get possible k designator
        ksym := nil;
        operand := AnsiString(fields[0]);
        GetToken(operand, token);
        if (Pos('(', String(token)) = 0) or
          (not FSymbols.TryGetValue(AdjustIdent('R$' + token), ksym)) then
            operand := token + operand;
        if (Assigned(ksym)) then
            FCurInst.k := ksym.Value;
        FCurInst.y := GetY(lineNum, operand, rel, b) - 1;
        FCurInst.b := b;
        FOutFile.EmitSingleWord(FLocationCounter.Value, rel, FCurInst.Value);
        FListFile.Value := FCurInst.Value;
        FListFile.Print;
        Inc(FLocationCounter.Value);
        // Load value to be moved
        op1 := FOpcodes.Items['ENT.Q'];
        FCurInst.Value := 0;
        FCurInst.f := op1.Opcode;
        FCurInst.k := 3;
        operand := AnsiString(fields[1]);
        FCurInst.y := GetY(lineNum, operand, rel, b);
        FCurInst.b := 7;
        FOutFile.EmitSingleWord(FLocationCounter.Value, rel, FCurInst.Value);
        FListFile.InitLine(lineNum, FLocationCounter.Value, '');
        FListFile.Value := FCurInst.Value;
        FListFile.Print;
        Inc(FLocationCounter.Value);
        // Store value to be moved
        op1 := FOpcodes.Items['STR.Q'];
        FCurInst.Value := 0;
        FCurInst.f := op1.Opcode;
        FCurInst.k := 3;
        operand := AnsiString(fields[2]);
        FCurInst.y := GetY(lineNum, operand, rel, b);
        FCurInst.b := 7;
        FOutFile.EmitSingleWord(FLocationCounter.Value, rel, FCurInst.Value);
        FListFile.InitLine(lineNum, FLocationCounter.Value, '');
        FListFile.Value := FCurInst.Value;
        FListFile.Print;
        Inc(FLocationCounter.Value);
        // Decrement loop count
        op1 := FOpcodes.Items['BJP.B'];
        FCurInst.Value := 0;
        FCurInst.f := op1.Opcode;
        FCurInst.j := 7;
        FCurInst.y := FLocationCounter.Value - 2;
        FOutFile.EmitSingleWord(FLocationCounter.Value, rel, FCurInst.Value);
        FListFile.InitLine(lineNum, FLocationCounter.Value, '');
        FListFile.Value := FCurInst.Value;
        FListFile.Print;
        Inc(FLocationCounter.Value);
    finally
        fields.Free;
    end;
end;

procedure TAssembler.DoORG(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    addr: Integer;
begin
    GetNumber(ops, addr);
    FLocationCounter.Value := addr;
    if (FPass = 2) then
    begin
        if (not FTransferAddrEmitted) then
        begin
            FOutFile.EmitTransferAddr(FLocationCounter.Value, FTransferAddr,
              FObjCodeSize);
            FTransferAddrEmitted := True;
        end;
        FListFile.Print;
    end;
end;

procedure TAssembler.DoOUTPUTS(lineNum: Integer; ops: AnsiString; op: TOpcode);
begin
    if (FPass = 2) then
    begin
        FListFile.Print;
        FListFile.Print('**** Warning: Not implemented');
    end;
end;

procedure TAssembler.DoPROCEDURE(lineNum: Integer; ops: AnsiString;
  op: TOpcode);
// Due to a lack of documentation this is only partially implemented
var
    token: AnsiString;
    sym: TSymbol;
begin
    GetToken(ops, token);
    if (token = '') then
        raise Exception.Create('PROCEDURE directive requires an identifier');
    if (FSymbols.TryGetValue(AdjustIdent(token), sym)) then
    begin
        if (FPass = 1) then
            raise Exception.CreateFmt('%s is defined multiple times', [token]);
    end else
    begin
        sym := TSymbol.Create;
        sym.id := AdjustIdent(token);
        sym.SourceID := token;
        sym.SymbolType := stProcedure;
        sym.DefLine := lineNum;
        sym.Value := FLocationCounter.Value;
        sym.Relocatable := True;
        sym.DefCount := 1;
        FSymbols.Add(AdjustIdent(token), sym);
    end;
    FEntryLabel := sym;

    if (FPass = 2) then
    begin
        FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, 0);
        FListFile.Value := 0;
        FListFile.Print;
    end;

    Inc(FLocationCounter.Value);
end;

procedure TAssembler.DoPROGRAM(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    p: TProgram;
begin
    FAllocationType := atNone;
    if (FPass = 1) then
    begin
        if (Assigned(FStmtLabel)) then
        begin
            Dec(FStmtLabel.DefCount);
            FStmtLabel.DefLine := 0;
            if (not Assigned(FPrograms.Programs[FStmtLabel.id])) then
            begin
                p := TProgram.Create;
                p.ProgramName := FStmtLabel.id;
                p.StartAddr := FLocationCounter.Value;
                FPrograms.Add(p);
                FCrntProgram := p;
                if (FPrograms.Count = 1) then
                    FTransferAddr := FLocationCounter.Value;
            end;
        end;
    end else
    begin
        if (Assigned(FStmtLabel)) then
        begin
            p := FPrograms.Programs[FStmtLabel.id];
            if (not p.TransferAddrEmitted) then
            begin
                FOutFile.EmitTransferAddr(p.StartAddr, FTransferAddr,
                  p.EndAddr - p.StartAddr);
                p.TransferAddrEmitted := True;
            end;
        end;
        FListFile.Print;
    end;
end;

procedure TAssembler.DoPUT(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    op1: TOpcode;
    fields: TSpurtStringList;
begin
    if (FPass = 1) then
    begin
        Inc(FLocationCounter.Value, 2);
    end else
    begin
        fields := TSpurtStringList.Create;
        try
            // separate ops into source and destination operand
            fields.DelimitedText := String(ops);
            if (fields.Count <> 2) then
                raise Exception.Create('PUT directive requires 2 operands');
            op1 := FOpcodes.Items['ENT.Q'];
            FCurInst.Value := 0;
            FCurInst.f := op1.Opcode;
            DoGeneral(lineNum, AnsiString(fields[0]), op1);
            Inc(FLocationCounter.Value);
            FListFile.InitLine(lineNum, FLocationCounter.Value, '');
            op1 := FOpcodes.Items['STR.Q'];
            FCurInst.Value := 0;
            FCurInst.f := op1.Opcode;
            DoGeneral(lineNum, AnsiString(fields[1]), op1);
            Inc(FLocationCounter.Value);
        finally
            fields.Free;
        end;
    end;
end;

procedure TAssembler.DoRELALLOC(lineNum: Integer; ops: AnsiString; op: TOpcode);
begin
    FAllocationType := atRelative;
    if (FPass = 1) then
    begin
        if (Assigned(FStmtLabel)) then
            Dec(FStmtLabel.DefCount);
        { TODO : request allocation base value from console }
    end else
        FListFile.Print;
end;

procedure TAssembler.DoRESERVE(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    token: AnsiString;
    val: Integer;
    rel: Boolean;

    procedure GetValue(op: String; var rel: Boolean; var val: Integer);
    var
        c: Char;
        sym: TSymbol;
    begin
        c := FirstChar(op);
        if ((c = '+') or (c = '-') or ((c >= '0') and (c <= '9'))) then
        begin
            val := NumberToInt(op);
            rel := False;
        end else
        begin
            if (not FSymbols.TryGetValue(AdjustIdent(AnsiString(op)), sym)) then
                raise Exception.CreateFmt('%s is undefined', [op]);
            if (sym.DefCount < 1) then
                raise Exception.CreateFmt('%s is undefined', [op]);
            sym.xref.Add(lineNum);
            val := sym.Value;
            rel := sym.Relocatable;
        end;
    end;

begin
    GetToken(ops, token);
    if (token = '') then
        raise Exception.Create('RESERVE requires an operand');
    GetValue(String(token), rel, val);
    if (FPass = 1) then
    begin
        Inc(FLocationCounter.Value, val);
    end else
    begin
        while (val > 0) do
        begin
            FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, 0);
            Inc(FLocationCounter.Value);
            Dec(val);
        end;
        FListFile.Print;
    end;
end;

procedure TAssembler.DoRIL(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    token: AnsiString;
    op1: TOpcode;
begin
    if (FPass = 2) then
    begin
        GetToken(ops, token);
        if (token = '.') then
            GetToken(ops, token);
        if (token = '') then
        begin
            op1 := FOpcodes.Items['JP'];
            FCurInst.f := op1.Opcode;
        end else if (token = 'ALL') then
        begin
            op1 := FOpcodes.Items['TERM.IN'];
            FCurInst.f := op1.Opcode;
            FCurInst.khat := 1;
        end else
            raise Exception.Create('Illegal opcode');
        FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, FCurInst.Value);
        FListFile.Value := FCurInst.Value;
        FListFile.Print;
    end;
    Inc(FLocationCounter.Value);
end;

procedure TAssembler.DoRILEX(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    token: AnsiString;
    sym: TSymbol;
    op1: TOpcode;
begin
    if (FPass = 1) then
    begin
        Inc(FLocationCounter.Value, 1);
        Exit;
    end;
    // Check for channel identifier
    GetToken(ops, token);
    if (token = '') then
        raise Exception.Create('Channel number expected');

    op1 := FOpcodes.Items['TERM.IN'];
    FCurInst.f := op1.Opcode;
    if (token = 'ALL') then
    begin
        // lock out external interrupts on all channels
        FCurInst.khat := 2;
        FCurInst.b := 0;
    end else
    begin
        if (FSymbols.TryGetValue(AdjustIdent(token), sym)) then
        begin
            if ((sym.SymbolType <> stSystem) and (sym.SubstituteValue <> ''))
            then
            begin
                token := sym.SubstituteValue;
                sym.xref.Add(lineNum);
            end;
        end;
        if ((not FSymbols.TryGetValue(token, sym)) or (sym.Value < 0) or
          (sym.Value > 15)) then
            raise Exception.CreateFmt
              ('%s is not a valid channel identifier', [token]);
        FCurInst.jhat := sym.Value;
        FCurInst.khat := 3;
        FCurInst.b := 0;
    end;
    FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, FCurInst.Value);
    FListFile.Value := FCurInst.Value;
    FListFile.Print;
    Inc(FLocationCounter.Value, 1);
end;

procedure TAssembler.DoSIL(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    op1: TOpcode;
begin
    if (FPass = 2) then
    begin
        op1 := FOpcodes.Items['TERM.IN'];
        FCurInst.f := op1.Opcode;
        FCurInst.khat := 1;
        FCurInst.b := 1;
        FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, FCurInst.Value);
        FListFile.Value := FCurInst.Value;
        FListFile.Print;
    end;
    Inc(FLocationCounter.Value);
end;

procedure TAssembler.DoSILEX(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    token: AnsiString;
    sym: TSymbol;
    op1: TOpcode;
begin
    if (FPass = 1) then
    begin
        Inc(FLocationCounter.Value, 1);
        Exit;
    end;
    // Check for channel identifier
    GetToken(ops, token);
    if (token = '') then
        raise Exception.Create('Channel number expected');

    op1 := FOpcodes.Items['TERM.IN'];
    FCurInst.f := op1.Opcode;
    if (token = 'ALL') then
    begin
        // lock out external interrupts on all channels
        FCurInst.khat := 2;
        FCurInst.b := 1;
    end else
    begin
        if (FSymbols.TryGetValue(AdjustIdent(token), sym)) then
        begin
            if ((sym.SymbolType <> stSystem) and (sym.SubstituteValue <> ''))
            then
            begin
                token := sym.SubstituteValue;
                sym.xref.Add(lineNum);
            end;
        end;
        if ((not FSymbols.TryGetValue(token, sym)) or (sym.Value < 0) or
          (sym.Value > 15)) then
            raise Exception.CreateFmt
              ('%s is not a valid channel identifier', [token]);
        FCurInst.jhat := sym.Value;
        FCurInst.khat := 3;
        FCurInst.b := 1;
    end;
    FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, FCurInst.Value);
    FListFile.Value := FCurInst.Value;
    FListFile.Print;
    Inc(FLocationCounter.Value, 1);
end;

procedure TAssembler.DoTERM(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    token: AnsiString;
    sym: TSymbol;
    op1: TOpcode;
begin
    if (FPass = 1) then
    begin
        Inc(FLocationCounter.Value, 1);
        Exit;
    end;
    // Check for channel identifier
    GetToken(ops, token);
    if (token = '') then
        raise Exception.Create('Channel number expected');
    if (token = 'ALL') then
    begin
        // terminate input and output on all channels
        op1 := FOpcodes.Items['TERM.OUT'];
        FCurInst.f := op1.Opcode;
        FCurInst.khat := 2;
    end else
    begin
        if (FSymbols.TryGetValue(AdjustIdent(token), sym)) then
        begin
            if ((sym.SymbolType <> stSystem) and (sym.SubstituteValue <> ''))
            then
            begin
                token := sym.SubstituteValue;
                sym.xref.Add(lineNum);
            end;
        end;
        if ((not FSymbols.TryGetValue(token, sym)) or (sym.Value < 0) or
          (sym.Value > 15)) then
            raise Exception.CreateFmt
              ('%s is not a valid channel identifier', [token]);
        FCurInst.jhat := sym.Value;
        // get the buffer mode
        GetToken(ops, token);
        if (token = '.') then
            GetToken(ops, token);
        if (token = 'COM') then
        begin
            op1 := FOpcodes.Items['TERM.OUT'];
            FCurInst.f := op1.Opcode;
            FCurInst.khat := 1;
        end else if (token = 'INPUT') then
        begin
            op1 := FOpcodes.Items['TERM.IN'];
            FCurInst.f := op1.Opcode;
        end else if (token = 'OUTPUT') then
        begin
            op1 := FOpcodes.Items['TERM.OUT'];
            FCurInst.f := op1.Opcode;
        end else
            raise Exception.Create('Invalid buffer mode');
    end;
    FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, FCurInst.Value);
    FListFile.Value := FCurInst.Value;
    FListFile.Print;
    Inc(FLocationCounter.Value, 1);
end;

procedure TAssembler.DoTYPEDEC(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    token, stemp: AnsiString;
    opcode: TOpcode;
    b: Integer;
    fields: TSpurtStringList;
begin
    FIOLibRequired := True;
    fields := TSpurtStringList.Create;
    try
        fields.DelimitedText := String(ops);
        // Save A in case we need it later
        opcode := FOpcodes['STR.A'];
        FCurInst.Value := 0;
        FCurInst.f := opcode.Opcode;
        FCurInst.k := 3;
        FCurInst.y := TYPEA_ENTRY;
        if (FPass = 2) then
        begin
            FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, FCurInst.Value);
            FListFile.Value := FCurInst.Value;
            FListFile.Print;
            FListFile.InitLine(lineNum, FLocationCounter.Value + 1, '');
        end;
        Inc(FLocationCounter.Value);

        while (fields.Count > 0) do
        begin
            stemp := AnsiString(fields[0]);
            GetToken(AnsiString(stemp), token);
            if (token = 'A') then
            begin
                Opcode := FOpcodes['ENT.A'];
                FCurInst.Value := 0;
                FCurInst.f := Opcode.Opcode;
                FCurInst.k := 3;
                FCurInst.y := TYPEA_ENTRY;
                if (FPass = 2) then
                begin
                    FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, FCurInst.Value);
                    FListFile.Value := FCurInst.Value;
                    FListFile.Print;
                    FListFile.InitLine(lineNum, FLocationCounter.Value + 1, '');
                end;
                Inc(FLocationCounter.Value);
                CallTypeDec(lineNum);
            end else if (token = 'Q') then
            begin
                Opcode := FOpcodes['STR.Q'];
                FCurInst.Value := 0;
                FCurInst.f := Opcode.Opcode;
                FCurInst.k := 4;
                if (FPass = 2) then
                begin
                    FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, FCurInst.Value);
                    FListFile.Value := FCurInst.Value;
                    FListFile.Print;
                    FListFile.InitLine(lineNum, FLocationCounter.Value + 1, '');
                end;
                Inc(FLocationCounter.Value);
                CallTypeDec(lineNum);
            end else if ((token[1] = 'B') and TryStrToInt(Copy(String(token), 2), b) and
              (b >= 1) and (b <= 7)) then
            begin
                Opcode := FOpcodes['STR.B'];
                FCurInst.Value := 0;
                FCurInst.f := Opcode.Opcode;
                FCurInst.j := b;
                FCurInst.k := 7;
                if (FPass = 2) then
                begin
                    FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, FCurInst.Value);
                    FListFile.Value := FCurInst.Value;
                    FListFile.Print;
                    FListFile.InitLine(lineNum, FLocationCounter.Value + 1, '');
                end;
                Inc(FLocationCounter.Value);
                CallTypeDec(lineNum);
            end else if (token = FIOSeparator + AnsiString('CR') + FIOSeparator) then
            begin
                CallTypeCR(lineNum);
            end else if (token = FIOSeparator + AnsiString('SP') + FIOSeparator) then
            begin
                CallTypeSP(lineNum);
            end else if (token = FIOSeparator + AnsiString('TAB') + FIOSeparator) then
            begin
                CallTypeTAB(lineNum);
            end else
            begin
                stemp := token + stemp;
                opcode := FOpcodes.Items['ENT.A'];
                FCurInst.Value := 0;
                FCurInst.f := opcode.Opcode;
                if (FPass = 2) then
                begin
                    DoGeneral(lineNum, stemp, opcode);
                    FListFile.InitLine(lineNum, FLocationCounter.Value, '');
                end;
                CallTypeDec(lineNum);
                Inc(FLocationCounter.Value);
            end;
            fields.Delete(0);
        end;
        // Restore A
        Opcode := FOpcodes['ENT.A'];
        FCurInst.Value := 0;
        FCurInst.f := Opcode.Opcode;
        FCurInst.k := 3;
        FCurInst.y := TYPEA_ENTRY;
        if (FPass = 2) then
        begin
            FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, FCurInst.Value);
            FListFile.Value := FCurInst.Value;
            FListFile.Print;
            FListFile.InitLine(lineNum, FLocationCounter.Value + 1, '');
        end;
        Inc(FLocationCounter.Value);
    finally
        fields.Free;
    end;
end;

procedure TAssembler.DoTYPET(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    text, control: AnsiString;
    c: AnsiChar;
    sepSeen: Boolean;
    op1: TOpcode;
    word: UInt32;
    i, len, bytes: Integer;
begin
    FIOLibRequired := True;
    // Assemble the text to be printed.
    ops := TrimRight(ops);
    text := '';
    sepSeen := False;
    for c in ops do
    begin
        if (c = FIOSeparator) then
        begin
            if (sepSeen) then
            begin
                if (control = 'CR') then
                    text := text + #04#03
                else if (control = 'SP') then
                    text := text + #05
                else if (control = 'TAB') then
                    text := text + #05;
                sepSeen := False;
            end else
            begin
                sepSeen := True;
                control := '';
            end;

        end else
        begin
            if (sepSeen) then
                control := control + c
            else
                text := text + TCodeTranslator.AsciiToFieldata(c);
        end;
    end;
    text := text + #63;
    len := (Length(text) + 4) div 5;
    // Generate the code to print the text
    if (FPass = 2) then
    begin
        op1 := FOpcodes['RJP.MAN'];
        FCurInst.Value := 0;
        FCurInst.f := op1.Opcode;
        FCurInst.y := TYPET_ENTRY;
        FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, FCurInst.Value);
        FListFile.Value := FCurInst.Value;
        FListFile.Print;
        FListFile.InitLine(lineNum, FLocationCounter.Value + 1, '');

        FOutFile.EmitSingleWord(FLocationCounter.Value + 1, rtWord,
          FLocationCounter.Value + 3);
        FListFile.Value := FLocationCounter.Value + 3;
        FListFile.Print;
        FListFile.InitLine(lineNum, FLocationCounter.Value + 2, '');

        op1 := FOpcodes['JP.MAN'];
        FCurInst.Value := 0;
        FCurInst.f := op1.Opcode;
        FCurInst.y := FLocationCounter.Value + 3 + len;
        FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, FCurInst.Value);
        FListFile.Value := FCurInst.Value;
        FListFile.Print;
        FListFile.InitLine(lineNum, FLocationCounter.Value + 3, '');
    end;
    Inc(FLocationCounter.Value, 3);
    word := 0;
    i := 0;
    while ((len > 0) and (i < Length(text))) do
    begin
        word := (word shl 6) or Byte(text[i + 1]);
        Inc(i);
        if ((i mod 5) = 0) then
        begin
            if (FPass = 2) then
            begin
                FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, word);
                FListFile.Value := word;
                FListFile.Print;
                Inc(FLocationCounter.Value);
                FListFile.InitLine(lineNum, FLocationCounter.Value, '');
            end else
                Inc(FLocationCounter.Value);
            word := 0;
            Dec(len);
        end;
    end;
    bytes := i mod 5;
    if (bytes <> 0) then
    begin
        // Emit last word
        if (FPass = 2) then
        begin
            word := word shl (30 - (bytes * 6));
            FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, word);
            FListFile.Value := word;
            FListFile.Print;
            Inc(FLocationCounter.Value);
            FListFile.InitLine(lineNum, FLocationCounter.Value, '');
        end else
            Inc(FLocationCounter.Value);
    end;
end;

procedure TAssembler.DoUTAG(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    word, h1, h2: UInt32;
    b: Byte;
    rel1, rel2, rel: TRelocatableType;
begin
    if (FPass = 2) then
    begin
        h1 := GetY(lineNum, ops, rel1, b);
        if (b <> 0) then
            raise Exception.Create('B-register not allowed');
        h2 := GetY(lineNum, ops, rel2, b);
        if (b <> 0) then
            raise Exception.Create('B-register not allowed');
        if ((rel1 <> rtNone) and (rel2 <> rtNone)) then
            rel := rtH1H2
        else if (rel1 <> rtNone) then
            rel := rtH1
        else if (rel2 <> rtNone) then
            rel := rtH2
        else
            rel := rtNone;
        word := ((h1 and $7FFF) shl 15) or (h2 and $7FFF);
        FOutFile.EmitSingleWord(FLocationCounter.Value, rel, word);
        FListFile.Value := word;
        FListFile.Print;
    end;
    Inc(FLocationCounter.Value);
end;

procedure TAssembler.DoVRBL(lineNum: Integer; ops: AnsiString; op: TOpcode);
var
    token: AnsiString;
    sym: TSymbol;
    val: Integer;
begin
    GetToken(ops, token);
    if (token = '') then
        raise Exception.Create('Variable name expected, got ''''');
    if (FSymbols.TryGetValue(AdjustIdent(token), sym)) then
    begin
        if (FPass = 1) then
            raise Exception.CreateFmt('%s is multiply defined', [token]);
    end else
    begin
        sym := TSymbol.Create;
        sym.id := AdjustIdent(token);
        sym.SourceID := token;
        sym.SymbolType := stIdentifier;
        sym.DefLine := lineNum;
        sym.Value := FLocationCounter.Value;
        sym.Relocatable := True;
        sym.DefCount := 1;
        FSymbols.Add(AdjustIdent(token), sym);
    end;

    GetToken(ops, token);
    if (token = '.') then
        GetToken(ops, token);
    if ((token <> 'FXW') and (token <> 'FXH')) then
        raise Exception.CreateFmt('Expected FXW or FXH, got %s', [token]);

    GetToken(ops, token);
    if (token = '.') then
        GetToken(ops, token);
    GetNumber(token, val);
    if ((val < 0) or (val > 30)) then
        raise Exception.Create('Invalid scale');
    sym.Scale := val;

    if (FPass = 2) then
    begin
        FOutFile.EmitSingleWord(FLocationCounter.Value, rtNone, 0);
        FListFile.Value := 0;
        FListFile.Print;
    end;

    Inc(FLocationCounter.Value);
end;

procedure TAssembler.DoWord(lineNum: Integer; var ops: AnsiString);
var
    fields: TSpurtStringList;
    val, val2: Integer;
    rel, rel2: Boolean;
    relType: TRelocatableType;

    procedure GetValue(op: String; var rel: Boolean; var val: Integer);
    var
        c: Char;
        sym: TSymbol;
    begin
        c := FirstChar(op);
        if ((c = '+') or (c = '-') or ((c >= '0') and (c <= '9'))) then
        begin
            val := NumberToInt(op);
            rel := False;
        end else
        begin
            if (not FSymbols.TryGetValue(AdjustIdent(AnsiString(op)), sym)) then
                raise Exception.CreateFmt('%s is undefined', [op]);
            if (sym.DefCount < 1) then
                raise Exception.CreateFmt('%s is undefined', [op]);
            sym.xref.Add(lineNum);
            val := sym.Value;
            rel := sym.Relocatable;
        end;
    end;

begin
    if (FPass = 1) then
    begin
        Inc(FLocationCounter.Value);
        Exit;
    end;

    fields := TSpurtStringList.Create;
    try
        fields.DelimitedText := String(ops);
        if ((fields.Count <> 1) and (fields.Count <> 2)) then
            raise Exception.Create('Constants may have only 1 or 2 values');
        if (fields.Count = 1) then
        begin
            GetValue(fields[0], rel, val);
            if (rel) then
                relType := rtWord
            else
                relType := rtNone;
            fields.Delete(0);
            ops := AnsiString(fields.DelimitedText);
        end else
        begin
            if (fields[1] = '') then
            begin
                GetValue(fields[0], rel, val);
                if (rel) then
                    relType := rtWord
                else
                    relType := rtNone;
            end else
            begin
                GetValue(fields[0], rel, val);
                GetValue(fields[1], rel2, val2);
                val := ((val and $7FFF) shl 15) or (val2 and $7FFF);
                if (rel and rel2) then
                    relType := rtH1H2
                else if (rel) then
                    relType := rtH1
                else if (rel2) then
                    relType := rtH2
                else
                    relType := rtNone;
            end;
            fields.Delete(0);
            fields.Delete(0);
            ops := AnsiString(fields.DelimitedText);
        end;
        if (val < 0) then
            val := val - 1;
        FOutFile.EmitSingleWord(FLocationCounter.Value, relType, val);
        FListFile.Value := val;
        FListFile.Print;
        Inc(FLocationCounter.Value);
    finally
        fields.Free;
    end;
end;

procedure TAssembler.GetChannel(var ops, channel: AnsiString);
// Depending on the SPURT dialect, channel mnemonics may be given
// as: Cn or C.n or CnACTIVEIN or Cn.ACTIVEIN or CnACTIVEOUT or Cn.ACTIVEOUT
// as: Fn or F.n or FnACTIVEIN or Fn.ACTIVEIN or FnACTIVEOUT or Fn.ACTIVEOUT
//
// This routine validates any format and converts it so a standard
// Cn[ACTIVEIN | ACTIVEOUT]
var
    fields: TSpurtStringList;
    Mnemonic, chan, active: String;
    c: Char;
    num: Integer;
begin
    fields := TSpurtStringList.Create;
    try
        fields.DelimitedText := String(ops);
        if (fields[0] = '') then
            raise Exception.Create('MEANS directive must have an operand');
        if ((fields[0] = 'C') or (fields[0] = 'F')) then
        begin
            // Channel mnemonic given in 2 parts
            Mnemonic := fields[0];
            fields.Delete(0);
            if (fields.Count > 0) then
            begin
                Mnemonic := Mnemonic + fields[0];
                fields.Delete(0);
            end;
        end else
        begin
            // Channel mnemonic given in 1 part
            Mnemonic := fields[0];
            fields.Delete(0);
        end;
        if (fields.Count > 0) then
        begin
            // activein or activeout given as separate operand
            Mnemonic := Mnemonic + fields[0];
            fields.Delete(0);
        end;
        ops := AnsiString(fields.DelimitedText);
        // Now we should have something like Cchan#active??
        // So we need validate the entire mnemonic.
        c := (Copy(Mnemonic, 1, 1) + ' ')[1];
        if ((c <> 'C') and (c <> 'F')) then
            raise Exception.Create('Invalid channel identifier');
        if (c = 'F') then
            Mnemonic[1] := 'C';
        c := (Copy(Mnemonic, 3, 1) + ' ')[1];
        if ((c >= '0') and (c <= '9')) then
        begin
            chan := Copy(Mnemonic, 1, 3);
            active := Copy(Mnemonic, 4);
        end else
        begin
            chan := Copy(Mnemonic, 1, 2);
            active := Copy(Mnemonic, 3);
        end;
        active := AdjustIdent(active);
        // Validate the channel #
        if (not TryStrToInt(Copy(chan, 2), num)) then
            raise Exception.Create('Invalid channel identifier');
        if ((num < 0) or (num > 15)) then
            raise Exception.Create('Invalid channel identifier');
        if ((active <> '') and (active <> 'ACTIVEIN') and
          (active <> 'ACTIVEOUT')) then
            raise Exception.Create('Invalid channel identifier');

        channel := AnsiString(Mnemonic);
    finally
        fields.Free;
    end;
end;

procedure TAssembler.GetFields(sline: AnsiString;
  var lbl, operands, cmnt: AnsiString);
const
    invLabels: array [1 .. 26] of AnsiString = ('A', 'Q', 'B0', 'B1', 'B2',
      'B3', 'B4', 'B5', 'B6', 'B7', 'C0', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6',
      'C7', 'C8', 'C9', 'C10', 'C11', 'C12', 'C13', 'C14', 'C15');

var
    i: Integer;
    l: AnsiString;
    c: AnsiChar;
begin
    lbl := '';
    operands := '';
    cmnt := '';
    if (FTab = ' ') then
    begin
        // Separate fields as described in the 490 SPURT manual. Label starts in column 8,
        // instruction and comments separated from the label by spaces.
        if (Length(sline) < 8) then
            Exit;
        if (sline[7] <> ' ') then
        begin
            // if continued line then we have operands and a comment but no label
            operands := Trim(Copy(sline, 8));
            i := AnsiPos(AnsiString(' '), operands);
            if (i <> 0) then
            begin
                cmnt := Trim(Copy(operands, i + 1));
                operands := Copy(operands, 1, i - 1);
            end;
            Exit;
        end;
        if (sline[8] <> ' ') then
        begin
            lbl := Trim(Copy(sline, 8));
            i := AnsiPos(AnsiString(' '), lbl);
            if (i <> 0) then
            begin
                operands := Trim(Copy(lbl, i + 1));
                lbl := Copy(lbl, 1, i - 1);
            end;
        end else
        begin
            operands := Trim(Copy(sline, 8));
        end;
        if ((AnsiPos(AnsiString('FD.'), operands) = 1) or
          (AnsiPos(AnsiString('TYPET.'), operands) = 1)) then
        begin
            ;
        end else
        begin
            i := AnsiPos(AnsiString(' '), operands);
            if (i <> 0) then
            begin
                cmnt := Trim(Copy(operands, i + 1));
                operands := Trim(Copy(operands, 1, i - 1));
            end;
        end;
    end else
    begin
        // Separate fields in 1230 and 642 style source files where
        // fields are separated by the TAB character. A line of only
        // '..' was used to indicate the end of a paper tape (source)
        // file.
        sline := Trim(sline);
        if (sline = '..') then
        begin
            operands := sline;
        end else
        begin
            i := AnsiPos(FTab, sline);
            if (i = 0) then
            begin
                lbl := Trim(sline);
            end else
            begin
                lbl := Trim(Copy(sline, 1, i - 1));
                operands := Trim(Copy(sline, i + 1));
                i := AnsiPos(FTab, operands);
                if (i > 0) then
                begin
                    cmnt := Trim(Copy(operands, i + 1));
                    operands := Trim(Copy(operands, 1, i - 1));
                end;
            end;
        end;
    end;
    // Special for Joe Cousins source
    if (AnsiPos(AnsiString('COMMENT'), lbl) = 1) then
    begin
        operands := AnsiString('COMMENT') + '.' + Copy(lbl, 8) + operands;
        lbl := '';
    end;
    //
    c := FirstChar(lbl);
    if ((c = 'O') or (c = 'X') or ((c >= '0') and (c <= '9'))) then
        raise Exception.Create
          ('Label may not start with ''O'', ''X'' or ''0'' thru ''9''');
    for l in invLabels do
    begin
        if (l = lbl) then
            raise Exception.Create('Label may not be a register or channel ID');
    end;
end;

function TAssembler.GetNumber(var ops: AnsiString; var num: Integer): Boolean;
// Check the next token in the operand to see if it is a number and returns its value.
var
    fields: TSpurtStringList;
begin
    num := 0;
    fields := TSpurtStringList.Create;
    try
        fields.DelimitedText := String(ops);
        num := NumberToInt(fields[0]);
        fields.Delete(0);
        ops := AnsiString(fields.DelimitedText);
        Result := True;
    finally
        fields.Free;
    end;
end;

function TAssembler.GetOpcode(var ops: AnsiString; var rslt: TOpcode;
  var b: Integer): Boolean;
// Extract the opcode from the instruction. Assembler opcodes may consist of the opcode itself
// and a modifier. A modifier may be an register (A, Q, B0 - B7 or something else). The ops
// argument will be modified by having the opcode removed.
var
    flds: TSpurtStringList;
    Opcode: String;
begin
    if (ops = '..') then
    begin
        Result := True;
        rslt := FOpcodes.Items['..'];
        Exit;
    end;
    b := 0;
    flds := TSpurtStringList.Create;
    try
        flds.DelimitedText := String(ops);
        if (flds.Count = 0) then
        begin
            rslt := nil;
            Result := False;
            Exit;
        end;
        Opcode := AdjustIdent(flds[0]);
        if (flds.Count > 1) then
        begin
            if ((Length(flds[1]) = 2) and (FirstChar(AnsiString(flds[1])) = 'B'))
            then
            begin
                Opcode := Opcode + '.B';
                b := StrToInt(Copy(flds[1], 2));
            end else
                Opcode := Opcode + '.' + AdjustIdent(flds[1]);
            Result := FOpcodes.TryGetValue(Opcode, rslt);
            if (Result) then
            begin
                flds.Delete(0);
                flds.Delete(0);
                ops := AnsiString(flds.DelimitedText);
                Exit;
            end;
        end;
        Opcode := AdjustIdent(flds[0]);
        Result := FOpcodes.TryGetValue(Opcode, rslt);
        if (Result) then
        begin
            flds.Delete(0);
            ops := AnsiString(flds.DelimitedText);
        end;
    finally
        flds.Free;
    end;
end;

procedure TAssembler.GetToken(var ops, token: AnsiString);
var
    i: Integer;
begin
    token := '';
    ops := Trim(ops);
    if (ops = '') then
        Exit;

    i := 1;
    if (ops[1] = '.') then
    begin
        token := ops[1];
        i := 2;
    end else if ((ops[1] = '.') or (ops[i] = ')')) then
    begin
        while ((i <= Length(ops)) and ((ops[i] = '.') or (ops[i] = ')'))) do
        begin
            token := ops[i];
            Inc(i);
        end;
    end else if ((ops[1] = '+') or (ops[1] = '-')) then
    begin
        token := ops[1];
        i := 2;
    end else
    begin
        while ((i <= Length(ops)) and (ops[i] <> '+') and (ops[i] <> '-') and
          (ops[i] <> '.') and (ops[i] <> '(') and (ops[i] <> ')')) do
        begin
            token := token + ops[i];
            Inc(i);
        end;
    end;
    if (ops[i] = '(') then
    begin
        token := token + ops[i];
        Inc(i);
    end;
    ops := Copy(ops, i);
end;

function TAssembler.GetY(lineNum: Integer; var ops: AnsiString;
  var rel: TRelocatableType; var b: Byte): Integer;
var
    token, opr, sign: AnsiString;
    sym: TSymbol;
begin
    Result := 0;
    opr := 'X';
    sign := '';
    b := 0;
    GetToken(ops, token);
    while ((token <> '') and (token <> ')') and (token <> '.') and
      (token <> ').')) do
    begin
        if (((FirstChar(token) >= 'A') and (FirstChar(token) <= 'Z')) or
          (token = '$')) then
        begin
            // process identifiers
            if (opr = '') then
                raise Exception.Create('Address expression syntax error');
            if (not FSymbols.TryGetValue(AdjustIdent(token), sym)) then
                raise Exception.CreateFmt('%s is undefined', [token]);
            if (sym.DefCount < 1) then
                raise Exception.CreateFmt('%s is undefined', [token]);
            if (FPass = 2) then
                sym.xref.Add(lineNum);
            if ((token[1] = 'B') and (sym.SymbolType = stSystem)) then
            begin
                // B register
                b := sym.Value;
            end else
            begin
                // user identifier
                if (sym.Relocatable) then
                    rel := rtH2;
                if (opr = '+') then
                    Result := Result + sym.Value
                else if (opr = '-') then
                    Result := Result - sym.Value
                else
                    Result := sym.Value;
            end;
            opr := '';
        end else if ((token[1] >= '0') and (token[1] <= '9')) then
        begin
            // process constants
            if (opr = '+') then
                Result := Result + NumberToInt(String(sign + token))
            else if (opr = '-') then
                Result := Result - NumberToInt(String(sign + token))
            else
                Result := NumberToInt(String(sign + token));
            opr := '';
            sign := '';
        end else if ((opr <> '') and (token = '-')) then
        begin
            sign := token;
        end else if ((token <> '-') and (token <> '+')) then
            raise Exception.CreateFmt('%s is not a valid operator', [token])
        else
            opr := token;
        GetToken(ops, token);
    end;
end;

function TAssembler.NumberToInt(num: String): Integer;
begin
    Result := 0;
    if (num = '') then
        Exit;
    //
    if (num[Length(num)] = 'D') then
    begin
        num := Copy(num, 1, Length(num) - 1);
        if (not TryStrToInt(String(num), Result)) then
            raise Exception.CreateFmt
              ('%s is not a valid decimal number', [num]);
    end else
    begin
        try
            Result := Octal(String(num));
        except
            raise Exception.CreateFmt('%s is not a valid octal number', [num]);
        end;
    end;
end;

function TAssembler.Pass0: Boolean;
begin
    { TODO : Write macro processor pass here }
    Result := True;
end;

procedure TAssembler.Pass1(SrcFile: TSrcFileStream);
var
    lbl, operands, cmnt: AnsiString;
    lbl1, operands1: AnsiString;
    sym: TSymbol;
    Opcode: TOpcode;
    b: Integer;
begin
    FPass := 1;
    FStmtLabel := nil;
    while (not SrcFile.Eof) do
    begin
        try
            ReadStatement(SrcFile, lbl, operands, cmnt);
            if (lbl <> '') then
            begin
                lbl1 := AdjustIdent(lbl);
                if (FSymbols.TryGetValue(lbl1, sym)) then
                begin
                    if (sym.DefCount = 0) then
                    begin
                        sym.Value := FLocationCounter.Value;
                        sym.DefLine := SrcFile.LineNumber;
                        sym.DefCount := 1;
                        sym.Relocatable := True;
                    end else
                    begin
                        case sym.AllocationType of
                          atNone:
                            Inc(sym.DefCount);
                          atDirect, atRelative:
                          begin
                            FLocationCounter.Value := sym.Value;
                            FListFile.Address := sym.Value;
                          end;
                          atIndirect:
                          begin
                            { TODO : need to do something for indirect allocation here. not sure what. }
                          end;
                        end;
                    end;
                    FStmtLabel := sym;
                end else
                begin
                    sym := TSymbol.Create;
                    sym.id := lbl1;
                    sym.SourceID := lbl;
                    sym.SymbolType := stIdentifier;
                    sym.DefLine := SrcFile.LineNumber;
                    sym.Value := FLocationCounter.Value;
                    sym.Relocatable := True;
                    sym.DefCount := 1;
                    FSymbols.Add(lbl1, sym);
                    FStmtLabel := sym;
                end;
            end;
            if (operands <> '') then
            begin
                operands1 := StringReplace(operands, FSeparator,
                  AnsiString('.'), [rfReplaceAll]);
                if (GetOpcode(operands1, Opcode, b)) then
                begin
                    if (Opcode.OperandType = otDirective) then
                        Opcode.SpurtProc(SrcFile.LineNumber, operands1, Opcode)
                    else
                        Inc(FLocationCounter.Value);
                end else
                begin
                    if (FAllocationType <> atNone) then
                        AllocValue(operands1)
                    else if (((operands1[1] >= '0') and (operands1[1] <= '9'))
                      or (operands1[1] = '-')) then
                    begin
                        DoWord(SrcFile.LineNumber, operands);
                    end else
                    begin
                        GetToken(operands1, lbl);
                        if ((FSymbols.TryGetValue(AdjustIdent(lbl), sym)) and
                          (sym.SymbolType = stProcedure)) then
                            Inc(FLocationCounter.Value)
                        else
                            raise Exception.Create('Illegal opcode');
                    end;
                end;
            end;
            if (Assigned(FCrntProgram)) then
                FCrntProgram.EndAddr := FLocationCounter.Value;
        except
            on E: Exception do
            begin;
            end;
        end;
    end;
    { TODO : This is probably incorrent, but it will do as a starting point. }
    FObjCodeSize := FLocationCounter.Value - FTransferAddr;
end;

procedure TAssembler.Pass2(SrcFile: TSrcFileStream);
var
    lbl, operands, cmnt: AnsiString;
    lbl1, operands1: AnsiString;
    fname: String;
    Opcode: TOpcode;
    b: Integer;
    p: TProgram;
    sym: TSymbol;
    rel: TRelocatableType;
begin
    FPass := 2;
    if (not Assigned(FOutFile)) then
    begin
        case FOutputType of
          otImage, otExecutable:
            FOutFile := TMemImageStream.Create(FObjectFile, fmCreate);
          otObject:
            FOutFile := TRelocatableStream.Create(FObjectFile, fmCreate);
          otAbsolute:
            FOutFile := TAbsoluteStream.Create(FObjectFile, fmCreate);
        end;
        // Reset source file to beginning only if we are not being
        // called recursively.
        SrcFile.Reset;
        SrcFile.Column := 81;
        FStmtLabel := nil;
        if (FOutputType = otExecutable) then
            FLocationCounter.Value := 96
        else
            FLocationCounter.Value := 0;
        if (FPrograms.Count > 0) then
        begin
            p := FPrograms[0];
            FOutFile.EmitTransferAddr(p.StartAddr, FTransferAddr,
              p.EndAddr - p.StartAddr);
            p.TransferAddrEmitted := True;
        end;
        FListFile.Enabled := True;
    end;
    while (not SrcFile.Eof) do
    begin
        try
            FCurInst.Value := 0;
            ReadStatement(SrcFile, lbl, operands, cmnt);
            if ((lbl = '') and (operands = '') and (cmnt = '')) then
            begin
                FListFile.Print;
                Continue;
            end;
            if (lbl <> '') then
            begin
                lbl1 := AdjustIdent(lbl);
                FStmtLabel := FSymbols.Items[lbl1];
                if (FStmtLabel.DefCount > 1) then
                    raise Exception.CreateFmt
                      ('%s is defined multiple times', [lbl1]);
                if ((FAllocationType = atNone) and
                  (FStmtLabel.DefLine <= SrcFile.LineNumber)) then
                begin
                    case FStmtLabel.AllocationType of
                      atDirect, atRelative:
                       begin
                         FLocationCounter.Value := FStmtLabel.Value;
                         FListFile.Address := FStmtLabel.Value;
                       end;
                    end;
                end;
            end;
            if (operands <> '') then
            begin
                operands1 := StringReplace(operands, FSeparator,
                  AnsiString('.'), [rfReplaceAll]);
                if (GetOpcode(operands1, Opcode, b)) then
                begin
                    if (Opcode.OperandType = otDirective) then
                        // Process a directive, mono or poly-op
                        Opcode.SpurtProc(SrcFile.LineNumber, operands1, Opcode)
                    else
                    begin
                        // Generate an instruction
                        FCurInst.Value := 0;
                        FCurInst.f := Opcode.Opcode;
                        FCurInst.j := b;
                        if (Opcode.InstType = it77) then
                        begin
                            FCurInst.f := $3F;
                            FCurInst.g := Opcode.Opcode;
                        end;
                        try
                            Opcode.SpurtProc(SrcFile.LineNumber,
                              operands1, Opcode);
                        finally
                            Inc(FLocationCounter.Value);
                        end;
                    end;
                end else
                begin
                    if (FAllocationType <> atNone) then
                        AllocValue(operands1)
                    else if (((operands1[1] >= '0') and (operands1[1] <= '9'))
                      or (operands1[1] = '-')) then
                    begin
                        // Generate 1 word of storage
                        DoWord(SrcFile.LineNumber, operands1);
                    end else
                    begin
                        GetToken(operands1, lbl);
                        if ((FSymbols.TryGetValue(AdjustIdent(lbl), sym)) and
                          (sym.SymbolType = stProcedure)) then
                        begin
                            // Call a procedure
                            sym.xref.Add(SrcFile.LineNumber);
                            Opcode := FOpcodes['RJP.MAN'];
                            FCurInst.Clear;
                            FCurInst.f := Opcode.Opcode;
                            FCurInst.y := sym.Value;
                            if (sym.Relocatable) then
                                rel := rtH2
                            else
                                rel := rtNone;
                            FOutFile.EmitSingleWord(FLocationCounter.Value, rel,
                              FCurInst.Value);
                            FListFile.Value := FCurInst.Value;
                            FListFile.Print;
                            Inc(FLocationCounter.Value);
                        end else
                            raise Exception.Create('Illegal opcode');
                    end;
                end;
            end else
                FListFile.Print;
        except
            on E: Exception do
            begin
                FListFile.Print;
                FListFile.Print(Format('**** %s', [E.Message]));
                WriteLn(Format('%s: %s', [FListFile.lineNum, E.Message]));
                FStmtLabel := nil;
                Inc(FErrorCount);
            end;
        end;
    end;
    try
        if (FIOLibRequired) then
        begin
            fname := ExtractFilePath(ParamStr(0)) + '..\..\Bin\iolib.mem';
            if (not FileExists(fname)) then
                fname := ExtractFilePath(ParamStr(0)) + 'iolib.mem';
            FOutFile.Append(fname);
        end;
    except
        on E: Exception do
        begin
            FListFile.Print;
            FListFile.Print(Format('**** %s', [E.Message]));
            WriteLn(Format('%s: %s', [FListFile.lineNum, E.Message]));
            FStmtLabel := nil;
            Inc(FErrorCount);
        end;
    end;
end;

procedure TAssembler.PrintXref;
var
    syms: TStringList;
    sym: TSymbol;
    val: String;
    i, l, Count: Integer;
    s, ref, id: String;
begin
    if (not FPrintXref) then
        Exit;

    syms := TStringList.Create;
    try
        syms.Sorted := True;
        syms.OwnsObjects := False;
        for sym in FSymbols.Values do
            syms.AddObject(String(sym.id), sym);
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
              (sym.SymbolType <> stJDesignator) and (sym.SymbolType <> stForm))
            then
            begin
                if (sym.IsEntry) then
                    id := String(sym.id) + '*'
                else
                    id := String(sym.id);
                val := Copy(FormatOctal(sym.Value), 6);
                if (sym.IsExternal) then
                    val := '?????';
                s := Format('%-10.10s %s %4d ', [id, val, sym.DefLine]);
                ref := '';
                Count := 0;
                for l in sym.xref do
                begin
                    ref := ref + Format('%4d ', [l]);
                    Inc(Count);
                    if (Count >= 15) then
                    begin
                        FListFile.Print(Format('%s %s', [s, ref]));
                        s := StringOfChar(' ', 22);
                        ref := '';
                        Count := 0;
                    end;
                end;
                if (Count > 0) then
                    FListFile.Print(Format('%s %s', [s, ref]));
            end;
        end;
    finally
        syms.Free;
    end;
end;

procedure TAssembler.ReadStatement(SrcFile: TSrcFileStream;
  var lbl, operands, cmnt: AnsiString);
var
    sline: AnsiString;
    l, o, c: AnsiString;
    emsg: String;
begin
    emsg := '';
    sline := UpperCase(SrcFile.ReadLine);
    sline := StringReplace(sline, FSeparator, AnsiString('.'), [rfReplaceAll]);
    try
        GetFields(sline, lbl, operands, cmnt);
    except
        on E: Exception do
        begin
            emsg := E.Message;
        end;
    end;
    if (FPass = 2) then
    begin
        if (FTab = ' ') then
            FListFile.InitLine(SrcFile.LineNumber,
              FLocationCounter.Value, sline)
        else
        begin
            if (Pos('COMMENT', String(operands)) <> 0) then
                FListFile.InitLine(SrcFile.LineNumber, FLocationCounter.Value,
                  AnsiString(Format('       %-10.10s %s',
                  [lbl, StringReplace(operands + cmnt, FSeparator,
                  AnsiString('.'), [rfReplaceAll])])))
            else
                FListFile.InitLine(SrcFile.LineNumber, FLocationCounter.Value,
                  AnsiString(Format('       %-10.10s %-40.40s %s',
                  [lbl, StringReplace(operands, FSeparator, AnsiString('.'),
                  [rfReplaceAll]), cmnt])));
        end;
        if (emsg <> '') then
            raise Exception.Create(emsg);
    end;
    if (FTab = ' ') then
    begin
        // if we are processing a 490 style SPURT file, then check for
        // line continuation.
        sline := UpperCase(SrcFile.PeekLine);
        while ((Length(sline) >= 7) and (sline[7] <> ' ')) do
        begin
            sline := SrcFile.ReadLine;
            if (FPass = 2) then
            begin
                FListFile.Print;
                FListFile.InitLine(SrcFile.LineNumber,
                  FLocationCounter.Value, sline);
            end;
            GetFields(sline, l, o, c);
            operands := operands + o;
        end;
    end;
end;

{ TSpurtStrintList }

constructor TSpurtStringList.Create;
begin
    inherited;
    Delimiter := '.';
    QuoteChar := '~';
    StrictDelimiter := True;
end;

function TSpurtStringList.GetDelimitedText: String;
var
    i: Integer;
begin
    Result := '';
    for i := 0 to Count - 1 do
    begin
        if (i <> 0) then
            Result := Result + '.' + Strings[i]
        else
            Result := Strings[i];
    end;
end;

procedure TSpurtStringList.SetDelimitedText(const Value: String);
begin
    inherited DelimitedText := Value;
end;

end.
