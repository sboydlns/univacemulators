unit U494Opcodes;

interface

uses SysUtils;

type
  T494InstructionType = ( itOther = 0, itRead, itStore, itReplace, it77, itIO );

  T494OperandType = ( otGeneral, otBRegister, ot77, otIO, otDirective );

  T494JInterpretation = ( jiOther = 0, jiNormal, jiPlus1, jiLP, jiAddQ, jiNone );

  T494Opcode = packed record
  public
    SpurtMnemonic: String;
    AsmMnemonic: String;
    Opcode: Byte;
    InstType: T494InstructionType;
    JInterpret: T494JInterpretation;
    OperandType: T494OperandType;
    Priviledged: Boolean;
  end;

const
    // Tables of known opcodes. Unknown (illegal) opcodes have 'UNK'
    // in the mnemonic field.
    U494StdOpcodes: array [0..63] of T494Opcode = (
        (SpurtMnemonic: 'UNK'; AsmMnemonic: 'Unk'; Opcode: 0),
        (SpurtMnemonic: 'RSH.Q'; AsmMnemonic: 'RSQ'; Opcode: 1; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'RSH.A'; AsmMnemonic: 'RSA'; Opcode: 2; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'RSH.AQ'; AsmMnemonic: 'RSAQ'; Opcode: 3; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'COM.A'; AsmMnemonic: 'TA'; Opcode: 4; InstType: itRead; jInterpret: jiOther; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'LSH.Q'; AsmMnemonic: 'LSQ'; Opcode: 5; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'LSH.A'; AsmMnemonic: 'LSA'; Opcode: 6; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'LSH.AQ'; AsmMnemonic: 'LSAQ'; Opcode: 7; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'ENT.Q'; AsmMnemonic: 'LQ'; Opcode: 8; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'ENT.A'; AsmMnemonic: 'LA'; Opcode: 9; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'ENT.B'; AsmMnemonic: 'LB'; Opcode: 10; InstType: itRead; jInterpret: jiOther; OperandType: otBRegister; Priviledged: False),
        (SpurtMnemonic: 'EX-FCT'; AsmMnemonic: 'EXF'; Opcode: 11; InstType: itIO; jInterpret: jiNormal; OperandType: otIO; Priviledged: True),
        (SpurtMnemonic: 'STR.Q'; AsmMnemonic: 'SQ'; Opcode: 12; InstType: itStore; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'STR.A'; AsmMnemonic: 'SA'; Opcode: 13; InstType: itStore; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'STR.B'; AsmMnemonic: 'SB'; Opcode: 14; InstType: itStore; jInterpret: jiOther; OperandType: otBRegister; Priviledged: False),
        (SpurtMnemonic: 'STR'; AsmMnemonic: 'SC'; Opcode: 15; InstType: itIO; jInterpret: jiNormal; OperandType: otIO; Priviledged: True),
        (SpurtMnemonic: 'ADD.A'; AsmMnemonic: 'A'; Opcode: 16; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'SUB.A'; AsmMnemonic: 'AN'; Opcode: 17; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'MUL'; AsmMnemonic: 'M'; Opcode: 18; InstType: itRead; jInterpret: jiOther; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'DIV'; AsmMnemonic: 'D'; Opcode: 19; InstType: itRead; jInterpret: jiOther; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'RPL.A+Y'; AsmMnemonic: 'RA'; Opcode: 20; InstType: itReplace; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'RPL.A-Y'; AsmMnemonic: 'RAN'; Opcode: 21; InstType: itReplace; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'ADD.Q'; AsmMnemonic: 'AQ'; Opcode: 22; InstType: itRead; jInterpret: jiAddQ; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'SUB.Q'; AsmMnemonic: 'ANQ'; Opcode: 23; InstType: itRead; jInterpret: jiAddQ; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'ENT.Y+Q'; AsmMnemonic: 'LAQ'; Opcode: 24; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'ENT.Y-Q'; AsmMnemonic: 'LANQ'; Opcode: 25; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'STR.A+Q'; AsmMnemonic: 'SAQ'; Opcode: 26; InstType: itStore; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'STR.A-Q'; AsmMnemonic: 'SANQ'; Opcode: 27; InstType: itStore; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'RPL.Y+Q'; AsmMnemonic: 'RAQ'; Opcode: 28; InstType: itReplace; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'RPL.Y-Q'; AsmMnemonic: 'RANQ'; Opcode: 29; InstType: itReplace; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'RPL.Y+1'; AsmMnemonic: 'RI'; Opcode: 30; InstType: itReplace; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'RPL.Y-1'; AsmMnemonic: 'RD'; Opcode: 31; InstType: itReplace; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'ENT.LP'; AsmMnemonic: 'LLP'; Opcode: 32; InstType: itRead; jInterpret: jiLP; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'ADD.LP'; AsmMnemonic: 'ALP'; Opcode: 33; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'SUB.LP'; AsmMnemonic: 'ANLP'; Opcode: 34; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'COM.MASK'; AsmMnemonic: 'TLP'; Opcode: 35; InstType: itRead; jInterpret: jiOther; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'RPL.LP'; AsmMnemonic: 'RLP'; Opcode: 36; InstType: itReplace; jInterpret: jiLP; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'RPL.A+LP'; AsmMnemonic: 'RALP'; Opcode: 37; InstType: itReplace; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'RPL.A-LP'; AsmMnemonic: 'RANLP'; Opcode: 38; InstType: itReplace; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'STR.LP'; AsmMnemonic: 'SAND'; Opcode: 39; InstType: itStore; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'SEL.SET'; AsmMnemonic: 'OR'; Opcode: 40; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'SEL.CP'; AsmMnemonic: 'XOR'; Opcode: 41; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'SEL.CL'; AsmMnemonic: 'NOT'; Opcode: 42; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'SEL.SU'; AsmMnemonic: 'SSU'; Opcode: 43; InstType: itRead; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'RSE.SET'; AsmMnemonic: 'ROR'; Opcode: 44; InstType: itReplace; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'RSE.CP'; AsmMnemonic: 'RXOR'; Opcode: 45; InstType: itReplace; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'RSE.CL'; AsmMnemonic: 'RNOT'; Opcode: 46; InstType: itReplace; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'RSE.SU'; AsmMnemonic: 'RSSU'; Opcode: 47; InstType: itReplace; jInterpret: jiNormal; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'JP'; AsmMnemonic: 'JT'; Opcode: 48; InstType: itRead; jInterpret: jiOther; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'JP.MAN'; AsmMnemonic: 'J'; Opcode: 49; InstType: itRead; jInterpret: jiOther; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'JP.ACTI'; AsmMnemonic: 'JACTI'; Opcode: 50; InstType: itIO; jInterpret: jiNormal; OperandType: otIO; Priviledged: True),
        (SpurtMnemonic: 'JP.ACTO'; AsmMnemonic: 'JACTO'; Opcode: 51; InstType: itIO; jInterpret: jiNormal; OperandType: otIO; Priviledged: True),
        (SpurtMnemonic: 'RJP'; AsmMnemonic: 'SLJT'; Opcode: 52; InstType: itRead; jInterpret: jiOther; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'RJP.MAN'; AsmMnemonic: 'SLJ'; Opcode: 53; InstType: itRead; jInterpret: jiOther; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'TERM.IN'; AsmMnemonic: 'TERMIN'; Opcode: 54; InstType: itIO; jInterpret: jiNormal; OperandType: otIO; Priviledged: True),
        (SpurtMnemonic: 'TERM.OUT'; AsmMnemonic: 'TERMOT'; Opcode: 55; InstType: itIO; jInterpret: jiNormal; OperandType: otIO; Priviledged: True),
        (SpurtMnemonic: 'RPT'; AsmMnemonic: 'R'; Opcode: 56; InstType: itRead; jInterpret: jiOther; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'BSK.B'; AsmMnemonic: 'TBI'; Opcode: 57; InstType: itRead; jInterpret: jiOther; OperandType: otBRegister; Priviledged: False),
        (SpurtMnemonic: 'BJP.B'; AsmMnemonic: 'JBD'; Opcode: 58; InstType: itRead; jInterpret: jiOther; OperandType: otBRegister; Priviledged: False),
        (SpurtMnemonic: 'IN'; AsmMnemonic: 'IN'; Opcode: 59; InstType: itIO; jInterpret: jiNormal; OperandType: otIO; Priviledged: True),
        (SpurtMnemonic: 'OUT'; AsmMnemonic: 'OUT'; Opcode: 60; InstType: itIO; jInterpret: jiNormal; OperandType: otIO; Priviledged: True),
        (SpurtMnemonic: 'IN.MON'; AsmMnemonic: 'INMON'; Opcode: 61; InstType: itIO; jInterpret: jiNormal; OperandType: otIO; Priviledged: True),
        (SpurtMnemonic: 'OUT.MON'; AsmMnemonic: 'OUTMON'; Opcode: 62; InstType: itIO; jInterpret: jiNormal; OperandType: otIO; Priviledged: True),
        (SpurtMnemonic: 'UNK'; AsmMnemonic: 'UNK'; Opcode: 63)
    );
    // Extended instructions (opcode 77). This table list the opcodes
    // given in the second 6-bits.
    U494ExtOpcodes: array [0..63] of T494Opcode = (
        (SpurtMnemonic: 'UNK'; AsmMnemonic: 'UNK'; Opcode: 0),
        (SpurtMnemonic: 'FADD'; AsmMnemonic: 'FA'; Opcode: 1; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'FSUB'; AsmMnemonic: 'FAN'; Opcode: 2; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'FMUL'; AsmMnemonic: 'FM'; Opcode: 3; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'UNK'; AsmMnemonic: 'UNK'; Opcode: 4; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'FDIV'; AsmMnemonic: 'FD'; Opcode: 5; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'FPP'; AsmMnemonic: 'FP'; Opcode: 6; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'FPU'; AsmMnemonic: 'FU'; Opcode: 7; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'DTEST'; AsmMnemonic: 'DT'; Opcode: 8; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'DADD'; AsmMnemonic: 'DA'; Opcode: 9; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'DSUB'; AsmMnemonic: 'DAN'; Opcode: 10; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'DCME'; AsmMnemonic: 'DTE'; Opcode: 11; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'DCP'; AsmMnemonic: 'DN'; Opcode: 12; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'DADDC'; AsmMnemonic: 'DAC'; Opcode: 13; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'DSUBB'; AsmMnemonic: 'DANB'; Opcode: 14; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'DCOML'; AsmMnemonic: 'DTL'; Opcode: 15; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'UNK'; AsmMnemonic: 'UNK'; Opcode: 16),
        (SpurtMnemonic: 'DPENT'; AsmMnemonic: 'DPL'; Opcode: 17; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'DPADD'; AsmMnemonic: 'DPA'; Opcode: 18; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'DPCME'; AsmMnemonic: 'DPTE'; Opcode: 19; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'DPCP'; AsmMnemonic: 'DPN'; Opcode: 20; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'DPSTR'; AsmMnemonic: 'DPS'; Opcode: 21; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'DPSUB'; AsmMnemonic: 'DPAN'; Opcode: 22; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'DPCML'; AsmMnemonic: 'DPTL'; Opcode: 23; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'SFSH'; AsmMnemonic: 'SFS'; Opcode: 24; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'CREL'; AsmMnemonic: 'CPL'; Opcode: 25; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'CREU'; AsmMnemonic: 'CPU'; Opcode: 26; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'DCVL'; AsmMnemonic: 'DCL'; Opcode: 27; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'DCVU'; AsmMnemonic: 'DCU'; Opcode: 28; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'CRSL'; AsmMnemonic: 'CUL'; Opcode: 29; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'CRSU'; AsmMnemonic: 'CUU'; Opcode: 30; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'XQT'; AsmMnemonic: 'ER'; Opcode: 31; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'EBJP.B0'; AsmMnemonic: 'LBPJB0'; Opcode: 32; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'EBJP.B1'; AsmMnemonic: 'LBPJB1'; Opcode: 33; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'EBJP.B2'; AsmMnemonic: 'LBPJB2'; Opcode: 34; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'EBJP.B3'; AsmMnemonic: 'LBPJB3'; Opcode: 35; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'EBJP.B4'; AsmMnemonic: 'LBPJB4'; Opcode: 36; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'EBJP.B5'; AsmMnemonic: 'LBPJB5'; Opcode: 37; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'EBJP.B6'; AsmMnemonic: 'LBPJB6'; Opcode: 38; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'EBJP.B7'; AsmMnemonic: 'LBPJB7'; Opcode: 39; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'UNK'; AsmMnemonic: 'UNK'; Opcode: 40),
        (SpurtMnemonic: 'LRSQ'; AsmMnemonic: 'LRSQ'; Opcode: 41; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'TSET'; AsmMnemonic: 'TSET'; Opcode: 42; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'MACE'; AsmMnemonic: 'MATE'; Opcode: 43; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'EXRN'; AsmMnemonic: 'EXRN'; Opcode: 44; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'LRSA'; AsmMnemonic: 'LRSA'; Opcode: 45; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'LRSAQ'; AsmMnemonic: 'LRSAQ'; Opcode: 46; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'MACL'; AsmMnemonic: 'MATL'; Opcode: 47; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'UNK'; AsmMnemonic: 'UNK'; Opcode: 48),
        (SpurtMnemonic: 'EIR'; AsmMnemonic: 'EIR'; Opcode: 49; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: True),
        (SpurtMnemonic: 'LPLR'; AsmMnemonic: 'LPLR'; Opcode: 50; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: True),
        (SpurtMnemonic: 'UNK'; AsmMnemonic: 'UNK'; Opcode: 51),
        (SpurtMnemonic: 'UNK'; AsmMnemonic: 'UNK'; Opcode: 52),
        (SpurtMnemonic: 'STR.IFR'; AsmMnemonic: 'SIFR'; Opcode: 53; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: True),
        (SpurtMnemonic: 'ENT.RIR'; AsmMnemonic: 'ERIR'; Opcode: 54; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: True),
        (SpurtMnemonic: 'UNK'; AsmMnemonic: 'UNK'; Opcode: 55),
        (SpurtMnemonic: 'SYNC'; AsmMnemonic: 'SYNC'; Opcode: 56; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: True),
        (SpurtMnemonic: 'EWB'; AsmMnemonic: 'LBW'; Opcode: 57; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'SCN'; AsmMnemonic: 'SCN'; Opcode: 58; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: True),
        (SpurtMnemonic: 'ESCR'; AsmMnemonic: 'ECSR'; Opcode: 59; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: True),
        (SpurtMnemonic: 'UNK'; AsmMnemonic: 'UNK'; Opcode: 60),
        (SpurtMnemonic: 'SWB'; AsmMnemonic: 'SBW'; Opcode: 61; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False),
        (SpurtMnemonic: 'UNK'; AsmMnemonic: 'UNK'; Opcode: 62),
        (SpurtMnemonic: 'LOG'; AsmMnemonic: 'LOG'; Opcode: 63; InstType: it77; JInterpret: jiNone; OperandType: ot77; Priviledged: False)
    );

    U494PsuedoOps: array [0..12] of T494Opcode = (
        (SpurtMnemonic: 'COM.Q'; AsmMnemonic: 'TQ'; Opcode: 4; InstType: itRead; jInterpret: jiOther; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'COM.AQ'; AsmMnemonic: 'TR'; Opcode: 4; InstType: itRead; jInterpret: jiOther; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'CL.B'; AsmMnemonic: 'ZB'; Opcode: 10; InstType: itRead; jInterpret: jiOther; OperandType: otBRegister; Priviledged: False),
        (SpurtMnemonic: 'NO.OP'; AsmMnemonic: 'NOP'; Opcode: 10; InstType: itRead; jInterpret: jiOther; OperandType: otBRegister; Priviledged: False),
        (SpurtMnemonic: 'NO-OP'; AsmMnemonic: 'UNK'; Opcode: 10; InstType: itRead; jInterpret: jiOther; OperandType: otBRegister; Priviledged: False),
        (SpurtMnemonic: 'CP.Q'; AsmMnemonic: 'NQ'; Opcode: 12; InstType: itRead; jInterpret: jiOther; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'CP.A'; AsmMnemonic: 'NA'; Opcode: 13; InstType: itRead; jInterpret: jiOther; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'CL.Q'; AsmMnemonic: 'ZQ'; Opcode: 14; InstType: itRead; jInterpret: jiOther; OperandType: otBRegister; Priviledged: False),
        (SpurtMnemonic: 'CL'; AsmMnemonic: 'SZ'; Opcode: 14; InstType: itRead; jInterpret: jiOther; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'CL.A'; AsmMnemonic: 'ZA'; Opcode: 17; InstType: itRead; jInterpret: jiOther; OperandType: otGeneral; Priviledged: False),
        (SpurtMnemonic: 'EX-COM'; AsmMnemonic: 'UNK'; Opcode: 11; InstType: itIO; jInterpret: jiOther; OperandType: otIO; Priviledged: True),
        (SpurtMnemonic: 'EX-COM-MW'; AsmMnemonic: 'UNK'; Opcode: 60; InstType: itIO; jInterpret: jiOther; OperandType: otIO; Priviledged: True),
        (SpurtMnemonic: 'RILJP'; AsmMnemonic: 'UNK'; Opcode: 48; InstType: itRead; jInterpret: jiOther; OperandType: otGeneral; Priviledged: True)
    );

    AsmDirectives: array [0..20] of T494Opcode = (
        (AsmMnemonic: 'EQU'; OperandType: otDirective),
        (AsmMnemonic: 'RES'; OperandType: otDirective),
        (AsmMnemonic: 'LIT'; OperandType: otDirective),
        (AsmMnemonic: 'FORM'; OperandType: otDirective),
        (AsmMnemonic: 'START'; OperandType: otDirective),
        (AsmMnemonic: 'END'; OperandType: otDirective),
        (AsmMnemonic: 'DLD'; OperandType: otDirective),
        (AsmMnemonic: 'UTAG'; OperandType: otDirective),
        (AsmMnemonic: 'DO'; OperandType: otDirective),
        (AsmMnemonic: 'COMMON'; OperandType: otDirective),
        (AsmMnemonic: 'BLOCKDATA'; OperandType: otDirective),
        (AsmMnemonic: 'XREF'; OperandType: otDirective),
        (AsmMnemonic: 'EDEF'; OperandType: otDirective),
        (AsmMnemonic: 'EXPRESSION'; OperandType: otDirective),
        (AsmMnemonic: 'INPUT'; OperandType: otDirective),
        (AsmMnemonic: 'INPUTFORM'; OperandType: otDirective),
        (AsmMnemonic: 'LET'; OperandType: otDirective),
        (AsmMnemonic: 'UNLIST'; OperandType: otDirective),
        (AsmMnemonic: 'LIST'; OperandType: otDirective),
        (AsmMnemonic: 'ENTRY'; OperandType: otDirective),
        (AsmMnemonic: 'EXIT'; OperandType: otDirective)
    );

    SpurtDirectives: array [0..22] of T494Opcode = (
        (SpurtMnemonic: 'C-CONTROL'; OperandType: otDirective),
        (SpurtMnemonic: 'OUTPUTS'; OperandType: otDirective),
        (SpurtMnemonic: 'ALLOCATION'; OperandType: otDirective),
        (SpurtMnemonic: 'REL-ALLOC'; OperandType: otDirective),
        (SpurtMnemonic: 'INDR-ALLOC'; OperandType: otDirective),
        (SpurtMnemonic: 'PROGRAM'; OperandType: otDirective),
        (SpurtMnemonic: 'MEANS'; OperandType: otDirective),
        (SpurtMnemonic: 'EQUALS'; OperandType: otDirective),
        (SpurtMnemonic: 'ENTRY'; OperandType: otDirective),
        (SpurtMnemonic: 'EXIT'; OperandType: otDirective),
        (SpurtMnemonic: 'U-TAG'; OperandType: otDirective),
        (SpurtMnemonic: 'PUT'; OperandType: otDirective),
        (SpurtMnemonic: 'INCREMENT'; OperandType: otDirective),
        (SpurtMnemonic: 'COMMENT'; OperandType: otDirective),
        (SpurtMnemonic: 'RESERVE'; OperandType: otDirective),
        (SpurtMnemonic: 'CLEAR'; OperandType: otDirective),
        (SpurtMnemonic: 'MOVE'; OperandType: otDirective),
        (SpurtMnemonic: 'TERM'; OperandType: otDirective),
        (SpurtMnemonic: 'SIL.ALL'; OperandType: otDirective),
        (SpurtMnemonic: 'SIL-EX'; OperandType: otDirective),
        (SpurtMnemonic: 'RIL'; OperandType: otDirective),
        (SpurtMnemonic: 'RIL-EX'; OperandType: otDirective),
        (SpurtMnemonic: '..'; OperandType: otDirective)
    );


implementation

end.