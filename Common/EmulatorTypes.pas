unit EmulatorTypes;

interface

uses Windows, WinApi.SHFolder, SysUtils, Classes, Generics.Collections, Forms, SyncObjs,
     Graphics, Printers;

type
  TCodeTranslator = class
  public
    class function AsciiToFieldata(c: AnsiChar): AnsiChar; overload;
    class function AsciiToFieldata(s: AnsiString): AnsiString; overload;
    class function AsciiToEbcdic(B: Byte): AnsiChar; overload;
    class function AsciiToEbcdic(s: AnsiString): AnsiString; overload;
    class function AsciiToHollerith8(c: AnsiChar): Byte; overload;
    class function AsciiToXS3(c: AnsiChar): Byte; overload;
    class function EbcdicToAscii(b: Byte): AnsiChar; overload;
    class function EbcdicToAscii(s: AnsiString): AnsiString; overload;
    class function FieldataToAscii(b: Byte): AnsiChar; overload;
    class function FieldataToAscii(s: AnsiString): AnsiString; overload;
    class function FieldataToHollerith12(c: AnsiChar): Word; overload;
    class function Hollerith12ToFieldata(w: Word): Byte; overload;
    class function Hollerith12ToHollerith8(w: Word): Byte; overload;
    class function Hollerith12ToXS3(w: Word): Byte; overload;
    class function Hollerith8ToAscii(b: Byte): AnsiChar; overload;
    class function Hollerith8ToAscii(bfr: array of Byte): AnsiString; overload;
    class function Hollerith8ToHollerith12(b: Byte): WORD; overload;
    class function Printer16ToAscii(b: Byte): AnsiChar; overload;
    class function Printer16ToAscii(bfr: array of Byte): AnsiString; overload;
    class function Printer48ToAscii(b: Byte): AnsiChar; overload;
    class function Printer48ToAscii(bfr: array of Byte): AnsiString; overload;
    class function Printer63ToAscii(b: Byte): AnsiChar; overload;
    class function Printer63ToAscii(bfr: array of Byte): AnsiString; overload;
    class function XS3ToAscii(b: Byte): AnsiChar; overload;
    class function XS3ToAscii(bfr: array of Byte): AnsiString; overload;
    class function XS3ToHollerith12(b: Byte): Word; overload;
  end;

  TXlateItem = record
    Hollerith: Byte;
    Native: AnsiChar;
  end;

  TXS3XlateItem = record
    Hollerith: Word;
    Native: Byte;
  end;

  // A class to hold a simulated carriage control tape. The list contains a 1 byte
  // entry for each line on the page.
  // Bit 7 (MSB) = channel 7
  // Bit 6       = channel 6
  // ...
  // Bit 1       = channel 1
  TCarriageControlTape = class(TList<Byte>)
  private
      FLPI: Byte;
      procedure MakeDefaultTape;
      procedure SetLPI(const Value: Byte);
  public
      constructor Create(lpi: Byte); reintroduce;
      property lpi: Byte read FLPI write SetLPI;
  end;

  TPrintNotifyEvent = procedure(Sender: TObject; lineNum: Integer; text: AnsiString) of object;
  THomeNotifyEvent = procedure(Sender: TObject) of object;

  TPrinterBitmap = class(TBitmap)
  private
      FPPI: Integer;
      FLPI: Byte;
      FPageNumber: Integer;
      FLock: TCriticalSection;
      procedure SetLPI(const Value: Byte);
  public
      constructor Create(lpi: Byte; ppi: Integer); reintroduce;
      destructor Destroy; override;
      procedure Clear;
      procedure Lock;
      procedure Print(lineNum: Integer; text: AnsiString);
      procedure SendToPrinter;
      procedure Unlock;
      property lpi: Byte read FLPI write SetLPI;
  end;

const
    // Excess 3 character codes
    X3_SPACE = $00;
    X3_RIGHT_SQUARE = $01;
    X3_MINUS = $02;
    X3_0 = $03;
    X3_1 = $04;
    X3_2 = $05;
    X3_3 = $06;
    X3_4 = $07;
    X3_5 = $08;
    X3_6 = $09;
    X3_7 = $0A;
    X3_8 = $0B;
    X3_9 = $0C;
    X3_BACK_SLASH = $0D;
    X3_SEMI = $0E;
    X3_LEFT_SQUARE = $0F;
    X3_AMP = $10;
    X3_COLON = $11;
    X3_PERIOD = $12;
    X3_QUESTION = $13;
    X3_A = $14;
    X3_B = $15;
    X3_C = $16;
    X3_D = $17;
    X3_E = $18;
    X3_F = $19;
    X3_G = $1A;
    X3_H = $1B;
    X3_I = $1C;
    X3_SHARP = $1D;
    X3_LESS = $1E;
    X3_EQUAL = $1F;
    X3_APOS = $20;
    X3_ASTERIX = $21;
    X3_DOLLAR = $22;
    X3_EXCLAM = $23;
    X3_J = $24;
    X3_K = $25;
    X3_L = $26;
    X3_M = $27;
    X3_N = $28;
    X3_O = $29;
    X3_P = $2A;
    X3_Q = $2B;
    X3_R = $2C;
    X3_LEFT_PAREN = $2D;
    X3_AT = $2E;
    X3_DELTA = $2F;
    X3_NOT_EQUAL = $30;
    X3_PERCENT = $31;
    X3_COMMA = $32;
    X3_PLUS = $33;
    X3_SLASH = $34;
    X3_S = $35;
    X3_T = $36;
    X3_U = $37;
    X3_V = $38;
    X3_W = $39;
    X3_X = $3A;
    X3_Y = $3B;
    X3_Z = $3C;
    X3_LOZENGE = $3D;
    X3_GREATER = $3E;
    X3_RIGHT_PAREN = $3F;
    // Fieldata character codes
    FD_AT = $00;
    FD_LEFT_SQUARE = $01;
    FD_RIGHT_SQUARE = $02;
    FD_SHARP = $03;
    FD_DELTA = $04;
    FD_SPACE = $05;
    FD_A = $06;
    FD_B = $07;
    FD_C = $08;
    FD_D = $09;
    FD_E = $0A;
    FD_F = $0B;
    FD_G = $0C;
    FD_H = $0D;
    FD_I = $0E;
    FD_J = $0F;
    FD_K = $10;
    FD_L = $11;
    FD_M = $12;
    FD_N = $13;
    FD_O = $14;
    FD_P = $15;
    FD_Q = $16;
    FD_R = $17;
    FD_S = $18;
    FD_T = $19;
    FD_U = $1A;
    FD_V = $1B;
    FD_W = $1C;
    FD_X = $1D;
    FD_Y = $1E;
    FD_Z = $1F;
    FD_RIGHT_PAREN = $20;
    FD_MINUS = $21;
    FD_PLUS = $22;
    FD_LESS = $23;
    FD_EQUAL = $24;
    FD_GREATER = $25;
    FD_AMP = $26;
    FD_DOLLAR = $27;
    FD_ASTERIX = $28;
    FD_LEFT_PAREN = $29;
    FD_PERCENT = $2A;
    FD_COLON = $2B;
    FD_QUESTION = $2C;
    FD_EXCLAMATION = $2D;
    FD_COMMA = $2E;
    FD_BACK_SLASH = $2F;
    FD_0 = $30;
    FD_1 = $31;
    FD_2 = $32;
    FD_3 = $33;
    FD_4 = $34;
    FD_5 = $35;
    FD_6 = $36;
    FD_7 = $37;
    FD_8 = $38;
    FD_9 = $39;
    FD_APOS = $3A;
    FD_SEMI_COLON = $3B;
    FD_SLASH = $3C;
    FD_PERIOD = $3D;
    FD_LOZENGE = $3E;
    FD_STOP = $3F;
    // Hollerith card column bits
    HOLLERITH_12 = $8000;
    HOLLERITH_11 = $4000;
    HOLLERITH_0 = $2000;
    HOLLERITH_1 = $1000;
    HOLLERITH_2 = $0800;
    HOLLERITH_3 = $0400;
    HOLLERITH_4 = $0200;
    HOLLERITH_5 = $0100;
    HOLLERITH_6 = $0080;
    HOLLERITH_7 = $0040;
    HOLLERITH_8 = $0020;
    HOLLERITH_9 = $0010;
    // Carriage control tape channels
    CHANNEL7 = $80;
    CHANNEL6 = $40;
    CHANNEL5 = $20;
    CHANNEL4 = $10;
    CHANNEL3 = $08;
    CHANNEL2 = $04;
    CHANNEL1 = $02;


  function CardFileDir: String;
  function ExeDir: String;
  function UserDataDir: String;

implementation

const
  // ASCII to Fieldata translate table (for 494 printer codes)
  AsciiFieldata: array [0..127] of Byte = (
    FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE,             // 00-07
    FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE,             // 08-0F
    FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE,             // 10-17
    FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE,             // 18-1F
    FD_SPACE, FD_EXCLAMATION, FD_SPACE, FD_SHARP, FD_DOLLAR, FD_PERCENT, FD_AMP, FD_APOS,       // 20-27
    FD_LEFT_PAREN, FD_RIGHT_PAREN, FD_ASTERIX, FD_PLUS, FD_COMMA, FD_MINUS, FD_PERIOD, FD_SLASH, // 28-2F
    FD_0, FD_1, FD_2, FD_3, FD_4, FD_5, FD_6, FD_7,                                             // 30-37
    FD_8, FD_9, FD_COLON, FD_SEMI_COLON, FD_LESS, FD_EQUAL, FD_GREATER, FD_QUESTION,            // 38-3F
    FD_AT, FD_A, FD_B, FD_C, FD_D, FD_E, FD_F, FD_G,                                            // 41-47
    FD_H, FD_I, FD_J, FD_K, FD_L, FD_M, FD_N, FD_O,                                             // 48-4F
    FD_P, FD_Q, FD_R, FD_S, FD_T, FD_U, FD_V, FD_W,                                             // 50-57
    FD_X, FD_Y, FD_Z, FD_LEFT_SQUARE, FD_BACK_SLASH,  FD_RIGHT_SQUARE, FD_DELTA, FD_SPACE,      // 58-5F
    FD_AT, FD_A, FD_B, FD_C, FD_D, FD_E, FD_F, FD_G,                                            // 60-67
    FD_H, FD_I, FD_J, FD_K, FD_L, FD_M, FD_N, FD_O,                                             // 68-6F
    FD_P, FD_Q, FD_R, FD_S, FD_T, FD_U, FD_V, FD_W,                                             // 70-77
    FD_X, FD_Y, FD_Z, FD_LOZENGE, FD_SPACE, FD_SPACE, FD_SPACE, FD_SPACE                        // 78-7F
  );
  // Fieldata to ASCII translation table (for 494 printer codes)
  FieldataAscii: array [0..63] of AnsiChar = (
    '@', '[', ']', '#', '^', ' ', 'A', 'B',
    'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
    'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
    'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    ')', '-', '+', '<', '=', '>', '&', '$',
    '*', '(', '%', ':', '?', '!', ',', '\',
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', '''', ';', '/', '.', '{', ' '
  );
  // ASCII to EBCDIC translate table
  AsciiEbcdic: array [0..255] of Byte = (
    $00, $01, $02, $03, $37, $2d, $2e, $2f,
    $16, $05, $25, $0b, $0c, $0d, $0e, $0f,
    $10, $11, $12, $13, $3c, $3d, $32, $26,
    $18, $19, $3f, $27, $1c, $1d, $1e, $1f,
    $40, $4f, $7f, $7b, $5b, $6c, $50, $7d,
    $4d, $5d, $5c, $4e, $6b, $60, $4b, $61,
    $f0, $f1, $f2, $f3, $f4, $f5, $f6, $f7,
    $f8, $f9, $7a, $5e, $4c, $7e, $6e, $6f,
    $7c, $c1, $c2, $c3, $c4, $c5, $c6, $c7,
    $c8, $c9, $d1, $d2, $d3, $d4, $d5, $d6,
    $d7, $d8, $d9, $e2, $e3, $e4, $e5, $d6,
    $e7, $e8, $e9, $4a, $e0, $5a, $5f, $6d,
    $79, $81, $82, $83, $84, $85, $86, $87,
    $88, $89, $91, $92, $93, $94, $95, $96,
    $97, $98, $99, $a2, $a3, $a4, $a5, $a6,
    $a7, $a8, $a9, $c0, $6a, $d0, $a1, $07,
    $20, $21, $22, $23, $24, $15, $06, $17,
    $28, $29, $2a, $2b, $2c, $09, $0a, $1b,
    $30, $31, $1a, $33, $34, $35, $36, $08,
    $38, $39, $3a, $3b, $04, $14, $3e, $e1,
    $41, $42, $43, $44, $45, $46, $47, $48,
    $49, $51, $52, $53, $54, $55, $56, $57,
    $58, $59, $62, $63, $64, $65, $66, $67,
    $68, $69, $70, $71, $72, $73, $74, $75,
    $76, $77, $78, $80, $8a, $8b, $8c, $8d,
    $8e, $8f, $90, $9a, $9b, $9c, $9d, $9e,
    $9f, $a0, $aa, $ab, $ac, $ad, $ae, $af,
    $b0, $b1, $b2, $b3, $b4, $b5, $b6, $b7,
    $b8, $b9, $ba, $bb, $bc, $bd, $be, $bf,
    $ca, $cb, $cc, $cd, $ce, $cf, $da, $db,
    $dc, $dd, $de, $df, $ea, $eb, $ec, $ed,
    $ee, $ef, $fa, $fb, $fc, $fd, $fe, $ff
  );

  // EBCDIC to ASCII translate table
  EbcdicAscii: array [0..255] of Byte = (
    $00, $01, $02, $03, $00, $09, $00, $7f,
    $00, $00, $00, $0b, $0c, $0d, $0e, $0f,
    $10, $11, $12, $13, $00, $0a, $08, $00,
    $18, $19, $00, $00, $1c, $1d, $1e, $1f,
    $00, $00, $1c, $00, $00, $0a, $17, $1b,
    $00, $00, $00, $00, $00, $05, $06, $07,
    $00, $00, $16, $00, $00, $00, $00, $04,
    $00, $00, $00, $00, $14, $15, $00, $1a,
    $20, $00, $e2, $e4, $e0, $e1, $e3, $e5,
    $e7, $f0, $a2, $2e, $3c, $28, $2b, $7c,
    $26, $e9, $ea, $eb, $e8, $ed, $ee, $ef,
    $ec, $df, $21, $24, $2a, $29, $3b, $5e,
    $2d, $2f, $c2, $c4, $c0, $c1, $c3, $c5,
    $c7, $d1, $a6, $2c, $25, $5f, $3e, $3f,
    $f8, $c9, $ca, $cb, $c8, $cd, $ce, $cf,
    $cc, $5f, $3a, $23, $40, $27, $3d, $22,
    $d8, $61, $62, $63, $64, $65, $66, $67,
    $68, $69, $ab, $bb, $f0, $0d, $de, $b1,
    $b0, $6a, $6b, $6c, $6d, $6e, $6f, $70,
    $71, $72, $aa, $ba, $e6, $b8, $c6, $a4,
    $b5, $7e, $73, $74, $75, $76, $77, $78,
    $79, $7a, $a1, $bf, $d0, $5b, $fe, $ae,
    $ac, $a3, $a5, $95, $a9, $a7, $b6, $bc,
    $bd, $be, $dd, $a8, $af, $5d, $92, $d7,
    $7b, $41, $42, $43, $44, $45, $46, $47,
    $48, $49, $9b, $f4, $f6, $f2, $f3, $f5,
    $7d, $4a, $4b, $4c, $4d, $4e, $4f, $50,
    $51, $52, $b9, $fb, $fc, $f9, $fa, $ff,
    $5c, $f7, $53, $54, $55, $56, $57, $58,
    $59, $5a, $b2, $d4, $d6, $d2, $d3, $d5,
    $30, $31, $32, $33, $34, $35, $36, $37,
    $38, $39, $b3, $db, $dc, $d9, $da, $00
  );

  Print63Ascii: array [0..63] of Char = (
    ' ', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
    'H', 'I', Chr(189), '.', '<', '(', '+', '|',
    '&', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', '!', '$', '*', ')', ';', Chr(170),
    '-', '/', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', '\', ',', '%', '_', '>', '?',
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', ':', '#', '@', '''', '=', '"'
  );

  Print48Ascii: array[0..63] of Char = (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', '+', '&', '%', '#', '@', '''',
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', '.', '$', '*', '-', '/', ','
  );

  // Hollerith punch to ASCII translate table
  HollerithAscii: array [0..93] of TXLateItem = (
    (Hollerith: $31; Native: 'A' ),
    (Hollerith: $51; Native: 'B' ),
    (Hollerith: $11; Native: 'C' ),
    (Hollerith: $21; Native: 'D' ),
    (Hollerith: $41; Native: 'E' ),
    (Hollerith: $71; Native: 'F' ),
    (Hollerith: $61; Native: 'G' ),
    (Hollerith: $09; Native: 'H' ),
    (Hollerith: $81; Native: 'I' ),
    (Hollerith: $32; Native: 'J' ),
    (Hollerith: $52; Native: 'K' ),
    (Hollerith: $12; Native: 'L' ),
    (Hollerith: $22; Native: 'M' ),
    (Hollerith: $42; Native: 'N' ),
    (Hollerith: $72; Native: 'O' ),
    (Hollerith: $62; Native: 'P' ),
    (Hollerith: $0A; Native: 'Q' ),
    (Hollerith: $82; Native: 'R' ),
    (Hollerith: $54; Native: 'S' ),
    (Hollerith: $14; Native: 'T' ),
    (Hollerith: $24; Native: 'U' ),
    (Hollerith: $44; Native: 'V' ),
    (Hollerith: $74; Native: 'W' ),
    (Hollerith: $64; Native: 'X' ),
    (Hollerith: $0C; Native: 'Y' ),
    (Hollerith: $84; Native: 'Z' ),
    (Hollerith: $04; Native: '0' ),
    (Hollerith: $30; Native: '1' ),
    (Hollerith: $50; Native: '2' ),
    (Hollerith: $10; Native: '3' ),
    (Hollerith: $20; Native: '4' ),
    (Hollerith: $40; Native: '5' ),
    (Hollerith: $70; Native: '6' ),
    (Hollerith: $60; Native: '7' ),
    (Hollerith: $08; Native: '8' ),
    (Hollerith: $80; Native: '9' ),
    (Hollerith: $00; Native: ' ' ),
    (Hollerith: $01; Native: '&' ),
    (Hollerith: $02; Native: '-' ),
    (Hollerith: $34; Native: '/' ),
    (Hollerith: $05; Native: '{' ),
    (Hollerith: $35; Native: 'a' ),
    (Hollerith: $55; Native: 'b' ),
    (Hollerith: $15; Native: 'c' ),
    (Hollerith: $25; Native: 'd' ),
    (Hollerith: $45; Native: 'e' ),
    (Hollerith: $75; Native: 'f' ),
    (Hollerith: $65; Native: 'g' ),
    (Hollerith: $0D; Native: 'h' ),
    (Hollerith: $85; Native: 'i' ),
    (Hollerith: $03; Native: '|' ),
    (Hollerith: $33; Native: 'j' ),
    (Hollerith: $53; Native: 'k' ),
    (Hollerith: $13; Native: 'l' ),
    (Hollerith: $23; Native: 'm' ),
    (Hollerith: $43; Native: 'n' ),
    (Hollerith: $73; Native: 'o' ),
    (Hollerith: $63; Native: 'p' ),
    (Hollerith: $0B; Native: 'q' ),
    (Hollerith: $83; Native: 'r' ),
    (Hollerith: $06; Native: '}' ),
    (Hollerith: $36; Native: '~' ),
    (Hollerith: $56; Native: 's' ),
    (Hollerith: $16; Native: 't' ),
    (Hollerith: $26; Native: 'u' ),
    (Hollerith: $46; Native: 'v' ),
    (Hollerith: $76; Native: 'w' ),
    (Hollerith: $66; Native: 'x' ),
    (Hollerith: $0E; Native: 'y' ),
    (Hollerith: $86; Native: 'z' ),
    (Hollerith: $59; Native: '[' ),
    (Hollerith: $19; Native: '.' ),
    (Hollerith: $29; Native: '<' ),
    (Hollerith: $49; Native: '(' ),
    (Hollerith: $79; Native: '+' ),
    (Hollerith: $69; Native: '!' ),
    (Hollerith: $5A; Native: ']' ),
    (Hollerith: $2A; Native: '*' ),
    (Hollerith: $4A; Native: ')' ),
    (Hollerith: $7A; Native: ';' ),
    (Hollerith: $5C; Native: '\' ),
    (Hollerith: $1C; Native: ',' ),
    (Hollerith: $2C; Native: '%' ),
    (Hollerith: $4C; Native: '_' ),
    (Hollerith: $7C; Native: '>' ),
    (Hollerith: $6C; Native: '?' ),
    (Hollerith: $38; Native: '`' ),
    (Hollerith: $58; Native: ':' ),
    (Hollerith: $18; Native: '#' ),
    (Hollerith: $28; Native: '@' ),
    (Hollerith: $78; Native: '=' ),
    (Hollerith: $68; Native: '"' ),
    (Hollerith: $48; Native: '''' ),
    (Hollerith: $1A; Native: '$' )
   );
   // 12-bit Hollerith to XS3
   HollerithXS3 : array [0..63] of TXS3XlateItem = (
    ( Hollerith: HOLLERITH_12 or HOLLERITH_1; Native: X3_A ),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_2; Native: X3_B ),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_3; Native: X3_C ),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_4; Native: X3_D ),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_5; Native: X3_E ),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_6; Native: X3_F ),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_7; Native: X3_G ),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_8; Native: X3_H ),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_9; Native: X3_I ),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_1; Native: X3_J ),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_2; Native: X3_K ),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_3; Native: X3_L ),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_4; Native: X3_M ),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_5; Native: X3_N ),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_6; Native: X3_O ),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_7; Native: X3_P ),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_8; Native: X3_Q ),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_9; Native: X3_R ),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_2; Native: X3_S ),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_3; Native: X3_T ),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_4; Native: X3_U ),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_5; Native: X3_V ),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_6; Native: X3_W ),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_7; Native: X3_X ),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_8; Native: X3_Y ),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_9; Native: X3_Z ),
    ( Hollerith: HOLLERITH_0; Native: X3_0),
    ( Hollerith: HOLLERITH_1; Native: X3_1),
    ( Hollerith: HOLLERITH_2; Native: X3_2),
    ( Hollerith: HOLLERITH_3; Native: X3_3),
    ( Hollerith: HOLLERITH_4; Native: X3_4),
    ( Hollerith: HOLLERITH_5; Native: X3_5),
    ( Hollerith: HOLLERITH_6; Native: X3_6),
    ( Hollerith: HOLLERITH_7; Native: X3_7),
    ( Hollerith: HOLLERITH_8; Native: X3_8),
    ( Hollerith: HOLLERITH_9; Native: X3_9),
    ( Hollerith: HOLLERITH_12; Native: X3_AMP),
    ( Hollerith: HOLLERITH_11; Native: X3_MINUS),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_0; Native: X3_QUESTION),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_0; Native: X3_EXCLAM),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_1; Native: X3_SLASH),
    ( Hollerith: HOLLERITH_2 or HOLLERITH_8; Native: X3_PLUS),
    ( Hollerith: HOLLERITH_3 or HOLLERITH_8; Native: X3_SHARP),
    ( Hollerith: HOLLERITH_4 or HOLLERITH_8; Native: X3_AT),
    ( Hollerith: HOLLERITH_5 or HOLLERITH_8; Native: X3_COLON),
    ( Hollerith: HOLLERITH_6 or HOLLERITH_8; Native: X3_GREATER),
    ( Hollerith: HOLLERITH_7 or HOLLERITH_8; Native: X3_APOS),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_3 or HOLLERITH_8; Native: X3_PERIOD),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_4 or HOLLERITH_8; Native: X3_LOZENGE),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_5 or HOLLERITH_8; Native: X3_LEFT_SQUARE),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_6 or HOLLERITH_8; Native: X3_LESS),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_7 or HOLLERITH_8; Native: X3_EQUAL),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_3 or HOLLERITH_8; Native: X3_DOLLAR),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_4 or HOLLERITH_8; Native: X3_ASTERIX),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_5 or HOLLERITH_8; Native: X3_RIGHT_SQUARE),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_6 or HOLLERITH_8; Native: X3_SEMI),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_7 or HOLLERITH_8; Native: X3_DELTA),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_2 or HOLLERITH_8; Native: X3_NOT_EQUAL),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_3 or HOLLERITH_8; Native: X3_COMMA),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_4 or HOLLERITH_8; Native: X3_PERCENT),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_5 or HOLLERITH_8; Native: X3_LEFT_PAREN),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_6 or HOLLERITH_8; Native: X3_BACK_SLASH),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_7 or HOLLERITH_8; Native: X3_RIGHT_PAREN),
    ( Hollerith: 0; Native: X3_SPACE)
   );
   // 12-bit Hollerith to Fieldata
   HollerithFieldata : array [0..63] of TXS3XlateItem = (
    ( Hollerith: HOLLERITH_7 or HOLLERITH_8; Native: FD_AT),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_5 or HOLLERITH_8; Native: FD_LEFT_SQUARE),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_5 or HOLLERITH_8; Native: FD_RIGHT_SQUARE),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_7 or HOLLERITH_8; Native: FD_SHARP),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_5 or HOLLERITH_8; Native: FD_DELTA),
    ( Hollerith: 0; Native: FD_SPACE),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_1; Native: FD_A),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_2; Native: FD_B),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_3; Native: FD_C),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_4; Native: FD_D),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_5; Native: FD_E),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_6; Native: FD_F),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_7; Native: FD_G),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_8; Native: FD_H),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_9; Native: FD_I),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_1; Native: FD_J),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_2; Native: FD_K),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_3; Native: FD_L),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_4; Native: FD_M),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_5; Native: FD_N),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_6; Native: FD_O),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_7; Native: FD_P),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_8; Native: FD_Q),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_9; Native: FD_R),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_2; Native: FD_S),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_3; Native: FD_T),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_4; Native: FD_U),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_5; Native: FD_V),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_6; Native: FD_W),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_7; Native: FD_X),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_8; Native: FD_Y),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_9; Native: FD_Z),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_4 or HOLLERITH_8; Native: FD_RIGHT_PAREN),
    ( Hollerith: HOLLERITH_11; Native: FD_MINUS),
    ( Hollerith: HOLLERITH_12; Native: FD_PLUS),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_6 or HOLLERITH_8; Native: FD_LESS),
    ( Hollerith: HOLLERITH_3 or HOLLERITH_8; Native: FD_EQUAL),
    ( Hollerith: HOLLERITH_6 or HOLLERITH_8; Native: FD_GREATER),
    ( Hollerith: HOLLERITH_2 or HOLLERITH_8; Native: FD_AMP),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_3 or HOLLERITH_8; Native: FD_DOLLAR),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_4 or HOLLERITH_8; Native: FD_ASTERIX),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_4 or HOLLERITH_8; Native: FD_LEFT_PAREN),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_5 or HOLLERITH_8; Native: FD_PERCENT),
    ( Hollerith: HOLLERITH_5 or HOLLERITH_8; Native: FD_COLON),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_0; Native: FD_QUESTION),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_0; Native: FD_EXCLAMATION),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_3 or HOLLERITH_8; Native: FD_COMMA),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_6 or HOLLERITH_8; Native: FD_BACK_SLASH),
    ( Hollerith: HOLLERITH_0; Native: FD_0),
    ( Hollerith: HOLLERITH_1; Native: FD_1),
    ( Hollerith: HOLLERITH_2; Native: FD_2),
    ( Hollerith: HOLLERITH_3; Native: FD_3),
    ( Hollerith: HOLLERITH_4; Native: FD_4),
    ( Hollerith: HOLLERITH_5; Native: FD_5),
    ( Hollerith: HOLLERITH_6; Native: FD_6),
    ( Hollerith: HOLLERITH_7; Native: FD_7),
    ( Hollerith: HOLLERITH_8; Native: FD_8),
    ( Hollerith: HOLLERITH_9; Native: FD_9),
    ( Hollerith: HOLLERITH_4 or HOLLERITH_8; Native: FD_APOS),
    ( Hollerith: HOLLERITH_11 or HOLLERITH_6 or HOLLERITH_8; Native: FD_SEMI_COLON),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_1; Native: FD_SLASH),
    ( Hollerith: HOLLERITH_12 or HOLLERITH_3 or HOLLERITH_8; Native: FD_PERIOD),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_7 or HOLLERITH_8; Native: FD_LOZENGE),
    ( Hollerith: HOLLERITH_0 or HOLLERITH_2 or HOLLERITH_8; Native: FD_STOP)
   );

   XS3Ascii: array [0..63] of AnsiChar = (
//  Because certain special XS3 characters don't exist in ASCII, I had to subsitute
// '{' for X3_LOZENGE, '~' for X3_NOT_EQUAL and '^' for X3_TRIANGLE.
//   0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
    ' ', ']', '-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '\', ';', '[',
    '&', ':', '.', '?', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', '#', '<', '=',
    '''', '*', '$', '!', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', '(', '@', '^',
    '~', '%', ',', '+', '/', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '{', '>', ')'
   );

function ExeDir: String;
begin
    Result := ExtractFilePath(Application.ExeName);
end;

function CardFileDir: String;
var
    split: Integer;
begin
    Result := ExeDir;
    // If we are running in the development environment,
    // remove win32/debug from the end of the path
    split := Pos('win32\debug', LowerCase(Result));
    if (split > 0) then
        Result := Copy(Result, 1, split - 1);
    Result := Result + 'CardFiles\';
end;

function GetDir(csidl: Integer): String;
var
    stat: Integer;
    path: array [0 .. MAX_PATH] of Char;
begin
    stat := ShGetFolderPath(0, csidl, 0, SHGFP_TYPE_CURRENT, path);
    if (stat = 0) then
    begin
        SetString(Result, path, StrLen(path));
        if (Result[Length(Result)] = '\') then
            Result := Copy(Result, 1, Length(Result) - 1);
    end
    else
    begin
        Result := '.';
    end;
end;

function UserDataDir: String;
begin
    Result := GetDir(CSIDL_APPDATA);
end;

{ TCodeTranslator }

class function TCodeTranslator.AsciiToEbcdic(B: Byte): AnsiChar;
begin
    Result := AnsiChar(AsciiEbcdic[b]);
end;

class function TCodeTranslator.AsciiToEbcdic(s: AnsiString): AnsiString;
var
    c: AnsiChar;
begin
    Result := '';
    for c in s do
        Result := Result + AnsiChar(AsciiEbcdic[Byte(c)]);
end;

class function TCodeTranslator.AsciiToFieldata(s: AnsiString): AnsiString;
var
    c: AnsiChar;
begin
    Result := '';
    for c in s do
        Result := Result + AsciiToFieldata(c);
end;

class function TCodeTranslator.AsciiToFieldata(c: AnsiChar): AnsiChar;
var
    i: Integer;
begin
    i := Ord(c);
    if ((i >= Low(AsciiFieldata)) and (i <= High(AsciiFieldata))) then
        Result := AnsiChar(AsciiFieldata[i])
    else
        Result := AnsiChar(AsciiFieldata[Ord(' ')]);
end;

class function TCodeTranslator.AsciiToHollerith8(c: AnsiChar): Byte;
var
    item: TXlateItem;
begin
    Result := 0;
    for item in HollerithAscii do
    begin
        if (item.Native = c) then
        begin
            Result := item.Hollerith;
            Exit;
        end;
    end;
end;

class function TCodeTranslator.AsciiToXS3(c: AnsiChar): Byte;
var
    i: Integer;
begin
    for i := Low(XS3Ascii) to High(XS3Ascii) do
    begin
        if (XS3Ascii[i] = c) then
        begin
            Result := Byte(i);
            Exit;
        end;
    end;
    Result := X3_SPACE;
end;

class function TCodeTranslator.EbcdicToAscii(B: Byte): AnsiChar;
begin
    Result := AnsiChar(EbcdicAscii[b]);
end;

class function TCodeTranslator.EbcdicToAscii(s: AnsiString): AnsiString;
var
    c: AnsiChar;
begin
    Result := '';
    for c in s do
        Result := Result + AnsiChar(EbcdicAscii[Byte(c)]);
end;

class function TCodeTranslator.FieldataToAscii(b: Byte): AnsiChar;
begin
    Result := ' ';
    if (b <= 63) then
        Result := FieldataAscii[b];
end;

class function TCodeTranslator.FieldataToAscii(s: AnsiString): AnsiString;
var
    c: AnsiChar;
begin
    Result := '';
    for c in s do
    begin
        Result := Result + FieldataToAscii(Ord(c));
    end;
end;

class function TCodeTranslator.FieldataToHollerith12(c: AnsiChar): Word;
var
    xi: TXS3XlateItem;
begin
    for xi in HollerithFieldata do
    begin
        if (xi.Native = Ord(c)) then
        begin
            Result := xi.Hollerith;
            Exit;
        end;
    end;
    Result := 0;
end;

class function TCodeTranslator.Hollerith8ToAscii(b: Byte): AnsiChar;
var
    item: TXlateItem;
begin
    Result := ' ';
    for item in HollerithAscii do
    begin
        if (item.Hollerith = b) then
        begin
            Result := item.Native;
            Exit;
        end;
    end;
end;

class function TCodeTranslator.Hollerith12ToFieldata(w: Word): Byte;
var
    xi: TXS3XlateItem;
begin
    for xi in HollerithFieldata do
    begin
        if (xi.Hollerith = w) then
        begin
            Result := xi.Native;
            Exit;
        end;
    end;
    Result := X3_SPACE;
end;

class function TCodeTranslator.Hollerith12ToHollerith8(w: Word): Byte;
var
    b: Byte;
begin
    b := 0;
    if ((w and $8000) <> 0) then            // 12 punch
        b := b or $01;
    if ((w and $4000) <> 0) then            // 11 punch
        b := b or $02;
    if ((w and $2000) <> 0) then            // 0 punch
        b := b or $04;
    if ((w and $1000) <> 0) then            // 1 punch
        b := b or $30;
    if ((w and $0800) <> 0) then            // 2 punch
        b := b or $50;
    if ((w and $0400) <> 0) then            // 3 punch
        b := b or $10;
    if ((w and $0200) <> 0) then            // 4 punch
        b := b or $20;
    if ((w and $0100) <> 0) then            // 5 punch
        b := b or $40;
    if ((w and $0080) <> 0) then            // 6 punch
        b := b or $70;
    if ((w and $0040) <> 0) then            // 7 punch
        b := b or $60;
    if ((w and $0020) <> 0) then            // 8 punch
        b := b or $08;
    if ((w and $0010) <> 0) then            // 9 punch
        b := b or $80;
    Result := b;
end;

class function TCodeTranslator.Hollerith12ToXS3(w: Word): Byte;
var
    xi: TXS3XlateItem;
begin
    for xi in HollerithXS3 do
    begin
        if (xi.Hollerith = w) then
        begin
            Result := xi.Native;
            Exit;
        end;
    end;
    Result := X3_SPACE;
end;

class function TCodeTranslator.Hollerith8ToAscii(bfr: array of Byte): AnsiString;
var
    b: Byte;
begin
    Result := '';
    for b in bfr do
    begin
        Result := Result + Hollerith8ToAscii(b);
    end;
end;

class function TCodeTranslator.Hollerith8ToHollerith12(b: Byte): WORD;
var
    w: WORD;
    test: Byte;
begin
    w := 0;
    if ((b and $01) = $01) then             // 12 punch
        w := w or $8000;
    if ((b and $02) = $02) then             // 11 punch
        w := w or $4000;
    if ((b and $04) = $04) then             // 0 punch
        w := w or $2000;
    test := b and $70;
    { TODO : change this to a case statement }
    if (test = $30) then                    // 1 punch
        w := w or $1000
    else if (test = $50) then               // 2 punch
        w := w or $0800
    else if (test = $10) then               // 3 punch
        w := w or $0400
    else if (test = $20) then               // 4 punch
        w := w or $0200
    else if (test = $40) then               // 5 punch
        w := w or $0100
    else if (test = $70) then               // 6 punch
        w := w or $0080
    else if (test = $60) then               // 7 punch
        w := w or $0040;
    if ((b and $08) = $08) then             // 8 punch
        w := w or $0020;
    if ((b and $80) = $80) then             // 9 punch
        w := w or $0010;
    Result := w;
end;

class function TCodeTranslator.Printer16ToAscii(b: Byte): AnsiChar;
begin
    Result := Printer48ToAscii(b);
end;

class function TCodeTranslator.Printer16ToAscii(bfr: array of Byte): AnsiString;
begin
    Result := Printer48ToAscii(bfr);
end;

class function TCodeTranslator.Printer48ToAscii(b: Byte): AnsiChar;
begin
    Result := AnsiChar(Print48Ascii[b and $3F]);
end;

class function TCodeTranslator.Printer48ToAscii(bfr: array of Byte): AnsiString;
var
    b: Byte;
begin
    Result := '';
    for b in bfr do
    begin
        Result := Result + AnsiChar(Print48Ascii[b and $3F]);
    end;
end;

class function TCodeTranslator.Printer63ToAscii(b: Byte): AnsiChar;
begin
    Result := AnsiChar(Print63Ascii[b and $3F]);
end;

class function TCodeTranslator.Printer63ToAscii(bfr: array of Byte): AnsiString;
var
    b: Byte;
begin
    Result := '';
    for b in bfr do
    begin
        Result := Result + AnsiChar(Print63Ascii[b and $3F]);
    end;
end;

class function TCodeTranslator.XS3ToAscii(b: Byte): AnsiChar;
begin
    Result := XS3Ascii[b and $3f];
end;

class function TCodeTranslator.XS3ToAscii(bfr: array of Byte): AnsiString;
var
    b: Byte;
begin
    Result := '';
    for b in bfr do
        Result := Result + XS3ToAscii(b);
end;

class function TCodeTranslator.XS3ToHollerith12(b: Byte): Word;
var
    xi: TXS3XlateItem;
begin
    for xi in HollerithXS3 do
    begin
        if (xi.Native = b) then
        begin
            Result := xi.Hollerith;
            Exit;
        end;
    end;
    Result := 0;
end;

{ TCarriageControlTape }

constructor TCarriageControlTape.Create(lpi: Byte);
begin
    inherited Create;
    Self.lpi := lpi;
end;

procedure TCarriageControlTape.MakeDefaultTape;
var
    i: Integer;
begin
    Clear;
    // Initialize a default carriage control tape for the given LPI
    for i := 0 to (FLPI * 11) - 1 do
        Add(0);
    Items[Round(FLPI * 0.5) - 2] := CHANNEL7; // Set channel 7 (top of page) at 0.5" from top
    Items[Round((FLPI * 10.5)) - 1] := CHANNEL1; // Set channel 1 (overflow) at 0.5" from bottom
end;

procedure TCarriageControlTape.SetLPI(const Value: Byte);
begin
    FLPI := Value;
    MakeDefaultTape;
end;

{ TPrinterBitmap }

procedure TPrinterBitmap.Clear;
begin
    Lock;
    try
        Canvas.Brush.Color := clWhite;
        Canvas.Brush.Style := bsSolid;
        Canvas.FillRect(Rect(0, 0, Width - 1, Height - 1));
    finally
        Unlock
    end;
end;

constructor TPrinterBitmap.Create(lpi: Byte; ppi: Integer);
begin
    inherited Create;
    FLock := TCriticalSection.Create;
    Lock;
    try
        FLPI := lpi;
        FPPI := ppi;
        Monochrome := True;
        Height := 11 * FPPI; // 11x15
        Width := 15 * FPPI;
        Canvas.Font.Name := 'Courier New';
        Canvas.Font.Style := [fsBold];
        Canvas.Font.Height := FPPI div FLPI; // font size for 8 LPI
    finally
        Unlock;
    end;
end;

destructor TPrinterBitmap.Destroy;
begin
    FreeAndNil(FLock);
    inherited;
end;

procedure TPrinterBitmap.Lock;
begin
    FLock.Enter;
    Canvas.Lock;
end;

procedure TPrinterBitmap.Print(lineNum: Integer; text: AnsiString);
var
    r: TRect;
    h: Integer;
    s: String;
begin
    Lock;
    try
        h := FPPI div FLPI;
        r := Rect(150, lineNum * h, 3149, ((lineNum + 1) * h) - 1);
        Canvas.Brush.Style := bsClear;
        s := String(text);
        Canvas.TextRect(r, s, [tfNoPrefix]);
    finally
        Unlock;
    end;
end;

procedure TPrinterBitmap.SendToPrinter;
begin
    Lock;
    try
        { TODO : Don't know why this is needed but it is or nothing prints. }
        SaveToFile(Format('c:\temp\u92printer_%d.bmp', [FPageNumber]));
        Inc(FPageNumber);
        Printer.Orientation := poLandscape;
        Printer.BeginDoc;
        Printer.Canvas.Lock;
        try
            Printer.Canvas.StretchDraw(Rect(0, 0, Printer.PageWidth - 1, Printer.PageHeight - 1),
              Self);
        finally
            Printer.EndDoc;
            Printer.Canvas.Unlock;
        end;
    finally
        Unlock;
    end;
end;

procedure TPrinterBitmap.SetLPI(const Value: Byte);
begin
    FLPI := Value;
    Lock;
    try
        Canvas.Font.Height := FPPI div FLPI; // font size for current LPI
    finally
        Unlock;
    end;
end;

procedure TPrinterBitmap.Unlock;
begin
    FLock.Leave;
    Canvas.Unlock;
end;
end.
