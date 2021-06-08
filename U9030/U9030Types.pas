unit U9030Types;

interface

uses Windows, WinApi.SHFolder, Messages, SysUtils, Classes, Generics.Collections,
     Generics.Defaults, Forms,
     EmulatorTypes;

type
  EMachineCheck = class(Exception)
  public
    IntCode: Byte;
  end;

  EProgramException = class(Exception)
  public
    IntCode: Byte;
  end;

  EOperationException = class(EProgramException)
  public
    constructor Create(const Msg: String); reintroduce;
  end;

  EAddressException = class(EProgramException)
  public
    constructor Create(const Msg: String); reintroduce;
  end;

  EProtectionException = class(EProgramException)
  public
    constructor Create(const Msg: String); reintroduce;
  end;

  ESpecificationException = class(EProgramException)
  public
    constructor Create(const Msg: String); reintroduce;
  end;

  EIllegalOpcode = class(EProgramException)
  public
    constructor Create(const Msg: String); reintroduce;
  end;

  EPrivilegedInst = class(EProgramException)
  public
    constructor Create(const Msg: String); reintroduce;
  end;

  EFixedOverflow = class(EProgramException)
  public
    constructor Create(const Msg: String); reintroduce;
  end;

  EFixedDivideException = class(EProgramException)
  public
    constructor Create(const Msg: String); reintroduce;
  end;

  EExecuteException = class(EProgramException)
  public
    constructor Create(const Msg: String); reintroduce;
  end;

  EDataException = class(EProgramException)
  public
    constructor Create(const Msg: String); reintroduce;
  end;

  EDecimalOverflow = class(EProgramException)
  public
    constructor Create(const Msg: String); reintroduce;
  end;

  EDecimalDivideException = class(EProgramException)
  public
    constructor Create(const Msg: String); reintroduce;
  end;

  TMemoryAddress = UInt32;
  THalfWord = Int16;
  TWord = Int32;
  TDblWord = Int64;

  TAlignment = ( aHalfWord = 2, aWord = 4, aDblWord = 8 );

  TProcessorStates = ( psHalted, psError, psSingleStep );
  TProcessorState = set of TProcessorStates;

  TOpcodeProc = procedure of object;

  TInstType = ( itDirective, itRR, itRS, itRX, itSI, itSS1, itSS2, itBranch, itUnknown );

  TOpcode = class
  public
    Opcode: Byte;
    Length: Byte;
    Privileged: Boolean;
    Proc: TOpcodeProc;
    InstType: TInstType;
    Code: String;
  end;

  TOpcodeList = class
  private
    FOpcodes: array [0..255] of TOpcode;
    function GetOpcode(op: Byte): TOpcode;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(opcode: TOpcode);
    function FindOpcode(op: Byte): TOpcode; overload;
    function FindOpcode(code: String): TOpcode; overload;
    function IsOpcode(op: Byte): Boolean;
    property Opcodes[op: Byte]: TOpcode read GetOpcode; default;
  end;

  function DataDir: String;
  function Opcode(op, length: Byte;
                  proc: TOpcodeProc;
                  code: String = '';
                  typ: TInstType = itUnknown;
                  priv: Boolean = False): TOpcode;
  function WinError: String;

implementation

function DataDir: String;
begin
    Result := UserDataDir + '\U9030';
    if (not DirectoryExists(Result)) then
        ForceDirectories(Result);
end;

function Opcode(op, length: Byte;
                proc: TOpcodeProc;
                code: String;
                typ: TInstType;
                priv: Boolean): TOpcode;
begin
    Result := TOpcode.Create;
    Result.Opcode := op;
    Result.Length := length;
    Result.Privileged := priv;
    Result.Proc := proc;
    Result.InstType := typ;
    Result.Code := code;
end;

// Return the text of the most recent Windows error message
function WinError: String;
var
    stat: Integer;
    msg: Pointer;
begin
    stat := GetLastError;
    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or
                  FORMAT_MESSAGE_FROM_SYSTEM or
                  FORMAT_MESSAGE_IGNORE_INSERTS,
                  nil,
                  stat,
                  0, // Default language
                  PChar(@msg),
                  0,
                  nil);
    SetString(Result, PChar(msg), StrLen(PChar(msg)));
    LocalFree(HLOCAL(msg));
end;

{ TU92OpcodeList }

procedure TOpcodeList.Add(opcode: TOpcode);
begin
    FOpcodes[opcode.Opcode] := opcode;
end;

constructor TOpcodeList.Create;
begin
    Add(Opcode($03, 2, nil, 'STR', itRR, True));
    Add(Opcode($04, 2, nil, 'SPM', itRR));
    Add(Opcode($05, 2, nil, 'BALR', itRR));
    Add(Opcode($06, 2, nil, 'BCTR', itRR));
    Add(Opcode($07, 2, nil, 'BCR', itRR));
    Add(Opcode($08, 2, nil, 'SSK', itRR, True));
    Add(Opcode($09, 2, nil, 'ISK', itRR, True));
    Add(Opcode($0A, 2, nil, 'SVC', itRR));
    Add(Opcode($0D, 2, nil, 'BASR', itRR));
    Add(Opcode($10, 2, nil, 'LPR', itRR));
    Add(Opcode($11, 2, nil, 'LNR', itRR));
    Add(Opcode($12, 2, nil, 'LTR', itRR));
    Add(Opcode($13, 2, nil, 'LCR', itRR));
    Add(Opcode($14, 2, nil, 'NR', itRR));
    Add(Opcode($15, 2, nil, 'CLR', itRR));
    Add(Opcode($16, 2, nil, 'OR', itRR));
    Add(Opcode($17, 2, nil, 'XR', itRR));
    Add(Opcode($18, 2, nil, 'LR', itRR));
    Add(Opcode($19, 2, nil, 'CR', itRR));
    Add(Opcode($1A, 2, nil, 'AR', itRR));
    Add(Opcode($1B, 2, nil, 'SR', itRR));
    Add(Opcode($1C, 2, nil, 'MR', itRR));
    Add(Opcode($1D, 2, nil, 'DR', itRR));
    Add(Opcode($1E, 2, nil, 'ALR', itRR));
    Add(Opcode($1F, 2, nil, 'SLR', itRR));
    Add(Opcode($20, 2, nil, 'LDPR', itRR));
    Add(Opcode($21, 2, nil, 'LNDR', itRR));
    Add(Opcode($22, 2, nil, 'LTDR', itRR));
    Add(Opcode($23, 2, nil, 'LDCR', itRR));
    Add(Opcode($24, 2, nil, 'HDR', itRR));
    Add(Opcode($28, 2, nil, 'LDR', itRR));
    Add(Opcode($29, 2, nil, 'CDR', itRR));
    Add(Opcode($2A, 2, nil, 'ADR', itRR));
    Add(Opcode($2B, 2, nil, 'SDR', itRR));
    Add(Opcode($2C, 2, nil, 'MDR', itRR));
    Add(Opcode($2D, 2, nil, 'DDR', itRR));
    Add(Opcode($2E, 2, nil, 'AWR', itRR));
    Add(Opcode($2F, 2, nil, 'SWR', itRR));
    Add(Opcode($30, 2, nil, 'LPER', itRR));
    Add(Opcode($31, 2, nil, 'LNER', itRR));
    Add(Opcode($32, 2, nil, 'LTER', itRR));
    Add(Opcode($33, 2, nil, 'LCER', itRR));
    Add(Opcode($34, 2, nil, 'HER', itRR));
    Add(Opcode($38, 2, nil, 'LER', itRR));
    Add(Opcode($39, 2, nil, 'CER', itRR));
    Add(Opcode($3A, 2, nil, 'AER', itRR));
    Add(Opcode($3B, 2, nil, 'SER', itRR));
    Add(Opcode($3C, 2, nil, 'MER', itRR));
    Add(Opcode($3D, 2, nil, 'DER', itRR));
    Add(Opcode($3E, 2, nil, 'AUR', itRR));
    Add(Opcode($3F, 2, nil, 'SUR', itRR));
    Add(Opcode($40, 4, nil, 'STH', itRX));
    Add(Opcode($41, 4, nil, 'LA', itRX));
    Add(Opcode($42, 4, nil, 'STC', itRX));
    Add(Opcode($43, 4, nil, 'IC', itRX));
    Add(Opcode($44, 4, nil, 'EX', itRX));
    Add(Opcode($45, 4, nil, 'BAL', itRX));
    Add(Opcode($46, 4, nil, 'BCT', itRX));
    Add(Opcode($47, 4, nil, 'BC', itBranch));
    Add(Opcode($48, 4, nil, 'LH', itRX));
    Add(Opcode($49, 4, nil, 'CH', itRX));
    Add(Opcode($4A, 4, nil, 'AH', itRX));
    Add(Opcode($4B, 4, nil, 'SH', itRX));
    Add(Opcode($4C, 4, nil, 'MH', itRX));
    Add(Opcode($4D, 4, nil, 'BAS', itRX));
    Add(Opcode($4E, 4, nil, 'CVD', itRX));
    Add(Opcode($4F, 4, nil, 'CVB', itRX));
    Add(Opcode($50, 4, nil, 'ST', itRX));
    Add(Opcode($54, 4, nil, 'N', itRX));
    Add(Opcode($55, 4, nil, 'CL', itRX));
    Add(Opcode($56, 4, nil, 'O', itRX));
    Add(Opcode($57, 4, nil, 'X', itRX));
    Add(Opcode($58, 4, nil, 'L', itRX));
    Add(Opcode($59, 4, nil, 'C', itRX));
    Add(Opcode($5A, 4, nil, 'A', itRX));
    Add(Opcode($5B, 4, nil, 'S', itRX));
    Add(Opcode($5C, 4, nil, 'M', itRX));
    Add(Opcode($5D, 4, nil, 'D', itRX));
    Add(Opcode($5E, 4, nil, 'AL', itRX));
    Add(Opcode($60, 4, nil, 'STD', itRX));
    Add(Opcode($68, 4, nil, 'LD', itRX));
    Add(Opcode($69, 4, nil, 'CD', itRX));
    Add(Opcode($6A, 4, nil, 'AD', itRX));
    Add(Opcode($6B, 4, nil, 'SD', itRX));
    Add(Opcode($6C, 4, nil, 'MD', itRX));
    Add(Opcode($6D, 4, nil, 'DD', itRX));
    Add(Opcode($6E, 4, nil, 'AW', itRX));
    Add(Opcode($6F, 4, nil, 'SW', itRX));
    Add(Opcode($70, 4, nil, 'STE', itRX));
    Add(Opcode($78, 4, nil, 'LE', itRX));
    Add(Opcode($79, 4, nil, 'CE', itRX));
    Add(Opcode($7A, 4, nil, 'AE', itRX));
    Add(Opcode($7B, 4, nil, 'SE', itRX));
    Add(Opcode($7C, 4, nil, 'ME', itRX));
    Add(Opcode($7D, 4, nil, 'DE', itRX));
    Add(Opcode($7E, 4, nil, 'AU', itRX));
    Add(Opcode($7F, 4, nil, 'SU', itRX));
    Add(Opcode($80, 4, nil, 'SSM', itSI, True));
    Add(Opcode($82, 4, nil, 'LPSW', itSI, True));
    Add(Opcode($83, 4, nil, 'DIAG', itSI, True));
    Add(Opcode($86, 4, nil, 'BXH', itRS));
    Add(Opcode($87, 4, nil, 'BXLE', itRS));
    Add(Opcode($88, 4, nil, 'SRL', itRS));
    Add(Opcode($89, 4, nil, 'SLL', itRS));
    Add(Opcode($8A, 4, nil, 'SRA', itRS));
    Add(Opcode($8B, 4, nil, 'SLA', itRS));
    Add(Opcode($8C, 4, nil, 'SRDL', itRS));
    Add(Opcode($8D, 4, nil, 'SLDL', itRS));
    Add(Opcode($8E, 4, nil, 'SRDA', itRS));
    Add(Opcode($8F, 4, nil, 'SLDA', itRS));
    Add(Opcode($90, 4, nil, 'STM', itRS));
    Add(Opcode($91, 4, nil, 'TM', itSI));
    Add(Opcode($92, 4, nil, 'MVI', itSI));
    Add(Opcode($93, 4, nil, 'TS', itSI));
    Add(Opcode($94, 4, nil, 'NI', itSI));
    Add(Opcode($95, 4, nil, 'CLI', itSI));
    Add(Opcode($96, 4, nil, 'OI', itSI));
    Add(Opcode($97, 4, nil, 'XI', itSI));
    Add(Opcode($98, 4, nil, 'LM', itRS));
    Add(Opcode($99, 4, nil, 'HPR', itSI, True));
    Add(Opcode($9A, 4, nil, 'AI', itSI));
    Add(Opcode($9C, 4, nil, 'SIO', itSI, True));
    Add(Opcode($A2, 4, nil, 'SSFS', itRS, True));
    Add(Opcode($A3, 4, nil, 'SSRS', itRS, True));
    Add(Opcode($A6, 4, nil, 'AI', itSI));
    Add(Opcode($A9, 4, nil, 'HPR', itSI, True));
    Add(Opcode($AA, 4, nil, 'AH', itRS));
    Add(Opcode($AB, 4, nil, 'SH', itRS));
    Add(Opcode($B0, 4, nil, 'SSTM', itRS, True));
    Add(Opcode($B1, 4, nil, 'LCS', itRS, True));
    Add(Opcode($B8, 4, nil, 'SLM', itRS, True));
    Add(Opcode($D1, 6, nil, 'MVN', itSS1));
    Add(Opcode($D2, 6, nil, 'MVC', itSS1));
    Add(Opcode($D3, 6, nil, 'MVZ', itSS1));
    Add(Opcode($D4, 6, nil, 'NC', itSS1));
    Add(Opcode($D5, 6, nil, 'CLC', itSS1));
    Add(Opcode($D6, 6, nil, 'OC', itSS1));
    Add(Opcode($D7, 6, nil, 'XC', itSS1));
    Add(Opcode($DC, 6, nil, 'TR', itSS1));
    Add(Opcode($DD, 6, nil, 'TRT', itSS1));
    Add(Opcode($DE, 6, nil, 'ED', itSS1));
    Add(Opcode($DF, 6, nil, 'EDMK', itSS1));
    Add(Opcode($F1, 6, nil, 'MVO', itSS2));
    Add(Opcode($F2, 6, nil, 'PACK', itSS2));
    Add(Opcode($F3, 6, nil, 'UNPK', itSS2));
    Add(Opcode($F8, 6, nil, 'ZAP', itSS2));
    Add(Opcode($F9, 6, nil, 'CP', itSS2));
    Add(Opcode($FA, 6, nil, 'AP', itSS2));
    Add(Opcode($FB, 6, nil, 'SP', itSS2));
    Add(Opcode($FC, 6, nil, 'MP', itSS2));
    Add(Opcode($FD, 6, nil, 'DP', itSS2));
end;

destructor TOpcodeList.Destroy;
var
    i: Integer;
begin
    for i := 0 to 255 do
        FreeAndNil(FOpcodes[i]);
    inherited Destroy;
end;

function TOpcodeList.FindOpcode(code: String): TOpcode;
var
    op: TOpcode;
begin
    for op in FOpcodes do
    begin
        if (Assigned(op) and (op.Code = code)) then
        begin
            Result := op;
            Exit;
        end;
    end;
    raise EIllegalOpcode.Create('Illegal opcode');
end;

function TOpcodeList.FindOpcode(op: Byte): TOpcode;
begin
    Result := FOpcodes[op];
    if (not Assigned(Result)) then
        raise EIllegalOpcode.Create('Illegal opcode');
end;

function TOpcodeList.GetOpcode(op: Byte): TOpcode;
begin
    Result := FOpcodes[op];
end;

function TOpcodeList.IsOpcode(op: Byte): Boolean;
begin
    try
        FindOpcode(op);
        Result := True;
    except
        Result := False;
    end;
end;

{ EAddressException }

constructor EAddressException.Create(const Msg: String);
begin
    inherited;
    IntCode := 5;
end;

{ EIllegalOpcode }

constructor EIllegalOpcode.Create(const Msg: String);
begin
    inherited;
    IntCode := 1;
end;

{ EPrivilegedInst }

constructor EPrivilegedInst.Create(const Msg: String);
begin
    inherited;
    IntCode := 2;
end;

{ EOperationException }

constructor EOperationException.Create(const Msg: String);
begin
    inherited;
    IntCode := 1;
end;

{ EProtectionException }

constructor EProtectionException.Create(const Msg: String);
begin
    inherited;
    IntCode := 4;
end;

{ ESpecificationException }

constructor ESpecificationException.Create(const Msg: String);
begin
    inherited;
    IntCode := 6;
end;

{ EFixedOverflowException }

constructor EFixedOverflow.Create(const Msg: String);
begin
    inherited;
    IntCode := 8;
end;

{ EFixedDivideException }

constructor EFixedDivideException.Create(const Msg: String);
begin
    inherited;
    IntCode := 9;
end;

{ EExecuteException }

constructor EExecuteException.Create(const Msg: String);
begin
    inherited;
    IntCode := 3;
end;

{ EDataException }

constructor EDataException.Create(const Msg: String);
begin
    inherited;
    IntCode := 7;
end;

{ EDecimalOverflow }

constructor EDecimalOverflow.Create(const Msg: String);
begin
    inherited;
    IntCode := 10;
end;

{ EDecimalDivideException }

constructor EDecimalDivideException.Create(const Msg: String);
begin
    inherited;
    IntCode := 11;
end;

end.
