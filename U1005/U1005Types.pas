unit U1005Types;

interface

uses SysUtils, Generics.Defaults, Generics.Collections;

type
  EProcessorError = class(Exception)
  end;

  EMemoryError = class(Exception)
  end;

  EReaderError = class(Exception)
  end;

  EPunchError = class(Exception)
  end;

  T1005States = ( usPowerOff, usPowerOn, usInitializing );
  T1005State = set of T1005States;

  T1005InstType = (
    // Federal Systems instruction types
   itClass1, itClass2, itClass3, itClass4, itShift, itJump, itCharacter, itProc,
   // Commercial instruction types
   it1Operand, it2Operand, itEdit,
   //
   itUnknown
  );

  T1005CPUStates = ( ucsSingleStep, ucsInstructionFetched, ucsHalted, ucsError );
  T1005CPUState = set of T1005CPUStates;

  T1005SystemType = ( stFedSys, stComm );

  TDirectiveProc = procedure of Object;
  TErrorEvent = procedure(Sender: TObject; E: Exception) of object;
  TDebugEvent = procedure(Sender: TObject; E: Exception) of object;
  TExecuteInstructionEvent = procedure(Sender: TObject) of object;
  TFetchInstructionEvent = procedure(Sender: TObject) of object;

  T1005OperandType = (otAddress, otOctal, otConstant, otIgnore );

  T1005OpCode = record
  public
    OpCode: Byte;
    OpCode2: Byte;
    Code: String;
    InstType: T1005InstType;
    Indirect: Boolean;

    FldA: T1005OperandType;
    FldB: T1005OperandType;
    FldC: T1005OperandType;
    Proc: TDirectiveProc;
  end;

    T1005OpCodeList = class(TList<T1005OpCode>)
  public
    procedure AddOpcode(opcode: T1005OpCode);
    function FindOpcode(opcode, opcode2: Byte): T1005Opcode;
    function IsOpcode(opcode, opcode2: Byte): Boolean;
  end;

  // 1005 address interface definition
  I1005Addr = interface(IInterface)
    procedure AdjustBankAsc(msl: I1005Addr);
    procedure AdjustBankDesc(msl: I1005Addr);
    procedure Assign(addr: I1005Addr);
    procedure Clear;
    function Clone: I1005Addr;
    function Compare(op2: I1005Addr): Integer;
    procedure Decode(var abank, arow, acol: Byte);
    procedure Decrement(count: Integer = 1);
    function GetCol: Byte;
    function GetRow: Byte;
    procedure Increment(count: Integer = 1);
    function NewObject: I1005Addr;
    function GetSequentialAddr: Integer;
    function Matches(op2: I1005Addr): Boolean;
    procedure SetAddr(row, col: Byte); overload;
    procedure SetAddr(abank, arow, acol: Byte); overload;
    procedure SetAddr(addr: Integer); overload;
    property Col: Byte read GetCol;
    property Row: Byte read GetRow;
    property SequentialAddr: Integer read GetSequentialAddr;
  end;

  // Federal systems (military) version of 1005
  T1005FedSysAddr = class(TInterfacedObject, I1005Addr)
  private
    FRow: Byte;
    FCol: Byte;
  public
    procedure AdjustBankAsc(msl: I1005Addr); virtual;
    procedure AdjustBankDesc(msl: I1005Addr); virtual;
    procedure Assign(addr: I1005Addr); virtual;
    procedure Clear; virtual;
    function Clone: I1005Addr; virtual;
    function Compare(op2: I1005Addr): Integer; virtual;
    procedure Decode(var abank, arow, acol: Byte); virtual;
    procedure Decrement(count: Integer = 1); virtual;
    function GetCol: Byte;
    function GetRow: Byte;
    procedure Increment(count: Integer = 1); virtual;
    function NewObject: I1005Addr; virtual;
    function GetSequentialAddr: Integer; virtual;
    function Matches(op2: I1005Addr): Boolean; virtual;
    procedure SetAddr(row, col: Byte); overload;  virtual;
    procedure SetAddr(abank, arow, acol: Byte); overload; virtual;
    procedure SetAddr(addr: Integer); overload;  virtual;
    property Row: Byte read FRow;
    property Col: Byte read FCol;
    property SequentialAddr: Integer read GetSequentialAddr;
  end;

  // Commercial version of 1005. The only difference to the Federal Systems
  // version seems to be in the bank descriptor bits.
  T1005CommAddr = class(T1005FedSysAddr)
  public
    function Clone: I1005Addr; override;
    procedure Decode(var abank, arow, acol: Byte); override;
    function Matches(op2: I1005Addr): Boolean; override;
    function NewObject: I1005Addr; override;
    procedure SetAddr(abank, arow, acol: Byte); override;
  end;

  T1005FedSysOperand = record
  private
    FM: I1005Addr;
    FL: I1005Addr;
  public
    procedure GetL(var bank, row, col: Byte);
    procedure GetM(var bank, row, col: Byte);
    procedure Init;
    procedure SetAddr(m: I1005Addr; l: I1005Addr); overload;
    procedure SetAddr(mrow, mcol, lrow, lcol: Byte); overload;
    procedure SetAddr(bank, mrow, mcol, lrow, lcol: Byte); overload;
    property L: I1005Addr read FL write FL;
    property M: I1005Addr read FM write FM;
  end;

  T1005CommOperands = record
  private
    FFlda: I1005Addr;
    FFldb: I1005Addr;
    FFldc: I1005Addr;
  public
    procedure Init;
    property Flda: I1005Addr read FFlda write FFlda;
    property Fldb: I1005Addr read FFldb write FFldb;
    property Fldc: I1005Addr read FFldc write FFldc;
  end;

function Opcode(op: Byte; code: String; it: T1005InstType = itUnknown; ind: Boolean = False; proc: TDirectiveProc = nil): T1005Opcode; overload;
function Opcode(op: Byte; code: String; it: T1005InstType; ind: Boolean; igna, ignb, ignc: T1005OperandType; proc: TDirectiveProc = nil): T1005Opcode; overload;
function Opcode(op, op2: Byte; code: String; it: T1005InstType = itUnknown; proc: TDirectiveProc = nil): T1005Opcode; overload;
function FedSysOperand: T1005FedSysOperand; overload;
function FedSysOperand(mrow, mcol, lrow, lcol: Byte): T1005FedSysOperand; overload;
function FedSysOperand(m, l: I1005Addr): T1005FedSysOperand; overload;

implementation

uses EmulatorTypes;

function NativeToRowCol(val: Byte): Byte;
// Translate a 1005 row or column number to its integer equivalent.
const
    xlate: array[0..31] of Byte = (
        1,                          // ' ' $00
        2,                          // ']' $01
        12,                         // '-' $02
        3,                          // '0' $03
        9,                          // '1' $04
        13,                         // '2' $05
        28,                         // '3' $06
        4,                          // '4' $07
        10,                         // '5' $08
        26,                         // '6' $09
        14,                         // '7' $0A
        16,                         // '8' $0B
        29,                         // '9' $0C
        23,                         // '\' $0D
        5,                          // ';' $0E
        18,                         // '[' $0F
        31,                         // '&' $10
        11,                         // ':' $11
        8,                          // '.' $12
        27,                         // '?' $13
        25,                         // 'A' $14
        15,                         // 'B' $15
        22,                         // 'C' $16
        17,                         // 'D' $17
        30,                         // 'E' $18
        7,                          // 'F' $19
        24,                         // 'G' $1A
        21,                         // 'H' $1B
        6,                          // 'I' $1C
        20,                         // '#' $1D
        19,                         // '<' $1E
        32                          // '=' $1F
    );

begin
    val := val and $1f;                         // strip off bank designator
    if ((val >= Low(xlate)) and (val <= High(xlate))) then
        Result := xlate[val]
    else
        raise EMemoryError.CreateFmt('Invalid native row / column designator (%d)', [val]);
end;

function RowColToNative(val: Byte): Byte;
// Translate an integer row or column number to its native 1005 equivalent
const
    xlate: array[1..32] of Byte = (
        X3_SPACE,                   // 1
        X3_RIGHT_SQUARE,            // 2
        X3_0,                       // 3
        X3_4,                       // 4
        X3_SEMI,                    // 5
        X3_I,                       // 6
        X3_F,                       // 7
        X3_PERIOD,                  // 8
        X3_1,                       // 9
        X3_5,                       // 10
        X3_COLON,                   // 11
        X3_MINUS,                   // 12
        X3_2,                       // 13
        X3_7,                       // 14
        X3_B,                       // 15
        X3_8,                       // 16
        X3_D,                       // 17
        X3_LEFT_SQUARE,             // 18
        X3_LESS,                    // 19
        X3_SHARP,                   // 20
        X3_H,                       // 21
        X3_C,                       // 22
        X3_BACK_SLASH,              // 23
        X3_G,                       // 24
        X3_A,                       // 25
        X3_6,                       // 26
        X3_QUESTION,                // 27
        X3_3,                       // 28
        X3_9,                       // 29
        X3_E,                       // 30
        X3_AMP,                     // 31
        X3_EQUAL                    // 32
    );

begin
    if ((val >= Low(xlate)) and (val <= High(xlate))) then
        Result := xlate[val]
    else
        raise EMemoryError.CreateFmt('Invalid native row / column designator (%d)', [val]);
end;


function Opcode(op: Byte; code: String; it: T1005InstType; ind: Boolean; proc: TDirectiveProc): T1005Opcode;
begin
    Result.Opcode := op;
    Result.Opcode2 := 0;
    Result.Code := code;
    Result.InstType := it;
    Result.Indirect := ind;
    Result.Proc := proc;
    Result.FldA := otAddress;
    Result.FldB := otAddress;
    Result.FldC := otAddress;
end;

function Opcode(op: Byte; code: String; it: T1005InstType; ind: Boolean; igna, ignb, ignc: T1005OperandType; proc: TDirectiveProc): T1005Opcode;
begin
    Result.Opcode := op;
    Result.Opcode2 := 0;
    Result.Code := code;
    Result.InstType := it;
    Result.Indirect := ind;
    Result.Proc := proc;
    Result.FldA := igna;
    Result.FldB := ignb;
    Result.FldC := ignc;
end;

function Opcode(op, op2: Byte; code: String; it: T1005InstType; proc: TDirectiveProc): T1005Opcode;
begin
    Result := Opcode(op, code, it, False, proc);
    Result.OpCode2 := op2;
end;

function FedSysOperand: T1005FedSysOperand;
begin
    Result.Init;
end;

function FedSysOperand(mrow, mcol, lrow, lcol: Byte): T1005FedSysOperand;
begin
    Result.Init;
    Result.SetAddr(mrow, mcol, lrow, lcol);
end;

function FedSysOperand(m, l: I1005Addr): T1005FedSysOperand;
begin
    Result.Init;
    Result.SetAddr(m, l);
end;

{ T1005OpCodeList }

procedure T1005OpCodeList.AddOpcode(opcode: T1005OpCode);
var
    i: Integer;
begin
    for i := 0 to Count - 1 do
    begin
        if ((Self[i].OpCode > opcode.OpCode) or
            ((Self[i].OpCode = opcode.OpCode) and (Self[i].OpCode2 > opcode.OpCode2))) then
        begin
            Insert(i, opcode);
            Exit;
        end;
    end;
    Add(opcode);
end;

function T1005OpCodeList.FindOpcode(opcode, opcode2: Byte): T1005Opcode;
var
    compare: TComparison<T1005Opcode>;
    i: Integer;
    oprec: T1005Opcode;
begin
    compare := function(const Left, Right: T1005Opcode): Integer
    begin
        if (Left.OpCode = Right.OpCode) then
        begin
            if (Left.Opcode2 < Right.Opcode2) then
                Result := -1
            else if (Left.Opcode2 > Right.Opcode2) then
                Result := 1
            else
                Result := 0;
        end else
        begin
            if (Left.Opcode < Right.Opcode) then
                Result := -1
            else if (Left.Opcode > Right.Opcode) then
                Result := 1
            else
                Result := 0;
        end;
    end;

    oprec.Opcode := opcode;
    oprec.OpCode2 := opcode2;
    if (not BinarySearch(oprec, i, TComparer<T1005Opcode>.Construct(compare))) then
        raise EProcessorError.Create('Illegal opcode');
    Result := Self[i];
end;

function T1005OpCodeList.IsOpcode(opcode, opcode2: Byte): Boolean;
begin
    try
        FindOpcode(opcode, opcode2);
        Result := True;
    except
        Result := False;
    end;
end;

{ TFedSys1005Addr }

procedure T1005FedSysAddr.AdjustBankDesc(msl: I1005Addr);
// **** For use with descending instructions
//
// This is used to adjust the bank bits of the LSL address to
// be consistent with the MSL. In the simple case, we just make
// the LSL the same as the MSL. In the case where the addresses
// span two banks we need to bump the LSL to the next bank.
var
    nbank, nrow, ncol: Byte;
    obank, orow, ocol: Byte;
begin
    Decode(obank, orow, ocol);
    msl.Decode(nbank, nrow, ncol);
    if ((orow < nrow) and (ocol < ncol) and (nbank < 4)) then
        Inc(nbank);
    SetAddr(nbank, orow, ocol);
end;

procedure T1005FedSysAddr.AdjustBankAsc(msl: I1005Addr);
// **** For use with ascending instructions
//
// This is used to adjust the bank bits of the LSL address to
// be consistent with the MSL. In the simple case, we just make
// the LSL the same as the MSL. In the case where the addresses
// span two banks we need to decrement the MSL to the previous bank.
var
    nbank, nrow, ncol: Byte;
    obank, orow, ocol: Byte;
begin
    Decode(obank, orow, ocol);
    msl.Decode(nbank, nrow, ncol);
    if ((orow < nrow) and (ocol < ncol) and (nbank > 1)) then
        Dec(nbank);
    SetAddr(nbank, orow, ocol);
end;

procedure T1005FedSysAddr.Assign(addr: I1005Addr);
begin
    FRow := addr.Row;
    FCol := addr.Col;
end;

procedure T1005FedSysAddr.Clear;
begin
    FRow := 0;
    FCol := 0;
end;

function T1005FedSysAddr.Clone: I1005Addr;
begin
    Result := T1005FedSysAddr.Create;
    Result.Assign(self);
end;

function T1005FedSysAddr.Compare(op2: I1005Addr): Integer;
var
    a1, a2: Integer;
begin
    a1 := SequentialAddr;
    a2 := op2.SequentialAddr;
    if (a1 < a2) then
        Result := -1
    else if (a1 > a2) then
        Result := 1
    else
        Result := 0;
end;

procedure T1005FedSysAddr.Decode(var abank, arow, acol: Byte);
begin
    arow := NativeToRowCol(Row);
    acol := NativeToRowCol(Col);
    abank := (((Row and $20) shr 5) or ((Col and $20) shr 4)) + 1;
end;

procedure T1005FedSysAddr.Decrement(count: Integer);
var
    bank, row, col: Byte;
begin
    Decode(bank, row, col);
    while (count > 0) do
    begin
        Dec(col);
        if (col < 1) then
        begin
            col := 31;
            Dec(row);
            if (row < 1) then
            begin
                row := 31;
                Dec(bank);
                if (bank < 1) then
                    bank := 4;
    //                raise EMemoryError.Create('Attempt to decrement past beginning of memory');
            end;
        end;
        Dec(count);
    end;
    SetAddr(bank, row, col);
end;

function T1005FedSysAddr.GetCol: Byte;
begin
    Result := FCol;
end;

function T1005FedSysAddr.GetRow: Byte;
begin
    Result := FRow;
end;

procedure T1005FedSysAddr.Increment(count: Integer);
var
    bank, row, col: Byte;
begin
    Decode(bank, row, col);
    while (count > 0) do
    begin
        Inc(col);
        if (col > 31) then
        begin
            col := 1;
            Inc(row);
            if (row > 31) then
            begin
                row := 1;
                Inc(bank);
                if (bank > 4) then
                    bank := 1;
    //                raise EMemoryError.Create('Attempt to increment past end of memory');
            end;
        end;
        Dec(count);
    end;
    SetAddr(bank, row, col);
end;

function T1005FedSysAddr.Matches(op2: I1005Addr): Boolean;
begin
    Result := (Compare(op2) = 0);
end;

function T1005FedSysAddr.NewObject: I1005Addr;
begin
    Result := T1005FedSysAddr.Create;
end;

function T1005FedSysAddr.GetSequentialAddr: Integer;
var
    bank, row, col: Byte;
begin
    Decode(bank, row, col);
    Result := ((bank - 1)  * 961) + ((row - 1) * 31) + col;
end;

procedure T1005FedSysAddr.SetAddr(row, col: Byte);
begin
    FRow := row;
    FCol := col;
end;

procedure T1005FedSysAddr.SetAddr(addr: Integer);
var
    bank, row, col: Integer;
begin
    Dec(addr);
    bank := (addr div 961) + 1;
    addr := addr mod 961;
    row := (addr div 31) + 1;
    col := (addr mod 31) + 1;
    SetAddr(bank, row, col);
end;

procedure T1005FedSysAddr.SetAddr(abank, arow, acol: Byte);
begin
    FRow := RowColToNative(arow);
    FCol := RowColToNative(acol);
    case abank of
      1:
        ;
      2:
        FRow := FRow or $20;
      3:
        FCol := FCol or $20;
      4:
      begin
        FRow := FRow or $20;
        FCol := FCol or $20;
      end
        else  raise EMemoryError.CreateFmt('Invalid bank descriptor (%d)', [abank]);
    end;
end;

{ TComm1005Addr }

function T1005CommAddr.Clone: I1005Addr;
begin
    Result := T1005CommAddr.Create;
    Result.Assign(Self);
end;

procedure T1005CommAddr.Decode(var abank, arow, acol: Byte);
begin
    arow := NativeToRowCol(Row);
    acol := NativeToRowCol(Col);
    abank := (((Col and $20) shr 5) or ((Row and $20) shr 4)) + 1;
end;

function T1005CommAddr.Matches(op2: I1005Addr): Boolean;
begin
    // Return True if the rows and columns, excluding the bank bits,
    // match.
    Result := (((Row and $1f) = (op2.Row and $1f)) and
               ((Col and $1f) = (op2.Col and $1f)));
end;

function T1005CommAddr.NewObject: I1005Addr;
begin
    Result := T1005CommAddr.Create;
end;

procedure T1005CommAddr.SetAddr(abank, arow, acol: Byte);
begin
    FRow := RowColToNative(arow);
    FCol := RowColToNative(acol);
    case abank of
      1:
        ;
      2:
        FCol := FCol or $20;
      3:
        FRow := FRow or $20;
      4:
      begin
        FRow := FRow or $20;
        FCol := FCol or $20;
      end
      else  raise EMemoryError.CreateFmt('Invalid bank descriptor (%d)', [abank]);
    end;
end;

{ T1005Operand }

procedure T1005FedSysOperand.GetL(var bank, row, col: Byte);
begin
    FL.Decode(bank, row, col);
end;

procedure T1005FedSysOperand.GetM(var bank, row, col: Byte);
var
    dummy: Byte;
begin
    FM.Decode(bank, row, col);
    FL.Decode(bank, dummy, dummy);
end;

procedure T1005FedSysOperand.Init;
begin
    FM := T1005FedSysAddr.Create;
    FL := T1005FedSysAddr.Create;
end;

procedure T1005FedSysOperand.SetAddr(mrow, mcol, lrow, lcol: Byte);
begin
    FM.SetAddr((mrow and $1f) or (lrow and $20), (mcol and $1f) or (lcol and $20));
    FL.SetAddr(lrow, lcol);
end;

procedure T1005FedSysOperand.SetAddr(m, l: I1005Addr);
begin
    FM.Assign(m);
    FL.Assign(l);
end;

procedure T1005FedSysOperand.SetAddr(bank, mrow, mcol, lrow, lcol: Byte);
begin
    FM.SetAddr(bank, mrow, mcol);
    FL.SetAddr(bank, lrow, lcol);
end;

{ T1005CommOperands }

procedure T1005CommOperands.Init;
begin
    FFlda := T1005CommAddr.Create;
    FFldb := T1005CommAddr.Create;
    FFldc := T1005CommAddr.Create;
end;

end.
