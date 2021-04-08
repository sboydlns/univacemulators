unit U9030Types;

interface

uses Windows, WinApi.SHFolder, Messages, SysUtils, Classes, Generics.Collections,
     Generics.Defaults, Forms;

type
  EMemoryError = class(Exception)
  end;

  EAlignmentError = class(Exception)
  end;

  EProcessorError = class(Exception)
  end;

  EDecimalError = class(Exception)
  end;

  TMemoryAddress = UInt32;
  THalfWord = Int16;
  TWord = Int32;
  TDblWord = Int64;

  TAlignment = ( aHalfWord = 2, aWord = 4, aDblWord = 8 );

  TOpcodeProc = procedure(fal: Byte; fad1, fad2: Smallint) of object;

  TInstType = ( itDirective, itRR, itRS, itRX, itSI, itSS1, itSS2, itBranch, itUnknown );

  TOpcode = record
  public
    Opcode: Byte;
    Length: Byte;
    Proc: TOpcodeProc;
    InstType: TInstType;
    Code: String;
  end;

  TOpcodeList = class (TList<TOpcode>)
  public
    function FindOpcode(opcode: Byte): TOpcode;
    function IsOpcode(opcode: Byte): Boolean;
  end;

  function Opcode(op, length: Byte; proc: TOpcodeProc; code: String = ''; typ: TInstType = itUnknown): TOpcode;

implementation

function Opcode(op, length: Byte; proc: TOpcodeProc; code: String; typ: TInstType): TOpcode;
begin
    Result.Opcode := op;
    Result.Length := length;
    Result.Proc := proc;
    Result.InstType := typ;
    Result.Code := code;
end;

{ TU92OpcodeList }

function TOpcodeList.FindOpcode(opcode: Byte): TOpcode;
var
    compare: TComparison<TOpcode>;
    i: Integer;
    oprec: TOpcode;
begin
    compare := function(const Left, Right: TOpcode): Integer
    begin
        if (Left.Opcode < Right.Opcode) then
            Result := -1
        else if (Left.Opcode > Right.Opcode) then
            Result := 1
        else
            Result := 0;
    end;

    oprec.Opcode := opcode;
    if (not BinarySearch(oprec, i, TComparer<TOpcode>.Construct(compare))) then
        raise EProcessorError.Create('Illegal opcode');
    Result := Self[i];
end;

function TOpcodeList.IsOpcode(opcode: Byte): Boolean;
begin
    try
        FindOpcode(opcode);
        Result := True;
    except
        Result := False;
    end;
end;

end.
