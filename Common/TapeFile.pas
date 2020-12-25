unit TapeFile;

interface

uses Windows, SysUtils, Classes;

const
  // Tape header flags
  TH_NEW_REC = $80;
  TH_TAPEMARK = $40;
  TH_END_REC = $20;

type
  TTapeHdr = packed record
    NextBlockLen: WORD;
    PrevBlockLen: WORD;
    Flag1: Byte;
    Flag2: Byte;
  end;
  // This class provides an interface for reading and writing
  // AWS format virtual tape files.
  TTapeFile = class(TFileStream)
  private
    FBot: Boolean;
    FEof: Boolean;
    FEot: Boolean;
    FFileNum: Integer;
    FLastHdr: TTapeHdr;
    function CheckBot: Boolean;
    function CheckEot: Boolean;
    procedure ReadHdrBackward(var bfr: TTapeHdr);
    procedure ReadHdrForward(var bfr: TTapeHdr);
    procedure Reverse(bfr: PByte; bfrLen: Integer);
  public
    constructor Create(const AFileName: string; Mode: Word); reintroduce;
    procedure BackwardSpaceBlock;
    procedure BackwardSpaceFile;
    procedure Erase;
    procedure ForwardSpaceBlock;
    procedure ForwardSpaceFile;
    function ReadBackward(bfr: PByte; bfrLen: Integer): Integer;
    function ReadForward(bfr: PByte; bfrLen: Integer): Integer;
    procedure Rewind;
    function Write(bfr: PByte; bfrLen: Integer): Integer; reintroduce;
    procedure WriteTapeMark;
    property Bot: Boolean read FBot;
    property Eof: Boolean read FEof;
    property Eot: Boolean read FEot;
    property FileNum: Integer read FFileNum;
  end;

implementation

{ TTapeFile }

procedure TTapeFile.BackwardSpaceBlock;
begin
    if (Position > 0) then
        Position := Position - FLastHdr.NextBlockLen;
    ReadHdrBackward(FLastHdr);
end;

procedure TTapeFile.BackwardSpaceFile;
begin
    repeat
        BackwardSpaceBlock;
    until FEof;
    FEof := False;
end;

function TTapeFile.CheckBot: Boolean;
begin
    FBot := (Position = 0);
    FEof := FBot;
    Result := FBot;
end;

function TTapeFile.CheckEot: Boolean;
begin
    FEot := (Position >= Size);
    FEof := FEot;
    Result := FEot;
end;

constructor TTapeFile.Create(const AFileName: string; Mode: Word);
begin
    inherited;
    FFileNum := 1;
end;

procedure TTapeFile.Erase;
begin
    SetEndOfFile(FHandle);                      // Truncate file to current size.
end;

procedure TTapeFile.ForwardSpaceBlock;
begin
    ReadHdrForward(FLastHdr);
    if (not FEot) then
        Position := Position + FLastHdr.NextBlockLen;
end;

procedure TTapeFile.ForwardSpaceFile;
begin
    repeat
        ForwardSpaceBlock;
    until FEof;
    FEof := False;
end;

function TTapeFile.ReadBackward(bfr: PByte; bfrLen: Integer): Integer;
var
    tapeBfr: PByte;
    tapeBfrLen: Integer;
    bytesRead: Integer;
    overrun: Boolean;
    count: Integer;
    hdr: TTapeHdr;
    blockLen: Integer;
begin
    tapebfr := nil;
    tapeBfrLen := 0;
    bytesRead := 0;
    overrun := False;

    try
        if (not CheckEot) then
        begin
            ReadHdrForward(hdr);
            blockLen := hdr.PrevBlockLen;
            Position := Position - SizeOf(hdr);
        end else
            blockLen := FLastHdr.NextBlockLen;
        repeat
            if (blockLen > tapeBfrLen) then
                ReallocMem(tapeBfr, blockLen);
            Position := Position - blockLen;
            count := inherited Read(tapeBfr^, blockLen);
            if (count >= (bfrLen - bytesRead)) then
            begin
                count := bfrLen - bytesRead;
                overrun := True;
            end;
            Move(tapeBfr^, (bfr + bytesRead)^, count);
            Inc(bytesRead, count);
            Position := Position - blockLen;
            ReadHdrBackward(FLastHdr);
        until (FEof or ((FLastHdr.Flag1 and TH_NEW_REC) <> 0));
        Reverse(bfr, bytesRead);
        Result := bytesRead;
        if (overrun) then
            raise Exception.Create('Buffer overrun');
    finally
        FreeMem(tapeBfr);
    end;
end;

function TTapeFile.ReadForward(bfr: PByte; bfrLen: Integer): Integer;
var
    tapeBfr: PByte;
    tapeBfrLen: Integer;
    bytesRead: Integer;
    overrun: Boolean;
    count: Integer;
begin
    Result := 0;
    tapebfr := nil;
    tapeBfrLen := 0;
    bytesRead := 0;
    overrun := False;

    try
        repeat
            ReadHdrForward(FLastHdr);
            if (FLastHdr.NextBlockLen > tapeBfrLen) then
                ReallocMem(tapeBfr, FLastHdr.NextBlockLen);
            count := inherited Read(tapeBfr^, FLastHdr.NextBlockLen);
            if (count >= (bfrLen - bytesRead)) then
            begin
                count := bfrLen - bytesRead;
                overrun := True;
            end;
            Move(tapeBfr^, (bfr + bytesRead)^, count);
            Inc(bytesRead, count);
        until (FEof or ((FLastHdr.Flag1 and TH_END_REC) <> 0));
        if (overrun) then
            raise Exception.Create('Buffer overrun');
        Result := bytesRead;
    finally
        FreeMem(tapeBfr);
    end;
end;

procedure TTapeFile.ReadHdrBackward(var bfr: TTapeHdr);
var
    count: Integer;
begin
    if (CheckBot) then
        Exit;
    Position := Position - SizeOf(TTapeHdr);
    count := inherited Read(bfr, SizeOf(bfr));
    if (count <> SizeOf(bfr)) then
        raise Exception.Create('Invalid tape file. Header too short.');
    Position := Position - SizeOf(TTapeHdr);
    if ((bfr.Flag1 and TH_TAPEMARK) <> 0) then
    begin
        FEof := True;
        Dec(FFileNum);
    end else
        FEof := False;
end;

procedure TTapeFile.ReadHdrForward(var bfr: TTapeHdr);
var
    count: Integer;
begin
    if (CheckEot) then
        Exit;
    count := inherited Read(bfr, SizeOf(bfr));
    if (count <> SizeOf(bfr)) then
        raise Exception.Create('Invalid tape file. Header too short.');
    if ((bfr.Flag1 and TH_TAPEMARK) <> 0) then
    begin
        FEof := True;
        Inc(FFileNum);
    end else
        FEof := False;
end;

procedure TTapeFile.Reverse(bfr: PByte; bfrLen: Integer);
var
    i: Integer;
    b: Byte;
begin
    for i := 0 to (bfrLen div 2) + (bfrLen mod 2) do
    begin
        b := (bfr + i)^;
        (bfr + i)^ := (bfr + bfrLen - i - 1)^;
        (bfr + bfrLen - i - 1)^ := b;
    end;
end;

procedure TTapeFile.Rewind;
begin
    Position := 0;
    FFileNum := 1;
end;

function TTapeFile.Write(bfr: PByte; bfrLen: Integer): Integer;
begin
    if (bfrLen > 65535) then
        raise Exception.Create('Write buffer count to large');
    FLastHdr.PrevBlockLen := FLastHdr.NextBlockLen;
    FLastHdr.NextBlockLen := bfrLen;
    FLastHdr.Flag1 := TH_NEW_REC or TH_END_REC;
    inherited Write(FLastHdr, SizeOf(TTapeHdr));
    inherited Write(bfr^, bfrLen);
    SetEndOfFile(FHandle);                      // Truncate file to current size.
    Result := bfrLen;
end;

procedure TTapeFile.WriteTapeMark;
begin
    FLastHdr.PrevBlockLen := FLastHdr.NextBlockLen;
    FLastHdr.NextBlockLen := 0;
    FLastHdr.Flag1 := TH_TAPEMARK;
    inherited Write(FLastHdr, SizeOf(TTapeHdr));
    SetEndOfFile(FHandle);                      // Truncate file to current size.
end;

end.
