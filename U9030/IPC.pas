unit IPC;

interface

uses SysUtils, Classes,
     U9030Types, Globals, Channels;

type
  // Unlike the IDA the IPC needs to be able to update the BCW in
  // real time in order for everything to work. This will slow things
  // down somewhat but it can't be helped.
  TIPCBCW = class
  private
    FBCWAddress: TMemoryAddress;
    function GetCommand: Byte;
    function GetActvKey: Byte;
    function GetActvAddress: TMemoryAddress;
    procedure SetActvAddress(const Value: TMemoryAddress);
    function GetReplAddress: TMemoryAddress;
    procedure SetReplAddress(const Value: TMemoryAddress);
    function GetActvChain: Boolean;
    function GetActvTerm: Boolean;
    function GetActvCount: THalfWord;
    procedure SetActvCount(const Value: THalfWord);
    function GetReplChain: Boolean;
    function GetReplCount: THalfWord;
    function GetReplKey: Byte;
    function GetReplTerm: Boolean;
    procedure SetReplCount(const Value: THalfWord);
    function GetF: Boolean;
    procedure SetActvTerm(const Value: Boolean);
  public
    constructor Create(bcw: TMemoryAddress);
    property ActvAddress: TMemoryAddress read GetActvAddress write SetActvAddress;
    property ActvChain:  Boolean read GetActvChain;
    property ActvCount: THalfWord read GetActvCount write SetActvCount;
    property ActvKey: Byte read GetActvKey;
    property ActvTerm:  Boolean read GetActvTerm write SetActvTerm;
    property Command: Byte read GetCommand;
    property F: Boolean read GetF;
    property ReplAddress: TMemoryAddress read GetReplAddress write SetReplAddress;
    property ReplChain:  Boolean read GetReplChain;
    property ReplCount: THalfWord read GetReplCount write SetReplCount;
    property ReplKey: Byte read GetReplKey;
    property ReplTerm:  Boolean read GetReplTerm;
  end;

  TIPCDevice = class(TDevice)
  protected
    function MakeStatus(dstat, cstat: Byte): TStatus;
  public
    procedure SIO; virtual; abstract;
  end;

  TIPC = class(TChannel)
  public
    function SIO(addr: TWord): Byte; override;
  end;


implementation

{ TIPCBCW }

constructor TIPCBCW.Create(bcw: TMemoryAddress);
begin
    FBCWAddress := bcw;
end;

function TIPCBCW.GetActvAddress: TMemoryAddress;
begin
    Result := Core.FetchWord(0, FBCWAddress) and $3ffff;
end;

function TIPCBCW.GetActvChain: Boolean;
begin
    Result := (Core.FetchByte(0, FBCWAddress + 6) and $80) <> 0;
end;

function TIPCBCW.GetActvCount: THalfWord;
begin
    Result := Core.FetchHalfWord(0, FBCWAddress + 6) and $3ff;
    if (Result = 0) then
        Result := 1024;
end;

function TIPCBCW.GetActvKey: Byte;
begin
    Result := (Core.FetchByte(0, FBCWAddress + 1) and $70) shr 4;
end;

function TIPCBCW.GetActvTerm: Boolean;
begin
    Result := (Core.FetchByte(0, FBCWAddress + 6) and $20) <> 0;
end;

function TIPCBCW.GetCommand: Byte;
begin
    Result := Core.FetchByte(0, FBCWAddress);
end;

function TIPCBCW.GetF: Boolean;
begin
    Result := (Core.FetchByte(0, FBCWAddress + 8) and $80) <> 0;
end;

function TIPCBCW.GetReplAddress: TMemoryAddress;
begin
    Result := Core.FetchWord(0, FBCWAddress + 8) and $3ffff;
end;

function TIPCBCW.GetReplChain: Boolean;
begin
    Result := (Core.FetchByte(0, FBCWAddress + 4) and $80) <> 0;
end;

function TIPCBCW.GetReplCount: THalfWord;
begin
    Result := Core.FetchHalfWord(0, FBCWAddress + 4) and $3ff;
end;

function TIPCBCW.GetReplKey: Byte;
begin
    Result := (Core.FetchByte(0, FBCWAddress + 9) and $70) shr 4;
end;

function TIPCBCW.GetReplTerm: Boolean;
begin
    Result := (Core.FetchByte(0, FBCWAddress + 4) and $20) <> 0;
end;

procedure TIPCBCW.SetActvAddress(const Value: TMemoryAddress);
var
    w: TWord;
begin
    w := Core.FetchWord(0, FBCWAddress);
    Core.StoreWord(0, FBCWAddress, (w and (not $3ffff)) or (TWord(Value) and $3ffff));
end;

procedure TIPCBCW.SetActvCount(const Value: THalfWord);
var
    hw: THalfWord;
begin
    hw := Core.FetchHalfWord(0, FBCWAddress + 6);
    Core.StoreHalfWord(0, FBCWAddress + 6, (hw and (not $3ff)) or (Value and $3ff));
end;

procedure TIPCBCW.SetActvTerm(const Value: Boolean);
var
    b: Byte;
begin
    b := Core.FetchByte(0, FBCWAddress + 6);
    if (Value) then
        b := b or $20
    else
        b := b and (not $20);
    Core.StoreByte(0, FBCWAddress + 6, b);
end;

procedure TIPCBCW.SetReplAddress(const Value: TMemoryAddress);
var
    w: TWord;
begin
    w := Core.FetchWord(0, FBCWAddress + 8);
    Core.StoreWord(0, FBCWAddress + 8, (w and (not $3ffff)) or (TWord(Value) and $3ffff));
end;

procedure TIPCBCW.SetReplCount(const Value: THalfWord);
var
    hw: THalfWord;
begin
    hw := Core.FetchHalfWord(0, FBCWAddress + 4);
    Core.StoreHalfWord(0, FBCWAddress + 4, (hw and $3ff) or (Value and $3ff));
end;

{ TIPC }

function TIPC.SIO(addr: TWord): Byte;
var
    dvcNum: Byte;
    dvc: TDevice;
begin
    if (FBusy) then                                         // Channel busy
    begin
        Result := 2;
        Exit;
    end;
    try
        FBusy := True;

        dvcNum := addr and $f;
        dvc := FDevices[dvcNum];

        if (not Assigned(dvc)) then                             // Unknown device
        begin
            Result := inherited SIO(addr);
            Exit;
        end;

        TraceSIO(dvcNum);

        if (dvc.Busy) then                                      // Device busy
        begin
            Result := 2;
            Exit;
        end;

        TIPCDevice(dvc).SIO;
        Result := 0;
    finally
        FBusy := False;
    end;
end;

{ TIPCDevice }

function TIPCDevice.MakeStatus(dstat, cstat: Byte): TStatus;
begin
    Result := TStatus.Create;
    Result.Device := FDeviceNum;
    Result.Length := 1;
    Result.ChannelNum := FChannel.ChannelNum;
    Result.DeviceNum := FDeviceNum;
    Result.DeviceStatus := dstat;
    Result.ChannelStatus := cstat;
end;

end.
