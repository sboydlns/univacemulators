unit U0717;

interface

uses SysUtils, Classes,
     CardFile, Channels, IPC;

const
  // 0717 reader command codes and flags
  RDR_READ = $02;
  RDR_SENSE = $04;
  // Sense byte 0
  RDRCMD_REJECT = $80;
  RDR_INTERVENTION = $40;
  RDR_EQUIP_CHECK = $10;
  RDR_DATA_CHECK = $08;
  RDR_OVERRUN = $04;
  RDR_STOP = $02;
  RDR_DEVICE_CHECK = $01;
  // Sense byte 1
  RDR_COL0_ERROR = $80;
  RDR_VALIDITY_CHECK = $40;
  RDR_COMPARE_ERROR = $20;
  RDR_RSEYNC_ERROR = $10;
  RDR_TRANSFER_CHECK = $08;
  RDR_COL51 = $02;
  RDR_COL66 = $01;

type
  T0717 = class(TIPCDevice)
  private
    FSense: array [0..1] of Byte;
    FBCW: TIPCBCW;
    FCommand: Byte;
    FCurrentFile: TCardFileStream;
    FReadStation: TCardRec;
    FHopperEmpty: Boolean;
    procedure ClearSense;
    procedure DeviceEnd;
    procedure DoFeed;
    procedure DoRead;
    procedure DoReadImage;
    procedure DoReadTranslate;
    procedure DoSense;
    function GetCurrentFileName: String;
    function OpenNextFile: Boolean;
    procedure ProcessCommand; override;
    function StoreBuffer(bfr: PByte; len: Integer): Boolean;
    procedure UnitCheck;
  public
    constructor Create(num: Byte); override;
    destructor Destroy; override;
    procedure AddFile(fname: String); override;
    procedure SIO; override;
    property CurrentFile: TCardFileStream read FCurrentFile;
    property CurrentFileName: String read GetCurrentFileName;
  end;

implementation

uses Memory, Globals;

{ T0717 }

procedure T0717.AddFile(fname: String);
begin
    inherited;
    FHopperEmpty := False;
    if (not Assigned(FCurrentFile)) then
        OpenNextFile;
end;

procedure T0717.ClearSense;
begin
    FillChar(FSense, SizeOf(FSense), 0);
end;

constructor T0717.Create(num: Byte);
begin
    inherited;
    FCommand := 0;
    FHopperEmpty := True;
    FBCW := TIPCBCW.Create(READER_BCW0);
end;

destructor T0717.Destroy;
begin
    if (not Terminated) then
    begin
        Terminate;
        WaitFor;
    end;
    FreeAndNil(FBCW);
    inherited;
end;

procedure T0717.DeviceEnd;
begin
    FChannel.QueueStatus(MakeStatus(DEVICE_END, 0));
end;

procedure T0717.DoFeed;
begin
        if ((not Assigned(FCurrentFile)) or (FCurrentFile.Eof)) then
        begin
            if (not OpenNextFile) then
            begin
                FHopperEmpty := True;
                Exit;
            end;
        end;
        FCurrentFile.ReadRaw(FReadStation);
        if (FCurrentFile.Eof) then
            FreeAndNil(FCurrentFile);
end;

procedure T0717.DoRead;
var
    diag, stn2, col80, col51, col66, img: Boolean;
begin
    ClearSense;
    diag := (FCommand and $80) <> 0;
    stn2 := (FCommand and $40) <> 0;
    col80 := (FCommand and $08) = 0;
    col51 := (FCommand and $18) = $08;
    col66 := (FCommand and $18) = $18;
    img := (FCommand and $04) <> 0;
    if (diag) then
        raise Exception.Create('DIAG not supported for reader');
    if (col51) then
        raise Exception.Create('51 column mode not supported for reader');
    if (col66) then
        raise Exception.Create('66 column mode not supported for reader');
    if (img) then
        DoReadImage
    else
        DoReadTranslate;
end;

procedure T0717.DoReadImage;
begin
    raise Exception.Create('Read image not supported');
end;

procedure T0717.DoReadTranslate;
var
    bfr: TCardRec;
    stemp: String;
    i: Integer;
begin
    DoFeed;
    if (FHopperEmpty) then
    begin
        FSense[0] := RDR_INTERVENTION or RDR_STOP;
        UnitCheck;
        Exit;
    end;

    TCardFileStream.ReadEbcdic(bfr, FReadStation);
    //
    StoreBuffer(@bfr.Columns, FBCW.ActvCount);
    DeviceEnd;
end;

procedure T0717.DoSense;
begin
    if (StoreBuffer(PByte(@FSense), 2)) then
        DeviceEnd;
end;

function T0717.GetCurrentFileName: String;
begin
    if (FFiles.Count > 0) then
        Result := FFiles[0].FileName
    else
        Result := '';
end;

function T0717.OpenNextFile: Boolean;
var
    cfr: TCardFileRec;
    ext: String;
begin
    Result := False;
    if (Assigned(FCurrentFile)) then
    begin
        FFiles.Delete(0);
        FreeAndNil(FCurrentFile);
    end;
    if (FFiles.Count > 0) then
    begin
        cfr := FFiles[0];
        if (cfr.FileName <> '') then
        begin
            ext := LowerCase(ExtractFileExt(cfr.FileName));
            if (ext = '.rpg') then
                FCurrentFile := TRPGCardStream.Create(cfr.FileName, fmOpenRead, cfr.RPGType)
            else
                FCurrentFile := TCardFileStream.Create(cfr.FileName, fmOpenRead)
        end else
            FCurrentFile := TBlankCardStream.Create(cfr.BlankCards);
        Result := True;
    end;
end;

procedure T0717.ProcessCommand;
begin
    if (FCommand <> 0) then
    begin
        if ((FCommand and RDR_READ) <> 0) then
            DoRead
        else if ((FCommand and RDR_SENSE) <> 0) then
            DoSense
        else
            raise Exception.Create('0773 command not implemented');
    end;
    FBusy := False;
    FCommand := 0;
end;

procedure T0717.SIO;
begin
    if (FBusy) then
        // Since this is checked at the channel level,
        // this should never happer.
        Exit;
    FBusy := True;
    FCommand := FBCW.Command;
    FCmdRecvd.SetEvent;
end;

function T0717.StoreBuffer(bfr: PByte; len: Integer): Boolean;
begin
    Result := True;
    while ((len > 0) and (FBCW.ActvCount <> 0)) do
    begin
        try
            Core.StoreByte(FBCW.ActvKey, FBCW.ActvAddress, bfr^);
            FBCW.ActvAddress := FBCW.ActvAddress + 1;
            FBCW.ActvCount := FBCW.ActvCount - 1;
            Inc(bfr);
            Dec(len);
        except
            Result := False;
            FChannel.QueueStatus(MakeStatus(DEVICE_END or UNIT_CHECK, INVALID_ADDRESS));
        end;
    end;
end;

procedure T0717.UnitCheck;
begin
    FChannel.QueueStatus(MakeStatus(UNIT_CHECK, 0));
end;

end.
