unit U8418;

interface

uses SysUtils, Classes;

type
  T8418Disk = class(TFileStream)
  private
    FMaxCylinder: Integer;
    FMaxTrack: Integer;
    FMaxSector: Integer;
    FSectorSize: Integer;
    FTrackSize: Integer;

    procedure CheckAddr(cyl, head, rec: Integer);
  public
    constructor Create(const AFileName: string; Mode: Word); virtual;
    procedure Format;
    procedure ReadSector(cyl, head, rec: Integer; bfr: PByte); overload;
    procedure ReadSector(track, rec: Integer; bfr: PByte); overload;
    procedure SeekSector(cyl, head, rec: Integer);
    procedure WriteSector(cyl, head, rec: Integer; bfr: PByte); overload;
    property MaxCylinder: Integer read FMaxCylinder;
    property MaxTrack: Integer read FMaxTrack;
    property MaxSector: Integer read FMaxSector;
    property SectorSize: Integer read FSectorSize;
    property TrackSize: Integer read FTrackSize;
  end;

  T8416Disk = class(T8418Disk)
  public
    constructor Create(const AFileName: string; Mode: Word); override;
  end;

implementation

{ T8418File }

const
  MAX_HEADS = 7;
  MAX_RECORDS = 40;
  RECORD_SIZE = 256;

procedure T8418Disk.CheckAddr(cyl, head, rec: Integer);
begin
    if ((cyl < 0) or (cyl >= FMaxCylinder)) then
        raise Exception.CreateFmt('Invalid cylinder number %d', [cyl]);
    if ((head < 0) or (head >= FMaxTrack)) then
        raise Exception.CreateFmt('Invalid track number %d', [cyl]);
    if ((rec < 1) or (rec > FMaxSector)) then
        raise Exception.CreateFmt('Invalid sector number %d', [cyl]);
end;

constructor T8418Disk.Create(const AFileName: string; Mode: Word);
begin
    inherited;
    FMaxCylinder := 815;
    FMaxTrack := MAX_HEADS;
    FMaxSector := MAX_RECORDS;
    FSectorSize := RECORD_SIZE;
    FTrackSize := FMaxSector * FSectorSize;
end;

procedure T8418Disk.Format;
var
    cyl, trk, sect, i: Integer;
    bfr: array [1..RECORD_SIZE] of Byte;
begin
    for i := Low(bfr) to High(bfr) do
        bfr[i] := 0;
    // Write enough data to fill the simulated disk
    for cyl := 0 to FMaxCylinder - 1 do
        for trk := 0 to MAX_HEADS - 1 do
            for sect := 0 to MAX_RECORDS - 1 do
                Write(bfr, RECORD_SIZE);
end;

procedure T8418Disk.ReadSector(track, rec: Integer; bfr: PByte);
var
    cyl, head: Integer;
begin
    cyl := track div FMaxTrack;
    head := track mod FMaxTrack;
    ReadSector(cyl, head, rec, bfr);
end;

procedure T8418Disk.ReadSector(cyl, head, rec: Integer; bfr: PByte);
begin
    SeekSector(cyl, head, rec);
    if (Read(bfr^, FSectorSize) <> FSectorSize) then
        raise Exception.CreateFmt('Error reading %d %d %d', [cyl, head, rec]);
end;

procedure T8418Disk.SeekSector(cyl, head, rec: Integer);
begin
    CheckAddr(cyl, head, rec);
    Position := ((((cyl * FMaxTrack) + head) * FMaxSector) + rec - 1) * FSectorSize;
end;

procedure T8418Disk.WriteSector(cyl, head, rec: Integer; bfr: PByte);
begin
    SeekSector(cyl, head, rec);
    if (Write(bfr^, FSectorSize) <> FSectorSize) then
        raise Exception.CreateFmt('Error writing %d %d %s', [cyl, head, rec]);
end;

{ T8416File }

constructor T8416Disk.Create(const AFileName: string; Mode: Word);
begin
    inherited;
    FMaxCylinder := 411;
end;

end.
