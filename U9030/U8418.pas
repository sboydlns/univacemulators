unit U8418;

interface

uses SysUtils, Classes;

type
  T8418File = class(TFileStream)
  private
    FMaxCylinder: Integer;
    FMaxTrack: Integer;
    FMaxSector: Integer;
    FSectorSize: Integer;

    procedure CheckAddr(cyl, trk, sec: Integer);
  public
    constructor Create(const AFileName: string; Mode: Word); virtual;
    procedure Format;
    procedure ReadSector(cyl, trk, sec: Integer; bfr: PByte);
    procedure SeekSector(cyl, trk, sec: Integer);
    property MaxCylinder: Integer read FMaxCylinder;
    property MaxTrack: Integer read FMaxTrack;
    property MaxSector: Integer read FMaxSector;
    property SectorSize: Integer read FSectorSize;
  end;

  T8416File = class(T8418File)
  public
    constructor Create(const AFileName: string; Mode: Word); override;
  end;

implementation

{ T8418File }

const
  MAX_TRACKS = 7;
  MAX_SECTORS = 40;
  SECTOR_SIZE = 256;

procedure T8418File.CheckAddr(cyl, trk, sec: Integer);
begin
    if ((cyl < 0) or (cyl >= FMaxCylinder)) then
        raise Exception.CreateFmt('Invalid cylinder number %d', [cyl]);
    if ((trk < 0) or (trk >= FMaxTrack)) then
        raise Exception.CreateFmt('Invalid track number %d', [cyl]);
    if ((sec < 1) or (sec > FMaxSector)) then
        raise Exception.CreateFmt('Invalid sector number %d', [cyl]);
end;

constructor T8418File.Create(const AFileName: string; Mode: Word);
begin
    inherited;
    FMaxCylinder := 808;
    FMaxTrack := MAX_TRACKS;
    FMaxSector := MAX_SECTORS;
    FSectorSize := SECTOR_SIZE;
end;

procedure T8418File.Format;
var
    cyl, trk, sect, i: Integer;
    bfr: array [1..SECTOR_SIZE] of Byte;
begin
    for i := Low(bfr) to High(bfr) do
        bfr[i] := 0;
    // Write enough data to fill the simulated disk
    for cyl := 0 to FMaxCylinder - 1 do
        for trk := 0 to MAX_TRACKS - 1 do
            for sect := 0 to MAX_SECTORS - 1 do
                Write(bfr, SECTOR_SIZE);
end;

procedure T8418File.ReadSector(cyl, trk, sec: Integer; bfr: PByte);
begin
    SeekSector(cyl, trk, sec);
    if (Read(bfr^, FSectorSize) <> FSectorSize) then
        raise Exception.CreateFmt('Error reading %d %d %s', [cyl, trk, sec]);
end;

procedure T8418File.SeekSector(cyl, trk, sec: Integer);
begin
    CheckAddr(cyl, trk, sec);
    Position := ((((cyl * FMaxTrack) + trk) * FMaxSector) + sec - 1) * FSectorSize;
end;

{ T8416File }

constructor T8416File.Create(const AFileName: string; Mode: Word);
begin
    inherited;
    FMaxCylinder := 404;
end;

end.
