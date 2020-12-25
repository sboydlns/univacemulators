unit U1005Device;

interface

uses SysUtils, CardFile, U1005Memory;

type
  T1005Device = class(TObject)
  protected
    FMemory: T1005Memory;
  public
    constructor Create(mem: T1005Memory); reintroduce; virtual;
  end;

  T1005CardDevice = class(T1005Device)
  protected
    FFiles: TCardFileList;
    FCurrentFile: TCardFileStream;
    FInputCount: Integer;
    FHopperEmpty: Boolean;
    function OpenNextFile: Boolean;
  public
    constructor Create(mem: T1005Memory); override;
    destructor Destroy; override;
    procedure AddBlankCards(count: Integer);
    procedure AddFile(fname: String);
    property HopperEmpty: Boolean read FHopperEmpty;
    property InputCount: Integer read FInputCount;
  end;

implementation

{ T1005CardDevice }

procedure T1005CardDevice.AddBlankCards(count: Integer);
var
    cfr: TCardFileRec;
begin
    Inc(FInputCount, count);
    FHopperEmpty := False;
    cfr.FileName := '';
    cfr.BlankCards := count;
    FFiles.Add(cfr);
end;

procedure T1005CardDevice.AddFile(fname: String);
var
    fin: TCardFileStream;
    cfr: TCardFileRec;
    cclIn: TCCLStream;
    cclr: TCCLRec;
    extn: String;
    rootDir: String;
    itemp: Integer;
begin
    extn := LowerCase(ExtractFileExt(fname));
    if (extn = '.ccl') then
    begin
        rootDir := '.';
        cclIn := TCCLStream.Create(fname, fmOpenRead);
        try
            while (cclIn.Read(cclr)) do
            begin
                case cclr.FileType of
                  ctRootDir:
                  begin
                    rootDir := cclr.Name;
                  end;
                  ctData:
                  begin
                    if ((Pos(':', cclr.Name) <> 2) and (Pos('\', cclr.Name) <> 1)) then
                        cclr.Name := rootDir + '\' + cclr.Name;
                    AddFile(cclr.Name);
                  end;
                  ctBlanks:
                  begin
                    if (TryStrToInt(cclr.Name, itemp)) then
                        AddBlankCards(itemp)
                    else
                        raise Exception.CreateFmt('Invalid # of blank cards in /BLANKS command (%s)',
                                                  [cclr.Name]);
                  end;
                end;
            end;
        finally
            cclIn.Free;
        end;
        Exit;
    end;
    fin := TCardFileStream.Create(fname, fmOpenRead);
    try
        FInputCount := FInputCount + fin.RecordCount;
        FHopperEmpty := False;
    finally
        fin.Free;
    end;
    cfr.FileName := fname;
    cfr.BlankCards := 0;
    FFiles.Add(cfr);
end;

constructor T1005CardDevice.Create(mem: T1005Memory);
begin
    inherited Create(mem);
    FFiles := TCardFileList.Create;
    FHopperEmpty := True;
end;

destructor T1005CardDevice.Destroy;
begin
    FreeAndNil(FFiles);
    inherited Destroy;
end;

function T1005CardDevice.OpenNextFile: Boolean;
var
    cfr: TCardFileRec;
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
            FCurrentFile := TCardFileStream.Create(cfr.FileName, fmOpenRead or fmShareDenyNone)
        end else
            FCurrentFile := TBlankCardStream.Create(cfr.BlankCards);
        Result := True;
    end;
end;

{ T1005Device }

constructor T1005Device.Create(mem: T1005Memory);
begin
    inherited Create;
    FMemory := mem;
end;

end.
