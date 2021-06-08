unit Trace;

interface

uses SysUtils, Classes, SyncObjs;

type
  TTraceFile = class(TFileStream)
  private
    FLock: TCriticalSection;
  public
    constructor Create(const AFileName: string; Mode: Word); reintroduce;
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

var
  IOTraceFile: TTraceFile;
  IOTraceEnabled: Boolean;
  SvcTraceFile: TTraceFile;
  SvcTraceEnabled: Boolean;

implementation

uses EmulatorTypes, U9030Types;

{ TIOTraceFile }

constructor TTraceFile.Create(const AFileName: string; Mode: Word);
begin
    inherited;
    FLock := TCriticalSection.Create;
end;

destructor TTraceFile.Destroy;
begin
    FreeAndNil(FLock);
    inherited;
end;

function TTraceFile.Write(const Buffer; Count: Integer): Longint;
begin
    if (IOTraceEnabled) then
    begin
        FLock.Acquire;
        try
            Result := inherited;
        finally
            FLock.Release;
        end;
    end else
        Result := 0;
end;

initialization

    if (not DirectoryExists(DataDir)) then
        ForceDirectories(DataDir);
    IOTraceFile := TTraceFile.Create(DataDir + '\U9030IO.trc', fmCreate);
    IOTraceEnabled := False;
    SvcTraceFile := TTraceFile.Create(DataDir + '\U9030Svc.trc', fmCreate);
    SvcTraceEnabled := False;

finalization

    FreeAndNil(IOTraceFile);

end.
