unit Linker;

interface

uses SysUtils, Classes, IOUtils, Generics.Collections, ObjFile, ListFile;

type
  TOutputType = ( otImage, otExecutable );

  TEntryPointList = class(TDictionary<AnsiString, TEntryPoint>)
  end;

  TExternal = packed record
    ID: AnsiString;
    Addr: UInt32;                               // actual address
    RefAddr: UInt32;                            // address of a reference in this module
  end;

  TExternalList = class(TList<TExternal>)
  public
    function IndexOf(addr: UInt32): Integer;
  end;

  TObjFile = class(TObject)
  public
    Name: String;
    StartAddr: UInt32;
    TransferAddr: UInt32;
    ObjSize: UInt32;
    Externals: TExternalList;
    constructor Create;
  end;

  TLinker = class(TObject)
  private
    FObjFiles: array of TObjFile;
    FOutputType: TOutputType;
    FEntryPoints: TEntryPointList;
    FErrorCount: Integer;
    FLocationCounter: UInt32;
    FOutFileName: String;
    FPrintFile: String;
    FTransferAddr: UInt32;
    FListFile: TPrintFileStream;
    procedure LinkModule(objFile: TObjFile; inFile: TRelocatableStream; outFile: TMemImageStream);
    procedure LoadEntryPoints(inFile: TRelocatableStream; objFile: TObjFile);
    procedure LoadExternals(inFile: TRelocatableStream; objFile: TObjFile);
    procedure ResolveExternals(objFile: TObjFile);
    procedure Pass1;
    procedure Pass2;
  public
    constructor Create;
    function Link(objFiles: array of String; outFile, listFile: String; outType: TOutputType): Integer;
  end;

implementation

uses U494Util;

{ TLinker }

constructor TLinker.Create;
begin
    FEntryPoints := TEntryPointList.Create;
end;

function TLinker.Link(objFiles: array of String; outFile, listFile: String; outType: TOutputType): Integer;
var
    i: Integer;
begin
    SetLength(FObjFiles, Length(objFiles));
    for i := Low(objFiles) to High(objFiles) do
    begin
        FObjFiles[i] := TObjFile.Create;
        FObjFiles[i].Name := objFiles[i];
    end;
    FOutFileName := outFile;
    FOutputType := outType;
    FPrintFile := listFile;

    FLocationCounter := 0;
    if (FOutputType = otExecutable) then
        FLocationCounter := 96;

    if (FPrintFile = '') then
    begin
        FPrintFile := TPath.GetDirectoryName(FOutFileName) + '\' +
                      TPath.GetFileNameWithoutExtension(FOutFileName) + '.map';
    end;
    FListFile := TPrintFileStream.Create(FPrintFile, fmCreate);
    FListFile.Enabled := True;

    try
        try
            Pass1;
            Pass2;
        except
          on E: Exception do
          begin
              FListFile.Print(Format('**** %s', [E.Message]));
              Inc(FErrorCount);
          end;
        end;
    finally
        FListFile.Free;
    end;

    WriteLn(Format('%-20.20s: %d error(s) encountered', [TPath.GetFileName(FOutFileName), FErrorCount]));
    Result := FErrorCount;
end;

procedure TLinker.LinkModule(objFile: TObjFile; inFile: TRelocatableStream; outFile: TMemImageStream);
var
    rel: TRelocatableType;
    word, lc, hw: UInt32;
    stat: Boolean;
    i: Integer;
begin
    lc := objFile.StartAddr;
    stat := inFile.FirstObjectWord(rel, word);
    while (stat) do
    begin
        i := objFile.Externals.IndexOf(lc);
        if (i = -1) then
        begin
            // not an external reference
            case rel of
              rtNone:
                ;
              rtWord:
                Inc(word, objFile.StartAddr);
              rtH1:
              begin
                hw := (word shr 15) and $7fff;
                Inc(hw, objFile.StartAddr);
                word := (word and $7fff) or (hw shl 15);
              end;
              rtH2:
              begin
                hw := word and $7fff;
                Inc(hw, objFile.StartAddr);
                word := (word and (not $7fff)) or hw;
              end;
              rtH1H2:
              begin
                hw := (word shr 15) and $7fff;
                Inc(hw, objFile.StartAddr);
                word := (word and $7fff) or (hw shl 15);
                hw := word and $7fff;
                Inc(hw, objFile.StartAddr);
                word := (word and (not $7fff)) or hw;
              end;
            end;
        end else
        begin
            // external reference. substitue address of apropriate entry point.
            word := (word and (not $7fff)) or (objFile.Externals[i].Addr and $7fff);
        end;
        outFile.EmitSingleWord(lc, rel, word);
        Inc(lc);
        stat := inFile.NextObjectWord(rel, word);
    end;
end;

procedure TLinker.LoadEntryPoints(inFile: TRelocatableStream; objFile: TObjFile);
var
    ep, dummy: TEntryPoint;
    id: AnsiString;
    stat: Boolean;
    sort: TStringList;
    i: Integer;
begin
    sort := TStringList.Create;
    try
        sort.Sorted := True;
        sort.Duplicates := dupAccept;
        stat := inFile.FirstEntryPoint(ep);
        while (stat) do
        begin
            id := ep.IDToString;
            if (FEntryPoints.TryGetValue(id, dummy)) then
            begin
                FListFile.Print(Format('%s is defined multiple times', [id]));
                Inc(FErrorCount);
            end;
            Inc(ep.Value, objFile.StartAddr);
            FEntryPoints.Add(id, ep);
            sort.Add(String(id));
            stat := inFile.NextEntryPoint(ep);
        end;
        for i := 0 to sort.Count - 1 do
        begin
            if (FEntryPoints.TryGetValue(AnsiString(sort[i]), ep)) then
                FListFile.Print(Format('%-10.10s %s',
                                       [AnsiString(ep.ID), Copy(FormatOctal(ep.Value), 5)]));
        end;
        FListFile.Print('');
    finally
        sort.Free;
    end;
end;

procedure TLinker.LoadExternals(inFile: TRelocatableStream; objFile: TObjFile);
var
    er: TExternalRef;
    ref: TErReference;
    ext: TExternal;
    stat, stat1: Boolean;
begin
    stat := inFile.FirstExternal(er);
    while (stat) do
    begin
        ext.ID := er.IDToString;
        stat1 := inFile.NextReference(ref);
        while (stat1) do
        begin
            ext.RefAddr := ref.RefAddress + objFile.StartAddr;
            objFile.Externals.Add(ext);
            stat1 := inFile.NextReference(ref);
        end;
        stat := inFile.NextExternal(er);
    end;
end;

procedure TLinker.Pass1;
var
    i: Integer;
    totSize: UInt32;
    objFile: TObjFile;
    infile: TRelocatableStream;
    xfer: String;
begin
    totSize := FLocationCounter;
    for i := Low(FObjFiles) to High(FObjFiles) do
    begin
        objFile := FObjFiles[i];
        infile := TRelocatableStream.Create(objFile.Name, fmOpenRead or fmShareDenyWrite);
        try
            infile.FetchTransferAddr(objFile.TransferAddr, objFile.ObjSize);
            objFile.StartAddr := totSize;
            xfer := '';
            if ((objFile.TransferAddr and $3fffffff) <> $3fffffff) then
            begin
                Inc(objFile.TransferAddr, totSize);
                if (FTransferAddr = 0) then
                    FTransferAddr := objFile.TransferAddr;
                xfer := Copy(FormatOctal(objFile.TransferAddr), 5);
            end;
            Inc(totSize, objFile.ObjSize);
            FListFile.Print('MODULE                START   SIZE   XFER');
            FListFile.Print(Format('%-20.20s %s %s %s',
                                   [TPath.GetFileName(objFile.Name),
                                    Copy(FormatOctal(objFile.StartAddr), 5),
                                    Copy(FormatOctal(objFile.ObjSize), 5),
                                    xfer]));
            FListFile.Print('');
            FListFile.Print('ENTRY POINTS');
            LoadEntryPoints(infile, objFile);
            LoadExternals(infile, objFile);
        finally
            infile.Free;
        end;
    end;
end;

procedure TLinker.Pass2;
var
    i: Integer;
    objFile: TObjFile;
    infile: TRelocatableStream;
    outFile: TMemImageStream;
begin
    outFile := TMemImageStream.Create(FOutFileName, fmCreate);
    try
        outFile.EmitTransferAddr(0, FTransferAddr, 0);
        for i := Low(FObjFiles) to High(FObjFiles) do
        begin
            objFile := FObjFiles[i];
            infile := TRelocatableStream.Create(objFile.Name, fmOpenRead or fmShareDenyWrite);
            try
                ResolveExternals(objFile);
                LinkModule(objFile, inFile, outFile);
            finally
                infile.Free;
            end;
        end;
    finally
        outFile.Free;
    end;
end;

procedure TLinker.ResolveExternals(objFile: TObjFile);
var
    i: Integer;
    ext: TExternal;
    ep: TEntryPoint;
begin
    for i := 0 to objFile.Externals.Count - 1 do
    begin
        ext := objFile.Externals[i];
        if (FEntryPoints.TryGetValue(ext.ID, ep)) then
        begin
            ext.Addr := ep.Value;
            objFile.Externals[i] := ext;
        end else
        begin
            FListFile.Print(Format('%s defined in %s not defined',
                                   [ext.ID, objFile.Name]));
            Inc(FErrorCount);
        end;
    end;
end;

{ TObjFile }

constructor TObjFile.Create;
begin
    Externals := TExternalList.Create;
end;

{ TExternalList }

function TExternalList.IndexOf(addr: UInt32): Integer;
begin
    for Result := 0 to Count - 1 do
    begin
        if (Items[Result].RefAddr = addr) then
            Exit;
    end;
    Result := -1;
end;

end.
