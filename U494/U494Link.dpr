program U494Link;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Linker in 'Linker.pas',
  ObjFile in 'ObjFile.pas',
  ListFile in 'ListFile.pas',
  U494Util in 'U494Util.pas',
  Bcd in '..\Common\Bcd.pas';

var
    Linker: TLinker;
    ObjFiles: array of String;
    OutFile: String;
    ListFile: String;
    showHelp: Boolean;
    otype: TOutputType;
    errs: Integer;

procedure ParseCmdLine;
var
    i: Integer;
begin
    SetLength(ObjFiles, 0);
    showHelp := False;
    otype := otImage;
    i := 1;
    while i <= ParamCount do
    begin
        if (Copy(ParamStr(i), 1, 1) <> '-') then
        begin
            SetLength(ObjFiles, Length(ObjFiles) + 1);
            ObjFiles[High(ObjFiles)] := ParamStr(i);
        end;
        if (ParamStr(i) = '-h') then
            showHelp := True
        else if (ParamStr(i) = '-mem') then
             otype := otImage
        else if (ParamStr(i) = '-exe') then
             otype := otExecutable
        else if (ParamStr(i) = '-o') then
        begin
            Inc(i);
            OutFile := ParamStr(i);
        end else if (ParamStr(i) = '-l') then
        begin
            Inc(i);
            ListFile := ParamStr(i);
        end;
        Inc(i);
    end;
end;

procedure Usage;
begin
    WriteLn('Usage: U494Link obj obj ... -h -o outputFile -l listFile -mem -exe');
    WriteLn('');
    WriteLn('obj = name of an object file, can be repeated as often as needed');
    WriteLn('-h = print this message');
    WriteLn('-mem = output memory image (default)');
    WriteLn('-exe = output MOS executable');
    WriteLn('-o = name of the output file');
    WriteLn('-l = name of the list file');
end;

begin
  try
    Linker := TLinker.Create;
    ParseCmdLine;
    if (showHelp) then
    begin
        Usage;
        Exit;
    end;
    if (Length(ObjFiles) = 0) then
        raise Exception.Create('No object files given');
    errs := Linker.Link(ObjFiles, OutFile, ListFile, otype);
    Halt(errs);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
