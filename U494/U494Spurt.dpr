program U494Spurt;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  AsmTypes in 'AsmTypes.pas',
  Spurt in 'Spurt.pas',
  U494Opcodes in 'U494Opcodes.pas',
  U494Util in 'U494Util.pas',
  Bcd in '..\Common\Bcd.pas';

var
    InFile: String;
    ProcDir: String;
    OutDir: String;
    xref: Boolean;
    showHelp: Boolean;
    tokenTrace: Boolean;
    noproc: Boolean;
    otype: TOutputType;
    tabc, sepc: String;
    errs: Integer;
    Assembler: TAssembler;

procedure ParseCmdLine;
var
    i: Integer;
begin
    InFile := '';
    OutDir := '';
    ProcDir := '';
    Xref := False;
    showHelp := False;
    tokenTrace := False;
    noproc := False;
    otype := otImage;
    tabc := '';
    sepc := '';
    i := 1;
    while i <= ParamCount do
    begin
        if ((Copy(ParamStr(i), 1, 1) <> '-') and (inFile = '')) then
            InFile := ParamStr(i);
        if (ParamStr(i) = '-tt') then
            tokenTrace := True
        else if (ParamStr(i) = '-h') then
            showHelp := True
        else if (ParamStr(i) = '-xref') then
             xref := True
        else if (ParamStr(i) = '-noproc') then
             noproc := True
        else if (ParamStr(i) = '-mem') then
             otype := otImage
        else if (ParamStr(i) = '-exe') then
             otype := otExecutable
        else if (ParamStr(i) = '-obj') then
             otype := otObject
        else if (ParamStr(i) = '-P') then
        begin
            Inc(i);
            ProcDir := ParamStr(i);
        end else if (ParamStr(i) = '-O') then
        begin
            Inc(i);
            OutDir := ParamStr(i);
        end else if (ParamStr(i) = '-tab') then
        begin
            Inc(i);
            tabc := Copy(ParamStr(i), 1, 1);
        end else if (ParamStr(i) = '-sep') then
        begin
            Inc(i);
            sepc := Copy(ParamStr(i), 1, 1);
        end;
        Inc(i);
    end;
    if (tabc = '') then
        tabc := #9;
    if (sepc = '') then
        sepc := '.';
end;

procedure Usage;
begin
    WriteLn('Usage: U494Asm srcFile -h -tt -xref -P procFolder -O outputFolder -mem -exe -obj');
    WriteLn('     : -tab c -sep c');
    WriteLn('');
    WriteLn('srcFile = the source file name');
    WriteLn('-h = print this message');
    WriteLn('-tt = enable token generator tracing');
    WriteLn('-xref = print identifier cross reference');
    WriteLn('-noproc = do not include expanded PROCs in listing');
    WriteLn('-P = folder containing proc library');
    WriteLn('-mem = output memory image (default)');
    WriteLn('-exe = output MOS executable');
    WriteLn('-obj = output relocatable object file for input to linker');
    WriteLn('-O = folder to contain output (memory image or relocatable) file');
    WriteLn('-tab = character used as TAB');
    WriteLn('-sep = character used as dot separator');
end;

begin
  try
    Assembler := TAssembler.Create;
    ParseCmdLine;
    if (showHelp) then
    begin
        Usage;
        Exit;
    end;
    if (inFile = '') then
        raise Exception.Create('No input file given');
    errs := Assembler.Assemble(InFile, tokenTrace, xref, ProcDir, noproc, otype, outDir, tabc, sepc);
    Halt(errs);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
