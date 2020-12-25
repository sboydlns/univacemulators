program Algol;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Compiler in 'Compiler.pas',
  SrcFile in 'SrcFile.pas',
  Tokens in 'Tokens.pas',
  CodeGen in 'CodeGen.pas',
  U494CodeGen in 'U494CodeGen.pas',
  Symbols in 'Symbols.pas',
  Statements in 'Statements.pas',
  Expressions in 'Expressions.pas',
  Literals in 'Literals.pas';

var
    compiler: TCompiler;
    InFile: String;
    OutDir: String;
    showHelp: Boolean;
    tokenTrace: Boolean;
    errs: Integer;
    tgt: TTargetType;

procedure ParseCmdLine;
var
    i: Integer;
begin
    InFile := '';
    OutDir := '';
    showHelp := False;
    tokenTrace := False;
    tgt := tgt494;
    StackSize := 100;
    i := 1;
    while i <= ParamCount do
    begin
        if ((Copy(ParamStr(i), 1, 1) <> '-') and (inFile = '')) then
            InFile := ParamStr(i);
        if (ParamStr(i) = '-tt') then
            tokenTrace := True
        else if (ParamStr(i) = '-h') then
            showHelp := True
        else if (ParamStr(i) = '-O') then
        begin
            Inc(i);
            OutDir := ParamStr(i);
        end else if (ParamStr(i) = '-t') then
        begin
            Inc(i);
            if (ParamStr(i) = '494') then
                tgt := tgt494
            else
                raise Exception.CreateFmt('%s is not a recognized target', [ParamStr(i)]);
        end else if (ParamStr(i) = '-s') then
        begin
            Inc(i);
            if (not TryStrToInt(ParamStr(i), StackSize)) then
                raise Exception.Create('Invalid stack size');
        end;
        Inc(i);
    end;
end;

procedure Usage;
begin
    WriteLn('Usage: Algol srcFile -h -tt -O outputFolder');
    WriteLn('');
    WriteLn('srcFile = the source file name');
    WriteLn('-h = print this message');
    WriteLn('-tt = enable token generator tracing');
    WriteLn('-O = folder to contain output (memory image or relocatable) file');
    WriteLn('-t = target type (494 or possibly others in the future');
    WriteLn('-s = stack size in words');
end;

begin
  try
    compiler := TCompiler.Create;
    ParseCmdLine;
    if (showHelp) then
    begin
        Usage;
        Exit;
    end;
    if (inFile = '') then
        raise Exception.Create('No input file given');
    errs := compiler.Compile(InFile, tokenTrace, outDir, tgt);
    Halt(errs);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
