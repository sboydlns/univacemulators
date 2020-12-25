unit SrcFile;

interface

uses SysUtils, Classes, Generics.Collections;

const
  C_EOF = AnsiChar(0);

type
  TCharStack = class(TStack<AnsiChar>)
  end;

  TSrcFile = class(TFileStream)
  private
    FUngotChars: TCharStack;
    FLineNum: Integer;
    FEof: Boolean;
    FSrcLine: AnsiString;
    FNewLinePending: Boolean;
  public
    constructor Create(fname: String); overload;
    function GetC: AnsiChar;
    procedure UngetC(c: AnsiChar);
    procedure UngetS(s: AnsiString);
    procedure PrintLine(s: AnsiString);
    property LineNum: Integer read FLineNum;
    property SrcLine: AnsiString read FSrcLine;
  end;

implementation

{ TSrcFile }

constructor TSrcFile.Create(fname: String);
begin
    inherited Create(fname, fmOpenRead or fmShareDenyNone);
    FUngotChars := TCharStack.Create;
end;

function TSrcFile.GetC: AnsiChar;
begin
    if (FUngotChars.Count > 0) then
        Result := FUngotChars.Pop
    else if (FEof) then
        Result := C_EOF
    else
    begin
        if (Read(Result, 1) = 0) then
            FEof := True;
        if (FNewLinePending) then
            FSrcLine := '';
        if ((Result <> #13) and (Result <> #10)) then
            FSrcLine := FSrcLine + Result;
        FNewLinePending := False;
    end;
    if (Result = #10) then
    begin
        Inc(FLineNum);
        FNewLinePending := True;
    end;
end;

procedure TSrcFile.PrintLine(s: AnsiString);
const
    crlf: AnsiString = #13#10;
begin
    Write(PAnsiChar(s)^, Length(s));
    Write(PAnsiChar(crlf)^, 2);
end;

procedure TSrcFile.UngetC(c: AnsiChar);
begin
    FUngotChars.Push(c);
    if (c = #10) then
        Dec(FLineNum);
end;

procedure TSrcFile.UngetS(s: AnsiString);
var
    i: Integer;
begin
    for i := Length(s) downto 1 do
        UngetC(s[i]);
end;

end.
