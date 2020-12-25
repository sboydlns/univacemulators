unit SrcFile;

interface

uses SysUtils, System.Classes, Generics.Collections;

type
  TSrcFileStream = class(TFileStream)
  protected
    FBuffer: array [0..255] of AnsiChar;
    FBufferHead: Integer;
    FBufferTail: Integer;
    FEof: Boolean;
    FLineNumber: Integer;
    FLinePeeked: Boolean;
    FPeekedLine: AnsiString;
    FPeekedEof: Boolean;
    FColumn: Integer;
    FCrntLine: AnsiString;
    function GetC: AnsiChar;
  public
    constructor Create(fname: String); overload;
    function PeekLine: AnsiString; virtual;
    function ReadLine: AnsiString; virtual;
    procedure Reset; virtual;
    procedure WriteLine(s: AnsiString);
    property Column: Integer read FColumn write FColumn;
    property Eof: Boolean read FEof;
    property Line: AnsiString read FCrntLine write FCrntLine;
    property LineNumber: Integer read FLineNumber;
  end;

  TSrcStringStream = class(TSrcFileStream)
  private
    FStrings: TList<AnsiString>;
    FLineIndex: Integer;
    function Pad(s: AnsiString): AnsiString;
  public
    constructor Create(initLineNum: Integer);
    destructor Destroy; override;
    procedure AddLine(s: AnsiString);
    function PeekLine: AnsiString; override;
    function ReadLine: AnsiString; override;
    procedure Reset; override;
  end;

implementation

{ TSrcFileStream }

constructor TSrcFileStream.Create(fname: String);
begin
    inherited Create(fname, fmOpenRead or fmShareDenyNone);
end;

function TSrcFileStream.GetC: AnsiChar;
begin
    Result := Chr(0);
    if (FBufferHead >= FBufferTail) then
    begin
        FBufferTail := Read(FBuffer, 256);
        if (FBufferTail < 1) then
        begin
            FEof := True;
            Exit;
        end;
        FBufferHead := 0;
    end;
    Result := FBuffer[FBufferHead];
    Inc(FBufferHead);
end;

function TSrcFileStream.PeekLine: AnsiString;
var
    c: AnsiChar;
begin
    Result := '';
    if (FPeekedEof) then
    begin
        FEof := FPeekedEof;
        Result := StringOfChar(AnsiChar(' '), 80);
        Exit;
    end;
    if (FLinePeeked) then
    begin
        Result := FPeekedLine;
        Exit;
    end;
    repeat
        c := GetC;
        case c of
          #0:   Break;
          #13:  ;
          #10:  Break;
          else  Result := Result + c;
        end;
    until Eof;
    Result := Result + StringOfChar(AnsiChar(' '), 80 - Length(Result));
    FLinePeeked := True;
    FPeekedLine := Result;
    FPeekedEof := FEof and (Trim(String(Result)) = '');
    FEof := False;
end;

function TSrcFileStream.ReadLine: AnsiString;
var
    c: AnsiChar;
begin
    Result := '';
    if (FLinePeeked) then
    begin
        Result := FPeekedLine;
        FLinePeeked := False;
        FEof := FPeekedEof;
    end else
    begin
        repeat
            c := GetC;
            case c of
              #0:   Break;
              #13:  ;
              #10:  Break;
              else  Result := Result + c;
            end;
        until Eof;
        Result := Result + StringOfChar(AnsiChar(' '), 80 - Length(Result));
    end;
    Inc(FLineNumber);
end;

procedure TSrcFileStream.Reset;
    // Reset the file to the beginning
begin
    Position := 0;
    FEof := False;
    FPeekedEof := False;
    FLinePeeked := False;
    FPeekedLine := '';
    FBufferHead := 0;
    FBufferTail := 0;
    FLineNumber := 0;
end;

procedure TSrcFileStream.WriteLine(s: AnsiString);
begin
    s := s + #13#10;
    Write(PAnsiChar(s)^, Length(s));
end;

{ TSrcStringStream }

procedure TSrcStringStream.AddLine(s: AnsiString);
begin
    FStrings.Add(s);
end;

constructor TSrcStringStream.Create(initLineNum: Integer);
begin
    inherited Create('NUL:');
    FLineNumber := initLineNum;
    FLineIndex := 0;
    FStrings := TList<AnsiString>.Create;
end;

destructor TSrcStringStream.Destroy;
begin
    FreeAndNil(FStrings);
    inherited Destroy;
end;

function TSrcStringStream.Pad(s: AnsiString): AnsiString;
begin
    if (Length(s) < 80) then
        Result := s + StringOfChar(AnsiChar(' '), 80 - Length(s))
    else
        Result := s;
end;

function TSrcStringStream.PeekLine: AnsiString;
begin
    if (FLineIndex < FStrings.Count) then
        Result := Pad(FStrings[FLineIndex])
    else
        Result := StringOfChar(AnsiChar(' '), 80);
end;

function TSrcStringStream.ReadLine: AnsiString;
begin
    if (FLineIndex < FStrings.Count) then
    begin
        Result := Pad(FStrings[FLineIndex]);
        Inc(FLineIndex);
        FEof := (FLineIndex >= FStrings.Count);
    end;
end;

procedure TSrcStringStream.Reset;
begin
    FLineIndex := 0;
    FColumn := 81;
end;

end.
