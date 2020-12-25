unit ListFile;

interface

uses SysUtils, System.Classes;

type
  TPrintFileStream = class(TFileStream)
  protected
    FEnabled: Boolean;
  public
    procedure Print(bfr: String);
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

  TListFileStream = class(TPrintFileStream)
  private
    FLineNum: String;
    FAddress: String;
    FSource: String;
    FValue: String;
    function GetValue: UInt32;
    procedure SetValue(const Value: UInt32);
    function GetAddress: UInt32;
    procedure SetAddress(const Value: UInt32);
  public
    procedure InitLine(lineNo: Integer; addr: UInt32; src: AnsiString);
    procedure Print; overload;
    property Address: UInt32 read GetAddress write SetAddress;
    property LineNum: String read FLineNum;
    property Source: String read FSource write FSource;
    property Value: UInt32 read GetValue write SetValue;
  end;

implementation

uses U494Util;

{ TListFileStream }

function TListFileStream.GetAddress: UInt32;
begin
    Result := Octal(FAddress);
end;

function TListFileStream.GetValue: UInt32;
begin
    Result := Octal(FValue);
end;

procedure TListFileStream.InitLine(lineNo: Integer; addr: UInt32; src: AnsiString);
begin
    FLineNum := Format('%4d', [lineNo]);
    FAddress := Copy(FormatOctal(addr), 6);
    FSource := String(src);
    FValue := StringOfChar(' ', 10);
end;

procedure TListFileStream.Print;
var
    s: AnsiString;
    p: PAnsiChar;
begin
    if (FEnabled) then
    begin
        s := AnsiString(Format('%s: %s %s %s'#13#10, [FLineNum, FAddress, FValue, FSource]));
        p := PAnsiChar(s);
        WriteBuffer(p^, Length(s));
    end;
end;

procedure TListFileStream.SetAddress(const Value: UInt32);
begin
    FAddress := Copy(FormatOctal(Value), 6);
end;

procedure TListFileStream.SetValue(const Value: UInt32);
begin
    FValue := FormatOctal(Value);
end;

{ TPrintFileStream }

procedure TPrintFileStream.Print(bfr: String);
var
    s: AnsiString;
    p: PAnsiChar;
begin
    if (FEnabled) then
    begin
        s := AnsiString(bfr + #13#10);
        p := PAnsiChar(s);
        WriteBuffer(p^, Length(s));
    end;
end;

end.
