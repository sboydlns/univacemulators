unit U494Util;

interface

uses SysUtils, Windows;

type

  T494Mode = ( m494, m490, m1230 );

  EMemoryException = class(Exception);
  EIllegalInstruction = class(Exception);
  EProgramProtection = class(Exception);

  T494CpuStates = ( csHalted );
  T494CpuState = set of T494CpuStates;

function FirstChar(s: AnsiString): AnsiChar; overload;
function FirstChar(s: String): Char; overload;
function FormatOctal(value: UInt32): String;
function Octal(value: String): UInt64;
function PublicDataDir: String;
function WinError: String;

implementation

uses SHFolder;

function FirstChar(s: AnsiString): AnsiChar;
begin
    if (Length(s) > 0) then
        Result := s[1]
    else
        Result := ' ';
end;

function FirstChar(s: String): Char;
begin
    if (Length(s) > 0) then
        Result := s[1]
    else
        Result := ' ';
end;

function FormatOctal(value: UInt32): String;
var
    count: Integer;
    digit: Byte;
begin
    Result := '';
    count := 0;
    while (count < 10) do
    begin
        digit := value - ((value div 8) * 8);
        Result := Char(digit + Ord('0')) + Result;
        value := value div 8;
        Inc(count);
    end;
end;

function GetDir(csidl: Integer): String;
var
    stat: Integer;
    path: array [0 .. MAX_PATH] of Char;
begin
    stat := ShGetFolderPath(0, csidl, 0, SHGFP_TYPE_CURRENT, path);
    if (stat = 0) then
    begin
        SetString(Result, path, StrLen(path));
        if (Result[Length(Result)] = '\') then
            Result := Copy(Result, 1, Length(Result) - 1);
    end
    else
    begin
        Result := '.';
    end;
end;

function Octal(value: String): UInt64;
var
    i: Integer;
    digit: Byte;
    sign: Integer;
begin
    Result := 0;
    sign := 1;
    value := Trim(value);
    i := 1;
    while (i <= Length(value)) do
    begin
        if (i = 1) then
        begin
            if (value[i] = '-') then
            begin
                sign := -1;
                Inc(i);
                Continue;
            end else if (value[i] = '+') then
            begin
                Inc(i);
                Continue;
            end;
        end;
        digit := Ord(value[i]) - Ord('0');
        if ((digit < 0) or (digit > 7)) then
            raise Exception.Create('Illegal octal digit');
        Result := (Result shl 3) + digit;
        Inc(i);
    end;
    Result := Result * sign;
end;

function PublicDataDir: String;
begin
    Result := GetDir(CSIDL_COMMON_APPDATA);
end;

// Return the text of the most recent Windows error message
function WinError: String;
var
    stat: Integer;
    msg: Pointer;
begin
    stat := GetLastError;
    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or
                  FORMAT_MESSAGE_FROM_SYSTEM or
                  FORMAT_MESSAGE_IGNORE_INSERTS,
                  nil,
                  stat,
                  0, // Default language
                  PChar(@msg),
                  0,
                  nil);
    SetString(Result, PChar(msg), StrLen(PChar(msg)));
    LocalFree(HLOCAL(msg));
end;

end.
