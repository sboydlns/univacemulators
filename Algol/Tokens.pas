unit Tokens;

interface

uses SysUtils, SrcFile, Generics.Collections, CodeGen;

type
  TTokenType = ( ttIdentifier = 0, ttInteger, ttReal, ttEof, 
                 ttArithmeticOp, ttLabel, ttRelationalOp,
                 ttBooleanOp, ttSequentailOp, ttSeparator,
                 ttDeclarator, ttLeftParen, ttRightParen,
                 ttBegin, ttEnd, ttSpecificator,
                 ttLogicalValue, ttString, ttLineSeparator,
                 ttComma, ttComment, ttAssignment, ttGoto,
                 ttIf, ttFor, ttColon, ttUnknown );
  TToken = packed record
    TType: TTokenType;
    Value: AnsiString;
  end;

  TTokenStack = class(TStack<TToken>)
  end;

  TTokenGen = class
  private
    FSrcFile: TSrcFile;
    FCodeGen: TCodeGen;
    FUngotTokens: TTokenStack;
    FTrace: Boolean;
    procedure GetArithmeticOp(var ttype: TTokenType; var token: AnsiString);
    procedure GetIdentifier(var ttype: TTokenType; var token: AnsiString);
    procedure GetInteger(var ttype: TTokenType; var token: AnsiString);
    procedure GetNumber(var ttype: TTokenType; var token: AnsiString);
    procedure GetString(var ttype: TTokenType; var token: AnsiString);
    function IsArithmeticOp(c: AnsiChar): Boolean;
    function IsDigit(c: AnsiChar): Boolean;
    function IsLetter(c: AnsiChar): Boolean;
    function IsLineSeparator(c: AnsiChar): Boolean;
    function IsSpace(c: AnsiChar): Boolean;
    function IsUnary(c: AnsiChar): Boolean;
    procedure SkipSpaces;
    procedure TraceToken(ttype: TTokenType; token: AnsiString);
  public
    constructor Create(srcFile: TSrcFile; cg: TCodeGen; trace: Boolean);
    procedure FlushLine;
    procedure Get(var ttype: TTokenType; var token: AnsiString);
    procedure Unget(ttype: TTokenType; token: AnsiString);
  end;

const
  TokenTypes: array [TTokenType] of String = (
    'Identifier',
    'Integer',
    'Real',
    'EOF',
    'ArithmeticOp',
    'Label',
    'RelationalOp',
    'BooleanOp',
    'SequentialOp',
    'Separator',
    'Declarator',
    'LeftParen',
    'RightParen',
    'Begin',
    'End',
    'Specificator',
    'LogicalValue',
    'String',
    'LineSeparator',
    'Comma',
    'Comment',
    'Assignment',
    'Goto',
    'If',
    'For',
    'Colon',
    'Unknown'
  );

implementation

uses AnsiStrings;

type
  TReservedWord = packed record
    ID: AnsiString;
    TokenType: TTokenType;
  end;

const
  ReservedWords: array [0..41] of TReservedWord = (
    ( ID: 'LSS'; TokenType: ttRelationalOp ),
    ( ID: 'LEQ'; TokenType: ttRelationalOp ),
    ( ID: 'EQL'; TokenType: ttRelationalOp ),
    ( ID: 'GEQ'; TokenType: ttRelationalOp ),
    ( ID: 'GTR'; TokenType: ttRelationalOp ),
    ( ID: 'NEQ'; TokenType: ttRelationalOp ),
    ( ID: 'EQUIV'; TokenType: ttBooleanOp ),
    ( ID: 'IMPL'; TokenType: ttBooleanOp ),
    ( ID: 'XOR'; TokenType: ttBooleanOp ),
    ( ID: 'OR'; TokenType: ttBooleanOp ),
    ( ID: 'AND'; TokenType: ttBooleanOp ),
    ( ID: 'NOT'; TokenType: ttBooleanOp ),
    ( ID: 'GO TO'; TokenType: ttGoto ),
    ( ID: 'GOTO'; TokenType: ttGoto ),
    ( ID: 'GO'; TokenType: ttGoto ),
    ( ID: 'IF'; TokenType: ttIf ),
    ( ID: 'THEN'; TokenType: ttSequentailOp ),
    ( ID: 'FOR'; TokenType: ttFor ),
    ( ID: 'ELSE'; TokenType: ttSequentailOp ),
    ( ID: 'DO'; TokenType: ttSequentailOp ),
    ( ID: 'STEP'; TokenType: ttSeparator ),
    ( ID: 'UNTIL'; TokenType: ttSeparator ),
    ( ID: 'WHILE'; TokenType: ttSeparator ),
    ( ID: 'OWN'; TokenType: ttDeclarator ),
    ( ID: 'BOOLEAN'; TokenType: ttDeclarator ),
    ( ID: 'INTEGER'; TokenType: ttDeclarator ),
    ( ID: 'REAL'; TokenType: ttDeclarator ),
    ( ID: 'STRING'; TokenType: ttDeclarator ),
    ( ID: 'ARRAY'; TokenType: ttDeclarator ),
    ( ID: 'SWITCH'; TokenType: ttDeclarator ),
    ( ID: 'PROCEDURE'; TokenType: ttDeclarator ),
    ( ID: 'EXTERNAL'; TokenType: ttDeclarator ),
    ( ID: 'LIST'; TokenType: ttDeclarator ),
    ( ID: 'FORMAT'; TokenType: ttDeclarator ),
    ( ID: 'LOCAL'; TokenType: ttDeclarator ),
    ( ID: 'BEGIN'; TokenType: ttBegin ),
    ( ID: 'END'; TokenType: ttEnd ),
    ( ID: 'VALUE'; TokenType: ttSpecificator ),
    ( ID: 'LABEL'; TokenType: ttSpecificator ),
    ( ID: 'TRUE'; TokenType: ttLogicalValue ),
    ( ID: 'FALSE'; TokenType: ttLogicalValue ),
    ( ID: 'COMMENT'; TokenType: ttComment )
  );
{ TTokenGen }

constructor TTokenGen.Create(srcFile: TSrcFile; cg: TCodeGen; trace: Boolean);
begin
    FSrcFile := srcFile;
    FCodeGen := cg;
    FTrace := trace;
    FUngotTokens := TTokenStack.Create;
end;

procedure TTokenGen.FlushLine;
var
    token: TToken;
    c: AnsiChar;
begin
    while (FUngotTokens.Count > 0) do
    begin
        token := FUngotTokens.Pop;
        if  ((token.TType = ttLineSeparator) or (token.TType = ttEof)) then
            Break;
    end;
    if (FUngotTokens.Count > 0) then
        Exit;
    c := FSrcFile.GetC;
    while ((c <> C_EOF) and (not IsLineSeparator(c))) do
    begin
        c := FSrcFile.GetC;
        IsSpace(c);                     // to force line to be printed if NL seen
    end;
end;

procedure TTokenGen.Get(var ttype: TTokenType; var token: AnsiString);
var
    c: AnsiChar;
    t: TToken;
begin
    if (FUngotTokens.Count > 0) then
    begin
        t := FUngotTokens.Pop;
        ttype := t.TType;
        token := t.Value;
        TraceToken(ttype, token);
        Exit;
    end;

    SkipSpaces;
    c := FSrcFile.GetC;
    if (IsLineSeparator(c)) then
    begin
        ttype := ttLineSeparator;
        token := c;
    end else if (IsLetter(c)) then
    begin
        FSrcFile.UngetC(c);
        GetIdentifier(ttype, token);
    end else if (IsDigit(c) or (c = '.') or (c = '&')) then
    begin
        FSrcFile.UngetC(c);
        GetNumber(ttype, token);
    end else if (IsArithmeticOp(c)) then
    begin
        FSrcFile.UngetC(c);
        GetArithmeticOp(ttype, token);
    end else if (c = '''') then
    begin
        FSrcFile.UngetC(c);
        GetString(ttype, token);
    end else if ((c = '(') or (c = '[')) then
    begin
        ttype := ttLeftParen;
        token := c;
    end else if ((c = ')') or (c = ']')) then
    begin
        ttype := ttRightParen;
        token := c;
    end else if (c = ',') then
    begin
        ttype := ttComma;
        token := c;
    end else if (c = ':') then
    begin
        c := FSrcFile.GetC;
        if (c = '=') then
        begin
            ttype := ttAssignment;
            token := ':=';
        end else
        begin
            FSrcFile.UngetC(c);
            ttype := ttColon;
        end;
    end else if (c = '=') then
    begin
        ttype := ttAssignment;
        token := ':=';
    end else if (c = C_EOF) then
    begin
        ttype := ttEof;
        token := '';
    end else
    begin
        ttype := ttUnknown;
        token := '';
    end;
    TraceToken(ttype, token);
end;

procedure TTokenGen.GetArithmeticOp(var ttype: TTokenType; var token: AnsiString);
var
    c: AnsiChar;
begin
    ttype := ttArithmeticOp;
    c := FSrcFile.GetC;
    token := c;
    if ((c = '/') or (c = '*')) then
    begin
        c := FSrcFile.GetC;
        if ((c = '/') or (c = '*')) then
            token := token + c
        else
            FSrcFile.UngetC(c);
    end;
end;

procedure TTokenGen.GetIdentifier(var ttype: TTokenType; var token: AnsiString);
var
    c: AnsiChar;
    rw: TReservedWord;
    tt: TTokenType;
    temp: AnsiString;
begin
    ttype := ttIdentifier;
    token := '';
    c := FSrcFile.GetC;
    while (IsLetter(c) or IsDigit(c)) do
    begin
        token := token + c;
        c := FSrcFile.GetC;
    end;
    token := AnsiUpperCase(token);
    if ((ttype = ttIdentifier) and (c = ':')) then
    begin
        c := FSrcFile.GetC;
        if (IsSpace(c)) then
        begin
            ttype := ttLabel;
            Exit;
        end else
        begin
            FSrcFile.UngetC(c);
            FSrcFile.UngetC(':');
        end;
    end else
        FSrcFile.UngetC(c);
    // Special case for GO TO statement
    if (token = 'GO') then
    begin
        SkipSpaces;
        GetIdentifier(tt, temp);
        if (temp = 'TO') then
            token := token + ' TO'
        else
            FSrcFile.UngetS(temp);
    end;
    for rw in ReservedWords do
    begin
        if (rw.ID = token) then
        begin
            ttype := rw.TokenType;
            FSrcFile.UngetC(c);
            Break;
        end;
    end;
end;

procedure TTokenGen.GetInteger(var ttype: TTokenType; var token: AnsiString);
var
    c: AnsiChar;
begin
    ttype := ttInteger;
    token := '';
    c := FSrcFile.GetC;
    while (IsDigit(c)) do
    begin
        token := token + c;
        c := FSrcFile.GetC;
    end;
    FSrcFile.UngetC(c);
end;

procedure TTokenGen.GetNumber(var ttype: TTokenType; var token: AnsiString);
var
    c: AnsiChar;
    tt: TTokenType;
    rslt, temp: AnsiString;
begin
    rslt := '';
    c := FSrcFile.GetC;
    if (IsDigit(c)) then
    begin
        FSrcFile.UngetC(c);
        GetInteger(tt, rslt);
        ttype := ttInteger;
    end else
        FSrcFile.UngetC(c);
    c := FSrcFile.GetC;
    if (c = '.') then
    begin
        rslt := rslt + c;
        GetInteger(tt, temp);
        rslt := rslt + temp;
        ttype := ttReal;
    end else
        FSrcFile.UngetC(c);
    c := FSrcFile.GetC;
    if (c = '&') then
    begin
        rslt := rslt + 'E';
        c := FSrcFile.GetC;
        if (IsUnary(c)) then
            rslt := rslt + c
        else
            FSrcFile.UngetC(c);
        GetInteger(tt, temp);
        rslt := rslt + temp;
        ttype := ttReal;            
    end else
        FSrcFile.UngetC(c);
    token := rslt;
end;

procedure TTokenGen.GetString(var ttype: TTokenType; var token: AnsiString);
var
    c: AnsiString;
begin
    ttype := ttString;
    token := '';
    c := FSrcFile.GetC;
    if (c = '''') then
    begin
        c := FSrcFile.GetC;
        while ((c <> '''') and (c <> C_EOF)) do
        begin
            if (c <> #13) then
            begin
                if ((c < ' ') or (c > '~')) then
                    c := ' ';
                token := token + c;
            end;
            c:= FSrcFile.GetC;
        end;
    end;
end;

function TTokenGen.IsArithmeticOp(c: AnsiChar): Boolean;
begin
    Result := (c = '+') or (c = '-') or (c = '/') or (c = '*');
end;

function TTokenGen.IsDigit(c: AnsiChar): Boolean;
begin
    Result := (c >= '0') and (c <= '9');
end;

function TTokenGen.IsLetter(c: AnsiChar): Boolean;
begin
    Result := ((c >= 'A') and (c <= 'Z') or
               (c >= 'a') and (c <= 'z'));
end;

function TTokenGen.IsLineSeparator(c: AnsiChar): Boolean;
begin
    Result := (c = ';') or (c = '$');
end;

function TTokenGen.IsSpace(c: AnsiChar): Boolean;
begin
    Result := (c = ' ') or (c = #9) or (c = #10) or (c = #13);
    if ((c = #10){ and FTrace}) then
        FCodeGen.Comment(FSrcFile.SrcLine);
//        FCodeGen.Comment(Format(AnsiString('Line %d'), [FSrcFile.LineNum]));
end;

function TTokenGen.IsUnary(c: AnsiChar): Boolean;
begin
    Result := (c = '-') or (c = '+');
end;

procedure TTokenGen.SkipSpaces;
var
    c: AnsiChar;
begin
    c := FSrcFile.GetC;
    while (IsSpace(c)) do
        c := FSrcFile.GetC;
    FSrcFile.UngetC(c);
end;

procedure TTokenGen.TraceToken(ttype: TTokenType; token: AnsiString);
begin
    if (FTrace) then
        WriteLn(Format('%s: %s', [TokenTypes[ttype], token]));
end;

procedure TTokenGen.Unget(ttype: TTokenType; token: AnsiString);
var
    t: TToken;
begin
    if (ttype <> ttUnknown) then
    begin
        t.TType := ttype;
        t.Value := token;
        FUngotTokens.Push(t);
    end;
end;

end.
