unit CaveUtilFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, SrcFile, Generics.Collections, CaveMapFrm;

type
  TLocation = class(TObject)
  private
    FId: Integer;
    FText: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    property ID: Integer read FId write FId;
    property Text: TStringList read FText;
  end;

  TTravel = class(TObject)
  private
    FFrom: Integer;
    FTo: Integer;
    FWhen: array of Integer;
    function GetWhen(i: Integer): Integer;
    function GetWhenCount: Integer;
  public
    procedure AddWhen(w: Integer);
    property From: Integer read FFrom write FFrom;
    property Too: Integer read FTo write FTo;
    property When[i: Integer]: Integer read GetWhen;
    property WhenCount: Integer read GetWhenCount;
  end;

  TVerb = class(TObject)
  private
    FId: Integer;
    FVerb: String;
  public
    property ID: Integer read FId write FId;
    property Verb: String read FVerb write FVerb;
  end;

  TCaveUtilForm = class(TForm)
    Label1: TLabel;
    FormatBtn: TButton;
    Label2: TLabel;
    CountMemo: TMemo;
    Label3: TLabel;
    MapBtn: TButton;
    procedure FormatBtnClick(Sender: TObject);
    procedure MapBtnClick(Sender: TObject);
  private
    FLocations: TList<TLocation>;
    FTravel: TList<TTravel>;
    FVerbs: TList<TVerb>;
    function FindLoc(id: Integer): Integer;
    function FindVerb(id: Integer): Integer;
    function FormatNumbers(outFile: TSrcFileStream; text: AnsiString): Integer;
    function FormatText(outFile: TSrcFileStream; text: AnsiString): Integer;
    procedure LoadConfig;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  CaveUtilForm: TCaveUtilForm;

implementation

uses AnsiStrings;

{$R *.dfm}

constructor TCaveUtilForm.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FLocations := TList<TLocation>.Create;
    FTravel := TList<TTravel>.Create;
    FVerbs := TList<TVerb>.Create;
end;

function TCaveUtilForm.FindLoc(id: Integer): Integer;
begin
    for Result := 0 to FLocations.Count - 1 do
    begin
        if (FLocations[Result].ID = id) then
            Exit;
    end;
    Result := -1;
end;

function TCaveUtilForm.FindVerb(id: Integer): Integer;
begin
    for Result := 0 to FVerbs.Count - 1 do
    begin
        if (FVerbs[Result].ID = id) then
            Exit;
    end;
    Result := -1;
end;

procedure TCaveUtilForm.FormatBtnClick(Sender: TObject);
var
    inFile, outFile: TSrcFileStream;
    lineIn, lt: AnsiString;
    i, lineType: Integer;
    split, len: Integer;
    counts, max: array[1..6] of Integer;
begin
    lineType := 0;
    for i := 1 to 6 do
    begin
        counts[i] := 0;
        max[i] := 0;
    end;
    inFile := TSrcFileStream.Create('..\..\77-03-31_adventure.txt');
    try
        outFile := TSrcFileStream.Create('..\..\u494_adventure.txt', fmCreate);
        try
            while (not inFile.Eof) do
            begin
                lineIn := inFile.ReadLine;
                for i := 1 to Length(lineIn) do
                begin
                    if (lineIn[i] = #9) then
                        lineIn[i] := ' ';
                end;
                lineIn := Trim(lineIn);
                if (lineIn = '') then
                    Continue;
                split := AnsiPos(AnsiString(' '), lineIn);
                if (split = 0) then
                begin
                    lt := lineIn;
                    lineType := StrToInt(String(lt));
                    if (lineType = -1) then
                        lineType := 9999;
                    outFile.WriteLine(AnsiString(Format('%-5d', [lineType])));
                    Continue;
                end;
                case lineType of
                    1, 2, 4, 5, 6:
                        len := FormatText(outFile, lineIn);
                    3:
                        len := FormatNumbers(outFile, lineIn);
                end;
                if ((lineType >= 1) and (lineType <= 6)) then
                begin
                    Inc(counts[lineType]);
                    if (len > max[lineType]) then
                        max[lineType] := len;
                end;
            end;
        finally
            outFile.Free;
        end;
    finally
        inFile.Free;
    end;

    for i := 1 to 6 do
    begin
        CountMemo.Lines.Add(Format('%d = %d Max = %d', [i, counts[i], max[i]]));
    end;
end;

function TCaveUtilForm.FormatNumbers(outFile: TSrcFileStream; text: AnsiString): Integer;
var
    lt, num, lineOut: AnsiString;
    i, split, lineType: Integer;
    nums: array [0..9] of Integer;
begin
    split := AnsiPos(AnsiString(' '), text);
    lt := Copy(text, 1, split - 1);
    text := Trim(Copy(text, split + 1));
    lineType := StrToInt(String(lt));
    if (lineType = -1) then
        lineType := 9999;

    for i := 0 to 9 do
        nums[i] := 0;
    i := 0;
    while ((text <> '') and (i < 10)) do
    begin
        split := AnsiPos(AnsiString(' '), text);
        if (split > 0) then
        begin
            num := Copy(text, 1, split - 1);
            text := Trim(Copy(text, split + 1));
        end else
        begin
            num := text;
            text := '';
        end;
        nums[i] := StrToInt(String(num));
        Inc(i);
    end;

    lineOut := AnsiString(Format('%-5d', [lineType]));
    for i := 0 to 9 do
    begin
        if (nums[i] <> 0) then
            lineOut := lineOut + AnsiString(Format('%-5d', [nums[i]]));
    end;
    outFile.WriteLine(lineOut);
    Result := Length(lineOut);
end;

function TCaveUtilForm.FormatText(outFile: TSrcFileStream; text: AnsiString): Integer;
var
    lt, lineOut: AnsiString;
    split, lineType: Integer;
begin
    split := AnsiPos(AnsiString(' '), text);
    lt := Copy(text, 1, split - 1);
    text := Trim(Copy(text, split + 1));
    lineType := StrToInt(String(lt));
    if (lineType = -1) then
        lineType := 9999;

    lineOut := AnsiString(Format('%-5d%s',
                                 [lineType,
                                  String(text) + StringOfChar(' ', 5 - (Length(text) mod 5))]));
    outFile.WriteLine(lineOut);
    Result := Length(lineOut);
end;

procedure TCaveUtilForm.LoadConfig;
var
    inFile: TSrcFileStream;
    line: AnsiString;
    rtype: String;
    rt: Integer;

    procedure SkipType;
    var
        id: Integer;
    begin
        id := 0;
        while (id <> 9999) do
        begin
            line := inFile.ReadLine;
            rtype := Trim(Copy(String(line), 1, 5));
            id := StrToInt(rtype);
        end;
    end;

    procedure LoadLocations;
    var
        id: Integer;
        l: TLocation;
    begin
        id := 0;
        while (id <> 9999) do
        begin
            line := inFile.ReadLine;
            rtype := Trim(Copy(String(line), 1, 5));
            id := StrToInt(rtype);
            if (id = 9999) then
                Continue;
            l := TLocation.Create;
            l.ID := id;
            l.Text.Add(Copy(String(line), 6));
            FLocations.Add(l);
        end;
    end;

    procedure LoadTravel;
    var
        i, from, too, verb: Integer;
        t: TTravel;
    begin
        from := 0;
        while (from <> 9999) do
        begin
            line := inFile.ReadLine;
            rtype := Trim(Copy(String(line), 1, 5));
            from := StrToInt(rtype);
            if (from = 9999) then
                Continue;
            rtype := Trim(Copy(String(line), 6, 5));
            too := StrToInt(rtype);
            t := TTravel.Create;
            FTravel.Add(t);
            t.From := from;
            t.Too := too;
            i := 11;
            rtype := Trim(Copy(String(line), i, 5));
            while (rtype <> '') do
            begin
                verb := StrToInt(rtype);
                t.AddWhen(verb);
                Inc(i, 5);
                rtype := Trim(Copy(String(line), i, 5));
            end;
        end;
    end;

    procedure LoadVerbs;
    var
        id: Integer;
        v: TVerb;
    begin
        id := 0;
        while (id <> 9999) do
        begin
            line := inFile.ReadLine;
            rtype := Trim(Copy(String(line), 1, 5));
            id := StrToInt(rtype);
            if (id = 9999) then
                Continue;
            v := TVerb.Create;
            v.ID := id;
            v.Verb := Copy(String(line), 6, 5);
            FVerbs.Add(v);
        end;
    end;

begin
    inFile := TSrcFileStream.Create('..\..\u494_adventure.txt');
    try
        while (not inFile.Eof) do
        begin
            line := inFile.ReadLine;
            rtype := Trim(Copy(String(line), 1, 5));
            rt := StrToInt(rtype);
            case rt of
              1:    LoadLocations;
              2:    SkipType;
              3:    LoadTravel;
              4:    LoadVerbs;
              5:    Break;
            end;
        end;
    finally
        inFile.Free;
    end;
end;

procedure TCaveUtilForm.MapBtnClick(Sender: TObject);
var
    i, lastFrom, l, w, v: Integer;
    s: String;
    t: TTravel;
begin
    LoadConfig;
    CaveMapForm.Show;
    lastFrom := 0;
    for i := 0 to FTravel.Count - 1 do
    begin
        t := FTravel[i];
        if (t.From <> lastFrom) then
        begin
            l := FindLoc(t.From);
            s := Format('From: %s', [FLocations[l].Text[0]]);
            CaveMapForm.Map.Lines.Add(s);
            lastFrom := t.From;
        end;
        l := FindLoc(t.Too);
        if (l = -1) then
            s := Format('To  : Custom(%d)', [t.Too])
        else
            s := Format('To  : %s', [FLocations[l].Text[0]]);
        CaveMapForm.Map.Lines.Add(s);
        s := 'When: ';
        for w := 0 to t.WhenCount - 1 do
        begin
            v := FindVerb(t.When[w]);
            if (v = -1) then
                s := Format('%s %-5.5d', [s, v])
            else
                s := Format('%s %s', [s, FVerbs[v].Verb]);
        end;
        CaveMapForm.Map.Lines.Add(s);
        CaveMapForm.Map.Lines.Add('');
    end;
end;

{ TLocation }

constructor TLocation.Create;
begin
    inherited Create;
    FText := TStringList.Create;
end;

destructor TLocation.Destroy;
begin
    FreeAndNil(FText);
    inherited Destroy;
end;

{ TTravel }

procedure TTravel.AddWhen(w: Integer);
begin
    SetLength(FWhen, Length(FWhen) + 1);
    FWhen[High(FWhen)] := w;
end;

function TTravel.GetWhen(i: Integer): Integer;
begin
    Result := FWhen[i];
end;

function TTravel.GetWhenCount: Integer;
begin
    Result := Length(FWhen);
end;

end.
