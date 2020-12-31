unit U494Config;

interface

uses SysUtils, Classes, Forms, XmlDoc, XmlIntf, U494Util, Generics.Collections;

type
  T494Drum = record
    Chan: Integer;
    DrumType: T494DrumType;
    DrumFile: String;
  end;

  TDrumList = class(TList<T494Drum>)
  end;

  T494Config = class(TObject)
  private
    FMode: T494Mode;
    FConsoleChan: Integer;
    FConsoleType: T494ConsoleType;
    FRdrPunChan: Integer;
    FPrinterChan: Integer;
    FDrums: TDrumList;
    FLoadFiles: TStringList;
    function GetLoadFileCount: Integer;
    function GetLoadFiles(idx: Integer): String;
    function GetDrumCount: Integer;
    function GetDrums(idx: Integer): T494Drum;
  public
    constructor Create;
    destructor Destroy; override;
    property ConsoleChan: Integer read FConsoleChan;
    property ConsoleType: T494ConsoleType read FConsoleType;
    property DrumCount: Integer read GetDrumCount;
    property Drums[idx: Integer]: T494Drum read GetDrums;
    procedure Load(AOwner: TComponent; fname: String);
    property LoadFileCount: Integer read GetLoadFileCount;
    property LoadFiles[idx: Integer]: String read GetLoadFiles;
    property Mode: T494Mode read FMode;
    property PrinterChan: Integer read FPrinterChan;
    property RdrPunChan: Integer read FRdrPunChan;
  end;

var
  gConfig: T494Config;

implementation

{ T494Config }

constructor T494Config.Create;
var
    drum: T494Drum;
begin
    FLoadFiles := TStringList.Create;
    FDrums := TDrumList.Create;
    FMode := m494;
    FConsoleChan := 0;
    FRdrPunChan := -1;
    FPrinterChan := -1;
    FConsoleType := ct1232;
    case FMode of
      m494:     FLoadFiles.Add('mos.mem');
      m490:     FLoadFiles.Add('mos.mem');
      m1230:    FLoadFiles.Add('monitor6.mem');
    end;
    drum.Chan := 5;
    drum.DrumType := dtFH880;
    drum.DrumFile := 'sysvol.drum';
    FDrums.Add(drum);
end;

destructor T494Config.Destroy;
begin
    FreeAndNil(FLoadFiles);
    FreeAndNil(FDrums);
    inherited;
end;

function T494Config.GetDrumCount: Integer;
begin
    Result := FDrums.Count;
end;

function T494Config.GetDrums(idx: Integer): T494Drum;
begin
    Result := FDrums[idx];
end;

function T494Config.GetLoadFileCount: Integer;
begin
    Result := FLoadFiles.Count;
end;

function T494Config.GetLoadFiles(idx: Integer): String;
begin
    Result := FLoadFiles[idx];
end;

procedure T494Config.Load(AOwner: TComponent; fname: String);
var
    xml: TXmlDocument;
    node, node1: IXmlNode;
    i, j: Integer;
    drum: T494Drum;

    function GetChan(node: IXmlNode): Integer;
    begin
        if (not TryStrToInt(node1.Text, Result)) then
            raise Exception.Create('Invalid console channel #');
        if (FMode = m494) then
            if ((Result < 0) or (Result > 23)) then
                raise Exception.Create('Invalid console channel #')
        else
            if ((Result < 0) or (Result > 15)) then
                raise Exception.Create('Invalid console channel #');
    end;

begin
    if (not FileExists(fname)) then
        raise Exception.Create('Config file not found');

    FLoadFiles.Clear;
    FDrums.Clear;
    xml := TXmlDocument.Create(AOwner);
    try
        xml.LoadFromFile(fname);
        for i :=0 to xml.DocumentElement.ChildNodes.Count - 1 do
        begin
            node := xml.DocumentElement.ChildNodes[i];
            if (node.NodeName = 'mode') then
            begin
                if (node.Text = '494') then
                    FMode := m494
                else if (node.Text = '490') then
                    Fmode := m490
                else if (node.Text = '1230') then
                    FMode := m1230
                else
                    raise Exception.CreateFmt('Configured mode (%s) invalid', [node.Text]);
            end else if (node.NodeName = 'load') then
            begin
                FLoadFiles.Add(node.Text);
            end else if (node.NodeName = 'console') then
            begin
                for j := 0 to node.ChildNodes.Count  -1 do
                begin
                    node1 := node.ChildNodes[j];
                    if (node1.NodeName = 'chan') then
                    begin
                        FConsoleChan := GetChan(node1);
                    end else if (node1.NodeName = 'type') then
                    begin
                        if (node1.Text = '1232') then
                            FConsoleType := ct1232
                        else
                            raise Exception.Create('Invalid console type');
                    end;
                end;
            end else if (node.NodeName = 'rdrpun') then
            begin
                for j := 0 to node.ChildNodes.Count  -1 do
                begin
                    node1 := node.ChildNodes[j];
                    if (node1.NodeName = 'chan') then
                    begin
                        FRdrPunChan := GetChan(node1);
                    end;
                end;
            end else if (node.NodeName = 'printer') then
            begin
                for j := 0 to node.ChildNodes.Count  -1 do
                begin
                    node1 := node.ChildNodes[j];
                    if (node1.NodeName = 'chan') then
                    begin
                        FPrinterChan := GetChan(node1);
                    end;
                end;
            end else if (node.NodeName = 'drum') then
            begin
                drum.Chan := 5;
                drum.DrumType := dtFH880;
                drum.DrumFile := 'sysvol.drum';
                for j := 0 to node.ChildNodes.Count  -1 do
                begin
                    node1 := node.ChildNodes[j];
                    if (node1.NodeName = 'chan') then
                    begin
                        drum.Chan := GetChan(node1);
                    end else if (node1.NodeName = 'type') then
                    begin
                        if (node1.Text = 'fh880') then
                            drum.DrumType := dtFH880
                        else
                            raise Exception.Create('Invalid drum type');
                    end else if (node1.NodeName = 'file') then
                    begin
                        drum.DrumFile := node1.Text;
                    end;
                end;
                FDrums.Add(drum);
            end;
        end;
        if (FLoadFiles.Count = 0) then
            case FMode of
              m494:     FLoadFiles.Add('mos.mem');
              m490:     FLoadFiles.Add('mos.mem');
              m1230:    FLoadFiles.Add('monitor6.mem');
            end;
        if (FDrums.Count = 0) then
        begin
            drum.Chan := 5;
            drum.DrumType := dtFH880;
            drum.DrumFile := 'sysvol.drum';
        end;
        if (FLoadFiles.Count = 0) then
            case FMode of
              m494:     FLoadFiles.Add('mos.mem');
              m490:     FLoadFiles.Add('mos.mem');
              m1230:    FLoadFiles.Add('monitor6.mem');
            end;
        if (FDrums.Count = 0) then
        begin
            drum.Chan := 5;
            drum.DrumType := dtFH880;
            drum.DrumFile := 'sysvol.drum';
            FDrums.Add(drum);
        end;
        xml.Free;
    except
        xml.Free;
        raise;
    end;
end;

initialization
    gConfig := T494Config.Create;

end.
