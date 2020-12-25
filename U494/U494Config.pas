unit U494Config;

interface

uses SysUtils, Classes, Forms, XmlDoc, XmlIntf, U494Util;

type
  T494Config = class(TObject)
  private
    FMode: T494Mode;
    FLoadFiles: TStringList;
    function GetLoadFileCount: Integer;
    function GetLoadFiles(idx: Integer): String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(AOwner: TComponent; fname: String);
    property LoadFileCount: Integer read GetLoadFileCount;
    property LoadFiles[idx: Integer]: String read GetLoadFiles;
    property Mode: T494Mode read FMode;
  end;

var
  gConfig: T494Config;

implementation

{ T494Config }

constructor T494Config.Create;
begin
    FLoadFiles := TStringList.Create;
end;

destructor T494Config.Destroy;
begin
    FreeAndNil(FLoadFiles);
    inherited;
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
    node: IXmlNode;
    i: Integer;
begin
    FMode := m494;
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
            end;
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
