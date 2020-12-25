unit U9200Config;

interface

uses SysUtils, Classes, Forms, XmlDoc, XmlIntf;

type
  TU92Config = class(TObject)
  private
    FIOTraceEnabled: Boolean;
    FTraceDir: String;
    FTapeChannel: Integer;
    FTapeCount: Integer;
  public
    procedure Load(xml: TXmlDocument);
    property IOTraceEnabled: Boolean read FIOTraceEnabled;
    property TraceDir: String read FTraceDir;
    property TapeChannel: Integer read FTapeChannel;
    property TapeCount: Integer read FTapeCount;
  end;

var
    gConfig: TU92Config;

implementation

{ TU92Config }

procedure TU92Config.Load(xml: TXmlDocument);
var
    node: IXmlNode;
    fname: String;
begin
    FTapeChannel := 5;
    FTapeCount := 8;
    try
        fname := Format('%s%s', [ExtractFilePath(Application.ExeName), 'U9200.cfg']);
        try
            xml.LoadFromFile(fname);
        except
            fname := Format('%s..\..\%s', [ExtractFilePath(Application.ExeName), 'U9200.cfg']);
            xml.LoadFromFile(fname);
        end;
        xml.Active;
        node := xml.DocumentElement.ChildNodes.FindNode('iotrace');
        if (Assigned(node)) then
            FIOTraceEnabled := (LowerCase(node.Text) = 'y');
        node := xml.DocumentElement.ChildNodes.FindNode('tracedir');
        if (Assigned(node)) then
            FTraceDir := node.Text;
        node := xml.DocumentElement.ChildNodes.FindNode('tapechannel');
        if (Assigned(node)) then
        begin
            if (not TryStrToInt(node.Text, FTapeChannel)) then
                FTapeChannel := 5;
        end;
        node := xml.DocumentElement.ChildNodes.FindNode('tapecount');
        if (Assigned(node)) then
        begin
            if (not TryStrToInt(node.Text, FTapeCount)) then
                FTapeCount := 8;
        end;
    except
        raise;
    end;
end;

initialization
    gConfig := TU92Config.Create;

end.
