unit Config;

interface

uses SysUtils, Classes, XmlDoc, XmlIntf;

type
  TConfigDiskType = ( cdtNone, cdt8416, cdt8418 );

  TConfigDisk = record
  public
    DiskType: TConfigDiskType;
    ChannelNum: Integer;
    DeviceNum: Integer;
    DiskFile: String;
    procedure Clear;
  end;

  TConfigPortType = ( cptNone, cptUniscope );

  TConfigPort = record
    PortType: TConfigPortType;
    DeviceNum: Integer;
    Rid: Integer;
    procedure Clear;
  end;

  TConfig = class(TObject)
  public
    Disks: array of TConfigDisk;
    Ports: array of TConfigPort;
    IODelay: Integer;
    IOTraceEnabled: Boolean;
    SvcTraceEnabled: Boolean;
    procedure Load(xml: TXmlDocument);
  end;

implementation

{ TConfig }

procedure TConfig.Load(xml: TXmlDocument);
var
    s: String;
    node: IXmlNode;
    disk: TConfigDisk;
    port: TConfigPort;
begin
    SetLength(Disks, 0);
    IODelay := 1;
    IOTraceEnabled := False;
    SvcTraceEnabled := False;

    node := xml.DocumentElement.ChildNodes.FindNode('iodelay');
    if (Assigned(node)) then
        if (not TryStrToInt(node.Text, IODelay)) then
            IODelay := 1;

    node := xml.DocumentElement.ChildNodes.FindNode('iotrace');
    if (Assigned(node)) then
        IOTraceEnabled := (LowerCase(node.Text) = 'y');

    node := xml.DocumentElement.ChildNodes.FindNode('svctrace');
    if (Assigned(node)) then
        SvcTraceEnabled := (LowerCase(node.Text) = 'y');

    node := xml.DocumentElement.ChildNodes.FindNode('ida');
    if (Assigned(node) and (node.ChildNodes.Count > 0)) then
    begin
        node := node.ChildNodes[0];
        while (Assigned(node) and (node.NodeName = 'disk')) do
        begin
            disk.Clear;
            disk.ChannelNum := 3;
            s := node.Attributes['type'];
            if (s = '8416') then
                disk.DiskType := cdt8416
            else if (s = '8418') then
                disk.DiskType := cdt8418
            else
                raise Exception.CreateFmt('Invalid disk type (%s)', [s]);
            s := node.Attributes['addr'];
            if (not TryStrToInt(s, disk.DeviceNum)) then
                raise Exception.CreateFmt('Invalid disk address (%s)', [s]);
            if ((disk.DeviceNum < 0) or (disk.DeviceNum > 7)) then
                raise Exception.CreateFmt('Invalid disk address (%s)', [s]);
            disk.DiskFile := node.Attributes['file'];
            SetLength(Disks, Length(Disks) + 1);
            Disks[High(Disks)] := disk;
            node := node.NextSibling;
        end;
    end;

    node := xml.DocumentElement.ChildNodes.FindNode('ipc');
    if (Assigned(node) and (node.ChildNodes.Count > 0)) then
    begin
        node := node.ChildNodes[0];
        while (Assigned(node) and (node.NodeName = 'commport')) do
        begin
            port.Clear;
            s := node.Attributes['type'];
            if (s = 'uniscope') then
                port.PortType := cptUniscope
            else
                raise Exception.CreateFmt('Invalid comm port type (%s)', [s]);
            s := node.Attributes['addr'];
            if (not TryStrToInt(s, port.DeviceNum)) then
                raise Exception.CreateFmt('Invalid comm port address (%s)', [s]);
            if ((port.DeviceNum < 4) or (disk.DeviceNum > 15)) then
                raise Exception.CreateFmt('Invalid comm port address (%s)', [s]);
            s := node.Attributes['rid'];
            if ((s <> '') and (not TryStrToInt(s, port.Rid))) then
                raise Exception.CreateFmt('Invalid RID (%s)', [s]);
            SetLength(Ports, Length(Ports) + 1);
            Ports[High(Ports)] := port;
            node := node.NextSibling;
        end;
    end;
end;

{ TConfigDisk }

procedure TConfigDisk.Clear;
begin
    DiskType := cdtNone;
    ChannelNum := 0;
    DeviceNum := -1;
    DiskFile := '';
end;

{ TConfigPort }

procedure TConfigPort.Clear;
begin
    PortType := cptNone;
    DeviceNum := -1;
    Rid := $21;
end;

end.
