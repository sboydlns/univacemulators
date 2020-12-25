unit U1005Files;

interface

uses SysUtils, CardFile, U1005Types;

type
  TFedSys1005ObjectFile = class(TCardFileStream)
  private
    function GetCardType: Byte;
    function GetEndAddr: I1005Addr;
    function GetStartAddr: I1005Addr;
  public
    Buffer: TCardRec;
    function Read: Integer; reintroduce;
    property CardType: Byte read GetCardType;
    property EndAddr: I1005Addr read GetEndAddr;
    property StartAddr: I1005Addr read GetStartAddr;
  end;

  TComm1005ObjectFile = class(TCardFileStream)
  private
    function GetCardType: Byte;
    function GetEndAddr: I1005Addr;
    function GetStartAddr: I1005Addr;
    function GetFromAddr: I1005Addr;
  public
    Buffer: TCardRec;
    function Read: Integer; reintroduce;
    property CardType: Byte read GetCardType;
    property EndAddr: I1005Addr read GetEndAddr;
    property FromAddr: I1005Addr read GetFromAddr;
    property StartAddr: I1005Addr read GetStartAddr;
  end;

implementation

uses EmulatorTypes;

{ T1005ObjectFile }

function TFedSys1005ObjectFile.GetCardType: Byte;
begin
    Result := Buffer.Columns[74];
end;

function TFedSys1005ObjectFile.GetEndAddr: I1005Addr;
begin
    Result := T1005FedSysAddr.Create;
    Result.SetAddr(Buffer.Columns[79], Buffer.Columns[80]);
end;

function TFedSys1005ObjectFile.GetStartAddr: I1005Addr;
begin
    Result := T1005FedSysAddr.Create;
    Result.SetAddr(Buffer.Columns[77], Buffer.Columns[78]);
end;

function TFedSys1005ObjectFile.Read: Integer;
begin
    Result := ReadXS3(Buffer);
end;

{ TComm1005ObjectFile }

function TComm1005ObjectFile.GetCardType: Byte;
begin
    Result := Buffer.Columns[74];
end;

function TComm1005ObjectFile.GetEndAddr: I1005Addr;
begin
    Result := T1005CommAddr.Create;
    Result.SetAddr(Buffer.Columns[79], Buffer.Columns[80]);
end;

function TComm1005ObjectFile.GetFromAddr: I1005Addr;
begin
    Result := T1005CommAddr.Create;
    Result.SetAddr(Buffer.Columns[75], Buffer.Columns[76]);
end;

function TComm1005ObjectFile.GetStartAddr: I1005Addr;
begin
    Result := T1005CommAddr.Create;
    Result.SetAddr(Buffer.Columns[77], Buffer.Columns[78]);
end;

function TComm1005ObjectFile.Read: Integer;
begin
    Result := ReadXS3(Buffer);
end;

end.
