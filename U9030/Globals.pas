unit Globals;

interface

uses U9030Types, Channels, Cpu, Memory;

var
  Adapters: TChannelList;
  Processor: TCpu;
  Core: TMemory;
  PSW: TPSW;
  CurInst: TInstruction;
  Opcodes: TOpcodeList;

implementation

end.
