unit Globals;

interface

uses EmulatorTypes, U9030Types, Channels, Cpu, Memory, Config;

var
  Adapters: TChannelList;
  Processor: TCpu;
  Core: TMemory;
  PSW: TPSW;
  CurInst: TInstruction;
  Opcodes: TOpcodeList;
  Configuration: TConfig;

implementation


end.
