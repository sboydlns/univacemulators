unit Statements;

interface

uses SysUtils, Generics.Collections, Symbols;

type
  TStatement = class
  end;

  TBlock = class(TStatement)
  public
    IsProgram: Boolean;
    IsMain: Boolean;
    IsProc: Boolean;
    BlockNum: Integer;
    Symbols: TSymbolTable;
    constructor Create;
    destructor Destroy; override;
  end;

  TBlockStack  = class(TStack<TBlock>)
  end;


implementation

{ TBlock }

constructor TBlock.Create;
begin
    Symbols := TSymbolTable.Create;
end;

destructor TBlock.Destroy;
begin
    FreeAndNil(Symbols);
    inherited;
end;

end.
