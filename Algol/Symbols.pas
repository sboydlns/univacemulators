unit Symbols;

interface

uses SysUtils, Classes;

type
  TDeclType = ( dtInteger = 0, dtReal, dtArray, dtString, dtLogical, dtUnknown );

  TSymbolType = ( stAny, stVariable, stLabel, stProcedure, stSwitch );

  TDeclaration = packed record
    ID: AnsiString;
    DType: TDeclType;
  end;


  TSymbol = class
  public
    ID: AnsiString;
    SymbolType: TSymbolType;
    DeclType: TDeclType;
    SymbolNum: Integer;
    IsStatic: Boolean;
    IsForward: Boolean;
    IsArray: Boolean;
    ArraySubscripts: Integer;
    NewArrayLabelNum: Integer;
  end;

  TSymbolTable = class(TStringList)
  private
    function GetSymbol(idx: Integer): TSymbol;
  public
    constructor Create; reintroduce;
    procedure Add(id: AnsiString; sym: TSymbol); reintroduce;
    function TryGetValue(id: AnsiString; st: TSymbolType; var sym: TSymbol): Boolean;
    property Symbols[idx: Integer]: TSymbol read GetSymbol;
  end;

const
  DeclTypes: array [TDeclType] of TDeclaration = (
    ( ID: 'INTEGER'; DType: dtInteger ),
    ( ID: 'REAL'; DType: dtReal ),
    ( ID: 'ARRAY'; DType: dtArray ),
    ( ID: 'STRING'; DType: dtString ),
    ( ID: 'BOOLEAN'; DType: dtLogical ),
    ( ID: ''; DType: dtUnknown )
  );

implementation

{ TSymbolTable }

procedure TSymbolTable.Add(id: AnsiString; sym: TSymbol);
begin
    if (IndexOf(String(id)) >= 0) then
        raise Exception.Create('Duplicate entry');
    AddObject(String(id), sym);
end;

constructor TSymbolTable.Create;
begin
    inherited;
    OwnsObjects := True;
end;

function TSymbolTable.GetSymbol(idx: Integer): TSymbol;
begin
    Result := TSymbol(Objects[idx]);
end;

function TSymbolTable.TryGetValue(id: AnsiString; st: TSymbolType; var sym: TSymbol): Boolean;
var
    i: Integer;
begin
    i := IndexOf(String(id));
    if ((i >= 0) and ((st = stAny) or (st = TSymbol(Objects[i]).SymbolType))) then
    begin
        Result := True;
        sym := TSymbol(Objects[i]);
    end else
    begin
        Result := False;
        sym := nil;
    end;
end;

end.
