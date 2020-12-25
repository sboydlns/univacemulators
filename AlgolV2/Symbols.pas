unit Symbols;

interface

uses Windows, SysUtils, Classes, Contnrs;

type
  TDeclType = ( dtInteger = 0, dtReal, dtArray, dtString, dtLogical, dtVoid, dtUnknown );

  TSymbolType = ( stAny, stVariable, stLabel, stProcedure, stSwitch );

  TDeclaration = packed record
    ID: AnsiString;
    DType: TDeclType;
  end;

  TSymbol = class(TObject)
  public
    ID: AnsiString;
    SymbolType: TSymbolType;
    DeclType: TDeclType;
    SymbolNum: Integer;
    StringLength: Integer;
    ArraySize: Integer;
    IsStatic: Boolean;
    IsForward: Boolean;
    IsArray: Boolean;
    Indices: TObjectList;
    ArraySubscripts: Integer;
    NewArrayLabelNum: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(src: TSymbol);
  end;

  TSwitchSymbol = class(TSymbol)
  public
    Targets: TObjectList;
    FirstSwitchNum: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TSubstringSymbol = class(TSymbol)
  public
    Container: TSymbol;
    Start: Integer;
    Length: Integer;
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
  DeclTypes: array [1..7] of TDeclaration = (
    ( ID: 'INTEGER'; DType: dtInteger ),
    ( ID: 'REAL'; DType: dtReal ),
    ( ID: 'REAL2'; DType: dtReal ),
    ( ID: 'ARRAY'; DType: dtArray ),
    ( ID: 'STRING'; DType: dtString ),
    ( ID: 'BOOLEAN'; DType: dtLogical ),
    ( ID: ''; DType: dtUnknown )
  );

implementation

uses Expressions;

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

{ TSymbol }

procedure TSymbol.Assign(src: TSymbol);
begin
    ID := src.ID;
    SymbolType := src.SymbolType;
    DeclType := src.DeclType;
    SymbolNum := src.SymbolNum;
    StringLength := src.StringLength;
    IsStatic := src.IsStatic;
    IsForward := src.IsForward;
end;

constructor TSymbol.Create;
begin
    Indices := TObjectList.Create;
end;

destructor TSymbol.Destroy;
begin
    FreeAndNil(Indices);
    inherited;
end;

{ TSwitchSymbol }

constructor TSwitchSymbol.Create;
begin
    Targets := TObjectList.Create;
end;

destructor TSwitchSymbol.Destroy;
begin
    FreeAndNil(Targets);
    inherited;
end;

end.
