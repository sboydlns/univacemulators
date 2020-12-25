unit Literals;

interface

uses SysUtils, Generics.Collections, Symbols;

type
  TLiteral = class
  public
    DeclType: TDeclType;
    Value: AnsiString;
    LiteralNum: Integer;
    RefCount: Integer;
    function AsInteger: Integer;
  end;

  TLiteralList = class(TList<TLiteral>)
  public
    function Add(lt: TDeclType; value: AnsiString): Integer; reintroduce;
  end;

implementation

{ TLiteralList }

function TLiteralList.Add(lt: TDeclType; value: AnsiString): Integer;
var
    i: Integer;
    lit: TLiteral;
begin
    for i := 0 to Count - 1 do
    begin
        if ((Items[i].DeclType = lt) and (Items[i].Value = value)) then
        begin
            Inc(Items[i].RefCount);
            Result := i;
            Exit;
        end;
    end;
    lit := TLiteral.Create;
    lit.DeclType := lt;
    lit.Value := value;
    lit.RefCount := 1;
    Result := inherited Add(lit);
    lit.LiteralNum := Count;
end;

{ TLiteral }

function TLiteral.AsInteger: Integer;
begin
    if (DeclType = dtInteger) then
    begin
        if (not TryStrToInt(String(Value), Result)) then
            Result := 0;
    end else
        Result := 0;
end;

end.
