unit Bcd;

interface

uses FmtBcd;

type
  TBcdHelper = record helper for TBcd
  private
    function GetNibble(idx: Integer): Byte;
    procedure SetNibble(idx: Integer; const Value: Byte);
  public
    procedure Clear;
    property Nibble[idx: Integer]: Byte read GetNibble write SetNibble;
  end;

function PackedSign(b: Byte): Smallint;

implementation

function PackedSign(b: Byte): Smallint;
begin
    case (b and $0F) of
      $0A,
      $0C,
      $0E,
      $0F:      Result := 1;
      else      Result := -1;
    end;
end;

{ TBcdHelper }

procedure TBcdHelper.Clear;
var
    i: Integer;
begin
    Precision := 0;
    SignSpecialPlaces := 0;
    for i := Low(Fraction) to High(Fraction) do
        Fraction[i] := 0;
end;

function TBcdHelper.GetNibble(idx: Integer): Byte;
begin
    Result := Fraction[idx div 2];
    if ((idx mod 2) = 0) then
        Result := Result shr 4
    else
        Result := Result and $0F;
end;

procedure TBcdHelper.SetNibble(idx: Integer; const Value: Byte);
var
    i: Integer;
    b: Byte;
begin
    i := idx div 2;
    b := Fraction[i];
    if ((idx mod 2) = 0) then
        Fraction[i] := (b and $0F) or (Value shl 4)
    else
        Fraction[i] := (b and $F0) or (Value and $0F);
end;

end.
