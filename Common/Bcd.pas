unit Bcd;

interface

uses FmtBcd;

type
  TBcdHelper = record helper for TBcd
  private
    function GetNibble(idx: Integer): Byte;
    procedure SetNibble(idx: Integer; const Value: Byte);
    function GetScale: Integer;
    procedure SetScale(const Value: Integer);
  public
    procedure Clear;
    function SignifDigits: Integer;
    property Nibble[idx: Integer]: Byte read GetNibble write SetNibble;
    property Scale: Integer read GetScale write SetScale;
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

function TBcdHelper.GetScale: Integer;
begin
    Result := SignSpecialPlaces and $3f;
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

procedure TBcdHelper.SetScale(const Value: Integer);
begin
    SignSpecialPlaces := (SignSpecialPlaces and $c0) or (Value and $3f);
end;

function TBcdHelper.SignifDigits: Integer;
var
    i, intDigits: Integer;
begin
    intDigits := Precision - Scale;
    Result := intDigits;
    for i := 0 to intDigits - 1 do
    begin
        if (Nibble[i] = 0) then
            Dec(Result)
        else
            Break;
    end;
end;

end.
