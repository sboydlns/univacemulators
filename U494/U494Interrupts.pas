unit U494Interrupts;

interface

uses SysUtils, Generics.Collections, SyncObjs;

type
  T494InterruptType = ( intIO, intClock );
  T494Interrupt = packed record
  public
    IType: T494InterruptType;
    Vector: Smallint;               // Address in low memory of interrupt vector
    Channel: Byte;
    Status: UInt32;
  end;

  T494InterruptQueue = class(TQueue<T494Interrupt>)
  private
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    function Dequeue: T494Interrupt;
    procedure Enqueue(const Value: T494Interrupt);
  end;

implementation

{ T494InterruptQueue }

constructor T494InterruptQueue.Create;
begin
    inherited Create;
    FLock := TCriticalSection.Create;
end;

function T494InterruptQueue.Dequeue: T494Interrupt;
begin
    FLock.Acquire;
    try
        Result := inherited Dequeue;
    finally
        FLock.Release;
    end;
end;

destructor T494InterruptQueue.Destroy;
begin
    FreeAndNil(FLock);
    inherited Destroy;
end;

procedure T494InterruptQueue.Enqueue(const Value: T494Interrupt);
var
    int: T494Interrupt;
    items: TArray<T494Interrupt>;
begin
    FLock.Acquire;
    try
        // Do not queue multiple instances of clock interrupts. The
        // clocks run constantly even if the CPU is stopped and we
        // don't want to flood the queue with clock interrupts.
        if (Value.IType = intClock) then
        begin
            items := ToArray;
            for int in items do
            begin
                if (int.Vector = Value.Vector) then
                    Exit;
            end;
        end;
        inherited Enqueue(Value);
    finally
        FLock.Release;
    end;
end;

end.
