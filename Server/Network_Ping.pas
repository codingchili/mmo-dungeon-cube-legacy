unit Network_Ping;

// low priority.

{$IFDEF Linux}{$Mode Delphi}{$ENDIF}

interface

type
  TPing = Record
  private
    time, idle, average: integer;
    ip, id: string;
  End;

type
  TPingThread = (TThread);

var
  pings: array of TPing;

implementation

end.