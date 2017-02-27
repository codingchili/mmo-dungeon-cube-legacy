unit System_Bans;

// contains the banned IPs and the reason, query this when receiving a packet. (before anything else)
// ipv6 ?
// on packet from banned ip return reason.

interface

{$IFDEF Linux}
{$MODE DELPHI}
{$ENDIF}

uses SysUtils;

type
  TIPBan = record
  public
    ip: string[25];
    reason: string[85];
    // time, left..
  end;

type
  TIPBanMan = class
  public
    IpBan: array of TIPBan;
    constructor create();
  end;

var
  IPBanMan: TIPBanMan;

implementation

constructor TIPBanMan.create;
begin
  setlength(IpBan, 0);
end;

// is banned?
// ban, (aID | IP, 'reason')

end.