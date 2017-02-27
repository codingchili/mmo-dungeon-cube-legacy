unit System_Cooldown;

{ manage spell cooldowns, make sure that no user can cast a spell while it is on cooldown,
  tell the user when the spell comes off cooldown, and he updates his ui. in case of network jitter
  this will prevent the server from denying a spellcast that may have not have been on cooldown. }

interface

uses SyncObjs, Classes;

type
    TCooldown = record
        sender: Pointer;
        timer, spell: integer;
    end;

type
    TCooldownMan = class
    private
        cooldowns: TList;
        cs: TCriticalSection;
    public
        procedure add(sender: Pointer; cooldown: single; spell: integer);
        procedure process(TPS: integer);
        function mayfire(sender: Pointer; spell: integer): boolean;
        constructor create();
    end;

implementation

uses Engine_Characters, Conf_Server;

constructor TCooldownMan.create;
begin
    cs := TCriticalSection.create;
    cooldowns := TList.create;
end;

// decrease ttl and remove those who have reached 0 ttl.
procedure TCooldownMan.process(TPS: integer);
var
    i: integer;
begin
    cs.Acquire;
    try
        i := cooldowns.Count - 1;
        while (i > -1) do
        begin
            TCooldown(cooldowns.Items[i]^).timer := TCooldown(cooldowns.Items[i]^).timer - (60 div TPS);
            if (TCooldown(cooldowns.Items[i]^).timer < 1) then
                cooldowns.Delete(i);

            dec(i);
        end;
    finally
        cs.Release;
    end;
end;

procedure TCooldownMan.add(sender: Pointer; cooldown: single; spell: integer);
var
    len: integer;
    cd: TCooldown;
begin
    cs.Acquire;
    try
        cd.sender := sender;
        cd.timer := trunc(cooldown * (60 / TPS));
        cd.spell := spell;
        cooldowns.add(@cd);
    finally
        cs.Release;
    end;
end;

function TCooldownMan.mayfire(sender: Pointer; spell: integer): boolean;
var
    i: integer;
begin
    result := true;
    cs.Acquire;
    try
        for i := 0 to cooldowns.Count - 1 do
            if (TCooldown(cooldowns.Items[i]^).sender = sender) and (TCooldown(cooldowns.Items[i]^).spell = spell) then
                result := false;
    finally
        cs.Release;
    end;
end;

end.
