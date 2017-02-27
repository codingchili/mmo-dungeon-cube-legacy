unit System_Licensing;

interface

//[todo: connect this to the database.]

{$IFDEF Linux}{$Mode Delphi}{$ENDIF}

uses {$IFDEF Win32}Clipboard,{$ENDIF}System_Log, SysUtils, SyncObjs, Math;

const
  SerialHi = 2326632141;   //valid key compositions   +05=max 7k=min
  SerialLo = 8979;

type
  TKeys = Record
    key: string[25];
  End;

procedure loadkeys(); // load active keys from .key
procedure savekeys(); // save active keys to .key
procedure initialize();
Function CheckRegistrationKey(key: string; isKeyGen: boolean): boolean;
Function MakeRegistrationKey(): string;

var // |value|
  letters : array [0..62] of string = ('e','9','q','C','0','B','p','y','t','m','i',
  'A','6','E','x','2','s','F','d','5','G','J','l','K','M','f','w','L','r','z','H',
  'o','3','b','N','h','8','I','O','Q','T','4','R','u','S','c','Y','W','n','Y','7',
  'V','k','X','g','Z','a','Y','1','D','P','j','U');
  validkeys: array of TKeys;

implementation

Function CheckRegistrationKey(key: string; isKeyGen: boolean): boolean;
var
j,w: integer;
blackkey: string;
value: extended;
begin
result := false;

if length(key) <> 25 then exit;

value := 0;

   for j := 1 to length(key) do begin
    w := -1;
    repeat                //calculate key value
      w := w+1;
    until
    letters[w] = key[j];

    value := value+(w*j);    //nonlinear

    case w of
    2: value := value+(math.power(j,w));
    4: value := value+(math.power(j,w));
    5: value := value+(math.power(j,w));
    8: value := value+(math.power(j,w));
    20: value := value+(math.power(j,2));
    10: value := value+86;
    13: value := value+(w*j);
    14: value := value+94;
    19: value := value+(w*j)+1;
    23: value := value+77;
    end;
   end;

  if (value <> serialHi) and (value <> serialLo) then result := false else
  result := true;
end;

// this needs to be in a thread. The server will lag when executed.
Function MakeRegistrationKey(): string;
var
  j, w: integeR;
  key: string;
  {$IFDEF Win32}clipboard: TClipBoard;  {$ENDIF}
begin
  Randomize;
  key := '';

  repeat
    key := '';

    for j := 1 to 25 do
      key := key + letters[random(62)];

  until CheckRegistrationKey(key, true) = true; // true = testing mode.

  {$IFDEF Win32}
  clipboard := TClipBoard.Create;
  clipboard.AsText := key;
  {$ENDIF}


  result := key;
end;


procedure loadkeys();
var
  keys: File of TKeys;
begin
  setlength(validkeys, 0);

  if not(fileexists('data/key.data')) then
    exit;

  assignFile(keys, 'data/key.data');
  reset(keys);

  while not eof(keys) do
  begin
    setlength(validkeys, length(validkeys) + 1);
    read(keys, validkeys[high(validkeys)]);
  end;

end;

procedure savekeys();
begin
  // save active/valid keys to file.
end;

procedure initialize();
begin
  print('Loading active Keys..', Brown);
  cs := TCriticalSection.Create;
  setlength(validkeys, 0);
  loadkeys();
  print(#9 + ' Done.', LightGreen, false);
end;

begin

end.