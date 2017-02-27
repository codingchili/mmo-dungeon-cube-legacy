unit Automated_Tests;

interface

uses sysutils, classes, dialogs;

type
  TTester = class(TThread)
  protected
    procedure Execute; Override;
  public
    constructor Create;
  private
    procedure ConnectWorldServer();
    procedure LoginMetaServer();
    procedure WorldCharacterList();
    procedure ViewThreadSafety();
  end;

procedure Run();

var
  init: boolean;
  test: TTester;

implementation

uses GUI_CharSelect, Network_WorldServer, Network_World, Main,
  Network_Metaserver, GUI_ServerSelect, GUI_LoginScreen;

procedure TTester.ConnectWorldServer();
begin
  CharSelect.SetView;
  worldcon := TConnection.Create('127.0.0.1', 15566);
  WorldConnection.sethost('127.0.0.1');
  WorldConnection.setport(15566);
end;

procedure TTester.LoginMetaServer;
begin
  MetaCon.login('chilimannenKY12' + chr(random($7E) + 21) +
    chr(random($7E) + 21), 'theFuriousPasswed11', 'thealkeyyyyjnklnlkjwnljn' +
    chr(random($7E) + 21) + chr(random($7E) + 21));
end;

procedure TTester.WorldCharacterList();
begin
  ServerScreen.SetView;

  while (Main.View = TView.TLogin) do
    sleep(1); // wait for login

  while true do
  begin
    while (Main.View = TView.TCharacterSelect) do
      sleep(100);
    //ConnectWorldServer();
    sleep(1000);
  end;
end;

procedure TTester.ViewThreadSafety;
var
  View: integer;
begin
  while true do
  begin
    View := random(3);
    case (View) of
      0:
        LoginScreen.SetView();
      1:
        ServerScreen.SetView();
      2:
        CharSelect.SetView();
      3:
        ;
    end;
    sleep(85);
  end;
end;

procedure TTester.Execute;
begin
  //ViewThreadSafety();

  sleep(1000);
  self.LoginMetaServer;
  sleep(1000);
  self.ConnectWorldServer;
  sleep(1000);
  self.WorldCharacterList;
end;

constructor TTester.Create;
begin
  inherited Create(false);
end;

procedure Run();
begin
  if init then
    exit;

  init := true;

  //test := TTester.Create;
end;

end.
