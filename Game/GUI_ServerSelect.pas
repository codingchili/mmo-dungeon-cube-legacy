unit GUI_ServerSelect;

// todo: have different backgrounds for offline and online servers.

{$IFDEF Linux}
{$Mode Delphi}
{$ENDIF}

interface

uses GUI_Engine, GUI_element, GUI_Element_Background, GUI_Element_Panel, GUI_Element_Text, GUI_Element_Button, Controls,
    System_UtilExt, {$IFDEF Win32}Windows, {$ENDIF} Network_MetaServer,
    SysUtils, Effects_Blending, Conf_Protocol, System_Keyboard, Mouse_Proxy, math;

const
    MAXSERVERLIST = 100;

type
    TSortMode = (Ascending, Descending);

type
    TSortColumn = (Name, Online, Population, Mode);

type
    TServerInfo = Record
    public
        Online: boolean;
        port, players: integer;
        ip, name_text, mode_text, status_text: string;
        bar: TGUIButton;
        modes, quota, name, Population: TGUIText;
    End;

type
    TServerScreen = class
    private
        BG: TGUIBackGround;
        serverframe, typeframe: TGUIPanel;
        name, types, Online, Population: TGUIText;
        backbutton, exitbutton: TGUIButton;
        ServerList: array [0 .. MAXSERVERLIST] of TServerInfo; // max servers
        servers: integer;
        updatelock: boolean;
        sortmode: TSortMode;
        sortcolumn: TSortColumn;
        procedure exit();
        procedure back();
        procedure OnServerList;
        procedure SortName();
        procedure SortPlayers();
        procedure SortPopulation();
        procedure SortType();
        procedure ResolvePos;
        function serverpos(name: string): integer;
        function statuscolor(status: string): cardinal;
    public
        engine: TGUIEngine;
        constructor create();
        procedure NullServer();
        procedure AddServer(name, Mode, ip: string; port, players, slots: integer; Population, Online: string);
        procedure Logic;
        procedure Draw;
        procedure SetView();
        procedure Resized();
    end;

procedure Initialize;

var
    serverscreen: TServerScreen;

implementation

uses Main, System_Initializer, Network_WorldServer, GUI_CharSelect,
    network_world, Conf_Spritemap, System_Log, GUI_StatusBox, GUI_Menu, GUI_LoginScreen, GUI_Banner, Rectarea;

procedure Initialize();
begin
    serverscreen := TServerScreen.create;
end;

procedure TServerScreen.Resized;
begin
    serverframe.rect.top := initializer.sheight div 2 - (SERVERFRAME_HEIGHT div 2);
    serverframe.rect.left := initializer.swidth div 2 - (SERVERFRAME_WIDTH div 2);

    exitbutton.rect.top := initializer.sheight - 60;
    exitbutton.rect.left := initializer.swidth - 74;
    backbutton.rect.top := initializer.sheight - 60;
    backbutton.rect.left := initializer.swidth - 114;
end;

procedure TServerScreen.SetView;
begin
    GUIBanner.TextFromFile('data/banner_server.txt', -1);
    Main.View := TView.TServerSelect;
end;

procedure TServerScreen.exit();
begin
    // cleanup
    halt;
end;

procedure TServerScreen.back();
begin
    LoginScreen.SetView();
end;

// recalculate  server info rect.
procedure TServerScreen.ResolvePos();
var
    i: integer;
begin
    for i := 0 to servers - 1 do
    begin
        ServerList[i].bar.rect.top := (SERVERBAR_HEIGHT + 2) * (i + 1) + 8;;
    end;
end;

procedure TServerScreen.SortPlayers();
var
    tmp: TServerInfo;
    i: integer;
    dirty: boolean;
begin
    // a, b, c
    sortcolumn := TSortColumn.Online;
    dirty := true;
    if (self.sortmode = TSortMode.Ascending) then
    begin
        while (dirty) do
        begin
            dirty := false;
            for i := 1 to servers - 1 do
            begin
                if (ServerList[i - 1].players > ServerList[i + 0].players) then
                begin
                    tmp := ServerList[i + 0];
                    ServerList[i + 0] := ServerList[i - 1];
                    ServerList[i - 1] := tmp;
                    dirty := true;
                end;
            end;
        end;
        self.sortmode := TSortMode.Descending;
    end
    // c, b, a
    else if (self.sortmode = TSortMode.Descending) then
    begin
        while (dirty) do
        begin
            dirty := false;
            for i := 1 to servers - 1 do
            begin

                if (ServerList[i - 1].players < ServerList[i - 0].players) then
                begin
                    tmp := ServerList[i + 0];
                    ServerList[i + 0] := ServerList[i - 1];
                    ServerList[i - 1] := tmp;
                    dirty := true;
                end;
            end;
        end;
        self.sortmode := TSortMode.Ascending;
    end;

    self.ResolvePos;
end;

procedure TServerScreen.SortPopulation();
var
    tmp: TServerInfo;
    i: integer;
    dirty: boolean;
begin
    // a, b, c
    sortcolumn := TSortColumn.Population;
    dirty := true;
    if (self.sortmode = TSortMode.Ascending) then
    begin
        while (dirty) do
        begin
            dirty := false;
            for i := 1 to servers - 1 do
            begin
                if CompareStr(ServerList[i - 1].status_text, ServerList[i + 0].status_text) < 0 then
                begin
                    tmp := ServerList[i + 0];
                    ServerList[i + 0] := ServerList[i - 1];
                    ServerList[i - 1] := tmp;
                    dirty := true;
                end;
            end;
        end;
        self.sortmode := TSortMode.Descending;
    end
    // c, b, a
    else if (self.sortmode = TSortMode.Descending) then
    begin
        while (dirty) do
        begin
            dirty := false;
            for i := 1 to servers - 1 do
            begin

                if CompareStr(ServerList[i - 1].status_text, ServerList[i - 0].status_text) > 0 then
                begin
                    tmp := ServerList[i + 0];
                    ServerList[i + 0] := ServerList[i - 1];
                    ServerList[i - 1] := tmp;
                    dirty := true;
                end;
            end;
        end;
        self.sortmode := TSortMode.Ascending;
    end;

    self.ResolvePos;
end;

procedure TServerScreen.SortType();
var
    tmp: TServerInfo;
    i: integer;
    dirty: boolean;
begin
    // a, b, c
    sortcolumn := TSortColumn.Mode;
    dirty := true;
    if (self.sortmode = TSortMode.Ascending) then
    begin
        while (dirty) do
        begin
            dirty := false;
            for i := 1 to servers - 1 do
            begin
                if CompareStr(ServerList[i - 1].mode_text, ServerList[i + 0].mode_text) < 0 then
                begin
                    tmp := ServerList[i + 0];
                    ServerList[i + 0] := ServerList[i - 1];
                    ServerList[i - 1] := tmp;
                    dirty := true;
                end;
            end;
        end;
        self.sortmode := TSortMode.Descending;
    end
    // c, b, a
    else if (self.sortmode = TSortMode.Descending) then
    begin
        while (dirty) do
        begin
            dirty := false;
            for i := 1 to servers - 1 do
            begin

                if CompareStr(ServerList[i - 1].mode_text, ServerList[i - 0].mode_text) > 0 then
                begin
                    tmp := ServerList[i + 0];
                    ServerList[i + 0] := ServerList[i - 1];
                    ServerList[i - 1] := tmp;
                    dirty := true;
                end;
            end;
        end;
        self.sortmode := TSortMode.Ascending;
    end;

    self.ResolvePos;
end;

// sorting methods modify ServerList[i].bar x values.
// loop after sort.   y := (SERVERBAR_HEIGHT + 2) * servers + 8;
procedure TServerScreen.SortName();
var
    tmp: TServerInfo;
    i: integer;
    dirty: boolean;
begin
    // a, b, c
    sortcolumn := TSortColumn.name;

    dirty := true;
    if (self.sortmode = TSortMode.Ascending) then
    begin
        while (dirty) do
        begin
            dirty := false;
            for i := 1 to servers - 1 do
            begin
                if CompareStr(ServerList[i - 1].name_text, ServerList[i + 0].name_text) < 0 then
                begin
                    tmp := ServerList[i + 0];
                    ServerList[i + 0] := ServerList[i - 1];
                    ServerList[i - 1] := tmp;
                    dirty := true;
                end;
            end;
        end;
        self.sortmode := TSortMode.Descending;
    end
    // c, b, a
    else if (self.sortmode = TSortMode.Descending) then
    begin
        while (dirty) do
        begin
            dirty := false;
            for i := 1 to servers - 1 do
            begin

                if CompareStr(ServerList[i - 1].name_text, ServerList[i - 0].name_text) > 0 then
                begin
                    tmp := ServerList[i + 0];
                    ServerList[i + 0] := ServerList[i - 1];
                    ServerList[i - 1] := tmp;
                    dirty := true;
                end;
            end;
        end;
        self.sortmode := TSortMode.Ascending;
    end;

    self.ResolvePos;
end;

constructor TServerScreen.create;
begin
    sortmode := TSortMode.Descending;
    sortcolumn := TSortColumn.name;
    engine := TGUIEngine.create('controls.png');

    servers := 0;

    BG := TGUIBackGround.create(Pointer(engine), 'background.png');

    // ------------- nav buttons -----------------------------------------------------------------
    exitbutton := TGUIButton.create(Pointer(engine), initializer.swidth - 74, initializer.sheight - 60,
      NAV_ICON_EXIT_ACTIVE, NAV_ICON_EXIT_HOVER, '', exit);
    exitbutton.rect.width := NAV_ICON_SIZE;
    exitbutton.rect.height := NAV_ICON_SIZE;

    backbutton := TGUIButton.create(Pointer(engine), initializer.swidth - 114, initializer.sheight - 60,
      NAV_ICON_BACK_ACTIVE, NAV_ICON_BACK_HOVER, '', back);
    backbutton.rect.width := NAV_ICON_SIZE;
    backbutton.rect.height := NAV_ICON_SIZE;
    // --------------------------------------------------------------------------------------------

    serverframe := TGUIPanel.create(Pointer(engine), trunc(initializer.swidth / 2) - (SERVERFRAME_WIDTH div 2),
      trunc(initializer.sheight / 2) - (SERVERFRAME_HEIGHT div 2), SERVERFRAME_WIDTH, SERVERFRAME_HEIGHT);

    // typeframe := TGUIPanel.create(Pointer(engine), trunc(serverframe.rect.width / 2 - SERVERBAR_WIDTH / 2), 4,
    // SERVERBAR_WIDTH, SERVERBAR_HEIGHT);
    // typeframe.setparent(Pointer(serverframe));

    name := TGUIText.create(Pointer(engine), 4 + 20 + 5, 0 + 14, 'Name', $FF00FFFF, $FFFF0000);
    types := TGUIText.create(Pointer(engine), 365 + 20, 0 + 14, 'Type', $FF00FFFF, $FFFF0000);
    Online := TGUIText.create(Pointer(engine), 465 + 20, 0 + 14, 'Online', $FF00FFFF, $FFFF0000);
    Population := TGUIText.create(Pointer(engine), 625 + 20, 0 + 14, 'Population', $FF00FFFF, $FFFF0000);

    name.setclickable(self.SortName);
    types.setclickable(self.SortType);
    Online.setclickable(self.SortPlayers);
    Population.setclickable(self.SortPopulation);

    name.setparent(Pointer(serverframe));
    types.setparent(Pointer(serverframe));
    Online.setparent(Pointer(serverframe));
    Population.setparent(Pointer(serverframe));

    engine.pack;
end;

procedure TServerScreen.OnServerList();
var
    i: integer;
begin
    for i := 0 to servers - 1 do
    begin
        if (ServerList[i].bar.GetRect.Contains(Tmouse.CursorPos)) and (ServerList[i].Online = true) then
        begin
            CharSelect.SetView;
            WorldConnection.sethost(ServerList[i].ip);
            WorldConnection.setport(ServerList[i].port);
            worldcon := TConnection.create(ServerList[i].ip, ServerList[i].port);
        end;
    end;
end;

procedure TServerScreen.Logic();
var
    i: integer;
begin
    if (Keyboard.GetAsyncKeyState(VK_F5) <> 0) and not(updatelock) then
    begin
        MetaCon.ListServers;
        updatelock := true;
    end
    else if (Keyboard.GetAsyncKeyState(VK_F5) = 0) then
        updatelock := false;

    engine.Logic;
end;

procedure TServerScreen.Draw();
begin
    engine.Draw;
end;

procedure TServerScreen.NullServer();
begin
    statusbox.hide;

    // revert to prevent auto-flip
    if (sortmode = TSortMode.Ascending) then
        sortmode := TSortMode.Descending
    else
        sortmode := TSortMode.Ascending;

    // apply sorting
    case (sortcolumn) of
        TSortColumn.Online:
            self.SortPlayers;
        TSortColumn.name:
            self.SortName;
        TSortColumn.Population:
            self.SortPopulation;
        TSortColumn.Mode:
            self.SortType;
    end;
end;

function TServerScreen.serverpos(name: string): integer;
var
    i: integer;
begin
    result := -1;
    for i := 0 to servers - 1 do
        if (ServerList[i].name_text = name) then
            result := i;
end;

function TServerScreen.statuscolor(status: string): cardinal;
begin
    result := $FFFFFFFF;
    if (status = 'Low') then
        result := $FF00FF00;
    if (status = 'Medium') then
        result := $FFFCDC3B;
    if (status = 'High') then
        result := $FFFF6103;
    if (status = 'Full') then
        result := $FFFF0000;
end;

// rebuild when addserver? btn - text text text - btn - text - text
procedure TServerScreen.AddServer(name, Mode, ip: string; port, players, slots: integer; Population, Online: string);
var
    x, y, pos: integer;
begin
    pos := serverpos(name);

    if pos > -1 then
    begin
        ServerList[pos].quota.text := '[' + inttostr(players) + '/' + inttostr(slots) + ']';
        ServerList[pos].Population.text := Population;
        ServerList[pos].modes.text := Mode;
        ServerList[pos].Population.color := statuscolor(Population);

        ServerList[pos].players := players;
        ServerList[pos].status_text := Population;
        ServerList[pos].mode_text := Mode;
        ServerList[pos].name_text := name;

    end
    else
    begin
        inc(servers);
        pos := servers - 1;

        ServerList[pos].port := port;
        ServerList[pos].ip := ip;

        x := (serverframe.rect.width div 2) - (SERVERBAR_WIDTH div 2);
        y := (SERVERBAR_HEIGHT + 2) * servers + 8;

        ServerList[pos].bar := TGUIButton.create(Pointer(engine), x, y, SERVERBAR_WIDTH, SERVERBAR_HEIGHT,
          SPRITE_BAR_ONLINE, SPRITE_BAR_HOVER, '', OnServerList);

        ServerList[pos].players := players;
        ServerList[pos].status_text := Population;
        ServerList[pos].mode_text := Mode;
        ServerList[pos].name_text := name;

        ServerList[pos].name := TGUIText.create(Pointer(engine), 12, 3, name);
        ServerList[pos].modes := TGUIText.create(Pointer(engine), 365 + 2, 3, Mode);
        ServerList[pos].Population := TGUIText.create(Pointer(engine), 625 + 2, 3, Population, statuscolor(Population));
        ServerList[pos].quota := TGUIText.create(Pointer(engine), 465 + 2, 3,
          '[' + inttostr(players) + '/' + inttostr(slots) + ']');
        ServerList[pos].bar.setparent(Pointer(serverframe));

        ServerList[pos].name.setparent(Pointer(ServerList[pos].bar));
        ServerList[pos].modes.setparent(Pointer(ServerList[pos].bar));
        ServerList[pos].Population.setparent(Pointer(ServerList[pos].bar));
        ServerList[pos].quota.setparent(Pointer(ServerList[pos].bar));

        engine.pack;
    end;

    if (Online = '1') then
    begin
        ServerList[pos].bar.fsprite := SPRITE_BAR_HOVER;
        ServerList[pos].bar.sprite := SPRITE_BAR_ONLINE;
        ServerList[pos].Online := true;
        ServerList[pos].bar.enabled := true
    end
    else
    begin
        ServerList[pos].bar.fsprite := SPRITE_BAR_OFFLINE;
        ServerList[pos].bar.sprite := SPRITE_BAR_OFFLINE;
        ServerList[pos].Online := false;
        ServerList[pos].bar.enabled := false
    end;
end;

end.
