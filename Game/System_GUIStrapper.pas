unit System_GUIStrapper;

interface

{$IFDEF Linux}{$MODE Delphi}{$ENDIF}

uses Classes;

type
  TGUIStrapper = class(TThread)
  protected
    Procedure Execute; override;
  private
    completed: boolean;
  public
    constructor create;
  end;

procedure Initialize;

var
  GUIStrapper: TGUIStrapper;

implementation

Uses System_UtilExt, Engine_Map, System_Audio, GUI_Engine, GUI_LoginScreen,
  GUI_ServerSelect, GUI_StatusBox,
  GUI_CharSelect, GUI_Bars, GUI_Chat, GUI_Menu, GUI_Inventory, GUI_SkillBar,
  GUI_License, GUI_Banner, GUI_Windowed,
  Main, GUI_SplashMain, Network_MetaServer, GUI_Targeting;

// updates SplashMain with progress? or gets read by SplashMain.
procedure TGUIStrapper.Execute;
begin
  FreeOnTerminate := true;

  SplashMain.SetText('Verifying Resources..', $FFFF0000, 5);
  FileMan.FileSearch('media/gui/', 'background.png');
  // check if all vital files exists
  // check if my version is ok
  FileMan.Load();

  SplashMain.SetText('Connecting..', $FFFF0000, 5);
  MetaCon.syncconnect;

  if MetaCon.Connected then
  begin
    SplashMain.SetText('Connected!', $FFFF0000, 10);
    // sleep(300);
  end
  else
  begin
    SplashMain.SetText('Connection Failed.', $FFFF0000, 10);
    // sleep(300);
  end;

{$IFDEF Win32}
  SplashMain.SetText('Loading Sound..', $FFFF0000, 20);
  // SoundMaster.load();   //needs previous init, events may be pumped at this point
  // events is ignored until it is loaded.
{$ENDIF}

  // sleep(300);
  // SplashMain.SetText('Checking Version..', $FFFF5A00, 20);
  // sleep(300);
  // SplashMain.SetText('Downloading..');
  // all downloaded files will be marked dirty in FileMan
  // dirty files will be reloaded from source
  // clear all files not in use from GameImages (to free memory - may not be required)
  // rescan folders to reload new images
  // sleep(1700);

  // SplashMain.SetText('Applying Changes..');
  // sleep(1250);

  SplashMain.SetText('Scanning Files..', $FFFFCD00, 35);
  FileMan.FileSearch(root + 'media/gui', '*.png', true);
  FileMan.FileSearch(root + 'media/gui', '*.jpg', true);
  SplashMain.SetText('Loading Files..', $FFEBFF00, 40);
  // load files 40-80%  sleep(1000);
  FileMan.Load;

  SplashMain.SetText('Loading System..', $FF00FF00, 90);
  Engine_Map.Initialize;
  GUI_Engine.Initialize;
  GUI_LoginScreen.Initialize;
  GUI_ServerSelect.Initialize;
  GUI_StatusBox.Initialize;
  GUI_CharSelect.Initialize;
  GUI_Bars.Initialize;
  GUI_Chat.Initialize;
  GUI_Menu.Initialize;
  GUI_SkillBar.Initialize;
  GUI_License.Initialize;
  GUI_Banner.Initialize;
  GUI_Targeting.Initialize;
  GUI_Inventory.Initialize;
  SplashMain.SetText('Waiting..', $FF00FF00, 100);

  Main.initgui := true;

  // LoginScreen.SetView;

  with PerformanceStats do
  begin
    render_update := 0;
    proc_update := 0;
  end;
  LoginScreen.SetView;
  // set main view on complete.
end;

constructor TGUIStrapper.create;
begin
  inherited create(false);
end;

procedure Initialize;
begin
  GUIStrapper := TGUIStrapper.create;
end;

end.
