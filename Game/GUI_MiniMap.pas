unit GUI_MiniMap;

interface

// todo:  this stuff is not done yet :s

{$IFDEF Linux}{$MODE Delphi}{$ENDIF}

uses
  SysUtils, GUI_Engine, GUI_Element_Panel, GUI_Element_Button, GUI_Element_Edit, GUI_Element_Text;

// read stats from player and display here...
const
  WIDTH: integer = 150;
  HEIGHT: integer = 150;
  FRAME_WIDTH: integer = 300;
  FRAME_HEIGHT: integer = 175;

type
  TGUIMiniMap = class
  public
    engine: TGUIEngine;
    procedure Logic();
    procedure Draw();
  private
    frame: TGUIPanel;
    btntogglevisibility: TGUIButton;
    cordtext, maptext: TGUIText;
    constructor Create;
  end;

procedure Initialize;

var
  GUIMiniMap: TGUIMiniMap;

implementation

uses Engine_Player, Conf_SpriteMap, Main, System_Initializer, network_worldserver, GUI_CharSelect, Network_MetaServer,
  GUI_StatusBox, GUI_LoginScreen, GUI_ServerSelect, Network_World, AsphyreTypes, AbstractCanvas, Engine_Map, Vectors2,
  Math, System_Multiplayer;

// update x,y cords
procedure TGUIMiniMap.Logic();
begin
  maptext.text := MapMan.Image.Name;
  cordtext.text := 'x: ' + IntToStr(player.getX) + ', y:' + IntToStr(player.getY);
  engine.Logic;
end;

// draw dots on map, use pixel?
procedure TGUIMiniMap.Draw();
var
  p4: TPoint4;
  i: integer;
begin
  engine.Draw;

  GameCanvas.UseImagePx(MapMan.Image, Point4(0, 0, 3000, 0, 3000, 3000, 0, 3000));
  p4 := pRotate4(point2(frame.GetRect.left + (WIDTH div 2) + 10, frame.GetRect.top + (HEIGHT div 2) + 5),
    point2(HEIGHT, WIDTH), point2(HEIGHT div 2, WIDTH div 2), DegToRad(45), 1);

  p4[0].Y := p4[0].Y + (HEIGHT div 2);
  p4[2].Y := p4[2].Y - (HEIGHT div 2);

  GameCanvas.TexMap(p4, cColor4($FFFFFFFF), TBlendingEffect.beNormal);

  Multiplayer.Lock;
  for i := 0 to high(Multiplayer.player) do
  begin
    GameCanvas.UseImagePx(GameImages.Items[GameImages.Resolve('controls.png')],
      Point4(SPRITE_PARTICLE.x, SPRITE_PARTICLE.Y, SPRITE_PARTICLE.x + 6, SPRITE_PARTICLE.Y, SPRITE_PARTICLE.x + 6,
      SPRITE_PARTICLE.Y + 6, SPRITE_PARTICLE.x, SPRITE_PARTICLE.Y + 6));

    GameCanvas.TexMap(pBounds4(trunc(frame.GetRect.left + Multiplayer.player[i].x / 32),
      trunc(frame.GetRect.top + Multiplayer.player[i].Y / 32), 4, 4), cColor4($FFFF0000), TBlendingEffect.beNormal);
  end;
  Multiplayer.Unlock;

  GameCanvas.UseImagePx(GameImages.Items[GameImages.Resolve('controls.png')],
    Point4(SPRITE_PARTICLE.x, SPRITE_PARTICLE.Y, SPRITE_PARTICLE.x + 6, SPRITE_PARTICLE.Y, SPRITE_PARTICLE.x + 6,
    SPRITE_PARTICLE.Y + 6, SPRITE_PARTICLE.x, SPRITE_PARTICLE.Y + 6));

  GameCanvas.TexMap(pBounds4(trunc(frame.GetRect.left + (player.x / 32)), trunc(frame.GetRect.top + (player.Y / 32)), 4,
    4), cColor4($FF0000FF), TBlendingEffect.beNormal);
end;
// make smaller, topleft might look better.

procedure Initialize;
begin
  GUIMiniMap := TGUIMiniMap.Create;
end;

constructor TGUIMiniMap.Create;
begin
  engine := TGUIEngine.Create('controls.png');

  frame := TGUIPanel.Create(Pointer(engine), trunc(Initializer.SWidth / 2) - (FRAME_WIDTH div 2),
    trunc(Initializer.Sheight / 2 - (FRAME_HEIGHT div 2)), FRAME_WIDTH, FRAME_HEIGHT);
  frame.draggable := true;
  frame.visible := true;

  cordtext := TGUIText.Create(Pointer(engine), 300, 8, 'x:y', $FFFFFFFF);
  cordtext.setparent(Pointer(frame));

  maptext := TGUIText.Create(Pointer(engine), 8, 8, 'Map Name#', $FFFFFFFF);
  maptext.setparent(Pointer(frame));

  engine.pack;
end;

end.
