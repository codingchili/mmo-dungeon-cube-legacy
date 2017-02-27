unit Engine_Map;





// keep track of current map, perform blocking checks (anti-rubber sys)

interface

{$IFDEF Linux}
{$MODE Delphi}
{$ENDIF}

uses sysutils, System_UtilExt, AsphyreImages, AsphyreTypes, {$IFDEF Win32}AsphyrePNG, {$ENDIF}
  vectors2px, Dialogs, Classes, Map_Trees {$IFDEF Win32}, Windows{$ENDIF};

const
  BUFFER = 65536;
  // amount of map-data we can cache before we need to release memory.
  // should create a thread in background to cache up maps while buffer not full,
  // buffer ticket locations from inventory, adjacent maps from portals, available map-travels.

type
  TMapMan = class
  private
  public
    ID, deadID, port, width, height: integer;
    Image: TAsphyreImage;
    maptrees: TMapTrees;
    procedure Load(name: string);
    function IsLoaded(name: string): boolean;
    procedure DrawMap();
    procedure DrawEntity(postdraw: boolean);
    constructor create();
  end;

procedure Initialize;

var
  MapMan: TMapMan;

implementation

uses Main, GUI_StatusBox, System_Initializer, System_Camera, Vectors2, Math, AbstractCanvas, Effects_Blending;

constructor TMapMan.create;
begin
  ID := -1;
  Image := TAsphyreImage.create;
  maptrees := TMapTrees.create;
  width := 5000;
  height := 5000;
end;

procedure TMapMan.DrawEntity(postdraw: boolean);
begin
  maptrees.Draw(postdraw);
end;

procedure TMapMan.DrawMap();
var
  p4: TPoint4;
begin
  GameCanvas.UseImagePx(MapMan.Image, Point4(0, 0, width, 0, width, height, 0, height));
  p4 := pRotate4(point2(cam.x + width div 2, cam.y + height div 2), point2(width, height),
    point2(width div 2, height div 2), DegToRad(45), 1);
  p4[0].y := cam.y - height div 4 + height div 2;
  p4[2].y := cam.y + height div 4 + height div 2;
  GameCanvas.TexMap(p4, clWhite4, TBlendingEffect.beNormal);
end;

function TMapMan.IsLoaded(name: string): boolean;
begin
  if (name = Image.name) then
    result := true
  else
    result := false;
end;

// check if current map = map, else unload last map if mapID <> -1, then load map into element 0.
procedure TMapMan.Load(name: string);
var
  i: integer;
  str: TFileStream;
begin

  if (Image.name <> name) then
  begin
    if not(FileExists(root + 'media/maps/' + name + '.png')) then
    begin
      Statusbox.show('Resource: ' + 'media/maps/' + name + '.png' + ' Missing!');
      Sleep(3000);

{$IFDEF Win32}
      ExitProcess(0);
{$ELSE}
      Halt;
{$ENDIF}
    end;

    // Image.RemoveAllTextures();
    FreeAndNil(Image);

    Image := TAsphyreImage.create;
    Image.LoadFromFile(root + 'media/maps/' + name + '.png');


    // Main.RenderForm.loadbar.Progress := 0;
    // Main.RenderForm.loadbar.MaxValue := 40;

    // for i := 0 to 40 do
    // begin
    // Sleep(16);
    // Main.RenderForm.loadbar.Progress := i;
    // Main.RenderForm.loadbar.Update;
    // end;
    // Main.RenderForm.loadbar.Progress := 0;

    Image.PatternSize := Point2px(100, 100);
    Image.VisibleSize := Point2px(3000, 3000);
    Image.MipMapping := true;
    Image.DynamicImage := false;
    Image.PatternCount := 1;
  end;
  Image.name := name;
end;

procedure Initialize;
begin
  MapMan := TMapMan.create;
end;

end.
