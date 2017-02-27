unit GUI_Element_Image;

interface

// draws an image 1:1 to screen
{$IFDEF Linux}{$MODE Delphi}{$ENDIF}

uses Rectarea, GUI_Engine, GUI_Element, Vectors2, AsphyreTypes, AbstractCanvas, Types, Math, AsphyreImages;

type
  TGUIImage = class(TGUIElement)
  private
    rotable: boolean;
    image: integer;
    filter: cardinal;
    customgraphics: boolean;
  public
   // rect: Rectarea.TRect;
    rotation: integer;
    sprite: TPoint;
    procedure SetRotable(rotable: boolean);
    procedure filterwith(color: cardinal);
    procedure Draw(); override;
    procedure Logic(); override;
    procedure MouseEvent(var event: TGUIMouseEvent); override;
    constructor Create(engine: Pointer; left, top, width, height: integer; sprite: TPoint); overload;
    constructor Create(engine: Pointer; left, top, width, height: integer; imagefile: String); overload;
  end;

implementation

uses System_Initializer, System_UtilExt;

{ TGUIText }

constructor TGUIImage.Create(engine: Pointer; left, top, width, height: integer; sprite: TPoint);
begin
  inherited Create(engine, left, top, width, height);
  self.mode := Mode_Image; // engine identifier
  self.sprite := sprite;
  self.rotable := false;
  customgraphics := false;
  filter := $FFFFFFFF;
  rotation := 0;
  TGUIEngine(engine).Add(TGUIElement(self));
end;

constructor TGUIImage.Create(engine: Pointer; left, top, width, height: integer; imagefile: String);
begin
  inherited Create(engine, left, top, width, height);
  self.mode := Mode_Image; // engine identifier
  image := GameImages.Resolve(imagefile);
  customgraphics := true;
  self.rotable := false;
  filter := $FFFFFFFF;
  rotation := 0;
  TGUIEngine(engine).Add(TGUIElement(self));
end;

procedure TGUIImage.filterwith(color: cardinal);
begin
  self.filter := color;
end;

procedure TGUIImage.SetRotable(rotable: boolean);
begin
  self.rotable := rotable;
end;

procedure TGUIImage.MouseEvent(var event: TGUIMouseEvent);
begin
  //
end;

procedure TGUIImage.Draw;
var
 drawrect: Rectarea.TRect;
begin
drawrect := self.GetRect;

  if (customgraphics) then
    GameCanvas.UseImagePx(GameImages.items[image], Point4(sprite.x, sprite.y, drawrect.width + sprite.x, sprite.y,
      drawrect.width + sprite.x, drawrect.height + sprite.y, sprite.x, drawrect.height + sprite.y))
  else
    GameCanvas.UseImagePx(GameImages.items[self.spritesheet], Point4(sprite.x, sprite.y, drawrect.width + sprite.x,
      sprite.y, drawrect.width + sprite.x, drawrect.height + sprite.y, sprite.x, drawrect.height + sprite.y));

  if (rotable) then
    GameCanvas.TexMap(pRotate4(Point2(drawrect.left, drawrect.top), Point2(drawrect.width, drawrect.height),
      Point2(drawrect.width div 2, drawrect.height div 2), DegToRad(rotation), 1), cColor4(filter), TBlendingEffect.beNormal);

  if not(rotable) then
    GameCanvas.TexMap(pBounds4(drawrect.left, drawrect.top, drawrect.width, drawrect.height), cColor4(filter),
      TBlendingEffect.beNormal);
end;

procedure TGUIImage.Logic;
begin
 // rect := self.GetRect;
end;

end.
