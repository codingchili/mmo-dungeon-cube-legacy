unit GUI_Element_Background;

interface

{$IFDEF Linux}
{$MODE Delphi}
{$ENDIF}
// scales an image to fit the screen

uses GUI_Engine, GUI_Element, Vectors2, AsphyreTypes, AbstractCanvas, SysUtils, Effects_Blending;

type
  TGUIBackground = class(TGUIElement)
  private
    blend: TFade;
  public
    constructor Create(engine: Pointer; image: string);
    procedure Draw(); override;
    procedure Logic(); override;
    procedure fadein();
  end;

implementation

uses System_Initializer;

{ TGUIText }

constructor TGUIBackground.Create(engine: Pointer; image: string);
begin
  inherited Create(engine, 0, 0, initializer.swidth, initializer.sheight);
  self.mode := Mode_Background; // engine identifier
  self.spritesheet := GameImages.Resolve(image);
  blend.full;

  TGUIEngine(engine).Add(TGUIElement(self));
end;

procedure TGUIBackground.Draw;
begin
  GameCanvas.UseImagePx(GameImages.items[spritesheet], Point4(0, 0, 1920, 0, 1920, 1080, 0, 1080));
  GameCanvas.TexMap(pBounds4(0, 0, initializer.swidth, initializer.sheight), cColor4(blend.value or $00FFFFFF), TBlendingEffect.beNormal);
end;

procedure TGUIBackground.fadein;
begin
  blend.ins;
end;

procedure TGUIBackground.Logic;
begin
  blend.fade;
end;

end.
