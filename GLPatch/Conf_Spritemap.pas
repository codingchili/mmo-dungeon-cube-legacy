unit Conf_Spritemap;

interface

uses Types;

CONST
  SPRITE_BTN: TPoint = (X: 260; Y: 160);
  SPRITE_BTN_HOVER: TPoint = (X: 260; Y: 182);
  SPRITE_BTN_DISABLED: TPoint = (X: 389; Y: 160);

  SPRITE_BAR_HOVER: TPoint = (X: 0; Y: 203);
  SPRITE_BAR_ONLINE: TPoint = (X: 0; Y: 224);
  SPRITE_BAR_OFFLINE: TPoint = (X: 0; Y: 245);

  SPRITE_INPUTFIELD: TPoint = (X: 0; Y: 159);
  SPRITE_INPUTFIELD_FOCUS: TPoint = (X: 0; Y: 181);
  SPRITE_INPUTFIELD_DISCRETE: TPoint = (X: 517; Y: 160);

  SPRITE_BARFRAME: TPoint = (X: 0; Y: 266);
  SPRITE_XPBAR: TPoint = (X: 0; Y: 313);
  SPRITE_HPBAR: TPoint = (X: 0; Y: 277);
  SPRITE_MPBAR: TPoint = (X: 0; Y: 286);
  SPRITE_ENBAR: TPoint = (X: 0; Y: 295);
  SPRITE_FRBAR: TPoint = (X: 0; Y: 304);
  SPRITE_MINIBAR: TPoint = (X: 388; Y: 195);
  SPRITE_MINIBARFILL: TPoint = (X: 389; Y: 200);

  SPRITE_BTN_WARRIOR: TPoint = (X: 0; Y: 0);
  SPRITE_BTN_MAGE: TPoint = (X: 159; Y: 0);
  SPRITE_BTN_THIEF: TPoint = (X: 318; Y: 0);

  SPRITE_BTN_HOVER_WARRIOR: TPoint = (X: 477; Y: 0);
  SPRITE_BTN_HOVER_MAGE: TPoint = (X: 636; Y: 0);
  SPRITE_BTN_HOVER_THIEF: TPoint = (X: 795; Y: 0);

  SPRITE_STAT_ADD: TPoint = (X: 388; Y: 181);
  SPRITE_STAT_ADD_HOVER: TPoint = (X: 408; Y: 181);
  SPRITE_STAT_ADD_DISABLED: TPoint = (X: 398; Y: 181);

  // SPELLS
  SPELL_MAGE_BASIC: TPoint = (X: 0; Y: 31);
  SPELL_THIEF_BASIC: TPoint = (X: 223; Y: 0);
  SPELL_WARRIOR_BASIC: TPoint = (X: 207; Y: 15);
  SPELL_WARRIOR_THROWINGAXE: TPoint = (X: 271; Y: 15);

  SPELL_THIEF_ARROW1: TPoint = (X: 15; Y: 15);
  SPELL_THIEF_ARROW2: TPoint = (X: 31; Y: 15);
  SPELL_THIEF_ARROW3: TPoint = (X: 79; Y: 15);
  SPELL_THIEF_ARROW4: TPoint = (X: 63; Y: 15);

  // particle
 // SPRITE_PARTICLE: TPoint = (X: 418; Y: 181);
 SPRITE_PARTICLE: TPoint = (X: 0; Y: 0);

  SPRITE_MAGE_BASIC: TPoint = (X: 296; Y: 267);
  SPRITE_THIEF_BASIC: TPoint = (X: 336; Y: 267);
  SPRITE_WARRIOR_BASIC: TPoint = (X: 376; Y: 267);
  SPRITE_SKILL_EMPTY: TPoint = (X: 416; Y: 267);

  SERVERBAR_WIDTH = 700;
  SERVERBAR_HEIGHT = 21;
  BTN_WIDTH_PROFESSION = 159;
  BTN_HEIGHT_PROFESSION = 159;

  BAR_HEIGHT = 11;
  BAR_HEIGHT_FILL = 9;
  BAR_WIDTH = 294;

  NAV_ICON_SIZE = 32;

  NAV_ICON_EXIT_HOVER: TPoint = (X: 777; Y: 159);
  NAV_ICON_EXIT_ACTIVE: TPoint = (X: 809; Y: 159);

  NAV_ICON_BACK_ACTIVE: TPoint = (X: 841; Y: 159);
  NAV_ICON_BACK_HOVER: TPoint = (X: 873; Y: 159);
  NAV_ICON_BACK_DISABLED: TPoint = (X: 905; Y: 159);

  SPRITE_GIFSPINNER: TPoint = (X: 395; Y: 310);

implementation

end.