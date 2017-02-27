unit System_Items;

interface

type
  TItemTypes = (SlotHead, SlotShirt, SlotLegs, SlotHands, SlotBoots, SlotRings, SlotAmulet, SlotCloak, Potion,
    Quest, Spell);

  {
    STR = increased physical damage
    SPR = increased hp/mp-regen and increased healingpower
    INT = incresed magical damage
    CON = increased health pool
    DEX = all physical damage chance to critical (+50%), reduces cooldown on basic attack, increases movement speed.
    DEF = reduced physical damage taken
    RES = reduced magical damage taken
  }

type
  TItemStats = record
  public
    STR, SPR, INT, CON, DEX, defence, resist: integer;
  end;

type
  TItemRarity = (low, medium, better, high, top, unranked);

type
  TItem = Record
  public
    name: String;
    quality, id: integer;
    stats: TItemStats;
    types: TItemTypes;
    rarity: TItemRarity;
    description: String[80];
  End;

type
  TItemPool = class
  private
    items: Array of TItem;
  public
    class function QualityText(itemQuality: TItemRarity): string;
    constructor create();
  end;

procedure Initialize;

var
  itemPool: TItemPool;

implementation

procedure Initialize;
begin
  itemPool := TItemPool.create;
end;

class function TItemPool.QualityText(itemQuality: TItemRarity): string;
begin
  result := '';
  case (itemQuality) of
    TItemRarity.low:
      ;
  end;
end;

constructor TItemPool.create;
begin
  // load items from items.bin
end;

end.
