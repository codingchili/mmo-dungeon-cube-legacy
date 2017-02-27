unit Player_ItemPack;

//every user has an ItemPack connected, the itempack consist of their
//wielded equipment, inventory items, currency, it is bound to a key
//and

{$IFDEF Linux}
{$MODE DELPHI}
{$ENDIF}

interface

type
  TSlot = (Head, Chest, Legs, Weapon, Potion, Crafting);

type
  TType = (Bow, Staff, Sword, Armor);

type
  TEffect = (Haste, Farsight, Regeneration, Heal, Mana, Magicresist,
    Physresist, Stealth);

type
  TClass = (Mage, Warrior, Thief, All);

type
  TRarity = (None, Godly, Very, Rare, Uncommon, Common, Junk);

type
  Tlooting = (Boss, Worldboss, Monster, Player);

type
  TItem = Record
    name: string[25];
    description: string[50];
    slot: TSlot;
    effect: TEffect;
    types: TType;
    restriction: TClass;
    looting: Tlooting;
    attack, Armor, prdef, mrdef, INT, STR, WIS, CON, DEX, ACC, Level,
      Gold: integer;
    duration: single;
  private
  end;

var
  Items: Array of TItem;

implementation

// procedure create(item: TItem); overload;
// begin

// end;

// load item data.
procedure load();
begin

end;

function additem(name, description: string; slot: TSlot; effect: TEffect;
  types: TType): TItem;
begin
  result.name := name;
  result.description := description;
  result.slot := slot;
  result.effect := effect;
  result.types := types;

  result.Gold := 0;
end;

begin
  setlength(Items, 0);

  // create(Tweapon('', '', ));
  // initialize here until there is an ItemMaker(tm).
end.