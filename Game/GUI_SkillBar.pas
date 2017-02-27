unit GUI_SkillBar;

interface

{$IFDEF Linux}
{$MODE Delphi}
{$ENDIF}
// detect, load, fire skills, do cooldown calculation, spell list..

// call processcooldowns on the spells in the bar, also cast hasEnergyLeft to see if we may cast.. draw accordingly

uses
{$IFDEF Win32} Windows, {$ENDIF}
    SyncObjs, SysUtils, GUI_Engine, GUI_Element_Button, GUI_Element_Panel, GUI_Element_Text,
    GUI_Element_Image, GUI_Element, conf_protocol, Classes,
    System_Keyboard, Controls, System_Spells, Mouse_Proxy;

type
    TOnCast = procedure(id: integer) of object;

type
    TSkillAvailability = (Unavailable, Available, Cooldown, Energy);

type
    TSkillItem = record
    public
        Icon: TGUIImage;
        border: TGUIPanel;
        Spell: ^TSpell;
    end;

type
    TSkillBar = class
    public
        engine: TGUIEngine;
        skills: array [0 .. 8] of TSkillItem;
        procedure Logic;
        procedure Draw;
        procedure Refresh();
    private
        castframe, castfill: TGUIPanel;
        casttitle: TGUIText;
        castbar: TGUIImage;
        casttime, castleft: integer;
        frame, framehint: TGUIPanel;
        cs: TCriticalSection;
        procedure setCast(text: string; time: integer); // append cast modifiers? - walk breaks or spell blocks walk?
        constructor Create;
    end;

procedure Initialize;

const
    SKILLBAR_WIDTH: integer = 360;
    SKILLBAR_HEIGHT: integer = 40;
    SKILL_SIZE = 32;

var
    SkillBar: TSkillBar;
    hoverid: integer;
    empty_spell: TSpell;

implementation

uses System_Log, Engine_Player, Main, System_Initializer, System_Camera, Vectors2, AsphyreTypes, Conf_Spritemap,
    Network_World, System_UtilExt, GUI_StatusBox, GUI_Chat;

procedure TSkillBar.setCast(text: string; time: integer);
begin
    casttime := time + 30;
    castleft := time + 30;
    casttitle.text := text;
    casttitle.rect.left := (175 div 2) - (trunc(GameFonts[0].TextWidth(text)) div 2);
end;

procedure TSkillBar.Refresh();
var
    i: integer;
begin
    skills[0].Spell := @SpellMan.SpellSet[(Ord(Player.profession))].basic;

    for i := 0 to 8 do
    begin
        skills[i].Icon.spritesheet := GameImages.Resolve('spellset.png');
        skills[i].Icon.sprite := skills[i].Spell^.sprite;
    end;
end;

procedure TSkillBar.Logic();
var
    i: integer;
begin
    framehint.Hide;

    if (castleft > 0) then
    begin
        castfill.rect.width := trunc(castframe.rect.width * (castleft / casttime));
        castfill.rect.left := castframe.rect.width - castfill.rect.width;
        dec(castleft);
        castframe.visible := true;
    end
    else
        castframe.visible := false;
    // TODO fade the castbar!

    for i := 0 to 8 do
    begin
        if (skills[i].Icon.GetRect.Contains(TMouse.CursorPos)) and (skills[i].Spell^.name <> '') then
        begin
            framehint.rect.left := trunc(TMouse.CursorPos.X - framehint.rect.width / 2);
            framehint.rect.Top := frame.rect.Top - framehint.rect.height - 10;
            framehint.Show;
            hoverid := i;
        end;

        if (SpellMan.SpellState(skills[i].Spell^) = TSpellState.Available) or (true) then
        begin
            SkillBar.skills[i].Icon.filterwith($FFFFFFFF);

            // implement hotkey please
            if (Keyboard.getasynckeystate(VK_RBUTTON) <> 0) then
            begin
                if (SpellMan.Cast(SkillBar.skills[i].Spell^)) then
                    setCast(SkillBar.skills[i].Spell^.name, SkillBar.skills[i].Spell^.casttime);
            end;
        end
        else
            SkillBar.skills[i].Icon.filterwith($FF903232);

        // return true/false - if true then cast = true, and then show castbar for cast-time
        // when full, the bar should fade
        // reset bar alpha on cast
    end;

    engine.Logic;
end;

procedure TSkillBar.Draw();
var
    i: integer;
    hint: string;
begin
    engine.Draw;

    { for i := 0 to 8 do
      begin
      if (SkillBar.skills[i].Spell^.timer > 0) then
      begin
      hint := FormatFloat('0.0', (SkillBar.skills[i].Spell^.timer / 60));
      GameFonts.Items[0].TextOut(point2(SkillBar.skills[i].border.GetRect.left + round(GameFonts[0].TextWidth(hint) / 2),
      SkillBar.skills[i].Icon.GetRect.Top + 36), hint, cColor2($FFFFFFFF));
      end;
      end; }

    // name color should depend on school, red - offensive, yellow - utility, green - healing

    // red: chaos mage, marksman, berserk
    // yellow: assasin, diviner, mage hunter
    // green: hunter, protector, guardian

    // damage value color should depend on type of damage,
    // red: physical damage
    // magenta: magical damage
    // green: poison damage
    // lighred: bleed damage

    // if statusframe is visible then perform a custom drawText here to fill it.

    // depending on the type of attack, the damage is calculated differently. (spellpower/attackpower)

    // dont display values that are 0

    // pre compile the list of attributes, calls to formatfloat/trunc/inttostr only on update.
    if (framehint.visible) then
    begin
        i := 11;
        GameFonts[0].TextOut(point2(trunc(framehint.rect.left + 5), trunc(framehint.rect.Top + 5)),
          skills[hoverid].Spell^.name, cColor2($FF00FFFF)); // change color depending on school

        GameFonts[0].TextOut(point2(framehint.rect.left + framehint.rect.width -
          (GameFonts[0].TextWidth('CD ' + FormatFloat('0.#', skills[hoverid].Spell^.Cooldown) + 's') + 5),
          framehint.rect.Top + 5), 'CD ' + FormatFloat('0.#', skills[hoverid].Spell^.Cooldown) + 's',
          cColor2($FFFFFFFF));

        if (skills[hoverid].Spell^.damage > 0) then
        begin
            i := i + 16;
            GameFonts[0].TextOut(point2(framehint.rect.left + 5, framehint.rect.Top + i),
              'Damage: ' + FormatFloat('0.#', Player.attackpower * (skills[hoverid].Spell^.damage)) + ' (' +
              FormatFloat('0.#', skills[hoverid].Spell^.damage * 100) + '%)',
              // calculate damage by attack/spell-power multiplied with spell mods.. ex: 'Damage: 1178 (175%)' color shows the scaler
              cColor2($FFFFFFFF));
        end;

        if (skills[hoverid].Spell^.duration > 0) then
        begin
            i := i + 16;
            GameFonts[0].TextOut(point2(framehint.rect.left + 5, framehint.rect.Top + i),
              'Duration: ' + FormatFloat('0.#', skills[hoverid].Spell^.duration), // draw if the spell applies on-hit
              cColor2($FFFFFFFF));
        end;

        if (skills[hoverid].Spell^.casttime > 0) then
        begin
            i := i + 16;
            GameFonts[0].TextOut(point2(framehint.rect.left + 5, framehint.rect.Top + i),
              'Cast-Time: ' + FormatFloat('0.#', skills[hoverid].Spell^.casttime), cColor2($FFFFFFFF));

        end;
        if (skills[hoverid].Spell^.ttl > 0) then
        begin
            i := i + 16;
            GameFonts[0].TextOut(point2(framehint.rect.left + 5, framehint.rect.Top + i),
              'Range: ' + inttostr(skills[hoverid].Spell^.ttl), cColor2($FFFFFFFF));
        end;

        i := i + 24;
        GameFonts[0].TextOut(point2(framehint.rect.left + 5, framehint.rect.Top + i),
          skills[hoverid].Spell^.description, cColor2($FFFFFFFF));

        framehint.rect.height := 5 + i + 14;
    end;
end;

procedure Initialize;
begin
    SkillBar := TSkillBar.Create;
end;

constructor TSkillBar.Create;
var
    i: integer;
begin
    cs := TCriticalSection.Create;

    engine := TGUIEngine.Create('controls.png');

    frame := TGUIPanel.Create(Pointer(engine), trunc(initializer.SWidth / 2 - SKILLBAR_WIDTH / 2),
      trunc(initializer.sheight - 55), SKILLBAR_WIDTH, SKILLBAR_HEIGHT);
    frame.draggable := true;

    framehint := TGUIPanel.Create(Pointer(engine), 0, 0, 206, 116);
    framehint.Hide;

    empty_spell.sprite := SPELL_SKILL_EMPTY;
    empty_spell.SpellType := TSpellType.EMPTY;

    // CAST BAR
    castframe := TGUIPanel.Create(Pointer(engine), (SKILLBAR_WIDTH div 2) - (175 div 2), -22, 175, 18);
    castframe.setparent(Pointer(frame));
    castfill := TGUIPanel.Create(Pointer(engine), 0, 0, 175, 18);
    castframe.setparent(Pointer(frame));
    castfill.setparent(Pointer(castframe));
    castfill.fill := $80AAAAAA;

    casttitle := TGUIText.Create(Pointer(engine), 8, 2, 'Forked Lightning', $FFFFFFFF);
    casttitle.setparent(Pointer(castframe));
    // END CAST BAR

    engine.pack;

    for i := 0 to high(skills) do
    begin
        skills[i].border := TGUIPanel.Create(Pointer(engine), 40 * i + 2, 2, 36, 36);
        skills[i].border.setparent(Pointer(frame));
        skills[i].border.fill := $66000002;
    end;

    for i := 0 to high(skills) do
    begin
        skills[i].Spell := @empty_spell;
        skills[i].Icon := TGUIImage.Create(Pointer(engine), 40 * i + 5, 5, SKILL_SIZE, SKILL_SIZE,
          skills[i].Spell.sprite);
        skills[i].Icon.setparent(Pointer(frame));
    end;

    { engine.Add(framehint);
      engine.Add(frame); }
    // engine.pack;
end;

end.
