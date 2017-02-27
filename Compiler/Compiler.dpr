program Compiler;

{$APPTYPE CONSOLE}
{$R *.res}
// -----------------------------------------------------
{ Build Script For Spells/Professions/Skill-Trees
  DungeonCube V1.00a  ~ RD }
// -----------------------------------------------------

{$DEFINE SPELLCOMPILE}

uses
  System.SysUtils,
  Windows,
  Conf_Spritemap in '..\Game\Conf_Spritemap.pas',
  System_Log in '..\Game\System_Log.pas',
  System_Spells in '..\Game\System_Spells.pas',
  Build_Profession in 'Build_Profession.pas',
  Build_Dialog in 'Build_Dialog.pas',
  Build_Spells in 'Build_Spells.pas',
  Build_CharInfo in 'Build_CharInfo.pas';

const
  VERSION: string = 'v1.00';

begin
  SetConsoleTitle(PChar('Compiler - Binary Gen [' + VERSION + '] ~RD'));
  System_Log.Initialize;
  System_Log.logging := true;

  print('Compiler Init! [' + VERSION + '] ...', red, false);
  print('');
  print('Compiling...', yellow);
  // CompileProfessions;
  CompileSpells;
  CompileCharInfo();

  print('');
  print('All Files Compiled Sucessfully!', magenta);
  sleep(100);
  readln;

end.
