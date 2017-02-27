unit Keyboard;

interface

uses Windows, Forms, Controls, Dialogs, SysUtils, Math;

// block type: water:lava etc

const
  BLOCKING = -1;
  WALKABLE = 0;
  LAVA = 1;
  WATER = 2;
  ENDPOINT = 3;
  ENTRYPOINT = 4;

type
  TBlocks = Class
    blocktype: array [0 .. 100, 0 .. 100] of integer;
    constructor create;
    procedure PlaceBlock(x, y: integer);
    procedure SaveToFile(filename: string);
    procedure LoadFromFile(filename: string);
  End;

var
  block_type: integer = 1; // -1 : none, 0: grass,  1: water, 2: lava
  blocks: TBlocks;

implementation

uses MEMain, Camera;

constructor TBlocks.create;
var
  i, j: integer;
begin
  for j := 0 to high(blocktype) do
    for i := 0 to high(blocktype) do
      blocktype[j, i] := (-1);
end;

Procedure TBlocks.LoadFromFile(filename: string);
var
  blockFile: textFile;
  i: integer;
  j: integer;
begin
  assignFile(blockFile, 'maps/' + filename + '.bmap');
  reset(blockFile);

  for j := 0 to high(blocks.blocktype) do
    for i := 0 to high(blocks.blocktype) do
      readln(blockFile, blocks.blocktype[j, i]);

  closefile(blockFile);
end;

Procedure TBlocks.SaveToFile(filename: string);
var
  blockFile: textFile;
  i: integer;
  j: integer;
begin
  assignFile(blockFile, 'maps/' + filename + '.bmap');
  rewrite(blockFile);

  for j := 0 to high(blocks.blocktype) do
    for i := 0 to high(blocks.blocktype) do
      writeln(blockFile, blocks.blocktype[j, i]);

  closefile(blockFile);
end;

Procedure TBlocks.PlaceBlock(x, y: integer);
begin
  x := trunc(x / 30);
  y := trunc(y / 30);

  blocktype[x, y] := block_type;
End;

begin
  blocks := TBlocks.create;

END.
