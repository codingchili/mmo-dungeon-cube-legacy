unit System_BlockMapper;

interface

uses SysUtils, Math;

const
  BLOCKING = -1;
  WALKABLE = 0;
  LAVA = 1;
  WATER = 2;
  ENDPOINT = 3;
  ENTRYPOINT = 4;

  {
    Type TMap = class
    //map-name
    //map vars from file
    //locations, positions }

type
  TBlocks = Class
  public
    blocktype: array [0 .. 100, 0 .. 100] of integer;
    constructor create(mapname: string); // or hold a temporary map? ->
    procedure PlaceBlock(x, y: integer); // server can create blocking tiles. (boss, ore, tree?)
    procedure SaveToFile(mapname: string); // server may permanently change tiles.
    procedure LoadFromFile(mapname: string);
  End;

var
  block_type: integer = 1; // -1 : none, 0: grass,  1: water, 2: lava
  // blocks: TBlocks;

implementation

constructor TBlocks.create(mapname: string);
var
  i, j: integer;
begin
  for j := 0 to high(blocktype) do
    for i := 0 to high(blocktype) do
      blocktype[j, i] := (-1); // initialize all undefined as BLOCKING (op)
end;

Procedure TBlocks.LoadFromFile(mapname: string);
var
  blockFile: textFile;
  i: integer;
  j: integer;
begin
  assignFile(blockFile, 'maps/' + mapname + '/map/.bmap');
  reset(blockFile);

  for j := 0 to high(blocktype) do
    for i := 0 to high(blocktype) do
      readln(blockFile, blocktype[j, i]);

  closefile(blockFile);
end;

Procedure TBlocks.SaveToFile(mapname: string);
var
  blockFile: textFile;
  i: integer;
  j: integer;
begin
  assignFile(blockFile, 'maps/' + mapname + '/map.bmap');
  rewrite(blockFile);

  for j := 0 to high(blocktype) do
    for i := 0 to high(blocktype) do
      writeln(blockFile, blocktype[j, i]);

  closefile(blockFile);
end;

Procedure TBlocks.PlaceBlock(x, y: integer);
begin
  x := trunc(x / 30);
  y := trunc(y / 30);

  blocktype[x, y] := block_type;
End;

begin

END.
