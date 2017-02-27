unit Map_Trees;

interface

uses AsphyreImages, SysUtils;

type
  TTreeState = (Planted, Sapling, Young, Mature, Aged, Decaying, LiteChopped, MedChopped, Chopped);

type
  TTreeType = (Cherry, Oak, Pine, spruce, birch, arrow, yew);

type
  TTree = record
    x, y: integer;
    state: TTreeState;
  end;

type
  TMapTrees = class
  private
    image: TAsphyreImage; // replace with spritesheet.
    tree: array of TTree;
    bufindex: integer;
    procedure buffdepth(iLo, iHi: integer);
  public
    procedure Draw(postdraw: boolean);
    procedure Spawn(x: integer; y: integer; state: TTreeState; treetype: TTreeType);
    constructor create();
  end;

implementation

uses System_Initializer, AbstractCanvas, AsphyreTypes, System_Camera, System_Log, Engine_Player;

// 310x400
procedure TMapTrees.Draw(postdraw: boolean);
var
  i, len: integer;
begin
  len := length(tree);
  i := bufindex;

  while (i < len) do
  begin

    if (player.y < (tree[i].y + 340)) and not(postdraw) then
      break;

    GameCanvas.UseImagePx(image, Point4(0, 0, 310, 0, 310, 400, 0, 400));
    GameCanvas.TexMap(pBounds4(tree[i].x + cam.x, tree[i].y + cam.y, 310, 400), cColor4($FFFFFFFF),
      TBlendingEffect.beNormal);

    inc(i);
  end;

  if not(postdraw) then
    bufindex := i
  else
    bufindex := 0;
end;

procedure TMapTrees.Spawn(x: integer; y: integer; state: TTreeState; treetype: TTreeType);
begin
  SetLength(tree, length(tree) + 1);
  tree[length(tree) - 1].x := x;
  tree[length(tree) - 1].y := y;
end;

// sort on x, sort on player x
procedure TMapTrees.buffdepth(iLo, iHi: integer);
var
  Lo, Hi, Pivot: integer;
  T: TTree;
begin
  Lo := iLo;
  Hi := iHi;
  Pivot := ord(tree[(Lo + Hi) div 2].y);
  repeat
    while ord(tree[Lo].y) < Pivot do
      inc(Lo);
    while ord(tree[Hi].y) > Pivot do
      Dec(Hi);
    if Lo <= Hi then
    begin
      T := tree[Lo];
      tree[Lo] := tree[Hi];
      tree[Hi] := T;
      inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;
  if Hi > iLo then
    buffdepth(iLo, Hi);
  if Lo < iHi then
    buffdepth(Lo, iHi);
end;

constructor TMapTrees.create;
var
  i, j: integer;
begin
  randomize;
  image := TAsphyreImage.create;
  image.LoadFromFile('media/maps/tree/cherry.png');

  for i := 0 to 50 do
      Spawn(Random(2000) + 400, random(2000) + 400, TTreeState.Mature, TTreeType.Cherry);

  buffdepth(0, high(tree));
  bufindex := 0;
end;

end.
