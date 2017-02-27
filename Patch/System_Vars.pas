unit System_Vars;

interface

type
  TPackType = (ptMovement = 0, ptSpell = 1, ptLoot = 2, ptInventory = 3, ptChat = 4,
    ptNPC = 5, ptControl = 6, ptPatch = 7, ptPing = 8, ptLobby = 9);

type
  TAction = (Up = 0, Down = 1, Left = 2, Right = 3, View = 4, Send = 5,
    Talk = 6, Cast = 7, Disconnect = 8, Connect = 9, Registrate = 10, None = 11,
    Version = 12, Download = 13, Count = 14, FileData = 15, Msg = 16,
    login = 17, User = 18, FileSize = 19);
  // TODO: northeast, southeast, northwest, southwest

type
  TReply = (Success = 1, Failed = 0, Registered = 2);

  const
  PORT = 1556;
  HOST = '127.0.0.1';

implementation

end.
