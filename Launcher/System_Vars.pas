unit System_Vars;

interface

type
  TPackType = (Login = 0, Serverlist = 1, Ping = 2, Patch = 3, Disconnect = 8, Connect = 9, Character = 10, Map = 11,
    License = 12);

type
  TAction = (None = 0, FileData = 1, Version = 2, count = 3, filesize = 4, Success = 5, Failure = 6, Registered = 7,
    Add = 10, Download = 11, Update = 12, Movement = 13, Load = 14, Attribute = 15, Info = 16, Exist = 17,
    Unavailable = 18, Name = 19, Select = 20, Attributes = 21, Online = 22, Offline = 23, AlreadyLoggedOn = 24);

const
  PORT = 1576;
  HOST = '127.0.0.1';//'dungeoncube.se';

implementation

end.
