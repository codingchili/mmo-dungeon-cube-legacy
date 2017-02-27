unit System_ServerList;

interface

//save and initialize on boot, all servers are then initialized as offline.

type
 TServerInfo = Record
   public
   name, host, status: string;
   users, slots, port, ping: integer; //ping from master.
 End;

type
 TServerList = class
   public
   servers: array of TServerInfo;
   procedure pingpacket(name: string);
   procedure add(name, host: string; port: integer);
   procedure update(status: string; users, slots: integer);
 end;

implementation

   procedure TServerList.pingpacket(name: string);
   begin

   end;
   procedure TServerList.add(name, host: string; port: integer);
   begin

   end;
   procedure TServerList.update(status: string; users, slots: integer);
   begin

   end;

end.
