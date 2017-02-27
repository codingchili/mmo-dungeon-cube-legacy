unit Networking;

interface

uses IdTCPClient, IdSocketHandle, Main, SysUtils, Classes, Windows, System_Vars,
  Patching;

var
  dl: TIdTCPClient;
  key: string;
  current, update: double;
  percent, filesize, index, lastwork, filecount: Integer;
  tick: Int64;
  filesizes: array of Integer;

procedure Connect;
procedure initialize;
procedure download(index: Integer);

implementation


procedure Connect();
begin
  // connect to patch service
  try
    dl.Connect;
  except
    Main.setstatustext('Connection Failure.');
    Sleep(750);
    Main.terminate;
  end;
end;

procedure download(index: Integer);
var
  afile: TFileStream;
begin
  try
    dl.Socket.WriteLn(key + '&' + IntToStr(ord(ptpatch)) + '&' +
      IntToStr(ord(FileData)) + '&' + FloatToStr(current) + '&' +
      IntToStr(index), TEncoding.Unicode);

    filename := dl.Socket.ReadLn('Z', TEncoding.Unicode);
    filename := StringReplace(filename, '/', '\', [rfReplaceAll, rfIgnoreCase]);
    ForceDirectories(GetCurrentDir + '\' + ExtractFilePath(filename));

    if fileexists(filename) then
      DeleteFile(PChar(filename));

    afile := TFileStream.Create(filename, fmCreate);
    dl.Socket.ReadStream(afile, filesizes[index]);
  finally
    FreeAndNil(afile);
  end;
end;

procedure initialize;
begin
  dl := TIdTCPClient.Create();
  dl.HOST := HOST;
  dl.PORT := PORT;
  dl.UseNagle := false;
  dl.OnWorkBegin := Main.netGUI.IdTCPClient1WorkBegin;
  dl.OnWork := Main.netGUI.IdTCPClient1Work;
  dl.OnWorkEnd := Main.netGUI.IdTCPClient1WorkEnd;
  dl.ConnectTimeout := 1250;
  dl.ReadTimeout := 2000;
end;

end.
