unit Conf_Patching;

// the patcher connects to this part and performs a version check.
// we either return that their version is current, or issue a proxy download.

interface

{$IFDEF Linux}
{$MODE DELPHI}
{$ENDIF}

uses {$IFDEF Linux}FileUtil, {$ENDIF} System_Log, System_UtilExt, SysUtils, Classes;

type
    patchfile = record
        name: string;
        version: double;
        size: integer;
        // data: TMemoryStream;
        bytes: TBytes;
    end;

type
    filemeta = record
        name: string;
        version: double;
        size, index: integer;
    end;

type
    TPatchList = array of filemeta;

type
    TPatchConfiguration = class
    public
        files: array of patchfile;
        version: double;
        function filepos(version: double; index: integer): integer;
        function filecount(version: double): integer;
        function metadata(version: double): TPatchList;
        constructor create;
    end;

const
    fProxy = 'deploy/'; // folder proxy, dont upload files in the same folder.

procedure initialize();

var
    configs: TPatchConfiguration;

implementation

function TPatchConfiguration.metadata(version: double): TPatchList;
var
    i, pos: integer;
begin
    try
        setlength(result, 0);

        for i := 0 to length(files) - 1 do
        begin
            if (files[i].version > version) then
            begin
                pos := length(result);
                setlength(result, length(result) + 1);
                result[pos].name := files[i].name;
                result[pos].version := files[i].version;
                result[pos].size := files[i].size;
                result[pos].index := i;
            end;
        end;
    except
        on e: exception do
            print(e.Message);
    end;
end;

// get files by mask
function TPatchConfiguration.filepos(version: double; index: integer): integer;
var
    i: integer;
begin
    result := 0;
    try
        for i := 0 to high(files) do
            if (files[i].version > version) then
                if index > 0 then
                    dec(index)
                else
                begin
                    result := i;
                    break;
                end;
    except
        System_Log.print('Invalid patch parameters received.', System_Log.Red);
    end;
end;

function TPatchConfiguration.filecount(version: double): integer;
var
    i: integer;
begin
    result := 0;

    for i := 0 to high(files) do
        if (files[i].version > version) then
            result := result + 1;
end;

procedure initialize();
begin
    configs := TPatchConfiguration.create;
end;

// load file versions on to the file table.
constructor TPatchConfiguration.create();
var
    i: integer;
    data: TextFile;
    error: boolean;
    filever: double;
    line, name: string;
    fStream: TFileStream;
begin
    setlength(files, 0);
    version := 0.0;
    error := false;

    print('Reading Patch.. ' + #9);

    if fileexists('conf/patch.conf') then
    begin
        try
            AssignFile(data, 'conf/patch.conf');
            reset(data);

            while not(eof(data)) do
            begin
                // parse commands, comment, section, version..
                readln(data, line);

                if (length(line) = 0) then
                    continue
                else if (line[1] = '#') then
                begin
                    print(#10 + #13 + 'Loading ' + copy(line, 2, length(line) - 1) + ' ..', Magenta);
                    continue;
                end
                else if (line[1] = ':') and (length(line) > 1) then
                begin
                    filever := SafeFloat(copy(line, 2, length(line) - 1));
                    continue;
                end
                else
                    name := line;

                if not(fileexists(fProxy + name)) then
                begin
                    print(' Fatal Error. [Missing File: ' + name + ']', Red);
                    error := true;
                    continue;
                end
                else
                begin
                    setlength(files, length(files) + 1);
                    files[high(files)].version := filever;
                    files[high(files)].name := name;
{$IFDEF Win32}
                    files[high(files)].size := FileSize(fProxy + name);
{$ELSE}
                    files[high(files)].size := FileUtil.FileSize(fProxy + name);
{$ENDIF}
                    print(#9 + #9 + line + #9 + #9 + ' [' + FloatToStr(filever) + ']', Green);

                    try
                        fStream := TFileStream.create(fProxy + name, fmOpenRead, 0);

                        setlength(files[high(files)].bytes, fStream.size);
                        fStream.ReadBuffer(files[high(files)].bytes[0], fStream.size);

                        FreeAndNil(fStream);
                    except
                        on e: exception do
                        begin
                            print(' Fatal Error. [' + e.Message + ': ' + name + ']', Red);
                            error := true;
                            continue;
                        end;
                    end;

                    // latest version changes.
                    if (files[high(files)].version > version) then
                        version := files[high(files)].version;
                end;
            end;

        finally
            closefile(data);
            // not(error) then
            // print(' Done. [' + FloatToStr(version) + ']', System_Log.LightGreen);
            print('');
        end;
    end
    else
        print(' Failed. [Missing Patch Configuration]', System_Log.Red, false);
end;

end.
