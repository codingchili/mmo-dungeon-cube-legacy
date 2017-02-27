unit GUI_TextFilter;

interface

{$IFDEF Linux}
{$MODE Delphi}
{$ENDIF}
// static class for filtering input from keyboard into visual components.

type
  TTextFilter = (Username, Password, Serial, Chat);

type
  TextFilter = class
  private
    class function Username(const key: word): boolean;
    class function Password(const key: word): boolean;
    class function Serial(const key: word): boolean;
    class function Chat(const key: word): boolean;
  public
    class function Filter(const key: word; Filter: TTextFilter): boolean;
  end;

implementation

class function TextFilter.Filter(const key: word; Filter: TTextFilter): boolean;
begin
  case (Filter) of
    TTextFilter.Username:
      result := Username(key);
    TTextFilter.Password:
      result := Password(key);
    TTextFilter.Serial:
      result := Serial(key);
    TTextFilter.Chat:
      result := Chat(key);
  end;
end;

// a-Z, contain 1 space, not beginning or ending.
class function TextFilter.Username(const key: word): boolean;
begin
  result := ((key >= $30) and (key <= $5A)) or ((key >= $60) and (key <= $6F)) or (key = $08);
end;

// a-Z, 0-9, !#"%&/()=?^*[]{}€$£;:_,.-
class function TextFilter.Password(const key: word): boolean;
begin
end;

// A-Z, 0-9
class function TextFilter.Serial(const key: word): boolean;
begin
end;

// a-Z, 0-9, ,.-;:_*^^!"#%&/\()=?`{[]}'*
class function TextFilter.Chat(const key: word): boolean;
begin
end;

end.
