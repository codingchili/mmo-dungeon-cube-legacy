unit GUI_TextFilter;

interface

 {$IFDEF Linux}
{$Mode Delphi}
{$ENDIF}

// static class for filtering input from keyboard into visual components.

type
  TASCIIFilter = (filterUsername, filterPassword, filterSerial, filterChat);

type
  TextSanitizer = class
  private
    class function Username(const text: string): string;
    class function Password(const text: string): string;
    class function Serial(const text: string): string;
    class function Chat(const text: string): string;
  public
    class function Filter(const text: string; Filter: TASCIIFilter): string;
  end;

implementation

class function TextSanitizer.Filter(const text: string; Filter: TASCIIFilter): string;
begin
  case (Filter) of
    filterUsername:
      result := Username(text);
    filterPassword:
      result := Password(text);
    filterSerial:
      result := Serial(text);
    filterChat:
      result := Chat(text);
  end;
end;

//a-Z, contain 1 space, not beginning or ending.
class function TextSanitizer.Username(const text: string): string;
begin
end;

//a-Z, 0-9, !#"%&/()=?^*[]{}€$£;:_,.-
class function TextSanitizer.Password(const text: string): string;
begin
end;

//A-Z, 0-9
class function TextSanitizer.Serial(const text: string): string;
begin
end;

//a-Z, 0-9, ,.-;:_*^^!"#%&/\()=?`{[]}'*
class function TextSanitizer.Chat(const text: string): string;
begin
end;

end.