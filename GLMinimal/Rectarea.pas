unit Rectarea;

{$IFDEF Linux}{$mode delphi}{$H+}{$ENDIF}

interface

uses
  SysUtils, Types;

type
  TRect = record
    top, left, width, height, right, bottom: integer;
    function setLocation(left, top: integer): TRect;
    function topLeft(): TPoint;
    function contains(Point: TPoint): boolean;
  end;

function new(left, top, width, height: integer): TRect;

implementation

function TRect.setLocation(left, top: integer): TRect;
begin
  self.top := top;
  self.left := left;
  self.right := left + self.width;
  self.bottom := top + self.height;
  result := self;
end;

function TRect.topLeft(): TPoint;
begin
  result.x := left;
  result.y := top;
end;

function new(left, top, width, height: integer): TRect;
begin
  result.top := top;
  result.left := left;
  result.width := width;
  result.height := height;
end;

function TRect.contains(Point: TPoint): boolean;
begin
  if ((Point.x > self.left) and (Point.x < self.left + self.width)) and
    ((Point.y > self.top) and (Point.y < self.top + self.height)) then
    result := true
  else
    result := false;
end;

end.
