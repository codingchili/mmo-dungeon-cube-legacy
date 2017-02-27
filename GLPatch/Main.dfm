object RenderForm: TRenderForm
  Left = 0
  Top = 0
  Align = alBottom
  BorderStyle = bsNone
  Caption = 'RenderForm'
  ClientHeight = 43
  ClientWidth = 301
  Color = clBlack
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  OnMouseDown = FormMouseDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 22
    Width = 196
    Height = 13
    Caption = 'Attempting to Download Patcher Files....'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clCream
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object popupmenu: TPopupMenu
    Left = 214
    Top = 4
    object ShowVersion1: TMenuItem
      Caption = 'View'
      Enabled = False
      object Speed1: TMenuItem
        Caption = 'Speed'
        Checked = True
        Enabled = False
      end
      object Filename1: TMenuItem
        Caption = 'Filename'
        Checked = True
        Enabled = False
      end
      object FilesLeft1: TMenuItem
        Caption = 'Files Left'
        Checked = True
        Enabled = False
      end
      object Downloaded1: TMenuItem
        Caption = 'Downloaded'
        Enabled = False
      end
      object ServerIp1: TMenuItem
        Caption = 'Server Ip'
        Enabled = False
      end
      object ServerGeo1: TMenuItem
        Caption = 'Region'
        Enabled = False
      end
    end
    object Settings1: TMenuItem
      Caption = 'Settings'
      object Autostart1: TMenuItem
        Caption = 'Autostart'
        OnClick = Autostart1Click
      end
      object SetRegion1: TMenuItem
        Caption = 'Set Region'
        object EU1: TMenuItem
          Caption = 'Europe'
          Enabled = False
        end
        object NA1: TMenuItem
          Caption = 'West'
          Enabled = False
        end
        object AS1: TMenuItem
          Caption = 'East'
          Enabled = False
        end
        object Custom1: TMenuItem
          Caption = 'Custom..'
          OnClick = Custom1Click
        end
      end
      object ResetVersion1: TMenuItem
        Caption = 'Reset Version'
        OnClick = ResetVersion1Click
      end
      object Compact1: TMenuItem
        Caption = 'Compact'
        OnClick = Compact1Click
      end
    end
    object Close1: TMenuItem
      Caption = 'Close'
      OnClick = Close1Click
    end
  end
  object smooth: TTimer
    Interval = 16
    Left = 248
    Top = 4
  end
end
