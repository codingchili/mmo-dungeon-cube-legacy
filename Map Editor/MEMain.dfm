object window: Twindow
  Left = 0
  Top = 0
  Caption = 'DatMap'
  ClientHeight = 933
  ClientWidth = 1157
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object panel_toolbar: TPanel
    Left = 110
    Top = 368
    Width = 611
    Height = 41
    TabOrder = 0
    object type_select: TComboBox
      Left = 248
      Top = 11
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'type_select'
      Items.Strings = (
        'Select Block Type')
    end
    object button_fill: TButton
      Left = 387
      Top = 11
      Width = 75
      Height = 22
      Caption = 'Fill'
      TabOrder = 1
      OnClick = button_fillClick
    end
    object combo_width: TComboBox
      Left = 481
      Top = 11
      Width = 48
      Height = 21
      ItemIndex = 6
      TabOrder = 2
      Text = '6400'
      OnChange = combo_widthChange
      Items.Strings = (
        '480'
        '960'
        '1600'
        '2080'
        '2400'
        '3200'
        '6400')
    end
    object combo_height: TComboBox
      Left = 548
      Top = 11
      Width = 53
      Height = 21
      ItemIndex = 6
      TabOrder = 3
      Text = '6400'
      OnChange = combo_heightChange
      Items.Strings = (
        '480'
        '960'
        '1600'
        '2080'
        '2400'
        '3200'
        '6400')
    end
  end
  object scroll_view: TScrollBox
    Left = 164
    Top = 32
    Width = 509
    Height = 341
    HorzScrollBar.Increment = 32
    VertScrollBar.Increment = 32
    VertScrollBar.Style = ssFlat
    VertScrollBar.Tracking = True
    AutoScroll = False
    DragKind = dkDock
    TabOrder = 1
    OnMouseDown = scroll_viewMouseDown
    object image_target: TImage
      Left = 0
      Top = 0
      Width = 6000
      Height = 6000
    end
    object image_mouse: TImage
      Left = 500
      Top = 500
      Width = 500
      Height = 500
      OnMouseDown = image_mouseMouseDown
    end
  end
  object menu: TPopupMenu
    Left = 412
    Top = 832
    object New1: TMenuItem
      Caption = 'New'
      OnClick = New1Click
    end
    object Load1: TMenuItem
      Caption = 'Load'
      OnClick = Load1Click
    end
    object Save1: TMenuItem
      Caption = 'Save'
      OnClick = Save1Click
    end
    object Exit1: TMenuItem
      Caption = 'Exit'
      OnClick = Exit1Click
    end
  end
  object timer_camupdate: TTimer
    Interval = 50
    OnTimer = timer_camupdateTimer
    Left = 296
    Top = 772
  end
  object timer_freedraw: TTimer
    Interval = 4
    OnTimer = timer_freedrawTimer
    Left = 280
    Top = 840
  end
end
