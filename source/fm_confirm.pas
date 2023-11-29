unit fm_confirm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Buttons, ExtCtrls, Dialogs, LCLType, Controls;

type

  { TfmConfirm }

  TfmConfirm = class(TForm)
    bbYes:     TBitBtn;
    bbNo:      TBitBtn;
    bbCancel:  TBitBtn;
    Label1:    TLabel;
    lbMessage: TLabel;
    pButtons:  TPanel;
    pMessage:  TPanel;
    pValueR:   TPanel;

    procedure FormShow(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure FormDeactivate(Sender: TObject);

  private
    FButtons:       TMsgDlgButtons;
    FWindowCaption: String;
    FWindowParent:  TForm;
    FWindowText:    String;

    procedure SetButtons(AValue: TMsgDlgButtons);
    procedure SetWindowCaption(AValue: String);
    procedure SetWindowText(AValue: String);

  public
    property Buttons: TMsgDlgButtons read FButtons write SetButtons;
    property WindowCaption: String read FWindowCaption write SetWindowCaption;
    property WindowText: String read FWindowText write SetWindowText;
    property WindowParent: TForm read FWindowParent write FWindowParent;

    // быстрый вариант вызова окна подтверждения, AButtons = [mbYes, mbNo, mbCancel]
    function Show(ACaption, AText: String; AButtons: TMsgDlgButtons;
      AParent: TForm): TModalResult;
  end;

var
  fmConfirm: TfmConfirm;

implementation

{$R *.lfm}

{ TfmConfirm }

procedure TfmConfirm.SetWindowCaption(AValue: String);
  begin
    FWindowCaption := AValue;
    Caption        := FWindowCaption;
  end;

procedure TfmConfirm.SetWindowText(AValue: String);
  begin
    FWindowText       := AValue;
    lbMessage.Caption := FWindowText;
  end;

procedure TfmConfirm.SetButtons(AValue: TMsgDlgButtons);
  begin
    FButtons := AValue;

    bbYes.Visible    := mbYes in FButtons;
    bbNo.Visible     := mbNo in FButtons;
    bbCancel.Visible := mbCancel in FButtons;
    pButtons.Visible := FButtons <> [];
  end;


procedure TfmConfirm.FormShow(Sender: TObject);
  begin
    lbMessage.Caption := String(lbMessage.Caption).Replace('---', '—');
    lbMessage.Caption := String(lbMessage.Caption).Replace('--', '–');

    lbMessage.Constraints.MinWidth := pButtons.Width;
    if FWindowParent = nil then
      begin
      Top  := (Screen.Height - Height) div 2;
      Left := (Screen.Width - Width) div 2;
      end
    else
      begin
      Top  := FWindowParent.Top + (FWindowParent.Height - Height) div 2;
      Left := FWindowParent.Left + (FWindowParent.Width - Width) div 2;
      end;
  end;

procedure TfmConfirm.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  begin
    case UTF8Key of

      // Enter
      chr(13):
        bbYes.Click;

      // Esc
      chr(27):
        if bbCancel.Visible then
          bbCancel.Click
        else
          bbNo.Click;
      end;
  end;

procedure TfmConfirm.FormDeactivate(Sender: TObject);
  begin
    if FButtons = [] then Close;
  end;

function TfmConfirm.Show(ACaption, AText: String;
  AButtons: TMsgDlgButtons; AParent: TForm): TModalResult;
  begin
    WindowParent  := AParent;
    WindowState   := AParent.WindowState;
    WindowText    := AText;
    WindowCaption := ACaption;
    Buttons       := AButtons;

    if FButtons = [] then
      inherited Show
    else
      Result := fmConfirm.ShowModal;

    lbMessage.Constraints.MinWidth := 0;
  end;

end.
