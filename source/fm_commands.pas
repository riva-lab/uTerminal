unit fm_commands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Grids, Buttons,
  ActnList, LazUTF8;

type

  { TfmCommands }

  TfmCommands = class(TForm)
    acAdd:         TAction;
    acDelete:      TAction;
    acOK:          TAction;
    acCancel:      TAction;
    acUpdate:      TAction;
    alActionList1: TActionList;
    BitBtn1:       TBitBtn;
    BitBtn2:       TBitBtn;
    BitBtn3:       TBitBtn;
    BitBtn4:       TBitBtn;
    BitBtn5:       TBitBtn;
    edSequence:    TEdit;
    edAnswer:      TEdit;
    lbInput:       TLabel;
    lbResponce:    TLabel;
    Panel2:        TPanel;
    pSequence:     TPanel;
    Panel4:        TPanel;
    Panel5:        TPanel;
    sgSequences:   TStringGrid;
    procedure acAddExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acOKExecute(Sender: TObject);
    procedure acUpdateExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sgSequencesSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);

  private
    FAnswerListA:   TStringList;
    FSequenceListA: TStringList;
    FAnswerList:    TStringList;
    FSequenceList:  TStringList;

    procedure UpdateTable;

  public
    property AnswerList: TStringList read FAnswerList write FAnswerList;
    property SequenceList: TStringList read FSequenceList write FSequenceList;
  end;

var
  fmCommands: TfmCommands;

implementation

{$R *.lfm}

{ TfmCommands }

procedure TfmCommands.FormCreate(Sender: TObject);
  begin
    FSequenceListA := TStringList.Create;
    FAnswerListA   := TStringList.Create;
    FSequenceList  := TStringList.Create;
    FAnswerList    := TStringList.Create;
  end;

procedure TfmCommands.FormShow(Sender: TObject);
  begin
    FSequenceListA.Text := FSequenceList.Text;
    FAnswerListA.Text   := FAnswerList.Text;

    sgSequences.Columns.Items[0].Title.Caption := lbInput.Caption;
    sgSequences.Columns.Items[1].Title.Caption := lbResponce.Caption;

    UpdateTable;
  end;


procedure TfmCommands.sgSequencesSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
  begin
    if Visible and (aRow > 0) then
      begin
      if FSequenceListA.Count >= aRow then
        edSequence.Text := FSequenceListA.Strings[aRow - 1];

      if FAnswerListA.Count >= aRow then
        edAnswer.Text := FAnswerListA.Strings[aRow - 1];
      end
    else
      begin
      edSequence.Text := '';
      edAnswer.Text   := '';
      end;
  end;

procedure TfmCommands.UpdateTable;
  var
    not_empty: Boolean;
    i:         Integer;
  begin
    sgSequences.RowCount := FSequenceListA.Count + 1;

    for i := 1 to FSequenceListA.Count do
      begin
      sgSequences.Cols[0].Strings[i] := FSequenceListA.Strings[i - 1];
      sgSequences.Cols[1].Strings[i] := FAnswerListA.Strings[i - 1];
      end;

    not_empty        := FSequenceListA.Count > 0;
    acDelete.Enabled := not_empty;
    acUpdate.Enabled := not_empty;

    if not_empty then
      begin
      edSequence.Text := FSequenceListA.Strings[sgSequences.Row - 1];
      edAnswer.Text   := FAnswerListA.Strings[sgSequences.Row - 1];
      end;
  end;


procedure TfmCommands.acAddExecute(Sender: TObject);
  begin
    if (edSequence.Text <> '') and (edAnswer.Text <> '') then
      begin
      FSequenceListA.Append(edSequence.Text);
      FAnswerListA.Append(edAnswer.Text);
      UpdateTable;
      sgSequences.Row := sgSequences.RowCount - 1;
      end;
  end;

procedure TfmCommands.acUpdateExecute(Sender: TObject);
  var
    index: Integer;
  begin
    index := sgSequences.Selection.Top - 1;

    if index >= 0 then
      begin
      FSequenceListA.Strings[index] := edSequence.Text;
      FAnswerListA.Strings[index]   := edAnswer.Text;
      end;

    UpdateTable;
  end;

procedure TfmCommands.acDeleteExecute(Sender: TObject);
  var
    index: Integer;
  begin
    index := sgSequences.Selection.Top - 1;

    if index >= 0 then
      begin
      FSequenceListA.Delete(index);
      FAnswerListA.Delete(index);
      end;

    UpdateTable;
  end;


procedure TfmCommands.acOKExecute(Sender: TObject);
  begin
    FSequenceList.Text := FSequenceListA.Text;
    FAnswerList.Text   := FAnswerListA.Text;

    ModalResult := mrOk;
  end;

procedure TfmCommands.acCancelExecute(Sender: TObject);
  begin
    ModalResult := mrCancel;
  end;

end.
