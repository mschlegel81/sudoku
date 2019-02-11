UNIT endOfGameUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, sudoku,sudokuHighscores;

TYPE
  TendOfGameForm = class(TForm)
    Button10: TButton;
    EnterNameGroupBox: TGroupBox;
    Label10: TLabel;
    Label12: TLabel;
    Label4: TLabel;
    LoserGroupBox: TGroupBox;
    nameEdit: TEdit;
  private

  public

  end;

PROCEDURE showEndOfGameForm(CONST goodEnoughForHallOfFame:boolean);
IMPLEMENTATION
VAR
  endOfGameForm: TendOfGameForm=nil;

PROCEDURE showEndOfGameForm(CONST goodEnoughForHallOfFame: boolean);
  begin
    if endOfGameForm=nil then begin
      endOfGameForm:=TendOfGameForm.create(nil);
    end;

    endOfGameForm.LoserGroupBox.visible:=not(goodEnoughForHallOfFame);
    endOfGameForm.EnterNameGroupBox.visible:=goodEnoughForHallOfFame;
    endOfGameForm.ShowModal;
    if goodEnoughForHallOfFame then begin
      winnerEntry.name:=endOfGameForm.nameEdit.text;
      config.addHOFEntry(config.riddle.getModeIdx,winnerEntry);
    end;
    showHallOfFame(config.riddle.getModeIdx);
  end;

INITIALIZATION
  {$I endOfGameUnit.lrs}
  endOfGameCallback:=@showEndOfGameForm;

FINALIZATION
  if endOfGameForm<>nil then FreeAndNil(endOfGameForm);

end.

