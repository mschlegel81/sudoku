UNIT sudokuHighscores;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, Grids,sudoku;

TYPE

  { ThighscoresForm }

  ThighscoresForm = class(TForm)
    Button6: TButton;
    HOFGroupBox: TGroupBox;
    HOFStringGrid: TStringGrid;
    TabControl1: TTabControl;
    PROCEDURE TabControl1Change(Sender: TObject);
  private

  public

  end;

PROCEDURE showHallOfFame(modeIdx:byte);
IMPLEMENTATION
VAR
  highscoresForm: ThighscoresForm=nil;

PROCEDURE showHallOfFame(modeIdx: byte);
  begin
    config.riddle.pauseGame;
    if highscoresForm=nil then highscoresForm:=ThighscoresForm.create(nil);
    highscoresForm.TabControl1.TabIndex:=modeIdx;
    highscoresForm.ShowModal;
  end;

PROCEDURE ThighscoresForm.TabControl1Change(Sender: TObject);
  VAR modeIdx:byte;
      i,h:longint;
  begin
    modeIdx:=TabControl1.TabIndex;
    HOFGroupBox.caption:='Bestenliste '+intToStr(C_sudokuStructure[modeIdx].size)+'x'
                                       +intToStr(C_sudokuStructure[modeIdx].size);
    for i:=0 to 19 do begin
      HOFStringGrid.Cells[1,i+1]:=config.hallOfFame[modeIdx,i].name;
      HOFStringGrid.Cells[2,i+1]:=intToStr(config.hallOfFame[modeIdx,i].given);
      HOFStringGrid.Cells[3,i+1]:=formattedTime(config.hallOfFame[modeIdx,i]);
      if config.hallOfFame[modeIdx,i].markErrors
        then HOFStringGrid.Cells[4,i+1]:='X'
        else HOFStringGrid.Cells[4,i+1]:='';
    end;
    HOFStringGrid.AutoSizeColumns;
    h:=0;
    for i:=0 to 4 do h:=h+HOFStringGrid.ColWidths[i];
    HOFGroupBox.width:=h+10;
    HOFGroupBox.Left:=(width -HOFGroupBox.width ) shr 1;
  end;

INITIALIZATION
  {$I sudokuHighscores.lrs}
FINALIZATION
  if highscoresForm<>nil then FreeAndNil(highscoresForm);

end.

