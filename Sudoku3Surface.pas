UNIT Sudoku3Surface;

{$mode objfpc}{$H+}
INTERFACE

USES
  Classes, sysutils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  sudoku,Menus, StdCtrls, Buttons, ComCtrls, sudokuSettings,exportUnit,sudokuHighscores;

TYPE

  { TSudokuMainForm }

  TSudokuMainForm = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItemHOF04: TMenuItem;
    MenuItemHOF06: TMenuItem;
    MenuItemHOF08: TMenuItem;
    MenuItemHOF09: TMenuItem;
    MenuItemHOF12: TMenuItem;
    MenuItemHOF15: TMenuItem;
    MenuItemHOF16: TMenuItem;
    MenuItemNG04: TMenuItem;
    MenuItemNG06: TMenuItem;
    MenuItemNG08: TMenuItem;
    MenuItemNG09: TMenuItem;
    MenuItemNG12: TMenuItem;
    MenuItemNG15: TMenuItem;
    MenuItemNG16: TMenuItem;
    NumButton_c: TButton;
    NumButton_d: TButton;
    NumButton_e: TButton;
    NumButton_f: TButton;
    NumButton_g: TButton;
    NumButton_h: TButton;
    NumButton_i: TButton;
    NumButton_j: TButton;
    NumButton_k: TButton;
    NumButton_l: TButton;
    NumButton_m: TButton;
    NumButton_n: TButton;
    NumButton_o: TButton;
    NumButton_p: TButton;
    NumButton_a: TButton;
    NumButton_b: TButton;
    NumPanel: TPanel;
    Label11: TLabel;
    MainImage: TImage;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE MainImageMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE MenuItem2Click(Sender: TObject);
    PROCEDURE MenuItem3Click(Sender: TObject);
    PROCEDURE MenuItem8Click(Sender: TObject);
    PROCEDURE MenuItem9Click(Sender: TObject);
    PROCEDURE MenuItemHOF04Click(Sender: TObject);
    PROCEDURE MenuItemHOF06Click(Sender: TObject);
    PROCEDURE MenuItemHOF08Click(Sender: TObject);
    PROCEDURE MenuItemHOF09Click(Sender: TObject);
    PROCEDURE MenuItemHOF12Click(Sender: TObject);
    PROCEDURE MenuItemHOF15Click(Sender: TObject);
    PROCEDURE MenuItemHOF16Click(Sender: TObject);
    PROCEDURE MenuItemNG04Click(Sender: TObject);
    PROCEDURE MenuItemNG06Click(Sender: TObject);
    PROCEDURE MenuItemNG08Click(Sender: TObject);
    PROCEDURE MenuItemNG09Click(Sender: TObject);
    PROCEDURE MenuItemNG12Click(Sender: TObject);
    PROCEDURE MenuItemNG15Click(Sender: TObject);
    PROCEDURE MenuItemNG16Click(Sender: TObject);
    PROCEDURE NumButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    PROCEDURE initButtonPanel(size:byte);
    { public declarations }
  end;

VAR
  SudokuMainForm: TSudokuMainForm;

IMPLEMENTATION
PROCEDURE TSudokuMainForm.FormShow(Sender: TObject);
begin
  DoubleBuffered:=true;
  configuring:=false;
  initButtonPanel(config.riddle.getFieldSize);
end;

PROCEDURE TSudokuMainForm.MainImageMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if not(configuring) and not(config.riddle.isPaused) then begin
    keyboardMode:=false;
    if  (x>x0) and (x<x0+config.riddle.getFieldSize*quadSize)
    and (y>y0) and (y<y0+config.riddle.getFieldSize*quadSize) then begin
      selectX:=(x-x0) div quadSize;
      selectY:=(y-y0) div quadSize;
      if (button=mbRight) then begin
        config.riddle.clearState(selectX,selectY);
        config.riddle.renderRiddle;
        NumPanel.visible:=false;
      end else if (SelectX<0) or (selectX>=config.riddle.getFieldSize)
               or (SelectY<0) or (selectY>=config.riddle.getFieldSize) then NumPanel.visible:=false
      else if (config.riddle.givenState(selectX,selectY)) then config.riddle.renderRiddle
      else begin
        NumPanel.Left:=x0+selectX*quadSize+(quadSize-NumPanel.width) shr 1;
        if NumPanel.Left<0                    then NumPanel.Left:=0;
        if NumPanel.Left>width-NumPanel.width then NumPanel.Left:=width-NumPanel.width;
        NumPanel.top :=y0+selectY*quadSize+(quadSize-NumPanel.height) shr 1;
        if NumPanel.top<0                      then NumPanel.top:=0;
        if NumPanel.top>height-NumPanel.height then NumPanel.top:=height-NumPanel.height;
        NumPanel.visible:=true;
      end;
    end;
  end;
end;

PROCEDURE TSudokuMainForm.MenuItem2Click(Sender: TObject);
  begin
    if not(configuring) then config.riddle.switchPause;
  end;

PROCEDURE TSudokuMainForm.MenuItem3Click(Sender: TObject);
  begin
    config.riddle.pauseGame;
    showOptions;
  end;

PROCEDURE TSudokuMainForm.MenuItem8Click(Sender: TObject);
  begin
    config.riddle.pauseGame;
    configuring:=true;
    config.riddle.renderRiddle;
    showExportForm;
    configuring:=false;
    config.riddle.renderRiddle;
  end;

PROCEDURE TSudokuMainForm.MenuItem9Click(Sender: TObject);
begin
  close;
end;

PROCEDURE TSudokuMainForm.MenuItemHOF04Click(Sender: TObject); begin showHallOfFame(0); end;
PROCEDURE TSudokuMainForm.MenuItemHOF06Click(Sender: TObject); begin showHallOfFame(1); end;
PROCEDURE TSudokuMainForm.MenuItemHOF08Click(Sender: TObject); begin showHallOfFame(2); end;
PROCEDURE TSudokuMainForm.MenuItemHOF09Click(Sender: TObject); begin showHallOfFame(3); end;
PROCEDURE TSudokuMainForm.MenuItemHOF12Click(Sender: TObject); begin showHallOfFame(4); end;
PROCEDURE TSudokuMainForm.MenuItemHOF15Click(Sender: TObject); begin showHallOfFame(5); end;
PROCEDURE TSudokuMainForm.MenuItemHOF16Click(Sender: TObject); begin showHallOfFame(6); end;
PROCEDURE TSudokuMainForm.MenuItemNG04Click(Sender: TObject);
begin config.riddle.initGame(4); config.gameIsDone:=false; config.riddle.renderRiddle; initButtonPanel(4); end;
PROCEDURE TSudokuMainForm.MenuItemNG06Click(Sender: TObject);
begin config.riddle.initGame(6); config.gameIsDone:=false; config.riddle.renderRiddle; initButtonPanel(6); end;
PROCEDURE TSudokuMainForm.MenuItemNG08Click(Sender: TObject);
begin config.riddle.initGame(8); config.gameIsDone:=false; config.riddle.renderRiddle; initButtonPanel(8); end;
PROCEDURE TSudokuMainForm.MenuItemNG09Click(Sender: TObject);
begin config.riddle.initGame(9); config.gameIsDone:=false; config.riddle.renderRiddle; initButtonPanel(9); end;
PROCEDURE TSudokuMainForm.MenuItemNG12Click(Sender: TObject);
begin config.riddle.initGame(12); config.gameIsDone:=false; config.riddle.renderRiddle; initButtonPanel(12); end;
PROCEDURE TSudokuMainForm.MenuItemNG15Click(Sender: TObject);
begin config.riddle.initGame(15); config.gameIsDone:=false; config.riddle.renderRiddle; initButtonPanel(15); end;
PROCEDURE TSudokuMainForm.MenuItemNG16Click(Sender: TObject);
begin config.riddle.initGame(16); config.gameIsDone:=false; config.riddle.renderRiddle; initButtonPanel(16); end;

PROCEDURE TSudokuMainForm.NumButtonClick(Sender: TObject);
begin
  config.riddle.setState(selectX,selectY,strToInt(TButton(Sender).caption),false);
  config.riddle.renderRiddle;
  NumPanel.visible:=false;
end;

PROCEDURE TSudokuMainForm.FormResize(Sender: TObject);
  begin
    config.riddle.renderRiddle;
  end;

PROCEDURE TSudokuMainForm.FormKeyDown(Sender: TObject; VAR key: word;
  Shift: TShiftState);
  VAR bKey:byte;
begin
  NumPanel.visible:=false;
  if not(configuring) then begin
    bKey:=key;
    if config.riddle.isPaused and (Shift=[]) and (key=80) then begin
      config.riddle.switchPause;
      config.riddle.renderRiddle;
    end else if not(config.riddle.isPaused) and
       (Shift=[]) and
       (key<=105) and
       (bKey in [8,37..40,46,48..57,80,96..105]) then begin
      keyboardMode:=true;
      case bKey of
        38    : if selectY>0                            then dec(selectY);
        40    : if selectY<config.riddle.getFieldSize-1 then inc(selectY);
        37    : if selectX>0                            then dec(selectX);
        39    : if selectX<config.riddle.getFieldSize-1 then inc(selectX);
        48, 96: config.riddle.setState(selectX,selectY,0,true);
        49, 97: config.riddle.setState(selectX,selectY,1,true);
        50, 98: config.riddle.setState(selectX,selectY,2,true);
        51, 99: config.riddle.setState(selectX,selectY,3,true);
        52,100: config.riddle.setState(selectX,selectY,4,true);
        53,101: config.riddle.setState(selectX,selectY,5,true);
        54,102: config.riddle.setState(selectX,selectY,6,true);
        55,103: config.riddle.setState(selectX,selectY,7,true);
        56,104: config.riddle.setState(selectX,selectY,8,true);
        57,105: config.riddle.setState(selectX,selectY,9,true);
         8, 46: config.riddle.clearState(selectX,selectY);
        80    : config.riddle.switchPause;
      end; //case
      config.riddle.renderRiddle;
    end;
  end;
end;

PROCEDURE TSudokuMainForm.FormCreate(Sender: TObject);
  begin
    MainImage.height:=screen.height;
    MainImage.width:=screen.width;
    sudoku.mainImage:=MainImage;
  end;

PROCEDURE TSudokuMainForm.initButtonPanel(size:byte);
  begin
    case size of
      4: begin
           NumButton_a.caption:='1'; NumButton_a.visible:=true;
           NumButton_b.caption:='2'; NumButton_b.visible:=true;
           NumButton_e.caption:='3'; NumButton_e.visible:=true;
           NumButton_f.caption:='4'; NumButton_f.visible:=true;
           NumPanel.width :=2*32;
           NumPanel.height:=2*32;
         end;
      6: begin
           NumButton_a.caption:='1'; NumButton_a.visible:=true;
           NumButton_b.caption:='2'; NumButton_b.visible:=true;
           NumButton_c.caption:='3'; NumButton_c.visible:=true;
           NumButton_e.caption:='4'; NumButton_e.visible:=true;
           NumButton_f.caption:='5'; NumButton_f.visible:=true;
           NumButton_g.caption:='6'; NumButton_g.visible:=true;
           NumPanel.width :=3*32;
           NumPanel.height:=2*32;
         end;
      8: begin
           NumButton_a.caption:='1'; NumButton_a.visible:=true;
           NumButton_b.caption:='2'; NumButton_b.visible:=true;
           NumButton_c.caption:='3'; NumButton_c.visible:=true;
           NumButton_e.caption:='4'; NumButton_e.visible:=true;
           NumButton_f.caption:='5'; NumButton_f.visible:=true;
           NumButton_g.caption:='6'; NumButton_g.visible:=true;
           NumButton_i.caption:='7'; NumButton_i.visible:=true;
           NumButton_j.caption:='8'; NumButton_j.visible:=true;
                                     NumButton_k.visible:=false;
           NumPanel.width :=3*32;
           NumPanel.height:=3*32;
         end;
      9: begin
           NumButton_a.caption:='1'; NumButton_a.visible:=true;
           NumButton_b.caption:='2'; NumButton_b.visible:=true;
           NumButton_c.caption:='3'; NumButton_c.visible:=true;
           NumButton_e.caption:='4'; NumButton_e.visible:=true;
           NumButton_f.caption:='5'; NumButton_f.visible:=true;
           NumButton_g.caption:='6'; NumButton_g.visible:=true;
           NumButton_i.caption:='7'; NumButton_i.visible:=true;
           NumButton_j.caption:='8'; NumButton_j.visible:=true;
           NumButton_k.caption:='9'; NumButton_k.visible:=true;
           NumPanel.width :=3*32;
           NumPanel.height:=3*32;
         end;
     12: begin
           NumButton_a.caption:='1';  NumButton_a.visible:=true;
           NumButton_b.caption:='2';  NumButton_b.visible:=true;
           NumButton_c.caption:='3';  NumButton_c.visible:=true;
           NumButton_d.caption:='4';  NumButton_d.visible:=true;
           NumButton_e.caption:='5';  NumButton_e.visible:=true;
           NumButton_f.caption:='6';  NumButton_f.visible:=true;
           NumButton_g.caption:='7';  NumButton_g.visible:=true;
           NumButton_h.caption:='8';  NumButton_h.visible:=true;
           NumButton_i.caption:='9';  NumButton_i.visible:=true;
           NumButton_j.caption:='10'; NumButton_j.visible:=true;
           NumButton_k.caption:='11'; NumButton_k.visible:=true;
           NumButton_l.caption:='12'; NumButton_l.visible:=true;
           NumPanel.width :=4*32;
           NumPanel.height:=3*32;
         end;
     15: begin
           NumButton_a.caption:='1';  NumButton_a.visible:=true;
           NumButton_b.caption:='2';  NumButton_b.visible:=true;
           NumButton_c.caption:='3';  NumButton_c.visible:=true;
           NumButton_d.caption:='4';  NumButton_d.visible:=true;
           NumButton_e.caption:='5';  NumButton_e.visible:=true;
           NumButton_f.caption:='6';  NumButton_f.visible:=true;
           NumButton_g.caption:='7';  NumButton_g.visible:=true;
           NumButton_h.caption:='8';  NumButton_h.visible:=true;
           NumButton_i.caption:='9';  NumButton_i.visible:=true;
           NumButton_j.caption:='10'; NumButton_j.visible:=true;
           NumButton_k.caption:='11'; NumButton_k.visible:=true;
           NumButton_l.caption:='12'; NumButton_l.visible:=true;
           NumButton_m.caption:='13'; NumButton_m.visible:=true;
           NumButton_n.caption:='14'; NumButton_n.visible:=true;
           NumButton_o.caption:='15'; NumButton_o.visible:=true;
                                      NumButton_p.visible:=false;
           NumPanel.width :=4*32;
           NumPanel.height:=4*32;
         end;
     16: begin
           NumButton_a.caption:='1';  NumButton_a.visible:=true;
           NumButton_b.caption:='2';  NumButton_b.visible:=true;
           NumButton_c.caption:='3';  NumButton_c.visible:=true;
           NumButton_d.caption:='4';  NumButton_d.visible:=true;
           NumButton_e.caption:='5';  NumButton_e.visible:=true;
           NumButton_f.caption:='6';  NumButton_f.visible:=true;
           NumButton_g.caption:='7';  NumButton_g.visible:=true;
           NumButton_h.caption:='8';  NumButton_h.visible:=true;
           NumButton_i.caption:='9';  NumButton_i.visible:=true;
           NumButton_j.caption:='10'; NumButton_j.visible:=true;
           NumButton_k.caption:='11'; NumButton_k.visible:=true;
           NumButton_l.caption:='12'; NumButton_l.visible:=true;
           NumButton_m.caption:='13'; NumButton_m.visible:=true;
           NumButton_n.caption:='14'; NumButton_n.visible:=true;
           NumButton_o.caption:='15'; NumButton_o.visible:=true;
           NumButton_p.caption:='16'; NumButton_p.visible:=true;
           NumPanel.width :=4*32;
           NumPanel.height:=4*32;
         end;
    end;
  end;

INITIALIZATION
  {$I Sudoku3Surface.lrs}
  randomize;
  config.create;
  winnerEntry.time:=-1;

FINALIZATION
  config.destroy;

end.

