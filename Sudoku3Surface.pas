UNIT Sudoku3Surface;

{$mode objfpc}{$H+}
INTERFACE

USES
  Classes, sysutils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  sudoku,Menus, StdCtrls, Buttons, ComCtrls, Grids,dos,serializationUtil;

TYPE
  TSudokuMainForm = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button8: TButton;
    Button9: TButton;
    ExportSolutions_CB: TCheckBox;
    ExportSY_RB1: TRadioButton;
    ExportSY_RB2: TRadioButton;
    ExportSY_RB3: TRadioButton;
    ExportSP_RB1: TRadioButton;
    ExportSP_RB2: TRadioButton;
    ExportSP_RB3: TRadioButton;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    ExportNumberEdit: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    ExportDiff_LB: TListBox;
    Label3: TLabel;
    ExportSizeListBox: TListBox;
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
    DiffListBox: TListBox;
    NumPanel: TPanel;
    ExportProgressBar: TProgressBar;
    ExportFT1_RB: TRadioButton;
    ExportFT2_RB: TRadioButton;
    ExportFT3_RB: TRadioButton;
    ExportSX_RB1: TRadioButton;
    ExportSX_RB2: TRadioButton;
    ExportSX_RB3: TRadioButton;
    SaveDialog1: TSaveDialog;
    SetCol3Button: TButton;
    SetCol4Button: TButton;
    SetCol5Button: TButton;
    SetCol6Button: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    SetCol1Button: TButton;
    SetCol2Button: TButton;
    Label12: TLabel;
    LoserGroupBox: TGroupBox;
    nameEdit: TEdit;
    ExportGroupBox: TGroupBox;
    EnterNameGroupBox: TGroupBox;
    HOFGroupBox: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    MarkErrorsCB: TCheckBox;
    MenuItemHOF04: TMenuItem;
    MenuItemHOF06: TMenuItem;
    MenuItemHOF08: TMenuItem;
    MenuItemHOF09: TMenuItem;
    MenuItemHOF12: TMenuItem;
    MenuItemHOF15: TMenuItem;
    MenuItemHOF16: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItemNG09: TMenuItem;
    MenuItemNG12: TMenuItem;
    MenuItemNG15: TMenuItem;
    MenuItemNG16: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItemNG04: TMenuItem;
    MenuItemNG06: TMenuItem;
    MenuItemNG08: TMenuItem;
    HOFStringGrid: TStringGrid;
    XSymmCB: TCheckBox;
    ySymmCB: TCheckBox;
    pSymmCB: TCheckBox;
    ColorDialog1: TColorDialog;
    FontDialog1: TFontDialog;
    DiffGroupBox1: TGroupBox;
    FontLabel: TLabel;
    BG1Shape: TShape;
    BG2Shape: TShape;
    GridShape: TShape;
    GivenShape: TShape;
    Label7: TLabel;
    MenuItem5: TMenuItem;
    NeutralShape: TShape;
    ConfShape: TShape;
    ViewGroupBox: TGroupBox;
    MainImage: TImage;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PROCEDURE Button1Click(Sender: TObject);
    PROCEDURE Button2Click(Sender: TObject);
    PROCEDURE Button3Click(Sender: TObject);
    PROCEDURE Button4Click(Sender: TObject);
    PROCEDURE Button5Click(Sender: TObject);
    PROCEDURE Button6Click(Sender: TObject);
    PROCEDURE Button7Click(Sender: TObject);
    PROCEDURE Button8Click(Sender: TObject);
    PROCEDURE Button9Click(Sender: TObject);
    PROCEDURE ExportNumberEditEditingDone(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormKeyDown(Sender: TObject; VAR key: word; Shift: TShiftState);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE MainImageMouseDown(Sender: TObject; button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    PROCEDURE MenuItem2Click(Sender: TObject);
    PROCEDURE MenuItem4Click(Sender: TObject);
    PROCEDURE MenuItem5Click(Sender: TObject);
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
    PROCEDURE NameEditEditingDone(Sender: TObject);
    PROCEDURE NumButtonClick(Sender: TObject);
    PROCEDURE SetCol1ButtonClick(Sender: TObject);
    PROCEDURE SetCol2ButtonClick(Sender: TObject);
    PROCEDURE SetCol3ButtonClick(Sender: TObject);
    PROCEDURE SetCol4ButtonClick(Sender: TObject);
    PROCEDURE SetCol5ButtonClick(Sender: TObject);
    PROCEDURE SetCol6ButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    PROCEDURE showHOF(modeIdx:byte);
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

PROCEDURE TSudokuMainForm.MenuItem4Click(Sender: TObject);
begin
  config.riddle.pauseGame;
  configuring:=true;
  FontLabel.Font.name    :=config.Font.name;
  if config.Font.bold and config.Font.italic then FontLabel.Font.style:=[fsBold,fsItalic]
  else if config.Font.bold                   then FontLabel.Font.style:=[fsBold]
  else if config.Font.italic                 then FontLabel.Font.style:=[fsItalic];
  BG1Shape .Brush.color   :=config.view.bgColTop   ;
  BG2Shape .Brush.color   :=config.view.bgColBottom;
  GridShape.Brush.color   :=config.view.gridCol    ;
  GivenShape  .Brush.color:=config.view.givenCol   ;
  NeutralShape.Brush.color:=config.view.neutralCol ;
  ConfShape   .Brush.color:=config.view.confCol    ;
  ViewGroupBox.visible:=true;
end;

PROCEDURE TSudokuMainForm.MenuItem5Click(Sender: TObject);
begin
  config.riddle.pauseGame;
  configuring:=true;
  MarkErrorsCB.checked  :=config.difficulty.markErrors;
  XSymmCB     .checked  :=config.difficulty.xSymm;
  ySymmCB     .checked  :=config.difficulty.ySymm;
  pSymmCB     .checked  :=config.difficulty.ptSymm;
  DiffListBox .ItemIndex:=config.difficulty.diff;
  DiffGroupBox1.visible:=true;
end;

PROCEDURE TSudokuMainForm.MenuItem8Click(Sender: TObject);
begin
  config.riddle.pauseGame;
  configuring:=true;
  ExportGroupBox.visible:=true;
end;

PROCEDURE TSudokuMainForm.MenuItem9Click(Sender: TObject);
begin
  close;
end;

PROCEDURE TSudokuMainForm.MenuItemHOF04Click(Sender: TObject);
begin showHOF(0); end;
PROCEDURE TSudokuMainForm.MenuItemHOF06Click(Sender: TObject);
begin showHOF(1); end;
PROCEDURE TSudokuMainForm.MenuItemHOF08Click(Sender: TObject);
begin showHOF(2); end;
PROCEDURE TSudokuMainForm.MenuItemHOF09Click(Sender: TObject);
begin showHOF(3); end;
PROCEDURE TSudokuMainForm.MenuItemHOF12Click(Sender: TObject);
begin showHOF(4); end;
PROCEDURE TSudokuMainForm.MenuItemHOF15Click(Sender: TObject);
begin showHOF(5); end;
PROCEDURE TSudokuMainForm.MenuItemHOF16Click(Sender: TObject);
begin showHOF(6); end;
PROCEDURE TSudokuMainForm.MenuItemNG04Click(Sender: TObject);
begin config.riddle.destroy; config.riddle.create(4); config.gameIsDone:=false; config.riddle.renderRiddle; initButtonPanel(4); end;
PROCEDURE TSudokuMainForm.MenuItemNG06Click(Sender: TObject);
begin config.riddle.destroy; config.riddle.create(6); config.gameIsDone:=false; config.riddle.renderRiddle; initButtonPanel(6); end;
PROCEDURE TSudokuMainForm.MenuItemNG08Click(Sender: TObject);
begin config.riddle.destroy; config.riddle.create(8); config.gameIsDone:=false; config.riddle.renderRiddle; initButtonPanel(8); end;
PROCEDURE TSudokuMainForm.MenuItemNG09Click(Sender: TObject);
begin config.riddle.destroy; config.riddle.create(9); config.gameIsDone:=false; config.riddle.renderRiddle; initButtonPanel(9); end;
PROCEDURE TSudokuMainForm.MenuItemNG12Click(Sender: TObject);
begin config.riddle.destroy; config.riddle.create(12); config.gameIsDone:=false; config.riddle.renderRiddle; initButtonPanel(12); end;
PROCEDURE TSudokuMainForm.MenuItemNG15Click(Sender: TObject);
begin config.riddle.destroy; config.riddle.create(15); config.gameIsDone:=false; config.riddle.renderRiddle; initButtonPanel(15); end;
PROCEDURE TSudokuMainForm.MenuItemNG16Click(Sender: TObject);
begin config.riddle.destroy; config.riddle.create(16); config.gameIsDone:=false; config.riddle.renderRiddle; initButtonPanel(16); end;

PROCEDURE TSudokuMainForm.NameEditEditingDone(Sender: TObject);
begin
  {$ifdef debugMode} writeMyState; {$endif}
  if (winnerEntry.time>0) and not(config.gameIsDone) then begin
    config.gameIsDone:=true;
    winnerEntry.name:=nameEdit.text;
    config.addHOFEntry(config.riddle.getModeIdx,winnerEntry);
    EnterNameGroupBox.visible:=false;
    showHOF(config.riddle.getModeIdx);
  end else begin
    EnterNameGroupBox.visible:=false;
    showHOF(config.riddle.getModeIdx);
  end;
  config.riddle.create(C_sudokuStructure[config.riddle.getModeIdx].size);
  config.gameIsDone:=false;
  config.riddle.pauseGame;
end;

PROCEDURE TSudokuMainForm.NumButtonClick(Sender: TObject);
begin
  config.riddle.setState(selectX,selectY,strToInt(TButton(Sender).caption),false);
  config.riddle.renderRiddle;
  NumPanel.visible:=false;
end;

PROCEDURE TSudokuMainForm.SetCol1ButtonClick(Sender: TObject);
begin
  ColorDialog1.color:=         BG1Shape.Brush.color;
  if ColorDialog1.execute then BG1Shape.Brush.color:=ColorDialog1.color;
end;

PROCEDURE TSudokuMainForm.SetCol2ButtonClick(Sender: TObject);
begin
  ColorDialog1.color:=         BG2Shape.Brush.color;
  if ColorDialog1.execute then BG2Shape.Brush.color:=ColorDialog1.color;
end;

PROCEDURE TSudokuMainForm.SetCol3ButtonClick(Sender: TObject);
begin
  ColorDialog1.color:=         GridShape.Brush.color;
  if ColorDialog1.execute then GridShape.Brush.color:=ColorDialog1.color;
end;

PROCEDURE TSudokuMainForm.SetCol4ButtonClick(Sender: TObject);
begin
  ColorDialog1.color:=         GivenShape.Brush.color;
  if ColorDialog1.execute then GivenShape.Brush.color:=ColorDialog1.color;
end;

PROCEDURE TSudokuMainForm.SetCol5ButtonClick(Sender: TObject);
begin
  ColorDialog1.color:=         NeutralShape.Brush.color;
  if ColorDialog1.execute then NeutralShape.Brush.color:=ColorDialog1.color;
end;

PROCEDURE TSudokuMainForm.SetCol6ButtonClick(Sender: TObject);
begin
  ColorDialog1.color:=         ConfShape.Brush.color;
  if ColorDialog1.execute then ConfShape.Brush.color:=ColorDialog1.color;
end;

PROCEDURE TSudokuMainForm.FormResize(Sender: TObject);
begin
  config.riddle.renderRiddle;
  LoserGroupBox    .Left:=(width -LoserGroupBox    .width ) shr 1;
  LoserGroupBox    .top :=(height-LoserGroupBox    .height) shr 1;
  EnterNameGroupBox.Left:=(width -EnterNameGroupBox.width ) shr 1;
  EnterNameGroupBox.top :=(height-EnterNameGroupBox.height) shr 1;
  HOFGroupBox      .Left:=(width -HOFGroupBox      .width ) shr 1;
  HOFGroupBox      .top :=(height-HOFGroupBox      .height) shr 1;
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

PROCEDURE TSudokuMainForm.Button2Click(Sender: TObject);
begin
  ViewGroupBox.visible:=false;
  configuring:=false;
end;

PROCEDURE TSudokuMainForm.Button3Click(Sender: TObject);
begin
  FontDialog1.Font:=FontLabel.Font;
  if FontDialog1.execute then FontLabel.Font:=FontDialog1.Font;
end;

PROCEDURE TSudokuMainForm.Button4Click(Sender: TObject);
begin
  config.difficulty.markErrors:=MarkErrorsCB.checked ;
  config.difficulty.xSymm     :=XSymmCB     .checked ;
  config.difficulty.ySymm     :=ySymmCB     .checked ;
  config.difficulty.ptSymm    :=pSymmCB     .checked ;
  config.difficulty.diff      :=DiffListBox .ItemIndex;
end;

PROCEDURE TSudokuMainForm.Button5Click(Sender: TObject);
begin
  DiffGroupBox1.visible:=false;
  configuring:=false;
end;

PROCEDURE TSudokuMainForm.Button6Click(Sender: TObject);
begin
  configuring:=false;
  HOFGroupBox.visible:=false;
end;

PROCEDURE TSudokuMainForm.Button7Click(Sender: TObject);
begin
  configuring:=false;
  LoserGroupBox.visible:=false;
  config.riddle.create(C_sudokuStructure[config.riddle.getModeIdx].size);
  config.gameIsDone:=false;
  config.riddle.pauseGame;
end;

VAR solutionOut:boolean;
    solutions:array of string;
    outFile:textFile;

PROCEDURE writeOut(txt:string);
  begin
    if solutionOut
      then solutions[length(solutions)-1]:=solutions[length(solutions)-1]+txt
      else write(outFile,txt);
   end;

PROCEDURE writelnOut(txt:string);
  begin
    if solutionOut
      then begin
        solutions[length(solutions)-1]:=solutions[length(solutions)-1]+txt;
        setLength(solutions,length(solutions)+1);
        solutions[length(solutions)-1]:='';
      end
      else writeln(outFile,txt);
  end;

PROCEDURE TSudokuMainForm.Button8Click(Sender: TObject);
VAR s:T_sudoku;
    x:word;
    txtOut,sx,sy,sc:boolean;
    riddleSize:byte;
    diffic:word;
    numberOfRiddles:word;

FUNCTION enumString(x:word):string;
  begin
    if ExportSolutions_CB.checked then begin
      str(x,result);
      result:='\#'+result;
    end else result:='';
  end;

FUNCTION correctedExtension(fileName:string; ext:string):string;
  VAR a,b,c:shortstring;
  begin
    FSplit(fileName,a,b,c);
    if c<>ext then result:=a+b+ext
              else result:=fileName;
  end;

begin
  if SaveDialog1.execute then begin
    randomize;
    txtOut:=ExportFT1_RB.checked;
    if txtOut then SaveDialog1.fileName:=correctedExtension(SaveDialog1.fileName,'.txt')
              else SaveDialog1.fileName:=correctedExtension(SaveDialog1.fileName,'.tex');
    assignFile(outFile,SaveDialog1.fileName);
    rewrite(outFile);
    case byte(ExportSizeListBox.ItemIndex) of
      0: riddleSize:= 4;
      1: riddleSize:= 6;
      2: riddleSize:= 8;
      3: riddleSize:= 9;
      4: riddleSize:=12;
      5: riddleSize:=15;
      6: riddleSize:=16;
    end;
    diffic:=((75-5*DiffListBox.ItemIndex)*sqr(riddlesize)) div 100;
    numberOfRiddles:=strToInt(ExportNumberEdit.text);
    setLength(solutions,2);
    if txtOut then solutions[0]:='LOESUNGEN:'
              else solutions[0]:='\newpage \begin{Large} L\"osungen \end{Large}';
    solutions[1]:='';
    solutionOut:=false;
    if not(txtOut) then writeLatexHeader(@writelnOut);
    ExportProgressBar.max:=numberOfRiddles;
    for x:=1 to numberOfRiddles do begin
      ExportProgressBar.position:=x;
      if ExportSX_RB3.checked then sx:=(random(3)=0) else sx:=ExportSX_RB1.checked;
      if ExportSY_RB3.checked then sy:=(random(3)=0) else sy:=ExportSY_RB1.checked;
      if ExportSP_RB3.checked then sc:=(random(3)=0) else sc:=ExportSP_RB1.checked;
      s.create(riddleSize,sx,sy,sc,diffic);
      solutionOut:=false;
      if txtOut then s.writeTxtForm  (@writeOut,@writelnOut)
                else s.writeLaTexForm(@writeOut,@writelnOut,enumString(x),false);
      if ExportSolutions_CB.checked then begin
        solutionOut:=true;
        s.solve;
        if txtOut then s.writeTxtForm  (@writeOut,@writelnOut)
                  else s.writeLaTexForm(@writeOut,@writelnOut,enumString(x),true);
      end;
      s.destroy;
    end;
    if ExportSolutions_CB.checked then begin
      solutionOut:=false;
      for x:=0 to length(solutions)-1 do writelnOut(solutions[x]);
    end;
    setLength(solutions,0);
    if not(txtOut) then writelnOut(C_LaTeX_fileFooter);
    closeFile(outFile);
    if ExportFT3_RB.checked then begin
      Exec('cmd','/C pdflatex '+SaveDialog1.fileName);
      Exec('cmd','/C start '+correctedExtension(SaveDialog1.fileName,'.pdf'));
    end else Exec('cmd','/C start '+SaveDialog1.fileName);
  end;
end;

PROCEDURE TSudokuMainForm.Button9Click(Sender: TObject);
begin
  ExportGroupBox.visible:=false;
  configuring:=false;
end;

PROCEDURE TSudokuMainForm.ExportNumberEditEditingDone(Sender: TObject);
VAR num:longint;
begin
  num:=strToInt(ExportNumberEdit.text);
  if num<1 then num:=1 else if num>200 then num:=200;
  ExportNumberEdit.text:=intToStr(num);
end;

PROCEDURE TSudokuMainForm.FormCreate(Sender: TObject);
begin
  MainImage.height:=screen.height;
  MainImage.width:=screen.width;
  sudoku.mainImage:=MainImage;
end;

PROCEDURE TSudokuMainForm.Button1Click(Sender: TObject);
begin
  config.Font.name       :=FontLabel.Font.name;
  config.Font.italic     :=fsItalic in FontLabel.Font.style;
  config.Font.bold       :=fsBold   in FontLabel.Font.style;
  config.view.bgColTop   :=BG1Shape .Brush.color;
  config.view.bgColBottom:=BG2Shape .Brush.color;
  config.view.gridCol    :=GridShape.Brush.color;
  config.view.givenCol   :=GivenShape  .Brush.color;
  config.view.neutralCol :=NeutralShape.Brush.color;
  config.view.confCol    :=ConfShape   .Brush.color;
  config.riddle.renderRiddle;
end;

PROCEDURE TSudokuMainForm.showHOF(modeIdx:byte);
  VAR i:byte;
      h:longint;
  begin
    configuring:=true;
    config.riddle.pauseGame;
    HOFGroupBox.caption:='Bestenliste '+intToStr(C_sudokuStructure[modeIdx].size)+'x'
                                       +intToStr(C_sudokuStructure[modeIdx].size);
    for i:=0 to 19 do begin
      HOFStringGrid.Cells[1,i+1]:=config.hallOfFame[modeIdx,i].name;
      HOFStringGrid.Cells[2,i+1]:=intToStr(config.hallOfFame[modeIdx,i].given);
      HOFStringGrid.Cells[3,i+1]:=FormatDateTime('hh:mm:ss',config.hallOfFame[modeIdx,i].time);
      if config.hallOfFame[modeIdx,i].markErrors
        then HOFStringGrid.Cells[4,i+1]:='X'
        else HOFStringGrid.Cells[4,i+1]:='';
    end;
    HOFStringGrid.AutoSizeColumns;
    h:=0;
    for i:=0 to 4 do h:=h+HOFStringGrid.ColWidths[i];
    HOFGroupBox.width:=h+10;
    HOFGroupBox.Left:=(width -HOFGroupBox.width ) shr 1;
    HOFGroupBox.visible:=true;
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

