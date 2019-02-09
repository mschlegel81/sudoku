UNIT Sudoku3Surface;

{$mode objfpc}{$H+}
INTERFACE

USES
  Classes, sysutils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  bufFiles,sudoku,mySimpleStrings,Menus, StdCtrls, Buttons, ComCtrls, Grids,dos;

TYPE

  hallOfFameEntry=record
    name :string;
    time :longint;
    given:word;
    markErrors:boolean;
  end;

  T_sudokuRiddle=object(serializable)
    private
      fieldSize:byte;
      modeIdx  :byte;
      state:array[0..15,0..15] of record
                                    given,conflicting:boolean;
                                    value:byte;
                                  end;
      startTime :longint;
      paused    :boolean;
      PROCEDURE checkConflicts;
    public
      CONSTRUCTOR create(size:byte);
      DESTRUCTOR  destroy;
      PROCEDURE   pauseGame;
      PROCEDURE   switchPause;
      FUNCTION    isPaused:boolean;
      FUNCTION    isSolved:boolean;
      FUNCTION    makeHOFEntry:hallOfFameEntry;
      PROCEDURE   setState(x,y,value:byte; append:boolean);
      PROCEDURE   clearState(x,y:byte);
      FUNCTION    givenState(x,y:byte):boolean;
      PROCEDURE   renderRiddle;
      FUNCTION  loadFromFile(VAR F:bufferedFile):boolean; virtual;  overload; //liest die Inhalte des Objektes aus einer bereits geöffneten Datei und gibt true zurück gdw. kein Fehler auftrat
      PROCEDURE saveToFile(VAR F:bufferedFile);           virtual;  overload; //schreibt die Inhalte des Objektes in eine bereits geöffnete Datei
      FUNCTION  defaultFilesize:longint;                  virtual;            //gibt die Puffergröße (=übliche Dateigröße) an
  end;

  T_config=object(serializable)
    view:record
           bgColTop,
           bgColBottom,
           gridCol,
           givenCol,
           neutralCol,
           confCol:longint;
         end;
    Font:record
           name:string;
           bold,italic:boolean;
         end;
    difficulty:record
                 markErrors,xSymm,ySymm,ptSymm:boolean;
                 diff:byte;
               end;
    hallOfFame:array [0..6,0..19] of hallOfFameEntry;
    riddle    :T_sudokuRiddle;
    gameIsDone:boolean;
    CONSTRUCTOR create;
    DESTRUCTOR  destroy;
    FUNCTION  loadFromFile(VAR F:bufferedFile):boolean; virtual; overload; //liest die Inhalte des Objektes aus einer bereits geöffneten Datei und gibt true zurück gdw. kein Fehler auftrat
    PROCEDURE saveToFile(VAR F:bufferedFile);           virtual; overload; //schreibt die Inhalte des Objektes in eine bereits geöffnete Datei
    FUNCTION  defaultFilesize:longint;                  virtual;           //gibt die Puffergröße (=übliche Dateigröße) an
    FUNCTION  isGoodEnough(modeIdx:byte; newEntry:hallOfFameEntry):boolean;
    PROCEDURE addHOFEntry(modeIdx:byte; newEntry:hallOfFameEntry);
  end;

  { TSudokuMainForm }

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
  config        : T_config;
  quadSize,x0,y0,selectX,selectY:longint;
  keyboardMode  : boolean;
  configuring   : boolean;
  winnerEntry   : hallOfFameEntry;

IMPLEMENTATION
{$ifdef debugMode}
PROCEDURE writeMyState;
  begin
    writeln('quadSize              =',quadSize              );
    writeln('x0                    =',x0                    );
    writeln('y0                    =',y0                    );
    writeln('selectX               =',selectX               );
    writeln('selectY               =',selectY               );
    writeln('keyboardMode          =',keyboardMode          );
    writeln('configuring           =',configuring           );
    writeln('winnerEntry.name      =',winnerEntry.name      );
    writeln('winnerEntry.time      =',winnerEntry.time      );
    writeln('winnerEntry.given     =',winnerEntry.given     );
    writeln('winnerEntry.markErrors=',winnerEntry.markErrors);
    writeln('config.gameIsDone     =',config.gameIsDone);
    writeln;
  end;
{$endif}

FUNCTION intTime:longint;
  VAR day,hour,minute,second,deca:word;
  begin
    GetDate(hour,minute,day,deca);
    getTime(hour,minute,second,deca);
    result:=deca+100*(second+60*(minute+60*(hour+24*day)));
  end;

FUNCTION isBetterThan(size:byte; e1,e2:hallOfFameEntry):boolean;
  begin
    if e1.markErrors then begin
      if e2.markErrors then begin
        result:=(e1.time+6000)/(1+sqr(size)-e1.given)<
                (e2.time+6000)/(1+sqr(size)-e2.given);
      end else begin
        result:=(2*e1.time+6000)/(1+sqr(size)-e1.given)<
                (  e2.time+6000)/(1+sqr(size)-e2.given);
      end;
    end else begin
      if e2.markErrors then begin
        result:=(  e1.time+6000)/(1+sqr(size)-e1.given)<
                (2*e2.time+6000)/(1+sqr(size)-e2.given);
      end else begin
        result:=(e1.time+6000)/(1+sqr(size)-e1.given)<
                (e2.time+6000)/(1+sqr(size)-e2.given);
      end;
    end;
  end;

//T_sudokuRiddle:-----------------------------------------------------------------------------
PROCEDURE T_sudokuRiddle.pauseGame;
  begin
    if not(paused) then startTime:=intTime-startTime;
    paused:=true;
    renderRiddle;
  end;

PROCEDURE T_sudokuRiddle.switchPause;
  begin
    paused:=not(paused);
    startTime:=intTime-startTime;
    renderRiddle;
  end;

FUNCTION T_sudokuRiddle.isPaused:boolean;
  begin result:=paused; end;

FUNCTION T_sudokuRiddle.isSolved:boolean;
  VAR x,y:byte;
  begin
    result:=true;
    for x:=0 to fieldSize-1 do
    for y:=0 to fieldSize-1 do result:=result and
      (state[x,y].value<255) and not(state[x,y].conflicting);
  end;

FUNCTION T_sudokuRiddle.makeHOFEntry:hallOfFameEntry;
  VAR x,y:byte;
  begin
    result.time:=intTime-startTime;
    result.given:=0;
    for x:=0 to fieldSize-1 do
    for y:=0 to fieldSize-1 do
    if state[x,y].given then inc(result.given);
    result.markErrors:=config.difficulty.markErrors;
  end;

CONSTRUCTOR T_sudokuRiddle.create(size:byte);
  VAR aid:T_sudoku;
      i,j:byte;
  begin
    paused:=false;
    fieldSize:=size;
    modeIdx:=0;
    startTime:=intTime;
    while C_sudokuStructure[modeIdx].size<>size do inc(modeIdx);
    aid.create(size,config.difficulty.xSymm,
                    config.difficulty.ySymm,
                    config.difficulty.ptSymm,
                   ((75-5*config.difficulty.diff)*sqr(size)) div 100);
    for i:=0 to size-1 do
    for j:=0 to size-1 do begin
      state[i,j].value      :=aid.getSquare(i,j);
      state[i,j].given      :=(state[i,j].value<>255);
      state[i,j].conflicting:=false;
    end;
  end;

DESTRUCTOR T_sudokuRiddle.destroy;
  begin end;

PROCEDURE T_sudokuRiddle.checkConflicts;
  VAR x1,y1,x2,y2:byte;
  begin
    for x1:=0 to fieldsize-1 do
    for y1:=0 to fieldsize-1 do state[x1,y1].conflicting:=false;

    for x1:=0 to fieldsize-1 do
    for y1:=0 to fieldsize-1 do if state[x1,y1].value<>255 then
    for x2:=0 to fieldsize-1 do
    for y2:=0 to fieldsize-1 do if (state[x1,y1].value=state[x2,y2].value)
      and ((x1<>x2) or (y1<>y2)) then begin
      if (x1=x2) or //same column
         (y1=y2) or //same row
         (x1 div C_sudokuStructure[modeIdx].blocksize[0]=             // \
          x2 div C_sudokuStructure[modeIdx].blocksize[0]) and         //  \
         (y1 div C_sudokuStructure[modeIdx].blocksize[1]=             //  /same block
          y2 div C_sudokuStructure[modeIdx].blocksize[1]) then begin  // /
        state[x1,y1].conflicting:=true;
        state[x2,y2].conflicting:=true;
      end;
    end;
  end;

PROCEDURE T_sudokuRiddle.setState(x,y,value:byte; append:boolean);
  begin
    if (x    >=0) and (x<fieldSize)     and
       (y    >=0) and (y<fieldSize)     and
       not(state[x,y].given)            then begin

      if append and (state[x,y].value*10+value<=fieldSize)
        then value:=state[x,y].value*10+value;
      if (value>0) and (value<=fieldSize) then state[x,y].value:=value
                                          else state[x,y].value:=255;
    end;
    checkConflicts;
    if isSolved then begin
      configuring:=true;
      winnerEntry:=makeHOFEntry;
      if config.isGoodEnough(modeIdx,winnerEntry) then begin
        SudokuMainForm.EnterNameGroupBox.visible:=true;
        {$ifdef debugMode} writeMyState; {$endif}
      end else SudokuMainForm.LoserGroupBox.visible:=true;
    end;
  end;

PROCEDURE T_sudokuRiddle.clearState(x,y:byte);
  begin
    if (x    >=0) and (x<fieldSize)     and
       (y    >=0) and (y<fieldSize)     and
       not(state[x,y].given)            then begin
       state[x,y].value:=255;
    end;
    checkConflicts;
  end;

FUNCTION T_sudokuRiddle.givenState(x,y:byte):boolean;
  begin
    result:=(x    >=0) and (x<fieldSize) and
            (y    >=0) and (y<fieldSize) and (state[x,y].given);
  end;

PROCEDURE T_sudokuRiddle.renderRiddle;
  VAR gridTop,gridBottom:longint;

  FUNCTION interpolateColor(y:longint):longint;
    VAR r,g,b:byte;
        int1,int2:word;
    begin
      int1:=(4096*y) div (SudokuMainForm.height-19);
      int2:=4096-int1;
      r:=(int1*((config.view.bgColBottom       ) and 255)
         +int2*((config.view.bgColTop          ) and 255)) shr 12;
      g:=(int1*((config.view.bgColBottom shr  8) and 255)
         +int2*((config.view.bgColTop    shr  8) and 255)) shr 12;
      b:=(int1*((config.view.bgColBottom shr 16) and 255)
         +int2*((config.view.bgColTop    shr 16) and 255)) shr 12;
      interpolateColor:=r or g shl 8 or b shl 16;
    end;

  FUNCTION interpolateColor2(y:longint):longint;
    VAR r,g,b:byte;
        int1,int2:word;
    begin
      int1:=(4096*y) div (SudokuMainForm.height-19);
      int2:=4096-int1;
      r:=(int1*((gridBottom       ) and 255)
         +int2*((gridTop          ) and 255)) shr 12;
      g:=(int1*((gridBottom shr  8) and 255)
         +int2*((gridTop    shr  8) and 255)) shr 12;
      b:=(int1*((gridBottom shr 16) and 255)
         +int2*((gridTop    shr 16) and 255)) shr 12;
      interpolateColor2:=r or g shl 8 or b shl 16;
    end;

  PROCEDURE vLine(x,y0,y1:longint);
    VAR y:longint;
    begin
      for y:=y0 to y1 do SudokuMainForm.MainImage.Canvas.Pixels[x,y]:=interpolateColor2(y);
    end;

  CONST sudokuChar:array [0..6] of char=('S','U','D','O','K','U',' ');

  VAR x,y:longint;
      txt:string;
  begin
    gridTop:=(((((config.view.bgColTop       ) and 255)+((config.view.gridCol       ) and 255)) shr 1)       ) or
             (((((config.view.bgColTop shr  8) and 255)+((config.view.gridCol shr  8) and 255)) shr 1) shl  8) or
             (((((config.view.bgColTop shr 16) and 255)+((config.view.gridCol shr 16) and 255)) shr 1) shl 16);
    gridBottom:=(((((config.view.bgColBottom       ) and 255)+((config.view.gridCol       ) and 255)) shr 1)       ) or
                (((((config.view.bgColBottom shr  8) and 255)+((config.view.gridCol shr  8) and 255)) shr 1) shl  8) or
                (((((config.view.bgColBottom shr 16) and 255)+((config.view.gridCol shr 16) and 255)) shr 1) shl 16);

    quadSize:=10;
    while  (quadSize*fieldSize*1.1<SudokuMainForm.width    )
       and (quadSize*fieldSize*1.1<SudokuMainForm.height-19) do inc(quadSize);
    y0:=(SudokuMainForm.height-19-fieldSize*quadSize) shr 1;
    x0:=(SudokuMainForm.width    -fieldSize*quadSize) shr 1;
    for y:=0 to SudokuMainForm.height-19 do begin
      SudokuMainForm.MainImage.Canvas.Pen.color:=interpolateColor(y);
      SudokuMainForm.MainImage.Canvas.line(0,y,SudokuMainForm.width,y);
    end;

    for x:=0 to fieldSize do begin
      if x mod C_sudokuStructure[modeIdx].blocksize[0]<>0 then begin
        vLine(x0+x*quadSize,y0-1,y0+fieldSize*quadSize+1);
//        SudokuMainForm.MainImage.Canvas.Line(x0+x*quadSize  ,y0-1,x0+x*quadSize  ,y0+fieldSize*quadSize+1);
      end;
      if x mod C_sudokuStructure[modeIdx].blocksize[1]<>0 then begin
        SudokuMainForm.MainImage.Canvas.Pen.color:=interpolateColor2(y0+x*quadSize);
        SudokuMainForm.MainImage.Canvas.line(x0-1,y0+x*quadSize,  x0+fieldSize*quadSize+1,y0+x*quadSize  );
      end;
    end;

    SudokuMainForm.MainImage.Canvas.Pen.color:=config.view.gridCol;
    for x:=0 to fieldSize do begin
      if x mod C_sudokuStructure[modeIdx].blocksize[0]=0 then begin
        SudokuMainForm.MainImage.Canvas.line(x0+x*quadSize-1,y0-1,x0+x*quadSize-1,y0+fieldSize*quadSize+1);
        SudokuMainForm.MainImage.Canvas.line(x0+x*quadSize+1,y0-1,x0+x*quadSize+1,y0+fieldSize*quadSize+1);
      end;
      if x mod C_sudokuStructure[modeIdx].blocksize[1]=0 then begin
        SudokuMainForm.MainImage.Canvas.line(x0-1,y0+x*quadSize-1,x0+fieldSize*quadSize+1,y0+x*quadSize-1);
        SudokuMainForm.MainImage.Canvas.line(x0-1,y0+x*quadSize+1,x0+fieldSize*quadSize+1,y0+x*quadSize+1);
      end;
    end;
    if keyboardMode then begin
      SudokuMainForm.MainImage.Canvas.MoveTo(x0+selectX*quadSize+3,
                                             y0+selecty*quadSize+3);
      SudokuMainForm.MainImage.Canvas.LineTo(x0+selectX*quadSize+quadSize-3,
                                             y0+selecty*quadSize+3);
      SudokuMainForm.MainImage.Canvas.LineTo(x0+selectX*quadSize+quadSize-3,
                                             y0+selecty*quadSize+quadSize-3);
      SudokuMainForm.MainImage.Canvas.LineTo(x0+selectX*quadSize+3,
                                             y0+selecty*quadSize+quadSize-3);
      SudokuMainForm.MainImage.Canvas.LineTo(x0+selectX*quadSize+3,
                                             y0+selecty*quadSize+3);
    end;

    SudokuMainForm.MainImage.Canvas.Brush.style:=bsClear;
    SudokuMainForm.MainImage.Canvas.Font.size:=round(quadSize*0.9);
    SudokuMainForm.MainImage.Canvas.Font.height:=round(quadSize*0.9);
    SudokuMainForm.MainImage.Canvas.Font.name  :=config.Font.name;
    SudokuMainForm.MainImage.Canvas.Font.color:=config.view.givenCol;
    if config.Font.bold and config.Font.italic then SudokuMainForm.MainImage.Canvas.Font.style:=[fsBold,fsItalic]
    else if config.Font.bold                   then SudokuMainForm.MainImage.Canvas.Font.style:=[fsBold]
    else if config.Font.italic                 then SudokuMainForm.MainImage.Canvas.Font.style:=[fsItalic]
                                               else SudokuMainForm.MainImage.Canvas.Font.style:=[];
    if paused then for x:=0 to fieldSize-1 do
    for y:=0 to fieldSize-1 do begin
      txt:=sudokuChar[(x+y*fieldSize) mod 7];
      SudokuMainForm.MainImage.Canvas.textOut(x0+x*quadSize+(quadSize shr 1-SudokuMainForm.MainImage.Canvas.textWidth(txt) shr 1),
                            y0+y*quadSize,txt);
    end else for x:=0 to fieldSize-1 do
    for y:=0 to fieldSize-1 do if (state[x,y].value<255) then  begin
      if state[x,y].given
        then SudokuMainForm.MainImage.Canvas.Font.color:=config.view.givenCol
      else if state[x,y].conflicting and config.difficulty.markErrors
        then SudokuMainForm.MainImage.Canvas.Font.color:=config.view.confCol
        else SudokuMainForm.MainImage.Canvas.Font.color:=config.view.neutralCol;
      txt:=intToString(state[x,y].value);
      SudokuMainForm.MainImage.Canvas.textOut(x0+x*quadSize+(quadSize shr 1-SudokuMainForm.MainImage.Canvas.textWidth(txt) shr 1),
                            y0+y*quadSize,txt);
    end;
  end;

FUNCTION T_sudokuRiddle.loadFromFile(VAR F:bufferedFile):boolean;
//liest die Inhalte des Objektes aus einer bereits geöffneten Datei und gibt true zurück gdw. kein Fehler auftrat
  VAR i,j:byte;
  begin
    fieldsize:=f.readByte; result:=fieldSize in [4,6,8,9,12,15,16];
    modeIdx  :=f.readByte; result:=result and
                                  (modeIdx  in [0..6]) and
                                  (C_sudokuStructure[modeIdx].size=fieldSize);
    startTime:=intTime-f.readLongint;
    for i:=0 to fieldSize-1 do
    for j:=0 to fieldSize-1 do with state[i,j] do begin
      given:=f.readBoolean;
      value:=f.readByte;
    end;
    checkConflicts;
  end;

PROCEDURE T_sudokuRiddle.saveToFile(VAR F:bufferedFile);
//schreibt die Inhalte des Objektes in eine bereits geöffnete Datei
  VAR i,j:byte;
  begin
    f.writeByte(fieldsize);
    f.writeByte(modeIdx);
    f.writeLongint(intTime-startTime);
    for i:=0 to fieldSize-1 do
    for j:=0 to fieldSize-1 do with state[i,j] do begin
      f.writeBoolean(given);
      f.writeByte   (value);
    end;
  end;

FUNCTION T_sudokuRiddle.defaultFilesize:longint;
//gibt die Puffergröße (=übliche Dateigröße) an
  begin
    result:=512;
  end;

//-----------------------------------------------------------------------------:T_sudokuRiddle
//T_config:-----------------------------------------------------------------------------------
CONSTRUCTOR T_config.create;
  VAR i,j:byte;
  begin
    if not(loadFromFile('sudoku3.cfg')) then begin
      with view do begin
        bgColTop   :=0;
        bgColBottom:=2097152;
        gridCol    :=11184810;
        givenCol   :=11184810;
        neutralCol :=13158600;
        confCol    :=255;
      end;
      with Font do begin
        name:='default';
        bold:=false;
        italic:=false;
      end;
      with difficulty do begin
        markErrors:=true;
        xSymm :=true;
        ySymm :=true;
        ptSymm:=true;
        diff  :=5;
      end;
      for i:=0 to 6 do begin
        for j:=0 to 19 do with hallOfFame[i,j] do begin
          name:='';
          time:=maxLongint-7019+j;
          given:=sqr(C_sudokuStructure[i].size);
          markErrors:=true;
        end;
        hallOfFame[i,0].name:='Sudoku 3';
        hallOfFame[i,1].name:='von';
        hallOfFame[i,2].name:='Martin Schlegel';
        hallOfFame[i,3].name:='26.01.2008 - 27.01.2008';
        hallOfFame[i,4].name:='erstellt mit';
        hallOfFame[i,5].name:='Lazarus v0.9.22 Beta';
      end;
      riddle.create(9);
      gameIsDone:=false;
    end;
  end;

DESTRUCTOR  T_config.destroy;
  begin
    saveToFile('sudoku3.cfg');
  end;

FUNCTION  T_config.loadFromFile(VAR F:bufferedFile):boolean;
  VAR i,j:byte;
//liest die Inhalte des Objektes aus einer bereits geöffneten Datei und gibt true zurück gdw. kein Fehler auftrat
  begin
    with view do begin
      bgColTop   :=f.readLongint; result:=(bgColTop>=0) and (bgColTop<16777216);
      bgColBottom:=f.readLongint; result:=result and (bgColBottom>=0) and (bgColBottom<16777216);
      gridCol    :=f.readLongint; result:=result and (gridCol    >=0) and (gridCol    <16777216);
      givenCol   :=f.readLongint; result:=result and (givenCol   >=0) and (givenCol   <16777216);
      neutralCol :=f.readLongint; result:=result and (neutralCol >=0) and (neutralCol <16777216);
      confCol    :=f.readLongint; result:=result and (confCol    >=0) and (confCol    <16777216);
    end;
    with Font do begin
      name  :=f.readString;
      bold  :=f.readBoolean;
      italic:=f.readBoolean;
    end;
    with difficulty do begin
      markErrors:=f.readBoolean;
      xSymm     :=f.readBoolean;
      ySymm     :=f.readBoolean;
      ptSymm    :=f.readBoolean;
      diff      :=f.readByte;
    end;
    for i:=0 to 6 do for j:=0 to 19 do with hallOfFame[i,j] do begin
      name:=f.readString;
      time:=f.readLongint; result:=result and (time>0);
      given:=f.readWord;   result:=result and (given>0) and (given<=sqr(C_sudokuStructure[i].size));
      markErrors:=f.readBoolean;
    end;
    riddle.create(4);
    result:=result and riddle.loadFromFile(f);
    gameIsDone:=f.readBoolean;
  end;

PROCEDURE T_config.saveToFile(VAR F:bufferedFile);
  VAR i,j:byte;
//schreibt die Inhalte des Objektes in eine bereits geöffnete Datei
  begin
    with view do begin
      f.writeLongint(bgColTop   );
      f.writeLongint(bgColBottom);
      f.writeLongint(gridCol    );
      f.writeLongint(givenCol   );
      f.writeLongint(neutralCol );
      f.writeLongint(confCol    );
    end;
    with Font do begin
      f.writeString(name);
      f.writeBoolean(bold);
      f.writeBoolean(italic);
    end;
    with difficulty do begin
      f.writeBoolean(markErrors);
      f.writeBoolean(xSymm     );
      f.writeBoolean(ySymm     );
      f.writeBoolean(ptSymm    );
      f.writeByte   (diff      );
    end;
    for i:=0 to 6 do for j:=0 to 19 do with hallOfFame[i,j] do begin
      f.writeString (name);
      f.writeLongint(time);
      f.writeWord   (given);
      f.writeBoolean(markErrors);
    end;
    riddle.saveToFile(f);
    f.writeBoolean(gameIsDone);
  end;

FUNCTION  T_config.defaultFilesize:longint;
//gibt die Puffergröße (=übliche Dateigröße) an
  begin
    result:=2048;
  end;

FUNCTION  T_config.isGoodEnough(modeIdx:byte; newEntry:hallOfFameEntry):boolean;
  begin
    result:=isBetterThan(C_sudokuStructure[modeIdx].size,
                         newEntry,
                         hallOfFame[modeIdx,19]);
  end;

PROCEDURE T_config.addHOFEntry(modeIdx:byte; newEntry:hallOfFameEntry);
  VAR tmp:hallOfFameEntry;
      i  :byte;
  begin
    i:=19;
    hallOfFame[modeIdx,19]:=newEntry;
    while (i>0) and isBetterThan(C_sudokuStructure[modeIdx].size,
                      hallOfFame[modeIdx,i],
                      hallOfFame[modeIdx,i-1]) do begin
      tmp:=hallOfFame[modeIdx,i];
      hallOfFame[modeIdx,i]:=hallOfFame[modeIdx,i-1];
      hallOfFame[modeIdx,i-1]:=tmp;
      dec(i);
    end;
  end;
//-----------------------------------------------------------------------------------:T_config

{ TSudokuMainForm }

PROCEDURE TSudokuMainForm.FormShow(Sender: TObject);
begin
  DoubleBuffered:=true;
  configuring:=false;
  initButtonPanel(config.riddle.fieldSize);
end;

PROCEDURE TSudokuMainForm.MainImageMouseDown(Sender: TObject;
  button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if not(configuring) and not(config.riddle.isPaused) then begin
    keyboardMode:=false;
    if  (x>x0) and (x<x0+config.riddle.fieldSize*quadSize)
    and (y>y0) and (y<y0+config.riddle.fieldSize*quadSize) then begin
      selectX:=(x-x0) div quadSize;
      selectY:=(y-y0) div quadSize;
      if (button=mbRight) then begin
        config.riddle.clearState(selectX,selectY);
        config.riddle.renderRiddle;
        NumPanel.visible:=false;
      end else if (SelectX<0) or (selectX>=config.riddle.fieldSize)
               or (SelectY<0) or (selectY>=config.riddle.fieldSize) then NumPanel.visible:=false
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
    config.addHOFEntry(config.riddle.modeIdx,winnerEntry);
    EnterNameGroupBox.visible:=false;
    showHOF(config.riddle.modeIdx);
  end else begin
    EnterNameGroupBox.visible:=false;
    showHOF(config.riddle.modeIdx);
  end;
  config.riddle.create(C_sudokuStructure[config.riddle.modeIdx].size);
  config.gameIsDone:=false;
  config.riddle.pauseGame;
end;

PROCEDURE TSudokuMainForm.NumButtonClick(Sender: TObject);
begin
  config.riddle.setState(selectX,selectY,stringToInt(TButton(Sender).caption),false);
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
        38    : if selectY>0                         then dec(selectY);
        40    : if selectY<config.riddle.fieldSize-1 then inc(selectY);
        37    : if selectX>0                         then dec(selectX);
        39    : if selectX<config.riddle.fieldSize-1 then inc(selectX);
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
  config.riddle.create(C_sudokuStructure[config.riddle.modeIdx].size);
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
    numberOfRiddles:=stringToInt(ExportNumberEdit.text);
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
  num:=StringToInt(ExportNumberEdit.text);
  if num<1 then num:=1 else if num>200 then num:=200;
  ExportNumberEdit.text:=intToString(num);
end;

PROCEDURE TSudokuMainForm.FormCreate(Sender: TObject);
begin
  MainImage.height:=screen.height;
  MainImage.width:=screen.width;
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
      h,m,s,d:longint;
  begin
    configuring:=true;
    config.riddle.pauseGame;
    HOFGroupBox.caption:='Bestenliste '+intToString(C_sudokuStructure[modeIdx].size)+'x'
                                       +intToString(C_sudokuStructure[modeIdx].size);
    for i:=0 to 19 do begin
      HOFStringGrid.Cells[1,i+1]:=config.hallOfFame[modeIdx,i].name;
      HOFStringGrid.Cells[2,i+1]:=intToString(config.hallOfFame[modeIdx,i].given);
      h:=config.hallOfFame[modeIdx,i].time;
      d:=h mod 100; h:=h div 100;
      s:=h mod  60; h:=h div  60;
      m:=h mod  60; h:=h div  60;
      HOFStringGrid.Cells[3,i+1]:=timeToString(h,m,s,d,2);
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

