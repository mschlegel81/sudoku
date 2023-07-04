UNIT sudokuSettings;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls,sudoku;

TYPE

  { ToptionsForm }

  ToptionsForm = class(TForm)
    BG1Shape: TShape;
    BG2Shape: TShape;
    Button1: TButton;
    Button3: TButton;
    ColorDialog1: TColorDialog;
    ConfShape: TShape;
    DiffGroupBox1: TGroupBox;
    DiffListBox: TListBox;
    FontDialog1: TFontDialog;
    FontLabel: TLabel;
    GivenShape: TShape;
    GridShape: TShape;
    Label7: TLabel;
    MarkErrorsCB: TCheckBox;
    NeutralShape: TShape;
    PageControl1: TPageControl;
    pSymmCB: TCheckBox;
    SetCol1Button: TButton;
    SetCol2Button: TButton;
    SetCol3Button: TButton;
    SetCol4Button: TButton;
    SetCol5Button: TButton;
    SetCol6Button: TButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ViewGroupBox: TGroupBox;
    XSymmCB: TCheckBox;
    ySymmCB: TCheckBox;
    PROCEDURE Button3Click(Sender: TObject);
    PROCEDURE SetCol1ButtonClick(Sender: TObject);
    PROCEDURE SetCol2ButtonClick(Sender: TObject);
    PROCEDURE SetCol3ButtonClick(Sender: TObject);
    PROCEDURE SetCol4ButtonClick(Sender: TObject);
    PROCEDURE SetCol5ButtonClick(Sender: TObject);
    PROCEDURE SetCol6ButtonClick(Sender: TObject);
  private

  public

  end;

PROCEDURE showOptions;
IMPLEMENTATION
VAR
  optionsForm: ToptionsForm=nil;

PROCEDURE showOptions;
  begin
    if optionsForm=nil then optionsForm:=ToptionsForm.create(nil);
    with optionsForm do begin
      MarkErrorsCB.checked  :=config.difficulty.markErrors;
      XSymmCB     .checked  :=sym_x      in config.difficulty.symmetries;
      ySymmCB     .checked  :=sym_y      in config.difficulty.symmetries;
      pSymmCB     .checked  :=sym_center in config.difficulty.symmetries;
      DiffListBox .ItemIndex:=config.difficulty.diff;

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
    end;
    optionsForm.ShowModal;
    with optionsForm do begin
      config.difficulty.markErrors:=MarkErrorsCB.checked  ;
      config.difficulty.symmetries:=[];
      if XSymmCB.checked then include(config.difficulty.symmetries,sym_x     );
      if ySymmCB.checked then include(config.difficulty.symmetries,sym_y     );
      if pSymmCB.checked then include(config.difficulty.symmetries,sym_center);
      config.difficulty.diff      :=DiffListBox .ItemIndex;
    end;
  end;

PROCEDURE ToptionsForm.SetCol1ButtonClick(Sender: TObject);
  begin
    ColorDialog1.color:=         BG1Shape.Brush.color;
    if ColorDialog1.execute then BG1Shape.Brush.color:=ColorDialog1.color;
    config.view.bgColTop   :=BG1Shape .Brush.color   ;
    config.riddle.renderRiddle;
  end;

PROCEDURE ToptionsForm.Button3Click(Sender: TObject);
  begin
    FontDialog1.Font:=FontLabel.Font;
    if FontDialog1.execute then FontLabel.Font:=FontDialog1.Font;
    config.Font.name:=FontLabel.Font.name    ;
    config.Font.bold:=fsBold in FontLabel.Font.style;
    config.Font.italic:=fsItalic in FontLabel.Font.style;
    config.riddle.renderRiddle;
  end;

PROCEDURE ToptionsForm.SetCol2ButtonClick(Sender: TObject);
  begin
    ColorDialog1.color:=         BG2Shape.Brush.color;
    if ColorDialog1.execute then BG2Shape.Brush.color:=ColorDialog1.color;
    config.view.bgColBottom:=BG2Shape .Brush.color   ;
    config.riddle.renderRiddle;
  end;

PROCEDURE ToptionsForm.SetCol3ButtonClick(Sender: TObject);
  begin
    ColorDialog1.color:=         GridShape.Brush.color;
    if ColorDialog1.execute then GridShape.Brush.color:=ColorDialog1.color;
    config.view.gridCol    :=GridShape.Brush.color   ;
    config.riddle.renderRiddle;
  end;

PROCEDURE ToptionsForm.SetCol4ButtonClick(Sender: TObject);
  begin
    ColorDialog1.color:=         GivenShape.Brush.color;
    if ColorDialog1.execute then GivenShape.Brush.color:=ColorDialog1.color;
    config.view.givenCol   :=GivenShape  .Brush.color;
    config.riddle.renderRiddle;
  end;

PROCEDURE ToptionsForm.SetCol5ButtonClick(Sender: TObject);
  begin
    ColorDialog1.color:=         NeutralShape.Brush.color;
    if ColorDialog1.execute then NeutralShape.Brush.color:=ColorDialog1.color;
    config.view.neutralCol :=NeutralShape.Brush.color;
    config.riddle.renderRiddle;
  end;

PROCEDURE ToptionsForm.SetCol6ButtonClick(Sender: TObject);
  begin
    ColorDialog1.color:=         ConfShape.Brush.color;
    if ColorDialog1.execute then ConfShape.Brush.color:=ColorDialog1.color;
    config.view.confCol    :=ConfShape   .Brush.color;
    config.riddle.renderRiddle;
  end;

INITIALIZATION
  {$I sudokuSettings.lrs}

FINALIZATION
  if optionsForm<>nil then FreeAndNil(optionsForm);

end.

