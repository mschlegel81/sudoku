UNIT exportUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls,sudoku;

TYPE

  { TexportForm }

  TexportForm = class(TForm)
    Button8: TButton;
    Button9: TButton;
    ExportDiff_LB: TListBox;
    ExportFT1_RB: TRadioButton;
    ExportFT2_RB: TRadioButton;
    ExportGroupBox: TGroupBox;
    ExportNumberEdit: TEdit;
    ExportProgressBar: TProgressBar;
    ExportSizeListBox: TListBox;
    ExportSolutions_CB: TCheckBox;
    ExportSP_RB1: TRadioButton;
    ExportSP_RB2: TRadioButton;
    ExportSP_RB3: TRadioButton;
    ExportSX_RB1: TRadioButton;
    ExportSX_RB2: TRadioButton;
    ExportSX_RB3: TRadioButton;
    ExportSY_RB1: TRadioButton;
    ExportSY_RB2: TRadioButton;
    ExportSY_RB3: TRadioButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SaveDialog1: TSaveDialog;
    PROCEDURE Button8Click(Sender: TObject);
    PROCEDURE ExportNumberEditEditingDone(Sender: TObject);
  private

  public

  end;

PROCEDURE showExportForm;
IMPLEMENTATION
VAR
  exportForm: TexportForm=nil;

PROCEDURE showExportForm;
  begin
    if exportForm=nil then exportForm:=TexportForm.create(nil);
    exportForm.ShowModal;
  end;

{ TexportForm }

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

PROCEDURE TexportForm.Button8Click(Sender: TObject);
  VAR s:T_sudoku;
      x:word;
      txtOut:boolean;
      riddleSize,structIndex:byte;
      valuesGiven:word;
      numberOfRiddles:word;
      symmetries:T_symmetries;
  FUNCTION enumString(x:word):string;
    begin
      if ExportSolutions_CB.checked then begin
        str(x,result);
        result:='\#'+result;
      end else result:='';
    end;

  begin
    if SaveDialog1.execute then begin
      randomize;
      txtOut:=ExportFT1_RB.checked;
      if txtOut then SaveDialog1.fileName:=ChangeFileExt(SaveDialog1.fileName,'.txt')
                else SaveDialog1.fileName:=ChangeFileExt(SaveDialog1.fileName,'.tex');
      assignFile(outFile,SaveDialog1.fileName);
      rewrite(outFile);
      structIndex:=ExportSizeListBox.ItemIndex;
      case byte(ExportSizeListBox.ItemIndex) of
        0: riddleSize:= 4;
        1: riddleSize:= 6;
        2: riddleSize:= 8;
        3: riddleSize:= 9;
        4: riddleSize:=12;
        5: riddleSize:=15;
        6: riddleSize:=16;
      end;

      numberOfRiddles:=strToInt(ExportNumberEdit.text);
      setLength(solutions,2);
      if txtOut then solutions[0]:='LÖSUNGEN:'
                else solutions[0]:='\newpage \begin{Large} L\"osungen \end{Large}';
      solutions[1]:='';
      solutionOut:=false;
      if not(txtOut) then writeLatexHeader(@writelnOut);
      ExportProgressBar.max:=numberOfRiddles;
      for x:=1 to numberOfRiddles do begin
        ExportProgressBar.position:=x;
        symmetries:=[];
        if ExportSX_RB3.checked and (random(3)=0) or ExportSX_RB1.checked then include(symmetries,sym_x);
        if ExportSY_RB3.checked and (random(3)=0) or ExportSY_RB1.checked then include(symmetries,sym_y);
        if ExportSP_RB3.checked and (random(3)=0) or ExportSP_RB1.checked then include(symmetries,sym_center);
        valuesGiven:=config.storedRiddles.validDifficulties(structIndex,config.difficulty.symmetries).minimum;
        valuesGiven:=valuesGiven+round((sqr(riddleSize)-valuesGiven)*sqr(ExportDiff_LB.ItemIndex*0.1));
        s:=config.storedRiddles.getRiddle(structIndex,symmetries,valuesGiven);
        solutionOut:=false;
        if txtOut then s.writeTxtForm  (@writeOut,@writelnOut)
                  else s.writeLaTeXForm(@writeOut,@writelnOut,enumString(x),false);
        if ExportSolutions_CB.checked then begin
          solutionOut:=true;
          s.solve;
          if txtOut then s.writeTxtForm  (@writeOut,@writelnOut)
                    else s.writeLaTeXForm(@writeOut,@writelnOut,enumString(x),true);
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
    end;
  end;

PROCEDURE TexportForm.ExportNumberEditEditingDone(Sender: TObject);
  VAR num:longint;
  begin
    num:=strToInt(ExportNumberEdit.text);
    if num<1 then num:=1 else if num>200 then num:=200;
    ExportNumberEdit.text:=intToStr(num);
  end;

INITIALIZATION
  {$I exportUnit.lrs}

FINALIZATION
  if exportForm<>nil then FreeAndNil(exportForm);
end.

