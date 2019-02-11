PROGRAM sudoku3Project;

{$mode objfpc}{$H+}

USES
  {$ifdef UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$endif}{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, Sudoku3Surface, sudokuSettings, sudokuHighscores,
  endOfGameUnit, exportUnit;

{$R *.res}

begin
  Application.Scaled:=true;
  Application.title:='Sudoku 3';
  Application.initialize;
  Application.CreateForm(TSudokuMainForm, SudokuMainForm);
  Application.run;
end.

