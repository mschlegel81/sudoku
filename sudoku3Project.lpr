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

begin
  Application.title:='Sudoku 3';
  Application.initialize;
  Application.CreateForm(TSudokuMainForm, SudokuMainForm);
  Application.run;
end.

