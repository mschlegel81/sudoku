PROGRAM sudoku3Project;

{$mode objfpc}{$H+}

USES
  {$ifdef UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$endif}{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, Sudoku3Surface, sudokuSettings, sudokuHighscores;

begin
  Application.title:='Sudoku 3';
  Application.initialize;
  Application.CreateForm(TSudokuMainForm, SudokuMainForm);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.run;
end.

