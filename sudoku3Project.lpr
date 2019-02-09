program sudoku3Project;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, Sudoku3Surface;

begin
  Application.Title:='Sudoku 3';
  Application.Initialize;
  Application.CreateForm(TSudokuMainForm, SudokuMainForm);
  Application.Run;
end.

