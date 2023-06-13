..\lazarus32\lazbuild.exe sudoku3Project.lpi --bm=Default
..\lazarus32\fpc\3.2.2\bin\i386-win32\delp.exe -r . 
upx.exe --best --ultra-brute -o sudoku_upx.exe sudoku.exe
move /Y sudoku_upx.exe sudoku.exe
