UNIT sudoku;
INTERFACE
USES serializationUtil;
CONST
  C_sudokuStructure:array [0..6] of record
                                      size     :byte;
                                      blocksize:array[0..1] of byte;
                                      any      :word;
                                    end=
  ((size: 4; blocksize:(2,2); any:   15),
   (size: 6; blocksize:(2,3); any:   63),
   (size: 8; blocksize:(2,4); any:  255),
   (size: 9; blocksize:(3,3); any:  511),
   (size:12; blocksize:(3,4); any: 4095),
   (size:15; blocksize:(3,5); any:32767),
   (size:16; blocksize:(4,4); any:65535));

  C_LaTeX_fileHeader:array[0..5] of string=('\documentclass[12pt,a4paper]{report} \usepackage{graphics}' ,
                                            '\oddsidemargin 0in \evensidemargin 0in \topmargin 0in \textheight 23cm \textwidth 16cm',
                                            '\renewcommand\arraystretch{1.4}',
                                            '\setlength{\lineskip}{2.0ex plus0.5ex minus0.5ex}',
                                            '\setlength{\doublerulesep}{0.2mm}',
                                            '\begin{document} \begin{center}');
  C_LaTeX_fileFooter:string='\end{center}\end{document}';

TYPE
  T_sudokuState=(solved,unknown,unsolveable);

  FT_output=PROCEDURE(txt:string);

  { T_sudoku }

  T_sudoku=object(T_serializable)
    private
      el:array of word;
      fieldSize,structIdx:byte;
      FUNCTION fullSolve(fillRandom:boolean):T_sudokuState;
    public
      CONSTRUCTOR createUnfilled(size:byte);
      CONSTRUCTOR createFull    (size:byte);
      CONSTRUCTOR create(size:byte; symm_x,symm_y,symm_center:boolean; difficulty:word);
      FUNCTION    getSquare(x,y:byte):byte;
      FUNCTION    given:word;
      DESTRUCTOR  destroy;
      PROCEDURE   solve;
      PROCEDURE   writeTxtForm  (writeOut,writelnOut:FT_output);
      PROCEDURE   writeLaTeXForm(writeOut,writelnOut:FT_output; enumString:string; small:boolean);
      FUNCTION getSerialVersion:dword; virtual;
      FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
      PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  end;

PROCEDURE writeLatexHeader(writelnOut:FT_output);
IMPLEMENTATION
CONST C_bit:array[0..15] of word=(1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768);

PROCEDURE writeLatexHeader(writelnOut:FT_output);
  VAR i:byte;
  begin for i:=0 to 5 do writelnOut(C_Latex_fileHeader[i]); end;

FUNCTION T_sudoku.fullSolve(fillRandom: boolean): T_sudokuState;
  VAR i0,j0,i1,j1:byte;
      k,excludor:word;
      done:array of boolean;
      progress,totalProgress:boolean;

  FUNCTION determined(VAR v:word):boolean; inline;
    begin
      determined:=(v=C_bit[ 0]) or (v=C_bit[ 1]) or (v=C_bit[ 2]) or (v=C_bit[ 3]) or
                  (v=C_bit[ 4]) or (v=C_bit[ 5]) or (v=C_bit[ 6]) or (v=C_bit[ 7]) or
                  (v=C_bit[ 8]) or (v=C_bit[ 9]) or (v=C_bit[10]) or (v=C_bit[11]) or
                  (v=C_bit[12]) or (v=C_bit[13]) or (v=C_bit[14]) or (v=C_bit[15]);
           {      (v=    1) or (v=    2) or (v=    4) or (v=    8) or
                  (v=   16) or (v=   32) or (v=   64) or (v=  128) or
                  (v=  256) or (v=  512) or (v= 1024) or (v= 2048) or
                  (v= 4096) or (v= 8192) or (v=16384) or (v=32768);}
    end;

  PROCEDURE exclude(idx:word); inline;
    begin el[idx]:=el[idx] and excludor; end;

  begin
    setLength(done,length(el));
    for k:=0 to length(done)-1 do done[k]:=false;
    //---------------------------------------------------//
    //primary randomization                              //
    //                                                   //
    if fillRandom then for i0:=0 to fieldSize-1 do begin //
      repeat k:=random(fieldSize)                        //
      until el[k]=C_sudokuStructure[structIdx].any;      //
      el[k]:=C_bit[i0];                                  //
    end;                                                 //
    //                                                   //
    //primary randomization                              //
    //---------------------------------------------------//
    repeat
      totalProgress:=false;
      //exclude loop:--------------------------------------------------------------------------------------------------------//
      repeat                                                                                                                 //
        progress:=false;                                                                                                     //
        for k:=0 to length(el)-1 do if not(done[k]) then begin                                                               //
          if determined(el[k]) then begin                                                                                    //
            //exclude step:----------------------------------------------------------------------------------------------//  //
            //If a field is determined, its number may not occur in the same row, column or block.                       //  //
            excludor:=not(el[k]);                                                                                        //  //
            i0:=k mod fieldSize;                                                                                         //  //
            j0:=k div fieldSize;                                                                                         //  //
            //exclude value in row:                                                                                      //  //
            for i1:=0 to fieldSize-1 do if i1<>i0 then exclude(j0*fieldSize+i1);                                         //  //
            //exclude value in column:                                                                                   //  //
            for j1:=0 to fieldsize-1 do if j1<>j0 then exclude(j1*fieldSize+i0);                                         //  //
            //exclude value in block:                                                                                    //  //
            for i1:=(i0 div C_sudokuStructure[structIdx].blockSize[0])   *C_sudokuStructure[structIdx].blockSize[0]   to //  //
                   ((i0 div C_sudokuStructure[structIdx].blockSize[0])+1)*C_sudokuStructure[structIdx].blockSize[0]-1 do //  //
            for j1:=(j0 div C_sudokuStructure[structIdx].blockSize[1])   *C_sudokuStructure[structIdx].blockSize[1]   to //  //
                   ((j0 div C_sudokuStructure[structIdx].blockSize[1])+1)*C_sudokuStructure[structIdx].blockSize[1]-1 do //  //
            if (i1<>i0) or (j1<>j0) then exclude(j1*fieldSize+i1);                                                       //  //
            done[k]      :=true;                                                                                         //  //
            progress     :=true;                                                                                         //  //
            totalProgress:=true;                                                                                         //  //
            //------------------------------------------------------------------------------------------------:exclude step  //
          end;                                                                                                               //
        end;                                                                                                                 //
      until not(progress);                                                                                                   //
      //----------------------------------------------------------------------------------------------------------:exclude loop
      progress:=false;
      for k:=0 to length(el)-1 do if not(done[k]) and not(determined(el[k])) then begin
        //---------------------------------------------------------------------------------------------------------------//
        //include step                                                                                                   //
        //                                                                                                               //
        i0:=k mod fieldSize;                                                                                             //
        j0:=k div fieldSize;                                                                                             //
        excludor:=0;                                                                                                     //
        //include value in row:                                                                                          //
        for i1:=0 to fieldSize-1 do if i1<>i0 then excludor:=excludor or el[j0*fieldSize+i1];                            //
        excludor:=not(excludor) and el[k];                                                                               //
        if determined(excludor) then begin                                                                               //
          el[k]        :=excludor;                                                                                       //
          totalProgress:=true;                                                                                           //
        end else begin                                                                                                   //
          excludor:=0;                                                                                                   //
          //exclude value in column:                                                                                     //
          for j1:=0 to fieldsize-1 do if j1<>j0 then excludor:=excludor or el[j1*fieldSize+i0];                          //
          if determined(excludor) then begin                                                                             //
            el[k]        :=excludor;                                                                                     //
            totalProgress:=true;                                                                                         //
          end else begin                                                                                                 //
            //exclude value in block:                                                                                    //
            for i1:=(i0 div C_sudokuStructure[structIdx].blockSize[0])   *C_sudokuStructure[structIdx].blockSize[0]   to //
                   ((i0 div C_sudokuStructure[structIdx].blockSize[0])+1)*C_sudokuStructure[structIdx].blockSize[0]-1 do //
            for j1:=(j0 div C_sudokuStructure[structIdx].blockSize[1])   *C_sudokuStructure[structIdx].blockSize[1]   to //
                   ((j0 div C_sudokuStructure[structIdx].blockSize[1])+1)*C_sudokuStructure[structIdx].blockSize[1]-1 do //
            if (i1<>i0) or (j1<>j0) then excludor:=excludor or el[j1*fieldSize+i1];                                      //
            if determined(excludor) then begin                                                                           //
              el[k]        :=excludor;                                                                                   //
              totalProgress:=true;                                                                                       //
            end;                                                                                                         //
          end;                                                                                                           //
        end;                                                                                                             //
        //include step                                                                                                   //
        //---------------------------------------------------------------------------------------------------------------//
      end;

      //----------------------------------------------------------//
      //randomize step                                            //
      //                                                          //
      if not(totalProgress) and fillRandom then begin             //
        k:=0;                                                     //
        if not (totalProgress) then repeat                        //
          if (el[k]<>0) and not(determined(el[k])) then begin     //
            repeat i0:=random(fieldSize);                         //
            until (C_bit[i0] and el[k])>0;                        //
            el[k]:=C_bit[i0];                                     //
            totalProgress:=true;                                  //
          end else inc(k);                                        //
        until totalProgress or (k=length(el));                    //
      end;                                                        //
      //                                                          //
      //randomize step                                            //
      //----------------------------------------------------------//
    until not(totalProgress);

    //determine value to return
    result:=solved;
    k:=0;
    repeat
      if el[k]=0 then result:=unsolveable
      else if (result=solved) and not(determined(el[k])) then result:=unknown;
      inc(k);
    until (k=length(el)) or (result=unsolveable);
    setLength(done,0);
  end; //fullSolve

CONSTRUCTOR T_sudoku.createUnfilled(size: byte);
  VAR k:word;
  begin
    //correct size
    if size< 4 then size:= 4;
    if size>16 then size:=16;
    //lookup structure index matching size
    structIdx:=0;
    while (structIdx<7) and (C_sudokuStructure[structIdx].size<size) do inc(structIdx);
    //if unknown size was entered, select size=9x9 <-> structIdx=3
    if structIdx>=7 then structIdx:=3;
    //determine actual field size
    fieldSize:=C_sudokuStructure[structIdx].size;
    //allocate cells array
    setLength(el,fieldSize*fieldSize);
    //set all cells to unknown value
    for k:=0 to length(el)-1 do el[k]:=C_sudokuStructure[structIdx].any;
  end;

CONSTRUCTOR T_sudoku.createFull(size: byte);
  begin
    repeat createUnfilled(size); until fullSolve(true)=solved;
  end;

CONSTRUCTOR T_sudoku.create(size: byte; symm_x, symm_y, symm_center: boolean;
  difficulty: word);
  VAR copy:array of word;
      k:word;
      undefTries,outerUndefTries:word;
      undefList:array of word;
      lastUndef:longint;

  FUNCTION inUndefList(w:word):boolean;
    VAR i:longint;
    begin
      i:=0;
      result:=false;
      while (i<length(undefList)) and not(result) do begin
        result:=undefList[i]=w;
        inc(i);
      end;
    end;

  PROCEDURE undefine(i,j:byte; sx,sy,sc:boolean);
    begin
      if          sx then begin undefine(fieldSize-1-i,            j,false,sy   ,sc   );
                                undefine(          i,              j,false,sy   ,sc   );
      end else if sy then begin undefine(          i,fieldSize-1-j,false,false,sc   );
                                undefine(          i,              j,false,false,sc   );
      end else if sc then begin undefine(fieldSize-1-i,fieldSize-1-j,false,false,false);
                                undefine(          i,              j,false,false,false);
      end else if not(inUndefList(j*fieldSize+i)) then begin
        setLength(undefList,length(undefList)+1);
        undefList[length(undefList)-1]:=j*fieldSize+i;
      end;
    end;

  PROCEDURE applyUndefList;
    VAR i:longint;
    begin
      for i:=0 to length(el)-1 do el[i]:=copy[i];
      for i:=0 to length(undefList)-1 do el[undefList[i]]:=C_sudokuStructure[structIdx].any;
    end;

  begin
    //create solved field
    createFull(size);
    //create copy of solved riddle
    setLength(copy,length(el)); for k:=0 to length(el)-1 do copy[k]:=el[k];
    outerUndefTries:=0;
    repeat
      setLength(undefList,0);
      undefTries:=0;
      repeat
        lastUndef:=length(undefList);
        undefine(random(fieldSize),random(fieldSize),symm_x,symm_y,symm_center);
        applyUndefList;
        if (fullSolve(false)<>solved) then begin
          setLength(undefList,lastUndef);
          inc(undefTries);
        end;
      until (length(undefList)>=difficulty) or (undefTries>40);
      inc(outerUndefTries);
    until (length(undefList)>=difficulty) or (outerUndefTries>40);
    applyUndefList;
    setLength(undefList,0);
    setLength(copy,0);
  end;

FUNCTION T_sudoku.getSquare(x, y: byte): byte;
  begin
    if (x>=0) and (x<fieldsize) and
       (y>=0) and (y<fieldsize) then begin
      x:=y*fieldSize+x;
      result:=0;
      while (result<fieldSize) and (el[x]<>C_bit[result]) do inc(result);
      if result>=fieldsize then result:=255
                           else inc(result);
    end else result:=255;
  end;

FUNCTION T_sudoku.given: word;
  VAR i,j:word;
  begin
    result:=0;
    for i:=0 to length(el)-1 do
    for j:=0 to fieldSize-1 do if (el[i]=C_Bit[j]) then inc(result);
  end;

DESTRUCTOR T_sudoku.destroy;
  begin setLength(el,0); end;

PROCEDURE T_sudoku.solve;
  begin fullSolve(false); end;

PROCEDURE T_sudoku.writeTxtForm(writeOut, writelnOut: FT_output);
  FUNCTION numString(w:word):string;
    VAR i:byte;
    begin
      i:=0;
      while (i<16) and ((1 shl i)<>w) do inc(i);
      case i of
        0: result:=' 1 ';
        1: result:=' 2 ';
        2: result:=' 3 ';
        3: result:=' 4 ';
        4: result:=' 5 ';
        5: result:=' 6 ';
        6: result:=' 7 ';
        7: result:=' 8 ';
        8: result:=' 9 ';
        9: result:=' 10';
       10: result:=' 11';
       11: result:=' 12';
       12: result:=' 13';
       13: result:=' 14';
       14: result:=' 15';
       15: result:=' 16';
      else result:='   ';
      end;
    end;

  CONST C_filler:array[0..6,0..1] of string=
       (('#===#===#===#===#'                                                ,'#---+---#---+---#'                                                ),  // 4
        ('#===#===#===#===#===#===#'                                        ,'#---+---#---+---#---+---#'                                        ),  // 6
        ('#===#===#===#===#===#===#===#===#'                                ,'#---+---#---+---#---+---#---+---#'                                ),  // 8
        ('#===#===#===#===#===#===#===#===#===#'                            ,'#---+---+---#---+---+---#---+---+---#'                            ),  // 9
        ('#===#===#===#===#===#===#===#===#===#===#===#===#'                ,'#---+---+---#---+---+---#---+---+---#---+---+---#'                ),  //12
        ('#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#'    ,'#---+---+---#---+---+---#---+---+---#---+---+---#---+---+---#'    ),  //15
        ('#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#===#','#---+---+---+---#---+---+---+---#---+---+---+---#---+---+---+---#')); //16
        C_columnSep:array[0..6,0..16] of char=
        (('#','|','#','|','#',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '),   // 4
         ('#','|','#','|','#','|','#',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '),   // 6
         ('#','|','#','|','#','|','#','|','#',' ',' ',' ',' ',' ',' ',' ',' '),   // 8
         ('#','|','|','#','|','|','#','|','|','#',' ',' ',' ',' ',' ',' ',' '),   // 9
         ('#','|','|','#','|','|','#','|','|','#','|','|','#',' ',' ',' ',' '),   //12
         ('#','|','|','#','|','|','#','|','|','#','|','|','#','|','|','#',' '),   //15
         ('#','|','|','|','#','|','|','|','#','|','|','|','#','|','|','|','#'));  //16

  VAR i,j:byte;
      k  :word;
  begin
    writelnOut(C_filler[structIdx,0]);        //Kopfzeile
    k:=0;
    for j:=0 to fieldSize-1 do begin          //für jede Zeile...
      for i:=0 to fieldSize-1 do begin          //für jede Zelle...
        writeOut(C_columnSep[structIdx,i]);       //Spaltentrenner
        writeOut(numString(el[k]));               //Zellenwert
        inc(k);
      end;
      writelnOut(C_columnSep[structIdx,fieldSize]);              //abschließender Spaltentrenner
      if (j+1) mod (C_sudokuStructure[structIdx].blockSize[1])=0 //Trennzeile
        then writelnOut(C_filler[structIdx,0])
        else writelnOut(C_filler[structIdx,1]);
    end;
    writelnOut(' '); //abschließende Leerzeile
  end;

PROCEDURE T_sudoku.writeLaTeXForm(writeOut, writelnOut: FT_output;
  enumString: string; small: boolean);
  FUNCTION numString(w:word):string;
    VAR i:byte;
    begin
      i:=0;
      while (i<16) and ((1 shl i)<>w) do inc(i);
      case i of
        0: result:=' 1 ';
        1: result:=' 2 ';
        2: result:=' 3 ';
        3: result:=' 4 ';
        4: result:=' 5 ';
        5: result:=' 6 ';
        6: result:=' 7 ';
        7: result:=' 8 ';
        8: result:=' 9 ';
        9: result:=' 10';
       10: result:=' 11';
       11: result:=' 12';
       12: result:=' 13';
       13: result:=' 14';
       14: result:=' 15';
       15: result:=' 16';
      else result:='~~~';
      end;
    end;

  CONST C_header:array[0..6] of string=
       (('\begin{tabular}{||c|c||c|c||} \hline \hline'                          ),  // 4
        ('\begin{tabular}{||c|c||c|c||c|c||} \hline \hline'                     ),  // 6
        ('\begin{tabular}{||c|c||c|c||c|c||c|c||} \hline \hline'                ),  // 8
        ('\begin{tabular}{||c|c|c||c|c|c||c|c|c||} \hline \hline'               ),  // 9
        ('\begin{tabular}{||c|c|c||c|c|c||c|c|c||c|c|c||} \hline \hline'        ),  //12
        ('\begin{tabular}{||c|c|c||c|c|c||c|c|c||c|c|c||c|c|c||} \hline \hline' ),  //15
        ('\begin{tabular}{||c|c|c|c||c|c|c|c||c|c|c|c||c|c|c|c||} \hline \hline')); //16
  VAR i,j:byte;
      k  :word;
  begin
    if small then writelnOut('\scalebox{0.5}{');
    if enumString<>'' then writelnOut('\begin{tabular}{c}');
    writelnOut(C_header[structIdx]); //Tabellenheader
    k:=0;
    for j:=0 to fieldSize-1 do begin          //für jede Zeile...
      writeOut('  ');
      for i:=0 to fieldSize-1 do begin          //für jede Zelle...
        if i>0 then writeOut('&');              //Spaltentrenner
        writeOut(numString(el[k]));             //Zellenwert
        inc(k);
      end;
      if (j+1) mod (C_sudokuStructure[structIdx].blockSize[1])=0 //Trennzeile
        then writelnOut('\\ \hline \hline')
        else writelnOut('\\ \hline');
    end;
    if enumString<>''
      then writelnOut('\end{tabular} \\ '+enumString+' \end{tabular} ~~~ ')
      else writelnOut('\end{tabular} ~~~ ');
    if small then writelnOut('}');
  end;

FUNCTION T_sudoku.getSerialVersion: dword;
  begin
    result:=1;
  end;

FUNCTION T_sudoku.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  //liest die Inhalte des Objektes aus einer bereits geöffneten Datei und gibt true zurück gdw. kein Fehler auftrat
  VAR i:longint;
  begin
    fieldSize:=stream.readByte; result:=fieldSize in [4,6,8,9,12,15,16];
    structIdx:=stream.readByte; result:=result and
                                  (structIdx in [0..6]) and
                                  (C_sudokuStructure[structIdx].size=fieldSize);
    if result then begin
      setLength(el,sqr(fieldSize));
      for i:=0 to length(el)-1 do begin
        el[i]:=stream.readWord; result:=result and (el[i]>=0) and (el[i]<=C_sudokuStructure[structIdx].any);
      end;
    end;
  end;

PROCEDURE T_sudoku.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  //schreibt die Inhalte des Objektes in eine bereits geöffnete Datei
  VAR i:longint;
  begin
    stream.writeByte(fieldSize);
    stream.writeByte(structIdx);
    for i:=0 to length(el)-1 do stream.writeWord(el[i]);
  end;

end.
