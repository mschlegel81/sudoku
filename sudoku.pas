UNIT sudoku;
INTERFACE
USES serializationUtil,sysutils,ExtCtrls,Graphics;
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
  T_sudoku=object(T_serializable)
    private
      el:array of word;
      fieldSize,structIdx:byte;
      FUNCTION fullSolve(fillRandom:boolean):T_sudokuState;
    public
      CONSTRUCTOR createUnfilled(size:byte);
      CONSTRUCTOR createFull    (CONST size:byte);
      CONSTRUCTOR create(CONST size:byte; symm_x,symm_y,symm_center:boolean; difficulty:word);
      FUNCTION    getSquare(x,y:byte):byte;
      FUNCTION    given:word;
      DESTRUCTOR  destroy;
      PROCEDURE   solve;
      PROCEDURE   writeTxtForm  (CONST writeOut,writelnOut:FT_output);
      PROCEDURE   writeLaTeXForm(CONST writeOut,writelnOut:FT_output; CONST enumString:string; CONST small:boolean);
      FUNCTION getSerialVersion:dword; virtual;
      FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
      PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  end;

  hallOfFameEntry=record
    name :string;
    time :double;
    given:word;
    markErrors:boolean;
  end;

  T_sudokuRiddle=object(T_serializable)
    private
      fieldSize:byte;
      modeIdx  :byte;
      state:array[0..15,0..15] of record
                                    given,conflicting:boolean;
                                    value:byte;
                                  end;
      startTime :double;
      paused    :boolean;
      PROCEDURE checkConflicts;
    public
      CONSTRUCTOR create;
      PROPERTY getFieldSize:byte read fieldSize;
      PROPERTY getModeIdx  :byte read modeIdx;

      PROCEDURE initGame(size:byte);
      PROCEDURE   pauseGame;
      PROCEDURE   switchPause;
      PROPERTY    isPaused:boolean read paused;
      FUNCTION    isSolved:boolean;
      FUNCTION    makeHOFEntry:hallOfFameEntry;
      PROCEDURE   setState(CONST x,y:byte; value:byte; CONST append:boolean);
      PROCEDURE   clearState(CONST x,y:byte);
      FUNCTION    givenState(CONST x,y:byte):boolean;
      PROCEDURE   renderRiddle;
      FUNCTION getSerialVersion:dword; virtual;
      FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
      PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
  end;

  T_config=object(T_serializable)
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
    FUNCTION getSerialVersion:dword; virtual;
    FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean; virtual;
    PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper); virtual;
    FUNCTION  isGoodEnough(CONST modeIdx:byte; CONST newEntry:hallOfFameEntry):boolean;
    PROCEDURE addHOFEntry(CONST modeIdx:byte; CONST newEntry:hallOfFameEntry);
  end;

TYPE F_endOfGame=PROCEDURE(CONST b:boolean);

VAR
  config        : T_config;
  winnerEntry   : hallOfFameEntry;
  x0,y0,selectX,selectY:longint;
  configuring   ,
  keyboardMode  : boolean;

  quadSize      : longint;
  mainImage     : TImage;
  endOfGameCallback:F_endOfGame;

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

CONSTRUCTOR T_sudoku.createFull(CONST size: byte);
  begin
    repeat createUnfilled(size); until fullSolve(true)=solved;
  end;

CONSTRUCTOR T_sudoku.create(CONST size: byte; symm_x, symm_y, symm_center: boolean;
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
    if (x<fieldsize) and
       (y<fieldsize) then begin
      x+=y*fieldSize;
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

PROCEDURE T_sudoku.writeTxtForm(CONST writeOut, writelnOut: FT_output);
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

PROCEDURE T_sudoku.writeLaTeXForm(CONST writeOut, writelnOut: FT_output; CONST enumString: string; CONST small: boolean);
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
        el[i]:=stream.readWord; result:=result and (el[i]<=C_sudokuStructure[structIdx].any);
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
    if not(paused) then startTime:=now-startTime;
    paused:=true;
    renderRiddle;
  end;

PROCEDURE T_sudokuRiddle.switchPause;
  begin
    paused:=not(paused);
    startTime:=now-startTime;
    renderRiddle;
  end;

FUNCTION T_sudokuRiddle.isSolved: boolean;
  VAR x,y:byte;
  begin
    result:=true;
    for x:=0 to fieldSize-1 do
    for y:=0 to fieldSize-1 do result:=result and
      (state[x,y].value<255) and not(state[x,y].conflicting);
  end;

FUNCTION T_sudokuRiddle.makeHOFEntry: hallOfFameEntry;
  VAR x,y:byte;
  begin
    result.time:=now-startTime;
    result.given:=0;
    for x:=0 to fieldSize-1 do
    for y:=0 to fieldSize-1 do
    if state[x,y].given then inc(result.given);
    result.markErrors:=config.difficulty.markErrors;
  end;

PROCEDURE T_sudokuRiddle.initGame(size: byte);
  VAR aid:T_sudoku;
      i,j:byte;
  begin
    paused:=false;
    fieldSize:=size;
    modeIdx:=0;
    startTime:=now;
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

CONSTRUCTOR T_sudokuRiddle.create;
  begin
  end;

PROCEDURE T_sudokuRiddle.setState(CONST x, y: byte; value: byte;
  CONST append: boolean);
  begin
    if (x<fieldSize)     and
       (y<fieldSize)     and
       not(state[x,y].given)            then begin

      if append and (state[x,y].value*10+value<=fieldSize)
        then value:=state[x,y].value*10+value;
      if (value>0) and (value<=fieldSize) then state[x,y].value:=value
                                          else state[x,y].value:=255;
    end;
    checkConflicts;
    if isSolved then begin
      winnerEntry:=makeHOFEntry;
      endOfGameCallback(config.isGoodEnough(modeIdx,winnerEntry));
      initGame(C_sudokuStructure[modeIdx].size);
      config.gameIsDone:=false;
      pauseGame;
    end;
  end;

PROCEDURE T_sudokuRiddle.clearState(CONST x, y: byte);
  begin
    if (x<fieldSize)     and
       (y<fieldSize)     and
       not(state[x,y].given)            then begin
       state[x,y].value:=255;
    end;
    checkConflicts;
  end;

FUNCTION T_sudokuRiddle.givenState(CONST x, y: byte): boolean;
  begin
    result:=(x<fieldSize) and
            (y<fieldSize) and (state[x,y].given);
  end;

PROCEDURE T_sudokuRiddle.renderRiddle;
  VAR gridTop,gridBottom:longint;

  FUNCTION interpolateColor(y:longint):longint;
    VAR r,g,b:byte;
        int1,int2:word;
    begin
      int1:=(4096*y) div (mainImage.height);
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
      int1:=(4096*y) div (mainImage.height);
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
      for y:=y0 to y1 do MainImage.Canvas.Pixels[x,y]:=interpolateColor2(y);
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
    while  (quadSize*fieldSize*1.1<mainImage.width    )
       and (quadSize*fieldSize*1.1<mainImage.height-19) do inc(quadSize);
    y0:=(mainImage.height-19-fieldSize*quadSize) shr 1;
    x0:=(mainImage.width    -fieldSize*quadSize) shr 1;
    for y:=0 to mainImage.height-19 do begin
      mainImage.Canvas.Pen.color:=interpolateColor(y);
      mainImage.Canvas.line(0,y,mainImage.width,y);
    end;

    for x:=0 to fieldSize do begin
      if x mod C_sudokuStructure[modeIdx].blocksize[0]<>0 then begin
        vLine(x0+x*quadSize,y0-1,y0+fieldSize*quadSize+1);
      end;
      if x mod C_sudokuStructure[modeIdx].blocksize[1]<>0 then begin
        mainImage.Canvas.Pen.color:=interpolateColor2(y0+x*quadSize);
        mainImage.Canvas.line(x0-1,y0+x*quadSize,  x0+fieldSize*quadSize+1,y0+x*quadSize  );
      end;
    end;

    mainImage.Canvas.Pen.color:=config.view.gridCol;
    for x:=0 to fieldSize do begin
      if x mod C_sudokuStructure[modeIdx].blocksize[0]=0 then begin
        mainImage.Canvas.line(x0+x*quadSize-1,y0-1,x0+x*quadSize-1,y0+fieldSize*quadSize+1);
        mainImage.Canvas.line(x0+x*quadSize+1,y0-1,x0+x*quadSize+1,y0+fieldSize*quadSize+1);
      end;
      if x mod C_sudokuStructure[modeIdx].blocksize[1]=0 then begin
        mainImage.Canvas.line(x0-1,y0+x*quadSize-1,x0+fieldSize*quadSize+1,y0+x*quadSize-1);
        mainImage.Canvas.line(x0-1,y0+x*quadSize+1,x0+fieldSize*quadSize+1,y0+x*quadSize+1);
      end;
    end;
    if keyboardMode then begin
      mainImage.Canvas.MoveTo(x0+selectX*quadSize+3,
                              y0+selecty*quadSize+3);
      mainImage.Canvas.LineTo(x0+selectX*quadSize+quadSize-3,
                              y0+selecty*quadSize+3);
      mainImage.Canvas.LineTo(x0+selectX*quadSize+quadSize-3,
                              y0+selecty*quadSize+quadSize-3);
      mainImage.Canvas.LineTo(x0+selectX*quadSize+3,
                              y0+selecty*quadSize+quadSize-3);
      mainImage.Canvas.LineTo(x0+selectX*quadSize+3,
                              y0+selecty*quadSize+3);
    end;

    mainImage.Canvas.Brush.style:=bsClear;
    mainImage.Canvas.Font.size:=round(quadSize*0.9);
    mainImage.Canvas.Font.height:=round(quadSize*0.9);
    mainImage.Canvas.Font.name  :=config.Font.name;
    mainImage.Canvas.Font.color:=config.view.givenCol;
    if config.Font.bold and config.Font.italic then mainImage.Canvas.Font.style:=[fsBold,fsItalic]
    else if config.Font.bold                   then mainImage.Canvas.Font.style:=[fsBold]
    else if config.Font.italic                 then mainImage.Canvas.Font.style:=[fsItalic]
                                               else mainImage.Canvas.Font.style:=[];
    if paused then for x:=0 to fieldSize-1 do
    for y:=0 to fieldSize-1 do begin
      txt:=sudokuChar[(x+y*fieldSize) mod 7];
      mainImage.Canvas.textOut(x0+x*quadSize+(quadSize shr 1-mainImage.Canvas.textWidth(txt) shr 1),
                            y0+y*quadSize,txt);
    end else for x:=0 to fieldSize-1 do
    for y:=0 to fieldSize-1 do if (state[x,y].value<255) then  begin
      if state[x,y].given
        then mainImage.Canvas.Font.color:=config.view.givenCol
      else if state[x,y].conflicting and config.difficulty.markErrors
        then mainImage.Canvas.Font.color:=config.view.confCol
        else mainImage.Canvas.Font.color:=config.view.neutralCol;
      txt:=intToStr(state[x,y].value);
      mainImage.Canvas.textOut(x0+x*quadSize+(quadSize shr 1-mainImage.Canvas.textWidth(txt) shr 1),
                            y0+y*quadSize,txt);
    end;
  end;

FUNCTION T_sudokuRiddle.getSerialVersion: dword;
begin
  result:=12325862;
end;

FUNCTION T_sudokuRiddle.loadFromStream(VAR stream: T_bufferedInputStreamWrapper
  ): boolean;
  VAR i,j:byte;
  begin
    fieldsize:=stream.readByte; result:=fieldSize in [4,6,8,9,12,15,16];
    modeIdx  :=stream.readByte; result:=result and
                                  (modeIdx  in [0..6]) and
                                  (C_sudokuStructure[modeIdx].size=fieldSize);
    startTime:=now-stream.readDouble;
    for i:=0 to fieldSize-1 do
    for j:=0 to fieldSize-1 do with state[i,j] do begin
      given:=stream.readBoolean;
      value:=stream.readByte;
    end;
    checkConflicts;
  end;

PROCEDURE T_sudokuRiddle.saveToStream(VAR stream: T_bufferedOutputStreamWrapper
  );
  VAR i,j:byte;
  begin
    stream.writeByte(fieldsize);
    stream.writeByte(modeIdx);
    stream.writeDouble(now-startTime);
    for i:=0 to fieldSize-1 do
    for j:=0 to fieldSize-1 do with state[i,j] do begin
      stream.writeBoolean(given);
      stream.writeByte   (value);
    end;
  end;

//-----------------------------------------------------------------------------:T_sudokuRiddle
//T_config:-----------------------------------------------------------------------------------
CONSTRUCTOR T_config.create;
  VAR i,j:byte;
  begin
    riddle.create;
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
        hallOfFame[i,0].name:='Sudoku';
        hallOfFame[i,1].name:='von';
        hallOfFame[i,2].name:='Martin Schlegel';
        hallOfFame[i,3].name:='26.01.2008 - 10.02.2019';
        hallOfFame[i,4].name:='erstellt mit';
        hallOfFame[i,5].name:='Lazarus v1.8.4 Beta';
      end;
      riddle.initGame(9);
      gameIsDone:=false;
    end;
  end;

DESTRUCTOR T_config.destroy;
  begin
    saveToFile('sudoku3.cfg');
  end;

FUNCTION T_config.getSerialVersion: dword;
  begin
    result:=34562387;
  end;

FUNCTION T_config.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i,j:byte;
  begin
    with view do begin
      bgColTop   :=stream.readLongint; result:=(bgColTop>=0) and (bgColTop<16777216);
      bgColBottom:=stream.readLongint; result:=result and (bgColBottom>=0) and (bgColBottom<16777216);
      gridCol    :=stream.readLongint; result:=result and (gridCol    >=0) and (gridCol    <16777216);
      givenCol   :=stream.readLongint; result:=result and (givenCol   >=0) and (givenCol   <16777216);
      neutralCol :=stream.readLongint; result:=result and (neutralCol >=0) and (neutralCol <16777216);
      confCol    :=stream.readLongint; result:=result and (confCol    >=0) and (confCol    <16777216);
    end;
    with Font do begin
      name  :=stream.readAnsiString;
      bold  :=stream.readBoolean;
      italic:=stream.readBoolean;
    end;
    with difficulty do begin
      markErrors:=stream.readBoolean;
      xSymm     :=stream.readBoolean;
      ySymm     :=stream.readBoolean;
      ptSymm    :=stream.readBoolean;
      diff      :=stream.readByte;
    end;
    for i:=0 to 6 do for j:=0 to 19 do with hallOfFame[i,j] do begin
      name:=stream.readAnsiString;
      time:=stream.readDouble;  result:=result and (time>0);
      given:=stream.readWord;   result:=result and (given>0) and (given<=sqr(C_sudokuStructure[i].size));
      markErrors:=stream.readBoolean;
    end;
    riddle.initGame(4);
    result:=result and riddle.loadFromStream(stream);
    gameIsDone:=stream.readBoolean;
  end;

PROCEDURE T_config.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i,j:byte;
  begin
    with view do begin
      stream.writeLongint(bgColTop   );
      stream.writeLongint(bgColBottom);
      stream.writeLongint(gridCol    );
      stream.writeLongint(givenCol   );
      stream.writeLongint(neutralCol );
      stream.writeLongint(confCol    );
    end;
    with Font do begin
      stream.writeAnsiString(name);
      stream.writeBoolean(bold);
      stream.writeBoolean(italic);
    end;
    with difficulty do begin
      stream.writeBoolean(markErrors);
      stream.writeBoolean(xSymm     );
      stream.writeBoolean(ySymm     );
      stream.writeBoolean(ptSymm    );
      stream.writeByte   (diff      );
    end;
    for i:=0 to 6 do for j:=0 to 19 do with hallOfFame[i,j] do begin
      stream.writeAnsiString (name);
      stream.writeDouble (time);
      stream.writeWord   (given);
      stream.writeBoolean(markErrors);
    end;
    riddle.saveToStream(stream);
    stream.writeBoolean(gameIsDone);
  end;

FUNCTION T_config.isGoodEnough(CONST modeIdx: byte; CONST newEntry: hallOfFameEntry
  ): boolean;
  begin
    result:=isBetterThan(C_sudokuStructure[modeIdx].size,
                         newEntry,
                         hallOfFame[modeIdx,19]);
  end;

PROCEDURE T_config.addHOFEntry(CONST modeIdx: byte; CONST newEntry: hallOfFameEntry);
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

end.
