UNIT sudoku;
INTERFACE
USES serializationUtil,sysutils,ExtCtrls,Graphics,types;
CONST

  C_firstTime=maxLongint-7019;
  C_defaultStructureIndex=3;
  C_sudokuStructure:array [0..6] of record
                                      size     :byte;
                                      BlockSize:array[0..1] of byte;
                                      any      :word;
                                    end=
  ((size: 4; BlockSize:(2,2); any:   15),
   (size: 6; BlockSize:(2,3); any:   63),
   (size: 8; BlockSize:(2,4); any:  255),
   (size: 9; BlockSize:(3,3); any:  511),
   (size:12; BlockSize:(3,4); any: 4095),
   (size:15; BlockSize:(3,5); any:32767),
   (size:16; BlockSize:(4,4); any:65535));

  C_Latex_fileHeader:array[0..5] of string=('\documentclass[12pt,a4paper]{report} \usepackage{graphics}' ,
                                            '\oddsidemargin 0in \evensidemargin 0in \topmargin 0in \textheight 23cm \textwidth 16cm',
                                            '\renewcommand\arraystretch{1.4}',
                                            '\setlength{\lineskip}{2.0ex plus0.5ex minus0.5ex}',
                                            '\setlength{\doublerulesep}{0.2mm}',
                                            '\begin{document} \begin{center}');
  C_LaTeX_fileFooter:string='\end{center}\end{document}';

TYPE
  T_symmetry=(sym_x,sym_y,sym_center);
  T_symmetries=set of T_symmetry;
  T_sudokuState=(ss_solved,ss_unknown,ss_unsolveable);

  { T_setOfBytes }

  T_setOfBytes=object
    fill:longint;
    value:array[0..255] of byte;
    PROCEDURE add(CONST v:byte);
    PROCEDURE addAll(CONST b:T_setOfBytes);
    FUNCTION contains(CONST v:byte):boolean;
    FUNCTION minimum:byte;
  end;

  FT_output=PROCEDURE(txt:string);

  { T_sudoku }

  T_sudoku=object
    private
      el:array [0..255] of word;
      sym:T_symmetries;
      fieldSize,structIdx:byte;
      FUNCTION fullSolve(CONST fillRandom:boolean):T_sudokuState;
    public
      CONSTRUCTOR createUnfilled(size:byte);
      CONSTRUCTOR createFull    (CONST size:byte);
      CONSTRUCTOR create(CONST size:byte; CONST symmetries:T_symmetries; CONST difficulty:word);
      FUNCTION    clone(CONST undefList:T_setOfBytes):T_sudoku;
      FUNCTION    getSquare(CONST x,y:byte):byte;
      FUNCTION    given:word;
      DESTRUCTOR  destroy;
      PROCEDURE   solve;
      PROCEDURE   writeTxtForm  (CONST writeOut,writelnOut:FT_output);
      PROCEDURE   writeLaTeXForm(CONST writeOut,writelnOut:FT_output; CONST enumString:string; CONST small:boolean);
      PROCEDURE scramble(CONST allowCellPermutation:boolean);
      FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean;
      PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper);
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
      givenCount:longint;
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

  P_storedRiddles=^T_storedRiddles;
  T_storedRiddles=object
    private
      riddle:array[0..6] of record fill:longint; s:array [0..289] of T_sudoku; end;//~1MiB worth of riddles
      storedCs:TRTLCriticalSection;
      destructionPending:boolean;
      calculationRunning:boolean;
      PROCEDURE makeARiddle;
    public

      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE ensureBackgroundThread;

      FUNCTION getRiddle(CONST structIdx:byte; CONST symmetries:T_symmetries; CONST valuesGiven:byte):T_sudoku;
      FUNCTION validDifficulties(CONST structIdx:byte; CONST symmetries:T_symmetries):T_setOfBytes;
      PROCEDURE writeSummary;

      FUNCTION loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean;
      PROCEDURE saveToStream(VAR stream:T_bufferedOutputStreamWrapper);
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
                 markErrors:boolean;
                 symmetries:T_symmetries;
                 diff:byte;
               end;
    hallOfFame:array [0..6,0..31] of hallOfFameEntry;
    storedRiddles:T_storedRiddles;
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
  mainImage     : TImage;
  endOfGameCallback:F_endOfGame;

  x0,y0,quadSize: longint;

PROCEDURE writeLatexHeader(writelnOut:FT_output);
FUNCTION formattedTime(CONST hofEntry:hallOfFameEntry):string;
FUNCTION getMarkerBounds(CONST ix,iy:longint):TRect;
IMPLEMENTATION
USES BGRABitmapTypes,BGRABitmap,BGRAGraphics;
CONST C_bit:array[0..15] of word=(1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768);
VAR tempImage:TBGRABitmap=nil;

FUNCTION getMarkerBounds(CONST ix,iy:longint):TRect;
  begin
    result.Left  :=x0+ ix   *quadSize+3;
    result.Right :=x0+(ix+1)*quadSize-3;
    result.top   :=y0+ iy   *quadSize+3;
    result.Bottom:=y0+(iy+1)*quadSize-3;
  end;

FUNCTION formattedTime(CONST hofEntry:hallOfFameEntry):string;
  begin
    if hofEntry.time>=C_firstTime then result:='' else
    result:=FormatDateTime('[h]:mm:ss',hofEntry.time,[fdoInterval]);
  end;

PROCEDURE writeLatexHeader(writelnOut:FT_output);
  VAR i:byte;
  begin for i:=0 to 5 do writelnOut(C_Latex_fileHeader[i]); end;

{ T_storedRiddles }

PROCEDURE T_storedRiddles.makeARiddle;
  VAR struct:byte;
      symmetries:T_symmetries=[];
      sudoku:T_sudoku;
      minGiven:word;
      difficulty:word=256;
      i,g:longint;
  begin
    struct:=random(length(C_sudokuStructure));
    case byte(random(5)) of
        1: symmetries:=[sym_x];
        2: symmetries:=[sym_y];
        3: symmetries:=[sym_center];
        4: symmetries:=[sym_x,sym_y,sym_center];
      else symmetries:=[];
    end;

    minGiven:=sqr(C_sudokuStructure[struct].size);
    for i:=0 to riddle[struct].fill-1 do if riddle[struct].s[i].sym=symmetries then begin
      g:=riddle[struct].s[i].given;
      if g<minGiven then minGiven:=g;
    end;

    if minGiven=sqr(C_sudokuStructure[struct].size)
    then difficulty:=1
    else difficulty:=sqr(C_sudokuStructure[struct].size)-minGiven+1;
    sudoku.create(C_sudokuStructure[struct].size,symmetries,difficulty);

    enterCriticalSection(storedCs);
    if riddle[struct].fill<length(riddle[struct].s) then begin
      riddle[struct].s[riddle[struct].fill]:=sudoku;
      i:= riddle[struct].fill;
      inc(riddle[struct].fill);
    end else begin
      i:=0;
      while (i<length(riddle[struct].s)) and
            ((riddle[struct].s[i].sym<>symmetries) or (riddle[struct].s[i].sym=symmetries) and (riddle[struct].s[i].given<=sudoku.given)) do inc(i);
      if i<length(riddle[struct].s)
      then riddle[struct].s[i]:=sudoku
      else i:=-1;
    end;
    if i<>-1 then begin
      while (i>0) and (riddle[struct].s[i].given>riddle[struct].s[i-1].given) do begin
        sudoku:=riddle[struct].s[i];
        riddle[struct].s[i]:=riddle[struct].s[i-1];
        riddle[struct].s[i-1]:=sudoku;
        dec(i);
      end;
      while (i<riddle[struct].fill-1) and (riddle[struct].s[i].given<riddle[struct].s[i+1].given) do begin
        sudoku:=riddle[struct].s[i];
        riddle[struct].s[i]:=riddle[struct].s[i+1];
        riddle[struct].s[i+1]:=sudoku;
        inc(i);
      end;
    end;
    leaveCriticalSection(storedCs);
  end;

CONSTRUCTOR T_storedRiddles.create;
  VAR i:longint;
  begin
    initCriticalSection(storedCs);
    destructionPending:=false;
    calculationRunning:=false;
    for i:=0 to 6 do riddle[i].fill:=0;
  end;

DESTRUCTOR T_storedRiddles.destroy;
  begin
    destructionPending:=true;
    while calculationRunning do sleep(1);
    doneCriticalSection(storedCs);
  end;

FUNCTION makeRiddlesThread(p:pointer):ptrint;
  VAR closeThreadAfter:double;
  begin
    closeThreadAfter:=now+3/(24*60); //=3 Minutes
    with P_storedRiddles(p)^ do begin
      while not(destructionPending) and (now<closeThreadAfter) do makeARiddle;
      calculationRunning:=false;
    end;
    result:=0;
  end;

PROCEDURE T_storedRiddles.ensureBackgroundThread;
  begin
    enterCriticalSection(storedCs);
    if not(calculationRunning) and not(destructionPending) then begin
      calculationRunning:=true;
      beginThread(@makeRiddlesThread,@self);
    end;
    leaveCriticalSection(storedCs);
  end;

FUNCTION T_storedRiddles.getRiddle(CONST structIdx: byte; CONST symmetries: T_symmetries; CONST valuesGiven: byte): T_sudoku;
  VAR candidates:array[0..291] of T_sudoku;
      candidateCount:longint=0;
      bestMatchingGiven:word=65535;
      given:word;
      i:longint;
      j:longint=0;
  begin
    candidates[0].create(C_sudokuStructure[structIdx].size,symmetries,sqr(C_sudokuStructure[structIdx].size)-valuesGiven);
    candidateCount:=1;
    bestMatchingGiven:=candidates[0].given;
    enterCriticalSection(storedCs);
    //Only take the riddles into account that are at least as symmetric as required
    for i:=0 to riddle[structIdx].fill-1 do if riddle[structIdx].s[i].sym*symmetries=symmetries then begin
      given:=riddle[structIdx].s[i].given;
      if abs(valuesGiven-given)<=abs(valuesGiven-bestMatchingGiven) then begin
        bestMatchingGiven:=given;
        candidates[candidateCount]:=riddle[structIdx].s[i];
        inc(candidateCount);
      end;
    end;
    leaveCriticalSection(storedCs);
    //Filter again in order to restrict to the "most nearly right" number of given values
    for i:=0 to candidateCount-1 do if abs(valuesGiven-candidates[i].given)<=abs(valuesGiven-bestMatchingGiven)
       then begin
         candidates[j]:=candidates[i];
         inc(j);
       end;
    candidateCount:=j;
    //Result is any of the filtered riddles
    if candidateCount=0
    then result.create(C_sudokuStructure[structIdx].size,symmetries,sqr(C_sudokuStructure[structIdx].size)-valuesGiven)
    else result:=candidates[random(candidateCount)];
    result.scramble(symmetries=[]);
  end;

FUNCTION T_storedRiddles.validDifficulties(CONST structIdx:byte; CONST symmetries:T_symmetries):T_setOfBytes;
  VAR i:longint;
  begin
    enterCriticalSection(storedCs);
    result.fill:=0;
    for i:=0 to riddle[structIdx].fill-1 do if (riddle[structIdx].s[i].sym*symmetries=symmetries) then result.add(riddle[structIdx].s[i].given);
    leaveCriticalSection(storedCs);
  end;

PROCEDURE T_storedRiddles.writeSummary;
  CONST SYM_TMP:array[0..4] of record
          explanation:string;
          sym:T_symmetries;
        end=((explanation:'SYM'; sym:[sym_x,sym_y,sym_center]),
             (explanation:'Sx '; sym:[sym_x]),
             (explanation:'Sy '; sym:[sym_y]),
             (explanation:'Sc '; sym:[sym_center]),
             (explanation:'---'; sym:[]));
  VAR i,j,k:longint;
      counts:array[0..256] of longint;

  begin
    for i:=0 to 6 do begin
      writeln(C_sudokuStructure[i].size,'x',C_sudokuStructure[i].size,'-Riddles: ',riddle[i].fill);
      for j:=0 to 4 do begin
        for k:=0 to 256 do counts[k]:=0;
        for k:=0 to riddle[i].fill-1 do begin
          if riddle[i].s[k].sym=SYM_TMP[j].sym
          then inc(counts[riddle[i].s[k].given]);
        end;
        write(SYM_TMP[j].explanation,': ');
        for k:=0 to length(counts)-1 do if counts[k]>0 then write(k,'(',counts[k],') ');
        writeln;
      end;
    end;
  end;

FUNCTION T_storedRiddles.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i,j:longint;
  begin
    enterCriticalSection(storedCs);
    try
      result:=true;
      for i:=0 to 6 do begin
        riddle[i].fill:=stream.readLongint;
        if (riddle[i].fill<0) or (riddle[i].fill>length(riddle[i].s)) then exit(false);
        for j:=0 to riddle[i].fill-1 do result:=result and riddle[i].s[j].loadFromStream(stream);
      end;
      if not(result) then for i:=0 to 6 do riddle[i].fill:=0;
    finally
      leaveCriticalSection(storedCs);
    end;
  end;

PROCEDURE T_storedRiddles.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i,j:longint;
  begin
    enterCriticalSection(storedCs);
    try
      for i:=0 to 6 do begin
        stream.writeLongint(riddle[i].fill);
        for j:=0 to riddle[i].fill-1 do riddle[i].s[j].saveToStream(stream);
      end;
    finally
      leaveCriticalSection(storedCs);
    end;
  end;

{ T_setOfBytes }

PROCEDURE T_setOfBytes.add(CONST v: byte);
  VAR i:longint=0;
  begin
    while (i<fill) and (value[i]<>v) do inc(i);
    if i>=fill then begin
      value[i]:=v;
      inc(fill);
    end;
  end;

PROCEDURE T_setOfBytes.addAll(CONST b: T_setOfBytes);
  VAR i:longint;
  begin
    for i:=0 to b.fill-1 do add(b.value[i]);
  end;

FUNCTION T_setOfBytes.contains(CONST v: byte): boolean;
  VAR i:longint;
  begin
    for i:=0 to fill-1 do if value[i]=v then exit(true);
    result:=false;
  end;

FUNCTION T_setOfBytes.minimum:byte;
  VAR i:longint;
  begin
    result:=255;
    for i:=0 to fill-1 do if value[i]<result then result:=value[i];
  end;

FUNCTION T_sudoku.fullSolve(CONST fillRandom: boolean): T_sudokuState;
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
    end;

  PROCEDURE Exclude(idx:word); inline;
    begin el[idx]:=el[idx] and excludor; end;

  begin
    initialize(done);
    setLength(done,sqr(fieldSize));
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
        for k:=0 to sqr(fieldSize)-1 do if not(done[k]) then begin                                                           //
          if determined(el[k]) then begin                                                                                    //
            //exclude step:----------------------------------------------------------------------------------------------//  //
            //If a field is determined, its number may not occur in the same row, column or block.                       //  //
            excludor:=not(el[k]);                                                                                        //  //
            i0:=k mod fieldSize;                                                                                         //  //
            j0:=k div fieldSize;                                                                                         //  //
            //exclude value in row:                                                                                      //  //
            for i1:=0 to fieldSize-1 do if i1<>i0 then Exclude(j0*fieldSize+i1);                                         //  //
            //exclude value in column:                                                                                   //  //
            for j1:=0 to fieldSize-1 do if j1<>j0 then Exclude(j1*fieldSize+i0);                                         //  //
            //exclude value in block:                                                                                    //  //
            for i1:=(i0 div C_sudokuStructure[structIdx].BlockSize[0])   *C_sudokuStructure[structIdx].BlockSize[0]   to //  //
                   ((i0 div C_sudokuStructure[structIdx].BlockSize[0])+1)*C_sudokuStructure[structIdx].BlockSize[0]-1 do //  //
            for j1:=(j0 div C_sudokuStructure[structIdx].BlockSize[1])   *C_sudokuStructure[structIdx].BlockSize[1]   to //  //
                   ((j0 div C_sudokuStructure[structIdx].BlockSize[1])+1)*C_sudokuStructure[structIdx].BlockSize[1]-1 do //  //
            if (i1<>i0) or (j1<>j0) then Exclude(j1*fieldSize+i1);                                                       //  //
            done[k]      :=true;                                                                                         //  //
            progress     :=true;                                                                                         //  //
            totalProgress:=true;                                                                                         //  //
            //------------------------------------------------------------------------------------------------:exclude step  //
          end;                                                                                                               //
        end;                                                                                                                 //
      until not(progress);                                                                                                   //
      //----------------------------------------------------------------------------------------------------------:exclude loop
      progress:=false;
      for k:=0 to sqr(fieldSize)-1 do if not(done[k]) and not(determined(el[k])) then begin
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
          for j1:=0 to fieldSize-1 do if j1<>j0 then excludor:=excludor or el[j1*fieldSize+i0];                          //
          if determined(excludor) then begin                                                                             //
            el[k]        :=excludor;                                                                                     //
            totalProgress:=true;                                                                                         //
          end else begin                                                                                                 //
            //exclude value in block:                                                                                    //
            for i1:=(i0 div C_sudokuStructure[structIdx].BlockSize[0])   *C_sudokuStructure[structIdx].BlockSize[0]   to //
                   ((i0 div C_sudokuStructure[structIdx].BlockSize[0])+1)*C_sudokuStructure[structIdx].BlockSize[0]-1 do //
            for j1:=(j0 div C_sudokuStructure[structIdx].BlockSize[1])   *C_sudokuStructure[structIdx].BlockSize[1]   to //
                   ((j0 div C_sudokuStructure[structIdx].BlockSize[1])+1)*C_sudokuStructure[structIdx].BlockSize[1]-1 do //
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
        until totalProgress or (k=sqr(fieldSize));                //
      end;                                                        //
      //                                                          //
      //randomize step                                            //
      //----------------------------------------------------------//
    until not(totalProgress);

    //determine value to return
    result:=ss_solved;
    k:=0;
    repeat
      if el[k]=0 then result:=ss_unsolveable
      else if (result=ss_solved) and not(determined(el[k])) then result:=ss_unknown;
      inc(k);
    until (k=sqr(fieldSize)) or (result=ss_unsolveable);
    setLength(done,0);
  end; //fullSolve

CONSTRUCTOR T_sudoku.createUnfilled(size: byte);
  VAR k:word;
  begin
    //correct size
    if size<C_sudokuStructure[0                          ].size then size:=C_sudokuStructure[0                          ].size;
    if size>C_sudokuStructure[length(C_sudokuStructure)-1].size then size:=C_sudokuStructure[length(C_sudokuStructure)-1].size;
    //lookup structure index matching size
    structIdx:=0;
    while (structIdx<length(C_sudokuStructure)) and (C_sudokuStructure[structIdx].size<size) do inc(structIdx);
    if structIdx>=7 then structIdx:=C_defaultStructureIndex;
    //determine actual field size
    fieldSize:=C_sudokuStructure[structIdx].size;
    //set all cells to ss_unknown value
    for k:=0 to length(el)-1 do el[k]:=C_sudokuStructure[structIdx].any;
    sym:=[sym_x,sym_y,sym_center];
  end;

CONSTRUCTOR T_sudoku.createFull(CONST size: byte);
  begin
    repeat createUnfilled(size); until fullSolve(true)=ss_solved;
    sym:=[sym_x,sym_y,sym_center];
  end;

PROCEDURE writeOut  (txt:string); begin write  (txt); end;
PROCEDURE writelnOut(txt:string); begin writeln(txt); end;

CONSTRUCTOR T_sudoku.create(CONST size: byte; CONST symmetries:T_symmetries; CONST difficulty: word);
  FUNCTION extendUndefList(CONST undefList:T_setOfBytes; CONST i:byte):T_setOfBytes;
    FUNCTION mx(CONST k:longint):longint; begin result:=fieldSize-1-(k mod fieldSize)+fieldSize*             (k div fieldSize) ; end;
    FUNCTION my(CONST k:longint):longint; begin result:=            (k mod fieldSize)+fieldSize*(fieldSize-1-(k div fieldSize)); end;
    FUNCTION mc(CONST k:longint):longint; begin result:=fieldSize-1-(k mod fieldSize)+fieldSize*(fieldSize-1-(k div fieldSize)); end;
    VAR k:longint;
    begin
      initialize(result);
      result.fill:=1;
      result.value[0]:=i;
      if sym_x      in symmetries then for k:=0 to result.fill-1 do result.add(mx(result.value[k]));
      if sym_y      in symmetries then for k:=0 to result.fill-1 do result.add(my(result.value[k]));
      if sym_center in symmetries then for k:=0 to result.fill-1 do result.add(mc(result.value[k]));
      result.addAll(undefList);
    end;

  FUNCTION initialListTakingSymmetriesIntoAccount:T_setOfBytes;
    VAR ignored:T_setOfBytes;
        i,j,k:byte;
    begin
      initialize(result);  result.fill:=0;
      initialize(ignored); ignored.fill:=0;
      for k:=0 to fieldSize*fieldSize-1 do if not(ignored.contains(k)) then begin
        result.add(k);
        ignored:=extendUndefList(ignored,k);
      end;
      for i:=0 to result.fill-1 do begin
        repeat j:=random(result.fill) until j<>i;
        k:=result.value[i]; result.value[i]:=result.value[j]; result.value[j]:=k;
      end;
    end;

  VAR bestUndefList,
      undefList,
      extendedUndefList,
      undefCandidates:T_setOfBytes;
      k:longint;
      runsWithoutImprovement:longint=0;
  begin
    createFull(size);
    bestUndefList.fill:=0;
    repeat
      undefList.fill:=0;
      undefCandidates:=initialListTakingSymmetriesIntoAccount;
      while (undefList.fill<difficulty) and (undefCandidates.fill>0) do begin
        k:=undefCandidates.value[undefCandidates.fill-1];
        dec(undefCandidates.fill);
        extendedUndefList:=extendUndefList(undefList,k);
        if clone(extendedUndefList).fullSolve(false)=ss_solved
        then undefList:=extendedUndefList;
      end;
      if undefList.fill>bestUndefList.fill then begin
        bestUndefList:=undefList;
        runsWithoutImprovement:=0;
      end else inc(runsWithoutImprovement);
    until (bestUndefList.fill>=difficulty) or (runsWithoutImprovement>1.2*fieldSize);
    for k:=0 to bestUndefList.fill-1 do el[bestUndefList.value[k]]:=C_sudokuStructure[structIdx].any;
    sym:=symmetries;
  end;

FUNCTION T_sudoku.clone(CONST undefList: T_setOfBytes): T_sudoku;
  VAR k:longint;
  begin
    result.createUnfilled(fieldSize);
    for k:=0 to length(el)-1 do result.el[k]:=el[k];
    for k:=0 to undefList.fill-1 do result.el[undefList.value[k]]:=C_sudokuStructure[structIdx].any;
  end;

FUNCTION T_sudoku.getSquare(CONST x, y: byte): byte;
  VAR k:longint;
  begin
    if (x<fieldSize) and
       (y<fieldSize) then begin
      k:=x+y*fieldSize;
      result:=0;
      while (result<fieldSize) and (el[k]<>C_bit[result]) do inc(result);
      if result>=fieldSize then result:=255
                           else inc(result);
    end else result:=255;
  end;

FUNCTION T_sudoku.given: word;
  VAR i,j:word;
  begin
    result:=0;
    for i:=0 to sqr(fieldSize)-1 do
    for j:=0 to fieldSize-1 do if (el[i]=C_bit[j]) then inc(result);
  end;

DESTRUCTOR T_sudoku.destroy;
  begin end;

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

  CONST C_grid:array[0..6] of record
          header,
          fillLight,
          fillDouble,
          footer:string;
          separators:array[0..16] of string;
        end=
        ((header    : '╔═══╤═══╦═══╤═══╗';
          fillLight : '╟───┼───╫───┼───╢';
          fillDouble: '╠═══╪═══╬═══╪═══╣';
          footer    : '╚═══╧═══╩═══╧═══╝';
          separators:('║','│','║','│','║','','','','','','','','','','','','')),
         (header    : '╔═══╤═══╦═══╤═══╦═══╤═══╗';
          fillLight : '╟───┼───╫───┼───╫───┼───╢';
          fillDouble: '╠═══╪═══╬═══╪═══╬═══╪═══╣';
          footer    : '╚═══╧═══╩═══╧═══╩═══╧═══╝';
          separators:('║','│','║','│','║','│','║','','','','','','','','','','')),
         (header    : '╔═══╤═══╦═══╤═══╦═══╤═══╦═══╤═══╗';
          fillLight : '╟───┼───╫───┼───╫───┼───╫───┼───╢';
          fillDouble: '╠═══╪═══╬═══╪═══╬═══╪═══╬═══╪═══╣';
          footer    : '╚═══╧═══╩═══╧═══╩═══╧═══╩═══╧═══╝';
          separators:('║','│','║','│','║','│','║','│','║','','','','','','','','')),
         (header    : '╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗';
          fillLight : '╟───┼───┼───╫───┼───┼───╫───┼───┼───╢';
          fillDouble: '╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣';
          footer    : '╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝';
          separators:('║','│','│','║','│','│','║','│','│','║','','','','','','','')),
         (header    : '╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗';
          fillLight : '╟───┼───┼───╫───┼───┼───╫───┼───┼───╫───┼───┼───╢';
          fillDouble: '╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣';
          footer    : '╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝';
          separators:('║','│','│','║','│','│','║','│','│','║','│','│','║','','','','')),
         (header    : '╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗';
          fillLight : '╟───┼───┼───╫───┼───┼───╫───┼───┼───╫───┼───┼───╫───┼───┼───╢';
          fillDouble: '╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣';
          footer    : '╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝';
          separators:('║','│','│','║','│','│','║','│','│','║','│','│','║','│','│','║','')),
         (header    : '╔═══╤═══╤═══╤═══╦═══╤═══╤═══╤═══╦═══╤═══╤═══╤═══╦═══╤═══╤═══╤═══╗';
          fillLight : '╟───┼───┼───┼───╫───┼───┼───┼───╫───┼───┼───┼───╫───┼───┼───┼───╢';
          fillDouble: '╠═══╪═══╪═══╪═══╬═══╪═══╪═══╪═══╬═══╪═══╪═══╪═══╬═══╪═══╪═══╪═══╣';
          footer    : '╚═══╧═══╧═══╧═══╩═══╧═══╧═══╧═══╩═══╧═══╧═══╧═══╩═══╧═══╧═══╧═══╝';
          separators:('║','│','│','│','║','│','│','│','║','│','│','│','║','│','│','│','║')));

  VAR i,j:byte;
      k  :word;
  begin
    writelnOut(C_grid[structIdx].header);
    k:=0;
    for j:=0 to fieldSize-1 do begin                //für jede Zeile...
      for i:=0 to fieldSize-1 do begin              //für jede Zelle...
        writeOut(C_grid[structIdx].separators[i]);  //Spaltentrenner
        writeOut(numString(el[k]));                 //Zellenwert
        inc(k);
      end;
      writelnOut(C_grid[structIdx].separators[fieldSize]);              //abschließender Spaltentrenner
      if j=fieldSize-1
      then writelnOut(C_grid[structIdx].footer)
      else
      if (j+1) mod (C_sudokuStructure[structIdx].BlockSize[1])=0 //Trennzeile
        then writelnOut(C_grid[structIdx].fillDouble)
        else writelnOut(C_grid[structIdx].fillLight);
    end;
    writelnOut(' '); //abschließende Leerzeile
  end;

PROCEDURE T_sudoku.writeLaTeXForm(CONST writeOut, writelnOut: FT_output;
  CONST enumString: string; CONST small: boolean);
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
      if (j+1) mod (C_sudokuStructure[structIdx].BlockSize[1])=0 //Trennzeile
        then writelnOut('\\ \hline \hline')
        else writelnOut('\\ \hline');
    end;
    if enumString<>''
      then writelnOut('\end{tabular} \\ '+enumString+' \end{tabular} ~~~ ')
      else writelnOut('\end{tabular} ~~~ ');
    if small then writelnOut('}');
  end;

PROCEDURE T_sudoku.scramble(CONST allowCellPermutation:boolean);
  TYPE T_shortList=array[0..15] of byte;
  CONST C_orderedShortList:T_shortList=(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15);
  VAR simpleForm:array[0..15,0..15] of byte;
  PROCEDURE randomSubstitute;
    VAR sub:T_shortList;
        i,j:longint;
        tmp:byte;
    begin
      sub:=C_orderedShortList;
      for i:=0 to fieldSize-1 do begin
        repeat j:=random(fieldSize) until j<>i;
        tmp:=sub[i]; sub[i]:=sub[j]; sub[j]:=tmp;
      end;
      for i:=0 to fieldSize-1 do
      for j:=0 to fieldSize-1 do begin
        tmp:=simpleForm[i,j];
        if (tmp<fieldSize) then simpleForm[i,j]:=sub[tmp];
      end;
    end;

  FUNCTION permutation(CONST BlockSize:byte):T_shortList;
    VAR i,j,bi,bj,blockCount:longint;
        tmp:byte;
    begin
      result:=C_orderedShortList;
      //Scramble within blocks:
      for i:=0 to fieldSize-1 do begin
        repeat j:=(i div BlockSize)*BlockSize+random(BlockSize) until j<>i;
        tmp:=result[i]; result[i]:=result[j]; result[j]:=tmp;
      end;
      //Scramble blocks:
      blockCount:=fieldSize div BlockSize;
      for bi:=0 to blockCount-1 do begin
        repeat bj:=random(blockCount) until bj<>bi;
        for i:=0 to BlockSize-1 do begin
          tmp:=result[bi*BlockSize+i];
          result[bi*BlockSize+i]:=result[bj*BlockSize+i];
          result[bj*BlockSize+i]:=tmp;
        end;
      end;
    end;

  VAR i,j,i_,j_:longint;
      permI,permJ:T_shortList;
  begin
    //Randomly transpose (if possible)
    if (C_sudokuStructure[structIdx].BlockSize[0]=C_sudokuStructure[structIdx].BlockSize[1]) and (random>0.5) and (allowCellPermutation)
    then for i:=0 to fieldSize-1 do for j:=0 to fieldSize-1 do simpleForm[i,j]:=getSquare(j,i)-1
    else for i:=0 to fieldSize-1 do for j:=0 to fieldSize-1 do simpleForm[i,j]:=getSquare(i,j)-1;

    randomSubstitute;
    if allowCellPermutation
    then begin
      permI:=permutation(C_sudokuStructure[structIdx].BlockSize[0]);
      permJ:=permutation(C_sudokuStructure[structIdx].BlockSize[1]);
    end else begin
      permI:=C_orderedShortList;
      permJ:=C_orderedShortList;
    end;
    //Write back
    for i:=0 to fieldSize-1 do for j:=0 to fieldSize-1 do begin
      i_:=permI[i];
      j_:=permJ[j];
      if (simpleForm[i,j]<fieldSize)
      then el[i_+fieldSize*j_]:=C_bit[simpleForm[i,j]]
      else el[i_+fieldSize*j_]:=C_sudokuStructure[structIdx].any;
    end;
  end;

FUNCTION T_sudoku.loadFromStream(VAR stream:T_bufferedInputStreamWrapper):boolean;
  VAR i:longint;
      v:byte;
  begin
    result:=true;
    structIdx:=stream.readByte;
    if (structIdx>=length(C_sudokuStructure)) then exit(false);
    fieldSize:=C_sudokuStructure[structIdx].size;
    v:=stream.readByte;
    if v in [0..7] then begin
      sym:=[];
      if (v and 1)=1 then include(sym,sym_x);
      if (v and 2)=2 then include(sym,sym_y);
      if (v and 4)=4 then include(sym,sym_center);
    end else exit(false);
    for i:=0 to fieldSize*fieldSize-1 do begin
      v:=stream.readByte;
      if (v<fieldSize) then el[i]:=C_bit[v] else
      if v=255 then el[i]:=C_sudokuStructure[structIdx].any
      else exit(false);
    end;
  end;

PROCEDURE T_sudoku.saveToStream(VAR stream:T_bufferedOutputStreamWrapper);
  VAR i:longint;
      v:byte;
  begin
    stream.writeByte(structIdx);
    v:=0;
    if sym_x      in sym then inc(v);
    if sym_y      in sym then inc(v,2);
    if sym_center in sym then inc(v,4);
    stream.writeByte(v);
    for i:=0 to fieldSize*fieldSize-1 do begin
      v:=0;
      while (v<C_sudokuStructure[structIdx].size) and (el[i]<>C_bit[v]) do inc(v);
      if v>=C_sudokuStructure[structIdx].size then v:=255;
      stream.writeByte(v);
    end;
  end;

FUNCTION isBetterThan(e1,e2:hallOfFameEntry):boolean;
  begin
    result:=(ord(e1.markErrors)*2+e1.given)/24/60+e1.time<
            (ord(e2.markErrors)*2+e2.given)/24/60+e2.time;
  end;

//T_sudokuRiddle:-----------------------------------------------------------------------------
PROCEDURE T_sudokuRiddle.pauseGame;
  begin
    if not(paused) then begin
      startTime:=now-startTime;
      paused:=true;
      renderRiddle;
    end;
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
      structIndex:longint=-1;
      valuesGiven:word;
  begin
    for i:=0 to length(C_sudokuStructure)-1 do if size=C_sudokuStructure[i].size then structIndex:=i;
    paused:=false;
    fieldSize:=size;
    modeIdx:=0;
    startTime:=now;
    while C_sudokuStructure[modeIdx].size<>size do inc(modeIdx);
    valuesGiven:=config.storedRiddles.validDifficulties(structIndex,config.difficulty.symmetries).minimum;
    valuesGiven:=valuesGiven+round((sqr(size)-valuesGiven)*sqr(config.difficulty.diff*0.1));
    aid:=config.storedRiddles.getRiddle(structIndex,config.difficulty.symmetries,valuesGiven);
    givenCount:=0;
    for i:=0 to size-1 do
    for j:=0 to size-1 do begin
      state[i,j].value      :=aid.getSquare(i,j);
      state[i,j].given      :=(state[i,j].value<>255);
      if state[i,j].given then inc(givenCount);
      state[i,j].conflicting:=false;
    end;
  end;

PROCEDURE T_sudokuRiddle.checkConflicts;
  VAR x1,y1,x2,y2:byte;
  begin
    for x1:=0 to fieldSize-1 do
    for y1:=0 to fieldSize-1 do state[x1,y1].conflicting:=false;

    for x1:=0 to fieldSize-1 do
    for y1:=0 to fieldSize-1 do if state[x1,y1].value<>255 then
    for x2:=0 to fieldSize-1 do
    for y2:=0 to fieldSize-1 do if (state[x1,y1].value=state[x2,y2].value)
      and ((x1<>x2) or (y1<>y2)) then begin
      if (x1=x2) or //same column
         (y1=y2) or //same row
         (x1 div C_sudokuStructure[modeIdx].BlockSize[0]=             // \
          x2 div C_sudokuStructure[modeIdx].BlockSize[0]) and         //  \
         (y1 div C_sudokuStructure[modeIdx].BlockSize[1]=             //  /same block
          y2 div C_sudokuStructure[modeIdx].BlockSize[1]) then begin  // /
        state[x1,y1].conflicting:=true;
        state[x2,y2].conflicting:=true;
      end;
    end;
  end;

CONSTRUCTOR T_sudokuRiddle.create;
  begin  end;

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
    renderRiddle;
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
    renderRiddle;
  end;

FUNCTION T_sudokuRiddle.givenState(CONST x, y: byte): boolean;
  begin
    result:=(x<fieldSize) and
            (y<fieldSize) and (state[x,y].given);
  end;

PROCEDURE T_sudokuRiddle.renderRiddle;
  CONST sudokuChar:array [0..6] of char=('S','U','D','O','K','U',' ');

  VAR x,y:longint;
      txt:string;
      topColor,bottomColor,
      gridColor:TBGRAPixel;

  begin
    if tempImage=nil
    then tempImage:=TBGRABitmap.create(mainImage.width,mainImage.height)
    else tempImage.setSize            (mainImage.width,mainImage.height);

    tempImage.AntialiasingDrawMode:=dmLinearBlend;
    topColor   .FromColor(config.view.bgColTop);
    bottomColor.FromColor(config.view.bgColBottom);
    tempImage.CanvasBGRA.GradientFill(rect(0,0,tempImage.width,tempImage.height),topColor,bottomColor,gdVertical);
    quadSize:=10;
    while  (quadSize*fieldSize*1.1<mainImage.width    )
       and (quadSize*fieldSize*1.1<mainImage.height-19) do inc(quadSize);
    y0:=(mainImage.height-1-fieldSize*quadSize) shr 1;
    x0:=(mainImage.width   -fieldSize*quadSize) shr 1;

    gridColor.FromColor(config.view.gridCol,128);
    tempImage.CanvasBGRA.Pen.BGRAColor:=gridColor;
    tempImage.CanvasBGRA.AntialiasingMode:=amOn;

    for x:=0 to fieldSize do begin
      if x mod C_sudokuStructure[modeIdx].BlockSize[0]<>0 then begin
        tempImage.CanvasBGRA.MoveTo(x0+x*quadSize,y0-1);
        tempImage.CanvasBGRA.LineTo(x0+x*quadSize,y0+fieldSize*quadSize+1);
      end;
      if x mod C_sudokuStructure[modeIdx].BlockSize[1]<>0 then begin
        tempImage.CanvasBGRA.MoveTo(x0-1,y0+x*quadSize);
        tempImage.CanvasBGRA.LineTo(x0+fieldSize*quadSize+1,y0+x*quadSize  );
      end;
    end;

    gridColor.FromColor(config.view.gridCol);
    tempImage.CanvasBGRA.Pen.BGRAColor:=gridColor;
    for x:=0 to fieldSize do begin
      if x mod C_sudokuStructure[modeIdx].BlockSize[0]=0 then begin
        tempImage.CanvasBGRA.MoveTo(x0+x*quadSize-1,y0-1);
        tempImage.CanvasBGRA.LineTo(x0+x*quadSize-1,y0+fieldSize*quadSize+1);
        tempImage.CanvasBGRA.MoveTo(x0+x*quadSize+1,y0-1);
        tempImage.CanvasBGRA.LineTo(x0+x*quadSize+1,y0+fieldSize*quadSize+1);
      end;
      if x mod C_sudokuStructure[modeIdx].BlockSize[1]=0 then begin
        tempImage.CanvasBGRA.MoveTo(x0-1                   ,y0+x*quadSize-1);
        tempImage.CanvasBGRA.LineTo(x0+fieldSize*quadSize+1,y0+x*quadSize-1);
        tempImage.CanvasBGRA.MoveTo(x0-1,y0+x*quadSize+1);
        tempImage.CanvasBGRA.LineTo(x0+fieldSize*quadSize+1,y0+x*quadSize+1);
      end;
    end;

    tempImage.Canvas.Brush.style:=bsClear;
    tempImage.Canvas.Font.height:=round(quadSize*0.9);
    tempImage.Canvas.Font.name  :=config.Font.name;
    tempImage.Canvas.Font.color:=config.view.givenCol;
    if config.Font.bold and config.Font.italic then tempImage.Canvas.Font.style:=[fsBold,fsItalic]
    else if config.Font.bold                   then tempImage.Canvas.Font.style:=[fsBold]
    else if config.Font.italic                 then tempImage.Canvas.Font.style:=[fsItalic]
                                               else tempImage.Canvas.Font.style:=[];
    if paused then for x:=0 to fieldSize-1 do
    for y:=0 to fieldSize-1 do begin
      txt:=sudokuChar[(x+y*fieldSize) mod 7];
      tempImage.Canvas.textOut(x0+x*quadSize+(quadSize shr 1-tempImage.Canvas.textWidth(txt) shr 1),
                            y0+y*quadSize,txt);
    end else for x:=0 to fieldSize-1 do
    for y:=0 to fieldSize-1 do if (state[x,y].value<255) then  begin
      if state[x,y].given
        then tempImage.Canvas.Font.color:=config.view.givenCol
      else if state[x,y].conflicting and config.difficulty.markErrors
        then tempImage.Canvas.Font.color:=config.view.confCol
        else tempImage.Canvas.Font.color:=config.view.neutralCol;
      txt:=intToStr(state[x,y].value);
      tempImage.Canvas.textOut(x0+x*quadSize+(quadSize shr 1-tempImage.Canvas.textWidth(txt) shr 1),
                            y0+y*quadSize,txt);
    end;
    tempImage.Canvas.Font.height:=round(quadSize*0.3);
    tempImage.Canvas.Font.color:=config.view.givenCol;
    tempImage.Canvas.textOut(0,mainImage.height-round(1.5*tempImage.Canvas.textHeight('Vorgegeben')),'Vorgegeben: '+intToStr(givenCount));

    tempImage.draw(mainImage.Canvas,0,0);
    mainImage.Invalidate;
  end;

FUNCTION T_sudokuRiddle.getSerialVersion: dword;
begin
  result:=12325862;
end;

FUNCTION T_sudokuRiddle.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i,j:byte;
  begin
    fieldSize:=stream.readByte; result:=fieldSize in [4,6,8,9,12,15,16];
    modeIdx  :=stream.readByte; result:=result and
                                  (modeIdx  in [0..6]) and
                                  (C_sudokuStructure[modeIdx].size=fieldSize);
    startTime:=now-stream.readDouble;
    givenCount:=0;
    for i:=0 to fieldSize-1 do
    for j:=0 to fieldSize-1 do with state[i,j] do begin
      given:=stream.readBoolean;
      if given then inc(givenCount);
      value:=stream.readByte;
    end;
    if result then checkConflicts
              else initGame(4);
  end;

PROCEDURE T_sudokuRiddle.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i,j:byte;
  begin
    stream.writeByte(fieldSize);
    stream.writeByte(modeIdx);
    if isPaused then stream.writeDouble(    startTime)
                else stream.writeDouble(now-startTime);
    for i:=0 to fieldSize-1 do
    for j:=0 to fieldSize-1 do with state[i,j] do begin
      stream.writeBoolean(given);
      stream.writeByte   (value);
    end;
  end;

//-----------------------------------------------------------------------------:T_sudokuRiddle
//T_config:-----------------------------------------------------------------------------------
FUNCTION configFileName:string;
  begin
    result:=ChangeFileExt(paramStr(0),'.cfg');
  end;

CONSTRUCTOR T_config.create;
  VAR i,j:byte;
  begin
    riddle.create;
    storedRiddles.create;
    if not(loadFromFile(configFileName)) then begin
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
        symmetries:=[sym_center,sym_x,sym_y];
        diff  :=5;
      end;
      for i:=0 to 6 do begin
        for j:=0 to 31 do with hallOfFame[i,j] do begin
          name:='';
          time:=C_firstTime+j;
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
    storedRiddles.ensureBackgroundThread;
  end;

DESTRUCTOR T_config.destroy;
  begin
    storedRiddles.destructionPending:=true;
    saveToFile(configFileName);
    storedRiddles.destroy;
  end;

FUNCTION T_config.getSerialVersion: dword;
  begin
    result:=34562390;
  end;

FUNCTION T_config.loadFromStream(VAR stream: T_bufferedInputStreamWrapper): boolean;
  VAR i,j:byte;
  begin
    if not(inherited) then exit(false);
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
      i:=stream.readByte;
      if i in [0..7] then begin
        symmetries:=[];
        if (i and 1)=1 then include(symmetries,sym_x);
        if (i and 2)=2 then include(symmetries,sym_y);
        if (i and 4)=4 then include(symmetries,sym_center);
      end else exit(false);
      diff      :=stream.readByte;
    end;
    for i:=0 to 6 do for j:=0 to 31 do with hallOfFame[i,j] do begin
      name:=stream.readAnsiString;
      time:=stream.readDouble;  result:=result and (time>0);
      given:=stream.readWord;   result:=result and (given>0) and (given<=sqr(C_sudokuStructure[i].size));
      markErrors:=stream.readBoolean;
    end;
    result:=result and riddle.loadFromStream(stream);
    result:=result and storedRiddles.loadFromStream(stream);
    gameIsDone:=stream.readBoolean;
  end;

PROCEDURE T_config.saveToStream(VAR stream: T_bufferedOutputStreamWrapper);
  VAR i,j:byte;
  begin
    inherited;
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
      i:=0;
      if sym_x      in symmetries then inc(i);
      if sym_y      in symmetries then inc(i,2);
      if sym_center in symmetries then inc(i,4);
      stream.writeByte(i);
      stream.writeByte (diff      );
    end;
    for i:=0 to 6 do for j:=0 to 31 do with hallOfFame[i,j] do begin
      stream.writeAnsiString (name);
      stream.writeDouble (time);
      stream.writeWord   (given);
      stream.writeBoolean(markErrors);
    end;
    riddle.saveToStream(stream);
    storedRiddles.saveToStream(stream);
    stream.writeBoolean(gameIsDone);
  end;

FUNCTION T_config.isGoodEnough(CONST modeIdx: byte; CONST newEntry: hallOfFameEntry
  ): boolean;
  begin
    result:=isBetterThan(newEntry,
                         hallOfFame[modeIdx,19]);
  end;

PROCEDURE T_config.addHOFEntry(CONST modeIdx: byte; CONST newEntry: hallOfFameEntry);
  VAR tmp:hallOfFameEntry;
      i  :byte;
  begin
    i:=19;
    hallOfFame[modeIdx,19]:=newEntry;
    while (i>0) and isBetterThan(
                      hallOfFame[modeIdx,i],
                      hallOfFame[modeIdx,i-1]) do begin
      tmp:=hallOfFame[modeIdx,i];
      hallOfFame[modeIdx,i]:=hallOfFame[modeIdx,i-1];
      hallOfFame[modeIdx,i-1]:=tmp;
      dec(i);
    end;
  end;
//-----------------------------------------------------------------------------------:T_config

FINALIZATION
  if tempImage<>nil then FreeAndNil(tempImage);

end.
