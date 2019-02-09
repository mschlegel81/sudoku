//{$define debugMode}
UNIT buffiles;
{Unit zum Umgang mit Dateien inkl. automatischen Casts auf die Basistypen
Autor: Martin Schlegel
Datum: erstellt irgendwann 2005
       03.05.2006 - bufferedFile: Kommentare und Methoden für serielle Zugriffe hinzugefügt
                  - typeSignature erstellt
}
INTERFACE
USES dos;
TYPE
  bufferedFile=object
    //gepufferte Datei mit einer maximalen Größe von 4GB; Größe ist Bytegenau; Puffer ist höchstens 4MB groß
    private
      fil:file of byte;   //die eigentliche Datei
      buf_size,           //Größe des Puffers
      buf_fill,           //Füllstand des Puffers
      filepos:longint;    //Position in der Datei, die der Pufferposition 0 entspricht
      open:boolean;       //Flag, true, wenn die Datei geöffnet ist
      buf :PByte;         //der Puffer
      readonly:boolean;   //Flag, true wenn Datei nicht beschrieben werden darf (erstellt über Konstruktor createReadOnly)
      seekIndex:longint;
      PROCEDURE clearBuffer(idx0,idx1:longint); //setzt den Puffer von Index idx0 bis idx1 auf null
      PROCEDURE writeBuffer;                    //schreibt den Puffer zurück in die Datei
      PROCEDURE readBuffer(idx:longint);        //liest die Datei ab dem übergebenen Index in den Puffer
    public
      CONSTRUCTOR create        (filename:string; buffersize:longint);
      CONSTRUCTOR createReadOnly(filename:string; buffersize:longint);
      FUNCTION  exist:boolean;
      PROCEDURE rewriteFile;
      PROCEDURE resetFile;
      PROCEDURE closeFile;
      FUNCTION  endOfFile:boolean;
      FUNCTION  size:longint;

      //Methoden für Zugriff per Index
      FUNCTION  readByte    (VAR idx:longint)      :byte;     overload; //lesen einer Variable vom Typ     byte    vom Dateiindex idx
      PROCEDURE writeByte   (VAR idx:longint; value:byte);    overload; //schreiben einer Variable vom Typ byte    an Dateiindex idx
      FUNCTION  readBoolean (VAR idx:longint)      :boolean;  overload; //lesen einer Variable vom Typ     boolean vom Dateiindex idx
      PROCEDURE writeBoolean(VAR idx:longint; value:boolean); overload; //schreiben einer Variable vom Typ boolean an Dateiindex idx
      FUNCTION  readWord    (VAR idx:longint)      :word;     overload; //lesen einer Variable vom Typ     word    vom Dateiindex idx
      PROCEDURE writeWord   (VAR idx:longint; value:word);    overload; //schreiben einer Variable vom Typ word    an Dateiindex idx
      FUNCTION  readDWord   (VAR idx:longint)      :Dword;    overload; //lesen einer Variable vom Typ     Dword   vom Dateiindex idx
      PROCEDURE writeDWord  (VAR idx:longint; value:Dword);   overload; //schreiben einer Variable vom Typ Dword   an Dateiindex idx
      FUNCTION  readQWord   (VAR idx:longint)      :Qword;    overload; //lesen einer Variable vom Typ     Qword   vom Dateiindex idx
      PROCEDURE writeQWord  (VAR idx:longint; value:Qword);   overload; //schreiben einer Variable vom Typ Qword   an Dateiindex idx
      FUNCTION  readLongint (VAR idx:longint)      :longint;  overload; //lesen einer Variable vom Typ     longint vom Dateiindex idx
      PROCEDURE writeLongint(VAR idx:longint; value:longint); overload; //schreiben einer Variable vom Typ longint an Dateiindex idx
      FUNCTION  readChar    (VAR idx:longint)      :Char;     overload; //lesen einer Variable vom Typ     Char    vom Dateiindex idx
      PROCEDURE writeChar   (VAR idx:longint; value:char);    overload; //schreiben einer Variable vom Typ char    an Dateiindex idx
      FUNCTION  readsingle  (VAR idx:longint)      :single;   overload; //lesen einer Variable vom Typ     single  vom Dateiindex idx
      PROCEDURE writesingle (VAR idx:longint; value:single);  overload; //schreiben einer Variable vom Typ single  an Dateiindex idx
      FUNCTION  readdouble  (VAR idx:longint)      :double;   overload; //lesen einer Variable vom Typ     double  vom Dateiindex idx
      PROCEDURE writedouble (VAR idx:longint; value:double);  overload; //schreiben einer Variable vom Typ double  an Dateiindex idx
      FUNCTION  readString  (VAR idx:longint)      :string;   overload; //lesen einer Variable vom Typ     string  vom Dateiindex idx
      PROCEDURE writeString (VAR idx:longint; value:string);  overload; //schreiben einer Variable vom Typ string  an Dateiindex idx

      //Methoden für seriellen Zugriff
      PROCEDURE seekPos(position:longint);             //gehe zu übergebener Position in der Datei
      FUNCTION  getPos:longint;                        //gibt aktuelle Position des Schreib-/Lesezeigers in der Datei zurück
      FUNCTION  readByte          :byte;     overload; //lesen einer Variable vom Typ     byte    von aktueller Position
      PROCEDURE writeByte   (value:byte);    overload; //schreiben einer Variable vom Typ byte    an  aktueller Position
      FUNCTION  readBoolean       :boolean;  overload; //lesen einer Variable vom Typ     boolean von aktueller Position
      PROCEDURE writeBoolean(value:boolean); overload; //schreiben einer Variable vom Typ boolean an  aktueller Position
      FUNCTION  readWord          :word;     overload; //lesen einer Variable vom Typ     word    von aktueller Position
      PROCEDURE writeWord   (value:word);    overload; //schreiben einer Variable vom Typ word    an  aktueller Position
      FUNCTION  readDWord         :Dword;    overload; //lesen einer Variable vom Typ     Dword   von aktueller Position
      PROCEDURE writeDWord  (value:Dword);   overload; //schreiben einer Variable vom Typ Dword   an  aktueller Position
      FUNCTION  readQWord         :Qword;    overload; //lesen einer Variable vom Typ     Qword   von aktueller Position
      PROCEDURE writeQWord  (value:Qword);   overload; //schreiben einer Variable vom Typ Qword   an  aktueller Position
      FUNCTION  readLongint       :longint;  overload; //lesen einer Variable vom Typ     longint von aktueller Position
      PROCEDURE writeLongint(value:longint); overload; //schreiben einer Variable vom Typ longint an  aktueller Position
      FUNCTION  readChar          :Char;     overload; //lesen einer Variable vom Typ     Char    von aktueller Position
      PROCEDURE writeChar   (value:char);    overload; //schreiben einer Variable vom Typ char    an  aktueller Position
      FUNCTION  readsingle        :single;   overload; //lesen einer Variable vom Typ     single  von aktueller Position
      PROCEDURE writesingle (value:single);  overload; //schreiben einer Variable vom Typ single  an  aktueller Position
      FUNCTION  readdouble        :double;   overload; //lesen einer Variable vom Typ     double  von aktueller Position
      PROCEDURE writedouble (value:double);  overload; //schreiben einer Variable vom Typ double  an  aktueller Position
      FUNCTION  readString        :string;   overload; //lesen einer Variable vom Typ     string  von aktueller Position
      PROCEDURE writeString (value:string);  overload; //schreiben einer Variable vom Typ string  an  aktueller Position
      DESTRUCTOR destroy;
  end;

  serializable=object
  //Interface, das das Schreiben in und Lesen aus Objekten vom Typ bufferedFile unterstützt
    CONSTRUCTOR notReallyAConstructor; //formell Vorhanden um Fehlermeldung wg. "virtual" zu vermeiden
    FUNCTION  loadFromFile(filename:string):boolean;                       overload; //liest die Inhalte des Objektes aus der Datei mit dem übergebenen Namen und gibt true zurück gdw. kein Fehler auftrat
    PROCEDURE saveToFile(filename:string);                                 overload; //schreibt die Inhalte des Objektes in die Datei mit dem übergebenen Namen
    FUNCTION  loadFromFile(VAR F:bufferedFile):boolean; virtual; abstract; overload; //liest die Inhalte des Objektes aus einer bereits geöffneten Datei und gibt true zurück gdw. kein Fehler auftrat
    PROCEDURE saveToFile(VAR F:bufferedFile);           virtual; abstract; overload; //schreibt die Inhalte des Objektes in eine bereits geöffnete Datei
    FUNCTION  defaultFilesize:longint;                  virtual; abstract;           //gibt die Puffergröße (=übliche Dateigröße) an
  end;


FUNCTION Exists(filename:string):boolean;
IMPLEMENTATION
FUNCTION Exists(filename:string):boolean;
  VAR dir:searchRec;
  begin
    FindFirst(fileName,archive,Dir);
    result:=(dosError=0);
    FindClose(Dir);
  end;
//======================================================================
PROCEDURE bufferedFile.clearBuffer(idx0,idx1:longint);
  VAR i:longint;
  begin
    if (idx0>=0)and (idx0<=idx1) and (idx1<buf_size) //Falls korrekte Grenzen übergeben
      then for i:=idx0 to idx1 do (buf+i)^:=0; //Setze Puffereinträge auf null
  end;

PROCEDURE bufferedFile.writeBuffer;
  begin
    if (filepos>=0) and not(readonly) then begin
      seek(fil,filepos);             //Gehe zu der Position der Datei, ab der der Puffer beginnt
      blockwrite(fil,buf^,buf_fill); //schreibe den Puffer in die Datei
    end;
  end;

PROCEDURE bufferedFile.readBuffer(idx:longint);
  begin
    if idx>=0 then begin
      if not(open) then resetFile;
      if (idx>=filesize(fil)) and not(readonly) then begin     //Falls auf eine Position hinter dem Dateiende zugegriffen werden soll...
        clearbuffer(0,buf_size-1);          //Leere gesamten Puffer
        seek(fil,filesize(fil));            //gehe zum Dateiende
        while idx>filesize(fil)-buf_size do //solange index größer ist, als die Datei nach Hinzufügen eines kompletten Puffers wäre
          blockwrite(fil,buf^,buf_size);      //schreibe Puffer
        if idx>filesize(fil)                //falls Datei immer noch nicht groß genug
          then blockwrite(fil,buf^,idx-filesize(fil)); //schreibe verbleibenden Pufferanteil
      end; //if idx>...
      filepos:=idx;                       //speichere Position
      seek(fil,filepos);                  //gehe zu Anfangsposition in der Datei
      if filesize(fil)-idx<buf_size       //Falls nicht der gesamte Puffer gefüllt werden kann
        then buf_fill:=filesize(fil)-idx    //setze Pufferfüllstand auf Differenz zwischen Anfangsposition und Dateiende
        else buf_fill:=buf_size;            //sonst setze Pufferfüllstand auf Maximum
      if buf_fill>0                       //Falls mindestens ein Byte gelesen werden soll
        then blockread(fil,buf^,buf_fill);  //lies entsprechende Anzahl von Bytes aus Datei in Puffer
      clearBuffer(buf_fill,buf_size-1);   //Setze restlichen Puffer auf null
    end;
  end;

CONSTRUCTOR bufferedFile.create(filename:string; buffersize:longint);
  begin
    if (buffersize>41943034) or (buffersize<=0) //falls eine ungültige Puffergröße übergeben wurde
      then buffersize:=4194304; //setze Puffergröße auf 1MB
    assign(fil,filename);     //Weise der Datei den übergebenen Dateinamen zu
    buf_size:=buffersize;     //Speichere Puffergröße
    getMem(buf,buf_Size);     //Bereite Puffer vor
    clearbuffer(0,buf_Size-1); //Leere Puffer
    open:=false;              //setze Flag auf "Datei nicht geöffnet"
    filepos:=0;
    buf_fill:=0;
    readonly:=false;
  end;

CONSTRUCTOR bufferedFile.createReadOnly(filename:string; buffersize:longint);
  begin
    create(filename,buffersize);
    readonly:=true;
  end;

FUNCTION  bufferedFile.exist:boolean;
  begin
    if open then exist:=true else begin //Falls Datei geöffnet ist, so existiert sie, sonst:
      {$i-}       // I/O-Check auschalten
      reset(fil); // Datei öffnen
      {$i+}       // I/O-Check einschalten
      if ioresult=0 then begin //Falls beim öffnen der Datei kein Fehler auftrat
        close(fil);  //Datei schließen
        exist:=true; //Datei existiert
      end else exist:=false; //Falls beim Öffnen der Datei ein Fehler auftrat, so existiert sie nicht
    end;
  end;

PROCEDURE bufferedFile.rewriteFile;
  begin
    if (readonly) then resetFile else begin
      if not(open)         //Falls Datei noch nicht geöffnet
        then rewrite(fil); //Öffne Datei zum Schreiben neu
      open:=true;          //Setze Flag auf "Datei geöffnet"
    end;
  end;

PROCEDURE bufferedFile.resetFile;
  begin
    if not(open) then begin       //Falls Datei noch nicht geöffnet
      if exist then reset(fil)    //Falls Datei existiert, öffne existierende Datei
               else begin readOnly:=false; rewrite(fil) end; //sonst öffne Datei zum Schreiben neu
    end;
    open:=true;                   //Setze Flag auf "Datei geöffnet"
    readbuffer(0);
  end;

PROCEDURE bufferedFile.closeFile;
  begin
    writeBuffer; //Schreibe Puffer in Datei
    close(fil);  //schließe Datei
    open:=false; //setze Flag auf "Datei nicht geöffnet"
  end;

FUNCTION  bufferedFile.endOfFile:boolean;
  begin endOfFile:=(filepos+buf_fill>=filesize(fil)); end;

FUNCTION  bufferedFile.size:longint;
  begin
    if filesize(fil)>filepos+buf_fill then size:=filesize(fil)
                                      else size:=filepos+buf_fill;
  end;

FUNCTION  bufferedFile.readByte(VAR idx:longint):byte;
  begin
    if (idx<filepos) or           //falls zu schreibender Eintrag vor...
       (idx>=filepos+buf_size)    //oder hinter dem gepuffertem Bereich liegt,
    then begin
      if (idx>=filepos+buf_size)  //falls neuer Index hinter dem bisherigem Puffer liegt,
        then buf_fill:=buf_size;  //wird der aktuelle Puffer als vollständig gefüllt angesehen
      writeBuffer;                //schreibe aktuellen Puffer in die Datei
      readBuffer(idx);            //lies neuen Puffer ab gesuchtem Index ein
    end;
    if buf_fill<=idx-filepos then //Falls Pufferfüllstand hinter auszulesendem Index liegt
       buf_fill:=idx-filepos+1;   //aktualisiere Pufferfüllstand
    inc(idx);
    readByte:=(buf+idx-1-filepos)^;  //gib entsprechenden Eintrag des Puffers zurück
  end;

PROCEDURE bufferedFile.writeByte(VAR idx:longint; value:byte);
  begin
    if (idx<filepos) or           //falls zu schreibender Eintrag vor...
       (idx>=filepos+buf_size)    //oder hinter dem gepuffertem Bereich liegt,
    then begin
      if (idx>=filepos+buf_size)  //falls neuer Index hinter dem bisherigem Puffer liegt,
        then buf_fill:=buf_size;  //wird der aktuelle Puffer als vollständig gefüllt angesehen
      writeBuffer;                //schreibe aktuellen Puffer in die Datei
      readBuffer(idx);            //lies neuen Puffer ab gesuchtem Index ein
    end;
    if buf_fill<=idx-filepos then //Falls Pufferfüllstand hinter auszulesendem Index liegt
       buf_fill:=idx-filepos+1;   //aktualisiere Pufferfüllstand
    (buf+idx-filepos)^:=value;     //Schreibe Datum in Puffer
    inc(idx);
  end;

FUNCTION  bufferedFile.readWord(VAR idx:longint):word;
  begin
    readWord:=readByte(idx) or
             (readByte(idx) shl 8);
  end;
FUNCTION  bufferedFile.readDWord(VAR idx:longint):Dword;
  VAR aid:array[0..3] of byte;
      res:DWord;
      i:byte;
  begin
    for i:=0 to 3 do aid[i]:=readByte(idx);
    move(aid,res,4);
    readDWord:=res;
  end;

FUNCTION  bufferedFile.readQWord(VAR idx:longint):Qword;
  VAR aid:array[0..7] of byte;
      res:QWord;
      i:byte;
  begin
    for i:=0 to 7 do aid[i]:=readByte(idx);
    move(aid,res,8);
    readQWord:=res;
  end;

FUNCTION  bufferedFile.readLongint(VAR idx:longint):longint;
  VAR aid:array[0..3] of byte;
      res:longint;
      i:byte;
  begin
    for i:=0 to 3 do aid[i]:=readByte(idx);
    move(aid,res,4);
    readLongint:=res;
  end;

FUNCTION  bufferedFile.readChar(VAR idx:longint):Char;
  begin readChar:=chr(readbyte(idx)); end;

FUNCTION  bufferedFile.readsingle(VAR idx:longint):single;
  VAR aid:array[0..3] of byte;
      res:single;
      i:byte;
  begin
    for i:=0 to 3 do aid[i]:=readByte(idx);
    move(aid,res,4);
    readSingle:=res;
  end;

FUNCTION  bufferedFile.readdouble(VAR idx:longint):double;
  VAR aid:array[0..7] of byte;
      res:double;
      i:byte;
  begin
    for i:=0 to 7 do aid[i]:=readByte(idx);
    move(aid,res,8);
    readDouble:=res;
  end;

FUNCTION  bufferedFile.readBoolean (VAR idx:longint)      :boolean;
  begin readBoolean:=readByte(idx)>0; end;

FUNCTION  bufferedFile.readString  (VAR idx:longint)      :string;
  VAR l,i:byte;
      res:string;
  begin
    l:=readbyte(idx);
    res:='';
    if l>0 then for i:=1 to l do res:=res+readchar(idx);
    readString:=res;
  end;

PROCEDURE bufferedFile.writeWord(VAR idx:longint; value:word);
  VAR aid:array[0..1] of byte;
      i:byte;
  begin
    move(value,aid,2);
    for i:=0 to 1 do writeByte(idx,aid[i]);
  end;

PROCEDURE bufferedFile.writeDWord(VAR idx:longint; value:Dword);
  VAR aid:array[0..3] of byte;
      i:byte;
  begin
    move(value,aid,4);
    for i:=0 to 3 do writeByte(idx,aid[i]);
  end;

PROCEDURE bufferedFile.writeQWord(VAR idx:longint; value:Qword);
  VAR aid:array[0..7] of byte;
      i:byte;
  begin
    move(value,aid,8);
    for i:=0 to 7 do writeByte(idx,aid[i]);
  end;

PROCEDURE bufferedFile.writeLongint(VAR idx:longint; value:longint);
  VAR aid:array[0..3] of byte;
      i:byte;
  begin
    move(value,aid,4);
    for i:=0 to 3 do writeByte(idx,aid[i]);
  end;

PROCEDURE bufferedFile.writeChar(VAR idx:longint; value:char);
  begin writeByte(idx,ord(value)); end;

PROCEDURE bufferedFile.writesingle(VAR idx:longint; value:single);
  VAR aid:array[0..3] of byte;
      i:byte;
  begin
    move(value,aid,4);
    for i:=0 to 3 do writeByte(idx,aid[i]);
  end;

PROCEDURE bufferedFile.writedouble(VAR idx:longint; value:double);
  VAR aid:array[0..7] of byte;
      i:byte;
  begin
    move(value,aid,8);
    for i:=0 to 7 do writeByte(idx,aid[i]);
  end;

PROCEDURE bufferedFile.writeBoolean(VAR idx:longint; value:boolean);
  begin
    if value then writeByte(idx,255)
             else writeByte(idx,0);
  end;

PROCEDURE bufferedFile.writeString (VAR idx:longint; value:string);
  VAR i:byte;
  begin
    writeByte(idx,length(value));
    if length(value)>0 then for i:=1 to length(value) do
      writeChar(idx,value[i]);
  end;

DESTRUCTOR bufferedFile.destroy;
  begin
     if open then closeFile;
     freemem(buf,buf_size);
  end;

PROCEDURE bufferedFile.seekPos(position:longint);
//gehe zu übergebener Position in der Datei
  begin seekIndex:=position; end;

FUNCTION  bufferedFile.getPos:longint;
//gibt aktuelle Position des Schreib-/Lesezeigers in der Datei zurück
  begin getPos:=seekIndex; end;
FUNCTION  bufferedFile.readByte          :byte;
//lesen einer Variable vom Typ     byte    von aktueller Position
begin readByte    :=readByte    (seekIndex);       end;
PROCEDURE bufferedFile.writeByte   (value:byte);
//schreiben einer Variable vom Typ byte    an  aktueller Position
begin writeByte                 (seekIndex,value); end;
FUNCTION  bufferedFile.readBoolean       :boolean;
//lesen einer Variable vom Typ     boolean von aktueller Position
begin readBoolean :=readBoolean (seekIndex);       end;
PROCEDURE bufferedFile.writeBoolean(value:boolean);
//schreiben einer Variable vom Typ boolean an  aktueller Position
begin writeBoolean              (seekIndex,value); end;
FUNCTION  bufferedFile.readWord          :word;
//lesen einer Variable vom Typ     word    von aktueller Position
begin readWord    :=readWord    (seekIndex);       end;
PROCEDURE bufferedFile.writeWord   (value:word);
//schreiben einer Variable vom Typ word    an  aktueller Position
begin writeWord                 (seekIndex,value); end;
FUNCTION  bufferedFile.readDWord         :Dword;
//lesen einer Variable vom Typ     Dword   von aktueller Position
begin readDWord   :=readDWord   (seekIndex);       end;
PROCEDURE bufferedFile.writeDWord  (value:Dword);
//schreiben einer Variable vom Typ Dword   an  aktueller Position
begin writeDWord                (seekIndex,value); end;
FUNCTION  bufferedFile.readQWord         :Qword;
//lesen einer Variable vom Typ     Qword   von aktueller Position
begin readQWord   :=readQWord   (seekIndex);       end;
PROCEDURE bufferedFile.writeQWord  (value:Qword);
//schreiben einer Variable vom Typ Qword   an  aktueller Position
begin writeQWord                (seekIndex,value); end;
FUNCTION  bufferedFile.readLongint       :longint;
//lesen einer Variable vom Typ     longint von aktueller Position
begin readLongint :=readLongint (seekIndex);       end;
PROCEDURE bufferedFile.writeLongint(value:longint);
//schreiben einer Variable vom Typ longint an  aktueller Position
begin writeLongint              (seekIndex,value); end;
FUNCTION  bufferedFile.readChar          :Char;
//lesen einer Variable vom Typ     Char    von aktueller Position
begin readChar    :=readChar    (seekIndex);       end;
PROCEDURE bufferedFile.writeChar   (value:char);
//schreiben einer Variable vom Typ char    an  aktueller Position
begin writeChar                 (seekIndex,value); end;
FUNCTION  bufferedFile.readsingle        :single;
//lesen einer Variable vom Typ     single  von aktueller Position
begin readsingle  :=readsingle  (seekIndex);       end;
PROCEDURE bufferedFile.writesingle (value:single);
//schreiben einer Variable vom Typ single  an  aktueller Position
begin writesingle               (seekIndex,value); end;
FUNCTION  bufferedFile.readdouble        :double;
//lesen einer Variable vom Typ     double  von aktueller Position
begin readdouble  :=readdouble  (seekIndex);       end;
PROCEDURE bufferedFile.writedouble (value:double);
//schreiben einer Variable vom Typ double  an  aktueller Position
begin writedouble               (seekIndex,value); end;
FUNCTION  bufferedFile.readString        :string;
//lesen einer Variable vom Typ     string  von aktueller Position
begin readString  :=readString  (seekIndex);       end;
PROCEDURE bufferedFile.writeString (value:string);
//schreiben einer Variable vom Typ string  an  aktueller Position
begin writeString               (seekIndex,value); end;
//---------------------------------------------------------------------:bufferedFile
//implementation of serializable:---------------------------------------------------
CONSTRUCTOR serializable.notReallyAConstructor; begin end;
FUNCTION serializable.loadFromFile(filename:string):boolean;
//liest die Inhalte des Objektes aus der Datei mit dem übergebenen Namen und gibt true zurück gdw. kein Fehler auftrat
  VAR f:bufferedFile;
  begin
    result:=true;
    if exists(filename) then begin
      {$ifdef debugMode}
      writeln('file "',filename,'" exists - trying to load');
      {$endif}
      try
        f.createReadOnly(filename,defaultFileSize);
        f.resetFile;
        result:=loadFromFile(f);
        {$ifdef debugMode}
        if result then writeln('loading successful')
                  else writeln('error while loading');
        {$endif}
        f.closeFile;
        f.destroy;
      except
        result:=false;
      end;
    end else begin
      result:=false;
      {$ifdef debugMode}
      writeln('file "',filename,'" does not exist');
      {$endif}
    end;
  end;

PROCEDURE serializable.saveToFile(filename:string);
//schreibt die Inhalte des Objektes in die Datei mit dem übergebenen Namen
  VAR f:bufferedFile;
  begin
    try
      f.create(filename,defaultFileSize);
      f.rewriteFile;
      saveToFile(f);
      f.closeFile;
      f.destroy;
    except
      {$ifdef debugMode}
      writeln('error saving to file "',filename,'"');
      {$endif}
    end;
  end;
//---------------------------------------------------------------------------:serializable

begin

end.
