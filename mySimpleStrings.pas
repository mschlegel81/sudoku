UNIT mySimpleStrings;
INTERFACE
USES Classes;
TYPE stringArray=array of string;

FUNCTION natToString(x:QWord):string;
FUNCTION intToString(x:longint):string;
FUNCTION fltToString(x:Extended; decals:byte):string;
FUNCTION dateToString(year,month,day:word; format:byte):string;
FUNCTION timeToString(hour,minute,second,decals:word; format:byte):string;

FUNCTION stringToNat(s:string):QWord;
FUNCTION stringToInt(s:string):longint;
FUNCTION stringToFlt(s:string):extended;

FUNCTION switchDecimalSign(s:string):string;
FUNCTION replaceSign(s:string; cOld,cNew:char):string;
FUNCTION fillLeft(s:string; filler:char; resLength:byte):string;
FUNCTION fillRight(s:string; filler:char; resLength:byte):string;
FUNCTION removeSign(s:string; idx:byte):string;

FUNCTION smartSizeString(byteSize:QWord):string;

FUNCTION concatStringArrays(s1,s2:stringArray):stringArray;
PROCEDURE saveAsPlainText(filename:string; sa:stringArray);

FUNCTION filenameWithoutPath(x:string):string;

IMPLEMENTATION
FUNCTION natToString(x:QWord):string;
  VAR res:string; begin str(x,res); natToString:=res; end;

FUNCTION intToString(x:longint):string;
  VAR res:string; begin str(x,res); intToString:=res; end;

FUNCTION fltToString(x:Extended; decals:byte):string;
  VAR res:string; begin str(x:0:decals,res); fltToString:=res; end;
  
FUNCTION dateToString(year,month,day:word; format:byte):string;
  CONST monthnames:array[0..1,1..12] of string=
        (('Januar','Februar','März','April','Mai','Juni','Juli','August','September','Oktober','November','Dezember'),
         ('January','Febuary','March','April','May','June','July','August','September','October','November','December'));
  begin
    if month<1 then month:=1;
    if month>12 then month:=12;
    case format of
        1: dateToString:=fillLeft(intToString(day)  ,'0',2)+'.'
                        +fillLeft(intToString(month),'0',2)+'.'
                        +fillLeft(intToString(year) ,'0',4);
        2: dateToString:=fillLeft(intToString(year) ,'0',4)+'/'
                        +fillLeft(intToString(month),'0',2)+'/'
                        +fillLeft(intToString(day)  ,'0',2);
        3: dateToString:=intToString(day)+'.'+monthnames[0,month]+' '+intToString(year);
        4: dateToString:=monthnames[1,month]+' '+intToString(day)+', '+intToString(year);
      else dateToString:=intToString(day)+'.'+intToString(month)+'.'+intToString(year);
    end;
  end;

FUNCTION timeToString(hour,minute,second,decals:word; format:byte):string;
  begin
    case format of
        1: timeToString:=fillLeft(intToString(hour)  ,'0',2)+':'
                        +fillLeft(intToString(minute),'0',2)+'.'
                        +fillLeft(intToString(second),'0',2);
        2: timeToString:=fillLeft(intToString(hour)  ,'0',2)+':'
                        +fillLeft(intToString(minute),'0',2)+'.'
                        +fillLeft(intToString(second),'0',2)+','
                        +fillLeft(intToString(decals),'0',2);
      else timeToString:=fillLeft(intToString(hour)  ,'0',2)+':'
                        +fillLeft(intToString(minute),'0',2);
    end;
  end;

FUNCTION stringToNat(s:string):QWord;
  VAR res:QWord;
      err:byte;
  begin
    val(s,res,err);
    stringToNat:=res;
  end;

FUNCTION stringToInt(s:string):longint;
  VAR res:longint;
      err:byte;
  begin
    val(s,res,err);
    stringToInt:=res;
  end;

FUNCTION stringToFlt(s:string):extended;
  VAR res:extended;
      err:byte;
  begin
    val(s,res,err);
    stringToFlt:=res;
  end;

FUNCTION switchDecimalSign(s:string):string;
  VAR i:byte;
  begin
    if length(s)>1 then for i:=1 to length(s) do
      if      s[i]='.' then s[i]:=','
      else if s[i]=',' then s[i]:='.';
    switchDecimalSign:=s;
  end;

FUNCTION replaceSign(s:string; cOld,cNew:char):string;
  VAR i:byte;
  begin
    if length(s)>1 then for i:=1 to length(s) do
      if s[i]=cOld then s[i]:=cNew;
    replaceSign:=s;
  end;

FUNCTION fillLeft(s:string; filler:char; resLength:byte):string;
  begin
    while length(s)<resLength do s:=filler+s;
    fillLeft:=s;
  end;

FUNCTION fillRight(s:string; filler:char; resLength:byte):string;
  begin
    while length(s)<resLength do s:=s+filler;
    fillRight:=s;
  end;
  
FUNCTION removeSign(s:string; idx:byte):string;
  begin
    if (idx>0) and (idx<=length(s)) then
      removeSign:=copy(s,1,idx-1)+copy(s,idx+1,length(s)-idx-2);
  end;

FUNCTION smartSizeString(byteSize:QWord):string;
  VAR res:string;
  begin
    if      byteSize<      2048 then res:=natToString(byteSize)+'bytes'
    else if byteSize<   2097152 then res:=fltToString(byteSize/      1024,2)+'kB'
    else if byteSize<2147483648 then res:=fltToString(byteSize/   1048576,2)+'MB'
    else                             res:=fltToString(byteSize/1073741824,2)+'GB';
    smartSizeString:=res;
  end;

FUNCTION concatStringArrays(s1,s2:stringArray):stringArray;
  VAR res:stringArray;
      i:longint;
  begin
    setLength(res,length(s1)+length(s2));
    for i:=0 to length(s1)-1 do res[i           ]:=s1[i];
    for i:=0 to length(s2)-1 do res[i+length(s1)]:=s2[i];
    concatStringArrays:=res;
  end;

PROCEDURE saveAsPlainText(filename:string; sa:stringArray);
  VAR f:textFile;
      i:longint;
  begin
    assign(f,filename);
    rewrite(f);
    if length(sa)>0 then for i:=0 to length(sa)-1 do
      writeln(f,sa[i]);
    close(f);
  end;

FUNCTION filenameWithoutPath(x:string):string;
  begin
    if pos('\',x)=0
      then filenameWithoutPath:=x
      else filenameWithoutPath:=filenameWithoutPath(copy(x,pos('\',x)+1,length(x)-pos('\',x)));
  end;

end.


