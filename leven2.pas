{ (c) W.J. Heeringa 2021 }

{$R+ $B+}
program calculate_Levenshtein_distance(input,output,stderr);

uses 
  sysutils, math;

const
  mp      = 1000;
  maxreal = 1E20;

type
  headtype       = array[1..3] of char;
  supratype      = array[1..2] of char;
  diacritictype  = array[1..2] of char;

  segmenttype    = record
                     head      : headtype;
                     supra     : array[1..4] of supratype;
                     diacritic : array[1..4] of diacritictype;
                   end;

  lettertype     = record
                     segment   : array[1..2] of segmenttype;
                   end;

  wordtype       = record
                     letter    : array[1..80] of lettertype;
                     number    : integer;
                   end;

  listtype       = record
                     w_rd      : array[1..30] of wordtype;
                     number    : integer;
                   end;

  comptype       = record
                     number    : integer;
                     segment   : array[1..mp] of segmenttype;
                     vctype    : array[1..mp] of char;
                     
                     total     : real;
                     vector    : array[1..mp] of real;
                     matrix    : array[1..mp] of array[1..mp] of real;
                   end;

  segmenttype0   = record
                     sound     : integer;
                   end;

  lettertype0    = record
                     segment   : array[1..2] of segmenttype0;
                   end;

  wordtype0      = record
                     letter    : array[1..80] of lettertype0;
                     number    : integer;
                   end;

  listtype0      = record
                     w_rd      : array[1..30] of wordtype0;
                     number    : integer;
                   end;

  matrixtype     = array[0..80,0..80] of real;

  tracetype      = array[0..80,0..80] of integer;

  alignrec       = record
                     word1     :  integer;
                     word2     :  integer;
                     dist      :  real;
                   end;

  aligntype      = array[1..80] of alignrec;

var
  comp0,comp1,comp2           : comptype;
  meanIndel,maxIndel          : real;
  meanSubst,maxSubst          : real;
  lg,sm                       : boolean;
  empty                       : lettertype0;
  matrix1,matrix2,matrix3     : matrixtype;
  trace                       : tracetype;
  align                       : aligntype;
  allDist,allNum              : real;
  nname,fname,iname,sname     : string[255];
  method,normal,part          : integer;
  r,r0                        : real;
  saveAl,saveIn,saveAg,saveSo : boolean;
  saveXX                      : boolean;
  fileAl,fileIn,fileAg,fileSo : text;
  filename,itemname           : string[255];
  path                        : string[255];

procedure openr(var fp:text;name:string);
begin
  assign(fp,name);
  {$I-}
  reset(fp);
  {$I+}

  if IOResult <> 0
    then begin
           writeln(stderr,'Error opening file ',name);
           halt;
         end
    else {nothing};
end;

procedure openw(var fp:text;name:string);
begin
  assign(fp,name);
  {$I-}
  rewrite(fp);
  {$I+}

  if IOResult <> 0
    then begin
           writeln(stderr,'Error opening file ',name);
           halt;
         end
    else {nothing};
end;

procedure usage;
begin
  writeln(stderr);
  writeln(stderr,'(c) W.J. Heeringa 2021');
  writeln(stderr);
  writeln(stderr,'Usage: ',ParamStr(0),' namfile filfile itmfile segfile 1|2 0|1|2 0-4 0|1 0|1 0|1 0|1 0|1 path');
  writeln(stderr);
  writeln(stderr,'namfile: file with names                        ');
  writeln(stderr,'filfile: file with files                        ');
  writeln(stderr,'itmfile: file with items                        ');
  writeln(stderr,'segfile: file with segment distances            ');
  writeln(stderr);
  writeln(stderr,'1: Classical                                    ');
  writeln(stderr,'2: PMI-based                                    ');
  writeln(stderr);
  writeln(stderr,'0: Do nothing'                                   );
  writeln(stderr,'1: Divide word pair distance by alignment length');
  writeln(stderr);
  writeln(stderr,'0: All'                                          ); 
  writeln(stderr,'1: Only vowel     substitutions                 ');
  writeln(stderr,'2: Only vowel     indels                        ');
  writeln(stderr,'3: Only consonant substitutions                 ');
  writeln(stderr,'4: Only consonant indels                        ');
  writeln(stderr);
  writeln(stderr,'0: Do nothing                                   ');
  writeln(stderr,'1: Save    sound    distances                   ');
  writeln(stderr);
  writeln(stderr,'0: Do nothing                                   ');
  writeln(stderr,'1: Save  alignments                             ');
  writeln(stderr);
  writeln(stderr,'0: Do nothing                                   ');
  writeln(stderr,'1: Save  word pair  distances                   ');
  writeln(stderr);
  writeln(stderr,'0: Do nothing                                   ');
  writeln(stderr,'1: Save  aggregated distances                   ');
  writeln(stderr);
  writeln(stderr,'path: path of working directory                 ');
  writeln(stderr);
  halt;
end;

procedure getparameters;
begin
  if (paramcount=12)
    then begin
           nname:=paramstr(1);
           fname:=paramstr(2);
           iname:=paramstr(3);
           sname:=paramstr(4);
           
           if paramstr( 5)='1' then method:=1     else
           if paramstr( 5)='2' then method:=2     else usage;
           
           if paramstr( 6)='0' then normal:=0     else
           if paramstr( 6)='1' then normal:=1     else usage;
           
           if paramstr( 7)='0' then part  :=0     else
           if paramstr( 7)='1' then part  :=1     else
           if paramstr( 7)='2' then part  :=2     else
           if paramstr( 7)='3' then part  :=3     else
           if paramstr( 7)='4' then part  :=4     else usage;
           
           if paramstr( 8)='0' then saveSo:=false else
           if paramstr( 8)='1' then saveSo:=true  else usage;
           
           if paramstr( 9)='0' then saveAl:=false else
           if paramstr( 9)='1' then saveAl:=true  else usage;
    
           if paramstr(10)='0' then saveIn:=false else
           if paramstr(10)='1' then saveIn:=true  else usage;
    
           if paramstr(11)='0' then saveAg:=false else
           if paramstr(11)='1' then saveAg:=true  else usage;
           
           path:=paramstr(12);
         end
    else usage;
end;

function inheads(ch:char):boolean;
begin
  if ch='0'
    then inheads:=true
    else
  if ch='i'
    then inheads:=true
    else
  if ch='y'
    then inheads:=true
    else
  if ch='1'
    then inheads:=true
    else
  if ch='}'
    then inheads:=true
    else
  if ch='M'
    then inheads:=true
    else
  if ch='u'
    then inheads:=true
    else
  if ch='I'
    then inheads:=true
    else
  if ch='Y'
    then inheads:=true
    else
  if ch='U'
    then inheads:=true
    else
  if ch='e'
    then inheads:=true
    else
  if ch='2'
    then inheads:=true
    else
  if ch='8'
    then inheads:=true
    else
  if ch='7'
    then inheads:=true
    else
  if ch='o'
    then inheads:=true
    else
  if ch='@'
    then inheads:=true
    else
  if ch='E'
    then inheads:=true
    else
  if ch='9'
    then inheads:=true
    else
  if ch='3'
    then inheads:=true
    else
  if ch='V'
    then inheads:=true
    else
  if ch='O'
    then inheads:=true
    else
  if ch='{'
    then inheads:=true
    else
  if ch='6'
    then inheads:=true
    else
  if ch='a'
    then inheads:=true
    else
  if ch='&'
    then inheads:=true
    else
  if ch='A'
    then inheads:=true
    else
  if ch='Q'
    then inheads:=true
    else
  if ch='p'
    then inheads:=true
    else
  if ch='b'
    then inheads:=true
    else
  if ch='t'
    then inheads:=true
    else
  if ch='d'
    then inheads:=true
    else
  if ch='c'
    then inheads:=true
    else
  if ch='k'
    then inheads:=true
    else
  if ch='g'
    then inheads:=true
    else
  if ch='q'
    then inheads:=true
    else
  if ch='?'
    then inheads:=true
    else
  if ch='m'
    then inheads:=true
    else
  if ch='F'
    then inheads:=true
    else
  if ch='n'
    then inheads:=true
    else
  if ch='J'
    then inheads:=true
    else
  if ch='N'
    then inheads:=true
    else
  if ch='r'
    then inheads:=true
    else
  if ch='4'
    then inheads:=true
    else
  if ch='B'
    then inheads:=true
    else
  if ch='f'
    then inheads:=true
    else
  if ch='v'
    then inheads:=true
    else
  if ch='T'
    then inheads:=true
    else
  if ch='D'
    then inheads:=true
    else
  if ch='s'
    then inheads:=true
    else
  if ch='z'
    then inheads:=true
    else
  if ch='S'
    then inheads:=true
    else
  if ch='Z'
    then inheads:=true
    else
  if ch='C'
    then inheads:=true
    else
  if ch='x'
    then inheads:=true
    else
  if ch='G'
    then inheads:=true
    else
  if ch='X'
    then inheads:=true
    else
  if ch='R'
    then inheads:=true
    else
  if ch='h'
    then inheads:=true
    else
  if ch='K'
    then inheads:=true
    else
  if ch='w'
    then inheads:=true
    else
  if ch='P'
    then inheads:=true
    else
  if ch='j'
    then inheads:=true
    else
  if ch='M'
    then inheads:=true
    else
  if ch='l'
    then inheads:=true
    else
  if ch='L'
    then inheads:=true
    else
  if ch=''''
    then inheads:=true
    else
  if ch='"'
    then inheads:=true
    else
  if ch='%'
    then inheads:=true
    else inheads:=false;
end;

function insupras(ch:char):boolean;
begin
  if ch=':'
    then insupras:=true
    else insupras:=false;
end;

function indiacritics(ch:char):boolean;
begin
  if ch='0'
    then indiacritics:=true
    else
  if ch='v'
    then indiacritics:=true
    else
  if ch='h'
    then indiacritics:=true
    else
  if ch='+'
    then indiacritics:=true
    else
  if ch='-'
    then indiacritics:=true
    else
  if ch='='
    then indiacritics:=true
    else
  if ch='t'
    then indiacritics:=true
    else
  if ch='k'
    then indiacritics:=true
    else
  if ch='w'
    then indiacritics:=true
    else
  if ch='j'
    then indiacritics:=true
    else
  if ch='G'
    then indiacritics:=true
    else
  if ch='?'
    then indiacritics:=true
    else
  if ch='e'
    then indiacritics:=true
    else
  if ch='a'
    then indiacritics:=true
    else
  if ch='~'
    then indiacritics:=true
    else
  if ch='^'
    then indiacritics:=true
    else
  if ch='}'
    then indiacritics:=true
    else
  if ch='r'
    then indiacritics:=true
    else
  if ch='o'
    then indiacritics:=true
    else
  if ch='O'
    then indiacritics:=true
    else
  if ch='c'
    then indiacritics:=true
    else
  if ch='/'
    then indiacritics:=true
    else
  if ch='\'
    then indiacritics:=true
    else
  if ch='`'
    then indiacritics:=true
    else
  if ch='X'
    then indiacritics:=true
    else indiacritics:=false;
end;

procedure halt0(string0:string);
begin
  writeln(stderr, 'Check transcription of variety ', filename, ' and item ', itemname, '.');
  halt;
end;

procedure readchar(var fp:text;var ch:char);
begin
  if eoln(fp)
    then ch:='$'
    else read(fp,ch);
end;

procedure initsegment(var segment:segmenttype);
var
  p,d : integer;
begin
  segment.head:='   ';

  for p:=1 to 4 do begin
    segment.supra[p]:='  ';
  end;

  for d:=1 to 4 do begin
    segment.diacritic[d]:='  ';
  end;
end;

procedure initletter(var letter:lettertype);
var
  s : integer;
begin
  for s:=1 to 2 do begin
    initsegment(letter.segment[s]);
  end;
end;

procedure rhead(var fp:text;var head:headtype;var ch:char);
begin
  if inheads(ch)
    then begin
           head[1]:=ch;
           readchar(fp,ch);
           if  ch='\'
             then begin
                    head[2]:=ch;
                    readchar(fp,ch);
                  end
             else {nothing};
           if (ch='`') or (ch='<')
             then begin
                    head[3]:=ch;
                    readchar(fp,ch);
                  end
             else {nothing}
         end
    else halt0('head expected')
end;

procedure rsupra(var fp:text;var supra:supratype;var ch:char);
begin
  if ch=':'
    then begin
           supra[1]:=ch;
           readchar(fp,ch);
           
           if ch='\'
             then begin
                    supra[2]:=ch;
                    readchar(fp,ch);
                  end
             else {nothing};
         end
    else halt0(': expected');
end;

procedure rdiacritic(var fp:text;var diacritic:diacritictype;var ch:char);
begin
  if ch='_'
    then begin
           readchar(fp,ch);
           if indiacritics(ch)
             then begin
                    diacritic[1]:=ch;
                    readchar(fp,ch);
                    
                    if ch='\'
                      then begin
                             diacritic[2]:=ch;
                             readchar(fp,ch);
                           end
                      else {nothing};
                  end
             else halt0('diacritic expected');

         end
    else halt0('_ expected');
end;

procedure rsegment(var fp:text;var segment:segmenttype;var ch:char);
var
  p,d : integer;
begin
  p:=0;
  d:=0;

  while insupras(ch) or (ch='_') do begin
    if insupras(ch)
      then begin
             inc(p);
             rsupra(fp,segment.supra[p],ch)
           end
      else begin
             inc(d);
             rdiacritic(fp,segment.diacritic[d],ch);
           end;
  end;
end;

procedure rletter(var fp:text;var letter:lettertype;var ch:char);
var
  s : integer;
begin
  s:=1;

  initletter(letter);
  if inheads(ch)
    then begin
           rhead(fp,letter.segment[s].head,ch);
           rsegment(fp,letter.segment[s],ch)
         end
    else if ch='['
          then begin
                 readchar(fp,ch);
                 rhead(fp,letter.segment[s].head,ch);

                 while inheads(ch) do begin
                   inc(s);
                   rhead(fp,letter.segment[s].head,ch);
                 end;

                 if ch=']'
                   then readchar(fp,ch)
                   else halt0('] expected');
                   
                 rsegment(fp,letter.segment[1],ch)  
               end
          else halt0('stress or head or [ expected');
end;

procedure rword(var fp:text;var w_rd:wordtype;var ch:char);
var
  l : integer;
begin
  l:=1;
  rletter(fp,w_rd.letter[l],ch);
  while inheads(ch) or (ch='[') do begin
    inc(l);
    rletter(fp,w_rd.letter[l],ch);
  end;
  w_rd.number:=l;
end;

procedure rlist(var fp:text; var list:listtype;var ch:char);
var
  w : integer;
begin
  w:=1;
  rword(fp,list.w_rd[w],ch);
  while ch=' ' do begin
    inc(w);
    readchar(fp,ch);
    if ch='/'
      then begin
             readchar(fp,ch);
             if ch=' '
               then begin
                      readchar(fp,ch);
                      rword(fp,list.w_rd[w],ch)
                    end
               else halt0('space expected')
           end
      else halt0('/ expected')
  end;
  list.number:=w;
end;

function sound(segment:segmenttype):integer;
var
  t     : integer;
  found : boolean;
begin
  t:=0;
  found:=false;

  while (not found) and (t<comp1.number) do begin
    t:=t+1;
    found:=(segment.head=comp1.segment[t].head);
  end;

  if not found
    then begin
           writeln(stderr,segment.head,' not found in segment distance table');
           halt;
         end
    else {nothing};

  sound:=t;
end;

procedure change(list:listtype;var list0:listtype0);
var
  w,l : integer;
begin
  for w:=1 to list.number do begin
    for l:=1 to list.w_rd[w].number do begin
      list0.w_rd[w].letter[l].segment[1].sound:=sound(list.w_rd[w].letter[l].segment[1]);

      if list.w_rd[w].letter[l].segment[2].head='   '
        then begin
               list0.w_rd[w].letter[l].segment[2].sound:=list0.w_rd[w].letter[l].segment[1].sound;
             end
        else begin
               list0.w_rd[w].letter[l].segment[2].sound:=sound(list.w_rd[w].letter[l].segment[2]);
             end;
    end;

    list0.w_rd[w].number:=list.w_rd[w].number;
  end;

  list0.number:=list.number;
end;

procedure rline(var fp:text;var list0:listtype0);
var
  list : listtype;
  ch   : char;
begin
  readchar(fp,ch);

  if inheads(ch) or (ch='[')
    then rlist(fp,list,ch)
    else if ch='#'
           then begin
                  list.number:=0;
                  readchar(fp,ch);
                end
           else halt0('# expected');

  if ch='$'
    then readln(fp)
    else halt0('eoln expected');

  change(list,list0);
end;

function invowels(head:headtype):boolean;
begin
  if head='i  '
    then invowels:=true
    else
  if head='y  '
    then invowels:=true
    else
  if head='1  '
    then invowels:=true
    else
  if head='}  '
    then invowels:=true
    else
  if head='M  '
    then invowels:=true
    else
  if head='u  '
    then invowels:=true
    else
  if head='I  '
    then invowels:=true
    else
  if head='Y  '
    then invowels:=true
    else
  if head='U  '
    then invowels:=true
    else
  if head='e  '
    then invowels:=true
    else
  if head='2  '
    then invowels:=true
    else
  if head='8  '
    then invowels:=true
    else
  if head='7  '
    then invowels:=true
    else
  if head='o  '
    then invowels:=true
    else
  if head='@  '
    then invowels:=true
    else
  if head='E  '
    then invowels:=true
    else
  if head='9  '
    then invowels:=true
    else
  if head='3  '
    then invowels:=true
    else
  if head='V  '
    then invowels:=true
    else
  if head='O  '
    then invowels:=true
    else
  if head='{  '
    then invowels:=true
    else
  if head='6  '
    then invowels:=true
    else
  if head='a  '
    then invowels:=true
    else
  if head='&  '
    then invowels:=true
    else
  if head='A  '
    then invowels:=true
    else
  if head='Q  '
    then invowels:=true
    else invowels:=false;    
end;

function inconsonants(head:headtype):boolean;
begin
  if head='b <' 
    then inconsonants:=true 
    else
  if head='d <' 
    then inconsonants:=true 
    else
  if head='J\<' 
    then inconsonants:=true 
    else
  if head='g <' 
    then inconsonants:=true 
    else
  if head='F\<' 
    then inconsonants:=true 
    else
  if head='p  ' 
    then inconsonants:=true 
    else
  if head='b  ' 
    then inconsonants:=true 
    else
  if head='t  ' 
    then inconsonants:=true 
    else
  if head='d  ' 
    then inconsonants:=true 
    else
  if head='t `' 
    then inconsonants:=true 
    else
  if head='d `' 
    then inconsonants:=true 
    else
  if head='c  ' 
    then inconsonants:=true 
    else
  if head='J\ ' 
    then inconsonants:=true 
    else
  if head='k  ' 
    then inconsonants:=true 
    else
  if head='g  ' 
    then inconsonants:=true 
    else
  if head='q  ' 
    then inconsonants:=true 
    else
  if head='G\ ' 
    then inconsonants:=true 
    else
  if head='?  ' 
    then inconsonants:=true 
    else
  if head='m  ' 
    then inconsonants:=true 
    else
  if head='F  ' 
    then inconsonants:=true 
    else
  if head='n  ' 
    then inconsonants:=true 
    else
  if head='n `' 
    then inconsonants:=true 
    else
  if head='J  ' 
    then inconsonants:=true 
    else
  if head='N  ' 
    then inconsonants:=true 
    else
  if head='N\ ' 
    then inconsonants:=true 
    else
  if head='B\ ' 
    then inconsonants:=true 
    else
  if head='r  ' 
    then inconsonants:=true 
    else
  if head='R\ ' 
    then inconsonants:=true 
    else
  if head='4  ' 
    then inconsonants:=true 
    else
  if head='r `' 
    then inconsonants:=true 
    else
  if head='p\ ' 
    then inconsonants:=true 
    else
  if head='B  ' 
    then inconsonants:=true 
    else
  if head='f  ' 
    then inconsonants:=true 
    else
  if head='v  ' 
    then inconsonants:=true 
    else
  if head='T  ' 
    then inconsonants:=true 
    else
  if head='D  ' 
    then inconsonants:=true 
    else
  if head='s  ' 
    then inconsonants:=true 
    else
  if head='z  ' 
    then inconsonants:=true 
    else
  if head='S  ' 
    then inconsonants:=true 
    else
  if head='Z  ' 
    then inconsonants:=true 
    else
  if head='s `' 
    then inconsonants:=true 
    else
  if head='z `' 
    then inconsonants:=true 
    else
  if head='C  ' 
    then inconsonants:=true 
    else
  if head='j\ ' 
    then inconsonants:=true 
    else
  if head='x  ' 
    then inconsonants:=true 
    else
  if head='G  ' 
    then inconsonants:=true 
    else
  if head='X  ' 
    then inconsonants:=true 
    else
  if head='R  ' 
    then inconsonants:=true 
    else
  if head='X\ ' 
    then inconsonants:=true 
    else
  if head='?\ ' 
    then inconsonants:=true 
    else
  if head='h  ' 
    then inconsonants:=true 
    else
  if head='h\ ' 
    then inconsonants:=true 
    else
  if head='K  ' 
    then inconsonants:=true 
    else
  if head='K\ ' 
    then inconsonants:=true 
    else
  if head='w  ' 
    then inconsonants:=true 
    else
  if head='P  ' 
    then inconsonants:=true 
    else
  if head='v\ ' 
    then inconsonants:=true 
    else
  if head='r\ ' 
    then inconsonants:=true 
    else
  if head='r\`' 
    then inconsonants:=true 
    else
  if head='j  ' 
    then inconsonants:=true 
    else
  if head='M\ ' 
    then inconsonants:=true 
    else
  if head='l  ' 
    then inconsonants:=true 
    else
  if head='l `' 
    then inconsonants:=true 
    else
  if head='L  ' 
    then inconsonants:=true 
    else
  if head='L\ ' 
    then inconsonants:=true 
    else inconsonants:=false;
end;

procedure initComp0;
var
  fp  : text;
  i,j : integer;
  ch  : char;
  v   : real;
begin
  openr(fp,path+sname);

  readln(fp,comp0.number);

  for i:=1 to comp0.number do begin
    initsegment(comp0.segment[i]);

    read(fp,ch);
    rhead(fp,comp0.segment[i].head,ch);
    readln(fp);
    
    if invowels    (comp0.segment[i].head)
      then comp0.vctype[i]:='V'
      else
    if inconsonants(comp0.segment[i].head)
      then comp0.vctype[i]:='C'
      else comp0.vctype[i]:='-';
    
    comp0.matrix[i,i]:=0;
  end;

  for i:=2 to comp0.number do begin
    for j:=1 to (i-1) do begin
      readln(fp,v);
      comp0.matrix[i,j]:=v;
      comp0.matrix[j,i]:=v;
    end;
  end;

  close(fp);
end;

procedure copyComp0to1;
begin
  comp1:=comp0;
end;

procedure initComp2;
var
  i,j : integer;
begin
  comp2:=comp0;
  comp2.total:=0;

  for i:=1 to comp2.number do begin
    comp2.vector[i]:=0;
  end;

  for i:=2 to comp2.number do begin
    for j:=1 to (i-1) do begin
      comp2.matrix[i,j]:=0;
      comp2.matrix[j,i]:=0;
    end;
  end;
end;

procedure copyComp2to1;
begin
  comp1:=comp2;
end;

procedure IndelSubst;
var
  i,j               : integer;
  sumIndel,numIndel : real;
  sumSubst,numSubst : real;
begin
  maxIndel:=0;
  sumIndel:=0;
  numIndel:=0;
  
  for i:=2 to comp1.number do begin
    if (comp1.matrix[i,1]<>99999)
      then begin
             if (comp1.matrix[i,1] > maxIndel)
                then maxIndel:=comp1.matrix[i,1]
                else {nothing};
                
             sumIndel:=sumIndel+comp1.matrix[i,1];
             numIndel:=numIndel+1;
           end
      else {nothing};
  end;
  
  meanIndel:=sumIndel/numIndel;
  
  maxSubst:=0;
  sumSubst:=0;
  numSubst:=0;
  
  for i:=3 to comp1.number do begin
    for j:=2 to (i-1) do begin
      if (comp1.matrix[i,j]<>99999)
        then begin
               if (comp1.matrix[i,j] > maxSubst)
                 then maxSubst:=comp1.matrix[i,j]
                 else {nothing};
                 
               sumSubst:=sumSubst+comp1.matrix[i,j];
               numSubst:=numSubst+1;
             end  
        else {nothing};
    end;
  end;

  meanSubst:=sumSubst/numSubst;

{ writeln(stderr, 'Indel: ');                                       }
{ writeln(stderr, 'mean: ', meanIndel:0:4, ' max: ', maxIndel:0:4); }
{ writeln(stderr, 'Subst: ');                                       }
{ writeln(stderr, 'mean: ', meanSubst:0:4, ' max: ', maxSubst:0:4); }
{ writeln(stderr);                                                  }

  lg:=(maxIndel> (0.5*maxSubst));
  sm:=(maxIndel<=(0.5*maxSubst));
end;

procedure initEmpty;
var
  s : integer;
begin
  for s:=1 to 2 do begin
    empty.segment[s].sound:=1;
  end;
end;

function weight0(segment1,segment2:segmenttype0):real;
begin
  weight0:=comp1.matrix[segment1.sound,segment2.sound]
end;

function weight(letter1,letter2:lettertype0):real;
begin
  weight:=(weight0(letter1.segment[1],letter2.segment[1])+
           weight0(letter1.segment[2],letter2.segment[2]))/2;
end;

function min3(a,b,c:real):real;
begin
  if ((a<=b) and (a<=c)) then min3:=a else
  if ((b<=a) and (b<=c)) then min3:=b else min3:=c;
end;

function max3(a,b,c:real):real;
begin
  if ((a>=b) and (a>=c)) then max3:=a else
  if ((b>=a) and (b>=c)) then max3:=b else max3:=c;
end;

procedure check (var value1:real;prevA1,prevB1,prevC1:real;
                 var value2:real;prevA2,prevB2,prevC2:real;
                 var value3:real;prevA3,prevB3,prevC3:real);
begin
  value1:=min3(prevA1,prevB1,prevC1);

  if prevA1<>value1
    then prevA2:=-maxreal;

  if prevB1<>value1
    then prevB2:=-maxreal;

  if prevC1<>value1
    then prevC2:=-maxreal;

  value2:=max3(prevA2,prevB2,prevC2);
  
  if prevA1<>value1
    then prevA3:= maxreal;

  if prevB1<>value1
    then prevB3:= maxreal;

  if prevC1<>value1
    then prevC3:= maxreal;

  value3:=min3(prevA3,prevB3,prevC3);
end;

procedure addtrace(prevA1,prevB1,prevC1,
                   prevA2,prevB2,prevC2,
                   prevA3,prevB3,prevC3:real;l1,l2:integer);
var
  pointer : integer;
begin
  pointer:=0;

  if (prevA1=matrix1[l1,l2]) and ((lg and (prevA2=matrix2[l1,l2])) or (sm and (prevA3=matrix3[l1,l2])))
    then pointer:=pointer+8;

  if (prevB1=matrix1[l1,l2]) and ((lg and (prevB2=matrix2[l1,l2])) or (sm and (prevB3=matrix3[l1,l2])))
    then pointer:=pointer+4;

  if (prevC1=matrix1[l1,l2]) and ((lg and (prevC2=matrix2[l1,l2])) or (sm and (prevC3=matrix3[l1,l2])))
    then pointer:=pointer+2;

  trace[l1,l2]:=pointer;
end;

procedure addalign(l1,l2:integer;d:real;a:integer);
begin
  align[a].word1:=l1;
  align[a].word2:=l2;
  align[a].dist :=d ;
end;

procedure addFreqComp2(a:integer; word1,word2:wordtype0);
var
  l                               : integer;
  sound11,sound12,sound21,sound22 : integer;
begin
  for l:=(a-1) downto 1 do begin
    if align[l].word1=0
      then begin
             sound11:=1;
             sound12:=1;
           end
      else begin
             sound11:=word1.letter[align[l].word1].segment[1].sound;
             sound12:=word1.letter[align[l].word1].segment[2].sound;
           end;
           
    if align[l].word2=0
      then begin
             sound21:=1;
             sound22:=1;
           end
      else begin
             sound21:=word2.letter[align[l].word2].segment[1].sound;
             sound22:=word2.letter[align[l].word2].segment[2].sound;
           end;
    
    if sound11<>sound21
      then begin
             comp2.total:=comp2.total+0.5;
             
             comp2.vector[sound11]:=comp2.vector[sound11]+0.5;
             comp2.vector[sound21]:=comp2.vector[sound21]+0.5;
             
             comp2.matrix[sound11,sound21]:=comp2.matrix[sound11,sound21]+0.5;
             comp2.matrix[sound21,sound11]:=comp2.matrix[sound21,sound11]+0.5;
           end;

    if sound12<>sound22
      then begin
             comp2.total:=comp2.total+0.5;
             
             comp2.vector[sound12]:=comp2.vector[sound12]+0.5;
             comp2.vector[sound22]:=comp2.vector[sound22]+0.5;
             
             comp2.matrix[sound12,sound22]:=comp2.matrix[sound12,sound22]+0.5;
             comp2.matrix[sound22,sound12]:=comp2.matrix[sound22,sound12]+0.5;
           end;
  end;
end;

function segment(sound:integer):string;
var
  t     : integer;
  found : boolean;
begin
  t:=0;
  found:=false;

  while (not found) and (t<comp1.number) do begin
    t:=t+1;
    found:=(sound=t);
  end;

  if not found
    then begin
           writeln(stderr,sound,' not found in segment distance table');
           halt;
         end
    else {nothing};
    
  segment:=comp1.segment[t].head;
end;

procedure printalign(a:integer; word1,word2:wordtype0);
var
  l                  : integer;
  distance, alignLen : real;
begin
  writeln(fileAl);

  for l:=(a-1) downto 1 do begin
    if align[l].word1=0 
      then write(fileAl, '-', ' ':6)
      else begin
             write(fileAl, segment(word1.letter[align[l].word1].segment[1].sound));
             
             if word1.letter[align[l].word1].segment[1].sound<>
                word1.letter[align[l].word1].segment[2].sound
               then write(fileAl, segment(word1.letter[align[l].word1].segment[2].sound), ' ':1)
               else write(fileAl, ' ':4)
           end
  end;

  writeln(fileAl);

  for l:=(a-1) downto 1 do begin
    if align[l].word2=0 
      then write(fileAl, '-', ' ':6)
      else begin
             write(fileAl, segment(word2.letter[align[l].word2].segment[1].sound));

             if word2.letter[align[l].word2].segment[1].sound<>
                word2.letter[align[l].word2].segment[2].sound
               then write(fileAl, segment(word2.letter[align[l].word2].segment[2].sound), ' ':1)
               else write(fileAl, ' ':4)
           end
  end;

  writeln(fileAl);

  distance:=0;
  alignLen:=0;
  
  for l:=(a-1) downto 1 do begin
    write(fileAl, align[l].dist:3:2, ' ':3);
    distance:=distance+align[l].dist;
    
    if ((align[l].word1=0) or (align[l].word2=0))
      then alignLen:=alignLen+maxIndel
      else alignLen:=alignLen+maxSubst;
  end;

  if normal=0
    then writeln(fileAl, '   dist.: ', distance         :3:6, chr(10),chr(13))
    else writeln(fileAl, '   dist.: ', distance/alignLen:3:6, chr(10),chr(13));
end;

function vowel(sound:integer):boolean;
begin
  vowel    :=comp0.vctype[sound]='V';
end;

function consonant(sound:integer):boolean;
begin
  consonant:=comp0.vctype[sound]='C';
end;

procedure procalign(a:integer;word1,word2:wordtype0);
var
  l,l1,l2 : integer;
  distance: real;
begin
  distance:=0;

  for l:=(a-1) downto 1 do begin
    if align[l].word1=0 
      then l1:=1
      else l1:=word1.letter[align[l].word1].segment[1].sound;

    if align[l].word2=0 
      then l2:=1 
      else l2:=word2.letter[align[l].word2].segment[1].sound;

    if part=0
      then distance:=distance+align[l].dist
      else

    if part=1
      then if (vowel(l1) and vowel(l2))
             then distance:=distance+align[l].dist
             else {nothing}
      else

    if part=2
      then if ((l1=1) and vowel(l2))
             then distance:=distance+align[l].dist
             else
           if (vowel(l1) and (l2=1))
             then distance:=distance+align[l].dist
             else {nothing}
      else

    if part=3
      then if (consonant(l1) and consonant(l2))
             then distance:=distance+align[l].dist
             else {nothing}
      else

    if part=4
      then if ((l1=1) and consonant(l2))
             then distance:=distance+align[l].dist
             else
           if (consonant(l1) and (l2=1))
             then distance:=distance+align[l].dist
             else {nothing}
      else usage;
  end;
  
  allDist:=allDist+distance;
  allNum :=allNum +1;
end;

procedure proctrace(l1,l2,a:integer;word1,word2:wordtype0);
var
  pointer : integer;
begin
  if (l1>0) or (l2>0)
    then begin
           pointer:=trace[l1,l2];

           if ((pointer= 8) or (pointer=10) or (pointer=12) or (pointer=14)) and (l1>0)
             then begin
                    addalign(l1,0,matrix1[l1,l2]-matrix1[l1-1,l2],a);
                    proctrace(l1-1,l2,a+1,word1,word2);
                    pointer:=pointer-8;
                  end;

           if ((pointer= 4) or (pointer= 6) or (pointer=12) or (pointer=14)) and (l1>0) and (l2>0)
             then begin
                    addalign(l1,l2,matrix1[l1,l2]-matrix1[l1-1,l2-1],a);
                    proctrace(l1-1,l2-1,a+1,word1,word2);
                    pointer:=pointer-4;
                  end;

           if ((pointer= 2) or (pointer= 6) or (pointer=10) or (pointer=14)) and (l2>0)
             then begin
                    addalign(0,l2,matrix1[l1,l2]-matrix1[l1,l2-1],a);
                    proctrace(l1,l2-1,a+1,word1,word2);
                    pointer:=pointer-2;
                  end;
         end
    else begin
             if (method=2)
               then addFreqComp2(a, word1, word2);
    
             if (part<>0) and saveXX
               then procalign (a, word1, word2);
    
             if   saveAl  and saveXX
               then printalign(a, word1, word2);
         end;
end;

procedure Levenshtein(word1,word2:wordtype0;var distance,distanceP:real);
var
  l1,l2                : integer;
  prevA1,prevB1,prevC1 : real;
  prevA2,prevB2,prevC2 : real;
  prevA3,prevB3,prevC3 : real;
begin
  for l1:=0 to word1.number do begin
    for l2:=0 to word2.number do begin
      prevA1:= maxreal;
      prevB1:= maxreal;
      prevC1:= maxreal;

      prevA2:=-maxreal;
      prevB2:=-maxreal;
      prevC2:=-maxreal;
     
      prevA3:= maxreal;
      prevB3:= maxreal;
      prevC3:= maxreal;
      
      if (l1>0)
        then begin
               prevA1:=matrix1[l1-1,l2  ]+weight(word1.letter[l1],empty);
               if lg then prevA2:=matrix2[l1-1,l2  ]+maxIndel;
               if sm then prevA3:=matrix3[l1-1,l2  ]+maxIndel;
             end;

      if (l1>0) and (l2>0)
        then begin
               prevB1:=matrix1[l1-1,l2-1]+weight(word1.letter[l1],word2.letter[l2]);
               if lg then prevB2:=matrix2[l1-1,l2-1]+maxSubst;
               if sm then prevB3:=matrix3[l1-1,l2-1]+maxSubst;
             end;

      if (l2>0)
        then begin
               prevC1:=matrix1[l1  ,l2-1]+weight(empty,word2.letter[l2]);
               if lg then prevC2:=matrix2[l1  ,l2-1]+maxIndel;
               if sm then prevC3:=matrix3[l1  ,l2-1]+maxIndel;
             end;

      check(matrix1[l1,l2],prevA1,prevB1,prevC1,
            matrix2[l1,l2],prevA2,prevB2,prevC2,
            matrix3[l1,l2],prevA3,prevB3,prevC3);

      if matrix1[l1,l2]= maxreal then matrix1[l1,l2]:=0;
      if matrix2[l1,l2]=-maxreal then matrix2[l1,l2]:=0;
      if matrix3[l1,l2]= maxreal then matrix3[l1,l2]:=0;

      if ((method=2) or ((part<>0) and saveXX) or (saveAl and saveXX))
        then addtrace(prevA1,prevB1,prevC1,
                      prevA2,prevB2,prevC2,
                      prevA3,prevB3,prevC3,l1,l2);
    end;
  end;

  allDist:=0; allNum:=0;
  if ((method=2) or ((part<>0) and saveXX) or (saveAl and saveXX))
    then begin
           proctrace(word1.number,word2.number,1,word1,word2);         
  
           if ((part<>0) and saveXX)
             then distanceP:=allDist/allNum;
         end;

  if (normal=0)
    then distance :=matrix1[word1.number,word2.number]
    else
  if (normal=1) and lg
    then distance :=matrix1[word1.number,word2.number]/matrix2[word1.number,word2.number]
    else
  if (normal=1) and sm
    then distance :=matrix1[word1.number,word2.number]/matrix3[word1.number,word2.number]
    else {nothing};
    
  if (normal=0) and (part<>0)
    then distanceP:=distanceP
    else
  if (normal=1) and (part<>0) and lg
    then distanceP:=distanceP/matrix2[word1.number,word2.number]
    else
  if (normal=1) and (part<>0) and sm
    then distanceP:=distanceP/matrix3[word1.number,word2.number]
    else {nothing};
end;

procedure saveIndv;
begin
  if saveIn
    then writeln(fileIn, 'var1', #9, 'var2', #9, 'item', #9, 'dist');
end;

procedure calculate(filename10,filename1,filename20,filename2:string);
var
  list1,list2    : listtype0;
  w1,w2          : integer;
  file1,file2    : text;
  matrix,matrixP : array[1..30] of array[1..30] of real;
  c1,c2          : array[0..30] of integer;
  h1,h2          : integer;
  d,d0,s         : real;
  m0             : real;
  mean           : real;
  w10,w20        : integer;
  fp             : text;
begin
  openr(file1,path+filename1+'.dat');
  openr(file2,path+filename2+'.dat');
  openr(fp,path+iname);

  if saveAl and saveXX
    then writeln(fileAl,filename10,' versus ',filename20,chr(10),chr(13))
    else {nothing};

  m0:=0; s:=0;
  while (not eof(file1)) and (not eof(file2)) do begin
    readln(fp,itemname);
  
    filename:=filename10; 
    rline(file1,list1);
    
    filename:=filename20; 
    rline(file2,list2);

    if (list1.number>0) and (list2.number>0)
      then begin
             for w1:=1 to list1.number do begin
               for w2:=1 to list2.number do begin
                 if saveAl and saveXX
                   then writeln(fileAl, itemname)
                   else {nothing};
                 Levenshtein(list1.w_rd[w1],list2.w_rd[w2],matrix[w1,w2],matrixP[w1,w2])
               end;
             end;  
               
             for w1:=1 to list1.number do
               c1[w1]:=list2.number;
             for w2:=1 to list2.number do
               c2[w2]:=list1.number;

             d:=0;

             for h1:=1 to list1.number do begin
               for h2:=1 to list2.number do begin

                 d0:=1.7e+38;
                 for w1:=1 to list1.number do begin
                   for w2:=1 to list2.number do begin
                     if (matrix[w1,w2]<d0) and (c1[w1]>0) and (c2[w2]>0)
                       then begin
                              d0:=matrix[w1,w2];
                              w10:=w1;
                              w20:=w2;
                            end
                       else {nothing};
                   end;
                 end;

                 if (part=0)
                   then d:=d+matrix [w10,w20]
                   else d:=d+matrixP[w10,w20];

                 c1[w10]:=c1[w10]-1;
                 c2[w20]:=c2[w20]-1;
               end;
             end;

             d :=d/(list1.number*list2.number);
             s :=s +d;
             m0:=m0+1;
           end
      else d:=-1;
   
    if saveIn and saveXX
      then if d>=0
             then writeln(fileIn, filename10, #9, filename20, #9, itemname, #9, d:3:6)
             else writeln(fileIn, filename10, #9, filename20, #9, itemname, #9)
      else {nothing}
  end;

  if (m0>0)
    then mean:=s/m0
    else begin
            if FileExists(path+'individual.tsv') then DeleteFile(path+'individual.tsv');
            if FileExists(path+'aggregated.csv') then DeleteFile(path+'aggregated.csv');
            
            writeln(stderr, filename10, ' and ', filename20, ' do not share any item!');
            halt
         end;

  if saveAg and saveXX
    then write(fileAg,mean:3:3,',')
    else {nothing};

  close(file1);
  close(file2);
  close(fp);
end;

procedure compareVarieties;
var
  fp10,fp1,fp20,fp2                         : text;
  filename10,filename1,filename20,filename2 : string[255];
begin  
  openr(fp10,path+nname);
  openr(fp1 ,path+fname);
  
  readln(fp10);
  readln(fp1 );
  
  if saveAg and saveXX
    then writeln(fileAg,0);
  
  while not eof(fp1) do begin
    readln(fp10,filename10);
    readln(fp1 ,filename1 );
    
    openr(fp20,path+nname);
    openr(fp2 ,path+fname);

    readln(fp20,filename20);
    readln(fp2 ,filename2 );
    
    while (filename1<>filename2) do begin
      calculate(filename10,filename1,filename20,filename2);

      readln(fp20,filename20);
      readln(fp2 ,filename2 );
    end;
  
    if saveAg and saveXX
      then writeln(fileAg,0);  
    
    close(fp20);
    close(fp2);
  end;
  
  close(fp10);
  close(fp1);
end;

procedure calcDistComp2;
var
  pxy,px,py,p       : real;
  PMI,minPMI,maxPMI : real;
  i,j               : integer;
begin
  minPMI:= maxreal;
  maxPMI:=-maxreal;

  for i:=1 to comp2.number do begin
    for j:=1 to comp2.number do begin
      if (i<>j) and (comp0.matrix[i,j]<>99999)
        then begin
               if (comp2.matrix[i,j]>0)
                 then begin
                        pxy:=comp2.matrix[i,j]/ comp2.total   ;
                        px :=comp2.vector[i]  /(comp2.total*2);
                        py :=comp2.vector[j]  /(comp2.total*2);
                        p  :=pxy/(px*py);
                      end
                 else begin
                        p  :=0;
                      end;
                    
               PMI:=log2(p+1);
               comp2.matrix[i,j]:=PMI;
               minPMI:=min(minPMI,PMI);
               maxPMI:=max(maxPMI,PMI);
             end  
        else {nothing}
    end;
  end;

  for i:=1 to comp2.number do begin
    for j:=1 to comp2.number do begin
      if (i<>j) and (comp0.matrix[i,j]<>99999)
        then begin
               comp2.matrix[i,j]:=( maxPMI-comp2.matrix[i,j])/
                                  ( maxPMI-minPMI);
               comp2.matrix[i,j]:=(0*comp1.matrix[i,j])+
                                  (1*comp2.matrix[i,j]);
             end
        else comp2.matrix[i,j]:=comp0.matrix[i,j]
    end;
  end;
end;

procedure eucComp1Comp2(var d: real);
var
  i,j   : integer;
  sum   : real;
  v1,v2 : real;
begin
  sum:=0;

  for i:=2 to comp1.number do begin
    for j:=1 to (i-1) do begin
      if (comp1.matrix[i,j]<>99999)
        then begin  
               v1:=comp1.matrix[i,j];
               v2:=comp2.matrix[i,j];
               
               sum:=sum+sqr(v1-v2);
             end
        else {nothing};
    end;
  end;
  
  d:=sqrt(sum);
end;

procedure corComp1Comp2(var r: real);
var
  i,j               : integer;
  sumx,sumy         : real;
  sumxy,sumxx,sumyy : real;
  nxy,nxx,nyy       : real;
  v1,v2             : real;
  number            : real;
begin
  sumx:=0;
  sumy:=0;

  sumxy:=0;
  sumxx:=0;
  sumyy:=0;

  number:=0;

  for i:=2 to comp1.number do begin
    for j:=1 to (i-1) do begin
      if (comp1.matrix[i,j]<>99999)
        then begin
               number:=number+1;
        
               v1:=comp1.matrix[i,j];
               v2:=comp2.matrix[i,j];

               sumx:=sumx+v1;
               sumy:=sumy+v2;

               sumxy:=sumxy+(v1*v2);
               sumxx:=sumxx+(v1*v1);
               sumyy:=sumyy+(v2*v2);
             end
    end;
  end;

  nxy:=(sumx*sumy)/number;
  nxx:=(sumx*sumx)/number;
  nyy:=(sumy*sumy)/number;

  r:=(sumxy-nxy)/sqrt((sumxx-nxx)*(sumyy-nyy));
end;  

procedure saveComp1;
var
  i,j,h : integer;
begin
  if saveSo
    then begin
           for i:=1 to comp1.number do begin
             write(fileSo, ',');
             
             for h:=1 to 3 do begin
               if comp1.segment[i].head[h]<>' '
                 then write(fileSo,comp1.segment[i].head[h]);
             end;
           end;

           writeln(fileSo);
           
           for i:=1 to comp1.number do begin
             for h:=1 to 3 do begin
               if comp1.segment[i].head[h]<>' '
                 then write(fileSo,comp1.segment[i].head[h]);
             end;
            
             for j:=1 to comp1.number do begin
               write(fileSo, ',');

               if (comp1.matrix[i,j]<>99999)
                 then write(fileSo,comp1.matrix[i,j]:3:3)
                 else write(fileSo,'');
             end;
             
             writeln(fileSo);
           end;
         end
    else {nothing};       
end;

begin {main}
  getparameters;
  initEmpty;

  if saveSo then openw(fileSo,path+'sounddists.csv');
  if saveAl then openw(fileAl,path+'alignments.txt');
  if saveIn then openw(fileIn,path+'individual.tsv');
  if saveAg then openw(fileAg,path+'aggregated.csv');
  
  saveIndv;

  if method=1
    then saveXX:=true
    else saveXX:=false;

  initComp0;
  copyComp0to1;
  IndelSubst;
  initComp2;
  compareVarieties;
  
  if method=2
    then begin
           calcDistComp2;
    
           r:=0;
           repeat
             r0:=r;
             copyComp2to1;
             IndelSubst;
             initComp2;
             compareVarieties;
             calcDistComp2;
             corComp1Comp2(r);
           until (r<=r0);
           
           saveXX:=true;
           IndelSubst;
           initComp2;
           compareVarieties;
         end
    else {nothing};

  saveComp1;

  if saveSo then close(fileSo);
  if saveAl then close(fileAl);
  if saveIn then close(fileIn);
  if saveAg then close(fileAg);
end   {main}.
