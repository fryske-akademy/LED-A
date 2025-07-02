{ (c) W.J. Heeringa 2021 }

{$R+ $B+}
program symbols(input,output,stderr);

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

var
  infile,outfile              : text;
  nname,fname,iname           : string[255];
  procLen,procDia             : boolean;
  filename0,filename,itemname : string[255];
  path                        : string[255];
  fp0,fp                      : text;

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
  writeln(stderr,'Usage: ',ParamStr(0),' namfile filfile itmfile 0|1 0|1 path');
  writeln(stderr);
  writeln(stderr,'namfile: file with names       ');
  writeln(stderr,'filfile: file with files       ');
  writeln(stderr,'itmfile: file with items       ');
  writeln(stderr);
  writeln(stderr,'0: Do nothing                  ');
  writeln(stderr,'1: Process length              ');
  writeln(stderr);
  writeln(stderr,'0: Do nothing                  ');
  writeln(stderr,'1: Process diacritics          ');
  writeln(stderr);
  writeln(stderr,'path: path of working directory');
  writeln(stderr);
  halt;
end;

procedure getparameters;
begin
  if paramcount=6
   then begin
          nname:=paramstr(1);
          fname:=paramstr(2);
          iname:=paramstr(3);

          if paramstr(4)='0' then procLen:=false else
          if paramstr(4)='1' then procLen:=true  else usage;
          
          if paramstr(5)='0' then procDia:=false else
          if paramstr(5)='1' then procDia:=true  else usage;
          
          path:=paramstr(6);
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
  writeln(stderr, 'Check transcription of variety ', filename0, ' and item ', itemname, '.');
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
    else halt0('head expected');
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

procedure rline(var fp:text;var list:listtype);
var
  ch : char;
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
end;

procedure whead(var fp:text;var head:headtype);
var
  h : integer;
begin
  for h:=1 to 3 do begin
    if head[h]<>' '
      then write(fp,head[h])
      else {nothing};
  end;
end;

procedure wsupra(var fp:text;var supra:supratype);
var
  s : integer;
begin
  for s:=1 to 2 do begin
    if supra[s]<>' '
      then write(fp,supra[s])
      else {nothing};
  end;
end;

procedure wdiacritic(var fp:text;diacritic:diacritictype);
var
  d : integer;
begin
  if diacritic<>'  '
    then begin
           write(fp,'_');

           for d:=1 to 2 do begin
             if diacritic[d]<>' '
               then write(fp,diacritic[d])
               else {nothing};
           end;
         end;
end;

procedure wsegment(var fp:text;segment:segmenttype);
var
  s,d : integer;
begin
  for s:=1 to 4 do
    wsupra(fp,segment.supra[s]);

  for d:=1 to 4 do
    wdiacritic(fp,segment.diacritic[d]);
end;

procedure wletter(var fp:text;letter:lettertype);
var
  s : integer;
begin
  if (letter.segment[2].head='   ')
    then begin
           whead(fp,letter.segment[1].head);
           wsegment(fp,letter.segment[1])
         end
    else begin
           write(fp,'[');

           for s:=1 to 2 do begin
             whead(fp,letter.segment[s].head);
           end;

           write(fp,']');
           
           wsegment(fp,letter.segment[1]);
         end
end;

procedure wword(var fp:text;var w_rd:wordtype);
var
  l : integer;
begin
  for l:=1 to w_rd.number do begin
    wletter(fp,w_rd.letter[l]);
  end;
end;

procedure wlist(var fp:text;var list:listtype);
var
  w : integer;
begin
  wword(fp,list.w_rd[1]);
  for w:=2 to list.number do begin
    write(fp,' / ');
    wword(fp,list.w_rd[w]);
  end;
end;

procedure wline(var fp:text;var list:listtype);
begin
  if list.number=0
    then write(fp,'#')
    else wlist(fp,list);
  writeln(fp);
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

function instress(head:headtype):boolean;
begin
  if head='''  '
    then instress:=true
    else
  if head='"  '
    then instress:=true
    else
  if head='%  '
    then instress:=true
    else instress:=false;    
end;

procedure processStress(var w_rd:wordtype);
var
  l       : integer;
  letter0 : lettertype;
begin
  l:=0;
  while l<(w_rd.number) do begin
    l:=l+1;

    if instress(w_rd.letter[l].segment[1].head)
      then if (l=w_rd.number)
             then halt0('stress could not be put in front of a vowel')
             else
             
           if not invowels(w_rd.letter[l+1].segment[1].head)
             then begin
                    letter0:=w_rd.letter[l];
                    w_rd.letter[l]:=w_rd.letter[l+1];
                    w_rd.letter[l+1]:=letter0;
                  end
             else {nothing}
      else {nothing}
  end;
end;

function supra(segment:segmenttype;sup:supratype;var d:integer):boolean;
var
  found : boolean;
begin
  d:=0;
  found:=false;

  while not found and (d<4) do begin
    d:=d+1;
    found:=(segment.supra[d]=sup);
  end;

  supra:=found;
end;

function diacritic(segment:segmenttype;dia:diacritictype;var d:integer):boolean;
var
  found : boolean;
begin
  d:=0;
  found:=false;

  while not found and (d<4) do begin
    d:=d+1;
    found:=(segment.diacritic[d]=dia);
  end;

  diacritic:=found;
end;

function small(var letter:lettertype;var d:integer):boolean;
begin
  small:=diacritic(letter.segment[1],'X ',d);
end;

function normal(var letter:lettertype;var d:integer):boolean;
begin
  if diacritic(letter.segment[1],'X ',d)
    then normal:=false
    else

  if supra(letter.segment[1],':\',d)
    then normal:=false
    else

  if supra(letter.segment[1],': ',d)
    then normal:=false
    else normal:=true
end;

function halflong(var letter:lettertype;var d:integer):boolean;
begin
  halflong:=supra(letter.segment[1],':\',d);
end;

function long(var letter:lettertype;var d:integer):boolean;
begin
  long:=supra(letter.segment[1],': ',d);
end;

procedure processLength(var w_rd:wordtype);
var
  l,l0,d : integer;
begin
  l:=0;
  while (l<w_rd.number) do begin
    l:=l+1;

    if small(w_rd.letter[l],d)
      then begin
             w_rd.letter[l].segment[1].diacritic[d]:='  ';
           end
      else

    if normal(w_rd.letter[l],d)
      then begin
             w_rd.number:=w_rd.number+1;
             for l0:=w_rd.number downto (l+2) do begin
               w_rd.letter[l0]:=w_rd.letter[l0-1];
             end;

             l:=l+1;
             w_rd.letter[l]:=w_rd.letter[l-1];
           end
      else

    if halflong(w_rd.letter[l],d)
      then begin
             w_rd.letter[l].segment[1].supra[d]:='  ';

             w_rd.number:=w_rd.number+2;
             for l0:=w_rd.number downto (l+3) do begin
               w_rd.letter[l0]:=w_rd.letter[l0-2];
             end;

             l:=l+1;
             w_rd.letter[l]:=w_rd.letter[l-1];
             l:=l+1;
             w_rd.letter[l]:=w_rd.letter[l-1];
           end
      else

    if long(w_rd.letter[l],d)
      then begin
             w_rd.letter[l].segment[1].supra[d]:='  ';

             w_rd.number:=w_rd.number+3;
             for l0:=w_rd.number downto (l+4) do begin
               w_rd.letter[l0]:=w_rd.letter[l0-3];
             end;

             l:=l+1;
             w_rd.letter[l]:=w_rd.letter[l-1];
             l:=l+1;
             w_rd.letter[l]:=w_rd.letter[l-1];
             l:=l+1;
             w_rd.letter[l]:=w_rd.letter[l-1];
           end
      else {nothing};
  end;
end;

function aspirated(segment:segmenttype;var d:integer):boolean;
var
  found : boolean;
begin
  found:=false;

  d:=0;
  while not found and (d<4) do begin
    d:=d+1;
    found:=(segment.diacritic[d]='h ');
  end;

  aspirated:=found;
end;

function labialized(segment:segmenttype;var d:integer):boolean;
var
  found : boolean;
begin
  found:=false;

  d:=0;
  while not found and (d<4) do begin
    d:=d+1;
    found:=(segment.diacritic[d]='w ');
  end;

  labialized:=found;
end;

function palatalized(segment:segmenttype;var d:integer):boolean;
var
  found : boolean;
begin
  found:=false;

  d:=0;
  while not found and (d<4) do begin
    d:=d+1;
    found:=(segment.diacritic[d]='j ');
  end;

  palatalized:=found;
end;

function velarized(segment:segmenttype;var d:integer):boolean;
var
  found : boolean;
begin
  found:=false;

  d:=0;
  while not found and (d<4) do begin
    d:=d+1;
    found:=(segment.diacritic[d]='G ');
  end;

  velarized:=found;
end;

function pharyngealized(segment:segmenttype;var d:integer):boolean;
var
  found : boolean;
begin
  found:=false;

  d:=0;
  while not found and (d<4) do begin
    d:=d+1;
    found:=(segment.diacritic[d]='?\');
  end;

  pharyngealized:=found;
end;

function nasalized(segment:segmenttype;var d:integer):boolean;
var
  found : boolean;
begin
  found:=false;

  d:=0;
  while not found and (d<4) do begin
    d:=d+1;
    found:=(segment.diacritic[d]='~ ');
  end;

  nasalized:=found;
end;

procedure processDiacritics(var w_rd:wordtype);
var
  l,d       : integer;
  removeDia : boolean;
begin
  l:=0;
  while l<(w_rd.number) do begin
    l:=l+1;

    removeDia:=true;

    if      aspirated(w_rd.letter[l].segment[1],d) and (w_rd.letter[l].segment[2].head='   ')
      then w_rd.letter[l].segment[2].head:='h  '
      else
    
    if     labialized(w_rd.letter[l].segment[1],d) and (w_rd.letter[l].segment[2].head='   ')
      then w_rd.letter[l].segment[2].head:='w  '
      else
    
    if    palatalized(w_rd.letter[l].segment[1],d) and (w_rd.letter[l].segment[2].head='   ')
      then w_rd.letter[l].segment[2].head:='j  '
      else
    
    if      velarized(w_rd.letter[l].segment[1],d) and (w_rd.letter[l].segment[2].head='   ')
      then w_rd.letter[l].segment[2].head:='G  '
      else

    if pharyngealized(w_rd.letter[l].segment[1],d) and (w_rd.letter[l].segment[2].head='   ')
      then w_rd.letter[l].segment[2].head:='?\ '
      else

    if      nasalized(w_rd.letter[l].segment[1],d) and (w_rd.letter[l].segment[2].head='   ')
      then w_rd.letter[l].segment[2].head:='n  '
      else removeDia:=false;

    if removeDia
      then w_rd.letter[l].segment[1].diacritic[d]:='  '
      else {nothing}
  end;
end;  

procedure transform(var infile,outfile:text);
var
  list : listtype;
  w    : integer;
  fp   : text;
begin
  openr(fp,path+iname);

  while not eof(fp) do begin
    readln(fp,itemname);
    rline(infile,list);

    for w:=1 to list.number do begin
      processStress(list.w_rd[w]);
      
      if procLen
        then processLength(list.w_rd[w]);
        
      if procDia
        then processDiacritics(list.w_rd[w]);
    end;

    wline(outfile,list);
  end;
  
  close(fp);
end;

begin{main}
  getparameters;

  openr(fp0,path+nname);
  openr(fp ,path+fname);

  while not eof(fp) do begin
    readln(fp0,filename0);
    readln(fp ,filename );
    
    openr( infile,path+filename       );
    openw(outfile,path+filename+'.dat');

    transform(infile,outfile);

    close( infile);
    close(outfile);
  end;
  
  close(fp0);
  close(fp );
end  {main}.
