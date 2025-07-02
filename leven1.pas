{ (c) W.J. Heeringa 2022 }

{$R+ $B+}
program calculate_Levenshtein_distance(input,output,stderr);

uses 
  sysutils, math;

const
  maxreal = 1E20;

type
  wordtype   = record
                 letter    : array[1..1000] of char;
                 number    : integer;
               end;

  listtype   = record
                 w_rd      : array[1..30] of wordtype;
                 number    : integer;
               end;

  matrixtype = array[0..1000,0..1000] of real;

var
  matrix            : matrixtype;
  nname,fname,iname : string[255];
  saveIn,saveAg     : boolean;
  fileIn,fileAg     : text;
  filename,itemname : string[255];
  path              : string[255];

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
  writeln(stderr,'(c) W.J. Heeringa 2022');
  writeln(stderr);
  writeln(stderr,'Usage: ',ParamStr(0),' namfile filfile itmfile 0|1 0|1 0|1 path');
  writeln(stderr);
  writeln(stderr,'namfile: file with names       ');
  writeln(stderr,'filfile: file with files       ');
  writeln(stderr,'itmfile: file with items       ');
  writeln(stderr);
  writeln(stderr,'0: Do nothing                  ');
  writeln(stderr,'1: Save  word pair  distances  ');
  writeln(stderr);
  writeln(stderr,'0: Do nothing                  ');
  writeln(stderr,'1: Save  aggregated distances  ');
  writeln(stderr);
  writeln(stderr,'path: path of working directory');
  writeln(stderr);
  halt;
end;

procedure getparameters;
begin
  if (paramcount=6)
    then begin
           nname:=paramstr(1);
           fname:=paramstr(2);
           iname:=paramstr(3);

           if paramstr(4)='0' then saveIn:=false else
           if paramstr(4)='1' then saveIn:=true  else usage;
    
           if paramstr(5)='0' then saveAg:=false else
           if paramstr(5)='1' then saveAg:=true  else usage;
           
           path:=paramstr(6);
         end
    else usage;
end;

function inletters(ch:char):boolean;
begin
  if (ch<>'') and (ch<>' ') and (ch<>'#') and (ch<>'$')
    then inletters:=true
    else inletters:=false;
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

procedure rletter(var fp:text;var letter:char;var ch:char);
begin
  letter:=' ';

  if inletters(ch)
    then begin
           letter:=ch;
           readchar(fp,ch);
         end
    else halt0('character expected');
end;

procedure rword(var fp:text;var w_rd:wordtype;var ch:char);
var
  l : integer;
begin
  l:=1;
  rletter(fp,w_rd.letter[l],ch);
  while inletters(ch) do begin
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

  if inletters(ch)
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

function weight(letter1,letter2:char):real;
begin
  if (letter1<>letter2)
    then weight:=1
    else weight:=0;
end;

function min3(a,b,c:real):real;
begin
  if ((a<=b) and (a<=c)) then min3:=a else
  if ((b<=a) and (b<=c)) then min3:=b else min3:=c;
end;

procedure Levenshtein(word1,word2:wordtype;var distance:real);
var
  l1,l2             : integer;
  prevA,prevB,prevC : real;
begin
  for l1:=0 to word1.number do begin
    for l2:=0 to word2.number do begin
      prevA:= maxreal;
      prevB:= maxreal;
      prevC:= maxreal;

      if (l1>0)
        then prevA:=matrix[l1-1,l2  ]+0.5;

      if (l1>0) and (l2>0)
        then prevB:=matrix[l1-1,l2-1]+weight(word1.letter[l1],word2.letter[l2]);

      if (l2>0)
        then prevC:=matrix[l1  ,l2-1]+0.5;

      matrix[l1,l2]:=min3(prevA,prevB,prevC);
      if matrix[l1,l2]=maxreal then matrix[l1,l2]:=0;
    end;
  end;

  distance:=matrix[word1.number,word2.number];
  
  if (distance>0)
    then distance:=1;
end;

procedure saveIndv;
begin
  if saveIn
    then writeln(fileIn, 'var1', #9, 'var2', #9, 'item', #9, 'dist');
end;

procedure calculate(filename10,filename1,filename20,filename2:string);
var
  list1,list2 : listtype;
  w1,w2       : integer;
  file1,file2 : text;
  matrix      : array[1..30] of array[1..30] of real;
  c1,c2       : array[0..30] of integer;
  h1,h2       : integer;
  d,d0,s      : real;
  m0          : real;
  mean        : real;
  w10,w20     : integer;
  fp          : text;
begin
  openr(file1,path+filename1);
  openr(file2,path+filename2);
  openr(fp,path+iname);

  m0:=0; s:=0;
  while (not eof(file1)) and (not eof(file2)) do begin
    readln(fp,itemname);
  
    filename:=filename10; 
    rline(file1,list1);
    
    filename:=filename20; 
    rline(file2,list2);
    
    if (list1.number>0) and (list2.number>0) and (not ((list1.w_rd[1].letter[1]='-') and (list2.w_rd[1].letter[1]='-')))
      then begin
             for w1:=1 to list1.number do begin
               for w2:=1 to list2.number do begin
                 Levenshtein(list1.w_rd[w1],list2.w_rd[w2],matrix[w1,w2])
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

                 d:=d+matrix [w10,w20];

                 c1[w10]:=c1[w10]-1;
                 c2[w20]:=c2[w20]-1;
               end;
             end;

             d :=d/(list1.number*list2.number);
             s :=s +d;
             m0:=m0+1;
           end
      else d:=-1;
   
    if saveIn
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

  if saveAg
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
  
  if saveAg
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
  
    if saveAg
      then writeln(fileAg,0);  
    
    close(fp20);
    close(fp2);
  end;
  
  close(fp10);
  close(fp1);
end;

begin {main}
  getparameters;

  if saveIn then openw(fileIn,path+'individual.tsv');
  if saveAg then openw(fileAg,path+'aggregated.csv');
  
  saveIndv;

  compareVarieties;

  if saveIn then close(fileIn);
  if saveAg then close(fileAg);
end   {main}.
