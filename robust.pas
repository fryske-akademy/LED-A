{ (c) W.J. Heeringa 2023 }

program clustering_bootstrap(input,output,stderr);

uses 
  SysUtils;
  
const
  mn = 1500;
  mm = 1000;

type
  ddtype      = array[1..mn+mn-1] of array[1..mn+mn-1] of real;

  usedtype    = array[1..mn+mn-1] of boolean;

  clusterrec  = record
                  left,right : integer;
                  number     : integer;
                end;

  clustertype = array[1..mn+mn-1] of clusterrec;

  ctype       = array[1..mn] of real;

  ggtype      = array[1..mn] of array[1..mn] of real;

  ptype       = array[1..mn] of integer;

var
  m,n,mmin          : integer;
  t,i,j             : integer;
  dd,dd0            : ddtype;
  sd                : real;
  used              : usedtype;
  cluster           : clustertype;
  c                 : ctype;
  gg,gg0            : ggtype;
  number            : integer;
  dfile,logfile     : text;
  fname,iname,dname : string[255];
  methodc,methodr   : integer;
  nruns             : integer;
  p                 : ptype;
  group             : integer;
  mincor            : real;
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
  writeln(stderr,'(c) W. J. Heeringa 2023');
  writeln(stderr);
  writeln(stderr,'Usage: ',paramstr(0),' filfile itmfile disfile 1-7 1|2 1-5 path');
  writeln(stderr);
  writeln(stderr,'filfile: file with file names');
  writeln(stderr,'itmfile: file with items     ');
  writeln(stderr,'disfile: file with distances ');
  writeln(stderr);
  writeln(stderr,'1: Single linkage     ');
  writeln(stderr,'2: Complete linkage   ');
  writeln(stderr,'3: Unweighted average ');
  writeln(stderr,'4: Weighted average   ');
  writeln(stderr,'5: Unweighted centroid');
  writeln(stderr,'6: Weighted centroid  ');
  writeln(stderr,'7: Minimum variance   ');
  writeln(stderr);
  writeln(stderr,'1: bootstrap');
  writeln(stderr,'2: noise    ');
  writeln(stderr);
  writeln(stderr,'1:  lowest degree of stability');
  writeln(stderr,'5: highest degree of stability');
  writeln(stderr);
  writeln(stderr,'path: path of working directory');
  writeln(stderr);
  halt;
end;

procedure getparameters;
begin
  if paramcount=7
    then begin
           fname:=paramstr(1);
           iname:=paramstr(2);
           dname:=paramstr(3);
           
           if paramstr(4)='1' then methodc:=1         else
           if paramstr(4)='2' then methodc:=2         else
           if paramstr(4)='3' then methodc:=3         else
           if paramstr(4)='4' then methodc:=4         else
           if paramstr(4)='5' then methodc:=5         else
           if paramstr(4)='6' then methodc:=6         else
           if paramstr(4)='7' then methodc:=7         else usage;
                                         
           if paramstr(5)='1' then methodr:=1         else
           if paramstr(5)='2' then methodr:=2         else usage;

           if paramstr(6)='1' then mincor :=0.999     else
           if paramstr(6)='2' then mincor :=0.9999    else
           if paramstr(6)='3' then mincor :=0.99999   else
           if paramstr(6)='4' then mincor :=0.999999  else
           if paramstr(6)='5' then mincor :=0.9999999 else usage;

           path :=paramstr(7);
         end
   else usage;
end;

procedure countVarieties;
var
  fp : text;
  s  : string[255];
begin
  openr(fp,path+fname);

  n:=0;
  while not eof(fp) do begin
    inc(n);
    readln(fp,s);
  end;

  close(fp);
end;

procedure countItems;
var
  fp : text;
  s  : string[255];
begin
  openr(fp,path+iname);

  m:=0;
  while not eof(fp) do begin
    inc(m);
    readln(fp,s);
  end;

  close(fp);
end;

procedure readLine(var fp:text; var value:real);
var
  i,p  : integer;
  s,s0 : string[255];
begin
  readln(fp,s);
  i:=length(s);

  while (s[i]<>#9) do begin
    i:=i-1;
  end;

  p:=i+1;

  s0:=copy(s,p,length(s));

  if (DefaultFormatSettings.DecimalSeparator=',')
    then s0:=StringReplace(s0, '.', ',', [rfReplaceAll, rfIgnoreCase])
    else {nothing};

  if (p<=length(s))
    then value:=StrToFloat(s0)
    else value:=-1;
end;

procedure read_dd1(var fp:text);
var
  i,j,w : integer;
  v     : real;
  list  : array[1..mm] of real;
  index : integer;
  sum   : real;
  count : real;
begin
  reset(fp);
  readln(fp);

  for i:=1 to mn do begin
    for j:=1 to mn do begin
      dd[i,j]:=0;
    end;
  end;

  for w:=1 to m do begin
    list[w]:=0;
  end;

  for w:=1 to m do begin
    index:=random(m)+1; 
    list[index]:=list[index]+1;
  end;

  for i:=2 to n do begin
    for j:=1 to (i-1) do begin
      sum  :=0;
      count:=0;
      
      for w:=1 to m do begin
        readLine(fp,v);

        if (v>=0) and (list[w]>=1)
          then begin
                 sum  :=sum  +(v*list[w]);          
                 count:=count+   list[w];
               end
          else {};
      end;
      
      dd[i,j]:=sum/count;
      dd[j,i]:=dd[i,j];
    end;
  end;
end;

procedure read_dd2(var fp:text);
var
  i,j,w : integer;
  v     : real;
  sum   : real;
  count : real;
begin
  reset(fp);
  readln(fp);

  for i:=1 to mn do begin
    for j:=1 to mn do begin
      dd0[i,j]:=0;
    end;
  end;

  for i:=2 to n do begin
    for j:=1 to (i-1) do begin
      sum  :=0;
      count:=0;
      
      for w:=1 to m do begin
        readLine(fp,v);

        if (v>=0)
          then begin
                 sum  :=sum  +v;          
                 count:=count+1;
               end
          else {};
      end;
      
      dd0[i,j]:=sum/count;
      dd0[j,i]:=dd0[i,j];
    end;
  end;
end;

procedure init_sd;
var
  i,j      : integer;
  sum,mean : real;
begin
  sum:=0;
  for j:=2 to n do begin
    for i:=1 to (j-1) do begin
      sum:=sum+dd0[i,j];
    end;      
  end;
  
  mean:=sum/(((n*n)-n)/2);

  sum:=0;
  for j:=2 to n do begin
    for i:=1 to (j-1) do begin
      sum:=sum+sqr(dd0[i,j]-mean)
    end;
  end;
  
  mean:=sum/((((n*n)-n)/2)-1);

  sd:=sqrt(mean);
end;

procedure init_gg;
var
  i,j : integer;
begin
  for j:=2 to n do begin
    for i:=1 to (j-1) do begin
      gg[i,j]:=0;
    end;
  end;
end;

function rand(min,max:real):real;
begin
  rand:=min+((random(maxint)/(maxint-1))*(max-min));
end;

procedure rand_dd;
var
  i,j : integer;
begin
  for j:=2 to n do begin
    for i:=1 to (j-1) do begin
      dd[i,j]:=dd0[i,j]+rand(0,sd);
      dd[j,i]:=dd [i,j];
    end;
  end;    
end;

procedure initUsed;
var
  t : integer;
begin
  for t:=1 to (mn+mn-1) do
    used[t]:=false;
end;

procedure initClusters;
var
  k : integer;
begin
  for k := 1 to n do begin
    cluster[k]. left := k;
    cluster[k].right := k;
    cluster[k].number:= 1;
  end;
end;

procedure searchSmallest(var i,j:integer;t:integer);
var
  ii,jj    : integer;
  smallest : real;
begin
  smallest := 1.7E+38;
  for jj := 2 to t - 1 do
    if not used[jj]
      then begin
             for ii := 1 to jj - 1 do
               if not used[ii]
                 then begin
                        if (dd[ii,jj] < smallest)
                          then begin
                                 i := ii;
                                 j := jj;
                                 smallest := dd[i,j];
                               end
                          else {nothing};
                      end
                 else {nothing};
           end
      else {nothing};

  used[i]:=true;
  used[j]:=true;
end;

procedure singleLinkage(i,j,k,t:integer);
var
  dki, dkj, dkij : real;
begin
  if i > k then
    dki := dd[k,i]
  else
    dki := dd[i,k];

  if j > k then
    dkj := dd[k,j]
  else
    dkj := dd[j,k];

  if dki<dkj then
    dkij:=dki
  else
    dkij:=dkj;

  dd[k,t] := dkij;
end;

procedure completeLinkage(i,j,k,t:integer);
var
  dki, dkj, dkij : real;
begin
  if i > k then
    dki := dd[k,i]
  else
    dki := dd[i,k];

  if j > k then
    dkj := dd[k,j]
  else
    dkj := dd[j,k];

  if dki>dkj then
    dkij:=dki
  else
    dkij:=dkj;

  dd[k,t] := dkij;
end;

procedure unweightedAverage(i,j,k,t:integer);
var
  ni, nj         : integer;
  dki, dkj, dkij : real;
begin
  ni := cluster[i].number;
  nj := cluster[j].number;

  if i > k then
    dki := dd[k,i]
  else
    dki := dd[i,k];

  if j > k then
    dkj := dd[k,j]
  else
    dkj := dd[j,k];

  dkij := (ni / (ni + nj)) * dki
       +  (nj / (ni + nj)) * dkj;

  dd[k,t] := dkij;
end;

procedure weightedAverage(i,j,k,t:integer);
var
  dki, dkj, dkij : real;
begin
  if i > k then
    dki := dd[k,i]
  else
    dki := dd[i,k];

  if j > k then
    dkj := dd[k,j]
  else
    dkj := dd[j,k];

  dkij := (1/2) * dki
       +  (1/2) * dkj;

  dd[k,t] := dkij;
end;

procedure unweightedCentroid(i,j,k,t:integer);
var
  ni, nj              : integer;
  dki, dkj, dij, dkij : real;
begin
  ni := cluster[i].number;
  nj := cluster[j].number;

  if i > k then
    dki := dd[k,i]
  else
    dki := dd[i,k];

  if j > k then
    dkj := dd[k,j]
  else
    dkj := dd[j,k];

  dij := dd[i,j];

  dkij := (  ni        /   (ni + nj)               ) * dki
       +  (       nj   /                (ni + nj)  ) * dkj
       -  (( ni * nj ) / ( (ni + nj ) * (ni + nj) )) * dij;

  dd[k,t] := dkij;
end;

procedure weightedCentroid(i,j,k,t:integer);
var
  dki, dkj, dij, dkij : real;
begin
  if i > k then
    dki := dd[k,i]
  else
    dki := dd[i,k];

  if j > k then
    dkj := dd[k,j]
  else
    dkj := dd[j,k];

  dij := dd[i,j];

  dkij := (1/2) * dki
       +  (1/2) * dkj
       -  (1/4) * dij;

  dd[k,t] := dkij;
end;

procedure minimumVariance(i,j,k,t:integer);
var
  nk, ni, nj          : integer;
  dki, dkj, dij, dkij : real;
begin
  ni := cluster[i].number;
  nj := cluster[j].number;
  nk := cluster[k].number;

  if i > k then
    dki := dd[k,i]
  else
    dki := dd[i,k];

  if j > k then
    dkj := dd[k,j]
  else
    dkj := dd[j,k];

  dij := dd[i,j];

  dkij := ((nk + ni) / (nk + ni + nj)) * dki
       +  ((nk + nj) / (nk + ni + nj)) * dkj
       -  ( nk       / (nk + ni + nj)) * dij;

  dd[k,t] := dkij;
end;

procedure updateMatrix(i,j,t:integer);
var
  k : integer;
begin
  for k := 1 to (t - 1) do
    case methodc of
      1: singleLinkage     (i,j,k,t);
      2: completeLinkage   (i,j,k,t);
      3: unweightedAverage (i,j,k,t);
      4: weightedAverage   (i,j,k,t);
      5: unweightedCentroid(i,j,k,t);
      6: weightedCentroid  (i,j,k,t);
      7: minimumVariance   (i,j,k,t);
    end;
end;

function distance(i,j:integer):real;
begin
  if i>j
    then distance:=dd[j,i]
    else distance:=dd[i,j];
end;

procedure calc(i,j:integer;var rl,ll,lr,rr:real);
begin
  rl:=distance(cluster[i].right,cluster[j]. left);
  ll:=distance(cluster[i]. left,cluster[j]. left);
  lr:=distance(cluster[i]. left,cluster[j].right);
  rr:=distance(cluster[i].right,cluster[j].right);
end;

procedure swap(var i,j:integer);
var
  t : integer;
begin
  t:=i; i:=j; j:=t;
end;

procedure mirror(t:integer);
begin
  if cluster[t].number>1
    then begin
           swap(cluster[t].left,cluster[t].right);
           mirror(cluster[t]. left);
           mirror(cluster[t].right);
         end
    else {nothing};
end;

procedure check(i,j:integer);
var
  rl,ll,lr,rr : real;
begin
  calc(i,j,rl,ll,lr,rr);

  if (rl<=ll) and (rl<=lr) and (rl<=rr)
    then {nothing}
    else

  if (ll<=lr) and (ll<=rl) and (ll<=rr)
    then begin
           mirror(i);
         end
    else

  if (lr<=ll) and (lr<=rl) and (lr<=rr)
    then begin
           mirror(i);
           mirror(j);
         end
    else

  if (rr<=ll) and (rr<=lr) and (rr<=rl)
    then begin
           mirror(j);
         end
    else {nothing};
end;

procedure makeCluster(i,j,t:integer);
begin
  check(i,j);
  cluster[t]. left  := i;
  cluster[t].right  := j;
  cluster[t].number := cluster[i].number + cluster[j].number;
end;

function inTree(node,term:integer):boolean;
begin
  if (node=term)
    then intree:=true
    else
  if (node<=n)
    then intree:=false
    else

  inTree:=inTree(cluster[node].left ,term) or
          inTree(cluster[node].right,term)
end;

function cv(i,j:integer;t0:integer):real;
var
  t     : integer;
  found : boolean;
begin
  found:=false;
  t:=n+n-t0;

  while (not found) and (t<(n+n-1))do begin
    t:=t+1;
    found:=(inTree(cluster[t].left,i) and inTree(cluster[t].right,j)) or
           (inTree(cluster[t].left,j) and inTree(cluster[t].right,i));
  end;

  if found 
    then if cluster[t].left<cluster[t].right
           then cv:=dd[cluster[t].left,cluster[t].right]
           else cv:=dd[cluster[t].right,cluster[t].left]
    else cv:=0;
end;

function copheneticCorrelation(t:integer):real;
var
  i,j                 : integer;
  sumx, sumy          : real;
  sumxy, sumxx, sumyy : real;
  nxy, nxx, nyy       : real;
  v1, v2              : real;
begin
  sumx:=0;
  sumy:=0;

  sumxy:=0;
  sumxx:=0;
  sumyy:=0;

  for j:=2 to n do begin
    for i:=1 to (j-1) do begin
      v1:=dd[i,j];
      v2:=cv(i,j,t);

      sumx:=sumx+v1;
      sumy:=sumy+v2;

      sumxy:=sumxy+(v1*v2);
      sumxx:=sumxx+(v1*v1);
      sumyy:=sumyy+(v2*v2);
    end;
  end;

  nxy:=(sumx*sumy)/(((n*n)-n)/2);
  nxx:=(sumx*sumx)/(((n*n)-n)/2);
  nyy:=(sumy*sumy)/(((n*n)-n)/2);

  copheneticCorrelation:=(sumxy-nxy)/sqrt((sumxx-nxx)*(sumyy-nyy));
end;

function log10(value:real):real;
begin
  log10:=ln(value)/ln(10);
end;

procedure findElbow(c:ctype;n:integer;var number:integer);
var
  i                         : integer;
  sumx,sumy,sumxx,sumxy     : real;
  a,b                       : real;
  predicted,residue,largest : real;
begin
  sumx:=0;
  sumy:=0;
  
  sumxx:=0;      
  sumxy:=0;

  for i:=1 to n do begin
    sumx:=sumx+log10(i);
    sumy:=sumy+ c[i];
    
    sumxx:=sumxx+(log10(i)*log10(i));
    sumxy:=sumxy+(log10(i)* c[i]);
  end;
  
  b:=((n*sumxy)-(sumx*sumy))/((n*sumxx)-(sumx*sumx));
  a:=(sumy/n)-((b*sumx)/n);

  largest:=-1*1.7E+38;
  number :=0;
  
  for i:=1 to n do begin
    predicted:=a+(b*log10(i));                
    residue  :=c[i]-predicted;

    if residue > largest
      then begin
             largest:=residue;
             number:=i;             	     
           end
      else {nothing};	    
  end;
end;

procedure updateCounts(t0:integer);
var
  i,j   : integer;
  t     : integer;
  found : boolean;
begin
  gg0:=gg;

  for j:=2 to n do begin
    for i:=1 to (j-1) do begin
      found:=false;
      t:=n+n-t0;

      while (not found) and (t<(n+n-1))do begin
        t:=t+1;
        found:=(inTree(cluster[t].left,i) and inTree(cluster[t].right,j)) or
               (inTree(cluster[t].left,j) and inTree(cluster[t].right,i));
      end;

      if not found 
        then gg[i,j]:=gg[i,j]+1
        else {nothing};
    end;
  end;
end;

function correlation:real;
var
  i,j                 : integer;
  sumx, sumy          : real;
  sumxy, sumxx, sumyy : real;
  nxy, nxx, nyy       : real;
  v1, v2              : real;
  den, corr           : real;
begin
  sumx:=0;
  sumy:=0;

  sumxy:=0;
  sumxx:=0;
  sumyy:=0;

  for j:=2 to n do begin
    for i:=1 to (j-1) do begin
      v1:=gg0[i,j];
      v2:=gg [i,j];

      sumx:=sumx+v1;
      sumy:=sumy+v2;

      sumxy:=sumxy+(v1*v2);
      sumxx:=sumxx+(v1*v1);
      sumyy:=sumyy+(v2*v2);
    end;
  end;

  nxy:=(sumx*sumy)/(((n*n)-n)/2);
  nxx:=(sumx*sumx)/(((n*n)-n)/2);
  nyy:=(sumy*sumy)/(((n*n)-n)/2);

  den:= sqrt((sumxx-nxx)*(sumyy-nyy));
  
  if (den > 0)
    then corr:=(sumxy-nxy)/den
    else corr:=0;
  
  Append (logfile);
  writeln(logfile, corr);
  close  (logfile);
  
  correlation:=corr;
end;

procedure setLinks;
var
  i,j : integer;
begin
  for i:=1 to n do begin
    gg[i,i]:=1;
  end;

  for j:=2 to n do begin
    for i:=1 to (j-1) do begin
      if ((gg[i,j]/nruns)>=0.95)
        then gg[i,j]:=0
        else gg[i,j]:=1;
        
      gg[j,i]:=gg[i,j]	
    end;
  end;
end;

function inGroup(i:integer):boolean;
var
  j     : integer;
  found : boolean;
begin
  j:=0;
  found:=false;

  while (j<n) and (not found) do begin
    j:=j+1;
    found:=(gg[i,j]=0);
  end;

  inGroup:=found;
end;

procedure updateGroups(i:integer);
var
  j : integer;
begin
  for j:=1 to n do begin
    if (gg[i,j]=0) and (p[j]=-1)
      then begin
             p[j]:=p[i];
             updateGroups(j);
           end
      else {nothing};
  end;
end;

procedure findGroups;
var
  i : integer;
begin
  for i:=1 to mn do begin
    p[i]:=-1;
  end;

  group:=0;
  for i:=1 to n do begin
    if ingroup(i) and (p[i]=-1)
      then begin
             group:=group+1;
             p[i]:=group;
             updategroups(i);
           end
      else 

    if (not ingroup(i)) and (p[i]=-1)
      then begin
             p[i]:=0;
           end
      else {nothing};
  end;
end;

procedure printPartition;
var
  i      : integer;
  fp,fpo : text;
  s      : string[255];
begin
  openr(fp ,path+fname);
  openw(fpo,path+'partition.csv');

  i:=0;
  while not eof(fp) do begin
    inc(i);
    readln(fp,s);
    writeln(fpo,p[i]:4,',',s)
  end;

  close(fp );
  close(fpo);
end;

begin{main}
  getparameters;
  
  openw(logfile,path+'logfile.txt');
  close(logfile);
  
  countVarieties;
  countItems;
  
  case methodr of
    1 : mmin:=1;
    2 : mmin:=0;
  end;

  if (n>=3) and (n<=mn) and (m>mmin) and (m<=mm)
    then begin
           openr(dfile,path+dname);
           
           if methodr=2
             then begin
                    read_dd2(dfile);
                    init_sd;
                  end;

           init_gg;
           randomize;

           nruns:=0;

           repeat
             nruns:=nruns+1;
           
             if methodr=1
               then begin
                      read_dd1(dfile);
                    end;

             if methodr=2
               then begin
                      rand_dd;
                    end;

             initUsed;
             initClusters;

             for t := n + 1 to n + n - 1 do begin
               searchSmallest(i,j,t);
               updateMatrix(i,j,t);
               makeCluster(i,j,t);
             end;

             c[1]:=0;
             for t := 2 to n do begin
               c[t]:=sqr(copheneticCorrelation(t));
             end;    

             findElbow(c,n,number);
             updateCounts(number);
           until (nruns>1) and (correlation>mincor);

           close(dfile);

           writeln(stderr, 'Number of runs: ', nruns);

           setLinks;
           findGroups;
           printPartition;
         end
    else if (methodr=1) and (m=1)
           then writeln(stderr, 'Bootstrap clustering cannot be applied to this data set, try clustering with noise instead!')
           else writeln(stderr, 'Robust    clustering cannot be applied to this data set!');
end  {main}.
