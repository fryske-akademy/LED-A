{ (c) W.J. Heeringa 2022 }

program calculate_conbachs_alfa(input,output,stderr);

uses 
  SysUtils;

const
  mn = 1000;
  mm = 1500;

type
   wtype      = array[1..mm] of real;
  ddtype      = array[1..mn] of array[1..mn] of real;

var
  n,nn,m      : longint;
  mean        : wtype;
  meanT       : real;
  total       : ddtype;
  variance    : real;
  varianceT   : real;
  alpha       : real;
  fp          : text;
  fname,iname : string[255];
  dname,path  : string[255];

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
  writeln(stderr,'Usage: ',paramstr(0),' filfile itmfile disfile path');
  writeln(stderr);
  writeln(stderr,'filfile: file with file names     ');
  writeln(stderr,'itmfile: file with items          ');
  writeln(stderr,'disfile: file with distances      ');
  writeln(stderr);
  writeln(stderr,'path   : path of working directory');
  writeln(stderr);
  halt;
end;

procedure getparameters;
begin
  if paramcount=4
   then begin
          fname:=paramstr(1);
          iname:=paramstr(2);
          dname:=paramstr(3);
          path :=paramstr(4);
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

procedure readdist(var fp:text;var value:real);
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

procedure calcMissing(var fp:text);
var
  i,j,t : longint;
  value : real;
  m0    : longint;
begin
  readln(fp);

  for i:=2 to n do begin
    for j:=1 to (i-1) do begin
      total[j,i]:=0;
      m0:=0;
    
      for t:=1 to m do begin
        readdist(fp,value);
                
        if (value<>-1)
          then begin
                 total[j,i]:=total[j,i]+value;
                 m0:=m0+1;
               end
          else {nothing};
      end;
      
      total[j,i]:=total[j,i]/m0;
    end;
  end;
end;

procedure calcMean(var fp:text;var mean:wtype;var meanT:real);
var
  i,j,t : longint;
  value : real;
  sum   : wtype;
  sumT  : real;
begin
  readln(fp);

  for t:=1 to m do begin
    sum[t]:=0;
  end;

  sumT:=0;
    
  for i:=2 to n do begin
    for j:=1 to (i-1) do begin
      total[i,j]:=0;
    
      for t:=1 to m do begin
        readdist(fp,value);
        
        if (value=-1)
          then value:=total[j,i]
          else {nothing};
        
        sum  [t]  :=sum  [t]  +value;
        total[i,j]:=total[i,j]+value;
        sumT      :=sumT      +value;
      end;
    end;
  end;
  
  for t:=1 to m do begin
    mean[t]:=sum[t]/nn;
  end;
  
  meanT:=sumT/nn;
end;

procedure calcVariance(var fp:text;mean:wtype;meanT:real;var variance,varianceT:real);
var
  i,j,t : longint;
  value : real;
  sum   : real;
  sumT  : real;
begin
  readln(fp);

  sum :=0;
  sumT:=0;
  
  for i:=2 to n do begin
    for j:=1 to (i-1) do begin
      for t:=1 to m do begin
        readdist(fp,value);
        
        if (value=-1)
          then value:=total[j,i]
          else {nothing};
        
        sum:=sum+sqr(value-mean[t]);
      end;
      
      sumT:=sumT+sqr(total[i,j]-meanT);
    end;
  end;
  
  variance :=sum /(nn-1);
  varianceT:=sumT/(nn-1);
end;

begin{main}
  getparameters;

  countVarieties;
  countItems;

  if (n>2) and (n<=mn) and (m>1) and (m<=mm) and FileExists(path+dname)
    then begin
           nn:=((n*n)-n) div 2;
           openr(fp,path+dname);

           calcMissing(fp);
           reset(fp);
           calcMean(fp,mean,meanT);
           reset(fp);
           calcVariance(fp,mean,meanT,variance,varianceT);
          
           alpha := (m / (m-1)) * (1-variance/varianceT);
           writeln(stderr, 'Cronbach''s alpha: ', alpha:0:4);

           close(fp);
         end
end  {main}.
