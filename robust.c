/* (c) W.J. Heeringa 2025 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <locale.h>
#include <time.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <stdbool.h>
#include <errno.h>

/* const */

#define mn 1500
#define mm 1000

/* type */

typedef double ddtype[mn + mn - 1][mn + mn - 1];

typedef bool usedtype[mn + mn - 1];

typedef struct clusterrec 
{
  short left, right, number;
} clusterrec;

typedef clusterrec clustertype[mn + mn - 1];

typedef double ctype[mn];

typedef double ggtype[mn][mn];

typedef short ptype[mn];

/* var */

short       m, n, mmin, t, i, j;
ddtype      dd, dd0;
double      sd;
usedtype    used;
clustertype cluster;
ctype       c;
ggtype      gg, gg0;
short       number;
char        *programname;
FILE        *dfile, *logfile;
char        fname[256], iname[256], dname[256];
short       methodc, methodr, numIter, nIter;
ptype       p;
short       group;
double      cr;
double      stabcoef;
char        path[1024], path0[1024];
char        *endptr;
struct      lconv *loc;

/* functions */

int fEOLN (FILE *f)
{
  register char ch;

  ch = getc (f);

  if (ch == EOF)
    return 1;

  if (ch == '\r')
  {
    ch = getc (f);
  }

  ungetc (ch, f);
  return (ch == '\n');
}

int fEOF (FILE *f)
{
  register char ch;

  if (feof (f))
    return 1;

  if (f == stdin)
    return 0; /* not safe to look-ahead on the keyboard! */

  ch = getc (f);

  if (ch == EOF)
    return 1;

  ungetc (ch, f);
  return 0;
}

void die (char const *format, ...)
{
  va_list list;

  fprintf (stderr, "\nError %s: ", programname);

  va_start (list, format);
  vfprintf (stderr, format, list);

  fprintf (stderr, "\n\n");

  exit (1);
}

void openr(FILE **fp,char *name)
{
  *fp = NULL;

  if (*fp != NULL)
  {
    *fp = freopen(name, "r", *fp);
  }
  else
  {
    *fp = fopen(name, "r");
  }
  if (*fp == NULL)
  {
     die ("Opening file \"%s\": %s", name, strerror (errno));
  }
}

void openw(FILE **fp,char *name)
{
  *fp = NULL;

  if (*fp != NULL)
  {
    *fp = freopen(name, "w", *fp);
  }
  else
  {
    *fp = fopen(name, "w");
  }
  if (*fp == NULL)
  {
     die ("Opening file \"%s\": %s", name, strerror (errno));
  }
}

void get_programname (char const *argv0)
{
#ifdef __MSDOS__
  char name [256];

  fnsplit (argv0, NULL, NULL, name, NULL);
  programname = strdup (name);

#else /* linux */
  char *p;

  p = strrchr (argv0, '/');

  if (p)
    programname = strdup (p + 1);
  else
    programname = strdup (argv0);
#endif
}

void usage()
{
  fprintf
  (
    stderr,
    "\n"
    "(c) W. J. Heeringa 2025\n"
    "\n"
    "Usage: %s filfile itmfile disfile 1-7 1|2 nIter path\n"
    "\n"
    "filfile: file with files       \n"
    "itmfile: file with items       \n"
    "disfile: file with distances   \n"
    "\n"
    "1: Single linkage              \n"
    "2: Complete linkage            \n"
    "3: Unweighted average          \n"
    "4: Weighted average            \n"
    "5: Unweighted centroid         \n"
    "6: Weighted centroid           \n"
    "7: Minimum variance            \n"
    "\n"
    "1: bootstrap                   \n"
    "2: noise                       \n"
    "\n"
    "nIter: number of iterations    \n"
    "\n"
    "path: path of working directory\n"
    "\n",

    programname
  );
  exit (1);
}

void countVarieties()
{
  FILE *fp = NULL;
  char s[256];
  char *TEMP;

  strcpy(path0, path);
  strcat(path0, fname);
  openr(&fp, path0);

  n = 0;
  while (!fEOF(fp)) 
  {
    n++;
    fgets(s, 256, fp);
    TEMP = strchr(s, '\n');
    if (TEMP != NULL)
      *TEMP = 0;
  }

  fclose(fp);
}

void countItems()
{
  FILE *fp = NULL;
  char s[256];
  char *TEMP;

  strcpy(path0, path);
  strcat(path0, iname);
  openr(&fp, path0);

  m = 0;
  while (!fEOF(fp)) 
  {
    m++;
    fgets(s, 256, fp);
    TEMP = strchr(s, '\n');
    if (TEMP != NULL)
      *TEMP = 0;
  }

  fclose(fp);
}

void replace_char(char *str, char oldChar, char newChar) 
{
  while (*str) 
  {
    if (*str == oldChar) *str = newChar;
    str++;
  }
}

void readLine(FILE **fp, double *value)
{
  short i, p;
  char s[256], s0[256];
  char *TEMP;
  char *endptr;
  
  fgets(s, 256, *fp);
  TEMP = strchr(s, '\n');
  if (TEMP != NULL)
    *TEMP = 0;
  i = strlen(s);

  while (s[i-1] != '\t')
    i--;

  p = i + 1;

  strcpy(s0, s + p - 1);

  if (loc->decimal_point[0] == ',' && loc->decimal_point[1] == '\0') 
  {
    replace_char(s0, '.', ',');
  } 
  else {}

  if (p <= strlen(s)) 
  {
    *value = strtod(s0, &endptr);
  } 
  else
  {
    *value = -1.0;
  }
}

void read_dd1(FILE **fp)
{
  short i, j, w;
  double v;
  double list[mm];
  short index;
  double sum, count;

  rewind(*fp);
  fscanf(*fp, "%*[^\n]");

  getc(*fp);
  for (i = 0; i <= mn - 1; i++) 
  {
    for (j = 0; j <= mn - 1; j++)
    {
      dd[i][j] = 0.0;
    }
  }

  for (w = 0; w <= m - 1; w++)
    list[w] = 0.0;

  for (w = 1; w <= m; w++) 
  {
    index = rand() % m + 1;
    list[index-1]++;
  }

  for (i = 2; i <= n; i++) 
  {
    for (j = 0; j <= i - 2; j++) 
    {
      sum = 0.0;
      count = 0.0;

      for (w = 0; w <= m - 1; w++) 
      {
        readLine(fp, &v);

        if (v >= 0 && list[w] >= 1) 
        {
	      sum += v * list[w];
	      count += list[w];
	    }
      }

      dd[i-1][j] = sum / count;
      dd[j][i-1] = dd[i-1][j];
    }
  }
}

void read_dd2(FILE **fp)
{
  short i, j, w;
  double v, sum, count;

  rewind(*fp);
  fscanf(*fp, "%*[^\n]");

  getc(*fp);
  for (i = 0; i <= mn - 1; i++) 
  {
    for (j = 0; j <= mn - 1; j++)
    {
      dd0[i][j] = 0.0;
    }
  }

  for (i = 2; i <= n; i++) 
  {
    for (j = 0; j <= i - 2; j++) 
    {
      sum = 0.0;
      count = 0.0;

      for (w = 1; w <= m; w++) 
      {
	    readLine(fp, &v);

	    if (v >= 0) 
	    {
	      sum += v;
	      count++;
	    }
      }

      dd0[i-1][j] = sum / count;
      dd0[j][i-1] = dd0[i-1][j];
    }
  }
}

void init_sd()
{
  short i, j;
  double sum = 0.0;
  double mean;
  double TEMP;

  for (j = 2; j <= n; j++) 
  {
    for (i = 0; i <= j - 2; i++)
    {
      sum += dd0[i][j-1];
    }
  }

  mean = sum / ((n * n - n) / 2.0);

  sum = 0.0;
  for (j = 2; j <= n; j++) 
  {
    for (i = 0; i <= j - 2; i++) 
    {
      TEMP = dd0[i][j-1] - mean;
      sum += TEMP * TEMP;
    }
  }

  mean = sum / ((n * n - n) / 2.0 - 1);

  sd = sqrt(mean);
}

void init_gg()
{
  short i, j;

  for (j = 2; j <= n; j++) 
  {
    for (i = 0; i <= j - 2; i++)
    {
      gg[i][j-1] = 0.0;
    }
  }
}

double rand_(double min, double max)
{
  return (min + ((double)rand() / (RAND_MAX)) * (max - min));
}

void rand_dd()
{
  short i, j;

  for (j = 2; j <= n; j++) 
  {
    for (i = 0; i <= j - 2; i++) 
    {
      dd[i][j-1] = dd0[i][j-1] + rand_(0.0, sd);
      dd[j-1][i] = dd[i][j-1];
    }
  }
}

void initUsed()
{
  short t;

  for (t = 0; t <= mn + mn - 2; t++)
    used[t] = false;
}

void initClusters()
{
  short k;

  for (k = 1; k <= n; k++) 
  {
    cluster[k-1].left = k;
    cluster[k-1].right = k;
    cluster[k-1].number = 1;
  }
}

void searchSmallest(short *i, short *j, short t)
{
  short ii, jj;
  double smallest = 1.7e+38;

  for (jj = 2; jj <= t - 1; jj++) 
  {
    if (!used[jj-1]) 
    {
      for (ii = 1; ii <= jj - 1; ii++) 
      {
	    if (!used[ii-1]) 
	    {
	      if (dd[ii-1][jj-1] < smallest) 
	      {
	        *i = ii;
	        *j = jj;
	        smallest = dd[*i - 1][*j - 1];
          }
        }
      }
    }
  }

  used[*i - 1] = true;
  used[*j - 1] = true;
}

void singleLinkage(short i, short j, short k, short t)
{
  double dki, dkj, dkij;

  if (i > k)
    dki = dd[k-1][i-1];
  else
    dki = dd[i-1][k-1];

  if (j > k)
    dkj = dd[k-1][j-1];
  else
    dkj = dd[j-1][k-1];

  if (dki < dkj)
    dkij = dki;
  else
    dkij = dkj;

  dd[k-1][t-1] = dkij;
}

void completeLinkage(short i, short j, short k, short t)
{
  double dki, dkj, dkij;

  if (i > k)
    dki = dd[k-1][i-1];
  else
    dki = dd[i-1][k-1];

  if (j > k)
    dkj = dd[k-1][j-1];
  else
    dkj = dd[j-1][k-1];

  if (dki > dkj)
    dkij = dki;
  else
    dkij = dkj;

  dd[k-1][t-1] = dkij;
}

void unweightedAverage(short i, short j, short k, short t)
{
  short ni, nj;
  double dki, dkj, dkij;

  ni = cluster[i-1].number;
  nj = cluster[j-1].number;

  if (i > k)
    dki = dd[k-1][i-1];
  else
    dki = dd[i-1][k-1];

  if (j > k)
    dkj = dd[k-1][j-1];
  else
    dkj = dd[j-1][k-1];

  dkij = (double)ni / (ni + nj) * dki + 
         (double)nj / (ni + nj) * dkj;

  dd[k-1][t-1] = dkij;
}

void weightedAverage(short i, short j, short k, short t)
{
  double dki, dkj, dkij;

  if (i > k)
    dki = dd[k-1][i-1];
  else
    dki = dd[i-1][k-1];

  if (j > k)
    dkj = dd[k-1][j-1];
  else
    dkj = dd[j-1][k-1];

  dkij = 1.0 / 2 * dki + 
         1.0 / 2 * dkj;

  dd[k-1][t-1] = dkij;
}

void unweightedCentroid(short i, short j, short k, short t)
{
  short ni, nj;
  double dki, dkj, dij, dkij;

  ni = cluster[i-1].number;
  nj = cluster[j-1].number;

  if (i > k)
    dki = dd[k-1][i-1];
  else
    dki = dd[i-1][k-1];

  if (j > k)
    dkj = dd[k-1][j-1];
  else
    dkj = dd[j-1][k-1];

  dij = dd[i-1][j-1];

  dkij = (double)ni        /  (ni + nj)              * dki + 
         (double)      nj  /              (ni + nj)  * dkj - 
         (double)(ni * nj) / ((ni + nj) * (ni + nj)) * dij;

  dd[k-1][t-1] = dkij;
}

void weightedCentroid(short i, short j, short k, short t)
{
  double dki, dkj, dij, dkij;

  if (i > k)
    dki = dd[k-1][i-1];
  else
    dki = dd[i-1][k-1];

  if (j > k)
    dkj = dd[k-1][j-1];
  else
    dkj = dd[j-1][k-1];

  dij = dd[i-1][j-1];

  dkij = 1.0 / 2 * dki + 
         1.0 / 2 * dkj - 
         1.0 / 4 * dij;

  dd[k-1][t-1] = dkij;
}

void minimumVariance(short i, short j, short k, short t)
{
  short nk, ni, nj;
  double dki, dkj, dij, dkij;

  ni = cluster[i-1].number;
  nj = cluster[j-1].number;
  nk = cluster[k-1].number;

  if (i > k)
    dki = dd[k-1][i-1];
  else
    dki = dd[i-1][k-1];

  if (j > k)
    dkj = dd[k-1][j-1];
  else
    dkj = dd[j-1][k-1];

  dij = dd[i-1][j-1];

  dkij = (double)(nk + ni) / (nk + ni + nj) * dki +
	     (double)(nk + nj) / (nk + ni + nj) * dkj -
	     (double) nk       / (nk + ni + nj) * dij;

  dd[k-1][t-1] = dkij;
}

void updateMatrix(short i, short j, short t)
{
  short k;

  for (k = 1; k <= t - 1; k++) 
  {
    switch (methodc) 
    {
      case 1:
        singleLinkage(i, j, k, t);
        break;

      case 2:
        completeLinkage(i, j, k, t);
        break;

      case 3:
        unweightedAverage(i, j, k, t);
        break;

      case 4:
        weightedAverage(i, j, k, t);
        break;

      case 5:
        unweightedCentroid(i, j, k, t);
        break;

      case 6:
        weightedCentroid(i, j, k, t);
        break;

      case 7:
        minimumVariance(i, j, k, t);
        break;
    }
  }
}

double distance(short i, short j)
{
  if (i > j)
    return (dd[j-1][i-1]);
  else
    return (dd[i-1][j-1]);
}

void calc(short i, short j, double *rl, double *ll, double *lr, double *rr)
{
  *rl = distance(cluster[i-1].right, cluster[j-1].left);
  *ll = distance(cluster[i-1].left , cluster[j-1].left);
  *lr = distance(cluster[i-1].left , cluster[j-1].right);
  *rr = distance(cluster[i-1].right, cluster[j-1].right);
}

void swap_(short *i, short *j)
{
  short t;

  t = *i;
  *i = *j;
  *j = t;
}

void mirror(short t)
{
  if (cluster[t-1].number > 1)
  {
    swap_(&cluster[t-1].left, &cluster[t-1].right);
    mirror(cluster[t-1].left);
    mirror(cluster[t-1].right);
  }
  else
    return;
}

void check(short i, short j)
{
  double rl, ll, lr, rr;

  calc(i, j, &rl, &ll, &lr, &rr);

  if (rl <= ll && rl <= lr && rl <= rr)
  {
    return;
  }
  else
  
  if (ll <= lr && ll <= rl && ll <= rr) 
  {
    mirror(i);
  }
  else
  
  if (lr <= ll && lr <= rl && lr <= rr) 
  {
    mirror(i);
    mirror(j);
  } 
  else 
  
  if (rr <= ll && rr <= lr && rr <= rl) 
  {
    mirror(j);
  }
  else
  {
    return;
  }  
}

void makeCluster(short i, short j, short t)
{
  check(i, j);
  cluster[t-1].left = i;
  cluster[t-1].right = j;
  cluster[t-1].number = cluster[i-1].number + cluster[j-1].number;
}

bool inTree(short node, short term)
{
  if (node == term)
    return true;
  else
  
  if (node <= n)
    return false;
  else
    return (inTree(cluster[node-1].left , term) || 
            inTree(cluster[node-1].right, term));
}

double cv(short i, short j, short t0)
{
  short t;
  bool found = false;

  t = n + n - t0;

  while (!found && t < n + n - 1) 
  {
    t++;
    found = (inTree(cluster[t-1].left, i) && inTree(cluster[t-1].right, j)) ||
	        (inTree(cluster[t-1].left, j) && inTree(cluster[t-1].right, i));
  }

  if (found) 
  {
    if (cluster[t-1].left < cluster[t-1].right)
      return (dd[cluster[t-1].left - 1][cluster[t-1].right - 1]);
    else
      return (dd[cluster[t-1].right - 1][cluster[t-1].left - 1]);
  } 
  else
    return 0.0;
}

double copheneticCorrelation(short t)
{
  short i, j;
  double sumx = 0.0, sumy = 0.0, sumxy = 0.0, sumxx = 0.0, sumyy = 0.0;
  double nxy, nxx, nyy, v1, v2;

  for (j = 2; j <= n; j++) 
  {
    for (i = 1; i <= j - 1; i++) 
    {
      v1 = dd[i-1][j-1];
      v2 = cv(i, j, t);

      sumx += v1;
      sumy += v2;

      sumxy += v1 * v2;
      sumxx += v1 * v1;
      sumyy += v2 * v2;
    }
  }

  nxy = sumx * sumy / ((n * n - n) / 2.0);
  nxx = sumx * sumx / ((n * n - n) / 2.0);
  nyy = sumy * sumy / ((n * n - n) / 2.0);

  return ((sumxy - nxy) / sqrt((sumxx - nxx) * (sumyy - nyy)));
}

double log10_(double value)
{
  return (log(value) / log(10.0));
}

void findElbow(double *c, short n, short *number)
{
  short i;
  double sumx = 0.0, sumy = 0.0, sumxx = 0.0, sumxy = 0.0;
  double a, b, predicted, residue, largest = -1 * 1.7e+38;

  for (i = 1; i <= n; i++) 
  {
    sumx += log10_((double)i);
    sumy += c[i-1];

    sumxx += log10_((double)i) * log10_((double)i);
    sumxy += log10_((double)i) * c[i-1];
  }

  b = (n * sumxy - sumx * sumy) / (n * sumxx - sumx * sumx);
  a = sumy / n - b * sumx / n;

  *number = 0;

  for (i = 1; i <= n; i++) 
  {
    predicted = a + b * log10_((double)i);
    residue = c[i-1] - predicted;

    if (residue > largest) 
    {
      largest = residue;
      *number = i;
    }
  }
}

void updateCounts(short t0)
{
  short i, j, t;
  bool found;

  memcpy(gg0, gg, sizeof(ggtype));

  for (j = 2; j <= n; j++) 
  {
    for (i = 1; i <= j - 1; i++) 
    {
      found = false;
      t = n + n - t0;

      while (!found && t < n + n - 1) 
      {
        t++;
        found = (inTree(cluster[t-1].left, i) && inTree(cluster[t-1].right, j)) ||
                (inTree(cluster[t-1].left, j) && inTree(cluster[t-1].right, i));
      }

      if (!found) 
      {
        gg[i-1][j-1]++;
      }
    }
  }
}

double correlation()
{
  short i, j;
  double sumx = 0.0, sumy = 0.0, sumxy = 0.0, sumxx = 0.0, sumyy = 0.0;
  double nxy, nxx, nyy, v1, v2, den, corr;

  for (j = 2; j <= n; j++) 
  {
    for (i = 0; i <= j - 2; i++) 
    {
      v1 = gg0[i][j-1];
      v2 = gg [i][j-1];

      sumx += v1;
      sumy += v2;

      sumxy += v1 * v2;
      sumxx += v1 * v1;
      sumyy += v2 * v2;
    }
  }

  nxy = sumx * sumy / ((n * n - n) / 2.0);
  nxx = sumx * sumx / ((n * n - n) / 2.0);
  nyy = sumy * sumy / ((n * n - n) / 2.0);

  den = sqrt((sumxx - nxx) * (sumyy - nyy));

  if (den > 0)
    corr = (sumxy - nxy) / den;
  else
    corr = 0.0;
  
  strcpy(path0, path);
  strcat(path0, "logfile.txt");
    
  if (logfile != NULL) 
  {
    logfile = freopen(path0, "a", logfile);
  } 
  else
    logfile = fopen  (path0, "a");
  
  fprintf(logfile, "% .5E\n", corr);

  return corr;
}

void setLinks()
{
  short i, j;

  for (i = 0; i <= n - 1; i++)
  {
    gg[i][i] = 1.0;
  }

  for (j = 2; j <= n; j++) 
  {
    for (i = 0; i <= j - 2; i++) 
    {
      if (gg[i][j-1] / numIter >= 0.95)
      {
        gg[i][j-1] = 0.0;
      }
      else
      {
        gg[i][j-1] = 1.0;
      }

      gg[j-1][i] = gg[i][j-1];
    }
  }
}

bool inGroup(short i)
{
  short j = 0;
  bool found = false;

  while (j < n && !found) 
  {
    j++;
    found = (gg[i-1][j-1] == 0);
  }

  return found;
}

void updateGroups(short i)
{
  short j;

  for (j = 1; j <= n; j++) 
  {
    if (gg[i-1][j-1] == 0 && p[j-1] == -1) 
    {
      p[j-1] = p[i-1];
      updateGroups(j);
    }
  }
}

void findGroups()
{
  short i;

  for (i = 0; i <= mn - 1; i++)
  {
    p[i] = -1;
  }

  group = 0;

  for (i = 1; i <= n; i++) 
  {
    if (inGroup(i) && p[i-1] == -1) 
    {
      group++;
      p[i-1] = group;
      updateGroups(i);
    } 
    else
    
    if (!inGroup(i) && p[i-1] == -1) 
    {
	  p[i-1] = 0;
    }
  }
}

void printPartition()
{
  short i = 0;
  FILE *fp = NULL, *fpo = NULL;
  char s[256];
  char *TEMP;

  strcpy(path0, path);
  strcat(path0, fname);
  openr(&fp, path0);

  strcpy(path0, path);
  strcat(path0, "partition.csv");
  openw(&fpo,path0);

  while (!fEOF(fp)) 
  {
    i++;
    fgets(s, 256, fp);
    TEMP = strchr(s, '\n');
    if (TEMP != NULL)
      *TEMP = 0;
    fprintf(fpo, "%4d,%s\n", p[i-1], s);
  }

  fclose(fp);
  fclose(fpo);
}

/* main */

int main(int argc, char *argv[])
{
  setlocale(LC_ALL, "");
  loc = localeconv();

  get_programname (argv [0]);

  if (argc == 8) 
  {
    strcpy(fname, argv[1]);
    strcpy(iname, argv[2]);
    strcpy(dname, argv[3]);

    if (!strcmp(argv[4] , "1")) methodc = 1; else
    if (!strcmp(argv[4] , "2")) methodc = 2; else
    if (!strcmp(argv[4] , "3")) methodc = 3; else
	if (!strcmp(argv[4] , "4")) methodc = 4; else
	if (!strcmp(argv[4] , "5")) methodc = 5; else
	if (!strcmp(argv[4] , "6")) methodc = 6; else
	if (!strcmp(argv[4] , "7")) methodc = 7; else usage();

    if (!strcmp(argv[5] , "1")) methodr = 1; else
    if (!strcmp(argv[5] , "2")) methodr = 2; else usage();

    numIter = strtol(argv[6], &endptr, 10);

    strcpy(path, argv[7]);
  }
  else
    usage();

  strcpy(path0, path);
  strcat(path0, "logfile.txt");
  openw(&logfile,path0);
  
  countVarieties();
  countItems();

  switch (methodr) 
  {
    case 1:
      mmin = 1;
      break;

    case 2:
      mmin = 0;
      break;
   }

  if (n >= 3 && n <= mn && m > mmin && m <= mm) 
  {
    strcpy(path0, path);
    strcat(path0, dname);
    openr(&dfile, path0);

    if (methodr == 2) 
    {
      read_dd2(&dfile);
      init_sd();
    }

    init_gg();
    srand((unsigned int) time(NULL));

    for (nIter = 1; nIter <= numIter; nIter++) 
    {
      if (methodr == 1)
	    read_dd1(&dfile);

      if (methodr == 2)
	    rand_dd();

      initUsed();
      initClusters();

      for (t = (n + 1); t <= (n + n - 1); t++) 
      {
	    searchSmallest(&i, &j, t);
	    updateMatrix(i, j, t);
	    makeCluster(i, j, t);
      }

      c[0] = 0.0;
      for (t = 2; t <= n; t++) 
      {
	    cr = copheneticCorrelation(t);
	    c[t-1] = cr * cr;
      }

      findElbow(c, n, &number);
      updateCounts(number);
      stabcoef = correlation();
    }

    fclose(dfile);

    fprintf(stderr, "Stability coefficient: %1.8f\n", stabcoef);

    setLinks();
    findGroups();
    printPartition();
  }
  else 

  if (methodr == 1 && m == 1)
    fprintf(stderr, "Bootstrap clustering cannot be applied to this data set, try clustering with noise instead!\n");
  else
    fprintf(stderr, "Robust    clustering cannot be applied to this data set!\n");

  fclose(logfile);

  return 0;
}
