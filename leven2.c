/* (c) W.J. Heeringa 2022 */

/* program calculate_Levenshtein_distance */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <stdbool.h>
#include <errno.h>

/* const */

#define mp      1000
#define maxreal 1e20

/* type */

typedef char headtype[3];
typedef char supratype[2];
typedef char diacritictype[2];

typedef struct segmenttype 
{
  headtype      head;
  supratype     supra[4];
  diacritictype diacritic[4];
} segmenttype;

typedef struct lettertype 
{
  segmenttype   segment[2];
} lettertype;

typedef struct wordtype 
{
  lettertype    letter[80];
  short         number;
} wordtype;

typedef struct listtype 
{
  wordtype      w_rd[30];
  short         number;
} listtype;

typedef struct comptype 
{
  short         number;
  segmenttype   segment[mp];
  char          vctype[mp];

  double        total;
  double        vector[mp];
  double        matrix[mp][mp];
} comptype;

typedef struct segmenttype0 
{
  short         sound;
} segmenttype0;

typedef struct lettertype0 
{
  segmenttype0  segment[2];
} lettertype0;

typedef struct wordtype0 
{
  lettertype0   letter[80];
  short number;
} wordtype0;

typedef struct listtype0 
{
  wordtype0     w_rd[30];
  short         number;
} listtype0;

typedef double matrixtype[81][81];

typedef short tracetype[81][81];

typedef struct alignrec 
{
  short         word1, word2;
  double        dist;
} alignrec;

typedef alignrec aligntype[80];

/* var */

comptype    comp0, comp1, comp2;
double      meanIndel, maxIndel, meanSubst, maxSubst;
bool        lg, sm;
lettertype0 empty;
matrixtype  matrix1, matrix2, matrix3;
tracetype   trace;
aligntype   align;
double      allDist, allNum;
char        *programname;
char        nname[256], fname[256], iname[256], sname[256];
short       method, normal, part;
double      r, r0;
bool        saveAl, saveIn, saveAg, saveCr, saveSo, saveXX;
FILE        *fileAl, *fileIn, *fileAg, *fileSo;
char        filename[256], itemname[256];
char        path[1024], path0[1024];

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
    "(c) W. J. Heeringa 2022\n"
    "\n"
    "Usage: %s namfile filfile itmfile segfile 1|2 0|1|2 0-4 0|1 0|1 0|1 0|1 path\n"
    "\n"
    "namfile: file with names                        \n"
    "filfile: file with files                        \n"
    "itmfile: file with items                        \n"
    "segfile: file with segment distances            \n"
    "\n"
    "1: Classical                                    \n"
    "2: PMI-based                                    \n"
    "\n"
    "0: Do nothing                                   \n"
    "1: Divide word pair distance by alignment length\n"
    "\n"
    "0: All                                          \n"
    "1: Only vowel     substitutions                 \n"
    "2: Only vowel     indels                        \n"
    "3: Only consonant substitutions                 \n"
    "4: Only consonant indels                        \n"
    "\n"
    "0: Do nothing                                   \n"
    "1: Save    sound    distances                   \n"
    "\n"
    "0: Do nothing                                   \n"
    "1: Save  alignments                             \n"
    "\n"
    "0: Do nothing                                   \n"
    "1: Save  word pair  distances                   \n"
    "\n"
    "0: Do nothing                                   \n"
    "1: Save  aggregated distances                   \n"
    "\n"
    "path: path to working directory                 \n"
    "\n",

    programname
  );
  exit (1);
}

bool inheads(char ch)
{
  if (ch == '0')
    return true;

  if (ch == 'i')
    return true;

  if (ch == 'y')
	return true;

  if (ch == '1')
    return true;
    
  if (ch == '}')
    return true;
    
  if (ch == 'M')
    return true;

  if (ch == 'u')
    return true;

  if (ch == 'I')
    return true;

  if (ch == 'Y')
    return true;

  if (ch == 'U')
    return true;

  if (ch == 'e')
    return true;

  if (ch == '2')
    return true;

  if (ch == '8')
    return true;
	
  if (ch == '7')
    return true;

  if (ch == 'o')
    return true;

  if (ch == '@')
    return true;

  if (ch == 'E')
    return true;

  if (ch == '9')
    return true;

  if (ch == '3')
	return true;

  if (ch == 'V')
    return true;

  if (ch == 'O')
    return true;
	  
  if (ch == '{')
    return true;

  if (ch == '6')
    return true;

  if (ch == 'a')
    return true;

  if (ch == '&')
    return true;

  if (ch == 'A')
    return true;

  if (ch == 'Q')
    return true;

  if (ch == 'p')
    return true;

  if (ch == 'b')
    return true;

  if (ch == 't')
    return true;

  if (ch == 'd')
	return true;
  
  if (ch == 'c')
    return true;
  
  if (ch == 'k')
    return true;
  
  if (ch == 'g')
    return true;

  if (ch == 'q')
    return true;

  if (ch == '?')
    return true;

  if (ch == 'm')
    return true;

  if (ch == 'F')
    return true;

  if (ch == 'n')
    return true;

  if (ch == 'J')
    return true;

  if (ch == 'N')
    return true;

  if (ch == 'r')
    return true;

  if (ch == '4')
    return true;

  if (ch == 'B')
    return true;

  if (ch == 'f')
    return true;

  if (ch == 'v')
    return true;

  if (ch == 'T')
    return true;

  if (ch == 'D')
    return true;

  if (ch == 's')
    return true;

  if (ch == 'z')
    return true;

  if (ch == 'S')
    return true;

  if (ch == 'Z')
    return true;

  if (ch == 'C')
    return true;

  if (ch == 'x')
    return true;

  if (ch == 'G')
    return true;

  if (ch == 'X')
    return true;

  if (ch == 'R')
    return true;
  
  if (ch == 'h')
    return true;

  if (ch == 'K')
    return true;

  if (ch == 'w')
    return true;

  if (ch == 'P')
    return true;

  if (ch == 'j')
    return true;

  if (ch == 'M')
    return true;

  if (ch == 'l')
    return true;

  if (ch == 'L')
    return true;

  if (ch == '\'')
    return true;

  if (ch == '"')
    return true;

  if (ch == '%')
    return true;
  else
    return false;
}

bool insupras(char ch)
{
  if (ch == ':')
    return true;
  else
    return false;
}

bool indiacritics(char ch)
{
  if (ch == '0')
    return true;

  if (ch == 'v')
    return true;

  if (ch == 'h')
	return true;

  if (ch == '+')
	return true;

  if (ch == '-')
    return true;

  if (ch == '=')
    return true;

  if (ch == 't')
    return true;

  if (ch == 'k')
    return true;

  if (ch == 'w')
    return true;

  if (ch == 'j')
    return true;

  if (ch == 'G')
    return true;

  if (ch == '?')
    return true;

  if (ch == 'e')
    return true;

  if (ch == 'a')
    return true;

  if (ch == '~')
	return true;

  if (ch == '^')
    return true;

  if (ch == '}')
    return true;

  if (ch == 'r')
    return true;

  if (ch == 'o')
    return true;

  if (ch == 'O')
    return true;

  if (ch == 'c')
    return true;

  if (ch == '/')
    return true;

  if (ch == '\\')
    return true;

  if (ch == '`')
    return true;

  if (ch == 'X')
    return true;
  else
    return false;
}

void halt0(char *string0)
{
  fprintf(stderr, "Check transcription of variety %s and item %s.\n", filename, itemname);
  exit(0);
}

void readchar(FILE **fp, char *ch)
{
  if (fEOLN(*fp))
    *ch = '$';
  else
    *ch = getc(*fp);
}

void initsegment(segmenttype *segment)
{
  short p, d;

  memcpy(segment->head, "   ", sizeof(headtype));

  for (p = 0; p <= 3; p++)
    memcpy(segment->supra[p], "  ", sizeof(supratype));

  for (d = 0; d <= 3; d++)
    memcpy(segment->diacritic[d], "  ", sizeof(diacritictype));
}

void initletter(lettertype *letter)
{
  short s;

  for (s = 0; s <= 1; s++)
    initsegment(&letter->segment[s]);
}

void rhead(FILE **fp, char *head, char *ch)
{
  if (inheads(*ch))
  {
    head[0] = *ch;
    readchar(fp, ch);
  
    if  (*ch == '\\') 
    {
      head[1] = *ch;
      readchar(fp, ch);
    }

    if ((*ch == '`' ) || (*ch == '<'))
    {
      head[2] = *ch;
      readchar(fp, ch);
    }
  }
  else
  {
    halt0("head expected");
    return;
  }  
}

void rsupra(FILE **fp, char *supra, char *ch)
{
  if (*ch == ':') 
  {
    supra[0] = *ch;
    readchar(fp, ch);
    
    if (*ch == '\\') 
    {
      supra[1] = *ch;
      readchar(fp, ch);
    }
  } 
  else
    halt0(": expected");
}

void rdiacritic(FILE **fp, char *diacritic, char *ch)
{
  if (*ch == '_') 
  {
    readchar(fp, ch);
  
    if (indiacritics(*ch)) 
    {
      diacritic[0] = *ch;
      readchar(fp, ch);
      
      if (*ch == '\\') 
      {
        diacritic[1] = *ch;
        readchar(fp, ch);
      }
    } 
    else
      halt0("diacritic expected");
  }
  else
  {
	halt0("_ expected");
    return;  
  }
}

void rsegment(FILE **fp, segmenttype *segment, char *ch)
{
  short p = 0, d = 0;

  while (insupras(*ch) || *ch == '_') 
  {
    if (insupras(*ch)) 
    {
      p++;
      rsupra(fp, segment->supra[p-1], ch);
    }
    else 
    {
      d++;
      rdiacritic(fp, segment->diacritic[d-1], ch);
    }
  }
}

void rletter(FILE **fp, lettertype *letter, char *ch)
{
  short s = 1;

  initletter(letter);
  if (inheads(*ch)) 
  {
    rhead(fp, letter->segment[s-1].head, ch);
    rsegment(fp, &letter->segment[s-1], ch);
    return;
  }
  
  if (*ch != '[') 
  {
    halt0("stress or head or [ expected");
    return;
  }
  readchar(fp, ch);
  rhead(fp, letter->segment[s-1].head, ch);

  while (inheads(*ch)) 
  {
    s++;
    rhead(fp, letter->segment[s-1].head, ch);
  }

  if (*ch == ']')
    readchar(fp, ch);
  else
    halt0("] expected");

  rsegment(fp, letter->segment, ch);
}

void rword(FILE **fp, wordtype *w_rd, char *ch)
{
  short l = 1;

  rletter(fp, &w_rd->letter[l-1], ch);
  while (inheads(*ch) || *ch == '[') 
  {
    l++;
    rletter(fp, &w_rd->letter[l-1], ch);
  }
  w_rd->number = l;
}

void rlist(FILE **fp, listtype *list, char *ch)
{
  short w = 1;

  rword(fp, &list->w_rd[w-1], ch);
  while (*ch == ' ') 
  {
    w++;
    readchar(fp, ch);
    if (*ch != '/') 
    {
      halt0("/ expected");
      continue;
    }
    readchar(fp, ch);
    if (*ch == ' ') 
    {
      readchar(fp, ch);
      rword(fp, &list->w_rd[w-1], ch);
    }
    else
      halt0("space expected");
  }
  
  list->number = w;
}

short sound(segmenttype segment)
{
  short t = 0;
  bool found = false;

  while (!found && t < comp1.number) 
  {
    t++;
    found = (strncmp(segment.head, comp1.segment[t-1].head, sizeof(headtype)) == 0);
  }

  if (!found) 
  {
    fprintf(stderr, "%.3s not found in segment distance table\n", segment.head);
    exit(0);
  }

  return t;
}

void change(listtype list, listtype0 *list0)
{
  short w, l, nr;

  for (w = 0; w <= list.number - 1; w++) 
  {
    nr = list.w_rd[w].number;
    
    for (l = 0; l <= nr - 1; l++) 
    {
      list0->w_rd[w].letter[l].segment[0].sound = sound(list.w_rd[w].letter[l].segment[0]);

      if (!strncmp(list.w_rd[w].letter[l].segment[1].head, "   ", sizeof(headtype)))
        list0->w_rd[w].letter[l].segment[1].sound = list0->w_rd[w].letter[l].segment[0].sound;
      else
        list0->w_rd[w].letter[l].segment[1].sound = sound(list.w_rd[w].letter[l].segment[1]);
    }

    list0->w_rd[w].number = list.w_rd[w].number;
  }

  list0->number = list.number;
}

void rline(FILE **fp, listtype0 *list0)
{
  listtype list;
  char ch;

  readchar(fp, &ch);

  if (inheads(ch) || ch == '[')
    rlist(fp, &list, &ch);
  else 
  
  if (ch == '#') 
  {
    list.number = 0;
    readchar(fp, &ch);
  } 
  else
    halt0("# expected");

  if (ch == '$') 
  {
    fscanf(*fp, "%*[^\n]");
    getc(*fp);
  } 
  else
    halt0("eoln expected");

  change(list, list0);
}

bool invowels(char *head)
{
  if (!strncmp(head, "i  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "y  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "1  ", sizeof(headtype)))
	return true;

  if (!strncmp(head, "}  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "M  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "u  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "I  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "Y  ", sizeof(headtype)))
    return true;
		
  if (!strncmp(head, "U  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "e  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "2  ", sizeof(headtype)))
	return true;

  if (!strncmp(head, "8  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "7  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "o  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "@  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "E  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "9  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "3  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "V  ", sizeof(headtype)))
	return true;

  if (!strncmp(head, "O  ", sizeof(headtype)))
	return true;

  if (!strncmp(head, "{  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "6  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "a  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "&  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "A  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "Q  ", sizeof(headtype)))
    return true;
  else
    return false;
}

bool inconsonants(char *head)
{
  if (!strncmp(head, "b <", sizeof(headtype)))
    return true;
    
  if (!strncmp(head, "d <", sizeof(headtype)))
    return true;

  if (!strncmp(head, "J\\<", sizeof(headtype)))
	return true;

  if (!strncmp(head, "g <", sizeof(headtype)))
	return true;
	
  if (!strncmp(head, "F\\<", sizeof(headtype)))
    return true;

  if (!strncmp(head, "p  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "b  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "t  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "d  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "t `", sizeof(headtype)))
    return true;

  if (!strncmp(head, "d `", sizeof(headtype)))
    return true;

  if (!strncmp(head, "c  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "J\\ ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "k  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "g  ", sizeof(headtype)))
	return true;

  if (!strncmp(head, "q  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "G\\ ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "?  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "m  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "F  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "n  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "n `", sizeof(headtype)))
    return true;

  if (!strncmp(head, "J  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "N  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "N\\ ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "B\\ ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "r  ", sizeof(headtype)))
    return true;
	
  if (!strncmp(head, "R\\ ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "4  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "r `", sizeof(headtype)))
    return true;

  if (!strncmp(head, "p\\ ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "B  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "f  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "v  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "T  ", sizeof(headtype)))
	return true;

  if (!strncmp(head, "D  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "s  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "z  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "S  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "Z  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "s `", sizeof(headtype)))
    return true;

  if (!strncmp(head, "z `", sizeof(headtype)))
    return true;

  if (!strncmp(head, "C  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "j\\ ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "x  ", sizeof(headtype)))
    return true;
  
  if (!strncmp(head, "G  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "X  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "R  ", sizeof(headtype)))
    return true;
 
  if (!strncmp(head, "X\\ ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "?\\ ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "h  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "h\\ ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "K  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "K\\ ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "w  ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "P  ", sizeof(headtype)))
    return true;
  
  if (!strncmp(head, "v\\ ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "r\\ ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "r\\`", sizeof(headtype)))
    return true;

  if (!strncmp(head, "j  ", sizeof(headtype)))
    return true;
  
  if (!strncmp(head, "M\\ ", sizeof(headtype)))
    return true;

  if (!strncmp(head, "l  ", sizeof(headtype)))
    return true;
  
  if (!strncmp(head, "l `", sizeof(headtype)))
    return true;

  if (!strncmp(head, "L  ", sizeof(headtype)))
    return true;
    
  if (!strncmp(head, "L\\ ", sizeof(headtype)))
    return true;
  else
    return false;
}

void initComp0()
{
  FILE   *fp;
  short  i, j;
  char   ch;
  double v;
  short  nr;
  char   path0[1024];

  strcpy(path0, path);
  strcat(path0, sname);
  openr(&fp, path0);

  fscanf(fp, "%hd%*[^\n]", &comp0.number);
  getc(fp);

  nr = comp0.number;
  for (i = 0; i <= nr - 1; i++) 
  {
    initsegment(&comp0.segment[i]);
    ch = getc(fp);
    rhead(&fp, comp0.segment[i].head, &ch);
    fscanf(fp, "%*[^\n]");
    getc(fp);
    
    if (invowels    (comp0.segment[i].head))
      comp0.vctype[i] = 'V';
    else
    if (inconsonants(comp0.segment[i].head))
      comp0.vctype[i] = 'C';
    else
      comp0.vctype[i] = '-';
    
    comp0.matrix[i][i] = 0.0;
  }

  nr = comp0.number;
  for (i = 1; i <= nr - 1; i++) 
  {
    for (j = 0; j <= i - 1; j++) 
    {
      fscanf(fp, "%lg%*[^\n]", &v);
      getc(fp);
      comp0.matrix[i][j] = v;
      comp0.matrix[j][i] = v;
    }
  }

  fclose(fp);
}

void copyComp0to1()
{
  comp1 = comp0;
}

void initComp2()
{
  short i, j, nr;

  comp2 = comp0;
  comp2.total = 0.0;

  nr = comp2.number;
  for (i = 0; i <= nr - 1; i++)
    comp2.vector[i] = 0.0;

  nr = comp2.number;
  for (i = 1; i <= nr - 1; i++) 
  {
    for (j = 0; j <= i - 1; j++) 
    {
      comp2.matrix[i][j] = 0.0;
      comp2.matrix[j][i] = 0.0;
    }
  }
}

void copyComp2to1()
{
  comp1 = comp2;
}

void IndelSubst()
{
  short  i, j;
  double sumIndel = 0.0, numIndel = 0.0, sumSubst = 0.0, numSubst = 0.0;
  short  nr;

  maxIndel = 0.0;
  nr = comp1.number;

  for (i = 1; i <= nr - 1; i++) 
  {
    if (comp1.matrix[i][0] != 99999L) 
    {
      if (comp1.matrix[i][0] > maxIndel)
        maxIndel = comp1.matrix[i][0];

      sumIndel += comp1.matrix[i][0];
      numIndel++;
    }
  }

  meanIndel = sumIndel / numIndel;

  maxSubst = 0.0;
  nr = comp1.number;

  for (i = 2; i <= nr - 1; i++) 
  {
    for (j = 1; j <= i - 1; j++) 
    {
      if (comp1.matrix[i][j] != 99999L) 
      {
	    if (comp1.matrix[i][j] > maxSubst)
	      maxSubst = comp1.matrix[i][j];

        sumSubst += comp1.matrix[i][j];
        numSubst++;
      }
    }
  }

  meanSubst = sumSubst / numSubst;

  lg = (maxIndel >  (0.5 * maxSubst));
  sm = (maxIndel <= (0.5 * maxSubst));
}

void initEmpty()
{
  short s;

  for (s = 0; s <= 1; s++)
    empty.segment[s].sound = 1;
}

double weight0(segmenttype0 segment1, segmenttype0 segment2)
{
  return (comp1.matrix[segment1.sound - 1][segment2.sound - 1]);
}

double weight(lettertype0 letter1, lettertype0 letter2)
{
  return ((weight0(letter1.segment[0], letter2.segment[0]) +
           weight0(letter1.segment[1], letter2.segment[1])) / 2);
}

double min3(double a, double b, double c)
{
  if (a <= b && a <= c)
    return a;
  else 
  
  if (b <= a && b <= c)
    return b;
  else
  
    return c;
}

double max3(double a, double b, double c)
{
  if (a >= b && a >= c)
    return a;
  else
  
  if (b >= a && b >= c)
    return b;
  else
  
    return c;
}

void check(double *value1, double prevA1, double prevB1, double prevC1, 
           double *value2, double prevA2, double prevB2, double prevC2,
           double *value3, double prevA3, double prevB3, double prevC3)
{
  *value1 = min3(prevA1, prevB1, prevC1);

  if (prevA1 != *value1)
    prevA2 = -maxreal;

  if (prevB1 != *value1)
    prevB2 = -maxreal;

  if (prevC1 != *value1)
    prevC2 = -maxreal;

  *value2 = max3(prevA2, prevB2, prevC2);

  if (prevA1 != *value1)
    prevA3 = maxreal;

  if (prevB1 != *value1)
    prevB3 = maxreal;

  if (prevC1 != *value1)
    prevC3 = maxreal;

  *value3 = min3(prevA3, prevB3, prevC3);
}

void addtrace(double prevA1, double prevB1, double prevC1, 
              double prevA2, double prevB2, double prevC2, 
              double prevA3, double prevB3, double prevC3, short l1, short l2)
{
  short pointer = 0;

  if (prevA1 == matrix1[l1][l2] && ((lg && prevA2 == matrix2[l1][l2]) || (sm && prevA3 == matrix3[l1][l2])))
    pointer += 8;

  if (prevB1 == matrix1[l1][l2] && ((lg && prevB2 == matrix2[l1][l2]) || (sm && prevB3 == matrix3[l1][l2])))
    pointer += 4;

  if (prevC1 == matrix1[l1][l2] && ((lg && prevC2 == matrix2[l1][l2]) || (sm && prevC3 == matrix3[l1][l2])))
    pointer += 2;

  trace[l1][l2] = pointer;
}

void addalign(short l1, short l2, double d, short a)
{
  align[a-1].word1 = l1;
  align[a-1].word2 = l2;
  align[a-1].dist  = d;
}

void addFreqComp2(short a, wordtype0 word1, wordtype0 word2)
{
  short l, sound11, sound12, sound21, sound22;

  for (l = a - 2; l >= 0; l--) 
  {
    if (align[l].word1 == 0) 
    {
      sound11 = 1;
      sound12 = 1;
    } 
    else 
    {
      sound11 = word1.letter[align[l].word1 - 1].segment[0].sound;
      sound12 = word1.letter[align[l].word1 - 1].segment[1].sound;
    }

    if (align[l].word2 == 0) 
    {
      sound21 = 1;
      sound22 = 1;
    } 
    else 
    {
      sound21 = word2.letter[align[l].word2 - 1].segment[0].sound;
      sound22 = word2.letter[align[l].word2 - 1].segment[1].sound;
    }

    if (sound11 != sound21) 
    {
      comp2.total += 0.5;

      comp2.vector[sound11-1] += 0.5;
      comp2.vector[sound21-1] += 0.5;

      comp2.matrix[sound11-1][sound21-1] += 0.5;
      comp2.matrix[sound21-1][sound11-1] += 0.5;
    }

    if (sound12 != sound22) 
    {
      comp2.total += 0.5;

      comp2.vector[sound12-1] += 0.5;
      comp2.vector[sound22-1] += 0.5;

      comp2.matrix[sound12-1][sound22-1] += 0.5;
      comp2.matrix[sound22-1][sound12-1] += 0.5;
    }
  }
}

char *segment(char *result, short sound)
{
  short t = 0;
  bool found = false;

  while (!found && t < comp1.number) 
  {
    t++;
    found = (sound == t);
  }

  if (!found) 
  {
    fprintf(stderr, "%d not found in segment distance table\n", sound);
    exit(0);
  }
  
  sprintf(result, "%.3s", comp1.segment[t-1].head);

  return result;
}

void printalign(short a, wordtype0 word1, wordtype0 word2)
{
  short l;
  double distance = 0.0, alignLen = 0.0;
  char result[256];

  putc('\n', fileAl);

  for (l = a - 2; l >= 0; l--) 
  {
    if (align[l].word1 == 0)
      fprintf(fileAl, "-%6c", ' ');
    else 
    {
      fputs(segment(result, word1.letter[align[l].word1 - 1].segment[0].sound), fileAl);

      if (word1.letter[align[l].word1 - 1].segment[0].sound != word1.letter[align[l].word1 - 1].segment[1].sound)
	    fprintf(fileAl, "%s ", segment(result, word1.letter[align[l].word1 - 1].segment[1].sound));
      else
        fprintf(fileAl, "%4c", ' ');
    }
  }

  putc('\n', fileAl);

  for (l = a - 2; l >= 0; l--) 
  {
    if (align[l].word2 == 0)
      fprintf(fileAl, "-%6c", ' ');
    else 
    {
      fputs(segment(result, word2.letter[align[l].word2 - 1].segment[0].sound), fileAl);

      if (word2.letter[align[l].word2 - 1].segment[0].sound != word2.letter[align[l].word2 - 1].segment[1].sound)
        fprintf(fileAl, "%s ", segment(result, word2.letter[align[l].word2 - 1].segment[1].sound));
      else
        fprintf(fileAl, "%4c", ' ');
    }
  }

  putc('\n', fileAl);

  for (l = a - 2; l >= 0; l--) 
  {
    fprintf(fileAl, "%3.2f%3c", align[l].dist, ' ');
    distance += align[l].dist;

    if (align[l].word1 == 0 || align[l].word2 == 0)
      alignLen += maxIndel;
    else
      alignLen += maxSubst;
  }

  if (normal == 0)
    fprintf(fileAl, "   dist.: %3.6f\n\015\n", distance);
  else
    fprintf(fileAl, "   dist.: %3.6f\n\015\n", distance / alignLen);
}

bool vowel(short sound)
{
  return (comp0.vctype[sound-1] == 'V');
}

bool consonant(short sound)
{
  return (comp0.vctype[sound-1] == 'C');
}

void procalign(short a, wordtype0 word1, wordtype0 word2)
{
  short l, l1, l2;
  double distance = 0.0;

  for (l = a - 2; l >= 0; l--) 
  {
    if (align[l].word1 == 0)
      l1 = 1;
    else
      l1 = word1.letter[align[l].word1 - 1].segment[0].sound;

    if (align[l].word2 == 0)
      l2 = 1;
    else
      l2 = word2.letter[align[l].word2 - 1].segment[0].sound;

    if (part == 0)
      distance += align[l].dist;
    else

    if (part == 1) 
    {
      if (vowel(l1) && vowel(l2))
	    distance += align[l].dist;
	  else
	    /*nothing*/;
    }
    else
    
    if (part == 2) 
    {
	  if (l1 == 1 && vowel(l2))
	    distance += align[l].dist;
	  else 

      if (vowel(l1) && l2 == 1)
        distance += align[l].dist;
       else 
         /*nothing*/;
    }
    else
	
    if (part == 3) 
    {
	  if (consonant(l1) && consonant(l2))
	    distance += align[l].dist;
	  else
	    /*nothing*/;
	}
	else
	
    if (part == 4) 
    {
	  if (l1 == 1 && consonant(l2))
		distance += align[l].dist;
      else 

      if (consonant(l1) && l2 == 1) 
        distance += align[l].dist;
      else
        /*nothing*/; 
	}
	else
      usage();
  }
  
  allDist += distance;
  allNum++;
}

void proctrace(short l1, short l2, short a, wordtype0 word1, wordtype0 word2)
{
  short pointer;

  if (l1 > 0 || l2 > 0) 
  {
    pointer = trace[l1][l2];

    if ((pointer == 8 || pointer == 10 || pointer == 12 || pointer == 14) && l1 > 0) 
    {
      addalign(l1, 0, matrix1[l1][l2] - matrix1[l1-1][l2], a);
      proctrace(l1 - 1, l2, a + 1, word1, word2);
      pointer -= 8;
    }

    if ((pointer == 4 || pointer == 6 || pointer == 12 || pointer == 14) && l1 > 0 && l2 > 0) 
    {
      addalign(l1, l2, matrix1[l1][l2] - matrix1[l1-1][l2-1], a);
      proctrace(l1 - 1, l2 - 1, a + 1, word1, word2);
      pointer -= 4;
    }

    if ((pointer == 2 || pointer == 6 || pointer == 10 || pointer == 14) && l2 > 0)
    {
      addalign(0, l2, matrix1[l1][l2] - matrix1[l1][l2-1], a);
      proctrace(l1, l2 - 1, a + 1, word1, word2);
      pointer -= 2;
	}
  }
  else
  {
    if (method == 2)
      addFreqComp2(a, word1, word2);

    if (part != 0 && saveXX)
      procalign(a, word1, word2);

    if (saveAl && saveXX)
      printalign(a, word1, word2);
  }	
}

void Levenshtein(wordtype0 word1, wordtype0 word2, double *distance, double *distanceP)
{
  short l1, l2;
  double prevA1, prevB1, prevC1, prevA2, prevB2, prevC2, prevA3, prevB3, prevC3;

  for (l1 = 0; l1 <= word1.number; l1++) 
  {
    for (l2 = 0; l2 <= word2.number; l2++) 
    {
      prevA1 = maxreal;
      prevB1 = maxreal;
      prevC1 = maxreal;

      prevA2 = -maxreal;
      prevB2 = -maxreal;
      prevC2 = -maxreal;

      prevA3 = maxreal;
      prevB3 = maxreal;
      prevC3 = maxreal;

      if (l1 > 0) 
      {
	    prevA1 = matrix1[l1-1][l2] + weight(word1.letter[l1-1], empty);
	    if (lg) prevA2 = matrix2[l1-1][l2] + maxIndel;
	    if (sm) prevA3 = matrix3[l1-1][l2] + maxIndel;
      }

      if (l1 > 0 && l2 > 0) 
      {
	    prevB1 = matrix1[l1-1][l2-1] + weight(word1.letter[l1-1], word2.letter[l2-1]);
        if (lg) prevB2 = matrix2[l1-1][l2-1] + maxSubst;
        if (sm) prevB3 = matrix3[l1-1][l2-1] + maxSubst;
      }

      if (l2 > 0) 
      {
        prevC1 = matrix1[l1][l2-1] + weight(empty, word2.letter[l2-1]);
        if (lg) prevC2 = matrix2[l1][l2-1] + maxIndel;
        if (sm) prevC3 = matrix3[l1][l2-1] + maxIndel;
      }

      check(&matrix1[l1][l2], prevA1, prevB1, prevC1, 
            &matrix2[l1][l2], prevA2, prevB2, prevC2,
            &matrix3[l1][l2], prevA3, prevB3, prevC3);

      if (matrix1[l1][l2] ==  maxreal) matrix1[l1][l2] = 0.0;
      if (matrix2[l1][l2] == -maxreal) matrix2[l1][l2] = 0.0;
      if (matrix3[l1][l2] ==  maxreal) matrix3[l1][l2] = 0.0;

      if ((method == 2) || ((part != 0) && saveXX) || (saveAl && saveXX))
        addtrace(prevA1, prevB1, prevC1, prevA2, prevB2, prevC2, prevA3, prevB3, prevC3, l1, l2);
    }
  }

  allDist = 0.0; allNum = 0.0;
  if ((method == 2) || ((part != 0) && saveXX) || (saveAl && saveXX))
  {
    proctrace(word1.number, word2.number, 1, word1, word2);

    if (part != 0 && saveXX)
      *distanceP = allDist / allNum;
  }

  if (normal == 0)
    *distance = matrix1[word1.number][word2.number];
  else
  if (normal == 1 && lg)
    *distance = matrix1[word1.number][word2.number] / matrix2[word1.number][word2.number];
  else
  if (normal == 1 && sm) 
    *distance = matrix1[word1.number][word2.number] / matrix3[word1.number][word2.number];
  else    
	/*nothing*/;

  if (normal == 0 && part != 0)
    *distanceP = *distanceP;
  else

  if (normal == 1 && part != 0 && lg)
    *distanceP = *distanceP/matrix2[word1.number][word2.number];
  else
  
  if (normal == 1 && part != 0 && sm)
    *distanceP = *distanceP/matrix3[word1.number][word2.number];
  else
    /*nothing*/;
}

void saveIndv()
{
  if (saveIn)
    fprintf(fileIn, "var1\tvar2\titem\tdist\n");
}

void calculate(char *filename10, char *filename1, char *filename20, char *filename2)
{
  listtype0 list1, list2;
  short     w1, w2;
  FILE      *file1 = NULL, *file2 = NULL;
  double    matrix[30][30], matrixP[30][30];
  short     c1[31], c2[31];
  short     h1, h2;
  double    d, d0;
  double    s = 0.0, m0 = 0.0;
  double    mean;
  short     w10, w20;
  FILE      *fp = NULL;
  char      *TEMP;
  char      path1[1024], path2[1024], path0[1024];
  char      ipath[1024], apath[1024];

  strcpy(path1, path);
  strcat(path1, filename1);
  strcat(path1, ".dat");
  openr(&file1, path1);

  strcpy(path2, path);
  strcat(path2, filename2);
  strcat(path2, ".dat");
  openr(&file2, path2);

  strcpy(path0, path);
  strcat(path0, iname);
  openr(&fp, path0);

  if (saveAl && saveXX)
    fprintf(fileAl, "%s versus %s\n\015\n", filename10, filename20);
    
  while (!fEOF(file1) && !fEOF(file2)) 
  {
    fgets(itemname, 256, fp);
    TEMP = strchr(itemname, '\n');
    if (TEMP != NULL)
      *TEMP = 0;
    
    strcpy(filename, filename10);
    rline(&file1, &list1);

    strcpy(filename, filename20);
    rline(&file2, &list2);

    if (list1.number > 0 && list2.number > 0) 
    {
      for (w1 = 0; w1 <= list1.number - 1; w1++) 
      {
	    for (w2 = 0; w2 <= list2.number - 1; w2++) 
	    {
	      if (saveAl && saveXX)
	        fprintf(fileAl, "%s\n", itemname);
	        
	      Levenshtein(list1.w_rd[w1], list2.w_rd[w2], &matrix[w1][w2], &matrixP[w1][w2]);
        }
      }

      for (w1 = 1; w1 <= list1.number; w1++)
        c1[w1] = list2.number;
      for (w2 = 1; w2 <= list2.number; w2++)
        c2[w2] = list1.number;

      d = 0.0;

      for (h1 = 1; h1 <= list1.number; h1++) 
      {
        for (h2 = 1; h2 <= list2.number; h2++) 
        {
          d0 = 1.7e+38;
          
          for (w1 = 1; w1 <= list1.number; w1++) 
          {
            for (w2 = 1; w2 <= list2.number; w2++) 
            {
              if (matrix[w1-1][w2-1] < d0 && c1[w1] > 0 && c2[w2] > 0) 
              {
                d0 = matrix[w1-1][w2-1];
                w10 = w1;
                w20 = w2;
              }
            }
	      }

          if (part == 0)
	        d += matrix [w10-1][w20-1];
	      else
            d += matrixP[w10-1][w20-1];

	      c1[w10]--;
	      c2[w20]--;
	    }
      }

      d /= list1.number * list2.number;
      s += d;
      m0++;
    } 
    else
      d = -1.0;

    if (saveIn && saveXX) 
    {
      if (d >= 0)
        fprintf(fileIn, "%s\t%s\t%s\t%3.6f\n", filename10, filename20, itemname, d);
      else
        fprintf(fileIn, "%s\t%s\t%s\t\n"     , filename10, filename20, itemname   );
    }
  }

  if (m0>0)
    mean = s / m0;
  else
  {
	strcpy(ipath, path);
    strcat(ipath, "individual.tsv");
	remove(ipath);
	
	strcpy(apath, path);
    strcat(apath, "aggregated.csv");
	remove(apath);
	  
	fprintf(stderr, "%s and %s do not share any item!\n", filename10, filename20);
	exit(0); 
  }

  if (saveAg && saveXX) 
  {
    fprintf(fileAg, "%3.3f,", mean);
  }

  fclose(file1);
  fclose(file2);
  fclose(fp);
}

void compareVarieties()
{
  FILE *fp10 = NULL, *fp1 = NULL, *fp20  = NULL, *fp2  = NULL;
  char filename10[256], filename1[256], filename20[256], filename2[256];
  char npath[256], fpath[256];
  char *TEMP;

  strcpy(npath, path);
  strcat(npath, nname);

  strcpy(fpath, path);
  strcat(fpath, fname);

  openr(&fp10, npath);
  openr(&fp1, fpath);

  fscanf(fp10, "%*[^\n]");
  getc(fp10);

  fscanf(fp1 , "%*[^\n]");
  getc(fp1 );

  if (saveAg && saveXX)
    fprintf(fileAg, "%d\n", 0);

  while (!fEOF(fp1)) 
  {
    fgets(filename10, 256, fp10);
    TEMP = strchr(filename10, '\n');
    if (TEMP != NULL)
      *TEMP = 0;

    fgets(filename1 , 256, fp1 );
    TEMP = strchr(filename1,  '\n');
    if (TEMP != NULL)
      *TEMP = 0;

    openr(&fp20, npath);
    openr(&fp2 , fpath);

    fgets(filename20, 256, fp20);
    TEMP = strchr(filename20, '\n');
    if (TEMP != NULL)
      *TEMP = 0;
      
    fgets(filename2, 256, fp2);
    TEMP = strchr(filename2 , '\n');
    if (TEMP != NULL)
      *TEMP = 0;

    while (strcmp(filename1, filename2)) 
    {
      calculate(filename10, filename1, filename20, filename2);

      fgets(filename20, 256, fp20);
      TEMP = strchr(filename20, '\n');
      if (TEMP != NULL)
        *TEMP = 0;
        
      fgets(filename2, 256, fp2);
      TEMP = strchr(filename2 , '\n');
      if (TEMP != NULL)
        *TEMP = 0;
    }

    if (saveAg && saveXX)
      fprintf(fileAg, "%d\n", 0);
      
    fclose(fp20);
    fclose(fp2);
  }

  fclose(fp10);
  fclose(fp1);
}

double min2(double a, double b)
{
  if (a <= b)
    return a;
  else 
    return b;
}

double max2(double a, double b)
{
  if (a >= b)
    return a;
  else
    return b;
}

void calcDistComp2()
{
  double pxy, px, py, p, PMI;
  double minPMI = maxreal, maxPMI = -maxreal;
  short  i, j, nr;

  nr = comp2.number;

  for (i = 0; i <= nr - 1; i++) 
  {
    for (j = 0; j <= nr - 1; j++) 
    {
      if (i != j && comp0.matrix[i][j] != 99999L) 
      {
	    if (comp2.matrix[i][j] > 0) 
	    {
          pxy = comp2.matrix[i][j] / comp2.total;
          px = comp2.vector[i] / (comp2.total * 2);
          py = comp2.vector[j] / (comp2.total * 2);
          p = pxy / (px * py);
	    }
	    else
          p = 0.0;

        PMI = log2(p + 1);
	    comp2.matrix[i][j] = PMI;
	    minPMI = min2(minPMI, PMI);
	    maxPMI = max2(maxPMI, PMI);
      }
      else
        /*nothing*/;
    }
  }

  for (i = 0; i <= nr - 1; i++) 
  {
    for (j = 0; j <= nr - 1; j++) 
    {
      if (i != j && comp0.matrix[i][j] != 99999L) 
      {
        comp2.matrix[i][j] = (maxPMI - comp2.matrix[i][j])/ 
                             (maxPMI - minPMI);
        comp2.matrix[i][j] = (0.0 * comp1.matrix[i][j])+ 
                             (1.0 * comp2.matrix[i][j]);
      } 
      else
        comp2.matrix[i][j] = comp0.matrix[i][j];
    }
  }
}

void eucComp1Comp2(double *d)
{
  short  i, j, nr;
  double sum = 0.0;
  double v1, v2;
  double difv1v2;

  nr = comp1.number;

  for (i = 1; i <= nr - 1; i++) 
  {
    for (j = 0; j <= i - 1; j++) 
    {
      if (comp1.matrix[i][j] != 99999L) 
      {
        v1 = comp1.matrix[i][j];
        v2 = comp2.matrix[i][j];

	    difv1v2 = v1 - v2;
        sum += difv1v2 * difv1v2;
      }
    }
  }

  *d = sqrt(sum);
}

void corComp1Comp2(double *r)
{
  short  i, j, nr;
  double sumx = 0.0, sumy = 0.0, sumxy = 0.0, sumxx = 0.0, sumyy = 0.0;
  double nxy, nxx, nyy, v1, v2;
  double number = 0.0;

  nr = comp1.number;

  for (i = 1; i <= nr - 1; i++) 
  {
    for (j = 0; j <= i - 1; j++) 
    {
      if (comp1.matrix[i][j] != 99999L) 
      {
        number++;

        v1 = comp1.matrix[i][j];
        v2 = comp2.matrix[i][j];

        sumx += v1;
        sumy += v2;

        sumxy += v1 * v2;
        sumxx += v1 * v1;
        sumyy += v2 * v2;
      }
    }
  }

  nxy = sumx * sumy / number;
  nxx = sumx * sumx / number;
  nyy = sumy * sumy / number;

  *r = (sumxy - nxy) / sqrt((sumxx - nxx) * (sumyy - nyy));
}

void saveComp1()
{
  short i, j, h, nr;

  if (!saveSo)
    return;
  
  nr = comp1.number;
  
  for (i = 0; i <= nr - 1; i++) 
  {
    putc(',', fileSo);

    for (h = 0; h <= 2; h++) 
    {
      if (comp1.segment[i].head[h] != ' ')
        putc(comp1.segment[i].head[h], fileSo);
    }
  }

  putc('\n', fileSo);

  for (i = 0; i <= nr - 1; i++) 
  {
    for (h = 0; h <= 2; h++) 
    {
      if (comp1.segment[i].head[h] != ' ')
        putc(comp1.segment[i].head[h], fileSo);
    }

    for (j = 0; j <= nr - 1; j++) 
    {
      putc(',', fileSo);

      if (comp1.matrix[i][j] != 99999L)
        fprintf(fileSo, "%3.3f", comp1.matrix[i][j]);
    }

    putc('\n', fileSo);
  }
}

/* main */

int main(int argc, char *argv[])
{  
  fileAl = NULL;
  fileIn = NULL;
  fileAg = NULL;
  fileSo = NULL;

  get_programname (argv [0]);

  if (argc == 13) 
  {
    strcpy(nname, argv[1]);
    strcpy(fname, argv[2]);
    strcpy(iname, argv[3]);
    strcpy(sname, argv[4]);

    if (!strcmp(argv[5] , "1")) method = 1    ; else
    if (!strcmp(argv[5] , "2")) method = 2    ; else usage();

    if (!strcmp(argv[6] , "0")) normal = 0    ; else
    if (!strcmp(argv[6] , "1")) normal = 1    ; else usage();

    if (!strcmp(argv[7] , "0")) part   = 0    ; else
    if (!strcmp(argv[7] , "1")) part   = 1    ; else
    if (!strcmp(argv[7] , "2")) part   = 2    ; else
	if (!strcmp(argv[7] , "3")) part   = 3    ; else
	if (!strcmp(argv[7] , "4")) part   = 4    ; else usage();

    if (!strcmp(argv[8] , "0")) saveSo = false; else
    if (!strcmp(argv[8] , "1")) saveSo = true ; else usage();

    if (!strcmp(argv[9] , "0")) saveAl = false; else
    if (!strcmp(argv[9] , "1")) saveAl = true ; else usage();

    if (!strcmp(argv[10], "0")) saveIn = false; else
    if (!strcmp(argv[10], "1")) saveIn = true ; else usage();

    if (!strcmp(argv[11], "0")) saveAg = false; else
    if (!strcmp(argv[11], "1")) saveAg = true ; else usage();
    
    strcpy(path, argv[12]);
  }
  else
    usage();

  initEmpty();

  if (saveSo) 
  {
    strcpy(path0, path);
    strcat(path0, "sounddists.csv");
    openw(&fileSo,path0);
  }
  if (saveAl) 
  {
    strcpy(path0, path);
    strcat(path0, "alignments.txt");
    openw(&fileAl,path0);
  }
  if (saveIn) 
  {
    strcpy(path0, path);
    strcat(path0, "individual.tsv");
    openw(&fileIn,path0);
  }
  if (saveAg) 
  {
    strcpy(path0, path);
    strcat(path0, "aggregated.csv");
    openw(&fileAg,path0);
  }
  
  saveIndv();

  if (method == 1)
    saveXX = true;
  else
    saveXX = false;

  initComp0();
  copyComp0to1();
  IndelSubst();
  initComp2();
  compareVarieties();

  if (method == 2) 
  {
    calcDistComp2();

    r = 0.0;
    do 
    {
      r0 = r;
      copyComp2to1();
      IndelSubst();
      initComp2();
      compareVarieties();
      calcDistComp2();
      corComp1Comp2(&r);
    } while (r > r0);

    saveXX = true;
    IndelSubst();
    initComp2();
    compareVarieties();
  }
  else
    /*nothing*/;

  saveComp1();

  if (saveSo) fclose(fileSo);
  if (saveAl) fclose(fileAl);
  if (saveIn) fclose(fileIn);
  if (saveAg) fclose(fileAg);

  return 0;
}
