/*
	f77290.c
		a f77 (fixed format) -> f90 (free format) converter;
		if QPRECISION is defined, also converts double prec
		to quad prec (using the conventions of qcomplex.f90)
		Sept 97, last modified 18 Aug 04 th
*/

#define QPRECISION
#define MAXLINELENGTH 82

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#define upper(a) &a[sizeof(a)/sizeof(char *)]

const char *ops[] = {
  ".eq.", "==", ".ne.", "/=",
  ".le.", "<=", ".ge.", ">=",
  ".lt.", "<",  ".gt.", ">" };

const char *types[] = {
#ifdef QPRECISION
  "type(complex32)",
#else
  "double complex",
#endif
  "double precision", 
  "integer", "real", "character", "complex", "logical" };

const char *units[] = {
  "function", "subroutine", "block data", "program" };

typedef struct sourceline {
  struct sourceline *next;
  int label, indent;
  char s[132];
} SOURCELINE;

void f90name(char *oldname)
{
  char *p = strchr(oldname, '.');
  if( p && (*(p + 1) | 0x20) == 'f' ) strcpy(p, ".f90");
  else strcat(oldname, "90");
}

int isnumber(char c1, char c2, char c3, char c4)
{
  int i2 = isdigit(c2), i3 = isdigit(c3);

  if( i3 ) return i2 || ((c2 | 0x21) == 'e' && isdigit(c1)) ||
    ((c2 == '+' || c2 == '-') && (c1 | 0x21) == 'e');
  if( i2 ) return (c3 | 0x21) == 'e' &&
    (c4 == '+' || c4 == '-' || isdigit(c4));
  return (c2 | 0x21) == 'e' && isdigit(c1) &&
    (c3 == '+' || c3 == '-') && isdigit(c4);
}

#ifdef QPRECISION
void typereplace(char *s, char *d, char *from, char *to)
{
  int i;
  char *p, s2[200];

  if( (p = strstr(s, from)) ) {
    i = strlen(from);
    strcpy(s2, p + i);
    strcpy(p, to);
    strcat(p, s2);
    p = d + (int)(p - s);
    strcpy(s2, p + i);
    strcpy(p, to);
    strcat(p, s2);
  }
}
#endif

int main(int argc, char **argv)
{
  FILE *f;
  char s[512], s2[512], *p, *d, *d2;
  char fnstack[500], *funcname[10], **fnp = funcname;
  char functype[50], ch;
  const char **pp;
  int lnr = 0, maxllen = MAXLINELENGTH, cont, space, i, throwout = 0;
  int indent = 0, defertype = 0, justif = 0, param = 0;
  SOURCELINE *start = NULL, *current = NULL, *new, *last = NULL;
  SOURCELINE *xref[300], **xrp = xref, **xxp;
  int gotos[150], *gp = gotos, donum[150], *dgp = donum, *ip;
  char *dos[150], **dp = dos, **cp;
#ifdef QPRECISION
  int usemodule = 0;
#endif

  if( argc < 2 ) {
    fprintf(stderr, "usage: %s file.f [file.f90]\n"
      "  translates fixed-style f77 source code file.f to "
      "free-style f90 source code.\n"
      "  if the file is -, stdin/stdout is used.\n",
      argv[0]);
    exit(1);
  }
  if( strcmp(argv[1], "-") == 0 ) f = stdin;
  else if( (f = fopen(argv[1],"r")) == NULL ) {
    fprintf(stderr, "%s not found\n", argv[1]);
    exit(2);
  }
  if( (p = getenv("MAXLINELENGTH")) ) maxllen = atoi(p);

  *funcname = fnstack;
  while( !feof(f) ) {
    *s = 0;
    ++lnr;
    fgets(s, sizeof(s), f);
    *(s + strlen(s) - 1) = 0;
    if( *s == 0 ) continue;
    p = s;
    cont = 0;
    if( strncmp(s, "     ", 5) == 0 && s[5] > ' ' ) {
      if( throwout ) continue;
      p += 6;
      if( last ) {
        d = last->s + (i = strlen(last->s));
        d2 = p + strspn(p, " \t");
        space = strchr(",()=/", *(d - 1)) || strchr(",()=/", *d2) ||
          (*(d - 1) >= 'A' && *(d - 1) <= 'z' && *d2 >= 'A' && *d2 <= 'z');
        if( space ) *d++ = ' ';
        cont = 1 + (i + strlen(p) < maxllen && last == current);
        if( cont == 1 ) *d++ = '&';
        *d = 0;
      }
    }
    else if( strncasecmp(p + strspn(p, " \t"), "intrinsic", 9) == 0 ) {
      throwout = 1;
      continue;
    }
    throwout = 0;
    if( cont < 2 ) {
      new = malloc(sizeof(SOURCELINE));
      if( !start ) start = current = new;
      else {
        current->next = new;
        current = new;
      }
      current->indent = indent;
      d = current->s;
      if( *s == '\t' || cont == 1 ) current->label = 0;
      else {
        current->label = strtol(s, &d2, 10);
        if( d2 != s ) p = d2, *xrp++ = current;
      }
      if( cont == 1 ) {
        current->indent += 2;
        if( !space ) *d++ = '&';
      }
    }
    p = (char *)memccpy(d, p + strspn(p, " \t"), 0, 256) - 2;
    while( p > s && (*p == ' ' || *p == '\t') ) --p;
    *++p = 0;
    if( *s == '*' || (*s | 0x20) == 'c' ) {
      *d = '!';
			/* this is a dirty hack to cure some problems
			   the DEC f90 compiler has with FF */
      if( strstr(s, "#] declarations") ) strcpy(p, "\ncontinue");
      continue;
    }
    for( p = s, d2 = d; *d2; ) *p++ = tolower(*d2++);
    *p = 0;
    if( strncmp(s, "include", 7) == 0 || strncmp(s, "#include", 8) == 0 ) {
      if( (p = strpbrk(d + 7, "'\"<")) && (d2 = strpbrk(++p, "'\">")) ) {
        ch = *d2;
        *d2 = 0;
        f90name(p);
        *(p += strlen(p)) = ch;
        *(p + 1) = 0;
      }
    }
#ifdef QPRECISION
    if( usemodule && !cont ) {
      new = malloc(sizeof(SOURCELINE));
      memcpy(new, current, sizeof(SOURCELINE));
      current->next = new;
      current->label = 0;
      strcpy(current->s, "use qcomplex");
      current = new;
      d = current->s;
      usemodule = 0;
    }
#endif
    if( defertype && !cont && strncmp(s, "implicit", 8) ) {
      new = malloc(sizeof(SOURCELINE));
      memcpy(new, current, sizeof(SOURCELINE));
      current->next = new;
      current->label = 0;
      strcpy(current->s, functype);
      strcat(current->s, *(fnp - 1) + 9);
      current = new;
      d = current->s;
      param = justif = defertype = 0;
    }
#ifdef QPRECISION
    typereplace(s, d, "double complex", "type(complex32)");
    typereplace(s, d, "complex*16", "type(complex32)");
    typereplace(s, d, "double precision", "real*16");
    typereplace(s, d, "real*8", "real*16");
    typereplace(s, d, "real*4", "real*16");
#endif
    if( *s == '#' ) continue;
    if( !cont ) param = justif = 0;
    last = current;
    for( pp = ops; pp < upper(ops); pp += 2 )
      while( (p = strstr(s, *pp)) ) {
        strcpy((char *)memccpy(d + (int)(p - s), *(pp + 1), 0, 10) - 1,
          d + (int)(p - s + 4));
        strcpy((char *)memccpy(p, *(pp + 1), 0, 10) - 1, p + 4);
      }
    for(pp = units; pp < upper(units); ++pp)
      if( strncmp(s, *pp, strlen(*pp)) == 0 ) {
copyfname:
#ifdef QPRECISION
        if( fnp == funcname ) usemodule = 1;
#endif
        for( d2 = *fnp, p = d; *p && *p != '('; ) *d2++ = *p++;
        *d2++ = 0;
        *++fnp = d2;
        gp = gotos;
        dp = dos;
        dgp = donum;
        xrp = xref;
        goto lineok;
      }
    for( pp = types; pp < upper(types); ++pp )
      if( strncmp(s, *pp, i = strlen(*pp)) == 0 ) {
        p = d + (i += strspn(d + i, "*0123456789() \t"));
        if( strncmp(s + i, "function", 8) == 0 ) {
          memcpy(functype, d, i);
          strcpy(functype + i, ":: ");
          strcpy(d, p);
          defertype = 1;
          goto copyfname;
        }
        strcpy(s2, p);
        strcpy(p, ":: ");
        strcpy(p + 3, s2);
        break;
      }
    if( strcmp(s, "end") == 0 || strncmp(s, "end function", 12) == 0 ||
      strncmp(s, "end subroutine", 14) == 0 ) {
      if( fnp == funcname ) {
        *d = 0;
        fprintf(stderr,
          "warning: superfluous END statement in line %d\n", lnr);
      }
      else {
        for( xxp = xref; xxp < xrp; ++xxp ) {
          i = (*xxp)->label;
          for( ip = gotos; ip < gp; ++ip )
            if(*ip == i) goto keep;
          for( cp = dos, ip = donum; cp < dp; ++cp, ++ip )
            if( *ip == i ) {
              strcpy(*cp, *cp + strspn(*cp, "0123456789 \t"));
              new = malloc(sizeof(SOURCELINE));
              new->next = (*xxp)->next;
              new->indent = (*xxp)->indent;
              (*xxp)->label = new->label = 0;
              strcpy(new->s, "enddo");
              (*xxp)->next = new;
            }
keep: ;
        }
        *(d + 3) = ' ';
        strcpy(d + 4, *--fnp);
        strcat(d + 4, "\n");
      }
      current->indent = indent = 0;
      gp = gotos;
      dgp = donum;
      dp = dos;
      xrp = xref;
    }
    else if( justif || (strncmp(s, "if", 2) == 0 && !isalnum(s[2])) ) {
      if( strstr(s, "then") ) justif = 0, indent += 2;
      else justif = 1;
    }
    else if( strncmp(s, "else", 4) == 0 )
      current->indent -= 2;
    else if( strcmp(s, "endif") == 0 || strcmp(s, "end if") == 0 )
      indent -= 2, current->indent -= 2;
    else if( strncmp(s, "do ", 3) == 0 ) {
      i = strtol(d + 3, &p, 10);
      if( i ) *dgp++ = i, *dp++ = d + 3;
    }
    if( (p = strstr(s, "goto")) || (p = strstr(s, "go to")) )
      if( (i = strtol(p + 5, &d2, 10)) ) *gp++ = i;
#ifdef QPRECISION
    if( strncmp(s, "parameter", 9) == 0 ) param = 1;
    if( param ) {
      p = d;
      while( (p = strchr(p, '=')) )
        if( *(p += 1 + strspn(p + 1, " \t")) == '(' ) {
          strcpy(s2, p);
          strcpy(p, "complex32");
          strcpy(p += 9, s2);
        }
      p = s;
      i = 0;
      while( (p = strstr(p, "dcmplx")) ) {
        strcpy(s2, d2 = d + (int)((p += 6) - s + i));
        strcpy(d2 - 6, "complex32");
        strcpy(d2 + 3, s2);
        i += 3;
      }
    }
			/* statement functions _are_ a problem if they
			   contain type(complex32) functions :-(,
			   therefore:  */
    else if( !cont && (strncmp(s, "absc(", 5) == 0 ||
                       strncmp(s, "absr(", 5) == 0 ||
                       strncmp(s, "norm(", 5) == 0) ) {
      while( (p = strstr(s + 5, "dble(")) ) {
        strcpy(p, p + 3);
        p = d + (int)(p - s);
        memcpy(p, p + 5, i = strcspn(p + 5, ")"));
        strcpy(p += i, "%re");
        strcpy(p + 3,p + 6);
      }
      while( (p = strstr(s + 5, "dimag(")) ) {
        strcpy(p, p + 4);
        p = d + (int)(p - s);
        memcpy(p, p + 6, i = strcspn(p + 6, ")"));
        strcpy(p += i, "%im");
        strcpy(p + 3, p + 7);
      }
    }
#endif
lineok: ;
  }
  fclose(f);
  current->next = NULL;

  if( argc > 2 ) {
    if(strcmp(p = argv[2], "-") == 0) f = stdout;
  }
  else {
    if( f == stdin ) f = stdout;
    else {
      strcpy(s, argv[1]);
      f90name(p = s);
    }
  }
  if( f != stdout && (f = fopen(p,"w")) == NULL ) {
    fprintf(stderr, "cannot create %s\n", p);
    exit(2);
  }

  indent = 0;
  for( new = start; new; new = new->next ) {
    if( *new->s == '#' || *new->s == '!' ) {
      fprintf(f, "%s\n", new->s);
      continue;
    }
    if( strcasecmp(new->s, "enddo") == 0 ||
        strcasecmp(new->s, "end do") == 0 ) indent -= 2;
    new->indent += indent;
    if( new->label ) {
      for( i = new->label; i; i /= 10 ) --new->indent;
      if( --new->indent <= 0 ) *s = 0;
      else {
        memset(s, ' ', new->indent);
        *(s + new->indent) = 0;
      }
      fprintf(f, "%d %s%s\n", new->label, s, new->s);
    }
    else if( strcasecmp(new->s, "continue") ) {
      if(new->indent <= 0) *s = 0;
      else {
        memset(s, ' ', new->indent);
        *(s + new->indent) = 0;
      }
      fprintf(f, "%s%s\n", s, new->s);
    }
    if( strncasecmp(new->s, "do ", 3) == 0 && !isdigit(new->s[3]) )
      indent += 2;
  }
  fclose(f);
  return 0;
}

