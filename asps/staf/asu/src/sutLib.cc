/*:Copyright 1996, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         csuLib.c
*:DESCRIPTION:  SUT - String UTility functions
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      10may96-v001a-cet- combine several files
*:HISTORY:      02may96-v000a-cet- creation
*:<---------------------------------------------------------------------
*/

/*-------------------------------------------- INCLUDES             --*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#if defined (AIX) || defined(linux) || defined(HPUX)
#include <fnmatch.h>
#elif defined(IRIX) || defined(sun)
#include <libgen.h>
#endif

#include "sutLib.h"
#include "asuAlloc.h"

/*-------------------------------------------- MACROS               --*/
#define _PRINTF printf("%s.%d-",__FILE__,__LINE__);fflush(0);printf
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif
/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      int sutMatchWild
*:DESCRIPTION:  Matches a wild-card pattern to a string
*:ARGUMENTS:    
*:RETURN VALUE: TRUE or FALSE
*:<---------------------------------------------------------------------
*/
int 
sutMatchWild(char *pattern,char* string)
{
#if defined(AIX) || defined(HPUX) || defined(linux)
   int flags = 0;	
   return !fnmatch(pattern,string,flags);
#elif defined(sun) || defined(IRIX)
   return gmatch(string,pattern);
#else
   return 0;
#endif
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      int sutMatchReg
*:DESCRIPTION:  Matches a regular expression pattern to a string
*:ARGUMENTS:    
*:RETURN VALUE: TRUE or FALSE
*:<---------------------------------------------------------------------
*/
int 
sutMatchReg(char *pattern, char* string)
{
  // This is commented out to quell pedantic compilers.  If the code
  // after `return FALSE' is put back in, revisit these decls.
  static void *pp = &pattern;
  static void *ps = &string;
#if 0 
   int isMatch=FALSE;
   char *rexp=NULL;
   char *ret0[9];		/* HACK - string limit */
   char *newcursor=NULL;
   char *name=NULL;
#endif

   return FALSE;
   /* -----------------------------------------------------------
   hjw 8Mar98, ret has not been set , name is freed without a malloc()

   rexp = (char*)MALLOC(strlen(pattern) +5);
   sprintf(rexp,"(%s)$0",pattern);fflush(0);

      isMatch=FALSE;
   if( 0 == strcmp(ret0[0],string) ){
      isMatch=TRUE;
   }
   else {
      isMatch=FALSE;
   }
   FREE(name);
   FREE(rexp);
   return isMatch;

   ----------------------------------------------------------- */
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      int sutMatchPrefix
*:DESCRIPTION:  Matches a prefix string to a string (ignores whitespc)
*:ARGUMENTS:    
*:RETURN VALUE: TRUE or FALSE
*:<---------------------------------------------------------------------
*/
int sutMatchPrefix(char *prefix,char* string)
{
   char *s=NULL; 
   char *p=NULL;

   if( string == strstr(string, prefix) ){
      return (int)TRUE;
   }
   if( 0== sutStripWhitespace(&s,string)
   ||  0== sutStripWhitespace(&p,prefix)
   ){
      FREE(s); FREE(p);
      return (int)FALSE;
   }
   if( string == strstr(s, p) ){
      FREE(s); FREE(p);
      return (int)TRUE;
   }
 
   if(s) FREE(s);
   if(p) FREE(p);
 
   return (int)FALSE;
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      int sutStripWhitespace
*:DESCRIPTION:  Removes whitespace from a string
*:ARGUMENTS:    
*:RETURN VALUE: Length of resultant string
*:<---------------------------------------------------------------------
*/
int sutStripWhitespace(char **outstring,char* string)
{
   char *whtspc=" 	\0\n";
   size_t plen=0;
   size_t slen=0;
   char *s=string;
   char *o=NULL;
   s += (plen = strspn(s,whtspc));
   slen = strcspn(s,whtspc);
   o = (char*)MALLOC(slen +1);
   strncpy(o,s,slen); 
   o[slen]=0; /* hjw 19Feb98 */
   *outstring=o;
   return (int)slen;
}

/*--------------------------------------------------------------------*/
/*- Return the N-th token of a delimited string. -*/
char* strntok(const char * str,const char * del,const int n)
{
   int i;
   size_t len;
   char *d,*w;

   d = (char*)str;
   for( i=0;i<n;i++ ){
      d += strspn(d,del);       /* skip del */
      d += strcspn(d,del);      /* skip non-delimiters */
   }
   d += strspn(d,del);          /* skip tokens */
   if( strcspn(d,del) > 0 ){
      len=strcspn(d,del);        /* hjw 27mar98   I removed CALLOC */
      w = (char*)MALLOC(len+1);  /* and substituted MALLOC.        */
      strncpy(w,d,len);
      w[len]=0;
      return (char*)w;
   }
   return NULL;
}

/*--------------------------------------------------------------------*/
/*- Split a delimited string into an array of components. -*/
int strsplit(const char * str,const char * del,char*** a)
{
   int i,j;
   char *buf;
   char *token;
   char *aa[1024];                      /* LIMIT ON # COMPONENTS */

/* if( !(buf = strpbrk(str,del)) )return 0; // NO DELIMITERS FOUND */
/* will return 1 and entire str array... as strtok */

   buf = (char*)MALLOC(strlen(str) +1);
   strcpy(buf,str);
   
   i = 0;
   for (token = strtok(buf, del); token; token = strtok(NULL, del)) {
      /* _PRINTF("Token[%d]: >%s<\n", i, token); */
      aa[i++] = token;
   }
   (*a) = (char**)MALLOC(i*sizeof(char**));
   for(j=0;j<i;j++){
      (*a)[j] = (char*)MALLOC(strlen(aa[j]) +1);
      strcpy((*a)[j],aa[j]);
   }
   FREE(buf);
   return i;
}

/*--------------------------------------------------------------------*/
/*- Extract components bracketed by open & close del.s in a string. -*/
int strbracket(const char *str, const char * od, const char * cd,
			char*** a)
{
   int i,j;
   char *aa[1024];                      /* LIMIT ON # COMPONENTS */
   char *buf;
   char *optr, *cptr;
   int olen, clen;

   buf = (char*)MALLOC(strlen(str) +1);
   strcpy(buf,str);
   olen = strlen(od);			/*length of opening delimiter*/
   clen = strlen(od);			/*length of closing delimiter*/
/*- Find first open bracket. -*/
   i = 0;
   while( (optr=strstr(buf,od)) ){
      buf = optr +1;
      if( (cptr=strstr(buf,cd)) ){
	 aa[i] = (char*)MALLOC((cptr-buf) +1);
	 strncpy(aa[i],buf,(cptr-buf)); 
	 aa[i][cptr-buf]=0; /* hjw 19Feb98 */
	 buf = cptr +clen;
	 i++;
      }
      else {
	 buf = NULL;
      }
   }
   (*a) = (char**)MALLOC(i*sizeof(char**));
   for(j=0;j<i;j++){
      (*a)[j] = aa[j];
      aa[j] = NULL;
   }
   FREE(buf);
   return i;
}

/*--------------------------------------------------------------------*/
int isInteger(char *c)
{
   int i;
   char *signs="+-";
   if( (!isdigit(c[0])) && (NULL == strchr(signs,c[0])) )return FALSE;
   for(i=1;i<(int)strlen(c);i++)if(!isdigit(c[i]))return FALSE;
   return TRUE;
}

/*--------------------------------------------------------------------*/
int sutFortran2Cindex(char ** index)
{
   int nc=0;
   char *s=*index;;
   char lb='[',rb=']',lp='(',rp=')';
   char *c, *cc, *ccc;
   char *a;
   int i;

/* _PRINTF("%s\n",s); */
   while (1){
      if( NULL == (c = strchr(s,lp))
      ||  NULL == (cc = strchr(s,rp))
      ){
	 return nc;
      }
      nc++;
      *c = lb;
      *cc = rb;
      if(cc > c){
	 a = (char*)malloc(cc-c);
	 strncpy(a,c+1,cc-c); 
	 a[cc-c]=0; /* hjw 19Feb98 */
/*	 ccc = strpbrk(a,","); */
	 i = atoi(a);
	 if( 0 < i ){
	    i--;
	    sprintf(a,"%d",i);
	    ccc = c;
	    for(i=0;i<cc-c;i++){
	      ccc++;
	      *ccc = *(a+i);
	    }
	 }
      }
      *cc = rb;
      free(a);
   }
}
