#include <stdio.h>
#include <math.h>
#ifndef WIN32
# include <string.h>
#endif /* WIN32 */

#include "asuAlloc.h"
/*--------------------------------------------------------------------*/
const char *id2name(const char *base, long id)
{
   static int ibuf = 0;
   static char buf[4][100]; int lbase,from,to;
   char *name;
   name = buf[(ibuf++)&3];

   lbase = strlen(base);
   strcpy(name,base); 
   if(id > 0) sprintf(name+lbase,"%ld",id);
   to=0;for(from=lbase;name[from];from++) {
     if (name[from]!=' ') name[to++]=name[from];}
   name[to] = '\0';
   return name;
}
/*--------------------------------------------------------------------*/
const char *shortname(const char *longname, size_t length)
{
  char *nn; size_t l1,l2,ll;
  static int ibuf = 0;
  static char buf[4][100];
  char *n;
  n = buf[(ibuf++)&3];
    
  l1=(length-1)/2; l2=(length-l1-1); ll=strlen(longname);
  strncpy(n,longname,length+1); 
  if( length < ll){
    n[0]=0; strncat(n,longname,l1); strcat(n,"~"); strcat(n,longname+ll-l2);}
  return n;
}
