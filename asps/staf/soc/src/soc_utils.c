#include <stdio.h>
#include <math.h>
#ifndef WIN32
# include <string.h>
#endif /* WIN32 */

#include "asuAlloc.h"
#define HERB980611

extern char *id2name(char *base, long id);
extern char *shortname(char *longname, size_t length);

/*--------------------------------------------------------------------*/
char *id2name(char *base, long id)
{
   char *name;
   if(id < 1){
      name = (char*)MALLOC(strlen(base) +1);
      strcpy(name,base);
      return name;
   }
   else {
#ifdef HERB980611
      size_t herb980611;
      herb980611=strlen(base) + 1 + 1 + log10((double)id);
      if(herb980611<strlen(base)+2) {
        printf("herb980611 is too small, crash imminent.\n"); exit(2);
      }
      name = (char*)MALLOC(herb980611);  
#else
      name = (char*)MALLOC(strlen(base) +1
                +(int)(1 +log10((double)id)));	/*INS++:BAD_DECL*/
#endif
      sprintf(name,"%s%ld",base,id);
      return name;
   }
}
/*--------------------------------------------------------------------*/
char *shortname(char *longname, size_t length)
{
	char *n=(char*)MALLOC(length +1);
	size_t l1=(length-1)/2;
	size_t l2=(length-l1-1);
	size_t ll=(strlen(longname)-l2);
	char *nn;
	char *b;

	n[length] = 0;
	if( length < strlen(longname) ){
		strncpy(n,longname,l1); 
		n[l1]=0; /* hjw 19Feb98 */
		nn = n; nn += (l1+1);
		b = longname; b += ll;
		strncpy(nn,b,l2); 
		nn[l2]=0; /* hjw 19Feb98 */
		n[l1] = '~';
	}
	else {
		strncpy(n,longname,length); 
		n[length]=0; /* hjw 19Feb98 */
	}
	return n;
}
