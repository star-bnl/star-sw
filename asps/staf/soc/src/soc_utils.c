#include "asuAlloc.h"
#include <math.h>
#include <strings.h>

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
      name = (char*)MALLOC(strlen(base) +1
                +(int)(1 +log10((double)id)));	/*INS++:BAD_DECL*/
      sprintf(name,"%s%d",base,id);
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

	n[length] = NULL;
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
