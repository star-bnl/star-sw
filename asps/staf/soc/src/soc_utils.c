#include "asuAlloc.h"

extern char *id2name(char *base, long id);

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
                +(int)(1 +log10((double)id)));
      sprintf(name,"%s%d",base,id);
      return name;
   }
}
