#include "cfortran.h"
#include "tls_f2h.h"

long tu_match_wild_or_exact(char *string, char *pattern)
{
   if( ((TU_MATCH_WILD(string,pattern)) == 1)
   ||  (strncmp(string,pattern,strlen(pattern)) == 0)
   ){
      return 1;
   } else {
      return 0;
   }
}
