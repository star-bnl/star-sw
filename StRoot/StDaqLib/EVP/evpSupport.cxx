#include "evpSupport.h"

int checkBank(char *m, char *what)
{
  if (m[0]!=what[0]) return -1;
  for (int i=1;what[i];i++) { if (m[i]!=what[i]) return -1;}
  return 0 ;
}
