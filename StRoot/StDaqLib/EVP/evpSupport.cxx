#include "evpSupport.h"

int OLDEVP::checkBank(char *m, char *what)
{
  int i;
  if (m[0]!=what[0]) return -1;
  for (i=1;what[i];i++) { if (m[i]!=what[i]) return -1;}
  if (i==8 || m[i]==' ') return 0;
  return -1 ;
}
