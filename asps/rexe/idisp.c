/*-- Author :    Pavel Nevski   01/09/99*/
int idisp0_ (char  *a, char  *b) { return  b-a;    }
int idisp1_ (char  *a, char  *b) { return (b-a)+1; }
int idisp2_ (short *a, short *b) { return ((b-a)) + 1; }
int idisp4_ (int   *a, int   *b) { return ((b-a)) + 1; }
int iponter_(int   *a, int  **b) { return (*b-a); } 

#ifdef PrivateMalloc
#include <stdlib.h>
int malloc_(int  *size){return (int) malloc((size_t) *size);}
void  free_(int  *ptr) { int i = *ptr;  free  ((char *)i);}
#endif
