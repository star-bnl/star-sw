/*
* $Id: idisp.c,v 1.5 2019/02/13 17:02:43 jwebb Exp $
* $Name:  $
* $Log: idisp.c,v $
* Revision 1.5  2019/02/13 17:02:43  jwebb
* Fix for 64 bit adresses.
*
* Revision 1.4  2005/07/18 22:22:11  fisyak
* Add flag WithoutPGI to get free_ and malloc_ without PGI
*
* Revision 1.3  2004/08/12 19:12:04  fisyak
* remove memcpy_
*
* Revision 1.2  2004/03/01 17:26:33  fisyak
* Get rid of staf
*
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
*
* Revision 1.2  2001/03/05 11:55:22  nevski
* headers clean-up
*
* Revision 1.1  2001/02/27 10:15:17  nevski
*  first working release
*/
/*CMZ :          28/05/2000  14.36.45  by  Pavel Nevski*/
/*-- Author :    Pavel Nevski   01/09/99*/
/* provide displacements for pgf77 simulation */
__UINT64_TYPE__ idisp0_ (a,b)   char  *a,*b;     { return (b-a);    }
__UINT64_TYPE__ idisp1_ (a,b)   char  *a,*b;     { return (b-a)+(__UINT64_TYPE__)1; }
__UINT64_TYPE__ idisp2_ (a,b)   short *a,*b;     { return (b-a)+(__UINT64_TYPE__)1; }
__UINT64_TYPE__ idisp4_ (a,b)   int   *a,*b;     { return (b-a)+(__UINT64_TYPE__)1; }
__UINT64_TYPE__ iponter_(a,b)   int   *a,**b;    { return (*b-a);  }
#ifdef  WithoutPGI
#include <stdlib.h>
int malloc_(int  *size){return (int) malloc((size_t) *size);}
void  free_(int  *ptr) { int i = *ptr;  free  ((char *)i);}
#include <string.h>
void memcpy_(void *dest, const void *src, size_t *n) { memcpy (dest, src, *n); }
#endif
