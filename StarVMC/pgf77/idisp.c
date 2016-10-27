/*
* $Id: idisp.c,v 1.2 2011/02/11 15:54:46 fisyak Exp $
* $Name:  $
* $Log: idisp.c,v $
* Revision 1.2  2011/02/11 15:54:46  fisyak
* 64 bits corrections
*
* Revision 1.1.1.1  2008/12/10 20:50:34  fisyak
* Merge with macos version
*
* Revision 1.1  2005/09/13 14:23:03  fisyak
* add idisp from starsim in order to load reconstruction package libraries compiled with g77 via mortra
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
#ifdef  WithoutPGI
#include <stdlib.h>
#include <string.h>
#ifndef  __LP64__
long long idisp0_ (a,b)   char  *a,*b;     { return  b-a;    }
long long idisp1_ (a,b)   char  *a,*b;     { return (b-a)+1; }
long long idisp2_ (a,b)   short *a,*b;     { return (b-a)+1; }
long long idisp4_ (a,b)   int   *a,*b;     { return (b-a)+1; }
long long iponter_(a,b)   int   *a,**b;    { return (*b-a);  }
#else
#if 0
#include <stdio.h>
unsigned int chkloc(void *iadr)
{
  /* 64 bit architectures may exceed the 32 bit address space !               */

  /* AMD64/Intel EM64T architectures have the dynamic segments above
     0x80000000000 and the stack immediately below this whereas the text and
     data segments are staring from 0x400000. The implementations address
     space is limited to 0x00007fffffffffff.
     Allocated memory with malloc/calloc is starting from the end of text and
     data segments upwards.                                                   

     IA64 architectures have the dynamic segments are above 0x2000000000000000,
     the stack is above 0x8000000000000000, the data segments starts at 
     0x6000000000000000 and the text segments start at 0x4000000000000000   
     Allocated memory with malloc/calloc is starting from the end of
     data segments upwards. All addresses here are expected to be in
     the data segment area.                                                   */

   const unsigned long mask=0xffffffff00000000;
   static unsigned long limit=0x00000000ffffffff;
   unsigned long jadr=((unsigned long) iadr & mask);
#if !defined (__ia64__)
    if ( jadr != 0x0000000000000000) {
#else
    if ( jadr != 0x6000000000000000) {
#endif
      printf("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
      printf("LOCB/LOCF: address %p exceeds the 32 bit address space\n", iadr);
      printf("or is not in the data segments\n");
      printf("This may result in program crash or incorrect results\n");
      printf("Therefore we will stop here\n");
      printf("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
      exit (999);
    }
    jadr=((unsigned long) iadr & limit);
    return ((unsigned) jadr);
}
long idisp0_ (void  *a,void  *b)  { return chkloc(b)-chkloc(a);       }
long idisp1_ (char  *a,char  *b)  { return idisp0_(a,b)+1;            }
long idisp2_ (short *a,short *b)  { return idisp0_(a,b)+sizeof(short);}
long idisp4_ (int   *a,int   *b)  { return idisp0_(a,b)+sizeof(int);  }
long iponter_(long  *a,long **b)  { return chkloc(*b)-chkloc(a);      }
#else
long idisp0_ (void  *a,void  *b)  { return b-a;                       }
long idisp1_ (char  *a,char  *b)  { return idisp0_(a,b)+1;            }
long idisp2_ (short *a,short *b)  { return idisp0_(a,b)+sizeof(short);}
long idisp4_ (int   *a,int   *b)  { return idisp0_(a,b)+sizeof(int);  }
long iponter_(long  *a,long **b)  { return *b-a;      }
#endif
#endif
long malloc_(long  *size){return (long) malloc((size_t) *size);}
void  free_(long  *ptr) { long i = *ptr;  free  ((char *)i);}
void memcpy_(void *dest, const void *src, size_t *n) { memcpy (dest, src, *n); }
#endif
