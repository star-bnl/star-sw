/*
 * $Id: acmain.c,v 1.1 1998/04/16 16:59:17 fisyak Exp $
 *
 * $Log: acmain.c,v $
 * Revision 1.1  1998/04/16 16:59:17  fisyak
 * 2nd pass with gstar
 *
 */
* ------------------------------------------------------
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
#if defined(CERNLIB_CC)
/*CMZ :          22/03/98  19.27.39  by  Pavel Nevski*/
/*-- Author :    Pavel Nevski   28/11/97*/
/*****************************************************/
/*                    m a i n                        */
/*****************************************************/
#include <string.h>
#include <math.h>
static int        Margc=0;
static char **    Margv=NULL;
extern "C"  int   agmain_ ();
#if defined(CERNLIB_HPUX)
extern "C"  void  FTN_INITRAP();
#endif
extern "C"  int   getarg_ (int*, char*, int);
extern "C"  void  k_setar (int , char** );
 
int main    (int argc, char *argv[])
{ Margc=argc;  Margv=argv;  k_setar(argc,argv);
#if defined(CERNLIB_HPUX)
  FTN_INITRAP();  fpsetmask(0);
#endif
  agmain_();
}
 
int getarg_ (int *k, char *args, int n)
{ int i=0;   if (*k<Margc) i=strlen(Margv[*k]);  if (i>n) i=n;
  strncpy(args,Margv[*k],i);   memset (args+i,' ',n-i); return 0;
}
 
#endif
