/*
 * $Id: acmain.cc,v 1.4 1998/06/28 23:31:37 perev Exp $
 *
 * $Log: acmain.cc,v $
 * Revision 1.4  1998/06/28 23:31:37  perev
 * STAF size and others
 *
 * Revision 1.3  1998/06/23 21:01:15  perev
 * cleanup of getarg for linux
 *
 * Revision 1.2  1998/06/23 00:45:19  perev
 * getarg fix
 *
 * Revision 1.1  1998/06/05 20:55:21  perev
 * AGI commit
 *
 * Revision 1.1  1998/04/16 16:59:17  fisyak
 * 2nd pass with gstar
 *
 */
/* --------------------------------------------------*/
/*CMZ :          22/03/98  19.27.39  by  Pavel Nevski*/
/*-- Author :    Pavel Nevski   28/11/97             */
/*****************************************************/
/*                    m a i n                        */
/*****************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
int        __argc_save=0;	// Pgf77
char **    __argv_save=NULL;	//

int xargc=0;			// g77
char **xargv=NULL;		// g77

int f77argc = 0;      		// For mips Fortran
char **f77argv=NULL; 		//


extern "C"  int   agmain_ ();
extern "C"  int   MAIN__();
#if defined(CERNLIB_HPUX)
extern "C"  void  FTN_INITRAP();
#endif
extern "C"  int   getarg_ (int*, char*, int);
extern "C"  int   iargc_ ();
extern "C"  void  k_setar (int , char** );
extern "C"  void  asuMallocInit();
extern "C"  void  asuStack(void *);
 
int main    (int argc, char *argv[])
{
/* Init of AsuStack test */
 asuStack(NULL);

 __argc_save=argc;  __argv_save=argv;  k_setar(argc,argv);
       xargc=argc;      xargv = argv;
   f77argc = argc;    f77argv = argv;

#if defined(CERNLIB_HPUX)
  FTN_INITRAP();  fpsetmask(0);
#endif
  asuMallocInit();

// Request Staf size

for (int i=1; i<=argc; i++) { // Search -S <number>
  if (argv[i] && (!(strncmp(argv[i],"-S",2)) || !(strncmp(argv[i],"-s",2))) ) { 
     int n = atoi(argv[i+1]); 
     void *s = malloc(n*4000000);
     if (! s) { // Error no space
       printf (" STAF. No space for %d Mega words\n",n);
       exit (1);
     }

     printf ("STAF got space %d Mega words\n",n);
     free(s);
     break;
} }


  agmain_();
}

#ifndef Linux 
int getarg_ (int *k, char *args, int n)
{ int i=0;  memset (args,' ',n); 
if (*k<__argc_save) i=strlen(__argv_save[*k]);  if (i>n) i=n;
  strncpy(args,__argv_save[*k],i);   
}
int iargc_() { return __argc_save;}


#endif
