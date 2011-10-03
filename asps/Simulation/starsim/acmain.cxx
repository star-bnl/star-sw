/********************************************************
* $Id: acmain.cxx,v 1.1.1.1 2004/01/12 23:49:36 potekhin Exp $
* $Log: acmain.cxx,v $
* Revision 1.1.1.1  2004/01/12 23:49:36  potekhin
*
* Revision 1.3  2002/04/29 00:58:33  nevski
* production support utilities
*
* Revision 1.2  2001/11/18 20:52:17  nevski
* get rid of stupid warning at link time, add pthread
*
* Revision 1.1  2001/02/27 10:14:06  nevski
*  first working release
*
*                       m a i n                         *
********************************************************/
#include <string.h>
#include <stdio.h>
#include <math.h>
extern "C"  { int agmain_(), MAIN__();  void  k_setar(int , char** ); 
              void  G77_date_y2kbuggy_0(), G77_vxtidate_y2kbuggy_0(); }
#if defined(CERNLIB_HPUX)
extern "C"  void  FTN_INITRAP();
#endif

/* different compilers use different symbols for GETARG: */
int       Margc=0;  char **       Margv=NULL;   // local
int       xargc=0;  char **       xargv=NULL;   // g77
int     f77argc=0;  char **     f77argv=NULL;   // For mips Fortran
int    f__xargc=0;  char **    f__xargv=NULL;   // g77 linux
int __argc_save=0;  char ** __argv_save=NULL;   // Pgf77

//const char *gCoPyRiGhT[] = {" "," "," "};
class  Copyright {public: Copyright(const char *id=0) { return; }};
static Copyright ATDataSetLibraryCopyright(" ");

int main   (int argc, char *argv[])
{     Margc  = argc;       Margv  = argv;
      xargc  = argc;       xargv  = argv;
   f77argc   = argc;     f77argv  = argv;
  f__xargc   = argc;    f__xargv  = argv;
 __argc_save = argc;  __argv_save = argv;
      k_setar(argc,argv);     // for kuip
#if defined(CERNLIB_HPUX)
  FTN_INITRAP();  fpsetmask(0);
#endif
  agmain_();
}

#if defined(CERNLIB_LINUX)
int   MAIN__                 (){ return printf(" MAIN__ called \n"); }
void  G77_date_y2kbuggy_0    (){printf(" G77_date_y2kbuggy_0 called \n"); }
void  G77_vxtidate_y2kbuggy_0(){printf(" G77_vxtidate_y2kbuggy_0 called \n");}
#endif

#if !defined(CERNLIB_LINUX)
/* define a uniform GETARG function for all compilers */
extern "C"  int   iargc_  ();
extern "C"  int   getarg_ (int*, char*, int);
int iargc_  ()        { return Margc; }
int getarg_ (int *k, char *args, int n)
{ int i=0;   if (*k<Margc) i=strlen(Margv[*k]);  if (i>n) i=n;
  strncpy(args,Margv[*k],i); memset (args+i,' ',n-i); return 0;
}
#endif

