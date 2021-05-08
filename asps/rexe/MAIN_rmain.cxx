#include "TROOT.h"
#include "TRint.h"
/* different compilers use different symbols for GETARG: */
int       Margc=0;  char **       Margv=NULL;   // local
int       xargc=0;  char **       xargv=NULL;   // g77
int     f77argc=0;  char **     f77argv=NULL;   // For mips Fortran
int __argc_save=0;  char ** __argv_save=NULL;   // Pgf77
/* define a uniform GETARGF function for all compilers */
extern "C" {
  int iargcf_  ()        { return Margc; }
  int getargf_ (int *k, char *args, int n) { 
    int i=0;   if (*k<Margc) i=strlen(Margv[*k]);  if (i>n) i=n;
    strncpy(args,Margv[*k],i); memset (args+i,' ',n-i); return 0;
  }
}
//______________________________________________________________________________
int main(int argcp, char **argv)
{
  static int argc = argcp;
#ifndef WIN32
   char appname[] = "Rint";
#else
///   char appname[] = "Root_Rint";
   char appname[] = "root4star";
#endif 

///   TRint *theApp = new TRint(appname, &argc, argv, 0, 0);
   TRint theApp(appname, &argc, argv, 0, 0);

 __argc_save=argc; __argv_save= argv; 
       xargc=argc;      xargv = argv;
   f77argc = argc;    f77argv = argv;
   Margc   = argc;     Margv  = argv;

 
   // Init Intrinsics, build all windows, and enter event loop
///   theApp->Run();
   theApp.Run();
 
//   delete theApp;
 
   return(0);
}
