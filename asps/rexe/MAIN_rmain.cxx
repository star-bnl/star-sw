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

// A dummy global definition to satisfy the linker when linking mysqlclient library statically with --whole-archive
// /opt/software/linux-scientific7-x86_64/gcc-4.8.5/mysql-5.7.27-pfyt3fwtkubcc5eazmoqfick3lgp67mf/lib/libmysqlclient.a(posix_timers.c.o): In function `my_timer_initialize':
// (.text+0x140): undefined reference to `key_thread_timer_notifier'
unsigned int key_thread_timer_notifier = 0;

//______________________________________________________________________________
int main(int argcp, char **argv)
{
  static int argc = argcp;
#ifndef WIN32
   char appname[] = "Rint";
#else
   char appname[] = "Root_Rint";
#endif 

   TRint *theApp = new TRint(appname, &argc, argv, 0, 0);

 __argc_save=argc; __argv_save= argv; 
       xargc=argc;      xargv = argv;
   f77argc = argc;    f77argv = argv;
   Margc   = argc;     Margv  = argv;

 
   // Init Intrinsics, build all windows, and enter event loop
   theApp->Run();
 
   delete theApp;
 
   return(0);
}
