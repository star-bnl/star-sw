/*
* $Id: traceqc.c,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Name:  $
* $Log: traceqc.c,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.3  2003/09/29 16:18:46  nevski
* linux tracing
*
* Revision 1.2  2001/03/05 11:55:22  nevski
* headers clean-up
*
* Revision 1.1  2001/02/27 10:15:18  nevski
*  first working release
*/

/*CMZ :  2.00/00 03/02/99  12.18.01  by  Pavel Nevski*/
/*-- Author :    Frans Rademaker & Julius Zoll       */
#include <stdio.h>

void U_STACK_TRACE();
void traceqc_()
{
#if defined(CERNLIB_HPUX)

   printf (" in traceqc   \n"); */
   U_STACK_TRACE();
  /* printf (" traceqc done \n"); */

#elif defined(CERNLIB_LINUX)

//#ifdef __linux__
#include <execinfo.h>
#include <stdio.h> 
#define  __USE_GNU
#include <dlfcn.h>
#define  DEPTH    20

  void*    ltrace[DEPTH];
  int      depth,n;
  unsigned long addr,symaddr,diff;
  Dl_info  info;  

  depth  = backtrace (ltrace, DEPTH);
  for (n = 0; n < depth; ++n) 
  {
    addr = (unsigned long) ltrace[n];
    if (dladdr (ltrace[n], &info) && info.dli_fname && info.dli_fname[0]) 
    {
      char *libname =  info.dli_fname;
      char *symname = (info.dli_sname&&info.dli_sname[0]?info.dli_sname:"???");
      symaddr       = (unsigned long) info.dli_saddr;
      diff          = (addr >= symaddr ? addr - symaddr : symaddr - addr);
      printf (" 0x%08lx %.100s %s 0x%lx [%.100s]\n",
	       addr, symname, addr >= symaddr ? "+" : "-", diff, libname);
    } 
    else { printf (" 0x%08lx <unknown function>\n", addr); }
  }

#else
   printf (" Interrupt trace routine not available yet \n");
#endif
}

