/*
 * $Id: traceqc.c,v 1.1 1998/04/16 17:03:34 fisyak Exp $
 *
 * $Log: traceqc.c,v $
 * Revision 1.1  1998/04/16 17:03:34  fisyak
 * 2nd pass with gstar
 *
 */
#if defined(CERNLIB_HPUX)
/*CMZ :          20/03/98  12.55.44  by  Pavel Nevski*/
/*CMZ :  1.30/00 22/04/97  14.49.55  by  Pavel Nevski*/
/*-- Author :    FR & JZ*/
#include <stdio.h>
void traceqc_()
{  void    U_STACK_TRACE();
  /* printf (" in traceqc   \n"); */
   U_STACK_TRACE();
  /* printf (" traceqc done \n"); */
}
 
#endif
