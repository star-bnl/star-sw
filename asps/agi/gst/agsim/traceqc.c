/*
 * $Id: traceqc.c,v 1.3 1998/07/10 01:09:37 fisyak Exp $
 *
 * $Log: traceqc.c,v $
 * Revision 1.3  1998/07/10 01:09:37  fisyak
 * remove comment
 *
 * Revision 1.2  1998/07/09 22:59:03  perev
 * replace fgsim.f -> fgsim.F
 *
 * Revision 1.1  1998/04/16 17:03:34  fisyak
 * 2nd pass with gstar
 *
 */
/*CMZ :          20/03/98  12.55.44  by  Pavel Nevski*/
/*CMZ :  1.30/00 22/04/97  14.49.55  by  Pavel Nevski*/
/*-- Author :    FR & JZ*/
#include <stdio.h>
void traceqc_()
{  
#ifdef CERNLIB_HPUX 
void    U_STACK_TRACE();
   U_STACK_TRACE();
#else
printf (" traceqc is not awalable \n"); 
#endif
}
 
