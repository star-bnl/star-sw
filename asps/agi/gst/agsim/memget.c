#include <stdlib.h>
/*
 * $Id: memget.c,v 1.5 2000/01/09 21:20:03 nevski Exp $
 *
 * $Log: memget.c,v $
 * Revision 1.5  2000/01/09 21:20:03  nevski
 * hit interface update
 *
 * Revision 1.4  1998/08/03 17:23:56  didenko
 * correction for NT version by Faine
 *
 * Revision 1.3  1998/07/20 20:17:29  perev
 * Mods for exe SGI64 and HP rubber Zebra
 *
 * Revision 1.2  1998/06/05 20:55:09  perev
 * AGI commit
 *
 * Revision 1.1  1998/04/16 17:03:33  fisyak
 * 2nd pass with gstar
 *
 */
/*-- Author :    Mark Hsu 2/1/91 HPCSD, Kingston, NY.*/
/* memget.c: allow dynamic memory allocation from FORTRAN
 * Mark Hsu 2/1/91 HPCSD, Kingston, NY.
 * usage from FORTRAN:    I = memget (n)
 *                        J = memgetf(n)
 * where n is the number of bytes (words) requested
 *       I (returned value) - the base address (bytes)
 * or    J (returned value) - the base address in word
 * Comments:
 * malloc() requires unsigned arg. FORTRAN passes signed integers
 * malloc() returns a pointer;     memget(f) returns an integer.
 */

#include "PAM.h"
#define memget_  F77_NAME(memget,  MEMGET)
#define memgetf_ F77_NAME(memgetf, MEMGETF)
#define iudivd_  F77_NAME(iudivd,  IUDIVD)

unsigned long type_of_call memget_  (unsigned int *n)
{  return ( (unsigned long) malloc(*n) );  }

unsigned long type_of_call memgetf_ (unsigned int *n)
{  return ( (unsigned long) malloc(*n<<2)>>2);  }

unsigned long type_of_call iudivd_ (unsigned long *u1, unsigned long *u2) 
{ return (*u1 / *u2);}
