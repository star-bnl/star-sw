#include <stdlib.h>
/*
 * $Id: memget.c,v 1.4 1998/08/03 17:23:56 didenko Exp $
 *
 * $Log: memget.c,v $
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
 * usage from FORTRAN:    a = memget(n)
 * where n is the number of bytes requested and the value returned
 * in a is the base address. To access the allocation, pass the
 * pointer value as an address by using the val function, e.g.,
 *
 *  iptr = memget(n)
 *  call fsub(n, val(iptr),...)
 *  . . .
 *  subroutine fsub(n,array,...)
 */
/*  char *malloc(); */
#include "PAM.h"
#define memget_ F77_NAME(memget, MEMGET)
#define iudivd_ F77_NAME(iudivd, IUDIVD)

unsigned long type_of_call memget_(unsigned int *n)
{
  /* malloc() requires unsigned arg. FORTRAN passes signed integers */
  /* malloc() returns a pointer;     memget() returns an integer.   */
  return ( (unsigned long) malloc(*n) );
}
unsigned long type_of_call iudivd_ (unsigned long *u1, unsigned long *u2) { return (*u1 / *u2);}
