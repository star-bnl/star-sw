/*
 * $Id: memget.c,v 1.1 1998/04/10 15:30:59 fisyak Exp $
 *
 * $Log: memget.c,v $
 * Revision 1.1  1998/04/10 15:30:59  fisyak
 * Move gstar into CVS
 *
 */
#include "pawlib/comis/comis/pilot.h"
/*CMZ :          08/07/97  16.16.29  by  Pavel Nevski*/
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
memget_(n)
int *n;
{
  /* malloc() requires unsigned arg. FORTRAN passes signed integers */
  unsigned i;
  i = (unsigned)  *n;
  /* malloc() returns a pointer;     memget() returns an integer.   */
  return ( (int) malloc(i) );
}
