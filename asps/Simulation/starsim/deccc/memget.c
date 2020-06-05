/*
* $Id: memget.c,v 1.4 2020/06/04 23:25:18 perev Exp $
* $Log: memget.c,v $
* Revision 1.4  2020/06/04 23:25:18  perev
* assert added to chack 64b consistensy
*
* Revision 1.3  2004/09/19 00:10:34  perev
* Walgrind pseudo leak fixed
*
* Revision 1.2  2004/06/26 00:17:51  potekhin
* Added stdlib.h
*
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
*
* Revision 1.2  2001/03/05 11:55:22  nevski
* headers clean-up
*
* Revision 1.1  2001/02/27 10:15:17  nevski
*  first working release
*/
/*CMZ :  2.00/01 09/01/2000  21.14.37  by  Pavel Nevski*/
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

#include <stdlib.h>
#include <assert.h>
void *fakePerevPointer=0;
unsigned long memget_  (unsigned int *n)
{assert(0); 
 return ( (unsigned long) malloc(*n) );  }

unsigned long memgetf_ (unsigned int *n)
{ assert(0);
  fakePerevPointer=malloc(*n<<2);
  return ( ((unsigned long)fakePerevPointer)>>2);  
}

