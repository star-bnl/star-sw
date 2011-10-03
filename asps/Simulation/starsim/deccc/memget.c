/*
* $Id: memget.c,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Log: memget.c,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
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

unsigned long memget_  (unsigned int *n)
{  return ( (unsigned long) malloc(*n) );  }

unsigned long memgetf_ (unsigned int *n)
{  return ( (unsigned long) malloc(*n<<2)>>2);  }

