/* tls_loc.c				created 29-JUL-1993  R. Morse
 *
 * This routine makes a machine independent fortran callable
 * %LOC function
 *
 *  	location = tls_loc ( object )
 *
 *      the integer*4 return is the address of object...
 */
#include "fortranc.h"
#define tls_loc_ F77_NAME(tls_loc,TLS_LOC)
int type_of_call tls_loc_( object )
int    object;
{
       return ( object ) ;
}
