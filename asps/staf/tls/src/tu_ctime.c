/* %W%  %G%  A Fortran version of the C ctime() routine.
 *
 */

/****************************************************************************
 *  tu_ctime - a Fortran callable routine that translates an integer time
 *		like the C ctime() function.
 *
 */

void tu_ctime_();
void tu_ctime();

#ifdef SUN
#include <time.h>
#include <string.h>
#endif /* SUN */

#ifdef HPUX
#include <time.h>
#include <string.h>
#endif /* HPUX */

#ifdef VMS

#include <descrip.h>
#include <time.h>
#include <string.h>

#endif /* VMS */


#ifdef VMS
void tu_ctime( i, c )
int *i;
struct dsc$descriptor *c;
{
  tu_ctime_( i, c->dsc$a_pointer, c->dsc$w_length);
}

#endif /* VMS */

void tu_ctime_( i, c, clen )
int *i;
char *c;
int clen;
{
    int minlen;
    minlen = 24;
    if(clen < minlen) minlen = clen;
    strncpy(c, ctime( i ), minlen);
}  

