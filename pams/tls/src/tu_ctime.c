/* %W%  %G%  A Fortran version of the C ctime() routine.
 *
 */

/****************************************************************************
 *  tu_ctime - a Fortran callable routine that translates an integer time
 *		like the C ctime() function.
 *
 *  12jun95 - cetull@lbl.gov - modify for Solaris
 */

void tu_ctime_();
void tu_ctime();

#ifdef VMS
#include <descrip.h>
#endif /* VMS */

#include <time.h>
#include <string.h>




#ifdef VMS
void tu_ctime( i, c )
int *i;
struct dsc$descriptor *c;
{
  tu_ctime_( i, c->dsc$a_pointer, c->dsc$w_length);
}

#endif /* VMS */

void tu_ctime_( i, c, clen )
time_t *i;
char *c;
int clen;
{
    int minlen;
    minlen = 24;
    if(clen < minlen) minlen = clen;
    strncpy(c, ctime( i ), minlen);
}  

