/* tu_time()  returns the C time() value (time in seconds since 1/1/70).
 *
 * Created on 11-Dec-1991 by D. Olson
 *
 */

#ifdef VMS
#include <time>
#else
#include <sys/types.h>
#include <sys/time.h>
#endif /* VMS */

/****************************************************************************
 * tu_time() - a Fortran callable function returning the C time() value.
 *
 */

#ifdef VMS
time_t tu_time()
#else
time_t tu_time_()
#endif /* VMS */
{
    time_t t;

    time(&t);

    return t;
}
