/* tu_sleep.c  A Fortran callable routine for the C sleep function.
 *
 * Created  26-March-1992  D. Olson
 *
 */

#ifdef VAX
#include <signal.h>
#endif /* VAX */

#ifdef __MAIN__
main()
{
    int i, nleft, nsecs, nslept;

    nsecs = 1;
    for(i=0;i<12;i++)
    {
	nleft = tu_sleep(&nsecs);
	nslept = nsecs - nleft;
	printf(" I slept for %d seconds.\n",nslept);
    }
}
#endif /* __MAIN__ */


int tu_sleep_( isecs )
     int *isecs;
{
    int nsecs, ans;
#ifdef VAX
    float secs;
#endif /* VAX */
    nsecs = *isecs;
/*
    printf("will sleep for %d seconds.\n",nsecs);
*/
#ifdef VAX
    secs = (float ) nsecs;
    lib$wait( &secs );
    ans = 0;
#else
    ans = sleep( nsecs );
#endif
    return ans;
}

#ifdef VMS

int tu_sleep( isecs )
     int *isecs;
{
    return tu_sleep_( isecs );
}
#endif /* VMS */
