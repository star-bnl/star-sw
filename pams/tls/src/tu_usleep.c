/* tu_usleep.c  A Fortran callable routine for the C usleep function.
 *
 * Created  31-March-1992  D. Olson
 *
 */

#ifdef VAX
#include <signal.h>
#endif /* VAX */

#ifdef HPUX

#include <time.h>
#include <sys/param.h>

#endif /* HPUX */

#ifdef __MAIN__
main()
{
    int i, nleft, nusecs, nslept;

    nusecs = 500000;
    for(i=0;i<12;i++)
    {
	nleft = tu_usleep(&nusecs);
	nslept = nusecs - nleft;
	printf(" I slept for %d micro seconds.\n",nslept);
    }
}
#endif /* __MAIN__ */


int tu_usleep_( usecs )
     int *usecs;
{
#ifdef HPUX
    struct itimerval now, next;
#endif /* HPUX */
    int nusecs, ans;
#ifdef VAX
    float secs;
#endif /* VAX */
    nusecs = *usecs;
/*
    printf("will sleep for %d micro seconds.\n",nusecs);
*/
#ifdef VAX
    secs = ((float ) nusecs)/1000000;
    lib$wait( &secs );
    ans = 0;
#endif /* VAX */
#ifdef SUN
    ans = usleep( nusecs );
#endif /* SUN */
#ifdef HPUX
    now.it_value.tv_sec = *usecs / 1000000;
    now.it_value.tv_usec =  *usecs % 1000000;
    now.it_interval.tv_sec = 0;
    now.it_interval.tv_usec = 0;
    setitimer( ITIMER_REAL, &now, &next );
    pause();
    ans = 0;
#endif /* HPUX */
    return ans;
}

#ifdef VMS

int tu_usleep( usecs )
     int *usecs;
{
    return tu_usleep_( usecs );
}
#endif /* VMS */
