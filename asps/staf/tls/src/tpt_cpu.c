/* tpt_cpu.c				created 25-Oct-1991  D. Olson
 *					modified 12jun95 - CETull
 *						for sun4os5
 *
 * This routine makes a machine independent fortran callable
 * cpu time function.
 *
 *  	call tpt_cpu(i, seconds, delta_seconds)
 *	if i = 0 then the timers are reset
 *	if i .not.= 0 seconds = elapsed cpu time since i=0
 *	and delta_seconds = cpu time since last call
 *	The resolution is machine dependent, usually about 1/60 second.
 */



#ifdef sun4 /* sun OS 4 definitions *******************************************/

#define HZ 60.
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/resource.h>
int getrusage();

#endif /* sun4 ****************************************************************/

#ifdef sun4os5 /* Solaris definition ******************************************/

#include <sys/types.h>
#include <sys/times.h>
#include <sys/param.h>
#endif /* sun4os5 ************************************************************/

#ifdef VAX /* VAX definitions ************************************************/

#include time
#define tms tbuffer
#define HZ 100.

#endif /* VAX ***************************************************************/

#ifdef IRIX /* IRIX definitions ***********************************************/

#include <sys/types.h>
#include <sys/times.h>
#include <sys/param.h>

#endif /* IRIX ***************************************************************/

#ifdef AIX /* AIX definitions ***********************************************/

#include <sys/types.h>
#include <sys/times.h>
#include <sys/param.h>

#endif /* AIX ***************************************************************/

#ifdef HPUX /* HPUX definitions **********************************************/

#include <sys/types.h>
#include <sys/times.h>
#include <sys/param.h>

#endif /* HPUX ***************************************************************/


void tpt_cpu_( i, seconds, delta_seconds )
int	*i;
float	*seconds, *delta_seconds;
{
	static struct tms now;
	static struct tms start;
	static int first = 1;
	static float cpu_last;

	if(first==1)
	{
		times( &start );
		first = 0;
	}
	times( &now );
	if(i==0)
	{
		*seconds = 0.;
		*delta_seconds = 0.;
		cpu_last = 0.;
	}
	else
	{
#ifndef VAX
		*seconds = (float) (now.tms_utime + now.tms_stime
				- start.tms_utime - start.tms_stime)/HZ;
#else
		*seconds = (float) (now.proc_user_time + now.proc_system_time
				- start.proc_user_time
				 - start.proc_system_time)/HZ;
#endif
		*delta_seconds = *seconds - cpu_last;
		cpu_last = *seconds;
	}
}

#ifdef VAX

void tpt_cpu( i, seconds, delta_seconds )
int	*i;
float	*seconds, *delta_seconds;
{
	tpt_cpu_( i, seconds, delta_seconds );
}

#endif /* VAX */

/***************************************************************************
 *  define high resolution timing routine for SUN
 */

#ifdef SUN

void tpt_cpu_fine_sun_( i, seconds, delta_seconds )
int	*i;
float	*seconds, *delta_seconds;
{
	static struct rusage stats, start;
	long cpu_sec, cpu_usec;
	static float cpu_last;
	float cpu;

	if( *i == 0 )
	{
		getrusage( RUSAGE_SELF, &start );
		*seconds = 0.;
		*delta_seconds = 0.;
		cpu_last = 0.;
	}
	else
	{
		getrusage( RUSAGE_SELF, &stats );
		cpu_sec = stats.ru_utime.tv_sec + stats.ru_stime.tv_sec
			- start.ru_utime.tv_sec - start.ru_stime.tv_sec;
		cpu_usec = stats.ru_utime.tv_usec +
			stats.ru_stime.tv_usec
			- start.ru_utime.tv_usec
			- start.ru_stime.tv_usec;
		cpu = (float)cpu_sec + ((float)cpu_usec)/1.e+6;
		*seconds = cpu;
		*delta_seconds = cpu - cpu_last;
		cpu_last = cpu;
	}
}

#endif /* SUN */
