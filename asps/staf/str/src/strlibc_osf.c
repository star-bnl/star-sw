
/*  General Description of this package:

	Filename: strlibc_osf.c  created 24-Aug-1995  R. Hackenburg
	                         copied from strlibc_sun.c .
*/


/*	Define structures, types etc., from the system:      */

#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/resource.h>


/*	Define procedures ("c- and f-callable functions"):     */




long	mclock_()

/*	This routine is made to look like the AT&T V.3 routine of the same
	name.

  FORTRAN callable:

	INTEGER CPU_ticks
	REAL    CPU_seconds
	INTEGER mclock
	INTEGER STRCPUTPS

	CPU_ticks   = mclock()  !Get CPU usage platform-dependent ticks.
	CPU_seconds = CPU_ticks / STRCPUTPS() !Divide by platform's CPU Ticks per Second.
*/

{
	static struct tms cpu;

	times( &cpu );   /*  Fill the "cpu" structure (tms).  */

	return( cpu.tms_utime );   /* user CPU time. */
}
