
/*  General Description of this package:

	Filename: strlibc_sun.c  created 3-Jan-1994  R. Hackenburg
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

	INTEGER CPU_ticks, CPU_seconds
	INTEGER mclock
	INTEGER STRCPUTPS

	CPU_ticks   = mclock()  !Get CPU usage platform-dependent ticks.
	CPU_seconds = CPU_ticks / STRCPUTPS() !Divide by platform's CPU Ticks per Second.
*/

{
	static struct tms cpu;

	times( &cpu );   /*  Fill the "cpu" structure (tms).  */

	return( cpu.tms_utime + cpu.tms_stime );
}
