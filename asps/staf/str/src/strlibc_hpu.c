/*/
/*  General Description of this package:

	Filename: strlibc_hpu.c  created 3-Jan-1994  R. Hackenburg

*/

/*	Define structures, types etc., from the system:      */

#define _INCLUDE_POSIX_SOURCE
#include <sys/times.h>
#include <sys/unistd.h>  /* This appears in the wrong place -- man pages say no "sys/" */


/*	Define procedures ("c- and f-callable functions"):     */



/**/
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
	struct tms cpu;

	times( &cpu );   /*  Fill the "cpu" structure (tms).  */

	return( cpu.tms_utime );   /* user CPU times. */
}
/**/
long	STRCPUTPS_()

/*	This routine returns the clock resolution.

  FORTRAN callable:

	REAL    CPU_seconds
	INETEGR CPU_ticks
	INTEGER STR_CPUTPS

	CPU_seconds = CPU_ticks / STRCPUTPS() !Divide by platform's CPU Ticks per Second.
*/

{
	long ticks_per_second;

	ticks_per_second = sysconf( _SC_CLK_TCK );

	return( ticks_per_second );
}
