/*/
static const char sccsid[] = "@(#)"__FILE__"\t\t1.6\tCreated 05 Sep 1995 19:14:00, \tcompiled "__DATE__;
/*  General Description of this package:

        Filename: strlibc_hpu.c  created 3-Jan-1994  R. Hackenburg
        Added strcpuuser_ and strcputps_ from strlibc_sgi.c,  created 3-Sep-1995  R. Hackenburg
*/

/*      Define structures, types etc., from the system:      */

#include <sys/times.h>
#include <sys/unistd.h>  /* This appears in the wrong place -- man pages say no "sys/" */


/*      Define procedures ("c- and f-callable functions"):     */



/**/
long    mclock_()

/*  Description:  This routine is made to look like the AT&T V.3 routine of the same name.

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
/**/
long    strcpuuser_()

/*  Description:  Returns the integer number of user CPU clock ticks.

  FORTRAN callable:

        INTEGER CPU_ticks
        REAL    CPU_seconds
        INTEGER STRCPUUSER
        INTEGER STRCPUTPS

        CPU_ticks   = STRCPUUSER()  !Get CPU usage platform-dependent ticks.
        CPU_seconds = CPU_ticks / STRCPUTPS() !Divide by platform's CPU Ticks per Second.
*/

{
        static struct tms cpu;

        times( &cpu );   /*  Fill the "cpu" structure (tms).  */

        return( cpu.tms_utime );   /* user CPU time. */
}
/**/
long    strcputps_()

/*  Description: Inquires from the system and returns the number of CPU ticks-per-second.

  FORTRAN callable:

        INTEGER CPU_ticks
        INTEGER STRCPUTPS

        CPU_ticks   = STRCPUTPS()  !Get platform-dependent CPU ticks-per-second.
*/

{
        return(sysconf( _SC_CLK_TCK ));
}
/**/
