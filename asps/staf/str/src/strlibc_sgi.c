
/*  General Description of this package:

        Filename: strlibc_sgi.c  created 3-Sep-1995  R. Hackenburg
        Copied from strlibc_sun.c,  created 3-Jan-1994  R. Hackenburg
        Augmented to include strcpuuser_ and strcputps_, 6-Sep-95
*/

static const char sccsid[] = "@(#)"__FILE__"\t\t1.6\tCreated 06 Sep 1995 19:14:00, \tcompiled "__DATE__;


/*      Define structures, types etc., from the system:      */

#include <unistd.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/resource.h>


/*      Define procedures ("c- and f-callable functions"):     */




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

