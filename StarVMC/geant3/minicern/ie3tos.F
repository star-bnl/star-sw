*
* $Id: ie3tos.F,v 1.1 2009/02/25 00:08:26 fisyak Exp $
*
* $Log: ie3tos.F,v $
* Revision 1.1  2009/02/25 00:08:26  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:15  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:10  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:50:10  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
#if defined(CERNLIB_QMIRTD)
#include "irtdgs/ie3tos.F"
#elif defined(CERNLIB_QMVAX)
#include "vaxgs/ie3tos.F"
#else
      SUBROUTINE IE3TOS (MS,MT,NWDO,JBAD)
C
C CERN PROGLIB# M220    IE3TOS          .VERSION KERNFOR  4.15  861204

C-    Convert single precision for input with copy
C-    from source in IEEE to target in native data format
C-    Dummy routine valid for machines with IEEE internal repr.

      DIMENSION    MS(99), MT(99)

      CALL UCOPY (MS,MT,NWDO)

      JBAD = 0
      RETURN
      END
#endif
