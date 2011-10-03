*
* $Id: intrac.F,v 1.1.1.1 2004/07/17 20:01:56 perev Exp $
*
* $Log: intrac.F,v $
* Revision 1.1.1.1  2004/07/17 20:01:56  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:27  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:47  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:30  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/02/15 17:50:37  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
#if defined(CERNLIB_QMVAX)
#include "vaxsys/intrac.F"
#elif defined(CERNLIB_QMMPW)
#include "mpwgs/intrac.F"
#elif defined(CERNLIB_QMSUN)
#include "local/sungs/intrac.F"
#else
      LOGICAL FUNCTION INTRAC(DUMMY)
C
C CERN PROGLIB# Z044    INTRAC          .VERSION KERNFOR  4.10  850320
C ORIG. 20/02/83  HRR, DUMMY VERSION
C
      INTRAC = .FALSE.
      RETURN
      END
#endif
