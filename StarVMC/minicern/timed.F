*
* $Id: timed.F,v 1.1.1.1 2004/07/17 20:01:57 perev Exp $
*
* $Log: timed.F,v $
* Revision 1.1.1.1  2004/07/17 20:01:57  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:47  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:30  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/02/15 17:50:38  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
#if defined(CERNLIB_QMVAX)
#include "vaxsys/timed.F"
#else
      SUBROUTINE TIMED (T)
C
C CERN PROGLIB# Z007    TIMED   DUMMY   .VERSION KERNFOR  4.05  821202
C
C-    DUMMY FOR NON-ESSENTIAL ROUTINE STILL MISSING ON YOUR MACHINE

      T = .99
      RETURN
      END
#endif
