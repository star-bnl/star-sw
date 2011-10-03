*
* $Id: gamma.F,v 1.1.1.1 2004/07/17 20:01:56 perev Exp $
*
* $Log: gamma.F,v $
* Revision 1.1.1.1  2004/07/17 20:01:56  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:44  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:34  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/04/01 15:01:54  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
#if (defined(CERNLIB_DOUBLE))&&(!defined(CERNLIB_IBM))
      FUNCTION GAMMA(X)
      DOUBLE PRECISION DGAMMA,D

      SROUND(D)=D+(D-SNGL(D))
      GAMMA=SROUND(DGAMMA(DBLE(X)))
      RETURN
      END
#endif
