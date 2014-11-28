*
* $Id: rannor.F,v 1.1 2009/02/25 00:08:27 fisyak Exp $
*
* $Log: rannor.F,v $
* Revision 1.1  2009/02/25 00:08:27  fisyak
* Clean up mess created after merging
*
* Revision 1.1.1.1  2008/12/10 20:49:50  fisyak
* Merge with macos version
*
* Revision 1.1  2005/06/09 00:05:17  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:16  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:49:53  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE RANNOR (A,B)
C
C CERN PROGLIB# V100    RANNOR          .VERSION KERNFOR  4.18  880425
C ORIG. 18/10/77
C
      Y = RNDM(-1.)
      IF (Y.EQ.0.)  Y = RNDM(-1.)
      Z = RNDM(-1.)

      X = 6.283185*Z
      R = SQRT (-2.0*LOG(Y))
      A = R*SIN (X)
      B = R*COS (X)
      RETURN
      END
