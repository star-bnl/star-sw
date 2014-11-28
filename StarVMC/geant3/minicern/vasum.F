*
* $Id: vasum.F,v 1.1 2009/02/25 00:08:29 fisyak Exp $
*
* $Log: vasum.F,v $
* Revision 1.1  2009/02/25 00:08:29  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:20  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:20  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:50:16  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      FUNCTION VASUM (A,N)
C
C CERN PROGLIB# F121    VASUM           .VERSION KERNFOR  1.0   710701
C ORIG. 01/07/71
C
      DIMENSION A(*)
C
      XX= 0.
      IF (N.LE.0)  GO TO 100
      DO 9 I= 1,N
    9 XX= XX + ABS (A(I))
C
  100 VASUM= XX
      RETURN
      END
