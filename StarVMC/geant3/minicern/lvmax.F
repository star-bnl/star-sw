*
* $Id: lvmax.F,v 1.1 2009/02/25 00:08:26 fisyak Exp $
*
* $Log: lvmax.F,v $
* Revision 1.1  2009/02/25 00:08:26  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:15  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:11  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:50:12  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      FUNCTION LVMAX (A,N)
C
C CERN PROGLIB# F121    LVMAX           .VERSION KERNFOR  1.0   720321
C ORIG. 01/07/71
C
      DIMENSION A(*)
C
      IF (N.LE.0)  GO TO 100
      J= 1
      AMAX = A(1)
      DO 9 I= 1,N
      IF (A(I).LE.AMAX)  GO TO 9
      AMAX= A(I)
      J= I
    9 CONTINUE
      LVMAX = J
      RETURN
  100 LVMAX=0
      RETURN
      END
