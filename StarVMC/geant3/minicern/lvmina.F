*
* $Id: lvmina.F,v 1.1 2009/02/25 00:08:27 fisyak Exp $
*
* $Log: lvmina.F,v $
* Revision 1.1  2009/02/25 00:08:27  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:15  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:11  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:50:13  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      FUNCTION LVMINA (A,N)
C
C CERN PROGLIB# F121    LVMINA          .VERSION KERNFOR  1.0   710701
C ORIG. 01/07/71
C
      DIMENSION A(*)
C
      IF (N.LE.0)  GO TO 100
      J= 1
      AMIN= ABS (A(1))
      DO 9 I= 1,N
      CA= ABS (A(I))
      IF (AMIN.LE.CA)  GO TO 9
      AMIN= CA
      J= I
    9 CONTINUE
      LVMINA = J
      RETURN
  100 LVMINA = 0
      RETURN
      END
