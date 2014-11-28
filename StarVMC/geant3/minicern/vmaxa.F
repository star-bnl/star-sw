*
* $Id: vmaxa.F,v 1.1 2009/02/25 00:08:30 fisyak Exp $
*
* $Log: vmaxa.F,v $
* Revision 1.1  2009/02/25 00:08:30  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:20  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:21  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:50:18  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      FUNCTION VMAXA (A,N)
C
C CERN PROGLIB# F121    VMAXA           .VERSION KERNFOR  1.0   710701
C ORIG. 01/07/71
C
      DIMENSION A(*)
C
      FMAX= 0.
      IF (N.LE.0)  GO TO 100
      DO 9 I= 1,N
      FMAX =  MAX (ABS (A(I)), FMAX)
    9 CONTINUE
C
  100 VMAXA= FMAX
      RETURN
      END
