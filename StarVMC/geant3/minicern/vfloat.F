*
* $Id: vfloat.F,v 1.1 2009/02/25 00:08:29 fisyak Exp $
*
* $Log: vfloat.F,v $
* Revision 1.1  2009/02/25 00:08:29  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:20  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:21  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:50:17  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE VFLOAT (IA,A,N)
C
C CERN PROGLIB# F121    VFLOAT          .VERSION KERNFOR  3.15  820113
C ORIG. 03/05/72
C
      DIMENSION A(*), IA(*)
C
      IF (N.LE.0)  RETURN
      DO 9 J=1,N
    9 A(J)= IA(J)
      RETURN
      END
