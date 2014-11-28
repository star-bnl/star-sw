*
* $Id: vmul.F,v 1.1 2009/02/25 00:08:30 fisyak Exp $
*
* $Log: vmul.F,v $
* Revision 1.1  2009/02/25 00:08:30  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:20  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:22  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:50:18  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE VMUL (A,B,X,N)
C
C CERN PROGLIB# F121    VMUL            .VERSION KERNFOR  1.0   710701
C ORIG. 01/07/71
C
      DIMENSION A(*),B(*),X(*)
C
      IF (N.LE.0)  RETURN
      DO 9 I= 1,N
    9 X(I)= A(I) * B(I)
      RETURN
      END
