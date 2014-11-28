*
* $Id: vline.F,v 1.1 2009/02/25 00:08:29 fisyak Exp $
*
* $Log: vline.F,v $
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
      SUBROUTINE VLINE (B,ALPHA,C,BETA,A,N)
C
C CERN PROGLIB# F121    VLINE           .VERSION KERNFOR  1.0   710701
C ORIG. 01/07/71
C
      DIMENSION A(*),B(*),C(*)
C
      IF (N.LE.0)  RETURN
      DO 9 I= 1,N
    9 A(I)= B(I)*ALPHA + C(I)*BETA
C
      RETURN
      END
