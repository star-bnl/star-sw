*
* $Id: vmatr.F,v 1.1 2005/06/09 00:05:20 fisyak Exp $
*
* $Log: vmatr.F,v $
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
      SUBROUTINE VMATR (A,G,X,N,K)
C
C CERN PROGLIB# F121    VMATR           .VERSION KERNFOR  1.0   710701
C ORIG. 01/07/71
C
      DIMENSION A(*),X(*),G(K,N)
C
      IF (N.LE.0)  RETURN
      DO 9 I= 1,K
      X(I)= 0.
      DO 9 J= 1,N
    9 X(I)= X(I) + G(I,J)*A(J)
      RETURN
      END
