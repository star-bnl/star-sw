*
* $Id: uswop.F,v 1.1 2009/02/25 00:08:29 fisyak Exp $
*
* $Log: uswop.F,v $
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
      SUBROUTINE USWOP (A,B,N)
C
C CERN PROGLIB# V303    USWOP           .VERSION KERNFOR  4.40  940929
C ORIG. 01/07/71 RKB
C
#if defined(CERNLIB_QINTCOPY)
      INTEGER   A(*),B(*), X
#endif
#if !defined(CERNLIB_QINTCOPY)
      DIMENSION A(*),B(*)
#endif
C
      IF (N.EQ.0) RETURN
         DO 10 I=1,N
      X=A(I)
      A(I)=B(I)
   10 B(I)=X
      RETURN
      END
