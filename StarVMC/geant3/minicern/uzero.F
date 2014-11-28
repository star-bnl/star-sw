*
* $Id: uzero.F,v 1.1 2009/02/25 00:08:29 fisyak Exp $
*
* $Log: uzero.F,v $
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
      SUBROUTINE UZERO (A,JL,JR)
C
C CERN PROGLIB# V300    UZERO           .VERSION KERNFOR  4.40  940929
C ORIG. 01/01/65 JZ
C
#if defined(CERNLIB_QINTZERO)
      INTEGER   A(*)
#endif
#if !defined(CERNLIB_QINTZERO)
      DIMENSION A(*)
#endif
C
      DO 29 J=JL,JR
#if defined(CERNLIB_QINTZERO)
   29 A(J)=0
#endif
#if !defined(CERNLIB_QINTZERO)
   29 A(J)=0.
#endif
      RETURN
      END
