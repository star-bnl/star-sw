*
* $Id: ublank.F,v 1.1 2009/02/25 00:08:29 fisyak Exp $
*
* $Log: ublank.F,v $
* Revision 1.1  2009/02/25 00:08:29  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:19  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:19  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:50:14  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE UBLANK (IV,JL,JR)
C
C CERN PROGLIB# V300    UBLANK          .VERSION KERNFOR  4.08  840613
C ORIG. 01/01/65 JZ
C
      DIMENSION IV(80)
      DATA IBL     /4H     /
C
C
      DO 29 J=JL,JR
   29 IV(J)= IBL
      RETURN
      END
