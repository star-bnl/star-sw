*
* $Id: ucopyn.F,v 1.1 2009/02/25 00:08:29 fisyak Exp $
*
* $Log: ucopyn.F,v $
* Revision 1.1  2009/02/25 00:08:29  fisyak
* Clean up mess created after merging
*
* Revision 1.1  2005/06/09 00:05:19  fisyak
* Add stuff for reconstruction
*
* Revision 1.1  2005/05/26 22:28:20  fisyak
* Add stuff necessary for reconstruction packages
*
* Revision 1.1.1.1  1996/02/15 17:50:15  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE UCOPYN (IA,IX,N)
C
C CERN PROGLIB# V301    UCOPYN          .VERSION KERNFOR  2.04  730725
C ORIG. 01/01/68
C
      DIMENSION IA(*),IX(*)
C
      IF (N.EQ.0) RETURN
      DO 10 J=1,N
   10 IX(J)= -IA(J)
      RETURN
      END
