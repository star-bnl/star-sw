*
* $Id: vblank.F,v 1.1.1.1 2004/07/17 20:01:57 perev Exp $
*
* $Log: vblank.F,v $
* Revision 1.1.1.1  2004/07/17 20:01:57  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:47  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:32  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/02/15 17:50:16  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE VBLANK (A,N)
C
C CERN PROGLIB# F121    VBLANK          .VERSION KERNFOR  4.08  840613
C ORIG. 01/07/71
C
      INTEGER      A(*), BLANK
      DATA  BLANK  / 4H     /
C
      IF (N.LE.0)  RETURN
      DO 9 I= 1,N
    9 A(I)= BLANK
      RETURN
      END
