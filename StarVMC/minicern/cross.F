*
* $Id: cross.F,v 1.1.1.1 2004/07/17 20:01:56 perev Exp $
*
* $Log: cross.F,v $
* Revision 1.1.1.1  2004/07/17 20:01:56  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:27  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:46  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:29  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/02/15 17:49:51  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE CROSS(X,Y,Z)
C
C CERN PROGLIB# F117    CROSS           .VERSION KERNFOR  2.02  730125
C ORIG. 01/01/64
C
      COMMON /SLATE/Z1,Z2,XQX(38)
      DIMENSION X(3),Y(3),Z(3)
C
C
      Z1=X(2)*Y(3)-X(3)*Y(2)
      Z2=X(3)*Y(1)-X(1)*Y(3)
      Z(3)=X(1)*Y(2)-X(2)*Y(1)
      Z(1)=Z1
      Z(2)=Z2
      RETURN
      END
