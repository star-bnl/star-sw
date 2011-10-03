*
* $Id: svgam7.F,v 1.1.1.1 2004/07/17 20:02:50 perev Exp $
*
* $Log: svgam7.F,v $
* Revision 1.1.1.1  2004/07/17 20:02:50  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:27  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:40  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:19  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:21:12  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 19/05/94  14.28.55  by  S.Ravndal
*-- Author :
      SUBROUTINE SVGAM7(A,X)
      DIMENSION U(2)
      REAL LA
      GA=A-1.
      LA=SQRT(2.*A-1.)
      EP=1.570796327+ATAN(GA/LA)
      RO=1.570796327-EP
    1 CALL GRNDM(U,2)
      X=GA+LA*TAN(EP*U(1)+RO)
      IF (X.EQ.0) GOTO 1
      Y=LOG(1.+((X-GA)/LA)**2) +GA*LOG(X/GA)-X+GA
      IF(LOG(U(2)).GT.Y) GOTO 1
      RETURN
      END
