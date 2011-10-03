* $Id: frmicc.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE FRMICC(GPART)
#include "geant321/crandm.inc"
      DIMENSION G(3)
      REAL * 8 GPART,G
      SAVE
C
      DO 10 I = 1,3
   10 G(I) = RANDC(ISEED)
C     FIND LARGEST OF 3 RANDOM NOS.
      IF(G(3).LT.G(2)) GO TO 40
C     3.GTE.2
      IF(G(3).LT.G(1))GO TO 30
C     3.GTE.2,AND 3.GTE.1
      GPART=G(3)
   20 RETURN
   30 GPART=G(1)
C     3.GTE.2 AND 3.LT.1 OR 3.LT.2 AND 2.LT.1
      GO TO 20
   40 IF(G(2).LT.G(1))GO TO 30
      GPART=G(2)
C     3.LT.2,AND 2.GTE.1
      GO TO 20
      END
