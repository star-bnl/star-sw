* $Id: creadg.F,v 1.1.1.1 2004/01/15 00:08:47 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CREADG(GEOSIG)
C
#include "gcalor/cjoint.inc"
#include "gcalor/cbert.inc"
C
      DIMENSION GEOSIG(240)
C
      K=1
      DO 20 J = 1,4
CZ data already read by CRBERT called by CALINI 19.june 92 CZ
CZ changed 5-28-92  crazy number for A=240 set to GEOSIG(239)
         DO 10 I = 4,594,10
            IF(K.LT.240) THEN
               GEOSIG(K) = SNGL(CRSC(I + (J-1)*600))
               GEOSIG(K) = 3.1416 * GEOSIG(K)**2 * 1.E+24
            ELSE
               GEOSIG(K) = GEOSIG(K-1)
            ENDIF
   10    K = K + 1
   20 CONTINUE
      RETURN
      END
