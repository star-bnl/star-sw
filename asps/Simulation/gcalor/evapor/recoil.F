* $Id: recoil.F,v 1.1.1.1 2004/01/15 00:08:47 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE RECOIL
C
#include "gcalor/cinout.inc"
#include "gcalor/ccomon.inc"
#include "geant321/camass.inc"
      SAVE
C
      IF(APR.LE.4.) THEN
        EREC = 0.0
      ELSE
        PX=0.
        PY=0.
        PZ=0.
        IF(NOPART.NE.0) THEN
          DO 10 I=1,NOPART
            TM = XMASS(KIND(I))*1000.
            PI = EP(I)*SQRT (1.+2.*TM/EP(I))
            PX = PI*ALPHA(I) +PX
            PY = PI*BETA(I) +PY
            PZ = PI*GAM(I)   +PZ
   10     CONTINUE
        ENDIF
        KT= TIP(NO)
        TM = XMASS(KT)*1000.
        PZ = EC(NO)*SQRT (1.+2.*TM/EC(NO)) - PZ
        AA  = APR * 931.49432
        EREC = SQRT (AA**2 +PX**2 +PY**2 +PZ**2)-AA
      ENDIF
      RETURN
      END
