* $Id: igecal.F,v 1.1.1.1 2004/01/15 00:08:47 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      FUNCTION IGECAL(IP)
C*******************************************
C
C INPUT : GEANT particle type
C OUTPUT: CALOR particle type or -1
C
C******************************************
C
C
      DIMENSION NGECAL(48)
C
C       convert GEANT particle code to CALOR
C -1 indicates a particle not implemented in CALOR
      DATA NGECAL/ -1, -1, -1, -1, -1, -1, -1,  2,  4, -1,
     +             -1, -1,  1,  0, -1, -1, -1, -1, -1, -1,
     +             28*-1/
C
C
      IF(IP .LE. 48 .AND. IP.GT.0) THEN
         IGECAL = NGECAL(IP)
      ELSE
         IGECAL = -1
      ENDIF
      RETURN
      END
