* $Id: icalge.F,v 1.1.1.1 2004/01/15 00:08:47 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      FUNCTION ICALGE(IP)
C*******************************************
C
C INPUT : CALOR particle type
C OUTPUT: GEANT particle type
C
C******************************************
C
C
      DIMENSION NCALGE(0:11)
C       convert CALOR particle code to GEANT
C                   p   n pi+ pi0 pi- mu+ mu- D   T   He3 Alpha Gamma
      DATA NCALGE/ 14, 13,  8,  7,  9,  5,  6, 45, 46, 49, 47,  1/
C
      IF(IP .LE. 11 .AND. IP.GE.0) THEN
         ICALGE = NCALGE(IP)
      ELSE
         ICALGE = 0
      ENDIF
      RETURN
      END
