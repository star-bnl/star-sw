* $Id: gtmed.F,v 1.1.1.1 2004/01/15 00:08:48 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE GTMED(MEDGEA,MEDMOR)
#include "geant321/mmicap.inc"
#include "geant321/minput.inc"
C get MICAP material number
      DO 10 I=1,MEDIA
         IF(LD(LGE2MO+I).EQ.MEDGEA) THEN
            MEDMOR = I
            GOTO 20
         ENDIF
   10 CONTINUE
      WRITE(IOUT,'('' MICAP GTMED: GEANT Medium '',I5, '
     +        //' '' not found ==> STOP'')') MEDGEA
      STOP
   20 RETURN
      END
