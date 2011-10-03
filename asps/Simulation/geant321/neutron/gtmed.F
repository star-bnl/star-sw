*
* $Id: gtmed.F,v 1.1.1.1 2004/01/15 00:12:21 potekhin Exp $
*
* $Log: gtmed.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:21  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:30:57  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:21:57  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.48  by  S.Giani
*-- Author :
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
      WRITE(IOUT,'('' MICAP GTMED: GEANT Medium '',I5,
     +             '' not found ==> STOP'')') MEDGEA
      STOP
   20 RETURN
      END
