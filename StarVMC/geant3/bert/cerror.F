* $Id: cerror.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CERROR(CARG)
C
#include "geant321/minput.inc"
#include "geant321/cerrcm.inc"
C
      CHARACTER*1 CARG(50),CPRT(50)
      CHARACTER*1 ZEI,CEND
      DATA CEND/'$'/
C
      DO 10 I=1,50
         ZEI=CARG(I)
         IF(ZEI.EQ.CEND) GOTO 20
         CPRT(I)=CARG(I)
   10 CONTINUE
   20 DO 30 J=I,50
         CPRT(J)=' '
   30 CONTINUE
      WRITE(IOUT,*) ' HETC : ERROR in ',CPRT
C
      CERRF = .TRUE.
      RETURN
      END
