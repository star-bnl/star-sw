* $Id: cerup.F,v 1.1.1.1 2004/01/15 00:08:47 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CERUP
C*****MODIFIED TO OBTAIN APR,ZPR AFTER CAS + EVAP (8-68,T.W.A.)
#include "gcalor/chevap.inc"
#include "gcalor/ccomon.inc"
#include "gcalor/ccomn3.inc"
#include "gcalor/ccomn2.inc"
#include "gcalor/cforcn.inc"
#include "gcalor/cinout.inc"
#include "gcalor/caz.inc"
      DIMENSION FPART(6)
      SAVE
C
      IF(EX) 10 ,20 ,50
   10 NEGEX =NEGEX +1
   20 DO 30  I=1,6
   30 NPART(I)=0
      HEVSUM = 0.0
      UU = 0.0
      RETURN
   40 LOWAZ=LOWAZ+1
      GO TO 20
   50 IF(APR.LE.4.)GO TO 40
      IF(ZPR.LE.2.)GO TO 40
      IF(APR.LE.ZPR)GO TO 40
      M2= APR
      M3= ZPR
      CALL CDRES(M2,M3,EX,NPART,EPART,HEVSUM,UU,EREC,HEPART)
      DO 60   I=1,6
   60 IF(NPART(I).GT.0) GOTO 70
      FKEY =1.
      CALL CDRES(M2,M3,EX,NPART,EPART,HEVSUM,UU,EREC,HEPART)
      FKEY=0.
   70 CONTINUE
   80 DO 90  I=1,6
         IF(NPART(I).GT.200) THEN
            CALL CERROR(' CERUP : N_EVAP > 200 $')
            WRITE(IO,10000) I,NPART(I)
10000     FORMAT(' CERUP: I=',I5,' NPART=',I5)
            NPART(I)=200
         ENDIF
         FPART(I)=NPART(I)
   90 CONTINUE
      ZPR=ZPR-FPART(2)-FPART(3)-2.*(FPART(5)+FPART(6)) -FPART(4)
      APR=APR-FPART(1)-FPART(2)-2.*FPART(3)-3.*(FPART(4)+FPART(5))
     +    -4.*FPART(6)
C         CHANGED JAN.1,1986
      NPART1=NPART(1)
      NPART2=NPART(2)
      IF(NPART1.LE.0)GO TO 110
      DO 100 I=1,NPART1
         KINDI(I)=2
         IBBARR(I)=1
  100 CONTINUE
  110 IF(NPART2.LE.0) RETURN
      DO 120 I=1,NPART2
         KINDI(I)=1
         IBBARR(I)=1
  120 CONTINUE
C        END OF CHANGE
      RETURN
      END
