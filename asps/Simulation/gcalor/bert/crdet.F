* $Id: crdet.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CRDET(NODATA,DATA,ENER)
      REAL*8 DATA(6426),ENER
#include "gcalor/cbert.inc"
      SAVE
C
      IE=DABS(ENER/20.0)
C     ENERGY INTERVAL
      UNIV=(ENER-DFLOAT(IE)*20.0)/20.0
C     INPT=0 IF WHOLE INTERVAL CONSIDERED
C     NODATA=DATA PER ENERGY INTERVAL
      DO10 I=1,25
   10 CRDT(I)=0.0
C     ANSWERS STORED IN CRDT
   20 K=(NODATA*IE)+1
   30 IF(INPT)40,50 ,80
   40 WRITE(6,*) ' CALOR: ERROR in CRDET ====> STOP'
      STOP
   50 N=NODATA
   60 L=K+NODATA
      DO70 I=1,N
         CRDT(I)=(DATA(L)-DATA(K))*UNIV+DATA(K)
         K=K+1
   70 L=L+1
      INPT=0
      RETURN
   80 K=INPT-1+K
      N=2
      GOTO60
C     NOT ALL PARTS EVALUATED
      END
