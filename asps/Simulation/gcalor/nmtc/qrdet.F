* $Id: qrdet.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE QRDET(NODATA,DATA,ENER)
      IMPLICIT REAL *8  (A-H,O-Z)
#include "gcalor/cbert3.inc"
#include "gcalor/cisob2.inc"
      DIMENSION DATA(380)
      SAVE
C
      IE=DABS(ENER/20.0)
      UNIV=(ENER-DFLOAT(IE)*20.0)/20.0
      DO10 I=1,25
   10 CRDT(I)=0.0
      K=(NODATA*IE)+1
      IF(INPT)50 ,20 ,50
   20 N=NODATA
   30 L=K+NODATA
      DO40 I=1,N
         CRDT(I)=(DATA(L)-DATA(K))*UNIV+DATA(K)
         K=K+1
   40 L=L+1
      INPT=0
      RETURN
   50 K=INPT-1+K
      N=2
      GOTO30
      END
