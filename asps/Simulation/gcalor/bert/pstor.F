* $Id: pstor.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE PSTOR
#include "gcalor/cbert.inc"
      SAVE
C
      L=(I1*12)-28
      IF(I2)10,60,70
   10 JJ=0
      IF(PM(3)-DNCMS)30,30,20
   20 I1=I1+1
      JJ=1
C     X-Y-Z-COORDINATES OF COLLISION POINT
   30 UNIV=DSQRT(PXYZ(I1)*PXYZ(I1)+PXYZ(I1+4)*PXYZ(I1+4)+PXYZ(I1+8)
     +*PXYZ(I1+8))
      K=I1+8
      DO40 I=I1,K,4
         PT(L)=PXYZ(I)/UNIV
   40 L=L+1
      I1=I1-JJ
   50 PT(L)=CLSM
      PT(L+1)=CURR(11)
      PT(L-6)=C(1)
      PT(L-5)=C(2)
      PT(L-4)=C(3)
      RETURN
   60 K=14
      GOTO90
   70 IF(I2-2)80,110,110
   80 K=17
   90 UNIV=DSQRT(PNIDK(K)*PNIDK(K)+PNIDK(K+1)*PNIDK(K+1)+PNIDK
     +(K+2)*PNIDK(K+2))
      J=K+2
      DO100 I=K,J
         PT(L)=PNIDK(I)/UNIV
  100 L=L+1
      GOTO50
  110 UNIV=DSQRT(PT(L-3)*PT(L-3)+PT(L-2)*PT(L-2)+PT(L-1)*PT(L-1))
      K=L-1
      M=L-3
      DO120 I=M,K
         PT(L)=PT(I)/UNIV
  120 L=L+1
      GOTO50
      END
