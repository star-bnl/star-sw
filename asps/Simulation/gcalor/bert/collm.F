* $Id: collm.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE COLLM(M)
      SAVE
#include "gcalor/cbert.inc"
C
      REAL *8 B
      UNIV=E(2)+COL(6)-COL(11)
      UNIVE=E(1)+COL(11)
      UNIVER=COL(1)+COL(6)
      K=16
      DO10 I=1,9,4
         COL(K)=(PXYZ(I)*UNIV-PXYZ(I+1)*UNIVE)/UNIVER
         COL(K+3)=(PXYZ(I)+PXYZ(I+1))/COL(1)
C     VX
   10 K=K+1
      COL(22)=(PXYZ(10)*PXYZ(5)-PXYZ(9)*PXYZ(6))/COL(1)
C     QX
      COL(23)=(PXYZ(2)*PXYZ(9)-PXYZ(10)*PXYZ(1))/COL(1)
C     QY
      COL(24)=(PXYZ(6)*PXYZ(1)-PXYZ(5)*PXYZ(2))/COL(1)
      A=SNT/COL(14)
      B=A*COL(10)
C     (-BETA*COS PHI*SIN THETA/ALPHA + COS THETA)/P1P*P3P
      UNIV=COL(10)*(CST-A*SOPC*COL(13))/COL(12)
      UNIVE=B*SOPS/COL(12)
C     P3P*SIN PHI*SIN THETA/P1P*ALPHA
      UNIVER=(SOPC*B)+((E(3)+COL(9))/(COL(7)+1.0))
C     COS PHI*SIN THETA*P3P/ALPHA  +  (E3+E3P)/(1.0+GAMMA)
      K=19
      DO20 I=3,11,4
         PXYZ(I)=COL(K)*UNIVER+COL(K+3)*UNIVE+COL(K-3)*UNIV
   20 K=K+1
      IF(M)30,40,30
   30 IF(PT(15))40,60,40
   40 DO50 I=1,9,4
   50 PXYZ(I+3)=PXYZ(I)+PXYZ(I+1)-PXYZ(I+2)
      IF(M)60,130,60
   60 IF(PT(3))70,100,70
   70 PT(4)=PM(3)
      I1=3
   80 I2=-1
      CALLPSTOR
   90 IF(I1-3)100,100,120
  100 IF(PT(15))110,120,110
  110 PT(16)=DNCMS
C     NUCLEON MASS PER CM
      I1=4
      GOTO80
  120 PT(27)=0.0
      PT(39)=0.0
  130 RETURN
      END
