* $Id: calcxs.F,v 1.1.1.1 2004/01/15 00:08:46 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CALCXS
C***********************************************
C
C calculate cross section for given material
C
C***********************************************
#include "gcalor/ccomon.inc"
#include "gcalor/cgeos.inc"
#include "gcalor/cxpd.inc"
C
      M=1
      DO 10  I=1,7
         SIGMX(I,M) = 0.0
   10 CONTINUE
      TOT = 0.0
      DO 20  L=1,NEL(MAT)
         EION(L,M) = CZFOI(ZZ(L,M))*1.0E-6
         NA = INT(A(L,M)+0.5)
         SIGG(L,M) = DEN(L,M)*GEOSIG(NA)
         TOT = TOT + SIGG(L,M)
   20 CONTINUE
      DO 30  IT=1,5
         HSIGG(IT,M) = DENH(M) * HSIGMX(IT) * 1.E24
   30 CONTINUE
      SIGMX(1,M) = TOT + HSIGG(1,M)
      SIGMX(2,M) = TOT + HSIGG(2,M)
      SIGMX(3,M) = TOT + SGPIMX + HSIGG(3,M)
      SIGMX(4,M) = 0.
      SIGMX(5,M) = TOT + SGPIMX + HSIGG(5,M)
      SIGMX(6,M) = SGMUMX
      SIGMX(7,M) = SGMUMX
C
      MT =MXMAT +1
      SIGMX(1,MT)= SGPIMX
      SIGMX(2,MT)= SGPIMX
      SIGMX(3,MT)= SGPIMX
      SIGMX(4,MT)= 0.
      SIGMX(5,MT)= SGPIMX
      SIGMX(6,MT)= SGMUMX
      SIGMX(7,MT)= SGMUMX
CZ      CALL RANGE
      K =1
      SUMARG = DENH(K)* 21.132
      III = NEL(K)
      DO 40   I=1,III
         SUMARG = DEN(I,K)*ZZ(I,K)*(ZZ(I,K)+1.)* (10.566-.333*
     +   ALOG(ZZ(I,K)* A(I,K))) +SUMARG
   40 CONTINUE
      ARG(K) = SQRT (.498*SUMARG)
      RETURN
      END
