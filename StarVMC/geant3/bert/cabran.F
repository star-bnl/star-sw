* $Id: cabran.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CABRAN(K1)
C
#include "geant321/crandm.inc"
C
#include "gcalor/cbert.inc"
C
      VALUE1 = RANDC(ISEED)
      VALUE1=VALUE1*SIGN
      VALUE2=0.0
      NOT=1
      DO10 I=2,K1
         VALUE2=CE(I)+VALUE2
         IF(VALUE2-VALUE1)10,20,20
   10 NOT=NOT+1
   20 RETURN
C     VALUE2=SUM OF CE FOR A PARTICULAR REGION--SUM F(I1)MASS
      END
