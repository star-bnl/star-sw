* $Id: rnmaxf.F,v 1.1.1.1 2004/01/15 00:08:48 potekhin Exp $
* $Name:  $
#include <commons/config.h>
C*********************************************************************
      FUNCTION RNMAXF(T)
C T := most probable value of distribution
C*********************************************************************
      DATA FF/0./
      SAVE FF,R1SQ,W,U
      U=EXPRNF(U)
      IF(FF) 30 ,10 ,30
   10 R1=FLTRNF(R1)
      R2=FLTRNF(R2)
      R1SQ=R1*R1
      R2SQ=R2*R2
      RSQ=R1SQ+R2SQ
      IF(RSQ-1.) 20 ,20 ,10
   20 W=EXPRNF(W)/RSQ
      FF=1.
      RNMAXF=(R2SQ*W+U)*T
      GO TO 40
   30 FF=0.
      RNMAXF=(R1SQ*W+U)*T
   40 RETURN
      END
