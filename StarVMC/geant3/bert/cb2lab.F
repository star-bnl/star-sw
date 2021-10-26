* $Id: cb2lab.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CB2LAB(ALPHA,BETA,GAM,U,V,W,ULAB,VLAB,WLAB)
C***************************************************************
C
C   convert direction cosines of paticles produced in BERT
C   into Lab system
C
C   input: ALPHA,BETA,GAM : direction consine in BERT
C          u,v,w          : direction cosines in Lab system of
C                           the projectile
C   output:ulab,vlab,wlab : direction cosine in Lab system
C
C
C**************************************************************
C
      RT = SQRT(U*U + V*V)
      if(RT.EQ.0.0) THEN
         SINTH = 0.0
         COSTH = 1.0
         COSPHI = 1.0
         SINPHI = 0.0
      ELSE
         SINTH = RT
         COSTH = W
         COSPHI = U/RT
         SINPHI = V/RT
      ENDIF
      T1   =  COSTH * ALPHA  + SINTH * GAM
      ULAB = COSPHI * T1 - SINPHI * BETA
      VLAB = SINPHI * T1 + COSPHI * BETA
      WLAB = COSTH * GAM -  SINTH * ALPHA
C
C     U = COSPHI*COSTH* ALPHA  -SINPHI* BETA  +COSPHI*SINTH* GAMA
C
C     V = SINPHI*COSTH* ALPHA  +COSPHI* BETA  +SINPHI*SINTH* GAMA
C
C     W =       -SINTH* ALPHA  +   0. * BETA         +COSTH* GAMA
C                      ROTATION MATRIX
      RETURN
      END
