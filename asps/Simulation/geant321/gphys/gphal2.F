*
* $Id: gphal2.F,v 1.1.1.1 2004/01/15 00:12:14 potekhin Exp $
*
* $Log: gphal2.F,v $
* Revision 1.1.1.1  2004/01/15 00:12:14  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:30:39  atlascvs
* Importing CERNLIB version 08.21.
*
* Revision 1.1.1.1  1995/10/24 10:21:29  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.22  by  S.Giani
*-- Author :
      FUNCTION GPHAL2(BET)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *  Generates Photoelectron Angular ditribution from L2 shell     *
C.    *                                                                *
C.    *    ==>CALLED BY : GPHOT                                        *
C.    *       AUTHOR    : J. Chwastowski                               *
C.    *                                                                *
C.    ******************************************************************
C.
      DOUBLE PRECISION SAUT1,SAUT2,SAUT3,SAUT4,SAUT5,SAUT6,SAUT7
      DOUBLE PRECISION S1,S2,S3,S4,S5,S6,S7,ONE,HALF
      DOUBLE PRECISION COST,SIN2T,X,XI,XI2,XI3,ANOR,DRN
      DOUBLE PRECISION GAMA,G1,G2,G3,G4,G5,BETA,B1
      DOUBLE PRECISION BETVAL,AMAJOR
      PARAMETER(BETMAX=0.999999)
      PARAMETER (ONE=1,HALF=ONE/2)
      DIMENSION BETVAL(15),AMAJOR(15),RAN(2)
      DATA BETVAL / 0.0101,0.101,0.2,0.301,0.401,0.501
     +             ,0.601,0.701,0.801,0.901,0.9901,0.99901
     +             ,0.999901,0.9999901,0.99999901 /
      DATA AMAJOR / 2.05,2.2,2.4,2.6,2.72,3.0,3.2,3.6
     +             ,4.4,6.2,25.,88.,280.,900.,2880. /
      DATA ANOR / 0.0 /
      BETA = MIN(BET,BETMAX)
      B1 = ONE-BETA
      G2 = ONE/((ONE-BETA)*(ONE+BETA))
      GAMA = SQRT(G2)
      G3 = G2*GAMA
      G4 = G2**2
      G5 = G4*GAMA
      G1 = GAMA-ONE
      DO 10 I = 1,15
       IF(BETA.LE.BETVAL(I)) THEN
        ANOR = AMAJOR(I)*G5
        GO TO 20
       ENDIF
   10 CONTINUE
   20 CONTINUE
      ANOR = ONE/ANOR
      S1 = HALF*GAMA*(3*GAMA+1)
      S2 = -0.125D0*G2*(9*G2+30*GAMA-7)
      S3 = 0.25D0*G3*(G3+6*G2+11*GAMA-2)
      S4 = -.125D0*G4*G1*(GAMA+7)
      S5 = 2*(GAMA+ONE)
      S6 = 2*GAMA*(GAMA+ONE)
      S7 = 0.125D0*G4*BETA**2*(3*GAMA+ONE)
      GPHAL2 = 0.0
   30 CONTINUE
      CALL GRNDM(RAN,2)
      DRN = 2*RAN(1)
      COST = (DRN-B1)/(DRN*BETA+B1)
      X = ONE-BETA*COST
      XI  = ONE/X
      XI2 = XI**2
      XI3 = XI**3
      SIN2T = (ONE-COST)*(ONE+COST)
      SAUT1 = S1*XI2
      SAUT2 = S2*XI
      SAUT3 = S3
      SAUT4 = S4*X
      SAUT5 = S5*XI3-S6*XI2-S7*XI
      SAUT6 = SAUT5*SIN2T
      SAUT7 = (SAUT1+SAUT2+SAUT3+SAUT4+SAUT6)*ANOR
      IF(RAN(2).GT.SAUT7) GO TO 30
      GPHAL2 = COST
      END
