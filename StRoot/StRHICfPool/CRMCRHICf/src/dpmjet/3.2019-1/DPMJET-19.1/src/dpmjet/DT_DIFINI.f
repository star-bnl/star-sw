
      SUBROUTINE DT_DIFINI
 
C***********************************************************************
C Initialization of common /DTDIKI/                                    *
C This version dated 12.02.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER k
      DOUBLE PRECISION OHALF , ONE , ZERO
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,OHALF=0.5D0,ONE=1.0D0)
 
C kinematics of diffractive interactions (DTUNUC 1.x)
      INCLUDE 'inc/dtdiki'
 
      DO k = 1 , 4
         PPOm(k) = ZERO
         PSC(k) = ZERO
         PPF(k) = ZERO
         PTF(k) = ZERO
         PPLm1(k) = ZERO
         PPLm2(k) = ZERO
         PTLm1(k) = ZERO
         PTLm2(k) = ZERO
      END DO
      DO k = 1 , 2
         XPH(k) = ZERO
         XPPo(k) = ZERO
         XTH(k) = ZERO
         XTPo(k) = ZERO
         IFPpo(k) = 0
         IFTpo(k) = 0
      END DO
      IDPr = 0
      IDXpr = 0
      IDTr = 0
      IDXtr = 0
 
      END SUBROUTINE
