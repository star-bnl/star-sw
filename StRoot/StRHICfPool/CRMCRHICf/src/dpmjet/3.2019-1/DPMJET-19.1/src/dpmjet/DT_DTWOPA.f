
      SUBROUTINE DT_DTWOPA(E1,E2,P1,P2,Cod1,Cod2,Cof1,Cof2,Sif1,Sif2,
     &                     It1,It2,Umoo,Ecm,P,N,Am1,Am2)
 
      IMPLICIT NONE
      DOUBLE PRECISION Am1 , Am2 , ama , ama2 , amb , amte , Cod1 , 
     &                 Cod2 , Cof1 , Cof2 , E1 , E2 , Ecm , P , P1 , 
     &                 P2 , Sif1 , Sif2 , tr , Umoo
      INTEGER It1 , It2 , N
      SAVE 
 
C     ******************************************************
C     QUASI TWO PARTICLE PRODUCTION
C     TWOPAR CALCULATES THE ENERGYS AND THE MOMENTA
C     FOR THE CREATED PARTICLES OR RESONANCES IT1 AND IT2
C     IN THE CM - SYSTEM
C     COD1,COD2,COF1,COF2,SIF1,SIF2 ARE THE ANGLES FOR
C     SPHERICAL COORDINATES
C     ******************************************************
 
C particle properties (BAMJET index convention),
C (dublicate of DTPART for HADRIN)
      INCLUDE 'inc/hnablt'
 
      ama = Am1
      amb = Am2
      ama2 = ama*ama
      E1 = ((Umoo-amb)*(Umoo+amb)+ama2)/(2.0D0*Umoo)
      E2 = Umoo - E1
      IF ( E1.LT.ama*1.00001D0 ) E1 = ama*1.00001D0
      amte = (E1-ama)*(E1+ama)
      amte = amte + 1.D-18
      P1 = SQRT(amte)
      P2 = P1
C     / P2 / = / P1 /  BUT OPPOSITE DIRECTIONS
C     DETERMINATION  OF  THE ANGLES
C     COS(THETA1)=COD1      COS(THETA2)=COD2
C     SIN(PHI1)=SIF1        SIN(PHI2)=SIF2
C     COS(PHI1)=COF1        COS(PHI2)=COF2
C     PHI IS UNIFORMLY DISTRIBUTED IN ( 0,2*PI )
      CALL DT_DSFECF(Cof1,Sif1)
      Cof2 = -Cof1
      Sif2 = -Sif1
C     CALCULATION OF THETA1
      CALL DT_DTCHOI(tr,P,P1,Ecm,E1,It1,It2,N,Am1,Am2)
      Cod1 = (tr-ama2-AMH(N)*AMH(N)+2.0D0*Ecm*E1)/(2.0D0*P*P1+1.D-18)
      IF ( Cod1.GT.0.9999999D0 ) Cod1 = 0.9999999D0
      Cod2 = -Cod1
      END SUBROUTINE
