
      SUBROUTINE DT_DTWOPD(Umo,Ecm1,Ecm2,Pcm1,Pcm2,Cod1,Cof1,Sif1,Cod2,
     &                     Cof2,Sif2,Am1,Am2)
 
C***********************************************************************
C Two-particle decay.                                                  *
C  UMO                 cm-energy of the decaying system       (input)  *
C  AM1/AM2             masses of the decay products           (input)  *
C  ECM1,ECM2/PCM1,PCM2 cm-energies/momenta of the decay prod. (output) *
C  COD,COF,SIF         direction cosines of the decay prod.   (output) *
C Revised by S. Roesler, 20.11.95                                      *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION Am1 , Am2 , Cod1 , Cod2 , Cof1 , Cof2 , DT_RNDM , 
     &                 Ecm1 , Ecm2 , ONE , Pcm1 , Pcm2 , Sif1 , Sif2 , 
     &                 TINY10 , TWO , Umo , ZERO
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,ONE=1.0D0,TWO=2.0D0,ZERO=0.0D0)
 
      IF ( Umo.LT.(Am1+Am2) ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) Umo , Am1 , Am2
99010    FORMAT (1X,'DTWOPD:    inconsistent kinematics - UMO,AM1,AM2 ',
     &           3E12.3)
         STOP
      END IF
 
      Ecm1 = ((Umo-Am2)*(Umo+Am2)+Am1*Am1)/(TWO*Umo)
      Ecm2 = Umo - Ecm1
      Pcm1 = SQRT((Ecm1-Am1)*(Ecm1+Am1))
      Pcm2 = Pcm1
      CALL DT_DSFECF(Sif1,Cof1)
      Cod1 = TWO*DT_RNDM(Pcm2) - ONE
      Cod2 = -Cod1
      Cof2 = -Cof1
      Sif2 = -Sif1
 
      END SUBROUTINE
