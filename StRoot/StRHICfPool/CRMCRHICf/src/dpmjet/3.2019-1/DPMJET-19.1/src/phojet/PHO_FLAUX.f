
      SUBROUTINE PHO_FLAUX(Equark,K)
C***********************************************************************
C
C    auxiliary subroutine to select flavours
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DEPS , DT_RNDM , Equark , PHO_BETAF , sum , 
     &                 wght , xi
      INTEGER K
      SAVE 
 
      PARAMETER (DEPS=1.D-14)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  some hadron information, will be deleted in future versions
      INCLUDE 'inc/pohdrn'
 
      DIMENSION wght(9)
 
C  calculate weights for given energy
      IF ( Equark.LT.QMAss(1) ) THEN
         IF ( IDEb(16).GE.5 .AND. LPRi.GT.4 ) WRITE (LO,'(1X,A,E12.3)')
     &         'PHO_FLAUX: VERY SMALL MASS' , Equark
         wght(1) = 0.5D0
         wght(2) = 0.5D0
         wght(3) = 0.D0
         wght(4) = 0.D0
         sum = 1.D0
      ELSE
         sum = 0.D0
         DO K = 1 , NFS
            IF ( Equark.GT.QMAss(K) ) THEN
               wght(K) = PHO_BETAF(Equark,QMAss(K),BET)
            ELSE
               wght(K) = 0.D0
            END IF
            sum = sum + wght(K)
         END DO
      END IF
C  sample flavours
      xi = sum*(DT_RNDM(sum)-DEPS)
      K = 0
      sum = 0.D0
 100  K = K + 1
      sum = sum + wght(K)
      IF ( xi.GT.sum ) GOTO 100
C  debug output
      IF ( IDEb(16).GE.20 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I5)')
     &         'PHO_FLAUX: selected flavour' , K
      END IF
      END SUBROUTINE
