
      SUBROUTINE PHO_GETLTR(P1,P2,Gam,Gamb,Dele,Irej)
C********************************************************************
C
C     calculate Lorentz boots for arbitrary Lorentz transformation
C
C     input:   P1    initial 4 vector
C              P2    final 4 vector
C
C     output:  GAM(3),GAMB(3)
C              DELE   energy deviation
C              IREJ   0 success
C                     1 failure
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Dele , DREL , Gam , Gamb , P1 , P2 , pa , pm1 , 
     &                 pp
      INTEGER i , Irej , k
      SAVE 
 
      PARAMETER (DREL=0.001D0)
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      DIMENSION P1(4) , P2(4) , Gam(3) , Gamb(3) , pa(4) , pp(4)
 
      Irej = 1
      DO k = 1 , 4
         pa(k) = P1(k)
         pp(k) = P1(k)
      END DO
      pm1 = P1(4)**2 - P1(1)**2 - P1(2)**2 - P1(3)**2
      DO i = 1 , 3
         pp(i) = P2(i)
         pp(4) = pm1 + pp(1)**2 + pp(2)**2 + pp(3)**2
         IF ( pp(4).LE.0.D0 ) RETURN
         pp(4) = SQRT(pp(4))
         Gamb(i) = (SQRT(pa(4)**2-pa(i)**2+pp(i)**2)*pp(i)-pa(4)*pa(i))
     &             /(pa(4)**2+pp(i)**2)
         Gam(i) = 1.D0/SQRT(1.D0-Gamb(i)**2)
         Gamb(i) = Gamb(i)*Gam(i)
         DO k = 1 , 4
            pa(k) = pp(k)
         END DO
      END DO
      Dele = P2(4) - pp(4)
      Irej = 0
C  consistency check
C     IF(ABS(P2(4)-PP(4))/MAX(P2(4),PP(4)).GT.DREL) THEN
C       PM2 = P2(4)**2-P2(1)**2-P2(2)**2-P2(3)**2
C       WRITE(LO,'(/1X,A,2E12.5)')
C    &    'PHO_GETLTR: INCONSISTENT ENERGIES',P2(4),PP(4)
C       WRITE(LO,'(1X,A,2E12.4)') 'INPUT MASSES',PM1,PM2
C       WRITE(LO,'(1X,A,4E12.4)') 'INPUT ',P1
C       WRITE(LO,'(1X,A,4E12.4)') 'OUTPUT',P2
C       WRITE(LO,'(1X,A,4E12.4)') 'INTERN',PP
C     ENDIF
      END SUBROUTINE
