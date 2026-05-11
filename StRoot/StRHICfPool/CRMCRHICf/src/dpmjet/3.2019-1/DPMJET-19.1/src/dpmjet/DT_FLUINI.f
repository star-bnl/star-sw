
      SUBROUTINE DT_FLUINI
 
C***********************************************************************
C Initialisation of the nucleon-nucleon cross section fluctuation      *
C treatment. The original version by J. Ranft.                         *
C This version dated 21.04.95 is revised by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION A , af , B , DX , fluix , flus , flusi , flusu , 
     &                 flusuu , OM , ONE , TINY10 , TWO , x , ZERO
      INTEGER i , j , N
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY10=1.0D-10,ONE=1.0D0,TWO=2.0D0)
      PARAMETER (A=0.1D0,B=0.893D0,OM=1.1D0,N=6,DX=0.003D0)
 
C n-n cross section fluctuations
      INCLUDE 'inc/dtxsfl'
      DIMENSION flusi(NBINS) , fluix(NBINS)
 
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,99010)
99010 FORMAT (/,1X,'FLUINI:  hadronic cross section fluctuations ',
     &        'treated')
 
      flusu = ZERO
      flusuu = ZERO
 
      DO i = 1 , NBINS
         x = DBLE(i)*DX
         fluix(i) = x
         flus = ((x-B)/(OM*B))**N
         IF ( flus.LE.20.0D0 ) THEN
            flusi(i) = (x/B)*EXP(-flus)/(x/B+A)
         ELSE
            flusi(i) = ZERO
         END IF
         flusu = flusu + flusi(i)
      END DO
      DO i = 1 , NBINS
         flusuu = flusuu + flusi(i)/flusu
         flusi(i) = flusuu
      END DO
 
C     WRITE(LOUT,1001)
C1001 FORMAT(1X,'FLUCTUATIONS')
C     CALL PLOT(FLUIX,FLUSI,1000,1,1000,0.0D0,0.06D0,0.0D0,0.01D0)
 
      DO i = 1 , NBINS
         af = DBLE(i)*0.001D0
         DO j = 1 , NBINS
            IF ( af.LE.flusi(j) ) THEN
               FLUixx(i) = fluix(j)
               GOTO 100
            END IF
         END DO
 100  END DO
      FLUixx(1) = fluix(1)
      FLUixx(NBINS) = fluix(NBINS)
 
      END SUBROUTINE
