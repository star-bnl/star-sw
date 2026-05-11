
      SUBROUTINE DT_DCHANT
 
      IMPLICIT NONE
      DOUBLE PRECISION hv , hwt , ONE , TINY10 , ZERO
      INTEGER i , ik1 , ik2 , j
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,ONE=1.0D0,ZERO=0.0D0)
 
C HADRIN: decay channel information
      INCLUDE 'inc/hndech'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
 
      DIMENSION hwt(IDMAX9)
 
C change of weights wt from absolut values into the sum of wt of a dec.
      DO j = 1 , IDMAX9
         hwt(j) = ZERO
      END DO
C     DO 999 KKK=1,210
C        WRITE(LOUT,'(A8,F5.2,2E10.3,2I4,2I10)')
C    &      ANAME(KKK),AAM(KKK),GA(KKK),TAU(KKK),IICH(KKK),IIBAR(KKK),
C    &      K1(KKK),K2(KKK)
C 999 CONTINUE
C     STOP
      DO i = 1 , 210
         ik1 = K1(i)
         ik2 = K2(i)
         hv = ZERO
         DO j = ik1 , ik2
            hv = hv + WT(j)
            hwt(j) = hv
C*sr 13.1.95
 
            IF ( LPRi.GT.4 .AND. hwt(j).GT.1.0001 ) WRITE (LOUt,99010)
     &           hwt(j) , j , i , ik1
99010       FORMAT (2X,' ERROR IN HWT =',1F10.5,' J,I,K1=',3I5)
         END DO
      END DO
      DO j = 1 , IDMAX9
         WT(j) = hwt(j)
      END DO
 
      END SUBROUTINE
