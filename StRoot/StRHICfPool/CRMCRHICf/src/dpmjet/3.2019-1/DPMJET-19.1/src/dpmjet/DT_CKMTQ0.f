
      SUBROUTINE DT_CKMTQ0(Q2,X,Ipar,Valu0,Vald0,Sea0)
 
C***********************************************************************
C This subroutine calculates F_2 and PDF below Q^2=Q_0^2=2 GeV^2       *
C an F_2-ansatz given in Capella et al. PLB 337(1994)358.              *
C                   IPAR  = 2212   proton                              *
C                         =  100   deuteron                            *
C This version dated 31.01.96 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION A , AA , ALPHAR , an , B , BBDEU , bd , BDD , 
     &                 BDP , bu , BUD , BUP , C , D , delta , DELTA0 , 
     &                 E , ONE , Q2 , Sea0
      DOUBLE PRECISION TINY9 , Vald0 , Valu0 , X , ZERO
      INTEGER Ipar
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TINY9=1.0D-9)
 
      PARAMETER (AA=0.1502D0,BBDEU=1.2D0,BUD=0.754D0,BDD=0.4495D0,
     &           BUP=1.2064D0,BDP=0.1798D0,DELTA0=0.07684D0,D=1.117D0,
     &           C=3.5489D0,A=0.2631D0,B=0.6452D0,ALPHAR=0.415D0,
     &           E=0.1D0)
 
      delta = DELTA0*(1.0D0+2.0D0*Q2/(Q2+D))
      an = 1.5D0*(1.0D0+Q2/(Q2+C))
C proton, deuteron
      IF ( (Ipar.EQ.2212) .OR. (Ipar.EQ.100) ) THEN
         IF ( Ipar.EQ.2212 ) THEN
            bu = BUP
            bd = BDP
         ELSE
            bu = BUD
            bd = BDD
         END IF
         Sea0 = AA*X**(-delta)*(1.0D0-X)**(an+4.0D0)*(Q2/(Q2+A))
     &          **(1.0D0+delta)
         Valu0 = bu*X**(1.0D0-ALPHAR)*(1.0D0-X)**an*(Q2/(Q2+B))
     &           **(ALPHAR)
         Vald0 = bd*X**(1.0D0-ALPHAR)*(1.0D0-X)**(an+1.0D0)*(Q2/(Q2+B))
     &           **(ALPHAR)
      ELSE
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A,I4,A)') 'CKMTQ0: IPAR =' , 
     &        Ipar , ' not implemented!'
         STOP
      END IF
      END SUBROUTINE
