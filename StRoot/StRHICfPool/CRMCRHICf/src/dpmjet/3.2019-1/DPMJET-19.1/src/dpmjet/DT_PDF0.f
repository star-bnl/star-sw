
      SUBROUTINE DT_PDF0(Q2,X,F2,Val,Sea,Glu,Ipar)
 
C***********************************************************************
C This subroutine calculates F_2 and PDF below Q^2=Q_0^2=2 GeV^2       *
C an F_2-ansatz given in Capella et al. PLB 337(1994)358.              *
C                   IPAR  = 2212   proton                              *
C                         =  100   deuteron                            *
C This version dated 31.01.96 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION A , AA , ALPHAR , an , B , BBDEU , BDD , BDP , 
     &                 BUD , BUP , C , D , delta , DELTA0 , E , F2 , 
     &                 f2pdf , Glu , glu0 , ONE
      DOUBLE PRECISION Q2 , Sea , sea0 , TINY9 , Val , vald0 , valu0 , 
     &                 X , ZERO
      INTEGER Ipar , NPOINT
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TINY9=1.0D-9)
 
      PARAMETER (AA=0.1502D0,BBDEU=1.2D0,BUD=0.754D0,BDD=0.4495D0,
     &           BUP=1.2064D0,BDP=0.1798D0,DELTA0=0.07684D0,D=1.117D0,
     &           C=3.5489D0,A=0.2631D0,B=0.6452D0,ALPHAR=0.415D0,
     &           E=0.1D0)
 
      PARAMETER (NPOINT=16)
C     DIMENSION ABSZX(NPOINT),WEIGHT(NPOINT)
      DIMENSION Sea(3) , Val(2)
 
      delta = DELTA0*(1.0D0+2.0D0*Q2/(Q2+D))
      an = 1.5D0*(1.0D0+Q2/(Q2+C))
C proton, deuteron
      IF ( (Ipar.EQ.2212) .OR. (Ipar.EQ.100) ) THEN
         CALL DT_CKMTQ0(Q2,X,Ipar,valu0,vald0,sea0)
         Sea(1) = 0.75D0*sea0
         Sea(2) = Sea(1)
         Sea(3) = Sea(1)
         Val(1) = 9.0D0/4.0D0*valu0
         Val(2) = 9.0D0*vald0
         glu0 = Sea(1)/(1.0D0-X)
         F2 = sea0 + valu0 + vald0
         f2pdf = 4.0D0/9.0D0*(Val(1)+2.0D0*Sea(1))
     &           + 1.0D0/9.0D0*(Val(2)+2.0D0*Sea(2))
     &           + 1.0D0/9.0D0*(2.0D0*Sea(3))
         IF ( ABS(F2-f2pdf).GT.TINY9 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A,2E15.5)')
     &            'inconsistent PDF! ' , F2 , f2pdf
            STOP
         END IF
C*PHOJET105a
C        CALL GSET(ZERO,ONE,NPOINT,ABSZX,WEIGHT)
C*PHOJET112
 
C        CALL PHO_GAUSET(ZERO,ONE,NPOINT,ABSZX,WEIGHT)
 
C*
C        SUMQ = ZERO
C        SUMG = ZERO
C        DO 1 J=1,NPOINT
C           CALL DT_CKMTQ0(Q2,ABSZX(J),IPAR,VALU0,VALD0,SEA0)
C           VALU0 = 9.0D0/4.0D0*VALU0
C           VALD0 = 9.0D0*VALD0
C           SEA0  = 0.75D0*SEA0
C           SUMQ  = SUMQ+ (VALU0+VALD0+6.0D0*SEA0) *WEIGHT(J)
C           SUMG  = SUMG+ (SEA0/(1.0D0-ABSZX(J)))  *WEIGHT(J)
C   1    CONTINUE
C        GLU = GLU0*(1.0D0-SUMQ)/SUMG
      ELSE
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A,I4,A)') 'PDF0:   IPAR =' , 
     &        Ipar , ' not implemented!'
         STOP
      END IF
 
      END SUBROUTINE
