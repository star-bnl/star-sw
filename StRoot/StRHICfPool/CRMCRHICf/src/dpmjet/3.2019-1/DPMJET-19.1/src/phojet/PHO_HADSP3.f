
      SUBROUTINE PHO_HADSP3(Iflb,Xs1,Xmax,Xsoft1,Irej)
C***********************************************************************
C
C     split hadron momentum XMAX into diquark & quark pair
C     using lower cut-off: AS
C
C     input:   IFLB    compressed particle code of particle to split
C              XS1     sum of x values already selected
C              XMAX    maximal x possible
C
C     output:  XS1     new sum of x values
C              XSOFT1  field of selected x values
C
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DEPS , psbar , psmes , pvbar1 , pvbar2 , pvmes1 , 
     &                 pvmes2 , xbmin , xdum1 , xdum2 , Xmax , xmin , 
     &                 xmmin , xpot1 , xpot2 , Xs1 , Xsoft1 , xsoft2
      INTEGER i , Iflb , IPHO_BAR3 , Irej
      SAVE 
      PARAMETER (DEPS=1.D-8)
 
      DIMENSION Xsoft1(50) , xsoft2(50)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  internal rejection counters
      INCLUDE 'inc/poloop'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
 
      DIMENSION xpot1(3) , xpot2(3) , xmin(2,3)
 
C  model exponents
      DATA pvmes1/ - 0.5D0/
      DATA pvmes2/ - 0.5D0/
      DATA psmes/ - 0.99D0/
      DATA pvbar1/1.5D0/
      DATA pvbar2/ - 0.5D0/
      DATA psbar/ - 0.99D0/
C
      Irej = 0
C
C  determine exponents
C  particle 1
C
      xmmin = 0.3D0/ECMp
      xbmin = 1.6D0/ECMp
C  mesonic particle
      IF ( IPHO_BAR3(Iflb,0).EQ.0 ) THEN
         xpot1(1) = pvmes1
         xmin(1,1) = xmmin
         xpot1(2) = pvmes2
         xmin(1,2) = xmmin
         xpot1(3) = psmes
         xmin(1,3) = xmmin
C  baryonic particle
      ELSE
         xpot1(1) = pvbar1
         xmin(1,1) = xbmin
         xpot1(2) = pvbar2
         xmin(1,2) = xmmin
         xpot1(3) = psbar
         xmin(1,3) = xmmin
      END IF
C  particle 2
C  mesonic particle
      xpot2(1) = pvmes1
      xmin(2,1) = xmmin
      xpot2(2) = pvmes2
      xmin(2,2) = xmmin
      xpot2(3) = psmes
      xmin(2,3) = xmmin
C
      xdum1 = 0.01D0
      xdum2 = 0.99D0
      CALL PHO_SELSXS(3,3,xpot1,xpot2,xmin,Xs1,xdum1,Xmax,xdum2,Xsoft1,
     &                xsoft2,Irej)
C  rejection?
      IF ( Irej.NE.0 ) THEN
         IF ( LPRi.GT.4 .AND. IDEb(74).GE.3 )
     &         WRITE (LO,'(1X,A,I6,2E12.4)')
     &         'PHO_HADSP3: rejection (IFLB,XS1,XMAX)' , Iflb , Xs1 , 
     &        Xmax
         IFAil(15) = IFAil(15) + 1
         Irej = 1
         RETURN
      END IF
C  debug output
      IF ( IDEb(74).GE.10 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I6,2E12.4)')
     &         'PHO_HADSP3: IFLB,XS1,XMAX' , Iflb , Xs1 , Xmax
         DO i = 1 , 3
            IF ( LPRi.GT.4 ) WRITE (LO,'(10X,I4,2E12.4)') i , Xsoft1(i)
     &           , xsoft2(i)
         END DO
      END IF
 
      END SUBROUTINE
