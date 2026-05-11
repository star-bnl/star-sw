
      SUBROUTINE PHO_HADSP2(Iflb,Xs1,Xmax,Xsoft1,Irej)
C***********************************************************************
C
C     split hadron momentum XMAX into two partons using
C     lower cut-off: AS
C
C     input:   IFLB    compressed particle code of particle to split
C              XS1     sum of x values already selected
C              XMAX    maximal x possible
C
C     output:  XS1     new sum of x values (without first one)
C              XSOFT1  field of selected x values
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DEPS , PHO_RNDBET , pvbar1 , pvbar2 , pvmes1 , 
     &                 pvmes2 , Xmax , xpot1 , xpot2 , xrest , Xs1 , 
     &                 Xsoft1 , xss1 , zz
      INTEGER Iflb , IPHO_BAR3 , Irej , iter , itmax
      SAVE 
 
      PARAMETER (DEPS=1.D-8)
 
      DIMENSION Xsoft1(50)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  internal rejection counters
      INCLUDE 'inc/poloop'
C  data on most recent hard scattering
      INCLUDE 'inc/pockin'
 
C  model exponents
      DATA pvmes1/ - 0.5D0/
      DATA pvmes2/ - 0.5D0/
      DATA pvbar1/1.5D0/
      DATA pvbar2/ - 0.5D0/
C
      Irej = 0
      itmax = 100
C
C  mesonic particle
      IF ( IPHO_BAR3(Iflb,0).EQ.0 ) THEN
         xpot1 = pvmes1 + 1.D0
         xpot2 = pvmes2 + 1.D0
C  baryonic particle
      ELSE
         xpot1 = pvbar1 + 1.D0
         xpot2 = pvbar2 + 1.D0
      END IF
      iter = 0
      xrest = 1.D0 - Xs1
C  selection loop
 100  iter = iter + 1
      IF ( iter.GE.itmax ) THEN
         IF ( IDEb(39).GE.3 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I8)')
     &            'PHO_HADSP2: REJECTION (ITER)' , iter
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,3E12.3)') 'XS1,XMAX,AS:' , 
     &           Xs1 , Xmax , AS
         END IF
         IFAil(14) = IFAil(14) + 1
         Irej = 1
         RETURN
      END IF
      zz = xrest*PHO_RNDBET(xpot2,xpot1)
      IF ( (zz.GT.Xmax) .OR. (zz.LT.AS) ) GOTO 100
      xss1 = Xs1 + zz
      IF ( (1.D0-xss1).LT.AS ) GOTO 100
C
      Xs1 = xss1
      Xsoft1(1) = 1.D0 - xss1
      Xsoft1(2) = zz
C  debug output
      IF ( IDEb(39).GE.10 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I8)')
     &         'PHO_HADSP2: ITMAX,ITER' , itmax , iter
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,3E10.3,5X,2E11.4)')
     &         'XS1,XMAX,AS  X1,X2:' , Xs1 , Xmax , AS , Xsoft1(1) , 
     &        Xsoft1(2)
      END IF
      END SUBROUTINE
