
      SUBROUTINE PHO_MKSLTR(P1,P2,Gam,Gamb)
C********************************************************************
C
C     calculate successive Lorentz boots for arbitrary Lorentz trans.
C
C     input:   P1                initial 4 vector
C              GAM(3),GAMB(3)    Lorentz boost parameters
C
C     output:  P2                final  4 vector
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Gam , Gamb , P1 , P2
      INTEGER i
      SAVE 
 
      DIMENSION P1(4) , P2(4) , Gam(3) , Gamb(3)
 
      P2(4) = P1(4)
      DO i = 1 , 3
         P2(i) = Gam(i)*P1(i) + Gamb(i)*P2(4)
         P2(4) = Gam(i)*P2(4) + Gamb(i)*P1(i)
      END DO
      END SUBROUTINE
