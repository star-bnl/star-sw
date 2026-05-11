
      SUBROUTINE DT_RNDMOU(Uout,Cout,Cdout,Cmout,Iout,Jout)
 
      IMPLICIT NONE
      DOUBLE PRECISION Cdout , Cmout , Cout , Uout
      INTEGER Iout , Jout , kkk
      SAVE 
 
C random number generator
      INCLUDE 'inc/dtrand'
 
      DIMENSION Uout(97)
 
      DO kkk = 1 , 97
         Uout(kkk) = U(kkk)
      END DO
      Cout = C
      Cdout = CD
      Cmout = CM
      Iout = I
      Jout = J
 
      END SUBROUTINE
