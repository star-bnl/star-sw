
      SUBROUTINE DT_RNDMIN(Uin,Cin,Cdin,Cmin,Iin,Jin)
 
      IMPLICIT NONE
      DOUBLE PRECISION Cdin , Cin , Cmin , Uin
      INTEGER Iin , Jin , kkk
      SAVE 
 
C random number generator
      INCLUDE 'inc/dtrand'
 
      DIMENSION Uin(97)
 
      DO kkk = 1 , 97
         U(kkk) = Uin(kkk)
      END DO
      C = Cin
      CD = Cdin
      CM = Cmin
      I = Iin
      J = Jin
 
      END SUBROUTINE
