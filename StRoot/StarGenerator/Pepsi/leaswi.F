

C ********************************************************************

      SUBROUTINE LEASWI(I,J)

C--       switch anticolour pointers for partons in rows I and J
C--       and colour pointers for the partons pointing back

      IMPLICIT NONE

C--   global variables
      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/


C--       local variables
      INTEGER I,J,KI5,KJ5

      KI5=K(I,5)
      KJ5=K(J,5)
      K(KI5,4)=J
      K(KJ5,4)=I
      K(I,5)=KJ5
      K(J,5)=KI5         

      END
