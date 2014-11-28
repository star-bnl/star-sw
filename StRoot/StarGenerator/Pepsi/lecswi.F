
C ********************************************************************

      SUBROUTINE LECSWI(I,J)

C--       switch colour pointers for partons in rows I and J 
C--       and anticolour pointers for the partons pointing back

      IMPLICIT NONE

C--       global variables
      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/


C--       local variables
      INTEGER I,J,KI4,KJ4

      KI4=K(I,4)
      KJ4=K(J,4)
      K(KI4,5)=J
      K(KJ4,5)=I
      K(I,4)=KJ4
      K(J,4)=KI4

      END
