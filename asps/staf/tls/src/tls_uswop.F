      SUBROUTINE tls_USWOP (A,B,N)
C
C CERN PROGLIB# V303    USWOP           .VERSION KERNFOR  4.40  940929
C ORIG. 01/07/71 RKB
C
      INTEGER   A(*),B(*), X
C
      IF (N.EQ.0) RETURN
         DO 10 I=1,N
      X=A(I)
      A(I)=B(I)
   10 B(I)=X
      RETURN
      END
