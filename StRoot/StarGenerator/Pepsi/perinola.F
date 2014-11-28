*72*********************************************************************
      DOUBLE PRECISION FUNCTION PERINOLA(ARG,TABLE)
       IMPLICIT NONE

      
      COMMON/ XARRAY / ENT
      DOUBLE PRECISION ENT(102)
      
      INTEGER NENT(2),NCOMB(7),IENT(7),NARG,KD,M,JA,I,JB,J,JR,IADR,IFADR
      INTEGER IL,K
      DOUBLE PRECISION ARG(2),TABLE(*),D(7),FAC

      NARG=2
      NENT(1)=76
      NENT(2)=26
      KD=1
      M=1
      JA=1
      DO 5 I=1,NARG
         NCOMB(I)=1
         JB=JA-1+NENT(I)
         DO 2 J=JA,JB
            IF (ARG(I).LE.ENT(J)) GO TO 3
 2       CONTINUE
         J=JB
 3       IF (J.NE.JA) GO TO 4
         J=J+1
 4       JR=J-1
         D(I)=(ENT(J)-ARG(I))/(ENT(J)-ENT(JR))
         IENT(I)=J-JA
         KD=KD+IENT(I)*M
         M=M*NENT(I)
 5    JA=JB+1
      PERINOLA=0.D0
 10   FAC=1.D0
      IADR=KD
      IFADR=1
      DO 15 I=1,NARG
         IF (NCOMB(I).EQ.0) GO TO 12
         FAC=FAC*(1.D0-D(I))
         GO TO 15
 12      FAC=FAC*D(I)
         IADR=IADR-IFADR
 15   IFADR=IFADR*NENT(I)
      PERINOLA=PERINOLA+FAC*TABLE(IADR)
      IL=NARG
 40   IF (NCOMB(IL).EQ.0) GO TO 80
      NCOMB(IL)=0
      IF (IL.EQ.NARG) GO TO 10
      IL=IL+1
      DO 50  K=IL,NARG
 50   NCOMB(K)=1
      GO TO 10
 80   IL=IL-1
      IF(IL.NE.0) GO TO 40
      RETURN
      END
