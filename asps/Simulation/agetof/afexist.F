C: definitions from Linux-gcc-dbg/agetof.def
*********************************************************
      FUNCTION AFEXIST(FILE)
      IMPLICIT NONE
      LOGICAL AFEXIST
      CHARACTER FILE*(*),TARGET*256
      INTEGER LSTAT,READLINK,LENOCC,STATB(13),ISTAT,L,K
      INTEGER ICSDEBUG
      COMMON /CSDEBUG/ ICSDEBUG
C *
C    Loop here
5011     CONTINUE
         INQUIRE(FILE=FILE(1:LENOCC(FILE)),EXIST=AFEXIST)
C    Check .NOT.AFEXIST
         IF (.NOT.AFEXIST) RETURN
         ISTAT = LSTAT(FILE(1:LENOCC(FILE)),STATB)
         ISTAT = IAND (STATB(3),130560)/512
C    Check ICSDEBUG>0
         IF (ICSDEBUG.GT.0) PRINT *,' ... stating ',
     *   FILE(1:LENOCC(FILE)),' istat=',ISTAT
         AFEXIST= ISTAT.EQ.64
C    Check ISTAT!=80
         IF (ISTAT.NE.80) RETURN
         K=LENOCC(FILE)
         L=READLINK(FILE(1:K),TARGET)
C    *
C       Loop here
            DO 5021 K=K,1,-1
C       Check FILE(K:K)=='/'
            IF (FILE(K:K).EQ.'/')GO TO 5022
5021     CONTINUE
5022     CONTINUE
C    *
C       Check (L>0 & K+L<256)
            IF ((L.GT.0 .AND. K+L.LT.256)) THEN
C       *
C          Check TARGET(1:1)=='/'
               IF (TARGET(1:1).EQ.'/') THEN
               FILE=TARGET(1:L)
            ELSE
               FILE(K+1:)=TARGET(1:L)
            END IF
         END IF
C    Check ICSDEBUG>0
         IF (ICSDEBUG.GT.0) PRINT *,' ... target  ',
     *   FILE(1:LENOCC(FILE))
      GO TO 5011
5012  CONTINUE
      END
