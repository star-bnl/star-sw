* $Id: filter.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
* $Log: filter.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.1  2001/02/28 22:56:12  nevski
* improved comment treatment, f77-structures etc
*
C------------------------------------------------------------------
      SUBROUTINE FILTER
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      CHARACTER*256 FILE
      COMMON /NAMEF/ NN,FILE
      INTEGER NN,N,I,J,MAX/72/
      CHARACTER LINE*72,KEY*10
20    OPEN (O(9),FILE=FILE,STATUS='OLD', ERR=30)
30    OPEN (O(9),FILE=FILE(1:NN)//'.g',STATUS='OLD')
40    CONTINUE
50       READ (O(9),'(A)',END=60) LINE
         N=0
         DO 71 I = 1,MAX
            J=ICHAR(LINE(I:I))
C       find first letter R or S
            IF (J.GT.96) J=J-32
            IF (J.EQ.32) THEN
               N=N+1
            ELSE IF (J.EQ.9) THEN
               N=6
            ELSE IF ((80.LE.J).AND.(J.LE.83)) THEN
               GOTO80
            ELSE
               GOTO50
            END IF
71       CONTINUE
80       IF(N.LT.6)GOTO40
         N=0
C     record or structure ?
         DO 91 I = I,MAX
            J=ICHAR(LINE(I:I))
            IF (J.GT.96) J=J-32
            IF(J.EQ.32)GOTO91
            N=N+1
            IF(N.GT.10)GOTO50
            KEY(N:N)=CHAR(J)
            IF(N.EQ.8 .AND. KEY(1:8) .EQ.'POINTER(')GOTO100
            IF(N.EQ.7 .AND. KEY(1:7) .EQ.'RECORD/')GOTO100
            IF(N.EQ.10 .AND. KEY(1:10).EQ.'STRUCTURE/')GOTO100
91       CONTINUE
      GOTO 40
41    CONTINUE
60    STOP
100   CLOSE(O(9))
99999 RETURN
      END
