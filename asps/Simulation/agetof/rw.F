*
* $Id: rw.F,v 1.3 2009/08/12 14:42:24 jeromel Exp $
*
* $Log: rw.F,v $
* Revision 1.3  2009/08/12 14:42:24  jeromel
* ICHAR of expected size 1 was of size 2 to 3
*
* Revision 1.2  2004/01/25 01:36:12  fisyak
* Clean up unused variable to avoid waning by Victor request
*
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:17  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:34  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:46  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      SUBROUTINE RW(WICHIN,DFILE,FROM,TO)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER WICHIN,DFILE,FROM,TO,LENOCC,ROUND
      CHARACTER*155 INCH(5),OUCH,SHAD
      DATA INCH/'**','**','**','**','**'/
      INTEGER LINE
      COMMON /LINENUM/ LINE(100)
      INTEGER NR
      COMMON /LINELEN/ NR
      INTEGER I/0/,J/0/,K/0/,L/0/,P/0/,CX/0/,QC/0/,LAN/0/,STF/0/,LIL/0/,
     *LINL/0/,TABI/0/,IBK/0/,ICM/0/,IER/0/
      INTEGER WICH/0/,NOC/0/,IUNI/0/,LM(5)/5*0/,BUFI(200)/
     *200*0/
      CHARACTER*66 STARS
      DATA STARS/' *****************************************************
     ************'/
C STATEMENT FUN
      ROUND(K)=MOD(K,5)+1
C
      IBK=ICHAR(' ')
      LINL = MAX(TO-FROM+1,0)
      IF( WICHIN.NE.0) GOTO 20
      LIL=ROUND(LIL)
      LM(LIL)=LINL
      INCH(LIL)='   '
      IER=1
C+SELF,IF=UNIX.
      TABI=9
      READ(DFILE,'(A)', END=30) INCH(LIL)
C+SELF.
      IER=0
30    NR=0
      ICM=13
      IF ((0.LT.DFILE).AND.(DFILE.LT.100)) LINE(DFILE)=LINE(DFILE)+1
      O(19)=LINE(DFILE)
      K=FROM-1
      DO 41 P = 1,LM(LIL)
         IF(K.GE.TO)GOTO41
         K=K+1
         O(K)=ICHAR(INCH(LIL)(P:P))
         IF (O(K).NE.IBK) NR=NR+1
         IF (O(K).EQ.TABI) THEN
50          O(K)=IBK
            IF (K-FROM.LT.6) THEN
               K=K+1
               GOTO50
            END IF
C   IF (MOD(K-FROM,8)n0 & K<TO) { +K; jMORE; } }
         END IF
         IF (O(K) .LT.IBK) THEN
            O(K)=IBK
         END IF
41    CONTINUE
      DO 61 K = K+1,TO
         O(K)=IBK
61    CONTINUE
      IF (NR.EQ.0) O(52)=IER
      GOTO 99999
20    WICH=WICHIN
      IF ((12.LT.WICH).AND.(WICH.LT.19)) WICH=33
      CX = 0
CLAST NONBLANK ->
      LAN = 0
      NOC=0
      SHAD='  '
      DO 71 L = 1,LINL
         QC = O(FROM+L-1)
         BUFI(L)=QC
         IF (WICH.EQ.33) THEN
            IF (DFILE.EQ.O(57)) THEN
               IF (QC.EQ.O(105)) THEN
                  QC=O(86)
                  STF=1
               END IF
               IF (QC.EQ.O(106)) THEN
                  QC=O(86)
                  STF=0
               END IF
               IF (STF.EQ.0 .AND. BUFI(1).NE.12) THEN
                  IF ((51.LT.QC).AND.(QC.LT.78)) QC=QC-42
C             - output in fortran only in capital
C             - fortran comment - uncontroled
                  IF (QC.EQ.83) NOC=1
                  IF (QC.GT.49 .AND. NOC.EQ.0) THEN
                     CX=CX+1
                     SHAD(L:L)='| '
                  END IF
               END IF
            END IF
            QC=O(MIN(MAX(0,QC),96)+769)
         END IF
         OUCH(L:L)=CHAR( QC )
         IF (QC .NE. IBK) LAN=L
71    CONTINUE
C- Nev - 15/04/96 - include statement is produced as in C with .inc exte
Cntion
      IF (OUCH(1:10).EQ.'C include ') THEN
         OUCH(1:10) ='#include "'
         DO 81 L = 11,LAN
            IF(OUCH(L:L).EQ.'  ')GOTO90
81       CONTINUE
         L=LAN+1
90       OUCH(L:L+4)='.inc"'
         LAN=MAX(LAN,L+4)
         CALL CUTOL(OUCH(1:LAN))
C- NEV - 19/03/93 - switch listing off !
      END IF
      IF (DFILE .NE. O(56) .OR. O(21) .NE. 0) THEN
         I=1
         IUNI=ABS(DFILE)
         WRITE(IUNI,'(A)') OUCH(I:LAN)
      END IF
      IF (CX.GT.0) THEN
         WRITE (O(58),'(1X,A)')OUCH(1:LAN)
         WRITE (O(58),'(1X,A)')SHAD(1:LAN)
         WRITE (O(58),'('' *** Illegal FORTRAN characters *** '')')
         IUNI=ABS(O(57))
         O(17)=O(17)+1
         WRITE (IUNI, '(A)' )SHAD(1:LAN)
         WRITE (IUNI,'('' *** Illegal FORTRAN characters *** '')')
      END IF
      IF (WICHIN.EQ.14 .OR. WICHIN.EQ.15 .OR. CX.GT.0) THEN
         WRITE(O(58), '(/''  parser error detected in the following line
     *s of the source file:'')')
         WRITE(O(58),'(1X,A)') STARS
         DO 101 K = 1,5
            J = ABS ( ROUND(LIL+K-1) )
            L=LENOCC(INCH(J))
            WRITE(O(58),'(1X,I5,1X,A)') O(19)+K-5,INCH(J)(1:L)
101      CONTINUE
         WRITE(O(58),'(1X,A)') STARS
         WRITE(O(58),'(1X,A/)') OUCH(1:LAN)
         IUNI=ABS(O(57))
         WRITE(IUNI,'(A)') OUCH(1:LAN)
      END IF
99999 RETURN
      END
