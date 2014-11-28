C: definitions from agetof.def
*******************************************************************************
      SUBROUTINE AFNAME (DEF,EXTEN)
*                                                                             *
* Description:                                                                *
* Find an auxilary file. If extention starts with a dot, filename is composed *
* of the executable name plus extention, otherwise extention is the filename. *
* Search is done first in the CWD, then in the directory where the executable *
* has come from - all logic of system search (PATH+links) is repeated.        *
* Modifications:                                                              *
* PN, 28.08.01 - If the executable name has an extention, it is ignored       *
* PN, 01.11.01 - bug corrected in file.extenstion search                      *
* PN, 24.11.01 - when search fails, look one level up in CMT environment      *
*                                                                             *
*******************************************************************************
      IMPLICIT NONE
      CHARACTER*4096 PATH/' '/
      CHARACTER*256 PROG/' '/,FILE,DEF0,LINE,LINO
*       character*12    exten/'.def'/
      CHARACTER*(*) EXTEN,DEF
      CHARACTER*24 PATTERN/'#chto v imeni tebe moem#'/
      INTEGER LENOCC,INDEX,I,J,L,N,IDOT
      LOGICAL AFEXIST
      INTEGER ICSDEBUG
      COMMON /CSDEBUG/ ICSDEBUG
*
      CALL GETARG(0,PROG)
*       call getarg(2,exten)
* local copy of 'name.exten' or just 'exten'
      L=LENOCC(PROG)
      J=0
      IDOT=L+1
C *
C    Loop here
         DO 5011 I=1,L
C    *
C       Check (PROG(I:I)=='/')
            IF ((PROG(I:I).EQ.'/')) THEN
            J=I
            IDOT=L+1
         END IF
C    Check PROG(I:I)=='.'
         IF (PROG(I:I).EQ.'.') IDOT=I
5011  CONTINUE
5012  CONTINUE
      DEF=EXTEN
C Check EXTEN(1:1)='.'
      IF (EXTEN(1:1).EQ.'.') DEF=PROG(J+1:IDOT-1)//EXTEN
C Check ICSDEBUG>0
      IF (ICSDEBUG.GT.0) PRINT *,'==> looking for ',DEF(1:LENOCC(DEF))
C Check AFEXIST(DEF)
      IF (AFEXIST(DEF)) RETURN
      DEF0=DEF
* explicitly addressed or in the path
C  addressed explicitly
      FILE=PROG
C *
C    Check J==0
         IF (J.EQ.0) THEN
         CALL GETENV('PATH',PATH)
         N=LENOCC(PATH)
         PATH(N+1:N+1)=':'
C    For-Loop
         I=1
         GO TO 5023
C       *
5021        I=I+J
5023        IF (I.GT.N+1) GO TO 5022
            J=INDEX(PATH(I:),':')
            FILE=PATH(I:I+J-2)//'/'//PROG
C       Check ICSDEBUG>0
            IF (ICSDEBUG.GT.0) PRINT *,'  > testing file ',
     *      FILE(1:LENOCC(FILE))
C       Check AFEXIST(FILE)
            IF (AFEXIST(FILE))GO TO 5022
         GO TO 5021
5022     CONTINUE
      END IF
* executable found, find extention
C *
C    Check AFEXIST(FILE)
C    * TRUNCATE //
         IF (AFEXIST(FILE)) THEN
C    *
C       Loop here
5031        IF(INDEX(FILE,'//').LE.0) GO TO 5032
            LINE=FILE
            I=INDEX(LINE,'//')
            FILE=LINE(:I)//LINE(I+2:)
         GO TO 5031
5032     CONTINUE
C              * cutout executable extention
         L=LENOCC(FILE)
         J=0
         IDOT=L+1
C    *
C       Loop here
            DO 5041 I=1,L
C       *
C          Check (FILE(I:I)=='/')
               IF ((FILE(I:I).EQ.'/')) THEN
               J=I
               IDOT=L+1
            END IF
C       Check FILE(I:I)=='.'
            IF (FILE(I:I).EQ.'.') IDOT=I
C       bug corrected 24.11.01, PN
5041     CONTINUE
5042     CONTINUE
C    Check EXTEN(1:1)!='.'
         IF (EXTEN(1:1).NE.'.') IDOT=J+1
C              * same directory candidate
         DEF=FILE(1:IDOT-1)//EXTEN
C    Check ICSDEBUG>0
         IF (ICSDEBUG.GT.0) PRINT *,'  > testing file ',
     *   DEF(1:LENOCC(DEF))
C    Check AFEXIST(DEF)
         IF (AFEXIST(DEF)) RETURN
C              * upper level candidate
         DEF=FILE(1:J)//'../'//FILE(J+1:IDOT-1)//EXTEN
C    Check ICSDEBUG>0
         IF (ICSDEBUG.GT.0) PRINT *,'  > testing file ',
     *   DEF(1:LENOCC(DEF))
C    Check AFEXIST(DEF)
         IF (AFEXIST(DEF)) RETURN
* automatic extraction
         DEF=' '
C    *
C       Check (AFEXIST(FILE))
            IF ((AFEXIST(FILE))) THEN
            OPEN (98,FILE=FILE,STATUS='OLD',ERR=  5050)
C       Check ICSDEBUG>0
            IF (ICSDEBUG.GT.0) PRINT *,'  > opening file ',
     *      FILE(1:LENOCC(FILE))
C       *
C          Loop here
5061           CONTINUE
               LINO=LINE
               LINE=' '
               READ (98,'(A)',END=5050) LINE
C          *
C             Check (DEF!= ' ')
                  IF ((DEF.NE. ' ')) THEN
C             Check INDEX(LINE,'endfile')>0
                  IF (INDEX(LINE,'endfile').GT.0)GO TO 5062
                  WRITE (97,'(A)') LINE(1:LENOCC(LINE))
               END IF
C          *
C             Check (LINO==PATTERN & INDEX(LINE,EXTEN)>0)
                  IF ((LINO.EQ.PATTERN .AND. INDEX(LINE,EXTEN).GT.0))
     *            THEN
                  DEF=DEF0
                  OPEN (97,FILE=DEF0,STATUS='NEW',ERR=5050)
               END IF
            GO TO 5061
5062        CONTINUE
         END IF
5050     CLOSE(98)
         CLOSE(97)
         RETURN
      END IF
      DEF = ' '
C Check ICSDEBUG>0
      IF (ICSDEBUG.GT.0) PRINT *,' file not found '
* :ok:  if (ICSDEBUG>0) print *,' def = ',%L(def)
      END
