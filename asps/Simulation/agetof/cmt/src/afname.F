*
* $Id: afname.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
* $Log: afname.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.1  2001/06/25 10:16:05  carnault
* fixing temporary fix for CMT only
*
* Revision 1.1  2001/02/28 22:56:10  nevski
* improved comment treatment, f77-structures etc
*
*
*******************************************************************************
      SUBROUTINE AFNAME (DEF,EXTEN)
*                                                                             *
* Find an auxilary file. If extention starts with a dot, filename is composed *
* of the executable name plus extention, otherwise extention is the filename. *
* Search is done first in the CWD, then in the directory where the executable *
* has come from - all logic of system search (PATH+links) is repeated.        *
*                                                                             *
*******************************************************************************
      IMPLICIT NONE 
      CHARACTER*4096 PATH/' '/
      CHARACTER*256 PROG/' '/,FILE,DEF0,LINE,LINO
      CHARACTER*(*) EXTEN,DEF
      CHARACTER*24 PATTERN/'AGE_UNIX_parser'/
      INTEGER LENOCC,INDEX,I,J,L,N

* management of possible suffixes to the agetof executable (CA)
      INTEGER IDOT

      LOGICAL AFEXIST
*
      CALL GETARG(0,PROG)
* local copy of 'name.exten' or just 'exten'

      L=LENOCC(PROG)
      J=0
      IDOT=0
C *
C    Loop here
         DO 5011 I=1,L
C    Check PROG(I:I)=='/'
         IF (PROG(I:I).EQ.'/') J=I
         IF (PROG(I:I).EQ.'.') IDOT=I
5011  CONTINUE
5012  CONTINUE
      DEF=EXTEN
      IF (IDOT.GT.0) L=IDOT-1
C Check EXTEN(1:1)='.'
      IF (EXTEN(1:1).EQ.'.') DEF=PROG(J+1:L)//EXTEN

C Check AFEXIST(DEF)
      IF (AFEXIST(DEF)) RETURN
      DEF0=DEF
* explicitly addressed or in the path:
C  addressed explicitly 
      FILE=PROG 
C    Check J==0
         IF (J.EQ.0) THEN
         CALL GETENV('PATH',PATH)
         N=LENOCC(PATH) 
         PATH(N+1:N+1)=':'
*       for (i=1; i<=N+1; i+=j)
         I=1 
C       Loop here
5021        CONTINUE
            I=I+J
C       Check I>N
            IF (I.GT.N)GO TO 5022
            J=INDEX(PATH(I:),':')
            FILE=PATH(I:I+J-2)//'/'//PROG
C       Check AFEXIST(FILE)
            IF (AFEXIST(FILE))GO TO 5022
         GO TO 5021
5022     CONTINUE
      END IF
* executable found, find extention
C    Check AFEXIST(FILE)
C    * TRUNCATE // 
         IF (AFEXIST(FILE)) THEN
C       Loop here 
5031        IF(INDEX(FILE,'//').LE.0) GO TO 5032
            LINE=FILE
            I=INDEX(LINE,'//')
            FILE=LINE(:I)//LINE(I+2:)
         GO TO 5031
5032     CONTINUE
         L=LENOCC(FILE)
         J=0
C    * 
C       Loop here
            DO 5041 I=1,L
C       Check FILE(I:I)=='/' 
            IF (FILE(I:I).EQ.'/') J=I
5041     CONTINUE 
5042     CONTINUE 
C    Check EXTEN(1:1)!='.'
         IF (EXTEN(1:1).NE.'.') L=J
         DEF=FILE(1:L)//EXTEN 
C    Check AFEXIST(DEF) 
         IF (AFEXIST(DEF)) RETURN
* automatic extraction
         DEF=' '
C       Check (AFEXIST(FILE))
            IF ((AFEXIST(FILE))) THEN 
            OPEN (98,FILE=FILE,STATUS='OLD',ERR=  5050)
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
      END 
