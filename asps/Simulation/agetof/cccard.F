*
* $Id: cccard.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: cccard.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:10  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:44:56  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:42  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      SUBROUTINE CCCARD(K)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER K,N,N5
      INTEGER I/0/,M/0/,R/0/,JMP/0/,K0/0/,K1/0/,K2/0/
      CHARACTER EXT*2, CL*120, C0*1, C1*1, CC*2
      CHARACTER*256 FILE,PILE
      COMMON /NAMEF/ N,FILE
      INTEGER LLIS,DDEB
      COMMON /MTRAC/ LLIS,DDEB
      CHARACTER VERSION*20,DVERS*4
      COMMON /MVERS/ VERSION,DVERS
      JMP=K
      GOTO(20,30,40,50,60,70,60,60,80,60,60,90,100,110,60,60,120,130,
     *140,150,160,170,60,60,60,60),JMP
20    O(10) =N5(0,2,1)
      GOTO 99999
30    O(37) =N5(0,132,132)
      O(38)=404+O(37)
      O(36)=536+O(37)
      GOTO 99999
40    O(41)=N5(10,132,132)
      O(43)=536+O(41)
      O(42)=404+O(41)
      GOTO 99999
50    O(13)=N5(0,2,0)
      GOTO 99999
70    O(15) =1
      GOTO 99999
80    IF (O(537+2).LT.10) THEN
         O(53)=N5(0,50,2)
         GOTO 99999
      END IF
      CL='   '
      C1='   '
      K0=0
      K1=0
      K2=0
      R=405
      DO 181 R = 405,O(38)
         C0=C1
         C1=CHAR(O(R))
         IF (C1.EQ.'; ') GOTO 190
         K0=K0+1
         CL(K0:K0)=C1
         IF (C0.EQ.'  ' .AND. C1.NE. '  ') K1=K0
         IF (C0.NE. '  ' .AND. C1.EQ.'  ') K2=K0
181   CONTINUE
190   IF (K2.EQ.0 .OR. K1.EQ.0 .OR. K1.GT.K2) GOTO 200
      CC=CL(K1:K1)//CL(K2-1:K2-1)
      IF (CC.EQ.''''''.OR.CC.EQ.'""'.OR.CC.EQ.'<>'.OR.CC.EQ.'()'.OR.CC.
     *EQ.'[]'.OR.CC.EQ.'{}') THEN
         K1=K1+1
         K2=K2-1
      END IF
      PILE=CL(K1:K2-1)
      M=K2-K1
      O(50)=O(50)+1
      O(O(50))=O(9)
      IF(O(50).GT.4024)CALLMESAGE(15, 30, 0,0)
      O(9)=O(9)+1
210   OPEN (O(9),FILE=PILE(1:M) , STATUS='OLD',FORM='FORMATTED',ERR=220)
      GOTO 99999
220   OPEN (O(9),FILE=PILE(1:M)//'.g' , STATUS='OLD',FORM='FORMATTED',
     *ERR=230)
      GOTO 99999
230   OPEN (O(9),FILE=PILE(1:M)//'.h' , STATUS='OLD',FORM='FORMATTED',
     *ERR=240)
      GOTO 99999
240   OPEN (O(9),FILE=PILE(1:M)//'.inc', STATUS='OLD',FORM='FORMATTED',
     *ERR=250)
      GOTO 99999
250   IF (PILE(1:1).EQ.'/ ') GOTO 200
260   OPEN (O(9),FILE='../inc/'//PILE(1:M),STATUS='OLD',FORM='FORMATTED'
     *,ERR=270)
      GOTO 99999
270   OPEN (O(9),FILE='../inc/'//PILE(1:M)//'.g', STATUS='OLD',FORM='FOR
     *MATTED',ERR=280)
      GOTO 99999
280   OPEN (O(9),FILE='../inc/'//PILE(1:M)//'.h', STATUS='OLD',FORM='FOR
     *MATTED',ERR=290)
      GOTO 99999
290   OPEN (O(9),FILE='../inc/'//PILE(1:M)//'.inc',STATUS='OLD',FORM='FO
     *RMATTED',ERR=200)
      GOTO 99999
200   IF (CC.NE.'[]') WRITE(O(58),*) '*WARNING! INCLUDE FILE NOT FOUND:
     *',CL(1:K0)
      O(9)=O(O(50))
      O(50)=O(50)-1
C (WARN I T1,TMX);
      GOTO 99999
90    O(21)=1
      GOTO 99999
100   O(15) =0
      GOTO 99999
110   O(21)=0
      GOTO 99999
120   O(26)=N5(0,1,1)
      GOTO 99999
130   I =N5(1,99,8)
      REWIND I
      GOTO 99999
140   O(30)=N5(0,2,0)
      GOTO 99999
150   O(29)=N5(0,9,0)
      GOTO 99999
160   O(50)=O(50)+1
      O(O(50))=O(9)
      IF(O(50).GT.4024)CALLMESAGE(15, 30, 0,0)
      O(9)=N5(1,99,O(9))
      IF (O(9).EQ.7 .AND. LLIS.GT.0) O(21)=1
C+SELF,IF=UNIX.
      EXT='.a'
      EXT(2:2)=CHAR(ICHAR(EXT(2:2))+O(9)-1)
300   OPEN (O(9),FILE=FILE,STATUS='OLD',ERR=310)
      GOTO 99999
310   OPEN (O(9),FILE=FILE(1:N)//EXT ,STATUS='OLD')
C+SELF.
      GOTO 99999
170   DVERS=CHAR(O(405+2))
CDVERS=N5(0,9,0)
      GOTO 99999
60    CALLMESAGE(14, 22, 0,0)
99999 RETURN
      END
