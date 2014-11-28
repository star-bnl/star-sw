*
* $Id: nxtcrd.F,v 1.3 2009/08/12 14:42:24 jeromel Exp $
*
* $Log: nxtcrd.F,v $
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
* Revision 1.2  2001/02/28 22:56:16  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:30  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:45  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      SUBROUTINE NXTCRD
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER D/0/,K/0/,I/0/,J/0/,L/0/,M/0/,U/0/,T/0/,Y/0/,W/0/,LO/0/,
     *BF/0/,R2/0/,TL/0/,TF/0/,MF/0/,LL/0/,TFN/0/,TFM/0/,IBK/0/,KP/0/,
     *FCOM/0/,EXCL/0/,EXCLO/0/,QUOT/0/,WAIT/0/,CC/0/,COMMENT/0/,FORT/0/,
     *F1/0/,F2/0/,F3/0/,FS(8)/8*0/
      INTEGER NERD,LINF,LLONG
      INTEGER NR
      COMMON /LINELEN/ NR
      INTEGER LLIS,DDEB
      COMMON /MTRAC/ LLIS,DDEB
      CHARACTER VERSION*20,DVERS*4
      COMMON /MVERS/ VERSION,DVERS
      CHARACTER*8 CF/'   '/,FF/'f '/
      INTEGER MQUOT/48/
      O(62)=0
      IF(O(52).EQ.2) CALLMESAGE(16,1,0,0)
      IBK=ICHAR(' ')
      IF (O(55).EQ.MQUOT .AND. EXCL.GT.0 .AND. QUOT.GT.0) THEN
         IF (O(21).GT.0) PRINT *,' Parser warning: closing quota in comm
     *ent field, line ',O(19)
         O(12)=O(43)
         O(O(12))=MQUOT
         QUOT=0
         O(55)=47
         GOTO 99999
      END IF
20    IF (WAIT.EQ.0) THEN
         COMMENT=0
         IF (O(52).EQ.1) THEN
            O(52)=0
            O(9)=O(O(50))
            O(50)=O(50)-1
            CALLMESAGE(13,4,U,U)
            IF (O(9).EQ.0) THEN
               O(52)=2
               O(15)=1
               O(12)=O(43)
               O(O(12))=O(79)
               GOTO 99999
            END IF
         END IF
         CALLRW(0,O(9),405,O(38))
         IF (O(52).EQ.1) THEN
            O(12)=537
            O(237)=O(76)
            DO 31 T = 537,O(43)
               O(T)=47
31          CONTINUE
            IF (O(15).EQ.0) THEN
               IF (KP.GT.0 .AND. O(55).EQ.85) THEN
                  KP=KP-1
                  O(537)=86
               ELSE
                  O(537)=79
               END IF
               LL=0
               GOTO40
            END IF
            DVERS='0 '
            GOTO20
         END IF
         F1=O(969+O(405))
         F2=O(969+O(405+1))
         F3=O(969+O(405+2))
         FORT=0
         IF (O(43).GT.O(36)) FORT=INDEX(VERSION,FF(1:1))
         IF(INDEX(VERSION,DVERS(1:1)).EQ.0 .AND. F1.NE.50)GOTO20
C    fortran comment recognition here
         CC=0
         IF ( F1.EQ.83 .OR. F1.EQ.12 .OR. F1.EQ.54 ) CC=1
         IF (FORT.EQ.0) THEN
            IF (((10.LE.F2).AND.(F2.LE.35)) .AND. F2.NE.F1) CC=0
            IF (((52.LE.F2).AND.(F2.LE.77)) .AND. F2.NE.F1) CC=0
            IF (F1.EQ.45 .OR. F1.EQ.80 .OR. CC.EQ.1) THEN
C          if the line is recognised as a comment, terminate the previou
C         s line
               O(12)=537
               O(237)=O(76)
               DO 51 T = 538,O(43)
                  O(T)=47
51             CONTINUE
               O(537)=O(79)
               O(538)=O(79)
               WAIT=1
               COMMENT=1
               GOTO 99999
            END IF
         ELSE IF (F1.EQ.45 .OR. F1.EQ.80 .OR. CC.EQ.1) THEN
            COMMENT=1
         END IF
      END IF
      WAIT=0
C Shift line to D positions to generate ';'(TRM) automatically
      D=MAX(O(43)-O(36),0)
      IF (O(969+O(405)).EQ.50 .OR. O(15).GT.0 ) D=0
      DO 61 K = 537,537+D
         O(K)=47
61    CONTINUE
      BF=537-405+D
      R2=405
      TF=-1
      TFN=-1
      TFM=-1
      CALLVZERO(FS,8)
      CF='  '
      EXCLO=EXCL
      EXCL=0
      MF=0
      QUOT=0
      FCOM=0
C convert to internal code and get first four letters in capital (FS)
      DO 71 K = 405,O(38)
         T=O(969+O(K))
         O(K+BF)=T
         IF (O(K).NE.IBK) THEN
            R2=K
            MF=MIN(MF+1,8)
            CF(MF:MF)=CHAR(O(K))
            IF ((52.LE.T).AND.(T.LE.72)) T=T-52+10
            FS(MF)=T
            IF (T.EQ.83 .AND. COMMENT.EQ.0) EXCL=1
            IF (T.EQ.MQUOT) QUOT=QUOT+1
            IF (FCOM.GT.0 .AND. T.GT.77) THEN
               T=47
               O(K+BF)=T
            END IF
            IF (T.EQ.83 .AND. COMMENT.EQ.0 .AND. FORT.GT.0 .AND. MF.GT.
     *      1 .AND. QUOT.EQ.0) THEN
               FCOM=1
               T=82
               O(K+BF)=T
            END IF
            IF (MF.EQ.1 .AND. K-405.EQ.5) THEN
C         TF is the 6th symbol
               TF=T
               TFN=O(969+O(K+1))
            END IF
         END IF
71    CONTINUE
C+SEQN;
      O(12)=537
      O(237)=O(76)
      CALLCLTOU(CF)
      TFM=FS(2)
Cprevious last symbol
      LO=LL
C new last symbol
      TL=R2+BF
      LL=O(TL)
      IF (COMMENT.GT.0) THEN
C    pure comment line
         O(12)=O(43)
         LL=0
         IF(O(25).EQ.0)GOTO20
         GOTO40
      END IF
      IF (D.GT.0) THEN
C    Recognize complex continuation lines
C    first, remove the explicit continuation sign from the current line
         IF ( O(TL).EQ.37 .OR. O(TL).EQ.93 ) O(TL)=47
C    possible continuations: $_(,.=-+*/) ... <>|&^?\  (! CPP trap)
         O(537)=O(79)
C   FORTRAN-LIKE continuation (6th symbol): ,.=-+ >|& or <*/ preceeded b
C   y ,.=-+
C   F
         IF (FORT.EQ.0 .AND. ( TF.LT.10 .OR. TF.GT.35 )) THEN
            IF (((39.LE.TF).AND.(TF.LE.43)) .OR. (88.LE.TF).AND.(TF.LE.
     *      90)) THEN
               O(537)=47
               O(537+6)=47
C         F
            ELSE IF ((((44.LE.TF).AND.(TF.LE.45)).OR.TF.EQ.87) .AND. (
     *      39.LE.LO).AND.(LO.LE.45)) THEN
               O(537)=47
               O(537+6)=47
C         F
            ELSE IF ((((0.LE.TF).AND.(TF.LE.9)) .AND. (38.LE.LO).AND.(
     *      LO.LE.45)).AND.(TFN.EQ.47)) THEN
               O(537)=47
               O(537+6)=47
C         F
            ELSE IF ((((0.LE.TF).AND.(TF.LE.9)) .AND. (38.LE.TFM).AND.(
     *      TFM.LE.45)).AND.(TFN.EQ.47)) THEN
               O(537)=47
               O(537+6)=47
            END IF
C    special fortran rule
C   F
         END IF
         IF (FORT.GT.0 .AND. (TF.GT.0.OR.NR.EQ.0)) THEN
            O(537)=47
            O(537+6)=47
C    no & EXCLO==0 - != used in mortran
         END IF
C   MORTRAN continuation: line ends with =-+,_\; or starts with { or ELS
C   E after }
         IF (FORT.EQ.0 .AND. LO.GT.35 .OR.FS(1).EQ.78) THEN
            IF (((41.LE.LO).AND.(LO.LE.43)).OR.(LO.EQ.37.OR.LO.EQ.93.OR.
     *      LO.EQ.39.OR.LO.EQ.79.OR.FS(1).EQ.78).OR. (LO.EQ.84.AND.CF(1:
     *      4).EQ.'ELSE')) THEN
               O(537)=47
            END IF
C    Recognize 'free-stile' comment line starting with * not in a first
C   position
         END IF
         IF (( O(537).EQ.O(79) .OR. LO.EQ.O(79) ) .AND. FS(1).EQ.45)
     *   THEN
            O(538)=82
            O(TL+1)=82
            LL=47
         END IF
      END IF
      IF (O(537).EQ.50) THEN
         IF (O(538).EQ.14) THEN
            O(237)=1
            O(405)=O(769+O(76))
            O(405+1)=O(405)
            O(12)=538+1
         ELSE IF (O(538).EQ.50) THEN
            O(52)=1
C       FRT=1
            GOTO20
         ELSE IF ((10.LE.O(538)).AND.(O(538).LE.35)) THEN
            CALLCCCARD(O(538)-9)
            O(12)=O(43)
            O(O(12))=O(79)
         ELSE IF ((52.LE.O(538)).AND.(O(538).LE.77)) THEN
            CALLCCCARD(O(538)-51)
            O(12)=O(43)
            O(O(12))=O(79)
         END IF
         IF (O(52).EQ.1 .OR. O(538).EQ.22 .OR. O(538).EQ.15) THEN
            O(12)=O(43)
            O(O(12))=O(79)
            GOTO 99999
         END IF
      END IF
      IF (O(538).EQ.43) THEN
         IF (CF(1:5).EQ.'+KEEP' .OR. CF(1:5).EQ.'+DECK' .OR. CF(1:6).EQ.
     *   '+PATCH') THEN
            O(537)=79
            IF (O(55).EQ.85) THEN
               O(537)=86
               KP=MAX(KP-1,0)
            END IF
            IF (CF(1:5).EQ.'+KEEP') THEN
               IF (CF(1:6).EQ.'+KEEP,') THEN
                  O(538)=79
                  O(537+6)=85
                  O(O(36))=40
                  KP=KP+1
               ELSE
                  O(538)=82
               END IF
            END IF
         END IF
      END IF
40    IF (O(21).EQ.1) THEN
         DO 81 J = 238,254
            O(J)=O(76)
81       CONTINUE
         IF (O(30).EQ.1) THEN
            J=LLONG(238,O(19),10,0)
         ELSE IF (O(30).EQ.2) THEN
            DO 91 J = 1,8
               O(237+J)=O(O(43)+J)
91          CONTINUE
         END IF
         IF (O(68).NE.0) O(247)=33
         O(249)=O(55)
         J=LLONG(250,O(11),10,0)
         IF (O(55).EQ.MQUOT) PRINT *,' Parser Warning: literal expressio
     *n not closed in line '
         IF (O(53).EQ.0) THEN
            W=254
C      NMX
            DO 101 K = 1,R2-405+D
               W=W+1
               O(W)=O(536+K)
101         CONTINUE
            CALLRW(18,O(56),237,W)
         ELSE
            I=MOD(MAX0(O(53)*O(11),0),90)
            L=537
            M=O(43)
110         IF(O(L).NE.O(76).OR.L.EQ.M)GOTO111
               L=L+1
            GOTO 110
111         CONTINUE
120         IF(O(M).NE.O(76).OR.L.EQ.M)GOTO121
               M=M-1
            GOTO 120
121         CONTINUE
130         CONTINUE
               W=255
               DO 141 Y = 1,I
                  O(W)=O(76)
                  W=W+1
141            CONTINUE
150            IF(W.GT.368 .OR. L.GT.M)GOTO151
                  O(W)=O(L)
                  W=W+1
                  L=L+1
               GOTO 150
151            CONTINUE
               CALLRW(18,O(56),237,W-1)
               IF(L.GT.M)GOTO131
               DO 161 K = 237,255
                  O(K)=O(76)
161            CONTINUE
            GOTO 130
131         CONTINUE
         END IF
      END IF
      IF (O(15).EQ.1 .AND. O(537).NE.50 .AND. O(537).NE.83 .AND. O(537).
     *NE.79 .OR. COMMENT.EQ.1) THEN
         CALLRW(23,O(57),405,R2)
         GOTO20
      END IF
      IF (O(10).EQ.1) THEN
         NERD=LINF(537,537+(R2-405),10)
      END IF
99999 RETURN
      END
