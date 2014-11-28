*
* $Id: linf.F,v 1.2 2004/01/25 01:36:12 fisyak Exp $
*
* $Log: linf.F,v $
* Revision 1.2  2004/01/25 01:36:12  fisyak
* Clean up unused variable to avoid waning by Victor request
*
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:15  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:16  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:44  cjo
* Importing agetof initial version
*
C-----------------------------------------------------------------------
      FUNCTION LINF(A,Z,IND)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER LINF,A,Z,IND,KIT/1/,LLONG,B0,SEQO/9999/
      INTEGER B/0/,C/0/,D/0/,H/0/,I/0/,J/0/,K/0/,KST/0/,NPL/0/,KUR/0/,
     *KUT/0/,LAST/0/,LIMC/0/,KOFF/0/
      LINF=A-1
      O(62)=0
      IF (O(A).EQ.O(96) .OR. (IND.EQ.12 .AND. (O(25).EQ.0.OR.O(10).EQ.1)
     *) ) GOTO 99999
      KST=0
      H=0
      B=A
      I=255
      KUT=KUR
      IF(O(B).NE.O(75))GOTO20
      B=B+2
      KIT=O(B-1)-1
      KUR=KUR+KIT
      KUT=KUR
      IF(KIT.NE.0)GOTO20
      KUT=KUT-1
      KIT=1
20    CONTINUE
      DO 31 J = 255,334
         O(J)=O(76)
31    CONTINUE
      IF (IND.EQ.15) THEN
40       IF(I.GE.261.OR.B.GT.Z)GOTO41
            IF (O(B).EQ.O(76)) THEN
               B=B+1
               GOTO40
            END IF
            IF ((0.LE.O(B)).AND.(O(B).LE.9)) THEN
               O(I)=O(B)
               I=I+1
               B=B+1
            ELSE
               GOTO41
            END IF
         GOTO 40
41       CONTINUE
         NPL=65
         D=45
         KOFF=7
      ELSE
         NPL=71
         D=12
         O(255)=D
         KOFF=MAX(MIN(80,O(39)),2)
      END IF
      IF (B.GT.Z) THEN
         IF (I.GT.255) THEN
            CALLRW(33,O(57),255,261)
            IF(IND.EQ.15)CALLMESAGE(14, 14, 0,0)
            GOTO 99999
         END IF
      END IF
      C=254+KOFF+KUT*O(18)
      IF(KUT.GT.LAST)C=C-O(18)
      IF(C.GT.254+50)C=254+7
      I=C
C non-empty output line
      IF (SEQO.GT.O(19)) SEQO=O(19)-1
50    IF(B.GT.Z)GOTO51
         B0=B
         LIMC=MIN0(B+NPL,Z)
60       IF(B.GT.LIMC .OR. I.GT.326)GOTO61
            IF (IND.EQ.15 .AND. O(B).EQ.O(86)) THEN
               IF(I.GE.326)GOTO61
               O(I)=O(B)
               I=I+1
            END IF
            IF (O(B).EQ.O(70)) THEN
               B=B+1
               GOTO61
            END IF
            O(I)=O(B)
            I=I+1
            B=B+1
         GOTO 60
61       CONTINUE
         IF (O(30).EQ.2) THEN
            DO 71 K = 1,8
               O(334-K+1)=O(O(36)-K+1)
71          CONTINUE
         ELSE IF (O(30).EQ.1) THEN
            K=LLONG(327,SEQO-1,10,0)
         END IF
         DO 81 K = 255,326
            IF(O(K).EQ.O(105)) KST=1
            IF(O(K).EQ.O(106)) KST=0
81       CONTINUE
C    BREAK LINES ONLY AT <NICE> PLACES
C       IF I>W72 & IND=='F' & KST == 0  { WHILE .(I-1)<38  {-B;-I;.I=BLA
C   ;} } }
         IF (I.GT.326 .AND. IND.EQ.15 .AND. KST.EQ.0) THEN
C       break line in a <nice> place J positions back
            J=0
90          IF(O(I-J-1).EQ.47.OR.O(I-J-1).EQ.39.OR.O(I-J-1).EQ.44.OR.O(
     *      I-J-1).EQ.41)GOTO91
               J=J+1
            GOTO 90
91          CONTINUE
            IF (B-J.LE.B0) THEN
               J=0
100            IF(O(I-J-1).GE.38)GOTO101
                  J=J+1
               GOTO 100
101            CONTINUE
            END IF
            IF (B-J.GT.B0) THEN
110            IF(J.LE.0)GOTO111
                  J=J-1
                  B=B-1
                  I=I-1
                  O(I)=O(76)
               GOTO 110
111            CONTINUE
            END IF
C       but it can not go back too far
         END IF
C   LOOKOUT
         CALLRW(33,O(57),255,334)
         DO 121 K = 255,334
            O(K)=O(76)
121      CONTINUE
         I=C
         IF (IND.EQ.15) THEN
            O(255+5)=D
         ELSE
            O(255)=D
         END IF
         IF (KST.GT.0) THEN
            I=261
            O(261-1)=45
         END IF
      GOTO 50
51    CONTINUE
      SEQO=O(19)
      IF(IND.EQ.12) O(A)=O(96)
      LAST=KUR
99999 RETURN
      END
