*
* $Id: expand.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: expand.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:12  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:04  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:42  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      SUBROUTINE EXPAND (F)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
C F is the number of the macro arguments
      INTEGER B,E,R,S,V,U,W,X
      EQUIVALENCE (O(1),B),(O(2),E),(O(3),R),(O(4),S), (O(5),V),(O(6),U)
     *,(O(7),W),(O(8),X)
      INTEGER F,NIB,USER,LEXP,LLONG
      INTEGER A/0/,T/0/,K/0/,J/0/,N/0/,P/0/,Q/0/,Y/0/,Z/0/,JV/0/,KRAQ/0/
     *,KRAP/0/,JMP/0/,NUL/0/,KGN/0/,SEF/0/
Cif nested decrement nest, recover environment
      T=0
      IF (F.LT.0) THEN
         O(23)=O(23)-1
         IF(O(23).EQ.0) O(15)=SEF
         DO 21 K = 1,6
            O(K)=O(6+U+1-K)
21       CONTINUE
         J=O(O(O(51)))
         IF ((10.LE.J).AND.(J.LE.12 .OR. J.EQ.30)) THEN
            IF (J.EQ.10) THEN
               JV=NIB(X,16)
C         convert = ex number
            ELSE
               IF (J.EQ.30) THEN
                  JV=USER(W+1,X)
               ELSE
                  JV=LEXP(W+1,X)
               END IF
C      compute
            END IF
            IF (O(63).EQ.0) THEN
               IF (J.EQ.12) THEN
                  X=W+LLONG(W,JV,10,0)
C            if pending {IF} if 'c' convert to decimal
               ELSE
                  IF (J.NE.30) THEN
                     X=W+1
                     O(X)=JV
                  END IF
               END IF
C                                leave binary
            ELSE
               X=W
               O(63)=O(63)-1
               IF (JV.EQ.0) THEN
                  P=O(65)
                  Q=O(64)
C               otherwise           false pointers
               ELSE
                  P=O(67)
                  Q=O(66)
               END IF
C                                true pointers
               O(44)=O(44)+1
               O(O(44))=O(64)+1
               O(44)=O(44)+1
               O(O(44))=E
               IF(O(44).GT.3674)CALLMESAGE(15, 18, 0,0)
               B=P
               E=Q
C          stack the pointers on I stack
            END IF
         END IF
C    pop Y stack for for base and w
         NUL=O(O(51))
         O(51)=O(51)-1
         W=O(O(51))
         O(51)=O(51)-1
         O(32)=O(O(51))
         O(51)=O(51)-1
C    not nested. find and  print Macro COMMENTS - commented out by PN
      ELSE
C    C=S; UNTIL C>V { IF .C==LDQ { NERD=LINF(C+2,C+.(C+1)+1,'C'); C=C+.(
C   C+1)+2;} +C; }
C    IF (MTRC>0) PRINT *,' F,S,V=',F,S,V;
C    save no.args, set cielX, get rep ptrs
         O(S-1)=F
         O(40)=S-2*F-3
         R=O(O(22)+2)
         B=R+6
         E=O(R+3)
C    increment count, if too big turn on trace
         R=O(R)
         O(62)=O(62)+1
         IF (O(62).GT.1000) THEN
            CALLMESAGE(14, 4, 0,0)
            O(29)=2
         END IF
         IF(O(29).GE.2) CALLMACTRC(22,O(22),S)
         IF (O(23).EQ.0) X=W
C   if ind print MATCHED...
C  IF(MTRCg1) PRINT *,'      begin main replacement loop ,B,E=',B,E;
Cbegin main replacement loop
      END IF
30    CONTINUE
C    IF(MTRCg1) PRINT *,' ===> ',CHAR(.(.B+MNOSET));
40       IF(B.GE.E)GOTO41
            IF (O(B).LT.O(107) .OR. O(B).EQ.O(113)) THEN
               X=X+1
               IF(X.GT.O(40)) CALLMESAGE(15, 3, 0,0)
               O(X)=O(B)
               B=B+1
               GOTO40
            END IF
            B=B+1
C       IF(MTRCg1) PRINT *,'  ==> ',CHAR(.(.B+MNOSET));
            JMP=O(B-1)-O(106)
            GOTO(50,60,70,80),JMP
60          IF ((1.LE.O(B)).AND.(O(B).LE.O(S-1))) THEN
               A=O(S-2*(O(B))-1)
               N=O(S-2*(O(B))-2)
90             IF(A.GE.N)GOTO91
                  X=X+1
                  IF(X.GT.O(40)) CALLMESAGE(15, 3, 0,0)
                  O(X)=O(A)
                  A=A+1
               GOTO 90
91             CONTINUE
               GOTO100
            END IF
            CALLMESAGE(15, 4, B,E)
50          IF (O(B).EQ.12) THEN
C          IF(MTRCg0) PRINT *,'   => ',CHAR(.(.B+MNOSET));
               B=B+1
               IF (O(B).EQ.23) THEN
                  IF (O(68).EQ.0) THEN
                     KGN=1
                     O(68)=1
                  ELSE
                     KGN=KGN+1
                  END IF
               ELSE IF (O(B).EQ.16) THEN
                  IF (O(68).EQ.1) THEN
                     KGN=KGN+1
                  END IF
               ELSE IF (O(B).EQ.14) THEN
                  KGN=KGN-1
                  IF(KGN.LE.0) O(68)=0
               END IF
               IF (O(71).NE.0) THEN
                  IF(O(68).EQ.0) O(32)=1705
                  IF(O(68).NE.0)O(32)=1225
               END IF
               GOTO100
            END IF
            IF(O(68).EQ.0)CALLKAT
            GOTO100
70          IF (O(B).EQ.O(107)) THEN
C          IF(MTRCg1) PRINT *,'   => ',CHAR(.(.B+MNOSET));
               B=B+1
               IF (O(B).EQ.16) THEN
                  B=B+1
                  IF (1.GT.O(B).OR.O(B).GT.O(S-1) ) CALLMESAGE(15, 4, B,
     *            E)
                  X=X+LLONG(X,O(S-2*(O(B))-2)-O(S-2*(O(B))-1),10,0)
                  IF(X.GT.O(40))CALLMESAGE(15, 3, 0,0)
                  B=B+2
                  GOTO40
               END IF
               IF (O(B).EQ.13) THEN
                  B=B+1
                  T=1
110               CONTINUE
                     IF( O(109).EQ.O(B)) T=T+1
                     IF( O(110).EQ.O(B)) T=T-1
                     IF(T.EQ.0)GOTO111
                     X=X+1
                     IF(X.GT.O(40)) CALLMESAGE(15, 3, 0,0)
                     O(X)=O(B)
                     B=B+1
                  GOTO 110
111               CONTINUE
                  GOTO100
               END IF
               IF (O(B).EQ.15) THEN
                  X=X+1
                  O(X)=O(97)
                  X=X+1
                  KRAP=X
                  KRAQ=O(23)
                  GOTO100
               END IF
            END IF
            O(23)=O(23)+1
            IF(O(23).EQ.1)SEF=O(15)
            O(15)=1
            O(51)=O(51)+1
            O(O(51))=O(32)
            O(51)=O(51)+1
            O(O(51))=W
            O(51)=O(51)+1
            O(O(51))=B
            IF(O(51).GT.4074)CALLMESAGE(15, 34, 0,0)
            W=X
            GOTO100
80          IF (KRAP.NE.0.AND.KRAQ.EQ.O(23)) THEN
               X=X+1
               O(X)=O(96)
               O(KRAP)=X-KRAP-1
               KRAP=0
               GOTO40
            END IF
            DO 121 K = 1,6
               O(O(40)-K)=O(K)
121         CONTINUE
            U=O(40)-7
            V=U+1
            Z=O(O(51))
            Y=O(Z)
            IF(O(Z+2).EQ.O(105)) Y=0
            IF((9.LT.Y).AND.(Y.LT.16.OR.Y.EQ.30)) Y=O(31)
            IF(0.GT.Y.OR.Y.GE.8) CALLMESAGE(15, 17, B,E)
            O(32)=Y*120+1225
            GOTO41
100         B=B+1
         GOTO 40
41       CONTINUE
         IF(O(23).GT.0)GOTO31
         IF ((O(44).EQ.3625)) THEN
            IF(R.LE.0)GOTO31
            B=R+6
            E=O(R+3)
            R=O(R)
         ELSE
            E=O(O(44))
            O(44)=O(44)-1
            B=O(O(44))
            O(44)=O(44)-1
         END IF
      GOTO 30
31    CONTINUE
C IF(MTRCg1) PRINT *,' end main replacement loop  ,B,E=',B,E;
      IF(O(29).GE.2) CALLMACTRC(33,W,X)
130   IF(X.LE.W)GOTO131
         V=V-1
         O(V)=O(X)
         X=X-1
      GOTO 130
131   CONTINUE
      S=V
99999 RETURN
      END
