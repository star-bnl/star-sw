*
* $Id: kat.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: kat.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:14  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:08  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:43  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      SUBROUTINE KAT
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER B,E,R,S,V,U,W,X
      EQUIVALENCE (O(1),B),(O(2),E),(O(3),R),(O(4),S), (O(5),V),(O(6),U)
     *,(O(7),W),(O(8),X),(L,O(47)),(M,O(48))
      INTEGER L,M,LLONG,NIB
      INTEGER JMP/0/,T/0/,Z/0/,IBG/0/,IUNI/0/,NBA/0/
      IF(O(B).LT.19.OR.O(B).GT.30) CALLMESAGE(15, 7, B-2,E)
      B=B+1
      JMP=O(B-1)-18
      GOTO(20,30,40,50,60,70,80,90,100,110,120,130),JMP
20    T=O(B)+78
      X=X+1
      O(X)=O(T)
      GOTO 99999
30    CALLKRUNC(0)
      GOTO 99999
40    IF (O(B).EQ.12) THEN
         X=X+LLONG(X,O(L-O(B+1))+O(B+2),10,1)
         B=B+2
         GOTO 99999
      END IF
      IF (O(B).EQ.16) THEN
         O(16)=O(16)+10
         X=X+LLONG(X,O(16),10,0)
         GOTO 99999
      END IF
      IF (O(B).EQ.28) THEN
         B=B+1
         O(47)=O(47)+1
         O(O(47))=NIB(X,10)
         IF(O(47).GT.3824)CALLMESAGE(15, 21, 0,0)
         GOTO140
      END IF
      IF (O(B).EQ.30) THEN
         B=B+1
         IF(O(B).EQ.1)O(L-1)=O(L)
         L=L-1
         IF (L.LT.3775) THEN
            CALLMESAGE(14, 7, 0,0)
            L=L+1
         END IF
         GOTO150
      END IF
      CALLMESAGE(15, 7, B-2,E)
50    IF (O(B).EQ.28) THEN
         B=B+1
         O(48)=O(48)+1
         O(O(48))=O(B)
         IF(O(48).GT.3874)CALLMESAGE(15, 22, 0,0)
         GOTO 99999
      END IF
      IF (O(B).EQ.27) THEN
         B=B+1
         O(M)=O(B)
         GOTO 99999
      END IF
      IF (O(B).EQ.30) THEN
         X=X+1
         O(X)=O(M)
         M=M-1
         IF (M.LT.3825) THEN
            CALLMESAGE(14, 8, 0,0)
            M=M+1
         END IF
         GOTO 99999
      END IF
      CALLMESAGE(15, 7, B-2,E)
60    IBG=O(X)
      X=X-1+LLONG(X-1,IBG,10,1)
      GOTO 99999
70    CALLDEFINE(S)
      GOTO 99999
80    O(63)=1
      B=B+4
      O(67)=B+O(B-1)
      O(66)=B+O(B-2)
      O(65)=B+O(B-3)
      O(64)=B+O(B-4)
      B=B-1
      GOTO 99999
90    O(16)=O(16)+10
      O(47)=O(47)+1
      O(O(47))=O(16)
      IF(O(47).GT.3824)CALLMESAGE(15, 21, 0,0)
140   IF (O(B).EQ.1) THEN
         Z=O(L-1)
         O(L-1)=O(L)
         O(L)=Z
      END IF
150   IF(O(B).NE.1.AND.O(B).NE.0) STOP 18
      GOTO 99999
100   IUNI=O(B)
      REWIND IUNI
      GOTO 99999
120   IF(O(29).GE.1) CALLMACTRC(22,O(22),S)
      B=B-1
      GOTO 99999
110   T=O(B)
      B=B+1
      IF ((0.LT.O(B)).AND.(O(B).LT.5)) THEN
         T=T+26*O(B)
         B=B+1
      END IF
      NBA=0
      GOTO160
130   T=201+O(B)
      B=B+1
      NBA=O(X)
      X=X-1
160   IF (O(B).EQ.43) THEN
         O(T)=O(T)+1
         GOTO 99999
      END IF
      IF (O(B).EQ.42) THEN
         O(T)=O(T)-1
         GOTO 99999
      END IF
      IF (O(B).EQ.41) THEN
         O(T)=O(X)
         X=X-1
         GOTO 99999
      END IF
      IF (O(B).EQ.12) THEN
         X=X+1
         O(X)=O(T)
         IF (NBA.GT.1) X=X-1+LLONG(X-1,O(T),NBA,1)
         GOTO 99999
      END IF
      CALLMESAGE(15, 7, B-3,E)
99999 RETURN
      END
