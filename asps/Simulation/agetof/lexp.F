*
* $Id: lexp.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: lexp.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:14  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:12  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:44  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      FUNCTION LEXP(A,Z)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER LEXP,A,Z,ISPEC
      INTEGER B/0/,C/0/,D/0/,E/0/,N ,K/0/,T/0/,X/0/,NQ/0/,MP/0/,MQ/0/,
     *MV/0/,MU/0/,MNN/0/,MXC/0/,JMP/0/,LONE/0/,NFLG/0/
      INTEGER P(100)
      EQUIVALENCE(N,O(49)),( P(1),O(4075) )
      IF (O(A+1).EQ.O(105)) THEN
         LEXP=1
         B=A+1
         C=B+1
         NQ=1
         MXC=Z
20       CONTINUE
            IF( O(105).EQ.O(C)) NQ=NQ+1
            IF( O(106).EQ.O(C)) NQ=NQ-1
            IF(NQ.EQ.0)GOTO21
            C=C+1
            IF(C.GT.MXC) CALLMESAGE(15, 35, 0,0)
         GOTO 20
21       CONTINUE
         D=C+1
         E=D+1
         IF (O(E).NE.O(105)) THEN
            LONE=1
            MP=E
            MQ=Z
            MV=B+1
            MU=C
            IF (ISPEC(MP,MQ,MV,MU,LONE).NE.3) LEXP=0
         ELSE
30          IF(E.GT.Z-1)GOTO31
               IF(O(B).NE.O(E)) LEXP=0
               B=B+1
               E=E+1
            GOTO 30
31          CONTINUE
            IF(B.NE.D) LEXP=0
         END IF
         IF(O(D).EQ.91) LEXP=1-LEXP
         GOTO 99999
      END IF
      LEXP=0
      NFLG=0
      N=3875
      MNN=N+2
      O(N-1)=38
      O(N)=0
      B=A
40    IF(B.GT.Z)GOTO41
         IF ((0.LE.O(B)).AND.(O(B).LE.9)) THEN
            O(N)=0
            NFLG=1
50          IF(0.GT.O(B).OR.O(B).GT.9)GOTO51
               O(N)=10*O(N)+O(B)
               B=B+1
            GOTO 50
51          CONTINUE
         ELSE IF (O(B).EQ.O(76)) THEN
            B=B+1
         ELSE
            T=B
            IF ((10.LE.O(T)).AND.(O(T).LE.29)) THEN
               X=O(T)*O(T+1)
               T=T+2
               IF (O(T).EQ.29.OR.O(T).EQ.13) THEN
                  X=X+O(T)
                  T=T+1
               END IF
               X=X/20
               IF((10.LT.X).AND.(X.LT.33))GOTO60
               GOTO70
            END IF
            X=O(T)
            T=T+1
            IF (X.EQ.45.AND.O(T).EQ.45) THEN
               T=T+1
               X=36
            ELSE IF (X.EQ.91) THEN
               X=29
               IF (O(T).EQ.41) THEN
                  T=T+1
                  X=16
               END IF
            ELSE IF (X.EQ.87) THEN
               X=30
               IF (O(T).EQ.41) THEN
                  T=T+1
                  X=14
               END IF
            ELSE IF (X.EQ.41) THEN
               X=18
               IF (O(T).EQ.88) THEN
                  T=T+1
                  X=11
               END IF
            ELSE IF (X.EQ.88) THEN
               X=23
               IF (O(T).EQ.41) THEN
                  T=T+1
                  X=11
               END IF
            ELSE IF (X.EQ.(89)) THEN
               X=32
            ELSE IF (X.EQ.(90)) THEN
               X=13
            END IF
            IF(P(X).EQ.0)GOTO70
60          IF(X.LT.10.OR.X.GT.O(81) .OR. (X.EQ.O(81).AND.NFLG.EQ.0))
     *      GOTO70
            IF (P(X)/100.GT.P(O(N-1))/100.OR.X.EQ.O(80)) THEN
               NFLG=0
               O(49)=O(49)+1
               O(O(49))=X
               O(49)=O(49)+1
               O(O(49))=0
               IF(O(49).GT.3974)CALLMESAGE(15, 23, 0,0)
               B=T
               GOTO40
            END IF
            IF (N.EQ.MNN) THEN
               LEXP=O(N)
               GOTO 99999
            END IF
            K=P(O(N-1))
            NFLG=1
            JMP=K-((K/100)*100)-1
            GOTO(80,90,100,110,120,130,140,150,160,170,180,190,200,210,
     *      220),JMP
140         IF (O(N).EQ.0) THEN
               CALLMESAGE(14, 10, 0,0)
               GOTO70
            END IF
            O(N-2) = O(N-2) / O(N)
            GOTO230
150         O(N-2) = O(N-2) * O(N)
            GOTO230
130         O(N-2) = O(N-2) + O(N)
            GOTO230
120         O(N-2) = O(N-2) - O(N)
            GOTO230
160         O(N-2) = O(N-2) **O(N)
            GOTO230
80          O(N-2) = O(N)
            B=T
            GOTO230
110         O(N-2) = 1-O(N)
            GOTO230
190         IF(O(N-2) .NE. O(N))GOTO240
            GOTO250
200         IF(O(N-2) .EQ. O(N))GOTO240
            GOTO250
180         IF(O(N-2) .LE. O(N))GOTO240
            GOTO250
210         IF(O(N-2) .GT. O(N))GOTO240
            GOTO250
170         IF(O(N-2) .GE. O(N))GOTO240
            GOTO250
220         IF(O(N-2) .LT. O(N))GOTO240
            GOTO250
100         IF(O(N-2).EQ.1 .AND. O(N).EQ.1)GOTO240
            GOTO250
90          IF(O(N-2).EQ.1 .OR. O(N).EQ.1)GOTO240
            GOTO250
240         O(N-2)=1
            GOTO230
250         O(N-2)=0
230         N=N-2
         END IF
      GOTO 40
41    CONTINUE
70    CALLMESAGE(14, 23, A,Z+1)
99999 RETURN
      END
