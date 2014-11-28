*
* $Id: define.F,v 1.1.1.1 2003/12/23 14:54:43 fisyak Exp $
*
* $Log: define.F,v $
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:11  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:01  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:42  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      SUBROUTINE DEFINE(S)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER S
      INTEGER L(3)/3*0/,A/0/,B/0/,C/0/,E/0/,F/0/,I/0/,J/0/,K/0/,M/0/,N/
     *0/,P/0/,R/0/,T/0/,Z/0/,LP/0/,NEED/0/
      IF (O(S-2*(1)-1).EQ.O(S-2*(1)-2)) THEN
         CALLMESAGE(14, 1, 0,0)
         GOTO 99999
      END IF
      O(20)=O(31)
      IF ( O(S-2*(2)-1).NE.O(S-2*(2)-2) )O(20)=O(O(S-2*(2)-1))
      DO 21 K = 1,3
         A=O(S-2*(K)-1)
         T=O(S-2*(K)-2)
         N=A-1
         Z=0
30       IF(A.GT.T)GOTO31
            N=N+1
            O(N)=O(A)
            A=A+1
            IF (O(N).EQ.O(86)) THEN
               IF (K.EQ.3 .OR. O(28).GT.0) THEN
                  IF (Z.EQ.0) THEN
                     O(N)=O(105)
                     Z=1
                  ELSE
                     IF (O(A).EQ.O(86) .AND. A.LE.T) THEN
                        A=A+1
                     ELSE
                        O(N)=O(106)
                        Z=0
                     END IF
                  END IF
               END IF
            ELSE IF (O(N).EQ.O(76)) THEN
               IF (Z.EQ.0) THEN
40                IF(O(A).NE.O(76).OR.A.GE.T)GOTO41
                     A=A+1
                  GOTO 40
41                CONTINUE
               END IF
            ELSE IF (O(N).EQ.O(78)) THEN
               IF (O(A).EQ.O(78)) THEN
                  A=A+1
               ELSE
                  O(N)=O(108)
               END IF
            ELSE IF (O(N).EQ.O(77)) THEN
               IF (O(A).EQ.O(77)) THEN
                  A=A+1
               ELSE
                  O(N)=O(107)
               END IF
            END IF
            IF (O(33).EQ.0) THEN
               IF (K.EQ.3) THEN
                  IF(O(N).EQ.O(84))O(N)=O(109)
                  IF(O(N).EQ.O(85))O(N)=O(110)
               END IF
            END IF
         GOTO 30
31       CONTINUE
         O(S-2*(K)-2)=N
         L(K)=O(S-2*(K)-2)-O(S-2*(K)-1)
21    CONTINUE
      IF(O(20).LT.12 .OR. O(20) .GT.14)GOTO50
      B=O(S-2*(1)-1)
      E=O(S-2*(1)-2)
      IF (O(20).EQ.14) THEN
         M=1585+O(B)
      ELSE
         M=1705+O(B)
      END IF
60    IF (O(M).EQ.0) THEN
         IF (O(20).EQ.12) THEN
            O(20)=O(31)
            GOTO50
         END IF
         CALLMESAGE(14, 2, 0,0)
         GOTO 99999
      END IF
      M=O(M)
      P = M+6
      LP=O(M+3)-P
      IF(LP.NE.L(1))GOTO60
      A=B
70    IF(O(P).NE.O(A))GOTO71
         P=P+1
         A=A+1
         IF(A.GE.E)GOTO80
      GOTO 70
71    CONTINUE
      GOTO60
80    IF (O(20).NE.12) THEN
C   d-link template
         IF(O(M).NE.0)O((O(M))+1)=O(M+1)
         O(O(M+1))=O(M)
C   fix cat pointer
         C=O(M+2)
C   mark macro dead
C   d-link all cats
         O(M+2)=O(90)
90       IF(C.EQ.0)GOTO91
            O(C+2)=O(90)
            C=O(C)
         GOTO 90
91       CONTINUE
         IF(O(13).GT.0)CALLMACTRC(13,0,S)
         GOTO 99999
      END IF
      NEED= 6+L(3)
      IF(NEED.GT. 376042-O(34)) CALLKRUNC(NEED)
      C=O(M+2)
100   IF(O(C).EQ.0)GOTO101
         C=O(C)
      GOTO 100
101   CONTINUE
      O(C)=O(34)
      O(O(34))=0
      O(O(34)+1)=C
      O(O(34)+2)=O(91)
      O(O(34)+3)=O(34)+NEED
      O(34)=O(34)+NEED
      A=O(S-2*(3)-2)
      K=L(3)
      DO 111 J = 1,K
         O(O(34)-J)=O(A-J)
111   CONTINUE
      IF(O(13).GT.0)CALLMACTRC(12,0,S)
      O(60)=376042-O(34)
      IF (O(60) .LE. 1000) PRINT *,' Parser warning: only ',O(60),' memo
     *ry left'
      GOTO 99999
50    NEED=L(1)+L(3)+2*6
      IF (NEED.GT.376042-O(34)) CALLKRUNC(2+NEED)
      A=O(S-2*(1)-1)
      Z=A+L(1)-1
      P=O(34)
      F=P+6
      I=F
120   IF(A.GT.Z)GOTO121
         O(F)=O(A)
         F=F+1
         A=A+1
      GOTO 120
121   CONTINUE
      IF (O(I).EQ.O(84) .AND. O(20).GT.2) THEN
         CALLMESAGE(14, 21, O(S-2*(1)-1),O(S-2*(1)-2))
         GOTO 99999
      END IF
      IF (O(I).GT.O(105)) THEN
         CALLMESAGE(14, 9, O(S-2*(1)-1),O(S-2*(1)-2))
         GOTO 99999
      END IF
      IF (0.GT.O(20).OR.O(20).GT.7) THEN
         CALLMESAGE(14, 12, O(S-2*(1)-1),O(S-2*(1)-2))
         GOTO 99999
      END IF
      I=O(I) + 1225 + O(20)*120
      O(P)=O(I)
      IF(O(P).NE.0)O((O(P))+1)=P
      O(P+1)=I
      O(I)=P
      O(P+2)=F
      O(P+3)=F
      O(F)=0
      O(F+1)=P+2
      O(F+3)=F+6+L(3)
      O(F+2)=O(91)
      R=F
      F=P+NEED
      E=O(S-2*(3)-2)
      K=L(3)
      IF (K.NE.0) THEN
         CALLDOIT(E-K,E)
         DO 131 J = 1,K
            O(F-J)=O(E-J)
131      CONTINUE
      END IF
      IF (O(13).GT.0) CALLMACTRC(15,P,R)
      O(34)= P +NEED
      O(60)=376042-O(34)
      O(61)=(100*O(60))/(376042-4175)
      IF (O(60) .LE. 1000) PRINT *,' Parser warning: only ',O(60),' memo
     *ry left'
99999 RETURN
      END
