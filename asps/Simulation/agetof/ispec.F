*
* $Id: ispec.F,v 1.2 2004/01/25 01:36:12 fisyak Exp $
*
* $Log: ispec.F,v $
* Revision 1.2  2004/01/25 01:36:12  fisyak
* Clean up unused variable to avoid waning by Victor request
*
* Revision 1.1.1.1  2003/12/23 14:54:43  fisyak
* ATLAS version of mortran (geant3 => agetof)
*
* Revision 1.3  2001/11/28 23:58:41  nevski
* restoring automatic definition search
*
* Revision 1.2  2001/02/28 22:56:14  nevski
* improved comment treatment, f77-structures etc
*
* Revision 1.1  1998/07/15 06:45:07  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:43  cjo
* Importing agetof initial version
*
C------------------------------------------------------------------
      FUNCTION ISPEC(PP,QQ,VV,IU,T)
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER ISPEC,PP,QQ,VV,IU,T
      INTEGER B/0/,C/0/,E/0/,F/0/,J/0/,M/0/,N/0/,P/0/,Q/0/,R/
     *0/,U/0/,V/0/,W/0/,JMP/0/,NUL/0/
      P=PP
      Q=QQ
      U=IU+1
      C=0
      M=0
      B=0
      V=VV
      O(45)=3675
      O(46)=3725
20    IF(P.GE.Q)GOTO21
         IF (O(V).EQ.O(76)) THEN
            IF (V.EQ.T) THEN
               V=V+1
               T=T+1
               IF(V.GT.U)GOTO30
               GOTO20
            END IF
         END IF
         IF (O(P).LE.100) THEN
            QQ=Q
            J = O(P)+1585
40          IF (O(J).EQ.0) THEN
               CALLMESAGE(14, 15, P,Q+1)
               GOTO50
            END IF
            J=O(J)
            R=P
            E=J+6
            F=O(J+3)
            IF(O(29).GE.6) CALLMACTRC(2,E,F)
            IF(O(54).GT.0) O(J+4)=O(J+4)+1
60          IF(E.GE.F)GOTO61
               IF(O(E).NE.O(R))GOTO40
               E=E+1
               R=R+1
            GOTO 60
61          CONTINUE
            P=O(J+2)+6
            Q=O(O(J+2)+3)
            IF(O(29).GE.4) CALLMACTRC(5,V,U)
            O(45)=O(45)+1
            O(O(45))=QQ
            O(45)=O(45)+1
            O(O(45))=R
            IF(O(45).GT.3724)CALLMESAGE(15, 19, 0,0)
            IF(O(54).GT.0) O(J+5)=O(J+5)+1
         ELSE IF (O(P).LT.O(106)) THEN
            P=P+1
            JMP=O(P-1)-100
            GOTO(70,80,90,100,110),JMP
70          IF (M.EQ.0) THEN
               V=O(O(46)-1)
            ELSE
               N=1
120            IF(N.EQ.0)GOTO121
                  IF( O(103).EQ.O(P)) N=N+1
                  IF( O(104).EQ.O(P)) N=N-1
                  P=P+1
                  IF(P.GT.Q)CALLMESAGE(14, 27, 0,0)
               GOTO 120
121            CONTINUE
               P=P-1
            END IF
            GOTO20
90          O(46)=O(46)+1
            O(O(46))=C
            O(46)=O(46)+1
            O(O(46))=V
            O(46)=O(46)+1
            O(O(46))=P
            IF(O(46).GT.3774)CALLMESAGE(15, 20, 0,0)
            C=0
            GOTO20
100         P=P+2
            IF (M.EQ.1) THEN
               C=C+1
               IF (C.LT.O(P-1)) THEN
                  P=O(O(46))
                  GOTO20
               END IF
            END IF
            IF(C.GE.O(P-2)) M=1
            IF( M.EQ.0 ) V=O(O(46)-1)
            IF ((O(46).EQ.3725)) THEN
               CALLMESAGE(14, 17, 0,0)
               GOTO50
            END IF
            NUL=O(O(46))
            O(46)=O(46)-1
            NUL=O(O(46))
            O(46)=O(46)-1
            C=O(O(46))
            O(46)=O(46)-1
            GOTO130
80          P=P+2
            M=0
            IF ((O(P-2).LE.O(V)).AND.(O(V).LE.O(P-1))) THEN
               M=1
               V=V+1
               IF(V.GT.U)GOTO30
            END IF
            GOTO130
110         W=V
            M=1
            IF (O(P).EQ.O(76)) THEN
140            IF(O(W).NE.O(76).OR.O(P).NE.O(76))GOTO141
                  W=W-1
                  P=P+1
               GOTO 140
141            CONTINUE
            END IF
150         IF(O(P).EQ.O(106))GOTO151
               IF(O(P).NE.O(W).OR.W.GT.U)M=0
               P=P+1
               W=W+1
            GOTO 150
151         CONTINUE
            P=P+1
            IF(M.EQ.1)V=W
130         IF(M.EQ.1)GOTO20
            IF(O(P).NE.O(101) .AND. O(P).NE.O(104))GOTO50
         ELSE
            CALLMESAGE(14, 19, P,Q+11)
            GOTO50
         END IF
      GOTO 20
21    CONTINUE
      IF (.NOT.(O(45).EQ.3675)) THEN
         P=O(O(45))
         O(45)=O(45)-1
         Q=O(O(45))
         O(45)=O(45)-1
         GOTO20
      END IF
      O(74)=1
      VV=V
      ISPEC=3
      IF(O(29).GE.6) CALLMACTRC(6,PP,QQ)
      GOTO 99999
50    O(74)=0
      ISPEC=2
      GOTO 99999
30    ISPEC=1
99999 RETURN
      END
