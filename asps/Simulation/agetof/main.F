*
* $Id: main.F,v 1.2 2004/01/25 01:36:12 fisyak Exp $
*
* $Log: main.F,v $
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
* Revision 1.1  1998/07/15 06:45:24  cjonions
* agetof moved into Tools
*
* Revision 1.1.1.1  1998/01/28 12:57:44  cjo
* Importing agetof initial version
*
C_PLN_20-oct-94v_ux_2.
C-page 7----============================================================
      PROGRAM MAIN
      IMPLICIT NONE
      INTEGERO(500000)
      COMMON/NAMO/O
      INTEGER C,M,MXC,NERD,LINF,STATUS,MUSER
      INTEGER B,E,R,S,V,U,W,X
      EQUIVALENCE (O(1),B),(O(2),E),(O(3),R),(O(4),S), (O(5),V),(O(6),U)
     *,(O(7),W),(O(8),X),(C,O(12)),(MXC,O(43)),(O(22),M)
      INTEGER A/0/,D/0/,J/0/,N/0/,P/0/,Q/0/,T/0/,Y/0/,CS/0/,AA/0/,DD/0/,
     *KB/0/,IW/0/,KC/0/,KP/0/,MQ/0/,KX/0/,N1/0/,N2/0/,N3/0/,AO/0/,
     *AC/0/,SK/0/,NB/0/,PP/0/,PS/0/,QQ/0/,QS/0/,WS/0/,VS/0/,JMP/0/,NUL/
     *0/,MBK/0/,MFL/0/,MXD/0/
C PLN - 20-03-93 - no binary output. LONG->LLONG
C Arguments of INITAL are:
C I/O  Units = Definitions, Listing, FORTRAN, Errors, Parser
C Then =  Starting FORTRAN label and Inital input line length
C when making cmz, select generate all and change C+SELF,'. '
C 25-05-95 LINF call in EXPAND commented out.
      CALLINITAL(30,6,5,6,7,0,120)
      IW=376043
      X=W
      STATUS=MUSER('INIT','     ',0)
      IF (STATUS.NE.1) THEN
         CALLMESAGE(15, 35, 0,0)
      END IF
      GOTO10
20    IF(O(54).GT.0) O(M+5)=O(M+5)+1
      CALLEXPAND(A)
30    IF (S.GT.U) THEN
         IF (O(23).GT.0) THEN
            CALLEXPAND(-1)
            GOTO30
         END IF
         S=438021
         U=S-1
         GOTO10
      END IF
      M=O(32)+O(S)
40    IF(O(M).EQ.0)GOTO50
      M=O(M)
      D=M+6
      MXD=O(M+3)
      O(74)=0
      MBK=0
      O(69)=0
      MQ=0
      DD=0
      A=0
      T=S
      V=S
      SK=0
C 4-try...
      IF(O(29).GE.4) CALLMACTRC(2,D,MXD)
C against...
      IF(O(29).GE.4) CALLMACTRC(5,V,U+1)
C AO=AC; AC=.V;  IF (('A'l(.(V-1))l'Z') & ('A'l(.V)l'Z'))
C { IF(MTRCg4) PRINT *,'skipping ? ',(CHAR(XREP(MIN(MAX(0,.(V+I)),96))),
CI=-1,3); SK=1; }
      IF(O(54).GT.0) O(M+4)=O(M+4)+1
60    IF(V.GT.U)GOTO61
         IF (O(V).EQ.O(97)) THEN
            V=V+O(V+1)+3
            GOTO60
         END IF
C   #
         IF (MQ.EQ.0) THEN
            IF (O(D).EQ.O(113)) THEN
               O(S-2*(A+1)-1)=V
               O(S-2*(A+1)-2)=V
               D=D+2
               MQ=1
               DD=0
               IF(O(D-6).EQ.O(107)) DD=D
               PP=D
               QQ=O(D-1)+D
               GOTO60
C      [
            END IF
            IF (O(D).EQ.O(84)) THEN
               IF (O(D+1).LE.100 .AND. O(32).GT.1585) THEN
                  O(S-2*(A+1)-1)=V
                  O(S-2*(A+1)-2)=V
                  D=D+1
                  MQ=1
                  DD=0
                  IF(O(D-5).EQ.O(107)) DD=D
                  PP=D
                  QQ=D
70                IF(O(QQ).EQ.O(85))GOTO71
                     QQ=QQ+1
                     IF (QQ.GT.MXD) THEN
                        CALLMESAGE(14, 20, 0,0)
                        GOTO40
                     END IF
                  GOTO 70
71                CONTINUE
                  GOTO60
               END IF
            END IF
            IF (O(D).EQ.O(V)) THEN
               IF (O(V).LT.O(107)) THEN
                  IF( O(105).EQ.O(D)) O(69)=O(69)+1
                  IF( O(106).EQ.O(D)) O(69)=O(69)-1
                  IF(O(D).EQ.O(99)) MBK=1
                  IF(O(D).EQ.O(98)) MBK=0
                  V=V+1
                  T=V
                  GOTO80
               END IF
C
            END IF
            IF (O(V).EQ.O(76)) THEN
               IF (O(69)+MBK.EQ.0) THEN
                  V=V+1
                  GOTO60
               END IF
C      d
            END IF
            IF (O(D).EQ.O(107)) THEN
               N1=O(D+1)
               N2=O(D+2)
               N3=O(D+3)
               D=D+4
               T=V
               O(S-2*(A+1)-1)=V
               O(S-2*(A+1)-2)=V
               A=A+1
               DD=D
               KC=0
               KB=0
               KP=0
               GOTO60
            END IF
         ELSE
            AA=V
            IF (O(69).NE.0) AA=0
            GOTO90
         END IF
C      P, Q, V, U, AA   was   (PP,QQ,VV,IU,T )
         GOTO100
90       CS=0
         MFL=0
         O(74)=0
         VS=V
         O(45)=3675
         O(46)=3725
         P=PP
         Q=QQ
110      IF(P.GE.Q)GOTO111
            IF (O(VS).EQ.O(76)) THEN
               IF (VS.EQ.AA) THEN
                  VS=VS+1
                  AA=AA+1
                  IF(VS.GT.U)GOTO120
                  GOTO110
               END IF
            END IF
            IF (O(P).LE.100) THEN
               J = O(P)+1585
130            IF (O(J).EQ.0) THEN
                  CALLMESAGE(14, 15, P,Q+1)
                  GOTO140
               END IF
               J=O(J)
               R=P
               IF(O(54).GT.0) O(J+4)=O(J+4)+1
               PS=J+6
               QS=O(J+3)
C          6-TRY ...
               IF(O(29).GE.6) CALLMACTRC(2,PS,QS)
150            IF(PS.GE.QS)GOTO151
                  IF(O(PS).NE.O(R))GOTO130
                  PS=PS+1
                  R=R+1
               GOTO 150
151            CONTINUE
               O(45)=O(45)+1
               O(O(45))=Q
               O(45)=O(45)+1
               O(O(45))=R
               IF(O(45).GT.3724)CALLMESAGE(15, 19, 0,0)
               P=O(J+2)+6
               Q=O(O(J+2)+3)
C         AGAINST...
               IF(O(29).GE.4) CALLMACTRC(5,VS,U)
               IF(O(54).GT.0) O(J+5)=O(J+5)+1
            ELSE IF (O(P).LT.O(106)) THEN
               P=P+1
               JMP=O(P-1)-100
               GOTO(160,170,180,190,200),JMP
160            IF (MFL.EQ.0) THEN
                  VS=O(O(46)-1)
               ELSE
                  N=1
210               IF(N.EQ.0)GOTO211
                     IF( O(103).EQ.O(P)) N=N+1
                     IF( O(104).EQ.O(P)) N=N-1
                     P=P+1
                     IF(P.GT.Q)CALLMESAGE(14, 27, 0,0)
                  GOTO 210
211               CONTINUE
                  P=P-1
               END IF
               GOTO110
180            O(46)=O(46)+1
               O(O(46))=CS
               O(46)=O(46)+1
               O(O(46))=VS
               O(46)=O(46)+1
               O(O(46))=P
               IF(O(46).GT.3774)CALLMESAGE(15, 20, 0,0)
               CS=0
               GOTO110
190            P=P+2
               IF (MFL.EQ.1) THEN
                  CS=CS+1
                  IF (CS.LT.O(P-1)) THEN
                     P=O(O(46))
                     GOTO110
                  END IF
               END IF
               IF(CS.GE.O(P-2))MFL=1
               IF( MFL.EQ.0 ) VS=O(O(46)-1)
               IF ((O(46).EQ.3725)) THEN
                  CALLMESAGE(14, 17, 0,0)
                  GOTO140
               END IF
               NUL=O(O(46))
               O(46)=O(46)-1
               NUL=O(O(46))
               O(46)=O(46)-1
               CS=O(O(46))
               O(46)=O(46)-1
               GOTO220
170            P=P+2
               MFL=0
               IF ((O(P-2).LE.O(VS)).AND.(O(VS).LE.O(P-1))) THEN
                  MFL=1
                  VS=VS+1
                  IF(VS.GT.U)GOTO120
               END IF
               GOTO220
200            WS=VS
               MFL=1
               IF (O(P).EQ.O(76)) THEN
230               IF(O(WS).NE.O(76).OR.O(P).NE.O(76))GOTO231
                     WS=WS-1
                     P=P+1
                  GOTO 230
231               CONTINUE
               END IF
240            IF(O(P).EQ.O(106))GOTO241
                  IF(O(P).NE.O(WS).OR.WS.GT.U) MFL=0
                  P=P+1
                  WS=WS+1
               GOTO 240
241            CONTINUE
               P=P+1
               IF(MFL.EQ.1)VS=WS
220            IF(MFL.EQ.1)GOTO110
               IF(O(P).NE.O(101) .AND. O(P).NE.O(104))GOTO140
            ELSE
               CALLMESAGE(14, 19, P,Q+11)
               GOTO140
            END IF
         GOTO 110
111      CONTINUE
         IF (.NOT.(O(45).EQ.3675)) THEN
            P=O(O(45))
            O(45)=O(45)-1
            Q=O(O(45))
            O(45)=O(45)-1
            GOTO110
         END IF
C   SUCCESSFULL
         O(74)=1
         V=VS
         IF(O(29).GE.6) CALLMACTRC(6,PP,QQ)
         GOTO250
140      O(74)=0
         GOTO100
100      IF (O(V).EQ.O(76)) THEN
            IF (MQ.EQ.1) THEN
               V=V+1
               GOTO60
            END IF
         END IF
         IF(DD.EQ.0)GOTO40
         IF (O(74).GT.0) THEN
            IF (O(DD-5).EQ.O(107)) THEN
               O(74)=0
               A=A-1
               GOTO90
            END IF
         END IF
260      CONTINUE
            IF (O(T).EQ.O(97)) THEN
               T=T+O(T+1)+3
               GOTO260
            END IF
C       PN, 24.11.97
            IF(O(T).EQ.O(106))GOTO40
            IF (MBK.GT.0) THEN
270            IF( O(99).EQ.O(T)) MBK=MBK+1
               IF( O(98).EQ.O(T)) MBK=MBK-1
               IF (MBK.EQ.0) THEN
                  V=T
                  O(S-2*(A)-2)=V
                  GOTO261
               END IF
               T=T+1
               IF(T.GT.U)GOTO40
               GOTO270
            END IF
            IF (O(T).EQ.O(105)) THEN
               NB=1
280            IF(NB.EQ.0)GOTO281
                  T=T+1
                  IF(T.GT.U)GOTO40
                  IF( O(105).EQ.O(T)) NB=NB+1
                  IF( O(106).EQ.O(T)) NB=NB-1
               GOTO 280
281            CONTINUE
            END IF
            IF (O(69).EQ.0) THEN
               IF( O(84).EQ.O(T)) KC=KC+1
               IF( O(85).EQ.O(T)) KC=KC-1
               IF(KC.LT.0)GOTO40
               IF (N1.GT.0) THEN
                  IF(38.EQ.O(T)) KP=KP+1
                  IF(46.EQ.O(T)) KP=KP-1
                  IF(KP.LT.0)GOTO40
               END IF
               IF (N2.GT.0) THEN
                  IF( O(82).EQ.O(T)) KB=KB+1
                  IF( O(83).EQ.O(T)) KB=KB-1
                  IF(KB.LT.0)GOTO40
               END IF
               IF (N3.GT.0) THEN
                  IF(O(T).EQ.O(79))GOTO40
               END IF
            END IF
            T=T+1
            IF (T.GT.U) THEN
               IF(O(15).EQ.1)GOTO40
               GOTO10
            END IF
            IF (KC+KP+KB.EQ.0) THEN
               IF(MQ.EQ.0) D=DD
290            V=T
               O(S-2*(A)-2)=V
               GOTO261
            END IF
         GOTO 260
261      CONTINUE
         GOTO60
250      IF(DD.NE.0)O(S-2*(A)-2)=T
         A=A+1
         IF(AA.NE.0)O(S-2*(A)-1)=AA
         O(S-2*(A)-2)=V
         D=Q
C    IF(MTRCg1) PRINT *,'next match',(CHAR(XREP(MIN(MAX(0,.(AA+I)),96)))
C   ,I=0,3);
         MQ=0
80       D=D+1
         IF(D.GE.MXD)GOTO20
      GOTO 60
61    CONTINUE
120   IF(O(15).EQ.1)GOTO40
      IF (S.EQ.U) THEN
         S=438021
         O(S)=O(U)
         U=S
      END IF
10    O(55)=O(76)
C   IF(MTRCg1) PRINT *,' *** in QUE C,U=',C,U;
C            m a i n    i n p u t   l o o p
      KX=0
300   CONTINUE
         U=U+1
         IF(U.GT.500000) CALLMESAGE(15, 1, 0,0)
         O(U)=O(C)
         C=C+1
         IF(C.GT.O(43))CALLNXTCRD
C      IF(MTRCg1) PRINT *,' input ',CHAR(.(.U+MNOSET));
         IF (O(U).EQ.O(84)) THEN
            O(55)=O(U)
            KX=1
310         IF(KX.EQ.0)GOTO311
C          IF(MTRCg1&.Un' ') PRINT *,'   put ',CHAR(.(.U+MNOSET));
               U=U+1
               IF(U.GT.500000) CALLMESAGE(15, 1, 0,0)
               O(U)=O(C)
               C=C+1
               IF(C.GT.O(43))CALLNXTCRD
               IF (O(U).EQ.O(87)) THEN
320               IF(O(C).EQ.O(87).OR.C.GT.MXC)GOTO321
                     U=U+1
                     IF(U.GT.500000) CALLMESAGE(15, 1, 0,0)
                     O(U)=O(C)
                     C=C+1
                  GOTO 320
321               CONTINUE
C             copy citation
                  IF (C.GT.MXC) THEN
                     CALLNXTCRD
                     IF (O(26).NE.1) THEN
                        GOTO320
                     END IF
                  END IF
C             continue copy ?
                  U=U+1
                  IF(U.GT.500000) CALLMESAGE(15, 1, 0,0)
                  O(U)=O(C)
                  C=C+1
                  IF(C.GT.O(43))CALLNXTCRD
C             otherwise done !
               END IF
               IF( O(84).EQ.O(U)) KX=KX+1
               IF( O(85).EQ.O(U)) KX=KX-1
               IF (O(73).EQ.1) THEN
                  IF (52.LE.O(U) .AND. O(U) .LE. 77) O(U)=O(U)-52+10
               END IF
C         lower TO UPPER
            GOTO 310
311         CONTINUE
            O(55)=O(76)
         END IF
         IF (O(U).EQ.O(87)) THEN
330         IF (O(26).NE.1) O(55)=O(87)
            O(U)=O(97)
            U=U+1
C        Open citation,
            Y=U
340         IF(O(C).EQ.O(87).OR.C.GT.MXC)GOTO341
               U=U+1
               IF(U.GT.500000) CALLMESAGE(15, 1, 0,0)
               O(U)=O(C)
               C=C+1
            GOTO 340
341         CONTINUE
C       now copy it and
            U=U+1
            O(U)=O(96)
            O(Y)=U-Y-1
            IF (O(Y).EQ.0) THEN
               U=U-2
               O(U)=O(76)
C       always close it.
            END IF
            IF (C.GT.MXC) THEN
               IF (O(26).EQ.1) THEN
                  O(55)=O(76)
                  CALLNXTCRD
                  GOTO10
               END IF
C          On EOL reset it
C         else
               CALLNXTCRD
               U=U+1
               GOTO330
            END IF
C       or continue,
C      else
            O(55)=O(76)
            C=C+1
            IF(C.GT.O(43))CALLNXTCRD
            GOTO10
C       otherwise done !
         END IF
         IF (O(U).EQ.O(86) .AND. O(28).GT.0) THEN
            O(55)=O(U)
            O(U)=O(105)
350         CONTINUE
               U=U+1
               IF(U.GT.500000) CALLMESAGE(15, 1, 0,0)
               O(U)=O(C)
               C=C+1
               IF(C.GT.O(43))CALLNXTCRD
               IF (O(U).EQ.O(86)) THEN
                  IF (O(C).NE.O(86)) THEN
                     O(U)=O(106)
                     O(55)=O(76)
                     GOTO351
C            else ''
                  END IF
                  C=C+1
                  IF(C.GT.O(43))CALLNXTCRD
               END IF
            GOTO 350
351         CONTINUE
         END IF
         IF (O(U).EQ.O(76)) THEN
            IF (S.NE.U) THEN
               IF(O(U-1).EQ.O(76)) U=U-1
            END IF
360         CONTINUE
               IF(C.EQ.MXC.OR.O(C).NE.O(76))GOTO361
               C=C+1
            GOTO 360
361         CONTINUE
         END IF
         IF (O(73).EQ.1) THEN
            IF ((52.LE.O(U)).AND.(O(U).LE.77)) O(U)=O(U)-52+10
         END IF
C   lower TO UPPER
         IF (KX.EQ.0) THEN
            IF(O(U).EQ.O(79))GOTO301
            IF( O(82).EQ.O(U)) O(11)=O(11)+1
            IF( O(83).EQ.O(U)) O(11)=O(11)-1
         END IF
      GOTO 300
301   CONTINUE
      GOTO30
50    IF (O(23).GT.0) THEN
         X=X+1
         IF(X.GT.O(40)) CALLMESAGE(15, 3, 0,0)
         O(X)=O(S)
         S=S+1
         IF (O(X).EQ.O(92).AND.O(32).NE.1345) THEN
370         CONTINUE
               X=X+1
               IF(X.GT.O(40)) CALLMESAGE(15, 3, 0,0)
               O(X)=O(S)
               S=S+1
               IF(O(X).EQ.O(93))GOTO371
            GOTO 370
371         CONTINUE
         END IF
         GOTO30
      END IF
380   CONTINUE
         IF (O(S).EQ.O(79)) THEN
            IF (W.GE.IW) W=LINF (IW,W,15)
         ELSE IF (O(S).EQ.O(97)) THEN
            IF (O(68).EQ.0) THEN
               NERD=LINF(S+2,S+O(S+1)+1,12)
               S=S+O(S+1)+2
            END IF
         ELSE IF (O(S).EQ.O(92)) THEN
            O(24)=1
         ELSE IF (O(S).EQ.O(93)) THEN
            O(24)=0
         ELSE IF (O(68).EQ.0) THEN
            W=W+1
            O(W)=O(S)
            IF (O(W).EQ.O(105)) THEN
390            IF(O(W).EQ.O(106))GOTO391
                  W=W+1
                  S=S+1
                  O(W)=O(S)
               GOTO 390
391            CONTINUE
            END IF
         END IF
C    IF(MTRCg4) PRINT *,' in fail ',(CHAR(XREP(MIN(MAX(0,.(S+I)),96))),I
C   =0,3);
         SK=0
         AO=O(S)
         S=S+1
         AC=O(S)
         IF (AO.LE.35 .AND. AC.LE.35) SK=1
         IF(S.GT.U .OR. O(24)+SK.EQ.0)GOTO381
      GOTO 380
381   CONTINUE
      GOTO30
      END
