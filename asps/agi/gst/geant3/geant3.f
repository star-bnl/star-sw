*CMZ :          25/11/97  09.57.18  by  Pavel Nevski
*-- Author :
C_PLN_20-oct-94v_ux_2.
CUNIT(9) ANO(10) SBKK(11) CRS(12) MDEF(13) MINP(14) FRT(15) LABL(16)
CMERR(17) INDF(18) SEQN(19) TYP(20) LIST(21) MCOM(22) NEST(23) NSCN(24)
CPCOM(25) LAZY(26) MRKL(27) SFLG(28) MTRC(29) SEQF(30) DFLT(31) BASE(32)
CBLGR(33) FREE(34) LOMRK(35) TMX(36) NMX(37) RMX(38) INDC(39) MXX(40)
CNSIG(41) RSIG(42) TSIG(43) ISK(44) JSK(45) KSK(46) LSK(47) MSK(48)
CNSK(49) USK(50) YSK(51) EOF(52) INDM(53) HIST(54) QCHR(55) MOUT(56)
CFOUT(57) EOUT(58) MODE(59) LOX(60) LEFT(61) MCNT(62) MIF(63) KFE(64)
CKFB(65) KTE(66) KTB(67) NGN(68) SQ(69) EOL(70) GEO(71) XTR(72)
CUPLO(73) SPOK(74) ICUR(75) BLA(76) BAT(77) BLB(78) TRM(79) LPR(80)
CRPR(81) LSB(82) RSB(83) LCB(84) RCB(85) BQ(86) DQ(87) XTR(88)
CXTR(89) DED(90) NIL(91) LNS(92) RNS(93) RIF(94) LIF(95) RDQ(96)
CLDQ(97) RMB(98) LMB(99) ZRO(100) ALT(101) RNG(102) LIP(103) RIP(104)
CLQ(105) RQ(106) GA(107) GP(108) LGB(109) RGB(110) NAT(111) NLB(112)
CNEU(113) MF1(114) MF2(115) MF3(116) MF4(117) MF5(118) MF6(119) MF7(120)
CMF8(121)_MF9(122)_MFA(123)_MFB(124)_MFC(125)_MFD(126)
CNODE6 NSM100 NEGC20 NSX120 BFF132 MXU256000 MNXBF193043 HOME224521
CEVLIM1000 MXERR100 G1237 G0236 G2238 G11247 G13249 G14250
CR1405 R0404 R80484 R100504 T1537 T0536 T2538 T72608
CW1255_W0254_W7261_W72326_W73327_W80334
CSREG1 UREG201 GBF237 WBF255 RBF405 TBF537 ISET669 OSET769
CMORF869 TRT969 0CT1225 1CT1345 2CT1465 3CT1585 4CT1705 5CT1825
C6CT1945 7CT2065 8CT2185 9CT2305 ACT2425 BCT2545 CCT2665 DCT2785
CECT2905 FCT3025 GCT3145 HCT3265 ICT3385 JCT3505 ISK3625 JSK3675
CKSK3725_LSK3775_MSK3825_NSK3875_USK3975_YSK4025_PJT4075_MBF4175
C !ANNOTATE;
C-page 7----============================================================
C======------
      PROGRAM MAIN
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      EQUIVALENCE(O(1),B),(O(2),E),(O(3),R),(O(4),S), (O(5),V),(O(6),U),
     *(O(7),W),(O(8),X),(C,O(12)),(MXC,O(43)),(O(22),M)
C PLN - 20-03-93 - no binary output. LONG->LLONG
C Arguments of INITAL are:
C I/O  Units = Definitions, Listing, FORTRAN, Errors, GEANT3
C Then =  Starting FORTRAN label and Inital input line length
C when making cmz, select generate all and change C+SELF,'. '
C 25-05-95 LINF call in EXPAND commented out.
      CALLINITAL(30,6,5,6,7,0,120)
      IW=193043
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
         S=224521
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
      IF(O(29).GE.4) CALLMACTRC(2,D,MXD)
      IF(O(29).GE.4) CALLMACTRC(5,V,U+1)
      IF(O(54).GT.0) O(M+4)=O(M+4)+1
60    IF(V.GT.U)GOTO61
         IF (O(V).EQ.O(97)) THEN
            V=V+O(V+1)+3
            GOTO60
         END IF
         IF (MQ.EQ.0) THEN
            IF (O(D).EQ.O(113)) THEN
               O(S-2*(A+1)-1)=V
               O(S-2*(A+1)-2)=V
               D=D+2
               MQ=1
               DD=0
               IF(O(D-6).EQ.O(107))DD=D
               PP=D
               QQ=O(D-1)+D
               GOTO60
            END IF
            IF (O(D).EQ.O(84)) THEN
               IF (O(D+1).LE.100.AND.O(32).GT.1585) THEN
                  O(S-2*(A+1)-1)=V
                  O(S-2*(A+1)-2)=V
                  D=D+1
                  MQ=1
                  DD=0
                  IF(O(D-5).EQ.O(107))DD=D
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
            END IF
            IF (O(V).EQ.O(76)) THEN
               IF (O(69)+MBK.EQ.0) THEN
                  V=V+1
                  GOTO60
               END IF
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
            IF(O(69).NE.0)AA=0
            GOTO90
         END IF
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
110      CONTINUE
            IF (O(T).EQ.O(97)) THEN
               T=T+O(T+1)+3
               GOTO110
            END IF
C       PN, 24.11.97
            IF(O(T).EQ.O(106))GOTO40
            IF (MBK.GT.0) THEN
120            IF( O(99).EQ.O(T)) MBK=MBK+1
               IF( O(98).EQ.O(T)) MBK=MBK-1
               IF (MBK.EQ.0) THEN
                  V=T
                  O(S-2*(A)-2)=V
                  GOTO111
               END IF
               T=T+1
               IF(T.GT.U)GOTO40
               GOTO120
            END IF
            IF (O(T).EQ.O(105)) THEN
               NB=1
130            IF(NB.EQ.0)GOTO131
                  T=T+1
                  IF(T.GT.U)GOTO40
                  IF( O(105).EQ.O(T)) NB=NB+1
                  IF( O(106).EQ.O(T)) NB=NB-1
               GOTO 130
131            CONTINUE
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
140            V=T
               O(S-2*(A)-2)=V
               GOTO111
            END IF
         GOTO 110
111      CONTINUE
         GOTO60
150      IF(DD.NE.0)O(S-2*(A)-2)=T
         A=A+1
         IF(AA.NE.0)O(S-2*(A)-1)=AA
         O(S-2*(A)-2)=V
         D=Q
         MQ=0
80       D=D+1
         IF(D.GE.MXD)GOTO20
      GOTO 60
61    CONTINUE
160   IF(O(15).EQ.1)GOTO40
      IF (S.EQ.U) THEN
         S=224521
         O(S)=O(U)
         U=S
      END IF
10    O(55)=O(76)
C   IF(MTRCg1) PRINT *,' *** in QUE C,U=',C,U;
C            m a i n    i n p u t   l o o p
      KX=0
170   CONTINUE
         U=U+1
         IF(U.GT.256000) CALLMESAGE(15, 1, 0,0)
         O(U)=O(C)
         C=C+1
         IF(C.GT.O(43))CALLNXTCRD
C      IF(MTRCg1) PRINT *,' input ',CHAR(.(.U+MNOSET));
         IF (O(U).EQ.O(84)) THEN
            O(55)=O(U)
            KX=1
180         IF(KX.EQ.0)GOTO181
C          IF(MTRCg1&.Un' ') PRINT *,'   put ',CHAR(.(.U+MNOSET));
               U=U+1
               IF(U.GT.256000) CALLMESAGE(15, 1, 0,0)
               O(U)=O(C)
               C=C+1
               IF(C.GT.O(43))CALLNXTCRD
               IF( O(84).EQ.O(U)) KX=KX+1
               IF( O(85).EQ.O(U)) KX=KX-1
               IF (O(73).EQ.1) THEN
                  IF (52.LE.O(U) .AND. O(U) .LE. 77) O(U)=O(U)-52+10
               END IF
C         lower TO UPPER
            GOTO 180
181         CONTINUE
            O(55)=O(76)
         END IF
         IF (O(U).EQ.O(87)) THEN
190         IF (O(26).NE.1) O(55)=O(87)
            O(U)=O(97)
            U=U+1
C        Open citation,
            Y=U
200         IF(O(C).EQ.O(87).OR.C.GT.MXC)GOTO201
               U=U+1
               IF(U.GT.256000) CALLMESAGE(15, 1, 0,0)
               O(U)=O(C)
               C=C+1
            GOTO 200
201         CONTINUE
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
               GOTO190
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
210         CONTINUE
               U=U+1
               IF(U.GT.256000) CALLMESAGE(15, 1, 0,0)
               O(U)=O(C)
               C=C+1
               IF(C.GT.O(43))CALLNXTCRD
               IF (O(U).EQ.O(86)) THEN
                  IF (O(C).NE.O(86)) THEN
                     O(U)=O(106)
                     O(55)=O(76)
                     GOTO211
C            else ''
                  END IF
                  C=C+1
                  IF(C.GT.O(43))CALLNXTCRD
               END IF
            GOTO 210
211         CONTINUE
         END IF
         IF (O(U).EQ.O(76)) THEN
            IF (S.NE.U) THEN
               IF(O(U-1).EQ.O(76)) U=U-1
            END IF
220         CONTINUE
               IF(C.EQ.MXC.OR.O(C).NE.O(76))GOTO221
               C=C+1
            GOTO 220
221         CONTINUE
         END IF
         IF (O(73).EQ.1) THEN
            IF (52.LE.O(U) .AND. O(U) .LE. 77) O(U)=O(U)-52+10
         END IF
C   lower TO UPPER
         IF (KX.EQ.0) THEN
            IF(O(U).EQ.O(79))GOTO171
            IF( O(82).EQ.O(U)) O(11)=O(11)+1
            IF( O(83).EQ.O(U)) O(11)=O(11)-1
         END IF
      GOTO 170
171   CONTINUE
      GOTO30
50    IF (O(23).GT.0) THEN
         X=X+1
         IF(X.GT.O(40)) CALLMESAGE(15, 3, 0,0)
         O(X)=O(S)
         S=S+1
         IF (O(X).EQ.O(92).AND.O(32).NE.1345) THEN
230         CONTINUE
               X=X+1
               IF(X.GT.O(40)) CALLMESAGE(15, 3, 0,0)
               O(X)=O(S)
               S=S+1
               IF(O(X).EQ.O(93))GOTO231
            GOTO 230
231         CONTINUE
         END IF
         GOTO30
      END IF
240   CONTINUE
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
250            IF(O(W).EQ.O(106))GOTO251
                  W=W+1
                  S=S+1
                  O(W)=O(S)
               GOTO 250
251            CONTINUE
            END IF
         END IF
         S=S+1
         IF(S.GT.U .OR. O(24).EQ.0)GOTO241
      GOTO 240
241   CONTINUE
C   P, Q, V, U, AA   was   (PP,QQ,VV,IU,T )
      GOTO30
90    CS=0
      MFL=0
      O(74)=0
      VS=V
      O(45)=3675
      O(46)=3725
      P=PP
      Q=QQ
260   IF(P.GE.Q)GOTO261
         IF (O(VS).EQ.O(76)) THEN
            IF (VS.EQ.AA) THEN
               VS=VS+1
               AA=AA+1
               IF(VS.GT.U)GOTO160
               GOTO260
            END IF
         END IF
         IF (O(P).LE.100) THEN
            J = O(P)+1585
270         IF (O(J).EQ.0) THEN
               CALLMESAGE(14, 15, P,Q+1)
               GOTO280
            END IF
            J=O(J)
            R=P
            IF(O(54).GT.0) O(J+4)=O(J+4)+1
            PS=J+6
            QS=O(J+3)
            IF(O(29).GE.6) CALLMACTRC(2,PS,QS)
290         IF(PS.GE.QS)GOTO291
               IF(O(PS).NE.O(R))GOTO270
               PS=PS+1
               R=R+1
            GOTO 290
291         CONTINUE
            O(45)=O(45)+1
            O(O(45))=Q
            O(45)=O(45)+1
            O(O(45))=R
            IF(O(45).GT.3724)CALLMESAGE(15, 19, 0,0)
            P=O(J+2)+6
            Q=O(O(J+2)+3)
            IF(O(29).GE.4) CALLMACTRC(5,VS,U)
            IF(O(54).GT.0) O(J+5)=O(J+5)+1
         ELSE IF (O(P).LT.O(106)) THEN
            P=P+1
            JMP=O(P-1)-100
            GOTO(300,310,320,330,340),JMP
300         IF (MFL.EQ.0) THEN
               VS=O(O(46)-1)
            ELSE
               N=1
350            IF(N.EQ.0)GOTO351
                  IF( O(103).EQ.O(P)) N=N+1
                  IF( O(104).EQ.O(P)) N=N-1
                  P=P+1
                  IF(P.GT.Q)CALLMESAGE(14, 27, 0,0)
               GOTO 350
351            CONTINUE
               P=P-1
            END IF
            GOTO260
320         O(46)=O(46)+1
            O(O(46))=CS
            O(46)=O(46)+1
            O(O(46))=VS
            O(46)=O(46)+1
            O(O(46))=P
            IF(O(46).GT.3774)CALLMESAGE(15, 20, 0,0)
            CS=0
            GOTO260
330         P=P+2
            IF (MFL.EQ.1) THEN
               CS=CS+1
               IF (CS.LT.O(P-1)) THEN
                  P=O(O(46))
                  GOTO260
               END IF
            END IF
            IF(CS.GE.O(P-2))MFL=1
            IF( MFL.EQ.0 ) VS=O(O(46)-1)
            IF ((O(46).EQ.3725)) THEN
               CALLMESAGE(14, 17, 0,0)
               GOTO280
            END IF
            NUL=O(O(46))
            O(46)=O(46)-1
            NUL=O(O(46))
            O(46)=O(46)-1
            CS=O(O(46))
            O(46)=O(46)-1
            GOTO360
310         P=P+2
            MFL=0
            IF ((O(P-2).LE.O(VS)).AND.(O(VS).LE.O(P-1))) THEN
               MFL=1
               VS=VS+1
               IF(VS.GT.U)GOTO160
            END IF
            GOTO360
340         WS=VS
            MFL=1
            IF (O(P).EQ.O(76)) THEN
370            IF(O(WS).NE.O(76).OR.O(P).NE.O(76))GOTO371
                  WS=WS-1
                  P=P+1
               GOTO 370
371            CONTINUE
            END IF
380         IF(O(P).EQ.O(106))GOTO381
               IF(O(P).NE.O(WS).OR.WS.GT.U) MFL=0
               P=P+1
               WS=WS+1
            GOTO 380
381         CONTINUE
            P=P+1
            IF(MFL.EQ.1)VS=WS
360         IF(MFL.EQ.1)GOTO260
            IF(O(P).NE.O(101) .AND. O(P).NE.O(104))GOTO280
         ELSE
            CALLMESAGE(14, 19, P,Q+11)
            GOTO280
         END IF
      GOTO 260
261   CONTINUE
      IF (.NOT.(O(45).EQ.3675)) THEN
         P=O(O(45))
         O(45)=O(45)-1
         Q=O(O(45))
         O(45)=O(45)-1
         GOTO260
      END IF
CSUCCESSFULL
      O(74)=1
      V=VS
      IF(O(29).GE.6) CALLMACTRC(6,PP,QQ)
      GOTO150
280   O(74)=0
      GOTO100
      END
*CMZ :          06/11/97  12.11.44  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      SUBROUTINE EXPAND (F)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
C F is the number of the macro arguments
      EQUIVALENCE(O(1),B),(O(2),E),(O(3),R),(O(4),S), (O(5),V),(O(6),U),
     *(O(7),W),(O(8),X)
      DATA KGN/0/,SEF/0/
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
*CMZ :  1.00/00 19/09/95  15.17.28  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      SUBROUTINE KAT
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      EQUIVALENCE(O(1),B),(O(2),E),(O(3),R),(O(4),S), (O(5),V),(O(6),U),
     *(O(7),W),(O(8),X)
      EQUIVALENCE (L,O(47)),(M,O(48))
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
150   IF(O(B).NE.1.AND.O(B).NE.0)STOP18
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
*CMZ :          05/08/98  23.28.15  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      SUBROUTINE DEFINE(S)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      DIMENSION L(3)
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
      IF(NEED.GT. 193042-O(34)) CALLKRUNC(NEED)
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
      O(60)=193042-O(34)
      IF (O(60) .LE. 100) PRINT *,' Geant3 warning: only ',O(60),
     *                    ' memory left, you may get a problem'
      GOTO 99999
50    NEED=L(1)+L(3)+2*6
      IF (NEED.GT.193042-O(34)) CALLKRUNC(2+NEED)
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
      O(60)=193042-O(34)
      O(61)=(100*O(60))/(193042-4175)
      IF (O(60) .LE. 100) PRINT *,' Geant3 warning: only ',O(60),
     *                    ' memory left, you may get a problem'
99999 RETURN
      END
*CMZ :  1.00/00 19/09/95  15.17.28  by  Pavel Nevski
*-- Author :
C----------------------------------------------------------------
      SUBROUTINE DOIT(A,Z)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      DIMENSION S(900)
      X=0
      T=Z
20    IF(T.LT.A)GOTO21
         IF (O(T).EQ.O(95)) THEN
            IF (O(T-1).EQ.O(94)) THEN
               S(X+1)=T+1
               S(X+2)=O(T)
               X=X+2
               T=T-1
            ELSE
               S(X+1)=T+1
               X=X+1
30             IF(O(T).EQ.O(107).AND.O(T+1).EQ.25)GOTO31
                  T=T-1
                  IF(T.LT.A)GOTO 99999
               GOTO 30
31             CONTINUE
               P=T+6
               O(P-1)= S(X)-P
               X=X-1
               IF (S(X).EQ.O(95)) THEN
                  O(P-4)= S(X-2)-P
                  O(P-3)= S(X-1)-P
                  O(P-2)= O(P-3)-2
                  X=X-3
               ELSE
                  O(P-2)=S(X)-P
                  O(P-3)=S(X)-P
                  O(P-4)=S(X)-P
                  X=X-1
               END IF
            END IF
         ELSE IF (O(T).EQ.O(94)) THEN
            S(X+1)= T
            X=X+1
         END IF
         T=T-1
      GOTO 20
21    CONTINUE
99999 RETURN
      END
*CMZ :          05/08/98  23.29.02  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      SUBROUTINE KRUNC(R)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      IF (O(35).NE.0) THEN
         M=O(35)
20       IF(M.EQ.O(34))GOTO21
            IF (O(M+2).GE.0) THEN
               IF(O(M).NE.0)O((O(M))+1)=O(M+1)
               O(O(M+1))=O(M)
            END IF
            O(M+2)=O(90)
            M=O(M+3)
         GOTO 20
21       CONTINUE
      END IF
      N=4175
      M=O(N+3)
      D=0
      O(O(34))=0
30    IF(N.EQ.O(34))GOTO31
         M=N
         N= O(M+3)
C    P=predecessor S=successor C=concatenation
         P=O(M+1)
         S=O(M)
         C=O(M+2)
         IF (C.EQ.O(90)) THEN
            D=D+ N-M
            GOTO30
         END IF
         IF (S.GT.0) O(S+1)=O(S+1)-D
         IF (C.GT.0) O(N+1)=O(N+1)-D
         O(P)=O(P)-D
         O(M+3)=O(M+3)-D
C   MOVE IT
         DO 41 I = M,N
            O(I-D)=O(I)
41       CONTINUE
      GOTO 30
31    CONTINUE
      FRE=O(34)
      O(34)=O(34)-D
      O(O(34))=0
      IF (R.GT.D)CALLMESAGE(15, 6, 0,0)
      O(60)=193042-O(34)
      O(61)=(100*(193042-FRE))/(193042-4175)
CPN: control space in macro buffer here ?
      IF(R.EQ.0)CALLMESAGE(13,1,D,0)
      RT=100*(193042-FRE)/(193042-4175)
      IF (RT .LE. 5) PRINT *,' Geant3 WARNING: only ',RT,
     *      '% of buffer left, you may run in a problem '
99999 RETURN
      END
*CMZ :  1.00/00 19/09/95  15.17.28  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      FUNCTION NIB(L,B)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      M=1
      N=0
20    IF(O(L).NE.O(76))GOTO21
         L=L-1
      GOTO 20
21    CONTINUE
30    IF(0.GT.O(L).OR.O(L).GE.B)GOTO31
         N=N+O(L)*M
         M=M*B
         L=L-1
      GOTO 30
31    CONTINUE
      IF (O(L).EQ.42) THEN
         L=L-1
         N=-N
      END IF
      NIB=N
99999 RETURN
      END
*CMZ :  1.00/00 19/09/95  15.17.28  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      FUNCTION LLONG(LOC,NUM,B,IND)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      IF(O(68).NE.0)GOTO 99999
      N=IABS(NUM)
      L=LOC
      I=N/B
      J=1
      IF (IND.EQ.0) THEN
         L=L+1
         O(L)=O(76)
      END IF
20    IF(I.LE.0)GOTO21
         I=I/B
         J=J+1
      GOTO 20
21    CONTINUE
CCOUNT DIGITS IN EXPANSION
      IF (NUM.LT.0) THEN
         L=L+1
         O(L)=42
      END IF
      LLONG=L-LOC+J
30    CONTINUE
         K=N/B
         O(L+J)=N-K*B
         N=K
         J=J-1
         IF(J.LE.0)GOTO31
      GOTO 30
31    CONTINUE
99999 RETURN
      END
*CMZ :  1.00/00 19/09/95  15.17.28  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      FUNCTION N5(L,M,K)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      J=537+4
      N5=NIB(J,10)
      IF (L.GT.N5.OR.N5.GT.M) THEN
         CALLMESAGE(14, 3, 0,0)
         N5=K
      END IF
99999 RETURN
      END
*CMZ :  1.00/00 19/09/95  15.17.28  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      FUNCTION USER(A,Z)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      CHARACTER*132 IN,OUT
      DO 21 K = A,Z
         IN(K-A+1:K-A+1)=CHAR(O(769+O(K)))
21    CONTINUE
      LENOUT=LEN(OUT)
      STATUS=MUSER(IN(2:Z-A),OUT,LENOUT)
      IF (STATUS.NE.1) THEN
         CALLMESAGE(14, 30, A,Z+1)
         Z=A
         O(A)=36
      ELSE
         Z=A+LENOUT-1
         DO 31 K = A,Z
            O(K)=O(969+ICHAR(OUT(1+K-A:)))
31       CONTINUE
      END IF
      USER=1
99999 RETURN
      END
*CMZ :          04/11/97  02.51.29  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      FUNCTION LEXP(A,Z)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      DIMENSION P(100)
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
*CMZ :  1.00/00 19/09/95  15.17.28  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      FUNCTION ISPEC(PP,QQ,VV,IU,T)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
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
            IF(C.GE.O(P-2))M=1
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
*CMZ :          04/11/97  02.51.29  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      SUBROUTINE MACTRC(W,R,S)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      IF ( O(23).GT.0 .AND. O(29).LT.3 .AND. W.NE.4)GOTO 99999
      IF (W.EQ.2) THEN
         CALLDUMDUM(255,45,37,37,37,22,42,29,27,34,47)
         O(255+4)=(O(32)-1225)/120
         CALLCRIT(R,S)
      ELSE IF (W.EQ.4) THEN
         CALLDUMDUM(255,47,45,28,29,27,10,23,16,14,47)
         CALLCRIT(R,S)
      ELSE IF (W.EQ.5) THEN
         CALLDUMDUM(255,47,37,10,16,10,18,23,28,29,47)
         CALLCRIT(R,S)
      ELSE IF (W.EQ.6) THEN
         CALLDUMDUM(255,47,37,24,20,47,28,25,14,12,47)
         CALLCRIT(R,S)
      END IF
      IF (W.EQ.33) THEN
         CALLDUMDUM(255,47,47,27,14,13,30,12,14,13,47)
         IF(O(23).GT.0) O(255+1)=O(23)
         CALLCRIT(R+1,S+1)
      ELSE IF (W.EQ.15) THEN
         CALLDUMDUM(255,47,47,13,14,15,18,23,14,13,47)
         CALLCRIT(R+6,O(R+3))
         IF (R+6.LT.O(R+3)) THEN
            CALLDUMDUM(255,47,47,47,47,31,10,21,30,14,47)
            O(255+2)=O(20)
            CALLCRIT(S+6,O(S+3))
         ELSE
            CALLDUMDUM(255,47,23,30,21,21,47,31,10,21,47)
            CALLCRIT(S,S)
         END IF
      ELSE IF (W.EQ.12) THEN
         CALLDUMDUM(255,47,10,25,25,14,23,13,14,13,47)
         CALLCRIT(O(S-2*(3)-1),O(S-2*(3)-2) )
         CALLDUMDUM(255,47,47,47,47,47,47,47,29,24,47)
         CALLCRIT(O(S-2*(1)-1),O(S-2*(1)-2) )
      ELSE IF (W.EQ.13) THEN
         CALLDUMDUM(255,47,47,27,14,22,24,31,14,13,47)
         CALLCRIT(O(S-2*(1)-1),O(S-2*(1)-2) )
      ELSE IF (W.EQ.22) THEN
         CALLDUMDUM(255,47,47,22,10,29,12,17,14,13,47)
         IF (O(R+6).LT.0) GOTO 99999
         O(255+1)=(O(32)-1225)/120
         CALLCRIT(R+6,O(R+3) )
         K=O(S-1)
         IF (K.NE.0) THEN
            DO 21 M = 1,K
               CALLDUMDUM(255,47,47,47,47,10,27,16,47,47,47)
               O(255+8)=M
               T=O(S-2*(M)-1)
               N=O(S-2*(M)-2)
               IF (T.NE.N) CALLCRIT(T,N)
21          CONTINUE
         END IF
      END IF
99999 RETURN
      END
*CMZ :          04/11/97  02.51.29  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      SUBROUTINE CRIT(A,Z)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      DIMENSION TITO(9)
      DATA SH/35/
      B=A
20    IF(B.GE.Z)GOTO21
         C=255+9
         IF (O(255).EQ.47) THEN
            C=C+SH
         END IF
C    PLN - dense tracing
30       IF(C.GE.334.OR.B.GE.Z)GOTO31
            C=C+1
            O(C)=O(B)
            B=B+1
            IF(O(C).EQ.O(76))O(C)=37
            IF(O(C).GT.100+11) O(C)=100
            IF(O(C).EQ.-1) O(C)=O(82)
            IF(O(C).EQ.-2) O(C)=O(83)
            IF(O(C).EQ.-3 .OR.O(C).EQ.-4)O(C)=91
         GOTO 30
31       CONTINUE
         IF (O(255).EQ.47) THEN
            DO 41 C1 = 255+1,255+9
               O(C1+SH)=O(C1)
               O(C1)=TITO(C1-255)
41          CONTINUE
            CALLRW(33,O(56),255,C)
            DO 51 C1 = 255,334
               O(C1)=47
51          CONTINUE
            GOTO 99999
         ELSE
            DO 61 C1 = 255+1,255+9
               TITO(C1-255)=O(C1)
61          CONTINUE
         END IF
      GOTO 20
21    CONTINUE
99999 RETURN
      END
*CMZ :          12/11/97  16.26.54  by  Pavel Nevski
*CMZU:  1.00/01 06/12/95  00.55.09  by  Pavel Nevski
*CMZ :  1.00/00 06/10/95  14.09.54  by  Pavel Nevski
*-- Author :
C-----------------------------------------------------------------------
C-------------
      SUBROUTINE MESAGE (LEVEL,NO,K1,K2)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      IF (LEVEL.EQ.14 .OR. LEVEL.EQ.15) O(17)=O(17)+1
      CALLFIND(LEVEL,NO,K1)
      IF(K2.NE.0)CALLMACTRC(4,K1,K2)
      IF (LEVEL.EQ.13) THEN
         CALLFIND(13,2,O(60))
         CALLFIND(13,3,O(61))
      END IF
      IF (LEVEL .LE.14) THEN
         IF(O(17).LT.100)RETURN
         CALLFIND(15,10,0)
      END IF
      IF (O(55).EQ.O(86).OR. O(55).EQ.O(87)) THEN
         O(17)=O(17)+1
         IF(O(55).EQ.O(86)) CALLFIND(16,5,0)
         IF(O(55).EQ.O(87)) CALLFIND(16,4,0)
      END IF
      IF (O(47).GT.3775.OR.O(48).GT.3825) O(17)=O(17)+1
      IF (O(48).GT.3825) THEN
         CALLFIND(16,3,O(48)-3825)
      END IF
20    IF(O(47).LE.3775)GOTO21
         CALLFIND(16,2,O(O(47)))
         O(47)=O(47)-1
      GOTO 20
21    CONTINUE
      CALLFIND(13,3,O(61))
      STATUS=MUSER('EXIT','    ',0)
      IF (STATUS.NE.1) THEN
         O(17)=O(17)+1
      END IF
      IF (O(17).EQ.0) THEN
         CALLFIND(16,6,0)
      ELSE
         CALLFIND(16,7,O(17))
         STOP 'Geant3 compilation error(s) occured'
      END IF
C     CALL TIMEX(TT); PRINT *,' elapsed time = ',TT
      STOP 'GEANT3 parser terminated normally'
      END
*CMZ :  1.00/00 19/09/95  15.17.28  by  Pavel Nevski
*-- Author :
C-----------------------------------------------------------------------
C---------------
      SUBROUTINE FIND(L,N,V)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      A=255
      C=A+9
      Q=1945+L
20    IF (O(Q).NE.0) THEN
         Q=O(Q)
         IF(O(Q+7).NE.N)GOTO20
         R=O(Q+2)+6
         S=O(O(Q+2)+3)
30       IF(R.GE.S)GOTO31
            C=C+1
            O(C)=O(R)
            R=R+1
         GOTO 30
31       CONTINUE
      ELSE
         C=C+LLONG(C,N,10,0)
      END IF
      IF(L.EQ.15) CALLDUMDUM(255,47,45,45,45,15,10,29,10,21,47)
      IF(L.EQ.14) CALLDUMDUM(255,47,45,32,10,27,23,18,23,16,47)
      IF (L.EQ.13.OR.L.EQ.16) THEN
         CALLDUMDUM(255,47,47,47,47,47,47,47,47,47,47)
         IF(V.NE.0) NERD=LLONG(A,V,10,0)
      END IF
      CALLRW(L,O(56),A,C)
      DO 41 K = A,C
         O(K)=O(76)
41    CONTINUE
      C=255+8
99999 RETURN
      END
*CMZ :  1.00/00 19/09/95  15.17.28  by  Pavel Nevski
*-- Author :
C-----------------------------------------------------------------------
C------------
      FUNCTION LINF(A,Z,IND)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      DATA LAST/0/,KUR/0/,KIT/1/,KUT/0/
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
C non-empty output line
      I=C
50    IF(B.GT.Z)GOTO51
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
            K=LLONG(327,O(19)-1,10,0)
         END IF
         DO 81 K = 255,326
            IF(O(K).EQ.O(105))KST=1
            IF(O(K).EQ.O(106))KST=0
81       CONTINUE
C    BREAK LINES ONLY AT <NICE> PLACES
         IF (I.GT.326) THEN
            IF (IND.EQ.15 .AND. KST .EQ. 0) THEN
90             IF(O(I-1).GE.38)GOTO91
                  B=B-1
                  I=I-1
                  O(I)=O(76)
               GOTO 90
91             CONTINUE
            END IF
         END IF
C   LOOKOUT
         CALLRW(33,O(57),255,334)
         DO 101 K = 255,334
            O(K)=O(76)
101      CONTINUE
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
      IF(IND.EQ.12) O(A)=O(96)
      LAST=KUR
99999 RETURN
      END
*CMZ :  1.00/00 19/09/95  15.17.28  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      SUBROUTINE DUMDUM(L,A,B,C,D,E,F,G,H,I,J)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      O(L)=A
      O(L+1)=B
      O(L+2)=C
      O(L+3)=D
      O(L+4)=E
      O(L+5)=F
      O(L+6)=G
      O(L+7)=H
      O(L+8)=I
      O(L+9)=J
99999 RETURN
      END
*CMZ :          30/04/98  15.39.15  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      SUBROUTINE CCCARD(K)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      CHARACTER EXT*2, CL*120, C0*1, C1*1, CC*2
      CHARACTER*160 FILE,PILE
      COMMON /NAMEF/ N,FILE
      JMP=K
      GOTO(20,30,40,50,60,70,60,60,80,60,60,90,100,110,60,60,120,130,
     *140,150,160,60,60,60,60,60),JMP
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
      DO 171 R = 405,O(38)
         C0=C1
         C1=CHAR(O(R))
         IF(C1.EQ.'; ')GOTO171
         K0=K0+1
         CL(K0:K0)=C1
         IF (C0.EQ.'  ' .AND. C1.NE. '  ') K1=K0
         IF (C0.NE. '  ' .AND. C1.EQ.'  ') K2=K0
171   CONTINUE
      IF (K2.EQ.0 .OR. K1.EQ.0 .OR. K1.GT.K2) GOTO 180
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
190   OPEN (O(9),FILE=PILE(1:M) , STATUS='OLD',FORM='FORMATTED',ERR=200)
      GOTO 99999
200   OPEN (O(9),FILE=PILE(1:M)//'.g' , STATUS='OLD',FORM='FORMATTED',
     *ERR=210)
      GOTO 99999
210   OPEN (O(9),FILE=PILE(1:M)//'.h' , STATUS='OLD',FORM='FORMATTED',
     *ERR=220)
      GOTO 99999
220   OPEN (O(9),FILE=PILE(1:M)//'.inc', STATUS='OLD',FORM='FORMATTED',
     *ERR=230)
      GOTO 99999
230   IF (PILE(1:1).EQ.'/ ') GOTO 180
240   OPEN (O(9),FILE='../inc/'//PILE(1:M),STATUS='OLD',FORM='FORMATTED'
     *,ERR=250)
      GOTO 99999
250   OPEN (O(9),FILE='../inc/'//PILE(1:M)//'.g', STATUS='OLD',FORM='FOR
     *MATTED',ERR=260)
      GOTO 99999
260   OPEN (O(9),FILE='../inc/'//PILE(1:M)//'.h', STATUS='OLD',FORM='FOR
     *MATTED',ERR=270)
      GOTO 99999
270   OPEN (O(9),FILE='../inc/'//PILE(1:M)//'.inc',STATUS='OLD',FORM='FO
     *RMATTED',ERR=180)
      GOTO 99999
180   IF (CC.NE.'[]')WRITE(O(58),*)'*WARNING! INCLUDE FILE NOT FOUND: '
     *,CL(1:K0)
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
C+SELF,IF=UNIX.
      EXT='.a'
      EXT(2:2)=CHAR(ICHAR(EXT(2:2))+O(9)-1)
280   OPEN (O(9),FILE=FILE(1:N)//EXT ,STATUS='OLD')
C+SELF.
      GOTO 99999
60    CALLMESAGE(14, 22, 0,0)
99999 RETURN
      END
*CMZ :          13/12/97  15.00.11  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      SUBROUTINE INITAL(I1,I2,I3,I4,I5,I6,I7)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      EQUIVALENCE(O(1),B),(O(2),E),(O(3),R),(O(4),S), (O(5),V),(O(6),U),
     *(O(7),W),(O(8),X)
      CHARACTER*160 FILE,FF,FFO,FFL,FFE
      COMMON /NAMEF/ NN,FILE
C PLN - 20-mar-93   -    this was for writing binary out - not used in m
Cy version
C IF I3<0 {MODE=1;SEQN=5;KOUNT=FREE+1;FRT=0;UNIT=5;
C
C           .(1)=0;.(2)=KOUNT; WRITE(MINP) (O(K),K=1,KOUNT);
C
C     QUIT;}
      CHARACTER*2 CL
      DO 21 K = 8,256000
         O(K)=0
21    CONTINUE
      O(9)=I1
      IUNI=I1
      O(56)=I2
      O(57)=I3
      O(58)=I4
      O(14)=I5
      O(16)=I6
      O(37)=I7
      O(41)=O(37)
      O(38)=404+O(37)
      O(36)=536+O(37)
      O(43)=536+O(41)
      O(42)=404+O(41)
C+SELF,IF=UNIX.
      J=0
      FF='-i'
30    CONTINUE
         J=J+1
         CL=FF
         CALLGETARG(J,FF)
         I=LENOCC(FF)
         IF(I.EQ.0)GOTO31
         N=I
         DO 41 K = 1,I
            IF (FF(K:K).EQ.'. ') N=K-1
41       CONTINUE
         IF (CL.EQ.'-i') THEN
            FILE=FF
            NN=N
            FFO=FF(1:N)//'.f'
            FFL=FF(1:N)//'.l'
            FFE=FF(1:N)//'.e'
         END IF
         IF (CL.EQ.'-o') THEN
            FFO=FF(1:I)
            FFL=FF(1:N)//'.l'
            FFE=FF(1:N)//'.e'
         END IF
         IF (CL.EQ.'-l') THEN
            O(56)=12
            FFL=FF(1:N)//'.l'
         END IF
         IF (CL.EQ.'-e') THEN
            O(58)=13
            FFE=FF(1:N)//'.e'
         END IF
         IF (FF.EQ.'-h') THEN
            STOP 'usage: geant3 [-i] file[.g] [-o,e,l file]'
         END IF
      GOTO 30
31    CONTINUE
      OPEN (O(9),FILE='geant3.def', STATUS='OLD')
      OPEN (O(57),FILE=FFO, STATUS='UNKNOWN')
      IF (O(56).NE.6) OPEN (O(56),FILE=FFL, STATUS='UNKNOWN')
      IF (O(58).NE.6) OPEN (O(58),FILE=FFE, STATUS='UNKNOWN')
C+SELF.
      CALLRAW
      O(32)=1705
      DO 51 K = 1,6
         O(K)=224521
51    CONTINUE
      O(40)=224521
      O(55)=O(76)
      O(113)=113
      O(75)=-13
      O(70)=-14
      O(U)=O(79)
      W=193043-1
      O(W)=O(79)
      CALLDUMDUM(44,3625,3675,3725,3775,3825,3875,3975,4025,0,0)
      CALLNXTCRD
99999 RETURN
      END
*CMZ :          30/04/98  06.06.00  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      SUBROUTINE RAW
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      EQUIVALENCE(O(1),B),(O(2),E),(O(3),R),(O(4),S), (O(5),V),(O(6),U),
     *(O(7),W),(O(8),X)
      DIMENSION P(100),BOO1(0:26),BOO2(0:26)
      EQUIVALENCE(P(1),O(4075))
      DATA K1/24000/
      DATA (BOO1(K),K=0,26)/26,24,24,24,24,8,0,3,38,107,1,1,0,46,107,1,
     *1,0,38,107,1,1,0,46,107,24,0/
      DATA (BOO2(K),K=0,26)/26,24,24,24,24,8,0,3,-1,107,0,0,0,-2,107,0,
     *0,0,-1,107,0,0,0,-2,107,24,0/
20    IF(O(769).EQ.ICHAR('0 '))GOTO21
         CALLRW(0,O(9),769,769+114)
      GOTO 20
21    CONTINUE
C  wN M(R1,R1+21);  wN F(R1,R1+21);
CMAKE INPUT AND OUTPUT SAME
      DO 31 K = 769,868
         O(K-100)=O(K)
31    CONTINUE
      DO 41 K = 1,100
         J=O(669+K-1)
         O(969+J) = K-1
41    CONTINUE
      O(31)=4
      O(34)=4175
      DO 51 K = 1,10
         O(K+100)=K+100
         O(K+110)=K+110
         O(100-K)=-K
51    CONTINUE
C              BLANK BAT BLB TRM LPR RPR LSB RSB LCB RCB
      CALLDUMDUM(76, 47, 81, 80, 79, 38, 46, 78, 84, 85, 86)
      O(86)=48
      O(87)=82
      DO 61 K = 0,5
         BOO1(K) = BOO1(K)+K1
         BOO2(K) = BOO2(K)+K1+100
61    CONTINUE
      DO 71 K = 0,26
         O(K+K1) = BOO1(K)
         O(K+100+K1) = BOO2(K)
71    CONTINUE
      CALLDEFINE(K1+8)
      CALLDEFINE(K1+108)
      P(46)=101
      P(38)=202
      P(89)=303
      P(90)=404
      P(91)=505
      P(42)=706
      P(43)=707
      P(44)=808
      P(45)=809
      P(47)=910
      P( 32)=303
      P( 12)=404
      P( 29)=505
      P( 11)=611
      P( 14)=612
      P( 16)=613
      P( 18)=614
      P( 23)=615
      P( 30)=616
      O(31)=6
99999 RETURN
      END
*CMZ :  1.30/00 08/05/96  22.31.52  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      SUBROUTINE RW(WICHIN,DFILE,FROM,TO)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      CHARACTER*155 INCH(5),OUCH,SHAD
      CHARACTER*66 STARS
      CHARACTER*1 TAB
      INTEGER STF/0/,LIL/0/,TABI,BUFI(200),EVIL(200),PV(5)/5*0/,LM(5)/5*
     *0/,LINE(100)/100*0/
      DATA STARS (1:66)/' **********************************************
     *******************'/
C STATEMENT FUN
      ROUND(K)=MOD(K,5)+1
C
      IBK=ICHAR('  ')
      LINL = MAX(TO-FROM+1,0)
      IF( WICHIN.NE.0) GOTO 20
      LIL=ROUND(LIL)
      LM(LIL)=LINL
      INCH(LIL) (1:LM(LIL))=';   '
      TABI=9
      READ(DFILE,'(A)', END=30) INCH(LIL) (1:LM(LIL))
      IF (DFILE.LT.100) LINE(DFILE)=LINE(DFILE)+1
      O(19)=LINE(DFILE)
      TAB=CHAR(TABI)
      K=FROM-1
      DO 41 P = 1,LM(LIL)
         IF(K.GE.TO)GOTO41
         K=K+1
         O(K)=ICHAR(INCH(LIL)(P:P))
         IF (O(K).EQ.TABI) THEN
C       {K=K,MIN((FROM+((K-FROM)/8+1)*8),TO);
            O(K)=IBK
C      }
         END IF
41    CONTINUE
      DO 51 K = K+1,TO
         O(K)=IBK
51    CONTINUE
      GOTO 99999
20    WICH=WICHIN
      IF(12.LT.WICH .AND. WICH.LT.19 )WICH=33
      CX = 0
CLAST NONBLANK ->
      LAN = 0
      DO 61 L = 1,LINL
         QC = O(FROM+L-1)
         BUFI(L)=QC
         IF (WICH.EQ.33) THEN
            SHAD(L:L) = '  '
            IF (DFILE.EQ.O(57)) THEN
               IF (QC.EQ.O(105)) THEN
                  QC=O(86)
                  STF=1
               END IF
               IF (QC.EQ.O(106)) THEN
                  QC=O(86)
                  STF=0
               END IF
               IF (STF.EQ.0 .AND. BUFI(1).NE.12) THEN
C             PLN correction 20-mar-93 - output in fortran only in capit
C            al
                  IF (QC.LT.0 .OR. QC.GT.50) THEN
                     IF (QC.LT.78) THEN
                        QC=QC-42
C                   .(FROM+L-1) = QC;
                     ELSE
                        CX=CX+1
                        EVIL(CX) = QC
                        SHAD(L:L)='| '
                     END IF
                  END IF
               END IF
            END IF
            QC=O(QC+769)
         END IF
         OUCH(L:L)=CHAR( QC )
         IF (QC .NE. IBK) LAN=L
61    CONTINUE
C- Nev - 15/04/96 - include statement is produced as in C with .inc exte
Cntion
      IF (OUCH(1:10).EQ.'C include ') THEN
         OUCH(1:10) ='#include "'
         DO 71 L = 11,LAN
            IF(OUCH(L:L).EQ.'  ')GOTO80
71       CONTINUE
         L=LAN+1
80       OUCH(L:L+4)='.inc"'
         LAN=MAX(LAN,L+4)
         CALL CUTOL(OUCH(1:LAN))
C- NEV - 19/03/93 - switch listing off !
      END IF
      IF (DFILE .NE. O(56) .OR. O(21) .NE. 0) THEN
         I=1
C   If (DFILE==MOUT & LAN>12) I=12;
         WRITE(DFILE,'(A)') OUCH(I:LAN)
      END IF
      IF (CX.GT.0) THEN
         WRITE (O(58),'(1X,A)')OUCH(1:LAN)
         WRITE (O(58),'(1X,A)')SHAD(1:LAN)
         WRITE (DFILE,'(A)')SHAD(1:LAN)
         IF (DFILE.EQ.O(57)) THEN
            WRITE(DFILE,'('' Illegal FORTRAN characters '')')
            WRITE(O(58), '(''  geant error detected in the following lin
     *es of the source file:'')')
            O(17)=O(17)+1
            DO 91 K = 1,5
               PV(K) = ABS ( ROUND(LIL+K-1) )
91          CONTINUE
            WRITE(O(58),'(1X,A)') STARS
            DO 101 K = 1,5
               WRITE(O(58),'(I5,1X,A)') O(19)+K-5,INCH(PV(K))(1:LM(PV(K)
     *         ))
101         CONTINUE
            WRITE(O(58),'(1X,A)') STARS
         END IF
      END IF
      IF (WICHIN.EQ.14 .OR. WICHIN.EQ.15) THEN
         DO 111 K = 1,5
            PV(K) = ABS ( ROUND(LIL+K-1) )
111      CONTINUE
         WRITE(O(58),'(1X,A)') STARS
         DO 121 K = 1,5
            WRITE(O(58),'(1X,A)') INCH(PV(K)) (1:LM(PV(K)))
121      CONTINUE
         WRITE(O(58),'(1X,A)') STARS
         WRITE(O(58),'(1X,A)') OUCH(1:LAN)
         WRITE(O(57),'(A)') OUCH(1:LAN)
      END IF
      GOTO 99999
30    O(52)=1
99999 RETURN
      END
*CMZ :  1.00/00 19/09/95  15.17.28  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      FUNCTION MUSER(IN,OUT,LENOUT)
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      CHARACTER*132 IN,OUT
C ARGUMENTS are: Input string, Output string, Output lengt=
      MUSER=1
99999 RETURN
      END
 
*CMZ :          30/04/98  06.39.37  by  Pavel Nevski
*-- Author :
C------------------------------------------------------------------
      SUBROUTINE NXTCRD
      IMPLICIT INTEGER (A-Z)
      DIMENSIONO(256000)
      COMMON /NAMO/O
      DATA LL/0/,KEEP/0/
      INTEGER FS(8)/8*0/
      CHARACTER*8 CF/'   '/
      DATA SS/0/,FRC/0/
      O(62)=0
      IF(O(52).EQ.2) CALLMESAGE(16,1,0,0)
      IBK=ICHAR('   ')
20    IF (SS.EQ.0) THEN
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
            IF (O(15).EQ.0 .AND. KEEP.GT.0 .AND. O(55).EQ.85) THEN
               KEEP=KEEP-1
               O(537)=86
               LL=0
               FRC=0
               GOTO40
            END IF
            GOTO20
         END IF
C       this is a BREAK-LINE condition only   -  insert a ';;' line and
C   wait
         F1=O(969+O(405))
         F2=O(969+O(405+1))
         IF (FRC.EQ.0 .AND. ( F1.EQ.45.OR.F1.EQ.80.OR. (F1.EQ.83.OR.F1.
     *   EQ.12.OR.F1.EQ.54 ) .AND. (37.LE.F2).AND.(F2.LE.50))) THEN
            O(12)=537
            O(237)=O(76)
            DO 51 T = 538,O(43)
               O(T)=47
51          CONTINUE
            O(537)=O(79)
            O(538)=O(79)
            SS=1
            GOTO 99999
         END IF
      END IF
      SS=0
C Shift line to D positions to generate ';'(TRM) automatically
      D=MAX(O(43)-O(36),0)
      IF (O(969+O(405)).EQ.50.OR. O(15).GT.0) D=0
      DO 61 K = 537,537+D
         O(K)=47
61    CONTINUE
      BF=537-405+D
      R2=405
      TF=0
      MF=0
      CALLVZERO(FS,8)
      CF='  '
C convert to internal code and get first four letters in capital (FS)
      DO 71 K = 405,O(38)
         T=O(969+O(K))
         O(K+BF)=T
         IF (O(K).NE.IBK) THEN
            R2=K
            MF=MIN(MF+1,8)
            IF ((52.LE.T).AND.(T.LE.72)) T=T-52+10
            FS(MF)=T
            CF(MF:MF)=CHAR(O(K))
C      TF is the 6th symbol
            IF (MF.EQ.1 .AND. K-405.EQ.5) TF=T
         END IF
71    CONTINUE
C+SEQN;
      O(12)=537
      O(237)=O(76)
      CALLCLTOU(CF)
      FRC=0
Cprevious last symbol
      LO=LL
C new last symbol
      TL=R2+BF
C pure comment line
      LL=O(TL)
      IF (O(538).EQ.45.OR.O(538).EQ.80.OR.(O(538).EQ.83.OR.O(538).EQ.12.
     *OR.O(538).EQ.54 ) .AND. (37.LE.O(538+1)).AND.(O(538+1).LE.50))
     *THEN
         FRC=1
         O(12)=O(43)
         LL=1
         IF(O(25).EQ.0)GOTO20
         GOTO40
      END IF
      IF (D.GT.0) THEN
C    Recognize complex continuation lines
C    first, remove the explicit continuation sign from the current line
         IF ( O(TL).EQ.37 .OR. O(TL).EQ.93 ) O(TL)=47
C    possible continuations: $_(,.=-+/*) ... <>|&^?\
         O(537)=O(79)
C   FORTRAN continuation (6th symbol): ,.=-+ >|& or </* preceeded by ,.=
C   -+
C   F
         IF (TF.GT.35) THEN
            IF (((39.LE.TF).AND.(TF.LE.43)) .OR. (88.LE.TF).AND.(TF.LE.
     *      90)) THEN
               O(537)=47
               O(537+6)=47
C         F
            ELSE IF ((((44.LE.TF).AND.(TF.LE.45)).OR.TF.EQ.87) .AND. (
     *      39.LE.LO).AND.(LO.LE.45)) THEN
               O(537)=47
               O(537+6)=47
            END IF
         END IF
C   MORTRAN continuation: line ends with =-+,_\; or starts with { or ELS
C   E after }
         IF (LO.GT.35.OR.FS(1).EQ.78) THEN
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
               KEEP=KEEP-1
            END IF
            IF (CF(1:5).EQ.'+KEEP') THEN
               IF (CF(1:6).EQ.'+KEEP,') THEN
                  O(538)=79
                  O(537+6)=85
                  O(O(36))=40
                  KEEP=KEEP+1
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
         IF (O(55).EQ.48) PRINT *,' Geant Warning: literal expression no
     *t closed in line '
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
     *NE.79 .OR. FRC.EQ.1) THEN
         CALLRW(23,O(57),405,R2)
         GOTO20
      END IF
      IF (O(10).EQ.1) THEN
         NERD=LINF(537,537+(R2-405),10)
      END IF
99999 RETURN
      END
