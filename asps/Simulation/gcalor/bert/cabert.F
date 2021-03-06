* $Id: cabert.F,v 1.1.1.1 2004/01/15 00:08:45 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CABERT(IBERT,FINPUT,NOPART,KIND,ERAY,ARAY,BRAY,GRAY)
C*************************************************************************
C
C calculate collision of particle with nucleus
C
C input: Finput(1) = A of nucleus
C        Finput(2) = Z of nucleus
C        Finput(3) = Ekin of incident particle
C        Finput(4) = energy cut off used in intranuclear cascade default = 0
C        Finput(5) = No. of incident particles = 1.0
C        Finput(6) = Angular distance
C        Finput(7) = particle type a la CALOR + 1
C        Finput(8) = same as Finput(4)
C
C output:NOPART > 0  -> no. of particles generated
C               = 0  -> collision with no escaping particle
C               = -1 -> pseudo collision
C        KIND(1-NOPART) -> particle type
C        ERAY(1-NOPART) -> kinetic energy
C        A,B,GRAY(1-NOPART) -> direction cosine (x,y,z-axis)
C                              incident particle with GRAY = 1 !!!
C
C*************************************************************************
C
      DIMENSION FINPUT(*),KIND(*),ERAY(*),ARAY(*),BRAY(*),GRAY(*)
C
C     A.C.3526(3410-44) CASCADE CALCULATION
C
#include "geant321/crandm.inc"
#include "gcalor/cjoint.inc"
#include "gcalor/cinout.inc"
#include "gcalor/cbert.inc"
#include "gcalor/crn.inc"
#include "gcalor/cmunpu.inc"
#include "gcalor/crun.inc"
      REAL*8 DCLN(80)  , DCIN(115) , PPAC(19)  , POAC(19)  , FMXSN(161),
     +       FMXDN(130), FMXSP(117), PDCI(60)  , PDCH(55)  , DCHN(143) ,
     +       DCHNA(36) , DCHNB(60) , PSPCL(158), PDPCL(130), SPCLN(158),
     +       DPCLN(130), FSLN(176) , FRINN(161), DMIN(101) , PPSCL(117),
     +       PNSCL(117), PMSCL(117), PNNSL(117), PCFSL(234), FRIPN(117),
     +       PNMI(101) , PNFSL(234), PNEC(126) , PNNEC(126), PMXC(126) ,
     +       PMEC(126) , PPEC(126) , PEC(176)  , ECN(176)  , PPDC(6426),
     +       PMDD(6426), PMDX(6426), PNDD(6426)
      DIMENSION ICC(12)
      REAL * 8 ZERO,XINC
      EQUIVALENCE (TAPCRS(1),DCLN(1))    , (TAPCRS(81),DCIN(1))   ,
     +            (TAPCRS(196),PPAC(1))  , (TAPCRS(215),POAC(1))  ,
     +            (TAPCRS(234),FMXSN(1)) , (TAPCRS(395),FMXDN(1)) ,
     +            (TAPCRS(525),FMXSP(1)) , (TAPCRS(642),PDCI(1))  ,
     +            (TAPCRS(702),PDCH(1))  , (TAPCRS(757),DCHN(1))  ,
     +            (TAPCRS(900),DCHNA(1)) , (TAPCRS(936),DCHNB(1)) ,
     +            (TAPCRS(996),PSPCL(1)) , (TAPCRS(1154),PDPCL(1)),
     +            (TAPCRS(1284),SPCLN(1)), (TAPCRS(1442),DPCLN(1)),
     +            (TAPCRS(1572),FSLN(1)) , (TAPCRS(1748),FRINN(1)),
     +            (TAPCRS(1909),DMIN(1)) , (TAPCRS(2010),PPSCL(1))
      EQUIVALENCE (TAPCRS(2127),PNSCL(1)), (TAPCRS(2244),PMSCL(1)),
     +            (TAPCRS(2361),PNNSL(1)), (TAPCRS(2478),PCFSL(1)),
     +            (TAPCRS(2712),FRIPN(1)), (TAPCRS(2829),PNMI(1)) ,
     +            (TAPCRS(2930),PNFSL(1)), (TAPCRS(3164),PNEC(1)) ,
     +            (TAPCRS(3290),PNNEC(1)), (TAPCRS(3416),PMXC(1)) ,
     +            (TAPCRS(3542),PMEC(1)) , (TAPCRS(3668),PPEC(1)) ,
     +            (TAPCRS(3794),PEC(1))  , (TAPCRS(3970),ECN(1))  ,
     +            (TAPCRS(4146),PPDC(1)) , (TAPCRS(10572),PMDD(1)),
     +            (TAPCRS(16998),PMDX(1)), (TAPCRS(23424),PNDD(1))
C
      SAVE
      AMASNO =DBLE(FINPUT(1))
      ZEE = DBLE(FINPUT(2))
      EINC = DBLE(FINPUT(3))
      CTOFE = DBLE(FINPUT(4))
      CASESN = DBLE(FINPUT(5))
      ANDIT = DBLE(FINPUT(6))
      CTOFEN = DBLE(FINPUT(8))
      PRTIN = FINPUT(7)
      KE = 0
      IF(IBERT)40,10,40
C           CHANGED SEPT.1,1987
   10 CONTINUE
CZ BERT dataset already read by CRBERT called by CALINI 19. june 92
      NRT = 0
      SF  =1.
C     SF=1.0 AT PRESENT
      LN=2
C     NOR=RECORD NUMBER
C     NRT=NUMBER OF FILES
      RANDI(1)=16896
      PNMS=.708D13
C     PI+ OR - MASS IN RECIP. CM
      DNCMS=4.758D13
      SQNM=DNCMS*DNCMS
C     NUCLEON MASS SQUARED
C     NUCLEON MASS IN RECIP. CM
      RCPMV=.50613D11
C     RECIPROCAL CM PER MV
      POMS=.684D13
C     PI0 MASS IN RECIP. CM
      IFIVE=5
      ISIX=6
      ZERO=0.0
      NE=0
      BEGRU = 0.0
      DO 20 I=1,3
   20 XI(I)=0.0
      DO 30 I=1,19
         POAC(I)=POAC(I)+POAC(I)
   30 PPAC(I)=PPAC(I)/SF
C     P0AC(19),PPAC(19)
C     SF IS A SCALE FACTOR SUBJECT TO CHANGE
C     ANDIT
C     ISOBAR ANGULAR DISTRIBUTION  0,50 PERCENT ISOTROPIC 50
C     PERCENT FORWARD-BACKWARD-1,ALL ISOTROPIC-2,ALL FORWARD-BACKWARD
C     =0,ALL OF WORD IN CRDET TO BE CONSIDERED.  NOT 0, ONLY PART. INPT
C     ESCAPING PARTICLE STORAGE     ESPS
C     NUMBER OF FORBIDDEN COLLISIONS FOR NEUTRONS     FCN
C     NUMBER OF FORBIDDEN COLLISIONS FOR PROTONS   FCP
C     PARTICLE WITH VELOCITY LESS THAN CRITERION   PLVC
C     PARTICLE WITH VELOCITY GREATER THAN CRITERION   PGVC
   40 DO 50 I=1,60
         IPEC(I)=0
   50 CONTINUE
      I18=0
      I19=0
      DO 60 I=1,2114
         ESPS(I)=0.0
   60 CONTINUE
      DO 70 I= 4515,4849
         ESPS(I)=0.D0
   70 CONTINUE
      DO 80 I=1,10
         COUNT(I)=0.0D0
   80 CONTINUE
      DO 90 I=1,12
         ICC(I)=0
   90 CONTINUE
      SPACE(13)=EINC
      NO=AMASNO
      NMAS=1+(NO-1)*10
      DO 100 I=1,10
         OUT(I)=CRSC(NMAS)
         NMAS=NMAS+1
  100 CONTINUE
      CALL ROUT1
      IF(PRTIN.GT.4) THEN
         CALL CERROR(' BERT called for muon$')
         NOPART=-1
         RETURN
      ENDIF
      NO= PRTIN + 1.
      IF(SPACE(4).GT.100.0) SPACE(4)=100.0
      VALUE1=EINC+SPACE(4)
      IF(NO.LT.3) GOTO 2540
      IF(NO.EQ.4) THEN
         CALL CERROR(' BERT called for pi0$')
         NOPART = -1
         RETURN
      ENDIF
C ----- Charged pions ------
      CALL ROUT2(PPAC(1))
      IF(I1.EQ.0) THEN
         CALL CERROR(' BERT Epion > 2.5 GeV$')
         RETURN
      ENDIF
      IV=2
      IF(NO.EQ.5) IV=0
      CALL CALXYI(1,14,30)
      IP=1
  110 CALL ROUT3
      IF(BEGRU.EQ.0.0) GOTO 2460
C     IF BEGRU=0, LAST RUN COMPLETED--BG6A
      KK=I1
      XINC=XI(1)
C     XINC=X-COORDINATE INC.PART.
      CALL ROUT4
      IF(I1.LT.0.0) GOTO 2820
      I1=KK
  120 IF(IN.NE.0) GOTO 2200
      IF(EX.GT.D(2)) GOTO 650
  130 CURR(2)=OUT(13)
      WKRPN(3)=CURR(2)
C     K.E. WITH RESPECT TOPROTONS RG.3
      WKRPN(6)=OUT(16)
C     K.E. WITH RESPECT TONEUTRONS RG.3
      IFCA=0
  140 CALL CBG6CA(3,3)
  150 IFCC=0
      CALL CABRAN(6)
      KNOT=NOT
      IF(NOT.EQ.4) GOTO 380
      CALL CABG6C(ISW(11))
      VALUE1=RLKE
      IF(IN.NE.0) GOTO 1980
      IF(NOT.EQ.4) GOTO 380
      IF(NOT.LT.4) THEN
         ANY=SPACE(NOT+13)
         GOTO 170
      ELSE
         ANY=S(NOT-4)
      ENDIF
  160 IF(NOT-5)170,600,630
C     IT=1-6  PIPPS(20051),BG129(21011),PIMPD(21004),PIPND(20644)
  170 CALL ROUT5(PPEC(1),PMEC(1),PMXC(1))
C     PPEC(126),PMEC(126),PMXC(126)
  180 IF(CLSM-2.0)880 ,740,190
C     (PIM-P)EXCHANGE SCATTERING CRS.
  190 IF(VALUE1.GT.VALUE2) GOTO 300
  200 IF(ISW(1).NE.0) GOTO 240
      IFC=IFCC+1
      IF(IN.NE.0) GOTO 1990
  210 C(3)=0.0
  220 C(1)=CURR(4)
      C(2)=CURR(5)
      C(3)=C(3)+CURR(6)+EX+D(1)
  230 CONTINUE
      GOTO(960 ,1230,1240,1300,1310,1410,1420,1450,1460,1420,1470,1830,1
     +840 ,1460,1850,1860,1870,1880,1960,1930,1940,1950,2360,2420,2430,2
     +440 ,1420,2450),IT
  240 IF(ISW(2))280,250,280
  250 IFC=2+IFCC
  260 IF(IN)2000,270,2000
  270 C(3)=D(2)+D(3)
      GOTO 220
  280 IFC=3+IFCC
      IF(IN)480,290,480
  290 C(3)=D(2)+D(3)+D(4)+D(5)
      GOTO 220
C     IFC(1-3),BG6C(1502),BG6F(3243),BG6K(4055) COLLISION
  300 CALL SIGNEX
  310 IF(ISW(1))320,120,320
  320 IF(IN)2010,330,2010
  330 IF(EX-D(6))130,130,340
  340 IF(ISW(2))360,350,360
  350 IPEC(7)=IPEC(7)+1
C     NO. OF ESCAPED PARTICLES ON REGION 2
      GOTO 110
  360 IPEC(11)=IPEC(11)+1
C     NO. OF ESCAPED PARTICES ON REGION 1
      GOTO 110
  370 I3=1
      GOTO 390
  380 I3=-1
  390 CALL ROUT6
      IF(I3)400 ,410,450
  400 CALL CERROR(' BERT CURR(1) < 3 or > 5$')
      NOPART=-1
      RETURN
  410 CALL ROUT6A
      IF(CLSM-2.0)910 ,940 ,2180
  420 IFCA=1
  430 IF(ISW(1))440,210,440
  440 IF(ISW(2))290,270,290
C     NON-DEUTERON ABSORPTION
  450 CALL ROUT7
      IF(I3)400 ,460,460
  460 CALL ROUT7A
      I3=I3
      GOTO(480,470 ,590,540,210,290,500,510,270),I3
  470 CALL CERROR(' BERT PWD or NWD < 7$')
      NOPART = -1
      RETURN
  480 VALUE1=EX+D(4)+D(5)
  490 IF(CURR(10)-2.0)500,510,510
  500 C(1)=VALUE1*CURR(7)+CURR(4)
      C(2)=VALUE1*CURR(8)+CURR(5)
      C(3)=VALUE1*CURR(9)+CURR(6)
      GO TO 230
  510 VALUE1=VALUE1+D(3)
      GO TO 520
  520 IF(CURR(10)-2.0)500,500,530
  530 VALUE1=VALUE1+D(2)
      GO TO 500
  540 IF(INC)550,570,550
  550 C(3)=D(2)
      IF(ISW(3))560,220,560
  560 C(3)=C(3)+D(3)+D(4)
      GO TO 220
  570 IF(ISW(3))580,520,580
  580 VALUE1=EX+D(4)
      GO TO 490
  590 VALUE1=EX
      IF(INC)270,490,270
  600 IF(RLKE.GT.2500.0) THEN
         CALL CERROR(' BERT RLKE > 2.5 GeV$')
         RLKE = 2500.0
      ENDIF
      IF(RLKE-180.0)610,610,620
  610 CALL SIGNEX
      IF(CLSM-2.0)850 ,760,310
  620 VALUE1=VALUE1-180.0
      CALL CRJAB(1,PPSCL(1))
C     PPSCL(117)
C     (PIP-P)SINGLE PROD. CRS. LOW ENERGY
      GO TO 180
  630 IF(RLKE.GT.2500.0) THEN
         CALL CERROR(' BERT RLKE > 2.5 GeV (2)$')
         RLKE = 2500.0
      ENDIF
      IF(RLKE-180.0)610,610,640
  640 VALUE1=VALUE1-180.0
      CALL CRJAB(1,PMSCL(1))
C     PMSCL(117)
C     (PIM-P)SINGLE PROD. CRS. LOW ENERGY
      GO TO 180
  650 IF(D(3))670,660,670
  660 IPEC(2)=IPEC(2)+1
C     NO. OF PARTICLES INCIDENT ON REGION 3 ESCAPING
      GO TO 110
  670 ISW(1)=1
      CALL SPAC32(31)
  680 IF(IN)2020,690  ,2020
  690 IF(EX-D(3))710,710,810
  700 IF(IN)720,710,720
  710 CURR(2)=OUT(14)
      WKRPN(2)=CURR(2)
      WKRPN(5)=OUT(17)
C     K.E. FOR PROTONS AND NEUTRONS REGION 2
  720 CALL CBG6CA(2,2)
      GO TO 150
  730 IV=-1
      GO TO 750
  740 IV=0
  750 CALL ROUT8
      I3=I3
      GOTO(760,2030,220,580),I3
  760 IF(ISW(3))770,680,770
  770 IF(EX-D(5))700 ,700 ,780
  780 IF(IN)2040,790  ,2040
  790 CALL SPAC32(32)
  800 IF(EX-D(6))130,130,360
  810 IF(D(4))840 ,820 ,840
  820 CALL SPAC32(32)
  830 IF(EX-D(6))130,130,350
  840 ISW(2)=1
      ISW(3)=1
      CALL SPAC32(30)
  850 IF(IN)2050,860  ,2050
  860 CALL ROUT10
      IF(I3)770,870 ,870
  870 CALL CBG6CA(1,1)
      GO TO 150
  880 IF(VALUE1-VALUE2)890 ,890 ,900
  890 IFC=9+IFCC
      IF(IN)2060,270,2060
  900 CALL SIGNEX
      IF(IN)2050,860  ,2050
  910 IF(IN)930 ,920 ,930
  920 IFCA=6
      GO TO 270
  930 IFCA=9*IABS(I6-2)+13*(I6-1)*(3-I6)
      GOTO2060
  940 IF(IN)2070,950 ,2070
  950 IFCA=7
      GOTO550
  960 I3=1
      GOTO1000
  970 I3=4
      GOTO1000
  980 I3=2
      GOTO1000
  990 I3=3
 1000 CALLROUT11(PPDC(1))
C     PPDCL(378)
      I3=I3
      GOTO(1180,1270,1400,1010),I3
 1010 CST=CRDT(2)-DABS(SNT*(CRDT(2)-CRDT(1)))
 1020 SNT=DSQRT(1.0-CST*CST)
 1030 CALL ROUT12
      IF(I3)1040,1050,1110
 1040 CALL CERROR(' BERT COM < -5E-6$')
      NOPART=-1
      RETURN
 1050 IF(EFRN-VALUE1)1150,1060,1060
 1060 FCN=FCN+1.0
 1070 IV=-1
      GOTO1090
 1080 IV=0
 1090 I1=0
      CALLROUT13
      IF(I3)460,1100,410
 1100 IFC=IFC
      GOTO(120,830 ,800 ,2840,3210,3270,680,770,850 ,3150,3250,3230,2200
     +    ,2010,2010,2080,2090,2100,2020,770,2200,2010,2010,2050,2200,20
     +10  ,2010,2020,2110,2050),IFC
 1110 IF(EFRP-VALUE1)1150,1120,1120
 1120 FCP=FCP+1.0
      GOTO1070
 1130 I3=0
      GOTO1160
 1140 I3=-1
      GOTO1160
 1150 I3=1
 1160 CALLROUT14
      I3=I3
      GOTO(1110,1050,1170,3560,2120),I3
 1170 CALL CERROR(' BERT I3=3$')
      NOPART=-1
      RETURN
 1180 I3=1
      GOTO1210
 1190 I3=3
      GOTO1210
 1200 I3=4
 1210 CALLROUT15(PPDC(1))
C     HPPDCI(45),PPDCI(170)
      I3=I3
      GOTO(1250,1340,1020,1220,1260,1390),I3
 1220 CALL CERROR(' BERT I3=4$')
C     SNN(RLKE GTE 1000)  DCINTP(RLKE GTE CRS.SECT.VALUES)
      NOPART=-1
      RETURN
 1230 PT(2)=5.0
      IK=IT
C     BG129  (PIM-N)
      PT(14)=2.0
      GOTO980
 1240 PT(2)=5.0
C     PIPNX  DIR. SCAT.
      PT(14)=1.0
      IK=IT
      GOTO 980
 1250 I3=1
      GOTO1280
 1260 I3=2
      GOTO1280
 1270 I3=3
 1280 CALLROUT16(PMDD(1))
C     HPMDDI(45),PMDDI(170),PMDDL(378)
 1290 IF(I3)1020,1190,1020
 1300 PT(2)=3.0
      PT(14)=2.0
      IK=3
      GO TO 980
 1310 PT(14)=2.0
 1320 IK=IT
 1330 PT(2)=4.0
      PM(3)=POMS
C     PI 0 MASS/CM
      GO TO 990
 1340 IF(IK-23)1350,2390,1350
 1350 I3=1
      GO TO 1380
 1360 I3=2
      GO TO 1380
 1370 I3=3
 1380 CALL ROUT16(PMDX(1))
C     HPMDXI(45),PMDXI(170),PMDXL(378)
      GO TO 1290
 1390 IF(IK-23)1360,2370,1360
 1400 IF(IK-23)1370,2380,1370
C     (PIM-P)XCH.
 1410 PT(14)=1.0
      GO TO 1320
 1420 PT(2)=1.0
C     PIM+(PP)  ABS
C     IT=10,PIP+(NN)  ABS
 1430 PT(14)=2.0
 1440 CALL CAPOL1(CST,SNT)
      GO TO 1030
 1450 PT(2)=2.0
C     PIN+(NN)  ABS
      GO TO 1430
 1460 PT(2)=1.0
C     PIN+(PP)  ABS    ALS0 PI+ ABS
      PT(14)=1.0
      GO TO 1440
 1470 ISW(9)=0
      ISW(10)=0
 1480 I3=0
      GO TO 1500
 1490 I3=-1
 1500 CALL ROUT17(FRIPN(1),PNMI(1),FMXSP(1),PCFSL(1),PNFSL(1))
C     FRIPN(117),PNMI(101),FMXSP(117),PCFSL(234),PNFSL(234)
      IF(I3) 1510,1640,1520
 1510 CALL CERROR(' BERT I3 < 0$')
C     COLL(COM LT -5.0E-6)  ECPL(ERROR IN CURR ,STRKP,PT(26),
C     PT(2),PT(14) OR PT(37))  ISW10=0
      NOPART=-1
      RETURN
 1520 K=3
 1530 IF(PT(K-1)-1.0)1650,1540,1650
 1540 IF(PT(K))1560,1560,1550
 1550 IF(PT(K)-EFRP)1560,1560,1580
 1560 FCP=FCP+1.0
C     NO. FORBIDDEN COLLISIONS INVOLVING PROTONS
 1570 PM(4)=DNCMS
      GO TO 1080
 1580 M=PT(K-1)
      IF(PT(K)-ECO (M)) 1590,1590,1600
 1590 PT(K)=0.0
      PNBC(M)=PNBC(M)+1.0
 1600 IF(COL(15)-1.0)1640,1740,1610
 1610 IF(COL(15)-3.0)1720,1710,1620
 1620 IF(COL(15)-5.0)1730,1780,1630
 1630 CALL CERROR(' BERT COL(15)>5$')
      NOPART=-1
      RETURN
 1640 CALL COLLM(0)
      IF(PT(38)) 1700,1690,1700
 1650 IF(PT(K-1)-2.0) 1810,1660,1810
 1660 IF(PT(K)) 1680,1680,1670
 1670 IF(PT(K)-EFRN) 1680,1680,1580
 1680 FCN = FCN+1.0
      GO TO 1570
 1690 I3=1
      GO TO 1750
 1700 I3=2
      GO TO 1750
 1710 I3=4
      GO TO 1750
 1720 I3=5
      GO TO 1750
 1730 I3=6
      GO TO 1750
 1740 I3=3
 1750 CALL ROUT18
      I3=I3
      K=IV
      GO TO (1530,1760,1600,1900,1770),I3
 1760 CALL CERROR(' BERT PT(K-1) < 3$')
      NOPART=-1
      RETURN
 1770 I18=I18+1
      GO TO 1570
 1780 CALL ROUT19
      IF(I3)1790,1900,1800
 1790 CALL CERROR(' BERT PT(K-1)<3, >6 K<27$')
      NOPART=-1
      RETURN
 1800 I19=I19+1
      GO TO 1570
 1810 IF(COL(15)-1.0)1640,1820,1820
 1820 CALL CERROR(' BERT COL(15) >=1$')
      NOPART=-1
      RETURN
 1830 I3=2
      GO TO 1910
 1840 I3=3
      GO TO 1910
 1850 I3=4
      GOTO1910
 1860 I3=5
      GOTO1910
 1870 I3=6
      GOTO1910
 1880 I3=7
      GOTO1910
 1890 I3=8
      GOTO1910
 1900 I3=1
 1910 CALL ROUT20(DCIN(1),DCLN(1),DCHN(1),PDCI(1),PDCH(1))
C     DCIN(115),DCLN(80),DCHN(64),PDCI(52),PDCH(64)
      I3=I3
      GOTO(1920,1140,1490,1430,1200,1020,1440,1030),I3
 1920 CALL CERROR(' BERT RLKE>3.5GeV$')
      NOPART=-1
      RETURN
 1930 I3=2
      GOTO1970
 1940 I3=3
      GOTO1970
 1950 I3=4
      GOTO1970
 1960 I3=1
 1970 CALL ROUT21(FRINN(1),DMIN(1),  FMXSN(1),FMXDN(1),FSLN(1))
C     FRINN(161),DMIN(101),FMXSN(161),FMXDN(130),FSLN(176)
      GOTO1500
 1980 IV=2
      GOTO2210
 1990 IV=3
      GOTO2210
 2000 IV=4
      GOTO2210
 2010 IV=5
      GOTO2210
 2020 IV=6
      GOTO2210
 2030 IV=7
      GOTO2210
 2040 IV=8
      GOTO2210
 2050 IV=9
      GOTO2210
 2060 IV=10
      GOTO2210
 2070 IV=11
      GOTO2210
 2080 IV=12
      GOTO2210
 2090 IV=13
      GOTO2210
 2100 IV=14
      GOTO2210
 2110 IV=15
      GOTO2210
 2120 IV=16
      GOTO2210
 2130 IV=17
      GOTO2210
 2140 IV=18
      GOTO2210
 2150 IV=19
      GOTO2210
 2160 IV=20
      GOTO2210
 2170 IV=21
      GOTO2210
 2180 IV=22
      GOTO2210
 2190 IV=23
      GOTO2210
 2200 IV=1
 2210 CALLROUT22(PPAC(1),POAC(1),PNEC(1),PMXC(1),PNNEC(1))
C     PPAC(19),POAC(19),PNEC(126),PMXC(126),PNNEC(126)
      IV=IV
      IF(I1)2820,2220,2220
 2220 GOTO(2350,2230,770,1130,2890,500,520,490,3400,3010,2990,3310,720,1
     +60 ,870 ,420,480,580,2900,140,2330,3020,2240),IV
 2230 CALL CERROR(' BERT COM>3500,ESPS(1)>=30. COM>2500$')
      NOPART=-1
      RETURN
 2240 CALL CERROR(' BERT IV > 22$')
      NOPART=-1
      RETURN
 2250 XABS=1.0
      VALUE1 = RANDC(ISEED)
      IF(VALUE1-PPNDA)2260,450,450
C     PROB. PIN-DEUT ABS
 2260 IT=27
C     BG117(20040) PI0 ABS
      MED=MED
      ABSEC=-HVP(MED)
      GO TO 370
 2270 IF(RLKE-2500.0)2290,2290,2280
 2280 CALL CERROR(' BERT RLKE > 2.5GeV$')
      RLKE=2500.0
 2290 IF(RLKE-180.0) 2320,2320,2300
 2300 IF(NOT-6) 2310,2310,2340
 2310 VALUE1=RLKE-180.0
      CALL CRJAB(1,PNSCL(1))
C     PNSCL(117)
C     (PIN-P)SINGLE PRODUCTION CRS. LOW EN.
      GO TO 3020
 2320 IF(CLSM-2.0)3550,3510,3040
 2330 IF(NOT-6)2190,2270,2270
 2340 VALUE1=RLKE-180.0
      CALL CRJAB(1,PNNSL(1))
C     PNNSL(117)
C     (PIN-N)SINGLE PROD. CRS. LOW EN.
      GOTO3020
 2350 ISW(11)=0
      GOTO2130
 2360 PT(14)=1.0
      GO TO 1320
 2370 I3=2
      GO TO 2400
 2380 I3=3
      GO TO 2400
 2390 I3=1
 2400 CALL ROUT16(PNDD(1))
C     HPNDDI(45),PNDDI(170),PNDDL(378)
C     (PIN-P)DRCT. CROSS SECTION INT. EN.
C     (PIN-P)DRCT DIFF. CRS. SEC. LOW ENERGY
 2410 GO TO 1290
 2420 PT(2)=3.0
      PT(14)=2.0
      IK=IT
      GO TO 980
 2430 PT(14)=2.0
      IK=23
      GO TO 1330
 2440 PT(2)=5.0
      GO TO 970
 2450 ISW(9)=2
      GO TO 1890
 2460 ITOTE  =IPEC(2)+IPEC(7)+IPEC(11)
      IF(ITOTE-1)2500,2480,2470
 2470 CALL CERROR('BERT1$')
 2480 NOPART = -1
 2490 CONTINUE
      RETURN
 2500 NOPART = ESPS(1)
      IF(NOPART-60)2520,2520,2510
 2510 WRITE(IO ,10000) NOPART
10000 FORMAT(' BERT : NOPART HAS EXCEEDED THE MAXIMUM = ',I6)
      NOPART = 60
 2520 CONTINUE
      DO 2530 NDEX = 1,NOPART
         KLMN = 8*(NDEX-1) + 1
         KIND(NDEX) = ESPS(KLMN+1)-1.
         ERAY(NDEX) = ESPS(KLMN+2)
         ARAY(NDEX) = ESPS(KLMN+3)
         BRAY(NDEX) = ESPS(KLMN+4)
 2530 GRAY(NDEX) = ESPS(KLMN+5)
      GO TO 2490
 2540 VALUE2=EINC+SPACE(12)
      IF(VALUE1-160.0)2550,2550,2570
 2550 SPACE(33)=1.4D-24
C     NO PRODUCTION POSSIBLE
      FMAX(2)=1.4D-24
      SPACE(34)=0.46D-24
      FMAX(1)=0.46D-24
      DO2560 I=9,12
 2560 S(I)=0.0
C     EINC+50.0 IS LESS THAN 160.0
      GOTO2740
 2570 CALLCBOVER(VALUE2,DNCMS,ANS)
C     NUCLEON MASS=CONSTANT  ANS=P1/E1
      IF(VALUE1-560.0)2580,2580,2660
 2580 S(11)=0.0
      S(12)=0.0
C     SINGLE PRODUCTION POSSIBLE--S(11),S(12) DOUBLE PROD.
      IF(VALUE1-400.0)2600,2590,2590
 2590 S(9)=22.6D-27*ANS
      S(10)=14.0D-27*ANS
      SPACE(44)=56.0D-27
      SPACE(45)=27.0D-27*ANS
      GOTO2650
 2600 IF(VALUE1-300.0)2620,2610,2610
 2610 S(9)=20.0D-27*ANS
      S(10)=14.0D-27*ANS
      SPACE(44)=0.106D-24
      SPACE(45)=36.0D-27*ANS
      GOTO2650
 2620 IF(VALUE1-200.0)2640,2630,2630
 2630 S(9)=11.4D-27*ANS
      S(10)=11.2D-27*ANS
      SPACE(44)=0.313D-24
      SPACE(45)=0.103D-24
      GOTO2650
 2640 S(9)=1.95D-27*ANS
      S(10)=1.7D-27*ANS
      SPACE(44)=0.52D-24
      SPACE(45)=0.176D-24
 2650 SPACE(33)=SPACE(44)
      SPACE(34)=SPACE(45)
      GOTO2740
 2660 IF(VALUE1-3600.0)2680,2680,2670
 2670 CALL CERROR(' BERT VALUE1 > 3.6GeV$')
      NOPART=-1
      RETURN
 2680 S(9)=22.6D-27*ANS
C     DOUBLE PRODUCTION POSSIBLE
      S(10)=14.0D-27*ANS
      IF(VALUE1-800.0)2690,2700,2700
 2690 S(11)=1.9D-27*ANS
      S(12)=9.0D-27*ANS
      SPACE(46)=38.4D-27*ANS
      SPACE(47)=27.2D-27*ANS
      GOTO2730
 2700 IF(VALUE1-1680.0)2710,2720,2720
 2710 S(11)=10.8D-27*ANS
      S(12)=17.4D-27*ANS
      SPACE(46)=33.0D-27*ANS
      SPACE(47)=27.2D-27*ANS
      GOTO2730
 2720 SPACE(46)=25.0D-27*ANS
      SPACE(47)=26.5D-27*ANS
      S(10)=13.6D-27*ANS
      S(11)=18.0D-27*ANS
      S(12)=23.6D-27*ANS
 2730 SPACE(33)=SPACE(46)
      SPACE(34)=SPACE(47)
 2740 GO TO (2750,3630), NO
 2750 IV=1
 2760 CALL CALXYI(9,33,41)
      IP=2
 2770 IF(NO-2)2790,2780,2780
 2780 ISW(4)=0
      GO TO 2800
 2790 ISW(4)=1
 2800 CALL UNDIS
      IF(BEGRU) 2810,2460,2810
 2810 XABS=0.0
      XINC=XI(1)
C     XINC=X-COORDINATE INC.PART.
      INC=1
C     0 IF PARTICLE CASCADE
      CURR(1)=NO
      CURR(3)=DNCMS
C     NUCLEON MASS/CM
      CALL CALGEO
      IF(I1) 2820,2830,2830
 2820 CALL CERROR(' BERT error in GEOM$')
      NOPART=-1
      RETURN
 2830 CALL PARTIN
      CALL SPAC32(43)
 2840 IF(EX-D(2))2850,2850,3120
 2850 WKRPN(3)=OUT(13)
      WKRPN(6)=OUT(16)
      CURR(2)=WKRPN(6)
      IF(ISW(4))2860,2870,2860
 2860 CURR(2)=WKRPN(3)
C     K.E.WITH RESPECT TO NEUTRONS(PROTONS), RG.3
 2870 CALL CBG6CA(3,0)
      IFCA=3
 2880 IFCC=3
 2890 KA=6
 2900 CALL CABRAN(KA)
      KNOT=NOT+6
      IF(IN)2920,2920,2910
 2910 KNOT=KNOT+6
 2920 IF(KNOT-17)2930,2250,2930
 2930 CALL CABG6C(ISW(4))
      IF(RLKE)2940,2940,2950
 2940 CALL CERROR(' BERT RLKE <= 0.0$')
      NOPART=-1
      RETURN
 2950 VALUE1=RLKE
      IF(IN) 2140,2960,2140
 2960 IF(NOT-5) 2970,3400,3400
 2970 IF(NOT-2)2980,3000,3310
 2980 ANY=SPACE(33)
 2990 CALL CRJAB(1,ECN(1))
C     ECN(176)
C     (N-P)ELASTIC CRS. SCATTERING
      GOTO3020
 3000 ANY=SPACE(34)
 3010 CALL CRJAB(1,PEC(1))
C     PEC(176)
C     (P-P)ELASTIC SCAT. CRS.
 3020 IF(CLSM-2.0)3540,3500,3030
 3030 IF(VALUE1-VALUE2)200,200,3040
 3040 CALLSIGNEX
      IF(ISW(1))3070,3050,3070
 3050 IF(IN)3060 ,2840,3060
 3060 IF(CURR(1)-2.0)2200,2200,2150
 3070 IF(IN)2010,3080,2010
 3080 IF(EX-D(6))2850,2850,3090
 3090 IF(ISW(2))3110,3100,3110
 3100 IPEC(7)=IPEC(7)+1
C     NO. OF ESCAPED PARTICLES ON RG.2
      GOTO 2770
 3110 IPEC(11)=IPEC(11)+1
      GOTO 2770
C     NO. OF PARTICLES ESCAPED ON RG.1
 3120 IF(D(3))3140,3130,3140
 3130 IPEC(2)=IPEC(2)+1
      GOTO 2770
C     NO. OF PARTICLES INCIDENT ON RG.3 ESCAPING
 3140 ISW(1)=1
      CALL SPAC32(42)
 3150 IF(EX-D(3))3160,3160,3190
 3160 WKRPN(2)=OUT(14)
      WKRPN(5)=OUT(17)
      CURR(2)=WKRPN(5)
      IF(ISW(4))3170,3180,3170
 3170 CURR(2)=WKRPN(2)
C     K.E.WITH RESPECT TO NEUTRONS(PROTONS) RG.2
 3180 CALL CBG6CA(2,0)
      GOTO2880
 3190 IF(D(4))3220,3200,3220
 3200 CALL SPAC32(43)
 3210 IF(EX-D(6))2850,2850,3100
 3220 ISW(2)=1
      ISW(3)=1
      CALL SPAC32(41)
 3230 IF(EX-D(4))3280,3280,3240
 3240 CALL SPAC32(42)
 3250 IF(EX-D(5))3160,3160,3260
 3260 CALL SPAC32(43)
 3270 IF(EX-D(6))2850,2850,3110
 3280 WKRPN(1)=OUT(15)
      WKRPN(4)=OUT(18)
      CURR(2)=WKRPN(4)
      IF(ISW(4))3290,3300,3290
 3290 CURR(2)=WKRPN(1)
C     K.E. WITH RESPECT TO NEUTRONS(PROTONS) RG.1
 3300 CALL CBG6CA(1,0)
      GOTO 2880
 3310 IF(RLKE-3500.0)3330,3330,3320
 3320 CALL CERROR(' BERT RLKE>3.5GeV (2)$')
      RLKE=3500.0
 3330 IF(RLKE-360.0)3530,3530,3340
 3340 VALUE1=RLKE-360.0
      IF(IN)3360,3350,3360
 3350 ANY=S(KNOT)
 3360 IF(NOT-4)3380,3390,3370
 3370 CALL CERROR(' BERT NOT=5$')
      NOPART=-1
      RETURN
 3380 CALL CRJAB(1,PSPCL(1))
C     PSPCL(158)
C     (P-P) SING. PROD. CRS. LOW ENERGY
      GOTO 3020
 3390 CALL CRJAB(1,SPCLN(1))
C     SPCLN(158)
C     (N-P) SINGLE PROD. CRS. LOW ENERGY
      GOTO 3020
 3400 IF(RLKE-3500.0)3410,3410,3320
 3410 IF(RLKE-920.0)3530,3530,3420
 3420 VALUE1=RLKE-920.0
      IF(NOT-6)3440,3470,3430
 3430 CALL CERROR(' BERT NOT > 6$')
      NOPART=-1
      RETURN
 3440 IF(IN)3460,3450,3460
 3450 ANY=S(11)
 3460 CALL CRJAB(1,PDPCL(1))
C     PDPCL(130)
C     (P-P) DOUBLE PRODUCTION CRS. LOW ENERGY
      GOTO 3020
 3470 IF(IN)3490,3480,3490
 3480 ANY=S(12)
 3490 CALL CRJAB(1,DPCLN(1))
C     DPCLN(130)
C     (N-P) DOUBLE PRODUCTION CRS. LOW ENERGY
      GOTO 3020
 3500 IF(VALUE1-VALUE2)730,730,3510
 3510 CALL SIGNEX
      IF(IN)2160,3520,2160
 3520 IF(ISW(3))3250,3150,3250
 3530 IF(CLSM-2.0)3550,3510,3040
 3540 IF(VALUE1-VALUE2)890 ,890 ,3550
 3550 CALL SIGNEX
      IF(IN)2170,3230,2170
 3560 IF(ESPS(1))3580,3570,3580
 3570 NWDS=1
      GOTO 3610
 3580 NWDS=ESPS(1)*8.0+1.5
C     TOTAL NO. OF WORDS(ESCAPING PARTICLES)
      IF(COUNT(6).GE.0.0) GO TO 3610
C MINUS,RECORD NOT REPRESENTATIVE,SKIP
      DO 3590 I=1,NWDS
 3590 ESPS(I) = 0.0
      DO 3600 I=1,5
 3600 COUNT(I) = 0.0D0
 3610 NOR=NOR+1
 3620 IN=0
      GOTO(110,2770),IP
 3630 IV=-1
      GOTO 2760
C1370 RETURN
      END
