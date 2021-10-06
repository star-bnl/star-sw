* $Id: chetc.F,v 1.1.1.1 2004/01/15 00:08:49 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CHETC(DOSKAL)
C*************************************************************
C
C process intranuclear-cascade and evaporation
C call scaling for smooth transition to FLUKA
C If H-Atoms present -> special particle-H collision
C generate de-exitation gammas
C
C*************************************************************
C
#define ATLAS_CDEBUG 1
C      Interface common
#include "gcalor/calgea.inc"
#include "geant321/gcflag.inc"
#include "geant321/gcunit.inc"
C
C  CALOR commons
#include "gcalor/ccomon.inc"
#include "gcalor/ccomn2.inc"
#include "gcalor/ccomn3.inc"
#include "gcalor/cjoint.inc"
#include "gcalor/cxpd.inc"
#include "gcalor/cgeos.inc"
#include "gcalor/chevap.inc"
#include "gcalor/ctncol.inc"
#include "geant321/crandm.inc"
#include "geant321/cmass.inc"
C
      DIMENSION F(8),PCAP(100,2),NPCOL(12),AP(12),ZP(12)
C
      LOGICAL DOSKAL,INIT
      INTEGER IBERT
      SAVE
C
      DATA AP/ 1., 1., 0., 0., 0., 0., 0., 2., 3., 3., 4., 0./
      DATA ZP/ 1., 0., 0., 0., 0., 0., 0., 1., 1., 2., 2., 0./
C
      DATA INIT/.TRUE./
C
      IF(INIT) THEN
         IBERT = 0
         INIT = .FALSE.
      ENDIF
      INTCAL = 12
      NO = 1
      ATARGT = 0.0
      ZTARGT = 0.0
      ITYP = IPINC + 1
      TIP(1)  = FLOAT(IPINC)
      WT(NO) = 1.0
      EC(1) = EINC
      MAT = 1
      MXMAT = 1
      NEL(1) = NCEL
C   copy material data to CALOR
      DO 10  I=1,NCEL
         DEN(I,1) = DMED(I)
         ZZ(I,1) = ZMED(I)
         A(I,1) = AMED(I)
         DENH(1) = HDEN
   10 CONTINUE
C    calculate x-section for given material
      CALL CALCXS
   20 CONTINUE
C
C --------- start of intranuclear cascade ----------------------
C
C
      R = RANDC(ISEED)
      IF(E(NO) - 1.0)100,30 ,100
C*      PI- CAPTURE ?
   30 IF(TIP(NO).NE.4) GOTO 100
#if defined(ATLAS_CDEBUG)
      if (Idebug.gt.1) WRITE(LOUT,900)
 900  format(' CHETC NMTC: PI- to be captured ')
#endif
      INTCAL = 18
      M=1
      LMX=NEL(M)
      DO 40  L=1,LMX
   40 PCAP(L,M)=DEN(L,M)*ZZ(L,M)
      PCAPS=0.
      DO 50  L=1,LMX
   50 PCAPS=PCAPS+PCAP(L,M)
      DO 60  L=1,LMX
   60 PCAP(L,M)=PCAP(L,M)/PCAPS
      LMX=NEL(MAT)
      DO 80  L=1,LMX
         R0=R
         R=R-PCAP(L,MAT)
         IF(R.LE.0.) GO TO 70
         GO TO 80
   70    LELEM=L
         GO TO 90
   80 CONTINUE
      CALL CERROR('NMTC$')
   90 DKWT=1.
      GO TO 250
  100 CONTINUE
C
      IF(ITYP.GE.6) GO TO 220
      R = R-HSIGG(ITYP,MAT)/SIGMX(ITYP,MAT)
      IF(R) 110,110 ,230
  110 CONTINUE
CZ 10/11/92 SCALING added
C scaling selected ?
      IF(DOSKAL) THEN
         INC = ITYP - 1
C get H cross-section
         CALL SHXSEC(EC(1),INC,HEHT,HEHEL,HELNEL)
         R = RANDC(ISEED)
         IF(HEHT*DENH(MAT)*1.E 24/HSIGG(ITYP,MAT)-R) 210,120,120
  120    CONTINUE
         ATARGT = 1.
         ZTARGT = 1.
         R = RANDC(ISEED)
         IF(HEHEL/HEHT.GT.R) THEN
C ---- elastic
            CALL CSCATT(INC,EC(NO),KIND,EP,ALPHA,BETA,GAM)
            NOPART = 2
            GOTO 160
         ELSE
C ---- nonelastic
            EHICUT = EMAXPI*1000.
            IF(TIP(NO).LE.1.) EHICUT = EMAXP*1000.
            CALL SKALEH(IBERT,INC,HEHT,EC(NO),NOPART,KIND,EP,ALPHA,
     +      BETA,GAM,EHICUT)
            GOTO 160
         ENDIF
      ELSE
         CALL GTHSIG(2)
         R = RANDC(ISEED)
         IF(HSIG*DENH(MAT)*1.E 24/HSIGG(ITYP,MAT)-R) 210,130,130
  130    CONTINUE
#if defined(ATLAS_CDEBUG)
         IF (IDEBUG.GT.1) WRITE(LOUT,901)
 901     format(' CHETC NMTC: H-atom collision; Call CPCOL')
#endif
  140    CONTINUE
         ATARGT = 1.0
         ZTARGT = 1.0
         UU = 0.0
         CALL CPCOL(IBERT,ITYP,HSIG,EC(NO),NOPART,KIND,EP,ALPHA,BETA,
     +   GAM)
C            CHANGED NOV.1,1986
         IBERT = 1
      ENDIF
      IF(NOPART.LE.0) GO TO 170
      DO 150 I=1,NOPART
         KINDI(I)=KIND(I)+1
         IBB=1
         IF(KINDI(I).GT.2)IBB=0
         IBBARR(I)=IBB
  150 CONTINUE
  160 APR = 0.0
      ZPR = 0.0
      GOTO 190
  170 CONTINUE
C             END OF CHANGE
C
  180 CONTINUE
      LELEM = 1
      APR=A(LELEM,MAT)
      ZPR =ZZ(LELEM,MAT)
  190 EX= 0.
      EREC =0.
      DO 200 I = 1,6
  200 NPART(I) = 0
      UU = 0.
      GOTO 430
  210 CONTINUE
  220 CONTINUE
#if defined(ATLAS_CDEBUG)
      IF (IDEBUG.GT.1) WRITE(LOUT,902)
 902  format(' CHETC NMTC: PSEUDO COLLISION forced')
#endif
      NOPART = -1
      GO TO 180
  230 NNN = NEL(MAT)
      DO 240 LEM = 1,NNN
         LELEM = LEM
CZ no hydrogen accepted in BERT ; CZ 2 JUN 92
         IF(A(LELEM,MAT) .LT. 2.0) GOTO 240
         R = R - SIGG(LEM,MAT)/SIGMX(ITYP,MAT)
         IF(R) 250,250,240
  240 CONTINUE
      GOTO 220
C------------- elastic neutron-nucleus scattering not implemented -----
C
  250 ETOT = EINC + PMASS(IPINC+1)*1.E3
CZ
CZ   check if mass number to low for BERT (A>=4)
      IF(A(LELEM,MAT).LT.4.0) THEN
C
CZ 2.95         CALL CERROR('NMTC: A < 4$')
CZ   set A=4 (brute force, but will work )
         A(LELEM,MAT) = 4.0
      ENDIF
#if defined(ATLAS_CDEBUG)
      IF (IDEBUG.GT.1) WRITE(LOUT,911) A(LELEM,MAT),ZZ(LELEM,MAT),LELEM
 911  format(' NMTC: nucleus collision A=',F9.3,' Z=',F6.0,' LELEM=',i6)
#endif
      F(1) = A(LELEM,MAT)
      F(2) = ZZ(LELEM,MAT)
      F(3) = EC(NO)
      F(4) = 0.0
      F(5) = 1.0
      F(6) = 0.0
      F(7) = TIP(NO)
      F(8) = 0.0
C
CZ 10/11/92 SCALING added
      IF(DOSKAL) THEN
         EHICUT = EMAXPI*1000.
         IF(TIP(NO).LE.1.) EHICUT = EMAXP*1000.
         CALL CSKALE(IBERT,F,NOPART,KIND,EP,ALPHA,BETA,GAM,EHICUT,
     +   RMFAS,EX,EREC)
      ELSE
         CALL CABERT(IBERT,F,NOPART,KIND,EP,ALPHA,BETA,GAM)
      ENDIF
      IBERT = 1
C
      IF(NOPART.GT.0) THEN
         DO 260 I=1,NOPART
            KINDI(I)=KIND(I)+1
            IBB=1
            IF(KINDI(I).GT.2) IBB=0
            IBBARR(I)=IBB
  260    CONTINUE
      ENDIF
      IF(NOPART.LT.0) THEN
C ----------- Pseudo collision -------------------------
C   if incident particle pi- with 1 MeV -> repeat BERT,
C   reason : pi- capture (below cutoff)
         IF(IPINC.EQ.4 .AND. EINC.EQ.1.0) GO TO 250
C  pi- capture ? -> retry
         IF(EINC.EQ.1.0) GOTO 20
         GOTO 420
      ELSE IF(NOPART.EQ.0) THEN
C ------------------- no particle escaped nucleus ------
         GO TO(270,270,280,290,280,290,290),ITYP
  270    APR = A(LELEM,MAT) + 1.
         ZPR = ZZ(LELEM,MAT) + 1. - TIP(1)
         EX = EC(NO) + 7.
         GO TO 400
  280    APR = A(LELEM,MAT)
         ZPR = ZZ(LELEM,MAT) + 3.- TIP(1)
         EX= EC(NO) + PMASS(ITYP)*1.E3
         GO TO 400
  290    CALL CERROR('NMTC: PI0,MU+-$')
C ---------------NOPART GT 0 --------------------------
      ELSE
  300    PI0 =0.
         SUME = 0.
         PRONO = 0.
         PIPOS = 0.
         PINEG = 0.
         DO 360 N=1,NOPART
            LK= KIND(N)+1
            GO TO(310,350,320,330,340,370,370),LK
  310       PRONO =PRONO +1.
            GO TO 350
  320       PIPOS = PIPOS +1.
            GO TO 350
  330       PI0 = PI0+1.
            GO TO 350
  340       PINEG= PINEG +1.
  350       SUME = SUME + EP(N)
  360    CONTINUE
         CHGPIS =PIPOS +PINEG
         FPT = NOPART
         FPT = FPT -CHGPIS-PI0
         IF(TIP(1)-1.) 380,380,390
  370    CALL CERROR('NMTC: MU+-$')
  380    APR=A(LELEM,MAT) + 1. - FPT
         ZPR= ZZ(LELEM,MAT) + 1. - TIP(1) - PRONO - PIPOS + PINEG
         IF(ZPR.LT.0.) THEN
            CALL CERROR(' NMTC: Zpr < 0$')
            EREC = 0.0
            EX = 0.0
            ZPR = 0
            GOTO 410
         ENDIF
         IF(.NOT.DOSKAL) EX= EINC + (1.-FPT)*7.0-SUME- CHGPIS*PMASS(3)*
     +   1.E3-PI0*PMASS(4)*1.E3
         GO TO 400
  390    APR = A(LELEM,MAT) - FPT
         ZPR = ZZ(LELEM,MAT)+ 3.-TIP(1)-PRONO-PIPOS + PINEG
         IF(ZPR.LT.0.) THEN
            CALL CERROR(' NMTC: Zpr < 0$')
            EREC = 0.0
            ZPR = 0
            GOTO 410
         ENDIF
         IF(.NOT.DOSKAL) EX= EINC+(1.-CHGPIS)*PMASS(3)*1.E3-SUME
     +   -FPT*7.-PI0*PMASS(4)*1.E3
      ENDIF
C calculate recoil energy of nucleus
  400 CONTINUE
      IF(.NOT.DOSKAL) CALL RECOIL
  410 EX=EX-EREC
#if defined(ATLAS_CDEBUG)
      IF (IDEBUG.GT.1) WRITE(LOUT,912) EX,EREC,SUME
 912  format(' NMTC: EX=',G12.5,' EREC=', G12.5,' Sume=',G12.5)
#endif
      IF(EX.LT.0.0) EX = 0.0
C
C -------- evaporation ------------------------------
C
      CALL CERUP
#if defined(ATLAS_CDEBUG)
      IF (IDEBUG.GT.1) WRITE(LOUT,913)  EX,EREC
 913  format(' NMTC: after ERUP EX=',G12.5,' EREC=',G12.5)
#endif
C ------------------  fill return variables -------------
  420 CONTINUE
      IBERT = 1
#if defined(ATLAS_CDEBUG)
      IF (IDEBUG.GT.1) WRITE(LOUT,914) NOPART,NPART(2),NPART(1),EX,UU
 914  format(' NMTC: NOPART=',i6,' P=',i6,
     +       ' N=',i6,' EX=',G12.5,' UU=',G12.5)
#endif
#if defined(ATLAS_CDEBUG)
      IF (IDEBUG.GT.1) WRITE(LOUT,915) APR,ZPR
 915  format(' NMTC: Residual nucleus A=',F8.2,' Z=',F6.0)
#endif
  430 CONTINUE
CZ set target nucleus
      IF(ATARGT.EQ.0.0) THEN
        ATARGT = A(LELEM,MAT)
        ZTARGT = ZZ(LELEM,MAT)
      ENDIF
      EXMED    = EX
      IF(APR.GE.0 .AND. ZPR.GE.0) THEN
        ERMED(1) = EREC
        AMED(1) = APR
        ZMED(1) = ZPR
        NRECOL  = 1
      ELSE
        AMED(1)  = 0.0
        ZMED(1)  = 0.0
        ERMED(1) = 0.0
        NRECOL   = 0
      ENDIF
      IF(NOPART.LT.0) THEN
C Pseudo collision
         NCOL   = 5
         NPHETC = 0
         EKINET(1) = EINC
         IPCAL(1)  = IPINC
         CALTIM(1) = 0.0
         UCAL(1,1) = UINC(1)
         UCAL(1,2) = UINC(2)
         UCAL(1,3) = UINC(3)
         EXMED    = 0.0
         NRECOL   = 1
         ERMED(1) = 0.0
         INTCAL   = 24
         AMED(1)  = A(LELEM,MAT)
         ZMED(1)  = ZZ(LELEM,MAT)
      ELSE
C get particles from intranuclear cascade
         NCOL    = 2
         NPHETC  = 0
         AMED(1) = APR
         ZMED(1) = ZPR
         IF(NOPART.GT.0) THEN
            DO 440 I=1,NOPART
               NPHETC = NPHETC + 1
               IF(NPHETC.GT.MXCP) NPHETC=MXCP
               EKINET(NPHETC) = EP(I)
               IPCAL(NPHETC) = KIND(I)
               CALTIM(NPHETC) = 0.0
C               transformation into Lab ssystem
               CALL CB2LAB(ALPHA(I),BETA(I),GAM(I), UINC(1),UINC(2),
     +         UINC(3),UCAL(NPHETC,1),UCAL(NPHETC,2),UCAL(NPHETC,3))
  440       CONTINUE
         ENDIF
C get evaporated neutrons
         IF(NPART(1).GT.0) THEN
            DO 450 I=1,NPART(1)
               NPHETC = NPHETC + 1
               IF(NPHETC.GT.MXCP) NPHETC=MXCP
               EKINET(NPHETC) = EPART(I,1)
               IPCAL(NPHETC)  = 1
               CALTIM(NPHETC) = 0.0
               CALL GTISO(ALP,BET,CAM)
               UCAL(NPHETC,1) = ALP
               UCAL(NPHETC,2) = BET
               UCAL(NPHETC,3) = CAM
  450       CONTINUE
         ENDIF
C get evaporated protons
         IF(NPART(2).GT.0) THEN
            DO 460 I=1,NPART(2)
               NPHETC = NPHETC + 1
               IF(NPHETC.GT.MXCP) NPHETC=MXCP
               EKINET(NPHETC) = EPART(I,2)
               IPCAL(NPHETC)  = 0
               CALTIM(NPHETC) = 0.0
               CALL GTISO(ALP,BET,CAM)
               UCAL(NPHETC,1) = ALP
               UCAL(NPHETC,2) = BET
               UCAL(NPHETC,3) = CAM
  460       CONTINUE
         ENDIF
C get evaporated heavy particles (alpha,deuteron, triton, He3)
C              particle type       10     7         8      9
         DO 480 I=3,6
            IF(NPART(I).GT.0) THEN
               DO 470 K=1,NPART(I)
                  NPHETC = NPHETC + 1
                  IF(NPHETC.GT.MXCP) NPHETC=MXCP
                  EKINET(NPHETC) = HEPART(K,I-2)
                  IPCAL(NPHETC) = I + 4
                  CALTIM(NPHETC) = 0.0
                  CALL GTISO(ALP,BET,CAM)
                  UCAL(NPHETC,1) = ALP
                  UCAL(NPHETC,2) = BET
                  UCAL(NPHETC,3) = CAM
  470          CONTINUE
            ENDIF
  480    CONTINUE
C generate de-exitation gammas
C
         IF(UU.GT.0.0) THEN
            EEX = UU
            EGTOT = 0.0
  490       CONTINUE
            EGAM = EEX * RANDC(ISEED)
            IF((EGTOT+EGAM) .GT. EEX) THEN
               EGAM = EEX - EGTOT
               EEX = 0.0
            ENDIF
            EGTOT = EGTOT + EGAM
            CALL AZIRN(SINA,COSA)
            COSP = SFLRAF(DUM)
            SINP = SQRT(1.0-COSP*COSP)
            NPHETC = NPHETC + 1
            IF(NPHETC.GT.MXCP) NPHETC=MXCP
            EKINET(NPHETC) = EGAM
            UCAL(NPHETC,1) = SINP * COSA
            UCAL(NPHETC,2) = SINP * SINA
            UCAL(NPHETC,3) = COSP
            IPCAL(NPHETC) = 11
            CALTIM(NPHETC) = 0.0
            IF(EEX.GT.0.0) GOTO 490
         ENDIF
#if defined(ATLAS_CDEBUG)
CZ following is only for debug purposes
         EETOT = 0.0
         PX = 0.0
         PY = 0.0
         PZ = 0.0
         ATOT = 0.0
         ZTOT = 0.0
         DO 500 I=1,12
            NPCOL(I) = 0
  500    CONTINUE
         DO 510 I=1,NPHETC
            IP = IPCAL(I)
            NPCOL(IP+1) = NPCOL(IP+1) + 1
            ATOT = ATOT + AP(IP+1)
            ZTOT = ZTOT + ZP(IP+1)
            ETOT = EKINET(I) + PMASS(IP+1)*1.E3
            if (Idebug.gt.1) WRITE(LOUT,921) i,
     +                       ekinet(i),pmass(ip+1),ip,Etot
 921        format(' CHETC: i=',i6,' ekin=',G12.5,' mass=',G12.5,
     +             ' ip=',i6,' Etot=',G12.5)
            EETOT = EETOT + ETOT
            PP = SQRT(ETOT*ETOT - PMASS(IP+1)**2*1.E6)
            PX = PX + UCAL(I,1)*PP
            PY = PY + UCAL(I,2)*PP
            PZ = PZ + UCAL(I,3)*PP
  510    CONTINUE
         IF(NPHETC.GT.0 .and. IDEBUG.GT.1) THEN
            WRITE(LOUT,922) NPHETC,(NPCOL(K),K=1,12),EETOT,PX,PY,PZ
 922        format(' ******* Collision summary *****',/,
     +              ' * No. of particles :',I4,/,
     +              ' * No. of protons   :',I4,/,
     +              ' * No. of neutrons  :',I4,/,
     +              ' * No. of Pi+       :',I4,/,
     +              ' * No. of Pi0       :',I4,/,
     +              ' * No. of Pi-       :',I4,/,
     +              ' * No. of Muon+     :',I4,/,
     +              ' * No. of Muon-     :',I4,/,
     +              ' * No. of Deuteron  :',I4,/,
     +              ' * No. of Triton    :',I4,/,
     +              ' * No. of He3       :',I4,/,
     +              ' * No. of Alpha     :',I4,/,
     +              ' * No. of Gamma     :',I4,/,
     +              ' * Total Energy     :',F8.2,' MeV',/,
     +              ' * Px,Py,Pz         :',3F8.2)
            WRITE(LOUT,923) APR,ZPR,ATOT,ZTOT,APR+ATOT,ZPR+ZTOT,
     +             A(LELEM,MAT)-APR-ATOT,ZZ(LELEM,MAT)-ZPR-ZTOT,
     +             NPART(1),NPART(2)
 923        format(' ****** RESIDUAL NUCLEUS *****',/,
     +              ' * Mass number    :',F4.0,/,
     +              ' * Charge number  :',F4.0,/,
     +              ' * A of particles :',F4.0,/,
     +              ' * Z of particles :',F4.0,/,
     +              ' * Sum of A       :',F4.0,/,
     +              ' * Sum of Z       :',F4.0,/,
     +              ' * Diff. of A to nucleus :',F4.0,/,
     +              ' * Diff. of Z to nucleus :',F4.0,/,
     +              ' * Evap  neutrons :',I4,/,
     +              ' * Evap  protons  :',I4)
         ENDIF
#endif
      ENDIF
      RETURN
      END
