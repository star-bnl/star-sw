 
C*********************************************************************
 
C...PYPTFS
C...Generates pT-ordered timelike final-state parton showers.
 
C...MODE defines how to find radiators and recoilers.
C... = 0 : based on colour flow between undecayed partons.
C... = 1 : for IPART <= NPARTD only consider primary partons,
C...       whether decayed or not; else as above.
C... = 2 : based on common history, whether decayed or not.
 
      SUBROUTINE PYPTFS(MODE,PTMAX,PTMIN,PTGEN)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      INTEGER PYK,PYCHGE,PYCOMP
C...Parameter statement to help give large particle numbers.
      PARAMETER (KSUSY1=1000000,KSUSY2=2000000,KTECHN=3000000,
     &KEXCIT=4000000,KDIMEN=5000000)
C...Parameter statement for maximum size of showers.
      PARAMETER (MAXNUR=1000)
C...Commonblocks.
      COMMON/PYPART/NPART,NPARTD,IPART(MAXNUR),PTPART(MAXNUR)
      COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
      COMMON/PYCTAG/NCT,MCT(4000,2)
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYDAT2/KCHG(500,4),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      SAVE /PYPART/,/PYJETS/,/PYCTAG/,/PYDAT1/,/PYDAT2/,/PYPARS/,
     &/PYINT1/
C...Local arrays.
      DIMENSION IPOS(2*MAXNUR),IREC(2*MAXNUR),IFLG(2*MAXNUR),
     &ISCOL(2*MAXNUR),ISCHG(2*MAXNUR),PTSCA(2*MAXNUR),IMESAV(2*MAXNUR),
     &PT2SAV(2*MAXNUR),ZSAV(2*MAXNUR),SHTSAV(2*MAXNUR),
     &MESYS(MAXNUR,0:2),PSUM(5),DPT(5,4)
C...Statement functions.
      SHAT(I,J)=(P(I,4)+P(J,4))**2-(P(I,1)+P(J,1))**2-
     &(P(I,2)+P(J,2))**2-(P(I,3)+P(J,3))**2
 
C...Initial values. Check that valid system.
      PTGEN=0D0
      IF(MSTJ(41).NE.1.AND.MSTJ(41).NE.2.AND.MSTJ(41).NE.11.AND.
     &MSTJ(41).NE.12) RETURN
      IF(NPART.LE.0) THEN
        CALL PYERRM(2,'(PYPTFS:) showering system too small')
        RETURN
      ENDIF
      PT2CMX=PTMAX**2
 
C...Mass thresholds and Lambda for QCD evolution.
      PMB=PMAS(5,1)
      PMC=PMAS(4,1)
      ALAM5=PARJ(81)
      ALAM4=ALAM5*(PMB/ALAM5)**(2D0/25D0)
      ALAM3=ALAM4*(PMC/ALAM4)**(2D0/27D0)
      PMBS=PMB**2
      PMCS=PMC**2
      ALAM5S=ALAM5**2
      ALAM4S=ALAM4**2
      ALAM3S=ALAM3**2
 
C...Cutoff scale for QCD evolution. Starting pT2.
      NFLAV=MAX(0,MIN(5,MSTJ(45)))
      PT0C=0.5D0*PARJ(82)
      PT2CMN=MAX(PTMIN,PT0C,1.1D0*ALAM3)**2
 
C...Parameters for QED evolution.
      AEM2PI=PARU(101)/PARU(2)
      PT0EQ=0.5D0*PARJ(83)
      PT0EL=0.5D0*PARJ(90)
 
C...Reset. Remove irrelevent colour tags.
      NEVOL=0
      DO 100 J=1,4
        PSUM(J)=0D0
  100 CONTINUE
      DO 110 I=MINT(84)+1,N
        IF(K(I,2).GT.0.AND.K(I,2).LT.6) K(I,5)=0
        IF(K(I,2).LT.0.AND.K(I,2).GT.-6) K(I,4)=0
  110 CONTINUE
      NPARTS=NPART
 
C...Begin loop to set up showering partons. Sum four-momenta.
      DO 210 IP=1,NPART
        I=IPART(IP)
        IF(MODE.NE.1.OR.I.GT.NPARTD) THEN
          IF(K(I,1).GT.10) GOTO 210
        ELSEIF(K(I,3).GT.MINT(84)) THEN
          IF(K(I,3).GT.MINT(84)+2) GOTO 210
        ELSE
          IF(K(K(I,3),3).GT.MINT(83)+6) GOTO 210
        ENDIF
        DO 120 J=1,4
          PSUM(J)=PSUM(J)+P(I,J)
  120   CONTINUE
 
C...Find colour and charge, but skip diquarks.
        IF(IABS(K(I,2)).GT.1000.AND.IABS(K(I,2)).LT.10000) GOTO 210
        KCOL=ISIGN(KCHG(PYCOMP(K(I,2)),2),K(I,2))
        KCHA=ISIGN(KCHG(PYCOMP(K(I,2)),1),K(I,2))
 
C...Either colour or anticolour charge radiates; for gluon both.
        DO 160 JSGCOL=1,-1,-2
          IF(KCOL.EQ.JSGCOL.OR.KCOL.EQ.2) THEN
            JCOL=4+(1-JSGCOL)/2
            JCOLR=9-JCOL
 
C...Basic info about radiating parton.
            NEVOL=NEVOL+1
            IPOS(NEVOL)=I
            IFLG(NEVOL)=0
            ISCOL(NEVOL)=JSGCOL
            ISCHG(NEVOL)=0
            PTSCA(NEVOL)=PTPART(IP)
 
C...Begin search for colour recoiler when MODE = 0 or 1.
            IF(MODE.LE.1) THEN
C...Find sister with matching anticolour to the radiating parton.
              IROLD=I
              IRNEW=K(IROLD,JCOL)/MSTU(5)
              MOVE=1
 
C...Skip radiation off loose colour ends.
  130         IF(IRNEW.EQ.0) THEN
                NEVOL=NEVOL-1
                GOTO 160
 
C...Optionally skip radiation on dipole to beam remnant.
              ELSEIF(MSTP(72).LE.1.AND.IRNEW.GT.MINT(53)) THEN
                NEVOL=NEVOL-1
                GOTO 160
 
C...For now always skip radiation on dipole to junction.
              ELSEIF(K(IRNEW,2).EQ.88) THEN
                NEVOL=NEVOL-1
                GOTO 160
 
C...For MODE=1: if reached primary then done.
              ELSEIF(MODE.EQ.1.AND.IRNEW.GT.MINT(84)+2.AND.
     &        IRNEW.LE.NPARTD) THEN
 
C...If sister stable and points back then done.
              ELSEIF(MOVE.EQ.1.AND.K(IRNEW,JCOLR)/MSTU(5).EQ.IROLD)
     &        THEN
                IF(K(IRNEW,1).LT.10) THEN
 
C...If sister unstable then go to her daughter.
                ELSE
                  IROLD=IRNEW
                  IRNEW=MOD(K(IRNEW,JCOLR),MSTU(5))
                  MOVE=2
                  GOTO 130
               ENDIF
 
C...If found mother then look for aunt.
              ELSEIF(MOVE.EQ.1.AND.MOD(K(IRNEW,JCOL),MSTU(5)).EQ.
     &        IROLD) THEN
                IROLD=IRNEW
                IRNEW=K(IROLD,JCOL)/MSTU(5)
                GOTO 130
 
C...If daughter stable then done.
              ELSEIF(MOVE.EQ.2.AND.K(IRNEW,JCOLR)/MSTU(5).EQ.IROLD)
     &        THEN
                IF(K(IRNEW,1).LT.10) THEN
 
C...If daughter unstable then go to granddaughter.
                ELSE
                  IROLD=IRNEW
                  IRNEW=MOD(K(IRNEW,JCOLR),MSTU(5))
                  MOVE=2
                  GOTO 130
                ENDIF
 
C...If daughter points to another daughter then done or move up.
              ELSEIF(MOVE.EQ.2.AND.MOD(K(IRNEW,JCOL),MSTU(5)).EQ.
     &        IROLD) THEN
                IF(K(IRNEW,1).LT.10) THEN
                ELSE
                  IROLD=IRNEW
                  IRNEW=K(IRNEW,JCOL)/MSTU(5)
                  MOVE=1
                  GOTO 130
                ENDIF
              ENDIF
 
C...Begin search for colour recoiler when MODE = 2.
            ELSE
              IROLD=I
              IRNEW=K(IROLD,JCOL)/MSTU(5)
  140         IF(K(IRNEW,JCOLR)/MSTU(5).NE.IROLD) THEN
C...Step up to mother if radiating parton already branched.
                IF(K(IRNEW,2).EQ.K(IROLD,2)) THEN
                  IROLD=IRNEW
                  IRNEW=K(IROLD,JCOL)/MSTU(5)
                  GOTO 140
C...Pick sister by history if no anticolour available.
                ELSE
                  IF(IROLD.GT.1.AND.K(IROLD-1,3).EQ.K(IROLD,3)) THEN
                    IRNEW=IROLD-1
                  ELSEIF(IROLD.LT.N.AND.K(IROLD+1,3).EQ.K(IROLD,3))
     &            THEN
                    IRNEW=IROLD+1
C...Last resort: pick at random among other primaries.
                  ELSE
                    ISTEP=MAX(1,MIN(NPART-1,INT(1D0+(NPART-1)*PYR(0))))
                    IRNEW=IPART(1+MOD(IP+ISTEP-1,NPART))
                  ENDIF
                ENDIF
              ENDIF
C...Trace down if sister branched.
  150         IF(K(IRNEW,1).GT.10) THEN
                IRNEW=MOD(K(IRNEW,JCOLR),MSTU(5))
                GOTO 150
              ENDIF
            ENDIF
 
C...Now found other end of colour dipole.
            IREC(NEVOL)=IRNEW
          ENDIF
  160   CONTINUE
 
C...Also electrical charge may radiate; so far only quarks and leptons.
        IF((MSTJ(41).EQ.2.OR.MSTJ(41).EQ.12).AND.KCHA.NE.0.AND.
     &  IABS(K(I,2)).LE.18) THEN
 
C...Basic info about radiating parton.
          NEVOL=NEVOL+1
          IPOS(NEVOL)=I
          IFLG(NEVOL)=0
          ISCOL(NEVOL)=0
          ISCHG(NEVOL)=KCHA
          PTSCA(NEVOL)=PTPART(IP)
 
C...Pick nearest (= smallest invariant mass) charged particle
C...as recoiler when MODE = 0 or 1 (but for latter among primaries).
          IF(MODE.LE.1) THEN
            IRNEW=0
            PM2MIN=VINT(2)
            DO 170 IP2=1,NPART+N-MINT(53)
              IF(IP2.EQ.IP) GOTO 170
              IF(IP2.LE.NPART) THEN
                I2=IPART(IP2)
                IF(MODE.NE.1.OR.I2.GT.NPARTD) THEN
                  IF(K(I2,1).GT.10) GOTO 170
                ELSEIF(K(I2,3).GT.MINT(84)) THEN
                  IF(K(I2,3).GT.MINT(84)+2) GOTO 170
                ELSE
                  IF(K(K(I2,3),3).GT.MINT(83)+6) GOTO 170
                ENDIF
              ELSE
                I2=MINT(53)+IP2-NPART
              ENDIF
              IF(KCHG(PYCOMP(K(I2,2)),1).EQ.0) GOTO 170
              PM2INV=(P(I,4)+P(I2,4))**2-(P(I,1)+P(I2,1))**2-
     &        (P(I,2)+P(I2,2))**2-(P(I,3)+P(I2,3))**2
              IF(PM2INV.LT.PM2MIN) THEN
                IRNEW=I2
                PM2MIN=PM2INV
              ENDIF
  170       CONTINUE
            IF(IRNEW.EQ.0) THEN
              NEVOL=NEVOL-1
              GOTO 210
            ENDIF
 
C...Begin search for charge recoiler when MODE = 2.
          ELSE
            IROLD=I
C...Pick sister by history; step up if parton already branched.
  180       IF(K(IROLD,3).GT.0.AND.K(K(IROLD,3),2).EQ.K(IROLD,2)) THEN
              IROLD=K(IROLD,3)
              GOTO 180
            ENDIF
            IF(IROLD.GT.1.AND.K(IROLD-1,3).EQ.K(IROLD,3)) THEN
              IRNEW=IROLD-1
            ELSEIF(IROLD.LT.N.AND.K(IROLD+1,3).EQ.K(IROLD,3)) THEN
              IRNEW=IROLD+1
C...Last resort: pick at random among other primaries.
            ELSE
              ISTEP=MAX(1,MIN(NPART-1,INT(1D0+(NPART-1)*PYR(0))))
              IRNEW=IPART(1+MOD(IP+ISTEP-1,NPART))
            ENDIF
C...Trace down if sister branched.
  190       IF(K(IRNEW,1).GT.10) THEN
              DO 200 IR=IRNEW+1,N
                IF(K(IR,3).EQ.IRNEW.AND.K(IR,2).EQ.K(IRNEW,2)) THEN
                  IRNEW=IR
                  GOTO 190
                ENDIF
  200         CONTINUE
            ENDIF
          ENDIF
          IREC(NEVOL)=IRNEW
        ENDIF
 
C...End loop to set up showering partons. System invariant mass.
  210 CONTINUE
      IF(NEVOL.LE.0) RETURN
      PSUM(5)=SQRT(MAX(0D0,PSUM(4)**2-PSUM(1)**2-PSUM(2)**2-PSUM(3)**2))
 
C...Check if 3-jet matrix elements to be used.
      M3JC=0
      ALPHA=0.5D0
      NMESYS=0
      IF(MSTJ(47).GE.1) THEN
 
C...Identify source: q(1), ~q(2), V(3), S(4), chi(5), ~g(6), unknown(0).
        KFSRCE=0
        IPART1=K(IPART(1),3)
        IPART2=K(IPART(2),3)
  220   IF(IPART1.EQ.IPART2.AND.IPART1.GT.0) THEN
          KFSRCE=IABS(K(IPART1,2))
        ELSEIF(IPART1.GT.IPART2.AND.IPART2.GT.0) THEN
          IPART1=K(IPART1,3)
          GOTO 220
        ELSEIF(IPART2.GT.IPART1.AND.IPART1.GT.0) THEN
          IPART2=K(IPART2,3)
          GOTO 220
        ENDIF
        ITYPES=0
        IF(KFSRCE.GE.1.AND.KFSRCE.LE.8) ITYPES=1
        IF(KFSRCE.GE.KSUSY1+1.AND.KFSRCE.LE.KSUSY1+8) ITYPES=2
        IF(KFSRCE.GE.KSUSY2+1.AND.KFSRCE.LE.KSUSY2+8) ITYPES=2
        IF(KFSRCE.GE.21.AND.KFSRCE.LE.24) ITYPES=3
        IF(KFSRCE.GE.32.AND.KFSRCE.LE.34) ITYPES=3
        IF(KFSRCE.EQ.25.OR.(KFSRCE.GE.35.AND.KFSRCE.LE.37)) ITYPES=4
        IF(KFSRCE.GE.KSUSY1+22.AND.KFSRCE.LE.KSUSY1+37) ITYPES=5
        IF(KFSRCE.EQ.KSUSY1+21) ITYPES=6
 
C...Identify two primary showerers.
        KFLA1=IABS(K(IPART(1),2))
        ITYPE1=0
        IF(KFLA1.GE.1.AND.KFLA1.LE.8) ITYPE1=1
        IF(KFLA1.GE.KSUSY1+1.AND.KFLA1.LE.KSUSY1+8) ITYPE1=2
        IF(KFLA1.GE.KSUSY2+1.AND.KFLA1.LE.KSUSY2+8) ITYPE1=2
        IF(KFLA1.GE.21.AND.KFLA1.LE.24) ITYPE1=3
        IF(KFLA1.GE.32.AND.KFLA1.LE.34) ITYPE1=3
        IF(KFLA1.EQ.25.OR.(KFLA1.GE.35.AND.KFLA1.LE.37)) ITYPE1=4
        IF(KFLA1.GE.KSUSY1+22.AND.KFLA1.LE.KSUSY1+37) ITYPE1=5
        IF(KFLA1.EQ.KSUSY1+21) ITYPE1=6
        KFLA2=IABS(K(IPART(2),2))
        ITYPE2=0
        IF(KFLA2.GE.1.AND.KFLA2.LE.8) ITYPE2=1
        IF(KFLA2.GE.KSUSY1+1.AND.KFLA2.LE.KSUSY1+8) ITYPE2=2
        IF(KFLA2.GE.KSUSY2+1.AND.KFLA2.LE.KSUSY2+8) ITYPE2=2
        IF(KFLA2.GE.21.AND.KFLA2.LE.24) ITYPE2=3
        IF(KFLA2.GE.32.AND.KFLA2.LE.34) ITYPE2=3
        IF(KFLA2.EQ.25.OR.(KFLA2.GE.35.AND.KFLA2.LE.37)) ITYPE2=4
        IF(KFLA2.GE.KSUSY1+22.AND.KFLA2.LE.KSUSY1+37) ITYPE2=5
        IF(KFLA2.EQ.KSUSY1+21) ITYPE2=6
 
C...Order of showerers. Presence of gluino.
        ITYPMN=MIN(ITYPE1,ITYPE2)
        ITYPMX=MAX(ITYPE1,ITYPE2)
        IORD=1
        IF(ITYPE1.GT.ITYPE2) IORD=2
        IGLUI=0
        IF(ITYPE1.EQ.6.OR.ITYPE2.EQ.6) IGLUI=1
 
C...Require exactly two primary showerers for ME corrections.
        NPRIM=0
        DO 230 I=1,N
          IF(K(I,3).EQ.IPART1.AND.K(I,2).NE.K(IPART1,2)) NPRIM=NPRIM+1
  230   CONTINUE
        IF(NPRIM.NE.2) THEN
 
C...Predetermined and default matrix element kinds.
        ELSEIF(MSTJ(38).NE.0) THEN
          M3JC=MSTJ(38)
          ALPHA=PARJ(80)
          MSTJ(38)=0
        ELSEIF(MSTJ(47).GE.6) THEN
          M3JC=MSTJ(47)
        ELSE
          ICLASS=1
          ICOMBI=4
 
C...Vector/axial vector -> q + qbar; q -> q + V.
          IF(ITYPMN.EQ.1.AND.ITYPMX.EQ.1.AND.(ITYPES.EQ.0.OR.
     &    ITYPES.EQ.3)) THEN
            ICLASS=2
            IF(KFSRCE.EQ.21.OR.KFSRCE.EQ.22) THEN
              ICOMBI=1
            ELSEIF(KFSRCE.EQ.23.OR.(KFSRCE.EQ.0.AND.
     &      K(IPART(1),2)+K(IPART(2),2).EQ.0)) THEN
C...gamma*/Z0: assume e+e- initial state if unknown.
              EI=-1D0
              IF(KFSRCE.EQ.23) THEN
                IANNFL=K(IPART1,3)
                IF(K(IANNFL,2).EQ.23) IANNFL=K(IANNFL,3)
                IF(IANNFL.NE.0) THEN
                  KANNFL=IABS(K(IANNFL,2))
                  IF(KANNFL.GE.1.AND.KANNFL.LE.18) EI=KCHG(KANNFL,1)/3D0
                ENDIF
              ENDIF
              AI=SIGN(1D0,EI+0.1D0)
              VI=AI-4D0*EI*PARU(102)
              EF=KCHG(KFLA1,1)/3D0
              AF=SIGN(1D0,EF+0.1D0)
              VF=AF-4D0*EF*PARU(102)
              XWC=1D0/(16D0*PARU(102)*(1D0-PARU(102)))
              SH=PSUM(5)**2
              SQMZ=PMAS(23,1)**2
              SQWZ=PSUM(5)*PMAS(23,2)
              SBWZ=1D0/((SH-SQMZ)**2+SQWZ**2)
              VECT=EI**2*EF**2+2D0*EI*VI*EF*VF*XWC*SH*(SH-SQMZ)*SBWZ+
     &        (VI**2+AI**2)*VF**2*XWC**2*SH**2*SBWZ
              AXIV=(VI**2+AI**2)*AF**2*XWC**2*SH**2*SBWZ
              ICOMBI=3
              ALPHA=VECT/(VECT+AXIV)
            ELSEIF(KFSRCE.EQ.24.OR.KFSRCE.EQ.0) THEN
              ICOMBI=4
            ENDIF
C...For chi -> chi q qbar, use V/A -> q qbar as first approximation.
          ELSEIF(ITYPMN.EQ.1.AND.ITYPMX.EQ.1.AND.ITYPES.EQ.5) THEN
            ICLASS=2
          ELSEIF(ITYPMN.EQ.1.AND.ITYPMX.EQ.3.AND.(ITYPES.EQ.0.OR.
     &    ITYPES.EQ.1)) THEN
            ICLASS=3
 
C...Scalar/pseudoscalar -> q + qbar; q -> q + S.
          ELSEIF(ITYPMN.EQ.1.AND.ITYPMX.EQ.1.AND.ITYPES.EQ.4) THEN
            ICLASS=4
            IF(KFSRCE.EQ.25.OR.KFSRCE.EQ.35.OR.KFSRCE.EQ.37) THEN
              ICOMBI=1
            ELSEIF(KFSRCE.EQ.36) THEN
              ICOMBI=2
            ENDIF
          ELSEIF(ITYPMN.EQ.1.AND.ITYPMX.EQ.4.AND.(ITYPES.EQ.0.OR.
     &    ITYPES.EQ.1)) THEN
            ICLASS=5
 
C...V -> ~q + ~qbar; ~q -> ~q + V; S -> ~q + ~qbar; ~q -> ~q + S.
          ELSEIF(ITYPMN.EQ.2.AND.ITYPMX.EQ.2.AND.(ITYPES.EQ.0.OR.
     &    ITYPES.EQ.3)) THEN
            ICLASS=6
          ELSEIF(ITYPMN.EQ.2.AND.ITYPMX.EQ.3.AND.(ITYPES.EQ.0.OR.
     &    ITYPES.EQ.2)) THEN
            ICLASS=7
          ELSEIF(ITYPMN.EQ.2.AND.ITYPMX.EQ.2.AND.ITYPES.EQ.4) THEN
            ICLASS=8
          ELSEIF(ITYPMN.EQ.2.AND.ITYPMX.EQ.4.AND.(ITYPES.EQ.0.OR.
     &    ITYPES.EQ.2)) THEN
            ICLASS=9
 
C...chi -> q + ~qbar; ~q -> q + chi; q -> ~q + chi.
          ELSEIF(ITYPMN.EQ.1.AND.ITYPMX.EQ.2.AND.(ITYPES.EQ.0.OR.
     &    ITYPES.EQ.5)) THEN
            ICLASS=10
          ELSEIF(ITYPMN.EQ.1.AND.ITYPMX.EQ.5.AND.(ITYPES.EQ.0.OR.
     &    ITYPES.EQ.2)) THEN
            ICLASS=11
          ELSEIF(ITYPMN.EQ.2.AND.ITYPMX.EQ.5.AND.(ITYPES.EQ.0.OR.
     &    ITYPES.EQ.1)) THEN
            ICLASS=12
 
C...~g -> q + ~qbar; ~q -> q + ~g; q -> ~q + ~g.
          ELSEIF(ITYPMN.EQ.1.AND.ITYPMX.EQ.2.AND.ITYPES.EQ.6) THEN
            ICLASS=13
          ELSEIF(ITYPMN.EQ.1.AND.ITYPMX.EQ.6.AND.(ITYPES.EQ.0.OR.
     &    ITYPES.EQ.2)) THEN
            ICLASS=14
          ELSEIF(ITYPMN.EQ.2.AND.ITYPMX.EQ.6.AND.(ITYPES.EQ.0.OR.
     &    ITYPES.EQ.1)) THEN
            ICLASS=15
 
C...g -> ~g + ~g (eikonal approximation).
          ELSEIF(ITYPMN.EQ.6.AND.ITYPMX.EQ.6.AND.ITYPES.EQ.0) THEN
            ICLASS=16
          ENDIF
          M3JC=5*ICLASS+ICOMBI
        ENDIF
 
C...Store pair that together define matrix element treatment.
        IF(M3JC.NE.0) THEN
          NMESYS=1
          MESYS(NMESYS,0)=M3JC
          MESYS(NMESYS,1)=IPART(1)
          MESYS(NMESYS,2)=IPART(2)
        ENDIF
 
C...Store qqbar or l+l- pairs for QED radiation.
        IF(KFLA1.LE.18.AND.KFLA2.LE.18) THEN
          NMESYS=NMESYS+1
          MESYS(NMESYS,0)=101
          IF(K(IPART(1),2)+K(IPART(2),2).EQ.0) MESYS(NMESYS,0)=102
          MESYS(NMESYS,1)=IPART(1)
          MESYS(NMESYS,2)=IPART(2)
        ENDIF
 
C...Store other qqbar/l+l- pairs from g/gamma branchings.
        DO 270 I1=1,N
          IF(K(I1,1).GT.10.OR.IABS(K(I1,2)).GT.18) GOTO 270
          I1M=K(I1,3)
  240     IF(I1M.GT.0.AND.K(I1M,2).EQ.K(I1,2)) THEN
            I1M=K(I1M,3)
            GOTO 240
          ENDIF
C...Move up this check to avoid out-of-bounds.
          IF(I1M.EQ.0) GOTO 270
          IF(K(I1M,2).NE.21.AND.K(I1M,2).NE.22) GOTO 270
          DO 260 I2=I1+1,N
            IF(K(I2,1).GT.10.OR.K(I2,2)+K(I1,2).NE.0) GOTO 260
            I2M=K(I2,3)
  250       IF(I2M.GT.0.AND.K(I2M,2).EQ.K(I2,2)) THEN
              I2M=K(I2M,3)
              GOTO 250
            ENDIF
            IF(I1M.EQ.I2M.AND.I1M.GT.0) THEN
              NMESYS=NMESYS+1
              MESYS(NMESYS,0)=66
              MESYS(NMESYS,1)=I1
              MESYS(NMESYS,2)=I2
              NMESYS=NMESYS+1
              MESYS(NMESYS,0)=102
              MESYS(NMESYS,1)=I1
              MESYS(NMESYS,2)=I2
            ENDIF
  260     CONTINUE
  270   CONTINUE
      ENDIF
 
C..Loopback point for counting number of emissions.
      NGEN=0
  280 NGEN=NGEN+1
 
C...Begin loop to evolve all existing partons, if required.
  290 IMX=0
      PT2MX=0D0
      DO 360 IEVOL=1,NEVOL
        IF(IFLG(IEVOL).EQ.0) THEN
 
C...Basic info on radiator and recoil.
          I=IPOS(IEVOL)
          IR=IREC(IEVOL)
          SHT=SHAT(I,IR)
          PM2I=P(I,5)**2
          PM2R=P(IR,5)**2
 
C...Invariant mass of "dipole".Starting value for pT evolution.
          SHTCOR=(SQRT(SHT)-P(IR,5))**2-PM2I
          PT2=MIN(PT2CMX,0.25D0*SHTCOR,PTSCA(IEVOL)**2)
 
C...Case of evolution by QCD branching.
          IF(ISCOL(IEVOL).NE.0) THEN
 
C...Parton-by-parton maximum scale from initial conditions.
          IF(MSTP(72).EQ.0) THEN
            DO 300 IPRT=1,NPARTS
              IF(IR.EQ.IPART(IPRT)) PT2=MIN(PT2,PTPART(IPRT)**2)
  300       CONTINUE
          ENDIF
 
C...If kinematically impossible then do not evolve.
            IF(PT2.LT.PT2CMN) THEN
              IFLG(IEVOL)=-1
              GOTO 360
            ENDIF
 
C...Check if part of system for which ME corrections should be applied.
            IMESYS=0
            DO 310 IME=1,NMESYS
              IF((I.EQ.MESYS(IME,1).OR.I.EQ.MESYS(IME,2)).AND.
     &        MESYS(IME,0).LT.100) IMESYS=IME
  310       CONTINUE
 
C...Special flag for colour octet states.
            MOCT=0
            IF(K(I,2).EQ.21) MOCT=1
            IF(K(I,2).EQ.KSUSY1+21) MOCT=2
 
C...Upper estimate for matrix element weighting and colour factor.
C...Note that g->gg and g->qqbar is split on two sides = "dipoles".
            WTPSGL=2D0
            COLFAC=4D0/3D0
            IF(MOCT.GE.1) COLFAC=3D0/2D0
            IF(IGLUI.EQ.1.AND.IMESYS.EQ.1.AND.MOCT.EQ.0) COLFAC=3D0
            WTPSQQ=0.5D0*0.5D0*NFLAV
 
C...Determine overestimated z range: switch at c and b masses.
  320       IZRG=1
            PT2MNE=PT2CMN
            B0=27D0/6D0
            ALAMS=ALAM3S
            IF(PT2.GT.1.01D0*PMCS) THEN
              IZRG=2
              PT2MNE=PMCS
              B0=25D0/6D0
              ALAMS=ALAM4S
            ENDIF
            IF(PT2.GT.1.01D0*PMBS) THEN
              IZRG=3
              PT2MNE=PMBS
              B0=23D0/6D0
              ALAMS=ALAM5S
            ENDIF
            ZMNCUT=0.5D0-SQRT(MAX(0D0,0.25D0-PT2MNE/SHTCOR))
            IF(ZMNCUT.LT.1D-8) ZMNCUT=PT2MNE/SHTCOR
 
C...Find evolution coefficients for q->qg/g->gg and g->qqbar.
            EVEMGL=WTPSGL*COLFAC*LOG(1D0/ZMNCUT-1D0)/B0
            EVCOEF=EVEMGL
            IF(MOCT.EQ.1) THEN
              EVEMQQ=WTPSQQ*(1D0-2D0*ZMNCUT)/B0
              EVCOEF=EVCOEF+EVEMQQ
            ENDIF
 
C...Pick pT2 (in overestimated z range).
  330       PT2=ALAMS*(PT2/ALAMS)**(PYR(0)**(1D0/EVCOEF))
 
C...Loopback if crossed c/b mass thresholds.
            IF(IZRG.EQ.3.AND.PT2.LT.PMBS) THEN
              PT2=PMBS
              GOTO 320
            ENDIF
            IF(IZRG.EQ.2.AND.PT2.LT.PMCS) THEN
              PT2=PMCS
              GOTO 320
            ENDIF
 
C...Finish if below lower cutoff.
            IF(PT2.LT.PT2CMN) THEN
              IFLG(IEVOL)=-1
              GOTO 360
            ENDIF
 
C...Pick kind of branching: q->qg/g->gg/X->Xg or g->qqbar.
            IFLAG=1
            IF(MOCT.EQ.1.AND.EVEMGL.LT.PYR(0)*EVCOEF) IFLAG=2
 
C...Pick z: dz/(1-z) or dz.
            IF(IFLAG.EQ.1) THEN
              Z=1D0-ZMNCUT*(1D0/ZMNCUT-1D0)**PYR(0)
            ELSE
              Z=ZMNCUT+PYR(0)*(1D0-2D0*ZMNCUT)
            ENDIF
 
C...Loopback if outside allowed range for given pT2.
            ZMNNOW=0.5D0-SQRT(MAX(0D0,0.25D0-PT2/SHTCOR))
            IF(ZMNNOW.LT.1D-8) ZMNNOW=PT2/SHTCOR
            IF(Z.LE.ZMNNOW.OR.Z.GE.1D0-ZMNNOW) GOTO 330
            PM2=PM2I+PT2/(Z*(1D0-Z))
            IF(Z*(1D0-Z).LE.PM2*SHT/(SHT+PM2-PM2R)**2) GOTO 330
 
C...No weighting for primary partons; to be done later on.
            IF(IMESYS.GT.0) THEN
 
C...Weighting of q->qg/X->Xg branching.
            ELSEIF(IFLAG.EQ.1.AND.MOCT.NE.1) THEN
              IF(1D0+Z**2.LT.WTPSGL*PYR(0)) GOTO 330
 
C...Weighting of g->gg branching.
            ELSEIF(IFLAG.EQ.1) THEN
              IF(1D0+Z**3.LT.WTPSGL*PYR(0)) GOTO 330
 
C...Flavour choice and weighting of g->qqbar branching.
            ELSE
              KFQ=MIN(5,1+INT(NFLAV*PYR(0)))
              PMQ=PMAS(KFQ,1)
              ROOTQQ=SQRT(MAX(0D0,1D0-4D0*PMQ**2/PM2))
              WTME=ROOTQQ*(Z**2+(1D0-Z)**2)
              IF(WTME.LT.PYR(0)) GOTO 330
              IFLAG=10+KFQ
            ENDIF
 
C...Case of evolution by QED branching.
          ELSEIF(ISCHG(IEVOL).NE.0) THEN
 
C...If kinematically impossible then do not evolve.
            PT2EMN=PT0EQ**2
            IF(IABS(K(I,2)).GT.10) PT2EMN=PT0EL**2
            IF(PT2.LT.PT2EMN) THEN
              IFLG(IEVOL)=-1
              GOTO 360
            ENDIF
 
C...Check if part of system for which ME corrections should be applied.
           IMESYS=0
            DO 340 IME=1,NMESYS
              IF((I.EQ.MESYS(IME,1).OR.I.EQ.MESYS(IME,2)).AND.
     &        MESYS(IME,0).GT.100) IMESYS=IME
  340      CONTINUE
 
C...Charge. Matrix element weighting factor.
            CHG=ISCHG(IEVOL)/3D0
            WTPSGA=2D0
 
C...Determine overestimated z range. Find evolution coefficient.
            ZMNCUT=0.5D0-SQRT(MAX(0D0,0.25D0-PT2EMN/SHTCOR))
            IF(ZMNCUT.LT.1D-8) ZMNCUT=PT2EMN/SHTCOR
            EVCOEF=AEM2PI*CHG**2*WTPSGA*LOG(1D0/ZMNCUT-1D0)
 
C...Pick pT2 (in overestimated z range).
  350       PT2=PT2*PYR(0)**(1D0/EVCOEF)
 
C...Finish if below lower cutoff.
            IF(PT2.LT.PT2EMN) THEN
              IFLG(IEVOL)=-1
              GOTO 360
            ENDIF
 
C...Pick z: dz/(1-z).
            Z=1D0-ZMNCUT*(1D0/ZMNCUT-1D0)**PYR(0)
 
C...Loopback if outside allowed range for given pT2.
            ZMNNOW=0.5D0-SQRT(MAX(0D0,0.25D0-PT2/SHTCOR))
            IF(ZMNNOW.LT.1D-8) ZMNNOW=PT2/SHTCOR
            IF(Z.LE.ZMNNOW.OR.Z.GE.1D0-ZMNNOW) GOTO 350
            PM2=PM2I+PT2/(Z*(1D0-Z))
            IF(Z*(1D0-Z).LE.PM2*SHT/(SHT+PM2-PM2R)**2) GOTO 350
 
C...Weighting by branching kernel, except if ME weighting later.
            IF(IMESYS.EQ.0) THEN
              IF(1D0+Z**2.LT.WTPSGA*PYR(0)) GOTO 350
            ENDIF
            IFLAG=3
          ENDIF
 
C...Save acceptable branching.
          IFLG(IEVOL)=IFLAG
          IMESAV(IEVOL)=IMESYS
          PT2SAV(IEVOL)=PT2
          ZSAV(IEVOL)=Z
          SHTSAV(IEVOL)=SHT
        ENDIF
 
C...Check if branching has highest pT.
        IF(IFLG(IEVOL).GE.1.AND.PT2SAV(IEVOL).GT.PT2MX) THEN
          IMX=IEVOL
          PT2MX=PT2SAV(IEVOL)
        ENDIF
  360 CONTINUE
 
C...Finished if no more branchings to be done.
      IF(IMX.EQ.0) GOTO 480
 
C...Restore info on hardest branching to be processed.
      I=IPOS(IMX)
      IR=IREC(IMX)
      KCOL=ISCOL(IMX)
      KCHA=ISCHG(IMX)
      IMESYS=IMESAV(IMX)
      PT2=PT2SAV(IMX)
      Z=ZSAV(IMX)
      SHT=SHTSAV(IMX)
      PM2I=P(I,5)**2
      PM2R=P(IR,5)**2
      PM2=PM2I+PT2/(Z*(1D0-Z))
 
C...Special flag for colour octet states.
      MOCT=0
      IF(K(I,2).EQ.21) MOCT=1
      IF(K(I,2).EQ.KSUSY1+21) MOCT=2
 
C...Restore further info for g->qqbar branching.
      KFQ=0
      IF(IFLG(IMX).GT.10) THEN
        KFQ=IFLG(IMX)-10
        PMQ=PMAS(KFQ,1)
        ROOTQQ=SQRT(MAX(0D0,1D0-4D0*PMQ**2/PM2))
      ENDIF
 
C...For branching g include azimuthal asymmetries from polarization.
      ASYPOL=0D0
      IF(MOCT.EQ.1.AND.MOD(MSTJ(46),2).EQ.1) THEN
C...Trace grandmother via intermediate recoil copies.
        KFGM=0
        IM=I
  370   IF(K(IM,3).NE.K(IM-1,3).AND.K(IM,3).NE.K(IM+1,3).AND.
     &  K(IM,3).GT.0) THEN
          IM=K(IM,3)
          IF(IM.GT.MINT(84)) GOTO 370
        ENDIF
        IGM=K(IM,3)
        IF(IGM.GT.MINT(84).AND.IGM.LT.IM.AND.IM.LE.I)
     &  KFGM=IABS(K(IGM,2))
C...Define approximate energy sharing by identifying aunt.
        IAU=IM+1
        IF(IAU.GT.N-3.OR.K(IAU,3).NE.IGM) IAU=IM-1
        IF(KFGM.NE.0.AND.(KFGM.LE.6.OR.KFGM.EQ.21)) THEN
          ZOLD=P(IM,4)/(P(IM,4)+P(IAU,4))
C...Coefficient from gluon production.
          IF(KFGM.LE.6) THEN
            ASYPOL=2D0*(1D0-ZOLD)/(1D0+(1D0-ZOLD)**2)
          ELSE
            ASYPOL=((1D0-ZOLD)/(1D0-ZOLD*(1D0-ZOLD)))**2
          ENDIF
C...Coefficient from gluon decay.
          IF(KFQ.EQ.0) THEN
            ASYPOL=ASYPOL*(Z*(1D0-Z)/(1D0-Z*(1D0-Z)))**2
          ELSE
            ASYPOL=-ASYPOL*2D0*Z*(1D0-Z)/(1D0-2D0*Z*(1D0-Z))
          ENDIF
        ENDIF
      ENDIF
 
C...Create new slots for branching products and recoil.
      INEW=N+1
      IGNEW=N+2
      IRNEW=N+3
      N=N+3
 
C...Set status, flavour and mother of new ones.
      K(INEW,1)=K(I,1)
      K(IGNEW,1)=3
      IF(KCHA.NE.0)  K(IGNEW,1)=1
      K(IRNEW,1)=K(IR,1)
      IF(KFQ.EQ.0) THEN
        K(INEW,2)=K(I,2)
        K(IGNEW,2)=21
        IF(KCHA.NE.0)  K(IGNEW,2)=22
      ELSE
        K(INEW,2)=-ISIGN(KFQ,KCOL)
        K(IGNEW,2)=-K(INEW,2)
      ENDIF
      K(IRNEW,2)=K(IR,2)
      K(INEW,3)=I
      K(IGNEW,3)=I
      K(IRNEW,3)=IR
 
C...Find rest frame and angles of branching+recoil.
      DO 380 J=1,5
        P(INEW,J)=P(I,J)
        P(IGNEW,J)=0D0
        P(IRNEW,J)=P(IR,J)
  380 CONTINUE
      BETAX=(P(INEW,1)+P(IRNEW,1))/(P(INEW,4)+P(IRNEW,4))
      BETAY=(P(INEW,2)+P(IRNEW,2))/(P(INEW,4)+P(IRNEW,4))
      BETAZ=(P(INEW,3)+P(IRNEW,3))/(P(INEW,4)+P(IRNEW,4))
      CALL PYROBO(INEW,IRNEW,0D0,0D0,-BETAX,-BETAY,-BETAZ)
      PHI=PYANGL(P(INEW,1),P(INEW,2))
      THETA=PYANGL(P(INEW,3),SQRT(P(INEW,1)**2+P(INEW,2)**2))
 
C...Derive kinematics of branching: generics (like g->gg).
      DO 390 J=1,4
        P(INEW,J)=0D0
        P(IRNEW,J)=0D0
  390 CONTINUE
      PEM=0.5D0*(SHT+PM2-PM2R)/SQRT(SHT)
      PZM=0.5D0*SQRT(MAX(0D0,(SHT-PM2-PM2R)**2-4D0*PM2*PM2R)/SHT)
      PT2COR=PM2*(PEM**2*Z*(1D0-Z)-0.25D0*PM2)/PZM**2
      PTCOR=SQRT(MAX(0D0,PT2COR))
      PZN=(PEM**2*Z-0.5D0*PM2)/PZM
      PZG=(PEM**2*(1D0-Z)-0.5D0*PM2)/PZM
C...Specific kinematics reduction for q->qg with m_q > 0.
      IF(MOCT.NE.1) THEN
        PTCOR=(1D0-PM2I/PM2)*PTCOR
        PZN=PZN+PM2I*PZG/PM2
        PZG=(1D0-PM2I/PM2)*PZG
C...Specific kinematics reduction for g->qqbar with m_q > 0.
      ELSEIF(KFQ.NE.0) THEN
        P(INEW,5)=PMQ
        P(IGNEW,5)=PMQ
        PTCOR=ROOTQQ*PTCOR
        PZN=0.5D0*((1D0+ROOTQQ)*PZN+(1D0-ROOTQQ)*PZG)
        PZG=PZM-PZN
      ENDIF
 
C...Pick phi and construct kinematics of branching.
  400 PHIROT=PARU(2)*PYR(0)
      P(INEW,1)=PTCOR*COS(PHIROT)
      P(INEW,2)=PTCOR*SIN(PHIROT)
      P(INEW,3)=PZN
      P(INEW,4)=SQRT(PTCOR**2+P(INEW,3)**2+P(INEW,5)**2)
      P(IGNEW,1)=-P(INEW,1)
      P(IGNEW,2)=-P(INEW,2)
      P(IGNEW,3)=PZG
      P(IGNEW,4)=SQRT(PTCOR**2+P(IGNEW,3)**2+P(IGNEW,5)**2)
      P(IRNEW,1)=0D0
      P(IRNEW,2)=0D0
      P(IRNEW,3)=-PZM
      P(IRNEW,4)=0.5D0*(SHT+PM2R-PM2)/SQRT(SHT)
 
C...Boost branching system to lab frame.
      CALL PYROBO(INEW,IRNEW,THETA,PHI,BETAX,BETAY,BETAZ)
 
C...Renew choice of phi angle according to polarization asymmetry.
      IF(ABS(ASYPOL).GT.1D-3) THEN
        DO 410 J=1,3
          DPT(1,J)=P(I,J)
          DPT(2,J)=P(IAU,J)
          DPT(3,J)=P(INEW,J)
  410   CONTINUE
        DPMA=DPT(1,1)*DPT(2,1)+DPT(1,2)*DPT(2,2)+DPT(1,3)*DPT(2,3)
        DPMD=DPT(1,1)*DPT(3,1)+DPT(1,2)*DPT(3,2)+DPT(1,3)*DPT(3,3)
        DPMM=DPT(1,1)**2+DPT(1,2)**2+DPT(1,3)**2
        DO 420 J=1,3
          DPT(4,J)=DPT(2,J)-DPMA*DPT(1,J)/MAX(1D-10,DPMM)
          DPT(5,J)=DPT(3,J)-DPMD*DPT(1,J)/MAX(1D-10,DPMM)
  420   CONTINUE
        DPT(4,4)=SQRT(DPT(4,1)**2+DPT(4,2)**2+DPT(4,3)**2)
        DPT(5,4)=SQRT(DPT(5,1)**2+DPT(5,2)**2+DPT(5,3)**2)
        IF(MIN(DPT(4,4),DPT(5,4)).GT.0.1D0*PARJ(82)) THEN
          CAD=(DPT(4,1)*DPT(5,1)+DPT(4,2)*DPT(5,2)+
     &    DPT(4,3)*DPT(5,3))/(DPT(4,4)*DPT(5,4))
          IF(1D0+ASYPOL*(2D0*CAD**2-1D0).LT.PYR(0)*(1D0+ABS(ASYPOL)))
     &    GOTO 400
        ENDIF
      ENDIF
 
C...Matrix element corrections for primary partons when requested.
      IF(IMESYS.GT.0) THEN
        M3JC=MESYS(IMESYS,0)
 
C...Identify recoiling partner and set up three-body kinematics.
        IRP=MESYS(IMESYS,1)
        IF(IRP.EQ.I) IRP=MESYS(IMESYS,2)
        IF(IRP.EQ.IR) IRP=IRNEW
        DO 430 J=1,4
          PSUM(J)=P(INEW,J)+P(IRP,J)+P(IGNEW,J)
  430   CONTINUE
        PSUM(5)=SQRT(MAX(0D0,PSUM(4)**2-PSUM(1)**2-PSUM(2)**2-
     &  PSUM(3)**2))
        X1=2D0*(PSUM(4)*P(INEW,4)-PSUM(1)*P(INEW,1)-PSUM(2)*P(INEW,2)-
     &  PSUM(3)*P(INEW,3))/PSUM(5)**2
        X2=2D0*(PSUM(4)*P(IRP,4)-PSUM(1)*P(IRP,1)-PSUM(2)*P(IRP,2)-
     &  PSUM(3)*P(IRP,3))/PSUM(5)**2
        X3=2D0-X1-X2
        R1ME=P(INEW,5)/PSUM(5)
        R2ME=P(IRP,5)/PSUM(5)
 
C...Matrix elements for gluon emission.
        IF(M3JC.LT.100) THEN
 
C...Call ME, with right order important for two inequivalent showerers.
          IF(MESYS(IMESYS,IORD).EQ.I) THEN
            WME=PYMAEL(M3JC,X1,X2,R1ME,R2ME,ALPHA)
          ELSE
            WME=PYMAEL(M3JC,X2,X1,R2ME,R1ME,ALPHA)
          ENDIF
 
C...Split up total ME when two radiating partons.
          ISPRAD=1
          IF((M3JC.GE.16.AND.M3JC.LE.19).OR.(M3JC.GE.26.AND.M3JC.LE.29)
     &    .OR.(M3JC.GE.36.AND.M3JC.LE.39).OR.(M3JC.GE.46.AND.M3JC.LE.49)
     &    .OR.(M3JC.GE.56.AND.M3JC.LE.64)) ISPRAD=0
          IF(ISPRAD.EQ.1) WME=WME*MAX(1D-10,1D0+R1ME**2-R2ME**2-X1)/
     &    MAX(1D-10,2D0-X1-X2)
 
C...Evaluate shower rate.
          WPS=2D0/(MAX(1D-10,2D0-X1-X2)*
     &    MAX(1D-10,1D0+R2ME**2-R1ME**2-X2))
          IF(IGLUI.EQ.1) WPS=(9D0/4D0)*WPS
 
C...Matrix elements for photon emission: still rather primitive.
        ELSE
 
C...For generic charge combination currently only massless expression.
          IF(M3JC.EQ.101) THEN
            CHG1=KCHG(PYCOMP(K(I,2)),1)*ISIGN(1,K(I,2))/3D0
            CHG2=KCHG(PYCOMP(K(IRP,2)),1)*ISIGN(1,K(IRP,2))/3D0
            WME=(CHG1*(1D0-X1)/X3-CHG2*(1D0-X2)/X3)**2*(X1**2+X2**2)
            WPS=2D0*(CHG1**2*(1D0-X1)/X3+CHG2**2*(1D0-X2)/X3)
 
C...For flavour neutral system assume vector source and include masses.
          ELSE
            WME=PYMAEL(11,X1,X2,R1ME,R2ME,0D0)*MAX(1D-10,
     &      1D0+R1ME**2-R2ME**2-X1)/MAX(1D-10,2D0-X1-X2)
            WPS=2D0/(MAX(1D-10,2D0-X1-X2)*
     &      MAX(1D-10,1D0+R2ME**2-R1ME**2-X2))
          ENDIF
        ENDIF
 
C...Perform weighting with W_ME/W_PS.
        IF(WME.LT.PYR(0)*WPS) THEN
          N=N-3
          IFLG(IMX)=0
          GOTO 290
        ENDIF
      ENDIF
 
C...Now for sure accepted branching. Save highest pT.
      IF(NGEN.EQ.1) PTGEN=SQRT(PT2)
 
C...Update status for obsolete ones. Bookkkep the moved original parton
C...and new daughter (arbitrary choice for g->gg or g->qqbar).
C...Do not bookkeep radiated photon, since it cannot radiate further.
      K(I,1)=K(I,1)+10
      K(IR,1)=K(IR,1)+10
      DO 440 IP=1,NPART
        IF(IPART(IP).EQ.I) IPART(IP)=INEW
        IF(IPART(IP).EQ.IR) IPART(IP)=IRNEW
  440 CONTINUE
      IF(KCHA.EQ.0) THEN
        NPART=NPART+1
        IPART(NPART)=IGNEW
      ENDIF
 
C...Initialize colour flow of branching.
C...Use both old and new style colour tags for flexibility.
      K(INEW,4)=0
      K(IGNEW,4)=0
      K(INEW,5)=0
      K(IGNEW,5)=0
      JCOLP=4+(1-KCOL)/2
      JCOLN=9-JCOLP
      MCT(INEW,1)=0
      MCT(INEW,2)=0
      MCT(IGNEW,1)=0
      MCT(IGNEW,2)=0
      MCT(IRNEW,1)=0
      MCT(IRNEW,2)=0
 
C...Trivial colour flow for l->lgamma and q->qgamma.
      IF(IABS(KCHA).EQ.3) THEN
        K(I,4)=INEW
        K(I,5)=IGNEW
      ELSEIF(KCHA.NE.0) THEN
        IF(K(I,4).NE.0) THEN
          K(I,4)=K(I,4)+INEW
          K(INEW,4)=MSTU(5)*I
          MCT(INEW,1)=MCT(I,1)
        ENDIF
        IF(K(I,5).NE.0) THEN
          K(I,5)=K(I,5)+INEW
          K(INEW,5)=MSTU(5)*I
          MCT(INEW,2)=MCT(I,2)
        ENDIF
 
C...Set colour flow for q->qg and g->gg.
      ELSEIF(KFQ.EQ.0) THEN
        K(I,JCOLP)=K(I,JCOLP)+IGNEW
        K(IGNEW,JCOLP)=MSTU(5)*I
        K(INEW,JCOLP)=MSTU(5)*IGNEW
        K(IGNEW,JCOLN)=MSTU(5)*INEW
        MCT(IGNEW,JCOLP-3)=MCT(I,JCOLP-3)
        NCT=NCT+1
        MCT(INEW,JCOLP-3)=NCT
        MCT(IGNEW,JCOLN-3)=NCT
        IF(MOCT.GE.1) THEN
          K(I,JCOLN)=K(I,JCOLN)+INEW
          K(INEW,JCOLN)=MSTU(5)*I
          MCT(INEW,JCOLN-3)=MCT(I,JCOLN-3)
        ENDIF
 
C...Set colour flow for g->qqbar.
      ELSE
        K(I,JCOLN)=K(I,JCOLN)+INEW
        K(INEW,JCOLN)=MSTU(5)*I
        K(I,JCOLP)=K(I,JCOLP)+IGNEW
        K(IGNEW,JCOLP)=MSTU(5)*I
        MCT(INEW,JCOLN-3)=MCT(I,JCOLN-3)
        MCT(IGNEW,JCOLP-3)=MCT(I,JCOLP-3)
      ENDIF
 
C...Daughter info for colourless recoiling parton.
      IF(K(IR,4).EQ.0.AND.K(IR,5).EQ.0) THEN
        K(IR,4)=IRNEW
        K(IR,5)=IRNEW
        K(IRNEW,4)=0
        K(IRNEW,5)=0
 
C...Colour of recoiling parton sails through unchanged.
      ELSE
        IF(K(IR,4).NE.0) THEN
          K(IR,4)=K(IR,4)+IRNEW
          K(IRNEW,4)=MSTU(5)*IR
          MCT(IRNEW,1)=MCT(IR,1)
        ENDIF
        IF(K(IR,5).NE.0) THEN
          K(IR,5)=K(IR,5)+IRNEW
          K(IRNEW,5)=MSTU(5)*IR
          MCT(IRNEW,2)=MCT(IR,2)
        ENDIF
      ENDIF
 
C...Vertex information trivial.
      DO 450 J=1,5
        V(INEW,J)=V(I,J)
        V(IGNEW,J)=V(I,J)
        V(IRNEW,J)=V(IR,J)
  450 CONTINUE
 
C...Update list of old radiators.
        DO 460 IEVOL=1,NEVOL
          IF(IPOS(IEVOL).EQ.I.AND.IREC(IEVOL).EQ.IR) THEN
            IPOS(IEVOL)=INEW
            IF(KCOL.NE.0.AND.ISCOL(IEVOL).EQ.KCOL) IPOS(IEVOL)=IGNEW
            IREC(IEVOL)=IRNEW
            IFLG(IEVOL)=0
          ELSEIF(IPOS(IEVOL).EQ.I) THEN
            IPOS(IEVOL)=INEW
            IFLG(IEVOL)=0
          ELSEIF(IPOS(IEVOL).EQ.IR.AND.IREC(IEVOL).EQ.I) THEN
            IPOS(IEVOL)=IRNEW
            IREC(IEVOL)=INEW
            IF(KCOL.NE.0.AND.ISCOL(IEVOL).NE.KCOL) IREC(IEVOL)=IGNEW
            IFLG(IEVOL)=0
          ELSEIF(IPOS(IEVOL).EQ.IR) THEN
            IPOS(IEVOL)=IRNEW
            IFLG(IEVOL)=0
          ENDIF
C...Update links of old connected partons.
          IF(IREC(IEVOL).EQ.I) THEN
            IREC(IEVOL)=INEW
            IFLG(IEVOL)=0
          ELSEIF(IREC(IEVOL).EQ.IR) THEN
            IREC(IEVOL)=IRNEW
            IFLG(IEVOL)=0
          ENDIF
  460   CONTINUE
 
C...q->qg or g->gg: create new gluon radiators.
      IF(KCOL.NE.0.AND.KFQ.EQ.0) THEN
        NEVOL=NEVOL+1
        IPOS(NEVOL)=INEW
        IREC(NEVOL)=IGNEW
        IFLG(NEVOL)=0
        ISCOL(NEVOL)=KCOL
        ISCHG(NEVOL)=0
        PTSCA(NEVOL)=SQRT(PT2)
        NEVOL=NEVOL+1
        IPOS(NEVOL)=IGNEW
        IREC(NEVOL)=INEW
        IFLG(NEVOL)=0
        ISCOL(NEVOL)=-KCOL
        ISCHG(NEVOL)=0
        PTSCA(NEVOL)=PTSCA(NEVOL-1)
      ENDIF
 
C...Update matrix elements parton list and add new for g/gamma->qqbar.
      DO 470 IME=1,NMESYS
        IF(MESYS(IME,1).EQ.I) MESYS(IME,1)=INEW
        IF(MESYS(IME,2).EQ.I) MESYS(IME,2)=INEW
        IF(MESYS(IME,1).EQ.IR) MESYS(IME,1)=IRNEW
        IF(MESYS(IME,2).EQ.IR) MESYS(IME,2)=IRNEW
  470 CONTINUE
      IF(KFQ.NE.0) THEN
        NMESYS=NMESYS+1
        MESYS(NMESYS,0)=66
        MESYS(NMESYS,1)=INEW
        MESYS(NMESYS,2)=IGNEW
        NMESYS=NMESYS+1
        MESYS(NMESYS,0)=102
        MESYS(NMESYS,1)=INEW
        MESYS(NMESYS,2)=IGNEW
      ENDIF
 
C...Global statistics.
      MINT(353)=MINT(353)+1
      VINT(353)=VINT(353)+PTCOR
      IF (MINT(353).EQ.1) VINT(358)=PTCOR
 
C...Loopback for more emissions if enough space.
      PT2CMX=PT2
      IF(NPART.LT.MAXNUR-1.AND.NEVOL.LT.2*MAXNUR-2.AND.
     &NMESYS.LT.MAXNUR-2.AND.N.LT.MSTU(4)-MSTU(32)-5) THEN
        GOTO 280
      ELSE
        CALL PYERRM(11,'(PYPTFS:) no more memory left for shower')
      ENDIF
 
C...Done.
  480 CONTINUE
 
      RETURN
      END
