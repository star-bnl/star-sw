 
C*********************************************************************
 
C...PYSLHA
C...Read/write spectrum or decay data from SLHA standard file(s).
C...P. Skands
 
C...MUPDA=1 : READ SPECTRUM ON LUN=IMSS(21)
C...MUPDA=2 : LOOK FOR DECAY TABLE FOR KF=KFORIG ON LUN=IMSS(22)
C...MUPDA=3 : WRITE SPECTRUM ON LUN=IMSS(23)
C...(MUPDA=4 : WRITE DECAY TABLE FOR KF=KFORIG ON LUN=IMSS(24))
C...MUPDA=5 : READ MASS FOR KF=KFORIG ONLY (WITH DECAY TABLE)
      SUBROUTINE PYSLHA(MUPDA,KFORIG,IRETRN)
 
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
      INTEGER PYK,PYCHGE,PYCOMP
      PARAMETER (KSUSY1=1000000,KSUSY2=2000000,KTECHN=3000000,
     &KEXCIT=4000000,KDIMEN=5000000)
C...Commonblocks.
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYDAT2/KCHG(500,4),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
      COMMON/PYDAT4/CHAF(500,2)
      CHARACTER CHAF*16
      CHARACTER*40 ISAVER,VISAJE
      COMMON/PYINT4/MWID(500),WIDS(500,5)
      SAVE /PYDAT1/,/PYDAT2/,/PYDAT3/,/PYDAT4/,/PYINT4/
C...SUSY blocks
      COMMON/PYMSSM/IMSS(0:99),RMSS(0:99)
      COMMON/PYSSMT/ZMIX(4,4),UMIX(2,2),VMIX(2,2),SMZ(4),SMW(2),
     &SFMIX(16,4),ZMIXI(4,4),UMIXI(2,2),VMIXI(2,2)
      COMMON/PYMSRV/RVLAM(3,3,3), RVLAMP(3,3,3), RVLAMB(3,3,3)
      SAVE /PYMSSM/,/PYSSMT/,/PYMSRV/
 
C...Local arrays, character variables and data.
      COMMON/PYLH3P/MODSEL(200),PARMIN(100),PAREXT(200),RMSOFT(0:100),
     &     AU(3,3),AD(3,3),AE(3,3)
      COMMON/PYLH3C/CPRO(2),CVER(2)
      SAVE /PYLH3P/,/PYLH3C/
      DIMENSION MMOD(100),MSPC(100),MDEC(100)
C...MMOD: flags to set for each block read in.
C... 1: MODSEL     2: MINPAR     3: EXTPAR     4: SMINPUTS
C...MSPC: Flags to set for each block read in.
C... 1: MASS       2: NMIX       3: UMIX       4: VMIX       5: SBOTMIX
C... 6: STOPMIX    7: STAUMIX    8: HMIX       9: GAUGE     10: AU
C...11: AD        12: AE        13: YU        14: YD        15: YE
C...16: SPINFO    17: ALPHA     18: MSOFT     19: QNUMBERS
      CHARACTER CPRO*12,CVER*12,CHNLIN*6
      CHARACTER DOC*11, CHDUM*120, CHBLCK*60
      CHARACTER CHINL*120,CHKF*9,CHTMP*16
      INTEGER VERBOS
      SAVE VERBOS
C...Date of last Change
      PARAMETER (DOC='01 Feb 2007')
C...MQREAD(0): Number of entries I in MQREAD
C...      (I): KF code for which a QNUMBERS block has been read.
      DIMENSION IDC(5),KFSUSY(50),MQREAD(0:100)
      SAVE KFSUSY,MQREAD
      DATA VERBOS /1/
      DATA NHELLO /0/
      DATA KFSUSY/
     &1000001,1000002,1000003,1000004,1000005,1000006,
     &2000001,2000002,2000003,2000004,2000005,2000006,
     &1000011,1000012,1000013,1000014,1000015,1000016,
     &2000011,2000012,2000013,2000014,2000015,2000016,
     &1000021,1000022,1000023,1000025,1000035,1000024,
     &1000037,1000039,     25,     35,     36,     37,
     &      6,     24,     45,     46,1000045, 9*0/
      RMFUN(IP)=PMAS(PYCOMP(IP),1)
 
C...Hello World
      IF (NHELLO.EQ.0) THEN
        WRITE(MSTU(11),5000) DOC
        NHELLO=1
      ENDIF

C...SLHA file assumed opened by user on unit LFN, stored in IMSS(20
C...+MUPDA).
      LFN=IMSS(20+MUPDA)
      IF (MUPDA.EQ.5) LFN=IMSS(21)
      IF (MUPDA.EQ.0) LFN=IMSS(21)
C...Flag that we have not yet found whatever we were asked to find.
      IRETRN=1
 
C...STOP IF LFN IS ZERO (i.e. if no LFN was given).
      IF (LFN.EQ.0) THEN
        WRITE(MSTU(11),*) '* (PYSLHA:) No valid unit given in IMSS'
        GOTO 9999
      ENDIF
 
C...If told to read spectrum, first zero all previous information.
      IF (MUPDA.EQ.1) THEN
C...Zero all block read flags
        DO 100 M=1,100
          MMOD(M)=0
          MSPC(M)=0
          MDEC(M)=0
  100   CONTINUE
C...Zero all (MSSM) masses, widths, and lifetimes in PYTHIA
        DO 110 ISUSY=1,36
          KC=PYCOMP(KFSUSY(ISUSY))
          PMAS(KC,1)=0D0
          PMAS(KC,2)=0D0
          PMAS(KC,3)=0D0
          PMAS(KC,4)=0D0
  110   CONTINUE
C...Zero all (3rd gen sfermion + gaugino/higgsino) mixing matrices.
        DO 130 J=1,4
          SFMIX(5,J) =0D0
          SFMIX(6,J) =0D0
          SFMIX(15,J)=0D0
          DO 120 L=1,4
            ZMIX(L,J) =0D0
            ZMIXI(L,J)=0D0
            IF (J.LE.2.AND.L.LE.2) THEN
              UMIX(L,J) =0D0
              UMIXI(L,J)=0D0
              VMIX(L,J) =0D0
              VMIXI(L,J)=0D0
            ENDIF
  120     CONTINUE
C...Zero signed masses.
          SMZ(J)=0D0
          IF (J.LE.2) SMW(J)=0D0
  130   CONTINUE
C...NB: RMSS array not zeroed.
        WRITE(MSTU(11),*)
     &       '* (PYSLHA:) Reading in SLHA spectrum from unit ', LFN
 
C...If reading decays, reset PYTHIA decay counters.
      ELSEIF (MUPDA.EQ.2) THEN
        KCC=100
        NDC=0
        BRSUM=0D0
        DO 140 KC=1,MSTU(6)
          IF(KC.GT.100.AND.KCHG(KC,4).GT.100) KCC=KC
          NDC=MAX(NDC,MDCY(KC,2)+MDCY(KC,3)-1)
  140   CONTINUE
      ELSEIF (MUPDA.EQ.5) THEN
C...Zero block read flags
        DO 150 M=1,100
          MSPC(M)=0
 150    CONTINUE
      ENDIF
 
C............READ
C...(spectrum or look for decays of KF=KFORIG or MASS of KF=KFORIG
      IF(MUPDA.EQ.0.OR.MUPDA.EQ.1.OR.MUPDA.EQ.2.OR.MUPDA.EQ.5) THEN
C...Initialize program and version strings
        CPRO(MUPDA)=' '
        CVER(MUPDA)=' '
 
C...Initialize read loop
        MERR=0
        NLINE=0
        CHBLCK=' '
C...READ NEW LINE INTO CHINL. GOTO 300 AT END-OF-FILE.
  160   CHINL=' '
        READ(LFN,'(A120)',END=300) CHINL
C...Count which line number we're at.
        NLINE=NLINE+1
        WRITE(CHNLIN,'(I6)') NLINE
 
C...Skip comment and empty lines without processing.
        IF (CHINL(1:1).EQ.'#'.OR.CHINL.EQ.' ') GOTO 160
 
C...We assume all upper case below. Rewrite CHINL to all upper case.
        INL=0
        IGOOD=0
  170   INL=INL+1
        IF (CHINL(INL:INL).NE.'#') THEN
          DO 180 ICH=97,122
            IF (CHAR(ICH).EQ.CHINL(INL:INL)) CHINL(INL:INL)=CHAR(ICH-32)
  180     CONTINUE
C...Extra safety. Chek for sensible input on line
          IF (IGOOD.EQ.0) THEN
            DO 190 ICH=48,90
              IF (CHAR(ICH).EQ.CHINL(INL:INL)) IGOOD=1
  190       CONTINUE
          ENDIF
          IF (INL.LT.120) GOTO 170
        ENDIF
        IF (IGOOD.EQ.0) GOTO 160
 
C...Check for BLOCK begin statement (spectrum).
        IF (CHINL(1:1).EQ.'B') THEN
          MERR=0
          READ(CHINL,'(A6,A)',ERR=460) CHDUM,CHBLCK
C...Check if another of this type of block was already read.
C...(logarithmic interpolation not yet implemented, so duplicates always
C...give errors)
          IF (CHBLCK(1:6).EQ.'MODSEL'.AND.MMOD(1).NE.0) MERR=7
          IF (CHBLCK(1:6).EQ.'MINPAR'.AND.MMOD(2).NE.0) MERR=7
          IF (CHBLCK(1:6).EQ.'EXTPAR'.AND.MMOD(3).NE.0) MERR=7
          IF (CHBLCK(1:8).EQ.'SMINPUTS'.AND.MMOD(4).NE.0) MERR=7
          IF (CHBLCK(1:4).EQ.'MASS'.AND.MSPC(1).NE.0) MERR=7
          IF (CHBLCK(1:4).EQ.'NMIX'.AND.MSPC(2).NE.0) MERR=7
          IF (CHBLCK(1:4).EQ.'UMIX'.AND.MSPC(3).NE.0) MERR=7
          IF (CHBLCK(1:4).EQ.'VMIX'.AND.MSPC(4).NE.0) MERR=7
          IF (CHBLCK(1:7).EQ.'SBOTMIX'.AND.MSPC(5).NE.0) MERR=7
          IF (CHBLCK(1:7).EQ.'STOPMIX'.AND.MSPC(6).NE.0) MERR=7
          IF (CHBLCK(1:7).EQ.'STAUMIX'.AND.MSPC(7).NE.0) MERR=7
          IF (CHBLCK(1:4).EQ.'HMIX'.AND.MSPC(8).NE.0) MERR=7
          IF (CHBLCK(1:5).EQ.'ALPHA'.AND.MSPC(17).NE.0) MERR=7
          IF (CHBLCK(1:5).EQ.'AU'.AND.MSPC(10).NE.0) MERR=7
          IF (CHBLCK(1:5).EQ.'AD'.AND.MSPC(11).NE.0) MERR=7
          IF (CHBLCK(1:5).EQ.'AE'.AND.MSPC(12).NE.0) MERR=7
          IF (CHBLCK(1:5).EQ.'MSOFT'.AND.MSPC(18).NE.0) MERR=7
C...Check for new particles
          IF (CHBLCK(1:8).EQ.'QNUMBERS'.OR.CHBLCK(1:8).EQ.'PARTICLE')
     &        THEN
            MSPC(19)=MSPC(19)+1
C...Read PDG code
            READ(CHBLCK(9:60),*) KFQ

            DO 121 MQ=1,MQREAD(0)
              IF (MQREAD(MQ).EQ.KFQ) THEN
                MERR=17
                GOTO 290
              ENDIF
 121        CONTINUE
            WRITE(MSTU(11),'(A,I9,A,F12.3)')
     &           ' * (PYSLHA:) Reading in '//CHBLCK(1:8)//
     &           ' for KF =',KFQ
            MQREAD(0)=MQREAD(0)+1
            MQREAD(MQREAD(0))=KFQ
            MSPC(19)=MSPC(19)+1
            KCQ=PYCOMP(KFQ)
            IF (KCQ.EQ.0) THEN
              DO 123 KCT=100,MSTU(6)
                IF(KCHG(KCT,4).GT.100) KCQ=KCT
 123          CONTINUE
              KCQ=KCQ+1
              KCC=KCQ
              KCHG(KCQ,4)=KFQ              
C...First write PDG code as name
              WRITE(CHTMP,*) KFQ
C...Then look for real name
              ICMT=9
 90           ICMT=ICMT+1
              IF (CHBLCK(ICMT:ICMT).NE.'#'.AND.ICMT.LT.59) GOTO 90
              IF (ICMT.LT.59) THEN
                READ(CHBLCK(ICMT+1:60),'(A)',ERR=95) CHDUM
                IF (CHDUM.NE.' ') CHTMP=CHDUM
              ENDIF
 95           CHAF(KCQ,1)=CHTMP
              MSTU(20)=0
C...Set stable for now
              PMAS(KCQ,2)=1D-6
              MWID(KCQ)=0
              MDCY(KCQ,1)=0
              MDCY(KCQ,2)=0
              MDCY(KCQ,3)=0
            ELSE
              WRITE(MSTU(11),*)
     &           '* (PYSLHA:) KF =',KFQ,' already exists: ',
     &             CHAF(KCQ,1), '. Entry ignored.'
              MERR=7
            ENDIF
          ENDIF
C...Finalize this line and read next.
          GOTO 290
C...Check for DECAY begin statement (decays).
        ELSEIF (CHINL(1:1).EQ.'D') THEN
          MERR=0
          BRSUM=0D0
          CHBLCK='DECAY'
C...Read KF code and WIDTH
          MPSIGN=1
          READ(CHINL(7:INL),*,ERR=470) KF, WIDTH
          IF (KF.LE.0) THEN
            KF=-KF
            MPSIGN=-1
          ENDIF
C...If this is not the KF we're looking for...
          IF (KF.NE.KFORIG.OR.MUPDA.NE.2) THEN
C...Set block skip flag and read next line.
            MERR=16
            GOTO 290
          ENDIF
 
C...Determine PYTHIA KC code of particle
          KCREP=0
          IF(KF.LE.100) THEN
            KCREP=KF
          ELSE
            DO 200 KCR=101,KCC
              IF(KCHG(KCR,4).EQ.KF) KCREP=KCR
  200       CONTINUE
          ENDIF
          KC=KCREP
          IF (KCREP.NE.0) THEN
C...Particle is already known. Don't do anything yet.
          ELSE
C...  Add new particle. Actually, this should not happen.
C...  New particles should be added already when reading the spectrum
C...  information, so go under previously stable category.
            KCC=KCC+1
            KC=KCC
          ENDIF
 
          IF (WIDTH.LE.0D0) THEN
C...Stable (i.e. LSP)
            WRITE(MSTU(11),*)
     &           '* (PYSLHA:) Reading in SLHA stable particle: ',
     &           CHAF(KCREP,1)
            IF (WIDTH.LT.0D0) THEN
              CALL PYERRM(19,'(PYSLHA:) Negative width forced to'//
     &             ' zero !')
              WIDTH=0D0
            ENDIF
            PMAS(KC,2)=1D-6
            MWID(KC)=0
            MDCY(KC,1)=0
C...Ignore any decay lines that may be present for this KF
            MERR=16
            MDCY(KC,2)=0
            MDCY(KC,3)=0
C...Return ok
            IRETRN=0
          ENDIF
C...Finalize and start reading in decay modes.
          GOTO 290
        ELSEIF (MOD(MERR,10).GE.6) THEN
C...If ignore block flag set, skip directly to next line.
          GOTO 160
        ENDIF
 
C...READ SPECTRUM
        IF (MUPDA.EQ.0.AND.MERR.EQ.0) THEN
          IF (CHBLCK(1:8).EQ.'QNUMBERS'.OR.CHBLCK(1:8).EQ.'PARTICLE') 
     &        THEN
            READ(CHINL,*) INDX, IVAL
            IF (INDX.EQ.1) KCHG(KCQ,1)=IVAL
            IF (INDX.EQ.3) KCHG(KCQ,2)=0
            IF (INDX.EQ.3.AND.IVAL.EQ.3) KCHG(KCQ,2)=1
            IF (INDX.EQ.3.AND.IVAL.EQ.-3) KCHG(KCQ,2)=-1
            IF (INDX.EQ.3.AND.IVAL.EQ.8) KCHG(KCQ,2)=2
            IF (INDX.EQ.4) THEN
              KCHG(KCQ,3)=IVAL
              IF (IVAL.EQ.1) THEN 
                CHTMP=CHAF(KCQ,1)
                IF (CHTMP.EQ.' ') THEN
                  WRITE(CHAF(KCQ,1),*) KCHG(KCQ,4)
                  WRITE(CHAF(KCQ,2),*) -KCHG(KCQ,4)
                ELSE
                  ILAST=17
 116              ILAST=ILAST-1
                  IF (CHTMP(ILAST:ILAST).EQ.' ') GOTO 116
                  IF (CHTMP(ILAST:ILAST).EQ.'+') THEN
                    CHTMP(ILAST:ILAST)='-'
                  ELSE
                    CHTMP(ILAST+1:MIN(16,ILAST+4))='bar'
                  ENDIF
                  CHAF(KCQ,2)=CHTMP
                ENDIF
              ENDIF
            ENDIF
          ELSE
            MERR=8
          ENDIF
        ELSEIF ((MUPDA.EQ.1.OR.MUPDA.EQ.5).AND.MERR.EQ.0) THEN
C...MASS: Mass spectrum
          IF (CHBLCK(1:4).EQ.'MASS') THEN
            READ(CHINL,*) KF, VAL
            MERR=1
            KC=0
            IF (MUPDA.EQ.1.OR.KF.EQ.KFORIG) THEN
C...Read in masses for anything
              MERR=0
              KC=PYCOMP(KF)
              IF (KC.NE.0) THEN
                MSPC(1)=MSPC(1)+1
                PMAS(KC,1) = ABS(VAL)
                IF (MUPDA.EQ.5) THEN
                  WRITE(MSTU(11),'(A,I9,A,F12.3)')
     &                 ' * (PYSLHA:) Reading in MASS entry for KF =',
     &                 KF, ', pole mass =', VAL
                  IRETRN=0
                ENDIF
C...  Signed masses
                IF (KF.EQ.1000021.AND.MSPC(18).EQ.0) RMSS(3)=VAL
                IF (KF.EQ.1000022) SMZ(1)=VAL
                IF (KF.EQ.1000023) SMZ(2)=VAL
                IF (KF.EQ.1000025) SMZ(3)=VAL
                IF (KF.EQ.1000035) SMZ(4)=VAL
                IF (KF.EQ.1000024) SMW(1)=VAL
                IF (KF.EQ.1000037) SMW(2)=VAL
              ENDIF
            ELSEIF (MUPDA.EQ.5) THEN
              MERR=0
            ENDIF
          ELSEIF (MUPDA.EQ.5) THEN
C...Only read MASS if MUPDA = 5. Skip any other blocks.
            MERR=8
          ELSEIF (CHBLCK(1:8).EQ.'QNUMBERS'.OR.
     &          CHBLCK(1:8).EQ.'PARTICLE') THEN
C...Don't print a warning for QNUMBERS when reading spectrum
            MERR=8
C...  MODSEL: Model selection and global switches
          ELSEIF (CHBLCK(1:6).EQ.'MODSEL') THEN
            READ(CHINL,*) INDX, IVAL
            IF (INDX.LE.200.AND.INDX.GT.0) THEN
              MODSEL(INDX)=IVAL
              MMOD(1)=MMOD(1)+1
              IF (INDX.EQ.3.AND.IVAL.EQ.1) THEN
C...  Switch on NMSSM
                WRITE(MSTU(11),*) '* (PYSLHA:) switching on NMSSM'
                IMSS(13)=MAX(1,IMSS(13))
C...  Add NMSSM states if not already done
 
                KFN=25
                KCN=KFN
                CHAF(KCN,1)='H_10'
                CHAF(KCN,2)=' '
 
                KFN=35
                KCN=KFN
                CHAF(KCN,1)='H_20'
                CHAF(KCN,2)=' '
 
                KFN=45
                KCN=KFN
                CHAF(KCN,1)='H_30'
                CHAF(KCN,2)=' '
 
                KFN=36
                KCN=KFN
                CHAF(KCN,1)='A_10'
                CHAF(KCN,2)=' '
 
                KFN=46
                KCN=KFN
                CHAF(KCN,1)='A_20'
                CHAF(KCN,2)=' '
 
                KFN=1000045
                KCN=PYCOMP(KFN)
                IF (KCN.EQ.0) THEN
                  DO 234 KCT=100,MSTU(6)
                    IF(KCHG(KCT,4).GT.100) KCN=KCT
 234              CONTINUE
                  KCN=KCN+1
                  KCHG(KCN,4)=KFN
                  MSTU(20)=0
                ENDIF
C...  Set stable for now
                PMAS(KCN,2)=1D-6
                MWID(KCN)=0
                MDCY(KCN,1)=0
                MDCY(KCN,2)=0
                MDCY(KCN,3)=0
                CHAF(KCN,1)='~chi_50'
                CHAF(KCN,2)=' '
              ENDIF
            ELSE
              MERR=1
            ENDIF
C...MINPAR: Minimal model parameters
          ELSEIF (CHBLCK(1:6).EQ.'MINPAR') THEN
            IF (MODSEL(1).NE.0) THEN
              READ(CHINL,*) INDX, VAL
              IF (INDX.LE.100.AND.INDX.GT.0) THEN
                PARMIN(INDX)=VAL
                MMOD(2)=MMOD(2)+1
              ELSE
                MERR=1
              ENDIF
            ELSEIF (MMOD(3).NE.0) THEN
              WRITE(MSTU(11),*)
     &             '* (PYSLHA:) MINPAR after EXTPAR !'
              MERR=1
            ELSE
              WRITE(MSTU(11),*)
     &             '* (PYSLHA:) Reading MINPAR, but no MODSEL !' 
              MERR=1
            ENDIF
C...tan(beta)
            IF (INDX.EQ.3) RMSS(5)=VAL
C...EXTPAR: non-minimal model parameters.
          ELSEIF (CHBLCK(1:6).EQ.'EXTPAR') THEN
            IF (MMOD(1).NE.0) THEN
              READ(CHINL,*) INDX, VAL
              IF (INDX.LE.200.AND.INDX.GT.0) THEN
                PAREXT(INDX)=VAL
                MMOD(3)=MMOD(3)+1
              ELSE
                MERR=1
              ENDIF
            ELSE
              WRITE(MSTU(11),*)
     &             '* (PYSLHA:) Reading EXTPAR, but no MODSEL !'
              MERR=1
            ENDIF
C...tan(beta)
            IF (INDX.EQ.25) RMSS(5)=VAL
          ELSEIF (CHBLCK(1:8).EQ.'SMINPUTS') THEN
            READ(CHINL,*) INDX, VAL
            IF (INDX.LE.3.OR.INDX.EQ.5.OR.INDX.GE.7) THEN
              MERR=1
            ELSEIF (INDX.EQ.4) THEN
              PMAS(PYCOMP(23),1)=VAL
            ELSEIF (INDX.EQ.6) THEN
              PMAS(PYCOMP(6),1)=VAL
            ENDIF
          ELSEIF (CHBLCK(1:4).EQ.'NMIX'.OR.CHBLCK(1:4).EQ.'VMIX'.OR
     $           .CHBLCK(1:4).EQ.'UMIX'.OR.CHBLCK(1:7).EQ.'STOPMIX'.OR
     $           .CHBLCK(1:7).EQ.'SBOTMIX'.OR.CHBLCK(1:7).EQ.'STAUMIX')
     $           THEN
C...NMIX,UMIX,VMIX,STOPMIX,SBOTMIX, and STAUMIX. Mixing.
            IM=0
            IF (CHBLCK(5:6).EQ.'IM') IM=1
  250       READ(CHINL,*) INDX1, INDX2, VAL
            IF (CHBLCK(1:1).EQ.'N'.AND.INDX1.LE.4.AND.INDX2.LE.4) THEN
              IF (IM.EQ.0) ZMIX(INDX1,INDX2) = VAL
              IF (IM.EQ.1) ZMIXI(INDX1,INDX2)= VAL
              MSPC(2)=MSPC(2)+1
            ELSEIF (CHBLCK(1:1).EQ.'U') THEN
              IF (IM.EQ.0) UMIX(INDX1,INDX2) = VAL
              IF (IM.EQ.1) UMIXI(INDX1,INDX2)= VAL
              MSPC(3)=MSPC(3)+1
            ELSEIF (CHBLCK(1:1).EQ.'V') THEN
              IF (IM.EQ.0) VMIX(INDX1,INDX2) = VAL
              IF (IM.EQ.1) VMIXI(INDX1,INDX2)= VAL
              MSPC(4)=MSPC(4)+1
            ELSEIF (CHBLCK(1:4).EQ.'STOP'.OR.CHBLCK(1:4).EQ.'SBOT'.OR
     $             .CHBLCK(1:4).EQ.'STAU') THEN
              IF (CHBLCK(1:4).EQ.'STOP') THEN
                KFSM=6
                ISPC=6
              ELSEIF (CHBLCK(1:4).EQ.'SBOT') THEN
                KFSM=5
                ISPC=5
              ELSEIF (CHBLCK(1:4).EQ.'STAU') THEN
                KFSM=15
                ISPC=7
              ENDIF
C...Set SFMIX element
              SFMIX(KFSM,2*(INDX1-1)+INDX2)=VAL
              MSPC(ISPC)=MSPC(ISPC)+1
            ENDIF
C...Running parameters
          ELSEIF (CHBLCK(1:4).EQ.'HMIX') THEN
            READ(CHBLCK(8:25),*,ERR=510) Q
            READ(CHINL,*) INDX, VAL
            MSPC(8)=MSPC(8)+1
            IF (INDX.EQ.1) THEN
              RMSS(4) = VAL
            ELSE
              MERR=1
              MSPC(8)=MSPC(8)-1
            ENDIF
          ELSEIF (CHBLCK(1:5).EQ.'ALPHA') THEN
            READ(CHINL,*,ERR=520) VAL
            RMSS(18)= VAL
            MSPC(17)=MSPC(17)+1
C...Higgs parameters set manually or with FeynHiggs.
            IMSS(4)=MAX(2,IMSS(4))
          ELSEIF (CHBLCK(1:2).EQ.'AU'.OR.CHBLCK(1:2).EQ.'AD'.OR
     &           .CHBLCK(1:2).EQ.'AE') THEN
            READ(CHBLCK(9:26),*,ERR=510) Q
            READ(CHINL,*) INDX1, INDX2, VAL
            IF (CHBLCK(2:2).EQ.'U') THEN
              AU(INDX1,INDX2)=VAL
              IF (INDX1.EQ.3.AND.INDX2.EQ.3) RMSS(16)=VAL
              MSPC(11)=MSPC(11)+1
            ELSEIF (CHBLCK(2:2).EQ.'D') THEN
              AD(INDX1,INDX2)=VAL
              IF (INDX1.EQ.3.AND.INDX2.EQ.3) RMSS(15)=VAL
              MSPC(10)=MSPC(10)+1
            ELSEIF (CHBLCK(2:2).EQ.'E') THEN
              AE(INDX1,INDX2)=VAL
              IF (INDX1.EQ.3.AND.INDX2.EQ.3) RMSS(17)=VAL
              MSPC(12)=MSPC(12)+1
            ELSE
              MERR=1
            ENDIF
          ELSEIF (CHBLCK(1:5).EQ.'MSOFT') THEN
            IF (MSPC(18).EQ.0) THEN
              READ(CHBLCK(9:25),*,ERR=510) Q
              RMSOFT(0)=Q
            ENDIF
            READ(CHINL,*) INDX, VAL
            RMSOFT(INDX)=VAL
            MSPC(18)=MSPC(18)+1
          ELSEIF (CHBLCK(1:5).EQ.'GAUGE') THEN
            MERR=8
          ELSEIF (CHBLCK(1:2).EQ.'YU'.OR.CHBLCK(1:2).EQ.'YD'.OR
     &           .CHBLCK(1:2).EQ.'YE') THEN
            MERR=8
          ELSEIF (CHBLCK(1:6).EQ.'SPINFO') THEN
            READ(CHINL(1:6),*) INDX
            IT=0
            MIRD=0
  260       IT=IT+1
            IF (CHINL(IT:IT).EQ.' ') GOTO 260
C...Don't read index
            IF (CHINL(IT:IT).EQ.CHAR(INDX+48).AND.MIRD.EQ.0) THEN
              MIRD=1
              GOTO 260
            ENDIF
            IF (INDX.EQ.1) CPRO(1)=CHINL(IT:IT+12)
            IF (INDX.EQ.2) CVER(1)=CHINL(IT:IT+12)
          ELSE
C...  Set unrecognized block flag.
            MERR=6
          ENDIF
 
C...DECAY TABLES
C...Read in decay information
        ELSEIF (MUPDA.EQ.2.AND.MERR.EQ.0) THEN
C...Read new decay chanel
          IF(CHINL(1:1).EQ.' '.AND.CHBLCK(1:5).EQ.'DECAY') THEN
            NDC=NDC+1
C...Read in branching ratio and number of daughters for this mode.
            READ(CHINL(4:50),*,ERR=480) BRAT(NDC)
            READ(CHINL(4:50),*,ERR=490) DUM, NDA
            IF (NDA.LE.5) THEN
              IF(NDC.GT.MSTU(7)) CALL PYERRM(27,
     &             '(PYSLHA:) Decay data arrays full by KF ='
     $             //CHAF(KC,1))
C...If first decay chanel, set decays start point in decay table
              IF(BRSUM.LE.0D0.AND.BRAT(NDC).GT.0D0) THEN 
                WRITE(MSTU(11),*)
     &              '* (PYSLHA:) Reading in SLHA decay table for ',
     &              CHAF(KCREP,1)
C...Set particle parameters (mass set when reading BLOCK MASS above)
                PMAS(KC,2)=WIDTH
                IF (KF.EQ.25.OR.KF.EQ.35.OR.KF.EQ.36) THEN
                  WRITE(MSTU(11),*)
     &                '*  Note: the Pythia gg->h/H/A cross section'//
     &                ' is proportional to the h/H/A->gg width'
                ENDIF
                PMAS(KC,3)=0D0
                PMAS(KC,4)=PARU(3)*1D-12/WIDTH
                MWID(KC)=2
                MDCY(KC,1)=1
                MDCY(KC,2)=NDC
                MDCY(KC,3)=0
C...Return ok
                IRETRN=0
              ENDIF
C...  Count up number of decay modes for this particle
              MDCY(KC,3)=MDCY(KC,3)+1
C...  Read in decay daughters.
              READ(CHINL(4:120),*,ERR=500) DUM,IDM, (IDC(IDA),IDA=1,NDA)
C...  Flip sign if reading antiparticle decays (if antipartner exists)
              DO 270 IDA=1,NDA
                IF (KCHG(PYCOMP(IDC(IDA)),3).NE.0)
     &               IDC(IDA)=MPSIGN*IDC(IDA)
  270         CONTINUE
C...Switch on decay channel, with products ordered in decreasing ABS(KF)
              MDME(NDC,1)=1
              IF (BRAT(NDC).EQ.0D0) MDME(NDC,1)=0
              BRSUM=BRSUM+BRAT(NDC)
 274          IFLIP=0
              DO 277 IDA=1,NDA-1
                IF (IABS(IDC(IDA+1)).GT.IABS(IDC(IDA))) THEN
                  ITMP=IDC(IDA)
                  IDC(IDA)=IDC(IDA+1)
                  IDC(IDA+1)=ITMP
                  IFLIP=IFLIP+1
                ENDIF
 277          CONTINUE
              IF (IFLIP.GT.0) GOTO 274
C              WRITE(MSTU(11),7510) BRAT(NDC), NDA, (IDC(IDA),IDA=1,NDA)
C...Treat as ordinary decay, no fancy stuff.
              MDME(NDC,2)=0
              DO 280 IDA=1,5
                IF (IDA.LE.NDA) THEN
                  KFDP(NDC,IDA)=IDC(IDA)
                ELSE
                  KFDP(NDC,IDA)=0
                ENDIF
  280         CONTINUE
            ELSE
              CALL PYERRM(7,'(PYSLHA:) Too many daughters on line '//
     &             CHNLIN)
              MERR=11
              NDC=NDC-1
            ENDIF
          ELSEIF(CHINL(1:1).EQ.'+') THEN
            MERR=11
          ELSEIF(CHBLCK(1:6).EQ.'DCINFO') THEN
            MERR=16
          ELSE
            MERR=16
          ENDIF
        ENDIF
C...  Error check.
  290   IF (MOD(MERR,10).EQ.1.AND.(MUPDA.EQ.1.OR.MUPDA.EQ.2)) THEN
          WRITE(MSTU(11),*) '* (PYSLHA:) Ignoring line '//CHNLIN//': '
     &         //CHINL(1:40)
          MERR=0
        ELSEIF (MERR.EQ.6.AND.MUPDA.EQ.1) THEN
          WRITE(MSTU(11),*) '* (PYSLHA:) Ignoring BLOCK '//
     &         CHBLCK(1:INL)//'... on line'//CHNLIN
        ELSEIF (MERR.EQ.8.AND.MUPDA.EQ.1) THEN
          WRITE(MSTU(11),*) '* (PYSLHA:) PYTHIA will not use BLOCK '
     &         //CHBLCK(1:INL)//'... on line'//CHNLIN
        ELSEIF (MERR.EQ.16.AND.MUPDA.EQ.2.AND.IMSS(21).EQ.0.AND.
     &         CHBLCK(1:1).NE.'D'.AND.VERBOS.EQ.1) THEN
          WRITE(MSTU(11),*) '* (PYSLHA:) Ignoring BLOCK '//CHBLCK(1:INL)
     &         //'... on line'//CHNLIN
        ELSEIF (MERR.EQ.7.AND.MUPDA.EQ.1) THEN
          WRITE(MSTU(11),*) '* (PYSLHA:) Ignoring extra BLOCK '/
     &         /CHBLCK(1:INL)//'... on line'//CHNLIN
        ELSEIF (MERR.EQ.2.AND.MUPDA.EQ.1) THEN
          WRITE (CHTMP,*) KF
          WRITE(MSTU(11),*)
     &         '* (PYSLHA:) Ignoring extra MASS entry for KF='//
     &         CHTMP(1:9)//' on line'//CHNLIN
        ENDIF
C...  End of loop
        GOTO 160
  300   CONTINUE
C...Set flag that KC codes have been rearranged.
        MSTU(20)=0
        VERBOS=0
 
C...Perform possible tests that new information is consistent.
        IF (MUPDA.EQ.1) THEN
          MSTU23=MSTU(23)
          MSTU27=MSTU(27)
C...Check Z and top masses
          IF (ABS(PMAS(PYCOMP(23),1)-91.2D0).GT.1D0) THEN
            WRITE(CHTMP,*) PMAS(PYCOMP(23),1)
            CALL PYERRM(19,'(PYSLHA:) note Z boson mass, M ='//CHTMP)
          ENDIF
          IF (ABS(PMAS(PYCOMP(6),1)-175D0).GT.25D0) THEN
            WRITE(CHTMP,*) PMAS(PYCOMP(6),1)
            CALL PYERRM(19,'(PYSLHA:) note top quark mass, M ='
     &           //CHTMP//'GeV')
          ENDIF
C...Check masses
          DO 310 ISUSY=1,37
            KF=KFSUSY(ISUSY)
C...Don't complain about right-handed neutrinos
            IF (KF.EQ.KSUSY2+12.OR.KF.EQ.KSUSY2+14.OR.KF.EQ.KSUSY2
     &           +16) GOTO 310
C...Only check gravitino in GMSB scenarios
            IF (MODSEL(1).NE.2.AND.KF.EQ.KSUSY1+39) GOTO 310
            KC=PYCOMP(KF)
            IF (PMAS(KC,1).EQ.0D0) THEN
              WRITE(CHTMP,*) KF
              CALL PYERRM(9
     &             ,'(PYSLHA:) No mass information found for KF = '
     &             //CHTMP)
            ENDIF
  310     CONTINUE
C...Check mixing matrices (MSSM only)
          IF (IMSS(13).EQ.0) THEN
            IF (MSPC(2).NE.16.AND.MSPC(2).NE.32) CALL PYERRM(9
     &           ,'(PYSLHA:) Inconsistent # of elements in NMIX')
            IF (MSPC(3).NE.4.AND.MSPC(3).NE.8) CALL PYERRM(9
     &           ,'(PYSLHA:) Inconsistent # of elements in UMIX')
            IF (MSPC(4).NE.4.AND.MSPC(4).NE.8) CALL PYERRM(9
     &           ,'(PYSLHA:) Inconsistent # of elements in VMIX')
            IF (MSPC(5).NE.4) CALL PYERRM(9
     &           ,'(PYSLHA:) Inconsistent # of elements in SBOTMIX')
            IF (MSPC(6).NE.4) CALL PYERRM(9
     &           ,'(PYSLHA:) Inconsistent # of elements in STOPMIX')
            IF (MSPC(7).NE.4) CALL PYERRM(9
     &           ,'(PYSLHA:) Inconsistent # of elements in STAUMIX')
            IF (MSPC(8).LT.1) CALL PYERRM(9
     &           ,'(PYSLHA:) Too few elements in HMIX')
            IF (MSPC(10).EQ.0) CALL PYERRM(9
     &           ,'(PYSLHA:) Missing A_b trilinear coupling')
            IF (MSPC(11).EQ.0) CALL PYERRM(9
     &           ,'(PYSLHA:) Missing A_t trilinear coupling')
            IF (MSPC(12).EQ.0) CALL PYERRM(9
     &           ,'(PYSLHA:) Missing A_tau trilinear coupling')
            IF (MSPC(17).LT.1) CALL PYERRM(9
     &           ,'(PYSLHA:) Missing Higgs mixing angle alpha')
          ENDIF
C...Check wavefunction normalizations.
C...Sfermions
          DO 320 ISPC=5,7
            IF (MSPC(ISPC).EQ.4) THEN
              KFSM=ISPC
              IF (ISPC.EQ.7) KFSM=15
              CHECK=ABS(SFMIX(KFSM,1)*SFMIX(KFSM,4)-SFMIX(KFSM,2)
     &             *SFMIX(KFSM,3))
              IF (ABS(1D0-CHECK).GT.1D-3) THEN
                KCSM=PYCOMP(KFSM)
                CALL PYERRM(17
     &               ,'(PYSLHA:) Non-orthonormal mixing matrix for ~'
     &               //CHAF(KCSM,1))
              ENDIF
            ENDIF
  320     CONTINUE
C...Neutralinos + charginos
          DO 340 J=1,4
            CN1=0D0
            CN2=0D0
            CU1=0D0
            CU2=0D0
            CV1=0D0
            CV2=0D0
            DO 330 L=1,4
              CN1=CN1+ZMIX(J,L)**2
              CN2=CN2+ZMIX(L,J)**2
              IF (J.LE.2.AND.L.LE.2) THEN
                CU1=CU1+UMIX(J,L)**2
                CU2=CU2+UMIX(L,J)**2
                CV1=CV1+VMIX(J,L)**2
                CV2=CV2+VMIX(L,J)**2
              ENDIF
  330       CONTINUE
C...NMIX normalization
            IF (MSPC(2).EQ.16.AND.(ABS(1D0-CN1).GT.1D-3.OR.ABS(1D0-CN2)
     &           .GT.1D-3).AND.IMSS(13).EQ.0) THEN
              CALL PYERRM(19,
     &             '(PYSLHA:) NMIX: Inconsistent normalization.')
              WRITE(MSTU(11),'(7x,I2,1x,":",2(1x,F7.4))') J, CN1, CN2
            ENDIF
C...UMIX, VMIX normalizations
            IF (MSPC(3).EQ.4.OR.MSPC(4).EQ.4.AND.IMSS(13).EQ.0) THEN
              IF (J.LE.2) THEN
                IF (ABS(1D0-CU1).GT.1D-3.OR.ABS(1D0-CU2).GT.1D-3) THEN
                  CALL PYERRM(19
     &                ,'(PYSLHA:) UMIX: Inconsistent normalization.')
                  WRITE(MSTU(11),'(7x,I2,1x,":",2(1x,F6.2))') J, CU1,
     &                 CU2
                ENDIF
                IF (ABS(1D0-CV1).GT.1D-3.OR.ABS(1D0-CV2).GT.1D-3) THEN
                  CALL PYERRM(19,
     &                '(PYSLHA:) VMIX: Inconsistent normalization.')
                  WRITE(MSTU(11),'(7x,I2,1x,":",2(1x,F6.2))') J, CV1,
     &                 CV2
                ENDIF
              ENDIF
            ENDIF
  340     CONTINUE
          IF (MSTU(27).EQ.MSTU27.AND.MSTU(23).EQ.MSTU23) THEN
            WRITE(MSTU(11),'(1x,"*"/1x,A/1x,"*")')
     &           '*  PYSLHA:  No spectrum inconsistencies were found.'
          ELSE
            WRITE(MSTU(11),'(1x,"*"/1x,A/1x,"*",A/1x,"*",A/)')
     &           '* (PYSLHA:) INCONSISTENT SPECTRUM WARNING.'
     &           ,'Warning: one or more (serious)'//
     &           ' inconsistencies were found in the spectrum!!!'
     &           ,'Read the error messages above and check your'//
     &           ' input file.'
          ENDIF
C...Increase precision in Higgs sector using FeynHiggs
          IF (IMSS(4).EQ.3) THEN
C...FeynHiggs needs MSOFT.
            IERR=0
            IF (MSPC(18).EQ.0) THEN
              WRITE(MSTU(11),'(1x,"*"/1x,A/)')
     &             '* (PYSLHA:) BLOCK MSOFT not found in SLHA file.'//
     &              ' Cannot call FeynHiggs.'
              IERR=-1
            ELSE
              WRITE(MSTU(11),'(1x,/1x,A/)')
     &             '* (PYSLHA:) Now calling FeynHiggs.'
              CALL PYFEYN(IERR)
              IF (IERR.NE.0) IMSS(4)=2
            ENDIF
          ENDIF
        ELSEIF (MUPDA.EQ.2.AND.IRETRN.EQ.0) THEN
          KF=KFORIG
          KC=PYCOMP(KF)
          WRITE(CHKF,8300) KF
          IF(MIN(PMAS(KC,1),PMAS(KC,2),PMAS(KC,3),PMAS(KC,1)-PMAS(KC,3
     $         ),PMAS(KC,4)).LT.0D0.OR.MDCY(KC,3).LT.0.OR.(MDCY(KC,3)
     $         .EQ.0.AND.MDCY(KC,1).GE.1)) CALL PYERRM(17
     $         ,'(PYSLHA:) Mass/width/life/(# channels) wrong for KF='
     $         //CHKF)
          BRSUM=0D0
          DO 360 IDA=MDCY(KC,2),MDCY(KC,2)+MDCY(KC,3)-1
            IF(MDME(IDA,2).GT.80) GOTO 360
            KQ=KCHG(KC,1)
            PMS=PMAS(KC,1)-PMAS(KC,3)-PARJ(64)
            MERR=0
            DO 350 J=1,5
              KP=KFDP(IDA,J)
              IF(KP.EQ.0.OR.KP.EQ.81.OR.IABS(KP).EQ.82) THEN
                IF(KP.EQ.81) KQ=0
              ELSEIF(PYCOMP(KP).EQ.0) THEN
                MERR=3
              ELSE
                KQ=KQ-PYCHGE(KP)
                KPC=PYCOMP(KP)
                PMS=PMS-PMAS(KPC,1)
                IF(MSTJ(24).GT.0) PMS=PMS+0.5D0*MIN(PMAS(KPC,2),
     &               PMAS(KPC,3))
              ENDIF
  350       CONTINUE
            IF(KQ.NE.0) MERR=MAX(2,MERR)
            IF(MWID(KC).EQ.0.AND.KF.NE.311.AND.PMS.LT.0D0)
     &           MERR=MAX(1,MERR)
            IF(MERR.EQ.3) CALL PYERRM(17,
     &           '(PYSLHA:) Unknown particle code in decay of KF ='
     $           //CHKF)
            IF(MERR.EQ.2) CALL PYERRM(17,
     &           '(PYSLHA:) Charge not conserved in decay of KF ='
     $           //CHKF)
            IF(MERR.EQ.1) CALL PYERRM(7,
     &           '(PYSLHA:) Kinematically unallowed decay of KF ='
     $           //CHKF)
            BRSUM=BRSUM+BRAT(IDA)
  360     CONTINUE
          WRITE(CHTMP,8500) BRSUM
C...If BR's don't add up to 1, rescale, but issue warning.
          IF(ABS(BRSUM).GT.0.0001D0.AND.ABS(BRSUM-1D0).GT.0.0005D0)
     $         THEN
            CALL PYERRM(7
     &           ,"(PYSLHA:) Forced rescaling of BR's for KF="//CHKF//
     &           ' ; sum was'//CHTMP(9:16)//'.')
            FAC=1D0/BRSUM
            DO 370 IDA=MDCY(KC,2),MDCY(KC,2)+MDCY(KC,3)-1
              IF(MDME(IDA,2).GT.80) GOTO 370
              BRAT(IDA)=FAC*BRAT(IDA)
  370       CONTINUE
          ENDIF
        ENDIF
 
C...WRITE SPECTRUM ON SLHA FILE
      ELSEIF(MUPDA.EQ.3) THEN
C...If SPYTHIA or ISASUSY runtime was called for SUGRA, update PARMIN.
        IF (IMSS(1).EQ.2.OR.IMSS(1).EQ.12) THEN
          MODSEL(1)=1
          PARMIN(1)=RMSS(8)
          PARMIN(2)=RMSS(1)
          PARMIN(3)=RMSS(5)
          PARMIN(4)=SIGN(1D0,RMSS(4))
          PARMIN(5)=RMSS(36)
        ENDIF
C...Write spectrum
        WRITE(LFN,7000) 'SLHA MSSM spectrum'
        WRITE(LFN,7000) 'Pythia 6.4: T. Sjostrand, S. Mrenna,'
     &    // ' P. Skands.'
        WRITE(LFN,7010) 'MODSEL',  'Model selection'
        WRITE(LFN,7110) 1, MODSEL(1)
        WRITE(LFN,7010) 'MINPAR', 'Parameters for minimal model.'
        IF (MODSEL(1).EQ.1) THEN
          WRITE(LFN,7210) 1, PARMIN(1), 'm0'
          WRITE(LFN,7210) 2, PARMIN(2), 'm12'
          WRITE(LFN,7210) 3, PARMIN(3), 'tan(beta)'
          WRITE(LFN,7210) 4, PARMIN(4), 'sign(mu)'
          WRITE(LFN,7210) 5, PARMIN(5), 'a0'
        ELSEIF(MODSEL(2).EQ.2) THEN
          WRITE(LFN,7210) 1, PARMIN(1), 'Lambda'
          WRITE(LFN,7210) 2, PARMIN(2), 'M'
          WRITE(LFN,7210) 3, PARMIN(3), 'tan(beta)'
          WRITE(LFN,7210) 4, PARMIN(4), 'sign(mu)'
          WRITE(LFN,7210) 5, PARMIN(5), 'N5'
          WRITE(LFN,7210) 6, PARMIN(6), 'c_grav'
        ENDIF
        WRITE(LFN,7000) ' '
        WRITE(LFN,7010) 'MASS', 'Mass spectrum'
        DO 380 I=1,36
          KF=KFSUSY(I)
          KC=PYCOMP(KF)
          IF (KF.EQ.1000039.AND.MODSEL(1).NE.2) GOTO 380
          KFSM=KF-KSUSY1
          IF (KFSM.GE.22.AND.KFSM.LE.37) THEN
            IF (KFSM.EQ.22)  WRITE(LFN,7220) KF, SMZ(1), CHAF(KC,1)
            IF (KFSM.EQ.23)  WRITE(LFN,7220) KF, SMZ(2), CHAF(KC,1)
            IF (KFSM.EQ.25)  WRITE(LFN,7220) KF, SMZ(3), CHAF(KC,1)
            IF (KFSM.EQ.35)  WRITE(LFN,7220) KF, SMZ(4), CHAF(KC,1)
            IF (KFSM.EQ.24)  WRITE(LFN,7220) KF, SMW(1), CHAF(KC,1)
            IF (KFSM.EQ.37)  WRITE(LFN,7220) KF, SMW(2), CHAF(KC,1)
          ELSE
            WRITE(LFN,7220) KF, PMAS(KC,1), CHAF(KC,1)
          ENDIF
  380   CONTINUE
C...SUSY scale
        RMSUSY=SQRT(PMAS(PYCOMP(KSUSY1+6),1)*PMAS(PYCOMP(KSUSY2+6),1))
        WRITE(LFN,7020) 'HMIX',RMSUSY,'Higgs parameters'
        WRITE(LFN,7210) 1, RMSS(4),'mu'
        WRITE(LFN,7010) 'ALPHA',' '
        WRITE(LFN,7210) 1, RMSS(18), 'alpha'
        WRITE(LFN,7020) 'AU',RMSUSY
        WRITE(LFN,7410) 3, 3, RMSS(16), 'A_t'
        WRITE(LFN,7020) 'AD',RMSUSY
        WRITE(LFN,7410) 3, 3, RMSS(15), 'A_b'
        WRITE(LFN,7020) 'AE',RMSUSY
        WRITE(LFN,7410) 3, 3, RMSS(17), 'A_tau'
        WRITE(LFN,7010) 'STOPMIX','~t mixing matrix'
        WRITE(LFN,7410) 1, 1, SFMIX(6,1)
        WRITE(LFN,7410) 1, 2, SFMIX(6,2)
        WRITE(LFN,7410) 2, 1, SFMIX(6,3)
        WRITE(LFN,7410) 2, 2, SFMIX(6,4)
        WRITE(LFN,7010) 'SBOTMIX','~b mixing matrix'
        WRITE(LFN,7410) 1, 1, SFMIX(5,1)
        WRITE(LFN,7410) 1, 2, SFMIX(5,2)
        WRITE(LFN,7410) 2, 1, SFMIX(5,3)
        WRITE(LFN,7410) 2, 2, SFMIX(5,4)
        WRITE(LFN,7010) 'STAUMIX','~tau mixing matrix'
        WRITE(LFN,7410) 1, 1, SFMIX(15,1)
        WRITE(LFN,7410) 1, 2, SFMIX(15,2)
        WRITE(LFN,7410) 2, 1, SFMIX(15,3)
        WRITE(LFN,7410) 2, 2, SFMIX(15,4)
        WRITE(LFN,7010) 'NMIX','~chi0 mixing matrix'
        DO 400 I1=1,4
          DO 390 I2=1,4
            WRITE(LFN,7410) I1, I2, ZMIX(I1,I2)
  390     CONTINUE
  400   CONTINUE
        WRITE(LFN,7010) 'UMIX','~chi^+ U mixing matrix'
        DO 420 I1=1,2
          DO 410 I2=1,2
            WRITE(LFN,7410) I1, I2, UMIX(I1,I2)
  410     CONTINUE
  420   CONTINUE
        WRITE(LFN,7010) 'VMIX','~chi^+ V mixing matrix'
        DO 440 I1=1,2
          DO 430 I2=1,2
            WRITE(LFN,7410) I1, I2, VMIX(I1,I2)
  430     CONTINUE
  440   CONTINUE
        WRITE(LFN,7010) 'SPINFO'
        IF (IMSS(1).EQ.2) THEN
          CPRO(1)='PYTHIA'
          CVER(1)='6.4'
        ELSEIF (IMSS(1).EQ.12) THEN
          ISAVER=VISAJE()
          CPRO(1)='ISASUSY'
          CVER(1)=ISAVER(1:12)
        ENDIF
        WRITE(LFN,7310) 1, CPRO(1), 'Spectrum Calculator'
        WRITE(LFN,7310) 2, CVER(1), 'Version number'
      ENDIF
 
C...Print user information about spectrum
      IF (MUPDA.EQ.1.OR.MUPDA.EQ.3) THEN
        IF (CPRO(MOD(MUPDA,2)).NE.' '.AND.CVER(MOD(MUPDA,2)).NE.' ')
     &       WRITE(MSTU(11),5030) CPRO(1), CVER(1)
        IF (IMSS(4).EQ.3) WRITE(MSTU(11),5040)
        IF (MUPDA.EQ.1) THEN
          WRITE(MSTU(11),5020) LFN
        ELSE
          WRITE(MSTU(11),5010) LFN
        ENDIF
 
        WRITE(MSTU(11),5400)
        WRITE(MSTU(11),5500) 'Pole masses'
        WRITE(MSTU(11),5700) (RMFUN(KSUSY1+IP),IP=1,6)
     $       ,(RMFUN(KSUSY2+IP),IP=1,6)
        WRITE(MSTU(11),5800) (RMFUN(KSUSY1+IP),IP=11,16)
     $       ,(RMFUN(KSUSY2+IP),IP=11,16)
        IF (IMSS(13).EQ.0) THEN
          WRITE(MSTU(11),5900) RMFUN(KSUSY1+21),RMFUN(KSUSY1+22)
     $         ,RMFUN(KSUSY1+23),RMFUN(KSUSY1+25),RMFUN(KSUSY1+35),
     $         RMFUN(KSUSY1+24),RMFUN(KSUSY1+37)
          WRITE(MSTU(11),6000) CHAF(25,1),CHAF(35,1),CHAF(36,1),
     &         CHAF(37,1), ' ', ' ',' ',' ',
     &         RMFUN(25), RMFUN(35), RMFUN(36), RMFUN(37)
        ELSEIF (IMSS(13).EQ.1) THEN
          KF1=KSUSY1+21
          KF2=KSUSY1+22
          KF3=KSUSY1+23
          KF4=KSUSY1+25
          KF5=KSUSY1+35
          KF6=KSUSY1+45
          KF7=KSUSY1+24
          KF8=KSUSY1+37
          WRITE(MSTU(11),6000) CHAF(PYCOMP(KF1),1),CHAF(PYCOMP(KF2),1),
     &         CHAF(PYCOMP(KF3),1),CHAF(PYCOMP(KF4),1),
     &         CHAF(PYCOMP(KF5),1),CHAF(PYCOMP(KF6),1),
     &         CHAF(PYCOMP(KF7),1),CHAF(PYCOMP(KF8),1),
     &         RMFUN(KF1),RMFUN(KF2),RMFUN(KF3),RMFUN(KF4),
     &         RMFUN(KF5),RMFUN(KF6),RMFUN(KF7),RMFUN(KF8)
          WRITE(MSTU(11),6000) CHAF(25,1), CHAF(35,1), CHAF(45,1),
     &         CHAF(36,1), CHAF(46,1), CHAF(37,1),' ',' ',
     &         RMFUN(25), RMFUN(35), RMFUN(45), RMFUN(36), RMFUN(46),
     &         RMFUN(37)
        ENDIF
        WRITE(MSTU(11),5400)
        WRITE(MSTU(11),5500) 'Mixing structure'
        WRITE(MSTU(11),6100) ((ZMIX(I,J), J=1,4),I=1,4)
        WRITE(MSTU(11),6200) (UMIX(1,J), J=1,2),(VMIX(1,J),J=1,2)
     &       ,(UMIX(2,J), J=1,2),(VMIX(2,J),J=1,2)
        WRITE(MSTU(11),6300) (SFMIX(5,J), J=1,2),(SFMIX(6,J),J=1,2)
     &       ,(SFMIX(15,J), J=1,2),(SFMIX(5,J),J=3,4),(SFMIX(6,J), J=3,4
     &       ),(SFMIX(15,J),J=3,4)
        WRITE(MSTU(11),5400)
        WRITE(MSTU(11),5500) 'Couplings'
        WRITE(MSTU(11),6400) RMSS(15),RMSS(16),RMSS(17)
        WRITE(MSTU(11),6450) RMSS(18), RMSS(5), RMSS(4)
        WRITE(MSTU(11),5400)
        WRITE(MSTU(11),6500)
 
      ENDIF
 
C...Only rewind when reading
      IF (MUPDA.LE.2.OR.MUPDA.EQ.5) REWIND(LFN)
 
 9999 RETURN
 
C...Serious error catching
  460 write(*,*) '* (PYSLHA:) read BLOCK error on line',NLINE
      write(*,*) CHINL(1:80)
      STOP
  470 WRITE(*,*) '* (PYSLHA:) read DECAY error on line',NLINE
      WRITE(*,*) CHINL(1:72)
      STOP
  480 WRITE(*,*) '* (PYSLHA:) read BR error on line',NLINE
      WRITE(*,*) CHINL(1:80)
      STOP
  490 WRITE(*,*) '* (PYSLHA:) read NDA error on line',NLINE
      WRITE(*,*) CHINL(1:80)
      STOP
  500 WRITE(*,*) '* (PYSLHA:) decay daughter read error on line',NLINE
      WRITE(*,*) CHINL(1:80)
  510 WRITE(*,*) '* (PYSLHA:) read Q error in BLOCK ',CHBLCK
      STOP
  520 WRITE(*,*) '* (PYSLHA:) read error in line ',NLINE,':'
      WRITE(*,*) CHINL(1:80)
      STOP
 
 8300 FORMAT(I9)
 8500 FORMAT(F16.5)
 
C...Formats for user information printout.
 5000 FORMAT(1x,15('*'),1x,'PYSLHA v1.08: SUSY/BSM SPECTRUM '
     &     ,'INTERFACE',1x,15('*')/1x,'*',2x
     &     ,'PYSLHA:  Last Change',1x,A,1x,'-',1x,'P.Z. Skands')
 5010 FORMAT(1x,'*',3x,'Wrote spectrum file on unit: ',I3)
 5020 FORMAT(1x,'*',3x,'Read spectrum file on unit: ',I3)
 5030 FORMAT(1x,'*',3x,'Spectrum Calculator was: ',A,' version ',A)
 5040 FORMAT(1x,'*',3x,'Higgs sector corrected with FeynHiggs')
 5100 FORMAT(1x,'*',1x,'Model parameters:'/1x,'*',1x,'----------------')
 5200 FORMAT(1x,'*',1x,3x,'M_0',6x,'M_1/2',5x,'A_0',3x,'Tan(beta)',
     &     3x,'Sgn(mu)',3x,'M_t'/1x,'*',1x,4(F8.2,1x),I8,2x,F8.2)
 5300 FORMAT(1x,'*'/1x,'*',1x,'Model spectrum :'/1x,'*',1x
     &     ,'----------------')
 5400 FORMAT(1x,'*',1x,A)
 5500 FORMAT(1x,'*',1x,A,':')
 5600 FORMAT(1x,'*',2x,2x,'M_GUT',2x,2x,'g_GUT',2x,1x,'alpha_GUT'/
     &       1x,'*',2x,1P,2(1x,E8.2),2x,E8.2)
 5700 FORMAT(1x,'*',4x,4x,'~d',2x,1x,4x,'~u',2x,1x,4x,'~s',2x,1x,
     &     4x,'~c',2x,1x,1x,'~b(12)',1x,1x,1x,'~t(12)'/1x,'*',2x,'L',1x
     &     ,6(F8.2,1x)/1x,'*',2x,'R',1x,6(F8.2,1x))
 5800 FORMAT(1x,'*'/1x,'*',4x,4x,'~e',2x,1x,2x,'~nu_e',2x,1x,3x,'~mu',2x
     &     ,1x,1x,'~nu_mu',1x,1x,'~tau(12)',1x,1x,'~nu_tau'/1x,'*',2x
     &     ,'L',1x,6(F8.2,1x)/1x,'*',2x,'R',1x,6(F8.2,1x))
 5900 FORMAT(1x,'*'/1x,'*',4x,4x,'~g',2x,1x,1x,'~chi_10',1x,1x,'~chi_20'
     &     ,1x,1x,'~chi_30',1x,1x,'~chi_40',1x,1x,'~chi_1+',1x
     &     ,1x,'~chi_2+'/1x,'*',3x,1x,7(F8.2,1x))
 6000 FORMAT(1x,'*'/1x,'*',3x,1x,8(1x,A7,1x)/1x,'*',3x,1x,8(F8.2,1x))
 6100 FORMAT(1x,'*',11x,'|',3x,'~B',3x,'|',2x,'~W_3',2x,'|',2x
     &     ,'~H_1',2x,'|',2x,'~H_2',2x,'|'/1x,'*',3x,'~chi_10',1x,4('|'
     &     ,1x,F6.3,1x),'|'/1x,'*',3x,'~chi_20',1x,4('|'
     &     ,1x,F6.3,1x),'|'/1x,'*',3x,'~chi_30',1x,4('|'
     &     ,1x,F6.3,1x),'|'/1x,'*',3x,'~chi_40',1x,4('|'
     &     ,1x,F6.3,1x),'|')
 6200 FORMAT(1x,'*'/1x,'*',6x,'L',4x,'|',3x,'~W',3x,'|',3x,'~H',3x,'|'
     &     ,12x,'R',4x,'|',3x,'~W',3x,'|',3x,'~H',3x,'|'/1x,'*',3x
     &     ,'~chi_1+',1x,2('|',1x,F6.3,1x),'|',9x,'~chi_1+',1x,2('|',1x
     &     ,F6.3,1x),'|'/1x,'*',3x,'~chi_2+',1x,2('|',1x,F6.3,1x),'|',9x
     &     ,'~chi_2+',1x,2('|',1x,F6.3,1x),'|')
 6300 FORMAT(1x,'*'/1x,'*',8x,'|',2x,'~b_L',2x,'|',2x,'~b_R',2x,'|',8x
     &     ,'|',2x,'~t_L',2x,'|',2x,'~t_R',2x,'|',10x
     &     ,'|',1x,'~tau_L',1x,'|',1x,'~tau_R',1x,'|'/
     &     1x,'*',3x,'~b_1',1x,2('|',1x,F6.3,1x),'|',3x,'~t_1',1x,2('|'
     &     ,1x,F6.3,1x),'|',3x,'~tau_1',1x,2('|',1x,F6.3,1x),'|'/
     &     1x,'*',3x,'~b_2',1x,2('|',1x,F6.3,1x),'|',3x,'~t_2',1x,2('|'
     &     ,1x,F6.3,1x),'|',3x,'~tau_2',1x,2('|',1x,F6.3,1x),'|')
 6400 FORMAT(1x,'*',3x,'  A_b = ',F8.2,4x,'      A_t = ',F8.2,4x
     &     ,'A_tau = ',F8.2)
 6450 FORMAT(1x,'*',3x,'alpha = ',F8.2,4x,'tan(beta) = ',F8.2,4x
     &     ,'   mu = ',F8.2)
 6500 FORMAT(1x,32('*'),1x,'END OF PYSLHA',1x,31('*'))
 
C...Format to use for comments
 7000 FORMAT('# ',A)
C...Format to use for block statements
 7010 FORMAT('Block',1x,A,3x,'#',1x,A)
 7020 FORMAT('Block',1x,A,1x,'Q=',1P,E16.8,0P,3x,'#',1x,A)
C...Indexed Int
 7110 FORMAT(1x,I4,1x,I4,3x,'#')
C...Non-Indexed Double
 7200 FORMAT(9x,1P,E16.8,0P,3x,'#',1x,A)
C...Indexed Double
 7210 FORMAT(1x,I4,3x,1P,E16.8,0P,3x,'#',1x,A)
C...Long Indexed Double (PDG + double)
 7220 FORMAT(1x,I9,3x,1P,E16.8,0P,3x,'#',1x,A)
C...Indexed Char(12)
 7310 FORMAT(1x,I4,3x,A12,3x,'#',1x,A)
C...Single matrix
 7410 FORMAT(1x,I2,1x,I2,3x,1P,E16.8,0P,3x,'#',1x,A)
C...Double Matrix
 7420 FORMAT(1x,I2,1x,I2,3x,1P,E16.8,3x,E16.8,0P,3x,'#',1x,A)
C...Write Decay Table
 7500 FORMAT('Decay',1x,I9,1x,'WIDTH=',1P,E16.8,0P,3x,'#',1x,A)
 7510 FORMAT(4x,1P,E16.8,0P,3x,I2,3x,'IDA=',1x,5(1x,I9),3x,'#',1x,A)
 
      END
