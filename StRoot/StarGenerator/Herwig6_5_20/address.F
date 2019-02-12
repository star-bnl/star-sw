C--------------------------------------------------------------------
C ...Common blocks from herwig6520.F
C
C ...JN: I wrote a pile of these before I realized that most were
C        probably unnecesary
C--------------------------------------------------------------------

c$$$c$$$      Function address_of_phorad()
c$$$c$$$      Integer address_of_phorad, loc
c$$$c$$$      COMMON/PHORAD/QEDRAD(NMXHEP)
c$$$c$$$      address_of_hiprant = loc ( qedrad(1) )
c$$$c$$$      return
c$$$c$$$      End

      Function address_of_hepeup()
      use iso_c_binding
      type(c_ptr) :: address_of_hepeup
      INTEGER MAXNUP
      PARAMETER (MAXNUP=500)
      INTEGER, target :: NUP
      INTEGER         :: IDPRUP,IDUP,ISTUP,MOTHUP,ICOLUP
      REAL(8) XWGTUP,SCALUP,AQEDUP,AQCDUP,PUP,VTIMUP,SPINUP
      COMMON/HEPEUP/NUP,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP,
     &              IDUP(MAXNUP),ISTUP(MAXNUP),MOTHUP(2,MAXNUP),
     &              ICOLUP(2,MAXNUP),PUP(5,MAXNUP),VTIMUP(MAXNUP),
     &              SPINUP(MAXNUP)
      address_of_hepeup = c_loc ( nup )
      return 
      End

      Function address_of_hwgup()
      use iso_c_binding
      type(c_ptr) :: address_of_hwgup
      INTEGER, target :: ILOC,JLOC
      INTEGER         :: MAXNUP
      PARAMETER (MAXNUP=500,NMXHEP=4000)
      COMMON /HWGUP/ILOC(NMXHEP),JLOC(MAXNUP)
      address_of_hwgup = c_loc ( iloc(1) )
      return
      End

c$$$c$$$      Function address_of_hwsint()
c$$$c$$$      type(c_ptr) :: address_of_hwsint
c$$$c$$$      COMMON/HWSINT/QRAT,QLAM
c$$$c$$$      address_of_hwsint = loc ( qrat )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwbvic()
c$$$c$$$      type(c_ptr) :: address_of_hwbvic
c$$$c$$$      COMMON/HWBVIC/NBV,IBV(18)
c$$$c$$$      address_of_hwbvic = loc ( nbv )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwcfrm()
c$$$c$$$      type(c_ptr) :: address_of_hwcfrm
c$$$c$$$      COMMON/HWCFRM/VCLUS(4,NMXHEP)
c$$$c$$$      address_of_hwcfrm = loc ( vclus(1) )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwhews()
c$$$c$$$      type(c_ptr) :: address_of_hwhews
c$$$c$$$      COMMON/HWHEWS/S(8,8,2),D(8,8)
c$$$c$$$      address_of_hwhews = loc ( s(1,1,1) )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwd3by()
c$$$c$$$      type(c_ptr) :: address_of_hwd3by
c$$$c$$$      COMMON/HWD3BY/F0,F0M,F1M,F1,F2,F3,F01,A(2,NDIAGR),B(2,NDIAGR),
c$$$c$$$     &     MS(NDIAGR),MWD(NDIAGR),MR(NDIAGR),MA(4),MA2(4),MB(4),MB2(4),
c$$$c$$$     &     M342,M232,M242,P,PM,CFTHRE,IDP,DRTYPE,NCTHRE,DRCF
c$$$c$$$      address_of_hwd3by = loc ( f0 )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwhewp()
c$$$c$$$      type(c_ptr) :: address_of_hwhewp
c$$$c$$$      COMMON/HWHEWP/XMASS(10),PLAB(5,10),PRW(5,2),PCM(5,10)
c$$$c$$$      address_of_hwhewp = loc ( xmass(1) )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwd4by()
c$$$c$$$      type(c_ptr) :: address_of_hwd4by
c$$$c$$$      COMMON/HWD4BY/A(2),B(2),MS(2),MWD(2),MR(2),M(5),M2(5),P,IDP
c$$$c$$$      address_of_hwd4by = loc ( a(1) )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_mcmas1()
c$$$c$$$      type(c_ptr) :: address_of_mcmas1
c$$$c$$$      COMMON/MCMAS1/
c$$$c$$$     *       NPMNQQ, NPMXQQ,
c$$$c$$$     *       AMASS(-20:MCNUM), CHARGE(-20:MCNUM), CTAU(-20:MCNUM),
c$$$c$$$     *       IDMC(-20:MCNUM), SPIN(-20:MCNUM),
c$$$c$$$     *       RWIDTH(-20:MCNUM), RMASMN(-20:MCNUM), RMASMX(-20:MCNUM),
c$$$c$$$     *       LPARTY(-20:MCNUM), CPARTY(-20:MCNUM),
c$$$c$$$     *       IMIXPP(-20:MCNUM), RMIXPP(-20:MCNUM),
c$$$c$$$     *       ICPMIX(-20:MCNUM), RCPMIX(-20:MCNUM),
c$$$c$$$     *       INVMC(0:MCSTBL)
c$$$c$$$      address_of_mcmas1 = loc ( npmnqq )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_mcpOl1()
c$$$c$$$      type(c_ptr) :: address_of_mcpOl1
c$$$c$$$      COMMON/MCPOL1/
c$$$c$$$     *       NPOLQQ, IPOLQQ(5,MPOLQQ)
c$$$c$$$      address_of_mcpOl1 = loc ( npOlqq )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_mcnams()
c$$$c$$$      type(c_ptr) :: address_of_mcnams
c$$$c$$$      COMMON/MCNAMS/
c$$$c$$$     *       QNAME(37), PNAME(-20:MCNUM)
c$$$c$$$      address_of_mcnams = loc ( qname(1) )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_mccm1a()
c$$$c$$$      type(c_ptr) :: address_of_mccm1a
c$$$c$$$      COMMON/MCCM1A/
c$$$c$$$     *   NCTLQQ, NDECQQ, IVRSQQ, IORGQQ, IRS1QQ(3), BFLDQQ,
c$$$c$$$     *   ENERQQ, BEAMQQ, BMPSQQ, BMNGQQ, EWIDQQ, BWPSQQ, BWNGQQ,
c$$$c$$$     *   BPOSQQ(3), BSIZQQ(3),
c$$$c$$$     *   IEVTQQ, IRUNQQ,
c$$$c$$$     *   IBMRAD, ECM, P4CMQQ(4), P4PHQQ(4),
c$$$c$$$     *   ENERNW, BEAMNW, BEAMP, BEAMN,
c$$$c$$$     *   NTRKMC, QQNTRK, NSTBMC, NSTBQQ, NCHGMC, NCHGQQ,
c$$$c$$$     *   IRANQQ(2), IRANMC(2), IRANCC(2), IRS2QQ(5),
c$$$c$$$     *   IPFTQQ(MCTRK), IPCDQQ(MCTRK), IPRNTV(MCTRK), ITYPEV(MCTRK,2),
c$$$c$$$     *   IDECSV(MCTRK), IDAUTV(MCTRK), ISTBMC(MCTRK), NDAUTV(MCTRK),
c$$$c$$$     *   IVPROD(MCTRK), IVDECA(MCTRK),
c$$$c$$$     *   PSAV(MCTRK,4), HELCQQ(MCTRK), P4QQ(4,MCTRK)
c$$$c$$$      address_of_mccm1a = loc ( nctlqq )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_mccm1b()
c$$$c$$$      type(c_ptr) :: address_of_mccm1b
c$$$c$$$      COMMON/MCCM1B/
c$$$c$$$     *   DATEQQ, TIMEQQ, FOUTQQ, FCTLQQ, FDECQQ, FGEOQQ,
c$$$c$$$     *   CCTLQQ(MCTLQQ), CDECQQ(MDECQQ)
c$$$c$$$      address_of_mccm1b = loc ( dateqq )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_mccm2()
c$$$c$$$      type(c_ptr) :: address_of_mccm2
c$$$c$$$      COMMON/MCCM2/
c$$$c$$$     *   NVRTX, XVTX(MCVRTX,3), TVTX(MCVRTX), RVTX(MCVRTX),
c$$$c$$$     *   ITRKIN(MCVRTX), NTRKOU(MCVRTX), ITRKOU(MCVRTX),
c$$$c$$$     *   IVKODE(MCVRTX)
c$$$c$$$      address_of_mccm2 = loc ( nvrtx )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_data1()
c$$$c$$$      type(c_ptr) :: address_of_data1
c$$$c$$$      COMMON/DATA1/QQIST,QQIFR,QQPUD,QQPS1,QQSIGM,QQMAS(15),QQPAR(25)
c$$$c$$$      address_of_data1 = loc ( qqist )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_data2()
c$$$c$$$      type(c_ptr) :: address_of_data2
c$$$c$$$      COMMON/DATA2/QQCZF(15),QQMESO(36),QQCMIX(6,2)
c$$$c$$$      address_of_data2 = loc ( qqczf(1) )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_data3()
c$$$c$$$      type(c_ptr) :: address_of_data3
c$$$c$$$      COMMON/DATA3/QQCND(3)
c$$$c$$$      address_of_data3 = loc ( qqcnd(1) )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_data5()
c$$$c$$$      type(c_ptr) :: address_of_data5
c$$$c$$$      COMMON/DATA5/QQBSPI(5),QQBSYM(3)
c$$$c$$$      address_of_data5 = loc ( qqbspi(1) )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_jet()
c$$$c$$$      type(c_ptr) :: address_of_jet
c$$$c$$$      COMMON/JET/QQN,QQK(250,2),QQP(250,5),QQNC,QQKC(10),QQPC(10,4),
c$$$c$$$     *  QQLASTN
c$$$c$$$      address_of_jet = loc ( qqn )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_inpout()
c$$$c$$$      type(c_ptr) :: address_of_inpout
c$$$c$$$      COMMON/INPOUT/EULUN0,EULUN1,EULUN2
c$$$c$$$      address_of_inpout = loc ( eulun0 )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_filnam()
c$$$c$$$      type(c_ptr) :: address_of_filnam
c$$$c$$$      COMMON/FILNAM/EUDATD,EUTIT
c$$$c$$$      address_of_filnam = loc ( eudatd )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hvyini()
c$$$c$$$      type(c_ptr) :: address_of_hvyini
c$$$c$$$      COMMON/HVYINI/AMINIE
c$$$c$$$      address_of_hvyini = loc ( aminie )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_runinf()
c$$$c$$$      type(c_ptr) :: address_of_runinf
c$$$c$$$      COMMON/RUNINF/EURUN,EUEVNT,EUWEI
c$$$c$$$      address_of_runinf = loc ( eurun )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_momgen()
c$$$c$$$      type(c_ptr) :: address_of_momgen
c$$$c$$$      COMMON/MOMGEN/EUNP,EUIP,EUAPM,EUPCM,EUPHEL,EUPVTX
c$$$c$$$      address_of_momgen = loc ( eunp )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_result()
c$$$c$$$      type(c_ptr) :: address_of_result
c$$$c$$$      COMMON/RESULT/EUTEIL,EUPTEI,EUINDX,EUORIG,EUDCAY,EUTHEL,EUSECV
c$$$c$$$      address_of_result = loc ( euteil )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_pctabl()
c$$$c$$$      type(c_ptr) :: address_of_pctabl
c$$$c$$$      COMMON/PCTABL/EUNPA,EUIPC,EUIPDG,EUPM,EUPLT,EUIDP
c$$$c$$$      address_of_pctabl = loc ( eunpa )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_convrt()
c$$$c$$$      type(c_ptr) :: address_of_convrt
c$$$c$$$      COMMON/CONVRT/EUCONV
c$$$c$$$      address_of_convrt = loc ( euconv )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_ffs()
c$$$c$$$      type(c_ptr) :: address_of_ffs
c$$$c$$$      COMMON/FFS/TB,BT
c$$$c$$$      address_of_ffs = loc ( tb )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_ssf()
c$$$c$$$      type(c_ptr) :: address_of_ssf
c$$$c$$$      COMMON/SFF/IT1,IB1,IT2,IB2
c$$$c$$$      address_of_ssf = loc ( it1 )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_phoqed()
c$$$c$$$      type(c_ptr) :: address_of_phoqed
c$$$c$$$      COMMON /PHOQED/ QEDRAD(NMXHEP)
c$$$c$$$      address_of_phoqed = loc ( qedrad(1) )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_taupos()
c$$$c$$$      type(c_ptr) :: address_of_taupos
c$$$c$$$      COMMON /TAUPOS/ NP1, NP2
c$$$c$$$      address_of_taupos = loc ( np1 )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_momdec()
c$$$c$$$      type(c_ptr) :: address_of_momdec
c$$$c$$$      COMMON / MOMDEC / Q1,Q2,P1,P2,P3,P4
c$$$c$$$      address_of_momdec = loc ( q1 )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_heprup()
c$$$c$$$      type(c_ptr) :: address_of_heprup
c$$$c$$$      COMMON /HEPRUP/ IDBMUP(2),EBMUP(2),PDFGUP(2),PDFSUP(2),
c$$$c$$$     &                IDWTUP,NPRUP,XSECUP(MAXPUP),XERRUP(MAXPUP),
c$$$c$$$     &                XMAXUP(MAXPUP),LPRUP(MAXPUP)
c$$$c$$$      address_of_heprup = loc ( idbmup(1) )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwcir2()
c$$$c$$$      type(c_ptr) :: address_of_hwcir2
c$$$c$$$      COMMON /HWCIR2/CIRCKP
c$$$c$$$      address_of_hwcir2 = loc ( circkp )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwhzbb()
c$$$c$$$      type(c_ptr) :: address_of_hwhzbb
c$$$c$$$      COMMON/HWHZBB/F(8,8)
c$$$c$$$      address_of_hwhzbb = loc ( f(1,1) )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwhzbc()
c$$$c$$$      type(c_ptr) :: address_of_hwhzbc
c$$$c$$$      COMMON /HWHZBC/G
c$$$c$$$      address_of_hwhzbc = loc ( g )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwpsom()
c$$$c$$$      type(c_ptr) :: address_of_hwpsom
c$$$c$$$      COMMON /HWPSOM/ WI
c$$$c$$$      address_of_hwpsom = loc ( wi )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwarea()
c$$$c$$$      type(c_ptr) :: address_of_hwarea
c$$$c$$$      COMMON /HWAREA/ Y,Q2,SHAT,Z,PHI,AJACOB,DSIGMA,ME,MP,ML,MREMIF,
c$$$c$$$     & MFIN1,MFIN2,RS,SMA,W2,RSHAT,IQK,IFLAVU,IFLAVD,IMIN,IMAX,IFL,LEP,
c$$$c$$$     & IPROO,CHARGD,INCLUD,INSIDE
c$$$c$$$      address_of_hwarea = loc ( y )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwhewq()
c$$$c$$$      type(c_ptr) :: address_of_hwhewq
c$$$c$$$      COMMON/HWHEWQ/ZH(8,8),ZCH(8,8),ZD(8,8)
c$$$c$$$      address_of_hwhewq = loc ( zh(1,1) )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwhewr()
c$$$c$$$      type(c_ptr) :: address_of_hwhewr
c$$$c$$$      COMMON/HWHEWR/CPFAC(12,12,8),CPALL(8)
c$$$c$$$      address_of_hwhewr = loc ( cpfac(1,1,1) )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwhgbc()
c$$$c$$$      type(c_ptr) :: address_of_hwhgbc
c$$$c$$$      COMMON/HWHGBC/ MW2,MZ2,GMW,GMZ,G,EE,CKM2,RF,LF,TAUI,CSW,CFAC1
c$$$c$$$      address_of_hwhgbc = loc ( mw2 )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwbosn()
c$$$c$$$      type(c_ptr) :: address_of_hwbosn
c$$$c$$$      COMMON /HWBOSN/XMASS(2),PLAB(5,10),PRW(5,2),PCM(5,10),IDBOS(2),
c$$$c$$$     &     IDRES,IDP(10),IOPT
c$$$c$$$      address_of_hwbosn = loc ( xmass(1) )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_cints()
c$$$c$$$      type(c_ptr) :: address_of_cints
c$$$c$$$      COMMON/CINTS/BI,CI,DI
c$$$c$$$      address_of_cints = loc ( bi )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_small()
c$$$c$$$      type(c_ptr) :: address_of_small
c$$$c$$$      COMMON/SMALL/EPSI
c$$$c$$$      address_of_small = loc ( epsi )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_sqsqh()
c$$$c$$$      type(c_ptr) :: address_of_sqsqh
c$$$c$$$      COMMON/SQSQH/JHIGGS,ILBL,JH,IF1MIN,IF1MAX,IF2MIN,IF2MAX
c$$$c$$$      address_of_sqsqh = loc ( jhiggs )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwrpin()
c$$$c$$$      type(c_ptr) :: address_of_hwrpin
c$$$c$$$      COMMON/HWRPIN/XMIN,XMAX,XPOW,FIRST
c$$$c$$$      address_of_hwrpin = loc ( xmin )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwsphc()
c$$$c$$$      type(c_ptr) :: address_of_hwsphc
c$$$c$$$      COMMON/HWSPHC/F3,F4,F3M,F4M,FTP,FTM,FUP,FUM,FST,A,B,MS,MWD,MR,MA,
c$$$c$$$     &     MA2,SH,TH,UH,IDP,DRTYPE
c$$$c$$$      address_of_hwsphc = loc ( f3 )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwspnc()
c$$$c$$$      type(c_ptr) :: address_of_hwspnc
c$$$c$$$      COMMON /HWSPNC/ SW,CW,TW,E,G,RT,ORT,MW,MZ,GS,AFN,AFG,AFC,OIJ,OIJP,
c$$$c$$$     &               OIJPP,HNN,HCC,HNC,HFF,HWW,HZZ,ZAB,HHB
c$$$c$$$      address_of_hwspnc = loc ( sw )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_hwtabc()
c$$$c$$$      type(c_ptr) :: address_of_hwtabc
c$$$c$$$      COMMON/HWTABC/XLAST,N0,IS,ID
c$$$c$$$      address_of_hwtabc = loc ( xlast )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_sascom()
c$$$c$$$      type(c_ptr) :: address_of_sascom
c$$$c$$$      COMMON/SASCOM/XPVMD(-6:6),XPANL(-6:6),XPANH(-6:6),XPBEH(-6:6),
c$$$c$$$     &XPDIR(-6:6)
c$$$c$$$      address_of_sascom = loc ( xpvmd(1) )
c$$$c$$$      return
c$$$c$$$      End
c$$$
c$$$c$$$      Function address_of_sasval()
c$$$c$$$      type(c_ptr) :: address_of_sasval
c$$$c$$$      COMMON/SASVAL/VXPVMD(-6:6),VXPANL(-6:6),VXPANH(-6:6),VXPDGM(-6:6)
c$$$c$$$      address_of_sasval = loc ( vxpvmd(1) )
c$$$c$$$      return
c$$$c$$$      End

C--------------------------------------------------------------------
C... Common blocks from herwig6510.inc
C--------------------------------------------------------------------

C New standard event common
      Function address_of_hepevt()
      use iso_c_binding
      type(c_ptr) :: address_of_hepevt
      PARAMETER (NMXHEP=4000)
      INTEGER, target :: NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      REAL(8) PHEP,VHEP
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     & JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      address_of_hepevt = c_loc ( nevhep )
      return
      End

C Beams, process and number of events
      Function address_of_hwbeam()
      use iso_c_binding
      type(c_ptr) :: address_of_hwbeam
      INTEGER,target :: IPART1,IPART2
      COMMON/HWBEAM/IPART1,IPART2
      address_of_hwbeam = c_loc ( ipart1 )
      return
      End

C The SetBeams subroutine sets the HWBMCH common block values
c$$$      Function address_of_hwbmch()
c$$$      type(c_ptr) :: address_of_hwbmch
c$$$      CHARACTER*8 PART1,PART2
c$$$      COMMON/HWBMCH/PART1,PART2
c$$$      address_of_hwbmch = loc ( part1 )
c$$$      return
c$$$      End

      Function address_of_hwproc()
      use iso_c_binding
      type(c_ptr) :: address_of_hwproc
      REAL(8), target :: EBEAM1,EBEAM2,PBEAM1,PBEAM2
      INTEGER IPROC,MAXEV
      COMMON/HWPROC/EBEAM1,EBEAM2,PBEAM1,PBEAM2,IPROC,MAXEV
      address_of_hwproc = c_loc ( ebeam1 )
      return
      End

C Basic parameters (and quantities derived from them)
      Function address_of_hwpram()
      use iso_c_binding
      type(c_ptr) :: address_of_hwpram
      REAL(8),target::AFCH,ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR,CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL,PHIMIX,PIFAC,
     & PRSOF,PSPLT,PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH,QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH,VCKM,VGCUT,VQCUT,
     & VPCUT,ZBINM,EFFMIN,OMHMIX,ET2MIX,PH3MIX,GCUTME
      INTEGER IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF,NBTRY,NCOLO,
     & NCTRY,NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,IOP4JT,
     & NPRFMT,CLDIR
      LOGICAL AZSOFT,AZSPIN,HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME,
     & PRNDEF,PRNTEX,PRNWEB
      COMMON/HWPRAM/AFCH(16,2),ALPHEM,B1LIM,BETAF,BTCLM,CAFAC,CFFAC,
     & CLMAX,CLPOW,CLSMR(2),CSPEED,ENSOF,ETAMIX,F0MIX,F1MIX,F2MIX,GAMH,
     & GAMW,GAMZ,GAMZP,GEV2NB,H1MIX,PDIQK,PGSMX,PGSPL(4),PHIMIX,PIFAC,
     & PRSOF,PSPLT(2),PTRMS,PXRMS,QCDL3,QCDL5,QCDLAM,QDIQK,QFCH(16),QG,
     & QSPAC,QV,SCABI,SWEIN,TMTOP,VFCH(16,2),VCKM(3,3),VGCUT,VQCUT,
     & VPCUT,ZBINM,EFFMIN,OMHMIX,ET2MIX,PH3MIX,GCUTME,
     & IOPREM,IPRINT,ISPAC,LRSUD,LWSUD,MODPDF(2),NBTRY,NCOLO,NCTRY,
     & NDTRY,NETRY,NFLAV,NGSPL,NSTRU,NSTRY,NZBIN,IOP4JT(2),NPRFMT,
     & AZSOFT,AZSPIN,CLDIR(2),HARDME,NOSPAC,PRNDEC,PRVTX,SOFTME,ZPRIME,
     & PRNDEF,PRNTEX,PRNWEB
      address_of_hwpram = c_loc ( afch(1,1) )
      return
      End

c$$$      Function address_of_hwprch()
c$$$      type(c_ptr) :: address_of_hwprch
c$$$      CHARACTER*4 BDECAY
c$$$      CHARACTER*20 AUTPDF
c$$$      COMMON/HWPRCH/AUTPDF(2),BDECAY
c$$$      address_of_hwprch = loc ( autpdf(1) )
c$$$      return
c$$$      End

c$$$C Parton shower common (same format as /HEPEVT/)
c$$$      Function address_of_hwpart()
c$$$      type(c_ptr) :: address_of_hwpart
c$$$      PARAMETER (NMXPAR=500)
c$$$      COMMON/HWPART/NEVPAR,NPAR,ISTPAR(NMXPAR),IDPAR(NMXPAR),
c$$$     & JMOPAR(2,NMXPAR),JDAPAR(2,NMXPAR),PPAR(5,NMXPARc),VPAR(4,NMXPAR)
c$$$      address_of_hwpart = loc ( nevpar )
c$$$      return
c$$$      End

c$$$C Parton polarization common
c$$$      Function address_of_hwparp()
c$$$      type(c_ptr) :: address_of_hwparp
c$$$      COMMON/HWPARP/DECPAR(2,NMXPAR),PHIPAR(2,NMXPAR),RHOPAR(2,NMXPAR),
c$$$     & TMPAR(NMXPAR)
c$$$      address_of_hwparp = loc ( decpar(1,1) )
c$$$      return
c$$$      End

c$$$C Electroweak boson common
c$$$      Function address_of_hwbosc()
c$$$      type(c_ptr) :: address_of_hwbosc
c$$$      PARAMETER (MODMAX=50,NMXHEP=4000)
c$$$      COMMON/HWBOSC/ALPFAC,BRHIG(12),ENHANC(12),GAMMAX,RHOHEP(3,NMXHEP),
c$$$     & IOPHIG,MODBOS(MODMAX)
c$$$      address_of_hwbosc = loc ( alpfac )
c$$$      return
c$$$      End

c$$$C Parton colour common
c$$$      Function address_of_hwparc()
c$$$      type(c_ptr) :: address_of_hwparc
c$$$      PARAMETER (NMXPAR=500)
c$$$      COMMON/HWPARC/JCOPAR(4,NMXPAR)
c$$$      address_of_hwparc = loc ( jcopar(1,1) )
c$$$      return
c$$$      End

C other HERWIG branching, event and hard subprocess common blocks
      Function address_of_hwbrch()
      use iso_c_binding
      type(c_ptr) :: address_of_hwbrch
      INTEGER INHAD,JNHAD
      REAL(8), target :: ANOMSC,HARDST,PTINT,XFACT
      COMMON/HWBRCH/ANOMSC(2,2),HARDST,PTINT(3,2),XFACT,INHAD,JNHAD,
     & NSPAC(7),ISLENT,BREIT,FROST,USECMF
      address_of_hwbrch = c_loc ( anomsc(1,1) )
      return
      End

      Function address_of_hwevnt()
      use iso_c_binding
      type(c_ptr) :: address_of_hwevnt
      REAL(8),target::AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,
     & WSQSUM
      INTEGER IDHW,IERROR,ISTAT,LWEVT,MAXER,MAXPR,NRN,NUMER,NUMERU,
     & NWGTS
      LOGICAL NOWGT,GENSOF
      PARAMETER (NMXHEP=4000)
      COMMON/HWEVNT/AVWGT,EVWGT,GAMWT,TLOUT,WBIGST,WGTMAX,WGTSUM,WSQSUM,
     & IDHW(NMXHEP),IERROR,ISTAT,LWEVT,MAXER,MAXPR,NOWGT,NRN(2),NUMER,
     & NUMERU,NWGTS,GENSOF
      address_of_hwevnt = c_loc ( avwgt )
      return
      End

      Function address_of_hwhard()
      use iso_c_binding
      type(c_ptr) :: address_of_hwhard
      REAL(8),target:: ASFIXD,CLQ,COSS,COSTH,CTMAX,DISF,EMLST,EMMAX,
     & EMMIN,EMPOW,EMSCA,EPOLN,GCOEF,GPOLN,OMEGA0,PHOMAS,PPOLN,
     & PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,SINS,THMAX,
     & Y4JT,TMNISR,TQWT,XX,XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,YJMIN,YWWMAX,
     & YWWMIN,WHMIN,ZJMAX,ZMXISR
      INTEGER IAPHIG,IBRN,IBSH,ICO,IDCMF,IDN,IFLMAX,IFLMIN,IHPRO,IPRO,
     & MAPQ,MAXFL
      LOGICAL BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL,DURHAM
      COMMON/HWHARD/ASFIXD,CLQ(7,6),COSS,COSTH,CTMAX,DISF(13,2),EMLST,
     & EMMAX,EMMIN,EMPOW,EMSCA,EPOLN(3),GCOEF(7),GPOLN,OMEGA0,PHOMAS,
     & PPOLN(3),PTMAX,PTMIN,PTPOW,Q2MAX,Q2MIN,Q2POW,Q2WWMN,Q2WWMX,QLIM,
     & SINS,THMAX,Y4JT,TMNISR,TQWT,XX(2),XLMIN,XXMIN,YBMAX,YBMIN,YJMAX,
     & YJMIN,YWWMAX,YWWMIN,WHMIN,ZJMAX,ZMXISR,IAPHIG,IBRN(2),IBSH,
     & ICO(10),IDCMF,IDN(10),IFLMAX,IFLMIN,IHPRO,IPRO,MAPQ(6),MAXFL,
     & BGSHAT,COLISR,FSTEVT,FSTWGT,GENEV,HVFCEN,TPOL,DURHAM
      address_of_hwhard = c_loc ( asfixd )
      return
      End

c$$$C Arrays for particle properties (NMXRES = max no of particles defined)
c$$$      Function address_of_hwprop()
c$$$      type(c_ptr) :: address_of_hwprop
c$$$      PARAMETER(NMXRES=500)
c$$$      COMMON/HWPROP/RLTIM(0:NMXRES),RMASS(0:NMXRES),RSPIN(0:NMXRES),
c$$$     & ICHRG(0:NMXRES),IDPDG(0:NMXRES),IFLAV(0:NMXRES),NRES,
c$$$     & VTOCDK(0:NMXRES),VTORDK(0:NMXRES),
c$$$     & QORQQB(0:NMXRES),QBORQQ(0:NMXRES)
c$$$      address_of_hwprop = loc ( rltim(0) )
c$$$      return
c$$$      End

c$$$      Function address_of_hwunam()
c$$$      type(c_ptr) :: address_of_hwunam
c$$$      PARAMETER(NMXRES=500)
c$$$      COMMON/HWUNAM/RNAME(0:NMXRES),TXNAME(2,0:NMXRES)
c$$$      address_of_hwunam = loc ( rname(0) )
c$$$      return
c$$$      End

c$$$      Function address_of_hwupdt()
c$$$      type(c_ptr) :: address_of_hwupdt
c$$$      PARAMETER(NMXRES=500)
c$$$      COMMON/HWUPDT/BRFRAC(NMXDKS),CMMOM(NMXDKS),DKLTM(NMXRES),
c$$$     & IDK(NMXDKS),IDKPRD(5,NMXDKS),LNEXT(NMXDKS),LSTRT(NMXRES),NDKYS,
c$$$     & NME(NMXDKS),NMODES(NMXRES),NPRODS(NMXDKS),DKPSET,RSTAB(0:NMXRES)
c$$$      address_of_hwupdt = loc ( brfac(1) )
c$$$      return
c$$$      End

c$$$C Weights used in cluster decays
c$$$c$$$      Function address_of_hwuwts()
c$$$c$$$      type(c_ptr) :: address_of_hwuwts
c$$$c$$$      PARAMETER(NMXRES=500)
c$$$c$$$      COMMON/HWUWTS/REPWT(0:3,0:4,0:4),SNGWT,DECWT,QWT(3),PWT(12),
c$$$c$$$     & SWTEF(NMXRES)
c$$$c$$$      address_of_hwuwts = loc ( repwt(0,0,0) )
c$$$c$$$      return
c$$$c$$$      End

c$$$C Parameters for cluster decays (NMXCDK = max total no of cluster
c$$$C                                         decay channels)
c$$$      Function address_of_hwuclu()
c$$$      type(c_ptr) :: address_of_hwuclu
c$$$      PARAMETER(NMXCDK=4000)
c$$$      COMMON/HWUCLU/CLDKWT(NMXCDK),CTHRPW(12,12),PRECO,RESN(12,12),
c$$$     & RMIN(12,12),LOCN(12,12),NCLDK(NMXCDK),NRECO,CLRECO
c$$$      address_of_hwuclu = loc ( cldkwt(1) )
c$$$      return
c$$$      End

c$$$C Variables controling mixing and vertex information
c$$$      Function address_of_hwdist()
c$$$      type(c_ptr) :: address_of_hwdist
c$$$      COMMON/HWDIST/EXAG,GEV2MM,HBAR,PLTCUT,VMIN2,VTXPIP(5),XMIX(2),
c$$$     & XMRCT(2),YMIX(2),YMRCT(2),IOPDKL,MAXDKL,MIXING,PIPSMR
c$$$      address_of_hwdist = loc ( exag )
c$$$      return
c$$$      End

C Custom common block to wrestle certain values out of Herwig
      Function address_of_custom()
      use iso_c_binding
      type(c_ptr) :: address_of_custom
      real, target :: hwmans
      COMMON /CUSTOM/HWMANS,HWMANT,HWMANU 
      address_of_custom = c_loc ( hwmans )
      return
      end
