TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_geant_Maker")) return 0;
  St_geant_Maker *geantMk = StMaker::GetChain()->GetMaker("geant");
  gSystem->Load("Pythia6_4_26");
  gSystem->Load("apythia");
  gSystem->Load("bpythia");
  geantMk->Do("call bpythia");
  //   ** These particles will be decayed by geant instead of pythia **
  geantMk->Do("/PYTHIA/MDCY (102,1)=0");//  ! PI0 111
  geantMk->Do("/PYTHIA/MDCY (106,1)=0");//  ! PI+ 211
  geantMk->Do("/PYTHIA/MDCY (109,1)=0");//  ! ETA 221
  geantMk->Do("/PYTHIA/MDCY (116,1)=0");//  ! K+ 321
  geantMk->Do("/PYTHIA/MDCY (112,1)=0");//  ! K_SHORT 310
  geantMk->Do("/PYTHIA/MDCY (105,1)=0");//  ! K_LONG 130
  geantMk->Do("/PYTHIA/MDCY (164,1)=0");//  ! LAMBDA0 3122
  geantMk->Do("/PYTHIA/MDCY (167,1)=0");//  ! SIGMA0 3212
  geantMk->Do("/PYTHIA/MDCY (162,1)=0");//  ! SIGMA- 3112
  geantMk->Do("/PYTHIA/MDCY (169,1)=0");//  ! SIGMA+ 3222
  geantMk->Do("/PYTHIA/MDCY (172,1)=0");//  ! Xi- 3312
  geantMk->Do("/PYTHIA/MDCY (174,1)=0");//  ! Xi0 3322
  geantMk->Do("/PYTHIA/MDCY (176,1)=0");//  ! OMEGA- 3334
  /* pythiaeRHIC/input 
pythia.txt        ! output file name
11                ! lepton beam type
100, 5            ! proton and electron beam energy
1000,100          ! Number of events 
1e-05, 0.99       ! xmin and xmax
1e-03,0.99        ! ymin and ymax 
0.1,20000         ! Q2min and Q2max 
F2PY,1998         ! F2-Model, R-Parametrisation
0                 ! switch for rad corrections; 0:no, 1:yes, 2:gen.lookup table 
1                 ! Pythia-Model = 0 standard GVMD generation in Pythia-x and Q2; = 1 GVMD model with generation in y and Q2 as for radgen
1,1               ! A-Tar and Z-Tar
1,1               ! nuclear pdf parameter1: nucleon mass number A, charge number Z
201               ! nuclear pdf parameter2: correction order x*100+y x= 1:LO, 2:NLO y:error set
! PMAS(4,1)=1.27    ! charm mass
MSEL=2
MSTP(14)=30
MSTP(15)=0
MSTP(16)=1
MSTP(17)=4 ! MSTP 17=6 is the R-rho measured as by hermes, =4 Default
MSTP(18)=3
MSTP(19)=1 ! Hermes MSTP-19=1 different Q2 suppression, default = 4
MSTP(20)=0 ! Hermes MSTP(20)=0 , default MSTP(20)=3
MSTP(32)=8
MSTP(38)=4
MSTP(51)=10150 ! if pdflib is linked than non pythia-pdfs are available,                         like MSTP(51)=4046 
MSTP(52)=2   ! ---> pdflib used MSTP   52=2
MSTP(53)=3
MSTP(54)=1
MSTP(55)=5
MSTP(56)=1
MSTP(57)=1
MSTP(58)=5
MSTP(59)=1
MSTP(60)=7
MSTP(61)=2
MSTP(71)=1
MSTP(81)=0
MSTP(82)=1
MSTP(91)=1
MSTP(92)=3      ! hermes MSTP(92)=4
MSTP(93)=1
MSTP(101)=3
MSTP(102)=1
MSTP(111)=1
MSTP(121)=0
! ----------- Now all the PARPs -----------
PARP(13)=1
PARP(18)=0.40 ! hermes PARP(18)=0.17
PARP(81)=1.9
PARP(89)=1800
PARP(90)=0.16
PARP(91)=0.40
PARP(93)=5.
PARP(99)=0.40
PARP(100)=5
PARP(102)=0.28
PARP(103)=1.0
PARP(104)=0.8
PARP(111)=2.
PARP(161)=3.00
PARP(162)=24.6
PARP(163)=18.8
PARP(164)=11.5
PARP(165)=0.47679
PARP(166)=0.67597 ! PARP165/166 are linked to MSTP17 as R_rho of HERMES is used
! PARP(166)=0.5    
! ----------- Now come all the switches for Jetset -----------
PARJ(1)=0.100
PARJ(2)=0.300
PARJ(11)=0.5
PARJ(12)=0.6
PARJ(21)= 0.40
PARJ(32)=1.0
PARJ(33)= 0.80
PARJ(41)= 0.30
PARJ(42)= 0.58
PARJ(45)= 0.5
!----------------------------------------------------------------------
MSTJ(1)=1
MSTJ(12)=1
MSTJ(45)=5
MSTU(16)=2
MSTU(112)=5
MSTU(113)=5
MSTU(114)=5
! ----------- Now all the CKINs for pythia ----------
CKIN(1)=1.
CKIN(2)=-1.
CKIN(3)=0.
CKIN(4)=-1.
CKIN(5)=1.00
CKIN(6)=1.00
CKIN(7)=-10.
CKIN(8)=10.
CKIN(9)=-40.
CKIN(10)=40.
CKIN(11)=-40.
CKIN(12)=40.
CKIN(13)=-40.
CKIN(14)=40.
CKIN(15)=-40.
CKIN(16)=40.
CKIN(17)=-1.
CKIN(18)=1.
CKIN(19)=-1.
CKIN(20)=1.
CKIN(21)=0.
CKIN(22)=1.
CKIN(23)=0.
CKIN(24)=1.
CKIN(25)=-1.
CKIN(26)=1.
CKIN(27)=-1.
CKIN(28)=1.
CKIN(31)=2.
CKIN(32)=-1.
CKIN(35)=0.
CKIN(36)=-1
CKIN(37)=0.
CKIN(38)=-1.
CKIN(39)=4.
CKIN(40)=-1.
CKIN(65)=1.e-09        ! Min for Q^2
CKIN(66)=-1.       ! Max for Q^2
CKIN(67)=0.
CKIN(68)=-1. 
CKIN(77)=2.0
CKIN(78)=-1.
  */
#if 0
pythia.txt        ! output file name
11                ! lepton beam type
100, 5            ! proton and electron beam energy
1000,100          ! Number of events 
1e-05, 0.99       ! xmin and xmax
1e-03,0.99        ! ymin and ymax 
0.1,20000         ! Q2min and Q2max 
F2PY,1998         ! F2-Model, R-Parametrisation
0                 ! switch for rad corrections; 0:no, 1:yes, 2:gen.lookup table 
1                 ! Pythia-Model = 0 standard GVMD generation in Pythia-x and Q2; = 1 GVMD model with generation in y and Q2 as for radgen
1,1               ! A-Tar and Z-Tar
1,1               ! nuclear pdf parameter1: nucleon mass number A, charge number Z
201               ! nuclear pdf parameter2: correction order x*100+y x= 1:LO, 2:NLO y:error set
! PMAS(4,1)=1.27    ! charm mass
#endif
 geantMk->Do("/PYTHIA/MSEL 2");//
 geantMk->Do("/PYTHIA/MSTP(14) 30");//
 geantMk->Do("/PYTHIA/MSTP(15) 0");//
 geantMk->Do("/PYTHIA/MSTP(16) 1");//
 geantMk->Do("/PYTHIA/MSTP(17) 4");// ! MSTP 17 6 is the R-rho measured as by hermes, =4 Default
 geantMk->Do("/PYTHIA/MSTP(18) 3");//
 geantMk->Do("/PYTHIA/MSTP(19) 1");// ! Hermes MSTP-19=1 different Q2 suppression, default = 4
 geantMk->Do("/PYTHIA/MSTP(20) 0");// ! Hermes MSTP(20)=0 , default MSTP(20)=3
 geantMk->Do("/PYTHIA/MSTP(32) 8");//
 geantMk->Do("/PYTHIA/MSTP(38) 4");//
 geantMk->Do("/PYTHIA/MSTP(51) 10150");// ! if pdflib is linked than non pythia-pdfs are available,                         like MSTP(51)=4046 
 geantMk->Do("/PYTHIA/MSTP(52) 2");//   ! ---> pdflib used MSTP   52=2
 geantMk->Do("/PYTHIA/MSTP(53) 3");//
 geantMk->Do("/PYTHIA/MSTP(54) 1");//
 geantMk->Do("/PYTHIA/MSTP(55) 5");//
 geantMk->Do("/PYTHIA/MSTP(56) 1");//
 geantMk->Do("/PYTHIA/MSTP(57) 1");//
 geantMk->Do("/PYTHIA/MSTP(58) 5");//
 geantMk->Do("/PYTHIA/MSTP(59) 1");//
 geantMk->Do("/PYTHIA/MSTP(60) 7");//
 geantMk->Do("/PYTHIA/MSTP(61) 2");//
 geantMk->Do("/PYTHIA/MSTP(71) 1");//
 geantMk->Do("/PYTHIA/MSTP(81) 0");//
 geantMk->Do("/PYTHIA/MSTP(82) 1");//
 geantMk->Do("/PYTHIA/MSTP(91) 1");//
 geantMk->Do("/PYTHIA/MSTP(92) 3");//      ! hermes MSTP(92) 4
 geantMk->Do("/PYTHIA/MSTP(93) 1");//
 geantMk->Do("/PYTHIA/MSTP(101) 3");//
 geantMk->Do("/PYTHIA/MSTP(102) 1");//
 geantMk->Do("/PYTHIA/MSTP(111) 1");//
 geantMk->Do("/PYTHIA/MSTP(121) 0");//
 //! ----------- Now all the PARPs -----------
 geantMk->Do("/PYTHIA/PARP(13) 1");//
 geantMk->Do("/PYTHIA/PARP(18) 0.40");// ! hermes PARP(18)=0.17
 geantMk->Do("/PYTHIA/PARP(81) 1.9");//
 geantMk->Do("/PYTHIA/PARP(89) 1800");//
 geantMk->Do("/PYTHIA/PARP(90) 0.16");//
 geantMk->Do("/PYTHIA/PARP(91) 0.40");//
 geantMk->Do("/PYTHIA/PARP(93) 5.");//
 geantMk->Do("/PYTHIA/PARP(99) 0.40");//
 geantMk->Do("/PYTHIA/PARP(100) 5");//
 geantMk->Do("/PYTHIA/PARP(102) 0.28");//
 geantMk->Do("/PYTHIA/PARP(103) 1.0");//
 geantMk->Do("/PYTHIA/PARP(104) 0.8");//
 geantMk->Do("/PYTHIA/PARP(111) 2.");//
 geantMk->Do("/PYTHIA/PARP(161) 3.00");//
 geantMk->Do("/PYTHIA/PARP(162) 24.6");//
 geantMk->Do("/PYTHIA/PARP(163) 18.8");//
 geantMk->Do("/PYTHIA/PARP(164) 11.5");//
 geantMk->Do("/PYTHIA/PARP(165) 0.47679");//
 geantMk->Do("/PYTHIA/PARP(166) 0.67597");// ! PARP165/166 are linked to MSTP17 as R_rho of HERMES is used
 // geantMk->Do("/PYTHIA/PARP(166) 0.5");
 // ----------- Now come all the switches for Jetset -----------
 geantMk->Do("/PYTHIA/PARJ(1) 0.100");//
 geantMk->Do("/PYTHIA/PARJ(2) 0.300");//
 geantMk->Do("/PYTHIA/PARJ(11) 0.5");//
 geantMk->Do("/PYTHIA/PARJ(12) 0.6");//
 geantMk->Do("/PYTHIA/PARJ(21)  0.40");//
 geantMk->Do("/PYTHIA/PARJ(32) 1.0");//
 geantMk->Do("/PYTHIA/PARJ(33)  0.80");//
 geantMk->Do("/PYTHIA/PARJ(41)  0.30");//
 geantMk->Do("/PYTHIA/PARJ(42)  0.58");//
 geantMk->Do("/PYTHIA/PARJ(45)  0.5");//
 //!----------------------------------------------------------------------
 geantMk->Do("/PYTHIA/MSTJ(1) 1");//
 geantMk->Do("/PYTHIA/MSTJ(12) 1");//
 geantMk->Do("/PYTHIA/MSTJ(45) 5");//
 geantMk->Do("/PYTHIA/MSTU(16) 2");//
 geantMk->Do("/PYTHIA/MSTU(112) 5");//
 geantMk->Do("/PYTHIA/MSTU(113) 5");//
 geantMk->Do("/PYTHIA/MSTU(114) 5");//
 //! ----------- Now all the CKINs for pythia ----------
 geantMk->Do("/PYTHIA/CKIN(1) 1.");//
 geantMk->Do("/PYTHIA/CKIN(2) -1.");//
 geantMk->Do("/PYTHIA/CKIN(3) 0.");//
 geantMk->Do("/PYTHIA/CKIN(4) -1.");//
 geantMk->Do("/PYTHIA/CKIN(5) 1.00");//
 geantMk->Do("/PYTHIA/CKIN(6) 1.00");//
 geantMk->Do("/PYTHIA/CKIN(7) -10.");//
 geantMk->Do("/PYTHIA/CKIN(8) 10.");//
 geantMk->Do("/PYTHIA/CKIN(9) -40.");//
 geantMk->Do("/PYTHIA/CKIN(10) 40.");//
 geantMk->Do("/PYTHIA/CKIN(11) -40.");//
 geantMk->Do("/PYTHIA/CKIN(12) 40.");//
 geantMk->Do("/PYTHIA/CKIN(13) -40.");//
 geantMk->Do("/PYTHIA/CKIN(14) 40.");//
 geantMk->Do("/PYTHIA/CKIN(15) -40.");//
 geantMk->Do("/PYTHIA/CKIN(16) 40.");//
 geantMk->Do("/PYTHIA/CKIN(17) -1.");//
 geantMk->Do("/PYTHIA/CKIN(18) 1.");//
 geantMk->Do("/PYTHIA/CKIN(19) -1.");//
 geantMk->Do("/PYTHIA/CKIN(20) 1.");//
 geantMk->Do("/PYTHIA/CKIN(21) 0.");//
 geantMk->Do("/PYTHIA/CKIN(22) 1.");//
 geantMk->Do("/PYTHIA/CKIN(23) 0.");//
 geantMk->Do("/PYTHIA/CKIN(24) 1.");//
 geantMk->Do("/PYTHIA/CKIN(25) -1.");//
 geantMk->Do("/PYTHIA/CKIN(26) 1.");//
 geantMk->Do("/PYTHIA/CKIN(27) -1.");//
 geantMk->Do("/PYTHIA/CKIN(28) 1.");//
 geantMk->Do("/PYTHIA/CKIN(31) 2.");//
 geantMk->Do("/PYTHIA/CKIN(32) -1.");//
 geantMk->Do("/PYTHIA/CKIN(35) 0.");//
 geantMk->Do("/PYTHIA/CKIN(36) -1");//
 geantMk->Do("/PYTHIA/CKIN(37) 0.");//
 geantMk->Do("/PYTHIA/CKIN(38) -1.");//
 geantMk->Do("/PYTHIA/CKIN(39) 4.");//
 geantMk->Do("/PYTHIA/CKIN(40) -1.");//
 geantMk->Do("/PYTHIA/CKIN(65) 1.e-09");//        ! Min for Q^2
 geantMk->Do("/PYTHIA/CKIN(66) -1.");//       ! Max for Q^2
 geantMk->Do("/PYTHIA/CKIN(67) 0.");//
 geantMk->Do("/PYTHIA/CKIN(68) -1. ");//
 geantMk->Do("/PYTHIA/CKIN(77) 2.0");//
 geantMk->Do("/PYTHIA/CKIN(78) -1.");//
  geantMk->Do("/PYTHIA/frame CMS");
  geantMk->Do("/PYTHIA/beam  p e-");

  //  Double_t sqrtS = 510;
  Double_t pE =  20; 
  Double_t pP = 100;
  geantMk->Do(Form("/PYTHIA/PBLUE 0. 0.  %f",pP));
  geantMk->Do(Form("/PYTHIA/PYELL 0. 0.  %f",pE));
  geantMk->Do("CALL PyTUNE(329)"); // set the pythia tune
  geantMk->Do("gspread   0.015 0.015 42.00");
  geantMk->Do("call pystat(1)");
  TDataSet *tableSet = new TDataSet("Pythia");
  return (TDataSet *)tableSet;
}
