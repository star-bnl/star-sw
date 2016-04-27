TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_geant_Maker")) return 0;
  St_geant_Maker *geantMk = StMaker::GetChain()->GetMaker("geant");
  gSystem->Load("Pythia6_4_28s");
  gSystem->Load("bpythia");
  geantMk->Do("call bpythia");
//   gSystem->Load("apythia");
//   geantMk->Do("call apythia");
  //   ** These particles will be decayed by geant instead of pythia **
  //  geantMk->Do("/PYTHIA/MDCY (102,1)=0");//  ! PI0 111
  geantMk->Do("/PYTHIA/MDCY (106,1)=0");//  ! PI+ 211
  //  geantMk->Do("/PYTHIA/MDCY (109,1)=0");//  ! ETA 221
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
  geantMk->Do("/PYTHIA/frame CMS");
  geantMk->Do("/PYTHIA/beam  p p");
  //  Double_t sqrtS = 510;
  Double_t sqrtS = 200;
  TString SqrtS(Form("/PYTHIA/ener  %f",sqrtS));
  cout << "Set sqrtS " << SqrtS << " GeV" <<endl;
  geantMk->Do(SqrtS.Data());
  //  geantMk->Do("CALL PyTUNE(329)"); // set the pythia tune Professor pT-ordered tune w. S0 CR model  (Feb 2009)
  // https://drupal.star.bnl.gov/STAR/system/files/Run12_EmbedRequest_0.pdf
  // Perugia 2012 tune, with nominal primordial kT and PDF set. Set PARP(90)=0.213
  geantMk->Do("CALL PyTUNE(383)"); // Perugia 2012 with Innsbruck ee fragmentation parameters
  geantMk->Do("/PYTHIA/PARP(90) = 0.213"); // 
  geantMk->Do("gspread   0.015 0.015 42.00");
#ifdef __Wenu__
  //  select W --> e nu production
  geantMk->Do("/PYTHIA/ckin 3=10.0");
  geantMk->Do("/PYTHIA/ckin 4=-1.0");
  geantMk->Do("/PYTHIA/msel  12");
  //  close all decay channels
  geantMk->Do("call closeDecays(24)"); // real call 
  //   ** enable W+/- --> e+/- nu
  geantMk->Do("call openDecay(24,206,1)");
#endif
  geantMk->Do("call pystat(1)");
  TDataSet *tableSet = new TDataSet("Pythia");
  return (TDataSet *)tableSet;
 }
