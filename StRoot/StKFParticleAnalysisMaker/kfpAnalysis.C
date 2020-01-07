/*
  FPE_OFF
  root.exe -q -b -x kfpAnalysis.C
*/
class StGoodTrigger;
#if 1
//void kfpAnalysis(Int_t N = 1000000, const Char_t *input = "/net/l404/data/fisyak/Pico/2016/125/17125034/st_physics_17125034_raw_5500079.picoDst.root", const Char_t *output = "picoAna.root", const Char_t *triggerSet = "y2016") {
//void kfpAnalysis(Int_t N = 1000000, const Char_t *input = "st_physics_adc_17125034_raw_1000007.femtoDst.root", const Char_t *output = "picoAna.root", const Char_t *triggerSet = "y2016") {
//void kfpAnalysis(Int_t N = 1000000, const Char_t *input = "/star/data01/pwg_tasks/picoDs/*picoDst.root", const Char_t *output = "picoAna.root", const Char_t *triggerSet = "y2011") {
//void kfpAnalysis(Int_t N = 1000000, const Char_t *input = "./st_mtd_19110005_raw_3500041.picoDst.root", const Char_t *output = "picoAna.root", const Char_t *triggerSet = "y2018") {
//void kfpAnalysis(Int_t N = 100000, const Char_t *input = "/gpfs01/star/pwg_tasks/tfg02/2010/11GeV/*picoDst.root", const Char_t *output = "picoAna2011AuAu11.root", const Char_t *triggerSet = "y2011") {
//void kfpAnalysis(Int_t N = 10000000, const Char_t *input = "/net/l401/data/scratch1/reco/2020/TFG19m/RF/11p5GeV/*picoDst.root", const Char_t *output = "picoAna2020AuAu11p5GeV.root", const Char_t *triggerSet = "y2020") {
//void kfpAnalysis(Int_t N = 1000, const Char_t *input = "/gpfs01/star/pwg_tasks/tfg02/2010/11GeV/st_physics_11148001_raw_1010001.picoDst.root", const Char_t *output = "picoAna2011AuAu11.root", const Char_t *triggerSet = "y2011") {
//void kfpAnalysis(Int_t N = 1000, const Char_t *input = "/gpfs01/star/pwg/fisyak/Pico/2010AuAu11/11148001.picoDst.root", const Char_t *output = "picoAna2011AuAu11.root", const Char_t *triggerSet = "y2011") {
void kfpAnalysis(Int_t N = 10000000, const Char_t *input = "/net/l401/data/scratch1/reco/2020/TFG19m/RF/11p5GeV.B/347/20347034/hlt_20347034_13_02_000.picoDst.root", const Char_t *output = "Ana2020AuAu11p5GeV.root", const Char_t *triggerSet = "y2020") {
#else
void kfpAnalysis(Int_t N = 1000, 
		 //		 const Char_t *input = "/star/rcf/test/dev/daq_sl302.stica/Wed/year_2014/AuAu200_production_low_2014/st_physics_15164004_raw_2000022.MuDst.root",
		 //		 const Char_t *input = "/star/data75/reco/27GeV_production_2018/FullField/P18ih/2018/158/19158009/st_physics_19158009_raw_3000013.MuDst.root",
		 //		 const Char_t *input = "root://xrdstar.rcf.bnl.gov:1095//home/starlib/home/starreco/reco/AuAu27_production_2011/FullField/P11id/2011/172/12172024:st_physics_12172024_raw_4010001.MuDst.root",
		 const Char_t *output = "picoAna2011AuAu27.root", const Char_t *triggerSet = "y2020") {
#endif
#if !defined(__CINT__)
  std::cout << "This code cannot be compiled" << std::endl;
#else
  //  gSystem->SetFPEMask(kInvalid | kDivByZero | kOverflow );
  Bool_t isPico = kTRUE;
  if (TString(input).Contains("MuDst")) isPico = kFALSE;
  gROOT->LoadMacro("lMuDst.C");
  TString Chain("r");
  Chain += triggerSet;
  if (! isPico) Chain += ",RMuDst";
  else          Chain += ",RpicoDst";
  Chain += ",kfpAna,mysql,nodefault,quiet";
  //  lMuDst(0,input,"ry2016,RpicoDst,mysql,PicoAnalysis,quiet,nodefault",output);
  lMuDst(-1,input,Chain,output);
//________________________________________________________________________________
// from  /gpfs01/star/pwg/mzyzak/2019_2_2/Template/femtoAnalysis.C
//   StKFParticleInterface::instance()->SetChiPrimaryCut(10);
//   StKFParticleInterface::instance()->SetPtCutCharm(0.7);
//   StKFParticleInterface::instance()->SetChi2TopoCutCharmManybodyDecays(2);
//   StKFParticleInterface::instance()->SetSoftKaonPIDMode();
//   StKFParticleInterface::instance()->SetSoftTofPidMode();
  std::cout << "KFParticleAnalysis: running analysis for triggerSet " << triggerSet << "." << std::endl; 
  StKFParticleAnalysisMaker* kfpAnalysis = (StKFParticleAnalysisMaker*) StMaker::GetTopChain()->Maker("KFParticleAnalysis");
  kfpAnalysis->AnalyseDsPhiPi();
  kfpAnalysis->CollectPIDHistograms();
  kfpAnalysis->CollectTrackHistograms();
  
  kfpAnalysis->AddDecayToReconstructionList( 310);  // K0
  kfpAnalysis->AddDecayToReconstructionList( 3122); // Lambda
  kfpAnalysis->AddDecayToReconstructionList(-3122); // Lambda_bar
  kfpAnalysis->AddDecayToReconstructionList( 3312); // Xi-
  kfpAnalysis->AddDecayToReconstructionList(-3312); // Xi+
  kfpAnalysis->AddDecayToReconstructionList( 3334); // Omega-
  kfpAnalysis->AddDecayToReconstructionList(-3334); // Omega+
//   
//   kfpAnalysis->AddDecayToReconstructionList( 113);  // rho
  kfpAnalysis->AddDecayToReconstructionList( 333);  // phi
//   kfpAnalysis->AddDecayToReconstructionList( 313);  // K*0
//   kfpAnalysis->AddDecayToReconstructionList(-313);  // K*0_bar
//   kfpAnalysis->AddDecayToReconstructionList( 323);  // K*+
//   kfpAnalysis->AddDecayToReconstructionList(-323);  // K*-
  
//   kfpAnalysis->AddDecayToReconstructionList( 3124); // Lambda*
//   kfpAnalysis->AddDecayToReconstructionList( 3124); // Lambda*
//   kfpAnalysis->AddDecayToReconstructionList( 3224); // Sigma*+
//   kfpAnalysis->AddDecayToReconstructionList( 3114); // Sigma*-
//   kfpAnalysis->AddDecayToReconstructionList( 3324); // Xi*0
//   kfpAnalysis->AddDecayToReconstructionList(-3324); // Xi*0
  
  kfpAnalysis->AddDecayToReconstructionList( 22);   // gamma
//   kfpAnalysis->AddDecayToReconstructionList( 111);  // pi0
//   kfpAnalysis->AddDecayToReconstructionList( 3212);  // Sigma0
//   
//   kfpAnalysis->AddDecayToReconstructionList( 3003); // Ln
//   kfpAnalysis->AddDecayToReconstructionList( 3103); // Lnn
  
//   kfpAnalysis->AddDecayToReconstructionList( 3004); // H3L
//   kfpAnalysis->AddDecayToReconstructionList( 3005); // H4L
  
//   kfpAnalysis->AddDecayToReconstructionList( 3006); // He4L

//   kfpAnalysis->AddDecayToReconstructionList( 3007); // He5L
//   kfpAnalysis->AddDecayToReconstructionList( 3203); // LLn
//   kfpAnalysis->AddDecayToReconstructionList( 3008); // H4LL
//   kfpAnalysis->AddDecayToReconstructionList( 3009); // H4LL
  
//   kfpAnalysis->AddDecayToReconstructionList( 3000); // LL
//   kfpAnalysis->AddDecayToReconstructionList( 3001); // Lppi
  
  
//   kfpAnalysis->AddDecayToReconstructionList( 9001); // pi+pi+
//   kfpAnalysis->AddDecayToReconstructionList(-9001); // pi-pi-
//   kfpAnalysis->AddDecayToReconstructionList( 9002); // pi+K+
//   kfpAnalysis->AddDecayToReconstructionList(-9002); // pi-K-
//   kfpAnalysis->AddDecayToReconstructionList( 9003); // K+K+
//   kfpAnalysis->AddDecayToReconstructionList(-9003); // K-K-
//   kfpAnalysis->AddDecayToReconstructionList( 9004); // pK+
//   kfpAnalysis->AddDecayToReconstructionList(-9004); // p-K-
  
  
  chain->Init();

  if(isPico)
  {
    StKFParticleInterface::instance()->CleanLowPVTrackEvents();
//     StKFParticleInterface::instance()->UseHFTTracksOnly();
  }
  
//   StKFParticleInterface::instance()->SetSoftKaonPIDMode();
  StKFParticleInterface::instance()->SetSoftTofPidMode();

//   StKFParticleInterface::instance()->SetChiPrimaryMaxCut(5.e3);
  StKFParticleInterface::instance()->SetChiPrimaryCut(12);
  
  StKFParticleInterface::instance()->SetMaxDistanceBetweenParticlesCut(1);
  StKFParticleInterface::instance()->SetLCut(0.f);
  
  StKFParticleInterface::instance()->SetChiPrimaryCut2D(8);
  StKFParticleInterface::instance()->SetChi2Cut2D(3);
  StKFParticleInterface::instance()->SetLdLCut2D(5);
  
  StKFParticleInterface::instance()->SetChi2CutXiOmega(3);
  StKFParticleInterface::instance()->SetChi2TopoCutXiOmega(3);
  StKFParticleInterface::instance()->SetLdLCutXiOmega(5);  

  
//   StKFParticleInterface::instance()->SetSecondaryCuts(3, 3, 5);
//________________________________________________________________________________

//  StKFParticleInterface::instance()->SetdEdXType(2); // dNdx

  TTree *tree = 0;
  if (! isPico) {
    StKFParticleAnalysisMaker *ana = ( StKFParticleAnalysisMaker *) chain->Maker("KFParticleAnalysis");
    if (! ana) return;
    ana->AnalyseMuDst();
    StMuDstMaker * MuMk = (StMuDstMaker *) StMaker::GetTopChain()->Maker("MuDst");
    if (! MuMk) return;
    MuMk->SetStatus("*",1);
    tree = MuMk->chain();
  } else {
    StPicoDstMaker * picoMk = (StPicoDstMaker *) StMaker::GetTopChain()->Maker("PicoDst");
    if (! picoMk) return;
    picoMk->SetStatus("*",1);
    tree = picoMk->chain();
  }
  if (! tree ) {
    cout << "No MuDst/PicoDst tree. Exit." << endl;
  }
  Long64_t nentries = tree->GetEntries();
  cout << "no. events in tree " <<nentries << endl;
  if (nentries <= 0) return;
  Long64_t nevent = N;
  nevent = TMath::Min(nevent,nentries);
  cout << nentries << " events in chain " << nevent << " will be read." << endl;
  //  new StGoodTrigger(triggerSet);
  //  chain->SetAttr(".Privilege",1,"StPicoDstMaker::*");
  chain->EventLoop(nevent);
#endif
  
}
