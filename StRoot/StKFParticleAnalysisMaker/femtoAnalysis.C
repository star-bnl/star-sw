/*
  FPE_OFF
  root.exe -q -b -x femtoAnalysis.C
*/
void femtoAnalysis(Int_t N = 1000000, 
		   const Char_t *input = "/gpfs01/star/pwg_tasks/tfg02/2021/RF/TFG21c.B/7p7GeV_2021/031/22031042/hlt_22031042_10_01_000.picoDst.root",
		   const Char_t *output = "femto.root",
                   int year = 2016,
                   TString flowFileList = "",
                   bool isPico = true) // list of files with centrality and reaction plane, files should be separated with ";"
{
#if !defined(__CINT__)
  std::cout << "This code cannot be compiled" << std::endl;
#else
  //  gSystem->SetFPEMask(kInvalid | kDivByZero | kOverflow );
  gROOT->LoadMacro("lMuDst.C");
  lMuDst(-1,input,"RpicoDst,mysql,kfpAna,quiet,nodefault",output);
  
  
  std::cout << "KFParticleAnalysis: running analysis for the year " << year << "." << std::endl; 
  StKFParticleAnalysisMaker* kfpAnalysis = (StKFParticleAnalysisMaker*) StMaker::GetTopChain()->Maker("KFParticleAnalysis");
  kfpAnalysis->AnalyseDsPhiPi();
//   kfpAnalysis->CollectPIDHistograms();
//   kfpAnalysis->CollectTrackHistograms();
  
  kfpAnalysis->AddDecayToReconstructionList( 310);
  kfpAnalysis->AddDecayToReconstructionList( 100321);
  kfpAnalysis->AddDecayToReconstructionList(-100321);
  kfpAnalysis->AddDecayToReconstructionList( 200321);
  kfpAnalysis->AddDecayToReconstructionList(-200321);
  kfpAnalysis->AddDecayToReconstructionList( 3122);
  kfpAnalysis->AddDecayToReconstructionList(-3122);
  kfpAnalysis->AddDecayToReconstructionList( 3312);
  kfpAnalysis->AddDecayToReconstructionList(-3312);
  kfpAnalysis->AddDecayToReconstructionList( 3334);
  kfpAnalysis->AddDecayToReconstructionList(-3334);
  
  kfpAnalysis->AddDecayToReconstructionList(   22);
  kfpAnalysis->AddDecayToReconstructionList(  111);
  kfpAnalysis->AddDecayToReconstructionList(  333);
//   kfpAnalysis->AddDecayToReconstructionList(  313);
//   kfpAnalysis->AddDecayToReconstructionList( -313);
//   kfpAnalysis->AddDecayToReconstructionList(  323);
//   kfpAnalysis->AddDecayToReconstructionList( -323);
  kfpAnalysis->AddDecayToReconstructionList( 3324);
  kfpAnalysis->AddDecayToReconstructionList(-3324);
  
  kfpAnalysis->AddDecayToReconstructionList( 3000);
  kfpAnalysis->AddDecayToReconstructionList( 3001);
  kfpAnalysis->AddDecayToReconstructionList( 3003);
  kfpAnalysis->AddDecayToReconstructionList( 3103);
  kfpAnalysis->AddDecayToReconstructionList( 3004);
  kfpAnalysis->AddDecayToReconstructionList( 3005);
  kfpAnalysis->AddDecayToReconstructionList( 3006);
  kfpAnalysis->AddDecayToReconstructionList( 3007);
  kfpAnalysis->AddDecayToReconstructionList( 3012);
  kfpAnalysis->AddDecayToReconstructionList( 3013);

  kfpAnalysis->AddDecayToReconstructionList( 1003004);
  kfpAnalysis->AddDecayToReconstructionList( 1003005);
  kfpAnalysis->AddDecayToReconstructionList( 1003006);
  kfpAnalysis->AddDecayToReconstructionList( 1003007);
  
  chain->Init();

  if(isPico)
    {
      StKFParticleInterface::instance()->CleanLowPVTrackEvents();
//    StKFParticleInterface::instance()->UseHFTTracksOnly();
    }
  
  StKFParticleInterface::instance()->SetSoftKaonPIDMode();
  StKFParticleInterface::instance()->SetSoftTofPidMode();
  

//   StKFParticleInterface::instance()->SetChiPrimaryMaxCut(5.e3);
  StKFParticleInterface::instance()->SetChiPrimaryCut(18.f);
  StKFParticleInterface::instance()->SetChiPrimaryCutFragments(8.f);
  
  StKFParticleInterface::instance()->SetMaxDistanceBetweenParticlesCut(1);
//   StKFParticleInterface::instance()->SetLCut(0.3f);
  StKFParticleInterface::instance()->SetLCut(0.f);
  
  StKFParticleInterface::instance()->SetChiPrimaryCut2D(0);
  StKFParticleInterface::instance()->SetChi2Cut2D(3);
  StKFParticleInterface::instance()->SetLdLCut2D(5);
  
  StKFParticleInterface::instance()->SetChi2CutXiOmega(3);
  StKFParticleInterface::instance()->SetChi2TopoCutXiOmega(3);
  StKFParticleInterface::instance()->SetLdLCutXiOmega(5);  
  
  StKFParticleInterface::instance()->SetChi2CutCharmManybodyDecays(3);
  StKFParticleInterface::instance()->SetChi2TopoCutCharmManybodyDecays(3);
  StKFParticleInterface::instance()->SetLdLCutCharmManybodyDecays(5);
  
  StPicoDstMaker* maker = (StPicoDstMaker *) StMaker::GetTopChain()->Maker("PicoDst");
  if (! maker) return;
  maker->SetStatus("*",1);
  TChain *tree = maker->chain();
  Long64_t nentries = tree->GetEntries();
  if (nentries <= 0) return;
  Long64_t nevent = N;
  nevent = TMath::Min(nevent,nentries);
  cout << nentries << " events in chain " << nevent << " will be read." << endl;
  TString Y("y"); Y += year;
  new StGoodTrigger(Y);
  chain->SetAttr(".Privilege",1,"StPicoDstMaker::*")
  chain->EventLoop(nevent);
#endif
  
}
