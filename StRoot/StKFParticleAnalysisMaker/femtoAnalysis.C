/*
  FPE_OFF
  root.exe -q -b -x femtoAnalysis.C
*/
//void femtoAnalysis(Int_t N = 1000000, const Char_t *input = "./*.femtoDst.root", const Char_t *output = "femto.root") {
void femtoAnalysis(Int_t N = 1000000, const Char_t *input = "/gpfs01/star/pwg/fisyak/Femto/2014/AuAu_200_production_mid_2014/132/15132007/15132007_0.femtoDst.root", const Char_t *output = "femto.root") {
#if !defined(__CINT__)
  std::cout << "This code cannot be compiled" << std::endl;
#else
  //  gSystem->SetFPEMask(kInvalid | kDivByZero | kOverflow );
  gROOT->LoadMacro("lMuDst.C");
  lMuDst(0,input,"RpicoDst,mysql,kfpAna,nodefault",output);
  //  lMuDst(0,input,"RpicoDst,mysql,kfpAna,quiet,nodefault",output);
  
  StKFParticleInterface::instance()->SetChiPrimaryCut(10);
//   StKFParticleInterface::instance()->SetPtCutCharm(0.7);
//   StKFParticleInterface::instance()->SetChi2TopoCutCharmManybodyDecays(2);
  StKFParticleInterface::instance()->SetSoftKaonPIDMode();
  StKFParticleInterface::instance()->SetSoftTofPidMode();
  
  StKFParticleInterface::instance()->AddDecayToReconstructionList(  421);
  StKFParticleInterface::instance()->AddDecayToReconstructionList( -421);
  StKFParticleInterface::instance()->AddDecayToReconstructionList(  426);
  StKFParticleInterface::instance()->AddDecayToReconstructionList(  429);
  StKFParticleInterface::instance()->AddDecayToReconstructionList( -429);
  StKFParticleInterface::instance()->AddDecayToReconstructionList(  411);
  StKFParticleInterface::instance()->AddDecayToReconstructionList( -411);
  StKFParticleInterface::instance()->AddDecayToReconstructionList(  431);
  StKFParticleInterface::instance()->AddDecayToReconstructionList( -431);
  StKFParticleInterface::instance()->AddDecayToReconstructionList( 4122);
  StKFParticleInterface::instance()->AddDecayToReconstructionList(-4122);
  
  StPicoDstMaker* maker = (StPicoDstMaker *) StMaker::GetTopChain()->Maker("PicoDst");
  if (! maker) return;
  maker->SetStatus("*",1);
  TChain *tree = maker->chain();
  Long64_t nentries = tree->GetEntries();
  if (nentries <= 0) return;
  Long64_t nevent = N;
  nevent = TMath::Min(nevent,nentries);
  cout << nentries << " events in chain " << nevent << " will be read." << endl;
  new StGoodTrigger("y2014");
  maker->SetAttr(".Privilege",1);
  chain->EventLoop(nevent);
#endif
  
}
