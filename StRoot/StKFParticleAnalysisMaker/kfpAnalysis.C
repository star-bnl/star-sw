/*
  FPE_OFF
  root.exe -q -b -x kfpAnalysis.C
*/
void kfpAnalysis(Int_t N = 1000000, const Char_t *input = "/net/l404/data/fisyak/Pico/2016/125/17125034/st_physics_17125034_raw_5500079.picoDst.root", const Char_t *output = "picoAna.root") {
#if !defined(__CINT__)
  std::cout << "This code cannot be compiled" << std::endl;
#else
  //  gSystem->SetFPEMask(kInvalid | kDivByZero | kOverflow );
  gROOT->LoadMacro("lMuDst.C");
  //  lMuDst(0,input,"ry2016,RpicoDst,mysql,PicoAnalysis,quiet,nodefault",output);
  lMuDst(0,input,"ry2016,RpicoDst,kfpAna,mysql,nodefault,quiet",output);
  maker = (StPicoDstMaker *) StMaker::GetTopChain()->Maker("PicoDst");
  if (! maker) return;
  maker->SetStatus("*",1);
  TChain *tree = maker->chain();
  Long64_t nentries = tree->GetEntries();
  if (nentries <= 0) return;
  Long64_t nevent = N;
  nevent = TMath::Min(nevent,nentries);
  cout << nentries << " events in chain " << nevent << " will be read." << endl;
  ((StBFChain *) StMaker::GetTopChain())->EventLoop(nevent);
#endif
  
}
