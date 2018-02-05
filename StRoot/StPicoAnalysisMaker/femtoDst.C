/*
  FPE_OFF
  root.exe -q -b -x femtoDst.C
*/
void femtoDst(Int_t N = 1000000, const Char_t *input = "/net/l404/data/fisyak/Pico/2016/125/17125034/st_physics_17125034_raw_5500079.picoDst.root", const Char_t *output = "") {
#if !defined(__CINT__)
  std::cout << "This code cannot be compiled" << std::endl;
#else
  //  gSystem->SetFPEMask(kInvalid | kDivByZero | kOverflow );
  gROOT->LoadMacro("lMuDst.C");
  //  lMuDst(0,input,"ry2016,RpicoDst,mysql,PicoMaker,quiet,nodefault",output);
  lMuDst(0,input,"ry2016,RpicoDst,FemtoDst,mysql,nodefault,quiet",0,output);
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
/*
  368257423 Nov 22 22:40 /net/l404/data/fisyak/Pico/2016/125/17125034/st_physics_17125034_raw_5500079.picoDst.root
  SetChiPrimaryCut(8)   48219329 Feb  5 14:52 st_physics_17125034_raw_5500079.femtoDst.root.8 => 7.64
  SetChiPrimaryCut(10)  36044394 Feb  5 15:14 st_physics_17125034_raw_5500079.femtoDst.root.10 => 10.22
  SetChiPrimaryCut(12)  27451026 Feb  5 15:19 st_physics_17125034_raw_5500079.femtoDst.root => 13.4
 */
