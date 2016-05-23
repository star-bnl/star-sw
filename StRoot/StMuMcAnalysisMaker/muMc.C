/*
  root.exe -q -b -x 'muMc.C(1e6,"../*MuDst.root")'
*/
void muMc(Int_t N = 1000000, const Char_t *input = "/star/subsys/tpc/fisyak/reco/2014/V0B/*MuDst.root", const Char_t *output = "muMc.root") {
#if !defined(__CINT__)
  std::cout << "This code cannot be compiled" << std::endl;
#else
  //  gSystem->SetFPEMask(kInvalid | kDivByZero | kOverflow );
  gROOT->LoadMacro("lMuDst.C");
  Char_t *file = gSystem->Which("./",output,kReadPermission);
  if (! file) {
    lMuDst(N,input,"MuMc,nodefault",output);
  } else {
    lMuDst(-1,"","MuMc,nodefault");
    TFile *f = new TFile(file);
    //    StBFChain *chain = StMaker::GetTopChain();
    chain->SetTFile(f);
    chain->Init();
  }
  delete [] file;
#endif
  
}
