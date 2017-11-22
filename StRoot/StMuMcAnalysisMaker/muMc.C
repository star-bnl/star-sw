/*
  root.exe -q -b -x 'muMc.C(1e6,"../*MuDst.root")'
*/
void muMc(Int_t N = 1000000, const Char_t *input = "*.MuDst.root", const Char_t *output = "muMc.root") {
#if !defined(__CINT__)
  std::cout << "This code cannot be compiled" << std::endl;
#else
  //  gSystem->SetFPEMask(kInvalid | kDivByZero | kOverflow );
  gROOT->LoadMacro("lMuDst.C");
  Char_t *file = 0; // gSystem->Which("./",output,kReadPermission);
  if (! file) {
    lMuDst(-1,input,"RMuDst,MuMc,quiet,mysql,nodefault",output);
    StMuMcAnalysisMaker *muMc = chain->Maker("MuMcAnalysis");
    if (muMc) {cout << "MuMcAnalysis has been found" << endl;}
    muMc->SetAttr("TrackPlots",1);
    //    muMc->SetAttr("VertexPlots",1);
    //    muMc->SetAttr("StoreCutNTuples",1);
    chain->Init();
    chain->EventLoop(N);
  } else {
    lMuDst(-1,"","RMuDst,MuMc,nodefault");
    TFile *f = new TFile(file);
    //    StBFChain *chain = StMaker::GetTopChain();
    chain->SetTFile(f);
    chain->Init();
  }
  delete [] file;
#endif
  
}
