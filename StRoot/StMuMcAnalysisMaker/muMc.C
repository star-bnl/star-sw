/*
  FPE_OFF
  root.exe -q -b -x 'muMc.C(1e6,"../*MuDst.root")'
*/
void muMc(Int_t N = 1000000, const Char_t *input = "*.MuDst.root", const Char_t *output = "muMc.root") {
#if !defined(__CINT__)
  std::cout << "This code cannot be compiled" << std::endl;
#else
  //  gSystem->SetFPEMask(kInvalid | kDivByZero | kOverflow );
  gROOT->LoadMacro("lMuDst.C");
  //  Char_t *file = gSystem->Which("./",output,kReadPermission);
  Char_t *file = 0;
  if (! file) {
    lMuDst(-1,input,"RMuDst,KFPInter,MuMc,quiet,mysql,nodefault",output);
    StMuMcAnalysisMaker *muMc = StMuMcAnalysisMaker::instance();
    if (muMc) {cout << "MuMcAnalysis has been found" << endl;}
    muMc->SetAttr("TrackPlots",1);
    //    muMc->SetAttr("VertexPlots",1);
    //    muMc->SetAttr("StoreCutNTuples",1);
    chain->Init();
    chain->EventLoop(N);
    TFile *f = chain->GetTFile();
    if (f) f->Write();
  } else {
    lMuDst(-1,"","RMuDst,MuMc,mysql,nodefault");
    TFile *f = new TFile(output);
    if (! f) return;
    //    StBFChain *chain = StMaker::GetTopChain();
    chain->SetTFile(f);
    StMuMcAnalysisMaker *muMc = StMuMcAnalysisMaker::instance();
    muMc->SetAttr("TrackPlots",1);
    chain->Init();
  }
  delete [] file;
  StMuMcAnalysisMaker::instance()->Draw();
#endif
  
}
