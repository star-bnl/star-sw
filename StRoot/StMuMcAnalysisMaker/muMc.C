/*
  root.exe -q -b -x 'muMc.C(1e6,"../*MuDst.root")'
  root.exe 'muMc.C(-1,"")'
  StMuMcAnalysisMaker::instance()->DrawAll();
*/
void muMc(Int_t N = 1000000, const Char_t *input = "./*MuDst.root", const Char_t *output = "muMc.root") {
#if !defined(__CINT__)
  std::cout << "This code cannot be compiled" << std::endl;
#else
  //  gSystem->SetFPEMask(kInvalid | kDivByZero | kOverflow );
  gROOT->LoadMacro("lMuDst.C");
  Char_t *file =  gSystem->Which("./",output,kReadPermission);
  if (! file) {
    lMuDst(N,input,"RMuDst,mysql,MuMc,quiet,nodefault",output);
  } else {
    lMuDst(-1,"","RMuDst,mysql,MuMc,nodefault");
    TFile *f = new TFile(file);
    //    StBFChain *chain = StMaker::GetTopChain();
    chain->SetTFile(f);
    chain->Init();
    StMuMcAnalysisMaker::instance()->DrawAll();
  }
  delete [] file;
#endif
  
}
