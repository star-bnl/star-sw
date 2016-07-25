void runFitB4G(Int_t icase=0, Int_t ihyp=-1) {
  gSystem->Load("libTable");
  gSystem->Load("St_base");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StBichsel");
  gROOT->LoadMacro("dEdxFit.C+");
  FitB4G(icase,ihyp);
}
