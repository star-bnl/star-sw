void fitTpcT(Char_t *opt = "Q") {
  TFile *f = (TFile *) gDirectory;
  TString Name(f->GetName());
  if (! Name.Contains(".root")) return;
  f->Close();
  TFile *f = new TFile(Name.Data(),"update");
  gROOT->LoadMacro("TpcT.C+");
  TpcT t;
  Char_t *histos[4] = {"InnerPadRc","OuterPadRc","InnerTimeRc","OuterTimeRc"};
  Int_t k1 = 0;
  Int_t k2 = 4;
  TString Opt(opt);
  if (Opt.Contains("Cross",TString::kIgnoreCase) ||
      Opt.Contains("K3",TString::kIgnoreCase))     k2 = 2;
  if (Opt.Contains("pShape",TString::kIgnoreCase) ||
      Opt.Contains("Tau",TString::kIgnoreCase))    k1 = 2;
  for (Int_t k = k1; k < k2; k++) 
    t.FitSlices(histos[k],Opt.Data());
  f->Write();
  f->Close();
}
