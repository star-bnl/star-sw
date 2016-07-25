void GetErrors(TProfile *prof){// get errors from profile histogram
  if (!prof) return;
  TString name(prof->GetName());
  name += "_err";
  Int_t n = prof->GetNbinsX();
  TH1D *err = new TH1D(name.Data(),prof->GetTitle(),
		       n,
		       prof->GetXaxis()->GetXmin(),
		       prof->GetXaxis()->GetXmax());
  for (int i=1; i<=n; i++) {
    err->Fill(prof->GetBinCenter(i), prof->GetBinError(i));
  }
}
