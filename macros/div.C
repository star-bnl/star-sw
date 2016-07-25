void div(TH1 *pmom){
  Int_t nx = pmom->GetNbinsX();
  for (int i=1;i<=nx;i++) {
    double val = pmom->GetBinContent(i)/pmom->GetBinWidth(i);
    pmom->SetBinContent(i,val);
  }
}
