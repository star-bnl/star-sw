TH1D *Shift(TH1D *hist, Double_t shiftX) {
  if (! hist) return 0;
  TH1D *shift = new TH1D(*hist);
  TString name(shift->GetName());
  name += "_shift";
  shift->SetName(name.Data());
  shift->Reset();
  Int_t nx = hist->GetNbinsX();
  for (int i = 1; i <=nx; i++) 
    shift->Fill(hist->GetBinCenter(i)+shiftX, hist->GetBinContent(i));
  return shift;
}
