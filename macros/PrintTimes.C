void PrintTimes(TH1D* mu, Double_t cut=-0.04, Int_t tZero = 19950101) {
  TDatime t0(tZero,0);
  Int_t timeOffSet = t0.Convert();
  Int_t nx = mu->GetNbinsX();
  TDatime d;
  for(Int_t i=1; i<=nx; i++) {
    if (mu->GetBinContent(i) < cut) {
      d.Set(timeOffSet+mu->GetBinCenter(i)); 
      d.Print();
    }
  }
}
