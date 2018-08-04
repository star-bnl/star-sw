void CleanH1(TH1* mu = 0) {
  if (! mu) return;
  Int_t nx = mu->GetXaxis()->GetNbins();
  for (Int_t ix = 1; ix < nx; ix++) {
    if (mu->GetBinError(ix) > 0) {
      if (TMath::Abs(mu->GetBinContent(ix)) < 2 * mu->GetBinError(ix)) {
	mu->SetBinContent(ix,0);
	mu->SetBinError(ix,0);
      }
    }
  }
}
