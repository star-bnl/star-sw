void Print(TProfile *binx, Double_t mean=0.) {
  Int_t n = binx->GetNbinsX();
  for (int i = 0; i<n+1; i++) 
    if (binx->GetBinContent(i) != 0) 
      printf("{%f,%f,%f},\n", 
	     binx->GetBinLowEdge(i), 
	     TMath::Exp(-(binx->GetBinContent(i)-mean)), 
	     binx->GetBinError(i));
}
