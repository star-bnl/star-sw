void NormX(TH2 *h2 = 0) {
  if (! h2 ) return;
  Int_t nx = h2->GetNbinsX();
  Int_t ny = h2->GetNbinsY();
  Double_t sumX = 0;
  for (Int_t i = 1; i <= nx; i++) {
    sumX += h2->GetBinContent(i,1);
    Double_t sumY = 0;
    for (Int_t j = 1; j <= ny; j++) {
      sumY += h2->GetBinContent(i,j);
    }
    for (Int_t j = 2; j <= ny; j++) {
      Double_t v = h2->GetBinContent(i,j);
      v /= sumY;
      h2->SetBinContent(i,j,v);
    }
  }
  for (Int_t i = 1; i <= nx; i++) {
    Int_t j = 1;
      Double_t v = h2->GetBinContent(i,j);
      v /= sumX;
      h2->SetBinContent(i,j,v);
  } 
}
