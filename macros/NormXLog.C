/*
   TH2F *neP = new TH2F(*nePI);
   neP->SetName("neP");
   neP->Add(nePO);
   .x NormXLog.C(neP)
 */
void NormXLog(TH2F *h2 = 0) {
  if (! h2 ) return;
  TH2F *norm = new TH2F(*h2);
  norm->SetName(Form("%s_norm",h2->GetName()));
  norm->Reset();
  Int_t nx = h2->GetNbinsX();
  Int_t ny = h2->GetNbinsY();
  Double_t sumX = 0;
  for (Int_t i = 1; i <= nx; i++) {
    sumX += h2->GetBinContent(i,1);
    Double_t sumY = 0;
    for (Int_t j = 1; j <= ny; j++) {
      sumY += h2->GetBinContent(i,j);
    }
    if (sumY > 0) {
      for (Int_t j = 1; j <= ny; j++) {
	Double_t v = h2->GetBinContent(i,j);
	if (v > 0) {
	  Double_t r = v/sumY;
	  Double_t vL = TMath::Log(r);
	  Double_t e  = TMath::Sqrt(r*(1-r))/v;
	  norm->SetBinContent(i,j,vL);
	  norm->SetBinError(i,j,e/v);
	}
      }
    }
  }
}
