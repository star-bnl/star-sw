TH1D *Diff(TH1D *h1, TH1D *h2, TH1D *h3=0, TH1D *h4=0) {
  if (! h1 && ! h2) return 0;
  TAxis *x1 = h1->GetXaxis();
  TAxis *x2 = h2->GetXaxis();
  if (h1->GetNbinsX() != h2->GetNbinsX() ||
      x1->GetXmin() != x2->GetXmin() ||
      x1->GetXmax() != x2->GetXmax()) {
    cout << "Try to subtruct histograms with different limits " << h1->GetName() << "\t" << h2->GetName() << endl;
    return 0;
  }
  if (h3) {
    TAxis *x3 = h3->GetXaxis();
    if (h1->GetNbinsX() != h3->GetNbinsX() ||
	x1->GetXmin() != x3->GetXmin() ||
	x1->GetXmax() != x3->GetXmax()) {
      cout << "Try to subtruct histograms with different limits " << h1->GetName() << "\t" << h3->GetName() << endl;
      return 0;
    }
    if (h4) {
      TAxis *x4 = h4->GetXaxis();
      if (h1->GetNbinsX() != h4->GetNbinsX() ||
	  x1->GetXmin() != x4->GetXmin() ||
	  x1->GetXmax() != x4->GetXmax()) {
	cout << "Try to subtruct histograms with different limits " << h1->GetName() << "\t" << h4->GetName() << endl;
	return 0;
      }
    }
  }
  TH1D *diff = new TH1D(*h1);
  diff->SetName(Form("Diff%s",h1->GetName()));
  Int_t nbinsx = h1->GetNbinsX();
  for (Int_t binx=0;binx<=nbinsx+1;binx++) {
    Double_t y1 = h1->GetBinContent(binx);
    Double_t y2 = h2->GetBinContent(binx);
    Double_t e1 = h1->GetBinError(binx);
    Double_t e2 = h2->GetBinError(binx);
    Double_t y  = y1-y2;
    Double_t ee = e1*e1+e2*e2;
    cout << binx 
	 << "\t" << y1 << " +/- " << e1 
	 << "\t" << y2 << " +/- " << e2
	 << "\t" << y  << " +/- " << TMath::Sqrt(ee) << endl;
    if (h3 && h4) {
      y += h3->GetBinContent(binx) - h4->GetBinContent(binx);
      ee += TMath::Power(h3->GetBinError(binx),2);
      ee += TMath::Power(h4->GetBinError(binx),2);
    }
    diff->SetBinContent(binx,y);
    diff->SetBinError(binx,TMath::Sqrt(ee));
  }
  return diff;
}
//________________________________________________________________________________
TH1D *Diff(const Char_t *name="EMKpi%%L%") {
  TH1D *h[4] = {0,0,0,0};
  const Char_t *Charge[2] = {"NP","PN"};
  const Char_t *DCA[2] = {"p","n"};
  for (Int_t i = 0; i < 2; i++) 
    for (Int_t j = 0; j < 2; j++) {
      TString Name(name);
      Name.ReplaceAll("%%",Charge[i]);
      Name.ReplaceAll("%",DCA[j]);
      h[2*i+j] = (TH1D *) gDirectory->Get(Name);
      if (h[2*i+j]) cout << "got " << h[2*i+j]->GetName() << endl;
    }
  return Diff(h[0],h[1],h[2],h[3]);
}
