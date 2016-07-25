void svtEff(TH2 *hist) {
  Int_t nx = hist->GetNbinsX();
  Int_t ny = hist->GetNbinsY();
  for (Int_t ix = 1; ix <= nx; ix++) {
    Double_t sum = 0;
    for (Int_t iy = 1; iy <= ny; iy++) {
      Int_t bin = hist->GetBin(ix,iy);
      sum += hist->GetBinContent(bin);
    }
    if (sum > 0) {
      cout << "ix " << ix-1;
      Int_t iz = 0;
      for (Int_t iy = 1; iy <= ix; iy++) {
	Int_t bin = hist->GetBin(ix,iy);
	Double_t y = hist->GetBinContent(bin)/sum;
	if (y > 1e-3) {
	  Double_t err = TMath::Sqrt(y*(1-y)/sum);
	  //	cout << Form("%8.3f",y);
	  cout << Form("\t%8.1f+/-%8.1f",100*y,100*err);
	}
#if 1
	if (iy == ix) {
	  Double_t p = ix - 1;
	  Double_t eff = y;
	  if (p > 0) eff = TMath::Power(y, 1./p);
	  cout << Form("\t===>%10.5f",eff);
	}
#endif
      }
      cout << endl;
    }
  }
}
