void PrintTH2(const TH2 *hist=0) {
  if (! hist) return;
  Int_t nx = hist->GetNbinsX();
  Int_t ny = hist->GetNbinsY();
  for (Int_t ix = 1; ix <= nx; ix++) {
    for (Int_t iy = 1; iy <= ix; iy++) {
      Int_t bin = hist->GetBin(ix,iy,0);
      Double_t cont = hist->GetBinContent(bin);
      if (TMath::Abs(cont) > 0.999) {
	cout << cont << "\t" 
	     << (ix-1) << "/" << (iy-1) << "\t" << (ix-1)/6 << "_" << (ix-1)%6 << "/"  
	     << (iy-1)/6 << "_" << (iy-1)%6 << endl;
      }
    }
  }
}
