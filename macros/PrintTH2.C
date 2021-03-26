void PrintTH2(const TH2 *hist=0) {
  if (! hist) return;
  Int_t nx = hist->GetNbinsX();
  Int_t ny = hist->GetNbinsY();
  cout << "  Double_t corrRS[" << ny << "][" << nx << "] = {" << endl;
  for (Int_t iy = 1; iy <= ny; iy++) {
    cout << "    {";
    for (Int_t ix = 1; ix <= nx; ix++) {
      Int_t bin = hist->GetBin(ix,iy,0);
      Double_t cont = hist->GetBinContent(bin);
      cout << Form("%6.2f",cont);
      if (ix != nx) cout << ",";
    }
    if (iy != ny) cout << "}, // " << iy << endl;
    else          cout << "}  // " << iy << endl;
  }
  cout << "  };" << endl;
}
