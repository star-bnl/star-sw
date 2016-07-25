void h1prnt(TH1 *hist = 0) {
  if (! hist) return;
  Int_t nx = hist->GetNbinsX();
  for (Int_t i = 1; i <= nx; i++) {
    cout << Form("%8.4f,",hist->GetBinContent(i));
  }
  cout << endl;
}
