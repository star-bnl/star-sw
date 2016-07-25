void SRContents(TH2 *h = 0) {
  if (! h) return;
  Int_t nx = h->GetNbinsX();
  Int_t ny = h->GetNbinsY();
  for (Int_t s = 1; s <= ny; s++) {
    cout << "sector\t" << s;
    Int_t N = 0;
    for (Int_t r = 1; r <= nx; r++) {
      if (h->GetBinContent(r,s)) continue;
      cout << "\t" << r;
      N++;
    }
    cout << "\t Total " << N << " missing" << endl;
  }
}
