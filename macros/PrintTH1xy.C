void PrintTH1xy(const TH1 *hist=0) {
  if (! hist) return;
  Int_t nx = hist->GetNbinsX();
  TArrayF X(nx);
  TArrayF Y(nx);
  Double_t total = hist->GetEntries();
  if (total <= 0) return;
  Int_t N = 0;
  for (Int_t ix = 1; ix <= nx; ix++) {
    Double_t cont = hist->GetBinContent(ix)/total;
    if (cont < 1e-5) continue;
    X[N] = hist->GetBinCenter(ix);
    Y[N] = cont;
    N++;
  }
  cout << "\t" << N << ",// " << hist->GetName() << endl << "\t{";
  for (Int_t i = 0; i < N; i++)  {
    cout << Form("%7.4f",X[i]);
    if (i != N - 1) cout << ",";
    else                cout << "}, // X" << endl << "\t{";
  }
  for (Int_t i = 0; i < N; i++)  {
    cout << Form("%7.4f",Y[i]);
    if (i != N - 1) cout << ",";
    else                cout << "}  // Y" << endl;
  }
}
