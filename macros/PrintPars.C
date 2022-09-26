//________________________________________________________________________________
void PrintPars(Int_t N, Double_t *pars, const Char_t *title = "") {
  cout << "  Double_t pars[" << N << "] = {";
  for (Int_t i = 0; i < N; i++) {
    cout << Form("%10.5g", pars[i]);
    if (i == N - 1) cout << "}; //" << title << endl;
    else            cout << ", ";
  }
}
//________________________________________________________________________________
void PrintPars(TF1 *f, const Char_t *title = "") {
  if (! f) return;
  Int_t nPar = f->GetNpar();
  TArrayD pars(nPar);
  f->GetParameters(pars.GetArray());
  PrintPars(nPar, pars.GetArray(), title);
}
