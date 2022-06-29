void PrintPars(Int_t N, Double_t *pars) {
  cout << "  Double_t pars[" << N << "] = {";
  for (Int_t i = 0; i < N; i++) {
    cout << Form("%10.5g", pars[i]);
    if (i == N - 1) cout << "};" << endl;
    else            cout << ", ";
  }
}
