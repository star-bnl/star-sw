void TH2Print(TH2 *hist) {
  if (! hist) return;
  Int_t nx = Correlations->GetNbinsX();
  Int_t ny = Correlations->GetNbinsY();
  const Char_t *vars[6] = {"u","v","w","alpha","beta","gamma"};
  cout << "===================================================" << endl;
  for (int i  = 1; i <= nx; i++) 
    for (int j = 1; j <= i &&j <= ny; j++) {
      Double_t cor = Correlations->GetCellContent(i,j); 
      if (TMath::Abs(cor) > 0.95) cout << vars[i%6] << "/" << i/6 << "\t" << vars[j%6] << "/" << j/6 << "\t" << cor << endl;
    }
}
