void SetSectorLabel(TAxis *x = 0) {
  if (! x) return;
  Int_t n = x->GetNbins();
  for (Int_t i = 1; i <= n; i+=6) {
    Double_t phi = x->GetBinCenter(i);
    Double_t Phi = phi;
    if (Phi < 0) Phi += 360;
    Int_t Sector = ( ( 30 - (int)(Phi/15.) )%24 ) / 2 ;
    if (phi < 0) Sector = 24 - Sector;
    if (Sector == 0) Sector = 12;
    TString S(Form("%2i",Sector));
    cout << "phi = " << phi << "\t" << S.Data() << endl;
    x->SetBinLabel(i, S.Data());
  }
}
