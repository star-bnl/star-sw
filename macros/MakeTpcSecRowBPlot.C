void MakeTpcSecRowBPlot(const Char_t *file = "TpcSecRowB.20060519.160001.old.root") {
  gSystem->Load("libStDb_Tables.so");
  TFile *f = new TFile(file);
  if (! f) return;
  St_TpcSecRowCor *Cor = (St_TpcSecRowCor *) f->Get("TpcSecRowB");
  if (! Cor) return;
  TpcSecRowCor_st *cor = Cor->GetTable();
  Int_t N = Cor->GetNRows();
  TH2D *SecRow = new TH2D("SecRow","Sector/Row Correction",45,0,45,N,0,N);
  SecRow->SetXTitle("Row");
  SecRow->SetYTitle("Sector");
  for (Int_t i = 0; i < N; i++, cor++) {
    for (Int_t j = 0; j < 45; j++)
      SecRow->Fill(j+0.5,i+0.5,cor->GainScale[j]);
  }
}
