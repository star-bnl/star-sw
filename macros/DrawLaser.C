TCanvas *c1 = 0;
TFile *RFF = 0;
TFile *RHF = 0;
TFile *ZF = 0;
void DrawLaser(Int_t sec = 2, Int_t row = 32) {
  if (! c1 ) c1 = new TCanvas();
  else       c1->Clear();
  c1->Divide(3,1);
  if (! RFF) RFF = TFile::Open("RFF.Fit6.root");
  if (! RHF) RHF = TFile::Open("RHF.Fit6.root");
  if (! ZF)  ZF = TFile::Open("ZF.Fit6.root");
  TFile *files[3] = {RFF, RHF, ZF};
  for (Int_t i = 0; i < 3; i++) {
    c1->cd(i+1);
    if (! files[i]) continue;
    TH2 *AvLaser = (TH2 *) files[i]->Get(Form("AvLaser_%02i_%02i",sec,row));
    if (! AvLaser) continue;
    AvLaser->Draw("colz");
  }
}
