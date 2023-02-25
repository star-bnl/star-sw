void DrawdEdxppbar() {
  const Char_t *files[3] = {"9p2GeV_2020_P23ia/9p2GeV_2020_P23ia_dEdx.B.root",
			    "9p2GeV_2020b_P23ia/9p2GeV_2020b_P23ia_dEdx.E.root",
			    "9p2GeV_2020c_P23ia/9p2GeV_2020c_P23ia_dEdx.F.root"};
  const Char_t *parts[2] = {"p","p-"};
  for (Int_t i = 0; i < 3; i++) {
    TFile *file = TFile::Open(files[i]);
    if (! file) continue;
    TString dirname(gSystem->BaseName(files[i]));
    dirname.ReplaceAll(".root"," ");
    TCanvas *c1 = new TCanvas(Form("c%i",i),dirname);
    c1->Divide(1,2);
    c1->cd(1)->SetLogz(1);
    for (Int_t j = 0; j < 2; j++) {
      TString Hist("/Tracks/");
      Hist += parts[j];
      Hist += "/hdEdX";
      TH2F *hdEdX = (TH2F *) file->Get(Hist);
      if (! hdEdX) continue;
      c1->cd(j+1)->SetLogz(1);
      hdEdX->Draw("colz");
      TLegend *l = new TLegend(0.1,0.7,0.8,0.9);
      Int_t indx = dirname.Index("_dEdx");
      TString PartN(dirname,indx);
      PartN += " "; PartN += parts[j];
      l->AddEntry(hdEdX,PartN);
      l->Draw();
      c1->Update();
    }
  }
}
