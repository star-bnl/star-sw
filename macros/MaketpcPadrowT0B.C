/*
  root.exe ZLMFreqTpcHitZTMfl0.root lDb.C ' MaketpcPadrowT0B.C("mu",20190201,5)' 
*/
//________________________________________________________________________________
void MaketpcPadrowT0B(const Char_t *name = "mu", Int_t date = 20190201, Int_t time = 5) {
  TH2 *mu = (TH2 *) gDirectory->Get(name);
  if (! mu) {
    cout << "Histogram " << name << " has not been found"  << endl;
    return;
  }
  St_tpcPadrowT0 *tpcPadrowT0 = new St_tpcPadrowT0("tpcPadrowT0",24);
  Double_t DV    = 5.55769; // cm/microsec
  tpcPadrowT0_st row;
  for (Int_t sector = 1; sector <= 24; sector++) {
    memset (&row, 0, sizeof(row));
    for (Int_t padrow = 1; padrow <= 72; padrow++) {
      Double_t val = mu->GetBinContent(sector, padrow);
      if (val < 200) continue;
      row.T0[padrow-1] = (208.707 - val)/DV;
      cout << "sector/padrow " << sector << "/" << padrow << " val = " << val << " corr = " <<  row.T0[padrow-1] << endl;
    }
    tpcPadrowT0->AddAt(&row);
  }
  tpcPadrowT0->Print(0,24);
  TDatime  dtime(date,time);
  TString filename(Form("tpcPadrowT0B.%08d.%06d.root",dtime.GetDate(),dtime.GetTime()));
  printf("Create %s\n",filename.Data());
  TFile *f = new TFile(filename.Data(),"recreate");
  tpcPadrowT0->Write();
  delete f;
}
