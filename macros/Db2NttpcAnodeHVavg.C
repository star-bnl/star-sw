// root.exe -q -b Load.C tpcAnodeHVavgTree.C+ Db2NttpcAnodeHVavg.C
//________________________________________________________________________________
void Load() {
  gROOT->LoadMacro("bfc.C");
  bfc(-1,"db,detDb,nodefault");
  chain->Init();
  dbMk = (St_db_Maker *) chain->Maker("db");
  dbMk->SetDebug(1);
  gROOT->LoadMacro("tpcAnodeHVavgTree.C+");
}
//________________________________________________________________________________
//void Db(const Char_t *tabNam  = "Calibrations/tpc/noiseElim", 
void Db2NttpcAnodeHVavg() {
  Load();
  TDatime t0(20090101,0);
  UInt_t u0 = t0.Convert();
  //  UInt_t u0 = 1268639999; //1262327101;
  TDatime t;
  UInt_t unow = t.Convert() + 5*3600;
  UInt_t u = u0;
  TDatime tt[2];
  Int_t run = 1110000000;
  TFile *f = tpcAnodeHVavgTree();
  while (u <= unow) {
    tpcAnodeHVavgC(u);
  }
  f->Write();
  delete f;
}
