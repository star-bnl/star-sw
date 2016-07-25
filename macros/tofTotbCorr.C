/* 
   root.exe 'Db.C("Calibrations/tof/tofTotbCorr",20100408,150001)' 'tofTotbCorr.C((St_tofTotbCorr *) table)'
*/
void tofTotbCorr(St_tofTotbCorr *table = 0) {
  if (! table) return;
  TH1D *tray   = new TH1D("tray","tray",120,0.5,120.5);
  TH1D *module = new TH1D("module","module",32,0.5,32.5);
  TH1D *cell   = new TH1D("cell","cell",6,0.5,6.5);
  TH3F *tmc    = new TH3F("tmc","tray, module, cell", 120,0.5,120.5, 32, 0.5, 32.5, 6, 0.5, 6.5);
  Int_t n = table->GetNRows();
  tofTotbCorr_st *row = table->GetTable();
  for (Int_t i = 0; i < n; i++, row++) {
    tray->Fill(row->trayId);
    module->Fill(row->moduleId);
    cell->Fill(row->cellId);
    tmc->Fill(row->trayId, row->moduleId, row->cellId);
  }
}
