/* 
   root.exe 'tpcPadGainT02B.C(20000615,0)'
*/
#include "Db.C"
//________________________________________________________________________________
void tpcPadGainT02B(Int_t date = 20000615, Int_t time = 0) {
  TString name("StarDb/Calibrations/tpc/tpcPadGainT0");
  Db(name.Data(),date,time);
  if (table) {
    TDatime t[2];
    dbMk->GetValidity(table,t);
    cout << "==============================================" << endl;
    Int_t Nrows = table->GetNRows();
    cout << "Found table " << table->GetName() << " with NRows = " << Nrows << " in db" << endl;
    cout << "Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
	 << " -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endl;
  }  
  St_tpcPadGainT0  *tpcPadGainT0 = (St_tpcPadGainT0  *) table;
  if (! tpcPadGainT0) {
    cout << " tpcPadGainT0 has not been found " << endl;
    return;
  }
  tpcPadGainT0_st *row = tpcPadGainT0->GetTable();
  TString Name(gSystem->BaseName(name.Data()));
  Name += Form("B.%06i.%06i.root",t[0].GetDate(),t[0].GetTime());
  TFile *fOut = new TFile(Name,"recreate");
  cout << "Open file " << Name.Data() << endl;
  tpcPadGainT0B_st rowB;
  St_tpcPadGainT0B *tableSet = new St_tpcPadGainT0B("tpcPadGainT0B",24);
  for (Int_t s = 1; s <= 24; s++) {
    memset(&rowB,0,tableSet->GetRowSize());
    for (Int_t r = 1; r <= 45; r++)
      for (Int_t p = 1; p <= 182; p++) {
        rowB.Gain[r-1][p-1] = row->Gain[s-1][r-1][p-1];
        rowB.T0[r-1][p-1] = row->T0[s-1][r-1][p-1];
      }
    tableSet->AddAt(&rowB);
  }
  tableSet->Write();
  delete fOut;
}
