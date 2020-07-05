/*
   root.exe 'Db.C("Calibrations/tpc/TpcAvgPowerSupply",20190101,0)' 'TpcAvgPowerSupplyScan.C+((St_TpcAvgPowerSupply *)table,20190801)'
 */
#include "TFile.h"
#include "TNtuple.h"
#include "tables/St_TpcAvgPowerSupply_Table.h"
#include "St_db_Maker/St_db_Maker.h"
TFile *fOut = 0;
TNtuple *FitP = 0;
static St_db_Maker *dbMk = 0;
//________________________________________________________________________________
void DumpTpcAvgPowerSupply(St_TpcAvgPowerSupply *tab, TDatime t) {
  struct Point_t {
    Float_t run, start_time, stop_time, i, Current, Charge, Voltage, date, time;
  };
  Point_t Point;
  if (! FitP) {
    fOut = new TFile("TpcAvgPowerSupply.root","recreate");
    FitP = new TNtuple("FitP","TpcAvgPowerSupply",
		       "run:start_time:stop_time:i:Current:Charge:Voltage:date:time");
  } 
  TpcAvgPowerSupply_st *row = tab->GetTable();
  Point.run = row->run;
  Point.start_time = row->start_time;
  Point.stop_time = row->stop_time;
  Point.date = t.GetDate();
  Point.time = t.GetTime();
  for (Int_t i = 0; i < 192; i++) {
    Point.i = i;
    Point.Current = row->Current[i];
    Point.Charge = row->Charge[i];
    Point.Voltage = row->Voltage[i];
    FitP->Fill(&Point.run);
  }
}
//________________________________________________________________________________
void TpcAvgPowerSupplyScan(St_TpcAvgPowerSupply *tabl, Int_t dmax = 20190101) {
  if (! dbMk) {
    dbMk = (St_db_Maker *) StMaker::GetTopChain()->Maker("db");
  }
  TDatime t[2];
  St_TpcAvgPowerSupply *next = tabl;
  dbMk->GetValidity(next,t);
  cout << "t[0]\t"; t[0].Print();
  cout << "t[1]\t"; t[1].Print();
  TString Path(tabl->Path());
  Path.ReplaceAll("bfc/.make/db/.const/","");
  TString h("/."); h += tabl->GetName();
  Path.ReplaceAll(h,""); cout << "Path " << Path.Data() << endl;
  while (next = (St_TpcAvgPowerSupply *) dbMk->GetDataBase(Path, &t[1])) {
    if (t[0].GetDate() > dmax) break;
    dbMk->GetValidity(next,t);
    cout << "t[0]\t"; t[0].Print();
    cout << "t[1]\t"; t[1].Print();
    DumpTpcAvgPowerSupply(next, t[0]);
    SafeDelete(next);
  }
}
