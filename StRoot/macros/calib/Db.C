class St_db_Maker;
St_db_Maker *dbMk = 0;
class TTable;
TTable *table = 0;
//________________________________________________________________________________
void Load() {
  if (gClassTable->GetID("StDbManager") < 0) {
    gROOT->LoadMacro("bfc.C");
    //    bfc(-1,"tpcDb,detDb,CorrX,nodefault");
    bfc(-1,"tpcDb,detDb,mysql,nodefault,CorrX"); // ,dbV20151120");
    dbMk = (St_db_Maker *) chain->Maker("db");
  }    
}
//________________________________________________________________________________
//void Db(const Char_t *tabNam  = "Calibrations/tpc/noiseElim", 
void Db(const Char_t *tabNam  = 
	"Geometry/tpc/tpcPadPlanes",
	Int_t date = -1, Int_t time = 0,
	Int_t debugL = 1,
	const Char_t *flavor="sim+ofl+laserDV"
	){ 
  if (dbMk == 0) Load();
  dbMk->SetDebug(debugL);
  Int_t D = date;
  Int_t T = time;
  if (D <= 0) {
    TDatime dt;
    D = dt.GetDate();
    T = dt.GetTime();
    cout << "Set Date " << D << " Time " << T << endl;
  }
  dbMk->SetDateTime(D,T); 
  TString TabNam(tabNam);
  if (TabNam.BeginsWith("StarDb/")) TabNam.ReplaceAll("StarDb/","");
  TString name(gSystem->BaseName(tabNam));
  TString Flavor(flavor);
  if (flavor != "")   dbMk->SetFlavor(flavor,name);
  dbMk->Init();
  table = (TTable *) dbMk->GetDataBase(TabNam);
  if (table) {
    TDatime t[2];
    dbMk->GetValidity(table,t);
    cout << "==============================================" << endl;
    Int_t Nrows = table->GetNRows();
    cout << "Found table " << table->GetName() << " with NRows = " << Nrows << " in db" << endl;
    cout << "Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
	 << " -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endl;
    if (name == "tpcPadrowT0") {
      Double_t t0Inner = 0, t0Outer = 0;
      Int_t      Inner = 0,   Outer = 0;
      tpcPadrowT0_st *row = ((St_tpcPadrowT0 *) table)->GetTable();
      for (Int_t sec = 0; sec < 24; sec++, row++) {
	for (Int_t r = 0; r < 45; r++) {
	  if (row->T0[r]) {
	    if (r < 13) {t0Inner += row->T0[r]; Inner++;}
	    else        {t0Outer += row->T0[r]; Outer++;}
	  }
	}
      }
      if (Inner > 0) t0Inner /= Inner;
      if (Outer > 0) t0Outer /= Outer;
      cout << name.Data() << "\tInner <T0> = " << t0Inner << "\tOuter <T0> = " << t0Outer << endl;
    }
    if (Nrows > 10) Nrows = 10;
    if (table->GetRowSize() < 256) table->Print(0,Nrows);
    cout << "==============================================" << endl;
#if 0
    name += Form(".%06i.%06i.root",t[0].GetDate(),t[0].GetTime());
    TFile *f = new TFile(name.Data(),"RECREATE");
    table->Write();
    delete f;
#else
    name += Form(".%06i.%06i.C",t[0].GetDate(),t[0].GetTime());
    ofstream out;
    out.open(name, ios::out);
    table->SavePrimitive(out,"");
#endif
  }
  else cout << "Table:" << tabNam << " has not been found" << endl;
}
//________________________________________________________________________________
void Db(const Char_t *tabNam  = "StarDb/AgiGeometry/Geometry",
	const Char_t *tag){ 
  if (dbMk == 0) Load();
  cout << "Db(" << tabNam << "," << tag << ")" << endl;
  Int_t date = StMaker::AliasDate(tag);
  Int_t time = StMaker::AliasTime(tag);
  Db(tabNam,date,time);
}
