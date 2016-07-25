class St_db_Maker;
St_db_Maker *dbMk = 0;
class TTable;
TTable *table = 0;
//________________________________________________________________________________
void Load() {
  if (gClassTable->GetID("StDbManager") < 0) {
    // Baseline shared libraries
    gSystem->Load("libTable");
    gSystem->Load("St_base"); 
    gSystem->Load("StChain");
    gSystem->Load("StUtilities");
    // DB-specific libs
    // may only need libStDb_Tables.so
    // but may need St_Tables.so... so I'm using this one
    //  gSystem->Load("libStDb_Tables.so");
    gSystem->Load("St_Tables.so");
    gSystem->Load("StDbLib.so");
    gSystem->Load("StDbBroker.so"); 
    gSystem->Load("St_db_Maker.so");
  }
  dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
  dbMk->SetDebug(1);
  //  dbMk->SetFlavor("ofl+sim");
  dbMk->SetFlavor("simu","svtWafersPosition"); 
  dbMk->Init();
}
//________________________________________________________________________________
//void Db(const Char_t *tabNam  = "Calibrations/tpc/noiseElim", 
void DbS(const Char_t *tabNam  = 
	"Survey/svt Geometry/svt/LadderOnSurvey",Int_t date = 20051101, Int_t time = 0 
	){ 
  if (dbMk == 0) Load();
  dbMk->SetDebug(2);
  dbMk->SetDateTime(date,time); 
  //  dbMk->SetFlavor("ofl+laserDV","tpcDriftVelocity");
  //  dbMk->SetMaxEntryTime(20040520,0);
  // to browse 1 database, use this one
  TDataSet *set = dbMk->GetDataBase(gSystem->DirName(tabNam));
  table = (TTable *) set->FindByName(gSystem->BaseName(tabNam));
  if (table) {
    TDatime t[2];
    dbMk->GetValidity(table,t);
    cout << "==============================================" << endl;
    Int_t Nrows = table->GetNRows();
    cout << "Found table " << table->GetName() << " with NRows = " << Nrows << " in db" << endl;
    cout << "Validity:" << t[0].GetDate() << "/" << t[0].GetTime()
	 << " -----   " << t[1].GetDate() << "/" << t[1].GetTime() << endl;
    if (Nrows > 10) Nrows = 10;
    table->Print(0,Nrows);
    cout << "==============================================" << endl;
    TString name(gSystem->BaseName(tabNam));
    name += Form(".%06i.%06i.old.root",t[0].GetDate(),t[0].GetTime());
    TFile *f = new TFile(name.Data(),"RECREATE");
    table->Write();
    delete f;
  }
  else cout << "Table:" << tabNam << " has not been found" << endl;
}







