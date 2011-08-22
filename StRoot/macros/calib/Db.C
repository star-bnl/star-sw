class St_db_Maker;
St_db_Maker *dbMk = 0;
class TTable;
TTable *table = 0;
//________________________________________________________________________________
void Load() {
  if (gClassTable->GetID("StDbManager") < 0) {
    // Baseline shared libraries
    //    gSystem->Load("libTable");
    gSystem->Load("St_base"); 
    gSystem->Load("liblog4cxx.so");
    gSystem->Load("StStarLogger");
    gSystem->Load("StChain");
    gSystem->Load("StUtilities");
    // DB-specific libs
    // may only need libStDb_Tables.so
    // but may need St_Tables.so... so I'm using this one
    //  gSystem->Load("libStDb_Tables.so");
    Char_t *mysql = "libmysqlclient";
    Char_t *libs[]  = {"", "/usr/mysql/lib/","/usr/lib/", 0}; // "$ROOTSYS/mysql-4.1.20/lib/",
    //Char_t *libs[]  = {"/usr/lib/", 0};
    Int_t i = 0;
    while ((libs[i])) {
      TString lib(libs[i]);
      lib += mysql;
      lib = gSystem->ExpandPathName(lib.Data());
      if (gSystem->DynamicPathName(lib,kTRUE)) {
	gSystem->Load(lib.Data()); cout << " + " << lib.Data() << endl;
	break;
      }
	i++;
    }
    gSystem->Load("St_Tables.so");
    gSystem->Load("StDbLib.so");
    gSystem->Load("StDbBroker.so"); 
    gSystem->Load("St_db_Maker.so");
  }
  //    dbMk = new St_db_Maker("db","MySQL:StarDb");
    dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
  //  dbMk->SetFlavor("sim","WaferOnLadder");
  //  dbMk->SetDebug(2);
  //  dbMk->SetFlavor("laserDV","tpcDriftVelocity");
  //  dbMk->SetFlavor("ofl+laserDV","tpcDriftVelocity");
  //  dbMk->SetFlavor("NewlaserDV","tpcDriftVelocity");
  //  dbMk->SetFlavor("ofl+sim");
  //   dbMk->SetFlavor("simu","svtWafersPosition"); 
  //   dbMk->SetFlavor("sim","tpcGlobalPosition");
  //   dbMk->SetFlavor("sim","tpcSectorPosition");
  //   dbMk->SetFlavor("sim","tpcISTimeOffsets");
  //   dbMk->SetFlavor("sim","tpcOSTimeOffsets");
  //  dbMk->SetFlavor("sim","starClockOnl");
  //  dbMk->SetFlavor("ofl+laserDV","tpcDriftVelocity");
  //  dbMk->SetMaxEntryTime(20060620,0);
  //  dbMk->SetMaxEntryTime(20080410,0);
}
//________________________________________________________________________________
//void Db(const Char_t *tabNam  = "Calibrations/tpc/noiseElim", 
void Db(const Char_t *tabNam  = 
	"Calibrations/svt/svtBadAnodes",
	Int_t date = 20091215, Int_t time = 0,
	Int_t debugL = 1,
	const Char_t *flavor="sim+ofl"
	){ 
  if (dbMk == 0) Load();
  dbMk->SetDebug(debugL);
  dbMk->SetDateTime(date,time); 
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
    if (Nrows > 10) Nrows = 10;
    //    table->Print(0,Nrows);
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
