class St_db_Maker;
St_db_Maker *dbMk = 0;
struct Date_t {
  Int_t date;
  Int_t time;
};
//________________________________________________________________________________
void Load() {
  if (gClassTable->GetID("StDbManager") < 0) {
    // Baseline shared libraries
    //    gSystem->Load("libTable");
    gSystem->Load("St_base"); 
    gSystem->Load("StChain");
    gSystem->Load("StUtilities");
    // DB-specific libs
    // may only need libStDb_Tables.so
    // but may need St_Tables.so... so I'm using this one
    //  gSystem->Load("libStDb_Tables.so");
    gSystem->Load("libmysqlclient");
    gSystem->Load("libStDb_Tables.so");
    gSystem->Load("StDbLib.so");
    gSystem->Load("StDbBroker.so"); 
    gSystem->Load("St_db_Maker.so");
  }
  dbMk = new St_db_Maker("db","MySQL:StarDb");
  //  dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
  dbMk->SetDebug(1);
  //  dbMk->SetFlavor("ofl+sim");
//   dbMk->SetFlavor("simu","svtWafersPosition"); 
//   dbMk->SetFlavor("sim","tpcGlobalPosition");
//   dbMk->SetFlavor("sim","tpcSectorPosition");
//   dbMk->SetFlavor("sim","tpcISTimeOffsets");
//   dbMk->SetFlavor("sim","tpcOSTimeOffsets");
  dbMk->Init();
}
#if 0
//________________________________________________________________________________
//void Db(const Char_t *tabNam  = "Calibrations/tpc/noiseElim", 
void Db2Nt(const Char_t *tabNam  = 	"Calibrations/tpc/TpcSecRowB"	){ 
  Date_t dates[] = {
    { 19980101,      0},
    { 20000614, 175430},
    { 20010701,      0},
    { 20010911,      0},
    { 20010924,      0},
    { 20011205,      0},
    { 20011221,  24512},
    { 20021105,      0},
    { 20030106,      0},
    { 20031120,      0},
    { 20031120,      1},
    { 20040104,      0},
    { 20040104,      1},
    { 20040205,      0},
    { 20040205,      1},
    { 20040217,      0},
    { 20040217,      1},
    { 20040324,      0},
    { 20040324,      1},
    { 20040404,      0},
    { 20040404,      1},
    { 20050111, 220000},
    { 20050403,  10000},
    { 20060308, 115800},
    { 20060406,  50000},
    { 20060424, 210001},
    { 20060510, 150601},
    { 20060519, 160001},
    {        0,      0}
  }; 
  if (dbMk == 0) Load();
  //  dbMk->SetDebug(4);
  //  dbMk->SetFlavor("ofl+laserDV","tpcDriftVelocity");
  //  dbMk->SetMaxEntryTime(20060620,0);
  // to browse 1 database, use this one
  TDataSet *set = dbMk->GetDataBase(gSystem->DirName(tabNam));
  Int_t i = 0;
  struct Row_t {
    Float_t time, sector, row, gain, rms;
  };
  Row_t row;
  f = new TFile("TpcSecRowB.root","RECREATE");
  TNtuple *FitP = new TNtuple("T","TpcSecRowB","time:sector:row:gain:rms");
  while (dates[i].date) {
    TDatime t(dates[i].date,dates[i].time);
    row.time = t.Convert();
    dbMk->SetDateTime(dates[i].date,dates[i].time+1); 
    //    dbMk->InitRun(1000+i);
    TDataSet *set = dbMk->GetDataBase(gSystem->DirName(tabNam));
    St_TpcSecRowCor *table =  (St_TpcSecRowCor *)set->FindByName(gSystem->BaseName(tabNam));
    if (table) {
      cout << "got " << tabNam << " for d/t " << dates[i].date << "/" << dates[i].time << endl;
      TpcSecRowCor_st *gains = table->GetTable();
      for (Int_t sector = 1; sector <= 24; sector++, gains++) {
	row.sector = sector;
	for (Int_t r = 1; r <= 45; r++) {
	  row.row = r;
	  row.gain = gains->GainScale[r-1];
	  row.rms  = gains->GainRms[r-1];
	  FitP->Fill(&row.time);
	  if (sector == 1 && r == 1) {
	    cout << "time " << t.AsString() << " s/r " << sector << "/" << r << " gain " <<  row.gain << row.rms << endl;
	  }
	}
      }
    } else {
      cout << "did not get " << tabNam << " for d/t " << dates[i].date << "/" << dates[i].time << endl;
    }
    i++;
  }
  f->Write();
}
#else
void Db2Nt(const Char_t *tabNam  = 	"Calibrations/tpc/tpcPadrowT0"	){ 
  Date_t dates[] = {
    { 20110202,      0},
    {        0,      0}
  }; 
  if (dbMk == 0) Load();
  //  dbMk->SetDebug(4);
  //  dbMk->SetFlavor("ofl+laserDV","tpcDriftVelocity");
  //  dbMk->SetMaxEntryTime(20060620,0);
  // to browse 1 database, use this one
  TDataSet *set = dbMk->GetDataBase(gSystem->DirName(tabNam));
  Int_t i = 0;
  struct Row_t {
    Float_t sector, row, T0;
  };
  Row_t row;
  f = new TFile("tpcPadrowT0.root","RECREATE");
  TNtuple *FitP = new TNtuple("T","tpcPadrowT0","sector:row:T0");
  while (dates[i].date) {
    TDatime t(dates[i].date,dates[i].time);
    row.time = t.Convert();
    dbMk->SetDateTime(dates[i].date,dates[i].time+1); 
    //    dbMk->InitRun(1000+i);
    TDataSet *set = dbMk->GetDataBase(gSystem->DirName(tabNam));
    St_tpcPadrowT0 *table =  (St_tpcPadrowT0 *)set->FindByName(gSystem->BaseName(tabNam));
    if (table) {
      cout << "got " << tabNam << " for d/t " << dates[i].date << "/" << dates[i].time << endl;
      tpcPadrowT0_st *t0 = table->GetTable();
      for (Int_t sector = 1; sector <= 24; sector++, t0++) {
	row.sector = sector;
	for (Int_t r = 1; r <= 45; r++) {
	  row.row = r;
	  row.T0  = t0->T0[r-1];
	  FitP->Fill(&row.sector);
	  if (sector == 1 && r == 1) {
	    cout << "time " << t.AsString() << " s/r " << sector << "/" << r << " T0 " <<  row.T0 << endl;
	  }
	}
      }
    } else {
      cout << "did not get " << tabNam << " for d/t " << dates[i].date << "/" << dates[i].time << endl;
    }
    i++;
  }
  f->Write();
}
#endif






