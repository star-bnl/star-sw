class St_db_Maker;
St_db_Maker *dbMk = 0;
//________________________________________________________________________________
void Load(Int_t debugL = 1) {
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
  //  dbMk = new St_db_Maker("db","MySQL:StarDb");
  dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
  dbMk->SetDebug(debugL);
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
  dbMk->Init();
}
//________________________________________________________________________________
void tpcPadT0Merge(){ 
  const Char_t *tpcGainName = "Calibrations/tpc/tpcGain";
  const Char_t *tpcT0Name   = "Calibrations/tpc/tpcT0";
  struct Datime_t {Int_t date; Int_t time;};
  Datime_t dt[] = {
    {20000614, 175430},
    {20000615,  0},
    {20010601,  0},
    {20010701,  0},
    {20010911,  0},
    {20010924,  0},
    {20011205,  0},
    {20021105,  0},
    {20030101,  1},
    {20030513,  1},
    {20031228,  0},
    {20040122, 120000},
    {20040209, 203000},
    {20040416, 145000},
    {20041024,  0},
    {20050111, 190700},
    {20050209, 152800},
    {20050311, 153700},
    {20050412, 184300},
    {20060308, 115800},
    {20060510, 150600},
    {20070330, 135100},
    {20070420, 142400},
    {20070503,  0},
    {20070524, 161901},
    {20071127, 151801},
    {20071127, 151802},
    {20080101,  2},
    {20080115, 93300},
    {20080115, 93301},
    {20080208, 174703},
    {20080218, 1712},
    {20080219, 94315},
    {20080219, 102708},
    {20080222, 70008},
    {20080223, 182619},
    {20080223, 194538},
    {20080224, 54959},
    {20080225, 203804},
    {20080228, 34753},
    {20080228, 164558},
    {20080228, 235924},
    {20080229, 203905},
    {20080301, 30910},
    {20080302, 102648},
    {20080302, 194942}
  };
  Int_t debugL = 1;
  if (dbMk == 0) Load(debugL);
  dbMk->SetDebug(4);
  Int_t N = sizeof(dt)/sizeof(Datime_t);
  for (Int_t i  = 0; i < N; i++) {
    dbMk->SetDateTime(dt[i].date,dt[i].time); 
    St_tpcGain *tpcGain = (St_tpcGain *) dbMk->GetDataBase(tpcGainName);
    St_tpcT0   *tpcT0   = (St_tpcT0 *) dbMk->GetDataBase(tpcT0Name);
    if (tpcGain && tpcT0) {
      TDatime tGain[2], tT0[2];
      dbMk->GetValidity(tpcGain,tGain);
      cout << "==============================================" << endl;
      Int_t Nrows = tpcGain->GetNRows();
      cout << "Found tpcGain " << tpcGain->GetName() << " with NRows = " << Nrows << " in db" << endl;
      cout << "Validity:" << tGain[0].GetDate() << "/" << tGain[0].GetTime()
	   << " -----   " << tGain[1].GetDate() << "/" << tGain[1].GetTime() << endl;
      //      tpcGain->Print(0,1);
      dbMk->GetValidity(tpcT0,tT0);
      cout << "==============================================" << endl;
      cout << "Found tpcT0 " << tpcT0->GetName() << " with NRows = " << tpcT0->GetNRows() << " in db" << endl;
      cout << "Validity:" << tT0[0].GetDate() << "/" << tT0[0].GetTime()
	   << " -----   " << tT0[1].GetDate() << "/" << tT0[1].GetTime() << endl;
      //      tpcT0->Print(0,1);
      if (Nrows != tpcT0->GetNRows()) {
	cout << "no. of rows does not match\t" << Nrows << "\t" << tpcT0->GetNRows() << endl;
	continue;
      }
      cout << "==============================================" << endl;
      St_tpcPadGainT0 *table = new St_tpcPadGainT0("tpcPadGainT0",1);
      tpcPadGainT0_st  tpcPadGainT0;
      memset(&tpcPadGainT0, 0, sizeof(tpcPadGainT0_st));
      tpcGain_st *Gain = tpcGain->GetTable();
      tpcT0_st   *T0 = tpcT0->GetTable();
      for (Int_t k = 0; k < Nrows; k++,Gain++,T0++) {
	memcpy (&tpcPadGainT0.Gain[k][0][0],&Gain->Gain[0][0], 45*182*sizeof(Float_t)); 
	memcpy (&tpcPadGainT0.T0[k][0][0],  &T0->T0[0][0],     45*182*sizeof(Float_t)); 
      }
      table->AddAt(&tpcPadGainT0);
      TString name("tpcPadGainT0");
      TDatime t = tGain[0];
      if (tT0[0].Convert() > tGain[0].Convert()) t = tT0[0];
      name += Form(".%06i.%06i.root",t.GetDate(),t.GetTime());
      cout << "Create Table " << name.Data() << endl;
      TFile *f = new TFile(name.Data(),"RECREATE");
      table->Write();
      delete f;
      delete table;
    }
    else {
      if (!tpcGain)  cout << "Table:" << tpcGainName << " has not been found" << endl;
      if (!tpcT0)  cout << "Table:" << tpcT0Name << " has not been found" << endl;
    }
  }
}






