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
  dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
  dbMk->SetDebug(1);
  //  dbMk->SetFlavor("laserDV","tpcDriftVelocity");
  dbMk->SetFlavor("NewlaserDV","tpcDriftVelocity");
  //  dbMk->SetFlavor("ofl+sim");
  //   dbMk->SetFlavor("simu","svtWafersPosition"); 
  //   dbMk->SetFlavor("sim","tpcGlobalPosition");
  //   dbMk->SetFlavor("sim","tpcSectorPosition");
  //   dbMk->SetFlavor("sim","tpcISTimeOffsets");
  //   dbMk->SetFlavor("sim","tpcOSTimeOffsets");
  dbMk->Init();
}
//________________________________________________________________________________
//void Db(const Char_t *tabNam  = "Calibrations/tpc/noiseElim", 
void SvtBadAnodes(const Char_t *tabNam  = 
	"Calibrations/svt/svtBadAnodes",
#if 0
	"Calibrations/svt/svtRDOstripped",
	"Calibrations/tpc/tpcGas",
	"Calibrations/svt/svtAnodeDriftCorr",
	"Calibrations/ssdStripCalib",
	"Calibrations/tpc/tpcGain",
	"Calibrations/tpc/TpcAdcCorrectionB",
	"Calibrations/tpc/TpcZCorrectionB",
	"Calibrations/tpc/TpcSecRowB",
	"Geometry/svt/svtWafersPosition",
	"Geometry/svt/WafersIDs",
	"Calibrations/tracker/ssdHitError",
	"Calibrations/svt/svtRDOs",
	"Calibrations/svt/svtDriftCorrection",
	"Geometry/ssd/ssdWafersPosition",
	"Calibrations/tpc/tpcGain",
	"RunLog/onl/starClockOnl",Int_t date = 20061101, Int_t time = 0 
	"Calibrations/tpc/tpcOSTimeOffsets",Int_t date = 20051101, Int_t time = 0 
	"Geometry/svt/svtWafersPosition",Int_t date = 20051101, Int_t time = 0 
	"Geometry/svt/svtWafersPosition",Int_t date = 20031101, Int_t time = 0 
	"RunLog/l3/l3AlgorithmInfo",Int_t date = 20041101, Int_t time = 0 
	"RunLog/onl/tpcRDOMasks", Int_t date = 20031120, Int_t time = 0 
	"Calibrations/tpc/tpcPressureB", Int_t date = 20031120, Int_t time = 220000 
	"Calibrations/tpc/tpcPressureB", Int_t date = 20050111, Int_t time = 220000 
	"Calibrations/tpc/tpcDriftVelocity",	Int_t date = 20040219,	Int_t time = 80354
	"Calibrations/tpc/TpcEdge",
	"Calibrations/tpc/TpcLengthCorrectionB",
	"Calibrations/tpc/TpcSecRowB",
	"Calibrations/tpc/TpcAdcCorrectionB",
	"Calibrations/tpc/TpcLengthCorrectionB",
	"Calibrations/tpc/TpcZCorrectionB",
	"Calibrations/tpc/tpcGain",
	"RunLog/onl/tpcRDOMasks",
#endif
	Int_t date = 20050120, Int_t time = 144259
	)
{ 
  if (dbMk == 0) Load();
  dbMk->SetDebug(4);
  dbMk->SetDateTime(date,time); 
  dbMk->SetMaxEntryTime(20060620,0);
#if 0
  //  to browse many databases, use this approach
  const char* dbs[] = {"Geometry","Calibrations","RunLog","Conditions",0}; 
  for(int i=0;dbs[i]!=0;i++)dbMk->GetDataBase(dbs[i]); 
#else
  // to browse 1 database, use this one
  TDataSet *set = dbMk->GetDataBase(gSystem->DirName(tabNam));
  TDataSetIter next(set,0);
  TDataSet *ds = 0;
  int Nbad = 0;
  TString Name(gSystem->BaseName(tabNam));
  while ((ds = next())) {
    //    cout << "ds " << ds->GetName() << "\t" << ds->GetTitle() << endl;
    if (Name != TString(ds->GetName())) continue;
    if (! ds->HasData()) continue;
    table = (TTable *) ds; //->FindByName(gSystem->BaseName(tabNam));
    cout << "table = " << table->GetName() << "/" << table->Path() <<" found" << endl;
    St_svtBadAnodes *Bad = (St_svtBadAnodes *) table;
    svtBadAnodes_st* ri = Bad->GetTable();
    int ibh = 0;
    //      for (int j=0; j<Nrows; j++,ri++){
    	cout << "id " << ri->ID << endl;
	for (int l = 0; l < 240; l++){
	  if (ri->isBadAnode[l]) {
	    Nbad++; ibh++;
	    cout << "  " << l;
	    if (!(ibh%10) ) cout << endl;
	  }
	}
	cout << endl;
	//      }
	cout << " Nbad=" << Nbad << endl;
#endif
  //  TBrowser* b2 = new TBrowser("TestDbMaker",dbMk);
  } 
}

