class StChain;
class St_db_Maker;
St_db_Maker *dbMk = 0;
class TTable;
TTable *table = 0;
class StTpcCoordinateTransform;
StTpcCoordinateTransform *transform = 0;
//________________________________________________________________________________
void Load() {
#if 1
  if (gClassTable->GetID("TTable") < 0) {
    gSystem->Load("libTable");
  }
  if (gClassTable->GetID("StDbManager") < 0) {
    gROOT->LoadMacro("bfc.C");
    TString Chain("db,tpcDb,mysql,NoDefault");
    bfc(-1,Chain.Data(),0,0,0);
    dbMk = (St_db_Maker *)chain->GetMaker("db");
    dbMk->SetDebug(1);
    dbMk->SetFlavor("ofl+sim");
  }
#else
  // rootmap version
  gSystem->Load("libSt_base");
  gSystem->Load("libStUtilities");
  StChain *chain = new StChain();
  St_db_Maker *dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");
  StTpcDbMaker *tpcDb = new StTpcDbMaker();
  dbMk->SetDebug(1);
  dbMk->SetFlavor("ofl+sim");
#endif
}

//________________________________________________________________________________
void Print(Int_t sector = 1, Int_t row = 24, Int_t pad = 1, Int_t time = 1, 
	   Double_t rx=0, Double_t ry=1, Double_t rz=0) {
  cout << endl; cout << endl;
  cout << "Directions =============================" << endl;
  StTpcLocalSectorDirection   dirLS(rx,ry,rz,sector,row);      cout << dirLS << endl;
  StTpcLocalSectorAlignedDirection     dirLSA;//cout << dirLSA << endl;
  transform->operator()(dirLS,dirLSA);                     cout << dirLSA << endl;
  StTpcLocalDirection                  dirL;
  transform->operator()(dirLSA,dirL);                      cout << dirL << endl;
  StGlobalDirection                    dirG;
  transform->operator()(dirL,dirG);                        cout << dirG << endl;
  cout << "Back ===================================" << endl;
  transform->operator()(dirG,dirL,sector,row);             cout << dirL << endl;
  transform->operator()(dirL,dirLSA);                      cout << dirLSA << endl;
  transform->operator()(dirLSA,dirLS);                     cout << dirLS << endl;
  cout << "Coordinates ============================" << endl;
  StTpcPadCoordinate coorP(sector, row, pad, time);             cout << coorP << endl; 
  StTpcLocalSectorCoordinate  coorLS;
  transform->operator()(coorP,coorLS);                     cout << coorLS << endl;
  StTpcLocalSectorAlignedCoordinate  coorLSA;
  transform->operator()(coorLS,coorLSA);                   cout << coorLSA << endl;
  StTpcLocalCoordinate  coorL;
  transform->operator()(coorLSA,coorL);                    cout << coorL << endl;
  StGlobalCoordinate    coorG;
  transform->operator()(coorL,coorG);                      cout << coorG << endl;
  cout << "Back ===================================" << endl;
  transform->operator()(coorG,coorL, sector, row);         cout << coorL << endl;
  transform->operator()(coorL,coorLSA);                    cout << coorLSA << endl;
  transform->operator()(coorLSA,coorLS);                   cout << coorLS  << endl;
  transform->operator()(coorLS,coorP);                     cout << coorP << endl; 
}
//________________________________________________________________________________
void testTpcCoordinateTransform(Int_t date = 20040125, Int_t time = 0) {
  if (dbMk == 0) Load();
  dbMk->SetDateTime(date,time); 
  chain->Init();
  chain->Make();
  if (! gStTpcDb) return;
  transform = new StTpcCoordinateTransform(gStTpcDb);
  Int_t sector = 12;
  Int_t row    = 45;
  Print(sector,row);
}
