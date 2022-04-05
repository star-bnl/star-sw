//
// $Id: TrsRead.C,v 1.4 2000/05/09 20:15:43 kathy Exp $
//
// $Log: TrsRead.C,v $
// Revision 1.4  2000/05/09 20:15:43  kathy
// transfer obsolete macros to /macros/obsolete;  update other macros so that they use standard default inputs plus only few events by default so they'll be easy to run in autoQA macro testing
//
// Revision 1.3  2000/01/20 18:18:16  calderon
// fixed for current TRS in dev.  Current DB in dev is ROOT DB, because
// Electronics DB from MySQL is not accessible.  When it is, just comment out
// ROOT DB part and uncomment MySQL DB part.
//
// Revision 1.2  2000/01/10 21:49:07  kathy
// put owner statement in macros
//
// Revision 1.1  1999/11/09 19:10:47  calderon
// Initial Commit
// Example macro to read .trs file.
/////////////////////////////////////////////////////////
// owner: Manuel Calderon
//
// Description:
// Read in a .trs file created with TrsWrite.C
//
//
////////////////////////////////////////////////////////
TBrowser *b = 0;
class StChain;
StChain  *chain=0;
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("libm");
  gSystem->Load("StUtilities");
  gSystem->Load("StChain");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTrsMaker");
}
void TrsRead(const Int_t Nevents=1)
{
  if (gClassTable->GetID("StChain") < 0) Load();
  chain = new StChain("trs");
  chain->SetDebug();

  //  Create the makers to be called by the current chain
  // ROOT Db
  const char* mainDB = "$STAR/StDb";
  St_db_Maker *dbMk = new St_db_Maker("db",mainDB);
  dbMk->SetDebug();
  chain->SetInput("params","db:StDb/params");

  const char* calibDB = "$STAR_ROOT/calib";
  St_db_Maker *calibMk = new St_db_Maker("calib",calibDB);
  chain->SetInput("calib","calib:calib");
  calibMk->SetDebug();
  StTrsMaker    *tpc_raw = new StTrsMaker("Trs");

  // MySQL DB
//   const char *mainDB = "MySQL:Geometry";
//   St_db_Maker *dbMk = new St_db_Maker("Geometry",mainDB);
//   dbMk->SetDebug();
//   dbMk->Init();
//   dbMk->GetDataBase("Geometry/tpc");
  
//   const char *calibDB = "MySQL:Calib";
//   St_db_Maker *calibMk = new St_db_Maker("Calib",calibDB);
//   calibMk->SetDebug();
//   calibMk->Init();
//   calibMk->GetDataBase("Calibrations/tpc");
//   StTpcDbMaker *tpcDbMk = new StTpcDbMaker("tpcDb");
//   tpcDbMk->Init();
//   tpcDbMk->Make();
//   cout << "The Db: " << gStTpcDb << endl;

  // Tell TRS to read the file.
  tpc_raw->readFile("test.trs");

  // Init the main chain and all its makers
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  gBenchmark->Start("trs");
  
  for (Int_t i =1; i <= Nevents; i++){
    if (chain->Make(i)) break;
    
    if (i != Nevents) chain->Clear();
    printf ("===========================================\n");
    printf ("=========================================== Done with Event no. %d\n",i);
    printf ("===========================================\n");
  }
  if (Nevents > 1) {
    chain->Finish();
    gBenchmark->Print("trs");
  }
  else b = new TBrowser;

}
