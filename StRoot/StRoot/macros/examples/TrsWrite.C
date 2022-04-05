//
// $Id: TrsWrite.C,v 1.6 2006/08/15 21:43:06 jeromel Exp $
//
// $Log: TrsWrite.C,v $
// Revision 1.6  2006/08/15 21:43:06  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.5  2000/01/25 16:06:37  fisyak
// g2r -> g2t
//
// Revision 1.4  2000/01/20 18:18:16  calderon
// fixed for current TRS in dev.  Current DB in dev is ROOT DB, because
// Electronics DB from MySQL is not accessible.  When it is, just comment out
// ROOT DB part and uncomment MySQL DB part.
//
// Revision 1.3  2000/01/10 21:49:07  kathy
// put owner statement in macros
//
// Revision 1.2  1999/11/16 22:31:02  calderon
// Version using ROOT Database (Hopefully soon we'll switch to
// the TPC Db).
//
// Revision 1.1  1999/11/09 19:12:04  calderon
// Initial Commit.
// Example to write a .trs file from a muon processed through TRS.
/////////////////////////////////////////////////////////
//
// owner: Manuel Calderon
//
// Description:
// Write a .trs file from a muon track.
//
////////////////////////////////////////////////////////
TBrowser *b = 0;
class StChain;
StChain  *chain=0;
class St_geant_Maker;
St_geant_Maker *geant=0;
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
  gSystem->Load("geometry");
  gSystem->Load("St_g2t");
  gSystem->Load("St_geant_Maker");
  gSystem->Load("StTrsMaker");
}
void TrsWrite(const Int_t Nevents=1)
{
  if (gClassTable->GetID("StChain") < 0) Load();
  chain = new StChain("trs");
  chain->SetDebug();
  chain->SetInput("EvtHddr",".make/geant/.const/EvtHddr");    

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

  // Geant
  geant = new St_geant_Maker("geant");
  geant->SetNwGEANT(10 000 000);
  geant->SetDebug();
  geant->SetIwtype(1);
  TString InFile("/afs/rhic.bnl.gov/star/tpc/trstest/trs_muon_10cmdrift_good.fzd");
  geant->SetInputFile(InFile.Data());
  chain->SetInput("geom","geant:geom");

  // TRS
  StTrsMaker    *tpc_raw = new StTrsMaker("Trs");
  tpc_raw->writeFile("test.trs",Nevents);

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
