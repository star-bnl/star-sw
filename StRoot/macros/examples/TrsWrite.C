//
// $Id: TrsWrite.C,v 1.3 2000/01/10 21:49:07 kathy Exp $
//
// $Log: TrsWrite.C,v $
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
  gSystem->Load("St_g2r");
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

  // Db
  const char* mainDB = "$STAR/StDb";
  St_db_Maker *dbMk = new St_db_Maker("db",mainDB);
  dbMk->SetDebug();
  chain->SetInput("params","db:StDb/params");

  const char* calibDB = "$STAR_ROOT/calib";
  St_db_Maker *calibMk = new St_db_Maker("calib",calibDB);
  chain->SetInput("calib","calib:calib");
  calibMk->SetDebug();

  // Geant
  geant = new St_geant_Maker("geant");
  geant->SetNwGEANT(10 000 000);
  geant->SetDebug();
  geant->SetIwtype(1);
  TString InFile("/afs/rhic/star/tpc/trstest/trs_muon_10cmdrift_good.fzd");
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
