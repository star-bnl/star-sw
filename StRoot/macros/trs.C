// $Id: trs.C,v 1.10 1999/05/06 03:21:30 fisyak Exp $
// $Log: trs.C,v $
// Revision 1.10  1999/05/06 03:21:30  fisyak
// synchronize FTPC and TPC slow/fast
//
// Revision 1.9  1999/04/01 23:39:48  fisyak
// Cleanup old macros
//
// Revision 1.7  1999/03/07 22:34:04  fisyak
// replace StSclRoot by StarClassLibrary
//
// Revision 1.6  1999/02/16 18:15:49  fisyak
// Check in the latest updates to fix them
//
// Revision 1.5  1999/02/10 20:54:15  lasiuk
// change file
//
////////////////////////////////////////////////////////
TBrowser *b = 0;
class StChain;
StChain  *chain=0;
class St_geant_Maker;
St_geant_Maker *geant=0;
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("geometry");
  gSystem->Load("St_g2r");
  gSystem->Load("St_geant_Maker");
  gSystem->Load("StTrsMaker");
}
void trs(const Int_t Nevents=1)
{
  if (gClassTable->GetID("StChain") < 0) Load();
  chain = new StChain("trs");
  chain->SetDebug();
  chain->SetInput("EvtHddr",".make/geant/.const/EvtHddr");    
 //  Create the makers to be called by the current chain
  const char *mainDB = "$STAR/StDb";
  St_db_Maker *dbMk = new St_db_Maker("db",mainDB);
  chain->SetInput("params","db:StDb/params");
  dbMk->SetDebug();  
  const char *calibDB = "$STAR_ROOT/calib";
  St_db_Maker *calibMk = new St_db_Maker("calib",calibDB);
  chain->SetInput("calib","calib:calib");
  calibMk->SetDebug();  

  geant = new St_geant_Maker("geant");
  geant->SetNwGEANT(10 000 000);
  geant->SetDebug();
  //  geant->SetNwPAW(1000000);
  geant->SetIwtype(1);
  //  TString InFile("/disk1/star/test/psc0049_08_40evts.fzd");
  //  geant->SetInputFile(InFile.Data());
  chain->SetInput("geom","geant:geom");
  //  geant->Do("gfile p /disk1/star/test/psc0049_08_40evts.fzd");
  //geant->Do("gfile p /star/u2b/lasiuk/onemuon.fz");
  //geant->Do("gfile p /star/u2b/lasiuk/msector.fz");
  //geant->Do("mode tpce prin 1 digi 2");   // make tpc_hit in local coordinates
  //  geant->LoadGeometry("detp geometry field_only");
  geant->LoadGeometry("detp geometry year_1b");
  geant->Do("subevent 0;");
  geant->Do("gkine 10 6 1. 1. -1. 1. 0 6.28  -1. 1.;");
  geant->Do("mode g2tm prin 1;");

  StTrsMaker    *tpc_raw = new StTrsMaker("Trs");
  tpc_raw->setTestData(kTRUE);
  //  chain->PrintInfo();
// Init the mai chain and all its makers
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  gBenchmark->Start("trs");
  Int_t i=0;
  for (Int_t i =1; i <= Nevents; i++){
    if (chain->Make(i)) break;
    St_DataSet *dst = chain->DataSet("dst");
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
