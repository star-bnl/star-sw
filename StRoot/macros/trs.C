// $Id: trs.C,v 1.7 1999/03/07 22:34:04 fisyak Exp $
// $Log: trs.C,v $
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
  gSystem->Load("StarClassLibrary");
  gSystem->Load("St_params_Maker");
  gSystem->Load("geometry");
  gSystem->Load("St_g2r");
  gSystem->Load("St_geant_Maker");
  gSystem->Load("St_TLA_Maker");
  gSystem->Load("St_xdfin_Maker");
  gSystem->Load("StTrsMaker");
}
void trs(const Int_t Nevents=1)
{
  if (gClassTable->GetID("StChain") < 0) Load();
  chain = new StChain("trs");
  St_params_Maker  *params = new St_params_Maker("params","params");
  //  St_TLA_Maker       *geom = new St_TLA_Maker("geom","run/geant/Run");
  geant = new St_geant_Maker("geant","event/geant/Event");
  geant->SetNwGEANT(20 000 000);
  //  geant->SetNwPAW(1000000);
  geant->SetIwtype(1);
  geant->Do("gfile p /disk1/star/test/psc0049_08_40evts.fzd");
  //geant->Do("gfile p /star/u2b/lasiuk/onemuon.fz");
  //geant->Do("gfile p /star/u2b/lasiuk/msector.fz");
  //geant->Do("mode tpce prin 1 digi 2");   // make tpc_hit in local coordinates
  //  geant->LoadGeometry("detp geometry field_only");
  StTrsMaker    *tpc_raw = new StTrsMaker("tpc_raw","event/raw_data/tpc");
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
