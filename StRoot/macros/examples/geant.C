// $Id: geant.C,v 1.10 1999/05/21 15:33:58 kathy Exp $
// $Log: geant.C,v $
// Revision 1.10  1999/05/21 15:33:58  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
// Revision 1.9  1999/05/06 03:21:28  fisyak
// synchronize FTPC and TPC slow/fast
//
// Revision 1.8  1999/04/01 23:39:47  fisyak
// Cleanup old macros
//
// Revision 1.7  1999/02/22 23:28:14  fisyak
// Cleaning up for SL99a
//
// Revision 1.6  1999/02/19 23:43:36  fisyak
// add parameters
//
// Revision 1.5  1999/02/05 16:31:42  fine
// StarGeom.C macro has been improved
//
// Revision 1.4  1999/02/02 17:33:11  fine
// makedoc.C creates html directory itself now
//
// Revision 1.3  1999/01/23 18:38:51  fisyak
// Cleanup for SL98l
//
// Revision 1.2  1999/01/08 21:39:54  fisyak
// Add Gene Van Buren bfc description
//
// Revision 1.1  1999/01/05 01:38:03  fisyak
// geant with St_Nodes
//=======================================================================
// owner: Pavel Nevski
// what it does: 
//=======================================================================
//
#define gtrack
TBrowser *b = 0;
class StChain;
StChain  *chain=0;
class St_geant_Maker;
St_geant_Maker *geant=0;
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("geometry");
  gSystem->Load("g2r");
  gSystem->Load("St_g2r");
  gSystem->Load("St_geant_Maker");
  gSystem->Load("St_TLA_Maker");
}
void geant(const Int_t Nevents=0,const Char_t *fzfile ="/disk1/star/test/psc0049_08_40evts.fzd")
{
  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();
  // Create the main chain object
  if (! chain) chain = new StChain("bfc");
  chain->SetDebug();
  //  Create the makers to be called by the current chain
  if (! geant) geant = new St_geant_Maker;
  geant->SetNwGEANT(20 000 000);
#ifdef gtrack
  geant->SetIwtype(1);
  geant->Do("debug on;");
  //  geant->LoadGeometry("detp geometry field_only");
  geant->LoadGeometry("detp geometry year_1b");
  geant->Do("subevent 0;");
  geant->Do("gkine 10 6 1. 1. -1. 1. 0 6.28  -1. 1.;");
  geant->Do("mode g2tm prin 1;");
  //  geant->Do("next;");
  //  geant->Do("dcut cave z 1 10 10 0.03 0.03;");
  geant->Do("debug on;");
  geant->Do("swit 2 3;");
#else
  TString cmd("gfile p ");
  cmd += fzfile;
  geant->Do(cmd.Data());
#endif
  chain->PrintInfo();
// Init the main chain and all its makers
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  Int_t i=0;
  for (Int_t i =1; i <= Nevents; i++)
  {
    if (chain->Make(i)>=kStEOF) break;
    if (i != Nevents) chain->Clear();
    printf ("===========================================\n");
    printf ("=========================================== Done with Event no. %d\n",i);
    printf ("===========================================\n");
  }
  if (Nevents > 1) {
    chain->Finish();
  }
  else {b = new TBrowser("GEANT",chain);}
  gSystem->Exec("echo Run completed on `hostname` at `date`");
}
