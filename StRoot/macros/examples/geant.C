// $Id: geant.C,v 1.2 1999/01/08 21:39:54 fisyak Exp $
// $Log: geant.C,v $
// Revision 1.2  1999/01/08 21:39:54  fisyak
// Add Gene Van Buren bfc description
//
// Revision 1.1  1999/01/05 01:38:03  fisyak
// geant with St_Nodes
//
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
  //  gSystem->Load("libmsg");
  //  gSystem->Load("libasu");
  //  gSystem->Load("libsoc");
  //  gSystem->Load("libtdm");
  //  gSystem->Load("libdui");
  //  gSystem->Load("gstar");
  //  gSystem->Load("St_gstar");
  //  gSystem->Load("g2t");
  //  gSystem->Load("St_g2t");
  gSystem->Load("St_geant_Maker");
  gSystem->Load("St_TLA_Maker");
}
void geant()
{
  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();
  // Create the main chain object
  if (! chain) chain = new StChain("bfc");
  //  Create the makers to be called by the current chain
  St_TLA_Maker geom = new St_TLA_Maker("geom","run/geant/Run");
  if (! geant) geant = new St_geant_Maker("geant","event/geant/Event");
  geant->SetNwGEANT(20 000 000);
  //  geant->SetNwPAW(1000000);
  geant->SetIwtype(1);
  //  geant->LoadGeometry("");
  geant->Do("gfile pz /disk1/star/kathy/auau_ce_b0-2_4041_4060.fzd;");
  //  geant->Do("zone 1 2;");
  //  geant->Do("next;");
  geant->Do("dcut cave x 0.1 10 10 0.03 0.03;");
  chain->PrintInfo();
// Init the mai chain and all its makers
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  Int_t i=0;
  Int_t Nevents = 0;
  for (Int_t i =1; i <= Nevents; i++)
  {
    if (chain->Make(i)) break;
    St_DataSet *dst = chain->DataSet("dst");
    if (i != Nevents) chain->Clear();
    printf ("===========================================\n");
    printf ("=========================================== Done with Event no. %d\n",i);
    printf ("===========================================\n");
  }
  if (Nevents > 1) {
    chain->Finish();
  }
}
