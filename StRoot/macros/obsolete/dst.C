// $Id: dst.C,v 1.5 1999/05/21 15:33:58 kathy Exp $
// $Log: dst.C,v $
// Revision 1.5  1999/05/21 15:33:58  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
// Revision 1.4  1999/05/06 03:21:28  fisyak
// synchronize FTPC and TPC slow/fast
//
// Revision 1.3  1999/01/03 20:59:55  fisyak
// Remove St_geom_Maker
//
// Revision 1.2  1998/12/29 19:38:22  fine
// STAR_shapes: test of the brand new St_NodeView class has been added
//
// Revision 1.1  1998/11/01 16:42:29  fisyak
// dst analysis
//=======================================================================
// owner: Yuri Fisyak
// what it does: 
//=======================================================================
//
TBrowser *b = 0;
class StChain;
StChain  *chain=0;
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
  gSystem->Load("St_xdfin_Maker");
  gSystem->Load("St_TLA_Maker");
}
void dst(const Int_t Nevents=1,
const Char_t *fileinp = "/disk1/star/test/tfs/psc0065_01_40evts_dst.xdf")
{
// Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();
  St_XDFFile   *xdf_in   = 0;
  if (fileinp)  xdf_in   = new St_XDFFile(fileinp,"r");
//TFile      *root_tree= new TFile("auau_central_hijing.tree.root","RECREATE");
// Create the main chain object
  StChain *chain = new StChain("dst_ana");
  //  St_params_Maker *params = new St_params_Maker("params","run/params");
  //  St_geant_Maker     *geant = new St_geant_Maker("geant","run/geant/Run");
  if (xdf_in) {
    St_xdfin_Maker *xdfin = new St_xdfin_Maker("xdfin");
    xdfin->Init_Done(kTRUE);
    chain->SetInputXDFile(xdf_in);
  }
  chain.PrintInfo();
// Init the mai chain and all its makers
  int iInit = chain.Init();
  if (iInit) chain.Fatal(iInit,"on init");
  gBenchmark->Start("dst");
  Int_t i=0;
  for (Int_t i =1; i <= Nevents; i++){
    if (chain.Make(i)) break;
    if (i != Nevents) chain.Clear();
    printf ("===========================================\n");
    printf ("=========================================== Done with Event no. %d\n",i);
    printf ("===========================================\n");
  }
  if (Nevents > 1) {
    chain.Finish();
    delete xdf_in;
    gBenchmark->Print("dst");
  }
  else b = new TBrowser("dst_ana");
}
