// $Id: dst.C,v 1.3 1999/01/03 20:59:55 fisyak Exp $
// $Log: dst.C,v $
// Revision 1.3  1999/01/03 20:59:55  fisyak
// Remove St_geom_Maker
//
// Revision 1.2  1998/12/29 19:38:22  fine
// STAR_shapes: test of the brand new St_NodeView class has been added
//
// Revision 1.1  1998/11/01 16:42:29  fisyak
// dst analysis
//
TBrowser *b = 0;
class StChain;
StChain  *chain=0;
void dst(const Int_t Nevents=1,
const Char_t *fileinp = "/disk1/star/auau200/hijing135/default/b0_3/year_1b/hadronic_off/tfs_dst/set302_06_80evts_h_dst.xdf")
{
// Dynamically link some shared libs
if (gClassTable->GetID("StChain") < 0){
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
  gSystem->Load("St_xdfin_Maker");
  gSystem->Load("St_params_Maker");
  gSystem->Load("geometry");
  gSystem->Load("St_geant_Maker");
  gSystem->Load("St_TLA_Maker");
  gSystem->Load("global");
  gSystem->Load("St_global");
  gSystem->Load("St_dst_Maker");
}
  St_XDFFile   *xdf_in   = 0;
  if (fileinp)  xdf_in   = new St_XDFFile(fileinp,"r");
//TFile      *root_tree= new TFile("auau_central_hijing.tree.root","RECREATE");
// Create the main chain object
  StChain *chain = new StChain("dst_ana");
  //  St_params_Maker *params = new St_params_Maker("params","run/params");
  //  St_geant_Maker     *geant = new St_geant_Maker("geant","run/geant/Run");
  if (xdf_in) {
    St_xdfin_Maker *xdfin = new St_xdfin_Maker("xdfin");
    chain->SetInputXDFile(xdf_in);
  }
  St_TLA_Maker       *glb = new St_TLA_Maker("global","event/data/global");
  St_dst_Maker       *dst = new St_dst_Maker("dst","dst");
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
