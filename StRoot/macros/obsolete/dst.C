// $Id: dst.C,v 1.1 1998/11/01 16:42:29 fisyak Exp $
// $Log: dst.C,v $
// Revision 1.1  1998/11/01 16:42:29  fisyak
// dst analysis
//
{
#if 0
void dst(
         const Char_t *fileinp = 
"/disk1/star/auau200/hijing135/default/b0_3/year2a/hadronic_on/tfs_dst/set187_02_48evts_h_dst.xdf")
{
#endif
  gSystem->Load("St_base.so");
  gSystem->Load("StChain.so");
  gSystem->Load("xdf2root.so");
  gSystem->Load("St_Tables.so");
  gSystem->Load("St_xdfin_Maker.so");
  gSystem->Load("St_geant_Maker.so");
  gSystem->Load("St_TLA_Maker.so");
  gSystem->Load("St_ana_Maker.so");
         const Char_t *fileinp = 
"/disk1/star/auau200/hijing135/default/b0_3/year2a/hadronic_on/tfs_dst/set187_02_48evts_h_dst.xdf";
         const Int_t Nevents=100;
  St_XDFFile   *xdf_in   = 0;
  if (fileinp)  xdf_in   = new St_XDFFile(fileinp,"r");
//TFile      *root_tree= new TFile("auau_central_hijing.tree.root","RECREATE");
// Create the main chain object
  //  StChain chain("bfc");
  StChainSpy chain("dstChain");

  if (xdf_in) {
    St_xdfin_Maker xdfin("xdfin");
    chain.SetInputXDFile(xdf_in);
  }
  St_geant_Maker    geant("geant","event/geant/Event");
  St_TLA_Maker    summary("run_summary","run/dst");
  St_ana_Maker        dst("dst","event/data/global/dst");
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
  else TBrowser b;
#if 0
#endif
}
