// $Id: tpt.C,v 1.1 1999/01/23 18:38:52 fisyak Exp $
// $Log: tpt.C,v $
// Revision 1.1  1999/01/23 18:38:52  fisyak
// Cleanup for SL98l
//
TBrowser *b = 0;
class StChain;
StChain  *chain=0;
void tpt(const Int_t  Nevents=1,
	 const Char_t *fileinp = "/afs/rhic/star/data/samples/hijet-g2t.xdf",
         const Char_t *fileout = 0,
         const Char_t *FileOut = 0){
// Dynamically link some shared libs
if (gClassTable->GetID("StChain") < 0){
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
  gSystem->Load("libmsg");
  gSystem->Load("libtls");
  gSystem->Load("St_params_Maker");
  gSystem->Load("St_xdfin_Maker");
  gSystem->Load("geometry");
  gSystem->Load("St_geant_Maker");
  gSystem->Load("St_TLA_Maker");
  gSystem->Load("tpc");
  gSystem->Load("St_tpc");
  gSystem->Load("St_tss_Maker");
  gSystem->Load("St_tcl_Maker");
  //  gSystem->Load("$ROOTSYS/test/libEvent");
  gSystem->Load("St_tpt_Maker");
}
  St_XDFFile   *xdf_in   = 0;
  if (fileinp)  xdf_in   = new St_XDFFile(fileinp,"r");
  St_XDFFile  *xdf_out   = 0;
  if (fileout) xdf_out   = new St_XDFFile(fileout,"wb");
  TFile       *root_out  = 0; 
  if (FileOut) root_out  =  new TFile(FileOut,"RECREATE");
// Create the main chain object
  chain = new StChain("bfc");
  //  StChainSpy chain("bfc");

//  Create the makers to be called by the current chain
  St_params_Maker *params = new St_params_Maker("params","run/params");
  St_geant_Maker    *geant = new St_geant_Maker("geant","event/geant/Event");
  if (xdf_in) {
    St_xdfin_Maker *xdfin = new St_xdfin_Maker("xdfin");
    chain->SetInputXDFile(xdf_in);
  }
  St_tcl_Maker         *tpc_hits = new St_tcl_Maker("tpc_hits","event/data/tpc/hits");
  //  tpc_hits.Save();
  St_tpt_Maker       *tpc_tracks = new St_tpt_Maker("tpc_tracks","event/data/tpc/tracks");
  //  tpc_tracks->Save();
  chain->PrintInfo();
// Init the chain and all its makers
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
//  chain->MakeTree("StChainTree","Title");
// Prepare TCanvas to show some histograms created by makers
  if (xdf_out){
    gBenchmark->Start("xdf out");
    xdf_out->NextEventPut(chain->DataSet("params")); // xdf output
    gBenchmark->Stop("xdf out");
  }
  if (root_out) {
    gBenchmark->Start("root i/o");
    root_out->cd();
    St_DataSet *run = chain->DataSet("params");// root output
    run->SetWrite();
    gBenchmark->Stop("root i/o");
  }
  gBenchmark->Start("bfc");
  Int_t i=0;
  for (Int_t i =1; i <= Nevents; i++){
    if (chain->Make(i)) break;
    St_DataSet *dst = chain->DataSet("tpc_tracks");
    if (dst) {
      if (xdf_out){
	gBenchmark->Start("xdf out");
	xdf_out->NextEventPut(dst); // xdf output
	gBenchmark->Stop("xdf out");
      }
      if (root_out){
	gBenchmark->Start("root i/o");
	root_out->cd();
	//      dst->SetWrite();// root output
	chain->FillClone();
	gBenchmark->Stop("root i/o");
      }
    }
    //    root_tree->cd();
    //    printf ("Fill Tree\n");
    //    chain->FillTree();
    //  histCanvas->Modified();
    //  histCanvas->Update();
    if (i != Nevents) chain->Clear();
    printf ("===========================================\n");
    printf ("=========================================== Done with Event no. %d\n",i);
    printf ("===========================================\n");
  }
  if (Nevents > 1) {
    chain->Finish();
    delete xdf_in;
    if (xdf_out){
      delete xdf_out;
      gBenchmark->Print("xdf out");
    }
    if (root_out){
      root_out->Write();   
      root_out->Close();   
      delete root_out;
      gBenchmark->Print("root i/o");
    }
    gBenchmark->Print("bfc");
  }
  else {if (!b)   b = new TBrowser;}
}
