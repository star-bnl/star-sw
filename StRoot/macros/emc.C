// $Id: emc.C,v 1.1 1999/01/21 00:54:27 fisyak Exp $
// $Log: emc.C,v $
// Revision 1.1  1999/01/21 00:54:27  fisyak
// Cleanup
//
void emc(const Int_t  Nevents=1,
         const Char_t *fileinp = "/afs/rhic/star/data/samples/hijet-g2t.xdf",
         const Char_t *fileout=0,
         const Char_t *FileOut=0)
{
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
  gSystem->Load("libmsg");
  gSystem->Load("libtls");
  gSystem->Load("St_params_Maker");
  gSystem->Load("St_xdfin_Maker");
  gSystem->Load("St_calib_Maker");
  gSystem->Load("St_evg_Maker");
  gSystem->Load("geometry");
  gSystem->Load("St_geant_Maker");
  gSystem->Load("St_TLA_Maker");
  gSystem->Load("tpc");
  gSystem->Load("St_tpc");
  gSystem->Load("St_tss_Maker");
  gSystem->Load("St_tcl_Maker");
  gSystem->Load("St_tpt_Maker");
  gSystem->Load("global");
  gSystem->Load("St_global");
  gSystem->Load("St_dst_Maker");
  gSystem->Load("St_run_summary_Maker");
  gSystem->Load("svt");
  gSystem->Load("St_svt");
  gSystem->Load("St_srs_Maker");
  gSystem->Load("St_stk_Maker");
  gSystem->Load("emc");
  gSystem->Load("St_emc");
  gSystem->Load("St_ems_Maker");
  gSystem->Load("St_emc_Maker");

  St_XDFFile   *xdf_in   = 0;
  if (fileinp)  xdf_in   = new St_XDFFile(fileinp,"r");
  St_XDFFile  *xdf_out   = 0;
  if (fileout) xdf_out   = new St_XDFFile(fileout,"w");
  TFile       *root_out  = 0; 
  if (FileOut) root_out  = new TFile(FileOut,"RECREATE");
//TFile      *root_tree= new TFile("auau_central_hijing.tree.root","RECREATE");
//Create the main chain object
  StChain chain("emc");
//  StChainSpy chain("emc");

  St_params_Maker params("params","run/params");
  St_TLA_Maker       *geom = new St_TLA_Maker("geom","run/geant/Run");
  St_geant_Maker    geant("geant","event/geant/Event");
  if (xdf_in) {
    St_xdfin_Maker xdfin("xdfin");
    chain.SetInputXDFile(xdf_in);
    //    St_DataSet *ev = chain.XDFFile()->NextEventGet();
    //    St_DataSet *ev = xdf_in->NextEventGet();
    //delete ev;
  }
  St_calib_Maker    calib("calib","calib"); 
  St_ems_Maker      emc_raw("emc_raw","event/raw_data/emc");
  St_emc_Maker      emc_hits("emc_hits","event/data/emc/hits");
  chain.PrintInfo();
// Init the mai chain and all its makers
  int iInit = chain.Init();
  if (iInit) chain.Fatal(iInit,"on init");
//  chain.MakeTree("StChainTree","Title");
// Prepare TCanvas to show some histograms created by makers
  if (xdf_out){
    gBenchmark->Start("xdf out");
    xdf_out->NextEventPut(chain.DataSet("params")); // xdf output
    gBenchmark->Stop("xdf out");
  }
  if (root_out) {
    gBenchmark->Start("root i/o");
    root_out->cd();
    St_DataSet *run = chain.DataSet("params");// root output
    run->SetWrite();
    gBenchmark->Stop("root i/o");
  }
  gBenchmark->Start("emc");
  Int_t i=0;
  for (Int_t i =1; i <= Nevents; i++){
    if (chain.Make(i)) break;
    St_DataSet *dst = chain.DataSet("dst");
    if (xdf_out){
      gBenchmark->Start("xdf out");
      xdf_out->NextEventPut(dst); // xdf output
      gBenchmark->Stop("xdf out");
    }
    if (root_out){
      gBenchmark->Start("root i/o");
      root_out->cd();
      dst->SetWrite();// root output
      gBenchmark->Stop("root i/o");
    }
    //    root_tree->cd();
    //    printf ("Fill Tree\n");
    //    chain.FillTree();
    //  histCanvas->Modified();
    //  histCanvas->Update();
    if (i != Nevents) chain.Clear();
    printf ("===========================================\n");
    printf ("=========================================== Done with Event no. %d\n",i);
    printf ("===========================================\n");
  }
  if (Nevents > 1) {
    chain.Finish();
    delete xdf_in;
    if (xdf_out){
      delete xdf_out;;
      gBenchmark->Print("xdf out");
    }
    if (root_out){
      root_out->Close();   
      delete root_out;
      gBenchmark->Print("root i/o");
    }
    gBenchmark->Print("emc");
  }
  else TBrowser b;
  //  TBrowser b;
  //  dst_emc::ShowHist();
}
