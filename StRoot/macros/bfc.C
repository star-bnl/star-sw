// $Id: bfc.C,v 1.31 1999/01/02 19:08:25 fisyak Exp $
// $Log: bfc.C,v $
// Revision 1.31  1999/01/02 19:08:25  fisyak
// Add ctf
//
// Revision 1.30  1998/12/29 19:38:22  fine
// STAR_shapes: test of the brand new St_NodeView class has been added
//
// Revision 1.29  1998/12/27 03:17:43  fine
// STAR_shapes to test two brand new classes: St_Node St_NodePosition
//
// Revision 1.28  1998/12/21 19:45:49  fisyak
// Move ROOT includes to non system
//
// Revision 1.27  1998/12/12 02:38:43  fisyak
// Clean up
//
// Revision 1.26  1998/12/10 01:43:46  fisyak
// Remove geant
//
// Revision 1.25  1998/11/25 21:58:36  fisyak
// Cleanup
//
// Revision 1.24  1998/11/07 02:45:05  fisyak
// cleanup analysis
//
// Revision 1.23  1998/11/06 01:42:18  fisyak
// Add ana.C
//
// Revision 1.22  1998/11/01 16:42:28  fisyak
// dst analysis
//
// Revision 1.21  1998/10/31 00:26:26  fisyak
// Makers take care about branches
//
// Revision 1.20  1998/10/21 20:30:56  fine
// makedoc macro creates "gif" directories and fill it up
//
// Revision 1.19  1998/10/18 21:20:49  fisyak
// typo
//
// Revision 1.18  1998/10/12 00:53:02  fisyak
// Add parameters for bfc
//
// Revision 1.17  1998/09/27 01:24:22  fisyak
// bfc.C for whole file
//
// Revision 1.16  1998/09/26 00:35:31  fisyak
// Add real files
//
// Revision 1.15  1998/09/26 00:17:27  fisyak
// Add SetWrite
//
// Revision 1.13  1998/09/23 20:23:23  fisyak
// Prerelease SL98h
//
// Revision 1.12  1998/09/18 14:35:33  fisyak
// Fix makers
//
// Revision 1.11  1998/09/15 20:55:35  fisyak
// Split St_DataSet -> St_DataSet + St_DataSetIter
//
// Revision 1.10  1998/08/26 12:15:15  fisyak
// Remove asu & dsl libraries
//
// Revision 1.9  1998/08/20 12:33:32  fisyak
// Splitted base libraries
//
// Revision 1.8  1998/08/18 14:05:08  fisyak
// Add to bfc dst
//
// Revision 1.7  1998/08/10 02:35:13  fisyak
// add laser
//
// Revision 1.6  1998/07/23 11:32:42  fisyak
// Small fixes
//
// Revision 1.5  1998/07/21 13:35:14  fine
// The new version of the macros: MakeHtmlTables and makedoc have been introduced
//
// Revision 1.4  1998/07/21 01:04:41  fisyak
// Clean up
//
// Revision 1.3  1998/07/21 00:36:49  fisyak
// tcl and tpt
//
// Revision 1.2  1998/07/20 15:08:19  fisyak
// Add tcl and tpt
//
TBrowser *b = 0;
class StChain;
StChain  *chain=0;
void Load(){
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("xdf2root");
    gSystem->Load("St_Tables");
    gSystem->Load("libmsg");
    gSystem->Load("libtls");
    gSystem->Load("St_params_Maker");
    //    gSystem->Load("St_db_Maker");
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
    gSystem->Load("ftpc");
    gSystem->Load("St_ftpc");
    gSystem->Load("St_fss_Maker");
    gSystem->Load("St_fcl_Maker");
    gSystem->Load("St_fpt_Maker");
    gSystem->Load("emc");
    gSystem->Load("St_emc");
    gSystem->Load("St_ems_Maker");
    gSystem->Load("St_emc_Maker");
    gSystem->Load("ctf");
    gSystem->Load("St_ctf");
    gSystem->Load("St_ctf_Maker");
    gSystem->Load("svt");
    gSystem->Load("St_svt");
    gSystem->Load("St_srs_Maker");
    gSystem->Load("St_stk_Maker");
    gSystem->Load("strange");
    gSystem->Load("St_strange");
    gSystem->Load("global");
    gSystem->Load("St_global");
    gSystem->Load("St_dst_Maker");
    gSystem->Load("St_run_summary_Maker");
}
bfc(const Int_t   Nevents=1,
    //    const Char_t *fileinp = "/disk1/star/auau200/hijing135/default/b0_3/year2a/hadronic_on/g2t/psc091_03_34evts.xdf",
    const Char_t *fileinp = "/afs/rhic/star/data/samples/hijet-g2t.xdf",
    //    const Char_t *fileinp = "/disk1/star/kathy/year2a_psc079_01_46evts.xdf",
    const Char_t *fileout =0,
    //    const Char_t *fileout = "hijet-bfc.xdf",
    const Char_t *FileOut = "hijet-bfc.root")
{
  // Dynamically link some shared libs
  if (gClassTable->GetID("StChain") < 0) Load();
  St_XDFFile   *xdf_in   = 0;
  if (fileinp)  xdf_in   = new St_XDFFile(fileinp,"r");
  St_XDFFile  *xdf_out   = 0;
  if (fileout) xdf_out   = new St_XDFFile(fileout,"wb");
  TFile       *root_out  = 0; 
  if (FileOut) root_out  =  new TFile(FileOut,"RECREATE");
  //TFile      *root_tree= new TFile("auau_central_hijing.tree.root","RECREATE");
  // Create the main chain object
  if (chain) delete chain;
  chain = new StChain("bfc");
  //  StChainSpy chain("bfc");
  
  //  Create the makers to be called by the current chain
  St_params_Maker *params = new St_params_Maker("params","params");
  St_geom_Maker     *geom = new St_geom_Maker("geom","run/geant/Run");
  if (xdf_in) {
    St_xdfin_Maker *xdfin = new St_xdfin_Maker("xdfin");
    chain->SetInputXDFile(xdf_in);
  }
  //  St_calib_Maker    *calib = new St_calib_Maker("calib","calib"); 
  //  St_evg_Maker      *evgen = new St_evg_Maker("evgen","event/evgen");
  St_geant_Maker    *geant = new St_geant_Maker("geant","event/geant/Event");
  //  St_fss_Maker   *ftpc_raw = new St_fss_Maker("ftpc_raw","event/raw_data/ftpc");
  //  St_tss_Maker    *tpc_raw = new St_tss_Maker("tpc_raw","event/raw_data/tpc");
  // Set parameters
  //  tpc_raw->adcxyzon();
  St_ems_Maker          *emc_raw = new St_ems_Maker("emc_raw","event/raw_data/emc");

  St_emc_Maker         *emc_hits = new St_emc_Maker("emc_hits","event/data/emc/hits");
  St_tcl_Maker         *tpc_hits = new St_tcl_Maker("tpc_hits","event/data/tpc/hits");
  St_srs_Maker         *svt_hits = new St_srs_Maker("svt_hits","event/data/svt/hits");
  St_fcl_Maker         *fcl_hits = new St_fcl_Maker("ftpc_hits","event/data/ftpc/hits");
  //  St_ctf_Maker         *ctf      = new St_ctf_Maker("ctf","event/data/ctf");
  St_tpt_Maker       *tpc_tracks = new St_tpt_Maker("tpc_tracks","event/data/tpc/tracks");
  St_stk_Maker       *stk_tracks = new St_stk_Maker("svt_tracks","event/data/svt/tracks");
  //St_TLA_Maker      *ftpc_tracks = new St_TLA_Maker("ftpc_tracks","event/data/ftpc/tracks");
  St_glb_Maker           *global = new St_glb_Maker("global","event/data/global");
  St_dst_Maker        *dst_Maker = new St_dst_Maker("dst","dst");
  //  St_dst_Maker        *dst_Maker = new St_dst_Maker("dst","event/data/global/dst");
  //  St_run_summary_Maker  *summary = new St_run_summary_Maker("run_summary","run/dst");
  
  // Create HTML docs of all Maker's involved
  //  chain->MakeDoc();
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
    St_DataSet *dst = chain->DataSet("dst");
    if (dst) {
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
      delete xdf_out;;
      gBenchmark->Print("xdf out");
    }
    if (root_out){
      root_out->Close();   
      delete root_out;
      gBenchmark->Print("root i/o");
    }
    gBenchmark->Print("bfc");
  }
  else {if (!b)   b = new TBrowser;}
}
