// $Id: bfc.C,v 1.24 1998/11/07 02:45:05 fisyak Exp $
// $Log: bfc.C,v $
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
void bfc(
         const Char_t *fileinp = 
"/disk1/star/auau200/hijing135/default/b0_3/year2a/hadronic_on/g2t/psc091_03_34evts.xdf",
         const Char_t *fileout=
"/disk1/star/auau200/hijing135/default/b0_3/year2a/hadronic_on/root/psc091_03_34evts_dst.xdf",
         const Char_t *FileOut=
"/disk1/star/auau200/hijing135/default/b0_3/year2a/hadronic_on/root/psc091_03_34evts_dst.root",
         const Int_t  Nevents=1000)
{
  gSystem->Load("St_base.so");
  gSystem->Load("StChain.so");
  gSystem->Load("xdf2root.so");
  gSystem->Load("St_Tables.so");
  gSystem->Load("libmsg.so");
  gSystem->Load("libtls.so");
  gSystem->Load("St_params_Maker.so");
  gSystem->Load("St_xdfin_Maker.so");
  gSystem->Load("St_calib_Maker.so");
  gSystem->Load("St_evg_Maker.so");
  gSystem->Load("St_geant_Maker.so");
  gSystem->Load("St_geom_Maker.so");
  gSystem->Load("St_TLA_Maker.so");
  gSystem->Load("tpc.sl");
  gSystem->Load("St_tpc.so");
  gSystem->Load("St_tss_Maker.so");
  gSystem->Load("St_tcl_Maker.so");
  gSystem->Load("St_tpt_Maker.so");
  gSystem->Load("global.sl");
  gSystem->Load("St_global.so");
  gSystem->Load("St_dst_Maker.so");
  gSystem->Load("St_run_summary_Maker.so");
  gSystem->Load("svt.sl");
  gSystem->Load("St_svt.so");
  gSystem->Load("St_srs_Maker.so");
  gSystem->Load("St_stk_Maker.so");
  //  gSystem->Load("ftpc.sl");
  //  gSystem->Load("St_ftpc.so");
  //  gSystem->Load("St_fss_Maker.so");
  //  gSystem->Load("St_fcl_Maker.so");
  //  gSystem->Load("St_fpt_Maker.so");
  St_XDFFile   *xdf_in   = 0;
  if (fileinp)  xdf_in   = new St_XDFFile(fileinp,"r");
  St_XDFFile  *xdf_out   = 0;
  if (fileout) xdf_out   = new St_XDFFile(fileout,"wb");
  TFile       *root_out  = 0; 
  if (FileOut) root_out  =  new TFile(FileOut,"RECREATE");
//TFile      *root_tree= new TFile("auau_central_hijing.tree.root","RECREATE");
// Create the main chain object
  //  StChain chain("bfc");
  StChainSpy chain("bfc");

//  Create the makers to be called by the current chain
  St_params_Maker params("params","run/params");
  St_geom_Maker     geom("geom","run/geant/Run");
  if (xdf_in) {
    St_xdfin_Maker xdfin("xdfin");
    chain.SetInputXDFile(xdf_in);
  }
  St_calib_Maker    calib("calib","calib"); 
  St_evg_Maker      evgen("evgen","event/evgen");
  St_geant_Maker    geant("geant","event/geant/Event");
//  St_fss_Maker   ftpc_raw("ftpc_raw","event/raw_data/ftpc");
//  St_tss_Maker    tpc_raw("tpc_raw","event/raw_data/tpc");
// Set parameters
//  tpc_raw.adcxyzon();
  St_srs_Maker         svt_hits("svt_hits","event/data/svt/hits");
  St_tcl_Maker         tpc_hits("tpc_hits","event/data/tpc/hits");
  St_TLA_Maker         ctf_hits("ctf_hits","event/data/ctf/hits");
//  St_fcl_Maker         fcl_hits("ftpc_hits","event/data/ftpc/hits");
  St_stk_Maker       stk_tracks("svt_tracks","event/data/svt/tracks");
  St_tpt_Maker       tpc_tracks("tpc_tracks","event/data/tpc/tracks");
//  St_TLA_Maker      ftpc_tracks("ftpc_tracks","event/data/ftpc/tracks");
  St_TLA_Maker           global("global","event/data/global");
  St_dst_Maker        dst_Maker("dst","event/data/global/dst");
  St_run_summary_Maker  summary("run_summary","run/dst");
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
  gBenchmark->Start("bfc");
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
    gBenchmark->Print("bfc");
  }
#if 0
  else TBrowser b;
#endif
}
