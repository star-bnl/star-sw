// $Id: bfc.C,v 1.19 1998/10/18 21:20:49 fisyak Exp $
// $Log: bfc.C,v $
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
void bfc(const Char_t *fileinp = 
"/disk1/star/auau200/hijing135/default/b0_20/year2a/hadronic_on/g2t/psc262_02_142evts.xdf", 
         const Char_t *fileout=
"/disk1/star/auau200/hijing135/default/b0_20/year2a/hadronic_on/root/psc262_02_142evts_dst.xdf",
         const Char_t *FileOut=
"/disk1/star/auau200/hijing135/default/b0_20/year2a/hadronic_on/root/psc262_02_142evts_dst.root",
         const Int_t  Nevents=1000)
{
   gSystem->Load("St_base.so");
   gSystem->Load("StChain.so");
   gSystem->Load("xdf2root.so");
   gSystem->Load("St_Tables.so");
   gSystem->Load("libmsg.so");
   gSystem->Load("libtls.so");
   gSystem->Load("tpc.sl");
   gSystem->Load("St_tpc.so");
   gSystem->Load("St_tss_Maker.so");
   gSystem->Load("St_tcl_Maker.so");
   gSystem->Load("St_tpt_Maker.so");
   gSystem->Load("svt.sl");
   gSystem->Load("St_svt.so");
   gSystem->Load("St_srs_Maker.so");
   gSystem->Load("St_stk_Maker.so");
   //   gSystem->Load("ftpc.sl");
   //   gSystem->Load("St_ftpc.so");
   //   gSystem->Load("St_fss_Maker.so");
   //   gSystem->Load("St_fcl_Maker.so");
   //   gSystem->Load("St_fpt_Maker.so");
   gSystem->Load("global.sl");
   gSystem->Load("St_global.so");
   gSystem->Load("St_dst_Maker.so");
   gSystem->Load("St_run_Maker.so");
   gSystem->Load("St_xdfin_Maker.so");


   //  Char_t *fileinp ="$SCRATCH/input.xdf";
   //  Char_t *fileout ="$SCRATCH/ouput.xdf";
   //  Char_t *FileOut ="$SCRATCH/ouput.root";
  St_XDFFile   *xdf_in   = 0;
  if (fileinp)  xdf_in   = new St_XDFFile(fileinp,"r");
  St_XDFFile  *xdf_out   = 0;
  if (fileout) xdf_out   = new St_XDFFile(fileout,"wb");
  TFile       *root_out  = 0; 
  if (FileOut) root_out  =  new TFile(FileOut,"RECREATE");
//TFile      *root_tree= new TFile("auau_central_hijing.tree.root","RECREATE");
// Create the main chain object
//  StChain chain("StChain");
  StChainSpy chain("StChain");

//  Create the makers to be called by the current chain
  St_run_Maker run_Maker("run_Maker","run/params");
  if (xdf_in) {
    St_xdfin_Maker xdfin("xdfin_Maker","event/geant");
    chain.SetInputXDFile(xdf_in);
  }
//St_calib_Maker calib("calib_Maker","run/calib"); 
//  St_evg_Maker evg_Maker("evg_Maker","event");
  St_srs_Maker srs_Maker("srs_Maker","event/data/svt/hits");
  //  St_fss_Maker fss_Maker("fss_Maker","event/raw_data/ftpc/pixels");
  //  St_tss_Maker tss_Maker("tss_Maker","event/raw_data/tpc");
// Set parameters
//  tss_Maker.adcxyzon();
  St_tcl_Maker tcl_Maker("tcl_Maker","event/data/tpc/hits");
  St_stk_Maker stk_Maker("stk_Maker","event/data/svt/tracks");
  St_tpt_Maker tpt_Maker("tpt_Maker","event/data/tpc/tracks");
  St_dst_Maker dst_Maker("dst_Maker","event/data/global");
  chain.PrintInfo();
// Init the mai chain and all its makers

  int iInit = chain.Init();
  if (iInit) chain.Fatal(iInit,"on init");
//  chain.MakeTree("StChainTree","Title");
// Prepare TCanvas to show some histograms created by makers
  if (xdf_out){
    gBenchmark->Start("xdf out");
    xdf_out->NextEventPut(chain.GetRun()); // xdf output
    gBenchmark->Stop("xdf out");
  }
  if (root_out) {
    gBenchmark->Start("root i/o");
    root_out->cd();
    St_DataSet *run = chain.GetRun();// root output
    run->SetWrite();
    gBenchmark->Stop("root i/o");
  }
  gBenchmark->Start("bfc");
  Int_t i=0;
  for (Int_t i =1; i <= Nevents; i++){
    if (chain.Make(i)) break;
    St_DataSetIter local(chain.DataSet());
    local.Cd(chain.GetName());
    St_DataSet *evnt = local("event");
    if (xdf_out){
      gBenchmark->Start("xdf out");
      xdf_out->NextEventPut(evnt); // xdf output
      gBenchmark->Stop("xdf out");
    }
    if (root_out){
      gBenchmark->Start("root i/o");
      root_out->cd();
      evnt->SetWrite();// root output
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
  else TBrowser b;
}
