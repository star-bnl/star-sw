// $Id: bfc.C,v 1.11 1998/09/15 20:55:35 fisyak Exp $
// $Log: bfc.C,v $
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
{
#pragma includepath "/afs/rhic/star/packages/dev/StRoot"
#pragma includepath "/afs/rhic/star/packages/dev/lib"
  //#include "macros/bfc.h"
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


//gSystem.Exec("rm *.log");
//  Char_t *filename = "/afs/rhic/star/data/samples/auau_central_hijing.xdf";
  Char_t *filename = "/disk1/star/auau200/year1a/central/hijing/set0001/regular/gst/auau_ce_b0-2_1001_1050.xdf";
  St_XDFFile *xdf_in   = new St_XDFFile(filename,"r");
  St_XDFFile *xdf_out  = new St_XDFFile("auau_central_hijing.xdf","w");
  TFile      *root_out=  new TFile("auau_central_hijing.root","RECREATE");
//TFile      *root_tree= new TFile("auau_central_hijing.tree.root","RECREATE");
// Create the main chain object
  StChain chain("StChain");
//  Create the makers to be called by the current chain
  St_run_Maker run_Maker("run_Maker","run/params");
  St_xdfin_Maker xdfin("xdfin_Maker","event/geant");
//St_calib_Maker calib("calib_Maker","run/calib"); 
  chain.SetInputXDFile(xdf_in);
//  St_evg_Maker evg_Maker("evg_Maker","event");
  St_srs_Maker srs_Maker("srs_Maker","event/data/svt/hits");
  //  St_fss_Maker fss_Maker("fss_Maker","event/raw_data/ftpc/pixels");
  //  St_tss_Maker tss_Maker("tss_Maker","event/raw_data/tpc");
  St_tcl_Maker tcl_Maker("tcl_Maker","event/data/tpc/hits");
  St_stk_Maker stk_Maker("stk_Maker","event/data/svt/tracks");
  St_tpt_Maker tpt_Maker("tpt_Maker","event/data/tpc/tracks");
  St_dst_Maker dst_Maker("dst_Maker","event/data/global");
// Set parameters
//  tss_Maker.adcxyzon();
  chain.PrintInfo();
// Init the mai chain and all its makers
  chain.Init();
//  chain.MakeTree("StChainTree","Title");
// Prepare TCanvas to show some histograms created by makers
  //  xdf_out->NextEventPut(chain.GetRun()); // xdf output
  root_out->cd();
  chain.GetRun()->Write();// root output
  gBenchmark->Start("bfc");
  for (Int_t i=0;i<1;i++){
    chain.Make(i);
    St_DataSetIter local(chain.DataSet());
    local.Cd(chain.GetName());
    St_DataSet *evnt = local("event");
    xdf_out->NextEventPut(evnt); // xdf output
    root_out->cd();
    evnt->Write();// root output
    //    root_tree->cd();
    //    printf ("Fill Tree\n");
    //    chain.FillTree();
  //  histCanvas->Modified();
  //  histCanvas->Update();
  //  chain.Clear();
  }
  //  chain.Finish();
  gBenchmark->Stop("bfc");
  gBenchmark->Print("bfc");
  // delete xdf_in;
  // delete xdf_out;;
  // root_out->Close();   
  // delete root_out;
  TBrowser b;
}
