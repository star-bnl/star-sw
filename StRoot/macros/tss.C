// $Id: tss.C,v 1.4 1998/08/20 12:33:33 fisyak Exp $
// $Log: tss.C,v $
// Revision 1.4  1998/08/20 12:33:33  fisyak
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
   gSystem->Load("St_base.so");
   gSystem->Load("libasu.so");
   gSystem->Load("libdsl.so");
   gSystem->Load("xdf2root.so");
   gSystem->Load("St_Tables.so");
   gSystem->Load("libmsg.so");
   gSystem->Load("libtls.so");
   gSystem->Load("tpc.sl");
   gSystem->Load("St_tpc.so");
   gSystem->Load("svt.sl");
   gSystem->Load("St_svt.so");
   gSystem->Load("global.sl");
   gSystem->Load("St_global.so");
   gSystem->Load("StChain.so");

#ifndef __CINT__
#include "Rtypes.h"
#include "St_XDFFile.h"
#include "St_DataSet.h"
#include "St_Module.h"
#include "St_Table.h"
#endif
//gSystem.Exec("rm *.log");
//  Char_t *filename = "/star/mds/data/SD98/auau200/bfc/central/hijing/set0001/regular/tss/auau_ce_b0-2_0001_0020.xdf";
//  Char_t *filename = "/afs/rhic/star/data/samples/event_0000050.xdf";
//  Char_t *filename = "/afs/rhic/star/data/samples/muons_100_ctb.dsl";
//  
  Char_t *filename = "/afs/rhic/star/data/samples/muons_100_ctb.dsl";
  //  Char_t *filename = "/disk1/star/tss/auau_ce_b0-2_4281_4300.xdf";
  St_XDFFile *xdf_in   = new St_XDFFile(filename,"r");
  St_XDFFile *xdf_out  = new St_XDFFile("muons_100_ctb.xdf","w");
  TFile      *root_out=  new TFile("muons_100_ctb.root","RECREATE");
  //  TFile      *root_tree= new TFile("muons_100_ctb.tree.root","RECREATE");
// Create the main chain object
  StChain chain("StChain");
//  Create the makers to be called by the current chain
  St_xdfin_Maker xdfin("xdfin_Maker","event/geant");
  chain.SetInputXDFile(xdf_in);
  St_run_Maker run_Maker("run_Maker","run/params");
//  St_evg_Maker evg_Maker("evg_Maker","event");
  St_srs_Maker srs_Maker("srs_Maker","event/data/svt/hits");
  St_tss_Maker tss_Maker("tss_Maker","event/raw_data/tpc");
  St_tcl_Maker tcl_Maker("tcl_Maker","event/data/tpc/hits");
  St_stk_Maker stk_Maker("stk_Maker","event/data/svt/tracks");
  St_tpt_Maker tpt_Maker("tpt_Maker","event/data/tpc/trackas");
//  St_dst_Maker dst_Maker("dst_Maker","event/data/dst");
// Set parameters
//  tss_Maker.adcxyzon();
  chain.PrintInfo();
// Init the mai chain and all its makers
  chain.Init();
//  chain.MakeTree("StChainTree","Title");
// Prepare TCanvas to show some histograms created by makers
  xdf_out->NextEventPut(chain.GetRun()); // xdf output
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
    printf ("Fill Tree\n");
    chain.FillTree();
  //  histCanvas->Modified();
  //  histCanvas->Update();
  //  chain.Clear();
  }
  chain.Finish();
  gBenchmark->Stop("bfc");
  gBenchmark->Print("bfc");
  // delete xdf_in;
  // delete xdf_out;;
  // root_out->Close();   
  // delete root_out;
  TBrowser b;
}
