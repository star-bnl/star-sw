// $Id: laser.C,v 1.4 1998/08/26 12:15:15 fisyak Exp $
// $Log: laser.C,v $
// Revision 1.4  1998/08/26 12:15:15  fisyak
// Remove asu & dsl libraries
//
// Revision 1.3  1998/08/20 12:33:33  fisyak
// Splitted base libraries
//
// Revision 1.2  1998/08/14 18:18:14  love
// An example analysis of 10 events
//
// Revision 1.1  1998/08/10 02:35:13  fisyak
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
   gSystem->Load("xdf2root.so");
   gSystem->Load("St_Tables.so");
   gSystem->Load("libmsg.so");
   gSystem->Load("libtls.so");
   gSystem->Load("svt.sl");
   gSystem->Load("St_svt.so");
   gSystem->Load("tpc.sl");
   gSystem->Load("St_tpc.so");
   gSystem->Load("StChain.so");

#ifndef __CINT__
#include "Rtypes.h"
#include "St_XDFFile.h"
#include "St_DataSet.h"
#include "St_Module.h"
#include "St_Table.h"
#endif
  Char_t *filename = "/scr20/love/star/test/log-p274-t23-f6-las.xdf"; 
  St_XDFFile xdffile_in(filename,"r");
// Create the main chain object
  StChain chain("StChain");
//  Create the makers to be called by the current chain
  St_xdfin_Maker xdfin("xdfin_Maker","event/raw_data/tpc");
  chain.SetInputXDFile(&xdffile_in);
  St_run_Maker run_Maker("run_Maker","run/params");
  St_laser_Maker laser_Maker("laser_Maker","event");
  //  St_tcl_Maker tcl_Maker("tcl_Maker","event/data/tpc/hits");
  //  St_tpt_Maker tpt_Maker("tpt_Maker","event/data/tpc/tracks");
  // Set parameters
  //  tss_Maker.adcxyzon();
  chain.PrintInfo();
// Init the mai chain and all its makers
   gBenchmark->Start("laser");
   chain.Init();
   TFile f("fntup.root","RECREATE");
// Prepare TCanvas to show some histograms created by makers
  gBenchmark->Start("laser");
  // Skip one bad event.
  St_DataSet *set = xdffile_in.NextEventGet();  
  delete set;
  for (Int_t i=0;i<10;i++){
    chain.Clear();
    chain.Make(i);
  //  histCanvas->Modified();
  //  histCanvas->Update();
  //chain.Clear();
  }
  laser_Maker.Histograms()->Write();
  gBenchmark->Stop("laser");
  gBenchmark->Print("laser");
  TBrowser b;
}
