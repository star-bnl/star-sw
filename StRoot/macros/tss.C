{
   gSystem->Load("lib/St_base.so");
   gSystem->Load("lib/St_Tables.so");
   gSystem->Load("lib/libmsg.so");
   gSystem->Load("lib/libtls.so");
   gSystem->Load("lib/tpc.sl");
   gSystem->Load("lib/St_tpc.so");
   gSystem->Load("lib/svt.sl");
   gSystem->Load("lib/St_svt.so");
   gSystem->Load("lib/StChain.so");

#ifndef __CINT__
#include "Rtypes.h"
#include "St_XDFFile.h"
#include "St_DataSet.h"
#include "St_Module.h"
#include "St_Table.h"
#include "StChain.h"
#include "St_tss_Maker.h"
#endif
//gSystem.Exec("rm *.log");
//  Char_t *filename = "/afs/rhic/star/data/samples/event_0000050.xdf";
  Char_t *filename = "/afs/rhic/star/data/samples/muons_100_ctb.dsl";
  St_XDFFile xdffile_in(filename,"r");

// Create the main chain object
  StChain chain("StChain");
//  chain.SetDebug(1);
//  Create the makers to be called by the current chain
  St_xdfin_Maker xdfin("Xdfin","event/geant");
  chain.SetInputXDFile(&xdffile_in);
//  St_evg_Maker evg("evg","event");
  St_srs_Maker srs_Maker("srs_Maker","event/data/svt");
  St_tss_Maker tss_Maker("tss_Maker","event/raw_data/tpc");
// Set parameters
  tss_Maker.adcxyzon();
  chain.PrintInfo();
// Init the main chain and all its makers
  chain.Init();
// Prepare TCanvas to show some histograms created by makers
  gBenchmark->Start("tss");
for (Int_t i=0;i<1;i++){
  chain.Make(i);
  //  histCanvas->Modified();
  //  histCanvas->Update();
  //  chain.Clear();
}
  gBenchmark->Stop("tss");
  gBenchmark->Print("tss");
  TBrowser b;
}
