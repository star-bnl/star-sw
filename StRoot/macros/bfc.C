{
//  gSystem->Load("lib/St_base.so");
//  gSystem->Load("lib/St_Tables.so");
//  gSystem->Load("lib/tpc.sl");
//  gSystem->Load("lib/St_tpc.so");
//  gSystem->Load("lib/StChain.so");

#ifndef __CINT__
#include "Rtypes.h"
#include "St_XDFFile.h"
#include "St_DataSet.h"
#include "St_Module.h"
#include "St_Table.h"
#endif
//gSystem.Exec("rm *.log");
  Char_t *filename = "/star/mds/data/SD98/auau200/bfc/central/hijing/set0001/regular/tss/auau_ce_b0-2_0001_0020.xdf";
  St_XDFFile xdffile_in(filename,"r");

// Create the main chain object
  StChain chain("StChain");

//  Create the makers to be called by the current chain
  St_xdfin_Maker xdfin("Xdfin");
//  St_evg_Maker evg("evg","event");
  St_tss_Maker tss("tss","event/raw_data/tpc");

  chain.PrintInfo();
  chain.SetInputXDFile(&xdffile_in);

// Init the mai chain and all its makers
  chain.Init();
// Prepare TCanvas to show some histograms created by makers
for (Int_t i=0;i<1;i++){
  chain.Make(i);
  //  histCanvas->Modified();
  //  histCanvas->Update();
}
  TBrowser b;
}
