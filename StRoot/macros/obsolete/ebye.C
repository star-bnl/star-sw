// $Id: ebye.C,v 1.1 1998/08/05 14:33:40 fisyak Exp $
// $Log: ebye.C,v $
// Revision 1.1  1998/08/05 14:33:40  fisyak
// Add ebye
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
   gSystem->Load("$STAR/lib/libasu.so");
   gSystem->Load("$STAR/lib/libdsl.so");
   gSystem->Load("$STAR/lib/St_base.so");
   gSystem->Load("$STAR/lib/St_Tables.so");
   gSystem->Load("$STAR/lib/libmsg.so");
   gSystem->Load("$STAR/lib/libtls.so");
   //   gSystem->Load("$STAR/lib/geometry.sl");
   gSystem->Load("$STAR/lib/ebye.sl");
   gSystem->Load("$STAR/lib/St_ebye.so");
   gSystem->Load("$STAR/lib/tpc.sl");
   gSystem->Load("$STAR/lib/St_tpc.so");
   gSystem->Load("$STAR/lib/svt.sl");
   gSystem->Load("$STAR/lib/St_svt.so");
   gSystem->Load("$STAR/lib/StChain.so"); 

#ifndef __CINT__
#include "Rtypes.h"
#include "St_XDFFile.h"
#include "St_DataSet.h"
#include "St_Module.h"
#include "St_Table.h"
#endif
  Char_t *filename = "/star/u2/dhammika/data/DST_prodrun2_evt1-99.xdf";
  St_XDFFile xdffile_in(filename,"r");
// Create the main chain object
  StChain chain("StChain");
//  Create the makers to be called by the current chain
  St_xdfin_Maker xdfin("xdfin_Maker","/");
  chain.SetInputXDFile(&xdffile_in);
  St_ebye_Maker ebye_Maker("ebye_Maker","event/ebey/sca");

  chain.PrintInfo();
// Init the mai chain and all its makers
  chain.Init();
  ebye_Maker.SetmakePrior(kTRUE);

// Prepare TCanvas to show some histograms created by makers
  gBenchmark->Start("ebye");

for (Int_t i=0;i<1;i++){
  chain.Make(i);
  //  histCanvas->Modified();
  //  histCanvas->Update();
  chain.Clear();
}
  gBenchmark->Stop("bfc");
  gBenchmark->Print("bfc");
  TBrowser b;
}
