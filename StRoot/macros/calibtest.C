//  
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
 //  gSystem->Load("global.sl");
 //  gSystem->Load("St_global.so");
//   gSystem->Load("St_dst_Maker.so");
//   gSystem->Load("St_run_Maker.so");
   gSystem->Load("St_xdfin_Maker.so");
   gSystem->Load("St_calib_Maker.so");


//gSystem.Exec("rm *.log");
//  Char_t *filename = "/afs/rhic/star/data/samples/auau_central_hijing.xdf";
  Char_t *filename = "/disk1/star/auau200/year1a/central/hijing/set0001/regular/gst/auau_ce_b0-2_1001_1050.xdf";
  St_XDFFile *xdf_in   = new St_XDFFile(filename,"r");
//  St_XDFFile *xdf_out  = new St_XDFFile("auau_central_hijing.xdf","w");
//  TFile      *root_out=  new TFile("auau_central_hijing.root","RECREATE");
//TFile      *root_tree= new TFile("auau_central_hijing.tree.root","RECREATE");
// Create the main chain object
  StChain chain("StChain");
//  Create the makers to be called by the current chain
//  St_run_Maker run_Maker("run_Maker","run/params");
//  St_xdfin_Maker xdfin("xdfin_Maker","event/geant");
  St_calib_Maker calib("calib_Maker","run/calib"); 
  chain.SetInputXDFile(xdf_in);
  chain.PrintInfo();
// Init the mai chain and all its makers
  chain.Init();
//  chain.MakeTree("StChainTree","Title");
// Prepare TCanvas to show some histograms created by makers
  //  xdf_out->NextEventPut(chain.GetRun()); // xdf output
 // root_out->cd();
  gBenchmark->Start("bfc");
  for (Int_t i=0;i<1;i++){
    calib.SetValidTime(19980101,0);
    chain.Make(i);
    St_DataSetIter local(chain.DataSet());
    local.Cd(chain.GetName());
    St_DataSet *evnt = local("event");
    // xdf_out->NextEventPut(evnt); // xdf output
   // root_out->cd();
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
//  calib.DataSet()->ls("*");
}
