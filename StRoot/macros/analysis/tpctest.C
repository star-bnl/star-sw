// $Id: tpctest.C,v 1.3 1998/11/16 02:43:26 fisyak Exp $
// $Log: tpctest.C,v $
// Revision 1.3  1998/11/16 02:43:26  fisyak
// new tpc
//
// Revision 1.2  1998/09/16 17:58:45  love
// tpctest slice plotting added
//
// Revision 1.1  1998/09/08 13:53:33  love
// laser.C divided into load.C and a new laser.C package of routines.  tpctest.C created to use tpt tracking
//

//
/*************************************************************\
   tpctest.C is a CINT script package to analyse data from the
   1997 TPC test at LBL which included both Cosmic Ray 
   triggers and laser events.  It (and the St_tpctest_Maker code)
   is derived from the tst.kumac of Iwona Sakrejda modified to
   use the regular tpt tracking code.
   There are four functions:  start() defines and
   initializes the system.  loop(n) analyses the next n events
   on the input file.  skip(n) skips n input records.  end()
   writes the final ntuple to the output.-- WALove 2 Sept 1998
\*************************************************************/
 
// Define globals over this package

Int_t ievt=0;  //local event counter 
#include "iostream.h"
#pragma includepath "/afs/rhic/star/packages/dev/StRoot/StChain"
class  StChain;
StChain *chain = 0;
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
  gSystem->Load("libmsg");
  gSystem->Load("libtls");
  gSystem->Load("tpc");
  gSystem->Load("St_tpc");
  gSystem->Load("St_TLA_Maker");
  gSystem->Load("St_xdfin_Maker");
  gSystem->Load("St_tpctest_Maker");
  gSystem->Load("St_params_Maker");
}
void start(){
#ifndef __CINT__
#include "Rtypes.h"
#include "St_XDFFile.h"
#include "St_DataSet.h"
#include "St_Module.h"
#include "St_Table.h"
#endif
// The following file is 1039 full 42+ track laser events.
// Char_t *filename="/star/mds/data/SD97/cosmic/Sept/log-p274-t23-f6-las.xdf";
   Char_t *filename="/disk1/star/SD97/cosmic/Sept/log-p274-t23-f6-las.xdf"; 
// The following file is 44 events of central membrane pattern only.
// Char_t *filename="/star/mds/data/SD97/cosmic/Sept/log-p285-t28-f6-las.xdf";
//  Char_t *filename="/scr20/love/star/test/log-p285-t28-f6-las.xdf";
// The following file is >3000 cosmic ray triggers.
// Char_t *filename="/star/mds/data/SD97/cosmic/Sept/log-p284-t27-f4-cos.xdf";
//Char_t *filename="/scr20/love/star/test/log-p284-t27-f4-cos.xdf";
 
  St_XDFFile *xdffile_in = new St_XDFFile (filename,"r");
// Create the main chain object
  chain = new StChain("StChain");
//  Create the makers to be called by the current chain
 
  St_params_Maker *param = new St_params_Maker;
  St_xdfin_Maker *xdfin=new  St_xdfin_Maker;
  chain->SetInputXDFile(xdffile_in);
  St_TLA_Maker *mini_daq  = new St_TLA_Maker("tpc_raw","event/raw_data/tpc");   
  St_tpctest_Maker *tpc = new St_tpctest_Maker ("tpctest","event/data/tpc");
  // Set slice selector.   nslice <0 plots all z in one view. 
  // 0 < nslice < 7 plots only the slice selected.  
  // nslice =7 plots seven slices in succession.
   tpc->Set_nslice(7);
  //
  chain->PrintInfo();
   // Create a root file to hold the ntuple.
   TFile *f=new TFile("tptntup.root","RECREATE");
// Init the chain and all its makers
   chain->Init();
}
void loop(Int_t nevt=1){
  gBenchmark->Start("TPCtest"); // time the loop

  for (Int_t i=ievt;i<ievt+nevt;i++){
    chain->Clear();
    chain->Make(i+1);//Tell Make the event number - starts from 1.
  }
  ievt += nevt;
  gBenchmark->Stop("TPCtest");
  gBenchmark->Print("TPCtest");
  gBenchmark->Reset();
}
void skip(Int_t nskip=1){
  for (Int_t i=0;i<nskip;i++){
  St_DataSet *set =chain->XDFFile()->NextEventGet();  
  delete set;
  }
  ievt += nskip;// keep the count of events straight.
}
void end(){ chain->Maker("tpctest")->Histograms()->Write();}
void tpctest(){
  Load();
  start();
  skip(5);
  loop(5);
  end();
}
