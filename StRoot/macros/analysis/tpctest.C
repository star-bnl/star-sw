// $Id: tpctest.C,v 1.1 1998/09/08 13:53:33 love Exp $
// $Log: tpctest.C,v $
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
  StChain   *chain = 0;

void start(){

#ifndef __CINT__
#include "Rtypes.h"
#include "St_XDFFile.h"
#include "St_DataSet.h"
#include "St_Module.h"
#include "St_Table.h"
#endif

// Char_t *filename="/star/mds/data/SD97/cosmic/Sept/log-p274-t23-f6-las.xdf";
// Char_t *filename="/star/mds/data/SD97/cosmic/Sept/log-p285-t28-f6-las.xdf";
// Char_t *filename="/star/mds/data/SD97/cosmic/Sept/log-p284-t27-f4-cos.xdf";
// Char_t *filename="/scr20/love/star/test/log-p284-t27-f4-cos.xdf";
// Char_t *filename="/scr20/love/star/test/log-p274-t23-f6-las.xdf"; 
  Char_t *filename="/scr20/love/star/test/log-p285-t28-f6-las.xdf";
 
  St_XDFFile *xdffile_in = new St_XDFFile (filename,"r");
// Create the main chain object
  chain= new StChain("StChain");
//  Create the makers to be called by the current chain
  St_xdfin_Maker *xdfin=new  St_xdfin_Maker ("xdfin_Maker","event/raw_data/tpc");
  chain->SetInputXDFile(xdffile_in);
  St_run_Maker *run_Maker = new St_run_Maker ("run_Maker","run/params");
  St_tpctest_Maker *tpctest_Maker = new St_tpctest_Maker ("tpctest_Maker","event");
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
void end(){ chain->Maker("tpctest_Maker")->Histograms()->Write();}
