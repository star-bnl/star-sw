//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Macro for running chain with different inputs                        //
// owner:  Yuri Fisyak                                                  //
//                                                                      //
// $Id: bfcZ.C,v 1.1 2001/03/24 03:08:58 fisyak Exp $
//////////////////////////////////////////////////////////////////////////
#ifndef __CINT__
#include "iostream.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TCanvas.h"
#include "TClassTable.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "StBFChain.h"
#include "StIOMaker.h"
void bfc (const Int_t Last, 
	  const Char_t *Chain,
	  const Char_t *infile, 
	  const Char_t *outfile, 
	  const Char_t *TreeFile);
//R__EXTERN StBFChain *chain;
#else
class StBFChain;
class TTree;
StBFChain *chain;
class StFlowSelection;
class StIOMaker;
#endif
//_____________________________________________________________________
void bfcZ(const Int_t First,
	 const Int_t Last,
	 const Char_t *Chain="gstar Cy2b tfs -NoHits TpcHitFilter",
	 const Char_t *infile=0,
	 const Double_t zmin=-200, const Double_t zmax=-100,
	 const Char_t *outfile=0,
	 const Char_t *TreeFile=0){ 
  gROOT->LoadMacro("bfc.C");
  bfc(-1,Chain,infile,outfile,TreeFile);
  if (chain->GetOption("TpcHitFilter")) {
    StTpcHitFilterMaker *filtMk = (StTpcHitFilterMaker *) chain->GetMaker("tpc_hit_filter");
    if (filtMk) {
      //      St_tcl_Maker *tclMk= (St_tcl_Maker *) chain->GetMaker("tpc_hits");
      if (zmin < 0 && zmax < 0) filtMk->WestOff();
      if (zmin > 0 && zmax > 0) filtMk->EastOff();
      filtMk->SetZrange(zmin,zmax);
    }
  }
  {
    TDatime t;
    printf ("QAInfo:Run is started at Date/Time %i/%i\n",t.GetDate(),t.GetTime());
    printf ("QAInfo:Run on %s in %s\n",
	    gSystem->HostName(),
	    gSystem->WorkingDirectory());
    printf ("QAInfo: with %s\n", chain->GetCVS());
  }
  // Init the chain and all its makers
  Int_t iTotal = 0, iBad = 0;
  St_XDFFile *xdf_out = 0;
  TBenchmark evnt;
  Int_t iMake = 0, i = First;
  if (Last >= 0) {
    Int_t iInit = chain->Init();
    if (iInit >=  kStEOF) {
      chain->Fatal(iInit,"on init");
      goto END;
    }
    StEvtHddr *hd = (StEvtHddr*)chain->GetDataSet("EvtHddr");
    if (hd) hd->SetRunNumber(-2); // to be sure that InitRun calls at least once
    // skip if any
    if (First > 1) {
      if (chain->GetOption("fzin")) {
	geant = (St_geant_Maker *) chain->GetMaker("geant");
	if (geant) {
	  if (geant->IsActive()) geant->Skip(First-1);
	}
      }
      else {
	StIOMaker *inpMk      = (StIOMaker *)      chain->GetMaker("inputStream");
	if (inpMk) {printf ("Skip %i Events\n",First-1);inpMk->Skip(First-1);}
      }
    }
  }
  xdf_out = chain->GetXdfOut();
  if (chain->GetOption("Event")) evMk  = (StEventMaker   *) chain->GetMaker("StEventMaker");  
  treeMk = chain->GetMaker("OutputStream");
 EventLoop: if (i <= Last && iMake != kStEOF && iMake != kStFatal) {
   evnt.Reset();
   evnt.Start("QAInfo:");
   chain->Clear();
   iMake = chain->Make(i);
   if (iMake <kStEOF) {
     if (xdf_out){
       St_DataSet *dstSet = chain->GetInputDS("dst");
       if (dstSet) xdf_out->NextEventPut(dstSet); // xdf output
     }
     iTotal++;
     if (treeMk && iMake == kStErr) {treeMk->Make(i); iBad++;}
     //    gSystem->Exec("ps ux");
     evnt.Stop("QAInfo:");
     //  evnt.Show("QAInfo:");
     printf ("QAInfo: Done with Event [no. %d/run %d/evt. %d/Date.Time %d.%d/sta %d] Real Time = %10.2f seconds Cpu Time =  %10.2f seconds \n",
	     i,chain->GetRunNumber(),chain->GetEventNumber(),chain->GetDate(), chain->GetTime(),
	     iMake,evnt.GetRealTime("QAInfo:"),evnt.GetCpuTime("QAInfo:"));
   }
   i++; goto EventLoop;
 }
 END:
  fflush(stdout);
  printf ("QAInfo:Run completed ");
  gSystem->Exec("date");
  if (evMk) Event = (StEvent *) chain->GetInputDS("StEvent");
  {
    TDatime t;
    printf ("\nQAInfo:Run is finished at Date/Time %i/%i; Total events processed :%i and not completed: %i\n",
	    t.GetDate(),t.GetTime(),iTotal,iBad);
  }
}
//_____________________________________________________________________
void bfcZ (const Int_t Last, 
	  const Char_t *Chain="gstar Cy2b tfs evout TpcHitFilter -NoHits",
	  const Char_t *infile=0, 
	   const Double_t zmin=-200, const Double_t zmax=-100,
	  const Char_t *outfile=0, 
	  const Char_t *TreeFile=0)
{
  bfcZ(1,Last,Chain,infile,zmin,zmax,outfile,TreeFile);
}

