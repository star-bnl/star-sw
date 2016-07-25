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
#include "DeDxTree.h"
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
class StFlowMaker;
StFlowMaker *flowMaker;
class StFlowSelection;
class StIOMaker;
#endif
void Flow(Int_t nevents=100000,
	  const char *MainFile="/star/data14/reco/DEV2/2000/08/st_physics_1241016_raw_0001.event.root",
	  const char* rootFile="flow_1241016.root")
{
  //#ifndef __CINT__
  gROOT->LoadMacro("bfc.C");
  //#endif
  //  bfc(-1,"in dEdxY2 event flow",MainFile,0,rootFile);
  //  bfc(-1,"in RichSpectra flow",MainFile,0,rootFile);
  bfc(-1,"in flow",MainFile,0,rootFile);
  //  chain->SetDEBUG(1);
  //  StMaker *dEdxY2 = chain->GetMaker("dEdxY2"); dEdxY2->SetMode(1);
  flowMaker = (StFlowMaker *) chain->GetMaker("Flow");
  flowMaker->PicoEventWrite(kTRUE);
  flowMaker->SetMode(1);
  StFlowSelection *select = flowMaker->FlowSelect();
  select->SetEtaPart(-3.,3.);
  select->SetPtPart(0,1000.);
  
//   StFile *file = new StFile();
//   file->AddFile("dev.flowpicoevent.root");

  //  flowMaker->SetPicoEventFileName(file); 
  if (nevents >= 0)   chain->Init();
  int istat=0,iev=1;
 EventLoop: if (iev<=nevents && !istat) {
   chain->Clear();
   istat = chain->Make(iev); // This should call the Make() method in ALL makers
//    StEvtHddr *fEvtHddr = (StEvtHddr*)chain->GetDataSet("EvtHddr");
//    if (fEvtHddr) fEvtHddr->SetRunNumber(fEvtHddr->GetRuNumber());
   iev++; goto EventLoop;
 } // Event Loop
}
