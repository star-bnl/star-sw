// $Id: StV0MiniDstMaker.cxx,v 1.1 1999/07/13 12:42:24 jones Exp $
// $Log: StV0MiniDstMaker.cxx,v $
// Revision 1.1  1999/07/13 12:42:24  jones
// *** empty log message ***
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StV0MiniDstMaker class                                               //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StV0MiniDstMaker.h"
#include "StEventMaker/StEventMaker.h"
#include "StChain/StChain.h"
#include "StEvent/StEvent.h"
#include "StV0MiniDst.hh"
#include <TFile.h>
#include <TOrdCollection.h>

#define v0_max 1000

void v0MiniDstFiller(StEvent&, TOrdCollection*);

//_____________________________________________________________________________
StV0MiniDstMaker::StV0MiniDstMaker(const char *name, const char *output) : StMaker(name){
  rootfile = output;
}
//_____________________________________________________________________________
StV0MiniDstMaker::~StV0MiniDstMaker(){
}
//_____________________________________________________________________________
Int_t StV0MiniDstMaker::Init(){

  printf("In StV0MiniDstMaker::Init() ...\n"); 

  // Create output ROOT file
  output = new TFile(rootfile,"RECREATE");

  // Create a TOrdCollection
  v0MiniDstCollection = new TOrdCollection(v0_max);

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StV0MiniDstMaker::Make(){

  printf("In StV0MiniDstMaker::Make() ...\n"); 

  // Get pointer to event
  StEventMaker* evMaker = (StEventMaker *) GetMaker("events");
  if( ! evMaker->event() ) return kStOK; 
  StEvent& event = *(evMaker->event());

  // Process the event
  v0MiniDstFiller(event,v0MiniDstCollection);

  return kStOK;
}
//_____________________________________________________________________________
Int_t StV0MiniDstMaker::Finish(){

  printf("In StV0MiniDstMaker::Finish() ...\n"); 

  StV0MiniDst* v0mdst;
  int n_v0 = v0MiniDstCollection->LastIndex()+1;

  printf("StV0MiniDstMaker: Number in v0MiniDstCollection %d\n",n_v0);
  for( int i=0; i<n_v0; i++){
    v0mdst = (StV0MiniDst *) v0MiniDstCollection->At(i);
  }

  // Change to output directory
  output->cd();

  // Write output
  v0MiniDstCollection->Write("V0MiniDst",kSingleKey);

  // Close output file
  output->Close();

  return kStOK;
}
//_____________________________________________________________________________
void StV0MiniDstMaker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: StV0MiniDstMaker.cxx,v 1.1 1999/07/13 12:42:24 jones Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

ClassImp(StV0MiniDstMaker)
