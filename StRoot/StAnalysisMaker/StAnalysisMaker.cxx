// $Id: StAnalysisMaker.cxx,v 1.2 1999/02/10 23:59:52 wenaus Exp $
// $Log: StAnalysisMaker.cxx,v $
// Revision 1.2  1999/02/10 23:59:52  wenaus
// cleanup
//
// Revision 1.1  1999/02/05 17:54:55  wenaus
// initial commit
//
//
///////////////////////////////////////////////////////////////////////////////
//
// StAnalysisMaker
//
// Description: 
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Torre Wenaus, BNL
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
 * Revision for new StEvent
#include "StRoot/StEventReaderMaker/StEventReaderMaker.h"
#include "StChain/StChain.h"
#include "StEvent/StRun.hh"
#include "StEvent/StEvent.hh"

static const char rcsid[] = "$Id: StAnalysisMaker.cxx,v 1.2 1999/02/10 23:59:52 wenaus Exp $";
#include "StMessMgr.h"
void summarizeEvent(StEvent& event);
//  specific analysis tasks.
void summarizeEvent(StEvent& event, Int_t &nevents);
StAnalysisMaker::StAnalysisMaker(const Char_t *name, const Char_t *title) : StMaker(name, title) {
  drawinit = kFALSE;
}

StAnalysisMaker::~StAnalysisMaker() {
}

Int_t StAnalysisMaker::Init() {
  return StMaker::Init();
}

Int_t StAnalysisMaker::Make() {
  StEventReaderMaker* evMaker = (StEventReaderMaker*) gStChain->Maker("events");
  if (! evMaker->event()) {
    // No event. We're done.
    return kStOK;
  }
  StEvent& ev = *(evMaker->event());
  StRun& run = *(evMaker->run());

  summarizeEvent(ev);
  long ntk = countPrimaryTracks(ev);
  cout << "Primary tracks: " << ntk << endl;

  return kStOK;
    drawinit = kFALSE;
    theTag = 0;
void StAnalysisMaker::MakeBranch() {
  StMaker::MakeBranch();
}

void StAnalysisMaker::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StAnalysisMaker.cxx,v 1.2 1999/02/10 23:59:52 wenaus Exp $\n");
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

void StAnalysisMaker::Clear(Option_t *opt) {
  SafeDelete(m_DataSet);
}

void StAnalysisMaker::SetBranch() {
  StMaker::SetBranch();
    tagFiller(ev,*theTag);

Int_t StAnalysisMaker::Finish() {
  return kStOK;
}

ClassImp(StAnalysisMaker)
    
    return kStOK;
}
