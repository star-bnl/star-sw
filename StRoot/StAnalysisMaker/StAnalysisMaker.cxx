// $Id: StAnalysisMaker.cxx,v 1.1 1999/02/05 17:54:55 wenaus Exp $
// $Log: StAnalysisMaker.cxx,v $
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

static const char rcsid[] = "$Id: StAnalysisMaker.cxx,v 1.1 1999/02/05 17:54:55 wenaus Exp $";
#include "StMessMgr.h"
long countPrimaryTracks(StEvent* event);
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
  StEvent& ev = *(evMaker->event());
  StRun& run = *(evMaker->run());
  cout << "StAnalysisMaker:  Reading Event " << 
    " Type " << ev.type() << " Run " << ev.runNumber() << endl;
  cout << " N vertex " << ev.vertexCollection()->size() << endl;
  cout << " N track " << ev.trackCollection()->size() << endl;
  cout << " N TPC hit " << ev.tpcHitCollection()->size() << endl;
  cout << " N FTPC hit " << ev.ftpcHitCollection()->size() << endl;
  cout << " N SVT hit " << ev.svtHitCollection()->size() << endl;

  long ntk = countPrimaryTracks(evMaker->event());
  cout << "Primary tracks: " << ntk << endl;

  return kStOK;
    drawinit = kFALSE;
    theTag = 0;
void StAnalysisMaker::MakeBranch() {
  StMaker::MakeBranch();
}

void StAnalysisMaker::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StAnalysisMaker.cxx,v 1.1 1999/02/05 17:54:55 wenaus Exp $\n");
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


long countPrimaryTracks(StEvent *event)
{
  // count vertex daughters
  StVertexCollection* theVertexCollection = event->vertexCollection();
  StVertexIterator itr;
  StVertex *vtx;
  for (itr = theVertexCollection->begin();
       itr != theVertexCollection->end(); itr++) {
    vtx = *itr;
    cout << vtx->index() << " " << vtx->daughters().size() << endl;
  }
  

  long counter = 0;
  StTrackCollection *tracks = event->trackCollection();
  StTrackIterator iter;
  StGlobalTrack *track;
  StVertex *vertex;
  for (iter = tracks->begin();
       iter != tracks->end(); iter++) {
    track = *iter;
    vertex = track->startVertex();
    if (vertex &&
        vertex->type() == primary)
      counter++;
  }
  return counter;
}
    
    return kStOK;
}
