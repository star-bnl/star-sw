// $Id: StAnalysisMaker.cxx,v 1.13 1999/08/09 19:38:32 kathy Exp $
// $Log: StAnalysisMaker.cxx,v $
// Revision 1.13  1999/08/09 19:38:32  kathy
// checkin Curtis' changes that print out the event # with each set of QAInfo stuff
//
// Revision 1.12  1999/08/06 21:25:33  fisyak
// Switch to StMessager
//
// Revision 1.11  1999/08/06 20:21:51  kathy
// back to old version that didn't write out QA info file, but now added QAInfo tag in front of information that QA team wants in summarizeEvent.cc - will also add a few more lines of output to summarizeEvent.cc soon
//
// Revision 1.10  1999/07/30 22:56:01  kathy
// added new method and input param qaflag so that if turned on, a log file will be printed out with QA information
//
// Revision 1.9  1999/07/15 13:56:40  perev
// cleanup
//
// Revision 1.8  1999/06/25 19:20:40  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.4  1999/06/24 21:56:46  wenaus
// Version minimally changed from standard StAnalysisMaker
//
// Revision 1.7  1999/05/01 00:54:59  perev
// Clear removed delet m_DataSet
//
// Revision 1.6  1999/03/30 15:33:43  wenaus
// eliminate obsolete branch methods
//
// Revision 1.5  1999/03/20 20:59:08  perev
// new maker schema
//
// Revision 1.4  1999/02/12 02:00:26  wenaus
// Incorporate tag loading example
//
// Revision 1.3  1999/02/11 15:39:15  wenaus
// cleanup
//
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
//  Sample maker to access and analyze StEvent
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
#include "StChain/StChain.h"
#include "StRun.h"
#include "StEvent.h"
#include "StAnalysisMaker.h"
static const char rcsid[] = "$Id: StAnalysisMaker.cxx,v 1.13 1999/08/09 19:38:32 kathy Exp $";
#include "StMessMgr.h"
//  Proptotypes of little functions which perform
//  specific analysis tasks.
//
void summarizeEvent(StEvent& event, Int_t &nevents);
Int_t StAnalysisMaker::Make() {
  StEvent* mEvent;
  mEvent = (StEvent *) GetInputDS("StEvent");
  if (! mEvent) return kStOK; // If no event, we're done
  StEvent& ev = *mEvent;

  // OK, we've got the event. Pass it and process it.
  summarizeEvent(ev,nevents); 
  long ntk = countPrimaryTracks(ev);
  gMessMgr->Info() << " StAnalysisMaker.cxx -- Primary tracks: " << ntk << endl;

  // Create and fill a tag
  if (theTag) delete theTag;
  theTag = new HighPtTag_st;
  tagFiller(ev,*theTag);
void tagFiller(StEvent& event, HighPtTag_st& hptTag);

  return kStOK;
    drawinit = kFALSE;
    theTag = 0;

StAnalysisMaker::StAnalysisMaker(const Char_t *name) : StMaker(name) {
  drawinit = kFALSE;
  theTag = 0;
  nevents = 0;
}

StAnalysisMaker::~StAnalysisMaker() {
}

Int_t StAnalysisMaker::Init() {
Int_t
StAnalysisMaker::Init()
{
void StAnalysisMaker::Clear(Option_t *opt) {
  delete theTag; theTag = 0;
  StMaker::Clear();
    tagFiller(ev,*theTag);

Int_t StAnalysisMaker::Finish() {
  return kStOK;
}

ClassImp(StAnalysisMaker)
    
    return kStOK;
}
