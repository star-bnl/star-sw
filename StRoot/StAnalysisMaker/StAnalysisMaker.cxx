/***************************************************************************
 *
 * $Id: StAnalysisMaker.cxx,v 2.0 1999/11/04 16:10:03 ullrich Exp $
 *
 * Author: Torre Wenaus, BNL,
 *         Thomas Ullrich, Nov 1999
 ***************************************************************************
 *
 * Description:  This is an example of a maker to perform analysis
 *               using StEvent.
 *               Use this as a template and customize it for your
 *               studies.
 *
 ***************************************************************************
 *
 * $Log: StAnalysisMaker.cxx,v $
 * Revision 2.0  1999/11/04 16:10:03  ullrich
 * Revision for new StEvent
 *
 **************************************************************************/
#include "StAnalysisMaker.h"
#include "StChain.h"
#include "StEventTypes.h"
#include "StMessMgr.h"

static const char rcsid[] = "$Id: StAnalysisMaker.cxx,v 2.0 1999/11/04 16:10:03 ullrich Exp $";

//
//  Proptotypes of little functions which perform
//  specific analysis tasks.
//
void summarizeEvent(StEvent& event, Int_t &nevents);
long countPrimaryTracks(StEvent& event);
void tagFiller(StEvent& event, HighPtTag_st& hptTag);

ClassImp(StAnalysisMaker)

StAnalysisMaker::StAnalysisMaker(const Char_t *name) : StMaker(name)
{
    drawinit = kFALSE;
    theTag = 0;
    nevents = 0;
}

StAnalysisMaker::~StAnalysisMaker() { /* noop */ }

Int_t
StAnalysisMaker::Init()
{
  return StMaker::Init();
}

void
StAnalysisMaker::Clear(Option_t *opt)
{
    delete theTag; theTag = 0;
    StMaker::Clear();
}

Int_t
StAnalysisMaker::Finish()
{
    return kStOK;
}

Int_t
StAnalysisMaker::Make()
{
    //
    //	This method is called every event. That's the
    //  right place to plug in your analysis to be
    //  done every event.
    //
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
    
    return kStOK;
}
