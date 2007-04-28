/***************************************************************************
 *
 * $Id: StRandyTopMapMaker.cxx,v 1.6 2007/04/28 17:56:15 perev Exp $
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
 * $Log: StRandyTopMapMaker.cxx,v $
 * Revision 1.6  2007/04/28 17:56:15  perev
 * Redundant StChain.h removed
 *
 * Revision 1.5  2000/05/02 22:13:24  laue
 * Memory leak fixed
 *
 * Revision 1.4  2000/04/03 23:18:36  rcwells
 * Improved speed in RandyTopMapMaker
 *
 * Revision 1.3  2000/04/03 20:28:10  rcwells
 * Removed remnant Pt tag in code from StAnalysisMaker.
 *
 * Revision 1.2  2000/04/02 20:41:17  rcwells
 * Fixed the FTPC bit check.
 *
 * Revision 2.1  1999/12/30 01:54:57  ogilvie
 * added countPrimaryPions as example how to use PID
 *
 * Revision 2.0  1999/11/04 16:10:03  ullrich
 * Revision for new StEvent
 *
 **************************************************************************/
#include "StRandyTopMapMaker.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StEvent.h"
#include "StTrackGeometry.h"
#include "StTrackTopologyMap.h"

static const char rcsid[] = "$Id: StRandyTopMapMaker.cxx,v 1.6 2007/04/28 17:56:15 perev Exp $";

ClassImp(StRandyTopMapMaker)

StRandyTopMapMaker::StRandyTopMapMaker(const Char_t *name) : StMaker(name)
{
//    drawinit = kFALSE;
}

StRandyTopMapMaker::~StRandyTopMapMaker() { 
}

Int_t
StRandyTopMapMaker::Init()
{
  return StMaker::Init();
}

void
StRandyTopMapMaker::Clear(Option_t *opt)
{
    StMaker::Clear();
}

Int_t
StRandyTopMapMaker::Finish()
{
    return kStOK;
}

Int_t
StRandyTopMapMaker::Make()
{
    //
    //	This method is called every event. That's the
    //  right place to plug in your analysis to be
    //  done every event.
    //
    StEvent* mEvent;
    mEvent = (StEvent *) GetInputDS("StEvent");
    if (! mEvent) return kStOK; // If no event, we're done
    
    // OK, we've got the event. Let's fix that Topology Map
    // This only fixes TPC pad rows and the turnaround flag
    StTrack* rTrack;
    int mult = mEvent->trackNodes().size();
    unsigned long mask2 = 0x80000000;
    unsigned long temp1;
    int pad, ipad;
    bool  padRow[46];    
    unsigned long map1, map2;
    StPtrVecHit myHitVector;
    StPtrVecHitIterator myHitIterator;
    StTpcHit* myHit;
    // For speed we try to remove the turnAround flag
    //    bool turnAround;
    for (unsigned long int icount=0; icount<(unsigned long int)mult; icount++){
      rTrack = mEvent->trackNodes()[icount]->track(global);
      // Changed code
      // This checks bits that should not be set
      temp1 = rTrack->topologyMap().data(1) & mask2;
      // If FTPC track, don't do anything
      if ( int(temp1) ) continue;
      //      numHits = rTrack->detectorInfo()->hits(kTpcId).size();
      //      turnAround = false;
      // Zero pad rows
      for (ipad=0; ipad<46; ipad++) padRow[ipad] = false;
      myHitVector = rTrack->detectorInfo()->hits(kTpcId);
      for (myHitIterator=myHitVector.begin(); myHitIterator!=myHitVector.end(); myHitIterator++) {
	myHit = (StTpcHit *)(*myHitIterator);
	pad = myHit->padrow();
	padRow[pad] = true;
      }
      /*
      for (ihit=0; ihit<numHits; ihit++) {
	myHit = (StTpcHit *)(rTrack->detectorInfo()->hits(kTpcId)[ihit]);
	pad = myHit->padrow();
	//	if ( padRow[pad] ) turnAround = true;
	padRow[pad] = true;
      }
      */
      map1 = map2 = 0;
      // Fill the map
      for (ipad=1; ipad<=24; ipad++) {
	if ( padRow[ipad] ) {
	  map1 |= 1UL<<(ipad+7);
	}
      }
      for (ipad=25; ipad<=45; ipad++) {
	if ( padRow[ipad] ) {
	  map2 |= 1UL<<(ipad-25);
	}
      }
      //      if ( turnAround ) map2 |= 1UL<<(30);
      StTrackTopologyMap aNewMap( map1,map2 );
      rTrack->setTopologyMap( aNewMap );
    }
    
    myHitVector.clear();
    return kStOK;
}
