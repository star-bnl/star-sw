/***************************************************************************
 *
 * $Id: StRandyTopMapMaker.cxx,v 1.1 2000/04/02 19:56:20 rcwells Exp $
 *
 * Author: Randy Wells, Ohio State,
 ***************************************************************************
 *
 * Description:  This is used to fix the topology map in StEvent
 *
 *
 ***************************************************************************
 **************************************************************************/
#include "StRandyTopMapMaker.h"
#include "StChain.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StEvent.h"
#include "StTrackGeometry.h"
#include "StTrackTopologyMap.h"

static const char rcsid[] = "$Id: StRandyTopMapMaker.cxx,v 1.1 2000/04/02 19:56:20 rcwells Exp $";

ClassImp(StRandyTopMapMaker)

StRandyTopMapMaker::StRandyTopMapMaker(const Char_t *name) : StMaker(name)
{
    drawinit = kFALSE;
    theTag = 0;
    nevents = 0;
}

StRandyTopMapMaker::~StRandyTopMapMaker() { /* noop */ }

Int_t
StRandyTopMapMaker::Init()
{
  return StMaker::Init();
}

void
StRandyTopMapMaker::Clear(Option_t *opt)
{
    delete theTag; theTag = 0;
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
    StEvent& ev = *mEvent;
    
    // OK, we've got the event. Let's fix that Topology Map
    // This only fixes TPC pad rows and the turnaround flag
    StTrack* rTrack;
    int mult = mEvent->trackNodes().size();
    unsigned long mask1 = 127;
    unsigned long mask2 = 2147483648;
    unsigned long temp1;
    unsigned long temp2;
    for (unsigned long int icount=0; icount<(unsigned long int)mult; icount++){
      rTrack = mEvent->trackNodes()[icount]->track(global);
      // This checks bits that should not be set
      temp1 = rTrack->topologyMap().data(0) & mask1;
      if ( int(temp1) ) cout << " something wrong with map(0) " << temp1 << endl;
      temp2 = rTrack->topologyMap().data(1) & mask2;
      if ( int(temp2) ) cout << " something wrong with map(1) " << temp2 << endl;
      // End check of bits that should not be set
      int numHits = rTrack->detectorInfo()->hits(kTpcId).size();
      bool padRow[46];
      bool turnAround = false;
      int pad;
      int ipad;
      unsigned long map1 = 0;
      unsigned long map2 = 0;
      StTpcHit* myHit;
      // Zero pad rows
      for (int ipad=0; ipad<46; ipad++) padRow[ipad] = false;
      for (int ihit=0; ihit<numHits; ihit++) {
	myHit = (StTpcHit *)rTrack->detectorInfo()->hits(kTpcId)[ihit];
	pad = myHit->padrow();
	if ( padRow[pad] ) turnAround = true;
	padRow[pad] = true;
      }
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
      if ( turnAround ) map2 |= 1UL<<(30);
      StTrackTopologyMap newMap( map1,map2 );
      rTrack->setTopologyMap( newMap );
    }
    
    return kStOK;
}
