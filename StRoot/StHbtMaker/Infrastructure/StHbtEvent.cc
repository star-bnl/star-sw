/***************************************************************************
 *
 * $Id: StHbtEvent.cc,v 1.6 2000/05/03 17:44:42 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   HbtEvent is the "transient microDST"  Objects of this class are
 *   generated from the input data by a Reader, and then presented to
 *   the Cuts of the various active Analyses.
 *
 ***************************************************************************
 *
 * $Log: StHbtEvent.cc,v $
 * Revision 1.6  2000/05/03 17:44:42  laue
 * StHbtEvent, StHbtTrack & StHbtV0 declared friend to StHbtIOBinary
 * StHbtParticle updated for V0 pos,neg track Id
 *
 * Revision 1.5  2000/02/18 21:32:23  laue
 * franksTrackCut changed. If mCharge is set to '0' there will be no cut
 * on charge. This is important for front-loaded cuts.
 *
 * copy constructor implemented for StHbtEvent, StHbtTrack and StHbtV0.
 *
 * franks1HistoD.cxx franks1HistoD.h franks2HistoD.cxx franks2HistoD.h
 * removed. We can now (CC5 on Solaris) use the versions (no D)
 *
 * Revision 1.4  1999/09/16 18:47:59  lisa
 * replace placeholder HbtV0Track stuff with Helens StHbtV0 classes
 *
 * Revision 1.3  1999/07/27 10:47:04  lisa
 * now works in dev on linux and solaris - mistake in deleting picoEvents fixed
 *
 * Revision 1.2  1999/07/19 14:24:05  hardtke
 * modifications to implement uDST
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"

//___________________
StHbtEvent::StHbtEvent(){
  mPrimVertPos[0]=-999.0;
  mPrimVertPos[1]=-999.0;
  mPrimVertPos[2]=-999.0;
  mTrackCollection = new StHbtTrackCollection;
  mV0Collection = new StHbtV0Collection;
}
//___________________
StHbtEvent::StHbtEvent(const StHbtEvent& ev, StHbtTrackCut* tCut, StHbtV0Cut* vCut){ // copy constructor with track and v0 cuts
  //cout << "StHbtEvent::StHbtEvent(const StHbtEvent& ev, StHbtTrackCut* tCut, StHbtV0Cut* vCut)" << endl;
  mEventNumber = ev.mEventNumber;
  mCtbMultiplicity = ev.mCtbMultiplicity;
  mZdcAdc[0] = ev.mZdcAdc[0];
  mZdcAdc[1] = ev.mZdcAdc[1];
  mTpcNhits = ev.mTpcNhits;
  mNumberOfTracks = ev.mNumberOfTracks;
  mNumberOfGoodTracks = ev.mNumberOfGoodTracks;
  mReactionPlane[0] = ev.mReactionPlane[0];
  mReactionPlane[1] = ev.mReactionPlane[1];
  mPrimVertPos = ev.mPrimVertPos;

  // create collections
  mTrackCollection = new StHbtTrackCollection;
  mV0Collection = new StHbtV0Collection;
  // copy track collection
  for ( StHbtTrackIterator tIter=ev.mTrackCollection->begin(); tIter!=ev.mTrackCollection->end(); tIter++) {
    if ( !tCut || tCut->Pass(*tIter) ) {
      //cout << " trackCut passed " << endl;
      StHbtTrack* trackCopy = new StHbtTrack(**tIter);
      mTrackCollection->push_back(trackCopy);
    }
  }
  // copy v0 collection
  for ( StHbtV0Iterator vIter=ev.mV0Collection->begin(); vIter!=ev.mV0Collection->end(); vIter++) {
    if ( !vCut || vCut->Pass(*vIter) ) {
      StHbtV0* v0Copy = new StHbtV0(**vIter);
      mV0Collection->push_back(v0Copy);
    }
  }
}
//___________________
StHbtEvent::~StHbtEvent(){
  StHbtTrackIterator iter;
  for (iter=mTrackCollection->begin();iter!=mTrackCollection->end();iter++){
    delete *iter;
  }
  mTrackCollection->clear();
  delete mTrackCollection;
  //must do the same for the V0 collection
  StHbtV0Iterator V0iter;
  for (V0iter=mV0Collection->begin();V0iter!=mV0Collection->end();V0iter++){
    delete *V0iter;
  }
  mV0Collection->clear();
  delete mV0Collection;
}
//___________________

