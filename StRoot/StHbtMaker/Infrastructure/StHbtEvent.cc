/***************************************************************************
 *
 * $Id: StHbtEvent.cc,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
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
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
//___________________
StHbtEvent::StHbtEvent(){
  mMult=0;
  mPrimVertPos[0]=-999.0;
  mPrimVertPos[1]=-999.0;
  mPrimVertPos[2]=-999.0;
  mTrackCollection = new StHbtTrackCollection;
}
//___________________
StHbtEvent::~StHbtEvent(){
  StHbtTrackIterator iter;
  for (iter=mTrackCollection->begin();iter!=mTrackCollection->end();iter++){
    delete *iter;
  }
  delete mTrackCollection;
}
//___________________

