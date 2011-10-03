/***************************************************************************
 *
 * $Id: StHbtPicoEvent.cc,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *  PicoEvents are last-step ultra-compressed "events" just containing
 *  bare information about the particles of interest.  They have already
 *  gone through Event and Track cuts, so only Pair cuts are left.
 *  PicoEvents are *internal* to the code, and are stored in the
 *  Event-mixing buffers.
 *           
 *
 ***************************************************************************
 *
 * $Log: StHbtPicoEvent.cc,v $
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtPicoEvent.hh"

//________________
StHbtPicoEvent::StHbtPicoEvent(){
  mFirstParticleCollection = new StHbtParticleCollection;
  mSecondParticleCollection = new StHbtParticleCollection;
}
//_________________
StHbtPicoEvent::~StHbtPicoEvent(){
  StHbtParticleIterator iter;
  for (iter=mFirstParticleCollection->begin();iter!=mFirstParticleCollection->end();iter++){
    delete *iter;
  }
  delete mFirstParticleCollection;
  mFirstParticleCollection =0;
  if (mSecondParticleCollection){
    for (iter=mSecondParticleCollection->begin();iter!=mSecondParticleCollection->end();iter++){
      delete *iter;
    }
    delete mSecondParticleCollection;
  }
}
//_________________
