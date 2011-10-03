/***************************************************************************
 *
 * $Id: StHbtPicoEvent.hh,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
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
 * $Log: StHbtPicoEvent.hh,v $
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtPicoEvent_hh
#define StHbtPicoEvent_hh

#include "StHbtMaker/Infrastructure/StHbtParticleCollection.hh"

class StHbtPicoEvent{
public:
  StHbtPicoEvent();
  ~StHbtPicoEvent();

  /* may want to have other stuff in here, like where is primary vertex */

  StHbtParticleCollection* FirstParticleCollection();
  StHbtParticleCollection* SecondParticleCollection();

private:
  StHbtParticleCollection* mFirstParticleCollection;
  StHbtParticleCollection* mSecondParticleCollection;
};

inline StHbtParticleCollection* StHbtPicoEvent::FirstParticleCollection(){return mFirstParticleCollection;}
inline StHbtParticleCollection* StHbtPicoEvent::SecondParticleCollection(){return mSecondParticleCollection;}



#endif
