/***************************************************************************
 *
 * $Id: StHbtPicoEvent.hh,v 1.2 2000/03/17 17:23:05 laue Exp $
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
 * Revision 1.2  2000/03/17 17:23:05  laue
 * Roberts new three particle correlations implemented.
 *
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
  StHbtParticleCollection* ThirdParticleCollection();

private:
  StHbtParticleCollection* mFirstParticleCollection;
  StHbtParticleCollection* mSecondParticleCollection;
  StHbtParticleCollection* mThirdParticleCollection;
};

inline StHbtParticleCollection* StHbtPicoEvent::FirstParticleCollection(){return mFirstParticleCollection;}
inline StHbtParticleCollection* StHbtPicoEvent::SecondParticleCollection(){return mSecondParticleCollection;}
inline StHbtParticleCollection* StHbtPicoEvent::ThirdParticleCollection(){return mThirdParticleCollection;}



#endif
