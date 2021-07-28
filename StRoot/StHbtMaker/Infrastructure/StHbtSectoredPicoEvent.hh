/***************************************************************************
 *
 * $Id: StHbtSectoredPicoEvent.hh,v 1.1 2000/04/12 01:47:04 willson Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   Specialized PicoEvents used in StHbtSectoredAnalysis.  A single
 *   PicoEvent contains a number of particle collections for each sector
 *   in momentum space.  These are grouped in a single dimensional array.
 *
 ***************************************************************************
 *
 * $Log: StHbtSectoredPicoEvent.hh,v $
 * Revision 1.1  2000/04/12 01:47:04  willson
 * Initial Installation
 *
 *
 **************************************************************************/

#ifndef StHbtSectoredPicoEvent_hh
#define StHbtSectoredPicoEvent_hh

#include "StHbtMaker/Infrastructure/StHbtParticleCollection.hh"

class StHbtSectoredPicoEvent{
public:
  StHbtSectoredPicoEvent(int);
  StHbtSectoredPicoEvent(int, int, int);
  ~StHbtSectoredPicoEvent();

  StHbtParticleCollection** FirstSectoredCollection();
  StHbtParticleCollection** SecondSectoredCollection();
  StHbtParticleCollection** ThirdSectoredCollection();

private:

  StHbtParticleCollection** mFirstSectoredCollection;
  StHbtParticleCollection** mSecondSectoredCollection;
  StHbtParticleCollection** mThirdSectoredCollection;
  int                       mNumBins;

};

inline StHbtParticleCollection** StHbtSectoredPicoEvent::FirstSectoredCollection(){return mFirstSectoredCollection;}
inline StHbtParticleCollection** StHbtSectoredPicoEvent::SecondSectoredCollection(){return mSecondSectoredCollection;}
inline StHbtParticleCollection** StHbtSectoredPicoEvent::ThirdSectoredCollection(){return mThirdSectoredCollection;}

#endif
