/***************************************************************************
 *
 *  
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     base class for particle-wise cuts
 * Users inherit from this class to make particular TrackCuts.
 *     Note that TrackCut is a derived class of ParticleCut
 * 
 ***************************************************************************
 *
 * 
 **************************************************************************/


#ifndef StHbtTrackCut_hh
#define StHbtTrackCut_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Base/StHbtParticleCut.hh"

class StHbtTrackCut : public StHbtParticleCut{

public:

  StHbtTrackCut(){/* no-op */};                       // default constructor. - Users should write their own
  virtual ~StHbtTrackCut(){/* no-op */};              // destructor

  virtual bool Pass(const StHbtTrack* track)=0;       // true if passes, false if not
  virtual StHbtParticleType Type(){return hbtTrack;}
};

#endif
