//StiRDLocalTrackSeedFinder.h
//M.L. Miller (Yale Software)
//11/01

#ifndef StiRDLocalTrackSeedFinder_HH
#define StiRDLocalTrackSeedFidner_HH

/*! \class StiRDLocalTrackSeedFinder
  StiRDLocalTrackSeedFinder is a ROOT rendered visual version of class
  StiLocalTrackSeedFinder.  Essentially, it displays the set of points that
  are selected as the seed of a track and used to initialize the kalman
  track.
  
  \author M.L. Miller (Yale Software)
  
 */

#include "Sti/StiLocalTrackSeedFinder.h"

class StiRootDrawableHits;
class StiDetectorContainer;
class StiHitContainer;

class StiRDLocalTrackSeedFinder : public StiLocalTrackSeedFinder
{
public:
  StiRDLocalTrackSeedFinder(const string& name,
			    Factory<StiKalmanTrack>* trackFactory,
			    StiHitContainer* hitContainer,
			    StiDetectorContainer    * detectorContainer);
  virtual ~StiRDLocalTrackSeedFinder();
  
  //Update state from StiIOBroker
  virtual void getNewState();
  
  //Over-write reset method
  virtual void reset();
  
protected:
  virtual StiKalmanTrack* makeTrack(StiHit*);
  StiRootDrawableHits* mdrawablehits;

private:
    
    //The following are not implemented, as they are non-trivial
    //and the default compiler generated versions will be wrong.
    StiRDLocalTrackSeedFinder();
    StiRDLocalTrackSeedFinder(const StiRDLocalTrackSeedFinder&);
    StiRDLocalTrackSeedFinder operator=(const StiRDLocalTrackSeedFinder&);    
};

#endif
