///\File StiRDLocalTrackSeedFinder.h
///\author M.L. Miller (Yale Software)
///\date 2001
#ifndef StiRDLocalTrackSeedFinder_HH
#define StiRDLocalTrackSeedFidner_HH
#include "Sti/StiLocalTrackSeedFinder.h"
class StiRootDrawableHits;
//class StiDetectorContainer;
//class StiHitContainer;

/*! \class StiRDLocalTrackSeedFinder
  StiRDLocalTrackSeedFinder is a ROOT rendered visual version of class
  StiLocalTrackSeedFinder.  Essentially, it displays the set of points that
  are selected as the seed of a track and used to initialize the kalman
  track.
  \author M.L. Miller (Yale Software)
 */class StiRDLocalTrackSeedFinder : public StiLocalTrackSeedFinder
{
public:
  StiRDLocalTrackSeedFinder(const string& name,
			    Factory<StiKalmanTrack>* trackFactory,
			    StiHitContainer* hitContainer,
			    StiDetectorContainer    * detectorContainer);
  virtual ~StiRDLocalTrackSeedFinder();
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
