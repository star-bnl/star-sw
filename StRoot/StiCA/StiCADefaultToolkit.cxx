#include "StiCA/StiCADefaultToolkit.h"
#include "Sti/Base/StiFactory.h"
#include "StiCA/StiCAKalmanTrack.h"
#include "StiCA/StiCAKalmanTrackFinder.h"


//______________________________________________________________________________
Factory<StiKalmanTrack>* StiCADefaultToolkit::getTrackFactory()
{
  if (_trackFactory) return _trackFactory;
  cout << "StiCADefaultToolkit::getTrackFactory() -I- "; 
      _trackFactory = StiFactory<StiCAKalmanTrack,StiKalmanTrack>::myInstance();
      _trackFactory->setFastDelete();
  return _trackFactory;
}

//______________________________________________________________________________
StiTrackFinder       * StiCADefaultToolkit::getTrackFinder()
{
  if (_trackFinder)
    return _trackFinder;
  _trackFinder = new StiCAKalmanTrackFinder(this);
  StiTrack::setTrackFinder(_trackFinder);
  getTrackFitter();
  return _trackFinder;
}
