#ifndef StiRootDrawableKalmanTrack_HH
#define StiRootDrawableKalmanTrack_HH
#include "Sti/StiKalmanTrack.h"
#include "StiGui/StiRootDrawableTrack.h"

/// \class StiRootDrawableKalmanTrack
/// Work class used to display Kalman tracks with ROOT
/// \author M.L. Miller (Yale Software)
/// \author Claude A Pruneau, Wayne State University
class StiRootDrawableKalmanTrack : public StiKalmanTrack, public StiRootDrawableTrack
{
public:
  StiRootDrawableKalmanTrack();
  virtual ~StiRootDrawableKalmanTrack();
  virtual void reset();
  virtual StiKalmanTrackNode * add(StiHit *h,double alpha, double eta, double curvature, double tanl);
  virtual StiKalmanTrackNode * add(StiKalmanTrackNode * node);
};

#endif
