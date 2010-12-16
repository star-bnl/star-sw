/// \File StvKalmanTrackFitter.h
/// \author Victor Perev 9/2010
#ifndef StvKalmanTrackFitter_HH
#define StvKalmanTrackFitter_HH
#include "StvTrackFitter.h"

/// \class StvKalmanTrackFitter
class StvTrack;

class StvKalmanTrackFitter : public StvTrackFitter
{
public:
  StvKalmanTrackFitter();
  virtual ~StvKalmanTrackFitter(){;}	
  virtual  int Refit(StvTrack *trak,int dir);
  virtual  int Fit(const StvTrack *trak,const StvHit *vtx,StvNode *node);
  virtual void Clear(const char *opt="");
  

protected:
char mBeg[1];
char mEnd[1];

ClassDef(StvKalmanTrackFitter,0);
};


#endif
