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
  StvKalmanTrackFitter(const char *name);
  virtual ~StvKalmanTrackFitter(){;}	
  virtual  int FitTrack(StvTrack *trak,int dir);
  virtual void Clear(const char *opt="");
  

protected:
char mBeg[1];
char mEnd[1];

ClassDef(StvKalmanTrackFitter,0);
};


#endif
