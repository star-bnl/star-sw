/// \File StvKalmanTrackFinder.h
/// \author Victor Perev 01/2010
#ifndef StvKalmanTrackFinder_HH
#define StvKalmanTrackFinder_HH
#include "StvTrackFinder.h"

class StvPars;
class StvTrack;
class StvDiver;
class StvHitter;
class StvHitVector;
/// \class StvKalmanTrackFinder

class StvKalmanTrackFinder : public StvTrackFinder
{
public:
  StvKalmanTrackFinder(const char *name="KalmanTrackFinder");
   ~StvKalmanTrackFinder(){;}
   int	FindTracks();
   int	Refit(int idir);
   int  FindTrack(int idir);
   int	FindPrimaries(const StvHits &vtxs);
   void	Reset();
   void	Clear(const char *opt="");
//   void	Add(const StvPars *par);
//   void	Add(const StvHitVector *hitV);
   
   

protected:
char mBeg[1];
const THelixTrack *mSeedHelx;
      StvTrack    *mCurrTrak;
StvDiver *mDive;
StvHitter *mHitter;
char mEnd[1];
private:
ClassDef(StvKalmanTrackFinder,0);
};

#endif
