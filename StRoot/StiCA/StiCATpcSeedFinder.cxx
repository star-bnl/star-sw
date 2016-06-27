#include <assert.h>
#include <string.h>
#include "Sti/StiToolkit.h"
#include "Sti/StiHit.h"
#include "StMessMgr.h"
#include "StEvent/StTpcHit.h"
#include "Sti/StiKalmanTrack.h"

#include "StiCATpcTrackerInterface.h"
//#define PRINT_SEED_STATISTIC
//#define PRINT_FIT_ERR_STATISTIC
#ifdef PRINT_FIT_ERR_STATISTIC
#include <map>
#endif // PRINT_FIT_ERR_STATISTIC
//#define EXTRAPOLATION_CUT
//#define KINK_REJECTION  
#define OVERLAP_REJECTION
//________________________________________________________________________________
Bool_t StiCATpcSeedFinder::SeedsCompareStatus(const Seed_t a, const Seed_t b)
{
  return (a.total_hits < b.total_hits);
}
//________________________________________________________________________________
StiTrack *StiCATpcSeedFinder::findTrack(double rMin)
{ 
  static StiCATpcTrackerInterface& caTrackerInt = StiCATpcTrackerInterface::Instance();
  if (!mSeeds || mSeeds->size()==0) {
    if (mEnded) return 0;
    caTrackerInt.SetNewEvent();
    // zero all banks before filling !!! 
    auto *map =  &StiToolkit::instance()->getHitContainer()->hits();


    // Run reconstruction by the CA Tracker
    caTrackerInt.SetHits(*map);
    caTrackerInt.Run();
    mSeeds = &caTrackerInt.GetSeeds();
    if (!mSeeds->size()) { mEnded = 2; return 0;}
    sort(mSeeds->begin(), mSeeds->end(),SeedsCompareStatus );
  }
  int begEndFail=0;
  while (mSeeds->size()) {
  
    Seed_t &aSeed = mSeeds->back();
    vector<StiHit*>        _seedHits;
    int nHits = aSeed.vhit.size();
    for (int iHit=0;iHit<nHits;iHit++) 
    {
      StiHit *hit = aSeed.vhit[iHit]->hit;
      if (!hit || hit->timesUsed()) {
        if (!iHit) begEndFail++;
        continue;
      }
      _seedHits.push_back(hit);
    }
    mSeeds->pop_back(); mEnded = !mSeeds->size(); if (mEnded) return 0;
    if (_seedHits.size() < 4) continue;

    StiKalmanTrack* track = static_cast<StiKalmanTrack*>(StiToolkit::instance()->getTrackFactory()->getInstance());
    if (1 || !begEndFail)
      track->initialize0(_seedHits, &aSeed.firstNodePars, &aSeed.lastNodePars/*, &aSeed.firstNodeErrs, &aSeed.lastNodeErrs*/ ); // use CATracker parameters. P.S errors should not be copied, they'd be initialized.
    else 
      track->initialize0(_seedHits); 
    return track;
  }
  mEnded = 3; return 0;
}
//________________________________________________________________________________
StiCATpcSeedFinder* StiCALoader::New() { return new StiCATpcSeedFinder;}
