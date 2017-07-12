#include <assert.h>
#include <string.h>
#include "TVector3.h"
#include "StMessMgr.h"
#include "StEvent/StEvent.h"
#include "StEvent/StTpcHit.h"
#include "StEvent/StTpcHit.h"

#include "StxInterface.h"
//#define PRINT_SEED_STATISTIC
//#define PRINT_FIT_ERR_STATISTIC
#ifdef PRINT_FIT_ERR_STATISTIC
#include <map>
#endif // PRINT_FIT_ERR_STATISTIC
//#define EXTRAPOLATION_CUT
//#define KINK_REJECTION  
#define OVERLAP_REJECTION
//________________________________________________________________________________
Bool_t StxSeedFinder::SeedsCompareStatus(const Seed_t a, const Seed_t b) {
  return (a.vhit.size() < b.vhit.size());
}
#if 0
//________________________________________________________________________________
StxTrack *StxSeedFinder::findTrack(double rMin) { 
  static StxInterface& caTrackerInt = StxInterface::Instance();
  if (!mSeeds || mSeeds->size()==0) {
    if (mEnded) return 0;
    caTrackerInt.SetNewEvent();
    // zero all banks before filling !!! 
    auto *map =  &StxToolkit::instance()->getHitContainer()->hits();


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
    vector<StxHit*>        _seedHits;
    int nHits = aSeed.vhit.size();
//	Workaround for bug in CA.  Sometimes:
//	1. hits badRxyed
//	2. same hit is used twice in one track

    auto myLambda = [](SeedHit_t *a, SeedHit_t *b) 
    { 
     const StxHit *ah = a->hit;
     const StxHit *bh = b->hit;
     return (ah->x_g()*ah->x_g()+ah->y_g()*ah->y_g())>(bh->x_g()*bh->x_g()+bh->y_g()*bh->y_g());
    };

    StxHit *preHit = aSeed.vhit[      0]->hit;
    StxHit *endHit = aSeed.vhit[nHits-1]->hit;
    int sortIt = 0;

    for (int iHit=0;iHit<nHits-1;iHit++) 
    {


      if (myLambda(aSeed.vhit[iHit],aSeed.vhit[iHit+1])) continue;
      if (!sortIt) sortIt = 100+iHit; 

    break;
    }


    if (sortIt) {
      std::sort(aSeed.vhit.begin(), aSeed.vhit.end(), myLambda);
      if (preHit != aSeed.vhit[      0]->hit) begEndFail+=1;
      if (endHit != aSeed.vhit[nHits-1]->hit) begEndFail+=2;
    }
    preHit = 0;
    for (int iHit=0;iHit<nHits;iHit++) 
    {
      StxHit *hit = aSeed.vhit[iHit]->hit;if (!hit) continue;
      if (hit->timesUsed() || hit == preHit) {


        if (!iHit || iHit == nHits-1) {
	  begEndFail++;
	}
        continue;
      }
      preHit = hit;
assert(!hit->timesUsed());
      _seedHits.push_back(hit);
    }


    StxKalmanTrack* track = 0;
    if (_seedHits.size() >=4)  {

    track = static_cast<StxKalmanTrack*>(StxToolkit::instance()->getTrackFactory()->getInstance());
    if (!begEndFail)
      track->initialize0(_seedHits, &aSeed.firstNodePars, &aSeed.lastNodePars/*, &aSeed.firstNodeErrs, &aSeed.lastNodeErrs*/ ); // use CATracker parameters. P.S errors should not be copied, they'd be initialized.
    else 
      track->initialize0(_seedHits); 
    }
    mSeeds->pop_back(); mEnded = !mSeeds->size();
    if (!track && !mEnded) continue;
    return track;
  }
  mEnded = 3; return 0;
}
#endif
