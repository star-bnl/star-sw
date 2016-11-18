#include <assert.h>
#include <string.h>
#include "TVector3.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiHit.h"
#include "StiUtilities/StiDebug.h"
#include "StMessMgr.h"
#include "StEvent/StTpcHit.h"
#include "Sti/StiKalmanTrack.h"
#include "StEvent/StTpcHit.h"

#include "StiCATpcTrackerInterface.h"
//#define PRINT_SEED_STATISTIC
//#define PRINT_FIT_ERR_STATISTIC
#ifdef PRINT_FIT_ERR_STATISTIC
#include <map>
#endif // PRINT_FIT_ERR_STATISTIC
//#define EXTRAPOLATION_CUT
//#define KINK_REJECTION  
#define OVERLAP_REJECTION
//#define StiCATpcSeedFinderBLOG
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
//	Workaround for bug in CA.  Sometimes:
//	1. hits badRxyed
//	2. same hit is used twice in one track

    auto myLambda = [](SeedHit_t *a, SeedHit_t *b) 
    { 
     const StiHit *ah = a->hit;
     const StiHit *bh = b->hit;
     return (ah->x_g()*ah->x_g()+ah->y_g()*ah->y_g())>(bh->x_g()*bh->x_g()+bh->y_g()*bh->y_g());
    };

    StiHit *preHit = aSeed.vhit[      0]->hit;
    StiHit *endHit = aSeed.vhit[nHits-1]->hit;
    int sortIt = 0;
#ifdef StiCATpcSeedFinderBLOG
    int unsRxy=0,unsPad=0,reUsed=0;
#endif //StiCATpcSeedFinderBLOG

    for (int iHit=0;iHit<nHits-1;iHit++) 
    {

#ifdef StiCATpcSeedFinderBLOG
       StiHit *hit = aSeed.vhit[iHit+1]->hit;
       StHit  *sthit = (StHit*)hit->stHit();
       double rXYsti = hit->rxy();
       double rXYste = sthit->position().perp();
       assert(fabs(rXYsti-rXYste)<1e-3);
       int padrow0 = ((StTpcHit*)(aSeed.vhit[iHit  ]->hit->stHit()))->padrow();
       int padrow1 = ((StTpcHit*)(aSeed.vhit[iHit+1]->hit->stHit()))->padrow();
StiDebug::Count("XlocOfPad",padrow0, hit->x());   
StiDebug::Count("RxyOfPad",padrow0, rXYste);   
StiDebug::Count("AngLoc",atan2(hit->y(),hit->x())/3.1415*180);   
       if (padrow0<=padrow1) {
         unsPad++;
         double rxy = sqrt(pow(hit->x_g(),2)+pow(hit->y_g(),2));
         double z = hit->z_g();
StiDebug::Count("UnsPadXY",aSeed.vhit[iHit+1]->hit->x_g(), aSeed.vhit[iHit+1]->hit->y_g());   
StiDebug::Count("UnsPadZR",z, rxy);   
       } 
#endif //StiCATpcSeedFinderBLOG

      if (myLambda(aSeed.vhit[iHit],aSeed.vhit[iHit+1])) continue;
      if (!sortIt) sortIt = 100+iHit; 

#ifdef StiCATpcSeedFinderBLOG
      unsRxy++;
      double rxy = sqrt(pow(hit->x_g(),2)+pow(hit->y_g(),2));
      double z = hit->z_g();
StiDebug::Count("UnsHitXY",aSeed.vhit[iHit+1]->hit->x_g(), aSeed.vhit[iHit+1]->hit->y_g());   
StiDebug::Count("UnsHitZR",z, rxy);   
static int printIt = 0;
      if (!printIt) continue;
{
    for (int iHit=0;iHit<nHits-1;iHit++) {
      StiHit *hit = aSeed.vhit[iHit]->hit;if (!hit) continue;
      double rxy = sqrt(pow(hit->x_g(),2)+pow(hit->y_g(),2));
      double z = hit->z_g();
      printf( "hit[%d] %p Rxy=%g\t Z=%g\n",iHit,hit,rxy,z);
}   }
    continue;
#endif //StiCATpcSeedFinderBLOG
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
      StiHit *hit = aSeed.vhit[iHit]->hit;if (!hit) continue;
      if (hit->timesUsed() || hit == preHit) {

#ifdef StiCATpcSeedFinderBLOG
        reUsed++;
        double rxy = sqrt(pow(hit->x_g(),2)+pow(hit->y_g(),2));

        if (hit == preHit) {StiDebug::Count("SameHitXY",hit->x_g(),hit->y_g());
	                    StiDebug::Count("SameHitZR",hit->z_g(),rxy       );}
        else               {StiDebug::Count("SkipHitXY",hit->x_g(),hit->y_g());
	                    StiDebug::Count("SkipHitZR",hit->z_g(),rxy       );}
#endif //StiCATpcSeedFinderBLOG

        if (!iHit || iHit == nHits-1) {
	  begEndFail++;
#ifdef StiCATpcSeedFinderBLOG
	  StiDebug::Count("BegEndXY" ,hit->x_g(),hit->y_g());
	  StiDebug::Count("BegEndZR" ,hit->z_g(),rxy       );
#endif //StiCATpcSeedFinderBLOG
	}
        continue;
      }
      preHit = hit;
assert(!hit->timesUsed());
      _seedHits.push_back(hit);
    }

#ifdef StiCATpcSeedFinderBLOG
    if (unsPad) {
    
       double pct = unsPad*100./nHits;
StiDebug::Count("UnsPad_Pct",unsPad, pct);   
    }
    if (unsRxy) {
    
       double pct = unsRxy*100./nHits;
StiDebug::Count("UnsRxy_Pct",unsRxy, pct);   
    }
    if (reUsed) {
    
       double pct = reUsed*100./nHits;
StiDebug::Count("Reused_Pct",reUsed, pct);   
    }
#endif //StiCATpcSeedFinderBLOG

    StiKalmanTrack* track = 0;
    if (_seedHits.size() >=4)  {

    track = static_cast<StiKalmanTrack*>(StiToolkit::instance()->getTrackFactory()->getInstance());
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
//________________________________________________________________________________
StiCATpcSeedFinder* StiCALoader::New() { return new StiCATpcSeedFinder;}
