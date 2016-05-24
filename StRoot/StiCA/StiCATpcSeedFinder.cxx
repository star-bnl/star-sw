#include "StiCATpcSeedFinder.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiHit.h"
#include "StMessMgr.h"
#include "StTpcHit.h"
#include "StiCAKalmanTrack.h"
#include "StiCAKalmanTrackFinder.h"

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
Bool_t StiCATpcSeedFinder::SeedsCompareStatus(const Seed_t a, const Seed_t b){
  return (a.total_hits < b.total_hits);
}
//________________________________________________________________________________
void StiCATpcSeedFinder::findTpcTracks(StiCATpcTrackerInterface &caTrackerInt) {
#ifdef PRINT_FIT_ERR_STATISTIC
   static std::map<int,unsigned int> statusMap;
#endif // PRINT_FIT_ERR_STATISTIC

  
  // zero all banks before filling !!! 
  HitMapToVectorAndEndType& map =  StiToolkit::instance()->getHitContainer()->hits();


  // Run reconstruction by the CA Tracker
  caTrackerInt.SetHits(map);
  caTrackerInt.Run();
  vector<Seed_t> &seeds = caTrackerInt.GetSeeds();


  sort(seeds.begin(), seeds.end(),SeedsCompareStatus );
#ifdef PRINT_SEED_STATISTIC
  Int_t nSeed = 0;
#endif // PRINT_SEED_STATISTIC
  while (! seeds.empty()) {
    Seed_t &aSeed = seeds.back();
    vector<StiHit*>        _seedHits;
    for (vector<SeedHit_t *>::iterator hitb = aSeed.vhit.begin(); hitb != aSeed.vhit.end(); hitb++) {
      StiHit *hit = (*hitb)->hit;
      if (!hit || hit->timesUsed()) continue;
      _seedHits.push_back(hit);
    }
    seeds.pop_back();
#ifdef PRINT_SEED_STATISTIC
    cout<< "seed: " << nSeed++ << "\t" <<aSeed.total_hits<<" total hits "; //<<aSeed.used_per_total;
    cout << " no. unused hits " << _seedHits.size();
#endif // PRINT_SEED_STATISTIC
    if (_seedHits.size() < 4) {
#ifdef PRINT_SEED_STATISTIC
      cout << endl; 
#endif // PRINT_SEED_STATISTIC
      continue;
    }
    StiCAKalmanTrack* track = static_cast<StiCAKalmanTrack*>(StiToolkit::instance()->getTrackFactory()->getInstance());
    
//   if (track->initialize(_seedHits)) {cout << " initialization failed" << endl; continue;} // use helix approximation
   track->initialize0(_seedHits, &aSeed.firstNodePars, &aSeed.lastNodePars/*, &aSeed.firstNodeErrs, &aSeed.lastNodeErrs*/ ); // use CATracker parameters. P.S errors should not be copied, they'd be initialized.
   StiCAKalmanTrackFinder *finderTmp = (StiCAKalmanTrackFinder *)track->getTrackFinder();
   int status = finderTmp->Fit(track);
#ifdef PRINT_FIT_ERR_STATISTIC
   StiCAKalmanTrackFinder::PrintFitStatus(status,track);

   if ( statusMap.find(status) != statusMap.end() ) statusMap[status]++;
   else statusMap[status] = 1;
#endif // PRINT_FIT_ERR_STATISTIC
   
   if (status){
     BFactory::Free(track); // delete not fitted track
     continue;
   }
   track->setSeedHitCount(track->getSeedHitCount()+100);
  } // while (! seeds.empty())

#ifdef PRINT_FIT_ERR_STATISTIC
   cout << " ---- Fit status statistic.  ---- " << endl;
   for ( std::map<int, unsigned int>::iterator it = statusMap.begin(); it != statusMap.end(); it++) {
     cout << (*it).second << " entries for:" << endl;
     StiCAKalmanTrackFinder::PrintFitStatus((*it).first,0);
   }
#endif // PRINT_FIT_ERR_STATISTIC
}
