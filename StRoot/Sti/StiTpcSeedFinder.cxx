// $Id: StiTpcSeedFinder.cxx,v 2.7 2012/05/07 14:55:38 fisyak Exp $
#ifdef DO_TPCCATRACKER
#include "StMessMgr.h"
#include "StiTpcSeedFinder.h"
#include "StiToolkit.h"
#include "StiHit.h"
#include "StTpcHit.h"
#include "StTpcHit.h"
#include "StiKalmanTrack.h"
#include "StiKalmanTrackFinder.h"

#include "StiTPCCATrackerInterface.h"
//#define PRINT_SEED_STATISTIC
//#define PRINT_FIT_ERR_STATISTIC
#ifdef PRINT_FIT_ERR_STATISTIC
#include <map>
#endif // PRINT_FIT_ERR_STATISTIC
//#define EXTRAPOLATION_CUT
//#define KINK_REJECTION  
#define OVERLAP_REJECTION
//________________________________________________________________________________
Bool_t StiTpcSeedFinder::SeedsCompareStatus(const Seed_t a, const Seed_t b){
  return (a.total_hits < b.total_hits);
}
//________________________________________________________________________________
void StiTpcSeedFinder::findTpcTracks(StiTPCCATrackerInterface &caTrackerInt) {
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
  static Int_t _debug = 0;
  if (_debug) {
    for (UInt_t i  = 0; i <  seeds.size(); i++) {
      LOG_INFO << "Seed " << i << endm;
      Seed_t &aSeed = seeds[i];
      aSeed.Print();
    }
  }
#ifdef PRINT_SEED_STATISTIC
  Int_t nSeed = 0;
#endif // PRINT_SEED_STATISTIC
  while (! seeds.empty()) {
    Seed_t &aSeed = seeds.back();
    vector<StiHit*>        _seedHits;
    UInt_t sector = 0;
    for (vector<SeedHit_t *>::iterator hitb = aSeed.vhit.begin(); hitb != aSeed.vhit.end(); hitb++) {
      StiHit *hit = (*hitb)->hit;
      if (!hit) continue; // || hit->timesUsed()) continue;
      if (StiKalmanTrackFinder::DoAlignment()) {
	const StTpcHit *tpcHit = dynamic_cast<const StTpcHit *>(hit->stHit());
	if (tpcHit) {
	  if (! sector) sector = tpcHit->sector();
	  else if (sector != tpcHit->sector()) continue;
	}
      }
      // check that all hits are coming from the same sector
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
    StiKalmanTrack* track = StiToolkit::instance()->getTrackFactory()->getInstance();
    
//   if (track->initialize(_seedHits)) {cout << " initialization failed" << endl; continue;} // use helix approximation
   track->initialize0(_seedHits, &aSeed.firstNodePars, &aSeed.lastNodePars/*, &aSeed.firstNodeErrs, &aSeed.lastNodeErrs*/ ); // use CATracker parameters. P.S errors should not be copied, they'd be initialized.
   StiKalmanTrackFinder *finderTmp = (StiKalmanTrackFinder *)track->getTrackFinder();
   int status = finderTmp->Fit(track);
#ifdef PRINT_FIT_ERR_STATISTIC
   StiKalmanTrackFinder::PrintFitStatus(status,track);

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
     StiKalmanTrackFinder::PrintFitStatus((*it).first,0);
   }
#endif // PRINT_FIT_ERR_STATISTIC
}
#endif /* DO_TPCCATRACKER */
// $Log: StiTpcSeedFinder.cxx,v $
// Revision 2.7  2012/05/07 14:55:38  fisyak
// Clean up from hard coded Tpc parameters
//
// Revision 2.6  2011/02/05 14:58:04  fisyak
// reduce print outs
//
// Revision 2.5  2011/01/18 14:41:54  fisyak
// Suppress seed print outs
//
// Revision 2.4  2010/09/06 18:20:49  fisyak
// Add TPCCATracker
//
// Revision 1.7  2010/08/12 17:46:47  ikulakov
// Change output file for caPerfo.
//
// Revision 1.6  2010/08/09 17:51:15  mzyzak
// StiPerformance is added; bug with cov matrix of the seed parameters is fixed; bug with the q/p sign of the seed parameters is fixed; functionality of the performance is extended
//
// Revision 1.5  2010/08/05 21:16:53  ikulakov
// Add fit status statistic.
//
// Revision 1.4  2010/08/04 13:45:46  ikulakov
// Fix - hz & sign pt.
//
// Revision 1.3  2010/08/02 16:45:27  ikulakov
// Use tracks params obtained from CATracker for StRoot KF fitter initialization.
//
// Revision 1.2  2010/07/29 16:19:11  fisyak
// GSI CA tracker
//
// Revision 2.3  2010/03/12 21:38:50  fisyak
// Add EXTRAPOLATION_CUT, OVERLAP_REJECTION and KINK_REJECTION flags (Y.Gorbunov)
//
// Revision 2.2  2010/03/11 22:59:00  fisyak
// Fix print out
//
// Revision 2.1  2010/02/16 23:11:14  fisyak
// Add Yury Gorbunov's TPC seed finder
//
