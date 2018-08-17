//*-- Author : Yuri Fisyak
// $Id: StxMaker.cxx,v 1.6 2013/09/16 19:54:04 fisyak Exp $
#include "StxMaker.h"
#include "StxCAInterface.h"
#include "StxSeedFinder.h"
#include "StEvent/StEvent.h"
#include "StEvent/StGlobalTrack.h"
#include "StEvent/StL3Trigger.h"
#include "StEvent/StTrackDetectorInfo.h"
#include "StEvent/StTrackNode.h"
	       //#include "StxMaker/StxStEventFiller.h"
#include "TMath.h"
#if 0
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiDefaultToolkit.h"
#include "StiMaker/StiStEventFiller.h"
#endif
ClassImp(StxMaker);
//_____________________________________________________________________________
Int_t StxMaker::Init(){
  // Create tables
  // Create Histograms    
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StxMaker::Make(){
  StEvent   *mEvent = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
  // zero all banks before filling !!! 
  if (! mEvent) {return kStWarn;};
  StxCAInterface& caTrackerInt = StxCAInterface::Instance();
  caTrackerInt.SetNewEvent();
  // Run reconstruction by the CA Tracker
  caTrackerInt.Run();
  vector<Seedx_t> &seeds = caTrackerInt.GetSeeds();
  caTrackerInt.RunPerformance();
  sort(seeds.begin(), seeds.end(),StxSeedFinder::SeedsCompareStatus );
#define PRINT_SEED_STATISTIC
#ifdef PRINT_SEED_STATISTIC
  Int_t nSeed = 0;
#endif // PRINT_SEED_STATISTIC
  
  while (! seeds.empty()) {
    Seedx_t &aSeed = seeds.back();
    vector<const StTpcHit*>        _seedHits;
    for (vector<const StTpcHit *>::iterator hitb = aSeed.vhit.begin(); hitb != aSeed.vhit.end(); hitb++) {
      const StTpcHit *hit = (*hitb);
      if (!hit) continue; // || hit->timesUsed()) continue;
#if 0
      hit->setTimesUsed(hit->timesUsed()+1);
#endif
      _seedHits.push_back(hit);
    }
    seeds.pop_back();
#ifdef PRINT_SEED_STATISTIC
    cout<< "seed: " << nSeed++ << "\t" <<aSeed.vhit.size()<<" total hits "; //<<aSeed.used_per_total;
    cout << " no. unused hits " << _seedHits.size() << endl;
#endif // PRINT_SEED_STATISTIC
    if (_seedHits.size() < 4) {
      continue;
    }
  }
  return kStOK;
}
// $Log: StxMaker.cxx,v $
