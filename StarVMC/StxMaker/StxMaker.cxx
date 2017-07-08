//*-- Author : Yuri Fisyak
// $Id: StxMaker.cxx,v 1.6 2013/09/16 19:54:04 fisyak Exp $
#include "StxMaker.h"
#include "StxInterface.h"
#include "StxSeedFinder.h"
#include "StEvent/StEvent.h"
#include "StEvent/StGlobalTrack.h"
#include "StEvent/StL3Trigger.h"
#include "StEvent/StTrackDetectorInfo.h"
#include "StEvent/StTrackNode.h"
	       //#include "StxMaker/StxStEventFiller.h"
#include "TMath.h"

ClassImp(StxMaker);
//_____________________________________________________________________________
Int_t StxMaker::Init(){
  // Create tables
  // Create Histograms    
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StxMaker::Make(){
#if 0
  StxStEventFiller *filler = StxStEventFiller::instance();
#endif
  StEvent   *mEvent = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
  // zero all banks before filling !!! 
  if (! mEvent) {return kStWarn;};
  StxInterface& caTrackerInt = StxInterface::Instance();
  caTrackerInt.SetNewEvent();
  // Run reconstruction by the CA Tracker
  caTrackerInt.Run(mEvent);
  vector<Seed_t> &seeds = caTrackerInt.GetSeeds();
#if 0
  caTrackerInt.SetStxTracks(StxToolkit::instance()->getTrackContainer());
#endif
  caTrackerInt.RunPerformance();
  Int_t key = 1;
  sort(seeds.begin(), seeds.end(),StxSeedFinder::SeedsCompareStatus );
  //#define PRINT_SEED_STATISTIC
#ifdef PRINT_SEED_STATISTIC
  Int_t nSeed = 0;
#endif // PRINT_SEED_STATISTIC
  
  while (! seeds.empty()) {
    Seed_t &aSeed = seeds.back();
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
#if 0    
    StxKalmanTrack* kTrack = StxToolkit::instance()->getTrackFactory()->getInstance();
//   if (kTrack->initialize(_seedHits)) {cout << " initialization failed" << endl; continue;} // use helix approximation
   kTrack->initialize0(_seedHits, &aSeed.firstNodePars, &aSeed.lastNodePars, &aSeed.firstNodeErrs, &aSeed.lastNodeErrs ); // use CATracker parameters. P.S errors should not be copied, they'd be initialized.
   kTrack->setSeedHitCount(kTrack->getSeedHitCount()+100);
   StHit dcaHit; dcaHit.makeDca();
   StxTrackNode *extenDca = kTrack->extendToVertex(&dcaHit);
   if (extenDca) {
     kTrack->add(extenDca,kOutsideIn);
   }
   if (Debug()) {
     kTrack->print();
   }
   if (badCAParameters(kTrack,kFALSE) || badCAParameters(kTrack,kTRUE)) {
     BFactory::Free(kTrack); // delete track
     continue;
   }
   if (filler && mEvent) {
     StL3Trigger *l3Trigger = mEvent->l3Trigger();
     if (! l3Trigger) {
       l3Trigger = new StL3Trigger();
       mEvent->setL3Trigger(l3Trigger);
     }
     StSPtrVecTrackNode& trNodeVec = l3Trigger->trackNodes(); 
     StTrackDetectorInfo* detInfo = new StTrackDetectorInfo;
     filler->fillDetectorInfo(detInfo,kTrack,kFALSE); //3d argument used to increase/not increase the refCount. MCBS oct 04.
     StSPtrVecTrackDetectorInfo& detInfoVec = l3Trigger->trackDetectorInfo();
     // track node where the new StTrack will reside
     StTrackNode* trackNode = new StTrackNode;
     // actual filling of StTrack from StxKalmanTrack
     StGlobalTrack* gTrack = new StGlobalTrack;
     // filling successful, set up relationships between objects
     //     filler->detInfoVec.push_back(detInfo);
     //cout <<"Setting key: "<<(unsigned short)(trNodeVec.size())<<endl;
     filler->fillTrack(gTrack,kTrack,detInfo);
     if (gTrack->geometry() &&  gTrack->outerGeometry()) {
       detInfoVec.push_back(detInfo);
       gTrack->setKey(key++);
       gTrack->setIdTruth();
       filler->fillDca(gTrack,kTrack);
       if (Debug()) {
	 cout << *gTrack << endl;
       }
       trackNode->addTrack(gTrack);
       trNodeVec.push_back(trackNode);
       // reuse the utility to fill the topology map
       // this has to be done at the end as it relies on
       // having the proper track->detectorInfo() relationship
       // and a valid StDetectorInfo object.
       //cout<<"Tester: Event Track Node Entries: "<<trackNode->entries()<<endl;
     } else {
       delete detInfo;
       delete gTrack;
     }
   }
   BFactory::Free(kTrack); // delete track
#endif
 } // while (! seeds.empty())
#if 0
   StxToolkit::instance()->getHitContainer()->reset();
#endif
   return kStOK;
}
// $Log: StxMaker.cxx,v $
