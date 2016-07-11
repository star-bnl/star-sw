//*-- Author : Victor Perevoztchikov
// 
// $Id: StHLTCAMaker.cxx,v 1.6 2013/09/16 19:54:04 fisyak Exp $
// $Log: StHLTCAMaker.cxx,v $
// Revision 1.6  2013/09/16 19:54:04  fisyak
// Add check for CA parameters
//
// Revision 1.5  2013/09/06 21:13:07  fisyak
// Move check for CA
//
// Revision 1.4  2013/08/30 20:18:01  fisyak
// Switch off cuts
//
// Revision 1.3  2013/08/29 15:45:11  fisyak
// Add protection versus bad parameters
//
// Revision 1.2  2013/08/26 22:54:43  fisyak
// Use covariance matrix from CA
//
// Revision 1.1.1.1  2013/08/14 12:58:34  fisyak
// First step
//
#include "StHLTCAMaker.h"
#include "StHLTTPCCATrackerInterface.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiTpcSeedFinder.h"
#include "Sti/StiKalmanTrackFinder.h"
#include "Sti/StiKalmanTrack.h"
#include "StEvent/StEvent.h"
#include "StEvent/StGlobalTrack.h"
#include "StEvent/StL3Trigger.h"
#include "StEvent/StTrackDetectorInfo.h"
#include "StEvent/StTrackNode.h"
#include "Sti/StiDefaultToolkit.h"
#include "StiMaker/StiStEventFiller.h"
#include "TMath.h"

ClassImp(StHLTCAMaker);
//_____________________________________________________________________________
/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t StHLTCAMaker::Init(){
  // Create tables
  // Create Histograms    
  return StMaker::Init();
}
//________________________________________________________________________________
Bool_t StHLTCAMaker::badCAParameters(StiKalmanTrack* track, Bool_t outer) {
  StiKalmanTrackNode* node = track->getInnOutMostNode(outer,3);
  StiHit *ihit = node->getHit();
  StThreeVectorF origin(node->x_g(),node->y_g(),node->z_g());
  StThreeVectorF hitpos(ihit->x_g(),ihit->y_g(),ihit->z_g());
  if (node->getDetector()) {
    double dif = (hitpos-origin).mag();
    if (dif>3.) {
      dif = node->z_g()-ihit->z_g();
      double nowChi2 = node->evaluateChi2(ihit);
      printf("***Track(%d) DIFF TOO BIG %g chi2 = %g %g\n",track->getId(),dif,node->getChi2(),nowChi2);
      printf("H=%g %g %g N =%g %g %g\n",ihit->x()   ,ihit->y()   ,ihit->z()
	     ,node->getX(),node->getY(),node->getZ());
      const StMeasuredPoint *mp = ihit->stHit();
      printf("H=%g %g %g N =%g %g %g\n",mp->position().x(),mp->position().y(),mp->position().z()
	     ,origin.x(),origin.y(),origin.z());
      if (TMath::Abs(dif) > 50.) return kTRUE;
    }
  }
  return kFALSE;
}
//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StHLTCAMaker::Make(){
  StiStEventFiller *filler = StiStEventFiller::instance();
  StEvent   *mEvent = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
  // zero all banks before filling !!! 
  HitMapToVectorAndEndType& map =  StiToolkit::instance()->getHitContainer()->hits();
  StHLTTPCCATrackerInterface& caTrackerInt = StHLTTPCCATrackerInterface::Instance();
  caTrackerInt.SetNewEvent();
  // Run reconstruction by the CA Tracker
  caTrackerInt.SetHits(map);
  caTrackerInt.Run();
  vector<Seed_t> &seeds = caTrackerInt.GetSeeds();
  caTrackerInt.SetStiTracks(StiToolkit::instance()->getTrackContainer());
  caTrackerInt.RunPerformance();
  Int_t key = 1;
  sort(seeds.begin(), seeds.end(),StiTpcSeedFinder::SeedsCompareStatus );
  //#define PRINT_SEED_STATISTIC
#ifdef PRINT_SEED_STATISTIC
  Int_t nSeed = 0;
#endif // PRINT_SEED_STATISTIC
  
  while (! seeds.empty()) {
    Seed_t &aSeed = seeds.back();
    vector<StiHit*>        _seedHits;
    for (vector<SeedHit_t *>::iterator hitb = aSeed.vhit.begin(); hitb != aSeed.vhit.end(); hitb++) {
      StiHit *hit = (*hitb)->hit;
      if (!hit) continue; // || hit->timesUsed()) continue;
      hit->setTimesUsed(hit->timesUsed()+1);
      _seedHits.push_back(hit);
    }
    seeds.pop_back();
#ifdef PRINT_SEED_STATISTIC
    cout<< "seed: " << nSeed++ << "\t" <<aSeed.total_hits<<" total hits "; //<<aSeed.used_per_total;
    cout << " no. unused hits " << _seedHits.size() << endl;
#endif // PRINT_SEED_STATISTIC
    if (_seedHits.size() < 4) {
      continue;
    }
    
    StiKalmanTrack* kTrack = StiToolkit::instance()->getTrackFactory()->getInstance();
//   if (kTrack->initialize(_seedHits)) {cout << " initialization failed" << endl; continue;} // use helix approximation
   kTrack->initialize0(_seedHits, &aSeed.firstNodePars, &aSeed.lastNodePars, &aSeed.firstNodeErrs, &aSeed.lastNodeErrs ); // use CATracker parameters. P.S errors should not be copied, they'd be initialized.
   kTrack->setSeedHitCount(kTrack->getSeedHitCount()+100);
   StiHit dcaHit; dcaHit.makeDca();
   StiTrackNode *extenDca = kTrack->extendToVertex(&dcaHit);
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
     // actual filling of StTrack from StiKalmanTrack
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
 } // while (! seeds.empty())

   StiToolkit::instance()->getHitContainer()->reset();
   return kStOK;
}
