#ifndef __StiCATpcSeedFinder_h__
#define __StiCATpcSeedFinder_h__
#include "Sti/StiHitContainer.h"

class StiCATpcTrackerInterface;

struct SeedHit_t {
#if 0
  Int_t mMinPad, mMaxPad, mMinTmbk, mMaxTmbk;
#endif
  Int_t padrow, status, taken, track_key ; //"m" for modified
  Double_t x,y,z;
  StiHit   *hit;
};
class Seed_t {
 public:
  vector<SeedHit_t *> vhit;
  Int_t total_hits;
  StiNodePars firstNodePars;
  StiNodePars lastNodePars;
  StiNodeErrs firstNodeErrs;
  StiNodeErrs lastNodeErrs;
  virtual void Print(Option_t *option="") const {
    firstNodePars.print();
    firstNodeErrs.print();
    lastNodePars.print();
    lastNodeErrs.print();
  }
};
class StiCATpcSeedFinder {
 public:
  static Bool_t   SeedsCompareStatus(const Seed_t a, const Seed_t b);
  static void     findTpcTracks(StiCATpcTrackerInterface &caTrackerInt);
};
#endif
