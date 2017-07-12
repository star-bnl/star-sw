#ifndef __StxSeedFinder_h__
#define __StxSeedFinder_h__
#include <vector>
#include "StEvent/StTpcHit.h"
class StxInterface;
class Seed_t {
 public:
  vector<const StTpcHit *> vhit;
#if 0
  StxNodePars firstNodePars;
  StxNodePars lastNodePars;
  StxNodeErrs firstNodeErrs;
  StxNodeErrs lastNodeErrs;
#endif
  virtual void Print(Option_t *option="") const {
#if 0
    firstNodePars.print();
    firstNodeErrs.print();
    lastNodePars.print();
    lastNodeErrs.print();
#endif
  }
};
class StxSeedFinder {
 public:
  StxSeedFinder(){mSeeds=0;mEnded=0;/* setName("CASeedFinder");*/}
  virtual void      startEvent(){mEnded=0;}; 
#if 0
  virtual StxTrack *findTrack(double rMin=0); 
#endif
  static Bool_t   SeedsCompareStatus(const Seed_t a, const Seed_t b);
//??  static void     findTpcTracks(StxInterface &caTrackerInt);
  protected:
  int mEnded;
  std::vector<Seed_t> *mSeeds;
};
#endif
