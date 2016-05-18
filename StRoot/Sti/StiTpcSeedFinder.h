// $Id: StiTpcSeedFinder.h,v 2.4.8.2 2016/05/18 20:07:20 smirnovd Exp $
#ifndef __StiTpcSeedFinder_h__
#define __StiTpcSeedFinder_h__
#ifdef DO_TPCCATRACKER
#include "StiHitContainer.h"

class StiTPCCATrackerInterface;

struct SeedHit_t {
#if 0
  Int_t mMinPad, mMaxPad, mMinTmbk, mMaxTmbk;
#endif
  Int_t padrow, status, taken, track_key ; //"m" for modified
  Double_t x,y,z;
  StiHit   *hit;
};
struct Seed_t {
  vector<SeedHit_t *> vhit;
  Int_t total_hits;
  StiNodePars firstNodePars;
  StiNodePars lastNodePars;
  StiNodeErrs firstNodeErrs;
  StiNodeErrs lastNodeErrs;
};
class StiTpcSeedFinder {
 public:
  static Bool_t   SeedsCompareStatus(const Seed_t a, const Seed_t b);
  static void     findTpcTracks(StiTPCCATrackerInterface &caTrackerInt);
};
// $Log: StiTpcSeedFinder.h,v $
// Revision 2.4.8.2  2016/05/18 20:07:20  smirnovd
// Revert previous commits on this branch
//
// Revision 2.4  2013/01/14 22:21:14  fisyak
// Clean up unused variables
//
// Revision 2.3  2012/05/07 14:55:38  fisyak
// Clean up from hard coded Tpc parameters
//
// Revision 2.2  2010/09/06 18:20:49  fisyak
// Add TPCCATracker
//
// Revision 1.4  2010/08/09 17:51:15  mzyzak
// StiPerformance is added; bug with cov matrix of the seed parameters is fixed; bug with the q/p sign of the seed parameters is fixed; functionality of the performance is extended
//
// Revision 1.3  2010/08/02 16:45:27  ikulakov
// Use tracks params obtained from CATracker for StRoot KF fitter initialization.
//
// Revision 1.2  2010/07/29 16:19:12  fisyak
// GSI CA tracker
//
// Revision 2.1  2010/02/16 23:11:14  fisyak
// Add Yury Gorbunov's TPC seed finder
//
#endif /* DO_TPCCATRACKER */
#endif
