// $Id: StiTpcSeedFinder.h,v 2.2 2010/09/06 18:20:49 fisyak Exp $
#ifndef __StiTpcSeedFinder_h__
#define __StiTpcSeedFinder_h__
#ifdef DO_TPCCATRACKER
#include "StiHitContainer.h"

class StiTPCCATrackerInterface;

struct SeedHit_t {
  Int_t mMinPad, mMaxPad, mMinTmbk, mMaxTmbk, padrow, status, taken, track_key ; //"m" for modified
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
  static Int_t    padp(Int_t pad, Int_t row);
  static Double_t padpF(Double_t pad, Int_t row);
  static Double_t padpFNoScale(Double_t pad, Int_t row);
  static Double_t shiftToPad(Double_t spad, Int_t row);
  static Double_t padToX(Double_t pad, Int_t row);
  static Double_t xToPad(Double_t x, Int_t row);
  static Double_t rowToY(Int_t row);
  static Double_t zToTime(Double_t z, Int_t row);
  static Double_t timeToZ(Double_t time, Int_t row);
  static void     clusterXYZ(Double_t pad, Int_t row, Double_t time, Double_t &x, Double_t &y, Double_t &z);
  static void     clusterPRT(Double_t x, Double_t z, Double_t &pad, Double_t &time, Int_t row);
  static Bool_t   OverONot(SeedHit_t *hit, SeedHit_t *match, Int_t tollerance);
  static Bool_t   HitsCompareStatus(const SeedHit_t a, const SeedHit_t b);
  static Bool_t   SeedsCompareStatus(const Seed_t a, const Seed_t b);
  static void     findTpcTracks(StiTPCCATrackerInterface &caTrackerInt);
};
// $Log: StiTpcSeedFinder.h,v $
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
