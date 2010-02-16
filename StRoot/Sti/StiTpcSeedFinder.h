// $Id: StiTpcSeedFinder.h,v 2.1 2010/02/16 23:11:14 fisyak Exp $
#ifndef __StiTpcSeedFinder_h__
#define __StiTpcSeedFinder_h__
#include "StiHitContainer.h"
struct Hit_t {
  Int_t mMinPad, mMaxPad, mMinTmbk, mMaxTmbk, padrow, status, taken, track_key ; //"m" for modified
  Double_t x,y,z;
  StiHit   *hit;
};
struct Seed_t {
  vector<Hit_t *> vhit;
  Int_t total_hits;
  Float_t used_per_total;
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
  static Bool_t   OverONot(Hit_t *hit, Hit_t *match, Int_t tollerance);
  static Bool_t   HitsCompareStatus(const Hit_t a, const Hit_t b);
  static Bool_t   SeedsCompareStatus(const Seed_t a, const Seed_t b);
  static void     findTpcTracks();
};
// $Log: StiTpcSeedFinder.h,v $
// Revision 2.1  2010/02/16 23:11:14  fisyak
// Add Yury Gorbunov's TPC seed finder
//
#endif
