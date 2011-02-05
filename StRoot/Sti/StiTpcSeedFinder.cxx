// $Id: StiTpcSeedFinder.cxx,v 2.6 2011/02/05 14:58:04 fisyak Exp $
#ifdef DO_TPCCATRACKER
#include "StiTpcSeedFinder.h"
#include "StiToolkit.h"
#include "StiHit.h"
#include "StMessMgr.h"
#include "StTpcHit.h"
#include "StiKalmanTrack.h"
#include "StiKalmanTrackFinder.h"

#include "StiTPCCATrackerInterface.h"
//#define PRINT_SEED_STATISTIC
//#define PRINT_FIT_ERR_STATISTIC
#ifdef PRINT_FIT_ERR_STATISTIC
#include <map>
#endif // PRINT_FIT_ERR_STATISTIC


static const Short_t kPadRows = 44;
static const Short_t kStartRow = 0;
static const Double_t innerSectorPadPitch = 0.335;
static const Double_t outerSectorPadPitch = 0.675;
static const Double_t mTimeBinWidth = 1.06580379191673078e-7;
static const Double_t DriftVelocity = 5.5e-6;
static const Int_t split = 99;
static const Int_t bsize = 64000;
static const Int_t kMaxSector = 24;
//#define EXTRAPOLATION_CUT
//#define KINK_REJECTION  
#define OVERLAP_REJECTION

static Double_t innerR[13] = {60.000,  64.800,  69.600,  74.400,  79.200, //  5
			      84.000,  88.800,  93.600,  98.800, 104.000, // 10
			      109.200, 114.400, 119.600};
static Double_t outerR[32] = {127.195, 129.195, // 15
			      131.195, 133.195, 135.195, 137.195, 139.195, // 20
			      141.195, 143.195, 145.195, 147.195, 149.195, // 25
			      151.195, 153.195, 155.195, 157.195, 159.195, // 30
			      161.195, 163.195, 165.195, 167.195, 169.195, // 35
			      171.195, 173.195, 175.195, 177.195, 179.195, // 40
			      181.195, 183.195, 185.195, 187.195, 189.195};// 45
static const Int_t NumberOfPadsAtRow[45] = {
    88, 96,104,112,118,126,134,142,150,158, // Inner
   166,174,182,
                98,100,102,104,106,106,108, // Outer
   110,112,112,114,116,118,120,122,122,124,
   126,128,128,130,132,134,136,138,138,140,
   142,144,144,144,144
};
//________________________________________________________________________________
Int_t StiTpcSeedFinder::padp(Int_t pad, Int_t row) {
  Int_t p = 0;
  p = (Int_t)(pad - NumberOfPadsAtRow[row]/2);
  if(row<13)
    p = (Int_t)p/2;
  return p;
}
//________________________________________________________________________________
Double_t StiTpcSeedFinder::padpF(Double_t pad, Int_t row) {
  Double_t p = 0;
  p = (Double_t)(pad - NumberOfPadsAtRow[row]/2);
  if(row<13)
    p = (Double_t)p/2;
  return p;
}
//________________________________________________________________________________
Double_t StiTpcSeedFinder::padpFNoScale(Double_t pad, Int_t row){
  Double_t spad = pad - NumberOfPadsAtRow[row]/2;
  return spad;
}
//________________________________________________________________________________
Double_t StiTpcSeedFinder::shiftToPad(Double_t spad, Int_t row){
  Double_t pad = spad + (NumberOfPadsAtRow[row])/2.;
  return pad;
}
//________________________________________________________________________________
Double_t StiTpcSeedFinder::padToX(Double_t pad, Int_t row){
  Double_t x = 0;
  if(row >= 13)
    x = outerSectorPadPitch * padpFNoScale(pad, row);
  else
    x = innerSectorPadPitch * padpFNoScale(pad, row);
  return x;
}
//________________________________________________________________________________
Double_t StiTpcSeedFinder::xToPad(Double_t x, Int_t row){
  Double_t pad = 0;
  if(row >= 13)
    pad = shiftToPad(x/outerSectorPadPitch, row);
  else
    pad = shiftToPad(x/innerSectorPadPitch, row);
  return pad;
}
//________________________________________________________________________________
Double_t StiTpcSeedFinder::rowToY(Int_t row){
  Double_t y;
  if(row>=13)
    y = outerR[row-13];
  else
    y = innerR[row];
  return y;
}
//________________________________________________________________________________
Double_t StiTpcSeedFinder::zToTime(Double_t z, Int_t row){
  Double_t time = 0;
  time = z;
  return time;
}
//________________________________________________________________________________
Double_t StiTpcSeedFinder::timeToZ(Double_t time, Int_t row){
  Double_t z = 0;
  z = time;
  return z;
}
//________________________________________________________________________________
void StiTpcSeedFinder::clusterXYZ(Double_t pad, Int_t row, Double_t time, Double_t &x, Double_t &y, Double_t &z) {
  x = padToX(pad, row);
  y = rowToY(row);
  z = timeToZ(time, row);
}
//________________________________________________________________________________
void StiTpcSeedFinder::clusterPRT(Double_t x, Double_t z, Double_t &pad, Double_t &time, Int_t row){
  pad = xToPad(x, row);
  time = zToTime(z, row);//(z * DriftVelocity) / mTimeBinWidth;
}
//________________________________________________________________________________
Bool_t StiTpcSeedFinder::OverONot(SeedHit_t *hit, SeedHit_t *match, Int_t tollerance) {
  Bool_t status = kFALSE;
  if( (((hit->mMinPad  >= match->mMinPad-tollerance && 
	 hit->mMinPad  <= match->mMaxPad+tollerance) || 
	(hit->mMaxPad  >= match->mMinPad-tollerance && 
	 hit->mMaxPad  <= match->mMaxPad+tollerance)) 
       && 
       (
	(hit->mMinTmbk >= match->mMinTmbk-tollerance && 
	 hit->mMinTmbk <= match->mMaxTmbk+tollerance) || 
	(hit->mMaxTmbk >= match->mMinTmbk-tollerance && 
	 hit->mMaxTmbk <= match->mMaxTmbk+tollerance)
	)
       ) 
      ||
      (((hit->mMinPad  >= match->mMinPad-tollerance &&
	 hit->mMaxPad  <= match->mMaxPad+tollerance) || 
	(hit->mMinPad  <= match->mMinPad+tollerance &&
	 hit->mMaxPad  >= match->mMaxPad-tollerance)) && 
       ((hit->mMinTmbk >= match->mMinTmbk+tollerance &&
	 hit->mMaxTmbk <= match->mMaxTmbk-tollerance) || 
	(hit->mMinTmbk <= match->mMinTmbk+tollerance && 
	 hit->mMaxTmbk >= match->mMaxTmbk-tollerance))) ) {
    status = kTRUE;
  }
  return status;
}
//________________________________________________________________________________
// sort for the status 
Bool_t StiTpcSeedFinder::HitsCompareStatus(const SeedHit_t a, const SeedHit_t b){
  return (a.status > b.status);
}
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
