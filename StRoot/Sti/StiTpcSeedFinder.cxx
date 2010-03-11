// $Id: StiTpcSeedFinder.cxx,v 2.2 2010/03/11 22:59:00 fisyak Exp $
#include "StiTpcSeedFinder.h"
#include "StiToolkit.h"
#include "StiHit.h"
#include "StMessMgr.h"
#include "StTpcHit.h"
#include "StiKalmanTrack.h"
#include "StiKalmanTrackFinder.h"
static const Short_t kPadRows = 44;
static const Short_t kStartRow = 0;
static const Double_t innerSectorPadPitch = 0.335;
static const Double_t outerSectorPadPitch = 0.675;
static const Double_t mTimeBinWidth = 1.06580379191673078e-7;
static const Double_t DriftVelocity = 5.5e-6;
static const Int_t split = 99;
static const Int_t bsize = 64000;
static const Int_t kMaxSector = 24;
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
Bool_t StiTpcSeedFinder::OverONot(Hit_t *hit, Hit_t *match, Int_t tollerance) {
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
Bool_t StiTpcSeedFinder::HitsCompareStatus(const Hit_t a, const Hit_t b){
  return (a.status > b.status);
}
//________________________________________________________________________________
Bool_t StiTpcSeedFinder::SeedsCompareStatus(const Seed_t a, const Seed_t b){
  return (a.total_hits < b.total_hits);
}
//________________________________________________________________________________
void StiTpcSeedFinder::findTpcTracks() {
  // zero all banks before filling !!! 
  Hit_t hitc;
  Double_t x,y,z;
  vector<Hit_t>        hitSecRow[25][46];
  HitMapToVectorAndEndType& map =  StiToolkit::instance()->getHitContainer()->hits();
  HitMapToVectorAndEndType::iterator it;
  StiHit *hit = 0;
  for (it=map.begin(); it!=map.end(); ++it)     {
    vector<StiHit*>& tempvec = (*it).second.hits();
    vector<StiHit*>::iterator  start = tempvec.begin();
    vector<StiHit*>::iterator  stop  = tempvec.end();
    for (vector<StiHit*>::iterator cit = start; cit != stop; cit++) {
      hit = *cit;
      if (! hit->stHit()) continue;
      const StTpcHit *tpcHit = dynamic_cast<const StTpcHit*>(hit->stHit());
      if ( ! tpcHit) continue;
      hitc.mMinPad  = padp((Int_t) tpcHit->minPad(),  tpcHit->padrow()-1);
      hitc.mMaxPad  = padp((Int_t) tpcHit->maxPad(),  tpcHit->padrow()-1);
      hitc.mMinTmbk = (Int_t) tpcHit->minTmbk();
      hitc.mMaxTmbk = (Int_t) tpcHit->maxTmbk();
      hitc.padrow = tpcHit->padrow()-1;
      clusterXYZ(tpcHit->pad(), tpcHit->padrow(), tpcHit->timeBucket(), x, y, z);
      hitc.x=x;
      hitc.y=y;
      hitc.z=z;
      hitc.status=0;
      hitc.taken=0;
      hitc.track_key=tpcHit->idTruth();
      hitc.hit  = hit;
      hitSecRow[(tpcHit->sector()-1)][hitc.padrow].push_back(hitc);
    }
  }
  //  done with hits .. let's keep going  .. first run .. marking 
  Hit_t *hitFirst, *hitSecond; 
  for(Int_t i=0;i<kMaxSector;i++){
    for(Int_t j=0;j<46;j++){
      for (vector<Hit_t>::iterator hitbIt1 = hitSecRow[i][j].begin();hitbIt1 != hitSecRow[i][j].end(); ++hitbIt1){
	for (vector<Hit_t>::iterator hitbIt2 = hitSecRow[i][(j+1)].begin();hitbIt2 != hitSecRow[i][(j+1)].end(); ++hitbIt2){
	  hitFirst=&(*hitbIt1);
	  hitSecond=&(*hitbIt2);
	  if(OverONot(hitFirst,hitSecond,2)) hitbIt2->status = hitbIt1->status + 1;
	}
      }
    }
  }
  // define here how many overlap can it be 
  for(Int_t i=0;i<kMaxSector;i++)
    for(Int_t j=0;j<46;j++)
      sort(hitSecRow[i][j].begin(), hitSecRow[i][j].end(),HitsCompareStatus );
  Int_t HitsUsedTimes = 0;
  vector<Seed_t> seeds;
  seeds.clear();
  Seed_t seed;
  seed.vhit.clear();
  
  for(Int_t i=0;i<kMaxSector;i++){
    for(Int_t j=45;j>0;j--){
      for (vector<Hit_t>::iterator hitbIt1 = hitSecRow[i][j].begin();hitbIt1 != hitSecRow[i][j].end(); ++hitbIt1){
	// check status here .. if it's done right 
	if(!hitbIt1->taken)
	  for (vector<Hit_t>::iterator hitbIt2 = hitSecRow[i][(j-1)].begin();hitbIt2 != hitSecRow[i][(j-1)].end(); ++hitbIt2){
	    if(OverONot((&(*hitbIt1)),(&(*hitbIt2)),2) && hitbIt2->taken < 3 && hitbIt1->taken < 3 )	      {
	      // do the search till the end of the list 
	      seed.vhit.clear();
	      HitsUsedTimes=0;
	      HitsUsedTimes+=hitbIt2->taken;
	      hitFirst=&(*hitbIt1);
	      hitSecond=&(*hitbIt2);
	      hitbIt2->taken++;
	      hitbIt1->taken++;
	      seed.vhit.push_back(hitSecond);
	      seed.vhit.push_back(hitFirst);
	      for(Int_t ij=(j-2);ij>=0;ij--){
		for (vector<Hit_t>::iterator hitbIt3 = hitSecRow[i][(ij)].begin();hitbIt3 != hitSecRow[i][(ij)].end(); ++hitbIt3){
		  if(OverONot(hitSecond,(&(*hitbIt3)),2) && hitSecond->taken < 3 && hitbIt3->taken < 3 ){
		    HitsUsedTimes+=hitbIt3->taken;
		    hitSecond=&(*hitbIt3);
		    hitbIt3->taken++;
		    seed.vhit.push_back(hitSecond);
		  }
		  else{
		    // something should go here ... 
		  }
		}
	      }
	      // skip the seed if it has too many reused hits 
	      if(seed.vhit.size()>4 && (((Float_t)HitsUsedTimes)/seed.vhit.size()) < 0.7 ){
		seed.total_hits=seed.vhit.size();
		seed.used_per_total=((Float_t)HitsUsedTimes)/seed.total_hits;
		seeds.push_back(seed);
	      }
	      else{
		for (vector<Hit_t *>::iterator cluIt = seed.vhit.begin();
		     cluIt != seed.vhit.end(); ++cluIt){
		  (*cluIt)->taken--;
		  // hits relase .. if the seed failed to be formed 
		}
	      }
	    }
	  }
      }
    }
  }
  sort(seeds.begin(), seeds.end(),SeedsCompareStatus );
  Int_t nSeed = 0;
  while (! seeds.empty()) {
    Seed_t &aSeed = seeds.back();
    vector<StiHit*>        _seedHits;
    for (vector<Hit_t *>::iterator hitb = aSeed.vhit.begin(); hitb != aSeed.vhit.end(); hitb++) {
      hit = (*hitb)->hit;
      if (!hit || hit->timesUsed()) continue;
      _seedHits.push_back(hit);
    }
    seeds.pop_back();
    cout<< "seed: " << nSeed++ << "\t" <<aSeed.total_hits<<" total hits "<<aSeed.used_per_total;
    cout << " no. unused hits " << _seedHits.size();
    if (_seedHits.size() < 4) {cout << endl; continue;}
    StiKalmanTrack* track = StiToolkit::instance()->getTrackFactory()->getInstance();
    if (track->initialize(_seedHits)) {cout << " initialization failed" << endl; continue;}
    if (((StiKalmanTrackFinder *)track->getTrackFinder())->Fit(track)) {cout << " fit failed " << endl; continue;}
    cout << " fitted with " << track->getFitPointCount() << endl;
    track->setSeedHitCount(track->getSeedHitCount()+100);
  }
}
// $Log: StiTpcSeedFinder.cxx,v $
// Revision 2.2  2010/03/11 22:59:00  fisyak
// Fix print out
//
// Revision 2.1  2010/02/16 23:11:14  fisyak
// Add Yury Gorbunov's TPC seed finder
//
