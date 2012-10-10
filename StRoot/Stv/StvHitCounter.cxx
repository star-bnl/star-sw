#include <math.h>
#include <assert.h>
#include "StvConst.h"

#include "Stv/StvHitCounter.h"
StvHitCounter *StvHitCounter::mgStvHitCounter=0;

//_____________________________________________________________________________
StvHitCounter::StvHitCounter()		
{
 Clear();
//  mMinTotHits	=6;	//Min number hits for track
//  mMinGoodHits=3;	//Min number good hits for track
//  mMinContHits=2;	//Min length of good hit sequence
//  mMaxContNits=13;	//Max length of acceptable non hit sequence
//  mMaxTotNits	=15;	//Max number of acceptable non hits
const StvConst *kons = StvConst::Inst();

 mMinTotHits  = kons->mMinTotHits;	
 mMinGoodHits = kons->mMinGoodHits;	
 mMinContHits = kons->mMinContHits;	
 mMaxContNits = kons->mMaxContNits;	
 mMaxTotNits  = kons->mMaxTotNits;	
}
//_____________________________________________________________________________
StvHitCounter *StvHitCounter::Inst()		
{
  if (!mgStvHitCounter) mgStvHitCounter = new StvHitCounter;
  return mgStvHitCounter;
}


//_____________________________________________________________________________
void StvHitCounter::AddHit()
{
  nPossHits++;  nTotHits++;nContHits++;
  if (!nContNits)		return;
  if (nContHits<mMinContHits) 	return;
  if (nContNits>mMaxContNits) 	nSeqLong++;
  nContNits=0;nSeqNits++;
}
//_____________________________________________________________________________
void StvHitCounter::AddNit()
{
  nPossHits++;nContNits++;nTotNits++;
  if (!nContHits) 	return;
  if (nContHits<mMinContHits) {nSeqShort++;} else { nGoodHits+=nContHits;}
  nContHits=0;nSeqHits++;
}
//_____________________________________________________________________________
int StvHitCounter::Reject() const
{
  int rej = 0;
  if (nGoodHits+nContHits<mMinGoodHits) rej+=1;
  if (nTotHits <mMinTotHits ) rej+=2;
//if (nTotNits+nContNits> mMaxTotNits) rej+=4;

  return rej;
}
//_____________________________________________________________________________
int StvHitCounter::Skip() const
{
  int rej = 0;
  if (nContNits>mMaxContNits) rej+= 1;
  if (nTotNits > mMaxTotNits) rej+= 2;
  return rej ;
}
//_____________________________________________________________________________
double StvHitCounter::Eff() const
{
  double p = nTotHits/(nPossHits-nContNits+1e-6);
  double q = 1-p;
  return p +3*sqrt(p*q/(nPossHits+1e-6));
}
