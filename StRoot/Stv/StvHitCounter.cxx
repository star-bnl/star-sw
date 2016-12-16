#include <math.h>
#include <assert.h>
#include "StvConst.h"

#include "Stv/StvHitCounter.h"

//_____________________________________________________________________________
StvHitCounter::StvHitCounter()		
{
 Clear();
//  mMinTotHits	=6;	//Min number hits for track
//  mMinGoodHits=3;	//Min number good hits for track
//  mMinContHits=2;	//Min length of good hit sequence
//  mMaxContNits=13;	//Max length of acceptable non hit sequence
//  mMaxTotNits	=15;	//Max number of acceptable non hits
}
//_____________________________________________________________________________
void StvHitCounter::SetCons(const StvKonst_st* kons)
{
 mKons = kons;
 mMinTotHits  = mKons->mMinTotHits;	
 mMinGoodHits = mKons->mMinGoodHits;	
 mMinContHits = mKons->mMinContHits;	
 mMaxContNits = mKons->mMaxContNits;	
 mMaxTotNits  = mKons->mMaxTotNits;	
}
//_____________________________________________________________________________
int StvHitCounter::MaxNitSeq() const
{
  if (nContNits>mContNits) return nContNits;
  else                     return mContNits;
}
//_____________________________________________________________________________
int StvHitCounter::MaxHitSeq() const
{
  if (nContHits>mContHits) return nContHits;
  else                     return mContHits;
}
//_____________________________________________________________________________
void StvHitCounter::AddHit()
{
  nPossHits++;  nTotHits++;nContHits++;
  if (!nContNits)		return;
  if (nContHits<mMinContHits) 	return;
  if (nContNits>mMaxContNits) 	nSeqLong++;
  if (mContNits<nContNits   )   mContNits=nContNits;
  nContNits=0;nSeqNits++;
}
//_____________________________________________________________________________
void StvHitCounter::AddNit()
{
  nPossHits++;nContNits++;nTotNits++;
  if (!nContHits) 	return;
  if (nContHits<mMinContHits) {nSeqShort++;} else { nGoodHits+=nContHits;}
  if (mContHits<nContHits   )   mContHits=nContHits;
  nContHits=0;nSeqHits++;
}
//_____________________________________________________________________________
int StvHitCounter::Reject() const
{
  int rej = 0;
  int nG = (nContHits>=mMinContHits)? nGoodHits+nContHits : nGoodHits;
  if (nG       <mMinGoodHits) rej+=1;
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
  double p = nTotHits/(nPossHits+1e-6);
  double q = 1-p;
  return p +1*sqrt(p*q/(nPossHits+1e-6));
}
