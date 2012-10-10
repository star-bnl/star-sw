/** 
 * \file  StvHitCounter.h
 * \brief Definition StvHitCounter
 * 
 */
#include <string.h>
#ifndef StvHitCounter_H
#define StvHitCounter_H 1

/// Hit counting
class StvHitCounter
{ 
StvHitCounter();
public:
static StvHitCounter *Inst();
void Clear()		{memset(mBeg,0,mMed-mBeg+1);}
void AddHit();
void AddNit();
int  Reject() const;
int  Skip()   const;
double Eff()  const;
private:
static StvHitCounter *mgStvHitCounter;
public:
char mBeg[1] ;
int nPossHits;	// Number of possible hits;
int nTotHits ;	// Total number of hits
int nGoodHits;  // Number of good hits (hits in sequences > kContHits
int nSeqHits ;	// Number of hit sequences
int nSeqShort;	// Number of too short hit sequences
int nTotNits ;	// Total number of Non Hits(Nits) 
int nSeqNits ;	// Number of Non Hit(Nit) sequences
int nSeqLong ;	// Number of too long Non Hit(Nit) sequences
int nContHits;	// Number of hits in current Hit sequence
int nContNits;	// Number of nits in current nonHit sequence
char mMed[1] ;
int mMinTotHits;	//Min number hits for track
int mMinGoodHits;	//Min number good hits for track
int mMinContHits;	//Min length of good hit sequence
int mMaxContNits;	//Max length of acceptable non hit sequence
int mMaxTotNits;	//Max number of acceptable non hits
char mEnd[1] ;
};

#endif

