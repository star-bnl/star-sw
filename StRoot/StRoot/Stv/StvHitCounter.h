/** 
 * \file  StvHitCounter.h
 * \brief Definition StvHitCounter
 * 
 */
#include <string.h>
#ifndef StvHitCounter_H
#define StvHitCounter_H 1
class StvKonst_st;

/// Hit counting
class StvHitCounter
{ 
public:
StvHitCounter();
void Clear()		{memset(mBeg,0,mMed-mBeg+1);}
void SetCons(const StvKonst_st*);

void AddHit();
void AddNit();
int  Reject() const;
int  Skip()   const;
double Eff()  const;
int  MaxNitSeq() const;	//Length of longesr Nits sequence
int  MaxHitSeq() const;	//Length of longesr Hits sequence
public:
char mBeg[1] ;
const StvKonst_st *mKons;
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
int mContHits;	// Number of hits in maximal Hit sequence
int mContNits;	// Number of nits in maximal nonHit sequence
char mMed[1] ;
int mMinTotHits;	//Min number hits for track
int mMinGoodHits;	//Min number good hits for track
int mMinContHits;	//Min length of good hit sequence
int mMaxContNits;	//Max length of acceptable non hit sequence
int mMaxTotNits;	//Max number of acceptable non hits
char mEnd[1] ;
};

#endif

