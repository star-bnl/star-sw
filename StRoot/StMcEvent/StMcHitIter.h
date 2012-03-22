/***************************************************************************
 *
 * $Id: StMcHitIter.h,v 2.1 2012/03/22 01:08:05 perev Exp $
 * $Log: StMcHitIter.h,v $
 * Revision 2.1  2012/03/22 01:08:05  perev
 * McHitIter added
 *
 *
 * 
 **************************************************************************/
#ifndef StMcHitIter_h
#define StMcHitIter_h
#include "StEvent/StEnumerations.h" 		//StDetectorId
#include "TObject.h" 		
class StMcEvent;
class StMcHit;

class StMcHitIter : public TObject {
public:
StMcHitIter(const StMcEvent *mcev);
void  Reset(const StMcEvent *mcev);
void Add(StDetectorId det);
const StMcHit *operator*();
const int *Path(int &lev) const 	{lev = mLev; return mPath;}
const int *MaxN() const 		{return mMaxN;}
StDetectorId GetDetId();
StMcHitIter &operator++();

protected:

int     mDets[kMaxDetectorId+1];
char 	mBeg[1];
const StMcEvent	*mMcEv;
const StMcHit 	*mMcHit;
int 	mLev;
int 	mPath[10];
int 	mMaxN[10];
char 	mEnd[1];

  ClassDef(StMcHitIter,0)
};

#endif

