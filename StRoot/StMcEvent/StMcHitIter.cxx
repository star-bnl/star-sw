/*!
 * \class  StMcHitIter
  * \author Victor Perev  perev@bnl.gov
 * \date   Jan 2012
 *
 ***************************************************************************
 *
 * $Id: StMcHitIter.cxx,v 2.2 2013/03/25 23:28:51 perev Exp $
 * $Log: StMcHitIter.cxx,v $
 * Revision 2.2  2013/03/25 23:28:51  perev
 * Mustafa.Pxl corrs
 *
 * Revision 2.1  2012/03/22 01:08:05  perev
 * McHitIter added
 *
 *
 **************************************************************************/
#include "StThreeVectorF.hh"

#include "StMcEvent.hh"
#include "StMcHitIter.h"
#include "StMcTpcHitCollection.hh"
#include "StMcFtpcHitCollection.hh"
#include "StMcCtbHitCollection.hh"
#include "StMcSvtHitCollection.hh"
#include "StMcSsdHitCollection.hh"
#include "StMcEmcHitCollection.hh"
#include "StMcTofHitCollection.hh"
#include "StMcBTofHitCollection.hh"
#include "StMcMtdHitCollection.hh"
#include "StMcPxlHitCollection.hh"
#include "StMcIstHitCollection.hh"
#include "StMcFgtHitCollection.hh"
#include "StMcEtrHitCollection.hh"
#include "StMcContainers.hh" 
#include "StMcHit.hh"

class myMcEvent: public StMcEvent {
public:
int Init(int *myOffset,int *myDetId) {
  int n=0;
  myOffset[n]= (char*)&mTpcHits  -(char*)this; myDetId[n] = kTpcId;	n++;
  myOffset[n]= (char*)&mSvtHits  -(char*)this; myDetId[n] = kSvtId;	n++;
  myOffset[n]= (char*)&mSsdHits  -(char*)this; myDetId[n] = kSsdId;	n++;
  myOffset[n]= (char*)&mFtpcHits -(char*)this; myDetId[n] = kFtpcWestId;n++;
  myOffset[n]= (char*)&mRichHits -(char*)this; myDetId[n] = 0;		n++;
  myOffset[n]= (char*)&mCtbHits  -(char*)this; myDetId[n] = kCtbId;	n++;
  myOffset[n]= (char*)&mTofHits  -(char*)this; myDetId[n] = 0;		n++;
  myOffset[n]= (char*)&mBTofHits -(char*)this; myDetId[n] = kTofId;	n++;
  myOffset[n]= (char*)&mMtdHits  -(char*)this; myDetId[n] = kMtdId;	n++;
  myOffset[n]= (char*)&mPxlHits  -(char*)this; myDetId[n] = kPxlId;	n++;
  myOffset[n]= (char*)&mIstHits  -(char*)this; myDetId[n] = kIstId;	n++;
  myOffset[n]= (char*)&mFgtHits  -(char*)this; myDetId[n] = kFgtId;	n++;
  myOffset[n]= (char*)&mEtrHits  -(char*)this; myDetId[n] = kEtrId;	n++;
  return n;
  }
};




ClassImp(StMcHitIter);
//______________________________________________________________________________
StMcHitIter::StMcHitIter(const StMcEvent *mcev)
{
    memset(mDets,0,sizeof(mDets));    
}
//______________________________________________________________________________
void StMcHitIter::Reset(const StMcEvent *mcev)
{
    memset(mBeg,0,mEnd-mBeg+1); mMcEv = mcev;
}
