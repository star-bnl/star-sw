/***************************************************************************
 *
 * $Id: StTriggerId.cxx,v 2.2 2003/02/18 21:34:46 jeromel Exp $
 *
 * Author: Thomas Ullrich, January 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerId.cxx,v $
 * Revision 2.2  2003/02/18 21:34:46  jeromel
 * Changed vector to arrays
 *
 * Revision 2.1  2003/01/30 18:14:15  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTriggerId.h"
#include "StMessMgr.h"


ClassImp(StTriggerId)

StTriggerId::StTriggerId() : mMask(0) {
  int i;

  mIdx = 0;
  for(i=0 ; i < TRIGGER_ID_DIM ; ++i){
    mId[i]                = 0;
    mVersion[i]           = 0;
    mNameVersion[i]       = 0;
    mThresholdVersion[i]  = 0;
    mPrescaleVersion[i]   = 0;
  }

  /* noop */
}

StTriggerId::~StTriggerId()  {
  /* noop */
}

unsigned int
StTriggerId::index(unsigned int id) const
{
    for (unsigned int i=0; i<TRIGGER_ID_DIM; i++)
	if (id == mId[i]) return i;
    return TRIGGER_ID_DIM; // no index found (no such trigger id)
}

unsigned int
StTriggerId::mask() const {return mMask;}

bool
StTriggerId::isTrigger(unsigned int id) const
{
    return index(id) < TRIGGER_ID_DIM;
}

unsigned int
StTriggerId::version(unsigned int id) const
{
    unsigned int i = index(id);
    if (i < TRIGGER_ID_DIM)
	return mVersion[i];
    else
	return 0;
}

unsigned int
StTriggerId::nameVersion(unsigned int id) const
{
    unsigned int i = index(id);
    if (i < TRIGGER_ID_DIM)
	return mNameVersion[i];
    else
	return 0;
}

unsigned int
StTriggerId::thresholdVersion(unsigned int id) const
{
    unsigned int i = index(id);
    if (i < TRIGGER_ID_DIM)
	return mThresholdVersion[i];
    else
	return 0;
}

unsigned int
StTriggerId::prescaleVersion(unsigned int id) const    
{
    unsigned int i = index(id);
    if (i < TRIGGER_ID_DIM)
	return mPrescaleVersion[i];
    else
	return 0;
}

vector<unsigned int>
StTriggerId::triggerIds() const {
  vector<unsigned int> retVec;
  for (unsigned int i=0; i< TRIGGER_ID_DIM; ++i) {
    if (mId[i]) retVec.push_back(mId[i]);
  }

  return retVec;
}

void
StTriggerId::setMask(unsigned int val) {mMask = val;}

void
StTriggerId::addTrigger(unsigned int id, unsigned int v,
			unsigned int nv, unsigned int tv,
			unsigned int pv)
{
  if( mIdx >= TRIGGER_ID_DIM){
    gMessMgr->Warning() << "StTriggerId::addTrigger : MAX Dimension reached. Cannot add !!" << endm;
  } else {
    mId[mIdx]               = id;
    mVersion[mIdx]          = v;
    mNameVersion[mIdx]      = nv;
    mThresholdVersion[mIdx] = tv;
    mPrescaleVersion[mIdx]  = pv;
    ++mIdx;
  }

}

ostream& operator<<(ostream&, const StTriggerId&);
    

    
