/***************************************************************************
 *
 * $Id: StTriggerId.cxx,v 2.5 2011/02/02 20:20:10 ullrich Exp $
 *
 * Author: Thomas Ullrich, January 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerId.cxx,v $
 * Revision 2.5  2011/02/02 20:20:10  ullrich
 * Extend to 64 bit (Jamie)
 *
 * Revision 2.4  2005/08/16 22:36:27  ullrich
 * Fixed bug in copy and assignement operator (mMask not assigned).
 *
 * Revision 2.3  2004/10/11 23:00:20  ullrich
 * Add copy constructor and assign. op., implement ostream op., define to enum.
 *
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
    mIdx = 0;
    for(int i=0 ; i < mMaxTriggerIds ; ++i) {
        mId[i]                = 0;
        mVersion[i]           = 0;
        mNameVersion[i]       = 0;
        mThresholdVersion[i]  = 0;
        mPrescaleVersion[i]   = 0;
    }
}

StTriggerId::~StTriggerId()  {/* noop */}

StTriggerId::StTriggerId(const StTriggerId &id) {
    mIdx = id.mIdx;
    mMask = id.mMask;
    for(int i=0 ; i < mMaxTriggerIds ; ++i){
        mId[i]                = id.mId[i];
        mVersion[i]           = id.mVersion[i];
        mNameVersion[i]       = id.mNameVersion[i];
        mThresholdVersion[i]  = id.mThresholdVersion[i];
        mPrescaleVersion[i]   = id.mPrescaleVersion[i];
    }  
}

const StTriggerId &StTriggerId::operator=(const StTriggerId &id) {
    if (&id==this) return id;
  
    mIdx = id.mIdx;
    mMask = id.mMask;
    
    for(int i=0 ; i < mMaxTriggerIds ; ++i) {
        mId[i]                = id.mId[i];
        mVersion[i]           = id.mVersion[i];
        mNameVersion[i]       = id.mNameVersion[i];
        mThresholdVersion[i]  = id.mThresholdVersion[i];
        mPrescaleVersion[i]   = id.mPrescaleVersion[i];
    }  
    return *this;
}

unsigned int
StTriggerId::index(unsigned int id) const
{
    for (unsigned int i=0; i<mMaxTriggerIds; i++)
        if (id == mId[i]) return i;
    return mMaxTriggerIds; // no index found (no such trigger id)
}

uint64_t
StTriggerId::mask() const {return mMask;}

bool
StTriggerId::isTrigger(unsigned int id) const
{
    return index(id) < mMaxTriggerIds;
}

unsigned int
StTriggerId::version(unsigned int id) const
{
    unsigned int i = index(id);
    if (i < mMaxTriggerIds)
	return mVersion[i];
    else
	return 0;
}

unsigned int
StTriggerId::nameVersion(unsigned int id) const
{
    unsigned int i = index(id);
    if (i < mMaxTriggerIds)
	return mNameVersion[i];
    else
	return 0;
}

unsigned int
StTriggerId::thresholdVersion(unsigned int id) const
{
    unsigned int i = index(id);
    if (i < mMaxTriggerIds)
	return mThresholdVersion[i];
    else
	return 0;
}

unsigned int
StTriggerId::prescaleVersion(unsigned int id) const    
{
    unsigned int i = index(id);
    if (i < mMaxTriggerIds)
	return mPrescaleVersion[i];
    else
	return 0;
}

vector<unsigned int>
StTriggerId::triggerIds() const {
    vector<unsigned int> retVec;
    for (unsigned int i=0; i< mMaxTriggerIds; ++i) {
        if (mId[i]) retVec.push_back(mId[i]);
    }
    
    return retVec;
}

void
StTriggerId::setMask(uint64_t val) {mMask = val;}

void
StTriggerId::addTrigger(unsigned int id, unsigned int v,
		    unsigned int nv, unsigned int tv,
		    unsigned int pv)
{
    if( mIdx >= mMaxTriggerIds){
        gMessMgr->Warning() << "StTriggerId::addTrigger : MAX Dimension reached. Cannot add !!" << endm;
    }
    else {
        mId[mIdx]               = id;
        mVersion[mIdx]          = v;
        mNameVersion[mIdx]      = nv;
        mThresholdVersion[mIdx] = tv;
        mPrescaleVersion[mIdx]  = pv;
        ++mIdx;
    }
}

ostream& operator<<(ostream& os, const StTriggerId& id)
{
    for (unsigned int i=0; i<id.maxTriggerIds(); i++) {
        os << i << '\t'
           << id.mId[i] << '\t'
           << id.mVersion[i] << '\t'
           << id.mNameVersion[i] << '\t'
           << id.mThresholdVersion[i] << '\t'
           << id.mPrescaleVersion[i] << endl;
    }
    return os;
}
