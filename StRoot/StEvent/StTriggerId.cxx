/***************************************************************************
 *
 * $Id: StTriggerId.cxx,v 2.1 2003/01/30 18:14:15 ullrich Exp $
 *
 * Author: Thomas Ullrich, January 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerId.cxx,v $
 * Revision 2.1  2003/01/30 18:14:15  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTriggerId.h"

ClassImp(StTriggerId)

StTriggerId::StTriggerId() : mMask(0) {/* noop */}

StTriggerId::~StTriggerId()  {/* noop */}

unsigned int
StTriggerId::index(unsigned int id) const
{
    for (unsigned int i=0; i<mId.size(); i++)
	if (id == mId[i]) return i;
    return mId.size(); // no index found (no such trigger id)
}

unsigned int
StTriggerId::mask() const {return mMask;}

bool
StTriggerId::isTrigger(unsigned int id) const
{
    return index(id) < mId.size();
}

unsigned int
StTriggerId::version(unsigned int id) const
{
    unsigned int i = index(id);
    if (i < mId.size())
	return mVersion[i];
    else
	return 0;
}

unsigned int
StTriggerId::nameVersion(unsigned int id) const
{
    unsigned int i = index(id);
    if (i < mId.size())
	return mNameVersion[i];
    else
	return 0;
}

unsigned int
StTriggerId::thresholdVersion(unsigned int id) const
{
    unsigned int i = index(id);
    if (i < mId.size())
	return mThresholdVersion[i];
    else
	return 0;
}

unsigned int
StTriggerId::prescaleVersion(unsigned int id) const    
{
    unsigned int i = index(id);
    if (i < mId.size())
	return mPrescaleVersion[i];
    else
	return 0;
}

vector<unsigned int>
StTriggerId::triggerIds() const {return mId;}

void
StTriggerId::setMask(unsigned int val) {mMask = val;}

void
StTriggerId::addTrigger(unsigned int id, unsigned int v,
			unsigned int nv, unsigned int tv,
			unsigned int pv)
{
    mId.push_back(id);
    mVersion.push_back(v);
    mNameVersion.push_back(nv);
    mThresholdVersion.push_back(tv);
    mPrescaleVersion.push_back(pv); 
}

ostream& operator<<(ostream&, const StTriggerId&);
    

    
