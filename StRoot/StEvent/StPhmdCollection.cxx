/********************************************************************
 *
 * $Id: StPhmdCollection.cxx,v 2.1 2002/12/20 22:33:00 ullrich Exp $
 *
 * Author: Subhasis Chattopadhyay, Dec 2002
 ********************************************************************
 *
 * Description: Base class for PMD collection
 *
 ********************************************************************
 *
 * $Log: StPhmdCollection.cxx,v $
 * Revision 2.1  2002/12/20 22:33:00  ullrich
 * Initial Revision.
 *
 ********************************************************************/
#include "StPhmdCollection.h"
#include "StPhmdDetector.h"

ClassImp(StPhmdCollection)

StPhmdCollection::StPhmdCollection()
{
    mCpvDetector = new StPhmdDetector(kPhmdCpvId);
    mDetector = new StPhmdDetector(kPhmdId);
}

StPhmdCollection::~StPhmdCollection()
{
    delete mCpvDetector;
    delete mDetector;
}
    
StPhmdDetector*
StPhmdCollection::detector(StDetectorId id)
{
    if (id == kPhmdCpvId)
	return mCpvDetector;
    else if (id == kPhmdId)
	return mDetector;
    else
	return 0;
}

const StPhmdDetector*
StPhmdCollection::detector(StDetectorId id) const
{
    if (id == kPhmdCpvId)
	return mCpvDetector;
    else if (id == kPhmdId)
	return mDetector;
    else
	return 0;
}

void
StPhmdCollection::setDetector(StPhmdDetector* val)
{
    if (val) {
        if (val->id() == kPhmdCpvId) {
            delete mCpvDetector;
            mCpvDetector = val;
        }
	else if (val->id() == kPhmdId) {
            delete mDetector;
            mDetector = val;
        }
    }
}



