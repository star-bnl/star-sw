 /***************************************************************************
 *
 * $Id: StEmcCollection.cxx,v 2.1 2000/02/23 17:34:05 ullrich Exp $
 *
 * Author: Akio Ogawa, Nov 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcCollection.cxx,v $
 * Revision 2.1  2000/02/23 17:34:05  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEmcCollection.h"
#include "StEmcDetector.h"

ClassImp(StEmcCollection)

static const char rcsid[] = "$Id: StEmcCollection.cxx,v 2.1 2000/02/23 17:34:05 ullrich Exp $";

StEmcCollection::StEmcCollection() {/* noop*/}

StEmcCollection::~StEmcCollection()  {/* noop*/}
    
const StEmcDetector*
StEmcCollection::detector(StDetectorId id) const
{
    if(id >= kBarrelEmcTowerId && id <= kEndcapSmdPhiStripId)
	return mDetector[id-kBarrelEmcTowerId]; 
    else
	return 0;
}

StEmcDetector*
StEmcCollection::detector(StDetectorId id)
{
    if(id >= kBarrelEmcTowerId && id <= kEndcapSmdPhiStripId)
	return mDetector[id-kBarrelEmcTowerId]; 
    else
	return 0;
}

void
StEmcCollection::setDetector(StEmcDetector* val)
{
    if (val) {
	UInt_t id = val->detectorId();
	if (id >= kBarrelEmcTowerId && id <= kEndcapSmdPhiStripId) {
	    if (mDetector[id-kBarrelEmcTowerId]) delete mDetector[id-kBarrelEmcTowerId];
	    mDetector[id-kBarrelEmcTowerId] = val;
	}
    } 
}

//const StSPtrVecEmcPoint&
//StEmcCollection::barrelPoints() const { return mBarrel; }

//StSPtrVecEmcPoint&
//StEmcCollection::barrelPoints() { return mBarrel; }

//const StSPtrVecEmcPoint&
//StEmcCollection::endcapPoints() const { return mEndcap; }

//StSPtrVecEmcPoint&
//StEmcCollection::endcapPoints() { return mEndcap; }
