/***************************************************************************
 *
 * $Id: StEmcTriggerDetector.cxx,v 2.2 2004/02/11 01:42:09 ullrich Exp $
 *
 * Author: Alex Suaide, Feb 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcTriggerDetector.cxx,v $
 * Revision 2.2  2004/02/11 01:42:09  ullrich
 * Added new constructor to load data from StTriggerData.
 *
 * Revision 2.1  2002/02/20 03:11:45  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include <algorithm>
#include "StEmcTriggerDetector.h"
#include "tables/St_dst_TrgDet_Table.h"
#include "StTriggerData.h"

static const char rcsid[] = "$Id: StEmcTriggerDetector.cxx,v 2.2 2004/02/11 01:42:09 ullrich Exp $";

ClassImp(StEmcTriggerDetector)

StEmcTriggerDetector::StEmcTriggerDetector()
{
    for(int i=0; i<mMaxTower; i++) {
	mHighTower[i]=0;
	mPatch[i]=0;
    }
}

StEmcTriggerDetector::StEmcTriggerDetector(const dst_TrgDet_st& t)
{
    for(int i=0; i<mMaxTower; i++) {
	mHighTower[i]=static_cast<char>(t.emcHiTower[i]);
	mPatch[i]=static_cast<char>(t.emcTrigPatch[i]);
    }
}

StEmcTriggerDetector::StEmcTriggerDetector(const StTriggerData&)
{
    for(int i=0; i<mMaxTower; i++) {
	mHighTower[i]=0;
	mPatch[i]=0;
    }
}

StEmcTriggerDetector::~StEmcTriggerDetector() {/* noop */}

int
StEmcTriggerDetector::numberOfTowers() const {return mMaxTower;}

int
StEmcTriggerDetector::highTower(unsigned int i) const
{
    if (i < mMaxTower)
        return static_cast<int>(mHighTower[i]);
    else
        return 0;
}

int
StEmcTriggerDetector::patch(unsigned int i) const
{
    if (i < mMaxTower)
        return static_cast<int>(mPatch[i]);
    else
        return 0;
}

void
StEmcTriggerDetector::setHighTower(unsigned int i, int val)
{
    if (i < mMaxTower)
        mHighTower[i] = static_cast<char>(val);
}

void
StEmcTriggerDetector::setPatch(unsigned int i, int val)
{
    if (i < mMaxTower)
        mPatch[i] = static_cast<char>(val);
}
