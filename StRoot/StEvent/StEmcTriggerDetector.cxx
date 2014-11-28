/***************************************************************************
 *
 * $Id: StEmcTriggerDetector.cxx,v 2.4 2007/07/11 23:06:45 perev Exp $
 *
 * Author: Alex Suaide, Feb 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcTriggerDetector.cxx,v $
 * Revision 2.4  2007/07/11 23:06:45  perev
 * Cleanup+fix StXXXTriggerDetector
 *
 * Revision 2.3  2004/08/03 17:22:16  ullrich
 * Major update by Akio and Marco.
 *
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

static const char rcsid[] = "$Id: StEmcTriggerDetector.cxx,v 2.4 2007/07/11 23:06:45 perev Exp $";

ClassImp(StEmcTriggerDetector)

StEmcTriggerDetector::StEmcTriggerDetector()
{
    memset(mBeg,0,mEnd-mBeg);
}

StEmcTriggerDetector::StEmcTriggerDetector(const dst_TrgDet_st& t)
{
    memset(mBeg,0,mEnd-mBeg);
    for(int i=0; i<mNPatch; i++) {
	mHighTower[i]=static_cast<char>(t.emcHiTower[i]);
	mPatch[i]=static_cast<char>(t.emcTrigPatch[i]);
    }
}

StEmcTriggerDetector::StEmcTriggerDetector(const StTriggerData &trg_dat)
{
    memset(mBeg,0,mEnd-mBeg);
    for(int i=0; i<mNPatch; i++) {
	mHighTower[i]=trg_dat.bemcHighTower(i);
	mPatch[i]=trg_dat.bemcJetPatch(i);
    }
    for(int i=0; i<mENPatch; i++) {
	mEHighTower[i]=trg_dat.eemcHighTower(i);
	mEPatch[i]=trg_dat.eemcJetPatch(i);
    }
    for(int i=0; i<mNBemcLayer1; i++) 
      mBemcLayer1[i]=trg_dat.bemcLayer1DSM(i);
    for(int i=0; i<mNEemcLayer1; i++) 
      mEemcLayer1[i]=trg_dat.eemcLayer1DSM(i);
    for(int i=0; i<mNEmcLayer2; i++) 
      mEmcLayer2[i]=trg_dat.emcLayer2DSM(i);
}

StEmcTriggerDetector::~StEmcTriggerDetector() {/* noop */}

int
StEmcTriggerDetector::numberOfTowers() const {return mNPatch;}

int
StEmcTriggerDetector::highTower(unsigned int i) const
{
    if (i < mNPatch)
        return static_cast<int>(mHighTower[i]);
    else
        return 0;
}

int
StEmcTriggerDetector::patch(unsigned int i) const
{
    if (i < mNPatch)
        return static_cast<int>(mPatch[i]);
    else
        return 0;
}

int
StEmcTriggerDetector::highTowerEndcap(unsigned int i) const
{
    if (i < mENPatch)
        return static_cast<int>(mEHighTower[i]);
    else
        return 0;
}

int
StEmcTriggerDetector::patchEndcap(unsigned int i) const
{
    if (i < mENPatch)
        return static_cast<int>(mEPatch[i]);
    else
        return 0;
}

unsigned short StEmcTriggerDetector::bemcLayer1(int idx) const {
  if (idx>=0 && idx<mNBemcLayer1) 
    return mBemcLayer1[idx];
  return 0;
}

unsigned short StEmcTriggerDetector::eemcLayer1(int idx) const {
  if (idx>=0 && idx<mNEemcLayer1) 
    return mEemcLayer1[idx];
  return 0;
}

unsigned short StEmcTriggerDetector::emcLayer2(int idx) const {
  if (idx>=0 && idx<mNEmcLayer2) 
    return mEmcLayer2[idx];
  return 0;
}

void
StEmcTriggerDetector::setHighTower(unsigned int i, int val)
{
    if (i < mNPatch)
        mHighTower[i] = static_cast<char>(val);
}

void
StEmcTriggerDetector::setPatch(unsigned int i, int val)
{
    if (i < mNPatch)
        mPatch[i] = static_cast<char>(val);
}

void
StEmcTriggerDetector::setHighTowerEndcap(unsigned int i, int val)
{
    if (i < mENPatch)
        mEHighTower[i] = static_cast<char>(val);
}

void
StEmcTriggerDetector::setPatchEndcap(unsigned int i, int val)
{
    if (i < mENPatch)
        mEPatch[i] = static_cast<char>(val);
}
