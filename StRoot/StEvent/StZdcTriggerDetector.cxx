/***************************************************************************
 *
 * $Id: StZdcTriggerDetector.cxx,v 2.8 2002/03/05 17:11:52 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StZdcTriggerDetector.cxx,v $
 * Revision 2.8  2002/03/05 17:11:52  ullrich
 * Corrected bug in constructor. Wrong table entry was used.
 *
 * Revision 2.7  2001/09/14 19:11:11  ullrich
 * Load corrected vertex-Z from trigger table.
 *
 * Revision 2.6  2001/07/12 22:58:33  ullrich
 * Added variable to store the vertex_z from timing info.
 *
 * Revision 2.5  2001/04/05 04:00:59  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.4  2000/07/13 12:51:09  ullrich
 * Added new method numberOfZdcWords to replace old one with wrong name.
 *
 * Revision 2.3  1999/12/21 15:09:28  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.2  1999/10/28 22:28:21  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:50  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <algorithm>
#include "StZdcTriggerDetector.h"
#include "tables/St_dst_TrgDet_Table.h"
#if !defined(ST_NO_NAMESPACES)
using std::fill_n;
using std::copy;
#endif

static const char rcsid[] = "$Id: StZdcTriggerDetector.cxx,v 2.8 2002/03/05 17:11:52 ullrich Exp $";

ClassImp(StZdcTriggerDetector)

StZdcTriggerDetector::StZdcTriggerDetector()
{
    fill_n(mAdc, static_cast<int>(mMaxZdcWords), 0);
    fill_n(mTdc, static_cast<int>(mMaxZdcWords), 0);
    fill_n(mSumAdc, 2, 0);
    mSum = 0;
    mVertexZ = 0;
}

StZdcTriggerDetector::StZdcTriggerDetector(const dst_TrgDet_st& t)
{
    copy(t.adcZDC+0, t.adcZDC+mMaxZdcWords, mAdc);
    copy(t.tdcZDC+0, t.tdcZDC+mMaxZdcWords, mTdc);
    mSumAdc[east] = t.adcZDCEast;
    mSumAdc[west] = t.adcZDCWest;
    mSum          = t.adcZDCsum;
    mVertexZ      = t.ZDCvertexZ;
}

StZdcTriggerDetector::~StZdcTriggerDetector() {/* noop */}

unsigned int
StZdcTriggerDetector::numberOfZdcCounters() const {return mMaxZdcWords;}

unsigned int
StZdcTriggerDetector::numberOfZdcWords() const {return mMaxZdcWords;}

float
StZdcTriggerDetector::adc(unsigned int i) const
{
    if (i < mMaxZdcWords)
        return mAdc[i];
    else
        return 0;
}

float
StZdcTriggerDetector::tdc(unsigned int i) const
{
    if (i < mMaxZdcWords)
        return mTdc[i];
    else
        return 0;
}

float
StZdcTriggerDetector::adcSum(StBeamDirection dir) const
{
    return mSumAdc[dir];
}

float
StZdcTriggerDetector::adcSum() const {return mSum;}

float
StZdcTriggerDetector::vertexZ() const {return mVertexZ;}

void
StZdcTriggerDetector::setAdc(unsigned int i, float val)
{
    if (i < mMaxZdcWords)
        mAdc[i] = val;
}

void
StZdcTriggerDetector::setTdc(unsigned int i, float val)
{
    if (i < mMaxZdcWords)
        mTdc[i] = val;
}

void
StZdcTriggerDetector::setAdcSum(StBeamDirection dir, float val)
{
    mSumAdc[dir] = val;
}

void
StZdcTriggerDetector::setAdcSum(float val)
{
    mSum = val;
}

void
StZdcTriggerDetector::setVertexZ(float val)
{
    mVertexZ = val;
}
