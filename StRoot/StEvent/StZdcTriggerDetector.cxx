/***************************************************************************
 *
 * $Id: StZdcTriggerDetector.cxx,v 2.2 1999/10/28 22:28:21 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StZdcTriggerDetector.cxx,v $
 * Revision 2.2  1999/10/28 22:28:21  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
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
using std::fill_n;
using std::copy;
#endif

static const char rcsid[] = "$Id: StZdcTriggerDetector.cxx,v 2.2 1999/10/28 22:28:21 ullrich Exp $";

ClassImp(StZdcTriggerDetector)

StZdcTriggerDetector::StZdcTriggerDetector()
{
    fill_n(mAdc, static_cast<int>(mMaxZdcCounter), 0);
    fill_n(mTdc, static_cast<int>(mMaxZdcCounter), 0);
    fill_n(mSumAdc, 2, 0);
    mSum = 0;
}

StZdcTriggerDetector::StZdcTriggerDetector(const dst_TrgDet_st& t)
{
    copy(t.adcZDC+0, t.adcZDC+mMaxZdcCounter, mAdc);
    copy(t.tdcZDC+0, t.tdcZDC+mMaxZdcCounter, mTdc);
    mSumAdc[east] = t.adcZDCEast;
    mSumAdc[west] = t.adcZDCWest;
    mSum          = t.adcZDCsum;
}

StZdcTriggerDetector::~StZdcTriggerDetector() {/* noop */}

UInt_t
StZdcTriggerDetector::numberOfZdcCounters() const {return mMaxZdcCounter;}

Float_t
StZdcTriggerDetector::adc(UInt_t i) const
{
    if (i < mMaxZdcCounter)
        return mAdc[i];
    else
        return 0;
}

Float_t
StZdcTriggerDetector::tdc(UInt_t i) const
{
    if (i < mMaxZdcCounter)
        return mTdc[i];
    else
        return 0;
}

Float_t
StZdcTriggerDetector::adcSum(StBeamDirection dir) const
{
    return mSumAdc[dir];
}

Float_t
StZdcTriggerDetector::adcSum() const {return mSum;}

void
StZdcTriggerDetector::setAdc(UInt_t i, Float_t val)
{
    if (i < mMaxZdcCounter)
        mAdc[i] = val;
}

void
StZdcTriggerDetector::setTdc(UInt_t i, Float_t val)
{
    if (i < mMaxZdcCounter)
        mTdc[i] = val;
}

void
StZdcTriggerDetector::setAdcSum(StBeamDirection dir, Float_t val)
{
    mSumAdc[dir] = val;
}

void
StZdcTriggerDetector::setAdcSum(Float_t val)
{
    mSum = val;
}
