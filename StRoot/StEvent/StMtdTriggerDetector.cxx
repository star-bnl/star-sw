/***************************************************************************
*
* $Id: StMtdTriggerDetector.cxx,v 2.1 2007/07/02 20:21:55 ullrich Exp $
*
* Author: Akio Agawa, July 2007
***************************************************************************
*
* Description:
*
***************************************************************************
*
* $Log: StMtdTriggerDetector.cxx,v $
* Revision 2.1  2007/07/02 20:21:55  ullrich
* Initial Revision.
*
**************************************************************************/
#include "StMtdTriggerDetector.h"
#include "StTriggerData.h"

static const char rcsid[] = "$Id: StMtdTriggerDetector.cxx,v 2.1 2007/07/02 20:21:55 ullrich Exp $";

ClassImp(StMtdTriggerDetector)

StMtdTriggerDetector::StMtdTriggerDetector()
{
    memset(mADC,0,sizeof(mADC));
    memset(mTDC,0,sizeof(mTDC));
}

StMtdTriggerDetector::StMtdTriggerDetector(const StTriggerData& tt)
{
    for(int i=0; i<mMaxMtdCounter; i++){
        mADC[west][i] = tt.mtdAdc(west,i);
        mADC[east][i] = tt.mtdAdc(east,i);
        mTDC[west][i] = tt.mtdTdc(west,i);
        mTDC[east][i] = tt.mtdTdc(east,i);
    }
}

StMtdTriggerDetector::~StMtdTriggerDetector() {/* noop */}

unsigned int
StMtdTriggerDetector::numberOfMtdCounters() const {return mMaxMtdCounter;}

unsigned short
StMtdTriggerDetector::adc(StBeamDirection eastwest, unsigned int i) const
{
    if (i < mMaxMtdCounter)
        return mADC[eastwest][i];
    else
        return 0;
}

unsigned short
StMtdTriggerDetector::tdc(StBeamDirection eastwest, unsigned int i) const
{
    if (i < mMaxMtdCounter)
        return mTDC[eastwest][i];
    else
        return 0;
}

void
StMtdTriggerDetector::setAdc(StBeamDirection eastwest, unsigned int i, unsigned short v)
{
    if (i < mMaxMtdCounter )
        mADC[eastwest][i] = v;
}

void
StMtdTriggerDetector::setTdc(StBeamDirection eastwest, unsigned int i, unsigned short v)
{
    if (i < mMaxMtdCounter )
        mTDC[eastwest][i] = v;
}

