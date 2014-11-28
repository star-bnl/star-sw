/***************************************************************************
 *
 * $Id: StVpdTriggerDetector.cxx,v 2.7 2013/10/30 15:47:16 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 * 
 ***************************************************************************
 *
 * $Log: StVpdTriggerDetector.cxx,v $
 * Revision 2.7  2013/10/30 15:47:16  ullrich
 * Added ADCmxq(), TDCmxq() and referring data member (WJL).
 *
 * Revision 2.6  2007/04/10 20:13:41  ullrich
 * Fixed bug in array subscript (Akio).
 *
 * Revision 2.5  2007/04/03 20:11:41  ullrich
 * Modified for actual VPD used in 2007.
 *
 * Revision 2.4  2001/04/05 04:00:59  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.3  1999/12/21 15:09:25  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.2  1999/10/28 22:28:12  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:48  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <algorithm>
#include "StVpdTriggerDetector.h"
#include "tables/St_dst_TrgDet_Table.h"
#include "StTriggerData.h"
#if !defined(ST_NO_NAMESPACES)
using std::fill_n;
using std::copy;
#endif

static const char rcsid[] = "$Id: StVpdTriggerDetector.cxx,v 2.7 2013/10/30 15:47:16 ullrich Exp $";

ClassImp(StVpdTriggerDetector)

StVpdTriggerDetector::StVpdTriggerDetector()
{
    memset(*mADC,0,mMaxVpdCounter*sizeof(unsigned short));
    memset(*mTDC,0,mMaxVpdCounter*sizeof(unsigned short));
    memset(*mADCmxq,0,mMaxVpdCounter*sizeof(unsigned short));		//WJL
    memset(*mTDCmxq,0,mMaxVpdCounter*sizeof(unsigned short));		//WJL
    //fill_n(mADC, static_cast<short>(mMaxVpdCounter), 0);
    //fill_n(mTDC, static_cast<short>(mMaxVpdCounter), 0);
    //fill_n(mEarliestTDC, 2, 0);
    mEarliestTDC[0]=0;
    mEarliestTDC[1]=0;
    mTimeDifference = 0;
    mYear = 0;
}

StVpdTriggerDetector::StVpdTriggerDetector(const dst_TrgDet_st&)
{
    // old legacy code removed - was wrong in the
    // first place (tu, April 3, 2007)
}

StVpdTriggerDetector::StVpdTriggerDetector(const StTriggerData& t)
{
    int i;
    mYear = t.year();
    if (mYear<2007) return;
    for (i=0; i<mMaxVpdCounter; i++){
		mADC[east][i] = t.vpdADC(east, i+1);
		mTDC[east][i] = t.vpdTDC(east, i+1);
		mADC[west][i] = t.vpdADC(west, i+1);
		mTDC[west][i] = t.vpdTDC(west, i+1);
		mADCmxq[east][i] = t.vpdADCHighThr(east, i+1);		//WJL
		mTDCmxq[east][i] = t.vpdTDCHighThr(east, i+1);		//WJL
		mADCmxq[west][i] = t.vpdADCHighThr(west, i+1);		//WJL
		mTDCmxq[west][i] = t.vpdTDCHighThr(west, i+1);		//WJL
		//
		//LOG_INFO<<"WJL... "<<i
		//	<<" ADCe "<<mADC[east][i]<<" "<<mADCmxq[east][i]
		//	<<" TDCe "<<mTDC[east][i]<<" "<<mTDCmxq[east][i]
		//	<<" ADCw "<<mADC[west][i]<<" "<<mADCmxq[west][i]
		//	<<" TDCw "<<mTDC[west][i]<<" "<<mTDCmxq[west][i]
		//	<<endm;
		//
    }
    mEarliestTDC[east] = t.vpdEarliestTDC(east);
    mEarliestTDC[west] = t.vpdEarliestTDC(west);
    mTimeDifference    = t.vpdTimeDifference();
}


StVpdTriggerDetector::~StVpdTriggerDetector() {/* noop */}

unsigned int
StVpdTriggerDetector::numberOfVpdCounters() const {return mMaxVpdCounter;}

unsigned short
StVpdTriggerDetector::ADC(StBeamDirection eastwest, unsigned int i) const
{
    if (i <= mMaxVpdCounter && i!=0)
        return mADC[eastwest][i-1];
    else
        return 0;
}

unsigned short
StVpdTriggerDetector::TDC(StBeamDirection eastwest, unsigned int i) const
{
    if (i <= mMaxVpdCounter && i!=0)
        return mTDC[eastwest][i-1];
    else
        return 0;
}

unsigned short
StVpdTriggerDetector::EarliestTDC(StBeamDirection eastwest) const
{
    return mEarliestTDC[eastwest];
}

void
StVpdTriggerDetector::setADC(StBeamDirection eastwest, unsigned int i, unsigned short v)
{
    if (i <= mMaxVpdCounter && i!=0)
        mADC[eastwest][i-1] = v;
}

void
StVpdTriggerDetector::setTDC(StBeamDirection eastwest, unsigned int i, unsigned short v)
{
    if (i <= mMaxVpdCounter && i!=0)
        mTDC[eastwest][i-1] = v;
}

unsigned short StVpdTriggerDetector::ADCmxq(StBeamDirection eastwest, unsigned int i) const 	//WJL
{
    if (i <= mMaxVpdCounter && i!=0)
        return mADCmxq[eastwest][i-1];
    else
        return 0;
}
unsigned short StVpdTriggerDetector::TDCmxq(StBeamDirection eastwest, unsigned int i) const 	//WJL
{
    if (i <= mMaxVpdCounter && i!=0)
        return mTDCmxq[eastwest][i-1];
    else
        return 0;
}
void StVpdTriggerDetector::setADCmxq(StBeamDirection eastwest, unsigned int i, unsigned short v) 	//WJL
{
    if (i <= mMaxVpdCounter && i!=0)
        mADCmxq[eastwest][i-1] = v;
}

void StVpdTriggerDetector::setTDCmxq(StBeamDirection eastwest, unsigned int i, unsigned short v) 	//WJL
{
    if (i <= mMaxVpdCounter && i!=0)
        mTDCmxq[eastwest][i-1] = v;
}

