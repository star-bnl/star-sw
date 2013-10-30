/*!
 * \class StVpdTriggerDetector 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StVpdTriggerDetector.h,v 2.5 2013/10/30 15:47:16 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVpdTriggerDetector.h,v $
 * Revision 2.5  2013/10/30 15:47:16  ullrich
 * Added ADCmxq(), TDCmxq() and referring data member (WJL).
 *
 * Revision 2.4  2007/04/03 20:11:41  ullrich
 * Modified for actual VPD used in 2007.
 *
 * Revision 2.3  2002/02/22 22:56:53  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/04/05 04:00:47  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  1999/10/13 19:44:20  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StVpdTriggerDetector_hh
#define StVpdTriggerDetector_hh
#include "StObject.h"
#include "StEnumerations.h"
 
class dst_TrgDet_st;
class StTriggerData;

class StVpdTriggerDetector : public StObject {
public:
    StVpdTriggerDetector();
    StVpdTriggerDetector(const dst_TrgDet_st&);
    StVpdTriggerDetector(const StTriggerData&);
    // StVpdTriggerDetector& operator=(const StVpdTriggerDetector&); use default
    virtual ~StVpdTriggerDetector();
    
    unsigned int   numberOfVpdCounters() const;
    unsigned short ADC(StBeamDirection eastwest, unsigned int pmt) const;
    unsigned short TDC(StBeamDirection eastwest, unsigned int pmt) const;
    unsigned short ADCmxq(StBeamDirection eastwest, unsigned int pmt) const;		//WJL
    unsigned short TDCmxq(StBeamDirection eastwest, unsigned int pmt) const;		//WJL
    unsigned short EarliestTDC(StBeamDirection eastwest) const;
    unsigned short TimeDifference() const {return mTimeDifference;};

    void setADC(StBeamDirection eastwest, unsigned int pmt, unsigned short v);
    void setTDC(StBeamDirection eastwest, unsigned int pmt, unsigned short v);
    void setADCmxq(StBeamDirection eastwest, unsigned int pmt, unsigned short v);		//WJL
    void setTDCmxq(StBeamDirection eastwest, unsigned int pmt, unsigned short v);		//WJL
    void setEarliestTDC(StBeamDirection eastwest, unsigned short v) {mEarliestTDC[eastwest]=v;}
    void setTimeDifference(unsigned short v) {mTimeDifference = v;}   

protected:
    enum {mMaxVpdCounter = 16};
    unsigned short mADC[2][mMaxVpdCounter];
    unsigned short mTDC[2][mMaxVpdCounter];
    unsigned short mADCmxq[2][mMaxVpdCounter];	//WJL
    unsigned short mTDCmxq[2][mMaxVpdCounter];	//WJL
    unsigned short mEarliestTDC[2];
    unsigned short mTimeDifference;
    unsigned short mYear;

    ClassDef(StVpdTriggerDetector,3)
};
#endif
