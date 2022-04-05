/***************************************************************************
 *
 * $Id: StMtdTriggerDetector.h,v 2.1 2007/07/02 20:21:55 ullrich Exp $
 *
 * Author: Akio Agawa, July 2007
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMtdTriggerDetector.h,v $
 * Revision 2.1  2007/07/02 20:21:55  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StMtdTriggerDetector_hh
#define StMtdTriggerDetector_hh
#include "StObject.h"
#include "StEnumerations.h"

class StTriggerData;

class StMtdTriggerDetector : public StObject {
public:
    StMtdTriggerDetector();
    StMtdTriggerDetector(const StTriggerData&);
    virtual ~StMtdTriggerDetector();
    
    unsigned int   numberOfMtdCounters() const;
    unsigned short adc(StBeamDirection eastwest, unsigned int pmt) const;
    unsigned short tdc(StBeamDirection eastwest, unsigned int pmt) const;

    void setAdc(StBeamDirection eastwest, unsigned int pmt, unsigned short v);
    void setTdc(StBeamDirection eastwest, unsigned int pmt, unsigned short v);

protected:
    enum {mMaxMtdCounter=8};
    unsigned short mADC[2][mMaxMtdCounter];
    unsigned short mTDC[2][mMaxMtdCounter];
    
    ClassDef(StMtdTriggerDetector,1)
};
#endif
