/***************************************************************************
 *
 * $Id: StZdcTriggerDetector.h,v 2.6 2001/07/13 16:25:20 perev Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StZdcTriggerDetector.h,v $
 * Revision 2.6  2001/07/13 16:25:20  perev
 * last static array fixed
 *
 * Revision 2.5  2001/07/12 22:58:33  ullrich
 * Added variable to store the vertex_z from timing info.
 *
 * Revision 2.4  2001/04/05 04:00:47  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.3  2000/07/13 12:51:13  ullrich
 * Added new method numberOfZdcWords to replace old one with wrong name.
 *
 * Revision 2.2  1999/12/20 12:54:48  ullrich
 * Adapted changed in trigger table dst_TrgDet
 *
 * Revision 2.1  1999/10/13 19:44:22  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StZdcTriggerDetector_hh
#define StZdcTriggerDetector_hh
#include "StObject.h"
#include "StEnumerations.h"

class dst_TrgDet_st;

class StZdcTriggerDetector : public StObject {
public:
    StZdcTriggerDetector();
    StZdcTriggerDetector(const dst_TrgDet_st&);
    // StZdcTriggerDetector(const StZdcTriggerDetector&);            use default
    // StZdcTriggerDetector& operator=(const StZdcTriggerDetector&); use default
    virtual ~StZdcTriggerDetector();
    
    float         adcSum(StBeamDirection) const;
    float         adcSum() const;
    unsigned int  numberOfZdcWords() const;
    float         adc(unsigned int) const;
    float         tdc(unsigned int) const;
    float         vertexZ() const;

    void setAdc(unsigned int, float);
    void setTdc(unsigned int, float);
    void setAdcSum(StBeamDirection, float);
    void setAdcSum(float);
    void setVertexZ(float);

    unsigned int   numberOfZdcCounters() const;  // usage depreciated, to be removed soon
    
protected:
    enum {mMaxZdcWords = 16};
    Float_t  mAdc[mMaxZdcWords];
    Float_t  mTdc[mMaxZdcWords];
    Float_t  mSumAdc[2];
    Float_t  mSum;
    Float_t  mVertexZ;
    
    ClassDef(StZdcTriggerDetector,2)
};
#endif
