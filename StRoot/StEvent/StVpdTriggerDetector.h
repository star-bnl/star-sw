/*!
 * \class StVpdTriggerDetector 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StVpdTriggerDetector.h,v 2.3 2002/02/22 22:56:53 jeromel Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVpdTriggerDetector.h,v $
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

class StVpdTriggerDetector : public StObject {
public:
    StVpdTriggerDetector();
    StVpdTriggerDetector(const dst_TrgDet_st&);
    // StVpdTriggerDetector(const StVpdTriggerDetector&);            use default
    // StVpdTriggerDetector& operator=(const StVpdTriggerDetector&); use default
    virtual ~StVpdTriggerDetector();
    
    unsigned int  numberOfVpdCounters() const;
    float         adc(unsigned int) const;
    float         time(unsigned int) const;
    float         minimumTime(StBeamDirection) const;
    float         vertexZ() const;

    void setAdc(unsigned int, float);
    void setTime(unsigned int, float);
    void setMinimumTime(StBeamDirection, float);
    void setVertexZ(float);
    
protected:
    enum {mMaxVpdCounter = 48};
    Float_t  mAdc[mMaxVpdCounter];
    Float_t  mTime[mMaxVpdCounter];
    Float_t  mMinimumTime[2];
    Float_t  mVertexZ;
    
    ClassDef(StVpdTriggerDetector,1)
};
#endif
