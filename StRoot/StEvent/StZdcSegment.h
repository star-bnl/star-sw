/***************************************************************************
 *
 * $Id: StZdcSegment.h,v 1.1 1999/01/30 03:58:11 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StZdcSegment.h,v $
 * Revision 1.1  1999/01/30 03:58:11  fisyak
 * Root Version of StEvent
 *
 * Revision 1.4  1999/04/28 22:27:41  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:54:28  wenaus
 * version with constructors for table-based loading
 *

#ifdef __ROOT__
#include "TObject.h"
#endif
class StZdcSegment : public TObject {
#include "StObject.h"
#include "StArray.h"
class StZdcSegment : public StObject {
public:
    StZdcSegment();
    StZdcSegment(Short_t id, Float_t a, Float_t t);
    ~StZdcSegment();
    // StZdcSegment(const StZdcSegment &right);
    // const StZdcSegment & operator=(const StZdcSegment &right);

    Short_t id() const;
    Float_t adc() const;
    Float_t tdc() const;

    void setId(Short_t);
    void setAdc(Float_t);
    void setTdc(Float_t);
    
protected:
#ifdef __ROOT__
	ClassDef(StZdcSegment,1)  //StZdcSegment structure
#endif
    Float_t mAdc;

};
StCollectionDef(ZdcSegment)

inline Short_t StZdcSegment::id() const { return mId; }

inline Float_t StZdcSegment::adc() const { return mAdc; }

inline Float_t StZdcSegment::tdc() const { return mTdc; }

#endif
