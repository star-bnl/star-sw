/***************************************************************************
 *
 * $Id: StTpcPixel.h,v 2.2 1999/12/13 20:16:34 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcPixel.h,v $
 * Revision 2.2  1999/12/13 20:16:34  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
 *
 * Revision 2.1  1999/10/13 19:44:04  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTpcPixel_hh
#define StTpcPixel_hh

#include "StObject.h"
#include "StEnumerations.h"

class dst_pixel_st;

class StTpcPixel : public StObject {
public:
    StTpcPixel();
    StTpcPixel(UShort_t, ULong_t);
    StTpcPixel(const dst_pixel_st&);
    virtual ~StTpcPixel();
    // StTpcPixel(const StSvtTpcPixel&);         use default
    // StTpcPixel& operator=(const StTpcPixel&); use default
    
    Int_t operator==(const StTpcPixel&) const;
    Int_t operator!=(const StTpcPixel&) const;

    UShort_t detector() const;
    UShort_t sector() const;
    UShort_t padrow() const;
    ULong_t  pad() const;
    ULong_t  timebin() const;
    ULong_t  adc() const;
    
protected:
    UShort_t  mDetectorSectorRow;
    ULong_t   mPadTimeAdc;
    
    ClassDef(StTpcPixel,1)  //StTpcPixel structure
};

#endif
