/***************************************************************************
 *
 * $Id: StRichPixel.h,v 2.1 1999/10/13 19:43:36 ullrich Exp $
 *
 * Author: Thomas Ullrich, Aug 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichPixel.h,v $
 * Revision 2.1  1999/10/13 19:43:36  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StRichPixel_hh
#define StRichPixel_hh

#include "StObject.h"
#include "StEnumerations.h"

class dst_rch_pixel_st;

class StRichPixel : public StObject {
public:
    StRichPixel();
    StRichPixel(UShort_t, UShort_t);      // pad, adc
    StRichPixel(const dst_rch_pixel_st&);
    // StRichPixel(const StRichPixel&);            use default
    // StRichPixel& operator=(const StRichPixel&); use default
    ~StRichPixel();
    
    Int_t operator==(const StRichPixel&) const;
    Int_t operator!=(const StRichPixel&) const;

    UShort_t  module() const;
    UShort_t  channel() const;
    UShort_t  adc() const;
    
protected:
    UShort_t  mPad;
    UShort_t  mAdc;
    
    ClassDef(StRichPixel,1)
};

#endif
