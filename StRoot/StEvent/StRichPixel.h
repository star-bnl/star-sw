/***************************************************************************
 *
 * $Id: StRichPixel.h,v 2.2 2000/01/10 17:12:21 lasiuk Exp $
 *
 * Author: Thomas Ullrich, Aug 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichPixel.h,v $
 * Revision 2.2  2000/01/10 17:12:21  lasiuk
 * remove dst_rch_pixel dependency;
 * change stored data to a single long;
 * modify unpacking routines;
 *
 * Revision 2.1  1999/10/13 19:43:36  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StRichPixel_hh
#define StRichPixel_hh

#include "StObject.h"
#include "StEnumerations.h"

class StRichPixel : public StObject {
public:
    StRichPixel();
    StRichPixel(ULong_t packedData);
    // StRichPixel(const StRichPixel&);            use default
    // StRichPixel& operator=(const StRichPixel&); use default
    ~StRichPixel();
    
    Int_t operator==(const StRichPixel&) const;
    Int_t operator!=(const StRichPixel&) const;

    UShort_t  pad() const;
    UShort_t  row() const;
    UShort_t  adc() const;
    
protected:
    ULong_t  mPackedData;
    
    ClassDef(StRichPixel,1)
};

#endif
