/***************************************************************************
 *
 * $Id: StRichPixel.h,v 2.4 2001/02/07 16:04:05 lasiuk Exp $
 *
 * Author: Thomas Ullrich, Aug 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichPixel.h,v $
 * Revision 2.4  2001/02/07 16:04:05  lasiuk
 * check the 11th bit for overflow.
 * Overflow is 1024
 *
 * Revision 2.3  2000/01/13 21:06:22  lasiuk
 * add rich pixel info/containers
 *
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

    void      setPackedData(ULong_t);
    
    UShort_t  pad() const;
    UShort_t  row() const;
    UShort_t  adc() const;
    
protected:
    ULong_t  mPackedData;
    
    ClassDef(StRichPixel,1)
};

inline void
StRichPixel::setPackedData(ULong_t pixel)
{
    mPackedData = pixel;
}

inline UShort_t
StRichPixel::pad() const
{
    return (mPackedData & 0xff);  // first 8 bits
}

inline UShort_t
StRichPixel::row() const
{
    return ( (mPackedData>>8) & 0xff);  // second 8 bits
}

inline UShort_t
StRichPixel::adc()  const
{
    //
    // 11 bits are stored.  The 11th bit is the overflow
    // if the 11th bit is set, return saturated
    // otherwise, return the 10 bit value
    //return ( (mPackedData>>16) & 0x3ff); // 10bits
    
    return ( ( (mPackedData>>26) & 0x1) ? 1024 : ( (mPackedData>>16) & 0x3ff) );

}

#endif
