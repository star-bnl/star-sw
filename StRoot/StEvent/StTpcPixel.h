/*!
 * \class StTpcPixel 
 * \author Thomas Ullrich, Jan 1999
 */
/***************************************************************************
 *
 * $Id: StTpcPixel.h,v 2.4 2002/02/22 22:56:52 jeromel Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcPixel.h,v $
 * Revision 2.4  2002/02/22 22:56:52  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.3  2001/04/05 04:00:44  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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
    StTpcPixel(unsigned short, unsigned int);
    StTpcPixel(const dst_pixel_st&);
    virtual ~StTpcPixel();
    // StTpcPixel(const StSvtTpcPixel&);         use default
    // StTpcPixel& operator=(const StTpcPixel&); use default
    
    int operator==(const StTpcPixel&) const;
    int operator!=(const StTpcPixel&) const;

    unsigned short detector() const;
    unsigned short sector() const;
    unsigned short padrow() const;
    unsigned int   pad() const;
    unsigned int   timebin() const;
    unsigned int   adc() const;
    
protected:
    UShort_t  mDetectorSectorRow;
    UInt_t    mPadTimeAdc;
    
    ClassDef(StTpcPixel,1)  //StTpcPixel structure
};

#endif
