/***************************************************************************
 *
 * $Id: StTpcPixel.cxx,v 2.1 1999/10/13 19:45:32 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcPixel.cxx,v $
 * Revision 2.1  1999/10/13 19:45:32  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:45:32  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StTpcPixel.h"

static const char rcsid[] = "$Id: StTpcPixel.cxx,v 2.1 1999/10/13 19:45:32 ullrich Exp $";

ClassImp(StTpcPixel)

StTpcPixel::StTpcPixel()
{
    mDetectorSectorRow = 0;
    mPadTimeAdc = 0;
}

StTpcPixel::StTpcPixel(UShort_t sr, ULong_t pta)
   : mDetectorSectorRow(sr), mPadTimeAdc(pta)
{ /* noop */ }

StTpcPixel::~StTpcPixel() { /* noop */ }
    
Int_t
StTpcPixel::operator==(const StTpcPixel& p) const
{
    return p.mDetectorSectorRow  == mDetectorSectorRow &&
           p.mPadTimeAdc == mPadTimeAdc;
}

Int_t
StTpcPixel::operator!=(const StTpcPixel& p) const
{
    return !(*this == p);  // use operator==()
}

UShort_t
StTpcPixel::detector() const
{
    return (mDetectorSectorRow & ~(~0U<<4));      // bits 0-3
}

UShort_t
StTpcPixel::sector() const
{
    return (mDetectorSectorRow>>4) & ~(~0U<<5);   // bits 4-8
}

UShort_t
StTpcPixel::row() const
{
    return (mDetectorSectorRow>>9) & ~(~0U<<6);   // bits 9-14
}

ULong_t
StTpcPixel::pad()  const
{
    return (mPadTimeAdc & ~(~0UL<<8));             // bits 0-7
}

ULong_t
StTpcPixel::timebin() const
{
    return (mPadTimeAdc>>8) & ~(~0UL<<9);          // bits 8-16
}

ULong_t
StTpcPixel::adc()  const
{
    return (mPadTimeAdc>>17) & ~(~0UL<<10);        // bits 17-26
}
