/***************************************************************************
 *
 * $Id: StTpcPixel.cxx,v 2.3 2001/04/05 04:00:57 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcPixel.cxx,v $
 * Revision 2.3  2001/04/05 04:00:57  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  1999/12/13 20:16:31  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
 *
 * Revision 2.1  1999/10/13 19:45:32  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StTpcPixel.h"

static const char rcsid[] = "$Id: StTpcPixel.cxx,v 2.3 2001/04/05 04:00:57 ullrich Exp $";

ClassImp(StTpcPixel)

StTpcPixel::StTpcPixel()
{
    mDetectorSectorRow = 0;
    mPadTimeAdc = 0;
}

StTpcPixel::StTpcPixel(unsigned short sr, unsigned int pta)
   : mDetectorSectorRow(sr), mPadTimeAdc(pta)
{ /* noop */ }

StTpcPixel::~StTpcPixel() { /* noop */ }
    
int
StTpcPixel::operator==(const StTpcPixel& p) const
{
    return p.mDetectorSectorRow  == mDetectorSectorRow &&
           p.mPadTimeAdc == mPadTimeAdc;
}

int
StTpcPixel::operator!=(const StTpcPixel& p) const
{
    return !(*this == p);  // use operator==()
}

unsigned short
StTpcPixel::detector() const
{
    return (mDetectorSectorRow & ~(~0U<<4));      // bits 0-3
}

unsigned short
StTpcPixel::sector() const
{
    return (mDetectorSectorRow>>4) & ~(~0U<<5);   // bits 4-8
}

unsigned short
StTpcPixel::padrow() const
{
    return (mDetectorSectorRow>>9) & ~(~0U<<6);   // bits 9-14
}

unsigned int
StTpcPixel::pad()  const
{
    return (mPadTimeAdc & ~(~0UL<<8));             // bits 0-7
}

unsigned int
StTpcPixel::timebin() const
{
    return (mPadTimeAdc>>8) & ~(~0UL<<9);          // bits 8-16
}

unsigned int
StTpcPixel::adc()  const
{
    return (mPadTimeAdc>>17) & ~(~0UL<<10);        // bits 17-26
}
