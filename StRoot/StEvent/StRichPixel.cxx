/***************************************************************************
 *
 * $Id: StRichPixel.cxx,v 2.1 1999/10/13 19:45:05 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichPixel.cxx,v $
 * Revision 2.1  1999/10/13 19:45:05  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:45:05  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StRichPixel.h"
#include "tables/dst_rch_pixel.h"

static const char rcsid[] = "$Id: StRichPixel.cxx,v 2.1 1999/10/13 19:45:05 ullrich Exp $";

ClassImp(StRichPixel)

StRichPixel::StRichPixel()
{
    mPad = mAdc = 0;
}

StRichPixel::StRichPixel(UShort_t pad, UShort_t adc)
   : mPad(pad), mAdc(adc)
{ /* noop */ }

StRichPixel::StRichPixel(const dst_rch_pixel_st& p)
   : mPad(p.pad), mAdc(p.adc)
{ /* noop */ }

StRichPixel::~StRichPixel() { /* noop */ }
    
Int_t
StRichPixel::operator==(const StRichPixel& p) const
{
    return p.mPad  == mPad &&
           p.mAdc == mAdc;
}

Int_t
StRichPixel::operator!=(const StRichPixel& p) const
{
    return !(*this == p);  // use operator==()
}

UShort_t
StRichPixel::module() const
{
    return mPad>>10;
}

UShort_t
StRichPixel::channel() const
{
    return mPad & 0x3ff;
}

UShort_t
StRichPixel::adc()  const
{
    return mAdc;
}
