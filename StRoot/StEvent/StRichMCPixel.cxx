/***************************************************************************
 *
 * $Id: StRichMCPixel.cxx,v 2.2 2001/04/05 04:00:53 ullrich Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichMCPixel.cxx,v $
 * Revision 2.2  2001/04/05 04:00:53  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  2000/05/22 21:44:41  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StRichMCPixel.h"

static const char rcsid[] = "$Id: StRichMCPixel.cxx,v 2.2 2001/04/05 04:00:53 ullrich Exp $";

ClassImp(StRichMCPixel)

StRichMCPixel::StRichMCPixel()
    : StRichPixel(0)
{ /* noop */ }

StRichMCPixel::StRichMCPixel(unsigned int rawData)
   : StRichPixel(rawData)
{ /* noop */ }

StRichMCPixel::StRichMCPixel(unsigned int packedData, const StSPtrVecRichMCInfo& mcinfo)
    : StRichPixel(packedData), mInfo(mcinfo)
{ /* noop */ }

StRichMCPixel::~StRichMCPixel() { /* noop */ }
    
int
StRichMCPixel::operator==(const StRichMCPixel& p) const
{
    return (p.mPackedData  == mPackedData);
}

int
StRichMCPixel::operator!=(const StRichMCPixel& p) const
{
    return !(*this == p);  // use operator==()
}

