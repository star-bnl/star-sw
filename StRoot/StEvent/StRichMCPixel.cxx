/***************************************************************************
 *
 * $Id: StRichMCPixel.cxx,v 2.1 2000/05/22 21:44:41 ullrich Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichMCPixel.cxx,v $
 * Revision 2.1  2000/05/22 21:44:41  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StRichMCPixel.h"

static const char rcsid[] = "$Id: StRichMCPixel.cxx,v 2.1 2000/05/22 21:44:41 ullrich Exp $";

ClassImp(StRichMCPixel)

StRichMCPixel::StRichMCPixel()
    : StRichPixel(0)
{ /* noop */ }

StRichMCPixel::StRichMCPixel(ULong_t rawData)
   : StRichPixel(rawData)
{ /* noop */ }

StRichMCPixel::StRichMCPixel(ULong_t packedData, const StSPtrVecRichMCInfo& mcinfo)
    : StRichPixel(packedData), mInfo(mcinfo)
{ /* noop */ }

StRichMCPixel::~StRichMCPixel() { /* noop */ }
    
Int_t
StRichMCPixel::operator==(const StRichMCPixel& p) const
{
    return (p.mPackedData  == mPackedData);
}

Int_t
StRichMCPixel::operator!=(const StRichMCPixel& p) const
{
    return !(*this == p);  // use operator==()
}

