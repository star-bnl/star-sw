/***************************************************************************
 *
 * $Id: StTofSlat.cxx,v 2.2 2001/04/05 04:00:56 ullrich Exp $
 *
 * Author: Wei-Ming Zhang, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 *
 * $Log: StTofSlat.cxx,v $
 * Revision 2.2  2001/04/05 04:00:56  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  2000/12/21 23:52:22  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTofSlat.h"

static const char rcsid[] = "$Id: StTofSlat.cxx,v 2.2 2001/04/05 04:00:56 ullrich Exp $";

ClassImp(StTofSlat)

StTofSlat::StTofSlat()
    : mSlatIndex(0), mAdc(0), mTdc(0)
{ /* nopt */ }

StTofSlat::StTofSlat(unsigned short slatId, unsigned short rawAdc, unsigned short rawTdc)
   : mSlatIndex(slatId), mAdc(rawAdc), mTdc(rawTdc)
{ /* noop */ }

StTofSlat::~StTofSlat() { /* noop */ }
    
int
StTofSlat::operator==(const StTofSlat& p) const
{
    return (p.mSlatIndex == mSlatIndex &&
            p.mAdc  == mAdc && p.mTdc  == mTdc);
}

int
StTofSlat::operator!=(const StTofSlat& p) const
{
    return !(*this == p);  // use operator==()
}

