/***************************************************************************
 *
 * $Id: StTofSlat.cxx,v 2.1 2000/12/21 23:52:22 ullrich Exp $
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
 * Revision 2.1  2000/12/21 23:52:22  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTofSlat.h"

static const char rcsid[] = "$Id: StTofSlat.cxx,v 2.1 2000/12/21 23:52:22 ullrich Exp $";

ClassImp(StTofSlat)

StTofSlat::StTofSlat()
    : mSlatIndex(0), mAdc(0), mTdc(0)
{ /* nopt */ }

StTofSlat::StTofSlat(UShort_t slatId, UShort_t rawAdc, UShort_t rawTdc)
   : mSlatIndex(slatId), mAdc(rawAdc), mTdc(rawTdc)
{ /* noop */ }

StTofSlat::~StTofSlat() { /* noop */ }
    
Int_t
StTofSlat::operator==(const StTofSlat& p) const
{
    return (p.mSlatIndex == mSlatIndex &&
            p.mAdc  == mAdc && p.mTdc  == mTdc);
}

Int_t
StTofSlat::operator!=(const StTofSlat& p) const
{
    return !(*this == p);  // use operator==()
}

