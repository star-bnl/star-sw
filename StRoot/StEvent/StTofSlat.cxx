/***************************************************************************
 *
 * $Id: StTofSlat.cxx,v 2.7 2003/07/28 21:00:22 ullrich Exp $
 *
 * Author: Wei-Ming Zhang, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofSlat.cxx,v $
 * Revision 2.7  2003/07/28 21:00:22  ullrich
 * Revised version: new but not inheriting from StHit as before.
 *
 * Revision 2.6  2003/07/11 00:01:03  jeromel
 * Re-adding preceeding revision
 *
 * Revision 2.3  2003/05/21 18:23:18  ullrich
 * Major Revision of ToF classes (F. Geurts)
 *
 * Revision 2.2  2001/04/05 04:00:56  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  2000/12/21 23:52:22  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTofSlat.h"
#include "StTrack.h"

static const char rcsid[] = "$Id: StTofSlat.cxx,v 2.7 2003/07/28 21:00:22 ullrich Exp $";

ClassImp(StTofSlat)

StTofSlat::StTofSlat()
  : mSlatIndex(0), mAdc(0), mTdc(0), mAssociatedTrack(0)
{ /* noop */ }

StTofSlat::StTofSlat(unsigned short slatId, unsigned short rawAdc,
                     unsigned short rawTdc, StTrack *track,
                     float zhit, unsigned short hitprof, unsigned short matchflag)
  : mSlatIndex(slatId), mAdc(rawAdc), mTdc(rawTdc), mAssociatedTrack(track),
    mZhit(zhit), mHitProf(hitprof), mMatchFlag(matchflag)
{ /* noop */ }

StTofSlat::~StTofSlat() { /* noop */ }

void
StTofSlat::setAssociatedTrack(StTrack* val) {mAssociatedTrack = val;}

StTrack*
StTofSlat::associatedTrack() {return mAssociatedTrack;}

const StTrack*
StTofSlat::associatedTrack() const {return mAssociatedTrack;}

void
StTofSlat::setPosition(const StThreeVectorF& p) {mPosition = p;}

const StThreeVectorF&
StTofSlat::position() const {return mPosition;}

   
int
StTofSlat::operator==(const StTofSlat& p) const
{
    return (p.mSlatIndex == mSlatIndex &&
            p.mAdc  == mAdc && p.mTdc  == mTdc &&
	    p.mAssociatedTrack == mAssociatedTrack &&
            p.mZhit == mZhit &&
            p.mHitProf == mHitProf &&
            p.mMatchFlag == mMatchFlag);
}

int
StTofSlat::operator!=(const StTofSlat& p) const
{
    return !(*this == p);  // use operator==()
}

