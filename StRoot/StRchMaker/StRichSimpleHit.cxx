/***************************************************************************
 *
 * $Id: StRichSimpleHit.cxx,v 2.2 2000/11/01 16:52:41 lasiuk Exp $
 *
 * Author: bl
 ***************************************************************************
 *
 * Description: Implementation of Hit definition
 *
 ***************************************************************************
 *
 * $Log: StRichSimpleHit.cxx,v $
 * Revision 2.2  2000/11/01 16:52:41  lasiuk
 * Use the enumerated types from StEvent.  correct the NAMESPACE macro
 * and print more bits in the printBit member
 *
 * Revision 2.1  2000/09/29 19:01:24  lasiuk
 * enumerated types added for flags
 * c'tor includes cp from persistent hit
 * number of pads added as a member
 *
 * Revision 2.0  2000/08/09 16:22:12  gans
 * Cosmetic Changes. Naming convention for TDrawable objects
 *
 * Revision 1.3  2000/05/23 16:55:55  lasiuk
 * Incorporate new MC info
 * add clone() where necessary
 * accomodate name changes
 *
 * Revision 1.2  2000/05/18 11:42:39  lasiuk
 * mods for pre StEvent writing
 *
 * Revision 1.1  2000/05/18 11:34:14  lasiuk
 * iRename revision
 *
 **************************************************************************/

#include "StRichSimpleHit.h"

StRichSimpleHit::StRichSimpleHit() {/* nopt */}

StRichSimpleHit::StRichSimpleHit(const StThreeVector<double>& xl, const StThreeVector<double>& dx)
    : mLocal(xl), mSigma(dx), mFlags(0)
{
    // This is used for off-line
}

#ifdef __ROOT__
StRichSimpleHit::StRichSimpleHit(const StRichHit* hit)
    : mGlobal(hit->position().x(),hit->position().y(),hit->position().z()),
      mLocal(hit->local().x(), hit->local().y(), hit->local().z()),
      mInternal(hit->internal().x(), hit->internal().y(), hit->internal().z()),
      mSigma(hit->positionError().x(), hit->positionError().y(), hit->positionError().z()),
      mCharge(hit->charge()),
      mMaxAmplitude(hit->maxAmplitude()),
      mClusterNumber(hit->clusterNumber()),
      mFlags(hit->reservedLong())
{
    cout << "StRichSimpleHit::StRichSimpleHit(const StRichHit*)" << endl;
}
#endif

StRichSimpleHit::~StRichSimpleHit() {/* nopt */}

ostream& operator<<(ostream& os, const StRichSimpleHit& hit)
{
    return (os << "StRichSimpleHit::> " << hit.internal() << ", q= "
	                          << hit.charge()   << ", (#"
	                          << hit.clusterNumber() << ')');
}

void StRichSimpleHit::printBits() const
{
    const int numberOfBits = 15;  // a long is really 32 bits you know
    cout << "StRichSimpleHitFlags: ";
    for(int ii=0; ii<numberOfBits; ii++) {
	unsigned long mask = static_cast<unsigned long>(pow(2.,ii));
	cout << ((mFlags & mask) == 0 ? 0 : 1);
    }
    cout << endl;
}
