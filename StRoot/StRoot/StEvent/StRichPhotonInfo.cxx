/***************************************************************************
 *
 * $Id: StRichPhotonInfo.cxx,v 2.4 2004/07/15 16:36:25 ullrich Exp $
 *
 * Author: Brian Lasiuk, Nov 2000
 ***************************************************************************
 *
 * Description: Implementation of persistent Photon Info definition
 *
 ***************************************************************************
 *
 * $Log: StRichPhotonInfo.cxx,v $
 * Revision 2.4  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.3  2001/04/05 04:00:53  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  2001/03/24 03:34:56  perev
 * clone() -> clone() const
 *
 * Revision 2.1  2000/11/25 11:51:03  lasiuk
 * Initial Revision
 *
 **************************************************************************/

#include "StRichPhotonInfo.h"

static const char rcsid[] = "$Id: StRichPhotonInfo.cxx,v 2.4 2004/07/15 16:36:25 ullrich Exp $";

ClassImp(StRichPhotonInfo)
    
StRichPhotonInfo::StRichPhotonInfo()
    : mD(-999), mSigma(-999), mAzimuth(-999)
{ /* nopt */ }

StRichPhotonInfo::StRichPhotonInfo(double d, double s, double psi)
    : mD(d), mSigma(s), mAzimuth(psi)
{  /* nopt */ }

StRichPhotonInfo::~StRichPhotonInfo() {/* nopt */}

ostream&
operator<<(ostream& os, const StRichPhotonInfo& hit)
{
    return (os << "StRichPhotonInfo::>"
            << " d= " << hit.d()
            << " sig= " << hit.sigma()
            << " psi= " << hit.azimuth() << " ");
}
