/***************************************************************************
 *
 * $Id: StRichHit.cxx,v 2.5 2009/11/23 16:34:07 fisyak Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description: Implementation of persistent Hit definition
 *
 ***************************************************************************
 *
 * $Log: StRichHit.cxx,v $
 * Revision 2.5  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.4  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.3  2001/04/05 04:00:52  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  2001/03/24 03:34:54  perev
 * clone() -> clone() const
 *
 * Revision 2.1  2000/05/22 21:44:47  ullrich
 * Initial Revision
 *
 **************************************************************************/

#include "StRichHit.h"

static const char rcsid[] = "$Id: StRichHit.cxx,v 2.5 2009/11/23 16:34:07 fisyak Exp $";

ClassImp(StRichHit)
    
StRichHit::StRichHit() {/* nopt */}

StRichHit::StRichHit(const StThreeVectorF& xl, const StThreeVectorF& dx)
    : mLocal(xl), mLError(dx)
{
    // This is used for off-line
}
StRichHit::StRichHit(const StThreeVectorF& xg, const StThreeVectorF& dx,
                     unsigned int hp, float q, float maxAdc, unsigned char tc)
    : StHit(xg,dx,hp,q,tc),mMaxAmplitude(maxAdc)
{
    // For Storage in the StRichHitCollection
}


StRichHit::~StRichHit() {/* nopt */}

ostream&
operator<<(ostream& os, const StRichHit& hit)
{
    return (os << "StRichHit::> " << hit.internal() << ", q= "
                                  << hit.charge()   << ", (#"
                                  << hit.clusterNumber() << ')');
}
