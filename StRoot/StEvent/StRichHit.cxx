/***************************************************************************
 *
 * $Id: StRichHit.cxx,v 2.1 2000/05/22 21:44:47 ullrich Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description: Implementation of persistent Hit definition
 *
 ***************************************************************************
 *
 * $Log: StRichHit.cxx,v $
 * Revision 2.1  2000/05/22 21:44:47  ullrich
 * Initial Revision
 *
 **************************************************************************/

#include "StRichHit.h"
#include "tables/St_dst_point_Table.h"

static const char rcsid[] = "$Id: StRichHit.cxx,v 2.1 2000/05/22 21:44:47 ullrich Exp $";

ClassImp(StRichHit)
    
StRichHit::StRichHit() {/* nopt */}

StRichHit::StRichHit(const StThreeVectorF& xl, const StThreeVectorF& dx)
    : mLocal(xl), mLError(dx)
{
    // This is used for off-line
}
StRichHit::StRichHit(const StThreeVectorF& xg, const StThreeVectorF& dx,
		     ULong_t hp, Float_t q, Float_t maxAdc, UChar_t tc)
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

StObject*
StRichHit::clone() { return new StRichHit(*this); }

