/***************************************************************************
 *
 * $Id: StRichMCHit.cxx,v 2.1 2000/05/22 21:44:29 ullrich Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description: Implementation of persistent Hit definition
 *
 ***************************************************************************
 *
 * $Log: StRichMCHit.cxx,v $
 * Revision 2.1  2000/05/22 21:44:29  ullrich
 * Initial Revision
 *
 **************************************************************************/

#include "StRichMCHit.h"

static const char rcsid[] = "$Id: StRichMCHit.cxx,v 2.1 2000/05/22 21:44:29 ullrich Exp $";

ClassImp(StRichMCHit)
    
StRichMCHit::StRichMCHit() {/* nopt */}

StRichMCHit::StRichMCHit(const StThreeVectorF& xl, const StThreeVectorF& dx)
    : StRichHit(xl, dx)
{
    // This is used for off-line
}

StRichMCHit::StRichMCHit(const StThreeVectorF& xg, const StThreeVectorF& dx,
		     ULong_t hp, Float_t q, Float_t maxAdc, UChar_t tc)
    : StRichHit(xg,dx,hp,q,maxAdc,tc)
{
    // For Storage in the StRichHitCollection
}

StRichMCHit::StRichMCHit(const StThreeVectorF& xg, const StThreeVectorF& dx,
			 ULong_t hp, Float_t q, Float_t maxAdc, UChar_t tc,
			 StRichMCInfo& info)
    : StRichHit(xg,dx,hp,q,maxAdc,tc), mInfo(info)
{
    // For Storage in the StRichHitCollection
}

StRichMCHit::~StRichMCHit() {/* noop */}

StObject*
StRichMCHit::clone() { return new StRichMCHit(*this); }

