/***************************************************************************
 *
 * $Id: StRichMCHit.cxx,v 2.4 2004/07/15 16:36:25 ullrich Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description: Implementation of persistent Hit definition
 *
 ***************************************************************************
 *
 * $Log: StRichMCHit.cxx,v $
 * Revision 2.4  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.3  2001/04/05 04:00:52  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  2001/03/24 03:34:55  perev
 * clone() -> clone() const
 *
 * Revision 2.1  2000/05/22 21:44:29  ullrich
 * Initial Revision
 *
 **************************************************************************/

#include "StRichMCHit.h"

static const char rcsid[] = "$Id: StRichMCHit.cxx,v 2.4 2004/07/15 16:36:25 ullrich Exp $";

ClassImp(StRichMCHit)
    
StRichMCHit::StRichMCHit() {/* nopt */}

StRichMCHit::StRichMCHit(const StThreeVectorF& xl, const StThreeVectorF& dx)
    : StRichHit(xl, dx)
{
    // This is used for off-line
}

StRichMCHit::StRichMCHit(const StThreeVectorF& xg, const StThreeVectorF& dx,
                     unsigned int hp, float q, float maxAdc, unsigned char tc)
    : StRichHit(xg,dx,hp,q,maxAdc,tc)
{
    // For Storage in the StRichHitCollection
}

StRichMCHit::StRichMCHit(const StThreeVectorF& xg, const StThreeVectorF& dx,
                         unsigned int hp, float q, float maxAdc, unsigned char tc,
                         StRichMCInfo& info)
    : StRichHit(xg,dx,hp,q,maxAdc,tc), mInfo(info)
{
    // For Storage in the StRichHitCollection
}

StRichMCHit::~StRichMCHit() {/* noop */}
