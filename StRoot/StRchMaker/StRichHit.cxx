/***************************************************************************
 *
 * $Id: StRichHit.cxx,v 1.1 2000/04/05 16:39:41 lasiuk Exp $
 *
 * Author: bl
 ***************************************************************************
 *
 * Description: Implementation of Hit definition
 *
 ***************************************************************************
 *
 * $Log: StRichHit.cxx,v $
 * Revision 1.1  2000/04/05 16:39:41  lasiuk
 * Initial Revision
 *
 * Revision 1.1  2000/04/05 16:39:41  lasiuk
 * Initial Revision
 *
 **************************************************************************/

#include "StRichHit.h"

StRichHit::StRichHit() {/* nopt */}

StRichHit::StRichHit(double x, double y)
    : mPosition(x,y,0)
{
    // For Diagnoistics ONLY
}

StRichHit::~StRichHit() {/* nopt */}

ostream& operator<<(ostream& os, const StRichHit& hit)
{
    return (os << "StRichHit::> " << hit.internal() << ", q= "
	                          << hit.charge()   << ", (#"
	                          << hit.clusterNumber() << ')');
}
