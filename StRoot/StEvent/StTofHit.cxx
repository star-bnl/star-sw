/***************************************************************************
 *
 * $Id: StTofHit.cxx,v 2.2 2001/03/24 03:34:59 perev Exp $
 *
 * Author: Wei-Ming Zhang, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 *
 * $Log: StTofHit.cxx,v $
 * Revision 2.2  2001/03/24 03:34:59  perev
 * clone() -> clone() const
 *
 * Revision 2.1  2000/12/21 23:52:24  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTofHit.h"

static const char rcsid[] = "$Id: StTofHit.cxx,v 2.2 2001/03/24 03:34:59 perev Exp $";

ClassImp(StTofHit)
    
StTofHit::StTofHit() : mSlatIndex(0), mNumberOfMips(0), mFlightTime(0)
{/* nopt */}

StTofHit::~StTofHit() {/* nopt */}

StObject*
StTofHit::clone() const { return new StTofHit(*this); }

ostream&
operator<<(ostream& os, const StTofHit& hit)
{
    return (os << "StTofHit::> "  << hit.position()    << ", Id= "
                                  << hit.slatIndex()   << ", (tof="
                                  << hit.flightTime()  << ", (nm="
                                  << hit.numberOfMips()<< ')');
}

