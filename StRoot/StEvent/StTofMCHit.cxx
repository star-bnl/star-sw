/***************************************************************************
 *
 * $Id: StTofMCHit.cxx,v 2.1 2001/04/26 01:07:42 ullrich Exp $
 *
 * Author: Wei-Ming Zhang, April 2001 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofMCHit.cxx,v $
 * Revision 2.1  2001/04/26 01:07:42  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTofMCHit.h"

static const char rcsid[] = "$Id: StTofMCHit.cxx,v 2.1 2001/04/26 01:07:42 ullrich Exp $";

ClassImp(StTofMCHit)
    
StTofMCHit::StTofMCHit()  {/* nopt */}

StTofMCHit::~StTofMCHit() {/* nopt */}

StObject*
StTofMCHit::clone() const { return new StTofMCHit(*this); }

ostream&
operator<<(ostream& os, const StTofMCHit& hit)
{
    return (os << "StTofMCHit::> "  << hit.position()  << ", slatId= "
                                    << hit.slatIndex() << ", trkId= "
                                    << hit.trkId()     << ", GeantId= "
                                    << hit.gId() << endl);
}
