/***************************************************************************
 *
 * $Id: StTofMCHit.cxx,v 1.1 2001/04/24 20:27:08 wzhang Exp $
 *
 * Author: Wei-Ming Zhang, April 2001
 ***************************************************************************
 *
 * Description:
 * Inherited from StTofHit with MC info (two Ids) added.
 *
 ***************************************************************************
 *
 * $Log: StTofMCHit.cxx,v $
 * Revision 1.1  2001/04/24 20:27:08  wzhang
 * First release
 *
 *
 **************************************************************************/
#include "StTofMCHit.h"

static const char rcsid[] = "$Id: StTofMCHit.cxx,v 1.1 2001/04/24 20:27:08 wzhang Exp $";

ClassImp(StTofMCHit)
    
StTofMCHit::StTofMCHit() 
{/* nopt */}

StTofMCHit::~StTofMCHit() {/* nopt */}

StObject*
StTofMCHit::MCClone() { return new StTofMCHit(*this); }

ostream&
operator<<(ostream& os, const StTofMCHit& hit)
{
    return (os << "StTofMCHit::> "  << hit.position()  << ", slatId= "
                                    << hit.slatIndex() << ", trkId= "
                                    << hit.trkId()     << ", GeantId= "
                                    << hit.gId() << endl);
}
