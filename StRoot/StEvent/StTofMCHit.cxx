/***************************************************************************
 *
 * $Id: StTofMCHit.cxx,v 2.2 2003/05/21 18:22:46 ullrich Exp $
 *
 * Author: Wei-Ming Zhang, April 2001 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofMCHit.cxx,v $
 * Revision 2.2  2003/05/21 18:22:46  ullrich
 * Major Revision of ToF classes (F. Geurts)
 *
 * Revision 2.1  2001/04/26 01:07:42  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTofMCHit.h"

static const char rcsid[] = "$Id: StTofMCHit.cxx,v 2.2 2003/05/21 18:22:46 ullrich Exp $";

ClassImp(StTofMCHit)
    
StTofMCHit::StTofMCHit()  {/* noop */}

StTofMCHit::~StTofMCHit() {/* noop */}

StObject*
StTofMCHit::clone() const { return new StTofMCHit(*this); }

ostream&
operator<<(ostream& os, const StTofMCHit& hit)
{
  return (os << "StTofMCHit::> " << ", trayId= " 
	  << hit.trayIndex() << ", moduleId= "
	  << hit.moduleIndex() << ", cell= " << hit.cellIndex()
	  << ", trkId= " << hit.trkId()
	  << ", GeantId= " << hit.gId());
}
