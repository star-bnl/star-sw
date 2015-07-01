/***************************************************************************
 *
 * $Id: StTofMCHit.cxx,v 2.3 2004/07/15 16:36:25 ullrich Exp $
 *
 * Author: Wei-Ming Zhang, April 2001 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofMCHit.cxx,v $
 * Revision 2.3  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.2  2003/05/21 18:22:46  ullrich
 * Major Revision of ToF classes (F. Geurts)
 *
 * Revision 2.1  2001/04/26 01:07:42  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTofMCHit.h"

static const char rcsid[] = "$Id: StTofMCHit.cxx,v 2.3 2004/07/15 16:36:25 ullrich Exp $";

ClassImp(StTofMCHit)
    
StTofMCHit::StTofMCHit()  {/* noop */}

StTofMCHit::~StTofMCHit() {/* noop */}

ostream&
operator<<(ostream& os, const StTofMCHit& hit)
{
  return (os << "StTofMCHit::> " << ", trayId= " 
	  << hit.trayIndex() << ", moduleId= "
	  << hit.moduleIndex() << ", cell= " << hit.cellIndex()
	  << ", trkId= " << hit.trkId()
	  << ", GeantId= " << hit.gId());
}
