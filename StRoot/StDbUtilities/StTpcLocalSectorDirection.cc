// * $Id: StTpcLocalSectorDirection.cc,v 1.1 2004/03/05 17:22:55 fisyak Exp $
#include "StTpcLocalSectorDirection.hh"
static const char rcsid[] = "$Id: StTpcLocalSectorDirection.cc,v 1.1 2004/03/05 17:22:55 fisyak Exp $";
ostream& operator<<(ostream& os, const StTpcLocalSectorDirection& a) {
  return os << "TPC_Local_Sector Direction( ("
	    << a.position().x()  << ", "
	    << a.position().y()  << ", "
	    << a.position().z()  << "),"
	    << a.fromSector() << ")";
}
