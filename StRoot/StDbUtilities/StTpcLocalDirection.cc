// * $Id: StTpcLocalDirection.cc,v 1.1 2004/03/05 17:22:54 fisyak Exp $
#include "StTpcLocalDirection.hh"
static const char rcsid[] = "$Id: StTpcLocalDirection.cc,v 1.1 2004/03/05 17:22:54 fisyak Exp $";
// Non-member Functions
ostream& operator<<(ostream& os, const StTpcLocalDirection& a) {
  return os << "TPC_Local Direction ("
	    << a.position().x() << ", "
	    << a.position().y() << ", "
	    << a.position().z() << ")";
}
