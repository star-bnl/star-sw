/***************************************************************************
 *
 * Authors: K.S. Engle and Richard Witt (witt@usna.edu), Jan 2013
 * based on StFgtHit
 *
 ***************************************************************************
 *
 * Description: see header file.
 *
 ***************************************************************************/

#include "StGmtHit.h"
#include <cmath>
ClassImp(StGmtHit)
//________________________________________________________________________________
ostream&  operator<<(ostream& os, const StGmtHit& v) {
  return os << Form("Gmt m %3i ",v.getModule())
	    << *((StHit *)&v)
	    << Form(" Adc X/Y  %5.1f/%5.1f locX = %8.3f +/- %7.3f locY = %8.3f +/- %7.3f",
		    v.getAdcX(), v.getAdcY(), v.getLocalX(), v.getErrorLocalX(), v.getLocalY(), v.getErrorLocalY()); 
}
//________________________________________________________________________________
void   StGmtHit::Print(Option_t *option) const {cout << *this << endl;}
//________________________________________________________________________________

