/***************************************************************************
 *
 * $Id: StRichCerenkovPhoton.cxx,v 1.1 2001/08/21 17:58:33 lasiuk Exp $
 *
 * Author:  bl Mar 27, 2001
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              for use in Cerenkov Ray Tracing Algorithm
 *              to be added in the StRichCerenkovHistogram
 ***************************************************************************
 *
 * $Log: StRichCerenkovPhoton.cxx,v $
 * Revision 1.1  2001/08/21 17:58:33  lasiuk
 * for 2000 analysis
 *
 **************************************************************************/

#include "StRichCerenkovPhoton.h"
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

StRichCerenkovPhoton::StRichCerenkovPhoton()  { /* nopt*/ }

 StRichCerenkovPhoton::StRichCerenkovPhoton(double theta,
					    double phi,
					    StRichHit* hit)
     : mTheta(theta), mPhi(phi), mHit(hit) { /* nopt*/ }
    
StRichCerenkovPhoton::~StRichCerenkovPhoton() {/* nopt */}

// Non-member function
ostream& operator<<(ostream& os, const StRichCerenkovPhoton& photon)
{
    return (os << "StRichCerenkovPhoton: theta = "
	    << (photon.theta()/degree) << "  phi = " << (photon.phi()/degree));
}
