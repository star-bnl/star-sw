/******************************************************
 * $Id: StRichMiniHit.cxx,v 1.1 2000/03/17 14:54:49 lasiuk Exp $
 *
 * Description:
 *  Implementation of the MiniHit object.
 *
 ******************************************************
 * $Log: StRichMiniHit.cxx,v $
 * Revision 1.1  2000/03/17 14:54:49  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.1  2000/03/17 14:54:49  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 ******************************************************/

#include "StRichMiniHit.h"
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

StRichMiniHit::StRichMiniHit() {/* nopt */}

StRichMiniHit::StRichMiniHit(StThreeVector<double> x,
			     StThreeVector<double> p,
			     int tP, short pID, double mass, StRichSignalType type)
    :  mX(x), mP(p), mTrackp(tP), mId(pID), mMass(mass), mType(type) {/* nopt*/ }

StRichMiniHit::~StRichMiniHit() {/*nopt*/}

ostream& operator<<(ostream& os, const StRichMiniHit& hit)
{
    return (os << "mX " << (hit.position()/centimeter) << " cm "
	       << "mP " << (hit.momentum()/GeV) << " GeV/c: "
	       << "track_p " << hit.trackp() << " " 
	       << "mass " << hit.mass()/GeV << " GeV "
	       << "id " << hit.id()
	       << " type " << (static_cast<int>(hit.type())));
}
