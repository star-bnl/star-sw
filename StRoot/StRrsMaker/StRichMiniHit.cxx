/******************************************************
 * $Id: StRichMiniHit.cxx,v 1.2 2000/04/05 15:59:30 lasiuk Exp $
 *
 * Description:
 *  Implementation of the MiniHit object.
 *
 ******************************************************
 * $Log: StRichMiniHit.cxx,v $
 * Revision 1.2  2000/04/05 15:59:30  lasiuk
 * short --> int
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
			     int tP, int iD, int gid, double mass, StRichSignalType type)
    :  mX(x), mP(p), mTrackp(tP), mId(iD), mGid(gid), mMass(mass), mType(type) {/* nopt*/ }

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
