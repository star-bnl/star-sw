/******************************************************
 * $Id: StRichGHit.cxx,v 1.4 2000/02/08 23:50:02 lasiuk Exp $
 *
 * Description:
 *  Implementation of the GHit object.
 *
 ******************************************************
 * $Log: StRichGHit.cxx,v $
 * Revision 1.4  2000/02/08 23:50:02  lasiuk
 * CC5 compatibility needs .c_str() for string printing
 *
 * Revision 1.5  2000/02/14 01:12:50  lasiuk
 * keep the track pointer info
 *
 * Revision 1.4  2000/02/08 23:50:02  lasiuk
 * CC5 compatibility needs .c_str() for string printing
 *
 * Revision 1.3  2000/02/08 16:23:43  lasiuk
 * change to class.  Augment constructors.
 * Incorporate system of units
 *
 * Revision 1.2  2000/01/27 17:05:35  lasiuk
 * add global information
 *
 * Revision 1.1  2000/01/18 21:32:00  lasiuk
 * Initial Revision
#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif

 *
 ******************************************************/

#include "StRichGHit.h"
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif
StRichGHit::StRichGHit(double x, double y, double z, int q, short pID) 
    : mXLocal(x, y, z), mQuad(q), mId(pID) {/* nopt*/ }

StRichGHit::StRichGHit(double x, double y, double z, double dE, short pID, string vID)
    :  mXLocal(x,y,z), mId(pID), mVolumeId(vID), mdE(dE) {/* nopt*/ }

StRichGHit::StRichGHit(double x, double y, double z, int track_p, short pID) 
    : mXLocal(x, y, z), mTrackp(track_p), mId(pID) {/* nopt*/ }

StRichGHit::StRichGHit(double x, double y, double z, short pID)
    : mXLocal(x,y,z), mId(pID) { /* nopt */ }
    : mXLocal(x,y,z), mId(pID), mVolumeId(vID), mdE(dE), mdS(ds), mP(px, py,pz)
StRichGHit::StRichGHit(double x, double y, double z, double dE, double ds, short pID, string vID,
		       double px, double py, double pz)
    : mXLocal(x,y,z), mP(px, py,pz), mdS(ds), mId(pID), mVolumeId(vID), mdE(dE)
{
    mCosX = mP.x()/abs(mP);
void StRichGHit::fill(double x, double y, double z, int quad,
    mCosZ = mP.z()/abs(mP);
}

StRichGHit::~StRichGHit() {/*nopt*/}

void StRichGHit::fill(double x, double y, double z, int track_p,
    mQuad = quad;
		      double dE ,short pID , string vID)
{
    mXLocal.setX(x);
    mXLocal.setY(y);
    mXLocal.setZ(z);
    mTrackp = track_p;
    mCosX = cosX;
    mCosY = cosY;
    mCosZ = cosZ;
void StRichGHit::fill(double x, double y, double z, int quad,
    mdE = dE;
    mId = pID;
    mVolumeId = vID;
}

void StRichGHit::fill(double x, double y, double z, int track_p,
		      double cosX, double cosY, double cosZ,
		      double step, double dE,
    mQuad = quad;
		      short pID , string vID)
{
    mXLocal.setX(x);
    mXLocal.setY(y);
    mXLocal.setZ(z);
    mTrackp = track_p;
    mCosX = cosX;
    mCosY = cosY;
    mCosZ = cosZ;
    mdS   = step;
    mdE = dE;
    mdS   = step;
    mdE = dE;
    mMass = mass;
    mId = pID;
    mVolumeId = vID;
}

void StRichGHit::addGlobal(double xx, double yy, double zz)
{
    mXGlobal.setX(xx);
    mXGlobal.setY(yy);
    mXGlobal.setZ(zz);
}
    os << "q        "  << mQuad << endl;
void StRichGHit::full(ostream& os = cout) const
{
    os << "XLocal:  "  << mXLocal << endl;
    os << "XGlobal: "  << mXGlobal << endl;
    os << "P:       "  << mP << endl;
    os << "track_p  "  << mTrackp << endl;
    os << "cos      "  << mCosX << ' ' << mCosY << ' ' << mCosZ << endl;
    os << "dE/ds    "  << mdE << ' ' << mdS << endl;
    os << "volume   "  << mVolumeId.c_str() << endl;
    os << "id       "  << mId << endl;
}

	    << "quadrant " << hit.quadrant() << endl);
{
    return (os << "Xlocal " << (hit.position()/centimeter) << " cm\n"
	    << "cos (" << hit.cosX() << ", " << hit.cosY() << ", " << hit.cosZ() << ")\n"
    	    << "dE/ds " << (hit.dE()/GeV) << " GeV: " << (hit.ds()/centimeter) << " cm\n"
	    << "P " << (hit.momentum()/GeV) << " GeV/c: "
	    << "track_p " << hit.trackp() << endl);
}

#ifndef ST_NO_NAMESPACES
//}
#endif
