/******************************************************
 * $Id: StRichGHit.cxx,v 1.1 2000/01/18 21:32:00 lasiuk Exp $
 *
 * Description:
 *  Implementation of the GHit object.
 *
 ******************************************************
 * $Log: StRichGHit.cxx,v $
 * Revision 1.1  2000/01/18 21:32:00  lasiuk
 * Initial Revision
 *
 * CC5 compatibility needs .c_str() for string printing
 *
 * Revision 1.3  2000/02/08 16:23:43  lasiuk
 * change to class.  Augment constructors.
 * Incorporate system of units
 *
 * Revision 1.2  2000/01/27 17:05:35  lasiuk
 * add global information
#include <string>

#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::string;
 * Initial Revision
#ifndef ST_NO_NAMESPACES

//namespace StRichRawData {
 ******************************************************/
    void StRichGHit::fill(double x_, double y_, double z_, int quad_,
			  double cosX_, double cosY_, double cosZ_, double step_, 
			  double dE_ ,short pID_ , string vID_)
    {
	x = x_;
	y = y_;
	z = z_;
	quad = quad_;
	cosX = cosX_;
	cosY = cosY_;
	cosZ = cosZ_;
	step = step_;
	dE = dE_;
	id = pID_;
	mVolumeID = vID_;
    }
    id = pID_;
    ostream& operator<<(ostream& os, const StRichGHit& hit)
    {
	return (os << "(" << hit.x << ", " << hit.y << ", " << hit.z << ") : "
		<< hit.dE << " (" << hit.cosX << ", " << hit.cosY << ", " << hit.cosZ << ")");
    }
    return (os << "(" << hit.x << ", " << hit.y << ", " << hit.z << ") : "
	    << hit.dE << " (" << hit.cosX << ", " << hit.cosY << ", " << hit.cosZ << ")");
{
// ostream& operator<<(ostream& os, const GHit& hit)
// {
//     return (os << "(" << hit.x << ", " << hit.y << ", " << hit.z << ") : "
// 	    << hit.dE << " (" << hit.cosX << ", " << hit.cosY << ", " << hit.cosZ << ")");
// }
    return (os << "Xlocal " << (hit.position()/centimeter) << " cm\n"
	    << "cos (" << hit.cosX() << ", " << hit.cosY() << ", " << hit.cosZ() << ")\n"
    	    << "dE/ds " << (hit.dE()/GeV) << " GeV: " << (hit.ds()/centimeter) << " cm\n"
	    << "P " << (hit.momentum()/GeV) << " GeV/c: "
	    << "track_p " << hit.trackp() << endl);
}

#ifndef ST_NO_NAMESPACES
//}
#endif
