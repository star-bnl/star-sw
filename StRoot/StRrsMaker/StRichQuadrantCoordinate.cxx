/*********************************************************************
 *
 * $Id: StRichQuadrantCoordinate.cxx,v 1.3 2000/02/29 18:06:27 lasiuk Exp $
 *
 * Author: brian Jan 20, 2000
 *
 **********************************************************************
 *
 * Description:  Quadrant!
 *
 **********************************************************************
 *
 * $Log: StRichQuadrantCoordinate.cxx,v $
 * Revision 1.3  2000/02/29 18:06:27  lasiuk
 * change operator<< to reflect proper x,y,z
 *
 * Revision 1.2  2000/02/08 23:45:48  lasiuk
 * Default constructor initializer changed for CC4.2
 *
 * Revision 1.1  2000/02/08 16:34:10  lasiuk
 * Initial Revision:  eventually for StUtilities
 *
 **********************************************************************/
#include "StRichQuadrantCoordinate.h"

StRichQuadrantCoordinate::StRichQuadrantCoordinate()
    : mPos(0,0,0) {/**/}

StRichQuadrantCoordinate::StRichQuadrantCoordinate(const double x, const double y, const double z, const int q)
    : mPos(x,y,z), mQuadrant(q) { /* nopt */}

StRichQuadrantCoordinate::StRichQuadrantCoordinate(const StThreeVector<double>& pos, const int q = -1)
    : mPos(pos), mQuadrant(q) { /* nopt */ }

StRichQuadrantCoordinate::~StRichQuadrantCoordinate() {/**/}

// Non-member Functions
ostream& operator<<(ostream& os, const StRichQuadrantCoordinate& a)
{
    return os << "Rich_Quad: x,y,z "
              << a.position().x() << ", "
              << a.position().y() << ", "
              << a.position().z() << ": "
	      << a.quadrant();
}
