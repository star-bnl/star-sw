/*********************************************************************
 *
 * $Id: StRichLocalCoordinate.cxx,v 2.0 2000/08/09 16:17:01 gans Exp $
 *
 * Author: brian Jan 20, 2000
 *
 **********************************************************************
 *
 * Description:  Raw data information along with access functions
 *
 **********************************************************************
 *
 * $Log: StRichLocalCoordinate.cxx,v $
 * Revision 2.0  2000/08/09 16:17:01  gans
 * Readded Files That were not added in last CVS. Cosmetic Changes, naming convention
 * for StRichDrawableT(foo)
 *
 * Revision 1.3  2000/02/29 18:05:50  lasiuk
 * change operator<< to reflect x,y,z
 *
 * Revision 1.2  2000/02/08 23:45:47  lasiuk
 * Default constructor initializer changed for CC4.2
 *
 * Revision 1.1  2000/02/08 16:34:08  lasiuk
 * Initial Revision:  eventually for StUtilities
 *
 **********************************************************************/
#include "StRichLocalCoordinate.h"

StRichLocalCoordinate::StRichLocalCoordinate()
    :mPos(0,0,0) {/*nopt*/}
StRichLocalCoordinate::StRichLocalCoordinate(const double x, const double y, const double z)
    : mPos(x,y,z) {/* nopt */ }

StRichLocalCoordinate::StRichLocalCoordinate(const StThreeVector<double>& pos)
    :mPos(pos) {/* nopt */}

StRichLocalCoordinate::~StRichLocalCoordinate() {/* nopt */}

// Non-member Functions
ostream& operator<<(ostream& os, const StRichLocalCoordinate& a)
{
    return os << "RICH_Local: x,y,z "
              << a.position().x() << ", "
              << a.position().y() << ", "
              << a.position().z();
}
