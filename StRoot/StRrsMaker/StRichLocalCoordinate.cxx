/*********************************************************************
 *
 * $Id: StRichLocalCoordinate.cxx,v 1.1 2000/02/08 16:34:08 lasiuk Exp $
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
 * Revision 1.1  2000/02/08 16:34:08  lasiuk
 * Initial Revision:  eventually for StUtilities
 *
 **********************************************************************/
#include "StRichLocalCoordinate.h"

StRichLocalCoordinate::StRichLocalCoordinate()
    :mPos(0) {/*nopt*/}
StRichLocalCoordinate::StRichLocalCoordinate(const double x, const double y, const double z)
    : mPos(x,y,z) {/* nopt */ }

StRichLocalCoordinate::StRichLocalCoordinate(const StThreeVector<double>& pos)
    :mPos(pos) {/* nopt */}

StRichLocalCoordinate::~StRichLocalCoordinate() {/* nopt */}

// Non-member Functions
ostream& operator<<(ostream& os, const StRichLocalCoordinate& a)
{
    return os << "RICH_Local: "
              << a.position().z() << ", "
              << a.position().y() << ", "
              << a.position().x();
}
