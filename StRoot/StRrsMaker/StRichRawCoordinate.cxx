/***********************************************************************
 *
 * $Id: StRichRawCoordinate.cxx,v 2.0 2000/08/09 16:17:03 gans Exp $
 *
 * Author: brian Jan 27, 2000
 *
 ************************************************************************
 *
 * Description:  Raw data information along with access functions
 *
 ************************************************************************
 *
 * $Log: StRichRawCoordinate.cxx,v $
 * Revision 2.0  2000/08/09 16:17:03  gans
 * Readded Files That were not added in last CVS. Cosmetic Changes, naming convention
 * for StRichDrawableT(foo)
 *
 * Revision 1.2  2000/04/05 16:03:54  lasiuk
 * fractional pad/column to give 1-1 correspondence
 *
 * Revision 1.1  2000/02/08 16:34:11  lasiuk
 * Initial Revision:  eventually for StUtilities
 *
 ***********************************************************************/

#include "StRichRawCoordinate.h"

StRichRawCoordinate::StRichRawCoordinate() {/**/}

StRichRawCoordinate::StRichRawCoordinate(const double pad, const double row)
    : mRow(row), mPad(pad) {/**/}

StRichRawCoordinate::~StRichRawCoordinate() {/**/}


// Non-Member function
ostream& operator<<(ostream& os, const StRichRawCoordinate& a)
{
    return os << "RAW pad: " << a.pad() << " row= " << a.row();
}
