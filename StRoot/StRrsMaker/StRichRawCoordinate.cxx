/***********************************************************************
 *
 * $Id: StRichRawCoordinate.cxx,v 1.2 2000/04/05 16:03:54 lasiuk Exp $
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
