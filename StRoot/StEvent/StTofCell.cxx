/***************************************************************************
 *
 * $Id: StTofCell.cxx,v 2.1 2003/05/21 18:24:20 ullrich Exp $
 *
 * Author: F. Geurts, May 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofCell.cxx,v $
 * Revision 2.1  2003/05/21 18:24:20  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTofCell.h"
#include "StTrack.h"

static const char rcsid[] = "$Id: StTofCell.cxx,v 2.1 2003/05/21 18:24:20 ullrich Exp $";

ClassImp(StTofCell)

StTofCell::StTofCell()
    : mTrayIndex(0), mModuleIndex(0), mCellIndex(0), mAdc(0), mTdc(0), mAssociatedTrack(0)
{ /* noop */ }

StTofCell::StTofCell(int trayId, int moduleId, int cellId, int rawAdc, int rawTdc, StTrack* track )
    : mTrayIndex(trayId), mModuleIndex(moduleId), mCellIndex(cellId), mAdc(rawAdc), mTdc(rawTdc), mAssociatedTrack(track)
{ /* noop */ }

StTofCell::~StTofCell() { /* noop */ }

void
StTofCell::setAssociatedTrack(StTrack* val)
{
    mAssociatedTrack = val;
}

StTrack*
StTofCell::associatedTrack()
{
    return mAssociatedTrack;
}

const StTrack*
StTofCell::associatedTrack() const
{
    return mAssociatedTrack;
}

int
StTofCell::operator==(const StTofCell& p) const
{
    return (p.mTrayIndex == mTrayIndex && p.mModuleIndex == mModuleIndex &&
	    p.mCellIndex == mCellIndex && p.mAdc  == mAdc && p.mTdc  == mTdc &&
	    p.mAssociatedTrack == mAssociatedTrack);
}

int
StTofCell::operator!=(const StTofCell& p) const
{
    return !(*this == p);  // use operator==()
}
