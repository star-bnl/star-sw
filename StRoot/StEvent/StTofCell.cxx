/***************************************************************************
 *
 * $Id: StTofCell.cxx,v 2.2 2003/08/05 17:12:32 ullrich Exp $
 *
 * Author: F. Geurts, May 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofCell.cxx,v $
 * Revision 2.2  2003/08/05 17:12:32  ullrich
 * Added position() methods and member.
 *
 * Revision 2.1  2003/05/21 18:24:20  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTofCell.h"
#include "StTrack.h"
#include "StThreeVectorD.hh"

static const char rcsid[] = "$Id: StTofCell.cxx,v 2.2 2003/08/05 17:12:32 ullrich Exp $";

ClassImp(StTofCell)

StTofCell::StTofCell()
  : mTrayIndex(0), mModuleIndex(0), mCellIndex(0), mAdc(0), mTdc(0), mAssociatedTrack(0)
{ /* noop */ }

StTofCell::StTofCell(int trayId, int moduleId, int cellId, int rawAdc, int rawTdc, StTrack* track, int flag, StThreeVectorD& pos)
  : mTrayIndex(trayId), mModuleIndex(moduleId), mCellIndex(cellId), mAdc(rawAdc), mTdc(rawTdc), mAssociatedTrack(track), mMatchFlag(flag), mPosition(pos)
{ /* noop */ }

StTofCell::~StTofCell() { /* noop */ }

void
StTofCell::setAssociatedTrack(StTrack* val)
{
    mAssociatedTrack = val;
}

StTrack*
StTofCell::associatedTrack() {return mAssociatedTrack;}

const StTrack*
StTofCell::associatedTrack() const {return mAssociatedTrack;}

void
StTofCell::setPosition(const StThreeVectorD& p) {mPosition = p;}

const StThreeVectorD&
StTofCell::position() const {return mPosition;}

int
StTofCell::operator==(const StTofCell& p) const
{
    return (p.mTrayIndex == mTrayIndex && p.mModuleIndex == mModuleIndex &&
	    p.mCellIndex == mCellIndex && p.mAdc  == mAdc && p.mTdc  == mTdc &&
	    p.mAssociatedTrack == mAssociatedTrack &&
            p.mMatchFlag == mMatchFlag);
}

int
StTofCell::operator!=(const StTofCell& p) const
{
    return !(*this == p);  // use operator==()
}
