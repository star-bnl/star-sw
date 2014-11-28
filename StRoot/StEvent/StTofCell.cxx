/***************************************************************************
 *
 * $Id: StTofCell.cxx,v 2.5 2007/09/19 17:32:02 ullrich Exp $
 *
 * Author: F. Geurts, May 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofCell.cxx,v $
 * Revision 2.5  2007/09/19 17:32:02  ullrich
 * New member (mLeadingEdgeTime,  mTrailingEdgeTime) and related functions and updates added.
 *
 * Revision 2.4  2004/02/05 17:59:30  ullrich
 * Changed $LINK to StLink mechanism and add new member.
 *
 * Revision 2.3  2003/08/28 23:24:17  jeromel
 * Modif in class
 *
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

static const char rcsid[] = "$Id: StTofCell.cxx,v 2.5 2007/09/19 17:32:02 ullrich Exp $";

ClassImp(StTofCell)

StTofCell::StTofCell()
    : mTrayIndex(0), mModuleIndex(0), mCellIndex(0), mDaqIndex(0), mAdc(0), mTdc(0),
      mLeadingEdgeTime(0), mTrailingEdgeTime(0), mAssociatedTrack(0), mZhit(0),
      mMatchFlag(0)
{ /* noop */ }

StTofCell::StTofCell(int trayId, int moduleId, int cellId, int daqId, 
		     int rawAdc, int rawTdc, StTrack *track, float zhit,
		     int flag, const StThreeVectorD& pos)
  : mTrayIndex(trayId), mModuleIndex(moduleId), mCellIndex(cellId),
    mDaqIndex(daqId), mAdc(rawAdc), mTdc(rawTdc),
    mLeadingEdgeTime(0.), mTrailingEdgeTime(0.),
    mAssociatedTrack(track), mZhit(zhit), mMatchFlag(flag), mPosition(pos)
{ /* noop */ }

StTofCell::StTofCell(int trayId, int moduleId, int cellId, int daqId, 
		     StTrack *track, float zhit,
		     int flag, const StThreeVectorD& pos)
  : mTrayIndex(trayId), mModuleIndex(moduleId), mCellIndex(cellId),
    mDaqIndex(daqId), mAdc(0), mTdc(0),
    mLeadingEdgeTime(0.), mTrailingEdgeTime(0.),
    mAssociatedTrack(track), mZhit(zhit), mMatchFlag(flag), mPosition(pos)
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
  return ( (p.mTrayIndex == mTrayIndex && p.mModuleIndex == mModuleIndex &&
	    p.mCellIndex == mCellIndex && p.mDaqIndex == mDaqIndex &&
	    p.mAdc  == mAdc && p.mTdc  == mTdc &&
	    p.mAssociatedTrack == mAssociatedTrack &&
	    p.mZhit == mZhit &&
            p.mMatchFlag == mMatchFlag) ||
	   (p.mTrayIndex == mTrayIndex && p.mModuleIndex == mModuleIndex &&
	    p.mCellIndex == mCellIndex && p.mDaqIndex == mDaqIndex &&
	    p.mLeadingEdgeTime  == mLeadingEdgeTime &&
	    p.mTrailingEdgeTime  == mTrailingEdgeTime &&
	    p.mAssociatedTrack == mAssociatedTrack &&
	    p.mZhit == mZhit &&
            p.mMatchFlag == mMatchFlag) );
}

int
StTofCell::operator!=(const StTofCell& p) const
{
    return !(*this == p);  // use operator==()
}
