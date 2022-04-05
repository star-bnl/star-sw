/***************************************************************************
 *
 * $Id: StTrackDetectorInfo.cxx,v 2.19 2016/02/24 22:02:11 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackDetectorInfo.cxx,v $
 * Revision 2.19  2016/02/24 22:02:11  ullrich
 * Set of changes to account for SST (compared to SSD)
 *
 * Revision 2.18  2015/05/19 20:09:45  perev
 * added Ist & Pxl
 *
 * Revision 2.17  2012/07/21 03:33:58  perev
 * Add Other hits
 *
 * Revision 2.16  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.15  2005/07/06 19:00:52  fisyak
 * Add include of StThreeVectorD.hh
 *
 * Revision 2.14  2004/10/20 18:55:13  ullrich
 * Name of enum changed: StStarMaxR(Z) now StStarMaxTrackRangeR(Z).
 *
 * Revision 2.13  2004/10/17 03:35:10  perev
 * Error check improved
 *
 * Revision 2.12  2004/10/13 16:11:59  ullrich
 * Added optional arg to addHit() to allow NOT to increase ref counter.
 *
 * Revision 2.11  2004/08/05 22:23:32  ullrich
 * Fixed bug in first argument type of setNumberOfPoints().
 *
 * Revision 2.10  2004/08/05 19:25:03  ullrich
 * Changes to the handling of numberOfPoints() to allow ITTF more flexibility.
 *
 * Revision 2.9  2004/07/15 16:36:26  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.8  2003/10/30 20:07:32  perev
 * Check of quality added
 *
 * Revision 2.7  2001/04/05 04:00:58  ullrich`
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.6  2001/03/24 03:35:00  perev
 * clone() -> clone() const
 *
 * Revision 2.5  2000/04/20 13:29:58  ullrich
 * Added new methods and removed inconsistencies in numberOfPoints().
 *
 * Revision 2.4  2000/01/20 14:43:07  ullrich
 * Fixed bug in numberOfPoints(). Sum was wrong.
 *
 * Revision 2.3  1999/11/01 12:45:09  ullrich
 * Modified unpacking of point counter
 *
 * Revision 2.2  1999/10/28 22:27:27  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:39  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <math.h>
#include "StDetectorId.h"
#include "StTrackDetectorInfo.h"
#include "StFunctional.h"
#include "StHit.h"
#include "StThreeVectorD.hh"
ClassImp(StTrackDetectorInfo)

static const char rcsid[] = "$Id: StTrackDetectorInfo.cxx,v 2.19 2016/02/24 22:02:11 ullrich Exp $";

StTrackDetectorInfo::StTrackDetectorInfo() : mNumberOfPoints(0),
					     mNumberOfPointsTpc(0),
					     mNumberOfPointsFtpcWest(0),
					     mNumberOfPointsFtpcEast(0),
					     mNumberOfPointsSvt(0),
					     mNumberOfPointsSsd(0),
					     mNumberOfPointsOth(0),
					     mNumberOfPointsIst(0),
					     mNumberOfPointsPxl(0)
{ /* noop */ }

StTrackDetectorInfo::~StTrackDetectorInfo() { /* noop */ }

const StThreeVectorF&
StTrackDetectorInfo::firstPoint() const { return mFirstPoint; }

const StThreeVectorF&
StTrackDetectorInfo::lastPoint() const { return mLastPoint; }

unsigned short
StTrackDetectorInfo::numberOfPoints() const
{
    if (mNumberOfPoints) {
	return (numberOfPoints(kTpcId) +
		numberOfPoints(kSvtId) +
		numberOfPoints(kSsdId));
    }
    else {
	return (numberOfPoints(kTpcId) +
		numberOfPoints(kFtpcWestId) +
		numberOfPoints(kFtpcEastId) +
		numberOfPoints(kSvtId) +
		numberOfPoints(kSsdId) +	// internally treat SST=SSD
		numberOfPoints(kIstId) +	
		numberOfPoints(kPxlId));	
    }
}

unsigned short
StTrackDetectorInfo::numberOfPoints(StDetectorId det) const
{
    if (mNumberOfPoints) {    
	// 1*tpc + 1000*svt + 10000*ssd (Helen/Spiros Oct 29, 1999)
	switch (det) {
	case kFtpcWestId:
	case kFtpcEastId:
	case kTpcId:
	    return mNumberOfPoints%1000;
	    break;
	case kSvtId:
	    return (mNumberOfPoints%10000)/1000;
	    break;
	case kSsdId:
	case kSstId:
	    return mNumberOfPoints/10000;
	    break;
	default:
	    return 0;
	}
    }
    else {
	switch (det) {
	case kFtpcWestId:
	    return mNumberOfPointsFtpcWest;
	    break;
	case kFtpcEastId:
	    return mNumberOfPointsFtpcEast;
	    break;
	case kTpcId:
	    return mNumberOfPointsTpc;
	    break;
	case kSvtId:
	    return mNumberOfPointsSvt;
	    break;
	case kSsdId:
	case kSstId:
	    return mNumberOfPointsSsd;
	    break;
	case kIstId:
	    return mNumberOfPointsIst;
	    break;
	case kPxlId:
	    return mNumberOfPointsPxl;
	    break;
	default:
	    return mNumberOfPointsOth;
	}
    }
}

unsigned short
StTrackDetectorInfo::numberOfReferencedPoints() const
{
    return static_cast<unsigned short>(mHits.size());
}

unsigned short
StTrackDetectorInfo::numberOfReferencedPoints(StDetectorId id) const
{
    unsigned short count = 0;
    for (StPtrVecHitConstIterator iter=mHits.begin(); iter != mHits.end(); iter++)
        if ((*iter)->detector() == id) count++;
    return count;
}

StPtrVecHit
StTrackDetectorInfo::hits(StHitFilter& filter) const
{
    StPtrVecHit vec;
    for (StPtrVecHitConstIterator iter=mHits.begin(); iter != mHits.end(); iter++)
        if (filter(*iter)) vec.push_back(*iter);
    return vec;
}

StPtrVecHit
StTrackDetectorInfo::hits(StDetectorId id) const
{
    StPtrVecHit vec;
    for (StPtrVecHitConstIterator iter=mHits.begin(); iter != mHits.end(); iter++)
        if ((*iter)->detector() == id) vec.push_back(*iter);
    return vec;
}

StPtrVecHit&
StTrackDetectorInfo::hits() { return mHits; }

const StPtrVecHit&
StTrackDetectorInfo::hits() const { return mHits; }

void
StTrackDetectorInfo::setFirstPoint(const StThreeVectorF& val)
{
    mFirstPoint = val;
}

void
StTrackDetectorInfo::setLastPoint(const StThreeVectorF& val)
{
    mLastPoint = val;
}

void
StTrackDetectorInfo::setNumberOfPoints(unsigned short val)
{
    mNumberOfPoints = val;
}

void
StTrackDetectorInfo::setNumberOfPoints(unsigned char val, StDetectorId det)
{
    mNumberOfPoints = 0;  // make sure old method is NOT active
    switch (det) {
    case kFtpcWestId:
	mNumberOfPointsFtpcWest = val;
	break;
    case kFtpcEastId:
	mNumberOfPointsFtpcEast = val;
	break;
    case kTpcId:
	mNumberOfPointsTpc = val;
	break;
    case kSvtId:
	mNumberOfPointsSvt = val;
	break;
    case kSsdId:
    case kSstId:
	mNumberOfPointsSsd = val;
	break;
    case kIstId:
	mNumberOfPointsIst = val;
    case kPxlId:
	mNumberOfPointsPxl = val;
    default:
	mNumberOfPointsOth+= val;

	break;
    }
}

void
StTrackDetectorInfo::addHit(StHit* hit, bool increaseRefCounter) // 2nd arg is optional, defaults to true
{
    if (hit) {
        mHits.push_back(hit);
        if (increaseRefCounter) hit->setTrackReferenceCount(hit->trackReferenceCount()+1);
    }
}

void
StTrackDetectorInfo::removeHit(StHit*& hit)
{
    // carefull here: mHits.erase(&hit) is not save at all
    for (StPtrVecHitIterator iter=mHits.begin(); iter != mHits.end(); iter++) {
        if (*iter == hit) mHits.erase(iter);
        int i = hit->trackReferenceCount();
        hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
    }
}
int  StTrackDetectorInfo::bad() const
{
   int ierr;
   
   ierr = mFirstPoint.bad();
   if(ierr) return 1+100*ierr;
   ierr = mLastPoint.bad();
   if(ierr) return 2+100*ierr;
   if (fabs(mFirstPoint.z())>kStarMaxTrackRangeZ) return 21;
   if (fabs(mLastPoint.z ())>kStarMaxTrackRangeZ) return 22;
   if (mFirstPoint.perp  () >kStarMaxTrackRangeR) return 31;
   if (mLastPoint.perp   () >kStarMaxTrackRangeR) return 32;

   return 0;
}
