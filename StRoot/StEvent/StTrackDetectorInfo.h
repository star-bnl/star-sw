/***************************************************************************
 *
 * $Id: StTrackDetectorInfo.h,v 2.4 2000/04/20 13:30:02 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackDetectorInfo.h,v $
 * Revision 2.4  2000/04/20 13:30:02  ullrich
 * Added new methods and removed inconsistencies in numberOfPoints().
 *
 * Revision 2.3  1999/11/01 12:45:12  ullrich
 * Modified unpacking of point counter
 *
 * Revision 2.2  1999/10/28 22:27:30  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:44:11  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTrackDetectorInfo_hh
#define StTrackDetectorInfo_hh
#include "StContainers.h"
#include "StObject.h"
#include "StThreeVectorF.hh"
#include "StEnumerations.h"

class StHitFilter;
class dst_track_st;
class StHit;

class StTrackDetectorInfo : public StObject {
public:
    StTrackDetectorInfo();
    StTrackDetectorInfo(const dst_track_st&);
    // StTrackDetectorInfo(const StTrackDetectorInfo&);             use default
    // StTrackDetectorInfo & operator=(const StTrackDetectorInfo&); use default
    virtual ~StTrackDetectorInfo();

    const StThreeVectorF& firstPoint() const;
    const StThreeVectorF& lastPoint() const;

    UShort_t              numberOfPoints() const;
    UShort_t              numberOfPoints(StDetectorId) const;
    
    UShort_t              numberOfReferencedPoints() const;
    UShort_t              numberOfReferencedPoints(StDetectorId) const;
    
    StPtrVecHit           hits(StDetectorId) const;
    StPtrVecHit           hits(StHitFilter&) const;
    StPtrVecHit&          hits();
    const StPtrVecHit&    hits() const;

    void setFirstPoint(const StThreeVectorF&);
    void setLastPoint(const StThreeVectorF&);
    void setNumberOfPoints(UShort_t);
    void addHit(StHit*);
    void removeHit(StHit*&);

protected:
    virtual StObject* clone();
    
private:
    StThreeVectorF mFirstPoint;
    StThreeVectorF mLastPoint;
    UShort_t       mNumberOfPoints;
    StPtrVecHit    mHits;

    ClassDef(StTrackDetectorInfo,1)
};

#endif
