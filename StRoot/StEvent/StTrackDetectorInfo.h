/*!
 * \class StTrackDetectorInfo 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StTrackDetectorInfo.h,v 2.10 2004/08/05 19:25:03 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackDetectorInfo.h,v $
 * Revision 2.10  2004/08/05 19:25:03  ullrich
 * Changes to the handling of numberOfPoints() to allow ITTF more flexibility.
 *
 * Revision 2.9  2004/07/15 16:36:26  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.8  2003/10/30 20:07:32  perev
 * Check of quality added
 *
 * Revision 2.7  2002/02/22 22:56:52  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.6  2001/04/05 04:00:45  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.5  2001/03/24 03:35:00  perev
 * clone() -> clone() const
 *
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

    unsigned short        numberOfPoints() const;
    unsigned short        numberOfPoints(StDetectorId) const;
    			 
    unsigned short        numberOfReferencedPoints() const;
    unsigned short        numberOfReferencedPoints(StDetectorId) const;
    
    StPtrVecHit           hits(StDetectorId) const;
    StPtrVecHit           hits(StHitFilter&) const;
    StPtrVecHit&          hits();
    const StPtrVecHit&    hits() const;

    void setFirstPoint(const StThreeVectorF&);
    void setLastPoint(const StThreeVectorF&);
    void setNumberOfPoints(unsigned short, StDetectorId);
    void addHit(StHit*);
    void removeHit(StHit*&);
    int  bad() const;
    
protected:
    void setNumberOfPoints(unsigned short);  // obsolete since ITTF
    
private:
    StThreeVectorF mFirstPoint;
    StThreeVectorF mLastPoint;
    UShort_t       mNumberOfPoints; // obsolete since ITTF
    UChar_t        mNumberOfPointsTpc;
    UChar_t        mNumberOfPointsFtpcWest;
    UChar_t        mNumberOfPointsFtpcEast;
    UChar_t        mNumberOfPointsSvt;
    UChar_t        mNumberOfPointsSsd;
    StPtrVecHit    mHits;

    ClassDef(StTrackDetectorInfo,2)
};

#endif
