/***************************************************************************
 *
 * $Id: StTriggerDetectorCollection.h,v 1.3 1999/04/27 01:24:29 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerDetectorCollection.h,v $
 * Revision 1.3  1999/04/27 01:24:29  fisyak
 * Fix intermidaiate version with pointer instead of referencies
 *
 * Revision 1.4  1999/04/28 22:27:39  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:54:14  wenaus
 * version with constructors for table-based loading
 *
#ifdef __ROOT__
#include "TObject.h"
#endif
#ifndef StTriggerDetectorCollection_hh
#define StTriggerDetectorCollection_hh
#include "StObject.h"
#include "StZdcSummary.h"
#include "StVpdSummary.h"
#include "StZdcSegment.h"
#include "StCtbCounter.h"
#include "StMwcSector.h"
class StTriggerDetectorCollection : public TObject {
#include "StZdcSegment.h"

class StTriggerDetectorCollection : public StObject {
public:
    StTriggerDetectorCollection();
    ~StTriggerDetectorCollection();
    StVecCtbCounter& ctbCounters();
    StVecMwcSector&  mwcSectors();
    StVecVpdCounter& vpdCounters();
    StVecZdcSegment& zdcSegments();
    StVecPtrMwcSector&  mwcSectors();
    StVecPtrVpdCounter& vpdCounters();
    StVecPtrZdcSegment& zdcSegments();
    StZdcSummary&    zdcSummary();
    StVecCtbCounter mCtbCounters;//!
    StVecMwcSector  mMwcSectors; //!
    StVecVpdCounter mVpdCounters;//!
    StVecZdcSegment mZdcSegments;//!
    StVpdSummary    mVpdSummary;
    StZdcSummary    mZdcSummary;
#ifdef __ROOT__
	ClassDef(StTriggerDetectorCollection,1)  //StTriggerDetectorCollection structure
#endif
    StVpdSummary       *mVpdSummary;
    StZdcSummary       *mZdcSummary;
inline StVecCtbCounter& StTriggerDetectorCollection::ctbCounters() { return mCtbCounters; }
};
inline StVecMwcSector&  StTriggerDetectorCollection::mwcSectors() { return mMwcSectors; }
inline StVecPtrCtbCounter& StTriggerDetectorCollection::ctbCounters() { return *mCtbCounters; }
inline StVecVpdCounter& StTriggerDetectorCollection::vpdCounters() { return mVpdCounters; }
inline StVecPtrMwcSector&  StTriggerDetectorCollection::mwcSectors() { return *mMwcSectors; }
inline StVecZdcSegment& StTriggerDetectorCollection::zdcSegments() { return mZdcSegments; }
inline StVecPtrVpdCounter& StTriggerDetectorCollection::vpdCounters() { return *mVpdCounters; }
inline StZdcSummary& StTriggerDetectorCollection::zdcSummary() { return mZdcSummary; }
inline StVecPtrZdcSegment& StTriggerDetectorCollection::zdcSegments() { return *mZdcSegments; }
inline StVpdSummary& StTriggerDetectorCollection::vpdSummary() { return mVpdSummary; }
inline StZdcSummary& StTriggerDetectorCollection::zdcSummary() { return *mZdcSummary; }

inline StVpdSummary& StTriggerDetectorCollection::vpdSummary() { return *mVpdSummary; }

#endif
