/***************************************************************************
 *
 * $Id: StTriggerDetectorCollection.hh,v 1.2 1999/01/15 22:54:14 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerDetectorCollection.hh,v $
 * Revision 1.2  1999/01/15 22:54:14  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StTriggerDetectorCollection_hh
#define StTriggerDetectorCollection_hh
#include "StEvent/StZdcSummary.hh"
#include "StEvent/StVpdSummary.hh"
#include "StEvent/StZdcSegment.hh"
#include "StEvent/StVpdCounter.hh"
#include "StEvent/StMwcSector.hh"
#include "StEvent/StCtbCounter.hh"
#include "StEvent/StVecCtbCounter.hh"
#include "StEvent/StVecMwcSector.hh"
#include "StEvent/StVecVpdCounter.hh"
#include "StEvent/StVecZdcSegment.hh"

class StTriggerDetectorCollection {
public:
    StTriggerDetectorCollection();
    ~StTriggerDetectorCollection();
    // StTriggerDetectorCollection(const StTriggerDetectorCollection&);
    // const StTriggerDetectorCollection & operator=(const StTriggerDetectorCollection&);
    
    StVecCtbCounter& ctbCounters();
    StVecMwcSector&  mwcSectors();
    StVecVpdCounter& vpdCounters();
    StVecZdcSegment& zdcSegments();
    StZdcSummary&    zdcSummary();
    StVpdSummary&    vpdSummary();

protected:
    StVecCtbCounter mCtbCounters;
    StVecMwcSector  mMwcSectors;
    StVecVpdCounter mVpdCounters;
    StVecZdcSegment mZdcSegments;
    StVpdSummary    mVpdSummary;
    StZdcSummary    mZdcSummary;
};

inline StVecCtbCounter& StTriggerDetectorCollection::ctbCounters() { return mCtbCounters; }

inline StVecMwcSector&  StTriggerDetectorCollection::mwcSectors() { return mMwcSectors; }

inline StVecVpdCounter& StTriggerDetectorCollection::vpdCounters() { return mVpdCounters; }

inline StVecZdcSegment& StTriggerDetectorCollection::zdcSegments() { return mZdcSegments; }

inline StZdcSummary& StTriggerDetectorCollection::zdcSummary() { return mZdcSummary; }

inline StVpdSummary& StTriggerDetectorCollection::vpdSummary() { return mVpdSummary; }

#endif
