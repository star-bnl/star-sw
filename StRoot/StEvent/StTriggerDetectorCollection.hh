/***************************************************************************
 *
 * $Id: StTriggerDetectorCollection.hh,v 1.1 1999/01/15 20:40:17 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerDetectorCollection.hh,v $
 * Revision 1.1  1999/01/15 20:40:17  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#ifndef StTriggerDetectorCollection_hh
#define StTriggerDetectorCollection_hh
#include "StZdcSummary.hh"
#include "StVpdSummary.hh"
#include "StZdcSegment.hh"
#include "StVpdCounter.hh"
#include "StMwcSector.hh"
#include "StCtbCounter.hh"
#include "StVecCtbCounter.hh"
#include "StVecMwcSector.hh"
#include "StVecVpdCounter.hh"
#include "StVecZdcSegment.hh"

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
