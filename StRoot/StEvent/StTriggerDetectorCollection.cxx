/***************************************************************************
 *
 * $Id: StTriggerDetectorCollection.cxx,v 1.5 1999/06/24 17:33:00 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerDetectorCollection.cxx,v $
 * Revision 1.5  1999/06/24 17:33:00  fisyak
 * Replace Collection by value to Collection by pointer for TBrowser
 *
 * Revision 1.5  1999/06/24 17:33:00  fisyak
 * Replace Collection by value to Collection by pointer for TBrowser
 *
 * Revision 1.4  1999/04/28 22:27:38  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:54:13  wenaus
 * version with constructors for table-based loading
 *
 * Revision 2.0  1999/10/12 18:43:18  ullrich
 * Completely Revised for New Version
 **************************************************************************/
static const Char_t rcsid[] = "$Id: StTriggerDetectorCollection.cxx,v 1.5 1999/06/24 17:33:00 fisyak Exp $";
#include "tables/dst_TrgDet.h"

static const char rcsid[] = "$Id: StTriggerDetectorCollection.cxx,v 1.5 1999/06/24 17:33:00 fisyak Exp $";
StTriggerDetectorCollection::StTriggerDetectorCollection():
mCtbCounters(new StVecPtrCtbCounter),
mMwcSectors(new StVecPtrMwcSector),
mVpdCounters(new StVecPtrVpdCounter),
mZdcSegments(new StVecPtrZdcSegment),
mVpdSummary(new StVpdSummary),
mZdcSummary(new StZdcSummary)
{ /* noop */ };

StTriggerDetectorCollection::~StTriggerDetectorCollection() {
  SafeDelete(mCtbCounters);
  SafeDelete(mMwcSectors);
  SafeDelete(mVpdCounters);
  SafeDelete(mZdcSegments);
  SafeDelete(mVpdSummary);
  SafeDelete(mZdcSummary);
};
const StZdcTriggerDetector&
StTriggerDetectorCollection::zdc() const { return mZdc; }

