/***************************************************************************
 *
 * $Id: StTriggerDetectorCollection.cxx,v 1.1 1999/01/30 03:58:09 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerDetectorCollection.cxx,v $
 * Revision 1.1  1999/01/30 03:58:09  fisyak
 * Root Version of StEvent
 *
 * Revision 1.4  1999/04/28 22:27:38  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:54:13  wenaus
 * version with constructors for table-based loading
 *
static const Char_t rcsid[] = "$Id: StTriggerDetectorCollection.cxx,v 1.1 1999/01/30 03:58:09 fisyak Exp $";
 * Completely Revised for New Version
#ifdef __ROOT__
 **************************************************************************/
static const Char_t rcsid[] = "$Id: StTriggerDetectorCollection.cxx,v 1.1 1999/01/30 03:58:09 fisyak Exp $";
#endif
StTriggerDetectorCollection::StTriggerDetectorCollection() { /* noop */ };
mVpdSummary(new StVpdSummary),
StTriggerDetectorCollection::~StTriggerDetectorCollection() { /* noop */ };
  SafeDelete(mVpdSummary);
  SafeDelete(mZdcSummary);
};
const StZdcTriggerDetector&
StTriggerDetectorCollection::zdc() const { return mZdc; }

