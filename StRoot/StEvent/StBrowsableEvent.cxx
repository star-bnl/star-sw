/***************************************************************************
 *
 * $Id: StBrowsableEvent.cxx,v 2.5 2000/02/23 17:35:52 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StBrowsableEvent.cxx,v $
 * Revision 2.5  2000/02/23 17:35:52  ullrich
 * Changes due to the addition of the EMC to StEvent
 *
 * Revision 2.5  2000/02/23 17:35:52  ullrich
 * Changes due to the addition of the EMC to StEvent
 *
 * Revision 2.4  2000/01/05 16:02:18  ullrich
 * SSD hits added to StEvent.
 *
 * Revision 2.3  1999/11/04 13:29:49  ullrich
 * Added constructor without summary table
 *
 * Revision 2.2  1999/10/28 22:24:50  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:44:24  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StBrowsableEvent.h"
#include "StEventSummary.h"
#include "StSoftwareMonitor.h"
#include "StTpcHitCollection.h"
#include "StFtpcHitCollection.h"
#include "StSvtHitCollection.h"
#include "StSsdHitCollection.h"
#include "StTriggerDetectorCollection.h"
#include "StL0Trigger.h"
#include "StContainers.h"
#include "StV0Vertex.h"
#include "StXiVertex.h"
#include "StKinkVertex.h"
#include "StTrackDetectorInfo.h"
#include "StTrackNode.h"
#include "StEmcCollection.h"
#include "StRichPixelCollection.h"

static const char rcsid[] = "$Id: StBrowsableEvent.cxx,v 2.5 2000/02/23 17:35:52 ullrich Exp $";

ClassImp(StBrowsableEvent)

StBrowsableEvent::StBrowsableEvent() { /* noop */ }
  
StBrowsableEvent::StBrowsableEvent(const event_header_st& evtHdr,
                                   const dst_event_summary_st& evtSum,
                                   const dst_summary_param_st& sumPar) :
StEvent(evtHdr, evtSum, sumPar) { /* noop */ }

StBrowsableEvent::StBrowsableEvent(const event_header_st& evtHdr) :
    StEvent(evtHdr) { /* noop */ }

StBrowsableEvent::~StBrowsableEvent() { /* noop */ }

void
StBrowsableEvent::browse(TBrowser *b)
{
    if (b) {
        if (mSummary)          b->Add(mSummary);
        if (mSoftwareMonitor)  b->Add(mSoftwareMonitor);
        if (mTpcHits)          b->Add(mTpcHits);
        if (mFtpcHits)         b->Add(mFtpcHits);
        if (mSvtHits)          b->Add(mSvtHits);
        if (mSsdHits)          b->Add(mSsdHits);
        if (mTriggerDetectors) b->Add(mTriggerDetectors);
        if (mL0Trigger)        b->Add(mL0Trigger);
        if (mEmcCollection)    b->Add(mEmcCollection);
        if (mRichPixels)       b->Add(mRichPixels);
        b->Add(&mTrackDetectorInfo);
        b->Add(&mTrackNodes);
        b->Add(&mPrimaryVertices);
        b->Add(&mV0Vertices);
        b->Add(&mXiVertices);
        b->Add(&mKinkVertices);
    }
    St_DataSet::Browse(b);
}
