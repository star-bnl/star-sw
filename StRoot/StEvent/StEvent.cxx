/***************************************************************************
 *
 * $Id: StEvent.cxx,v 2.14 2000/06/19 01:32:15 perev Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 * Do not touch anything here unless you really know what you are doing.
 *
 ***************************************************************************
 *
 * $Log: StEvent.cxx,v $
 * Revision 2.14  2000/06/19 01:32:15  perev
 * Thomas StEvent branches added
 *
 * Revision 2.16  2000/09/25 14:21:27  ullrich
 *  Thomas StEvent branches added
 *
 * Revision 2.15  2000/09/06 22:34:12  ullrich
 * Changed mBunchCrossingNumber from scalar to array to hold all 64 bits.
 *
 * Revision 2.14  2000/06/19 01:32:15  perev
 * Thomas StEvent branches added
 *
 * Revision 2.13  2000/05/24 15:46:05  ullrich
 * Added setSummary() method.
 *
 * Revision 2.12  2000/05/22 21:47:12  ullrich
 * Added RICH collection and related methods.
 *
 * Revision 2.11  2000/05/15 18:35:38  ullrich
 * All data member related to collections and containers are now
 * kept by pointer. The interface (public methods) stays the same.
 * Those methods which returns references were modified to create
 * an empty collection in case the pointer is null.
 *
 * Revision 2.10  2000/04/26 20:33:24  ullrich
 * Removed redundant virtual keywords.
 *
 * Revision 2.9  2000/04/20 14:27:29  perev
 * Add Dataset browser to StEvent browser
 *
 * Revision 2.8  2000/04/18 17:31:28  perev
 * StEvent::Browse overload of TDataSet:;One
 *
 * Revision 2.7  2000/03/29 16:54:11  ullrich
 * Added L3 trigger.
 *
 * Revision 2.6  2000/02/23 17:35:59  ullrich
 * Changes due to the addition of the EMC to StEvent
 *
 * Revision 2.5  2000/02/11 16:14:00  ullrich
 * Primary vertices automatically sorted in addPrimaryVertex().
 *
 * Revision 2.4  2000/01/13 21:06:22  lasiuk
 * add rich pixel info/containers
 *
 * Revision 2.3  2000/01/05 16:02:25  ullrich
 * SSD hits added to StEvent.
 *
 * Revision 2.2  1999/11/04 13:30:40  ullrich
 * Added constructor without summary table
 *
 * Revision 2.1  1999/10/28 22:25:07  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:41:53  ullrich
 *
 **************************************************************************/
#include <typeinfo>
#include <algorithm>
#include "StEvent.h"
#include "StEventInfo.h"
#include "StEventSummary.h"
#include "StSoftwareMonitor.h"
#include "StTpcHitCollection.h"
#include "StSvtHitCollection.h"
#include "StSsdHitCollection.h"
#include "StFtpcHitCollection.h"
#include "StEmcCollection.h"
#include "StRichCollection.h"
#include "StTrackDetectorInfo.h"
#include "StTriggerDetectorCollection.h"
#include "StPrimaryVertex.h"
#include "StL0Trigger.h"
#include "StL3Trigger.h"
#include "tables/St_event_header_Table.h"
#include "tables/St_dst_event_summary_Table.h"
#include "tables/St_dst_summary_param_Table.h"
#include "StAutoBrowse.h"
#ifndef ST_NO_NAMESPACES
using std::swap;
#endif

TString StEvent::mCvsTag  = "$Id: StEvent.cxx,v 2.14 2000/06/19 01:32:15 perev Exp $";
static const char rcsid[] = "$Id: StEvent.cxx,v 2.14 2000/06/19 01:32:15 perev Exp $";
void
StEvent::initToZero()

    mContent.resize(mContentLength);
    for (int i=0; i<mContentLength;i++) mContent[i]=0;
	    return;
	}    
    if (val) vec.push_back(val);
StEvent::initToZero() { /* noop */ }

    mContent[mInfo] = new StEventInfo(evtHdr);
StEvent::init(const event_header_st& evtHdr)
{
    mContent.push_back(new StEventInfo(evtHdr));
}

StEvent::StEvent() : St_DataSet("StEvent")
{
    initToZero();
}
  
StEvent::StEvent(const event_header_st& evtHdr,
                 const dst_event_summary_st& evtSum,
                 const dst_summary_param_st& sumPar) :
    St_DataSet("StEvent")
    mContent[mSummary] = new StEventSummary(evtSum, sumPar);
    initToZero();
    init(evtHdr);
    mContent.push_back(new StEventSummary(evtSum, sumPar));
}

StEvent::StEvent(const event_header_st& evtHdr) :
    St_DataSet("StEvent")
{
    initToZero();
    init(evtHdr);
{
}

StEvent::~StEvent()
{ /* noop */ }

    return mContent[mInfo] ? static_cast<StEventInfo*>(mContent[mInfo])->type() : TString();
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info ? info->type() : TString();
}

    return mContent[mInfo] ? static_cast<StEventInfo*>(mContent[mInfo])->id() : 0;
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info ? info->id() : 0;
}

    return mContent[mInfo] ? static_cast<StEventInfo*>(mContent[mInfo])->runId() : 0;
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info ? info->runId() : 0;
}

    return mContent[mInfo] ? static_cast<StEventInfo*>(mContent[mInfo])->time() : 0;
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info ? info->time() : 0;
}

    return mContent[mInfo] ? static_cast<StEventInfo*>(mContent[mInfo])->triggerMask() : 0;
StEvent::bunchCrossingNumber() const  
    _lookup(info, mContent);
    return mContent[mInfo] ? static_cast<StEventInfo*>(mContent[mInfo])->bunchCrossingNumber() : 0;
}

    return mContent[mInfo] ? static_cast<StEventInfo*>(mContent[mInfo])->bunchCrossingNumber(i) : 0;
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info ? info->bunchCrossingNumber(i) : 0;
}

    return static_cast<StEventInfo*>(mContent[mInfo]);
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info;
}

    return static_cast<StEventInfo*>(mContent[mInfo]);
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info;
}

    return static_cast<StEventSummary*>(mContent[mSummary]);
    StEventSummary* summary = 0;
    _lookup(summary, mContent);
    return summary;
}

    return static_cast<StEventSummary*>(mContent[mSummary]);
    StEventSummary* summary = 0;
    _lookup(summary, mContent);
    return summary;
}

const TString&
StEvent::cvsTag() { return mCvsTag; }

    return static_cast<StSoftwareMonitor*>(mContent[mSoftwareMonitor]);
    StSoftwareMonitor *monitor = 0;
    _lookup(monitor, mContent);
    return monitor;
}

    return static_cast<StSoftwareMonitor*>(mContent[mSoftwareMonitor]);
    StSoftwareMonitor *monitor = 0;
    _lookup(monitor, mContent);
    return monitor;
}

    return static_cast<StTpcHitCollection*>(mContent[mTpcHits]);
    StTpcHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

    return static_cast<StTpcHitCollection*>(mContent[mTpcHits]);
    StTpcHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

    return static_cast<StFtpcHitCollection*>(mContent[mFtpcHits]);
    StFtpcHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

    return static_cast<StFtpcHitCollection*>(mContent[mFtpcHits]);
    StFtpcHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

    return static_cast<StSvtHitCollection*>(mContent[mSvtHits]);
    StSvtHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

    return static_cast<StSvtHitCollection*>(mContent[mSvtHits]);
    StSvtHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

    return static_cast<StSsdHitCollection*>(mContent[mSsdHits]);
    StSsdHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

    return static_cast<StSsdHitCollection*>(mContent[mSsdHits]);
    StSsdHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

    return static_cast<StEmcCollection*>(mContent[mEmcCollection]);
    StEmcCollection *emc = 0;
    _lookup(emc, mContent);
    return emc;
}

    return static_cast<StEmcCollection*>(mContent[mEmcCollection]);
    StEmcCollection *emc = 0;
    _lookup(emc, mContent);
    return emc;
}

    return static_cast<StRichCollection*>(mContent[mRichCollection]);
    StRichCollection *rich = 0;
    _lookup(rich, mContent);
    return rich;
}

    return static_cast<StRichCollection*>(mContent[mRichCollection]);
    StRichCollection *rich = 0;
    _lookup(rich, mContent);
    return rich;
}

    return static_cast<StTriggerDetectorCollection*>(mContent[mTriggerDetectors]);
    StTriggerDetectorCollection *trg = 0;
    _lookup(trg, mContent);
    return trg;
}

    return static_cast<StTriggerDetectorCollection*>(mContent[mTriggerDetectors]);
    StTriggerDetectorCollection *trg = 0;
    _lookup(trg, mContent);
    return trg;
}

    return static_cast<StL0Trigger*>(mContent[mL0Trigger]);
    StL0Trigger *trg = 0;
    _lookup(trg, mContent);
    return trg;
}

    return static_cast<StL0Trigger*>(mContent[mL0Trigger]);
    StL0Trigger *trg = 0;
    _lookup(trg, mContent);
    return trg;
}

    return static_cast<StL3Trigger*>(mContent[mL3Trigger]);
    StL3Trigger *trg = 0;
    _lookup(trg, mContent);
    return trg;
}

    return static_cast<StL3Trigger*>(mContent[mL3Trigger]);
    StL3Trigger *trg = 0;
    _lookup(trg, mContent);
}


    if (!mContent[mTrackDetectorInfo])
        mContent[mTrackDetectorInfo] = new StSPtrVecTrackDetectorInfo;
    return *static_cast<StSPtrVecTrackDetectorInfo*>(mContent[mTrackDetectorInfo]);
    StSPtrVecTrackDetectorInfo *info = 0;
    _lookupOrCreate(info, mContent);
    return *info;
}

    if (!mContent[mTrackDetectorInfo])
        mContent[mTrackDetectorInfo] = new StSPtrVecTrackDetectorInfo;
    return *static_cast<StSPtrVecTrackDetectorInfo*>(mContent[mTrackDetectorInfo]);
    StSPtrVecTrackDetectorInfo *info = 0;
    _lookupOrCreate(info, mContent);
    return *info;
}

    if (!mContent[mTrackNodes])
        mContent[mTrackNodes] = new StSPtrVecTrackNode;
    return *static_cast<StSPtrVecTrackNode*>(mContent[mTrackNodes]);
    StSPtrVecTrackNode *nodes = 0;
    _lookupOrCreate(nodes, mContent);
    return *nodes;
}

    if (!mContent[mTrackNodes])
        mContent[mTrackNodes] = new StSPtrVecTrackNode;
    return *static_cast<StSPtrVecTrackNode*>(mContent[mTrackNodes]);
    StSPtrVecTrackNode *nodes = 0;
    _lookupOrCreate(nodes, mContent);
    return *nodes;
}

    return mContent[mPrimaryVertices] ? static_cast<StSPtrVecPrimaryVertex*>(mContent[mPrimaryVertices])->size() : 0;
    StSPtrVecPrimaryVertex *vertices = 0;
    _lookupOrCreate(vertices, mContent);
    return vertices ? vertices->size() : 0;
}

    if (mContent[mPrimaryVertices] && i < static_cast<StSPtrVecPrimaryVertex*>(mContent[mPrimaryVertices])->size())
        return (*static_cast<StSPtrVecPrimaryVertex*>(mContent[mPrimaryVertices]))[i];
    _lookup(vertices, mContent);
    if (vertices && i < vertices->size())
        return (*vertices)[i];
    else
        return 0;
}

    if (mContent[mPrimaryVertices] && i < static_cast<StSPtrVecPrimaryVertex*>(mContent[mPrimaryVertices])->size())
        return (*static_cast<StSPtrVecPrimaryVertex*>(mContent[mPrimaryVertices]))[i];
    _lookup(vertices, mContent);
    if (vertices && i < vertices->size())
        return (*vertices)[i];
    else
        return 0;
}

    if (!mContent[mV0Vertices])
        mContent[mV0Vertices] = new StSPtrVecV0Vertex;
    return *static_cast<StSPtrVecV0Vertex*>(mContent[mV0Vertices]);
    StSPtrVecV0Vertex *vertices = 0;
    _lookupOrCreate(vertices, mContent);
    return *vertices;
}

    if (!mContent[mV0Vertices])
        mContent[mV0Vertices] = new StSPtrVecV0Vertex;
    return *static_cast<StSPtrVecV0Vertex*>(mContent[mV0Vertices]);
    StSPtrVecV0Vertex *vertices = 0;
    _lookupOrCreate(vertices, mContent);
    return *vertices;
}

    if (!mContent[mXiVertices])
        mContent[mXiVertices] = new StSPtrVecXiVertex;
    return *static_cast<StSPtrVecXiVertex*>(mContent[mXiVertices]);
    StSPtrVecXiVertex *vertices = 0;
    _lookupOrCreate(vertices, mContent);
    return *vertices;
}

    if (!mContent[mXiVertices])
        mContent[mXiVertices] = new StSPtrVecXiVertex;
    return *static_cast<StSPtrVecXiVertex*>(mContent[mXiVertices]);
    StSPtrVecXiVertex *vertices = 0;
    _lookupOrCreate(vertices, mContent);
    return *vertices;
}

    if (!mContent[mKinkVertices])
        mContent[mKinkVertices] = new StSPtrVecKinkVertex;
    return *static_cast<StSPtrVecKinkVertex*>(mContent[mKinkVertices]);
    StSPtrVecKinkVertex *vertices = 0;
    _lookupOrCreate(vertices, mContent);
    return *vertices;
}

    if (!mContent[mKinkVertices])
        mContent[mKinkVertices] = new StSPtrVecKinkVertex;
    return *static_cast<StSPtrVecKinkVertex*>(mContent[mKinkVertices]);
    StSPtrVecKinkVertex *vertices = 0;
    _lookupOrCreate(vertices, mContent);
    return *vertices;
}

StSPtrVecObject&
StEvent::content() { return mContent; }

    if (!mContent[mInfo]) mContent[mInfo] = new StEventInfo;
    static_cast<StEventInfo*>(mContent[mInfo])->setType(val);
    StEventInfo* info = 0;
    _lookupOrCreate(info, mContent);
    info->setType(val);
}

    if (!mContent[mInfo]) mContent[mInfo] = new StEventInfo;
    static_cast<StEventInfo*>(mContent[mInfo])->setRunId(val);
    StEventInfo* info = 0;
    _lookupOrCreate(info, mContent);
    info->setRunId(val);
}

    if (!mContent[mInfo]) mContent[mInfo] = new StEventInfo;
    static_cast<StEventInfo*>(mContent[mInfo])->setId(val);
    StEventInfo* info = 0;
    _lookupOrCreate(info, mContent);
    info->setId(val);
}

    if (!mContent[mInfo]) mContent[mInfo] = new StEventInfo;
    static_cast<StEventInfo*>(mContent[mInfo])->setTime(val);
    StEventInfo* info = 0;
    _lookupOrCreate(info, mContent);
    info->setTime(val);
}

    if (!mContent[mInfo]) mContent[mInfo] = new StEventInfo;
    static_cast<StEventInfo*>(mContent[mInfo])->setTriggerMask(val);
StEvent::setBunchCrossingNumber(ULong_t val)
    _lookupOrCreate(info, mContent);
    info->setTriggerMask(val);
    static_cast<StEventInfo*>(mContent[mInfo])->setBunchCrossingNumber(val);

    if (!mContent[mInfo]) mContent[mInfo] = new StEventInfo;
    static_cast<StEventInfo*>(mContent[mInfo])->setBunchCrossingNumber(val, i);
    StEventInfo* info = 0;
    _lookupOrCreate(info, mContent);
    info->setBunchCrossingNumber(val, i);
}

    if (mContent[mInfo]) delete mContent[mInfo];
    mContent[mInfo] = val;
StEvent::setInfo(StEventInfo* val)
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mSummary]) delete mContent[mSummary];
    mContent[mSummary] = val;
StEvent::setSummary(StEventSummary* val)
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mSoftwareMonitor]) delete mContent[mSoftwareMonitor];
    mContent[mSoftwareMonitor] = val;
StEvent::setSoftwareMonitor(StSoftwareMonitor* val)
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mTpcHits]) delete mContent[mTpcHits];
    mContent[mTpcHits] = val;
StEvent::setTpcHitCollection(StTpcHitCollection* val)
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mFtpcHits]) delete mContent[mFtpcHits];
    mContent[mFtpcHits] = val;
StEvent::setFtpcHitCollection(StFtpcHitCollection* val)
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mSvtHits]) delete mContent[mSvtHits];
    mContent[mSvtHits] = val;
StEvent::setSvtHitCollection(StSvtHitCollection* val)
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mSsdHits]) delete mContent[mSsdHits];
    mContent[mSsdHits] = val;
StEvent::setSsdHitCollection(StSsdHitCollection* val)
{
    _lookupAndSet(val, mContent);
}


    if (mContent[mEmcCollection]) delete mContent[mEmcCollection];
    mContent[mEmcCollection] = val;
StEvent::setEmcCollection(StEmcCollection* val)
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mRichCollection]) delete mContent[mRichCollection];
    mContent[mRichCollection] = val;
StEvent::setRichCollection(StRichCollection* val)
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mTriggerDetectors]) delete mContent[mTriggerDetectors];
    mContent[mTriggerDetectors] = val;
StEvent::setTriggerDetectorCollection(StTriggerDetectorCollection* val)
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mL0Trigger]) delete mContent[mL0Trigger];
    mContent[mL0Trigger] = val;
StEvent::setL0Trigger(StL0Trigger* val)
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mL3Trigger]) delete mContent[mL3Trigger];
    mContent[mL3Trigger] = val;
StEvent::setL3Trigger(StL3Trigger* val)
{
    _lookupAndSet(val, mContent);
}

void
        if (!mContent[mPrimaryVertices])
            mContent[mPrimaryVertices] = new StSPtrVecPrimaryVertex;

	StSPtrVecPrimaryVertex* vertexVector =
	    static_cast<StSPtrVecPrimaryVertex*>(mContent[mPrimaryVertices]);
    if (vertex) {
	StSPtrVecPrimaryVertex* vertexVector = 0;
	_lookupOrCreate(vertexVector, mContent);
	vertexVector->push_back(vertex);
        
        //
        //  Sort new entry.
        //  Vertices are ordered according to number
        //  of daughter tracks in descending order.
        //
        for (int i=vertexVector->size()-1; i>0; i--) {
            if ((*vertexVector)[i]->numberOfDaughters() >
		(*vertexVector)[i-1]->numberOfDaughters())
                swap((*vertexVector)[i], (*vertexVector)[i-1]);
            else
                break;
        }
    }
}

void StEvent::Browse(TBrowser* b)
{
    StAutoBrowse::Browse(this,b);
    TDataSet::Browse(b);
}
