/***************************************************************************
 *
 * $Id: StEvent.cxx,v 2.11 2000/05/15 18:35:38 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEvent.cxx,v $
 * Revision 2.11  2000/05/15 18:35:38  ullrich
 * All data member related to collections and containers are now
 * kept by pointer. The interface (public methods) stays the same.
 * Those methods which returns references were modified to create
 * an empty collection in case the pointer is null.
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
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:41:53  ullrich
 *
#include "StRichPixelCollection.h"
#include <typeinfo>
#include <algorithm>
#include "StEvent.h"
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
TString StEvent::mCvsTag  = "$Id: StEvent.cxx,v 2.11 2000/05/15 18:35:38 ullrich Exp $";
static const char rcsid[] = "$Id: StEvent.cxx,v 2.11 2000/05/15 18:35:38 ullrich Exp $";
#include "tables/St_dst_event_summary_Table.h"
#include "tables/St_dst_summary_param_Table.h"
#include "StAutoBrowse.h"
#ifndef ST_NO_NAMESPACES
using std::swap;
#endif
    mRunId = 0;
    mId = 0;
    mTime = 0;
    mTriggerMask = 0;
    mBunchCrossingNumber = 0;
    mSummary = 0;
    mSoftwareMonitor = 0;
    mTpcHits = 0;
    mFtpcHits = 0;
    mRichPixels = 0;
    mSsdHits = 0;
    mEmcCollection = 0;
    mRichCollection = 0;
    mTriggerDetectors = 0;
    mL0Trigger = 0;
    mL3Trigger = 0;
    mTrackDetectorInfo = 0;
    mTrackNodes = 0;
    mPrimaryVertices = 0;
    mV0Vertices = 0;
    mXiVertices = 0;
    mKinkVertices = 0;
static const char rcsid[] = "$Id: StEvent.cxx,v 2.11 2000/05/15 18:35:38 ullrich Exp $";
void
StEvent::initToZero()

    mContent.resize(mContentLength);
    mType  = evtHdr.event_type;
    mRunId = evtHdr.exp_run_id;
    mId    = evtHdr.n_event;
    mTime  = evtHdr.time;
    mTriggerMask = evtHdr.trig_mask;
    mBunchCrossingNumber = evtHdr.bunch_cross;
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
    mSummary = new StEventSummary(evtSum, sumPar);
  
StEvent::StEvent(const event_header_st& evtHdr,
                 const dst_event_summary_st& evtSum,
                 const dst_summary_param_st& sumPar) :
    St_DataSet("StEvent")
    mContent[mSummary] = new StEventSummary(evtSum, sumPar);
    initToZero();
    init(evtHdr);
    mContent.push_back(new StEventSummary(evtSum, sumPar));
}

    delete mSummary;           mSummary = 0;
    delete mSoftwareMonitor;   mSoftwareMonitor = 0;
    delete mTpcHits;           mTpcHits = 0;
    delete mRichPixels;        mRichPixels = 0;
    delete mSvtHits;           mSvtHits = 0;
    delete mSsdHits;           mSsdHits = 0;
    delete mRichCollection;    mRichCollection = 0;
    delete mTriggerDetectors;  mTriggerDetectors = 0;
    delete mL0Trigger;         mL0Trigger = 0;
    delete mL3Trigger;         mL3Trigger = 0;
    delete mTrackDetectorInfo; mTrackDetectorInfo = 0;
    delete mTrackNodes;        mTrackNodes = 0;
    delete mPrimaryVertices;   mPrimaryVertices = 0;
    delete mV0Vertices;        mV0Vertices = 0;
    delete mXiVertices;        mXiVertices = 0;
    delete mKinkVertices;      mKinkVertices = 0;
StEvent::StEvent(const event_header_st& evtHdr) :
    St_DataSet("StEvent")
const TString&
StEvent::type() const { return mType; }

StEvent::~StEvent()
StEvent::id() const { return mId; }
    _lookup(info, mContent);
    return info ? info->type() : TString();
StEvent::runId() const { return mRunId; }
    _lookup(info, mContent);
    return info ? info->id() : 0;
StEvent::time() const { return mTime; }
    _lookup(info, mContent);
    return info ? info->runId() : 0;
StEvent::triggerMask() const { return mTriggerMask; }
    _lookup(info, mContent);
    return info ? info->time() : 0;
StEvent::bunchCrossingNumber() const { return mBunchCrossingNumber; }
    _lookup(info, mContent);
    return info;
StEvent::summary() { return mSummary; }
    _lookup(info, mContent);
    return info;
StEvent::summary() const { return mSummary; }
    _lookup(summary, mContent);
    return summary;
}

    return static_cast<StEventSummary*>(mContent[mSummary]);
StEvent::softwareMonitor() { return mSoftwareMonitor; }

const TString&
StEvent::softwareMonitor() const { return mSoftwareMonitor; }
    _lookup(monitor, mContent);
    return monitor;
StEvent::tpcHitCollection() { return mTpcHits; }
    _lookup(monitor, mContent);
    return monitor;
StEvent::tpcHitCollection() const { return mTpcHits; }
    _lookup(hits, mContent);
    return hits;
StEvent::ftpcHitCollection() { return mFtpcHits; }
    _lookup(hits, mContent);
    return hits;
StEvent::ftpcHitCollection() const { return mFtpcHits; }
    _lookup(hits, mContent);
    return hits;
StEvent::svtHitCollection() { return mSvtHits; }
    _lookup(hits, mContent);
    return hits;
StEvent::svtHitCollection() const { return mSvtHits; }
    _lookup(hits, mContent);
    return hits;
StEvent::ssdHitCollection() { return mSsdHits; }
    _lookup(hits, mContent);
    return hits;
StEvent::ssdHitCollection() const { return mSsdHits; }
    _lookup(hits, mContent);
    return hits;
StEvent::emcCollection() { return mEmcCollection; }
    _lookup(hits, mContent);
StRichPixelCollection*
StEvent::richPixelCollection() { return mRichPixels; }
    _lookup(emc, mContent);
const StRichPixelCollection*
StEvent::richPixelCollection() const { return mRichPixels; }
    _lookup(emc, mContent);
    return emc;
StEvent::richCollection() const { return mRichCollection; }
    _lookup(rich, mContent);
    return rich;
StEvent::triggerDetectorCollection() { return mTriggerDetectors; }
    _lookup(rich, mContent);
    return rich;
StEvent::triggerDetectorCollection() const { return mTriggerDetectors; }
    _lookup(trg, mContent);
    return trg;
StEvent::l0Trigger() { return mL0Trigger; }
    _lookup(trg, mContent);
    return trg;
StEvent::l0Trigger() const { return mL0Trigger; }
    _lookup(trg, mContent);
    return trg;
StEvent::l3Trigger() { return mL3Trigger; }
    _lookup(trg, mContent);
    return trg;
StEvent::l3Trigger() const { return mL3Trigger; }
    _lookup(trg, mContent);
    return trg;
}

    if (!mTrackDetectorInfo)
        mTrackDetectorInfo = new StSPtrVecTrackDetectorInfo;
    return *mTrackDetectorInfo;
}


    if (!mContent[mTrackDetectorInfo])
        mContent[mTrackDetectorInfo] = new StSPtrVecTrackDetectorInfo;
    if (!mTrackDetectorInfo)
        mTrackDetectorInfo = new StSPtrVecTrackDetectorInfo;
    return *mTrackDetectorInfo;
    return *info;
}

    if (!mContent[mTrackDetectorInfo])
        mContent[mTrackDetectorInfo] = new StSPtrVecTrackDetectorInfo;
    if (!mTrackNodes)
        mTrackNodes = new StSPtrVecTrackNode;
    return *mTrackNodes;
    return *info;
}

    if (!mContent[mTrackNodes])
        mContent[mTrackNodes] = new StSPtrVecTrackNode;
    if (!mTrackNodes)
        mTrackNodes = new StSPtrVecTrackNode;
    return *mTrackNodes;
    return *nodes;
}

    if (!mContent[mTrackNodes])
        mContent[mTrackNodes] = new StSPtrVecTrackNode;
    return mPrimaryVertices ? mPrimaryVertices->size() : 0;
    StSPtrVecTrackNode *nodes = 0;
    _lookupOrCreate(nodes, mContent);
    return *nodes;
}

    if (mPrimaryVertices && i < mPrimaryVertices->size())
        return (*mPrimaryVertices)[i];
    _lookupOrCreate(vertices, mContent);
    return vertices ? vertices->size() : 0;
}

    if (mContent[mPrimaryVertices] && i < static_cast<StSPtrVecPrimaryVertex*>(mContent[mPrimaryVertices])->size())
        return (*static_cast<StSPtrVecPrimaryVertex*>(mContent[mPrimaryVertices]))[i];
    _lookup(vertices, mContent);
    if (mPrimaryVertices && i < mPrimaryVertices->size())
        return (*mPrimaryVertices)[i];
    else
        return 0;
}

    if (mContent[mPrimaryVertices] && i < static_cast<StSPtrVecPrimaryVertex*>(mContent[mPrimaryVertices])->size())
        return (*static_cast<StSPtrVecPrimaryVertex*>(mContent[mPrimaryVertices]))[i];
    _lookup(vertices, mContent);
    if (!mV0Vertices)
        mV0Vertices = new StSPtrVecV0Vertex;
    return *mV0Vertices;
        return 0;
}

    if (!mContent[mV0Vertices])
        mContent[mV0Vertices] = new StSPtrVecV0Vertex;
    if (!mV0Vertices)
        mV0Vertices = new StSPtrVecV0Vertex;
    return *mV0Vertices;
    return *vertices;
}

    if (!mContent[mV0Vertices])
        mContent[mV0Vertices] = new StSPtrVecV0Vertex;
    if (!mXiVertices)
        mXiVertices = new StSPtrVecXiVertex;
    return *mXiVertices;
    return *vertices;
}

    if (!mContent[mXiVertices])
        mContent[mXiVertices] = new StSPtrVecXiVertex;
    if (!mXiVertices)
        mXiVertices = new StSPtrVecXiVertex;
    return *mXiVertices;
    return *vertices;
}

    if (!mContent[mXiVertices])
        mContent[mXiVertices] = new StSPtrVecXiVertex;
    if (!mKinkVertices)
        mKinkVertices = new StSPtrVecKinkVertex;
    return *mKinkVertices;
    return *vertices;
}

    if (!mContent[mKinkVertices])
        mContent[mKinkVertices] = new StSPtrVecKinkVertex;
    if (!mKinkVertices)
        mKinkVertices = new StSPtrVecKinkVertex;
    return *mKinkVertices;

StSPtrVecObject&
StEvent::content() { return mContent; }
StEvent::setType(const Char_t* val) { mType = val; }
    info->setType(val);
}
StEvent::setRunId(Long_t val) { mRunId = val; }
    info->setRunId(val);
}
StEvent::setId(Long_t val) { mId = val; }
    info->setId(val);
}
StEvent::setTime(Long_t val) { mTime = val; }
    info->setTime(val);
}
StEvent::setTriggerMask(ULong_t val) { mTriggerMask = val; }
    info->setTriggerMask(val);
    static_cast<StEventInfo*>(mContent[mInfo])->setBunchCrossingNumber(val);
StEvent::setBunchCrossingNumber(ULong_t val) { mBunchCrossingNumber = val; }
    info->setBunchCrossingNumber(val, i);
}

    if (mContent[mSummary]) delete mContent[mSummary];
    if (mSoftwareMonitor) delete mSoftwareMonitor;
    mSoftwareMonitor = val;
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mSoftwareMonitor]) delete mContent[mSoftwareMonitor];
    if (mTpcHits) delete mTpcHits;
    mTpcHits = val;
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mTpcHits]) delete mContent[mTpcHits];
    if (mFtpcHits) delete mFtpcHits;
    mFtpcHits = val;
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mFtpcHits]) delete mContent[mFtpcHits];
    if (mSvtHits) delete mSvtHits;
    mSvtHits = val;
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mSvtHits]) delete mContent[mSvtHits];
    if (mSsdHits) delete mSsdHits;
    mSsdHits = val;
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mSsdHits]) delete mContent[mSsdHits];
    mContent[mSsdHits] = val;
    if (mEmcCollection) delete mEmcCollection;
    mEmcCollection = val;
StEvent::setRichPixelCollection(StRichPixelCollection* val)
}
    if (mRichPixels) delete mRichPixels;
    mRichPixels = val;
    if (mContent[mEmcCollection]) delete mContent[mEmcCollection];
    if (mRichCollection) delete mRichCollection;
    mRichCollection = val;
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mRichCollection]) delete mContent[mRichCollection];
    if (mTriggerDetectors) delete mTriggerDetectors;
    mTriggerDetectors = val;
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mTriggerDetectors]) delete mContent[mTriggerDetectors];
    if (mL0Trigger) delete mL0Trigger;
    mL0Trigger = val;
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mL0Trigger]) delete mContent[mL0Trigger];
    if (mL3Trigger) delete mL3Trigger;
    mL3Trigger = val;
{
    _lookupAndSet(val, mContent);
}

    if (mContent[mL3Trigger]) delete mContent[mL3Trigger];
    mContent[mL3Trigger] = val;
        if (!mPrimaryVertices)
            mPrimaryVertices = new StSPtrVecPrimaryVertex;
        mPrimaryVertices->push_back(vertex);
        if (!mContent[mPrimaryVertices])
            mContent[mPrimaryVertices] = new StSPtrVecPrimaryVertex;

	StSPtrVecPrimaryVertex* vertexVector =
	    static_cast<StSPtrVecPrimaryVertex*>(mContent[mPrimaryVertices]);
    if (vertex) {
        for (int i=mPrimaryVertices->size()-1; i>0; i--) {
            if ((*mPrimaryVertices)[i]->numberOfDaughters() > (*mPrimaryVertices)[i-1]->numberOfDaughters())
                swap((*mPrimaryVertices)[i], (*mPrimaryVertices)[i-1]);
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
