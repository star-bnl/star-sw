/***************************************************************************
 *
 * $Id: StEvent.cxx,v 2.17 2000/09/25 14:47:25 ullrich Exp $
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
 * Revision 2.17  2000/09/25 14:47:25  ullrich
 * Fixed problem in _lookup() and _lookupOrCreate().
 *
 * Revision 2.16  2000/09/25 14:21:27  ullrich
 * Removed enums for content vector. Replaced by lookup function.
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
 * Completely Revised for New Version
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

TString StEvent::mCvsTag  = "$Id: StEvent.cxx,v 2.17 2000/09/25 14:47:25 ullrich Exp $";
static const char rcsid[] = "$Id: StEvent.cxx,v 2.17 2000/09/25 14:47:25 ullrich Exp $";

ClassImp(StEvent)

template<class T> void
_lookup(T*& val, StSPtrVecObject &vec)
{
    val = 0;
    for (unsigned int i=0; i<vec.size(); i++)
	if (vec[i] && typeid(*vec[i]) == typeid(T)) {
	    val = static_cast<T*>(vec[i]);
	    break;
	}
}

template<class T> void
_lookupOrCreate(T*& val, StSPtrVecObject &vec)
{
    T* t = 0;
    _lookup(t, vec);
    if (!t) {
	t = new T;
	vec.push_back(t);
    }
    val = t;
}

template<class T> void
_lookupAndSet(T* val, StSPtrVecObject &vec)
{
    for (unsigned int i=0; i<vec.size(); i++)
	if (vec[i] && typeid(*vec[i]) == typeid(T)) {
	    delete vec[i];
	    vec[i] = val;
	    return;
	}    
    if (val) vec.push_back(val);
}

void
StEvent::initToZero() { /* noop */ }

void
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
{
    initToZero();
    init(evtHdr);
    mContent.push_back(new StEventSummary(evtSum, sumPar));
}

StEvent::StEvent(const event_header_st& evtHdr) :
    St_DataSet("StEvent")
{
    initToZero();
    init(evtHdr);
}

StEvent::~StEvent()
{ /* noop */ }

TString
StEvent::type() const
{
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info ? info->type() : TString();
}

Long_t
StEvent::id() const
{
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info ? info->id() : 0;
}

Long_t
StEvent::runId() const
{
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info ? info->runId() : 0;
}

Long_t
StEvent::time() const 
{
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info ? info->time() : 0;
}

ULong_t
StEvent::triggerMask() const  
{
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info ? info->triggerMask() : 0;
}

ULong_t
StEvent::bunchCrossingNumber(UInt_t i) const  
{
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info ? info->bunchCrossingNumber(i) : 0;
}

StEventInfo*
StEvent::info()
{
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info;
}

const StEventInfo*
StEvent::info() const
{
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info;
}

StEventSummary*
StEvent::summary()
{
    StEventSummary* summary = 0;
    _lookup(summary, mContent);
    return summary;
}

const StEventSummary*
StEvent::summary() const
{
    StEventSummary* summary = 0;
    _lookup(summary, mContent);
    return summary;
}

const TString&
StEvent::cvsTag() { return mCvsTag; }

StSoftwareMonitor*
StEvent::softwareMonitor()
{
    StSoftwareMonitor *monitor = 0;
    _lookup(monitor, mContent);
    return monitor;
}

const StSoftwareMonitor*
StEvent::softwareMonitor() const
{
    StSoftwareMonitor *monitor = 0;
    _lookup(monitor, mContent);
    return monitor;
}

StTpcHitCollection*
StEvent::tpcHitCollection()
{
    StTpcHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

const StTpcHitCollection*
StEvent::tpcHitCollection() const
{
    StTpcHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

StFtpcHitCollection*
StEvent::ftpcHitCollection()
{
    StFtpcHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

const StFtpcHitCollection*
StEvent::ftpcHitCollection() const
{
    StFtpcHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

StSvtHitCollection*
StEvent::svtHitCollection()
{
    StSvtHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

const StSvtHitCollection*
StEvent::svtHitCollection() const
{
    StSvtHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

StSsdHitCollection*
StEvent::ssdHitCollection()
{
    StSsdHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

const StSsdHitCollection*
StEvent::ssdHitCollection() const
{
    StSsdHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

StEmcCollection*
StEvent::emcCollection()
{
    StEmcCollection *emc = 0;
    _lookup(emc, mContent);
    return emc;
}

const StEmcCollection*
StEvent::emcCollection() const
{
    StEmcCollection *emc = 0;
    _lookup(emc, mContent);
    return emc;
}

StRichCollection*
StEvent::richCollection()
{
    StRichCollection *rich = 0;
    _lookup(rich, mContent);
    return rich;
}

const StRichCollection*
StEvent::richCollection() const
{
    StRichCollection *rich = 0;
    _lookup(rich, mContent);
    return rich;
}

StTriggerDetectorCollection*
StEvent::triggerDetectorCollection()
{
    StTriggerDetectorCollection *trg = 0;
    _lookup(trg, mContent);
    return trg;
}

const StTriggerDetectorCollection*
StEvent::triggerDetectorCollection() const
{
    StTriggerDetectorCollection *trg = 0;
    _lookup(trg, mContent);
    return trg;
}

StL0Trigger*
StEvent::l0Trigger()
{
    StL0Trigger *trg = 0;
    _lookup(trg, mContent);
    return trg;
}

const StL0Trigger*
StEvent::l0Trigger() const
{
    StL0Trigger *trg = 0;
    _lookup(trg, mContent);
    return trg;
}

StL3Trigger*
StEvent::l3Trigger()
{
    StL3Trigger *trg = 0;
    _lookup(trg, mContent);
    return trg;
}

const StL3Trigger*
StEvent::l3Trigger() const
{
    StL3Trigger *trg = 0;
    _lookup(trg, mContent);
    return trg;
}


StSPtrVecTrackDetectorInfo&
StEvent::trackDetectorInfo()
{
    StSPtrVecTrackDetectorInfo *info = 0;
    _lookupOrCreate(info, mContent);
    return *info;
}

const StSPtrVecTrackDetectorInfo&
StEvent::trackDetectorInfo() const
{
    StSPtrVecTrackDetectorInfo *info = 0;
    _lookupOrCreate(info, mContent);
    return *info;
}

StSPtrVecTrackNode&
StEvent::trackNodes()
{
    StSPtrVecTrackNode *nodes = 0;
    _lookupOrCreate(nodes, mContent);
    return *nodes;
}

const StSPtrVecTrackNode&
StEvent::trackNodes() const
{
    StSPtrVecTrackNode *nodes = 0;
    _lookupOrCreate(nodes, mContent);
    return *nodes;
}

UInt_t
StEvent::numberOfPrimaryVertices() const
{
    StSPtrVecPrimaryVertex *vertices = 0;
    _lookupOrCreate(vertices, mContent);
    return vertices ? vertices->size() : 0;
}

StPrimaryVertex*
StEvent::primaryVertex(UInt_t i)
{
    StSPtrVecPrimaryVertex *vertices = 0;
    _lookup(vertices, mContent);
    if (vertices && i < vertices->size())
        return (*vertices)[i];
    else
        return 0;
}

const StPrimaryVertex*
StEvent::primaryVertex(UInt_t i) const
{
    StSPtrVecPrimaryVertex *vertices = 0;
    _lookup(vertices, mContent);
    if (vertices && i < vertices->size())
        return (*vertices)[i];
    else
        return 0;
}

StSPtrVecV0Vertex&
StEvent::v0Vertices()
{
    StSPtrVecV0Vertex *vertices = 0;
    _lookupOrCreate(vertices, mContent);
    return *vertices;
}

const StSPtrVecV0Vertex&
StEvent::v0Vertices() const
{
    StSPtrVecV0Vertex *vertices = 0;
    _lookupOrCreate(vertices, mContent);
    return *vertices;
}

StSPtrVecXiVertex&
StEvent::xiVertices()
{
    StSPtrVecXiVertex *vertices = 0;
    _lookupOrCreate(vertices, mContent);
    return *vertices;
}

const StSPtrVecXiVertex&
StEvent::xiVertices() const
{
    StSPtrVecXiVertex *vertices = 0;
    _lookupOrCreate(vertices, mContent);
    return *vertices;
}

StSPtrVecKinkVertex&
StEvent::kinkVertices()
{
    StSPtrVecKinkVertex *vertices = 0;
    _lookupOrCreate(vertices, mContent);
    return *vertices;
}

const StSPtrVecKinkVertex&
StEvent::kinkVertices() const
{
    StSPtrVecKinkVertex *vertices = 0;
    _lookupOrCreate(vertices, mContent);
    return *vertices;
}

StSPtrVecObject&
StEvent::content() { return mContent; }

void
StEvent::setType(const Char_t* val)
{
    StEventInfo* info = 0;
    _lookupOrCreate(info, mContent);
    info->setType(val);
}

void
StEvent::setRunId(Long_t val)
{
    StEventInfo* info = 0;
    _lookupOrCreate(info, mContent);
    info->setRunId(val);
}

void
StEvent::setId(Long_t val)
{
    StEventInfo* info = 0;
    _lookupOrCreate(info, mContent);
    info->setId(val);
}

void
StEvent::setTime(Long_t val)
{
    StEventInfo* info = 0;
    _lookupOrCreate(info, mContent);
    info->setTime(val);
}

void
StEvent::setTriggerMask(ULong_t val)
{
    StEventInfo* info = 0;
    _lookupOrCreate(info, mContent);
    info->setTriggerMask(val);
}

void
StEvent::setBunchCrossingNumber(ULong_t val, UInt_t i)
{
    StEventInfo* info = 0;
    _lookupOrCreate(info, mContent);
    info->setBunchCrossingNumber(val, i);
}

void
StEvent::setInfo(StEventInfo* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setSummary(StEventSummary* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setSoftwareMonitor(StSoftwareMonitor* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setTpcHitCollection(StTpcHitCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setFtpcHitCollection(StFtpcHitCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setSvtHitCollection(StSvtHitCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setSsdHitCollection(StSsdHitCollection* val)
{
    _lookupAndSet(val, mContent);
}


void
StEvent::setEmcCollection(StEmcCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setRichCollection(StRichCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setTriggerDetectorCollection(StTriggerDetectorCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setL0Trigger(StL0Trigger* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setL3Trigger(StL3Trigger* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::addPrimaryVertex(StPrimaryVertex* vertex)
{
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
