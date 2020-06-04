/***************************************************************************
 *
 * $Id: StEvent.cxx,v 2.59 2018/12/11 19:53:10 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 * Do not touch anything here unless you REALLY know what you are doing.
 *
 ***************************************************************************
 *
 * $Log: StEvent.cxx,v $
 * Revision 2.59  2018/12/11 19:53:10  ullrich
 * Added RICHf.
 *
 * Revision 2.58  2018/07/09 16:50:31  ullrich
 * Added EToF hooks.
 *
 * Revision 2.57  2018/02/08 17:36:26  ullrich
 * Changed for new EPD classes.
 *
 * Revision 2.56  2015/10/09 17:46:15  ullrich
 * Changed type of mIdTruth from ushort to int.
 *
 * Revision 2.55  2015/05/13 17:06:13  ullrich
 * Added hooks and interfaces to Sst detector (part of HFT).
 *
 * Revision 2.54  2014/04/10 16:00:12  jeromel
 * Changes to inlcude Ist structure (Thomas OK-ed / may revisit some comments)
 *
 * Revision 2.53  2013/03/05 14:49:44  ullrich
 * Added PxlHits to statistics().
 *
 * Revision 2.52  2013/03/05 14:42:45  ullrich
 * Added StPxl hits and Containers.
 *
 * Revision 2.51  2012/04/16 20:22:16  ullrich
 * Changes necessary to add Fgt package.
 *
 * Revision 2.50  2012/01/24 03:01:38  perev
 * Etr detector added
 *
 * Revision 2.49  2011/10/17 00:13:49  fisyak
 * Add handles for IdTruth info
 *
 * Revision 2.48  2011/10/13 17:52:22  perev
 * Comment++
 *
 * Revision 2.47  2011/04/25 21:25:09  ullrich
 * Modifications to hold MTD data.
 *
 * Revision 2.46  2011/04/01 19:43:19  perev
 * Allow zero pointer for TBrowser. debug
 *
 * Revision 2.45  2011/02/01 19:47:36  ullrich
 * Added HLT branch and hooks.
 *
 * Revision 2.44  2010/08/31 19:53:37  fisyak
 * Remove SoftwareMonitors
 *
 * Revision 2.43  2010/01/08 22:43:44  ullrich
 * Updates needed to add StFmsCollection and related classes.
 *
 * Revision 2.42  2009/11/23 22:22:25  ullrich
 * Minor cleanup performed and hooks for RPS added.
 *
 * Revision 2.41  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.40  2008/12/22 20:36:53  ullrich
 * Added hooks for new ToF (BTof)
 *
 * Revision 2.39  2006/04/25 23:21:25  ullrich
 * Modified addPrimaryVertex(). New 2nd arg: StPrimaryVertexOrder.
 *
 * Revision 2.38  2006/01/19 21:48:21  ullrich
 * Add RnD collection.
 *
 * Revision 2.37  2005/06/15 21:58:16  ullrich
 * Change sorting of primary tracks for PPV.
 *
 * Revision 2.36  2003/04/16 17:48:32  ullrich
 * Added StTriggerData and inherited classe(s).
 *
 * Revision 2.35  2003/01/30 18:36:31  ullrich
 * Added hooks for StTriggerIdCollection.
 *
 * Revision 2.34  2002/12/20 22:41:30  ullrich
 * Added PMD.
 *
 * Revision 2.33  2002/01/17 01:34:07  ullrich
 * Fixed the fix in psd() methods.
 *
 * Revision 2.32  2002/01/17 01:29:10  ullrich
 * Fixed bug in psd() methods.
 *
 * Revision 2.31  2002/01/03 20:59:33  ullrich
 * Added BBC and FPD.
 *
 * Revision 2.30  2001/12/01 15:40:47  ullrich
 * Added StDetectorState access function.
 *
 * Revision 2.29  2001/11/10 23:53:23  ullrich
 * Added calibration vertices.
 *
 * Revision 2.28  2001/11/07 21:19:42  ullrich
 * Added L1 trigger.
 *
 * Revision 2.27  2001/09/18 00:15:25  ullrich
 * Added StRunInfo and access functions.
 *
 * Revision 2.26  2001/06/05 21:59:56  perev
 *  Split in Streamer added
 *
 * Revision 2.25  2001/05/30 17:45:53  perev
 * StEvent branching
 *
 * Revision 2.24  2001/05/17 22:56:18  ullrich
 * Removed all usage of dst_summary_param.
 *
 * Revision 2.23  2001/04/25 17:42:28  perev
 * HPcorrs
 *
 * Revision 2.22  2001/04/23 19:28:13  ullrich
 * Added StClusteringHints and methods to access it.
 *
 * Revision 2.21  2001/04/05 04:00:49  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.20  2001/03/14 02:35:43  ullrich
 * Added container and methods to handle PSDs.
 *
 * Revision 2.19  2001/03/09 05:23:53  ullrich
 * Added new method statistics().
 *
 * Revision 2.18  2000/12/08 03:53:40  ullrich
 * Prepared hooks for ToF.
 *
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
#include "TClass.h"
#include "TDataSetIter.h"
#include "TObjectSet.h"
#include "TBrowser.h"
#include "StCalibrationVertex.h"
#include "StDetectorState.h"
#include "StEvent.h"
#include "StEventClusteringHints.h"
#include "StEventInfo.h"
#include "StEventSummary.h"
#include "StTpcHitCollection.h"
#include "StRnDHitCollection.h"
#include "StEtrHitCollection.h"
#include "StSvtHitCollection.h"
#include "StSsdHitCollection.h"
#include "StSstHitCollection.h"
#include "StFtpcHitCollection.h"
#include "StEmcCollection.h"
#include "StEpdCollection.h"
#include "StFmsCollection.h"
#include "StRHICfCollection.h"
#include "StRichCollection.h"
#include "StRpsCollection.h"
#include "StRunInfo.h"
#include "StTofCollection.h"
#include "StBTofCollection.h"
#include "StETofCollection.h"
#include "StMtdCollection.h"
#include "StFpdCollection.h"
#include "StPhmdCollection.h"
#include "StTrackDetectorInfo.h"
#include "StTriggerData.h"
#include "StTriggerDetectorCollection.h"
#include "StTriggerIdCollection.h"
#include "StPrimaryVertex.h"
#include "StL0Trigger.h"
#include "StL1Trigger.h"
#include "StL3Trigger.h"
#include "StPsd.h"
#include "event_header.h"
#include "StAutoBrowse.h"
#include "StEventBranch.h"
#include "StHltEvent.h"
#include "StFgtCollection.h"
#include "StPxlHitCollection.h"
#include "StIstHitCollection.h"
#include "StTrackNode.h"
#include "StTrack.h"

#ifndef ST_NO_NAMESPACES
using std::swap;
#endif

TString StEvent::mCvsTag  = "$Id: StEvent.cxx,v 2.59 2018/12/11 19:53:10 ullrich Exp $";
static const char rcsid[] = "$Id: StEvent.cxx,v 2.59 2018/12/11 19:53:10 ullrich Exp $";

ClassImp(StEvent)

//______________________________________________________________________________
template<class T> void _lookup(T*& val, StSPtrVecObject &vec)
{
    val = 0;
    for (unsigned int i=0; i<vec.size(); i++)
        if (vec[i] && typeid(*vec[i]) == typeid(T)) {
            val = static_cast<T*>(vec[i]);
            break;
        }
}

//______________________________________________________________________________
template<class T> void _lookupOrCreate(T*& val, StSPtrVecObject &vec)
{
    T* t = 0;
    _lookup(t, vec);
    if (!t) {
        t = new T;
        vec.push_back(t);
    }
    val = t;
}

//______________________________________________________________________________
template<class T> void _lookupAndSet(T* val, StSPtrVecObject &vec)
{
    for (unsigned int i=0; i<vec.size(); i++)
        if (vec[i] && typeid(*vec[i]) == typeid(T)) {
            delete vec[i];
            vec[i] = val;
            return;
        }
    if (!val) return;
    vec.push_back(val);
}

//______________________________________________________________________________
template<class T> void _lookupDynamic(T*& val, StSPtrVecObject &vec)
{
    val = 0;
    for (unsigned int i=0; i<vec.size(); i++)
        if (vec[i]) {
	    val = dynamic_cast<T*>(vec[i]);
	    if (val) break;	    
	}
}

//______________________________________________________________________________
template<class T> void _lookupDynamicAndSet(T* val, StSPtrVecObject &vec)
{
    T *test;
    for (unsigned int i=0; i<vec.size(); i++) {
        if (vec[i]) {
	    test = dynamic_cast<T*>(vec[i]);
	    if (test) {
		delete vec[i];
		vec[i] = val;
		return;
	    }	    
	}
    }
    if (!val) return;
    vec.push_back(val);
}

//______________________________________________________________________________
void StEvent::initToZero() { /* noop */ }

//______________________________________________________________________________
StEvent::StEvent() : StXRefMain("StEvent")
{
    GenUUId();		//Generate Universally Unique IDentifier
    initToZero();
}
  
//______________________________________________________________________________
StEvent::~StEvent()
{ /* noop */ }

TString
StEvent::type() const
{
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info ? info->type() : TString();
}

int
StEvent::id() const
{
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info ? info->id() : 0;
}

int
StEvent::runId() const
{
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info ? info->runId() : 0;
}

int
StEvent::time() const
{
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info ? info->time() : 0;
}

unsigned int
StEvent::triggerMask() const
{
    StEventInfo* info = 0;
    _lookup(info, mContent);
    return info ? info->triggerMask() : 0;
}

unsigned int
StEvent::bunchCrossingNumber(unsigned int i) const
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

StRunInfo*
StEvent::runInfo()
{
    StRunInfo* info = 0;
    _lookup(info, mContent);
    return info;
}

const StRunInfo*
StEvent::runInfo() const
{
    StRunInfo* info = 0;
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

//______________________________________________________________________________
StTpcHitCollection* StEvent::tpcHitCollection()
{
    StTpcHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

//______________________________________________________________________________
const StTpcHitCollection*
StEvent::tpcHitCollection() const
{
    StTpcHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

//______________________________________________________________________________
StRnDHitCollection* StEvent::rndHitCollection()
{
    StRnDHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

//______________________________________________________________________________
const StRnDHitCollection* StEvent::rndHitCollection() const
{
    StRnDHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

//______________________________________________________________________________
const StEtrHitCollection* StEvent::etrHitCollection() const
{
    StEtrHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

//______________________________________________________________________________
     StEtrHitCollection* StEvent::etrHitCollection() 
{
    StEtrHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}
//______________________________________________________________________________
StFtpcHitCollection* StEvent::ftpcHitCollection()
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

StSstHitCollection*
StEvent::sstHitCollection()
{
    StSstHitCollection *hits = 0;
    _lookup(hits, mContent);
    return hits;
}

const StSstHitCollection*
StEvent::sstHitCollection() const
{
    StSstHitCollection *hits = 0;
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

StFmsCollection*
StEvent::fmsCollection()
{
    StFmsCollection *fms = 0;
    _lookup(fms, mContent);
    return fms;
}

const StFmsCollection*
StEvent::fmsCollection() const
{
    StFmsCollection *fms = 0;
    _lookup(fms, mContent);
    return fms;
}

StRHICfCollection*
StEvent::rhicfCollection()
{
    StRHICfCollection *rhicf = 0;
    _lookup(rhicf, mContent);
    return rhicf;
}

const StRHICfCollection*
StEvent::rhicfCollection() const
{
    StRHICfCollection *rhicf = 0;
    _lookup(rhicf, mContent);
    return rhicf;
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

StRpsCollection*
StEvent::rpsCollection()
{
    StRpsCollection *rps = 0;
    _lookup(rps, mContent);
    return rps;
}

const StRpsCollection*
StEvent::rpsCollection() const
{
    StRpsCollection *rps = 0;
    _lookup(rps, mContent);
    return rps;
}

StTofCollection*
StEvent::tofCollection()
{
    StTofCollection *tof = 0;
    _lookup(tof, mContent);
    return tof;
}

const StTofCollection*
StEvent::tofCollection() const
{
    StTofCollection *tof = 0;
    _lookup(tof, mContent);
    return tof;
}

StBTofCollection*
StEvent::btofCollection()
{
    StBTofCollection *btof = 0;
    _lookup(btof, mContent);
    return btof;
}

const StBTofCollection*
StEvent::btofCollection() const
{
    StBTofCollection *btof = 0;
    _lookup(btof, mContent);
    return btof;
}

StETofCollection*
StEvent::etofCollection()
{
    StETofCollection *etof = 0;
    _lookup(etof, mContent);
    return etof;
}

const StETofCollection*
StEvent::etofCollection() const
{
    StETofCollection *etof = 0;
    _lookup(etof, mContent);
    return etof;
}

StEpdCollection*
StEvent::epdCollection()
{
    StEpdCollection *epd = 0;
    _lookup(epd, mContent);
    return epd;
}

const StEpdCollection*
StEvent::epdCollection() const
{
    StEpdCollection *epd = 0;
    _lookup(epd, mContent);
    return epd;
}

StMtdCollection*
StEvent::mtdCollection()
{
    StMtdCollection *mtd = 0;
    _lookup(mtd, mContent);
    return mtd;
}

const StMtdCollection*
StEvent::mtdCollection() const
{
    StMtdCollection *mtd = 0;
    _lookup(mtd, mContent);
    return mtd;
}

StFpdCollection*
StEvent::fpdCollection()
{
    StFpdCollection *fpd = 0;
    _lookup(fpd, mContent);
    return fpd;
}

const StFpdCollection*
StEvent::fpdCollection() const
{
    StFpdCollection *fpd = 0;
    _lookup(fpd, mContent);
    return fpd;
}

StPhmdCollection*
StEvent::phmdCollection()
{
    StPhmdCollection *phmd = 0;
    _lookup(phmd, mContent);
    return phmd;
}

const StPhmdCollection*
StEvent::phmdCollection() const
{
    StPhmdCollection *phmd = 0;
    _lookup(phmd, mContent);
    return phmd;
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

StTriggerIdCollection*
StEvent::triggerIdCollection()
{
    StTriggerIdCollection *trg = 0;
    _lookup(trg, mContent);
    return trg;
}

const StTriggerIdCollection*
StEvent::triggerIdCollection() const
{
    StTriggerIdCollection *trg = 0;
    _lookup(trg, mContent);
    return trg;
}

StTriggerData*
StEvent::triggerData()
{
    StTriggerData *trg = 0;
    _lookupDynamic(trg, mContent);
    return trg;
}

const StTriggerData*
StEvent::triggerData() const
{
    StTriggerData *trg = 0;
    _lookupDynamic(trg, mContent);
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

StL1Trigger*
StEvent::l1Trigger()
{
    StL1Trigger *trg = 0;
    _lookup(trg, mContent);
    return trg;
}

const StL1Trigger*
StEvent::l1Trigger() const
{
    StL1Trigger *trg = 0;
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

StHltEvent*
StEvent::hltEvent()
{
    StHltEvent *hlt = 0;
    _lookup(hlt, mContent);
    return hlt;
}

const StHltEvent*
StEvent::hltEvent() const
{
    StHltEvent *hlt = 0;
    _lookup(hlt, mContent);
    return hlt;
}

StFgtCollection*
StEvent::fgtCollection()
{
    StFgtCollection *fgtCollection = 0;
    _lookup(fgtCollection, mContent);
    return fgtCollection;
}

const StFgtCollection*
StEvent::fgtCollection() const
{
    StFgtCollection *fgtCollection = 0;
    _lookup(fgtCollection, mContent);
    return fgtCollection;
}

StIstHitCollection*
StEvent::istHitCollection()
{
    StIstHitCollection *istHitCollection = 0;
    _lookup(istHitCollection, mContent);
    return istHitCollection;
}

const StIstHitCollection*
StEvent::istHitCollection() const
{
    StIstHitCollection *istHitCollection = 0;
    _lookup(istHitCollection, mContent);
    return istHitCollection;
}

StPxlHitCollection*
StEvent::pxlHitCollection()
{
    StPxlHitCollection *pxlHitCollection = 0;
    _lookup(pxlHitCollection, mContent);
    return pxlHitCollection;
}

const StPxlHitCollection*
StEvent::pxlHitCollection() const
{
    StPxlHitCollection *pxlHitCollection = 0;
    _lookup(pxlHitCollection, mContent);
    return pxlHitCollection;
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

unsigned int
StEvent::numberOfPrimaryVertices() const
{
    StSPtrVecPrimaryVertex *vertices = 0;
    _lookupOrCreate(vertices, mContent);
    return vertices ? vertices->size() : 0;
}

StPrimaryVertex*
StEvent::primaryVertex(unsigned int i)
{
    StSPtrVecPrimaryVertex *vertices = 0;
    _lookup(vertices, mContent);
    if (vertices && i < vertices->size())
        return (*vertices)[i];
    else
        return 0;
}

const StPrimaryVertex*
StEvent::primaryVertex(unsigned int i) const
{
    StSPtrVecPrimaryVertex *vertices = 0;
    _lookup(vertices, mContent);
    if (vertices && i < vertices->size())
        return (*vertices)[i];
    else
        return 0;
}

unsigned int
StEvent::numberOfCalibrationVertices() const
{
    StSPtrVecCalibrationVertex *vertices = 0;
    _lookupOrCreate(vertices, mContent);
    return vertices ? vertices->size() : 0;
}

StCalibrationVertex*
StEvent::calibrationVertex(unsigned int i)
{
    StSPtrVecCalibrationVertex *vertices = 0;
    _lookup(vertices, mContent);
    if (vertices && i < vertices->size())
        return (*vertices)[i];
    else
        return 0;
}

const StCalibrationVertex*
StEvent::calibrationVertex(unsigned int i) const
{
    StSPtrVecCalibrationVertex *vertices = 0;
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

StDetectorState*
StEvent::detectorState(StDetectorId det)
{
    StSPtrVecDetectorState *states = 0;
    _lookup(states, mContent);
    if (states)
	for (unsigned int i=0; i<states->size(); i++)
	    if ((*states)[i]->detector() == det) return (*states)[i];
    return 0;			       
}

const StDetectorState*
StEvent::detectorState(StDetectorId det) const
{
    StSPtrVecDetectorState *states = 0;
    _lookup(states, mContent);
    if (states)
	for (unsigned int i=0; i<states->size(); i++)
	    if ((*states)[i]->detector() == det) return (*states)[i];
    return 0;			       
}

StPsd*
StEvent::psd(StPwg p, int i)
{
    StPsd *thePsd = 0;
    for (unsigned int k=0; k<mContent.size(); k++) {
	thePsd = dynamic_cast<StPsd*>(mContent[k]);
	if (thePsd && thePsd->pwg() == p && thePsd->id() == i)
	    return thePsd;
    }
    return 0;
}

const StPsd*
StEvent::psd(StPwg p, int i) const
{
    const StPsd *thePsd = 0;
    for (unsigned int k=0; k<mContent.size(); k++) {
	thePsd = dynamic_cast<StPsd*>(mContent[k]);
	if (thePsd && thePsd->pwg() == p && thePsd->id() == i)
	    return thePsd;
    }
    return 0;
}

unsigned int
StEvent::numberOfPsds() const
{
    int nPsds = 0;
    for (unsigned int i=0; i<mContent.size(); i++)
	if (dynamic_cast<StPsd*>(mContent[i])) nPsds++;
    return nPsds;
}

unsigned int
StEvent::numberOfPsds(StPwg p) const
{
    StPsd* thePsd;
    int nPsds = 0;
    for (unsigned int i=0; i<mContent.size(); i++) {
	thePsd = dynamic_cast<StPsd*>(mContent[i]);
	if (thePsd && thePsd->pwg() == p) nPsds++;
    }
    return nPsds;
}

StSPtrVecObject&
StEvent::content() { return mContent; }

const StEventClusteringHints*
StEvent::clusteringHints() const
{
    StEventClusteringHints *hints = 0;
    _lookupOrCreate(hints, mContent);
    return hints;
}

StEventClusteringHints*
StEvent::clusteringHints()
{
    StEventClusteringHints *hints = 0;
    _lookupOrCreate(hints, mContent);
    hints->SetParent(this);
    return hints;
}

void
StEvent::setType(const char* val)
{
    StEventInfo* info = 0;
    _lookupOrCreate(info, mContent);
    info->setType(val);
}

void
StEvent::setRunId(int val)
{
    StEventInfo* info = 0;
    _lookupOrCreate(info, mContent);
    info->setRunId(val);
}

void
StEvent::setId(int val)
{
    StEventInfo* info = 0;
    _lookupOrCreate(info, mContent);
    info->setId(val);
}

void
StEvent::setTime(int val)
{
    StEventInfo* info = 0;
    _lookupOrCreate(info, mContent);
    info->setTime(val);
}

void
StEvent::setTriggerMask(unsigned int val)
{
    StEventInfo* info = 0;
    _lookupOrCreate(info, mContent);
    info->setTriggerMask(val);
}

void
StEvent::setBunchCrossingNumber(unsigned int val, unsigned int i)
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
StEvent::setRunInfo(StRunInfo* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setSummary(StEventSummary* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setTpcHitCollection(StTpcHitCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setRnDHitCollection(StRnDHitCollection* val)
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
StEvent::setSstHitCollection(StSstHitCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setEmcCollection(StEmcCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setFmsCollection(StFmsCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setRHICfCollection(StRHICfCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setRichCollection(StRichCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setRpsCollection(StRpsCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setTofCollection(StTofCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setBTofCollection(StBTofCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setETofCollection(StETofCollection* val)
{
    _lookupAndSet(val, mContent);
}


void
StEvent::setEpdCollection(StEpdCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setMtdCollection(StMtdCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setFpdCollection(StFpdCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setPhmdCollection(StPhmdCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setTriggerDetectorCollection(StTriggerDetectorCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setTriggerIdCollection(StTriggerIdCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setTriggerData(StTriggerData* val)
{
    _lookupDynamicAndSet(val, mContent);
}

void
StEvent::setL0Trigger(StL0Trigger* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setL1Trigger(StL1Trigger* val)
{
    _lookupAndSet(val, mContent);
}

void 
StEvent::setL3Trigger(StL3Trigger* val)
{
    _lookupAndSet(val, mContent);
}

void 
StEvent::setEtrHitCollection(StEtrHitCollection* val)
{
    _lookupAndSet(val, mContent);
}

void 
StEvent::setHltEvent(StHltEvent* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setFgtCollection(StFgtCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setIstHitCollection(StIstHitCollection* val)
{
    _lookupAndSet(val, mContent);
}

void
StEvent::setPxlHitCollection(StPxlHitCollection* val)
{
    _lookupAndSet(val, mContent);
}

void 
StEvent::addPrimaryVertex(StPrimaryVertex* vertex, StPrimaryVertexOrder order)
{
    if (!vertex) return;  // not a valid vertex, do nothing

    //
    //  Add the vertex
    //
    StSPtrVecPrimaryVertex* vertexVector = 0;
    _lookupOrCreate(vertexVector, mContent);
    vertexVector->push_back(vertex);

    //
    //  Sort vertices.
    //  New vertex is last entry. We simply toggle through
    //  the container starting at the back until the new entry
    //  sits in place. Sorting strategy is given by
    //  enumeration StPrimaryVertexOrder.
    //
    int i;
    switch (order) {
    case(orderByNumberOfDaughters):   
        for (i=vertexVector->size()-1; i>0; i--) {
	  if ((*vertexVector)[i]->numberOfDaughters() > (*vertexVector)[i-1]->numberOfDaughters())
	      swap((*vertexVector)[i], (*vertexVector)[i-1]);
	  else
	      break;
        }
        break;
        
    case(orderByRanking):
    default:
        for (i=vertexVector->size()-1; i>0; i--) {
	  if ((*vertexVector)[i]->ranking() > (*vertexVector)[i-1]->ranking())
	      swap((*vertexVector)[i], (*vertexVector)[i-1]);
	  else
	      break;
        }
        break;
    }
}

void
StEvent::addCalibrationVertex(StCalibrationVertex* vertex)
{
    if (vertex) {
        StSPtrVecCalibrationVertex* vertexVector = 0;
        _lookupOrCreate(vertexVector, mContent);
        vertexVector->push_back(vertex);
    }
}

void
StEvent::addDetectorState(StDetectorState *state)
{
    if (state) {
        StSPtrVecDetectorState* stateVector = 0;
        _lookupOrCreate(stateVector, mContent);
        stateVector->push_back(state);
    }
}

void
StEvent::addPsd(StPsd* p)
{
    if (p) {
        if (psd(p->pwg(), p->id()))
            cerr << "StEvent::addPsd(): Error, PSD with same identifiers already exist. Nothing added." << endl;
        else
	    mContent.push_back(p);
    }
}

void StEvent::removePsd(StPsd* p)
{
    StSPtrVecObjectIterator iter;
    if (p) {
	for (iter = mContent.begin(); iter != mContent.end(); iter++)
	    if (*iter == p)
		mContent.erase(iter);
    }
}

void StEvent::Browse(TBrowser* b)
{
    if (!b) b = new TBrowser("StEvent",(TObject*)0);
    StAutoBrowse::Browse(this,b);
    TDataSet::Browse(b);
}

void StEvent::statistics()
{
    cout << "Statistics and information for event " << id() << endl;
    cout << "\tthis:                        " << static_cast<void*>(this) << endl;
//  cout << "\tcvsTag:                      " << cvsTag() << endl;
    cout << "\ttype:                        " << type() << endl;
    cout << "\tid:                          " << id() << endl;
    cout << "\trunId:                       " << runId() << endl;
    cout << "\ttime:                        " << time() << endl;
    cout << "\ttriggerMask:                 " << triggerMask() << endl;
    cout << "\tbunchCrossingNumber(0):      " << bunchCrossingNumber(0) << endl;
    cout << "\tbunchCrossingNumber(1):      " << bunchCrossingNumber(1) << endl;
    cout << "\tStEventSummary:              " << static_cast<void*>(summary()) << endl;
    cout << "\tStTpcHitCollection:          " << static_cast<void*>(tpcHitCollection()) << endl;
    cout << "\tStRnDHitCollection:          " << static_cast<void*>(rndHitCollection()) << endl;
    cout << "\tStFtpcHitCollection:         " << static_cast<void*>(ftpcHitCollection()) << endl;
    cout << "\tStSvtHitCollection:          " << static_cast<void*>(svtHitCollection()) << endl;
    cout << "\tStSsdHitCollection:          " << static_cast<void*>(ssdHitCollection()) << endl;
    cout << "\tStSstHitCollection:          " << static_cast<void*>(sstHitCollection()) << endl;
    cout << "\tStIstHitCollection:          " << static_cast<void*>(istHitCollection()) << endl;
    cout << "\tStPxlHitCollection:          " << static_cast<void*>(pxlHitCollection()) << endl;
    cout << "\tStEmcCollection:             " << static_cast<void*>(emcCollection()) << endl;
    cout << "\tStFmsCollection:             " << static_cast<void*>(fmsCollection()) << endl;
    cout << "\tStRHICfCollection:           " << static_cast<void*>(rhicfCollection()) << endl;
    cout << "\tStRichCollection:            " << static_cast<void*>(richCollection()) << endl;
    cout << "\tStRpsCollection:             " << static_cast<void*>(rpsCollection()) << endl;
    cout << "\tStTofCollection:             " << static_cast<void*>(tofCollection()) << endl;
    cout << "\tStBTofCollection:            " << static_cast<void*>(btofCollection()) << endl;
    cout << "\tStETofCollection:            " << static_cast<void*>(etofCollection()) << endl;
    cout << "\tStEpdCollection:             " << static_cast<void*>(epdCollection()) << endl;
    cout << "\tStMtdCollection:             " << static_cast<void*>(mtdCollection()) << endl;
    cout << "\tStFpdCollection:             " << static_cast<void*>(fpdCollection()) << endl;
    cout << "\tStPhmdCollection:            " << static_cast<void*>(phmdCollection()) << endl;
    cout << "\tStL0Trigger:                 " << static_cast<void*>(l0Trigger()) << endl;
    cout << "\tStL1Trigger:                 " << static_cast<void*>(l0Trigger()) << endl;
    cout << "\tStL3Trigger:                 " << static_cast<void*>(l3Trigger()) << endl;
    cout << "\tStHltEvent:                  " << static_cast<void*>(hltEvent()) << endl;
    cout << "\tStTriggerDetectorCollection: " << static_cast<void*>(triggerDetectorCollection()) << endl;
    cout << "\tStTriggerIdCollection:       " << static_cast<void*>(triggerIdCollection()) << endl;
    cout << "\tStTriggerData:               " << static_cast<void*>(triggerData()) << endl;
    cout << "\tStPrimaryVertex:             " << static_cast<void*>(primaryVertex(0)) << endl;
    cout << "\tnumberOfPrimaryVertices:     " << numberOfPrimaryVertices() << endl;
    cout << "\tStCalibrationVertex:         " << static_cast<void*>(calibrationVertex(0));
    cout << "\tnumberOfCalibrationVertices: " << numberOfCalibrationVertices() << endl;
    cout << "\t# of TPC hits:               " << (tpcHitCollection() ? tpcHitCollection()->numberOfHits() : 0) << endl;
    cout << "\t# of FTPC hits:              " << (ftpcHitCollection() ? ftpcHitCollection()->numberOfHits() : 0) << endl;
    cout << "\t# of SVT hits:               " << (svtHitCollection() ? svtHitCollection()->numberOfHits() : 0) << endl;
    cout << "\t# of SSD hits:               " << (ssdHitCollection() ? ssdHitCollection()->numberOfHits() : 0) << endl;
    cout << "\t# of IST hits:               " << (istHitCollection() ? istHitCollection()->numberOfHits() : 0) << endl;
    cout << "\t# of PXL hits:               " << (pxlHitCollection() ? pxlHitCollection()->numberOfHits() : 0) << endl;
    cout << "\t# of track nodes:            " << trackNodes().size() << endl;
    cout << "\t# of primary tracks:         " << (primaryVertex(0) ? primaryVertex(0)->numberOfDaughters() : 0) << endl;
    cout << "\t# of V0s:                    " << v0Vertices().size() << endl;
    cout << "\t# of Xis:                    " << xiVertices().size() << endl;
    cout << "\t# of Kinks:                  " << kinkVertices().size() << endl;
    cout << "\t# of hits in EMC:            " << (emcCollection() ? emcCollection()->barrelPoints().size() : 0) << endl;
    cout << "\t# of hits in EEMC:           " << (emcCollection() ? emcCollection()->endcapPoints().size() : 0) << endl;
    cout << "\t# of hits in FGT:            " << (fgtCollection() ? fgtCollection()->getNumHits() : 0) << endl;
    cout << "\t# of hits in RICH:           " << (richCollection() ? richCollection()->getRichHits().size() : 0) << endl;
    cout << "\t# of PSDs:                   " << numberOfPsds() << endl;
}

void StEvent::Split()
{
    StEventClusteringHints *clu = clusteringHints();  
    assert(clu);
    TDataSetIter next(this);
    TDataSet *ds;
//		Delete all the old EventBranches
    while ((ds=next())) {
	if (ds->IsA()!=StEventBranch::Class()) continue;
	Remove(ds); delete ds;
    }

    vector<string> brs = clu->listOfBranches();       // list of all branches for given mode (miniDST or DST)         
    int nbrs = brs.size();
    for (int ibr =0; ibr < nbrs; ibr++) { //loop over branches
	string sbr = brs[ibr];
	if(sbr.size()==0)		continue;
	const char *brName = sbr.c_str();
	assert(strncmp(brName,"evt_",4)==0 || strcmp(brName,"event")==0);
	
	UInt_t tally = ((clu->branchId(brName)) << 22) | 1 ;
	
	StEventBranch *obr = new StEventBranch(brName,this,tally);
	vector<string> cls = clu->listOfClasses(sbr.c_str());
	int ncls = cls.size();
	for (int icl =0; icl < ncls; icl++) { //loop over clases
	    string scl = cls[icl];
	    if(scl.size()==0) 	continue;
	    obr->AddKlass(scl.c_str());
	} //end clases
    } //end branches
}

Bool_t StEvent::Notify() {Split();return 0;}  

void StEvent::Streamer(TBuffer &R__b)
{
    // Stream an object of class StEvent.
    
    UInt_t R__s, R__c;
    if (R__b.IsReading()) {
        
        Version_t R__v = R__b.ReadVersion(&R__s, &R__c);
        if (R__v == 1) {
            TDataSet::Streamer(R__b);
            mContent.Streamer(R__b);
            R__b.CheckByteCount(R__s, R__c, Class());
            Split();
            return;
        }
        else { // version >=2
            StXRefMain::Streamer(R__b);
            R__b.CheckByteCount(R__s, R__c, Class());
        }
        
    }
    else /*writing*/ {
        
        TDataSetIter next(this);
        TDataSet *ds;
        while ((ds=next())) {
            if (ds->IsA()==StEventBranch::Class()) break;
        }
        if (!ds) {//Not splited yet
            Split();}
        
        R__c = R__b.WriteVersion(Class(), kTRUE);
        StXRefMain::Streamer(R__b);
        R__b.SetByteCount(R__c, kTRUE);   
    } 
}
//________________________________________________________________________________
StSPtrVecHit* StEvent::hitCollection(const Char_t *name) {
    StSPtrVecHit *theHitCollection = 0;
    TObjectSet *set = (TObjectSet *) FindByName(name);
    if (set) theHitCollection = (StSPtrVecHit *) set->GetObject();
    return theHitCollection;
}
//________________________________________________________________________________
void StEvent::addHitCollection(StSPtrVecHit* p, const Char_t *name) {
    if (p) {
        TObjectSet *set = (TObjectSet *) FindByName(name);
        if (set)
            cerr << "StEvent::addHitCollection(): Error, HitCollection with "
            <<  name << " already exist. Nothing added."  << endl;
        else {
            set = new TObjectSet(name,p,kTRUE);
            Add(set);
        }
    }
}
//________________________________________________________________________________
void StEvent::removeHitCollection(const Char_t *name) {
    TObjectSet *set = (TObjectSet *) FindByName(name);
    if (set) set->Delete();
}
//________________________________________________________________________________
void StEvent::setIdTruth() {
    StSPtrVecTrackNode& trackNode = trackNodes();
    UInt_t nTracks = trackNode.size();
    StTrackNode *node=0;
    for (UInt_t i = 0; i < nTracks; i++) {
        node = trackNode[i];
        if (!node) continue;
        UInt_t notr = node->entries();
        for (UInt_t t = 0; t < notr; t++) {
            StTrack *track = node->track(t);
            track->setIdTruth();
        }
    }
    // loop over all type of vertices
    Int_t noOfPrimaryVertices = numberOfPrimaryVertices();
    for (Int_t i = 0;  i < noOfPrimaryVertices; i++) primaryVertex(i)->setIdTruth();
    Int_t noOfCalibrationVertices = numberOfCalibrationVertices();
    for (Int_t i = 0;  i < noOfCalibrationVertices; i++) calibrationVertex(i)->setIdTruth();
    Int_t noOfv0Vertices = v0Vertices().size();
    for (Int_t i = 0;  i < noOfv0Vertices; i++) ((StVertex *) v0Vertices()[i])->setIdTruth();
    Int_t noOfxiVertices = xiVertices().size();
    for (Int_t i = 0;  i < noOfxiVertices; i++) ((StVertex *) xiVertices()[i])->setIdTruth();
    Int_t noOfkinkVertices = kinkVertices().size();
    for (Int_t i = 0;  i < noOfkinkVertices; i++) ((StVertex *) kinkVertices()[i])->setIdTruth();
}
