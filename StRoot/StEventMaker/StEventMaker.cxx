/*************************************************************************** 
 *
 * $Id: StEventMaker.cxx,v 2.6 1999/11/11 17:46:30 ullrich Exp $
 *
 * Author: Original version by T. Wenaus, BNL
 *         Revised version for new StEvent by T. Ullrich, Yale
 ***************************************************************************
 *
 * Description: Setup of StEvent
 *
 ***************************************************************************
 *
 * $Log: StEventMaker.cxx,v $
 * Revision 2.6  1999/11/11 17:46:30  ullrich
 * Added more checks and warning messages. Handling
 * of primary vertices made safer
 *
 * Revision 2.27  2000/05/26 11:36:19  ullrich
 * Default is to NOT print event info (doPrintEventInfo  = kFALSE).
 *
 * Revision 2.26  2000/05/26 11:34:08  ullrich
 * Skip the attempt of creating an instance of StRun in case
 * no dst dataset is available.
 *
 * Revision 2.25  2000/05/25 14:44:43  ullrich
 * Removed remaining pieces of the RICH pixel table.
 *
 * Revision 2.24  2000/05/24 15:48:15  ullrich
 * Instance of StEvent now also created if no DST dataset
 * is available.
 *
 * Revision 2.23  2000/05/22 21:53:41  ullrich
 * No more copying of RICH tables. RICH now writes directly
 * to StEvent. printEventInfo() and makeEvent() modified.
 *
 * Revision 2.22  2000/04/26 20:29:13  ullrich
 * Create instance of StEvent not StBrowsableEvent.
 *
 * Revision 2.21  2000/03/22 17:11:20  ullrich
 * Added further checks for case were tables exist but have
#include "StTrack.h"
 * zero length. Added for primary and global tracks.
 *
 * Revision 2.20  2000/02/23 12:11:49  ullrich
 * Added printout of covariant matrix to printTrackInfo().
 *
 * Revision 2.19  2000/02/17 18:19:05  ullrich
 * Adapted new SVT hit storage layout. Barrels instead of layers.
 *
 * Revision 2.18  2000/02/11 16:12:33  ullrich
 * Modified check for valid primary vertices.
 *
 * Revision 2.17  2000/02/08 21:14:16  genevb
 * Handle cases with no tracks.
 *
 * Revision 2.16  2000/01/25 20:11:11  ullrich
 * Fixed bug in loading the Xi vertices.
 *
 * Revision 2.15  2000/01/14 18:51:06  ullrich
 * Added printout of quasi-histos in the event summary
 * to printEventInfo().
 * With Victors help now possible to read the dst_summary_param
#if defined(__SUNPRO_CC)
 *
 * Revision 2.12  2000/01/10 18:20:32  ullrich
 * Create new StTrackDetectorInfo object for primary tracks if
 * first or last points differ from the referring global track.
 *
 * Revision 2.11  2000/01/05 16:07:44  ullrich
 * Added loading of SSD hits and handling of runco branch.
 *
 * Revision 2.10  1999/12/21 15:13:13  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
    doPrintRunInfo    = kTRUE;  // TMP, set to kFALSE later
    doPrintEventInfo  = kTRUE;  // TMP, set to kFALSE later
    doPrintMemoryInfo = kTRUE;  // TMP, set to kFALSE later
    doPrintRunInfo    = kFALSE; 
    doPrintEventInfo  = kFALSE;
    doPrintMemoryInfo = kTRUE;
 *
 * Revision 2.7  1999/11/17 14:10:27  ullrich
 * Added more checks to protect from corrupted table data.
 * Revision 2.6  1999/11/11 17:46:30  ullrich
 * Added more checks and warning messages. Handling
 * of primary vertices made safer
    doPrintRunInfo    = kTRUE;  // TMP 
    doPrintEventInfo  = kTRUE;  // TMP
 *
static const char rcsid[] = "$Id: StEventMaker.cxx,v 2.6 1999/11/11 17:46:30 ullrich Exp $";
 * Delete hit if it cannot be added to collection.
 *
 * Revision 2.3  1999/11/08 17:04:59  ullrich
 * Hits now allocated individually.
 *
 * Revision 2.2  1999/11/05 18:35:54  ullrich
 * Added methods and flags for debugging and monitoring.
 *
 * Revision 2.1  1999/11/04 19:55:37  ullrich
 * Corrected typo.
 *
 * Revision 2.0  1999/11/04 19:03:00  ullrich
 * Revised to build new StEvent version
 *
 **************************************************************************/
#include <vector>
#include <algorithm>
#include <utility>
#include "StEventMaker/StEventMaker.h"
#include "StEventMaker/StRootEventManager.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StGlobals.hh"
#include "StEvtHddr.h"

#if !defined(ST_NO_NAMESPACES)
    doPrintEventInfo  = kFALSE; 
    doPrintMemoryInfo = kTRUE;  
    doPrintCpuInfo    = kTRUE; 
#endif

#if defined(ST_NO_TEMPLATE_DEF_ARGS)
#define StVector(T) vector<T, allocator<T> >
#else
static const char rcsid[] = "$Id: StEventMaker.cxx,v 2.6 1999/11/11 17:46:30 ullrich Exp $";
#endif

static const char rcsid[] = "$Id: StEventMaker.cxx,v 2.6 1999/11/11 17:46:30 ullrich Exp $";

ClassImp(StEventMaker)
    doPrintEventInfo  = kFALSE;
{
    if(title) SetTitle(title);
    mEventManager = new StRootEventManager();
    mEventManager->setMaker(this);
    if (status == oocError) return kStWarn;
    doLoadSvtHits     = kTRUE;
    doLoadSsdHits     = kTRUE;
    doLoadTptTracks   = kFALSE;
    doPrintEventInfo  = kFALSE;
    doPrintMemoryInfo = kTRUE;
        status = makeRun();
        if (status == kStOK) AddRunCont(mCurrentRun);
    mCurrentEvent=0;
    StMaker::Clear();
}

StEventManager*
StEventMaker::eventManager() {return mEventManager;};
    if (status == kStOK) AddData(mCurrentEvent);
    

Int_t
	gMessMgr->Warning() << "StEventMaker::Make(): cannot open 'dstBranch'." << endm;
	return kStWarn;
    //
    // In this method we actually do not create anything but call
    // other methods which do that for us:
    // makeRun()      creates StRun and all its dependent classes
    // makeEvent()    creates StEvent and all its dependent classes
    //
    // Since this Maker should also work without any 'dst' dataset
        status = makeRun();	
    //  Init timing and memory snapshots
	    AddRunCont(mCurrentRun);
	else
	    gMessMgr->Warning() << "StEventMaker::Make(): no StRun object created." << endm;

	status = loadRunConstants();    
	if (status != kStOK)
	    gMessMgr->Warning() << "StEventMaker::Make(): cannot load run constants." << endm;
    //
    //  If no DST dataset is available we cannot setup StEvent
    //  properly. Nevertheless we will create an empty instance.
    //  Note that in this case also no instance of StRun can
    //  get instantiated.
    //  it with the parameters of the current StRun instance.
    int status = mEventManager->openEvent("dst");
	AddData(mCurrentEvent);
    if (isNewRun()) {
	gMessMgr->Warning() << "StEventMaker::Make(): no StEvent object created." << endm;
    }
    else
	mCreateEmptyInstance = kFALSE;

    //
    //  Setup run header and summary (StRun/StRunSummary)
    //  and get new run constants.
    //
    if (!mCreateEmptyInstance && isNewRun()) {
	StMemoryInfo::instance()->snapshot();
	StMemoryInfo::instance()->print();
            AddRunCont(mCurrentRun);
        else
	    gMessMgr->Warning() << "StEventMaker::loadRunConstants(): cannot open 'runcoBranch'." << endm;
    if (doPrintEventInfo) printEventInfo();
        StMemoryInfo::instance()->snapshot();
    return mDstSummaryParam ? kStOK : kStWarn;

Bool_t
StEventMaker::isNewRun()
{
    //
    //  Checks if we reached a new run by comparing the run header table
    //  with the parameters of the current StRun instance.
    //
    if (!mCurrentRun) return kTRUE;

        gMessMgr->Warning() << "StEventMaker::makeRun(): cannot load run_header_st, "
                            << "no StRun object created." << endm;
	mDstSummaryParam = theEventManager->returnTable_dst_summary_param(nrows);
	theEventManager->closeEvent();
	if (mDstSummaryParam) return kStOK;
        if (dstRunHeader->bfc_run_id == mCurrentRun->bfcId() &&
            dstRunHeader->exp_run_id == mCurrentRun->id())
            return kFALSE;
        else
	mDstSummaryParam = theEventManager->returnTable_dst_summary_param(nrows);
	theEventManager->closeEvent();
	if (mDstSummaryParam) return kStOK;
        return kFALSE;   // nothing we can do anyhow
}

Int_t
StEventMaker::loadRunConstants()
{
    //
    //  Load run constants. So far there is only the dst_summary_param
    //  table we have to handle. Create a new event manager to not
    //  screw up thing in outer scope.
    //
    if (mDstSummaryParam) return kStOK;
    StEventManager* theEventManager = new StRootEventManager();
    theEventManager->setMaker(this);
    long nrows;
    
    //  1st suppose we are in bfc
    if (theEventManager->openEvent("dst/.runco") != oocError) {
        mDstSummaryParam = theEventManager->returnTable_dst_summary_param(nrows);
        theEventManager->closeEvent();
        if (mDstSummaryParam) return kStOK;
    }
    
    //  2nd suppose we are in doEvents
    dst_summary_param_st* dstSummaryParam = mEventManager->returnTable_dst_summary_param(nrows);
    if (theEventManager->openEvent("dstRunco") != oocError) {
    if (!dstEventSummary || !dstSummaryParam)
        theEventManager->closeEvent();
        if (mDstSummaryParam) return kStOK;
        mCurrentEvent = new StBrowsableEvent(*dstEventHeader, *dstEventSummary, *dstSummaryParam);

    gMessMgr->Warning() << "StEventMaker::loadRunConstants(): cannot find dst_summary_param" << endm;
    return kStWarn;
}

Int_t
StEventMaker::makeRun()
{
    //
    //  To setup the StRun/StRunSummary object properly we have to pass
    //  the run_header_st and dst_run_summary_st tables to StRun.
    //  StRunSummary is automatically setup through StRun.
    //  If run_header_st doesn't exist we stop, if dst_run_summary_st
    //  doesn't exist we continue since StRun can handle this case.
    //
    long nrows;
    
    run_header_st* dstRunHeader = mEventManager->returnTable_run_header(nrows);
    if (!dstRunHeader) {
StEventMaker::makeEvent()
{
    //
    //  Setup StEvent itself.
    //        If we do not get the event header we better stop
    //  right away. If we cannot get the event summary
    //  we continue.
    //  create an empty instance only. This is OK in this
    //  case and therefore we do not return a warning or an
    //
    if (!dstEventHeader) {
        mCurrentEvent = 0;
        mCurrentEvent = new StEvent;
        mCurrentEvent = new StBrowsableEvent(*dstEventHeader);
        return kStWarn;
        mCurrentEvent = new StBrowsableEvent(*dstEventHeader, *dstEventSummary, *mDstSummaryParam);
	return kStOK;
    dst_event_summary_st* dstEventSummary = mEventManager->returnTable_dst_event_summary(nrows);

    if (!dstEventSummary || !mDstSummaryParam)

    dst_event_summary_st* dstEventSummary = mEventManager->returnTable_dst_event_summary(nrows);
        mCurrentEvent = new StEvent(*dstEventHeader, *dstEventSummary, *mDstSummaryParam);
    if (!dstEventHeader) 
        gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot load dst_event_header_st, "
                            << "will create incomplete instance of StEvent." << endm;
    
    //
    //  Create instance of StEvent, using whatever we got so far.
    //
    if (dstEventHeader && dstEventSummary && mDstSummaryParam) 
        mCurrentEvent = new StEvent(*dstEventHeader, *dstEventSummary, *mDstSummaryParam);
    else if (dstEventHeader)
        mCurrentEvent = new StEvent(*dstEventHeader);
    else if (dstEventSummary && mDstSummaryParam) {	
    //	Load trigger & trigger detector data
	mCurrentEvent->setSummary(new StEventSummary(*dstEventSummary, *mDstSummaryParam));

    else
        mCurrentEvent = new StEvent;

    //
    //  Setup the software monitors.
    //
    dst_mon_soft_ctb_st*  dstSoftMonCtb    = mEventManager->returnTable_dst_mon_soft_ctb(nrows);
    dst_mon_soft_emc_st*  dstSoftMonEmc    = mEventManager->returnTable_dst_mon_soft_emc(nrows);
    dst_mon_soft_ftpc_st* dstSoftMonFtpc   = mEventManager->returnTable_dst_mon_soft_ftpc(nrows);
    dst_mon_soft_glob_st* dstSoftMonGlobal = mEventManager->returnTable_dst_mon_soft_glob(nrows);
    dst_mon_soft_l3_st*   dstSoftMonL3     = mEventManager->returnTable_dst_mon_soft_l3(nrows);
    dst_mon_soft_rich_st* dstSoftMonRich   = mEventManager->returnTable_dst_mon_soft_rich(nrows);
    dst_mon_soft_svt_st*  dstSoftMonSvt    = mEventManager->returnTable_dst_mon_soft_svt(nrows);
    dst_mon_soft_tpc_st*  dstSoftMonTpc    = mEventManager->returnTable_dst_mon_soft_tpc(nrows);
    
    mCurrentEvent->setSoftwareMonitor(new StSoftwareMonitor(dstSoftMonTpc,
                                                            dstSoftMonSvt,
                                                            dstSoftMonFtpc,
                                                            dstSoftMonEmc,
                                                            dstSoftMonCtb,
	id = dstPrimaryTracks[i].id_start_vertex/10;
                                                            dstSoftMonGlobal,
                                                            dstSoftMonL3));
    
    //
    //        Load trigger & trigger detector data
    //
    StSPtrVecTrackNode         &trackNodes   = mCurrentEvent->trackNodes();
    StSPtrVecV0Vertex          &v0Vertices   = mCurrentEvent->v0Vertices();
    StSPtrVecXiVertex          &xiVertices   = mCurrentEvent->xiVertices();
    StSPtrVecKinkVertex        &kinkVertices = mCurrentEvent->kinkVertices();
    StTrackDetectorInfo        *info;
    StTrackNode                *node;
    
    int                        i;
    
    //
    //  Create global tracks.
    //  Since the further setup depends on the id of the tracks
    //  in dst_track we temporarily store the pointers in a vector
    //  (vecGlobalTracks) sorted according to their dst_track::id.
    //  This makes things a lot easier.
    //
    StGlobalTrack *gtrack = 0;
    dst_track_st *dstGlobalTracks = mEventManager->returnTable_dst_globtrk(nrows);
    maxId = dstPrimaryTracks ? max((long)dstPrimaryTracks[nrows-1].id, nrows) : 0;
            ptrack->setDetectorInfo(vecGlobalTracks[id]->detectorInfo());

    //
		detectorInfo.push_back(info);
	    }
	    ptrack->setDetectorInfo(info);
    StVector(unsigned int)    vecPrimaryVertexId(maxId+1, 0U);
    nfailed = 0;
    
    for (i=0; i<nrows; i++) {
        id = dstPrimaryTracks[i].id_start_vertex ? dstPrimaryTracks[i].id_start_vertex/10 : 0;
        if (!id) {
            nfailed++;
            continue;
        }
        ptrack = new StPrimaryTrack(dstPrimaryTracks[i]);
        vecPrimaryTracks[dstPrimaryTracks[i].id]   = ptrack;
    if (nfailed) 
	gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
			    << " primary tracks, no valid primary vertex found." << endm;
        if (id < vecGlobalTracks.size() && vecGlobalTracks[id]) {
            info = vecGlobalTracks[id]->detectorInfo();
            //
            //  Check if the existing detector info is still ok for the
            //  primary track. If not we have to create a new one.
            //  See also comments above on existing problems with this.
            //
            StThreeVectorF firstPoint(dstPrimaryTracks[i].x_first);
	k = 0;
	id = dstDedx[i].id_track; 
	if (id < vecGlobalTracks.size() && vecGlobalTracks[id]) {
	    vecGlobalTracks[id]->addPidTraits(new StDedxPidTraits(dstDedx[i]));
	    k++;
	}
	if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id]) {
	    vecPrimaryTracks[id]->addPidTraits(new StDedxPidTraits(dstDedx[i]));
	    k++;
	}
    //  entry in the temprary vector (vecPrimaryTracks)
    if (nfailed) 
        StV0Vertex *v0 = new StV0Vertex(dstVertices[id], dstV0Vertices[i]);
        id = dstV0Vertices[i].idneg;
        if (id < vecGlobalTracks.size()) v0->addDaughter(vecGlobalTracks[id]);
        id = dstV0Vertices[i].idpos;
        if (id < vecGlobalTracks.size()) v0->addDaughter(vecGlobalTracks[id]);
        v0Vertices.push_back(v0);
    dst_dedx_st *dstDedx = mEventManager->returnTable_dst_dedx(nrows);
        id = dstDedx[i].id_track;
	    dstVertices[i].vtx_id == kEventVtxId) {
            vecGlobalTracks[id]->addPidTraits(new StDedxPidTraits(dstDedx[i]));
            k++;
		if (vecPrimaryTracks[k] &&
		    vecPrimaryVertexId[k] == (unsigned int) dstVertices[i].id) {
            vecTptTracks[id]->addPidTraits(new StDedxPidTraits(dstDedx[i]));
		}
        if (!k) nfailed++;
        StXiVertex *xi = new StXiVertex(dstVertices[id], dstXiVertices[i]);
        id = dstXiVertices[i].id_v0 - 1;
        if (id < v0Vertices.size()) xi->setV0Vertex(v0Vertices[id]);
        id = dstXiVertices[i].id_b;
        if (id < vecGlobalTracks.size()) xi->addDaughter(vecGlobalTracks[id]);
        xiVertices.push_back(xi);
    //  is set to 0. At the end we delete all primary
    long nVertices;
    dst_vertex_st *dstVertices = mEventManager->returnTable_dst_vertex(nVertices);

    for (i=0; i<nVertices; i++) {
        if (dstVertices[i].iflag < 100 && dstVertices[i].iflag%10 == 1 &&
	if (id < static_cast<unsigned long>(nVertices)) {
	    StV0Vertex *v0 = new StV0Vertex(dstVertices[id], dstV0Vertices[i]);
	    if (id < vecGlobalTracks.size()) v0->addDaughter(vecGlobalTracks[id]);
	    id = dstV0Vertices[i].idpos;
        StKinkVertex *kink = new StKinkVertex(dstVertices[id], dstKinkVertices[i]);
        id = dstKinkVertices[i].idd;
        if (id < vecGlobalTracks.size()) kink->addDaughter(vecGlobalTracks[id]);
        id = dstKinkVertices[i].idp;
        if (id < vecGlobalTracks.size()) kink->setParent(vecGlobalTracks[id]);
        kinkVertices.push_back(kink);
    if (nfailed)
    //
    //  Setup V0 vertices
    //
    long nV0Vertices;
    dst_v0_vertex_st* dstV0Vertices = mEventManager->returnTable_dst_v0_vertex(nV0Vertices);
	if (id < static_cast<unsigned long>(nVertices)) {
			    << " Xi vertices, no valid id_vertex." << endm;
	    id = dstXiVertices[i].id_v0 - 1;
	    if (id < v0Vertices.size()) xi->setV0Vertex(v0Vertices[id]);
	    id  = dstXiVertices[i].id_b;       // no -1 here 
	    if (id < vecGlobalTracks.size()) xi->addDaughter(vecGlobalTracks[id]);
	    xiVertices.push_back(xi);
	}
	else
	    nfailed++;
    }
    if (nfailed) 
	gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
			    << " Xi vertices, invalid foreign key to vertex table." << endm;
    if (nfailed)
    if (doLoadTpcHits || doLoadFtpcHits || doLoadSvtHits) {
                            << " V0 vertices, no valid id_vertex." << endm;

    //
    //  Setup Xi vertices
    //
    long         nXiVertices;
    dst_xi_vertex_st* dstXiVertices = mEventManager->returnTable_dst_xi_vertex(nXiVertices);
	if (id < static_cast<unsigned long>(nVertices)) {
	    StKinkVertex *kink = new StKinkVertex(dstVertices[id], dstKinkVertices[i]);
	    id = dstKinkVertices[i].idd;
	    if (id < vecGlobalTracks.size()) kink->addDaughter(vecGlobalTracks[id]);
	    id = dstKinkVertices[i].idp;
	    if (id < vecGlobalTracks.size()) kink->setParent(vecGlobalTracks[id]);
	    kinkVertices.push_back(kink);
	}
	else
	    nfailed++;
    }
    if (nfailed) 
			    << " kink vertices, no valid id_vertex." << endm;

    }
    if (nfailed)
        gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
                            << " Xi vertices, invalid foreign key to vertex table." << endm;

    //
    //  Setup kinks
		    if (id < vecGlobalTracks.size() && vecGlobalTracks[id])
			vecGlobalTracks[id]->detectorInfo()->addHit(tpcHit);
		    else if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id])
			vecPrimaryTracks[id]->detectorInfo()->addHit(tpcHit);
        if (id < static_cast<unsigned long>(nVertices)) {
            StKinkVertex *kink = new StKinkVertex(dstVertices[id], dstKinkVertices[i]);
            id = dstKinkVertices[i].idd;
            if (id < vecGlobalTracks.size()) kink->addDaughter(vecGlobalTracks[id]);
            id = dstKinkVertices[i].idp;
            if (id < vecGlobalTracks.size()) kink->setParent(vecGlobalTracks[id]);
	
        }
        else
            nfailed++;
    }
	    info    = 0;
	    nfailed = 0;
                            << " kink vertices, no valid id_vertex." << endm;

    //
    //  Since the user might have decided to skip certain kind of hits
    //  we have to scan them all and get the first index and the total
    //  number of those which have to be loaded.
		    id = dstPoints[i].id_track;
		    if (id < vecGlobalTracks.size() && vecGlobalTracks[id]) {
			info = vecGlobalTracks[id]->detectorInfo();
			info->addHit(tpcHit);
		    }
		    if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id])
		    if (id < vecGlobalTracks.size() && vecGlobalTracks[id])
			vecGlobalTracks[id]->detectorInfo()->addHit(svtHit);
		    else if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id])
			vecPrimaryTracks[id]->detectorInfo()->addHit(svtHit);
        int  begin, end;
        
	    if (nfailed) 
		gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
				    << " TPC hits, wrong hardware address." << endm;
        if (doLoadTpcHits) {
            info    = 0;
            nfailed = 0;
            StSvtHit *svtHit;
            begin = index[kSvtId].first;
            end   = index[kSvtId].first+index[kSvtId].second;
	    info    = 0;
	    nfailed = 0;
                svtHit = new StSvtHit(dstPoints[i]);
                if (svtHitColl->addHit(svtHit)) {
                    id = dstPoints[i].id_track;
                        info = vecGlobalTracks[id]->detectorInfo();
                        info->addHit(svtHit);
                    }
		    id = dstPoints[i].id_track;
		    if (id < vecGlobalTracks.size() && vecGlobalTracks[id]) {
			info = vecGlobalTracks[id]->detectorInfo();
			info->addHit(ssdHit);
		    }
		    if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id])
			if (vecPrimaryTracks[id]->detectorInfo() != info)
		    if (id < vecGlobalTracks.size() && vecGlobalTracks[id])
			vecGlobalTracks[id]->detectorInfo()->addHit(ftpcHit);
		    else if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id])
			vecPrimaryTracks[id]->detectorInfo()->addHit(ftpcHit);

	    if (nfailed) 
		gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
				    << " SSD hits, wrong hardware address." << endm;
        if (doLoadSsdHits) {
	
            nfailed = 0;
            StSsdHit *ssdHit;
            begin = index[kSsdId].first;
            end   = index[kSsdId].first+index[kSsdId].second;
	    info    = 0;
	    nfailed = 0;
                ssdHit = new StSsdHit(dstPoints[i]);
		    if (id < vecGlobalTracks.size() && vecGlobalTracks[id])
			vecGlobalTracks[id]->detectorInfo()->addHit(ftpcHit);
		    else if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id])
			vecPrimaryTracks[id]->detectorInfo()->addHit(ftpcHit);
		    id = dstPoints[i].id_track;
		    if (id < vecGlobalTracks.size() && vecGlobalTracks[id]) {
			info = vecGlobalTracks[id]->detectorInfo();
			info->addHit(ftpcHit);
		    }
		    if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id])
			if (vecPrimaryTracks[id]->detectorInfo() != info)
			    vecPrimaryTracks[id]->detectorInfo()->addHit(ftpcHit);
		}
		else {
		    nfailed++;
		    delete ftpcHit;
		}
        
        //
        //        FTPC hits
        //
        if (doLoadFtpcHits) {
            info    = 0;
            nfailed = 0;
		else {
		    nfailed++;
		    delete ftpcHit;
		}
                    if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id])
                        if (vecPrimaryTracks[id]->detectorInfo() != info)
	    if (nfailed) 
		gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
				    << " FTPC hits, wrong hardware address." << endm;
                    nfailed++;
                    delete ftpcHit;
    
            }
            // east
            begin = index[kFtpcEastId].first;
            end   = index[kFtpcEastId].first+index[kFtpcEastId].second;
            for (i=begin; i<end; i++) {
                ftpcHit = new StFtpcHit(dstPoints[i]);
	mCurrentEvent->setRichPixelCollection(new StRichPixelCollection(dstRichPixel, nrows));
    //
    //  Load RICH pixel
    //
    nrows = 0;
    dst_rch_pixel_st* dstRichPixel = mEventManager->returnTable_dst_rch_pixel(nrows);
    if (dstRichPixel && nrows)
        mCurrentEvent->setRichPixelCollection(new StRichPixelCollection(dstRichPixel, nrows));
                if (ftpcHitColl->addHit(ftpcHit)) {
                    id = dstPoints[i].id_track;
                    if (id < vecGlobalTracks.size() && vecGlobalTracks[id]) {
                        info = vecGlobalTracks[id]->detectorInfo();
                        info->addHit(ftpcHit);
                    }
                    if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id])
    if (mCurrentRun) 
	mCurrentRun->Dump();
                }
                else {
                    nfailed++;
	 << (mCurrentRun ? (void*) (mCurrentRun->summary()) : 0)        << endl;
                }
            }
            mCurrentEvent->setFtpcHitCollection(ftpcHitColl);
            if (nfailed)
    StEvtHddr* header = dynamic_cast<StEvtHddr*>(GetInputDS("EvtHddr"));
    if (header) {
	mCurrentEvent->setTriggerMask(header->GetTriggerMask());
    }
    

		 << "] : \t"   << evtsum->tracksInPtBin(k) << endl;
	}
	cout << "--> StEventSummary quasi-histogram -> energy vs. eta" << endl;
	for (k=0; k<evtsum->numberOfBins(); k++) {
	    cout << k << "\t[" << evtsum->lowerEdgeEtaBin(k)
		 << " - "      << evtsum->upperEdgeEtaBin(k)
		 << "] : \t"   << evtsum->energyInEtaBin(k) << endl;
	}
	cout << "--> StEventSummary quasi-histogram -> energy vs. phi" << endl;
	for (k=0; k<evtsum->numberOfBins(); k++) {
	    cout << k << "\t[" << evtsum->lowerEdgePhiBin(k)
		 << " - "      << evtsum->upperEdgePhiBin(k)
		 << "] : \t"   << evtsum->energyInPhiBin(k) << endl;
	}
            cout << k << "\t[" << evtsum->lowerEdgeEtaBin(k)
                 << " - "      << evtsum->upperEdgeEtaBin(k)
                 << "] : \t"  <<  evtsum->tracksInEtaBin(k) << endl;
        }
	 << (void*) (mCurrentEvent->softwareMonitor())                  << endl;
        for (k=0; k<evtsum->numberOfBins(); k++) {
            cout << k << "\t[" << evtsum->lowerEdgePhiBin(k)
                 << " - "      << evtsum->upperEdgePhiBin(k)
                 << "] : \t"   << evtsum->tracksInPhiBin(k) << endl;
	cout << "---------------------------------------------------------" << endl;
	cout << "StTpcSoftwareMonitor at "
	     << (void*) (mCurrentEvent->softwareMonitor()->tpc())           << endl;
	cout << "---------------------------------------------------------" << endl;
	if (mCurrentEvent->softwareMonitor()->tpc())
	    mCurrentEvent->softwareMonitor()->tpc()->Dump();
	
	cout << "---------------------------------------------------------" << endl;
	cout << "StSvtSoftwareMonitor at "
	     << (void*) (mCurrentEvent->softwareMonitor()->svt())           << endl;
	cout << "---------------------------------------------------------" << endl;
	if (mCurrentEvent->softwareMonitor()->svt())
	    mCurrentEvent->softwareMonitor()->svt()->Dump();
	
	cout << "---------------------------------------------------------" << endl;
	cout << "StFtpcSoftwareMonitor at "
	     << (void*) (mCurrentEvent->softwareMonitor()->ftpc())          << endl;
	cout << "---------------------------------------------------------" << endl;
	if (mCurrentEvent->softwareMonitor()->ftpc())
	    mCurrentEvent->softwareMonitor()->ftpc()->Dump();
	
	cout << "---------------------------------------------------------" << endl;
	cout << "StEmcSoftwareMonitor at "
	     << (void*) (mCurrentEvent->softwareMonitor()->emc())           << endl;
	cout << "---------------------------------------------------------" << endl;
	if (mCurrentEvent->softwareMonitor()->emc())
	    mCurrentEvent->softwareMonitor()->emc()->Dump();
	
	cout << "---------------------------------------------------------" << endl;
	cout << "StRichSoftwareMonitor at "
	     << (void*) (mCurrentEvent->softwareMonitor()->rich())          << endl;
	cout << "---------------------------------------------------------" << endl;
	if (mCurrentEvent->softwareMonitor()->rich())
	    mCurrentEvent->softwareMonitor()->rich()->Dump();
	
	cout << "---------------------------------------------------------" << endl;
	cout << "StCtbSoftwareMonitor at "
	     << (void*) (mCurrentEvent->softwareMonitor()->ctb())           << endl;
	cout << "---------------------------------------------------------" << endl;
	if (mCurrentEvent->softwareMonitor()->ctb())
	    mCurrentEvent->softwareMonitor()->ctb()->Dump();
	
	cout << "---------------------------------------------------------" << endl;
	cout << "StL3SoftwareMonitor at "
	     << (void*) (mCurrentEvent->softwareMonitor()->l3())            << endl;
	cout << "---------------------------------------------------------" << endl;
	if (mCurrentEvent->softwareMonitor()->l3())
	    mCurrentEvent->softwareMonitor()->l3()->Dump();
	
	cout << "---------------------------------------------------------" << endl;
	cout << "StGlobalSoftwareMonitor at "
	     << (void*) (mCurrentEvent->softwareMonitor()->global())        << endl;
	cout << "---------------------------------------------------------" << endl;
	if (mCurrentEvent->softwareMonitor()->global())
	    mCurrentEvent->softwareMonitor()->global()->Dump();
        
        cout << "---------------------------------------------------------" << endl;
        cout << "StRichSoftwareMonitor at "
             << (void*) (mCurrentEvent->softwareMonitor()->rich())          << endl;
	 << (void*) (mCurrentEvent->l0Trigger())                        << endl;
        if (mCurrentEvent->softwareMonitor()->rich())
            mCurrentEvent->softwareMonitor()->rich()->Dump();
        
        cout << "---------------------------------------------------------" << endl;
        cout << "StCtbSoftwareMonitor at "
	 << (void*) (mCurrentEvent->triggerDetectorCollection())        << endl;
        cout << "---------------------------------------------------------" << endl;
        if (mCurrentEvent->softwareMonitor()->ctb())
	mCurrentEvent->triggerDetectorCollection()->Dump();
        
        cout << "---------------------------------------------------------" << endl;
	cout << "---------------------------------------------------------" << endl;
	cout << "StCtbTriggerDetector"                                      << endl;
	cout << "---------------------------------------------------------" << endl;
	mCurrentEvent->triggerDetectorCollection()->ctb().Dump();

	cout << "---------------------------------------------------------" << endl;
	cout << "StMwcTriggerDetector"                                      << endl;
	cout << "---------------------------------------------------------" << endl;
	mCurrentEvent->triggerDetectorCollection()->mwc().Dump();

	cout << "---------------------------------------------------------" << endl;
	cout << "StVpdTriggerDetector"                                      << endl;
	cout << "---------------------------------------------------------" << endl;
	mCurrentEvent->triggerDetectorCollection()->vpd().Dump();

	cout << "---------------------------------------------------------" << endl;
	cout << "StZdcTriggerDetector"                                      << endl;
	cout << "---------------------------------------------------------" << endl;
	mCurrentEvent->triggerDetectorCollection()->zdc().Dump();

    cout << "---------------------------------------------------------" << endl;
    cout << "StTriggerDetectorCollection at "
         << (void*) (mCurrentEvent->triggerDetectorCollection())        << endl;
    cout << "---------------------------------------------------------" << endl;
    if (mCurrentEvent->triggerDetectorCollection())
        mCurrentEvent->triggerDetectorCollection()->Dump();
	 << mCurrentEvent->trackDetectorInfo().size() << endl;
    if (mCurrentEvent->triggerDetectorCollection()) {
        cout << "---------------------------------------------------------" << endl;
	cout << "---------------------------------------------------------" << endl;
	cout << "StTrackDetectorInfo at "
	     << (void*) mCurrentEvent->trackDetectorInfo()[0]               << endl;
	cout << "---------------------------------------------------------" << endl;
	mCurrentEvent->trackDetectorInfo()[0]->Dump();
        cout << "StMwcTriggerDetector"                                      << endl;
        cout << "---------------------------------------------------------" << endl;
        mCurrentEvent->triggerDetectorCollection()->mwc().Dump();

        cout << "---------------------------------------------------------" << endl;
        cout << "StVpdTriggerDetector"                                      << endl;
        cout << "---------------------------------------------------------" << endl;
        mCurrentEvent->triggerDetectorCollection()->vpd().Dump();

	 << mCurrentEvent->trackNodes().size() << endl;
        cout << "StZdcTriggerDetector"                                      << endl;
        cout << "---------------------------------------------------------" << endl;
        mCurrentEvent->triggerDetectorCollection()->zdc().Dump();
    }
	     << mCurrentEvent->trackNodes()[0]->entries() << endl;
	cout << "---------------------------------------------------------" << endl;
	cout << "StTrackNode at "
	     << (void*) mCurrentEvent->trackNodes()[0]                      << endl;
	cout << "---------------------------------------------------------" << endl;
	mCurrentEvent->trackNodes()[0]->Dump();
	for (i=0; i<mCurrentEvent->trackNodes()[0]->entries(); i++)
	    printTrackInfo(mCurrentEvent->trackNodes()[0]->track(i));
    if (mCurrentEvent->trackDetectorInfo().size()) {
        cout << "---------------------------------------------------------" << endl;
        cout << "StTrackDetectorInfo at "
             << (void*) mCurrentEvent->trackDetectorInfo()[0]               << endl;
        cout << "---------------------------------------------------------" << endl;
        mCurrentEvent->trackDetectorInfo()[0]->Dump();
    }
    
    cout << "---------------------------------------------------------" << endl;
	 << mCurrentEvent->numberOfPrimaryVertices() << endl;
    cout << "Dumping first element in collection only (if available). " << endl;
    cout << "All tracks in the first node are printed separately  "     << endl;
    cout << "after the node info.                                     " << endl;
	     << mCurrentEvent->primaryVertex()->numberOfDaughters() << endl;
	cout << "---------------------------------------------------------" << endl;
	cout << "StPrimaryVertex at "
	     << (void*) mCurrentEvent->primaryVertex()                      << endl;
	cout << "---------------------------------------------------------" << endl;
	mCurrentEvent->primaryVertex()->Dump();
	if (mCurrentEvent->primaryVertex()->numberOfDaughters())
	    printTrackInfo(mCurrentEvent->primaryVertex()->daughter(0));	    
        cout << "---------------------------------------------------------" << endl;
        cout << "StTrackNode at "
             << (void*) mCurrentEvent->trackNodes()[0]                      << endl;
        cout << "---------------------------------------------------------" << endl;
        mCurrentEvent->trackNodes()[0]->Dump();
        for (i=0; i<mCurrentEvent->trackNodes()[0]->entries(); i++)
            printTrackInfo(mCurrentEvent->trackNodes()[0]->track(i));
	 << mCurrentEvent->v0Vertices().size() << endl;

    cout << "---------------------------------------------------------" << endl;
	cout << "---------------------------------------------------------" << endl;
	cout << "StV0Vertex at "
	     << (void*) mCurrentEvent->v0Vertices()[0]                      << endl;
	cout << "---------------------------------------------------------" << endl;
	mCurrentEvent->v0Vertices()[0]->Dump();	
    cout << "collection size = "
         << mCurrentEvent->numberOfPrimaryVertices() << endl;
    
    if (mCurrentEvent->numberOfPrimaryVertices()) {
        cout << "# primary tracks in first element = "
             << mCurrentEvent->primaryVertex()->numberOfDaughters() << endl;
        cout << "---------------------------------------------------------" << endl;
	 << mCurrentEvent->xiVertices().size() << endl;
             << (void*) mCurrentEvent->primaryVertex()                      << endl;
        cout << "---------------------------------------------------------" << endl;
	cout << "---------------------------------------------------------" << endl;
	cout << "StXiVertex at "
	     << (void*) mCurrentEvent->xiVertices()[0]                      << endl;
	cout << "---------------------------------------------------------" << endl;
	mCurrentEvent->xiVertices()[0]->Dump();	
    cout << "---------------------------------------------------------" << endl;
    cout << "StSPtrVecV0Vertex"                                         << endl;
    cout << "Dumping first element in collection only (if available). " << endl;
    cout << "---------------------------------------------------------" << endl;
    cout << "collection size = "
         << mCurrentEvent->v0Vertices().size() << endl;
    
	 << mCurrentEvent->kinkVertices().size() << endl;
        cout << "---------------------------------------------------------" << endl;
        cout << "StV0Vertex at "
	cout << "---------------------------------------------------------" << endl;
	cout << "StKinkVertex at "
	     << (void*) mCurrentEvent->kinkVertices()[0]                    << endl;
	cout << "---------------------------------------------------------" << endl;
	mCurrentEvent->kinkVertices()[0]->Dump();	
    cout << "---------------------------------------------------------" << endl;
    cout << "StSPtrVecXiVertex"                                         << endl;
    cout << "Dumping first element in collection only (if available). " << endl;
    cout << "---------------------------------------------------------" << endl;
    cout << "collection size = "
         << mCurrentEvent->xiVertices().size() << endl;
    
    if (mCurrentEvent->xiVertices().size()) {
        cout << "---------------------------------------------------------" << endl;
        cout << "StXiVertex at "
	nhits = tpcColl->numberOfHits();
	cout << "# of hits in collection = " << nhits << endl;
	gotOneHit = kFALSE;
	for (k=0; !gotOneHit && k<tpcColl->numberOfSectors(); k++)
	    for (j=0; !gotOneHit && j<tpcColl->sector(k)->numberOfPadrows(); j++)
		if (tpcColl->sector(k)->padrow(j)->hits().size()) {
		    tpcColl->sector(k)->padrow(j)->hits()[0]->Dump();
		    gotOneHit = kTRUE;
		}
    cout << "collection size = "
         << mCurrentEvent->kinkVertices().size() << endl;
        nhits = tpcColl->numberOfHits();
        cout << "# of hits in collection = " << nhits << endl;
        gotOneHit = kFALSE;
	for (k=0; !gotOneHit && k<svtColl->numberOfLayers(); k++)
	    for (j=0; !gotOneHit && j<svtColl->layer(k)->numberOfLadders(); j++)
		for (i=0; !gotOneHit && i<svtColl->layer(k)->ladder(j)->numberOfWafers(); i++)
		    if (svtColl->layer(k)->ladder(j)->wafer(i)->hits().size()) {
	gotOneHit = kFALSE;
	for (k=0; !gotOneHit && k<svtColl->numberOfBarrels(); k++)
	track->geometry()->Dump();
		    if (svtColl->barrel(k)->ladder(j)->wafer(i)->hits().size()) {
			svtColl->barrel(k)->ladder(j)->wafer(i)->hits()[0]->Dump();
	track->detectorInfo()->Dump();
    if (svtColl) {
        nhits = svtColl->numberOfHits();
	track->node()->Dump();
	cout << "collection size = " << richPixels->size() << endl;
	richPixels->Dump();
	
	if (richPixels->size()) {
	    cout << "---------------------------------------------------------" << endl;
	    cout << "StRichPixel (generated from first word in collection)    " << endl;
	    cout << "---------------------------------------------------------" << endl;
	    richPixels->pixel(0).Dump();
	}	
    }    
        if (richPixels->size()) {
            cout << "---------------------------------------------------------" << endl;

    //
    //   Info from some tables for comparisons.
    //   Only tables with varying # of rows are listed.
    //
    cout << "*********************************************************" << endl;
    cout << "*                   Table Information                   *" << endl;
	 << ") at " << (void*) track                                    << endl;
    cout << "globtrk:    ";
	track->Dump();
    cout << "tpt:        ";
	
	cout << "---> StTrack -> StGeometry ("<< track->geometry()->GetName()
	     << ") at " << (void*) (track->geometry()) << endl;
	if (track->geometry()) track->geometry()->Dump();

	cout << "---> StTrack -> StDetectorInfo at "
	     << (void*) (track->detectorInfo()) << endl;
	if (track->detectorInfo()) track->detectorInfo()->Dump();

	cout << "---> StTrack -> StTrackNode at "
	     << (void*) (track->node()) << endl;
	if (track->node()) track->node()->Dump();

	cout << "---> StTrack -> StPidTraits ("
	     << (track->pidTraits().size() ? 1 : 0 ) << " of "
	     <<  track->pidTraits().size() << " entries shown)" << endl;
	if (track->pidTraits().size()) track->pidTraits()[0]->Dump();

void
StEventMaker::printTrackInfo(StTrack* track)
{
    cout << "---------------------------------------------------------" << endl;
    cout << "StTrack (" << (track ? track->GetName() : "n/a")
         << ") at " << (void*) track                                    << endl;
    cout << "---------------------------------------------------------" << endl;
    if (track) {
        track->Dump();
        cout << "covariantMatrix():" << track->fitTraits().covariantMatrix() << endl;
        
        cout << "---> StTrack -> StGeometry ("<< track->geometry()->GetName()
             << ") at " << (void*) (track->geometry()) << endl;
        if (track->geometry()) track->geometry()->Dump();

        cout << "---> StTrack -> StDetectorInfo at "
             << (void*) (track->detectorInfo()) << endl;
        if (track->detectorInfo()) track->detectorInfo()->Dump();

        cout << "---> StTrack -> StTrackNode at "
             << (void*) (track->node()) << endl;
        if (track->node()) track->node()->Dump();

        cout << "---> StTrack -> StPidTraits ("
             << (track->pidTraits().size() ? 1 : 0 ) << " of "
             <<  track->pidTraits().size() << " entries shown)" << endl;
        if (track->pidTraits().size()) track->pidTraits()[0]->Dump();
    }
}

