/*************************************************************************** 
 *
 * $Id: StEventMaker.cxx,v 2.0 1999/11/04 19:03:00 ullrich Exp $
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
 * Revision 2.0  1999/11/04 19:03:00  ullrich
 * Revised to build new StEvent version
 *
 * Revision 2.27  2000/05/26 11:36:19  ullrich
 * Default is to NOT print event info (doPrintEventInfo  = kFALSE).
 *
 * Revision 2.26  2000/05/26 11:34:08  ullrich
 * Skip the attempt of creating an instance of StRun in case
 * no dst dataset is available.
 *
 * Removed remaining pieces of the RICH pixel table.
 *
 * Revision 2.24  2000/05/24 15:48:15  ullrich
 * Instance of StEvent now also created if no DST dataset
 * is available.
 *
#include <new.h>
 * Revision 2.23  2000/05/22 21:53:41  ullrich
 * No more copying of RICH tables. RICH now writes directly
 * to StEvent. printEventInfo() and makeEvent() modified.
 *
 * Revision 2.22  2000/04/26 20:29:13  ullrich
 * Create instance of StEvent not StBrowsableEvent.
 * Added further checks for case were tables exist but have
#include "TClass.h"
#include "StTrack.h"
 * zero length. Added for primary and global tracks.
#include "St_ObjectSet.h"
#include "St_DataSetIter.h"
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
    doLoadTpcHits  = kTRUE;
    doLoadFtpcHits = kTRUE;
    doLoadSvtHits  = kTRUE;
 * With Victors help now possible to read the dst_summary_param
#if defined(__SUNPRO_CC)
    doPrintRunInfo    = kTRUE;  // TMP, set to fFALSE later
    doPrintEventInfo  = kTRUE;  // TMP, set to fFALSE later
    doPrintMemoryInfo = kTRUE;  // TMP, set to fFALSE later
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
static const char rcsid[] = "$Id: StEventMaker.cxx,v 2.0 1999/11/04 19:03:00 ullrich Exp $";
 * Delete hit if it cannot be added to collection.
 *
 * Revision 2.3  1999/11/08 17:04:59  ullrich
 * Hits now allocated individually.
 *
 * Revision 2.2  1999/11/05 18:35:54  ullrich
 * Added methods and flags for debugging and monitoring.
 * Revision 2.0  1999/11/04 19:03:00  ullrich
 * Revised to build new StEvent version
 *
 **************************************************************************/
#include <vector>
#include <algorithm>
    //
    
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
        mCurrentRun = 0;

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
    //        Load trigger & trigger detector data
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
    unsigned int               id, k;
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
    dst_mon_soft_emc_st*  dstSoftMonEmc    = mEventManager->returnTable_dst_mon_soft_emc(nrows);
    dst_mon_soft_ftpc_st* dstSoftMonFtpc   = mEventManager->returnTable_dst_mon_soft_ftpc(nrows);
    dst_mon_soft_glob_st* dstSoftMonGlobal = mEventManager->returnTable_dst_mon_soft_glob(nrows);
    dst_mon_soft_l3_st*   dstSoftMonL3     = mEventManager->returnTable_dst_mon_soft_l3(nrows);
    dst_mon_soft_rich_st* dstSoftMonRich   = mEventManager->returnTable_dst_mon_soft_rich(nrows);
    dst_mon_soft_svt_st*  dstSoftMonSvt    = mEventManager->returnTable_dst_mon_soft_svt(nrows);
    
    mCurrentEvent->setSoftwareMonitor(new StSoftwareMonitor(dstSoftMonTpc,
                                                            dstSoftMonGlobal,
        vecPrimaryTracks[dstPrimaryTracks[i].id] = ptrack;
        vecPrimaryVertexId[dstPrimaryTracks[i].id] = dstPrimaryTracks[i].id_start_vertex/10;
    //
    //        Load trigger & trigger detector data
    //
            ptrack->setDetectorInfo(ptrack->detectorInfo());
    StSPtrVecV0Vertex          &v0Vertices   = mCurrentEvent->v0Vertices();
    //        Find and setup primary vertices (if any).
    StSPtrVecKinkVertex        &kinkVertices = mCurrentEvent->kinkVertices();
    StTrackDetectorInfo        *info;
    StTrackNode                *node;
    
    int                        i;
    
    //
    //  Create global tracks.
    //  Since the further setup depends on the id of the tracks
    //
    StGlobalTrack *gtrack = 0;
    dst_track_st *dstGlobalTracks = mEventManager->returnTable_dst_globtrk(nrows);
    maxId = dstPrimaryTracks ? max((long)dstPrimaryTracks[nrows-1].id, nrows) : 0;
	    ptrack->setDetectorInfo(info);
    StVector(unsigned int)    vecPrimaryVertexId(maxId+1, 0U);
    nfailed = 0;
    
    //        Setup V0 vertices
        id = dstPrimaryTracks[i].id_start_vertex ? dstPrimaryTracks[i].id_start_vertex/10 : 0;
        if (!id) {
                if (vecPrimaryTracks[k] &&
                    vecPrimaryVertexId[k] == (unsigned int) dstVertices[i].id)
        }
    if (nfailed) 
	gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
			    << " primary tracks, no valid primary vertex found." << endm;
        if (id < vecGlobalTracks.size() && vecGlobalTracks[id]) {
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
    //  Allocating small objects is not very efficient and that's why
    //  we are using here the placement new() operator. The memory for
    //  each kind of hit gets allocated in one chunk.
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
        char *buf;
	    id = dstXiVertices[i].id_v0 - 1;
	    if (id < v0Vertices.size()) xi->setV0Vertex(v0Vertices[id]);
	    id  = dstXiVertices[i].id_b;       // no -1 here 
	    if (id < vecGlobalTracks.size()) xi->addDaughter(vecGlobalTracks[id]);
	    xiVertices.push_back(xi);
	}

	    nfailed++;
            buf   = new char[sizeof(StTpcHit)*index[kTpcId].second];
    }
        int  nfailed;
                tpcHit = new(reinterpret_cast<void*>(buf)) StTpcHit(dstPoints[i]);
                buf += sizeof(StTpcHit);
    if (nfailed)
    if (doLoadTpcHits || doLoadFtpcHits || doLoadSvtHits) {
                            << " V0 vertices, no valid id_vertex." << endm;
                tpcHitColl->addHit(tpcHit);
                id = dstPoints[i].id_track;
                if (id < vecGlobalTracks.size() && vecGlobalTracks[id])
                    vecGlobalTracks[id]->detectorInfo()->addHit(tpcHit);
                else if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id])
                    vecPrimaryTracks[id]->detectorInfo()->addHit(tpcHit);
	    if (id < vecGlobalTracks.size()) kink->addDaughter(vecGlobalTracks[id]);
		else
		    delete tpcHit; 
	else
	    nfailed++;

    }
    if (nfailed)
            buf   = new char[sizeof(StSvtHit)*index[kSvtId].second];
        gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
                            << " Xi vertices, invalid foreign key to vertex table." << endm;
                svtHit = new(reinterpret_cast<void*>(buf)) StSvtHit(dstPoints[i]);
                buf += sizeof(StSvtHit);
    //  Setup kinks
		    if (id < vecGlobalTracks.size() && vecGlobalTracks[id])
			vecGlobalTracks[id]->detectorInfo()->addHit(tpcHit);
                svtHitColl->addHit(svtHit);
                id = dstPoints[i].id_track;
                if (id < vecGlobalTracks.size() && vecGlobalTracks[id])
                    vecGlobalTracks[id]->detectorInfo()->addHit(svtHit);
                else if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id])
                    vecPrimaryTracks[id]->detectorInfo()->addHit(svtHit);
        }
		else
		    delete svtHit; 		    

                            << " kink vertices, no valid id_vertex." << endm;
    //  we have to scan them all and get the first index and the total
    //  number of those which have to be loaded.
		    id = dstPoints[i].id_track;
            buf   = new char[sizeof(StFtpcHit)*index[kFtpcWestId].second];
		    if (id < vecGlobalTracks.size() && vecGlobalTracks[id]) {
			info = vecGlobalTracks[id]->detectorInfo();
			info->addHit(tpcHit);
                ftpcHit = new(reinterpret_cast<void*>(buf)) StFtpcHit(dstPoints[i]);
                buf += sizeof(StFtpcHit);
		    if (id < vecGlobalTracks.size() && vecGlobalTracks[id])
			vecGlobalTracks[id]->detectorInfo()->addHit(svtHit);
		    else if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id])
                ftpcHitColl->addHit(ftpcHit);
                id = dstPoints[i].id_track;
                if (id < vecGlobalTracks.size() && vecGlobalTracks[id])
                    vecGlobalTracks[id]->detectorInfo()->addHit(ftpcHit);
                else if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id])
            buf   = new char[sizeof(StFtpcHit)*index[kFtpcEastId].second];
                    vecPrimaryTracks[id]->detectorInfo()->addHit(ftpcHit);
            StSvtHit *svtHit;
		else
                ftpcHit = new(reinterpret_cast<void*>(buf)) StFtpcHit(dstPoints[i]);
                buf += sizeof(StFtpcHit);
                svtHit = new StSvtHit(dstPoints[i]);
                if (svtHitColl->addHit(svtHit)) {
                    id = dstPoints[i].id_track;
                ftpcHitColl->addHit(ftpcHit);
                id = dstPoints[i].id_track;
                if (id < vecGlobalTracks.size() && vecGlobalTracks[id])
                    vecGlobalTracks[id]->detectorInfo()->addHit(ftpcHit);
                else if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id])
                    vecPrimaryTracks[id]->detectorInfo()->addHit(ftpcHit);
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

