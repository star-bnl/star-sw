/*!
   StEventMaker

   Description: Setup of StEvent

   Author: Original version by T. Wenaus, BNL
           Revised version for new StEvent by T. Ullrich, Yale

*/
 
#include <vector>
#include <algorithm>
#include <utility>
#include <cstdlib>
#include "TError.h"
#include "StEventMaker/StEventMaker.h"
#include "StEventMaker/StRootEventManager.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StMemoryInfo.hh"
#include "StTimer.hh"
#include "StGlobals.hh"
#include "StEvtHddr.h"
#include "StTpcDb/StTpcDb.h"
#include "StDetectorDbMaker/StDetectorDbRichScalers.h"
#include "StDetectorDbMaker/StDetectorDbBeamInfo.h"
#include "StDetectorDbMaker/StDetectorDbTriggerID.h"
#include "StDAQMaker/StDAQReader.h"
#include "StPrompt.hh"
#include "StMath.hh"
#include <typeinfo>
#include <map>

#if !defined(ST_NO_NAMESPACES)
using std::vector;
using std::max;
using std::pair;
using std::map;
#endif

#if defined(ST_NO_TEMPLATE_DEF_ARGS)
#define StVector(T) vector<T, allocator<T> >
#else
#define StVector(T) vector<T>
#endif

static const char rcsid[] = "$Id: StEventMaker.cxx,v 2.84 2008/01/29 18:45:01 perev Exp $";

//______________________________________________________________________________
static int badDstTrack(dst_track_st *t)
{
    static int r0Len = 0;
    static int r0outLen = 0;
    if (!r0Len) {
        r0Len    = &(t->impact  ) - &(t->r0   ) + 1;
        r0outLen = &(t->invptout) - &(t->r0out) + 1;
    }
    if (t->iflag < 0)       			return 1;
    if ( StMath::tooBig(&(t->r0   ),r0Len   )) 	return 2; 
    // if ( StMath::tooBig(&(t->r0out),r0outLen)) 	return 3; 
    if ( StMath::tooBig(&(t->r0out),r0outLen))   {//fill by senseless but not carzy
        for (int i=0;i<r0outLen;i++) {(&(t->r0out))[i]=1.e+5;}}
    return 0;
}

// //______________________________________________________________________________
// static int badDstVertex(dst_vertex_st *t)
// {
//     static int xLen = 0;
//     if (!xLen) { xLen    = &(t->chisq[1]) - &(t->x) + 1;}
//     if (t->iflag <= 0)       			return 1;
//     if ( StMath::tooBig(&(t->x   ),xLen   )) 	return 2; 
//     if (t->chisq[0] < 0.)			return 3;
//     if (t->chisq[1] < 0.)			return 4;
//     return 0;
// }
//______________________________________________________________________________
static int badDstV0Vertex(dst_v0_vertex_st *t)
{
    static int dcanLen = 0;
    if (!dcanLen) { dcanLen = &(t->pos_pz) - &(t->dcan) + 1;}
    if ( StMath::tooBig(&(t->dcan),dcanLen)) 	return 1; 
    return 0;
}
//______________________________________________________________________________
static int badDstXiVertex(dst_xi_vertex_st *t)
{
    static int dcaLen = 0;
    if (!dcaLen) { dcaLen = &(t->pz_b) - &(t->dca) + 1;}
    if ( StMath::tooBig(&(t->dca),dcaLen)) 	return 1; 
    return 0;
}
//______________________________________________________________________________
static int badDstTkfVertex(dst_tkf_vertex_st *t)
{
    static int dcaLen = 0;
    if (!dcaLen) { dcaLen = &(t->theta_cm) - &(t->dca) + 1;}
    if ( StMath::tooBig(&(t->dca),dcaLen)) 	return 1; 
    return 0;
}

//______________________________________________________________________________
ClassImp(StEventMaker)
    //______________________________________________________________________________
    
    StEventMaker::StEventMaker(const char *name, const char *title) : StMaker(name)
{
    if(title) SetTitle(title);
    mEventManager = new StRootEventManager();
    mEventManager->setMaker(this);
    mCurrentEvent = 0;
    doLoadTpcHits     = kTRUE;
    doLoadFtpcHits    = kTRUE;
    doLoadSvtHits     = kTRUE;
    doLoadSsdHits     = kTRUE;
    doLoadTptTracks   = kFALSE;
    doLoadEstTracks   = kTRUE;
    doPrintEventInfo  = kFALSE;
    doPrintMemoryInfo = kFALSE;
    doPrintCpuInfo    = kFALSE;
    mCreateEmptyInstance = kFALSE;
}

StEventMaker::~StEventMaker()
{
    delete mEventManager;
}

void
StEventMaker::Clear(const char*)
{
    mCurrentEvent=0;
    StMaker::Clear();
}

StEventManager*
StEventMaker::eventManager() {return mEventManager;};

StEvent*
StEventMaker::event() { return mCurrentEvent;};

void
StEventMaker::setEventManager(StEventManager* mgr)
{
    mEventManager = mgr;
}

Int_t
StEventMaker::Init()
{
    return StMaker::Init();
}

Int_t
StEventMaker::Make()
{
    //
    // In this method we actually do not create anything but call
    // other methods which do that for us:
    // makeEvent()    creates StEvent and all its dependent classes
    //
    // Since this Maker should also work without any 'dst' dataset
    // we create *always* an instance of StEvent, even if it is empty.
    //
    
    //
    //  Init timing and memory snapshots
    //
    StTimer timer;
    if (doPrintCpuInfo) timer.start();
    if (doPrintMemoryInfo) StMemoryInfo::instance()->snapshot();
    
    //
    //  The current event is deleted automatically before every
    //  new event. It is added by using AddData().
    //  We do not need to delete it ourself.
    //
    //  If no DST dataset is available we cannot setup StEvent
    //  properly. Nevertheless we will create an empty instance.
    //
    int status = mEventManager->openEvent("dst");
    if (status == oocError) {
        gMessMgr->Warning() << "StEventMaker::Make(): cannot open dataset 'dst'." << endm;
        mCreateEmptyInstance = kTRUE;
    }
    else
        mCreateEmptyInstance = kFALSE;
    
    //
    //  Setup the event (StEvent and all subclasses)
    //
    status = makeEvent();
    if (status != kStOK)
        gMessMgr->Warning() << "StEventMaker::Make(): error in makeEvent(), no StEvent object created." << endm;
    
    mEventManager->closeEvent();
    
    //
    //  Print out some timing, memory usage and StEvent
    //  info if requested
    //
    if (doPrintEventInfo) printEventInfo();
    if (doPrintMemoryInfo) {
        StMemoryInfo::instance()->snapshot();
        StMemoryInfo::instance()->print();
    }
    if (doPrintCpuInfo) {
        timer.stop();
        LOG_DEBUG  << "CPU time for StEventMaker::Make(): "
             << timer.elapsedTime() << " sec\n" << endm;
    }
    
    return status;
}

StEvent*
StEventMaker::getStEventInstance()
{
    StEvent* exist = (StEvent*) GetInputDS("StEvent");
    if (exist) {
        gMessMgr->Info() << "StEventMaker::getStEventInstance(): existing instance found, no new object created." << endm;
        return exist;
    }
    else {
        exist = new StEvent;
	AddData(exist);
        return exist;
    }
}

void
StEventMaker::fillOuterTrackGeometry(StTrack* track, const dst_track_st& t)
{
    //
    // Here we have to require that the 'original' geometry
    // already exist. We cannot use the StTrackGeometry constructor
    // which takes the table as argument since we have now two
    // versions of track parameters stored in the tables.
    //
    if (!(track && track->geometry())) return;
    
    // Things which don't change
    short q = track->geometry()->charge();
    short h = track->geometry()->helicity();
    
    // New origin, psi and dip angle
    StThreeVectorF o(t.r0out*cos(t.phi0out*degree),
		 t.r0out*sin(t.phi0out*degree),
		 t.z0out);
    double psi = t.psiout*degree;
    double dip = atan(t.tanlout);
    
    // New momentum
    double invptout = t.invptout;
    double tanlout  = t.tanlout;
    //	bad idea, but what to do? Waiting ittf (VP)
    if (fabs(invptout)<1.e-10) {invptout = t.invpt;tanlout=t.tanl;}
    double pt = 1./invptout;
    double pz = pt*t.tanlout;
    StThreeVectorF p(pt*cos(psi), pt*sin(psi), pz);
    
    // New curvature
    double c = track->geometry()->curvature()*
        track->geometry()->momentum().perp()*invptout;
    
    // Assign to track
    track->setOuterGeometry(new StHelixModel(q, psi, c, dip, o, p, h));
}

Int_t
StEventMaker::makeEvent()
{
    //
    //  In case there's nothing to fill (no DST dataset) we
    //  create an empty instance only. This is OK in this
    //  case and therefore we do not return a warning or an
    //  error message.
    //
    if (mCreateEmptyInstance) {
        mCurrentEvent = getStEventInstance();
        return kStOK;
    }
    
    //
    //  Create AND setup/fill StEvent.
    //
    long nrows;
    
    //
    //  Event header and event summary
    //
    event_header_st* dstEventHeader = mEventManager->returnTable_event_header(nrows);
    dst_event_summary_st* dstEventSummary = mEventManager->returnTable_dst_event_summary(nrows);
    
    
    //
    //  Create instance of StEvent, using whatever we got so far.
    //
    mCurrentEvent = getStEventInstance();
    if (dstEventHeader  && !mCurrentEvent->info())
        mCurrentEvent->setInfo(new StEventInfo(*dstEventHeader));
    if (dstEventSummary && !mCurrentEvent->summary())
        mCurrentEvent->setSummary(new StEventSummary(*dstEventSummary));
    
    
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
    
    StSoftwareMonitor *mon = mCurrentEvent->softwareMonitor();
    if (!mon) mCurrentEvent->setSoftwareMonitor(mon = new StSoftwareMonitor());
    mon->setTpcSoftwareMonitor   (dstSoftMonTpc   );
    mon->setSvtSoftwareMonitor   (dstSoftMonSvt   );
    mon->setFtpcSoftwareMonitor  (dstSoftMonFtpc  );
    mon->setEmcSoftwareMonitor   (dstSoftMonEmc   );
    mon->setCtbSoftwareMonitor   (dstSoftMonCtb   );
    mon->setRichSoftwareMonitor  (dstSoftMonRich  );
    mon->setGlobalSoftwareMonitor(dstSoftMonGlobal);
    mon->setL3SoftwareMonitor    (dstSoftMonL3    );
    
    //
    //        Load trigger & trigger detector data
    //
    dst_TrgDet_st* dstTriggerDetectors = mEventManager->returnTable_dst_TrgDet(nrows);
    dst_L0_Trigger_st* dstL0Trigger    = mEventManager->returnTable_dst_L0_Trigger(nrows);
    dst_L1_Trigger_st* dstL1Trigger    = mEventManager->returnTable_dst_L1_Trigger(nrows);
    
    //
    // Get trgStructure structures from StTriggerDataMaker
    // and store them in StEvent. Note that they are need
    // pior to creating the old StTriggerDetectorCollection
    // since the latter uses StTriggerData at construction. 
    //
    if (!mCurrentEvent->triggerData()) {
        TObjectSet *os = (TObjectSet*)GetDataSet("StTriggerData");
        if (os) {
	  StTriggerData* pTrg = (StTriggerData*)os->GetObject();
	  assert(pTrg); 		// wrong, empty data
	  assert(os->IsOwner()); 	// wrong, data allready taken
	  os->DoOwner(0); 	        //change ownership
	  mCurrentEvent->setTriggerData(pTrg);
        }
    }
    
    //
    //  year 2001, 2002: use TrgDet tables
    //  year >= 2003: use info from StTriggerData
    //  Long term: get rid of StTriggerDetectorCollection
    //             and use StTriggerData
    //
    if (!mCurrentEvent->triggerDetectorCollection()) {
        if (mCurrentEvent->triggerData() && mCurrentEvent->triggerData()->year() >= 2003) 
	  mCurrentEvent->setTriggerDetectorCollection(new StTriggerDetectorCollection(*(mCurrentEvent->triggerData())));
        else if (dstTriggerDetectors) 
	  mCurrentEvent->setTriggerDetectorCollection(new StTriggerDetectorCollection(*dstTriggerDetectors));
    }
    
    StL0Trigger *l0t = mCurrentEvent->l0Trigger();
    if (!l0t) mCurrentEvent->setL0Trigger((l0t = new StL0Trigger()));
    if (mCurrentEvent->triggerData() && mCurrentEvent->triggerData()->year() >= 2003){
        l0t->set(mCurrentEvent->triggerData());
    }
    else {
        l0t->set(dstL0Trigger       );
        l0t->set(dstTriggerDetectors);
    }
    
    if (dstL0Trigger && dstL1Trigger && !mCurrentEvent->l1Trigger())
        mCurrentEvent->setL1Trigger(new StL1Trigger(*dstL0Trigger, *dstL1Trigger));
    
    //
    //  Trigger ID summary
    //
    Int_t idx;
    StTriggerIdCollection* triggerIdColl = mCurrentEvent->triggerIdCollection();
    if (!triggerIdColl) {
        mCurrentEvent->setTriggerIdCollection((triggerIdColl =
				       new StTriggerIdCollection()));
    }
    StTriggerId* trigId[3];
    triggerIdColl->setL1((trigId[0] = new StTriggerId()));
    triggerIdColl->setL2((trigId[1] = new StTriggerId()));
    triggerIdColl->setL3((trigId[2] = new StTriggerId()));
    
    St_DataSet *daqReaderSet=GetDataSet("StDAQReader");
    if (daqReaderSet) {
        
        StTrigSummary* trigSummary =
            ((StDAQReader*) (daqReaderSet->GetObject()))->getTrigSummary();
        if (!trigSummary) gMessMgr->Warning("StEventMaker: No StTrigSummary found");
        
        StDetectorDbTriggerID* dbTriggerId = StDetectorDbTriggerID::instance();
        if (!dbTriggerId) gMessMgr->Warning("StEventMaker: No StDetectorDbTriggerID found");
        
        if (trigSummary && dbTriggerId) {
	    
	    // The nominal is a pointer to one of the above. 
	    trigId[0]->setMask(trigSummary->L1summary[0]);
	    trigId[1]->setMask(trigSummary->L2summary[0]);
	    trigId[2]->setMask(trigSummary->L3summary[0]);
	    
	    
	    // Loop over trigger level
	    for(unsigned int trglevel=0 ; trglevel < 3 ; trglevel++){
		StTriggerId* whichTrig =  trigId[trglevel];
		
		// Loop over the triggers within this level
		for (unsigned int iTrg = 0; iTrg < dbTriggerId->getIDNumRows() ; iTrg++){
		    // Shift the mask by daqTrigId bits to examine that bit
		    if ( whichTrig->mask() &  (1 << (dbTriggerId->getDaqTrgId(iTrg)) )  ) {
			whichTrig->addTrigger(
			    dbTriggerId->getOfflineTrgId(iTrg),
			    dbTriggerId->getTrgVersion(iTrg),
			    dbTriggerId->getTrgNameVersion(iTrg),
			    dbTriggerId->getThreashVersion(iTrg),
			    dbTriggerId->getPsVersion(iTrg)
			    );
		    }
		}
		
		// Add in additional trigger ids to all levels; hack to fix prescale problem in runs 3-6
		for (unsigned int iTrg = 0; iTrg < dbTriggerId->getAdditionalTriggerIDNumRows() ; iTrg++){
		    // Shift the mask by daqTrigId bits to examine that bit
		    // Tweak for warnings; saved the run/event number as an unsigned int, but for some reason StEvent is int
		    int checkRun = dbTriggerId->getAdditionalTriggerIDRunNumber(iTrg);
		    int checkEvent = dbTriggerId->getAdditionalTriggerIDEventNumber(iTrg);

		    if (mCurrentEvent->runId() == checkRun &&
			mCurrentEvent->id() == checkEvent )
		    {
			    whichTrig->addTrigger(
				dbTriggerId->getAdditionalTriggerIDOfflineTrgId(iTrg),
				dbTriggerId->getAdditionalTriggerIDTrgVersion(iTrg),
				dbTriggerId->getAdditionalTriggerIDTrgNameVersion(iTrg),
				dbTriggerId->getAdditionalTriggerIDThreashVersion(iTrg),
				dbTriggerId->getAdditionalTriggerIDPsVersion(iTrg)
				);
		    }
		}
	    }
	    
	    // This just puts the pointer, not a deep copy
	    if ( (idx=dbTriggerId->getDefaultTriggerLevel() ) != kDbTriggerBadID ){
		triggerIdColl->setNominal(trigId[idx-1]);
	    }
	    // Now hack up the offline trigger ids for year 2006
	    
	   
	    if (mCurrentEvent->triggerData() && mCurrentEvent->runId()>7000000 && mCurrentEvent->runId()<8000000) {
		gMessMgr->Info("StEventMaker::Run 6, expanding L3 trigger id");
		
		// Hack for mapping of StDetectorDb to StL2TriggerResultType
		map<string,StL2TriggerResultType> mapDbToStL2TriggerResultType;
		
		mapDbToStL2TriggerResultType["l2Trg2006BEMCGammaPi"] = l2Trg2006BEMCGammaPi;
		mapDbToStL2TriggerResultType["l2Trg2006BEMCGammaPiRandom"] = l2Trg2006BEMCGammaPiRandom;
		mapDbToStL2TriggerResultType["l2Trg2006EEMCGammaPi"] = l2Trg2006EEMCGammaPi;
		mapDbToStL2TriggerResultType["l2Trg2006EEMCGammaPiRandom"] = l2Trg2006EEMCGammaPiRandom;
		mapDbToStL2TriggerResultType["l2Trg2006MonoJet"] = l2Trg2006MonoJet;
		mapDbToStL2TriggerResultType["l2Trg2006DiJet"] = l2Trg2006DiJet;
		mapDbToStL2TriggerResultType["l2Trg2006RandomJet"] = l2Trg2006RandomJet;

	
		// Do a deep copy of the l3 into l3Expanded
		StTriggerId *whichTrig = new StTriggerId(*triggerIdColl->l3());
		
		triggerIdColl->setL3Expanded(whichTrig);
		// Reset the nominal pointer
		triggerIdColl->setNominal(whichTrig);
		// Expand up the l3Expanded.
		for (unsigned int irow=0; irow<dbTriggerId->getTrigL3ExpandedNumRows(); ++irow) {
		    unsigned int oldtid = dbTriggerId->getTrigL3ExpandedL3TrgId(irow);
		    unsigned int newtid = dbTriggerId->getTrigL3ExpandedL3ExpandedTrgId(irow);
		   
		    
		    string testString = string(dbTriggerId->getTrigL3ExpandedL2TriggerResultType(irow));

		    
		    map<string,StL2TriggerResultType>::const_iterator p =
			mapDbToStL2TriggerResultType.find(
			    testString);
		    
		    if (p != mapDbToStL2TriggerResultType.end()) {
			StL2TriggerResultType l2Test = (*p).second;
			if (whichTrig->isTrigger(oldtid) &&
			    mCurrentEvent->triggerData()->isL2Triggered(l2Test)) {
			    
			    whichTrig->addTrigger(newtid,
						  whichTrig->version(oldtid),
						  whichTrig->nameVersion(oldtid),
						  whichTrig->thresholdVersion(oldtid),
						  whichTrig->prescaleVersion(oldtid)
				);
			}
		    }
		}
	    }
// We need to make a copy of the nominal to avoid problems in delete??
	    if (triggerIdColl->nominal()) triggerIdColl->setNominal(new StTriggerId(*(triggerIdColl->nominal())));
	}
    }
    
    
    else {
        gMessMgr->Warning("StEventMaker: No StDAQReader found");
    }
    
    
    
    
    //
    //  Some variables we need in the following
    //
    StSPtrVecTrackDetectorInfo &detectorInfo = mCurrentEvent->trackDetectorInfo();
    StSPtrVecTrackNode         &trackNodes   = mCurrentEvent->trackNodes();
    StSPtrVecV0Vertex          &v0Vertices   = mCurrentEvent->v0Vertices();
    StSPtrVecXiVertex          &xiVertices   = mCurrentEvent->xiVertices();
    StSPtrVecKinkVertex        &kinkVertices = mCurrentEvent->kinkVertices();
    StTrackDetectorInfo        *info;
    StTrackNode                *node;
    unsigned int               id, k, nfailed, nfailed2, idvtx;
    int                        i, h;
    int signOfField = mCurrentEvent->summary()->magneticField() < 0 ? -1 : 1;
    
    //
    //  Complete information in StEventInfo
    //
    StEventInfo *theInfo = mCurrentEvent->info();
    if (theInfo && dstTriggerDetectors) {
        theInfo->setBunchCrossingNumber(dstTriggerDetectors->bunchXing_lo,0);
        theInfo->setBunchCrossingNumber(dstTriggerDetectors->bunchXing_hi,1);
    }
    
    //
    //  Create global tracks.
    //  Since the further setup depends on the id of the tracks
    //  in dst_track we temporarily store the pointers in a vector
    //  (vecGlobalTracks) sorted according to their dst_track::id.
    //  This makes things a lot easier.
    //
    if (trackNodes.size())  return kStOK;
    
    dst_track_st *dstGlobalTracks = mEventManager->returnTable_dst_globtrk(nrows);
    if (!dstGlobalTracks) nrows = 0;
    int maxId=0,mxId=0;
    maxId = dstGlobalTracks && nrows>0 ? max((long)dstGlobalTracks[nrows-1].id, nrows) : 0;
    StGlobalTrack *gtrack = 0;
    StVector(StGlobalTrack*) vecGlobalTracks(maxId+101, gtrack);
    int bad;
    for (i=0; i<nrows; i++) {
        if ((bad=badDstTrack(dstGlobalTracks+i))) {
	  if (Debug())
	      Warning("makeEvent","global track rejected by %d",bad);
	  continue;
        }
        gtrack = new StGlobalTrack(dstGlobalTracks[i]);
        vecGlobalTracks[dstGlobalTracks[i].id] = gtrack;
        gtrack->setGeometry(new StHelixModel(dstGlobalTracks[i]));
        h =  gtrack->geometry()->charge()*signOfField > 0 ? -1 : 1;   //  h = -sign(q*B)
        gtrack->geometry()->setHelicity(h);
        fillOuterTrackGeometry(gtrack, dstGlobalTracks[i]);
        info = new StTrackDetectorInfo(dstGlobalTracks[i]);
        gtrack->setDetectorInfo(info);
        detectorInfo.push_back(info);
        node = new StTrackNode();
        node->addTrack(gtrack);          // node<->track association
        trackNodes.push_back(node);
    }
    
    //
    //  Create primary tracks.
    //  Like the global tracks, they are kept in a vector (vecPrimaryTracks)
    //  sorted according to their primary keys. Here we have to check
    //  if each primary track has a corresponding global track. If so we
    //  put it in the same node and let them share the same detector info,
    //  else we proceed as for the global tracks.
    //  Here we also have to temporarily save the primary vertex id to later
    //  assign the tracks to the right primary vertex in case there is more
    //  than one.
    //  A primary track is only stored if it has a valid primary vertex.
    //
    //  New: there's a slight problem with the detector info. The detector
    //       info might be different for the global and the referring primary
    //       track. But the hits are always the same (!) since the tables
    //       only allow a hit to reference 1 track. Hence hits always point
    //       to the global track. We have no reliable data on the hits of
    //       the primary track. All what might be different is the position of
    //       the first and last point which is stored in a seperate data member
    //       in StTrackDetectorInfo. In this case we have to create a new
    //       StTrackDetectorInfo object for the primary track.
    //
    StPrimaryTrack *ptrack = 0;
    dst_track_st *dstPrimaryTracks = mEventManager->returnTable_dst_primtrk(nrows);
    if (!dstPrimaryTracks) nrows = 0;
    mxId = dstPrimaryTracks && nrows>0 ? max((long) dstPrimaryTracks[nrows-1].id, nrows) : 0;
    if(mxId>maxId) maxId=mxId;
    StVector(StPrimaryTrack*) vecPrimaryTracks(maxId+1, ptrack);
    StVector(unsigned int)    vecPrimaryVertexId(maxId+101, 0U);
    nfailed = 0;
    
    for (i=0; i<nrows; i++) {
        if ((bad=badDstTrack(dstPrimaryTracks+i))) {
	  if (Debug()) Warning("makeEvent","primary track rejected by %d",bad);
	  nfailed++;
	  continue;
        }
        idvtx = dstPrimaryTracks[i].id_start_vertex ? dstPrimaryTracks[i].id_start_vertex/10 : 0;
        if (!idvtx) {
            nfailed++;
            continue;
        }
        ptrack = new StPrimaryTrack(dstPrimaryTracks[i]);
        ptrack->fitTraits().setPrimaryVertexUsedInFit(true); // NEW-tu Aug 13, 2004
        vecPrimaryTracks[dstPrimaryTracks[i].id]   = ptrack;
        vecPrimaryVertexId[dstPrimaryTracks[i].id] = idvtx;
        ptrack->setGeometry(new StHelixModel(dstPrimaryTracks[i]));
        h =  ptrack->geometry()->charge()*signOfField > 0 ? -1 : 1;   //  h = -sign(q*B)
        ptrack->geometry()->setHelicity(h);
        fillOuterTrackGeometry(ptrack, dstPrimaryTracks[i]);
        id = ptrack->key();
        if (id < vecGlobalTracks.size() && vecGlobalTracks[id]) {
            gtrack = vecGlobalTracks[id];
            info = gtrack->detectorInfo();
            //
            //  Check if the existing detector info is still ok for the
            //  primary track. If not we have to create a new one.
            //  See also comments above on existing problems with this.
            //
            StThreeVectorF firstPoint(dstPrimaryTracks[i].x_first);
            StThreeVectorF lastPoint(dstPrimaryTracks[i].x_last);
            if (firstPoint != info->firstPoint() || lastPoint != info->lastPoint()) {
                info = new StTrackDetectorInfo(dstPrimaryTracks[i]);
                detectorInfo.push_back(info);
            }
            ptrack->setDetectorInfo(info);
            node = gtrack->node();
            node->addTrack(ptrack);
        }
        else {
	  info = new StTrackDetectorInfo(dstPrimaryTracks[i]);
	  ptrack->setDetectorInfo(info);
	  detectorInfo.push_back(info);
	  node = new StTrackNode();
	  node->addTrack(ptrack);          // node<->track association
	  assert(node->track(primary) == ptrack);
	  trackNodes.push_back(node);
        }
    }
    if (nfailed)
        gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
                            << " primary tracks, no valid primary vertex found." << endm;
    
    //
    //  Create TPT tracks.
    //  TPT tracks are owned by the nodes as are global tracks.
    //  The scheme is, however, similar to primary tracks: checks if
    //  the referring node already exist and the detector info problem.
    //
    StTptTrack   *ttrack = 0;
    dst_track_st *dstTptTracks = 0;
    if (doLoadTptTracks) 
        dstTptTracks = mEventManager->returnTable_CpyTrk(nrows);
    if (!dstTptTracks) nrows = 0;
    mxId = dstTptTracks && nrows>0 ? max((long) dstTptTracks[nrows-1].id, nrows) : 0;
    if (mxId>maxId) maxId = mxId;
    StVector(StTptTrack*) vecTptTracks(maxId+101, ttrack);
    
    for (i=0; i<nrows; i++) {
        if ((bad=badDstTrack(dstTptTracks+i))) {
	  if (Debug())
	      Warning("makeEvent","tpt track rejected by %d",bad);
	  nfailed++;
	  continue;
        }
        ttrack = new StTptTrack(dstTptTracks[i]);
        vecTptTracks[dstTptTracks[i].id] = ttrack;
        ttrack->setGeometry(new StHelixModel(dstTptTracks[i]));
        h =  ttrack->geometry()->charge()*signOfField > 0 ? -1 : 1;   //  h = -sign(q*B)
        ttrack->geometry()->setHelicity(h);
        fillOuterTrackGeometry(ttrack, dstTptTracks[i]);
        id = ttrack->key();
        //
        //   Tpt tracks come in late. Good chance that there is already
        //   a node where it belongs to. If not we have to create one. 
        //   If a node already exist we have to check if the detector
        //   info available matches the one of the tpt track.
        //
        node = 0;
        info = 0;
        if (id < vecGlobalTracks.size() && vecGlobalTracks[id]) {
	  node = vecGlobalTracks[id]->node();
	  info = vecGlobalTracks[id]->detectorInfo();
        }
        else if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id]) {
	  node = vecPrimaryTracks[id]->node();
        }
        else {
	  node = new StTrackNode();
	  trackNodes.push_back(node);
        }
        
        //
        //  Check if the existing detector info is still ok for the
        //  tpt track. If not we have to create a new one.
        //
        if (info) {
	  StThreeVectorF firstPoint(dstTptTracks[i].x_first);
	  StThreeVectorF lastPoint(dstTptTracks[i].x_last);
	  if (firstPoint != info->firstPoint() || lastPoint != info->lastPoint()) info = 0;
        }
        
        if (!info) {
	  info = new StTrackDetectorInfo(dstTptTracks[i]);
	  detectorInfo.push_back(info);
        }
        
        ttrack->setDetectorInfo(info);
        node->addTrack(ttrack);          // node<->track association
    }
    
    //
    //  Create EST global tracks.
    //
    StEstGlobalTrack   *egtrack = 0;
    dst_track_st *dstEstGlobalTracks = 0;
    if (doLoadEstTracks) 
        dstEstGlobalTracks = mEventManager->returnTable_EstGlobal(nrows);
    if (!dstEstGlobalTracks) nrows = 0;
    mxId = dstEstGlobalTracks && nrows>0 ? max((long) dstEstGlobalTracks[nrows-1].id, nrows) : 0;
    if (mxId>maxId) maxId = mxId;
    StVector(StEstGlobalTrack*) vecEstGlobalTracks(maxId+101, egtrack);	
    nfailed = 0;
    nfailed2 = 0;
    
    for (i=0; i<nrows; i++) {
        if ((bad=badDstTrack(dstEstGlobalTracks+i))) {
	  if (Debug())
	      Warning("makeEvent","est global track rejected by %d",bad);
	  nfailed++;
	  continue;
        }
        //
        //   For each EST global track there must be already a node
        //   since EST doesn't create new tracks.
        //
        node = 0;
        info = 0;
        id = dstEstGlobalTracks[i].id;
        if (id < vecGlobalTracks.size() && vecGlobalTracks[id]) {
	  node = vecGlobalTracks[id]->node();
	  info = vecGlobalTracks[id]->detectorInfo();
        }
        if (!node) {
	  nfailed++;
	  continue;
        }
        
        //
        //   If there are no SVT hits on this track we do not store this track.
        //   It would be identical with the global which is already stored.
        //
        if ((dstEstGlobalTracks[i].n_point%10000)/1000 == 0) {
	  nfailed2++;
	  continue;
        }
        else {
	  info = new StTrackDetectorInfo(dstEstGlobalTracks[i]);
	  detectorInfo.push_back(info);
        }
        
        egtrack = new StEstGlobalTrack(dstEstGlobalTracks[i]);
        vecEstGlobalTracks[dstEstGlobalTracks[i].id] = egtrack;
        egtrack->setGeometry(new StHelixModel(dstEstGlobalTracks[i]));
        h =  egtrack->geometry()->charge()*signOfField > 0 ? -1 : 1;   //  h = -sign(q*B)
        egtrack->geometry()->setHelicity(h);
        fillOuterTrackGeometry(egtrack, dstEstGlobalTracks[i]);	
        egtrack->setDetectorInfo(info);
        node->addTrack(egtrack);          // node<->track association
    }
    if (nfailed)
        gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
                            << " EST global tracks, no referring track node found." << endm;
    if (nfailed2)
        gMessMgr->Warning() << "StEventMaker::makeEvent(): " << nfailed2
		        << " (out of " << nrows
		        << ") EST global tracks had no SVT hits and were not stored." << endm;
    
    //
    //  Create EST primary tracks.
    //
    StEstPrimaryTrack *eptrack = 0;
    dst_track_st *dstEstPrimaryTracks = 0;
    if (doLoadEstTracks) 
        dstEstPrimaryTracks = mEventManager->returnTable_EstPrimary(nrows);
    if (!dstEstPrimaryTracks) nrows = 0;
    mxId = dstEstPrimaryTracks && nrows>0 ? max((long) dstEstPrimaryTracks[nrows-1].id, nrows) : 0;
    if (mxId>maxId) maxId = mxId;
    StVector(StEstPrimaryTrack*) vecEstPrimaryTracks(maxId+1, eptrack);
    StVector(unsigned int)       vecEstPrimaryVertexId(maxId+1, 0U);
    nfailed = 0;
    
    for (i=0; i<nrows; i++) {
        if ((bad=badDstTrack(dstEstPrimaryTracks+i))) {
	  if (Debug())
	      Warning("makeEvent","estprimary track rejected by %d",bad);
	  nfailed++;
	  continue;
        }
        //
        //  Check for valid primary vertex
        //
        idvtx = dstEstPrimaryTracks[i].id_start_vertex ? dstEstPrimaryTracks[i].id_start_vertex/10 : 0;
        if (!idvtx) {
            nfailed++;
            continue;
        }
        
        //
        //   For each EST primary track there must be already a node
        //   since EST doesn't create new tracks. All EST globals
        //   with an SVT match should be in a node. There's a one-to
        //   -one/none relation between EST globals and EST primaries.
        //
        node = 0;
        info = 0;
        id = dstEstPrimaryTracks[i].id;
        if (id < vecEstGlobalTracks.size() && vecEstGlobalTracks[id]) {
	  node = vecEstGlobalTracks[id]->node();
	  info = vecEstGlobalTracks[id]->detectorInfo();
        }
        if (!node) continue;
        
        eptrack = new StEstPrimaryTrack(dstEstPrimaryTracks[i]);
        eptrack->fitTraits().setPrimaryVertexUsedInFit(true);    // NEW-tu Aug 13, 2004
        vecEstPrimaryTracks[dstEstPrimaryTracks[i].id]   = eptrack;
        vecEstPrimaryVertexId[dstEstPrimaryTracks[i].id] = idvtx;
        eptrack->setGeometry(new StHelixModel(dstEstPrimaryTracks[i]));
        h =  eptrack->geometry()->charge()*signOfField > 0 ? -1 : 1;   //  h = -sign(q*B)
        eptrack->geometry()->setHelicity(h);
        fillOuterTrackGeometry(eptrack, dstEstPrimaryTracks[i]);
        
        //
        //  We do not need a new detector info? 
        //  According to Helen the detector info of the EST 
        //  global and EST primary tracks are identical.
        //
        eptrack->setDetectorInfo(info);
        node->addTrack(eptrack);
    }
    if (nfailed)
        gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
                            << " EST primary tracks, no valid primary vertex found." << endm;
    
    //
    //  Load the dedx table and assign the dE/dx traits to all loaded
    //  global, tpt, primary, and EST tracks.
    //
    //  Since there is only one track ID there's no difference between
    //  standard tracks and EST tracks. The EST tracks will not
    //  have any dE/dx info from the SVT hits. 
    //
    dst_dedx_st *dstDedx = mEventManager->returnTable_dst_dedx(nrows);
    nfailed = 0;
    for (i=0; i<nrows; i++) {
        k = 0;
        id = dstDedx[i].id_track;
        if (id < vecGlobalTracks.size() && vecGlobalTracks[id] && dstDedx[i].det_id != kSvtId) {
            vecGlobalTracks[id]->addPidTraits(new StDedxPidTraits(dstDedx[i]));
            k++;
        }
        if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id] && dstDedx[i].det_id != kSvtId) {
            vecPrimaryTracks[id]->addPidTraits(new StDedxPidTraits(dstDedx[i]));
            k++;
        }
        if (id < vecTptTracks.size() && vecTptTracks[id] && dstDedx[i].det_id != kSvtId) {
            vecTptTracks[id]->addPidTraits(new StDedxPidTraits(dstDedx[i]));
            k++;
        }
        if (id < vecEstGlobalTracks.size() && vecEstGlobalTracks[id]) {
            vecEstGlobalTracks[id]->addPidTraits(new StDedxPidTraits(dstDedx[i]));
            k++;
        }
        if (id < vecEstPrimaryTracks.size() && vecEstPrimaryTracks[id]) {
	  vecEstPrimaryTracks[id]->addPidTraits(new StDedxPidTraits(dstDedx[i]));
	  k++;
        }
        if (!k) nfailed++;
    }
    if (nfailed)
        gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
                            << " dedx rows, no corresponding tracks found." << endm;
    
    //
    //  Find and setup primary vertices (if any).
    //  Add the referring primary tracks right away. 
    //  If the track could be assigned the corresponding
    //  entry in the temp vector (vecPrimaryTracksCopy)
    //  is set to 0. At the end we delete all primary
    //  tracks which couldn't be assigned to a vertex.
    //  Valid iflags in this context are = +[0-9]*10+1.
    //
    //  New: all vertices which are not event (primary)
    //  vertices are mostly for test and calibration
    //  purposes. People asked for it so we add them to
    //  a new container made for this purpose. They are
    //  stored via StEvent::addCalibrationVertex().
    //
    //  New: Add the possibility that a second primary 
    //  vertex is made using the EST primary tracks.
    //
    long nVertices;
    dst_vertex_st *dstVertices = mEventManager->returnTable_dst_vertex(nVertices);
    
    StVector(StPrimaryTrack*)    vecPrimaryTracksCopy = vecPrimaryTracks;
    StVector(StEstPrimaryTrack*) vecEstPrimaryTracksCopy = vecEstPrimaryTracks;
    
    for (i=0; i<nVertices; i++) {
        if (dstVertices[i].iflag <=0) 	 continue;
        // The code commented out below removes SVT-only vertices which we
        // need for various QA and debugging reasons. Should stay commented.
        //         if ((bad=badDstVertex(dstVertices+i))) {
        // 	  if (Debug())
        // 	      Warning("makeEvent","vertex rejected by %d",bad);
        // 	  dstVertices[i].iflag = -abs(dstVertices[i].iflag);
        // 	  continue;
        //         }
        
        if (dstVertices[i].iflag < 100 && dstVertices[i].iflag%10 == 1 &&
	  dstVertices[i].vtx_id == kEventVtxId ) {
            StPrimaryVertex *pvtx = new StPrimaryVertex(dstVertices[i]);
            for (k=0; k<vecPrimaryTracks.size(); k++) {
                if (vecPrimaryTracks[k] &&
                    vecPrimaryVertexId[k] == (unsigned int) dstVertices[i].id) {
                    pvtx->addDaughter(vecPrimaryTracks[k]);
                    vecPrimaryTracksCopy[k] = 0;
                }
            }
            for (k=0; k<vecEstPrimaryTracks.size(); k++) {
                if (vecEstPrimaryTracks[k] &&
                    vecEstPrimaryVertexId[k] == (unsigned int) dstVertices[i].id) {
                    pvtx->addDaughter(vecEstPrimaryTracks[k]);
                    vecEstPrimaryTracksCopy[k] = 0;
                }
            }
            mCurrentEvent->addPrimaryVertex(pvtx);
        }
        else {
            StCalibrationVertex *cvtx = new StCalibrationVertex(dstVertices[i]);
            mCurrentEvent->addCalibrationVertex(cvtx);
        }
    }
    nfailed = 0;
    for (k=0; k<vecPrimaryTracksCopy.size(); k++) {
        ptrack = vecPrimaryTracksCopy[k];
        if (!ptrack) continue;
        nfailed++;
        node = ptrack->node();    
        if (node) node->removeTrack(ptrack);
        delete ptrack;
        vecPrimaryTracksCopy[k] = 0;
        vecPrimaryTracks[k]     = 0;
    }
    if (nfailed)
        gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot assign " << nfailed
                            << " primary tracks, no corresponding primary vertex found." << endm;
    nfailed = 0;
    for (k=0; k<vecEstPrimaryTracksCopy.size(); k++){
        eptrack = vecEstPrimaryTracksCopy[k];
        if (!eptrack) continue;
        nfailed++;
        node = eptrack->node();
        if (node) node->removeTrack(eptrack);
        delete eptrack;
        vecEstPrimaryTracksCopy[k] = 0;
        vecEstPrimaryTracks[k] = 0;
    }
    if (nfailed)
        gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot assign " << nfailed
                            << " EST primary tracks, no corresponding primary vertex found." << endm;
    
    //
    //  Setup V0 vertices
    //
    long nV0Vertices;
    dst_v0_vertex_st* dstV0Vertices = mEventManager->returnTable_dst_v0_vertex(nV0Vertices);
    
    nfailed = 0;
    for (i=0; i<nV0Vertices; i++) {
        if((bad=badDstV0Vertex(dstV0Vertices+i))) {
	  if (Debug())
	      Warning("makeEvent","V0 vertex rejected by %d",bad);
	  nfailed++;
	  continue;
        }
        
        id = dstV0Vertices[i].id_vertex - 1;
        int ifail = 2;
        if (id < static_cast<unsigned long>(nVertices)
	  &&  dstVertices[id].iflag > 0) {
            StV0Vertex *v0 = new StV0Vertex(dstVertices[id], dstV0Vertices[i]);
            id = dstV0Vertices[i].idneg;
            if (id < vecGlobalTracks.size() && vecGlobalTracks[id] ) {
	      v0->addDaughter(vecGlobalTracks[id]);
	      ifail--;
            }
            id = dstV0Vertices[i].idpos;
            if (id < vecGlobalTracks.size() && vecGlobalTracks[id] ) {
	      v0->addDaughter(vecGlobalTracks[id]);
	      ifail--;
            }
            if (ifail) {
	      gMessMgr->Warning() << "StEventMaker::makeEvent(): V0 has <2 daughters. Delete it" << endm;
	      delete v0;
            }
	  else {
	      v0Vertices.push_back(v0);
            }
        }
        if (ifail) nfailed++;
    }
    if (nfailed)
        gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
                            << " V0 vertices, no valid id_vertex." << endm;
    
    //
    //  Setup Xi vertices
    //
    long         nXiVertices;
    dst_xi_vertex_st* dstXiVertices = mEventManager->returnTable_dst_xi_vertex(nXiVertices);
    
    nfailed = 0;
    for (i=0; i<nXiVertices; i++) {
        if ((bad=badDstXiVertex(dstXiVertices+i))) {
	  if (Debug())
	      Warning("makeEvent","Xi vertex rejected by %d",bad);
	  nfailed++;
	  continue;
        }
        id    = dstXiVertices[i].id_xi - 1;
        int ifail = 1;
        if (dstVertices[id].iflag > 0
	  && (id < static_cast<unsigned long>(nVertices))) {
            StXiVertex *xi = new StXiVertex(dstVertices[id], dstXiVertices[i]);
            id = dstXiVertices[i].id_v0 - 1;
            if (id < v0Vertices.size()) xi->setV0Vertex(v0Vertices[id]);
            id  = dstXiVertices[i].id_b;       // no -1 here
            if (id < vecGlobalTracks.size() && vecGlobalTracks[id]) {
	      xi->addDaughter(vecGlobalTracks[id]);
	      ifail--;
            }
            if (ifail) {
	      gMessMgr->Warning() << "StEventMaker::makeEvent(): Xi has <1 daughters. Delete it" << endm;
	      delete xi;
            }
	  else {
	      xiVertices.push_back(xi);
            }
        }
        if (ifail) nfailed++;
    }
    if (nfailed)
        gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
                            << " Xi vertices, invalid foreign key to vertex table." << endm;
    
    //
    //  Setup kinks
    //
    long nKinkVertices;
    dst_tkf_vertex_st* dstKinkVertices = mEventManager->returnTable_dst_tkf_vertex(nKinkVertices);
    
    nfailed = 0;
    for (i=0; i<nKinkVertices; i++) {
        if ((bad=badDstTkfVertex(dstKinkVertices+i))) {
	  if (Debug())
	      Warning("makeEvent","tkf vertex rejected by %d",bad);
	  nfailed++;
	  continue;
        }
        
        int ifail=3;
        id = dstKinkVertices[i].id_vertex - 1;
        if (dstVertices[id].iflag > 0
	  && (id < static_cast<unsigned long>(nVertices))) {
            StKinkVertex *kink = new StKinkVertex(dstVertices[id], dstKinkVertices[i]);
            id = dstKinkVertices[i].idd;
            if (id < vecGlobalTracks.size() && vecGlobalTracks[id]) {
	      ifail-=1; kink->addDaughter(vecGlobalTracks[id]);}
	  
            id = dstKinkVertices[i].idp;
            if (id < vecGlobalTracks.size()) {
	      ifail-=2; kink->setParent(vecGlobalTracks[id]);}
            if (ifail&1) {
	      gMessMgr->Warning() << "StEventMaker::makeEvent(): Kink has no daughter. Delete it" << endm;}
            if (ifail&2) {
	      gMessMgr->Warning() << "StEventMaker::makeEvent(): Kink has no parent.   Delete it" << endm;}
            if (ifail)
	      delete kink;
	  else
	      kinkVertices.push_back(kink);
        }
        if (ifail) nfailed++;
    }
    if (nfailed)
        gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
                            << " kink vertices, no valid id_vertex." << endm;
    
    //
    //  Setup hits
    //  Since the user might have decided to skip certain kind of hits
    //  we have to scan them all and get the first index and the total
    //  number of those which have to be loaded.
    //  This assumes that the hits are sorted according to detectors.
    //
    if (doLoadTpcHits || doLoadFtpcHits || doLoadSvtHits || doLoadSsdHits) {
        dst_point_st* dstPoints = mEventManager->returnTable_dst_point(nrows);
        typedef pair<int, int> intpair;
        StVector(intpair) index(16, intpair(nrows,0));     // start, size
        
        for (i=0; i<nrows; i++) {
            id = dstPoints[i].hw_position & 15;
            if (i < index[id].first) index[id].first = i;
            index[id].second++;
        }
        
        int  begin, end;
        
        
        //
        //  Make a compact list of unique detector infos
        //  for each track ID. Heavy use of STL here.
        //
        StSPtrVecTrackNode& allNodes = mCurrentEvent->trackNodes();
        mxId = allNodes.size();
        if (mxId>maxId) maxId = mxId;
        vector<vector<StTrackDetectorInfo*> > infomap(maxId+1);
        for (k=0; k<=(unsigned int)maxId; k++) {
	  if (k<vecTptTracks.size()        && vecTptTracks[k]           ) 
	      infomap[k].push_back(vecTptTracks[k]->detectorInfo()       );
	  if (k<vecGlobalTracks.size()     && vecGlobalTracks[k]        )
	      infomap[k].push_back(vecGlobalTracks[k]->detectorInfo()    );
	  if (k<vecEstGlobalTracks.size()  && vecEstGlobalTracks[k]     )
	      infomap[k].push_back(vecEstGlobalTracks[k]->detectorInfo() );
	  if (k<vecEstPrimaryTracks.size() && vecEstPrimaryTracks[k]    ) 
	      infomap[k].push_back(vecEstPrimaryTracks[k]->detectorInfo());
	  if (k<vecPrimaryTracks.size()    && vecPrimaryTracks[k]       )
	      infomap[k].push_back(vecPrimaryTracks[k]->detectorInfo()   );
        }
        
        vector<StTrackDetectorInfo*>::iterator iter;
        for (k=0; k<infomap.size(); k++) {
	  sort(infomap[k].begin(),infomap[k].end());
	  iter = unique(infomap[k].begin(),infomap[k].end());
	  infomap[k].erase(iter, infomap[k].end());
        }	
        
        //
        //        TPC hits
        //
        if (doLoadTpcHits) {
            info = 0;
            nfailed = 0;
            StTpcHit *tpcHit;
            begin = index[kTpcId].first;
            end   = index[kTpcId].first+index[kTpcId].second;
	  if (begin < end) {
	      StTpcHitCollection *tpcHitColl = new StTpcHitCollection;
	      for (i=begin; i<end; i++) {
		tpcHit = new StTpcHit(dstPoints[i]);
		if (tpcHitColl->addHit(tpcHit)) {
		    id = dstPoints[i].id_track;
		    for (k=0; k<infomap[id].size(); k++)
		        infomap[id][k]->addHit(tpcHit);
		}
		else {
		    nfailed++;
		    delete tpcHit;
		}
	      }
	      mCurrentEvent->setTpcHitCollection(tpcHitColl);
	  }
            if (nfailed)
                gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
                                    << " TPC hits, wrong hardware address." << endm;
        }
        
        //
        //        SVT hits
        //
        if (doLoadSvtHits) {
	  info = 0;
            nfailed = 0;
            StSvtHit *svtHit;
            begin = index[kSvtId].first;
            end   = index[kSvtId].first+index[kSvtId].second;
	  if (begin < end) {
	      StSvtHitCollection *svtHitColl = new StSvtHitCollection;
	      for (i=begin; i<end; i++) {
		svtHit = new StSvtHit(dstPoints[i]);
		if (svtHitColl->addHit(svtHit)) {
		    id = abs(dstPoints[i].id_track);           // new (Helen) 
		    for (k=0; k<infomap[id].size(); k++)
		        infomap[id][k]->addHit(svtHit);
		}
		else {
		    nfailed++;
		    delete svtHit;
		}
	      }
	      mCurrentEvent->setSvtHitCollection(svtHitColl);
	  }
            if (nfailed)
                gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
                                    << " SVT hits, wrong hardware address." << endm;
        }
        
        //
        //        SSD hits
        //
        if (doLoadSsdHits) {
            info    = 0;
            nfailed = 0;
            StSsdHit *ssdHit;
            begin = index[kSsdId].first;
            end   = index[kSsdId].first+index[kSsdId].second;
	  if (begin < end) {
	      StSsdHitCollection *ssdHitColl = new StSsdHitCollection;
	      for (i=begin; i<end; i++) {
		ssdHit = new StSsdHit(dstPoints[i]);
		if (ssdHitColl->addHit(ssdHit)) {
		    id = dstPoints[i].id_track;
		    for (k=0; k<infomap[id].size(); k++)
		        infomap[id][k]->addHit(ssdHit);
		}
		else {
		    nfailed++;
		    delete ssdHit;
		}
	      }
	      mCurrentEvent->setSsdHitCollection(ssdHitColl);
	  }
            if (nfailed)
                gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
                                    << " SSD hits, wrong hardware address." << endm;
        }
        
        //
        //        FTPC hits
        //
        if (doLoadFtpcHits) {
            info    = 0;
            nfailed = 0;
            StFtpcHit *ftpcHit;
            StFtpcHitCollection *ftpcHitColl = 0;
            // west
            begin = index[kFtpcWestId].first;
            end   = index[kFtpcWestId].first+index[kFtpcWestId].second;
	  if (begin < end) {
	      ftpcHitColl = new StFtpcHitCollection;
	      for (i=begin; i<end; i++) {
		ftpcHit = new StFtpcHit(dstPoints[i]);
		if (ftpcHitColl->addHit(ftpcHit)) {
		    id = dstPoints[i].id_track;
		    for (k=0; k<infomap[id].size(); k++)
		        infomap[id][k]->addHit(ftpcHit);
		}
		else {
		    nfailed++;
		    delete ftpcHit;
		}
	      }
	  }
	  // east
            begin = index[kFtpcEastId].first;
            end   = index[kFtpcEastId].first+index[kFtpcEastId].second;
	  if (begin < end) {
	      if (! ftpcHitColl) ftpcHitColl = new StFtpcHitCollection;
	      for (i=begin; i<end; i++) {
		ftpcHit = new StFtpcHit(dstPoints[i]);
		if (ftpcHitColl->addHit(ftpcHit)) {
		    id = dstPoints[i].id_track;
		    for (k=0; k<infomap[id].size(); k++)
		        infomap[id][k]->addHit(ftpcHit);
		}
		else {
		    nfailed++;
		    delete ftpcHit;
		}
	      }
	  }
            if (ftpcHitColl) mCurrentEvent->setFtpcHitCollection(ftpcHitColl);
            if (nfailed)
                gMessMgr->Warning() << "StEventMaker::makeEvent(): cannot store " << nfailed
                                    << " FTPC hits, wrong hardware address." << endm;
        }
    }
    
    //
    //  Add data from StEvtHddr we cannot get elsewhere
    //
    StEvtHddr* header = dynamic_cast<StEvtHddr*>(GetInputDS("EvtHddr"));
    if (header) {
        mCurrentEvent->setTriggerMask(header->GetTriggerMask());
        if (mCurrentEvent->info())
	  mCurrentEvent->info()->setEventSize(header->GetEventSize());
    }
    
    //
    //  Fill StRunInfo
    //
    StRunInfo* mCurrentRunInfo = new StRunInfo;
    StDetectorDbBeamInfo *dbBeamInfo = StDetectorDbBeamInfo::instance();
    StDetectorDbRichScalers* richScalers = StDetectorDbRichScalers::instance();
    
    mCurrentRunInfo->setRunId(mCurrentEvent->runId());
    mCurrentRunInfo->setProductionTime(time(0));                 
    mCurrentRunInfo->setProductionVersion(getenv("STAR_VERSION"));
    if (mCurrentEvent->summary())
        mCurrentRunInfo->setMagneticField(mCurrentEvent->summary()->magneticField());
    if (gStTpcDb) {
        mCurrentRunInfo->setTpcDriftVelocity(east, gStTpcDb->DriftVelocity());	
        mCurrentRunInfo->setTpcDriftVelocity(west, gStTpcDb->DriftVelocity());
    }
    if (dbBeamInfo) {
        mCurrentRunInfo->setCenterOfMassEnergy(dbBeamInfo->getBlueEnergy() + dbBeamInfo->getYellowEnergy());
        mCurrentRunInfo->setBeamMassNumber(blue, dbBeamInfo->getBlueMassNumber());  
        mCurrentRunInfo->setBeamMassNumber(yellow, dbBeamInfo->getYellowMassNumber());  
        mCurrentRunInfo->setBeamEnergy(blue, dbBeamInfo->getBlueEnergy());
        mCurrentRunInfo->setBeamEnergy(yellow, dbBeamInfo->getYellowEnergy());
        mCurrentRunInfo->setInitialBeamIntensity(blue, dbBeamInfo->getBlueIntensity());
        mCurrentRunInfo->setInitialBeamIntensity(yellow, dbBeamInfo->getYellowIntensity());
        mCurrentRunInfo->setBeamLifeTime(blue, dbBeamInfo->getBlueLifeTime());
        mCurrentRunInfo->setBeamLifeTime(yellow, dbBeamInfo->getYellowLifeTime());
        mCurrentRunInfo->setBeamFillNumber(blue, dbBeamInfo->getBlueFillNumber());
        mCurrentRunInfo->setBeamFillNumber(yellow, dbBeamInfo->getYellowFillNumber());
    }
    if (richScalers) {
        mCurrentRunInfo->setZdcWestRate(richScalers->getZDCWest());
        mCurrentRunInfo->setZdcEastRate(richScalers->getZDCEast());
        mCurrentRunInfo->setZdcCoincidenceRate(richScalers->getZDCX());
        mCurrentRunInfo->setBackgroundRate(richScalers->getMult());
        mCurrentRunInfo->setL0RateToRich(richScalers->getL0());
        mCurrentRunInfo->setBbcCoincidenceRate(richScalers->getBBCX());
        mCurrentRunInfo->setBbcEastRate(richScalers->getBBCEast());		
        mCurrentRunInfo->setBbcWestRate(richScalers->getBBCWest());		
        mCurrentRunInfo->setBbcBlueBackgroundRate(richScalers->getBBCBlueBkg());	
        mCurrentRunInfo->setBbcYellowBackgroundRate(richScalers->getBBCYellowBkg());
    }
    
    // Add SVT drift scaler to run info 
    St_DataSet *svtDriftScalerSet = GetDataSet("svt_hits/.data/svtDrift");
    if (svtDriftScalerSet) {
        St_dst_mon_soft_svt* svtDriftScaler = dynamic_cast<St_dst_mon_soft_svt*>(svtDriftScalerSet);    
        if (svtDriftScaler){
	  dst_mon_soft_svt_st* svtDriftScalerTable = svtDriftScaler->GetTable();
	  mCurrentRunInfo->setSvtDriftVelocityScaler(svtDriftScalerTable->res_drf_svt);
        }    
    }
    
    if (mCurrentRunInfo)
        mCurrentEvent->setRunInfo(mCurrentRunInfo);
    
    //
    //  Detector States
    //
    if (richScalers)
        mCurrentEvent->addDetectorState(new StDetectorState(kRichId, richScalers->getRichHVStatus()));
    
    return kStOK;
}

void
StEventMaker::printEventInfo()
{
    LOG_INFO << "*********************************************************" << endm;
    LOG_INFO << "*                  StEvent Information                  *" << endm;
    LOG_INFO << "*********************************************************" << endm;
    
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StEvent at " << (void*) mCurrentEvent                      << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (mCurrentEvent)
	 mCurrentEvent->Dump();
    else
        return;
    
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StRunInfo at " << (void*) mCurrentEvent->runInfo()         << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (mCurrentEvent->runInfo())
	 mCurrentEvent->runInfo()->Dump();
    
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StEventInfo at " << (void*) mCurrentEvent->info()          << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (mCurrentEvent->info())
	 mCurrentEvent->info()->Dump();
    
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StEventSummary at " << (void*) (mCurrentEvent->summary())  << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (mCurrentEvent->summary()) mCurrentEvent->summary()->Dump();
    if (mCurrentEvent->summary()) {
        unsigned int k;
 StEventSummary *evtsum = mCurrentEvent->summary();
    LOG_INFO << "--> StEventSummary quasi-histograms" << endm;
    LOG_INFO << "--> StEventSummary quasi-histogram -> # of tracks vs. eta" << endm;
        for (k=0; k<evtsum->numberOfBins(); k++) {
           LOG_INFO << k << "\t[" << evtsum->lowerEdgeEtaBin(k)
                 << " - "      << evtsum->upperEdgeEtaBin(k)
                 << "] : \t"  <<  evtsum->tracksInEtaBin(k) << endm;
        }
        LOG_INFO << "--> StEventSummary quasi-histogram -> # of tracks vs. phi" << endm;
        for (k=0; k<evtsum->numberOfBins(); k++) {
           LOG_INFO << k << "\t[" << evtsum->lowerEdgePhiBin(k)
                 << " - "      << evtsum->upperEdgePhiBin(k)
                 << "] : \t"   << evtsum->tracksInPhiBin(k) << endm;
        }
        LOG_INFO << "--> StEventSummary quasi-histogram -> # of tracks vs. pt" << endm;
        for (k=0; k<evtsum->numberOfBins(); k++) {
           LOG_INFO << k << "\t[" << evtsum->lowerEdgePtBin(k)
                 << " - "      << evtsum->upperEdgePtBin(k)
                 << "] : \t"   << evtsum->tracksInPtBin(k) << endm;
        }
        LOG_INFO << "--> StEventSummary quasi-histogram -> energy vs. eta" << endm;
        for (k=0; k<evtsum->numberOfBins(); k++) {
            LOG_INFO << k << "\t[" << evtsum->lowerEdgeEtaBin(k)
                 << " - "      << evtsum->upperEdgeEtaBin(k)
                 << "] : \t"   << evtsum->energyInEtaBin(k) << endm;
        }
        LOG_INFO << "--> StEventSummary quasi-histogram -> energy vs. phi" << endm;
        for (k=0; k<evtsum->numberOfBins(); k++) {
        LOG_INFO << k << "\t[" << evtsum->lowerEdgePhiBin(k)
                 << " - "      << evtsum->upperEdgePhiBin(k)
                 << "] : \t"   << evtsum->energyInPhiBin(k) << endm;
        }
    }
    
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSoftwareMonitor at "
         << (void*) (mCurrentEvent->softwareMonitor())                  << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (mCurrentEvent->softwareMonitor()) mCurrentEvent->softwareMonitor()->Dump();
    
    if (mCurrentEvent->softwareMonitor()) {
      LOG_INFO << "---------------------------------------------------------" << endm;
      LOG_INFO << "StTpcSoftwareMonitor at "
             << (void*) (mCurrentEvent->softwareMonitor()->tpc())           << endm;
      LOG_INFO << "---------------------------------------------------------" << endm;
        if (mCurrentEvent->softwareMonitor()->tpc())
            mCurrentEvent->softwareMonitor()->tpc()->Dump();
        
      LOG_INFO << "---------------------------------------------------------" << endm;
      LOG_INFO << "StSvtSoftwareMonitor at "
             << (void*) (mCurrentEvent->softwareMonitor()->svt())           << endm;
      LOG_INFO << "---------------------------------------------------------" << endm;
        if (mCurrentEvent->softwareMonitor()->svt())
            mCurrentEvent->softwareMonitor()->svt()->Dump();
        
     LOG_INFO<< "---------------------------------------------------------" << endm;
       LOG_INFO << "StFtpcSoftwareMonitor at "
             << (void*) (mCurrentEvent->softwareMonitor()->ftpc())          << endm;
       LOG_INFO << "---------------------------------------------------------" << endm;
        if (mCurrentEvent->softwareMonitor()->ftpc())
            mCurrentEvent->softwareMonitor()->ftpc()->Dump();
	
      LOG_INFO << "---------------------------------------------------------" << endm;
      LOG_INFO << "StEmcSoftwareMonitor at "
             << (void*) (mCurrentEvent->softwareMonitor()->emc())           << endm;
      LOG_INFO << "---------------------------------------------------------" << endm;
        if (mCurrentEvent->softwareMonitor()->emc())
            mCurrentEvent->softwareMonitor()->emc()->Dump();
        
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StRichSoftwareMonitor at "
             << (void*) (mCurrentEvent->softwareMonitor()->rich())          << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        if (mCurrentEvent->softwareMonitor()->rich())
            mCurrentEvent->softwareMonitor()->rich()->Dump();
        
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StCtbSoftwareMonitor at "
             << (void*) (mCurrentEvent->softwareMonitor()->ctb())           << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        if (mCurrentEvent->softwareMonitor()->ctb())
            mCurrentEvent->softwareMonitor()->ctb()->Dump();
        
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StL3SoftwareMonitor at "
             << (void*) (mCurrentEvent->softwareMonitor()->l3())            << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        if (mCurrentEvent->softwareMonitor()->l3())
            mCurrentEvent->softwareMonitor()->l3()->Dump();
        
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StGlobalSoftwareMonitor at "
             << (void*) (mCurrentEvent->softwareMonitor()->global())        << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        if (mCurrentEvent->softwareMonitor()->global())
            mCurrentEvent->softwareMonitor()->global()->Dump();
    }
    
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StL0Trigger at "
         << (void*) (mCurrentEvent->l0Trigger())                        << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (mCurrentEvent->l0Trigger()) mCurrentEvent->l0Trigger()->Dump();
    
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StTriggerDetectorCollection at "
         << (void*) (mCurrentEvent->triggerDetectorCollection())        << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (mCurrentEvent->triggerDetectorCollection())
        mCurrentEvent->triggerDetectorCollection()->Dump();

    if (mCurrentEvent->triggerDetectorCollection()) {
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StCtbTriggerDetector"                                      << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->triggerDetectorCollection()->ctb().Dump();
	
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StMwcTriggerDetector"                                      << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->triggerDetectorCollection()->mwc().Dump();
	
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StVpdTriggerDetector"                                      << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->triggerDetectorCollection()->vpd().Dump();
	
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StZdcTriggerDetector"                                      << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->triggerDetectorCollection()->zdc().Dump();
    }
    
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSPtrVecTrackDetectorInfo"                                << endm;
    LOG_INFO << "Dumping first element in collection only (if available). " << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "collection size = "
         << mCurrentEvent->trackDetectorInfo().size() << endm;
    
    if (mCurrentEvent->trackDetectorInfo().size()) {
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StTrackDetectorInfo at "
             << (void*) mCurrentEvent->trackDetectorInfo()[0]               << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->trackDetectorInfo()[0]->Dump();
    }
    
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSPtrVecTrackNode"                                        << endm;
    LOG_INFO << "Dumping first element in collection only (if available). " << endm;
    LOG_INFO << "All tracks in the first node are printed separately  "     << endm;
    LOG_INFO << "after the node info.                                     " << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "collection size = "
         << mCurrentEvent->trackNodes().size() << endm;
    
    unsigned int i;
    if (mCurrentEvent->trackNodes().size()) {
        LOG_INFO << "# tracks in first element = "
             << mCurrentEvent->trackNodes()[0]->entries() << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StTrackNode at "
             << (void*) mCurrentEvent->trackNodes()[0]                      << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->trackNodes()[0]->Dump();
        for (i=0; i<mCurrentEvent->trackNodes()[0]->entries(); i++)
            printTrackInfo(mCurrentEvent->trackNodes()[0]->track(i));
    }
    
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSPtrVecPrimaryVertex"                                    << endm;
    LOG_INFO << "Dumping first element in collection only (if available). " << endm;
    LOG_INFO << "The first daughter track (primary track) in the first    " << endm;
    LOG_INFO << "vertex is printed separately after the vertex info.      " << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "collection size = "
         << mCurrentEvent->numberOfPrimaryVertices() << endm;
    
    if (mCurrentEvent->numberOfPrimaryVertices()) {
        LOG_INFO << "# primary tracks in first element = "
             << mCurrentEvent->primaryVertex()->numberOfDaughters() << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StPrimaryVertex at "
             << (void*) mCurrentEvent->primaryVertex()                      << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->primaryVertex()->Dump();
        if (mCurrentEvent->primaryVertex()->numberOfDaughters())
            printTrackInfo(mCurrentEvent->primaryVertex()->daughter(0));
    }
    
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSPtrVecCalibrationVertex"                                << endm;
    LOG_INFO << "Dumping first element in collection only (if available). " << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "collection size = "
         << mCurrentEvent->numberOfCalibrationVertices() << endm;
    
    if (mCurrentEvent->numberOfCalibrationVertices())
        mCurrentEvent->calibrationVertex(0)->Dump();

    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSPtrVecV0Vertex"                                         << endm;
    LOG_INFO << "Dumping first element in collection only (if available). " << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "collection size = "
         << mCurrentEvent->v0Vertices().size() << endm;
    
    if (mCurrentEvent->v0Vertices().size()) {
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StV0Vertex at "
             << (void*) mCurrentEvent->v0Vertices()[0]                      << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->v0Vertices()[0]->Dump();
    }

    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSPtrVecXiVertex"                                         << endm;
    LOG_INFO << "Dumping first element in collection only (if available). " << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "collection size = "
         << mCurrentEvent->xiVertices().size() << endm;
    
    if (mCurrentEvent->xiVertices().size()) {
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StXiVertex at "
             << (void*) mCurrentEvent->xiVertices()[0]                      << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->xiVertices()[0]->Dump();
    }
    
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSPtrVecKinkVertex"                                       << endm;
    LOG_INFO << "Dumping first element in collection only (if available). " << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "collection size = "
         << mCurrentEvent->kinkVertices().size() << endm;
    
    if (mCurrentEvent->kinkVertices().size()) {
        LOG_INFO << "---------------------------------------------------------" << endm;
        LOG_INFO << "StKinkVertex at "
             << (void*) mCurrentEvent->kinkVertices()[0]                    << endm;
        LOG_INFO << "---------------------------------------------------------" << endm;
        mCurrentEvent->kinkVertices()[0]->Dump();
    }
    
    unsigned int       j=0, k=0, nhits=0;
    Bool_t             gotOneHit;
    StTpcHitCollection *tpcColl = mCurrentEvent->tpcHitCollection();
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StTpcHitCollection at " << (void*) tpcColl                 << endm;
    LOG_INFO << "Dumping collection size and one hit only."                 << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (tpcColl) {
        nhits = tpcColl->numberOfHits();
        LOG_INFO << "# of hits in collection = " << nhits << endm;
        gotOneHit = kFALSE;
        for (k=0; !gotOneHit && k<tpcColl->numberOfSectors(); k++)
            for (j=0; !gotOneHit && j<tpcColl->sector(k)->numberOfPadrows(); j++)
                if (tpcColl->sector(k)->padrow(j)->hits().size()) {
                    tpcColl->sector(k)->padrow(j)->hits()[0]->Dump();
                    gotOneHit = kTRUE;
                }
    }
    
    StFtpcHitCollection *ftpcColl = mCurrentEvent->ftpcHitCollection();
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StFtpcHitCollection at " << (void*) ftpcColl               << endm;
    LOG_INFO << "Dumping collection size and one hit only."                 << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (ftpcColl) {
        nhits = ftpcColl->numberOfHits();
        LOG_INFO << "# of hits in collection = " << nhits << endm;
        gotOneHit = kFALSE;
        for (k=0; !gotOneHit && k<ftpcColl->numberOfPlanes(); k++)
            for (j=0; !gotOneHit && j<ftpcColl->plane(k)->numberOfSectors(); j++)
                if (ftpcColl->plane(k)->sector(j)->hits().size()) {
                    ftpcColl->plane(k)->sector(j)->hits()[0]->Dump();
                    gotOneHit = kTRUE;
                }
    }
    
    StSvtHitCollection *svtColl = mCurrentEvent->svtHitCollection();
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSvtHitCollection at " << (void*) svtColl                 << endm;
    LOG_INFO << "Dumping collection size and one hit only."                 << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (svtColl) {
        nhits = svtColl->numberOfHits();
        LOG_INFO << "# of hits in collection = " << nhits << endm;
        gotOneHit = kFALSE;
        for (k=0; !gotOneHit && k<svtColl->numberOfBarrels(); k++)
            for (j=0; !gotOneHit && j<svtColl->barrel(k)->numberOfLadders(); j++)
                for (i=0; !gotOneHit && i<svtColl->barrel(k)->ladder(j)->numberOfWafers(); i++)
                    if (svtColl->barrel(k)->ladder(j)->wafer(i)->hits().size()) {
                        svtColl->barrel(k)->ladder(j)->wafer(i)->hits()[0]->Dump();
                        gotOneHit = kTRUE;
                    }
    }
    
    StSsdHitCollection *ssdColl = mCurrentEvent->ssdHitCollection();
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StSsdHitCollection at " << (void*) ssdColl                 << endm;
    LOG_INFO << "Dumping collection size and one hit only."                 << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (ssdColl) {
        nhits = ssdColl->numberOfHits();
        LOG_INFO << "# of hits in collection = " << nhits << endm;
        gotOneHit = kFALSE;
        for (k=0; !gotOneHit && k<ssdColl->numberOfLadders(); k++)
	    for (i=0; !gotOneHit && i<ssdColl->ladder(j)->numberOfWafers(); i++)
		if (ssdColl->ladder(j)->wafer(i)->hits().size()) {
		    ssdColl->ladder(j)->wafer(i)->hits()[0]->Dump();
		    gotOneHit = kTRUE;
		}
    }

    LOG_INFO << endm;

    //
    //   Info from some tables for comparisons.
    //   Only tables with varying # of rows are listed.
    //
    LOG_INFO << "*********************************************************" << endm;
    LOG_INFO << "*                   Table Information                   *" << endm;
    LOG_INFO << "*********************************************************" << endm;
    long nrows;
    LOG_INFO << "globtrk:    ";
    if (mEventManager->returnTable_dst_globtrk(nrows))    { LOG_INFO << nrows << endm; } else {LOG_INFO << "n/a" << endm;}
    LOG_INFO << "primtrk:    ";
    if (mEventManager->returnTable_dst_primtrk(nrows))    { LOG_INFO << nrows << endm; } else {LOG_INFO << "n/a" << endm;}
    LOG_INFO << "tpt:        ";
    if (mEventManager->returnTable_CpyTrk(nrows))         { LOG_INFO << nrows << endm; } else {LOG_INFO << "n/a" << endm;}
    LOG_INFO << "dedx:       ";
    if (mEventManager->returnTable_dst_dedx(nrows))       { LOG_INFO << nrows << endm; } else {LOG_INFO << "n/a" << endm;}
    LOG_INFO << "vertex:     ";
    if (mEventManager->returnTable_dst_vertex(nrows))     { LOG_INFO << nrows << endm; } else {LOG_INFO << "n/a" << endm;}
    LOG_INFO << "v0_vertex:  ";
    if (mEventManager->returnTable_dst_v0_vertex(nrows))  { LOG_INFO << nrows << endm; } else {LOG_INFO << "n/a" << endm;}
    LOG_INFO << "xi_vertex:  ";
    if (mEventManager->returnTable_dst_xi_vertex(nrows))  { LOG_INFO << nrows << endm; } else {LOG_INFO << "n/a" << endm;}
    LOG_INFO << "tkf_vertex: ";
    if (mEventManager->returnTable_dst_tkf_vertex(nrows)) { LOG_INFO << nrows << endm; } else {LOG_INFO << "n/a" << endm;}
    LOG_INFO << "point:      ";
    if (mEventManager->returnTable_dst_point(nrows))      { LOG_INFO << nrows << endm; } else {LOG_INFO << "n/a" << endm;}
    LOG_INFO << endm;
}

void
StEventMaker::printTrackInfo(StTrack* track)
{
    LOG_INFO << "---------------------------------------------------------" << endm;
    LOG_INFO << "StTrack (" << (track ? track->GetName() : "n/a")
         << ") at " << (void*) track                                    << endm;
    LOG_INFO << "---------------------------------------------------------" << endm;
    if (track) {
        track->Dump();
        LOG_INFO << "covariantMatrix():" << track->fitTraits().covariantMatrix() << endm;
	
        LOG_INFO << "---> StTrack -> StGeometry ("<< track->geometry()->GetName()
             << ") at " << (void*) (track->geometry()) << endm;
        if (track->geometry()) track->geometry()->Dump();

        LOG_INFO << "---> StTrack -> StGeometry (outer) ("<< track->outerGeometry()->GetName()
             << ") at " << (void*) (track->outerGeometry()) << endm;
        if (track->outerGeometry()) track->outerGeometry()->Dump();
	
        LOG_INFO << "---> StTrack -> StDetectorInfo at "
             << (void*) (track->detectorInfo()) << endm;
        if (track->detectorInfo()) track->detectorInfo()->Dump();
	
        LOG_INFO << "---> StTrack -> StTrackNode at "
             << (void*) (track->node()) << endm;
        if (track->node()) track->node()->Dump();
	
        LOG_INFO << "---> StTrack -> StPidTraits ("
             << (track->pidTraits().size() ? 1 : 0 ) << " of "
             <<  track->pidTraits().size() << " entries shown)" << endm;
        if (track->pidTraits().size()) track->pidTraits()[0]->Dump();
    }
}

/**************************************************************************
 * $Id: StEventMaker.cxx,v 2.84 2008/01/29 18:45:01 perev Exp $
 * $Log: StEventMaker.cxx,v $
 * Revision 2.84  2008/01/29 18:45:01  perev
 * WarnOff
 *
 * Revision 2.83  2007/08/24 17:26:29  fine
 * replace cout with LOG_INFO
 *
 * Revision 2.82  2007/08/24 15:07:43  perev
 * No print mem info by default
 *
 * Revision 2.81  2007/05/11 23:17:11  jeromel
 * Addition by J.Dunlop of random trigger (from db)
 *
 * Revision 2.80  2007/04/28 20:36:16  perev
 * Redundant StChain.h removed
 *
 * Revision 2.79  2006/09/14 00:08:04  ullrich
 * StTriggerDetectorCollection constructor changed and L2 \ninterface in StTriggerData (no run number needed.
 *
 * Revision 2.78  2006/08/21 20:58:20  ullrich
 * Pass run number to constructor of StTriggerDetectorCollection (Akio).
 *
 * Revision 2.77  2006/08/10 03:32:47  perev
 * Assert==>assert
 *
 * Revision 2.76  2006/05/04 19:15:40  ullrich
 * Added L3 expanded (Jamie).
 *
 * Revision 2.75  2005/12/07 19:38:19  perev
 * simplified logic if StEvent already exists
 *
 * Revision 2.74  2005/06/15 01:00:53  ullrich
 * Use abs() for SVT hit track ID
 *
 * Revision 2.73  2004/10/20 21:18:41  ullrich
 * Removed checks that prevented storage of calib. vertices.
 *
 * Revision 2.72  2004/08/28 18:52:02  fisyak
 * Replace StEvent Hit containers if there are entries in the corrensponding tables
 *
 * Revision 2.71  2004/08/13 18:45:44  ullrich
 * Set vertex-used-in-fit flag for primary tracks.
 *
 * Revision 2.70  2004/08/03 17:26:05  ullrich
 * Update of trigger part by Akio.
 *
 * Revision 2.69  2004/07/07 20:58:51  ullrich
 * Corrected way the SVT drift velocity scaler is obtained.
 *
 * Revision 2.68  2004/07/06 23:06:05  ullrich
 * Fill SVT drift velocity scaler into StRunInfo.
 *
 * Revision 2.67  2004/04/12 16:48:21  ullrich
 * Fixed bug in creating StTriggerDetectorCollection. StTriggerData
 * needs to be instantiated and added before StTriggerDetectorCollection
 * gets constructed.
 *
 * Revision 2.66  2004/02/14 19:25:27  perev
 * More strong check for secondaries objects
 *
 * Revision 2.65  2004/02/11 02:20:27  ullrich
 * Load StTriggerDetectorCollection using StTriggerData for runs
 * in 2003 and later. For 2001 and 2002 stick to TrgDet table.
 *
 * Revision 2.64  2004/01/22 23:35:55  ullrich
 * Fill new member of StRunInfo (RICH scaler).
 *
 * Revision 2.63  2003/12/29 00:00:34  perev
 * More debug prints
 *
 * Revision 2.62  2003/12/04 03:54:13  perev
 * Set empty, instead of crazy outer geometry
 *
 * Revision 2.61  2003/12/01 18:22:31  perev
 * Fix bug in bad track filter. Thanks to Helen
 *
 * Revision 2.60  2003/11/25 04:13:25  perev
 * FPE protection
 *
 * Revision 2.59  2003/10/13 04:38:35  perev
 * Bug with different collection lenth fixed
 *
 * Revision 2.58  2003/07/16 19:58:32  perev
 * Cleanup of StTriggerData2003 at all
 *
 * Revision 2.57  2003/04/16 17:52:28  ullrich
 * Cleaned up section where trigger structures are added,
 * that us removed debud print-outs.
 *
 * Revision 2.56  2003/04/15 22:37:54  jeromel
 * StTriggerDataMaker added
 *
 * Revision 2.55  2003/04/13 23:07:01  jeromel
 * Use of access method on null already fixed. Additional dbTriggerId fix closes bug # 85
 *
 * Revision 2.54  2003/04/08 18:44:03  ullrich
 * Added protection for cases where StDetectorDbTriggerID and
 * StTrigSummary couldn't be obtained (see Trigger Id summary).
 *
 * Revision 2.53  2003/02/19 16:35:04  jeromel
 * $LINK mechanism removed
 *
 * Revision 2.52  2003/02/18 23:25:55  jeromel
 * Loop over trigger blabla corrected
 *
 * Revision 2.51  2003/02/15 20:19:31  genevb
 * Added Trigger ID Summary
 *
 * Revision 2.50  2002/11/26 02:19:48  perev
 * StEventMaker ITTF modif
 *
 * Revision 2.49  2002/05/02 03:07:18  ullrich
 * Changed mechanism to reject EST tracks without SVT hits.
 *
 * Revision 2.48  2002/05/01 01:08:31  ullrich
 * Add SVT dE/dx only to EST tracks.
 *
 * Revision 2.47  2002/04/18 23:29:34  jeromel
 * Implementation of the SVT 2 tables scheme ...
 *
 * Revision 2.46  2002/02/25 19:34:14  ullrich
 * Fill parts of StRunInfo from StDetectorDbBeamInfo.
 *
 * Revision 2.45  2002/02/15 23:06:58  ullrich
 * Fill detector state for RICH.
 *
 * Revision 2.44  2002/01/31 23:50:28  ullrich
 * More filling of StRunInfo (by J. Gans).
 *
 * Revision 2.43  2002/01/11 16:44:12  ullrich
 * Fill bunch crossing numbers in StEventInfo.
 *
 * Revision 2.42  2001/12/21 22:39:32  ullrich
 * Disabled filling parts of StRunInfo.
 *
 * Revision 2.41  2001/12/21 21:13:03  ullrich
 * Fixed bug: loading multiple primary vertices.
 *
 * Revision 2.40  2001/11/10 23:54:21  ullrich
 * Added calibration vertices.
 *
 * Revision 2.39  2001/11/07 21:20:46  ullrich
 * Added L1 trigger.
 *
 * Revision 2.38  2001/09/28 22:22:05  ullrich
 * Load helix geometry at last point of each track.
 *
 * Revision 2.37  2001/09/19 04:49:05  ullrich
 * Set event size in StEventInfo.
 *
 * Revision 2.36  2001/09/18 00:16:06  ullrich
 * Fill and add StRunInfo.
 *
 * Revision 2.35  2001/09/12 23:49:22  ullrich
 * Removed code to build StRun and StRunSummary.
 *
 * Revision 2.34  2001/07/19 00:05:28  ullrich
 * New StL0Trigger needs additional table in constructor.
 *
 * Revision 2.33  2001/07/17 22:21:50  ullrich
 * Use B from event summary to set helicity of tracks.
 *
 * Revision 2.32  2001/05/17 22:46:37  ullrich
 * Removed loading of event summary params.
 *
 * Revision 2.31  2001/02/22 05:02:40  ullrich
 * Added new protected method getStEventInstance().
 * Modified maker to allow multiple calls of Make() within
 * one event. If instance already it is re-used and the data
 * from existing tables gets added.
 *
 * Revision 2.30  2000/11/02 16:33:28  ullrich
 * Fixed tiny memory leak.
 *
 * Revision 2.29  2000/08/30 05:37:02  ullrich
 * Obtain trigger mask from StEvtHddr dataset.
 *
 * Revision 2.28  2000/08/17 00:38:48  ullrich
 * Allow loading of tpt tracks.
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
 *
 * Revision 2.14  2000/01/14 13:58:03  ullrich
 * Create and fill the RICH pixel collection. Added also
 * the debug output for the RICH to printEventInfo().
 *
 * Revision 2.13  2000/01/11 16:05:34  ullrich
 * With Victors help now possible to read the dst_summary_param
 * table from the runco branch and build StEventSummary objects.
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
 *
 * Revision 2.9  1999/12/07 18:58:39  ullrich
 * Modified to get rid of some warnings on Linux
 *
 * Revision 2.8  1999/11/23 17:14:19  ullrich
 * Forgot to fill PID traits. Fixed now.
 *
 * Revision 2.7  1999/11/17 14:10:27  ullrich
 * Added more checks to protect from corrupted table data.
 *
 * Revision 2.6  1999/11/11 17:46:30  ullrich
 * Added more checks and warning messages. Handling
 * of primary vertices made safer
 *
 * Revision 2.5  1999/11/11 10:02:58  ullrich
 * Added warning message in case some hits cannot be stored.
 *
 * Revision 2.4  1999/11/10 22:40:27  ullrich
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
